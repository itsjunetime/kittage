//! An implementation of [`AsyncInputReader`] for [`crossterm::event::EventStream`]

use std::{
	pin::Pin,
	task::{Context, Poll},
	time::Duration
};

use crossterm::event::{Event, EventStream, KeyModifiers};
use futures_core::Stream;

use crate::AsyncInputReader;

/// An error that can happen while using [`EventStream`] as an [`AsyncInputReader`]
#[derive(Debug, thiserror::Error)]
pub enum InputErr {
	/// The expected bit of data was not received before the timeout duration elapsed
	#[error("{0}")]
	Timeout(#[from] tokio::time::error::Elapsed),
	/// Something went wrong on the IO side of things - this is bubbled up from [`EventStream`]'s
	/// [`Stream`] implementation
	#[error("{0}")]
	IO(#[from] std::io::Error)
}

impl AsyncInputReader for &mut EventStream {
	type Error = InputErr;
	async fn read_esc_delimited_str_with_timeout(
		&mut self,
		buf: &mut String,
		timeout: Duration
	) -> Result<(), Self::Error> {
		tokio::time::timeout(timeout, async {
			let mut found_one_esc = false;
			loop {
				match Next(Some(*self)).await {
					// we're done reading from input
					None => return Ok(()),
					Some(Err(e)) => return Err(e),
					Some(Ok(Event::Key(k))) =>
						if let Some(c) = k.code.as_char() {
							if k.modifiers.contains(KeyModifiers::ALT) {
								if !found_one_esc {
									found_one_esc = true;
								} else {
									return Ok(());
								}
							}

							buf.push(c);
						},
					Some(Ok(Event::Paste(s))) => match s.find('\x1b') {
						Some(pos) => {
							buf.push_str(&s[..pos]);
							return Ok(());
						}
						None => buf.push_str(&s)
					},
					// if it's a different type of event, I guess we just swallow it? Or should
					// we error here? Idk. [todo]
					Some(Ok(_)) => ()
				}
			}
		})
		.await?
		.map_err(InputErr::from)
	}
}

// mostly stolen from `futures`
struct Next<'a>(Option<&'a mut EventStream>);

impl Future for Next<'_> {
	type Output = Option<<EventStream as Stream>::Item>;

	fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		match &mut self.0 {
			Some(p) => match Pin::<&mut EventStream>::new(p).poll_next(cx) {
				Poll::Ready(t) => {
					self.0 = None;
					Poll::Ready(t)
				}
				Poll::Pending => Poll::Pending
			},
			None => Poll::Pending
		}
	}
}

impl Unpin for Next<'_> where EventStream: Unpin {}

#[cfg(test)]
mod tests {
	use std::{
		io::{Read, StdoutLock, pipe},
		num::NonZeroU32,
		panic::{set_hook, take_hook},
		path::PathBuf,
		process::{Command, Stdio}
	};

	use crossterm::terminal::{disable_raw_mode, enable_raw_mode};

	use super::*;
	use crate::{
		NumberOrId,
		action::Action,
		delete::{ClearOrDelete, DeleteConfig, WhichToDelete},
		display::DisplayConfig,
		image::Image,
		medium::Medium,
		lib_tests::png_path
	};

	const REAL_TEST_VAR: &str = "KITTYIMG_REAL_TEST";

	fn spawn_kitty_for(test: &'static str) {
		let test_binary = PathBuf::from(std::env::args().next().unwrap())
			.canonicalize()
			.unwrap();

		println!("trying to run 'env {REAL_TEST_VAR}=1 {test_binary:?} --nocapture {test}'");

		let (mut reader, writer) = pipe().unwrap();
		let status = Command::new("kitty")
			.args([
				"@",
				"launch",
				"--wait-for-child-to-exit",
				"--type=os-window",
				"--dont-take-focus",
				"--os-window-state=minimized",
				"--response-timeout=60",
				"--env"
			])
			.arg(format!("{REAL_TEST_VAR}=1"))
			.arg(test_binary)
			.arg("--nocapture")
			.arg(test)
			.env(REAL_TEST_VAR, "1")
			.stdout(writer)
			.stderr(Stdio::null())
			.stdin(Stdio::null())
			.spawn()
			.unwrap()
			.wait()
			.unwrap();

		assert!(status.success());

		let mut test_status = String::new();
		assert_ne!(reader.read_to_string(&mut test_status).unwrap(), 0);
		let test_status = test_status.trim_end();

		assert!(test_status.starts_with('0'), "got status '{test_status}'");
	}

	macro_rules! divert_if_not_spawned_test {
		($test:expr) => {
			if std::env::var(REAL_TEST_VAR).is_err() {
				spawn_kitty_for($test);
				return;
			}
		};
	}

	#[tokio::test]
	async fn transmit_display_then_display() {
		divert_if_not_spawned_test!("transmit_display_then_display");

		enable_raw_mode().unwrap();

		let old_hook = take_hook();
		set_hook(Box::new(move |panic_info| {
			drop(disable_raw_mode());
			old_hook(panic_info);
		}));

		let img_path = png_path();

		struct DualWriter<'stdout> {
			stdout: StdoutLock<'stdout>,
			buf: String
		}

		impl std::io::Write for DualWriter<'_> {
			fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
				self.buf.push_str(std::str::from_utf8(buf).unwrap());
				self.stdout.write(buf)
			}

			fn flush(&mut self) -> std::io::Result<()> {
				self.stdout.flush()
			}
		}

		let stdout = std::io::stdout();

		let mut writer = DualWriter {
			stdout: stdout.lock(),
			buf: String::new()
		};
		let mut ev_stream = EventStream::new();

		let res = Action::TransmitAndDisplay {
			image: Image {
				format: crate::PixelFormat::Png(None),
				medium: Medium::File(img_path.clone()),
				num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap())
			},
			config: DisplayConfig::default(),
			placement_id: None
		}
		.execute_async(&mut writer, &mut ev_stream)
		.await;

		if let Err(e) = res {
			println!("tried to print {:?}", writer.buf);
			panic!("{e}");
		}

		writer.buf.clear();

		let res = Action::Display {
			image_id: NonZeroU32::new(1).unwrap(),
			placement_id: NonZeroU32::new(2).unwrap(),
			config: DisplayConfig::default()
		}
		.execute_async(&mut writer, &mut ev_stream)
		.await;

		if let Err(e) = res {
			println!("tried to print {:?}", writer.buf);
			panic!("{e}");
		}

		writer.buf.clear();

		let res = Action::Delete(DeleteConfig {
			effect: ClearOrDelete::Delete,
			which: WhichToDelete::All
		})
		.execute_async(&mut writer, &mut ev_stream)
		.await;

		if let Err(e) = res {
			println!("tried to print {:?}", writer.buf);
			panic!("{e}");
		}

		disable_raw_mode().unwrap();
	}
}
