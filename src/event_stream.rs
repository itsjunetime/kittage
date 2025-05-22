//! An implementation of [`AsyncInputReader`] for [`crossterm::event::EventStream`]

use std::{
	pin::Pin,
	task::{Context, Poll},
	time::Duration
};

use crossterm::event::{Event, EventStream};
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

impl AsyncInputReader for EventStream {
	type Error = InputErr;
	async fn read_into_buf_until_char_with_timeout(
		&mut self,
		buf: &mut String,
		end: char,
		timeout: Duration
	) -> Result<(), Self::Error> {
		tokio::time::timeout(timeout, async {
			loop {
				match Next(self).await {
					// we're done reading from input
					None => return Ok(()),
					Some(Err(e)) => return Err(e),
					Some(Ok(Event::Key(k))) =>
						if let Some(c) = k.code.as_char() {
							if c == end {
								return Ok(());
							}
							buf.push(c);
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
struct Next<'a>(&'a mut EventStream);

impl Future for Next<'_> {
	type Output = Option<<EventStream as Stream>::Item>;

	fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		Pin::new(&mut *self.0).poll_next(cx)
	}
}

impl Unpin for Next<'_> where EventStream: Unpin {}
