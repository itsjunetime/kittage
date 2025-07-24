//! Struct for facilitating Tmux passthrough

use std::io::Write;

/// A struct to facilitate writing to tmux running on top of a terminal. In most cases, `W` is
/// going to be [`std::io::Stdout`].
///
/// This struct is made specifically to be used with this library. It cannot be arbitrarily used to
/// escape data to pass through Tmux.
pub struct TmuxWriter<W: Write> {
	/// the inner writer
	inner: W,
	/// Whether we've written the initial `\x1bPtmux;` that is required at the beginning of our
	/// message. This should be reset to `false` after we detect the final `\\` written to the
	/// buffer.
	wrote_first: bool
}

impl<W: Write> TmuxWriter<W> {
	/// Create a new instance of [`Self`], wrapping the given struct.
	pub fn new(inner: W) -> Self {
		Self {
			inner,
			wrote_first: false
		}
	}
}

impl<W: Write> Write for TmuxWriter<W> {
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		if !self.wrote_first {
			self.inner.write_all(b"\x1bPtmux;")?;
			self.wrote_first = true;
		}

		let mut last_idx_written = None;
		while let Some(next_pos) = memchr::memchr(b'\x1b', buf) {
			self.inner.write_all(&buf[..=next_pos])?;
			self.inner.write_all(&[0x1b])?;
			last_idx_written = Some(next_pos);
		}

		match last_idx_written {
			// If the last byte written was at idx usize::MAX, then there's no way the buffer
			// can be longer than that, so we wrote everything.
			Some(i) =>
				if let Some(i) = i.checked_add(1) {
					self.inner.write_all(&buf[i..])?;
				},
			None => self.inner.write_all(buf)?
		}

		if buf.last().is_some_and(|b| *b == b'\\') {
			self.inner.write_all(b"\x1b\\")?;
			self.wrote_first = false;
		}

		Ok(buf.len())
	}

	fn flush(&mut self) -> std::io::Result<()> {
		self.inner.flush()
	}
}
