use std::io::Write;

/// A struct to facilitate
pub struct TmuxWriter<W: Write> {
	inner: W
}

impl<W: Write> TmuxWriter<W> {
	pub fn new(mut inner: W) -> std::io::Result<Self> {
		inner.write_all(b"\x1bPtmux;")?;
		Ok(Self { inner })
	}
}

impl<W: Write> Write for TmuxWriter<W> {
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		let mut last_idx_written = None;
		while let Some(next_pos) = memchr::memchr(b'\x1b', buf) {
			self.inner.write_all(&buf[..=next_pos])?;
			self.inner.write_all(&[0x1b])?;
			last_idx_written = Some(next_pos);
		}

		match last_idx_written {
			Some(i) => match i.checked_add(1) {
				Some(i) => self.inner.write_all(&buf[i..]),
				// If the last byte written was at idx usize::MAX, then there's no way the buffer
				// can be longer than that, so we wrote everything.
				None => Ok(())
			},
			None => self.inner.write_all(buf)
		}
		.map(|()| buf.len())
	}

	fn flush(&mut self) -> std::io::Result<()> {
		self.inner.write_all(b"\x1b\\")?;
		self.inner.flush()
	}
}
