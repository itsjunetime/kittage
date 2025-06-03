//! The [`Medium`], [`SharedMemObject`] and [`ChunkSize`] structs for configuring how data is sent

use std::{borrow::Cow, io::Write, num::NonZeroU16, path::Path};

use base64::{engine::general_purpose::STANDARD_NO_PAD, write::EncoderWriter as Base64Encoder};

use crate::Encoder;

/// The amount of data that should be sent within a single escape code when transferring data via a
/// 'direct' medium (see [`Medium::Direct`]) - this data does not necessarily *need* to be chunked
/// when sending directly, but not chunking it increases the likelihood that the transferred image
/// will be rejected, as escape codes have maximum lengths on many platforms. See
/// [`ChunkSize::new`] for specifications of what it should contain
//
// INVARIANT: `self.0` must always be <= 1024
#[derive(Debug, PartialEq)]
pub struct ChunkSize(NonZeroU16);

impl ChunkSize {
	/// `size` represents the number of 4-byte chunks of (already base64-encoded) data sent within
	/// each escape code. If it is greater than 1024, this will return [`None`] as the protocol
	/// specifies that no more than 4096 bytes may be sent at a time.
	pub fn new(size: NonZeroU16) -> Option<Self> {
		(size.get() <= 1024).then_some(Self(size))
	}
}

impl Default for ChunkSize {
	fn default() -> Self {
		Self(NonZeroU16::new(1024).unwrap())
	}
}

/// The medium through which the file itself will be transferred to the terminal
#[derive(Debug, PartialEq)]
pub enum Medium<'data> {
	/// Direct (the data is transmitted within the escape code itself)
	Direct {
		/// The maximum length of data sent within each escape code. To quote from the
		/// specification:
		///
		/// > Remote clients, those that are unable to use the filesystem/shared memory to transmit data, must send the pixel data directly using escape codes. Since escape codes are of limited maximum length, the data will need to be chunked up for transfer.
		///
		/// See the documentation on [`ChunkSize`] for more details
		chunk_size: Option<ChunkSize>,
		/// The image data to be displayed - when using this to display an [`Image`] (or within an
		/// [`Action`]), this data must be of the same underlying image format as the
		/// [`PixelFormat`] specified.
		data: Cow<'data, [u8]>
	},
	/// A simple file (regular files only, not named pipes, device files, etc.)
	File(Box<Path>),
	/// A temporary file, the terminal emulator will delete the file after reading the pixel data.
	/// For security reasons the terminal emulator should only delete the file if it is in a known
	/// temporary directory, such as `/tmp`, `/dev/shm`, `TMPDIR` env var if present, and any
	/// platform specific temporary directories and the file has the string `tty-graphics-protocol`
	/// in its full file path
	TempFile(Box<Path>),
	/// A _shared memory object_, which on POSIX systems is a [POSIX shared memory
	/// object](https://pubs.opengroup.org/onlinepubs/9699919799/functions/shm_open.html)
	/// and on Windows is a [Named shared memory object]
	/// (https://docs.microsoft.com/en-us/windows/win32/memory/creating-named-shared-memory).
	SharedMemObject {
		/// The name of the given object to be opened and passed to the terminal
		name: Cow<'data, str>
	}
}

pub(crate) fn write_b64<W: Write>(data: &[u8], writer: W) -> std::io::Result<W> {
	let mut b64 = Base64Encoder::new(writer, &STANDARD_NO_PAD);
	b64.write_all_allow_empty(data)?;
	b64.finish()
}

impl Medium<'_> {
	pub(crate) fn write_data<W: Write>(&self, mut writer: W) -> Result<W, std::io::Error> {
		let (key, data) = match self {
			Self::Direct { data, chunk_size } => {
				if let Some(chunk_size) = chunk_size {
					write!(writer, ",t=d,")?;
					// spec says we first have to encode, and THEN chunk it. I think that we could
					// do some math and chunk it by like chunk_size * (8 / 6)? or something? but
					// I'll do that later or smth. Either way, before we do that, we have to encode
					// it to a string
					let b64_encoded_bytes_per_chunk = usize::from(chunk_size.0.get()) * 4;
					let pre_encoded_bytes_per_chunk = (b64_encoded_bytes_per_chunk / 4) * 3;
					let total_chunks = data.len().div_ceil(pre_encoded_bytes_per_chunk);

					// then, since we know that it's base64-encoded, we can be confident each
					// character only takes up a single byte, so we can just chunk it by-byte
					let mut chunks = data.chunks(pre_encoded_bytes_per_chunk);

					// we need to write the first one differently since it is just being added onto
					// the end of the current command
					if let Some(first) = chunks.next() {
						write!(writer, "m={};", u8::from(total_chunks != 1))?;
						writer = write_b64(first, writer)?;
						write!(writer, "\x1b\\")?;
					}

					// then all the ones after can just be printed in the same way
					for (idx, chunk) in chunks.enumerate() {
						write!(writer, "\x1b_Gm={};", u8::from(total_chunks != idx + 2))?;
						writer = write_b64(chunk, writer)?;
						write!(writer, "\x1b\\")?;
					}

					return Ok(writer);
				}

				// and if we don't chunk it at all, then just give it to be printed normally
				('d', &**data)
			}
			Self::File(path) => ('f', path.as_os_str().as_encoded_bytes()),
			Self::TempFile(path) => ('t', path.as_os_str().as_encoded_bytes()),
			Self::SharedMemObject { name } => ('s', name.as_bytes())
		};

		write!(writer, ",t={key};")?;
		let mut writer = Base64Encoder::new(writer, &STANDARD_NO_PAD);
		writer.write_all_allow_empty(data)?;
		writer.finish()
	}
}
