#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

pub mod delete;
pub mod display;
pub mod error;
#[cfg(any(test, feature = "crossterm-tokio"))]
pub mod event_stream;
pub mod image;

use std::{
	borrow::Cow,
	error::Error,
	fmt::Display,
	io::Write,
	num::{NonZeroU16, NonZeroU32},
	path::Path,
	time::Duration
};

use base64::{
	Engine,
	engine::{GeneralPurpose, general_purpose::STANDARD_NO_PAD},
	write::EncoderWriter as Base64Encoder
};
use delete::DeleteConfig;
use display::DisplayConfig;
use error::TransmitError;
use image::{Image, read_parse_response, read_parse_response_async};

trait Encoder<W: Write>: Write {
	fn write_all_allow_empty(&mut self, slice: &[u8]) -> std::io::Result<()> {
		let mut total_written = 0;
		while let Some(remaining) = slice.get(total_written..) {
			if remaining.is_empty() {
				break;
			}
			total_written += self.write(remaining)?;
		}
		Ok(())
	}
}

impl<W: Write> Encoder<W> for Base64Encoder<'_, GeneralPurpose, W> {}

trait Encodable<W: Write, const KEY: char>: PartialEq + Sized {
	const DEFAULT: Self;
	fn write_kv_encoded(&self, mut writer: W) -> std::io::Result<W> {
		if self == &Self::DEFAULT {
			return Ok(writer);
		}
		write!(writer, ",{KEY}=")?;
		self.write_value_to(writer)
	}
	fn write_value_to(&self, writer: W) -> std::io::Result<W>;
}

/// A Shared memory object which can be used for transferring an image over to the terminal. Works
/// on both unix and windows
pub struct SharedMemObject {
	#[cfg(unix)]
	inner: psx_shm::Shm,

	#[cfg(windows)]
	inner: winmmf::MemoryMappedFile<mmf::RwLock<'static>>
}

impl SharedMemObject {
	/// Construct a new instance after opening a [`psx_shm::Shm`]
	#[cfg(unix)]
	pub fn new(inner: psx_shm::Shm) -> Self {
		Self { inner }
	}

	/// Construct a new instance after opening a [`winmmf::MemoryMappedFile`]
	#[cfg(windows)]
	pub fn new(inner: winmmf::MemoryMappedFile<mmf::RwLock<'static>>) -> Self {
		Self { inner }
	}

	fn name(&self) -> Box<str> {
		#[cfg(unix)]
		{
			self.inner.name().into()
		}

		#[cfg(windows)]
		{
			self.inner.fullname().into()
		}
	}
}

/// The amount of data that should be sent within a single escape code when transferring data via a
/// 'direct' medium (see [`Medium::Direct`]) - this data does not necessarily *need* to be chunked
/// when sending directly, but not chunking it increases the likelihood that the transferred image
/// will be rejected, as escape codes have maximum lengths on many platforms. See
/// [`ChunkSize::new`] for specifications of what it should contain
//
// INVARIANT: `self.0` must always be <= 1024
pub struct ChunkSize(NonZeroU16);

impl ChunkSize {
	/// `size` represents the number of 4-byte chunks of (already base64-encoded) data sent within
	/// each escape code. If it is greater than 1024, this will return [`None`] as the protocol
	/// specifies that no more than 4096 bytes may be sent at a time.
	pub fn new(size: NonZeroU16) -> Option<Self> {
		(size.get() <= 1024).then_some(Self(size))
	}
}

/// The medium through which the file itself will be transferred to the terminal
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
	/// (represented here with a [`psx_shm::Shm`] and on Windows is a [Named shared memory
	/// object](https://docs.microsoft.com/en-us/windows/win32/memory/creating-named-shared-memory)
	/// (represented here with a [`winmmf::MemoryMappedFile`]). The terminal emulator must read the
	/// data from the memory object and then unlink and close it on POSIX and just close it on
	/// Windows.
	SharedMemObject(SharedMemObject)
}

impl Medium<'_> {
	fn write_data<W: Write>(&self, mut writer: W) -> Result<W, std::io::Error> {
		let name: Box<str>;
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
			Self::SharedMemObject(o) => {
				name = o.name();
				('s', name.as_bytes())
			}
		};

		write!(writer, ",t={key};")?;
		let mut writer = Base64Encoder::new(writer, &STANDARD_NO_PAD);
		writer.write_all_allow_empty(data)?;
		writer.finish()
	}
}

pub(crate) trait WriteUint: Write + Sized {
	fn write_uint<const KEY: char, E: Encodable<Self, KEY>>(self, u: E) -> std::io::Result<Self> {
		if u != E::DEFAULT {
			u.write_kv_encoded(self)
		} else {
			Ok(self)
		}
	}
}

impl<T: Write + Sized> WriteUint for T {}

/// The format of the image data that is being sent
#[derive(PartialEq)]
pub enum PixelFormat {
	/// 3 bytes per pixel, with color in the sRGB color space
	Rgb24(ImageDimensions, Option<Compression>),
	/// 4 bytes per pixel (3 for the color and 1 for the alpha), with color in the sRGB color space
	Rgba32(ImageDimensions, Option<Compression>),
	/// PNG data - you can specify if compression is being used, and if compression is being used
	/// then you must also supply the total size, in bytes, of the compressed data (not how much
	/// space it will decompress to, but rather how much space the terminal emulator should read to
	/// then decode to get the image)
	Png(Option<(Compression, u32)>)
}

impl<W: Write> Encodable<W, 'f'> for PixelFormat {
	// this isn't actually the default? but whatever, we aren't gonna touch it
	const DEFAULT: Self = Self::Rgb24(
		ImageDimensions {
			width: 0,
			height: 0
		},
		None
	);

	fn write_value_to(&self, mut writer: W) -> std::io::Result<W> {
		fn write_w_h_cmp<W: Write>(
			mut writer: W,
			width: u32,
			height: u32,
			cmp: Option<Compression>
		) -> std::io::Result<W> {
			writer = writer
				.write_uint::<SOURCE_WIDTH_KEY, _>(width)?
				.write_uint::<SOURCE_HEIGHT_KEY, _>(height)?;
			writer = cmp.write_kv_encoded(writer)?;
			Ok(writer)
		}

		match self {
			Self::Rgb24(ImageDimensions { width, height }, cmp) => {
				write!(writer, "24")?;
				write_w_h_cmp(writer, *width, *height, *cmp)
			}
			Self::Rgba32(ImageDimensions { width, height }, cmp) => {
				write!(writer, "32")?;
				write_w_h_cmp(writer, *width, *height, *cmp)
			}
			Self::Png(cmp_and_size) => {
				write!(writer, "100")?;
				if let Some((cmp, size)) = cmp_and_size {
					writer = Some(*cmp)
						.write_kv_encoded(writer)?
						.write_uint::<COMPRESSED_DATA_SIZE_KEY, _>(*size)?;
				}
				Ok(writer)
			}
		}
	}
}

/// The dimensions of the image to display. This specifies a specific crop of the image to display,
/// and must not specify a size larger than the source image. If it is smaller, only a cropped
/// subset of the original image will be sent/displayed
#[derive(PartialEq)]
pub struct ImageDimensions {
	/// The number of pixels wide, divided by 4 (for some reason)
	width: u32,
	/// The number of pixels high, divided by 4 (for some reason)
	height: u32
}

pub(crate) const SOURCE_WIDTH_KEY: char = 's';
pub(crate) const SOURCE_HEIGHT_KEY: char = 'v';
pub(crate) const COMPRESSED_DATA_SIZE_KEY: char = 's';
pub(crate) const READ_SIZE_KEY: char = 'S';
pub(crate) const READ_OFFSET_KEY: char = 'O';
pub(crate) const REMAINING_CHUNKS_KEY: char = 'm';
pub(crate) const TRANSFER_ID_KEY: char = 'i';
pub(crate) const IMAGE_NUMBER_KEY: char = 'I';
pub(crate) const PLACEMENT_ID_KEY: char = 'p';
pub(crate) const PIXEL_X_OFFSET_KEY: char = 'X';
pub(crate) const PIXEL_Y_OFFSET_KEY: char = 'Y';
pub(crate) const RELATIVE_HORIZ_CELL_OFFSET_KEY: char = 'H';
pub(crate) const RELATIVE_VERT_CELL_OFFSET_KEY: char = 'V';

pub(crate) const DISPLAY_START_X_KEY: char = 'x';
pub(crate) const DISPLAY_START_Y_KEY: char = 'y';
pub(crate) const DISPLAY_WIDTH_SLICE_KEY: char = 'w';
pub(crate) const DISPLAY_HEIGHT_SLICE_KEY: char = 'h';

pub(crate) const DISPLAY_COLS_KEY: char = 'c';
pub(crate) const DISPLAY_ROWS_KEY: char = 'r';

pub(crate) const Z_INDEX_KEY: char = 'z';

pub(crate) const CURSOR_MOVEMENT_POLICY_KEY: char = 'C';
pub(crate) const VIRTUAL_PLACEMENT_KEY: char = 'U';
pub(crate) const PARENT_ID_KEY: char = 'P';
pub(crate) const PARENT_PLACEMENT_KEY: char = 'Q';

macro_rules! impl_encodable_for_int {
	($int:ty => $($key:expr,)+) => {
		$(impl<W: Write> Encodable<W, $key> for $int {
			const DEFAULT: Self = 0;
			fn write_value_to(&self, mut writer: W) -> std::io::Result<W> {
				write!(writer, "{self}").map(|()| writer)
			}
		})*
	}
}

impl_encodable_for_int!(i32 => Z_INDEX_KEY, RELATIVE_HORIZ_CELL_OFFSET_KEY, RELATIVE_VERT_CELL_OFFSET_KEY, );
impl_encodable_for_int!(u8 => REMAINING_CHUNKS_KEY,);
impl_encodable_for_int!(u16 => DISPLAY_COLS_KEY, DISPLAY_ROWS_KEY,);
impl_encodable_for_int!(
	u32 => SOURCE_WIDTH_KEY, SOURCE_HEIGHT_KEY, TRANSFER_ID_KEY, IMAGE_NUMBER_KEY,
		   PLACEMENT_ID_KEY, DISPLAY_START_X_KEY, DISPLAY_START_Y_KEY,
		   DISPLAY_WIDTH_SLICE_KEY, DISPLAY_HEIGHT_SLICE_KEY, PARENT_ID_KEY, PARENT_PLACEMENT_KEY,
);
impl_encodable_for_int!(
	usize => COMPRESSED_DATA_SIZE_KEY, READ_SIZE_KEY, READ_OFFSET_KEY, PIXEL_X_OFFSET_KEY, PIXEL_Y_OFFSET_KEY,
);

/// Which method of compression was used to compress the data being sent to the terminal
#[derive(PartialEq, Clone, Copy)]
pub enum Compression {
	/// [RFC 1950](https://datatracker.ietf.org/doc/html/rfc1950.html) zlib based deflate
	/// compression
	ZlibDeflate
}

impl<W: Write> Encodable<W, 'o'> for Option<Compression> {
	const DEFAULT: Self = None;
	fn write_value_to(&self, mut writer: W) -> std::io::Result<W> {
		let Some(compression) = self else {
			return Ok(writer);
		};

		match compression {
			Compression::ZlibDeflate => write!(writer, "z")?
		}

		Ok(writer)
	}
}

/// The different actions one can take to interact with a terminal which supports the kitty image
/// protocol. This is the main interaction point with the terminal - one should construct an
/// [`Action`] and [`Action::send`] it
pub enum Action<'image, 'data> {
	/// This simply sends the image data to the terminal, but does not display it. It also
	/// transfers ownership of the image data to the terminal - for example, if a temp file is used
	/// to send the data, that file will be deleted after it is transmitted to the terminal. Once a
	/// `Transmit` is successfully sent, one can then display the sent image with
	/// [`Action::Display`]
	Transmit(Image<'data>),
	/// Display an image which was already transmitted to (and is now owned by) the terminal
	Display {
		/// The image ID of the image which you want to display
		image_id: ImageId,
		/// A 'placement ID' for this display - see the documentation for [`PlacementId`] for more
		/// info. This must be sent when displaying something that was already transmitted, but is
		/// optional when transmitting and displaying in-one (e.g. with
		/// [`Self::TransmitAndDisplay`]
		placement_id: PlacementId,
		/// The details about exactly how this image should be displayed - the location, cursor
		/// movement, etc
		config: DisplayConfig
	},
	/// Transmit and then display an image. Should act effectively the same as calling
	/// [`Action::Transmit`] and then [`Action::Display`] with the returned image id.
	TransmitAndDisplay {
		/// The image which will be transferred to the terminal and then displayed immediately
		/// after
		image: Image<'data>,
		/// The details about exactly how this image should be displayed - the location, cursor
		/// movement, etc
		config: DisplayConfig,
		/// The placement ID for this display, if you'd like to use one. It is not necessary when
		/// transmitting and displaying in-one
		placement_id: Option<PlacementId>
	},
	/// Query the terminal to determine if a specific image can be transmitted & displayed. The
	/// following is quoted from the spec:
	///
	/// Since a client has no a-priori knowledge of whether it shares a filesystem/shared memory with the terminal emulator, it can send an id with the control data, using the i key (which can be an arbitrary positive integer up to 4294967295, it must not be zero). If it does so, the terminal emulator will reply after trying to load the image, saying whether loading was successful or not. For example:
	///
	/// ```text
	/// <ESC>_Gi=31,s=10,v=2,t=s;<encoded /some-shared-memory-name><ESC>\
	/// ```
	///
	/// to which the terminal emulator will reply (after trying to load the data):
	///
	/// ```text
	/// <ESC>_Gi=31;error message or OK<ESC>\
	/// ```
	///
	/// Here the i value will be the same as was sent by the client in the original request. The message data will be a ASCII encoded string containing only printable characters and spaces. The string will be OK if reading the pixel data succeeded or an error message.
	///
	/// Sometimes, using an id is not appropriate, for example, if you do not want to replace a previously sent image with the same id, or if you are sending a dummy image and do not want it stored by the terminal emulator. In that case, you can use the query action, set a=q. Then the terminal emulator will try to load the image and respond with either OK or an error, as above, but it will not replace an existing image with the same id, nor will it store the image.
	///
	/// We intend that any terminal emulator that wishes to support it can do so. To check if a terminal emulator supports the graphics protocol the best way is to send the above query action followed by a request for the [primary device attributes](https://vt100.net/docs/vt510-rm/DA1.html). If you get back an answer for the device attributes without getting back an answer for the query action the terminal emulator does not support the graphics protocol.
	///
	/// This means that terminal emulators that support the graphics protocol, must reply to query actions immediately without processing other input. Most terminal emulators handle input in a FIFO manner, anyway.
	///
	/// So for example, you could send:
	///
	/// ```text
	/// <ESC>_Gi=31,s=1,v=1,a=q,t=d,f=24;AAAA<ESC>\<ESC>[c
	/// ```
	///
	/// If you get back a response to the graphics query, the terminal emulator supports the protocol, if you get back a response to the device attributes query without a response to the graphics query, it does not.
	Query(&'image Image<'data>),
	/// Delete a specific set of images. See the [`DeleteConfig`] documentation for more details
	/// about how to use
	Delete(DeleteConfig),
	TransmitAnimationFrames,
	ControlAnimation,
	ComposeAnimationFrames
}

impl Action<'_, '_> {
	/// Write the transmit code for this [`Action`] to `writer` - this is the first part of
	/// [`Self::execute`] and only does a part of what is necessary to fully interact with a
	/// terminal. The full details can be found at [`Self::execute`].
	pub fn write_transmit_to<W: Write>(self, mut writer: W) -> std::io::Result<W> {
		write!(writer, "a=")?;

		let (mut writer, image_to_unlink) = match self {
			Self::Transmit(image) => {
				write!(writer, "t,")?;
				(image.write_transmit_to(writer, None)?, Some(image))
			}
			Self::TransmitAndDisplay {
				image,
				config,
				placement_id
			} => {
				write!(writer, "T,")?;
				writer = config.write_to(writer)?;
				(image.write_transmit_to(writer, placement_id)?, Some(image))
			}
			Self::Query(image) => {
				write!(writer, "q,")?;
				(image.write_transmit_to(writer, None)?, None)
			}
			Self::Display {
				image_id,
				placement_id,
				config
			} => {
				write!(writer, "p,i={image_id},p={placement_id},")?;
				(config.write_to(writer)?, None)
			},
			Self::Delete(del) => {
				write!(writer, "d")?;
				(del.write_to(writer)?, None)
			},
			_ => todo!()
		};

		writer.flush()?;

		if let Some(img) = image_to_unlink {
			img.unlink_if_shm();
		}

		Ok(writer)
	}

	fn extract_num_or_id_and_placement(&self) -> (NumberOrId, Option<PlacementId>) {
		match self {
			Self::Transmit(img) => (img.num_or_id, None),
			Self::TransmitAndDisplay {
				image,
				placement_id,
				..
			} => (image.num_or_id, *placement_id),
			Self::Query(img) => (img.num_or_id, None),
			Self::Display {
				image_id,
				placement_id,
				..
			} => (NumberOrId::Id(*image_id), Some(*placement_id)),
			_ => todo!()
		}
	}

	/// This is the main point of interaction with this library - to display an image, you need to
	/// create an [`Action`] and then call this function on it.
	///
	/// This function does two main things:
	/// 1. Writes the necessary escape codes to `writer`, then flushes it.
	/// 2. Unlinks the shared memory object if necessary (see [`Medium::SharedMemObject`])
	/// 3. Reads in the terminal's response via `reader`
	/// 4. Parses the terminal's response and returns any errors that occur or are transmitted
	///
	/// Steps 1 & 2 are performed simply by calling [`Self::write_transmit_to`], so this library
	/// can be used in a sans-io method by using that instead. ([TODO]: make the parse method pub
	/// in a more ergonomic API)
	///
	/// For this function to work correctly, `writer` should be writing directly to something that
	/// flushes directly to a kitty-supporting terminal. This function assumes that, once flushed
	/// to `writer`, the terminal will respond and this response can be read by `reader`.
	pub fn execute<W: Write, I: InputReader>(
		self,
		writer: W,
		reader: I
	) -> Result<(W, ImageId), TransmitError<I::Error>> {
		let (id_or_num, placement_id) = self.extract_num_or_id_and_placement();

		let writer = self.write_transmit_to(writer)?;

		let img_id = read_parse_response(reader, id_or_num, placement_id)?;
		Ok((writer, img_id))
	}

	/// An async version of [`Self::execute`] - check its documentation for more details
	pub async fn execute_async<W: Write, I: AsyncInputReader>(
		self,
		writer: W,
		reader: I
	) -> Result<(W, ImageId), TransmitError<I::Error>> {
		let (id_or_num, placement_id) = self.extract_num_or_id_and_placement();

		let writer = self.write_transmit_to(writer)?;

		let img_id = read_parse_response_async(reader, id_or_num, placement_id).await?;
		Ok((writer, img_id))
	}
}

/// A trait to facilitate reading from stdin in an async manner - the async version of
/// [`InputReader`]
pub trait AsyncInputReader {
	/// The error type that can occur while trying to read
	type Error: Error;
	/// Read from stdin into the provided buffer until you encounter the provided character or the
	/// provided timeout is hit. Timeout measuring does not need to be exact - it is simply used to
	/// ensure this method doesn't block forever.
	fn read_into_buf_until_char_with_timeout(
		&mut self,
		buf: &mut String,
		end: char,
		timeout: Duration
	) -> impl Future<Output = Result<(), Self::Error>>;
}

/// A trait to facilitate reading from stdin - the sync version of [`AsyncInputReader`]
pub trait InputReader {
	/// The error type that can occur while trying to read
	type Error: Error;
	/// Read from stdin into the provided buffer until you encounter the provided character. Since
	/// this is not async, it can block forever waiting for that character to come through
	fn read_into_buf_until_char(&mut self, buf: &mut String, end: char) -> Result<(), Self::Error>;
}

/// Used to either specify an Image ID or Image Number - see the details on [`IdentifierType`] for
/// explanations of how Image IDs vs Numbers are used.
#[derive(Clone, Copy)]
pub enum NumberOrId {
	/// An Image Number ([`IdentifierType::ImageNumber`])
	Number(NonZeroU32),
	/// An Image Id ([`IdentifierType::ImageId`])
	Id(NonZeroU32)
}

/// Sometimes when reading a response from the protocol, we expect the terminal to give us a value
/// back. Sometimes we expect that value to be something specific (e.g. `1`), and sometimes we
/// don't care what the value is - we just want something. This enum is used to distinguish between
/// the two
#[derive(PartialEq, Debug)]
pub enum AnyValueOrSpecific<T> {
	/// It could be any value - we don't really care what
	Any,
	/// We expect it to be this specific value
	Specific(T)
}

impl<T: Display> Display for AnyValueOrSpecific<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Any => write!(f, "<any value>"),
			Self::Specific(val) => write!(f, "{val}")
		}
	}
}

/// Multiple types of identifiers exist within the kitty protocol, and this enum just exists to
/// distinguish between them where they perform similar operations
#[derive(Debug, PartialEq)]
pub enum IdentifierType {
	/// A [`NonZeroU32`] relating to a specific image - image IDs can be used to correlate between
	/// an image transmitted to the terminal and an image later displayed on the terminal. Using
	/// the same image ID between multiple operations ensures that they correlate to the same image
	ImageId,
	/// The official protocol documentation explains this best. It states the following:
	///
	/// If you are writing a program that is going to share the screen with other programs and you still want to use image ids, it is not possible to know what image ids are free to use. In this case, instead of using the i key to specify an image id use the I key to specify an image number instead. These numbers are not unique. When creating a new image, even if an existing image has the same number a new one is created. And the terminal will reply with the id of the newly created image. For example, when creating an image with I=13, the terminal will send the response:
	///
	/// ```sh
	/// <ESC>_Gi=99,I=13;OK<ESC>\
	/// ```
	///
	/// Here, the value of `i` is the id for the newly created image and the value of `I` is the same as was sent in the creation command.
	///
	/// All future commands that refer to images using the image number, such as creating placements or deleting images, will act on only the newest image with that number. This allows the client program to send a bunch of commands dealing with an image by image number without waiting for a response from the terminal with the image id. Once such a response is received, the client program should use the i key with the image id for all future communication.
	///
	/// > Note:
	/// > Specifying both i and I keys in any command is an error. The terminal must reply with an EINVAL error message, unless silenced.
	ImageNumber,
	/// To quote the specification:
	///
	/// Since there can be many placements per image, you can also give placements an id. To do so add the p key with a number between 1 and 4294967295. When you specify a placement id, it will be added to the acknowledgement code above. Every placement is uniquely identified by the pair of the image id and the placement id. If you specify a placement id for an image that does not have an id (i.e. has id=0), it will be ignored. In particular this means there can exist multiple images with image id=0, placement id=0. Not specifying a placement id or using p=0 for multiple put commands (a=p) with the same non-zero image id results in multiple placements the image.
	///
	/// An example response:
	///
	/// ```sh
	/// <ESC>_Gi=<image id>,p=<placement id>;OK<ESC>\
	/// ```
	///
	/// If you send two placements with the same image id and placement id the second one will replace the first. This can be used to resize or move placements around the screen, without flicker.
	PlacementId
}

/// An Id that can be used to identify a specific placement of an image so that you can have a
/// single image displayed multiple times on the same surface. See [`IdentifierType::PlacementId`]
pub type PlacementId = NonZeroU32;

/// An Id that can be used to identify an image sent to the terminal - see
/// [`IdentifierType::ImageId`]
pub type ImageId = NonZeroU32;

/// A number that can be used to identify an image sent to the terminal - see
/// [`IdentifierType::ImageNumber`]
pub type ImageNumber = NonZeroU32;

pub(crate) fn write_b64<W: Write>(data: &[u8], writer: W) -> std::io::Result<W> {
	let mut b64 = Base64Encoder::new(writer, &STANDARD_NO_PAD);
	b64.write_all_allow_empty(data)?;
	b64.finish()
}

#[cfg(test)]
mod tests {
	use std::{
		convert::Infallible,
		hash::{DefaultHasher, Hasher},
		path::PathBuf,
		process::{Command, Stdio},
		time::{SystemTime, UNIX_EPOCH}
	};

	use ::image::ImageReader;
use nix::{sys::stat::Mode, unistd::mkfifo};

	use super::*;
	use crate::{
		error::{TerminalError, TransmitError},
		image::{Image, parse_response}
	};

	fn spawn_kitty_get_io(input: &[u8]) -> String {
		let mut hasher = DefaultHasher::new();
		hasher.write(input);
		let val = hasher.finish();

		let now = SystemTime::now()
			.duration_since(UNIX_EPOCH)
			.unwrap()
			.as_nanos()
			& u128::from(u16::MAX);

		let fifo_path = format!("/tmp/.kittyimg_test_pipe_{val}_{now}");
		_ = std::fs::remove_file(&fifo_path);
		mkfifo(
			fifo_path.as_str(),
			Mode::S_IRWXU | Mode::S_IRWXG | Mode::S_IRWXO
		)
		.unwrap();

		let mut script_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
		script_path.push("kitty_test.sh");

		let mut cmd = Command::new("kitty")
			.args(["--start-as", "hidden", "bash"])
			.arg(script_path)
			.env("KITTYIMG_PIPE", fifo_path.as_str())
			.stdout(Stdio::null())
			.stdin(Stdio::null())
			.stderr(Stdio::null())
			.spawn()
			.unwrap();

		std::fs::write(&fifo_path, input).unwrap();
		let s = std::fs::read_to_string(fifo_path).unwrap();

		let status = cmd.wait().unwrap();

		assert!(status.success());
		s
	}

	fn spawn_kitty_with_image(img: Image) -> Result<(), TransmitError<Infallible>> {
		let mut output = Vec::new();
		img.write_transmit_to(&mut output, None).unwrap();

		println!(
			"here's output: {}",
			str::from_utf8(&output).unwrap().replace('\x1b', "\\e")
		);

		let response = spawn_kitty_get_io(&output);

		let num_or_id = img.num_or_id;
		parse_response(response, num_or_id, None).map_or_else(
			|e| Err(TransmitError::ParsingResponse(e)),
			|res| res.map_err(TransmitError::Terminal)
		)?;
		img.unlink_if_shm();
		Ok(())
	}

	fn png_path() -> Box<Path> {
		let mut manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
		manifest_dir.push("kitty.png");
		manifest_dir.into()
	}

	#[tokio::test]
	async fn basic_functionality() {
		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgb24(
				ImageDimensions {
					width: 64,
					height: 64
				},
				None
			),
			medium: Medium::File(png_path())
		};

		spawn_kitty_with_image(img).unwrap();
	}

	#[tokio::test]
	async fn correctly_fails() {
		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgb24(
				ImageDimensions {
					width: 100,
					height: 100
				},
				None
			),
			medium: Medium::Direct {
				data: (&[]).into(),
				chunk_size: None
			}
		};

		let err = spawn_kitty_with_image(img).unwrap_err();
		assert_eq!(
			err,
			TransmitError::Terminal(TerminalError::NoData("Insufficient image data".into()))
		);
	}

	#[tokio::test]
	async fn direct_unchunked_rgb24_succeeds() {
		let img_data = ImageReader::open(png_path()).unwrap().decode().unwrap().to_rgb8();
		dbg!(&img_data);

		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgb24(
				ImageDimensions {
					width: img_data.width(),
					height: img_data.height()
				},
				None
			),
			medium: Medium::Direct {
				data: img_data.as_raw().into(),
				chunk_size: None
			}
		};

		spawn_kitty_with_image(img).unwrap();
	}

	#[tokio::test]
	async fn direct_chunked_rgb24_succeeds() {
		let img_data = ImageReader::open(png_path()).unwrap().decode().unwrap().to_rgb8();

		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgb24(
				ImageDimensions {
					width: img_data.width(),
					height: img_data.height()
				},
				None
			),
			medium: Medium::Direct {
				data: img_data.as_raw().into(),
				chunk_size: Some(ChunkSize::new(NonZeroU16::new(32).unwrap()).unwrap())
			}
		};

		spawn_kitty_with_image(img).unwrap();
	}

	#[tokio::test]
	async fn direct_unchunked_rgba32_succeeds() {
		let img_data = ImageReader::open(png_path()).unwrap().decode().unwrap().to_rgba8();

		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgba32(
				ImageDimensions {
					width: img_data.width(),
					height: img_data.height()
				},
				None
			),
			medium: Medium::Direct {
				data: img_data.as_raw().into(),
				chunk_size: None
			}
		};

		spawn_kitty_with_image(img).unwrap();
	}

	#[tokio::test]
	async fn direct_chunked_rgba32_succeeds() {
		let img_data = ImageReader::open(png_path()).unwrap().decode().unwrap().to_rgba8();

		let img = Image {
			num_or_id: NumberOrId::Id(NonZeroU32::new(1).unwrap()),
			format: PixelFormat::Rgba32(
				ImageDimensions {
					width: img_data.width(),
					height: img_data.height()
				},
				None
			),
			medium: Medium::Direct {
				data: img_data.as_raw().into(),
				chunk_size: Some(ChunkSize::new(NonZeroU16::new(132).unwrap()).unwrap())
			}
		};

		spawn_kitty_with_image(img).unwrap();
	}
}
