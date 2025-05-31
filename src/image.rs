//! The [`Image`] struct and associated functions to control how it is transmitted to the terminal

use std::{borrow::Cow, io::Write, num::NonZeroU32, str::Split, time::Duration};

use image::DynamicImage;

use crate::{
	AnyValueOrSpecific, AsyncInputReader, Encodable, IMAGE_NUMBER_KEY, IdentifierType,
	ImageDimensions, ImageId, InputReader, NumberOrId, PLACEMENT_ID_KEY, PixelFormat, PlacementId,
	TRANSFER_ID_KEY, VERBOSITY_LEVEL_KEY, Verbosity, WriteUint,
	error::{ParseError, TerminalError, TransmitError},
	medium::{ChunkSize, Medium}
};

/// The data necessary to transmit or query or display (etc) an image on/to a receiving terminal
#[derive(Debug, PartialEq)]
pub struct Image<'data> {
	/// The number or ID that should be sent along with the transmission - see the [`NumberOrId`]
	/// documentation for what that means
	pub num_or_id: NumberOrId,
	/// The format that the sent data is stored in - see [`PixelFormat`]'s documentation for more
	/// details
	pub format: PixelFormat,
	/// The medium by which the data is sent (e.g. it's on disk, or stored in a in-memory buffer,
	/// or sent over stdout)
	pub medium: Medium<'data>
}

impl Image<'_> {
	pub(crate) fn write_transmit_to<W: Write>(
		&self,
		mut writer: W,
		placement_id: Option<NonZeroU32>,
		verbosity: Verbosity
	) -> std::io::Result<W> {
		match self.num_or_id {
			NumberOrId::Id(id) => write!(writer, "{TRANSFER_ID_KEY}={id}"),
			NumberOrId::Number(num) => write!(writer, "{IMAGE_NUMBER_KEY}={num}")
		}?;

		if let Some(p_id) = placement_id {
			writer = writer.write_uint::<PLACEMENT_ID_KEY, _>(p_id.get())?;
		}

		writer = writer.write_uint::<VERBOSITY_LEVEL_KEY, _>(verbosity as u8)?;

		// Write all format data, up to (and including) the ';'
		writer = self.format.write_kv_encoded(writer)?;
		// Then write the data itself, in its format
		writer = self.medium.write_data(writer)?;
		write!(writer, "\x1b\\")?;
		writer.flush()?;
		Ok(writer)
	}
}

#[cfg(feature = "image-crate")]
impl From<::image::DynamicImage> for Image<'static> {
	fn from(value: ::image::DynamicImage) -> Self {
		let (format, data) = Image::fmt_and_data_from(value);

		Self {
			num_or_id: NumberOrId::Number(NonZeroU32::new(1).unwrap()),
			format,
			medium: Medium::Direct {
				chunk_size: Some(ChunkSize::default()),
				data: data.into()
			}
		}
	}
}

#[cfg(feature = "image-crate")]
#[derive(thiserror::Error, Debug)]
pub enum FromShmErr {
	#[error("Couldn't open shm: {0}")]
	ShmOpen(std::io::Error),
	#[error("Couldn't set shm's size: {0}")]
	SetSize(std::io::Error),
	#[error("Couldn't mmap shm: {0}")]
	ShmMap(std::io::Error)
}

#[cfg(feature = "image-crate")]
impl<'data> Image<'data> {
	#[cfg(unix)]
	pub fn shm_from(
		image: ::image::DynamicImage,
		name: Cow<'data, str>
	) -> Result<(Self, memmap2::MmapMut), FromShmErr> {
		use psx_shm::{BorrowedMap, OpenMode, OpenOptions};

		let (format, data) = Image::fmt_and_data_from(image);

		let oflags = OpenOptions::CREATE | OpenOptions::READWRITE;
		let mode = OpenMode::R_USR | OpenMode::W_USR;
		let mut shm = psx_shm::Shm::open(&name, oflags, mode).map_err(FromShmErr::ShmOpen)?;

		shm.set_size(data.len()).map_err(FromShmErr::SetSize)?;

		let mut map = unsafe { shm.map(0) }.map_err(FromShmErr::ShmMap)?;

		map.map().copy_from_slice(&data);

		// TODO: I'm cheating here
		let map = unsafe { std::mem::transmute::<BorrowedMap<'_>, memmap2::MmapMut>(map) };
		std::mem::forget(shm);

		Ok((
			Self {
				num_or_id: NumberOrId::Number(NonZeroU32::new(1).unwrap()),
				format,
				medium: Medium::SharedMemObject { name }
			},
			map
		))
	}

	pub fn fmt_and_data_from(image: ::image::DynamicImage) -> (PixelFormat, Vec<u8>) {
		use DynamicImage::*;

		let (width, height) = (image.width(), image.height());
		let dim = ImageDimensions { width, height };
		match image {
			ImageLuma8(_) | ImageRgb8(_) | ImageLuma16(_) | ImageRgb16(_) | ImageRgb32F(_) =>
				(PixelFormat::Rgb24(dim, None), image.into_rgb8().into_vec()),
			ImageLumaA8(_) | ImageRgba8(_) | ImageLumaA16(_) | ImageRgba16(_) | ImageRgba32F(_)
			| _ => (
				PixelFormat::Rgba32(dim, None),
				image.into_rgba8().into_vec()
			)
		}
	}
}

pub(crate) async fn read_parse_response_async<I: AsyncInputReader>(
	mut reader: I,
	image: NumberOrId,
	placement_id: Option<PlacementId>
) -> Result<ImageId, TransmitError<'static, 'static, I::Error>> {
	let mut output = String::with_capacity("\x1b_Gi=;OK\x1b\\".len() + 10);
	// Try to get the terminal's repsonse
	if let Err(e) = reader
		.read_esc_delimited_str_with_timeout(&mut output, Duration::from_millis(1000))
		.await
	{
		return Err(TransmitError::ReadingInput(e));
	}

	parse_response(output, image, placement_id).map_or_else(
		|e| Err(TransmitError::ParsingResponse(e)),
		|res| res.map_err(TransmitError::Terminal)
	)
}

pub(crate) fn read_parse_response<I: InputReader>(
	mut reader: I,
	image: NumberOrId,
	placement_id: Option<PlacementId>
) -> Result<ImageId, TransmitError<'static, 'static, I::Error>> {
	let mut output = String::with_capacity("\x1b_Gi=;OK\x1b\\".len() + 10);
	// Try to get the terminal's repsonse
	if let Err(e) = reader.read_esc_delimited_str(&mut output) {
		return Err(TransmitError::ReadingInput(e));
	}

	parse_response(output, image, placement_id).map_or_else(
		|e| Err(TransmitError::ParsingResponse(e)),
		|res| res.map_err(TransmitError::Terminal)
	)
}

pub(crate) fn parse_response(
	output: String,
	image: NumberOrId,
	placement_id: Option<ImageId>
) -> Result<Result<ImageId, TerminalError>, ParseError> {
	if !output.starts_with("_G") {
		return Err(ParseError::NoStartSequence(output));
	}

	let input = output.trim_start_matches("_G");
	let Some(semicolon_pos) = input.find(';') else {
		return Err(ParseError::NoFinalSemicolon);
	};

	let options = input[..semicolon_pos]
		.split(',')
		.filter(|s| !s.is_empty())
		.map(|s| s.split('='));

	let mut found_place_id = None;
	let mut found_image_num = None;
	let mut found_image_id = None;

	let image_num = match image {
		NumberOrId::Number(i) => Some(i),
		NumberOrId::Id(_) => None
	};

	for mut opt in options {
		#[inline(always)]
		fn check_next_id<'input>(
			i: &mut impl Iterator<Item = &'input str>,
			expected: Option<NonZeroU32>,
			ty: IdentifierType
		) -> Result<Option<NonZeroU32>, ParseError> {
			match (expected, i.next()) {
				// if the iterator gives us something that could be an id, that's cool - we now
				// need to try to parse it into an id
				(Some(expected), Some(found)) => {
					// if it does parse and is equal to what we expect, then we're all cool;
					// otherwise, return the error that we couldn't parse it,
					match found.parse::<NonZeroU32>() {
						Ok(i) if i == expected => Ok(Some(i)),
						_ => Err(ParseError::DifferentIdInResponse {
							ty,
							found: found.to_string(),
							expected: AnyValueOrSpecific::Specific(expected)
						})
					}
				}
				(Some(_), None) => Err(ParseError::NoResponseId { ty }),
				// If the type we're passing in is an ImageId, then we must have sent an image
				// number instead of an id (since you need to send one or the other to the
				// terminal), so it's fine to have an unexpected id here.
				(None, Some(s)) =>
					if ty == IdentifierType::ImageId {
						s.parse::<NonZeroU32>().map(Some).map_err(|_| {
							ParseError::DifferentIdInResponse {
								ty,
								found: s.to_string(),
								expected: AnyValueOrSpecific::Any
							}
						})
					} else {
						Err(ParseError::IdInResponseButNotInRequest {
							ty,
							value: s.to_string()
						})
					},
				(None, None) => Ok(None) // Cool, all good
			}
		}

		// The 'split' iterator always returns at least one entry - I'm doing fully-qualified
		// to ensure this stays a `Split`
		match <Split<'_, _> as Iterator>::next(&mut opt).unwrap() {
			"i" => {
				// [todo] what should happen if they give us multiple instances of one ID? Just
				// error?
				found_image_id = check_next_id(
					&mut opt,
					match image {
						NumberOrId::Id(i) => Some(i),
						NumberOrId::Number(_) => None
					},
					IdentifierType::ImageId
				)?;
			}
			"p" => {
				found_place_id =
					check_next_id(&mut opt, placement_id, IdentifierType::PlacementId)?;
			}
			"I" => {
				found_image_num = check_next_id(&mut opt, image_num, IdentifierType::ImageNumber)?;
			}
			s => {
				return Err(ParseError::UnknownResponseKey(s.to_owned()));
			}
		}
	}

	let Some(found_image_id) = found_image_id else {
		let val = match image {
			NumberOrId::Id(i) => AnyValueOrSpecific::Specific(i),
			NumberOrId::Number(_) => AnyValueOrSpecific::Any
		};
		return Err(ParseError::MissingId {
			ty: IdentifierType::ImageId,
			val
		});
	};

	if let Some(place_id) = found_place_id.is_none().then_some(placement_id).flatten() {
		return Err(ParseError::MissingId {
			ty: IdentifierType::PlacementId,
			val: AnyValueOrSpecific::Specific(place_id)
		});
	}

	if let Some(image_num) = found_image_num.is_none().then_some(image_num).flatten() {
		return Err(ParseError::MissingId {
			ty: IdentifierType::ImageNumber,
			val: AnyValueOrSpecific::Specific(image_num)
		});
	}

	// this is chill - if the semicolon is the last thing in the string, then this'll just be
	// an empty string.
	let response = &input[semicolon_pos + 1..];

	if response == "OK" || response == "OK\n" {
		return Ok(Ok(found_image_id));
	}

	let mut split = response.split(':');
	let (Some(code), Some(reason)) = (split.next(), split.next()) else {
		return Err(ParseError::MalformedError(response.to_string()));
	};

	TerminalError::try_from((code, reason)).map_or_else(
		|e| {
			Err(ParseError::UnknownErrorCode {
				code: e.0.to_owned(),
				reason: reason.to_owned()
			})
		},
		|e| Ok(Err(e))
	)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn parse_good_responses() {
		#[allow(clippy::unnecessary_wraps)]
		fn id(i: u32) -> Result<Result<NonZeroU32, TerminalError>, ParseError> {
			Ok(Ok(NonZeroU32::new(i).unwrap()))
		}

		// just image id
		assert_eq!(
			parse_response(
				"_Gi=31;OK".into(),
				NumberOrId::Id(NonZeroU32::new(31).unwrap()),
				None
			),
			id(31)
		);

		// add image number
		assert_eq!(
			parse_response(
				"_Gi=1,I=12;OK".into(),
				NumberOrId::Number(NonZeroU32::new(12).unwrap()),
				None
			),
			id(1)
		);

		// add placement id
		assert_eq!(
			parse_response(
				"_Gi=1,p=2,I=12;OK".into(),
				NumberOrId::Number(NonZeroU32::new(12).unwrap()),
				Some(NonZeroU32::new(2).unwrap())
			),
			id(1)
		);

		// placement, but no image number
		assert_eq!(
			parse_response(
				"_Gi=987,p=2;OK".into(),
				NumberOrId::Id(NonZeroU32::new(987).unwrap()),
				Some(NonZeroU32::new(2).unwrap())
			),
			id(987)
		);

		// different order
		assert_eq!(
			parse_response(
				"_Gp=2,i=100,I=12;OK".into(),
				NumberOrId::Number(NonZeroU32::new(12).unwrap()),
				Some(NonZeroU32::new(2).unwrap())
			),
			id(100)
		);
	}

	fn simple(input: &'static str, err: ParseError) {
		assert_eq!(
			parse_response(
				input.into(),
				NumberOrId::Id(NonZeroU32::new(1).unwrap()),
				None
			),
			Err(err)
		);
	}

	#[test]
	fn parse_bad_responses() {
		fn nzu(u: u32) -> NonZeroU32 {
			NonZeroU32::new(u).unwrap()
		}

		let id = NumberOrId::Id(nzu(1));
		simple("", ParseError::NoStartSequence(String::new()));

		simple("Gi=1;OK", ParseError::NoStartSequence("Gi=1;OK".into()));

		simple("_Gi=1OK", ParseError::NoFinalSemicolon);

		simple("_Gi=1OK;", ParseError::DifferentIdInResponse {
			ty: IdentifierType::ImageId,
			found: "1OK".into(),
			expected: AnyValueOrSpecific::Specific(nzu(1))
		});

		simple("_Gi=;OK", ParseError::DifferentIdInResponse {
			ty: IdentifierType::ImageId,
			found: String::new(),
			expected: AnyValueOrSpecific::Specific(nzu(1))
		});

		simple("_Gi=2;OK", ParseError::DifferentIdInResponse {
			ty: IdentifierType::ImageId,
			found: "2".into(),
			expected: AnyValueOrSpecific::Specific(nzu(1))
		});

		simple("_Gi=1,p=4;OK", ParseError::IdInResponseButNotInRequest {
			ty: IdentifierType::PlacementId,
			value: "4".into()
		});

		simple("_Gp=4;OK", ParseError::IdInResponseButNotInRequest {
			ty: IdentifierType::PlacementId,
			value: "4".into()
		});

		simple("_Gi=1,I=0;OK", ParseError::IdInResponseButNotInRequest {
			ty: IdentifierType::ImageNumber,
			value: "0".into()
		});

		simple("_G;OK", ParseError::MissingId {
			ty: IdentifierType::ImageId,
			val: AnyValueOrSpecific::Specific(nzu(1))
		});

		assert_eq!(
			parse_response("_GI=2;OK".into(), NumberOrId::Number(nzu(2)), None),
			Err(ParseError::MissingId {
				ty: IdentifierType::ImageId,
				val: AnyValueOrSpecific::Any
			})
		);

		assert_eq!(
			parse_response("_Gi=2;OK".into(), NumberOrId::Number(nzu(2)), None),
			Err(ParseError::MissingId {
				ty: IdentifierType::ImageNumber,
				val: AnyValueOrSpecific::Specific(nzu(2))
			})
		);

		assert_eq!(
			parse_response("_Gi=1;OK".into(), id, Some(nzu(3))),
			Err(ParseError::MissingId {
				ty: IdentifierType::PlacementId,
				val: AnyValueOrSpecific::Specific(nzu(3))
			})
		);

		assert_eq!(
			parse_response(
				"_Gi=1,I=2;OK".into(),
				NumberOrId::Number(nzu(2)),
				Some(nzu(3))
			),
			Err(ParseError::MissingId {
				ty: IdentifierType::PlacementId,
				val: AnyValueOrSpecific::Specific(nzu(3))
			})
		);

		assert_eq!(
			parse_response(
				"_Gi=1,p=3;OK".into(),
				NumberOrId::Number(nzu(2)),
				Some(nzu(3))
			),
			Err(ParseError::MissingId {
				ty: IdentifierType::ImageNumber,
				val: AnyValueOrSpecific::Specific(nzu(2))
			})
		);

		simple("_Gt=0;OK", ParseError::UnknownResponseKey("t".into()));
		simple(
			"_Gi=1,meow=0;OK",
			ParseError::UnknownResponseKey("meow".into())
		);

		simple(
			"_Gi=1;You did something wrong",
			ParseError::MalformedError("You did something wrong".into())
		);
		simple("_Gi=1;EINVAL", ParseError::MalformedError("EINVAL".into()));

		simple("_Gi=1;EIDIOT:little idiot", ParseError::UnknownErrorCode {
			code: "EIDIOT".into(),
			reason: "little idiot".into()
		});
	}
}
