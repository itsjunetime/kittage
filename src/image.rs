//! The [`Image`] struct and associated functions to control how it is transmitted to the terminal

use std::{io::Write, num::NonZeroU32, str::Split, time::Duration};

use crate::{
	AnyValueOrSpecific, AsyncInputReader, Encodable, IMAGE_NUMBER_KEY, IdentifierType, ImageId,
	InputReader, NumberOrId, PLACEMENT_ID_KEY, PixelFormat, PlacementId, TRANSFER_ID_KEY,
	VERBOSITY_LEVEL_KEY, Verbosity, WriteUint,
	display::DisplayConfig,
	error::{ParseError, TerminalError, TransmitError},
	medium::Medium
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
		display_config: Option<&DisplayConfig>,
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

		if let Some(config) = display_config {
			writer = config.write_to(writer)?;
		}

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
				chunk_size: Some(crate::medium::ChunkSize::default()),
				data: data.into()
			}
		}
	}
}

#[cfg(feature = "image-crate")]
impl<'data> Image<'data> {
	/// Create an [`Image`] from the given [`image::DynamicImage`] and name. The given name will be
	/// passed to [`shm_open`] and then given to kitty for it to take ownership of.
	///
	/// The image returned from this will contain a [`Medium::SharedMemObject`].
	///
	/// If you can use shared memory objects, this provides much better performance than just
	/// calling `image.into()`.
	///
	/// The returned [`memmap2::MmapMut`] must be kept around until this is successfully sent to
	/// the terminal, and then it must be dropped to avoid a memory leak (as dropping it will unmap
	/// the memory that was sent to the terminal, which is fine as the terminal should've already
	/// copied the memory into its own storage).
	///
	/// [`shm_open`]: https://www.man7.org/linux/man-pages/man3/shm_open.3.html
	#[cfg(unix)]
	pub fn shm_from(image: ::image::DynamicImage, name: &str) -> std::io::Result<Self> {
		use crate::medium::SharedMemObject;

		let (format, data) = Image::fmt_and_data_from(image);

		let mut obj = SharedMemObject::create_new(name, data.len())?;
		obj.copy_in_buf(&data)?;

		Ok(Self {
			num_or_id: NumberOrId::Number(NonZeroU32::new(1).unwrap()),
			format,
			medium: Medium::SharedMemObject(obj)
		})
	}

	/// Pull the format and data (of that format) from a specific image. Used to convert a given
	/// [`image::DynamicImage`] into an [`Image`]
	pub fn fmt_and_data_from(image: ::image::DynamicImage) -> (PixelFormat, Vec<u8>) {
		use ::image::DynamicImage::*;

		let (width, height) = (image.width(), image.height());
		let dim = crate::ImageDimensions { width, height };
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
) -> Result<ImageId, TransmitError<I::Error>> {
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
) -> Result<ImageId, TransmitError<I::Error>> {
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
