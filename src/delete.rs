use std::{io::Write, num::NonZeroU32, ops::RangeInclusive};

use crate::{ImageId, PlacementId};

#[derive(PartialEq, Debug, Clone)]
pub struct DeleteConfig {
	effect: ClearOrDelete,
	which: WhichToDelete
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ClearOrDelete {
	Clear,
	Delete
}

impl ClearOrDelete {
	fn maybe_upper(self, c: char) -> char {
		match self {
			Self::Delete => c.to_ascii_uppercase(),
			Self::Clear => c
		}
	}
}

#[derive(PartialEq, Debug, Clone)]
struct CellLocation {
	x: u16,
	y: u16
}

#[derive(PartialEq, Debug, Clone)]
struct CellLocationZ {
	x: u16,
	y: u16,
	z: i32
}

#[derive(PartialEq, Debug, Clone)]
pub enum WhichToDelete {
	All,
	ImageId(ImageId, Option<PlacementId>),
	NewestWithNumber(ImageId, Option<PlacementId>),
	IntersectingWithCursor,
	AnimationFrames,
	PlacementsIntersectingCell(CellLocation),
	PlacementsIntersectingCellWithZ(CellLocationZ),
	IdRange(RangeInclusive<NonZeroU32>),
	PlacementsIntersectingColumn(u16),
	PlacementsIntersectingRow(u16),
	PlacementsWithZIndex(i32)
}

impl DeleteConfig {
	fn write_to<W: Write>(&self, mut w: W) -> std::io::Result<W> {
		let e = self.effect;

		write!(w, ",d=")?;

		match &self.which {
			WhichToDelete::All => write!(w, "{}", e.maybe_upper('a'))?,
			WhichToDelete::ImageId(img_id, placement_id) => {
				write!(w, "{},i={img_id}", e.maybe_upper('i'))?;
				if let Some(p_id) = placement_id {
					write!(w, ",p={p_id}")?;
				}
			}
			WhichToDelete::NewestWithNumber(img_num, placement_id) => {
				write!(w, "{},I={img_num}", e.maybe_upper('n'))?;
				if let Some(p_id) = placement_id {
					write!(w, ",p={p_id}")?;
				}
			}
			WhichToDelete::IntersectingWithCursor => write!(w, "{}", e.maybe_upper('c'))?,
			WhichToDelete::AnimationFrames => write!(w, "{}", e.maybe_upper('f'))?,
			WhichToDelete::PlacementsIntersectingCell(CellLocation { x, y }) =>
				write!(w, "{},x={x},y={y}", e.maybe_upper('p'))?,
			WhichToDelete::PlacementsIntersectingCellWithZ(CellLocationZ { x, y, z }) =>
				write!(w, "{},x={x},y={y},z={z}", e.maybe_upper('q'))?,
			WhichToDelete::IdRange(range) => write!(
				w,
				"{},x={},y={}",
				e.maybe_upper('r'),
				range.start(),
				range.end()
			)?,
			WhichToDelete::PlacementsIntersectingColumn(col) =>
				write!(w, "{},x={col}", e.maybe_upper('x'))?,
			WhichToDelete::PlacementsIntersectingRow(row) =>
				write!(w, "{},y={row}", e.maybe_upper('y'))?,
			WhichToDelete::PlacementsWithZIndex(z) => write!(w, "{},z={z}", e.maybe_upper('z'))?
		}

		Ok(w)
	}
}
