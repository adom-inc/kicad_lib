use kicad_sexpr::Sexpr;

use crate::{
    common::{LayerId, Uuid, Vec2D},
    convert::{FromSexpr, Parser, SerializationContext, ToSexpr},
    KiCadParseError,
};

use super::text::PcbText;

/// The `dimension` token defines a dimension object.
///
/// https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_dimension
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbDimension {
    /// The optional `locked` token specifies if the dimension can be moved.
    pub locked: bool,
    /// The `type` token attribute defines the type of dimension. Valid
    /// dimension types are `aligned`, `leader`, `center`, `orthogonal`, and
    /// `radial` (`radial` from version 7).
    pub kind: DimensionKind,
    /// The `layer` token defines the canonical layer the polygon resides on.
    pub layer: LayerId,
    /// The `tstamp` token defines the unique identifier of the dimension object.
    pub tstamp: Uuid,
    /// The `pts` token attributes define the list of `xy` coordinates of the
    /// dimension.
    pub points: [Vec2D; 2],
    /// The optional `height` token attribute defines the height of aligned
    /// dimensions.
    pub height: Option<f32>,
    /// The optional `orientation` token attribute defines the rotation angle
    /// for orthogonal dimensions.
    pub orientation: Option<f32>,
    /// The optional `leader_length` token attribute defines the distance from
    /// the marked radius to the knee for radial dimensions.
    pub leader_length: Option<f32>,
    /// The optional `gr_text` token attributes define the dimension text
    /// formatting for all dimension types except center dimensions.
    pub text: Option<PcbText>,
    /// The optional `format` token attributes define the dimension formatting
    /// for all dimension types except center dimensions.
    pub format: Option<DimensionFormat>,
    /// The `style` token attributes define the dimension style information.
    pub style: DimensionStyle,
}

impl FromSexpr for PcbDimension {
    fn from_sexpr(_parser: Parser) -> Result<Self, KiCadParseError> {
        todo!("impl FromSexpr for PcbDimension")
    }
}

impl ToSexpr for PcbDimension {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        todo!("impl ToSexpr for PcbDimension")
    }
}

/// See `kind` field in [`Dimension`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DimensionKind {
    Aligned,
    Leader,
    Center,
    Orthogonal,
    Radial,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct DimensionFormat {
    /// The optional `prefix` token attribute defines the string to add to the
    /// beginning of the dimension text.
    pub prefix: Option<String>,
    /// The optional `suffix` token attribute defines the string to add to the
    /// end of the dimension text.
    pub suffix: Option<String>,
    /// The units token attribute defines the dimension units used to display
    /// the dimension text. Valid units are as follows:
    ///   0 - Inches.
    ///   1 - Mils.
    ///   2 - Millimeters.
    ///   3 - Automatic.
    pub units: DimensionUnits,
    /// The units_format token attribute defines how the unit’s suffix is
    /// formatted. Valid units formats are as follows:
    ///   0 - No suffix.
    ///   1 - Bare suffix.
    ///   2 - Wrap suffix in parenthesis.
    pub units_format: DimensionUnitsFormat,
    /// The precision token attribute defines the number of significant digits
    /// to display. From version 7, a precision above 5 indicates a
    /// units-scaled precision:
    ///   6 - 0.00 in / 0 mils / 0.0 mm
    ///   7 - 0.000 in / 0 mils / 0.00 mm
    ///   8 - 0.0000 in / 0.0 mils / 0.000mm
    ///   9 - 0.00000 in / 0.00 mils / 0.0000mm
    pub precision: u8,
    /// The optional `override_value` token attribute defines the text to
    /// substitute for the actual physical dimension.
    pub override_value: Option<String>,
    /// The optional `suppress_zeros` token removes all trailing zeros from the
    /// dimension text.
    pub suppress_zeros: bool,
}

/// See `units` field in [`DimensionUnits`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum DimensionUnits {
    Inches = 0,
    /// Thousandths of an inch
    Mils = 1,
    Millimeters = 2,
    Automatic = 3,
}

/// See `units_format` field in [`DimensionFormat`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum DimensionUnitsFormat {
    NoSuffix = 0,
    BareSuffix = 1,
    WrapSuffixInParenthesis = 2,
}

/// https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_dimension_style
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct DimensionStyle {
    /// The `thickness` token attribute defines the line thickness of the
    /// dimension.
    pub thickness: f32,
    /// The `arrow_length` token attribute defines the length of the dimension
    /// arrows.
    pub arrow_length: f32,
    /// The text_position_mode token attribute defines the position mode of the
    /// dimension text. Valid position modes are as follows:
    ///   0 - Text is outside the dimension line.
    ///   1 - Text is in line with the dimension line.
    ///   2 - Text has been manually placed by the user.
    pub text_position_mode: DimensionTextPositionMode,
    /// The optional `extension_height` token attribute defines the length of
    /// the extension lines past the dimension crossbar.
    pub extension_height: Option<f32>,
    /// The optional text_frame token attribute defines the style of the frame
    /// around the dimension text. This only applies to leader dimensions.
    /// Valid text frames are as follows:
    ///   0 - No text frame.
    ///   1 - Rectangle.
    ///   2 - Circle.
    ///   3 - Rounded rectangle.
    pub text_frame: Option<TextFrameKind>,
    /// The optional `extension_offset` token attribute defines the distance
    /// from feature points to extension line start.
    pub extension_offset: Option<f32>,
    /// The optional keep_text_aligned token indicates that the dimension text
    /// should be kept in line with the dimension crossbar. When not defined,
    /// the dimension text is shown horizontally regardless of the orientation
    /// of the dimension.
    pub keep_text_aligned: bool,
}

/// See `text_position_mode` field in [`DimensionStyle`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum DimensionTextPositionMode {
    Outside = 0,
    Inline = 1,
    Manual = 2,
}

/// See `text_frame` field in [`DimensionStyle`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum TextFrameKind {
    NoFrame = 0,
    Rectangle = 1,
    Circle = 2,
    RoundedRectangle = 3,
}
