use kicad_sexpr::Sexpr;

use crate::{
    common::{CoordinatePointList, LayerId, Uuid, Vec2D},
    convert::{FromSexpr, Parser, ToSexpr},
    simple_to_from_string, KiCadParseError,
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
    /// The `uuid` token defines the unique identifier of the dimension object.
    pub uuid: Uuid,
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
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("dimension")?;

        let kind = parser.expect_symbol_with_name("type")?.parse()?;
        let locked = parser.maybe_bool_with_name("locked")?;
        let layer = parser.expect_string_with_name("layer")?.parse()?;
        let uuid = parser.expect::<Uuid>()?;
        let points = parser.expect::<CoordinatePointList>().and_then(|v| {
            v.try_into()
                .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                    expected: 2,
                    found: v.len(),
                })
        })?;
        let height = (kind == DimensionKind::Aligned)
            .then(|| parser.expect_number_with_name("height"))
            .transpose()?;
        let leader_length = (kind == DimensionKind::Radial)
            .then(|| parser.expect_number_with_name("leader_length"))
            .transpose()?;
        let orientation = (kind == DimensionKind::Orthogonal)
            .then(|| parser.expect_number_with_name("orientation"))
            .transpose()?;
        let (text, format) = (kind != DimensionKind::Center)
            .then(|| {
                let text = parser.expect::<PcbText>()?;
                let format = parser.expect::<DimensionFormat>()?;

                Ok::<_, KiCadParseError>((Some(text), Some(format)))
            })
            .transpose()?
            .unwrap_or((None, None));
        let style = parser.expect::<DimensionStyle>()?;

        parser.expect_end()?;

        Ok(Self {
            kind,
            locked,
            layer,
            uuid,
            points,
            height,
            orientation,
            leader_length,
            text,
            format,
            style,
        })
    }
}

impl ToSexpr for PcbDimension {
    fn to_sexpr(&self) -> Sexpr {
        Sexpr::list_with_name(
            "dimension",
            [
                Some(Sexpr::symbol_with_name("type", self.kind)),
                self.locked.then(|| Sexpr::bool_with_name("locked", true)),
                Some(Sexpr::string_with_name("layer", self.layer)),
                Some(self.uuid.to_sexpr()),
                Some(self.points.to_vec().to_sexpr()),
                self.height.map(|h| Sexpr::number_with_name("height", h)),
                self.leader_length
                    .map(|l| Sexpr::number_with_name("leader_length", l)),
                self.orientation
                    .map(|o| Sexpr::number_with_name("orientation", o)),
                self.text.as_ref().map(ToSexpr::to_sexpr),
                self.format.as_ref().map(ToSexpr::to_sexpr),
                Some(self.style.to_sexpr()),
            ],
        )
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

simple_to_from_string! {
    DimensionKind,
    aligned <-> Aligned,
    leader <-> Leader,
    center <-> Center,
    orthogonal <-> Orthogonal,
    radial <-> Radial,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct DimensionFormat {
    /// The `prefix` token attribute defines the string to add to the
    /// beginning of the dimension text.
    pub prefix: String,
    /// The optional `suffix` token attribute defines the string to add to the
    /// end of the dimension text.
    pub suffix: String,
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
    /// The optional `suppress_zeroes` token removes all trailing zeros from the
    /// dimension text.
    pub suppress_zeroes: bool,
}

impl FromSexpr for DimensionFormat {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("format")?;

        let prefix = parser.expect_string_with_name("prefix")?;
        let suffix = parser.expect_string_with_name("suffix")?;
        let units = parser
            .expect_number_with_name("units")
            .map(|n| n as u8)
            .map(DimensionUnits::try_from)??;
        let units_format = parser
            .expect_number_with_name("units_format")
            .map(|n| n as u8)
            .map(DimensionUnitsFormat::try_from)??;
        let precision = parser
            .expect_number_with_name("precision")
            .map(|n| n as u8)?;
        let override_value = parser.maybe_string_with_name("override_value")?;
        let suppress_zeroes = parser.maybe_symbol_matching("suppress_zeroes");

        parser.expect_end()?;

        Ok(Self {
            prefix,
            suffix,
            units,
            units_format,
            precision,
            override_value,
            suppress_zeroes,
        })
    }
}

impl ToSexpr for DimensionFormat {
    fn to_sexpr(&self) -> Sexpr {
        Sexpr::list_with_name(
            "format",
            [
                Some(Sexpr::string_with_name("prefix", &self.prefix)),
                Some(Sexpr::string_with_name("suffix", &self.suffix)),
                Some(Sexpr::number_with_name("units", self.units as u8 as f32)),
                Some(Sexpr::number_with_name(
                    "units_format",
                    self.units_format as u8 as f32,
                )),
                Some(Sexpr::number_with_name("precision", self.precision as f32)),
                self.override_value
                    .as_ref()
                    .map(|o| Sexpr::string_with_name("override_value", o)),
                self.suppress_zeroes
                    .then(|| Sexpr::symbol("suppress_zeroes")),
            ],
        )
    }
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

impl TryFrom<u8> for DimensionUnits {
    type Error = KiCadParseError;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        Ok(match n {
            0 => Self::Inches,
            1 => Self::Mils,
            2 => Self::Millimeters,
            3 => Self::Automatic,
            _ => return Err(KiCadParseError::invalid_enum_value::<Self>(n.to_string())),
        })
    }
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

impl TryFrom<u8> for DimensionUnitsFormat {
    type Error = KiCadParseError;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        Ok(match n {
            0 => Self::NoSuffix,
            1 => Self::BareSuffix,
            2 => Self::WrapSuffixInParenthesis,
            _ => return Err(KiCadParseError::invalid_enum_value::<Self>(n.to_string())),
        })
    }
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
    /// The `extension_offset` token attribute defines the distance
    /// from feature points to extension line start.
    pub extension_offset: f32,
    /// The optional keep_text_aligned token indicates that the dimension text
    /// should be kept in line with the dimension crossbar. When not defined,
    /// the dimension text is shown horizontally regardless of the orientation
    /// of the dimension.
    pub keep_text_aligned: bool,
}

impl FromSexpr for DimensionStyle {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("style")?;

        let thickness = parser.expect_number_with_name("thickness")?;
        let arrow_length = parser.expect_number_with_name("arrow_length")?;
        let text_position_mode = parser
            .expect_number_with_name("text_position_mode")
            .map(|n| n as u8)
            .map(DimensionTextPositionMode::try_from)??;
        let extension_height = parser.maybe_number_with_name("extension_height")?;
        let text_frame = parser
            .maybe_number_with_name("text_frame")?
            .map(|n| n as u8)
            .map(TextFrameKind::try_from)
            .transpose()?;
        let extension_offset = parser.expect_number_with_name("extension_offset")?;
        let keep_text_aligned = parser.maybe_symbol_matching("keep_text_aligned");

        parser.expect_end()?;

        Ok(Self {
            thickness,
            arrow_length,
            text_position_mode,
            extension_height,
            text_frame,
            extension_offset,
            keep_text_aligned,
        })
    }
}

impl ToSexpr for DimensionStyle {
    fn to_sexpr(&self) -> Sexpr {
        Sexpr::list_with_name(
            "style",
            [
                Some(Sexpr::number_with_name("thickness", self.thickness)),
                Some(Sexpr::number_with_name("arrow_length", self.arrow_length)),
                Some(Sexpr::number_with_name(
                    "text_position_mode",
                    self.text_position_mode as u8 as f32,
                )),
                self.extension_height
                    .map(|e| Sexpr::number_with_name("extension_height", e)),
                self.text_frame
                    .map(|t| Sexpr::number_with_name("text_frame", t as u8 as f32)),
                Some(Sexpr::number_with_name(
                    "extension_offset",
                    self.extension_offset,
                )),
                self.keep_text_aligned
                    .then(|| Sexpr::symbol("keep_text_aligned")),
            ],
        )
    }
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

impl TryFrom<u8> for DimensionTextPositionMode {
    type Error = KiCadParseError;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        Ok(match n {
            0 => Self::Outside,
            1 => Self::Inline,
            2 => Self::Manual,
            _ => return Err(KiCadParseError::invalid_enum_value::<Self>(n.to_string())),
        })
    }
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

impl TryFrom<u8> for TextFrameKind {
    type Error = KiCadParseError;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        Ok(match n {
            0 => Self::NoFrame,
            1 => Self::Rectangle,
            2 => Self::Circle,
            3 => Self::RoundedRectangle,
            _ => return Err(KiCadParseError::invalid_enum_value::<Self>(n.to_string())),
        })
    }
}
