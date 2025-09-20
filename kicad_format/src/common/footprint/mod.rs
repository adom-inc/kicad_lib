//! Common structures related to footprints.
//!
//! Used both in footprint library files and board files.

use kicad_sexpr::{Sexpr, SexprList};

use self::{
    shape::FootprintShape,
    text::{FootprintText, FootprintTextBox},
};
use super::{
    pad::Pad, symbol::LibraryId, zone::Zone, Group, Image, LayerId, Position, Property, Uuid, Vec3D,
};
use crate::{
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName, VecToMaybeSexprVec,
    },
    footprint_library::FootprintLibraryFile,
    simple_maybe_from_sexpr, KiCadParseError, SexprKind,
};

pub mod shape;
pub mod text;

/// A footprint inlined within a PCB file.
///
/// TODO: move to pcb module
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintInlined {
    pub library_link: LibraryId,
    pub locked: bool,
    pub placed: bool,
    pub layer: LayerId,
    pub tstamp: Uuid,
    pub position: Position,
    pub description: Option<String>,
    pub tags: Option<String>,
    pub properties: Vec<Property>,
    pub path: Option<String>,
    pub solder_mask_margin: Option<f32>,
    pub solder_paste_margin: Option<f32>,
    pub solder_paste_ratio: Option<f32>,
    pub clearance: Option<f32>,
    pub zone_connect: Option<ZoneConnectKind>,
    pub attributes: Option<FootprintAttributes>,
    pub private_layers: Option<Vec<LayerId>>,
    pub net_tie_pad_groups: Option<Vec<Vec<String>>>,
    pub graphics_items: Vec<FootprintGraphicsItem>,
    pub pads: Vec<Pad>,
    pub keep_out_zones: Vec<Zone>,
    pub groups: Vec<Group>,
    pub models: Vec<Model>,
}

impl FootprintInlined {
    /// Updates and inlined footprint from a footprint library file.
    ///
    /// This function does not update the library link, locked, placed, tstamp,
    /// position, properties, path, or fp_text/fp_text_box fields.
    pub fn update_from_library(&mut self, footprint_library_file: &FootprintLibraryFile) {
        self.layer = footprint_library_file.layer;
        self.description = footprint_library_file.description.clone();
        self.tags = footprint_library_file.tags.clone();
        self.solder_mask_margin = footprint_library_file.solder_mask_margin;
        self.solder_paste_margin = footprint_library_file.solder_paste_margin;
        self.solder_paste_ratio = footprint_library_file.solder_paste_ratio;
        self.clearance = footprint_library_file.clearance;
        self.zone_connect = footprint_library_file.zone_connect;
        self.attributes = footprint_library_file.attributes.clone();
        self.private_layers = footprint_library_file.private_layers.clone();
        self.net_tie_pad_groups = footprint_library_file.net_tie_pad_groups.clone();

        // Remove shapes and images
        self.graphics_items.retain(|g| {
            matches!(
                g,
                FootprintGraphicsItem::Text(_) | FootprintGraphicsItem::TextBox(_)
            )
        });
        // Add shapes and images from the footprint library file
        self.graphics_items.extend(
            footprint_library_file
                .graphics_items
                .iter()
                .filter(|g| {
                    matches!(
                        g,
                        FootprintGraphicsItem::Shape(_) | FootprintGraphicsItem::Image(_)
                    )
                })
                .cloned(),
        );

        // Update pads by trying to match the new pads with the existing pads
        //
        // Implemented from the algorithm used in the KiCad source code:
        // https://gitlab.com/kicad/code/kicad/-/blob/7.0.10/pcbnew/pcb_edit_frame.cpp#L2167
        let mut new_pads = footprint_library_file.pads.clone();

        for pad in &mut new_pads {
            if !pad.is_on_copper_layer() || pad.index.is_empty() {
                pad.net = None;
                continue;
            }

            let mut pad_model: Option<&Pad>;
            let mut last_pad = None;

            loop {
                pad_model = self.find_pad_by_number(&pad.index, last_pad);

                let Some(pad_model) = pad_model else {
                    break;
                };

                // a candidate is found
                if pad_model.is_on_copper_layer() {
                    break;
                }

                last_pad = Some(pad_model);
            }

            if let Some(pad_model) = pad_model {
                pad.pin_function = pad_model.pin_function.clone();
                pad.pin_type = pad_model.pin_type.clone();
            }

            pad.net = pad_model.and_then(|p| p.net.clone());
        }

        self.pads = new_pads;
        self.keep_out_zones = footprint_library_file.keep_out_zones.clone();
        self.groups = footprint_library_file.groups.clone();
        self.models = footprint_library_file.models.clone();
    }

    pub fn find_pad_by_number(&self, pad_number: &str, search_after: Option<&Pad>) -> Option<&Pad> {
        let start_index = search_after
            .and_then(|p| self.pads.iter().position(|pad| pad == p))
            .unwrap_or(0);

        self.pads[start_index..]
            .iter()
            .find(|p| p.index == pad_number)
    }
}

impl FromSexpr for FootprintInlined {
    fn from_sexpr(mut parser: Parser) -> Result<Self, crate::KiCadParseError> {
        parser.expect_symbol_matching("footprint")?;

        let library_link = parser.expect_string()?.parse::<LibraryId>()?;
        let locked = parser.maybe_symbol_matching("locked");
        let placed = parser.maybe_symbol_matching("placed");
        let layer = parser.expect_string_with_name("layer")?.parse()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;
        let position = parser.expect::<Position>()?;
        let description = parser.maybe_string_with_name("descr")?;
        let tags = parser.maybe_string_with_name("tags")?;
        let properties = parser.expect_many::<Property>()?;
        let path = parser.maybe_string_with_name("path")?;
        let solder_mask_margin = parser.maybe_number_with_name("solder_mask_margin")?;
        let solder_paste_margin = parser.maybe_number_with_name("solder_paste_margin")?;
        let solder_paste_ratio = parser.maybe_number_with_name("solder_paste_ratio")?;
        let clearance = parser.maybe_number_with_name("clearance")?;
        let zone_connect = parser
            .maybe_number_with_name("zone_connect")?
            .map(|n| n as u8)
            .map(ZoneConnectKind::try_from)
            .transpose()?;
        let attributes = parser.maybe::<FootprintAttributes>()?;
        let private_layers = parser
            .maybe_list_with_name("private_layers")
            .map(|mut a| {
                a.expect_many_strings()?
                    .into_iter()
                    .map(|s| s.parse())
                    .collect()
            })
            .transpose()?;
        let net_tie_pad_groups = parser
            .maybe_list_with_name("net_tie_pad_groups")
            .map(|mut a| {
                Ok::<_, KiCadParseError>(
                    a.expect_many_strings()?
                        .into_iter()
                        .map(|s| s.split(',').map(|s| s.to_owned()).collect::<Vec<String>>())
                        .collect(),
                )
            })
            .transpose()?;
        let graphics_items = parser.expect_many::<FootprintGraphicsItem>()?;
        let pads = parser.expect_many::<Pad>()?;
        let keep_out_zones = parser.expect_many::<Zone>()?;
        let groups = parser.expect_many::<Group>()?;
        let models = parser.expect_many::<Model>()?;

        parser.expect_end()?;

        Ok(Self {
            library_link,
            locked,
            placed,
            layer,
            tstamp,
            position,
            description,
            tags,
            properties,
            path,
            solder_mask_margin,
            solder_paste_margin,
            solder_paste_ratio,
            clearance,
            zone_connect,
            attributes,
            private_layers,
            net_tie_pad_groups,
            graphics_items,
            pads,
            keep_out_zones,
            groups,
            models,
        })
    }
}

simple_maybe_from_sexpr!(FootprintInlined, footprint);

impl ToSexpr for FootprintInlined {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "footprint",
            [
                vec![
                    Some(self.library_link.to_sexpr(context)),
                    self.locked.then(|| Sexpr::symbol("locked")),
                    self.placed.then(|| Sexpr::symbol("placed")),
                    Some(Sexpr::string_with_name("layer", self.layer)),
                    Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                    Some(self.position.to_sexpr(context)),
                    self.description
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("descr", s)),
                    self.tags
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("tags", s)),
                ],
                self.properties.into_sexpr_vec(context),
                vec![
                    self.path
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("path", s)),
                    self.solder_mask_margin
                        .map(|n| Sexpr::number_with_name("solder_mask_margin", n)),
                    self.solder_paste_margin
                        .map(|n| Sexpr::number_with_name("solder_paste_margin", n)),
                    self.solder_paste_ratio
                        .map(|n| Sexpr::number_with_name("solder_paste_ratio", n)),
                    self.clearance
                        .map(|n| Sexpr::number_with_name("clearance", n)),
                    self.zone_connect
                        .map(|n| Sexpr::number_with_name("zone_connect", n as u8 as f32)),
                    self.attributes.as_ref().map(|i| i.to_sexpr(context)),
                    self.private_layers.as_ref().map(|l| {
                        Sexpr::list_with_name(
                            "private_layers",
                            l.iter()
                                .map(Clone::clone)
                                .map(Sexpr::string)
                                .map(Option::Some)
                                .collect::<Vec<_>>(),
                        )
                    }),
                    self.net_tie_pad_groups.as_ref().map(|a| {
                        Sexpr::list_with_name(
                            "net_tie_pad_groups",
                            a.iter()
                                .map(|group| group.join(","))
                                .map(Sexpr::string)
                                .map(Option::Some)
                                .collect::<Vec<_>>(),
                        )
                    }),
                ],
                self.graphics_items.into_sexpr_vec(context),
                self.pads.into_sexpr_vec(context),
                self.keep_out_zones.into_sexpr_vec(context),
                self.groups.into_sexpr_vec(context),
                self.models.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

// ############################################################################

/// How pads are covered by copper in Zone
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum ZoneConnectKind {
    /// Pads are not covered
    NotConnected = 0,
    /// Use thermal relief for pads
    ThermalReliefs = 1,
    /// Pads are covered by copper
    SolidFill = 2,
    /// Thermal relief only for THT pads
    ThermalReliefsForPTH = 3,
}

impl TryFrom<u8> for ZoneConnectKind {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ZoneConnectKind::NotConnected),
            1 => Ok(ZoneConnectKind::ThermalReliefs),
            2 => Ok(ZoneConnectKind::SolidFill),
            _ => Err(KiCadParseError::InvalidEnumValue {
                value: value.to_string(),
                enum_name: std::any::type_name::<Self>(),
            }),
        }
    }
}

// ############################################################################

/// Attributes of the footprint (ex. SMD, through-hole, included in BOM, etc.)
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Default, Clone)]
pub struct FootprintAttributes {
    pub smd: bool,
    pub through_hole: bool,
    pub board_only: bool,
    pub exclude_from_pos_files: bool,
    pub exclude_from_bom: bool,
    pub allow_missing_courtyard: bool,
    pub allow_solder_mask_bridges: bool,
}

impl FromSexpr for FootprintAttributes {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("attr")?;

        let smd = parser.maybe_symbol_matching("smd");
        let through_hole = parser.maybe_symbol_matching("through_hole");
        let board_only = parser.maybe_symbol_matching("board_only");
        let exclude_from_pos_files = parser.maybe_symbol_matching("exclude_from_pos_files");
        let exclude_from_bom = parser.maybe_symbol_matching("exclude_from_bom");
        let allow_missing_courtyard = parser.maybe_symbol_matching("allow_missing_courtyard");
        let allow_solder_mask_bridges = parser.maybe_symbol_matching("allow_soldermask_bridges");

        parser.expect_end()?;

        Ok(Self {
            smd,
            through_hole,
            board_only,
            exclude_from_pos_files,
            exclude_from_bom,
            allow_missing_courtyard,
            allow_solder_mask_bridges,
        })
    }
}

simple_maybe_from_sexpr!(FootprintAttributes, attr);

impl ToSexpr for FootprintAttributes {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "attr",
            [
                self.smd.then(|| Sexpr::symbol("smd")),
                self.through_hole.then(|| Sexpr::symbol("through_hole")),
                self.board_only.then(|| Sexpr::symbol("board_only")),
                self.exclude_from_pos_files
                    .then(|| Sexpr::symbol("exclude_from_pos_files")),
                self.exclude_from_bom
                    .then(|| Sexpr::symbol("exclude_from_bom")),
                self.allow_missing_courtyard
                    .then(|| Sexpr::symbol("allow_missing_courtyard")),
                self.allow_solder_mask_bridges
                    .then(|| Sexpr::symbol("allow_soldermask_bridges")),
            ],
        )
    }
}

// ############################################################################

/// All the types of graphics items that can be part of a footprint (images,
/// text, text boxes, shapes, etc.)
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum FootprintGraphicsItem {
    Image(Image),
    Text(FootprintText),
    TextBox(FootprintTextBox),
    Shape(FootprintShape),
}

impl FromSexpr for FootprintGraphicsItem {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let Some(symbol) = parser.peek_symbol() else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Symbol,
            });
        };

        match symbol {
            "image" => Image::from_sexpr(parser).map(FootprintGraphicsItem::Image),
            "fp_text" => FootprintText::from_sexpr(parser).map(FootprintGraphicsItem::Text),
            "fp_text_box" => {
                FootprintTextBox::from_sexpr(parser).map(FootprintGraphicsItem::TextBox)
            }
            "fp_line" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            "fp_rect" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            "fp_circle" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            "fp_arc" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            "fp_poly" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            "fp_curve" => FootprintShape::from_sexpr(parser).map(FootprintGraphicsItem::Shape),
            _ => Err(KiCadParseError::invalid_enum_value::<Self>(symbol)),
        }
    }
}

impl MaybeFromSexpr for FootprintGraphicsItem {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "image",
            "fp_text",
            "fp_text_box",
            "fp_line",
            "fp_rect",
            "fp_circle",
            "fp_arc",
            "fp_poly",
            "fp_curve",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for FootprintGraphicsItem {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        match self {
            FootprintGraphicsItem::Image(image) => image.to_sexpr(context),
            FootprintGraphicsItem::Text(text) => text.to_sexpr(context),
            FootprintGraphicsItem::TextBox(text_box) => text_box.to_sexpr(context),
            FootprintGraphicsItem::Shape(shape) => shape.to_sexpr(context),
        }
    }
}

// ############################################################################

/// A 3D model associated with a footprint.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Model {
    pub file: String,
    pub hide: bool,
    pub opacity: Option<f32>,
    pub offset: Vec3D,
    pub scale: Vec3D,
    pub rotate: Vec3D,
}

impl FromSexpr for Model {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("model")?;

        let file = parser.expect_string()?;
        let hide = parser.maybe_symbol_matching("hide");
        let opacity = parser.maybe_number_with_name("opacity")?;
        let offset = parser.expect_list_with_name("offset")?.expect::<Vec3D>()?;
        let scale = parser.expect_list_with_name("scale")?.expect::<Vec3D>()?;
        let rotate = parser.expect_list_with_name("rotate")?.expect::<Vec3D>()?;

        parser.expect_end()?;

        Ok(Self {
            file,
            hide,
            opacity,
            offset,
            scale,
            rotate,
        })
    }
}

simple_maybe_from_sexpr!(Model, model);

impl ToSexpr for Model {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "model",
            [
                Some(Sexpr::string(&self.file)),
                self.hide.then(|| Sexpr::symbol("hide")),
                self.opacity.map(|o| Sexpr::number_with_name("opacity", o)),
                Some(Sexpr::list_with_name(
                    "offset",
                    [Some(self.offset.to_sexpr(context))],
                )),
                Some(Sexpr::list_with_name(
                    "scale",
                    [Some(self.scale.to_sexpr(context))],
                )),
                Some(Sexpr::list_with_name(
                    "rotate",
                    [Some(self.rotate.to_sexpr(context))],
                )),
            ],
        )
    }
}
