//! Footprint library file format (`.kicad_mod` files)

use kicad_sexpr::Sexpr;

use crate::{
    common::{
        footprint::{FootprintAttributes, FootprintGraphicsItem, Model, ZoneConnectKind},
        pad::Pad,
        zone::Zone,
        Group, LayerId, Property,
    },
    convert::{FromSexpr, Parser, SerializationContext, ToSexpr, VecToMaybeSexprVec},
    KiCadParseError,
};

/// Stores a footprint which can be instanced within a PCB board file
///
/// Footprints defined in a KiCad footprint library file differ slightly from
/// the footprints used board files. For the most part the differences are
/// that footprints a footprint library file include a file header, and omit
/// several attributes such as the footprint position, rotation, and locked
/// status.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintLibraryFile {
    pub name: String,
    pub version: u32,
    pub generator: String,
    pub layer: LayerId,
    pub description: Option<String>,
    pub tags: Option<String>,
    pub properties: Vec<Property>,
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

impl FromSexpr for FootprintLibraryFile {
    fn from_sexpr(mut parser: Parser) -> Result<Self, crate::KiCadParseError> {
        parser.expect_symbol_matching("footprint")?;

        let name = parser.expect_string()?;
        let version = parser.expect_number_with_name("version")? as u32;
        let generator = parser.expect_symbol_with_name("generator")?;
        let layer = parser.expect_string_with_name("layer")?.parse()?;
        let description = parser.maybe_string_with_name("descr")?;
        let tags = parser.maybe_string_with_name("tags")?;
        let properties = parser.expect_many::<Property>()?;
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

        Ok(FootprintLibraryFile {
            name,
            version,
            generator,
            layer,
            description,
            tags,
            properties,
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

impl ToSexpr for FootprintLibraryFile {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "footprint",
            [
                vec![
                    Some(Sexpr::string(&self.name)),
                    Some(Sexpr::number_with_name("version", self.version as f32)),
                    Some(Sexpr::symbol_with_name("generator", &self.generator)),
                    Some(Sexpr::string_with_name("layer", self.layer)),
                    self.description
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("descr", s)),
                    self.tags
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("tags", s)),
                ],
                self.properties.into_sexpr_vec(context),
                vec![
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
