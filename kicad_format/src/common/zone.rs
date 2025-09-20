//! Common structures related to board Zones.

use std::vec;

use kicad_sexpr::Sexpr;

use crate::{
    convert::{
        FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName,
        VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError,
};

use super::{CoordinatePointList, LayerId, Uuid};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Zone {
    pub locked: bool,
    pub net_number: i32,
    pub net_name: String,
    pub layers: Vec<LayerId>,
    pub tstamp: Uuid,
    pub name: Option<String>,
    pub hatch: Hatch,
    pub priority: Option<i32>,
    pub tear_drop_kind: Option<TearDropKind>,
    pub connect_pads: PadConnection,
    pub min_thickness: f32,
    pub filled_areas_thickness: bool,
    pub keep_out_settings: Option<KeepOutSettings>,
    pub fill_settings: FillSettings,
    pub polygon: Option<CoordinatePointList>,
    pub fill_polygons: Vec<FilledPolygon>,
    pub fill_segments: Vec<FilledSegments>,
}

impl FromSexpr for Zone {
    fn from_sexpr(mut parser: Parser) -> Result<Self, crate::KiCadParseError> {
        parser.expect_symbol_matching("zone")?;

        let locked = parser.maybe_symbol_matching("locked");
        let net_number = parser.expect_number_with_name("net")? as i32;
        let net_name = parser.expect_string_with_name("net_name")?;

        /* Zone may have a `layer` or a `layers` field */
        let maybe_layer = parser
            .maybe_list_with_name("layer")
            .map(|mut p| {
                let layer = p.expect_string()?.parse::<LayerId>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(layer)
            })
            .transpose()?;
        let maybe_layers = parser
            .maybe_list_with_name("layers")
            .map(|mut p| {
                let layers = p
                    .expect_many_strings()?
                    .into_iter()
                    .map(|s| s.parse())
                    .collect::<Result<Vec<_>, _>>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(layers)
            })
            .transpose()?;
        let layers = match (maybe_layer, maybe_layers) {
            (Some(layer), None) => vec![layer],
            (None, Some(layers)) => layers,
            (Some(_), Some(_)) => {
                return Err(KiCadParseError::FoundMutuallyExclusiveFields {
                    field1: "layer".to_string(),
                    field2: "layers".to_string(),
                });
            }
            _ => {
                return Err(KiCadParseError::ExpectedMutuallyExclusiveField {
                    field1: "layer".to_string(),
                    field2: "layers".to_string(),
                });
            }
        };
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;
        let name = parser.maybe_string_with_name("name")?;
        let hatch = parser.expect::<Hatch>()?;
        let priority = parser.maybe_number_with_name("priority")?.map(|n| n as i32);
        let tear_drop_kind = parser
            .maybe_list_with_name("attr")
            .map(|mut p| {
                let kind = p.expect_list_with_name("teardrop").and_then(|mut p| {
                    let kind = p.expect_symbol_with_name("type")?.parse()?;
                    p.expect_end()?;

                    Ok::<_, KiCadParseError>(kind)
                })?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(kind)
            })
            .transpose()?;
        let connect_pads = parser.expect::<PadConnection>()?;
        let min_thickness = parser.expect_number_with_name("min_thickness")?;
        let filled_areas_thickness = parser.expect_bool_with_name("filled_areas_thickness")?;
        let keep_out_settings = parser.maybe::<KeepOutSettings>()?;
        let fill_settings = parser.expect::<FillSettings>()?;
        let polygon = parser
            .maybe_list_with_name("polygon")
            .map(|mut p| {
                let polygon = p.expect::<CoordinatePointList>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(polygon)
            })
            .transpose()?;
        let fill_polygons = parser.expect_many::<FilledPolygon>()?;
        let fill_segments = parser.expect_many::<FilledSegments>()?;

        Ok(Self {
            locked,
            net_number,
            net_name,
            layers,
            tstamp,
            name,
            hatch,
            priority,
            tear_drop_kind,
            connect_pads,
            min_thickness,
            filled_areas_thickness,
            keep_out_settings,
            fill_settings,
            polygon,
            fill_polygons,
            fill_segments,
        })
    }
}

simple_maybe_from_sexpr!(Zone, zone);

impl ToSexpr for Zone {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let layer_or_layers = if self.layers.len() == 1 && !self.layers[0].is_wildcard() {
            Sexpr::string_with_name("layer", self.layers[0])
        } else {
            Sexpr::list_with_name(
                "layers",
                self.layers
                    .iter()
                    .map(Clone::clone)
                    .map(Sexpr::string)
                    .map(Option::Some)
                    .collect::<Vec<_>>(),
            )
        };

        Sexpr::list_with_name(
            "zone",
            [
                vec![
                    self.locked.then(|| Sexpr::symbol("locked")),
                    Some(Sexpr::number_with_name("net", self.net_number as f32)),
                    Some(Sexpr::string_with_name("net_name", &self.net_name)),
                    Some(layer_or_layers),
                    Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                    self.name
                        .as_ref()
                        .map(|name| Sexpr::string_with_name("name", name)),
                    Some(self.hatch.to_sexpr(context)),
                    self.priority
                        .map(|p| Sexpr::number_with_name("priority", p as f32)),
                    self.tear_drop_kind.map(|k| {
                        Sexpr::list_with_name(
                            "attr",
                            [Sexpr::list_with_name(
                                "teardrop",
                                [Sexpr::symbol_with_name("type", k)].map(Option::Some),
                            )]
                            .map(Option::Some),
                        )
                    }),
                    Some(self.connect_pads.to_sexpr(context)),
                    Some(Sexpr::number_with_name("min_thickness", self.min_thickness)),
                    Some(Sexpr::symbol_with_name(
                        "filled_areas_thickness",
                        if self.filled_areas_thickness {
                            "yes"
                        } else {
                            "no"
                        },
                    )),
                    self.keep_out_settings.as_ref().map(|i| i.to_sexpr(context)),
                    Some(self.fill_settings.to_sexpr(context)),
                    self.polygon
                        .as_ref()
                        .map(|p| Sexpr::list_with_name("polygon", [Some(p.to_sexpr(context))])),
                ],
                self.fill_polygons.into_sexpr_vec(context),
                self.fill_segments.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Hatch {
    pub style: HatchStyle,
    pub pitch: f32,
}

impl FromSexpr for Hatch {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("hatch")?;

        let style = parser.expect_symbol()?.parse()?;
        let pitch = parser.expect_number()?;

        parser.expect_end()?;

        Ok(Self { style, pitch })
    }
}

impl ToSexpr for Hatch {
    fn to_sexpr(&self, _: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "hatch",
            [
                Some(Sexpr::symbol(self.style)),
                Some(Sexpr::number(self.pitch)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HatchStyle {
    None,
    Edge,
    Full,
}

simple_to_from_string! {
    HatchStyle,
    none <-> None,
    edge <-> Edge,
    full <-> Full,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TearDropKind {
    PadVia,
    TrackEnd,
}

simple_to_from_string! {
    TearDropKind,
    padvia <-> PadVia,
    track_end <-> TrackEnd,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadConnection {
    pub kind: Option<PadConnectionKind>,
    pub clearance: f32,
}

impl FromSexpr for PadConnection {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("connect_pads")?;

        let kind = parser
            .maybe_symbol()
            .map(|s| s.parse::<PadConnectionKind>())
            .transpose()?;
        let clearance = parser.expect_number_with_name("clearance")?;

        parser.expect_end()?;

        Ok(Self { kind, clearance })
    }
}

impl ToSexpr for PadConnection {
    fn to_sexpr(&self, _: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "connect_pads",
            [
                self.kind.map(Sexpr::symbol),
                Some(Sexpr::number_with_name("clearance", self.clearance)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PadConnectionKind {
    ThruHoleOnly,
    Full,
    No,
}

simple_to_from_string! {
    PadConnectionKind,
    thru_hole_only <-> ThruHoleOnly,
    yes <-> Full,
    no <-> No,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct KeepOutSettings {
    pub tracks: KeepOut,
    pub vias: KeepOut,
    pub pads: KeepOut,
    pub copper_pour: KeepOut,
    pub footprints: KeepOut,
}

impl FromSexpr for KeepOutSettings {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("keepout")?;

        let tracks = parser.expect_symbol_with_name("tracks")?.parse()?;
        let vias = parser.expect_symbol_with_name("vias")?.parse()?;
        let pads = parser.expect_symbol_with_name("pads")?.parse()?;
        let copper_pour = parser.expect_symbol_with_name("copperpour")?.parse()?;
        let footprints = parser.expect_symbol_with_name("footprints")?.parse()?;

        parser.expect_end()?;

        Ok(Self {
            tracks,
            vias,
            pads,
            copper_pour,
            footprints,
        })
    }
}

simple_maybe_from_sexpr!(KeepOutSettings, keepout);

impl ToSexpr for KeepOutSettings {
    fn to_sexpr(&self, _: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "keepout",
            [
                Some(Sexpr::symbol_with_name("tracks", self.tracks)),
                Some(Sexpr::symbol_with_name("vias", self.vias)),
                Some(Sexpr::symbol_with_name("pads", self.pads)),
                Some(Sexpr::symbol_with_name("copperpour", self.copper_pour)),
                Some(Sexpr::symbol_with_name("footprints", self.footprints)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum KeepOut {
    Allowed,
    NotAllowed,
}

simple_to_from_string! {
    KeepOut,
    allowed <-> Allowed,
    not_allowed <-> NotAllowed,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FillSettings {
    pub filled: bool,
    pub mode: Option<ZoneFillMode>,
    pub thermal_gap: f32,
    pub thermal_bridge_width: f32,
    pub smoothing: Option<FillSmoothingStyle>,
    pub radius: Option<f32>,
    pub island_removal_mode: Option<FillIslandRemovalMode>,
    pub island_area_min: Option<f32>,
    pub hatch_thickness: Option<f32>,
    pub hatch_gap: Option<f32>,
    pub hatch_orientation: Option<f32>,
    pub hatch_smoothing_level: Option<HatchSmoothingLevel>,
    pub hatch_smoothing_value: Option<f32>,
    pub hatch_border_algorithm: Option<HatchBorderAlgorithm>,
    pub hatch_min_hole_area: Option<f32>,
}

impl FromSexpr for FillSettings {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fill")?;

        let filled = parser.maybe_symbol_matching("yes");
        let mode = parser
            .maybe_symbol_with_name("mode")?
            .map(|s| s.parse::<ZoneFillMode>())
            .transpose()?;
        let thermal_gap = parser.expect_number_with_name("thermal_gap")?;
        let thermal_bridge_width = parser.expect_number_with_name("thermal_bridge_width")?;
        let smoothing = parser
            .maybe_symbol_with_name("smoothing")?
            .map(|s| s.parse())
            .transpose()?;
        let radius = parser.maybe_number_with_name("radius")?;
        let island_removal_mode = parser
            .maybe_number_with_name("island_removal_mode")?
            .map(|n| n as u8)
            .map(FillIslandRemovalMode::try_from)
            .transpose()?;
        let island_area_min = parser.maybe_number_with_name("island_area_min")?;
        let hatch_thickness = parser.maybe_number_with_name("hatch_thickness")?;
        let hatch_gap = parser.maybe_number_with_name("hatch_gap")?;
        let hatch_orientation = parser.maybe_number_with_name("hatch_orientation")?;
        let hatch_smoothing_level = parser
            .maybe_number_with_name("hatch_smoothing_level")?
            .map(|n| n as u8)
            .map(HatchSmoothingLevel::try_from)
            .transpose()?;
        let hatch_smoothing_value = parser.maybe_number_with_name("hatch_smoothing_value")?;
        let hatch_border_algorithm = parser
            .maybe_symbol_with_name("hatch_border_algorithm")?
            .map(|n| n.parse())
            .transpose()?;
        let hatch_min_hole_area = parser.maybe_number_with_name("hatch_min_hole_area")?;

        parser.expect_end()?;

        Ok(Self {
            filled,
            mode,
            thermal_gap,
            thermal_bridge_width,
            smoothing,
            radius,
            island_removal_mode,
            island_area_min,
            hatch_thickness,
            hatch_gap,
            hatch_orientation,
            hatch_smoothing_level,
            hatch_smoothing_value,
            hatch_border_algorithm,
            hatch_min_hole_area,
        })
    }
}

impl ToSexpr for FillSettings {
    fn to_sexpr(&self, _: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "fill",
            [
                self.filled.then(|| Sexpr::symbol("yes")),
                self.mode.map(|m| Sexpr::symbol_with_name("mode", m)),
                Some(Sexpr::number_with_name("thermal_gap", self.thermal_gap)),
                Some(Sexpr::number_with_name(
                    "thermal_bridge_width",
                    self.thermal_bridge_width,
                )),
                self.smoothing
                    .map(|s| Sexpr::symbol_with_name("smoothing", s)),
                self.radius.map(|r| Sexpr::number_with_name("radius", r)),
                self.island_removal_mode
                    .map(|m| Sexpr::number_with_name("island_removal_mode", m as u8 as f32)),
                self.island_area_min
                    .map(|a| Sexpr::number_with_name("island_area_min", a)),
                self.hatch_thickness
                    .map(|t| Sexpr::number_with_name("hatch_thickness", t)),
                self.hatch_gap
                    .map(|g| Sexpr::number_with_name("hatch_gap", g)),
                self.hatch_orientation
                    .map(|o| Sexpr::number_with_name("hatch_orientation", o)),
                self.hatch_smoothing_level
                    .map(|l| Sexpr::number_with_name("hatch_smoothing_level", l as u8 as f32)),
                self.hatch_smoothing_value
                    .map(|v| Sexpr::number_with_name("hatch_smoothing_value", v)),
                self.hatch_border_algorithm
                    .map(|a| Sexpr::symbol_with_name("hatch_border_algorithm", a)),
                self.hatch_min_hole_area
                    .map(|a| Sexpr::number_with_name("hatch_min_hole_area", a)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ZoneFillMode {
    Solid,
    Hatched,
}

simple_to_from_string! {
    ZoneFillMode,
    solid <-> Solid,
    hatch <-> Hatched,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FillSmoothingStyle {
    Chamfer,
    Fillet,
}

simple_to_from_string! {
    FillSmoothingStyle,
    chamfer <-> Chamfer,
    fillet <-> Fillet,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum FillIslandRemovalMode {
    AlwaysRemoveIslands = 0,
    NeverRemoveIslands = 1,
    MinimumArea = 2,
}

impl TryFrom<u8> for FillIslandRemovalMode {
    type Error = KiCadParseError;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        Ok(match n {
            0 => Self::AlwaysRemoveIslands,
            1 => Self::NeverRemoveIslands,
            2 => Self::MinimumArea,
            _ => return Err(KiCadParseError::invalid_enum_value::<Self>(n.to_string())),
        })
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum HatchSmoothingLevel {
    NoSmoothing = 0,
    Fillet = 1,
    ArcMinimum = 2,
    ArcMaximum = 3,
}

impl TryFrom<u8> for HatchSmoothingLevel {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::NoSmoothing,
            1 => Self::Fillet,
            2 => Self::ArcMinimum,
            3 => Self::ArcMaximum,
            _ => {
                return Err(KiCadParseError::invalid_enum_value::<Self>(
                    value.to_string(),
                ))
            }
        })
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HatchBorderAlgorithm {
    UseHatchThickness,
    UseZoneMinimumThickness,
}

simple_to_from_string! {
    HatchBorderAlgorithm,
    hatch_thickness <-> UseHatchThickness,
    min_thickness <-> UseZoneMinimumThickness,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FilledPolygon {
    pub layer: LayerId,
    pub island: bool,
    pub polygon: CoordinatePointList,
}

impl FromSexpr for FilledPolygon {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("filled_polygon")?;

        let layer = parser
            .expect_string_with_name("layer")?
            .parse::<LayerId>()?;
        let island = parser.maybe_empty_list_with_name("island")?;
        let polygon = parser.expect::<CoordinatePointList>()?;

        parser.expect_end()?;

        Ok(Self {
            layer,
            island,
            polygon,
        })
    }
}

simple_maybe_from_sexpr!(FilledPolygon, filled_polygon);

impl ToSexpr for FilledPolygon {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "filled_polygon",
            [
                Some(Sexpr::string_with_name("layer", self.layer)),
                self.island.then(|| Sexpr::list_with_name("island", [])),
                Some(self.polygon.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FilledSegments {
    pub layer: LayerId,
    pub segments: CoordinatePointList,
}

impl FromSexpr for FilledSegments {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fill_segments")?;

        let layer = parser
            .expect_string_with_name("layer")?
            .parse::<LayerId>()?;
        let segments = parser.expect::<CoordinatePointList>()?;

        parser.expect_end()?;

        Ok(Self { layer, segments })
    }
}

simple_maybe_from_sexpr!(FilledSegments, fill_segments);

impl ToSexpr for FilledSegments {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "fill_segments",
            [
                Some(Sexpr::string_with_name("layer", self.layer)),
                Some(self.segments.to_sexpr(context)),
            ],
        )
    }
}
