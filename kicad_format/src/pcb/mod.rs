//! PCB board file format (`.kicad_pcb` files)

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    common::{
        footprint::FootprintInlined, pad::Net, zone::Zone, Group, Image, LayerId, PageSettings,
        PageSize, Property, StandardPageSize, TitleBlock, Uuid, Vec2D,
    },
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName, VecToMaybeSexprVec,
    },
    simple_to_from_string, KiCadParseError, SexprKind,
};

use self::{
    graphics::PcbGraphicsItem,
    setup::{
        BoardSetup, BoardStackup, DrillMarks, OutlineMode, PcbPlotOptions, PlotFormat,
        StackupLayer, StackupLayerId,
    },
};

pub mod graphics;
pub mod setup;

/// A PCB board file (`.kicad_pcb` file).
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbFile {
    pub version: u32,
    pub generator: String,
    pub general_settings: GeneralSettings,
    pub page_settings: PageSettings,
    pub title_block: Option<TitleBlock>,
    pub layers: Vec<BoardLayer>,
    pub setup: BoardSetup,
    pub properties: Vec<Property>,
    pub nets: Vec<Net>,
    pub footprints: Vec<FootprintInlined>,
    pub graphics_items: Vec<PcbGraphicsItem>,
    pub images: Vec<Image>,
    pub tracks: Vec<Track>,
    pub zones: Vec<Zone>,
    pub groups: Vec<Group>,
}

impl Default for PcbFile {
    #[allow(deprecated)]
    fn default() -> Self {
        Self {
            version: 20221018,
            generator: "kicad_lib".to_string(),
            general_settings: GeneralSettings { thickness: 1.6 },
            page_settings: PageSettings {
                size: PageSize::Standard(StandardPageSize::A4),
                portrait: false,
            },
            title_block: None,
            layers: vec![
                BoardLayer::new(LayerId::FCu, BoardLayerKind::Signal, None),
                BoardLayer::new(LayerId::BCu, BoardLayerKind::Signal, None),
                BoardLayer::new(
                    LayerId::BAdhes,
                    BoardLayerKind::User,
                    Some("B.Adhesive".to_string()),
                ),
                BoardLayer::new(
                    LayerId::FAdhes,
                    BoardLayerKind::User,
                    Some("F.Adhesive".to_string()),
                ),
                BoardLayer::new(LayerId::BPaste, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::FPaste, BoardLayerKind::User, None),
                BoardLayer::new(
                    LayerId::BSilkS,
                    BoardLayerKind::User,
                    Some("B.Silkscreen".to_string()),
                ),
                BoardLayer::new(
                    LayerId::FSilkS,
                    BoardLayerKind::User,
                    Some("F.Silkscreen".to_string()),
                ),
                BoardLayer::new(LayerId::BMask, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::FMask, BoardLayerKind::User, None),
                BoardLayer::new(
                    LayerId::DwgsUser,
                    BoardLayerKind::User,
                    Some("User.Drawings".to_string()),
                ),
                BoardLayer::new(
                    LayerId::CmtsUser,
                    BoardLayerKind::User,
                    Some("User.Comments".to_string()),
                ),
                BoardLayer::new(
                    LayerId::Eco1User,
                    BoardLayerKind::User,
                    Some("User.Eco1".to_string()),
                ),
                BoardLayer::new(
                    LayerId::Eco2User,
                    BoardLayerKind::User,
                    Some("User.Eco2".to_string()),
                ),
                BoardLayer::new(LayerId::EdgeCuts, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::Margin, BoardLayerKind::User, None),
                BoardLayer::new(
                    LayerId::BCrtYd,
                    BoardLayerKind::User,
                    Some("B.Courtyard".to_string()),
                ),
                BoardLayer::new(
                    LayerId::FCrtYd,
                    BoardLayerKind::User,
                    Some("F.Courtyard".to_string()),
                ),
                BoardLayer::new(LayerId::BFab, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::FFab, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User1, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User2, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User3, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User4, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User5, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User6, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User7, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User8, BoardLayerKind::User, None),
                BoardLayer::new(LayerId::User9, BoardLayerKind::User, None),
            ],
            setup: BoardSetup {
                stackup: Some(BoardStackup {
                    layers: vec![
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::FSilkS),
                            kind: "Top Silk Screen".to_string(),
                            color: None,
                            thickness: None,
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::FPaste),
                            kind: "Top Solder Paste".to_string(),
                            color: None,
                            thickness: None,
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::FMask),
                            kind: "Top Solder Mask".to_string(),
                            color: None,
                            thickness: Some(0.01),
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::FCu),
                            kind: "copper".to_string(),
                            color: None,
                            thickness: Some(0.035),
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::Dielectric(1),
                            kind: "core".to_string(),
                            color: None,
                            thickness: Some(1.51),
                            material: Some("FR4".to_string()),
                            epsilon_r: Some(4.5),
                            loss_tangent: Some(0.02),
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::BCu),
                            kind: "copper".to_string(),
                            color: None,
                            thickness: Some(0.035),
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::BMask),
                            kind: "Bottom Solder Mask".to_string(),
                            color: None,
                            thickness: Some(0.01),
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::BPaste),
                            kind: "Bottom Solder Paste".to_string(),
                            color: None,
                            thickness: None,
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                        StackupLayer {
                            id: StackupLayerId::BoardLayer(LayerId::BSilkS),
                            kind: "Bottom Silk Screen".to_string(),
                            color: None,
                            thickness: None,
                            material: None,
                            epsilon_r: None,
                            loss_tangent: None,
                        },
                    ],
                    copper_finish: Some("None".to_string()),
                    dielectric_constraints: false,
                    edge_connector: None,
                    castellated_pads: false,
                    edge_plating: false,
                }),
                pad_to_mask_clearance: 0.0,
                solder_mask_min_width: None,
                pad_to_paste_clearance: None,
                pad_to_paste_clearance_ratio: None,
                allow_soldermask_bridges_in_footprints: false,
                aux_axis_origin: None,
                grid_origin: None,
                plot_options: PcbPlotOptions {
                    layer_selection: 0x00010fc_ffffffff,
                    plot_on_all_layers_selection: 0x0000000_00000000,
                    disable_aperture_macros: false,
                    use_gerber_extensions: false,
                    use_gerber_attributes: true,
                    use_gerber_advanced_attributes: true,
                    create_gerber_job_file: true,
                    gerber_precision: None,
                    dashed_line_dash_ratio: 12.0,
                    dashed_line_gap_ratio: 3.0,
                    svg_precision: 4,
                    plot_frame_ref: false,
                    vias_on_mask: false,
                    plot_mode: OutlineMode::Filled,
                    use_aux_origin: false,
                    hpgl_pen_number: 1,
                    hpgl_pen_speed: 20,
                    hpgl_pen_diameter: 15.0,
                    dxf_use_polygon_mode: true,
                    dxf_use_imperial_units: true,
                    dxf_use_pcbnew_font: true,
                    postscript_negative: false,
                    postscript_a4_output: false,
                    plot_references: true,
                    plot_values: true,
                    plot_invisible_text: false,
                    sketch_pads_on_fab: false,
                    subtract_mask_from_silk: false,
                    output_format: PlotFormat::Gerber,
                    mirror: false,
                    drill_shape: DrillMarks::Small,
                    scale_selection: 1,
                    output_directory: "".to_string(),
                },
            },
            properties: Vec::new(),
            nets: Vec::new(),
            footprints: Vec::new(),
            graphics_items: Vec::new(),
            images: Vec::new(),
            tracks: Vec::new(),
            zones: Vec::new(),
            groups: Vec::new(),
        }
    }
}

impl FromSexpr for PcbFile {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("kicad_pcb")?;

        let version = parser.expect_number_with_name("version")? as u32;
        let generator = parser.expect_symbol_with_name("generator")?;
        let general_settings = parser.expect::<GeneralSettings>()?;
        let page_settings = parser.expect::<PageSettings>()?;
        let title_block = parser.maybe::<TitleBlock>()?;
        let layers = parser.expect_list_with_name("layers").and_then(|mut p| {
            let layers = p.expect_many::<BoardLayer>()?;
            p.expect_end()?;

            Ok(layers)
        })?;
        let setup = parser.expect::<BoardSetup>()?;
        let properties = parser.expect_many::<Property>()?;
        let nets = parser.expect_many::<Net>()?;
        let footprints = parser.expect_many::<FootprintInlined>()?;
        let graphics_items = parser.expect_many::<PcbGraphicsItem>()?;
        let images = parser.expect_many::<Image>()?;
        let tracks = parser.expect_many::<Track>()?;
        let zones = parser.expect_many::<Zone>()?;
        let groups = parser.expect_many::<Group>()?;

        parser.expect_end()?;

        Ok(Self {
            version,
            generator,
            general_settings,
            page_settings,
            title_block,
            layers,
            setup,
            properties,
            nets,
            footprints,
            graphics_items,
            images,
            tracks,
            zones,
            groups,
        })
    }
}

impl ToSexpr for PcbFile {
    fn to_sexpr(&self, context: SerializationContext) -> kicad_sexpr::Sexpr {
        Sexpr::list_with_name(
            "kicad_pcb",
            [
                &[
                    Some(Sexpr::number_with_name("version", self.version as f32)),
                    Some(Sexpr::symbol_with_name("generator", &self.generator)),
                    Some(self.general_settings.to_sexpr(context)),
                    Some(self.page_settings.to_sexpr(context)),
                    self.title_block.as_ref().map(|i| i.to_sexpr(context)),
                    Some(Sexpr::list_with_name(
                        "layers",
                        self.layers.into_sexpr_vec(context),
                    )),
                    Some(self.setup.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
                &self.nets.into_sexpr_vec(context),
                &self.footprints.into_sexpr_vec(context),
                &self.graphics_items.into_sexpr_vec(context),
                &self.images.into_sexpr_vec(context),
                &self.tracks.into_sexpr_vec(context),
                &self.zones.into_sexpr_vec(context),
                &self.groups.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

// ############################################################################

/// General board settings. Currently only holds the board thickness.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct GeneralSettings {
    pub thickness: f32,
}

impl FromSexpr for GeneralSettings {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("general")?;

        let thickness = parser.expect_number_with_name("thickness")?;

        parser.expect_end()?;

        Ok(Self { thickness })
    }
}

impl ToSexpr for GeneralSettings {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "general",
            [Some(Sexpr::number_with_name("thickness", self.thickness))],
        )
    }
}

// ############################################################################

/// TODO: Technically, this struct should encapsulate the notion that only copper layers are allowed to have a BoardLayerKind that isn't User, but seeing as KiCad itself does not validate this, it's not a priority.
///
/// Looks like:
/// ```text
/// (0 "F.Cu" signal ["Front Copper"])
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct BoardLayer {
    pub layer: LayerId,
    pub kind: BoardLayerKind,
    pub name: Option<String>,
}

impl BoardLayer {
    fn new(layer: LayerId, kind: BoardLayerKind, name: Option<String>) -> Self {
        Self { layer, kind, name }
    }
}

impl FromSexpr for BoardLayer {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let id = parser.expect_number()? as u8;
        let layer = parser.expect_string()?.parse::<LayerId>()?;

        if layer as u8 != id {
            return Err(KiCadParseError::NonMatchingLayerId { id, layer });
        }

        let kind = parser.expect_symbol()?.parse::<BoardLayerKind>()?;
        let name = parser.maybe_string();

        parser.expect_end()?;

        Ok(Self { layer, kind, name })
    }
}

impl MaybeFromSexpr for BoardLayer {
    fn is_present(sexpr: &kicad_sexpr::SexprList) -> bool {
        sexpr.first().is_some_and(|s| s.as_number().is_some())
    }
}

impl ToSexpr for BoardLayer {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list([
            Some(Sexpr::number(self.layer as u8 as f32)),
            Some(Sexpr::string(self.layer)),
            Some(Sexpr::symbol(self.kind)),
            self.name.as_ref().map(Sexpr::string),
        ])
    }
}

/// The kind of layer, which determines how it is used in the PCB.
///
/// Only copper layers can be `Signal`, `Power`, `Mixed`, or `Jumper`. All
/// other layers should be the `User` type.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Default, Copy, Clone)]
pub enum BoardLayerKind {
    #[default]
    Signal,
    Power,
    Mixed,
    Jumper,
    User,
}

simple_to_from_string! {
    BoardLayerKind,
    signal <-> Signal,
    power <-> Power,
    mixed <-> Mixed,
    jumper <-> Jumper,
    user <-> User,
}

// ############################################################################

/// Different types of tracks on the PCB.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum Track {
    Segment(TrackSegment),
    Via(TrackVia),
    Arc(TrackArc),
}

impl FromSexpr for Track {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let Some(symbol) = parser.peek_symbol() else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Symbol,
            });
        };

        match symbol {
            "segment" => TrackSegment::from_sexpr(parser).map(Track::Segment),
            "via" => TrackVia::from_sexpr(parser).map(Track::Via),
            "arc" => TrackArc::from_sexpr(parser).map(Track::Arc),
            _ => Err(KiCadParseError::invalid_enum_value::<Self>(symbol)),
        }
    }
}

impl MaybeFromSexpr for Track {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &["segment", "via", "arc"];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for Track {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        match self {
            Track::Segment(segment) => segment.to_sexpr(context),
            Track::Via(via) => via.to_sexpr(context),
            Track::Arc(arc) => arc.to_sexpr(context),
        }
    }
}

/// The simplest form of a track, a straight line segment.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TrackSegment {
    pub locked: bool,
    pub start: Vec2D,
    pub end: Vec2D,
    pub width: f32,
    pub layer: LayerId,
    pub net: i32,
    pub tstamp: Uuid,
}

impl FromSexpr for TrackSegment {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("segment")?;

        let locked = parser.maybe_symbol_matching("locked");
        let start = parser.expect_with_name::<Vec2D>("start")?;
        let end = parser.expect_with_name::<Vec2D>("end")?;
        let width = parser.expect_number_with_name("width")?;
        let layer = parser
            .expect_string_with_name("layer")?
            .parse::<LayerId>()?;
        let net = parser.expect_number_with_name("net")? as i32;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        parser.expect_end()?;

        Ok(Self {
            locked,
            start,
            end,
            width,
            layer,
            net,
            tstamp,
        })
    }
}

impl ToSexpr for TrackSegment {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "segment",
            [
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(self.start.to_sexpr_with_name("start", context)),
                Some(self.end.to_sexpr_with_name("end", context)),
                Some(Sexpr::number_with_name("width", self.width)),
                Some(Sexpr::string_with_name("layer", self.layer)),
                Some(Sexpr::number_with_name("net", self.net as f32)),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
            ],
        )
    }
}

/// A via, a hole in the PCB that connects two layers.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TrackVia {
    pub kind: ViaKind,
    pub locked: bool,
    pub position: Vec2D,
    pub size: f32,
    pub drill: f32,
    pub layers: (LayerId, LayerId),
    pub remove_unused_layers: bool,
    pub keep_end_layers: bool,
    pub free: bool,
    pub zone_layer_connections: Option<Vec<LayerId>>,
    pub net: i32,
    pub tstamp: Uuid,
}

impl FromSexpr for TrackVia {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("via")?;

        let kind = parser
            .maybe_symbol()
            .map(|k| k.parse::<ViaKind>())
            .transpose()?
            .unwrap_or_default();
        let locked = parser.maybe_symbol_matching("locked");
        let position = parser.expect_with_name::<Vec2D>("at")?;
        let size = parser.expect_number_with_name("size")?;
        let drill = parser.expect_number_with_name("drill")?;
        let layers = parser.expect_list_with_name("layers").and_then(|mut p| {
            let layers = (
                p.expect_string()?.parse::<LayerId>()?,
                p.expect_string()?.parse::<LayerId>()?,
            );
            p.expect_end()?;

            Ok(layers)
        })?;
        let remove_unused_layers = parser.maybe_empty_list_with_name("remove_unused_layers")?;
        let keep_end_layers = parser.maybe_empty_list_with_name("keep_end_layers")?;
        let free = parser.maybe_empty_list_with_name("free")?;
        let zone_layer_connections = parser
            .maybe_list_with_name("zone_layer_connections")
            .map(|mut p| {
                let layers = p
                    .expect_many_strings()?
                    .iter()
                    .map(|s| s.parse::<LayerId>())
                    .collect::<Result<Vec<_>, _>>()?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(layers)
            })
            .transpose()?;
        let net = parser.expect_number_with_name("net")? as i32;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        parser.expect_end()?;

        Ok(Self {
            kind,
            locked,
            position,
            size,
            drill,
            layers,
            remove_unused_layers,
            keep_end_layers,
            free,
            zone_layer_connections,
            net,
            tstamp,
        })
    }
}

impl ToSexpr for TrackVia {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "via",
            [
                if self.kind != ViaKind::Through {
                    Some(Sexpr::symbol_with_name("kind", self.kind))
                } else {
                    None
                },
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(self.position.to_sexpr_with_name("at", context)),
                Some(Sexpr::number_with_name("size", self.size)),
                Some(Sexpr::number_with_name("drill", self.drill)),
                Some(Sexpr::list_with_name(
                    "layers",
                    [
                        Some(Sexpr::string(self.layers.0)),
                        Some(Sexpr::string(self.layers.1)),
                    ],
                )),
                self.remove_unused_layers
                    .then(|| Sexpr::list_with_name("remove_unused_layers", [])),
                self.keep_end_layers
                    .then(|| Sexpr::list_with_name("keep_end_layers", [])),
                self.free.then(|| Sexpr::list_with_name("free", [])),
                self.zone_layer_connections.as_ref().map(|layers| {
                    Sexpr::list_with_name(
                        "zone_layer_connections",
                        layers
                            .iter()
                            .map(Clone::clone)
                            .map(Sexpr::string)
                            .map(Option::Some)
                            .collect::<Vec<_>>(),
                    )
                }),
                Some(Sexpr::number_with_name("net", self.net as f32)),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
            ],
        )
    }
}

/// The type of via, which determines which layers it can connect.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Default, Clone, Copy)]
pub enum ViaKind {
    /// Always a through hole via
    #[default]
    Through = 3,
    /// This via can be on internal layers
    BlindBuried = 2,
    /// This via which connect from an external layer to the near neighbor
    /// internal layer
    MicroVia = 1,
}

simple_to_from_string! {
    ViaKind,
    through <-> Through,
    blind <-> BlindBuried,
    micro <-> MicroVia,
}

/// An arc, a curved track on the PCB.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TrackArc {
    pub locked: bool,
    pub start: Vec2D,
    pub midpoint: Vec2D,
    pub end: Vec2D,
    pub width: f32,
    pub layer: LayerId,
    pub net: i32,
    pub tstamp: Uuid,
}

impl FromSexpr for TrackArc {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("arc")?;

        let locked = parser.maybe_symbol_matching("locked");
        let start = parser.expect_with_name::<Vec2D>("start")?;
        let midpoint = parser.expect_with_name::<Vec2D>("mid")?;
        let end = parser.expect_with_name::<Vec2D>("end")?;
        let width = parser.expect_number_with_name("width")?;
        let layer = parser
            .expect_string_with_name("layer")?
            .parse::<LayerId>()?;
        let net = parser.expect_number_with_name("net")? as i32;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        parser.expect_end()?;

        Ok(Self {
            locked,
            start,
            midpoint,
            end,
            width,
            layer,
            net,
            tstamp,
        })
    }
}

impl ToSexpr for TrackArc {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "arc",
            [
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(self.start.to_sexpr_with_name("start", context)),
                Some(self.midpoint.to_sexpr_with_name("mid", context)),
                Some(self.end.to_sexpr_with_name("end", context)),
                Some(Sexpr::number_with_name("width", self.width)),
                Some(Sexpr::string_with_name("layer", self.layer)),
                Some(Sexpr::number_with_name("net", self.net as f32)),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
            ],
        )
    }
}
