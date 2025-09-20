//! Common structures related to footprint pads

use kicad_sexpr::Sexpr;

use self::primitive::PadGraphicsPrimitive;

use super::footprint::ZoneConnectKind;
use crate::{
    common::{LayerId, Position, Uuid, Vec2D},
    convert::{FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName},
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError,
};

pub mod primitive;

/// A footprint pad
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Pad {
    pub index: String,
    pub kind: PadKind,
    pub shape: PadShape,
    pub locked: bool,
    pub position: Position,
    pub size: Vec2D,
    pub rect_delta: Option<Vec2D>,
    pub drill: Option<Drill>,
    pub property: Option<PadProperty>,
    pub layers: Vec<LayerId>,
    pub remove_unused_layer: bool,
    pub keep_end_layers: bool,
    pub zone_layer_connections: Option<Vec<LayerId>>,
    pub round_rect_radius_ratio: Option<f32>,
    pub chamfer_ratio: Option<f32>,
    pub chamfer: Option<Chamfer>,
    pub net: Option<Net>,
    pub pin_function: Option<String>,
    pub pin_type: Option<String>,
    pub die_length: Option<f32>,
    pub solder_mask_margin: Option<f32>,
    pub solder_paste_margin: Option<f32>,
    pub solder_paste_margin_ratio: Option<f32>,
    pub clearance: Option<f32>,
    pub zone_connect: Option<ZoneConnectKind>,
    pub thermal_bridge_width: Option<f32>,
    pub thermal_bridge_angle: Option<f32>,
    pub thermal_gap: Option<f32>,
    pub custom_pad_options: Option<CustomPadOptions>,
    pub custom_pad_primitives: Option<Vec<PadGraphicsPrimitive>>,
    pub tstamp: Uuid,
}

impl Pad {
    pub fn is_on_copper_layer(&self) -> bool {
        // FIXME: Use Layer Set
        self.layers.iter().any(|l| l.is_copper())
    }
}

impl FromSexpr for Pad {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pad")?;

        let index = parser.expect_string()?;
        let kind = parser.expect_symbol()?.parse::<PadKind>()?;
        let shape = parser.expect_symbol()?.parse::<PadShape>()?;
        let locked = parser.maybe_symbol_matching("locked");
        let position = parser.expect::<Position>()?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let rect_delta = parser.maybe_with_name::<Vec2D>("rect_delta")?;
        let drill = parser.maybe::<Drill>()?;
        let property = parser
            .maybe_list_with_name("property")
            .map(|mut p| {
                let property = p.expect_symbol()?.parse::<PadProperty>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(property)
            })
            .transpose()?;
        let layers = parser.expect_list_with_name("layers").and_then(|mut p| {
            let layers = p
                .expect_many_strings()?
                .into_iter()
                .map(|s| s.parse::<LayerId>())
                .collect::<Result<Vec<_>, _>>()?;
            p.expect_end()?;
            Ok::<_, KiCadParseError>(layers)
        })?;
        let remove_unused_layer = parser.maybe_empty_list_with_name("remove_unused_layer")?;
        let keep_end_layers = parser.maybe_empty_list_with_name("keep_end_layers")?;
        let zone_layer_connections = parser
            .maybe_list_with_name("zone_layer_connections")
            .map(|mut p| {
                let layers = p
                    .expect_many_strings()?
                    .into_iter()
                    .map(|s| s.parse::<LayerId>())
                    .collect::<Result<Vec<_>, _>>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(layers)
            })
            .transpose()?;
        let round_rect_radius_ratio = parser.maybe_number_with_name("roundrect_rratio")?;
        let chamfer_ratio = parser.maybe_number_with_name("chamfer_ratio")?;
        let chamfer = parser.maybe::<Chamfer>()?;
        let net = parser.maybe::<Net>()?;
        let pin_function = parser.maybe_string_with_name("pinfunction")?;
        let pin_type = parser.maybe_string_with_name("pintype")?;
        let die_length = parser.maybe_number_with_name("die_length")?;
        let solder_mask_margin = parser.maybe_number_with_name("solder_mask_margin")?;
        let solder_paste_margin = parser.maybe_number_with_name("solder_paste_margin")?;
        let solder_paste_margin_ratio =
            parser.maybe_number_with_name("solder_paste_margin_ratio")?;
        let clearance = parser.maybe_number_with_name("clearance")?;
        let zone_connect = parser
            .maybe_number_with_name("zone_connect")?
            .map(|n| n as u8)
            .map(ZoneConnectKind::try_from)
            .transpose()?;
        let thermal_bridge_width = parser.maybe_number_with_name("thermal_bridge_width")?;
        let thermal_bridge_angle = parser.maybe_number_with_name("thermal_bridge_angle")?;
        let thermal_gap = parser.maybe_number_with_name("thermal_gap")?;
        let custom_pad_options = parser.maybe::<CustomPadOptions>()?;
        let custom_pad_primitives = parser
            .maybe_list_with_name("primitives")
            .map(|mut p| {
                let primitives = p.expect_many::<PadGraphicsPrimitive>()?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(primitives)
            })
            .transpose()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        parser.expect_end()?;

        Ok(Self {
            index,
            kind,
            shape,
            locked,
            position,
            size,
            rect_delta,
            drill,
            property,
            layers,
            remove_unused_layer,
            keep_end_layers,
            zone_layer_connections,
            round_rect_radius_ratio,
            chamfer_ratio,
            chamfer,
            net,
            pin_function,
            pin_type,
            die_length,
            solder_mask_margin,
            solder_paste_margin,
            solder_paste_margin_ratio,
            clearance,
            zone_connect,
            thermal_bridge_width,
            thermal_bridge_angle,
            thermal_gap,
            custom_pad_options,
            custom_pad_primitives,
            tstamp,
        })
    }
}

simple_maybe_from_sexpr!(Pad, pad);

impl ToSexpr for Pad {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "pad",
            [
                Some(Sexpr::string(&self.index)),
                Some(Sexpr::symbol(self.kind)),
                Some(Sexpr::symbol(self.shape)),
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(self.position.to_sexpr(context)),
                Some(self.size.to_sexpr_with_name("size", context)),
                self.rect_delta
                    .as_ref()
                    .map(|r| r.to_sexpr_with_name("rect_delta", context)),
                self.drill.as_ref().map(|i| i.to_sexpr(context)),
                self.property
                    .map(|p| Sexpr::symbol_with_name("property", p)),
                Some(Sexpr::list_with_name(
                    "layers",
                    self.layers
                        .iter()
                        .map(Clone::clone)
                        .map(Sexpr::string)
                        .map(Option::Some)
                        .collect::<Vec<_>>(),
                )),
                self.remove_unused_layer
                    .then(|| Sexpr::list_with_name("remove_unused_layer", [])),
                self.keep_end_layers
                    .then(|| Sexpr::list_with_name("keep_end_layers", [])),
                self.zone_layer_connections.as_ref().map(|l| {
                    Sexpr::list_with_name(
                        "zone_layer_connections",
                        l.iter()
                            .map(Clone::clone)
                            .map(Sexpr::string)
                            .map(Option::Some)
                            .collect::<Vec<_>>(),
                    )
                }),
                self.round_rect_radius_ratio
                    .map(|n| Sexpr::number_with_name("roundrect_rratio", n)),
                self.chamfer_ratio
                    .map(|n| Sexpr::number_with_name("chamfer_ratio", n)),
                self.chamfer.as_ref().map(|i| i.to_sexpr(context)),
                self.net.as_ref().map(|i| i.to_sexpr(context)),
                self.pin_function
                    .as_ref()
                    .map(|s| Sexpr::string_with_name("pinfunction", s)),
                self.pin_type
                    .as_ref()
                    .map(|s| Sexpr::string_with_name("pintype", s)),
                self.die_length
                    .map(|n| Sexpr::number_with_name("die_length", n)),
                self.solder_mask_margin
                    .map(|n| Sexpr::number_with_name("solder_mask_margin", n)),
                self.solder_paste_margin
                    .map(|n| Sexpr::number_with_name("solder_paste_margin", n)),
                self.solder_paste_margin_ratio
                    .map(|n| Sexpr::number_with_name("solder_paste_margin_ratio", n)),
                self.clearance
                    .map(|n| Sexpr::number_with_name("clearance", n)),
                self.zone_connect
                    .map(|z| Sexpr::number_with_name("zone_connect", z as u8 as f32)),
                self.thermal_bridge_width
                    .map(|n| Sexpr::number_with_name("thermal_bridge_width", n)),
                self.thermal_bridge_angle
                    .map(|n| Sexpr::number_with_name("thermal_bridge_angle", n)),
                self.thermal_gap
                    .map(|n| Sexpr::number_with_name("thermal_gap", n)),
                self.custom_pad_options
                    .as_ref()
                    .map(|i| i.to_sexpr(context)),
                self.custom_pad_primitives.as_ref().map(|p| {
                    Sexpr::list_with_name(
                        "primitives",
                        p.iter()
                            .map(|i| i.to_sexpr(context))
                            .map(Option::Some)
                            .collect::<Vec<_>>(),
                    )
                }),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PadKind {
    ThroughHole,
    Smd,
    Connect,
    NpThroughHole,
}

simple_to_from_string! {
    PadKind,
    thru_hole <-> ThroughHole,
    smd <-> Smd,
    connect <-> Connect,
    np_thru_hole <-> NpThroughHole,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PadShape {
    Circle,
    Rect,
    Oval,
    Trapezoid,
    RoundRect,
    Custom,
}

simple_to_from_string! {
    PadShape,
    circle <-> Circle,
    rect <-> Rect,
    oval <-> Oval,
    trapezoid <-> Trapezoid,
    roundrect <-> RoundRect,
    custom <-> Custom,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Drill {
    pub diameter: f32,
    /// Present if the drill is oval
    pub width: Option<f32>,
    pub offset: Option<Vec2D>,
}

impl Drill {
    pub fn is_oval(&self) -> bool {
        self.width.is_some()
    }
}

impl FromSexpr for Drill {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("drill")?;

        let oval = parser.maybe_symbol_matching("oval");

        let diameter = parser.expect_number()?;
        let width = oval.then(|| parser.expect_number()).transpose()?;
        let offset = parser.maybe_with_name::<Vec2D>("offset")?;

        parser.expect_end()?;

        Ok(Self {
            diameter,
            width,
            offset,
        })
    }
}

simple_maybe_from_sexpr!(Drill, drill);

impl ToSexpr for Drill {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "drill",
            [
                self.is_oval().then(|| Sexpr::symbol("oval")),
                Some(Sexpr::number(self.diameter)),
                self.width.map(Sexpr::number),
                self.offset
                    .as_ref()
                    .map(|o| o.to_sexpr_with_name("offset", context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PadProperty {
    Bga,
    FiducialGlob,
    FiducialLoc,
    TestPoint,
    HeatSink,
    Castellated,
}

simple_to_from_string! {
    PadProperty,
    pad_prop_bga <-> Bga,
    pad_prop_fiducial_glob <-> FiducialGlob,
    pad_prop_fiducial_loc <-> FiducialLoc,
    pad_prop_testpoint <-> TestPoint,
    pad_prop_heatsink <-> HeatSink,
    pad_prop_castellated <-> Castellated,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Chamfer {
    pub top_left: bool,
    pub top_right: bool,
    pub bottom_left: bool,
    pub bottom_right: bool,
}

impl FromSexpr for Chamfer {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("chamfer")?;

        let top_left = parser.maybe_symbol_matching("top_left");
        let top_right = parser.maybe_symbol_matching("top_right");
        let bottom_left = parser.maybe_symbol_matching("bottom_left");
        let bottom_right = parser.maybe_symbol_matching("bottom_right");

        parser.expect_end()?;

        Ok(Self {
            top_left,
            top_right,
            bottom_left,
            bottom_right,
        })
    }
}

simple_maybe_from_sexpr!(Chamfer, chamfer);

impl ToSexpr for Chamfer {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "chamfer",
            [
                self.top_left.then(|| Sexpr::symbol("top_left")),
                self.top_right.then(|| Sexpr::symbol("top_right")),
                self.bottom_left.then(|| Sexpr::symbol("bottom_left")),
                self.bottom_right.then(|| Sexpr::symbol("bottom_right")),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Net {
    pub code: i32,
    pub name: String,
}

impl FromSexpr for Net {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("net")?;

        let code = parser.expect_number()? as i32;
        let name = parser.expect_string()?;

        parser.expect_end()?;

        Ok(Self { code, name })
    }
}

simple_maybe_from_sexpr!(Net, net);

impl ToSexpr for Net {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "net",
            [
                Some(Sexpr::number(self.code as f32)),
                Some(Sexpr::string(&self.name)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct CustomPadOptions {
    pub clearance: CustomPadClearanceKind,
    pub anchor: CustomPadAnchorShape,
}

impl FromSexpr for CustomPadOptions {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("options")?;

        let clearance = parser
            .expect_symbol_with_name("clearance")?
            .parse::<CustomPadClearanceKind>()?;
        let anchor = parser
            .expect_symbol_with_name("anchor")?
            .parse::<CustomPadAnchorShape>()?;

        parser.expect_end()?;

        Ok(Self { clearance, anchor })
    }
}

simple_maybe_from_sexpr!(CustomPadOptions, options);

impl ToSexpr for CustomPadOptions {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "options",
            [
                Some(Sexpr::symbol_with_name("clearance", self.clearance)),
                Some(Sexpr::symbol_with_name("anchor", self.anchor)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CustomPadClearanceKind {
    Outline,
    ConvexHull,
}

simple_to_from_string! {
    CustomPadClearanceKind,
    outline <-> Outline,
    convexhull <-> ConvexHull,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CustomPadAnchorShape {
    Rect,
    Circle,
}

simple_to_from_string! {
    CustomPadAnchorShape,
    rect <-> Rect,
    circle <-> Circle,
}
