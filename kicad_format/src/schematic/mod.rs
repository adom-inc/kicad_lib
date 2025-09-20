//! Schematic file format (`.kicad_sch` files)

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    common::{
        shape::{Shape, ShapeFillMode},
        symbol::{LibSymbol, SymbolProperty},
        Color, CoordinatePointList, Image, PageSettings, Position, Stroke, TextEffects, TitleBlock,
        Uuid, Vec2D,
    },
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName, VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError,
};

use self::{sheet::Sheet, symbol::Symbol};

pub mod sheet;
pub mod symbol;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SheetPinShape {
    Unspecified,
    Input,
    Output,
    Bidirectional,
    TriState,
    Dot,
    Round,
    Diamond,
    Rectangle,
}

simple_to_from_string! {
    SheetPinShape,
    passive <-> Unspecified,
    input <-> Input,
    output <-> Output,
    bidirectional <-> Bidirectional,
    tri_state <-> TriState,
    dot <-> Dot,
    round <-> Round,
    diamond <-> Diamond,
    rectangle <-> Rectangle,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SchematicFile {
    pub version: u32,
    pub generator: String,
    pub generator_version: Option<String>,
    pub uuid: Uuid,
    pub page_settings: PageSettings,
    pub title_block: Option<TitleBlock>,
    pub lib_symbols: Vec<LibSymbol>,
    pub bus_aliases: Vec<BusAlias>,
    pub junctions: Vec<Junction>,
    pub no_connects: Vec<NoConnect>,
    pub bus_entries: Vec<BusEntry>,
    pub lines: Vec<SchematicLine>,
    pub shapes: Vec<Shape>,
    pub images: Vec<Image>,
    pub text_boxes: Vec<SchematicTextBox>,
    pub texts: Vec<SchematicText>,
    pub local_labels: Vec<LocalLabel>,
    pub global_labels: Vec<GlobalLabel>,
    pub hierarchical_labels: Vec<HierarchicalLabel>,
    pub symbols: Vec<Symbol>,
    pub sheets: Vec<Sheet>,
    pub sheet_instances: Option<Vec<SchematicSheetInstance>>,
    pub embedded_fonts: Option<bool>,
}

impl FromSexpr for SchematicFile {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("kicad_sch")?;

        let version = parser.expect_number_with_name("version")? as u32;
        let generator = parser.expect_symbol_or_string_with_name("generator")?;
        let generator_version = parser.maybe_string_with_name("generator_version")?;
        let uuid = parser.expect::<Uuid>()?;
        let page_settings = parser.expect::<PageSettings>()?;
        let title_block = parser.maybe::<TitleBlock>()?;
        let lib_symbols = parser
            .expect_list_with_name("lib_symbols")
            .and_then(|mut p| {
                let symbols = p.expect_many::<LibSymbol>()?;
                p.expect_end()?;

                Ok(symbols)
            })?;
        let bus_aliases = parser.expect_many::<BusAlias>()?;
        let junctions = parser.expect_many::<Junction>()?;
        let no_connects = parser.expect_many::<NoConnect>()?;
        let bus_entries = parser.expect_many::<BusEntry>()?;
        let lines = parser.expect_many::<SchematicLine>()?;
        let shapes = parser.expect_many::<Shape>()?;
        let images = parser.expect_many::<Image>()?;
        let text_boxes = parser.expect_many::<SchematicTextBox>()?;
        let texts = parser.expect_many::<SchematicText>()?;
        let local_labels = parser.expect_many::<LocalLabel>()?;
        let global_labels = parser.expect_many::<GlobalLabel>()?;
        let hierarchical_labels = parser.expect_many::<HierarchicalLabel>()?;
        let symbols = parser.expect_many::<Symbol>()?;
        let sheets = parser.expect_many::<Sheet>()?;
        let sheet_instances = parser
            .maybe_list_with_name("sheet_instances")
            .map(|mut p| {
                let instances = p.expect_many::<SchematicSheetInstance>()?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(instances)
            })
            .transpose()?;
        let embedded_fonts = parser.maybe_bool_with_name("embedded_fonts")?;

        parser.expect_end()?;

        Ok(Self {
            version,
            generator,
            generator_version,
            uuid,
            page_settings,
            title_block,
            lib_symbols,
            bus_aliases,
            junctions,
            no_connects,
            bus_entries,
            lines,
            shapes,
            images,
            text_boxes,
            texts,
            local_labels,
            global_labels,
            hierarchical_labels,
            symbols,
            sheets,
            sheet_instances,
            embedded_fonts,
        })
    }
}

impl ToSexpr for SchematicFile {
    fn to_sexpr(&self, _context: SerializationContext) -> kicad_sexpr::Sexpr {
        // Around v8, a normalization effort took place and many core fields are serialized
        // differently. The existence of the `generator_version` field is a signal that we're
        // working with the updated format, and can follow those rules instead.
        let is_normalized = self.generator_version.is_some();
        let context = if is_normalized {
            SerializationContext::default()
        } else {
            SerializationContext::pre_v8()
        };

        let generator = if is_normalized {
            Sexpr::string_with_name("generator", &self.generator)
        } else {
            Sexpr::symbol_with_name("generator", &self.generator)
        };

        let generator_version = self
            .generator_version
            .as_ref()
            .map(|gv| Sexpr::string_with_name("generator_version", gv));

        let embedded_fonts = self
            .embedded_fonts
            .map(|value| Sexpr::bool_with_name("embedded_fonts", value));

        Sexpr::list_with_name(
            "kicad_sch",
            [
                &[
                    Some(Sexpr::number_with_name("version", self.version as f32)),
                    Some(generator),
                    generator_version,
                    Some(self.uuid.to_sexpr(context)),
                    Some(self.page_settings.to_sexpr(context)),
                    self.title_block.as_ref().map(|i| i.to_sexpr(context)),
                    Some(Sexpr::list_with_name(
                        "lib_symbols",
                        self.lib_symbols.into_sexpr_vec(context),
                    )),
                ][..],
                &self.bus_aliases.into_sexpr_vec(context),
                &self.junctions.into_sexpr_vec(context),
                &self.no_connects.into_sexpr_vec(context),
                &self.bus_entries.into_sexpr_vec(context),
                &self.lines.into_sexpr_vec(context),
                &self.shapes.into_sexpr_vec(context),
                &self.images.into_sexpr_vec(context),
                &self.text_boxes.into_sexpr_vec(context),
                &self.texts.into_sexpr_vec(context),
                &self.local_labels.into_sexpr_vec(context),
                &self.global_labels.into_sexpr_vec(context),
                &self.hierarchical_labels.into_sexpr_vec(context),
                &self.symbols.into_sexpr_vec(context),
                &self.sheets.into_sexpr_vec(context),
                &[self
                    .sheet_instances
                    .as_ref()
                    .map(|s| Sexpr::list_with_name("sheet_instances", s.into_sexpr_vec(context)))]
                    [..],
                &[embedded_fonts][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct BusAlias {
    pub name: String,
    pub members: Vec<String>,
}

impl FromSexpr for BusAlias {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("bus_alias")?;

        let name = parser.expect_string()?;
        let members = parser.expect_list_with_name("members").and_then(|mut p| {
            let members = p.expect_many_strings()?;

            p.expect_end()?;

            Ok(members)
        })?;

        parser.expect_end()?;

        Ok(Self { name, members })
    }
}

simple_maybe_from_sexpr!(BusAlias, bus_alias);

impl ToSexpr for BusAlias {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "bus_alias",
            [
                Some(Sexpr::string(&self.name)),
                Some(Sexpr::list_with_name(
                    "members",
                    self.members
                        .iter()
                        .map(Sexpr::string)
                        .map(Option::Some)
                        .collect::<Vec<_>>(),
                )),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Junction {
    pub position: Position,
    pub diameter: f32,
    pub color: Color,
    pub uuid: Uuid,
}

impl FromSexpr for Junction {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("junction")?;

        let position = parser.expect::<Position>()?;
        let diameter = parser.expect_number_with_name("diameter")?;
        let color = parser.expect::<Color>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            position,
            diameter,
            color,
            uuid,
        })
    }
}

simple_maybe_from_sexpr!(Junction, junction);

impl ToSexpr for Junction {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "junction",
            [
                Some(self.position.to_sexpr(context)),
                Some(Sexpr::number_with_name("diameter", self.diameter)),
                Some(self.color.to_sexpr(context)),
                Some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct NoConnect {
    pub position: Position,
    pub uuid: Uuid,
}

impl FromSexpr for NoConnect {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("no_connect")?;

        let position = parser.expect::<Position>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self { position, uuid })
    }
}

simple_maybe_from_sexpr!(NoConnect, no_connect);

impl ToSexpr for NoConnect {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "no_connect",
            [
                Some(self.position.to_sexpr(context)),
                Some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct BusEntry {
    pub position: Position,
    pub size: Vec2D,
    pub stroke: Stroke,
    pub uuid: Uuid,
}

impl FromSexpr for BusEntry {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("bus_entry")?;

        let position = parser.expect::<Position>()?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let stroke = parser.expect::<Stroke>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            position,
            size,
            stroke,
            uuid,
        })
    }
}

simple_maybe_from_sexpr!(BusEntry, bus_entry);

impl ToSexpr for BusEntry {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "bus_entry",
            [
                Some(self.position.to_sexpr(context)),
                Some(self.size.to_sexpr_with_name("size", context)),
                Some(self.stroke.to_sexpr(context)),
                Some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

/// Wires, Buses, or PolyLines
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SchematicLine {
    pub kind: SchematicLineKind,
    pub points: CoordinatePointList,
    pub stroke: Stroke,
    pub uuid: Uuid,
}

impl FromSexpr for SchematicLine {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let kind = parser.expect_symbol()?.parse::<SchematicLineKind>()?;

        let points = parser.expect::<CoordinatePointList>()?;
        let stroke = parser.expect::<Stroke>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            kind,
            points,
            stroke,
            uuid,
        })
    }
}

impl MaybeFromSexpr for SchematicLine {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr
            .first_symbol()
            .is_some_and(|s| matches!(s, "wire" | "bus" | "polyline"))
    }
}

impl ToSexpr for SchematicLine {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            self.kind.to_string(),
            [
                Some(self.points.to_sexpr(context)),
                Some(self.stroke.to_sexpr(context)),
                Some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub enum SchematicLineKind {
    Wire,
    Bus,
    PolyLine,
}

simple_to_from_string! {
    SchematicLineKind,
    wire <-> Wire,
    bus <-> Bus,
    polyline <-> PolyLine,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SchematicTextBox {
    pub text: String,
    pub position: Position,
    pub size: Vec2D,
    pub stroke: Stroke,
    pub fill: ShapeFillMode,
    pub effects: TextEffects,
    pub uuid: Option<Uuid>,
}

impl FromSexpr for SchematicTextBox {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("text_box")?;

        let text = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let stroke = parser.expect::<Stroke>()?;
        let fill = parser.expect::<ShapeFillMode>()?;
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.maybe::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            text,
            position,
            size,
            stroke,
            fill,
            effects,
            uuid,
        })
    }
}

simple_maybe_from_sexpr!(SchematicTextBox, text_box);

impl ToSexpr for SchematicTextBox {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "text_box",
            [
                Some(Sexpr::string(&self.text)),
                Some(self.position.to_sexpr(context)),
                Some(self.size.to_sexpr_with_name("size", context)),
                Some(self.stroke.to_sexpr(context)),
                Some(self.fill.to_sexpr(context)),
                Some(self.effects.to_sexpr(context)),
                self.uuid.as_ref().map(|i| i.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SchematicText {
    pub text: String,
    pub exclude_from_sim: Option<bool>,
    pub position: Position,
    pub effects: TextEffects,
    pub uuid: Uuid,
}

impl FromSexpr for SchematicText {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("text")?;

        let text = parser.expect_string()?;
        let exclude_from_sim = parser.maybe_bool_with_name("exclude_from_sim")?;
        let position = parser.expect::<Position>()?;
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            text,
            exclude_from_sim,
            position,
            effects,
            uuid,
        })
    }
}

simple_maybe_from_sexpr!(SchematicText, text);

impl ToSexpr for SchematicText {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "text",
            [
                Some(Sexpr::string(&self.text)),
                Some(self.position.to_sexpr(context)),
                Some(self.effects.to_sexpr(context)),
                Some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LocalLabel {
    pub text: String,
    pub position: Position,
    pub fields_autoplaced: bool,
    pub effects: TextEffects,
    pub uuid: Uuid,
    pub properties: Vec<SymbolProperty>,
}

impl FromSexpr for LocalLabel {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("label")?;

        let text = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let fields_autoplaced = parser.maybe_symbol_with_optional_boolean("fields_autoplaced");
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.expect::<Uuid>()?;
        let properties = parser.expect_many::<SymbolProperty>()?;

        parser.expect_end()?;

        Ok(Self {
            text,
            position,
            fields_autoplaced,
            effects,
            uuid,
            properties,
        })
    }
}

simple_maybe_from_sexpr!(LocalLabel, label);

impl ToSexpr for LocalLabel {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let fields_autoplaced = if context.explicit_booleans {
            self.fields_autoplaced
                .then(|| Sexpr::symbol_with_name("fields_autoplaced", "yes"))
        } else {
            self.fields_autoplaced
                .then(|| Sexpr::list_with_name("fields_autoplaced", []))
        };

        Sexpr::list_with_name(
            "label",
            [
                &[
                    Some(Sexpr::string(&self.text)),
                    Some(self.position.to_sexpr(context)),
                    fields_autoplaced,
                    Some(self.effects.to_sexpr(context)),
                    Some(self.uuid.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct GlobalLabel {
    pub text: String,
    pub shape: SheetPinShape,
    pub position: Position,
    pub fields_autoplaced: bool,
    pub effects: TextEffects,
    pub uuid: Uuid,
    pub properties: Vec<SymbolProperty>,
}

impl FromSexpr for GlobalLabel {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("global_label")?;

        let text = parser.expect_string()?;
        let shape = parser
            .expect_symbol_with_name("shape")?
            .parse::<SheetPinShape>()?;
        let position = parser.expect::<Position>()?;
        let fields_autoplaced = parser.maybe_symbol_with_optional_boolean("fields_autoplaced");
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.expect::<Uuid>()?;
        let properties = parser.expect_many::<SymbolProperty>()?;

        parser.expect_end()?;

        Ok(Self {
            text,
            shape,
            position,
            fields_autoplaced,
            effects,
            uuid,
            properties,
        })
    }
}

simple_maybe_from_sexpr!(GlobalLabel, global_label);

impl ToSexpr for GlobalLabel {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let fields_autoplaced = if context.explicit_booleans {
            self.fields_autoplaced
                .then(|| Sexpr::symbol_with_name("fields_autoplaced", "yes"))
        } else {
            self.fields_autoplaced
                .then(|| Sexpr::list_with_name("fields_autoplaced", []))
        };

        Sexpr::list_with_name(
            "global_label",
            [
                &[
                    Some(Sexpr::string(&self.text)),
                    Some(Sexpr::symbol_with_name("shape", self.shape)),
                    Some(self.position.to_sexpr(context)),
                    fields_autoplaced,
                    Some(self.effects.to_sexpr(context)),
                    Some(self.uuid.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

// TODO: This is a duplicate of the GlobalLabel struct. It should be refactored to use the same struct.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct HierarchicalLabel {
    pub text: String,
    pub shape: SheetPinShape,
    pub position: Position,
    pub fields_autoplaced: bool,
    pub effects: TextEffects,
    pub uuid: Uuid,
    pub properties: Vec<SymbolProperty>,
}

impl FromSexpr for HierarchicalLabel {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("hierarchical_label")?;

        let text = parser.expect_string()?;
        let shape = parser
            .expect_symbol_with_name("shape")?
            .parse::<SheetPinShape>()?;
        let position = parser.expect::<Position>()?;
        let fields_autoplaced = parser.maybe_symbol_with_optional_boolean("fields_autoplaced");
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.expect::<Uuid>()?;
        let properties = parser.expect_many::<SymbolProperty>()?;

        parser.expect_end()?;

        Ok(Self {
            text,
            shape,
            position,
            fields_autoplaced,
            effects,
            uuid,
            properties,
        })
    }
}

simple_maybe_from_sexpr!(HierarchicalLabel, hierarchical_label);

impl ToSexpr for HierarchicalLabel {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let fields_autoplaced = if context.explicit_booleans {
            self.fields_autoplaced
                .then(|| Sexpr::symbol_with_name("fields_autoplaced", "yes"))
        } else {
            self.fields_autoplaced
                .then(|| Sexpr::list_with_name("fields_autoplaced", []))
        };

        Sexpr::list_with_name(
            "hierarchical_label",
            [
                &[
                    Some(Sexpr::string(&self.text)),
                    Some(Sexpr::symbol_with_name("shape", self.shape)),
                    Some(self.position.to_sexpr(context)),
                    fields_autoplaced,
                    Some(self.effects.to_sexpr(context)),
                    Some(self.uuid.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SchematicSheetInstance {
    pub path: String,
    pub page: String,
}

impl FromSexpr for SchematicSheetInstance {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("path")?;

        let path = parser.expect_string()?;
        let page = parser.expect_string_with_name("page")?;

        parser.expect_end()?;

        Ok(Self { path, page })
    }
}

simple_maybe_from_sexpr!(SchematicSheetInstance, path);

impl ToSexpr for SchematicSheetInstance {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "path",
            [
                Some(Sexpr::string(&self.path)),
                Some(Sexpr::string_with_name("page", &self.page)),
            ],
        )
    }
}
