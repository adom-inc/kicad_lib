//! Common structures related to sub-sheets within a schematic.

use kicad_sexpr::Sexpr;

use crate::{
    common::{symbol::SymbolProperty, Color, Position, Stroke, TextEffects, Uuid, Vec2D},
    convert::{
        FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName,
        VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, KiCadParseError,
};

use super::SheetPinShape;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Sheet {
    pub position: Position,
    pub size: Vec2D,
    pub exclude_from_sim: Option<bool>,
    pub in_bom: Option<bool>,
    pub on_board: Option<bool>,
    pub dnp: Option<bool>,
    pub fields_autoplaced: bool,
    pub stroke: Stroke,
    pub fill: Color,
    pub uuid: Uuid,
    pub properties: Vec<SymbolProperty>,
    pub pins: Vec<SheetHierarchicalPin>,
    pub instances: Option<Vec<SheetInstance>>,
}

impl FromSexpr for Sheet {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("sheet")?;

        let position = parser.expect::<Position>()?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let exclude_from_sim = parser.maybe_bool_with_name("exclude_from_sim")?;
        let in_bom = parser.maybe_bool_with_name("in_bom")?;
        let on_board = parser.maybe_bool_with_name("on_board")?;
        let dnp = parser.maybe_bool_with_name("dnp")?;
        let fields_autoplaced = parser.maybe_symbol_with_optional_boolean("fields_autoplaced");
        let stroke = parser.expect::<Stroke>()?;
        let fill = parser.expect_list_with_name("fill").and_then(|mut p| {
            let color = p.expect::<Color>()?;
            p.expect_end()?;

            Ok(color)
        })?;
        let uuid = parser.expect::<Uuid>()?;
        let properties = parser.expect_many::<SymbolProperty>()?;
        let pins = parser.expect_many::<SheetHierarchicalPin>()?;
        let instances = parser
            .maybe_list_with_name("instances")
            .map(|mut p| {
                let instances = p.expect_many::<SheetInstance>()?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(instances)
            })
            .transpose()?;

        parser.expect_end()?;

        Ok(Self {
            position,
            size,
            exclude_from_sim,
            in_bom,
            on_board,
            dnp,
            fields_autoplaced,
            stroke,
            fill,
            uuid,
            properties,
            pins,
            instances,
        })
    }
}

simple_maybe_from_sexpr!(Sheet, sheet);

impl ToSexpr for Sheet {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let fields_autoplaced = if context.explicit_booleans {
            self.fields_autoplaced
                .then(|| Sexpr::symbol_with_name("fields_autoplaced", "yes"))
        } else {
            self.fields_autoplaced
                .then(|| Sexpr::list_with_name("fields_autoplaced", []))
        };

        let exclude_from_sim = self
            .exclude_from_sim
            .map(|value| Sexpr::bool_with_name("exclude_from_sim", value));
        let in_bom = self
            .in_bom
            .map(|value| Sexpr::bool_with_name("in_bom", value));
        let on_board = self
            .on_board
            .map(|value| Sexpr::bool_with_name("on_board", value));
        let dnp = self.dnp.map(|value| Sexpr::bool_with_name("dnp", value));

        Sexpr::list_with_name(
            "sheet",
            [
                &[
                    Some(self.position.to_sexpr(context)),
                    Some(self.size.to_sexpr_with_name("size", context)),
                    exclude_from_sim,
                    in_bom,
                    on_board,
                    dnp,
                    fields_autoplaced,
                    Some(self.stroke.to_sexpr(context)),
                    Some(Sexpr::list_with_name(
                        "fill",
                        [Some(self.fill.to_sexpr(context))],
                    )),
                    Some(self.uuid.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
                &self.pins.into_sexpr_vec(context),
                &[self
                    .instances
                    .as_ref()
                    .map(|i| Sexpr::list_with_name("instances", i.into_sexpr_vec(context)))][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]

pub struct SheetHierarchicalPin {
    pub name: String,
    pub shape: SheetPinShape,
    pub position: Position,
    pub effects: TextEffects,
    pub uuid: Uuid,
    uuid_last: bool,
}

impl FromSexpr for SheetHierarchicalPin {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin")?;

        let name = parser.expect_string()?;
        let shape = parser.expect_symbol()?.parse::<SheetPinShape>()?;
        let position = parser.expect::<Position>()?;
        // KiCad seems happy with these in any order, and newer versions seem to prefer putting it
        // first, although older schematics updated to the latest format retain the original
        // ordering.
        // TODO: I think eventually we should parse these fields without taking order
        // into account and validate after.
        let mut uuid_last = false;
        let mut uuid = parser.maybe::<Uuid>()?;
        let effects = parser.expect::<TextEffects>()?;
        if uuid.is_none() {
            uuid = Some(parser.expect::<Uuid>()?);
            uuid_last = true;
        }
        let uuid = uuid.unwrap();

        parser.expect_end()?;

        Ok(Self {
            name,
            shape,
            position,
            effects,
            uuid,
            uuid_last,
        })
    }
}

simple_maybe_from_sexpr!(SheetHierarchicalPin, pin);

impl ToSexpr for SheetHierarchicalPin {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let uuid = if !self.uuid_last {
            Some(self.uuid.to_sexpr(context))
        } else {
            None
        };

        Sexpr::list_with_name(
            "pin",
            [
                Some(Sexpr::string(&self.name)),
                Some(Sexpr::symbol(self.shape)),
                Some(self.position.to_sexpr(context)),
                uuid,
                Some(self.effects.to_sexpr(context)),
                self.uuid_last.then_some(self.uuid.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SheetInstance {
    pub project: String,
    pub path: String,
    pub page: String,
}

impl FromSexpr for SheetInstance {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("project")?;

        let project = parser.expect_string()?;

        let (path, page) = parser.expect_list_with_name("path").and_then(|mut p| {
            let path = p.expect_string()?;
            let page = p.expect_string_with_name("page")?;
            p.expect_end()?;

            Ok((path, page))
        })?;

        parser.expect_end()?;

        Ok(Self {
            project,
            path,
            page,
        })
    }
}

simple_maybe_from_sexpr!(SheetInstance, project);

impl ToSexpr for SheetInstance {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "project",
            [
                Some(Sexpr::string(&self.project)),
                Some(Sexpr::list_with_name(
                    "path",
                    [
                        Some(Sexpr::string(&self.path)),
                        Some(Sexpr::string_with_name("page", &self.page)),
                    ],
                )),
            ],
        )
    }
}
