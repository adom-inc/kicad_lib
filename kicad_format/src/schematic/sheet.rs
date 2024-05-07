//! Common structures related to sub-sheets within a schematic.

use kicad_sexpr::Sexpr;

use crate::{
    common::{symbol::SymbolProperty, Color, Position, Stroke, TextEffects, Uuid, Vec2D},
    convert::{FromSexpr, Parser, SexprListExt, ToSexpr, ToSexprWithName, VecToMaybeSexprVec},
    simple_maybe_from_sexpr, KiCadParseError,
};

use super::SheetPinShape;

#[derive(Debug, PartialEq, Clone)]
pub struct Sheet {
    pub position: Position,
    pub size: Vec2D,
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
        let fields_autoplaced = parser.maybe_empty_list_with_name("fields_autoplaced")?;
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
    fn to_sexpr(&self) -> Sexpr {
        Sexpr::list_with_name(
            "sheet",
            [
                &[
                    Some(self.position.to_sexpr()),
                    Some(self.size.to_sexpr_with_name("size")),
                    self.fields_autoplaced
                        .then(|| Sexpr::list_with_name("fields_autoplaced", [])),
                    Some(self.stroke.to_sexpr()),
                    Some(Sexpr::list_with_name("fill", [Some(self.fill.to_sexpr())])),
                    Some(self.uuid.to_sexpr()),
                ][..],
                &self.properties.into_sexpr_vec(),
                &self.pins.into_sexpr_vec(),
                &[self
                    .instances
                    .as_ref()
                    .map(|i| Sexpr::list_with_name("instances", i.into_sexpr_vec()))][..],
            ]
            .concat(),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]

pub struct SheetHierarchicalPin {
    pub name: String,
    pub shape: SheetPinShape,
    pub position: Position,
    pub effects: TextEffects,
    pub uuid: Uuid,
}

impl FromSexpr for SheetHierarchicalPin {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin")?;

        let name = parser.expect_string()?;
        let shape = parser.expect_symbol()?.parse::<SheetPinShape>()?;
        let position = parser.expect::<Position>()?;
        let effects = parser.expect::<TextEffects>()?;
        let uuid = parser.expect::<Uuid>()?;

        parser.expect_end()?;

        Ok(Self {
            name,
            shape,
            position,
            effects,
            uuid,
        })
    }
}

simple_maybe_from_sexpr!(SheetHierarchicalPin, pin);

impl ToSexpr for SheetHierarchicalPin {
    fn to_sexpr(&self) -> Sexpr {
        Sexpr::list_with_name(
            "pin",
            [
                Some(Sexpr::string(&self.name)),
                Some(Sexpr::symbol(self.shape)),
                Some(self.position.to_sexpr()),
                Some(self.effects.to_sexpr()),
                Some(self.uuid.to_sexpr()),
            ],
        )
    }
}

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
    fn to_sexpr(&self) -> Sexpr {
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
