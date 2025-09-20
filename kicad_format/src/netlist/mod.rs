//! Netlist file format (`.net` files)
//!
//! This module provides structures and implementations for parsing and
//! generating KiCad netlist files. These files describe the electrical
//! connections between components in a circuit design.

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    convert::{FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr},
    KiCadParseError,
};

/// Represents the entire contents of a KiCad netlist file.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct NetlistFile {
    /// The version of the netlist file format.
    pub version: String,
    /// Information about the design.
    pub design: Design,
    /// List of components in the circuit.
    pub components: Vec<Component>,
    /// List of library parts used in the circuit.
    pub libparts: Vec<LibPart>,
    /// List of libraries referenced in the netlist.
    pub libraries: Vec<Library>,
    /// List of nets (electrical connections) in the circuit.
    pub nets: Vec<Net>,
}

impl FromSexpr for NetlistFile {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("export")?;

        let version = parser.expect_string_with_name("version")?;
        let design = parser.expect::<Design>()?;
        let components = parser
            .expect_list_with_name("components")
            .and_then(|mut p| {
                let components = p.expect_many::<Component>()?;
                Ok(components)
            })?;
        let libparts = parser.expect_list_with_name("libparts").and_then(|mut p| {
            let libparts = p.expect_many::<LibPart>()?;
            Ok(libparts)
        })?;
        let libraries = parser
            .expect_list_with_name("libraries")
            .and_then(|mut p| {
                let libraries = p.expect_many::<Library>()?;
                Ok(libraries)
            })?;
        let nets = parser.expect_list_with_name("nets").and_then(|mut p| {
            let nets = p.expect_many::<Net>()?;
            Ok(nets)
        })?;

        parser.expect_end()?;

        Ok(Self {
            version,
            design,
            components,
            libparts,
            libraries,
            nets,
        })
    }
}

impl ToSexpr for NetlistFile {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "export",
            [
                Some(Sexpr::string_with_name("version", &self.version)),
                Some(self.design.to_sexpr(context)),
                Some(Sexpr::list_with_name(
                    "components",
                    self.components
                        .iter()
                        .map(|i| i.to_sexpr(context))
                        .map(Some)
                        .collect::<Vec<_>>(),
                )),
                Some(Sexpr::list_with_name(
                    "libparts",
                    self.libparts
                        .iter()
                        .map(|i| i.to_sexpr(context))
                        .map(Some)
                        .collect::<Vec<_>>(),
                )),
                Some(Sexpr::list_with_name(
                    "libraries",
                    self.libraries
                        .iter()
                        .map(|i| i.to_sexpr(context))
                        .map(Some)
                        .collect::<Vec<_>>(),
                )),
                Some(Sexpr::list_with_name(
                    "nets",
                    self.nets
                        .iter()
                        .map(|i| i.to_sexpr(context))
                        .map(Some)
                        .collect::<Vec<_>>(),
                )),
            ],
        )
    }
}

/// Represents design information in the netlist.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Design {
    /// The source of the design (e.g., filename).
    pub source: String,
    /// The date the netlist was generated.
    pub date: String,
    /// The tool used to generate the netlist.
    pub tool: String,
    /// List of sheets in the design.
    pub sheets: Vec<Sheet>,
}

impl FromSexpr for Design {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("design")?;

        let source = parser.expect_string_with_name("source")?;
        let date = parser.expect_string_with_name("date")?;
        let tool = parser.expect_string_with_name("tool")?;
        let sheets = parser.expect_many::<Sheet>()?;

        parser.expect_end()?;

        Ok(Self {
            source,
            date,
            tool,
            sheets,
        })
    }
}

impl ToSexpr for Design {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "design",
            [
                Some(Sexpr::string_with_name("source", &self.source)),
                Some(Sexpr::string_with_name("date", &self.date)),
                Some(Sexpr::string_with_name("tool", &self.tool)),
            ]
            .into_iter()
            .chain(self.sheets.iter().map(|s| Some(s.to_sexpr(context))))
            .collect::<Vec<_>>(),
        )
    }
}

/// Represents a sheet in the design.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Sheet {
    /// The sheet number.
    pub number: String,
    /// The name of the sheet.
    pub name: String,
    /// Timestamp information for the sheet.
    pub tstamps: Vec<String>,
    /// Title block information for the sheet.
    pub title_block: TitleBlock,
}

impl MaybeFromSexpr for Sheet {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "sheet")
    }
}

impl FromSexpr for Sheet {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("sheet")?;

        let number = parser.expect_string_with_name("number")?;
        let name = parser.expect_string_with_name("name")?;
        let tstamps = parser
            .expect_list_with_name("tstamps")?
            .expect_many_strings()?;
        let title_block = parser.expect::<TitleBlock>()?;

        parser.expect_end()?;

        Ok(Self {
            number,
            name,
            tstamps,
            title_block,
        })
    }
}

impl ToSexpr for Sheet {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "sheet",
            [
                Some(Sexpr::string_with_name("number", &self.number)),
                Some(Sexpr::string_with_name("name", &self.name)),
                Some(Sexpr::list_with_name(
                    "tstamps",
                    self.tstamps
                        .iter()
                        .map(|s| Some(Sexpr::string(s)))
                        .collect::<Vec<_>>(),
                )),
                Some(self.title_block.to_sexpr(context)),
            ],
        )
    }
}

/// Represents the title block information for a sheet.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TitleBlock {
    /// The title of the sheet.
    pub title: Option<String>,
    /// The company name.
    pub company: Option<String>,
    /// The revision number.
    pub rev: Option<String>,
    /// The date of the sheet.
    pub date: Option<String>,
    /// The source of the sheet.
    pub source: String,
    /// List of comments in the title block.
    pub comments: Vec<Comment>,
}

impl FromSexpr for TitleBlock {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("title_block")?;

        let title = parser.expect_list_with_name("title")?.maybe_string();
        let company = parser.expect_list_with_name("company")?.maybe_string();
        let rev = parser.expect_list_with_name("rev")?.maybe_string();
        let date = parser.expect_list_with_name("date")?.maybe_string();
        let source = parser.expect_string_with_name("source")?;
        let comments = parser.expect_many::<Comment>()?;

        parser.expect_end()?;

        Ok(Self {
            title,
            company,
            rev,
            date,
            source,
            comments,
        })
    }
}

impl ToSexpr for TitleBlock {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "title_block",
            [
                Some(Sexpr::list_with_name(
                    "title",
                    match self.title.as_ref() {
                        Some(t) => vec![Some(Sexpr::string(t))],
                        None => vec![],
                    },
                )),
                Some(Sexpr::list_with_name(
                    "company",
                    match self.company.as_ref() {
                        Some(c) => vec![Some(Sexpr::string(c))],
                        None => vec![],
                    },
                )),
                Some(Sexpr::list_with_name(
                    "rev",
                    match self.rev.as_ref() {
                        Some(r) => vec![Some(Sexpr::string(r))],
                        None => vec![],
                    },
                )),
                Some(Sexpr::list_with_name(
                    "date",
                    match self.date.as_ref() {
                        Some(d) => vec![Some(Sexpr::string(d))],
                        None => vec![],
                    },
                )),
                Some(Sexpr::string_with_name("source", &self.source)),
            ]
            .into_iter()
            .chain(self.comments.iter().map(|c| Some(c.to_sexpr(context))))
            .collect::<Vec<_>>(),
        )
    }
}

/// Represents a comment in the title block.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Comment {
    /// The comment number.
    pub number: String,
    /// The content of the comment.
    pub value: String,
}

impl MaybeFromSexpr for Comment {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "comment")
    }
}

impl FromSexpr for Comment {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("comment")?;

        let number = parser.expect_string_with_name("number")?;
        let value = parser.expect_string_with_name("value")?;

        parser.expect_end()?;

        Ok(Self { number, value })
    }
}

impl ToSexpr for Comment {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "comment",
            [
                Some(Sexpr::string_with_name("number", &self.number)),
                Some(Sexpr::string_with_name("value", &self.value)),
            ],
        )
    }
}

/// Represents a component in the circuit.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Component {
    /// The reference designator of the component.
    pub ref_: String,
    /// The value of the component.
    pub value: String,
    /// The footprint of the component.
    pub footprint: Option<String>,
    /// The datasheet reference for the component.
    pub datasheet: Option<String>,
    /// A description of the component.
    pub description: Option<String>,
    /// Fields associated with the component.
    pub fields: Fields,
    /// The library source of the component.
    pub libsource: LibSource,
    /// Properties of the component.
    pub properties: Vec<Property>,
    /// The sheet path of the component.
    pub sheetpath: SheetPath,
    /// Timestamp information for the component.
    pub tstamps: Vec<String>,
}

impl MaybeFromSexpr for Component {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "comp")
    }
}

impl FromSexpr for Component {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("comp")?;

        let ref_ = parser.expect_string_with_name("ref")?;
        let value = parser.expect_string_with_name("value")?;
        let footprint = parser.maybe_string_with_name("footprint")?;
        let datasheet = parser.maybe_string_with_name("datasheet")?;
        let description = parser.maybe_string_with_name("description")?;
        let fields = parser.expect::<Fields>()?;
        let libsource = parser.expect::<LibSource>()?;
        let properties = parser.expect_many::<Property>()?;
        let sheetpath = parser.expect::<SheetPath>()?;
        let tstamps = parser
            .expect_list_with_name("tstamps")?
            .expect_many_strings()?;

        parser.expect_end()?;

        Ok(Self {
            ref_,
            value,
            footprint,
            datasheet,
            description,
            fields,
            libsource,
            properties,
            sheetpath,
            tstamps,
        })
    }
}

impl ToSexpr for Component {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "comp",
            [
                Some(Sexpr::string_with_name("ref", &self.ref_)),
                Some(Sexpr::string_with_name("value", &self.value)),
                self.footprint
                    .as_ref()
                    .map(|f| Sexpr::string_with_name("footprint", f)),
                self.datasheet
                    .as_ref()
                    .map(|d| Sexpr::string_with_name("datasheet", d)),
                self.description
                    .as_ref()
                    .map(|d| Sexpr::string_with_name("description", d)),
                Some(self.fields.to_sexpr(context)),
                Some(self.libsource.to_sexpr(context)),
            ]
            .into_iter()
            .chain(self.properties.iter().map(|p| Some(p.to_sexpr(context))))
            .chain([
                Some(self.sheetpath.to_sexpr(context)),
                Some(Sexpr::list_with_name(
                    "tstamps",
                    self.tstamps
                        .iter()
                        .map(|s| Some(Sexpr::string(s)))
                        .collect::<Vec<_>>(),
                )),
            ])
            .collect::<Vec<_>>(),
        )
    }
}

/// Represents the sheet path of a component.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SheetPath {
    /// The names of the sheets in the path.
    pub names: String,
    /// The timestamps of the sheets in the path.
    pub tstamps: String,
}

impl MaybeFromSexpr for SheetPath {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "sheetpath")
    }
}

impl FromSexpr for SheetPath {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("sheetpath")?;

        let names = parser.expect_string_with_name("names")?;
        let tstamps = parser.expect_string_with_name("tstamps")?;

        parser.expect_end()?;

        Ok(Self { names, tstamps })
    }
}

impl ToSexpr for SheetPath {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "sheetpath",
            [
                Some(Sexpr::string_with_name("names", &self.names)),
                Some(Sexpr::string_with_name("tstamps", &self.tstamps)),
            ],
        )
    }
}

/// Represents the library source of a component.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibSource {
    /// The library name.
    pub lib: String,
    /// The part name.
    pub part: String,
    /// The description of the part.
    pub description: String,
}

impl FromSexpr for LibSource {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("libsource")?;

        let lib = parser.expect_string_with_name("lib")?;
        let part = parser.expect_string_with_name("part")?;
        let description = parser.expect_string_with_name("description")?;

        parser.expect_end()?;

        Ok(Self {
            lib,
            part,
            description,
        })
    }
}

impl ToSexpr for LibSource {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "libsource",
            [
                Some(Sexpr::string_with_name("lib", &self.lib)),
                Some(Sexpr::string_with_name("part", &self.part)),
                Some(Sexpr::string_with_name("description", &self.description)),
            ],
        )
    }
}

/// Represents a property of a component.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Property {
    /// The name of the property.
    pub name: String,
    /// The value of the property.
    pub value: Option<String>,
}

impl MaybeFromSexpr for Property {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "property")
    }
}

impl FromSexpr for Property {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("property")?;

        let name = parser.expect_string_with_name("name")?;
        let value = parser.maybe_string_with_name("value")?;

        parser.expect_end()?;

        Ok(Self { name, value })
    }
}

impl ToSexpr for Property {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "property",
            [
                Some(Sexpr::string_with_name("name", &self.name)),
                self.value
                    .as_ref()
                    .map(|v| Sexpr::string_with_name("value", v)),
            ],
        )
    }
}

/// Represents a library part used in the circuit.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibPart {
    /// The library name.
    pub lib: String,
    /// The part name.
    pub part: String,
    /// The description of the part.
    pub description: Option<String>,
    /// The documentation reference for the part.
    pub docs: Option<String>,
    /// The footprints associated with the part.
    pub footprints: Option<Vec<Footprint>>,
    /// The fields associated with the part.
    pub fields: Vec<Field>,
    /// The pins associated with the part.
    pub pins: Option<Vec<Pin>>,
}

impl MaybeFromSexpr for LibPart {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "libpart")
    }
}

impl FromSexpr for LibPart {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("libpart")?;

        let lib = parser.expect_string_with_name("lib")?;
        let part = parser.expect_string_with_name("part")?;
        let description = parser.maybe_string_with_name("description")?;
        let docs = parser.maybe_string_with_name("docs")?;
        let footprints = parser
            .maybe_list_with_name("footprints")
            .map(|mut p| {
                let footprints = p.expect_many::<Footprint>()?;
                p.expect_end()?;
                Ok::<Option<Vec<Footprint>>, KiCadParseError>(Some(footprints))
            })
            .transpose()
            .map(|x| x.unwrap_or_default())?;
        let fields = parser.expect_list_with_name("fields").and_then(|mut p| {
            let fields = p.expect_many::<Field>()?;
            p.expect_end()?;
            Ok(fields)
        })?;
        let pins = parser
            .maybe_list_with_name("pins")
            .map(|mut p| {
                let pins = p.expect_many::<Pin>()?;
                p.expect_end()?;
                Ok::<Option<Vec<Pin>>, KiCadParseError>(Some(pins))
            })
            .transpose()
            .map(|x| x.unwrap_or_default())?;

        parser.expect_end()?;

        Ok(Self {
            lib,
            part,
            description,
            docs,
            footprints,
            fields,
            pins,
        })
    }
}

impl ToSexpr for LibPart {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "libpart",
            [
                Some(Sexpr::string_with_name("lib", &self.lib)),
                Some(Sexpr::string_with_name("part", &self.part)),
                self.description
                    .as_ref()
                    .map(|d| Sexpr::string_with_name("description", d)),
                self.docs
                    .as_ref()
                    .map(|d| Sexpr::string_with_name("docs", d)),
                self.footprints.as_ref().map(|f| {
                    Sexpr::list_with_name(
                        "footprints",
                        f.iter()
                            .map(|i| i.to_sexpr(context))
                            .map(Some)
                            .collect::<Vec<_>>(),
                    )
                }),
                Some(Sexpr::list_with_name(
                    "fields",
                    self.fields
                        .iter()
                        .map(|i| i.to_sexpr(context))
                        .map(Some)
                        .collect::<Vec<_>>(),
                )),
                self.pins.as_ref().map(|pins| {
                    Sexpr::list_with_name(
                        "pins",
                        pins.iter()
                            .map(|i| i.to_sexpr(context))
                            .map(Some)
                            .collect::<Vec<_>>(),
                    )
                }),
            ],
        )
    }
}

/// Represents the fields associated with a component or library part.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Fields {
    /// The list of fields.
    pub fields: Vec<Field>,
}

impl FromSexpr for Fields {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fields")?;

        let fields = parser.expect_many::<Field>()?;

        parser.expect_end()?;

        Ok(Self { fields })
    }
}

impl ToSexpr for Fields {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "fields",
            self.fields
                .iter()
                .map(|i| i.to_sexpr(context))
                .map(Some)
                .collect::<Vec<_>>(),
        )
    }
}

/// Represents a field associated with a component or library part.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    /// The name of the field.
    pub name: String,
    /// The value of the field.
    pub value: Option<String>,
}

impl MaybeFromSexpr for Field {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "field")
    }
}

impl FromSexpr for Field {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("field")?;

        let name = parser.expect_string_with_name("name")?;
        let value = parser.maybe_string();

        parser.expect_end()?;

        Ok(Self { name, value })
    }
}

impl ToSexpr for Field {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "field",
            [
                Some(Sexpr::string_with_name("name", &self.name)),
                self.value.as_ref().map(Sexpr::string),
            ],
        )
    }
}

/// Represents a pin associated with a library part.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Pin {
    /// The pin number.
    pub num: String,
    /// The name of the pin.
    pub name: String,
    /// The type of the pin.
    pub type_: String,
}

impl MaybeFromSexpr for Pin {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "pin")
    }
}

impl FromSexpr for Pin {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin")?;

        let num = parser.expect_string_with_name("num")?;
        let name = parser.expect_string_with_name("name")?;
        let type_ = parser.expect_string_with_name("type")?;

        parser.expect_end()?;

        Ok(Self { num, name, type_ })
    }
}

impl ToSexpr for Pin {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "pin",
            [
                Some(Sexpr::string_with_name("num", &self.num)),
                Some(Sexpr::string_with_name("name", &self.name)),
                Some(Sexpr::string_with_name("type", &self.type_)),
            ],
        )
    }
}
/// Represents a library referenced in the netlist.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Library {
    /// The logical name of the library.
    pub logical: String,
    /// The URI of the library.
    pub uri: String,
}

impl MaybeFromSexpr for Library {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "library")
    }
}

impl FromSexpr for Library {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("library")?;

        let logical = parser.expect_string_with_name("logical")?;
        let uri = parser.expect_string_with_name("uri")?;

        parser.expect_end()?;

        Ok(Self { logical, uri })
    }
}

impl ToSexpr for Library {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "library",
            [
                Some(Sexpr::string_with_name("logical", &self.logical)),
                Some(Sexpr::string_with_name("uri", &self.uri)),
            ],
        )
    }
}

/// Represents a footprint associated with a library part.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Footprint {
    /// The name of the footprint.
    pub name: String,
}

impl MaybeFromSexpr for Footprint {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "fp")
    }
}

impl FromSexpr for Footprint {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fp")?;

        let name = parser.expect_string()?;

        parser.expect_end()?;

        Ok(Self { name })
    }
}

impl ToSexpr for Footprint {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name("fp", [Some(Sexpr::string(&self.name))])
    }
}

/// Represents a net (electrical connection) in the circuit.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Net {
    /// The code (identifier) of the net.
    pub code: String,
    /// The name of the net.
    pub name: String,
    /// List of nodes (connection points) in the net.
    pub nodes: Vec<Node>,
}

impl MaybeFromSexpr for Net {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "net")
    }
}

impl FromSexpr for Net {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("net")?;

        let code = parser.expect_string_with_name("code")?;
        let name = parser.expect_string_with_name("name")?;
        let nodes = parser.expect_many::<Node>()?;

        parser.expect_end()?;

        Ok(Self { code, name, nodes })
    }
}

impl ToSexpr for Net {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "net",
            [
                Some(Sexpr::string_with_name("code", &self.code)),
                Some(Sexpr::string_with_name("name", &self.name)),
            ]
            .into_iter()
            .chain(self.nodes.iter().map(|n| Some(n.to_sexpr(context))))
            .collect::<Vec<_>>(),
        )
    }
}

/// Represents a node (connection point) in a net.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    /// The reference designator of the component connected to this node.
    pub ref_: String,
    /// The pin number of the component connected to this node.
    pub pin: String,
    /// The function of the pin (optional).
    pub pinfunction: Option<String>,
    /// The type of the pin (optional).
    pub pintype: Option<String>,
}

impl MaybeFromSexpr for Node {
    fn is_present(sexpr: &SexprList) -> bool {
        sexpr.first_symbol().is_some_and(|x| x == "node")
    }
}

impl FromSexpr for Node {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("node")?;

        let ref_ = parser.expect_string_with_name("ref")?;
        let pin = parser.expect_string_with_name("pin")?;
        let pinfunction = parser.maybe_string_with_name("pinfunction")?;
        let pintype = parser.maybe_string_with_name("pintype")?;

        parser.expect_end()?;

        Ok(Self {
            ref_,
            pin,
            pinfunction,
            pintype,
        })
    }
}

impl ToSexpr for Node {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "node",
            [
                Some(Sexpr::string_with_name("ref", &self.ref_)),
                Some(Sexpr::string_with_name("pin", &self.pin)),
                self.pinfunction
                    .as_ref()
                    .map(|f| Sexpr::string_with_name("pinfunction", f)),
                self.pintype
                    .as_ref()
                    .map(|t| Sexpr::string_with_name("pintype", t)),
            ],
        )
    }
}
