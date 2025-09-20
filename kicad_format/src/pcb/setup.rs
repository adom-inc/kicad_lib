//! Common structures related to the PCB `setup` field in the file header

use std::str::FromStr;

use kicad_sexpr::Sexpr;

use crate::{
    common::{LayerId, Vec2D},
    convert::{
        FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName,
        VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError,
};

/// Properties of the PCB such as physical layer stackup, clearances, origins,
/// and plot options
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct BoardSetup {
    pub stackup: Option<BoardStackup>,
    pub pad_to_mask_clearance: f32,
    pub solder_mask_min_width: Option<f32>,
    pub pad_to_paste_clearance: Option<f32>,
    pub pad_to_paste_clearance_ratio: Option<f32>,
    pub allow_soldermask_bridges_in_footprints: bool,
    pub aux_axis_origin: Option<Vec2D>,
    pub grid_origin: Option<Vec2D>,
    pub plot_options: PcbPlotOptions,
}

impl FromSexpr for BoardSetup {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("setup")?;

        let stackup = parser.maybe::<BoardStackup>()?;
        let pad_to_mask_clearance = parser.expect_number_with_name("pad_to_mask_clearance")?;
        let solder_mask_min_width = parser.maybe_number_with_name("solder_mask_min_width")?;
        let pad_to_paste_clearance = parser.maybe_number_with_name("pad_to_paste_clearance")?;
        let pad_to_paste_clearance_ratio =
            parser.maybe_number_with_name("pad_to_paste_clearance_ratio")?;
        let allow_soldermask_bridges_in_footprints = parser
            .maybe_list_with_name("allow_soldermask_bridges_in_footprints")
            .map(|mut p| {
                p.expect_symbol_matching("yes")?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(())
            })
            .transpose()?
            .is_some();
        let aux_axis_origin = parser.maybe_with_name::<Vec2D>("aux_axis_origin")?;
        let grid_origin = parser.maybe_with_name::<Vec2D>("grid_origin")?;
        let plot_options = parser.expect::<PcbPlotOptions>()?;

        parser.expect_end()?;

        Ok(Self {
            stackup,
            pad_to_mask_clearance,
            solder_mask_min_width,
            pad_to_paste_clearance,
            pad_to_paste_clearance_ratio,
            allow_soldermask_bridges_in_footprints,
            aux_axis_origin,
            grid_origin,
            plot_options,
        })
    }
}

impl ToSexpr for BoardSetup {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "setup",
            [
                self.stackup.as_ref().map(|i| i.to_sexpr(context)),
                Some(Sexpr::number_with_name(
                    "pad_to_mask_clearance",
                    self.pad_to_mask_clearance,
                )),
                self.solder_mask_min_width
                    .map(|w| Sexpr::number_with_name("solder_mask_min_width", w)),
                self.pad_to_paste_clearance
                    .map(|p| Sexpr::number_with_name("pad_to_paste_clearance", p)),
                self.pad_to_paste_clearance_ratio
                    .map(|p| Sexpr::number_with_name("pad_to_paste_clearance_ratio", p)),
                self.allow_soldermask_bridges_in_footprints.then(|| {
                    Sexpr::symbol_with_name("allow_soldermask_bridges_in_footprints", "yes")
                }),
                self.aux_axis_origin
                    .as_ref()
                    .map(|a| a.to_sexpr_with_name("aux_axis_origin", context)),
                self.grid_origin
                    .as_ref()
                    .map(|g| g.to_sexpr_with_name("grid_origin", context)),
                Some(self.plot_options.to_sexpr(context)),
            ],
        )
    }
}

// ############################################################################

/// The physical layer stackup of the PCB
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct BoardStackup {
    pub layers: Vec<StackupLayer>,
    pub copper_finish: Option<String>,
    pub dielectric_constraints: bool,
    pub edge_connector: Option<EdgeConnectorConstraints>,
    pub castellated_pads: bool,
    pub edge_plating: bool,
}

impl FromSexpr for BoardStackup {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("stackup")?;

        let layers = parser.expect_many::<StackupLayer>()?;
        let copper_finish = parser.maybe_string_with_name("copper_finish")?;
        let dielectric_constraints = parser.expect_bool_with_name("dielectric_constraints")?;
        let edge_connector = parser
            .maybe_string_with_name("edge_connector")?
            .map(|s| s.parse::<EdgeConnectorConstraints>())
            .transpose()?;
        let castellated_pads = parser
            .maybe_list_with_name("castellated_pads")
            .map(|mut p| {
                p.expect_symbol_matching("yes")?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(())
            })
            .transpose()?
            .is_some();
        let edge_plating = parser
            .maybe_list_with_name("edge_plating")
            .map(|mut p| {
                p.expect_symbol_matching("yes")?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(())
            })
            .transpose()?
            .is_some();

        parser.expect_end()?;

        Ok(Self {
            layers,
            copper_finish,
            dielectric_constraints,
            edge_connector,
            castellated_pads,
            edge_plating,
        })
    }
}

simple_maybe_from_sexpr!(BoardStackup, stackup);

impl ToSexpr for BoardStackup {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "stackup",
            [
                &self.layers.into_sexpr_vec(context),
                &[
                    self.copper_finish
                        .as_ref()
                        .map(|c| Sexpr::string_with_name("copper_finish", c)),
                    Some(Sexpr::bool_with_name(
                        "dielectric_constraints",
                        self.dielectric_constraints,
                    )),
                    self.edge_connector
                        .as_ref()
                        .map(|e| Sexpr::string_with_name("edge_connector", e.to_string())),
                    self.castellated_pads
                        .then(|| Sexpr::symbol_with_name("castellated_pads", "yes")),
                    self.edge_plating
                        .then(|| Sexpr::symbol_with_name("edge_plating", "yes")),
                ][..],
            ]
            .concat(),
        )
    }
}

/// A physical layer in the PCB stackup. This can be either a canonical layer
/// or a dielectric layer (FR4)
///
/// FIXME: Intentionally not supporting the `addsublayer` token in dielectric
/// layers because it's not common and I dont feel like parsing that :)
///
/// FIXME: Intentionally not supporting the `locked` token within the
/// `thickness` token attribute because it's not common and I dont feel like
/// parsing that :)
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct StackupLayer {
    pub id: StackupLayerId,
    pub kind: String,
    pub color: Option<String>,
    pub thickness: Option<f32>,
    pub material: Option<String>,
    pub epsilon_r: Option<f32>,
    pub loss_tangent: Option<f32>,
}

impl FromSexpr for StackupLayer {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("layer")?;

        let id = parser.expect_string()?.parse::<StackupLayerId>()?;
        let kind = parser.expect_string_with_name("type")?;
        let color = parser.maybe_string_with_name("color")?;
        let thickness = parser.maybe_number_with_name("thickness")?;
        let material = parser.maybe_string_with_name("material")?;
        let epsilon_r = parser.maybe_number_with_name("epsilon_r")?;
        let loss_tangent = parser.maybe_number_with_name("loss_tangent")?;

        parser.expect_end()?;

        Ok(Self {
            id,
            kind,
            color,
            thickness,
            material,
            epsilon_r,
            loss_tangent,
        })
    }
}

simple_maybe_from_sexpr!(StackupLayer, layer);

impl ToSexpr for StackupLayer {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "layer",
            [
                Some(self.id.to_sexpr(context)),
                Some(Sexpr::string_with_name("type", &self.kind)),
                self.color
                    .as_ref()
                    .map(|c| Sexpr::string_with_name("color", c)),
                self.thickness
                    .map(|t| Sexpr::number_with_name("thickness", t)),
                self.material
                    .as_ref()
                    .map(|m| Sexpr::string_with_name("material", m)),
                self.epsilon_r
                    .map(|e| Sexpr::number_with_name("epsilon_r", e)),
                self.loss_tangent
                    .map(|l| Sexpr::number_with_name("loss_tangent", l)),
            ],
        )
    }
}

/// Layers in the board stackup are either defined board layers or dielectric
/// layers (i.e. FR4)
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
#[derive(Debug, PartialEq, Clone)]
pub enum StackupLayerId {
    BoardLayer(LayerId),
    /// Dielectric layers store an ID from 1 (top) to 31 (bottom).
    ///
    /// There are only 31 dielectric layers for 32 copper layers (one
    /// dielectric layer between each copper layer).
    Dielectric(u8),
}

impl FromStr for StackupLayerId {
    type Err = KiCadParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(id) = s.parse::<LayerId>() {
            return Ok(Self::BoardLayer(id));
        }

        if !s.starts_with("dielectric ") {
            return Err(KiCadParseError::InvalidLayer(s.to_string()));
        }

        let id = s.strip_prefix("dielectric ").unwrap();

        id.parse::<u8>()
            .map(Self::Dielectric)
            .map_err(|_| KiCadParseError::InvalidLayer(s.to_string()))
    }
}

impl ToSexpr for StackupLayerId {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::string(match self {
            StackupLayerId::BoardLayer(layer_id) => (*layer_id).into(),
            StackupLayerId::Dielectric(id) => format!("dielectric {}", id),
        })
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub enum EdgeConnectorConstraints {
    /// No edge connector in board
    None,
    /// Some edge connector in board
    InUse,
    /// Some connector in board, and the connector must be beveled
    Bevelled,
}

simple_to_from_string! {
    EdgeConnectorConstraints,
    none <-> None,
    yes <-> InUse,
    bevelled <-> Bevelled
}

// ############################################################################

/// Large field of settings that control how the PCB is plotted
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbPlotOptions {
    /// Set of layers to plot
    ///
    /// FIXME: update to use layer set/mask
    pub layer_selection: u64,
    /// Honestly no idea what this does
    ///
    /// Code says this:
    /// > "Set of layers that get plotted on each of the layers to plot."
    ///
    /// FIXME: update to use layer set/mask
    pub plot_on_all_layers_selection: u64,

    /// Disable aperture macros in Gerber format (only for broken Gerber readers)
    /// Ideally, should be never selected
    pub disable_aperture_macros: bool,
    /// When plotting gerber files, use a conventional set of Protel extensions
    /// instead of .gbr, that is now the official gerber file extension
    #[deprecated]
    pub use_gerber_extensions: bool,
    /// Include attributes from the Gerber X2 format (chapter 5 in revision J2)
    pub use_gerber_attributes: bool,
    /// Include net list info (only in Gerber X2 format) (chapter ? in revision ?)
    pub use_gerber_advanced_attributes: bool,
    /// Generate the auxiliary "job file" in gerber format
    pub create_gerber_job_file: bool,
    /// Precision of coordinates in Gerber files: accepted 5 or 6
    /// when units are in mm (6 or 7 in inches, but Pcbnew uses mm).
    /// 6 is the internal resolution of Pcbnew, but not always accepted by board maker
    /// 5 is the minimal value for professional boards.
    pub gerber_precision: Option<f32>,

    pub dashed_line_dash_ratio: f32,
    pub dashed_line_gap_ratio: f32,

    /// Precision of coordinates in SVG files: accepted 3 - 6
    /// 6 is the internal resolution of Pcbnew
    pub svg_precision: u32,

    /// Whether or not to plot/print frame references
    pub plot_frame_ref: bool,
    /// True if vias are drawn on Mask layer (ie untented, *exposed* by mask)
    pub vias_on_mask: bool,
    /// Filled or Sketch selects how to plot filled objects.
    ///
    /// Filled or Sketch not available with all drivers: some have fixed mode
    ///
    /// NOTE: Sketch gets serialized as 2 and not 0 (ㆆ_ㆆ)
    pub plot_mode: OutlineMode,
    /// Plot gerbers using auxiliary (drill) origin instead of absolute coordinates
    pub use_aux_origin: bool,

    /// HPGL only: pen number selection (1 to 9)
    pub hpgl_pen_number: i32,
    /// HPGL only: pen speed, always in cm/s (1 to 99 cm/s)
    pub hpgl_pen_speed: i32,
    /// HPGL only: pen diameter in MILS, useful to fill areas (However, it is
    /// in mm in hpgl files.)
    pub hpgl_pen_diameter: f32,

    /// DXF format: Plot items in outline (polygon) mode.
    ///
    /// In polygon mode, each item to plot is converted to a polygon and all
    /// polygons are merged.
    pub dxf_use_polygon_mode: bool,
    /// Use imperial units when plotting the DXF
    pub dxf_use_imperial_units: bool,
    /// Defines if the Pcbnew font (vector font) or the default font should be
    /// used for DXF plots.
    pub dxf_use_pcbnew_font: bool,

    /// Plot in negative color (supported only by some drivers)
    pub postscript_negative: bool,
    /// Auto-scale the plot to fit an A4 (landscape?) sheet
    pub postscript_a4_output: bool,

    /// Enable plotting of part references
    pub plot_references: bool,
    /// Enable plotting of part values
    pub plot_values: bool,
    /// Force plotting of fields marked invisible
    pub plot_invisible_text: bool,

    /// Plots pads outlines on fab layers
    pub sketch_pads_on_fab: bool,
    /// On gerbers 'scrape' away the solder mask from silkscreen (trim silks)
    pub subtract_mask_from_silk: bool,

    /// Plot format type (chooses the driver to be used)
    pub output_format: PlotFormat,
    /// Mirror the plot around the X axis
    pub mirror: bool,
    /// Holes can be not plotted, have a small mark or plotted in actual size
    pub drill_shape: DrillMarks,
    /// Scale ratio index (UI only)
    pub scale_selection: i32,
    /// Output directory for plot files (usually relative to the board file)
    pub output_directory: String,
}

#[allow(deprecated)]
impl FromSexpr for PcbPlotOptions {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pcbplotparams")?;

        let layer_selection = parse_bit_field(&parser.expect_symbol_with_name("layerselection")?)?;
        let plot_on_all_layers_selection =
            parse_bit_field(&parser.expect_symbol_with_name("plot_on_all_layers_selection")?)?;
        let disable_aperture_macros = parser.expect_alt_bool_with_name("disableapertmacros")?;
        let use_gerber_extensions = parser.expect_alt_bool_with_name("usegerberextensions")?;
        let use_gerber_attributes = parser.expect_alt_bool_with_name("usegerberattributes")?;
        let use_gerber_advanced_attributes =
            parser.expect_alt_bool_with_name("usegerberadvancedattributes")?;
        let create_gerber_job_file = parser.expect_alt_bool_with_name("creategerberjobfile")?;
        let gerber_precision = parser.maybe_number_with_name("gerberprecision")?;
        let dashed_line_dash_ratio = parser.expect_number_with_name("dashed_line_dash_ratio")?;
        let dashed_line_gap_ratio = parser.expect_number_with_name("dashed_line_gap_ratio")?;
        let svg_precision = parser.expect_number_with_name("svgprecision")? as u32;
        let plot_frame_ref = parser.expect_alt_bool_with_name("plotframeref")?;
        let vias_on_mask = parser.expect_alt_bool_with_name("viasonmask")?;
        let plot_mode = parser
            .expect_number_with_name("mode")
            .map(|m| m as u8)
            .map(OutlineMode::try_from)??;
        let use_aux_origin = parser.expect_alt_bool_with_name("useauxorigin")?;
        let hpgl_pen_number = parser.expect_number_with_name("hpglpennumber")? as i32;
        let hpgl_pen_speed = parser.expect_number_with_name("hpglpenspeed")? as i32;
        let hpgl_pen_diameter = parser.expect_number_with_name("hpglpendiameter")?;
        let dxf_use_polygon_mode = parser.expect_alt_bool_with_name("dxfpolygonmode")?;
        let dxf_use_imperial_units = parser.expect_alt_bool_with_name("dxfimperialunits")?;
        let dxf_use_pcbnew_font = parser.expect_alt_bool_with_name("dxfusepcbnewfont")?;
        let postscript_negative = parser.expect_alt_bool_with_name("psnegative")?;
        let postscript_a4_output = parser.expect_alt_bool_with_name("psa4output")?;
        let plot_references = parser.expect_alt_bool_with_name("plotreference")?;
        let plot_values = parser.expect_alt_bool_with_name("plotvalue")?;
        let plot_invisible_text = parser.expect_alt_bool_with_name("plotinvisibletext")?;
        let sketch_pads_on_fab = parser.expect_alt_bool_with_name("sketchpadsonfab")?;
        let subtract_mask_from_silk = parser.expect_alt_bool_with_name("subtractmaskfromsilk")?;
        let output_format = parser
            .expect_number_with_name("outputformat")
            .map(|m| m as u8)
            .map(PlotFormat::try_from)??;
        let mirror = parser.expect_alt_bool_with_name("mirror")?;
        let drill_shape = parser
            .expect_number_with_name("drillshape")
            .map(|m| m as u8)
            .map(DrillMarks::try_from)??;
        let scale_selection = parser.expect_number_with_name("scaleselection")? as i32;
        let output_directory = parser.expect_string_with_name("outputdirectory")?;

        parser.expect_end()?;

        Ok(Self {
            layer_selection,
            plot_on_all_layers_selection,
            disable_aperture_macros,
            use_gerber_extensions,
            use_gerber_attributes,
            use_gerber_advanced_attributes,
            create_gerber_job_file,
            gerber_precision,
            dashed_line_dash_ratio,
            dashed_line_gap_ratio,
            svg_precision,
            plot_frame_ref,
            vias_on_mask,
            plot_mode,
            use_aux_origin,
            hpgl_pen_number,
            hpgl_pen_speed,
            hpgl_pen_diameter,
            dxf_use_polygon_mode,
            dxf_use_imperial_units,
            dxf_use_pcbnew_font,
            postscript_negative,
            postscript_a4_output,
            plot_references,
            plot_values,
            plot_invisible_text,
            sketch_pads_on_fab,
            subtract_mask_from_silk,
            output_format,
            mirror,
            drill_shape,
            scale_selection,
            output_directory,
        })
    }
}

#[allow(deprecated)]
impl ToSexpr for PcbPlotOptions {
    fn to_sexpr(&self, _context: SerializationContext) -> kicad_sexpr::Sexpr {
        Sexpr::list_with_name(
            "pcbplotparams",
            [
                Some(Sexpr::symbol_with_name(
                    "layerselection",
                    assemble_bit_field(self.layer_selection),
                )),
                Some(Sexpr::symbol_with_name(
                    "plot_on_all_layers_selection",
                    assemble_bit_field(self.plot_on_all_layers_selection),
                )),
                Some(Sexpr::alt_bool_with_name(
                    "disableapertmacros",
                    self.disable_aperture_macros,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "usegerberextensions",
                    self.use_gerber_extensions,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "usegerberattributes",
                    self.use_gerber_attributes,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "usegerberadvancedattributes",
                    self.use_gerber_advanced_attributes,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "creategerberjobfile",
                    self.create_gerber_job_file,
                )),
                self.gerber_precision
                    .map(|p| Sexpr::number_with_name("gerberprecision", p)),
                Some(Sexpr::number_with_name(
                    "dashed_line_dash_ratio",
                    self.dashed_line_dash_ratio,
                )),
                Some(Sexpr::number_with_name(
                    "dashed_line_gap_ratio",
                    self.dashed_line_gap_ratio,
                )),
                Some(Sexpr::number_with_name(
                    "svgprecision",
                    self.svg_precision as f32,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "plotframeref",
                    self.plot_frame_ref,
                )),
                Some(Sexpr::alt_bool_with_name("viasonmask", self.vias_on_mask)),
                Some(Sexpr::number_with_name(
                    "mode",
                    // Oh the joys of the KiCad file format :)
                    // https://gitlab.com/kicad/code/kicad/-/blob/7.0.10/pcbnew/pcb_plot_params.cpp?ref_type=tags#L212
                    if self.plot_mode == OutlineMode::Sketch {
                        2
                    } else {
                        1
                    } as f32,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "useauxorigin",
                    self.use_aux_origin,
                )),
                Some(Sexpr::number_with_name(
                    "hpglpennumber",
                    self.hpgl_pen_number as f32,
                )),
                Some(Sexpr::number_with_name(
                    "hpglpenspeed",
                    self.hpgl_pen_speed as f32,
                )),
                Some(Sexpr::number_with_name(
                    "hpglpendiameter",
                    self.hpgl_pen_diameter,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "dxfpolygonmode",
                    self.dxf_use_polygon_mode,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "dxfimperialunits",
                    self.dxf_use_imperial_units,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "dxfusepcbnewfont",
                    self.dxf_use_pcbnew_font,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "psnegative",
                    self.postscript_negative,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "psa4output",
                    self.postscript_a4_output,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "plotreference",
                    self.plot_references,
                )),
                Some(Sexpr::alt_bool_with_name("plotvalue", self.plot_values)),
                Some(Sexpr::alt_bool_with_name(
                    "plotinvisibletext",
                    self.plot_invisible_text,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "sketchpadsonfab",
                    self.sketch_pads_on_fab,
                )),
                Some(Sexpr::alt_bool_with_name(
                    "subtractmaskfromsilk",
                    self.subtract_mask_from_silk,
                )),
                Some(Sexpr::number_with_name(
                    "outputformat",
                    self.output_format as u8 as f32,
                )),
                Some(Sexpr::alt_bool_with_name("mirror", self.mirror)),
                Some(Sexpr::number_with_name(
                    "drillshape",
                    self.drill_shape as u8 as f32,
                )),
                Some(Sexpr::number_with_name(
                    "scaleselection",
                    self.scale_selection as f32,
                )),
                Some(Sexpr::string_with_name(
                    "outputdirectory",
                    &self.output_directory,
                )),
            ],
        )
    }
}

fn parse_bit_field(raw: &str) -> Result<u64, KiCadParseError> {
    let stripped = raw.trim_start_matches("0x").replace('_', "");

    u64::from_str_radix(&stripped, 16).map_err(|e| KiCadParseError::InvalidLayerBitField {
        raw: raw.to_string(),
        error: e,
    })
}

fn assemble_bit_field(bit_field: u64) -> String {
    let low = bit_field as u32;
    let high = (bit_field >> 32) as u32;

    format!("0x{:0>7x}_{:0>8x}", high, low)
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum OutlineMode {
    Sketch = 0,
    Filled = 1,
}

impl TryFrom<u8> for OutlineMode {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 | 2 => Self::Sketch,
            1 => Self::Filled,
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
#[repr(u8)]
pub enum PlotFormat {
    Hpgl = 0,
    Gerber = 1,
    PostScript = 2,
    Dxf = 3,
    Pdf = 4,
    Svg = 5,
}

impl TryFrom<u8> for PlotFormat {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::Hpgl,
            1 => Self::Gerber,
            2 => Self::PostScript,
            3 => Self::Dxf,
            4 => Self::Pdf,
            5 => Self::Svg,
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
#[repr(u8)]
pub enum DrillMarks {
    No = 0,
    Small = 1,
    Full = 2,
}

impl TryFrom<u8> for DrillMarks {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::No,
            1 => Self::Small,
            2 => Self::Full,
            _ => {
                return Err(KiCadParseError::invalid_enum_value::<Self>(
                    value.to_string(),
                ))
            }
        })
    }
}
