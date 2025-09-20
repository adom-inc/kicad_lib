use std::path::Path;

use kicad_format::{
    convert::{FromSexpr, Parser, SerializationContext, ToSexpr},
    footprint_library::FootprintLibraryFile,
    netlist::NetlistFile,
    pcb::PcbFile,
    schematic::SchematicFile,
    symbol_library::SymbolLibraryFile,
};
use kicad_sexpr::Sexpr;

fn assert_sexprs_eq(input_sexpr: Sexpr, output_sexpr: Sexpr) {
    if input_sexpr == output_sexpr {
        return;
    }

    let mut output = String::new();

    for diff in diff::lines(&format!("{input_sexpr}"), &format!("{output_sexpr}")) {
        match diff {
            diff::Result::Left(l) => output.push_str(&format!(
                "{}",
                ansi_term::Color::Red.paint(format!("-{}\n", l))
            )),
            diff::Result::Both(l, _) => output.push_str(&format!(" {}\n", l)),
            diff::Result::Right(r) => output.push_str(&format!(
                "{}",
                ansi_term::Color::Green.paint(format!("+{}\n", r))
            )),
        }
    }

    panic!("input sexpr (red) did not match output sexpr (green): \n{output}");
}

fn assert_in_out_eq<T: FromSexpr + ToSexpr>(input: &str, path: &Path) {
    let input_sexpr = kicad_sexpr::from_str(input).unwrap();

    let parser = Parser::new(input_sexpr.as_list().unwrap().clone());
    let pcb = T::from_sexpr(parser)
        .unwrap_or_else(|e| panic!("Failed to parse file: {}\n{e}\n{e:?}", path.display()));

    let output_sexpr = pcb.to_sexpr(SerializationContext::pre_v8());

    assert_sexprs_eq(input_sexpr, output_sexpr);
}

fn test_files_in_dir<T: FromSexpr + ToSexpr, P: AsRef<Path>>(directory: P) {
    let files = std::fs::read_dir(directory)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    files.iter().for_each(|file| {
        if file.metadata().unwrap().is_dir() {
            return;
        }

        let input = std::fs::read_to_string(file.path()).unwrap();

        assert_in_out_eq::<T>(&input, &file.path());
    });
}

#[test]
fn test_footprint_library() {
    test_files_in_dir::<FootprintLibraryFile, _>("./tests/footprint_library")
}

#[test]
fn test_symbol_library() {
    test_files_in_dir::<SymbolLibraryFile, _>("./tests/symbol_library")
}

#[test]
fn test_schematic() {
    test_files_in_dir::<SchematicFile, _>("./tests/schematic")
}

#[test]
fn test_pre_v8_schematic() {
    test_files_in_dir::<SchematicFile, _>("./tests/schematic/pre_v8")
}

#[test]
fn test_pcb() {
    test_files_in_dir::<PcbFile, _>("./tests/pcb")
}

#[test]
fn test_netlist() {
    test_files_in_dir::<NetlistFile, _>("./tests/netlist")
}
