#![warn(clippy::all)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod code_fuzz;

#[cfg(test)]
mod tests {
    use diagnostics;
    use parser::test_utils;
    use crate::code_fuzz::CodeFuzzer;
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[quickcheck]
    fn generates_parseable_code(fuzz: CodeFuzzer) -> bool {
        let code = fuzz.code();
        println!("{}", code);
        false
        // match test_utils::parse_block(
        //     &format!("{{{}}}", code)
        // ) {
        //     Ok(_) => true,
        //     Err(err) => {
        //         // Pretty-print the parser error
        //         // use codespan_reporting::term::emit;
        //         // use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
        //         // let mut files = diagnostics::Files::new();
        //         // let file_id = files.add("test", code);
        //         // let writer = StandardStream::stderr(ColorChoice::Auto);
        //         // let config = codespan_reporting::term::Config::default();
        //         // emit(&mut writer.lock(), &config, &files, &err).unwrap();
        //         // false
        //     },
        // }
    }
}
