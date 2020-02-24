#![warn(clippy::all)]
#![allow(dead_code)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

// use insta;

pub mod hir;
pub mod lower;

mod control_flow_graph;
mod module_graph;
mod scope;

#[cfg(test)]

mod control_flow_graph_tests {
    use crate::control_flow_graph;
    use crate::lower;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use parser::test_utils;
    use petgraph;
    use quickcheck;
    fn cfg_snapshot(body: &str) {
        let input = format!("fn foo() {{{}}}", body);
        let fn_ast = test_utils::parse_fn_def(&input);
        let fn_hir = lower::lower_fn(fn_ast);
        fn_hir.graph.dfs();
        assert_snapshot!(fn_hir.graph.graphviz_output());
    }

    fn cfg_from_code(input: &str) -> control_flow_graph::ControlFlowGraph {
        let fn_ast = test_utils::parse_fn_def(input);
        let fn_hir = lower::lower_fn(fn_ast);
        fn_hir.graph
    }
    fn cfg_from_block(block: &str) -> control_flow_graph::ControlFlowGraph {
        println!("BLOCK");
        let input = format!("fn foo() {}", block);
        println!("{}", input);
        let fn_ast = test_utils::parse_fn_def(&input);
        let fn_hir = lower::lower_fn(fn_ast);
        fn_hir.graph
    }

    #[quickcheck]
    fn is_graph(g: control_flow_graph::MockControlFlowCodeSnippetBuilder) -> bool {
        let code = g.code();
        let cfg = cfg_from_block(code);
        println!("\n\n\n{}\n\n\n", code);
        true
    }

    // Snapshot for a large and complex branching example
    #[test]
    fn kitchen_sink_branching() {
        cfg_snapshot(
            "
        let a = 1;
        let b = 2;
        if a == b {
            let c = 2;
            return c;
        } else if a > b {
            let d = 5;
            return d;
        } else if a < b {
            if a == b {
                return 5;
            };
            return 6;
        } else {
            let a = 1;
        };
        let d = 2;
        let c = 6;
        if d == c {
            let c = 5;
        } else {
            return 5;
        };
        if false {
            if true {
                let c = 5;
            } else {
                let d = 10;
            };
        };
        ",
        );
    }

    #[test]
    fn return_from_if_true() {
        cfg_snapshot(
            "
          if true {
              return 1;
          } else {
              let a = 1;
          };
          let a = 1;
        ",
        )
    }

    // Blocks with no branching.
    mod no_branching {
        use super::cfg_snapshot;

        #[test]
        fn empty() {
            cfg_snapshot("");
        }

        #[test]
        fn single() {
            cfg_snapshot(
                "
                let a = 1;
              ",
            );
        }
        #[test]
        fn double() {
            cfg_snapshot(
                "
                let a = 1;
                let b = 2;
              ",
            );
        }

        #[test]
        fn many() {
            cfg_snapshot(
                "
                let a = 1;
                let b = 2;
                let c = 3;
                let d = 4;
                let e = 5;
                let f = [6, 7, 8];
                let g = { nine: 10 };
              ",
            );
        }
    }

    mod if_branching {
        use super::*;
        #[test]
        fn basic() {
            cfg_snapshot(
                "
                if true {
                    let a = 1;
                };
                ",
            );
        }

        #[test]
        fn leading_single_stmt_basic() {
            cfg_snapshot(
                "
                let a = 1;
                if true {
                    let a = 1;
                };
                ",
            );
        }
        #[test]
        fn leading_multi_stmt_basic() {
            cfg_snapshot(
                "
                let a = 1;
                let b = 2;
                let c = 3;
                if true {
                    let a = 1;
                };
                ",
            );
        }
        #[test]
        fn trailing_single_stmt_basic() {
            cfg_snapshot(
                "
                if true {
                    let a = 1;
                };
                let c = 3;
                ",
            );
        }
        #[test]
        fn trailing_multi_stmt_basic() {
            cfg_snapshot(
                "
                if true {
                    let a = 1;
                };
                let c = 3;
                let d = 4;
                ",
            );
        }
    }
}
