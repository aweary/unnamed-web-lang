use syntax::ast;

pub fn codegen_stmt(stmt: &ast::Stmt) -> String {
    let mut builder = String::new();
    builder.push_str("stmt;");
    builder
}

pub fn codegen_fn(fn_def: &mut ast::FnDef) {
    let mut builder = String::new();
    // Whether this should be generated as an arrow function
    // let use_arrow_fn = false;
    builder.push_str("function ");
    builder.push_str(fn_def.name.to_str());
    builder.push_str("(");
    // TODO Context.shortName()? Need to map identifiers to
    let mut param_short_name = 97;
    for (i, param) in fn_def.params.clone().into_iter().enumerate() {
        if i > 0 {
            builder.push(',');
        }
        builder.push(std::char::from_u32(param_short_name).unwrap());
        param_short_name += 1;
    }
    builder.push_str(")");
    builder.push_str("{");
    for stmt in fn_def.body.stmts.clone() {
        let gen_stmt = codegen_stmt(&stmt);
        builder.push_str(&gen_stmt);
    }
    builder.push_str("}");
    println!("codegen'in!");
    println!("{}", builder);
}
