use crate::ast::*;
use crate::parser::ParsingContext;
// use crate::visitor::walk_template;
use crate::visitor::{walk_template, walk_fn, Visitor};

use crate::ir::{Template, TemplateInstr, TemplateInstrList, TemplateKind};

pub struct TemplateIRPass {}

impl TemplateIRPass {
    pub fn new() -> Self {
        Self {}
    }
    pub fn compile_templates(&mut self, ctx: &ParsingContext, module: &Module) {
        self.visit_mod(&ctx, &module);
    }

    

    /**
     * Compile a Template AST node into a series of template IR instructions.
     * The template IR is structured like bytecode for a hybrid stack-register machine,
     * where the current element being modified is kept on the stack and static templates
     * or expressions are referenced with identifiers (conceptually, registers)
     */
    fn gen_template_instr(
        &mut self,
        ctx: &ParsingContext,
        template_id: TemplateId,
    ) -> (TemplateInstrList, bool) {
        let mut is_static = true;
        use TemplateInstr::{
            EmbedExpr, EmbedStaticTemplate, InsertText, PopElement, PushElement,
            SetDynamicAttribute, SetStaticAttribute,
        };
        let mut instrs = vec![];
        let template = ctx.resolve_template(template_id);
        let tag = template.name;
        instrs.push(PushElement(tag));
        for attr in &template.attrs {
            let maybe_attr_value = ctx.resolve_value_of(attr.value);
            if let Some(value) = maybe_attr_value {
                instrs.push(SetStaticAttribute(attr.name, value));
            } else {
                is_static = false;
                instrs.push(SetDynamicAttribute(attr.name, attr.value));
            }
        }
        if let Some(children) = &template.children {
            use TemplateChild::{Expr, Template, Text};
            for child in children {
                match child {
                    Expr(expr_id) => {
                        is_static = false;
                        instrs.push(EmbedExpr(*expr_id));
                    }
                    Template(template_id) => {
                        let (mut child_instrs, is_child_static) =
                            self.gen_template_instr(ctx, *template_id);
                        if is_child_static {
                            let instr_id = ctx.alloc_static_template_instrs(child_instrs);
                            instrs.push(EmbedStaticTemplate(instr_id));
                        } else {
                            is_static = false;
                            instrs.append(&mut child_instrs);
                        }
                    }
                    Text(text) => {
                        instrs.push(InsertText(*text));
                    }
                }
            }
        };
        instrs.push(PopElement);
        (instrs, is_static)
    }
}

impl<'ast> Visitor<'ast> for TemplateIRPass {

    fn visit_fn(&mut self, ctx: &ParsingContext, func: &FuncDecl) {
        println!("visiting function definition {:?}", func);
        walk_fn(self, &ctx, &func)
    }

    fn visit_template(&mut self, ctx: &ParsingContext, template_id: TemplateId) {
        let (instr, is_static) = self.gen_template_instr(ctx, template_id);
        let kind = if is_static {
            TemplateKind::Static
        } else {
            TemplateKind::Dynamic
        };
        let template = Template { kind, instr };
        println!("{:#?}", template);
        ctx.set_template_ir(template_id, template);
        walk_template(self, ctx, template_id);
    }
}
