use typed_arena::Arena;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Existential(pub u16);

pub enum Type<'a> {
    Unit,
    Existential(Existential),
    Function(&'a mut Type<'a>, &'a mut Type<'a>),
}

#[derive(Default)]
pub struct TypeArena<'a> {
    arena: Arena<Type<'a>>,
}

impl<'a> TypeArena<'a> {
    /// Allocate a new type
    pub fn alloc(&'a self, ty: Type<'a>) -> &'a mut Type {
        let ty = self.arena.alloc(ty);
        ty
    }

    pub fn new_function(
        &'a mut self,
        a: &'a mut Type<'a>,
        b: &'a mut Type<'a>,
    ) -> &'a mut Type<'a> {
        let func = Type::Function(a, b);
        self.alloc(func)
    }
}
