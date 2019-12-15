trait Queries {
  /// Query the type of a single IR expression
  fn type_of(&self, id: ExprId) -> Ty;
}