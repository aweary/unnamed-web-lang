component TodoItem(label: string, id: ID!) {
  // Initialize some state
  state done : bool = false;
  state hovered : bool = false;
  // Define an event callack for the root element
  event onClick = done.invert
  event onMouseEnter = hovered.set(true)
  event onMouseLeave = hovered.set(false)
  // Effect fires after update
  effect {
    document.title = label;
  }
  // Rendered content is implicitly returned
  <li onClick onMouseLeave onMouseEnter class="todo-item">
      {label}
  </li>
}

component TodoList(items: List<{..TodoItem.T, done: bool}>) {
  <ul class="todo-list">
    {items.map(
      item => <TodoItem label={item.label} id={item.id} />
    )}
  </ul>
}
