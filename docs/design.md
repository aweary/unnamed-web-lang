


## Paths

Much like Rust we have a concept of _paths_ where the `::` token signifies the delimeter between path parts. This used for resolving imports from packages as well as resolving variants from enums.

```rust
// An import
use std::events::Event

// Resolving an enum variant
const KeyboardEvent = Event::Keyboard;
```

The reason we use `::` for variants is to make it clear that it's a distinct operation from resolving properties with `.`; variants have special behavior and we want that to be clear.

