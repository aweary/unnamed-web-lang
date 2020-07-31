# Compilation Strategy

This outlines how the compiler handles the _compilation_ step; that is, the final step where it generates the executable code.

## Outline

This language targets the web which means it primarily outputs JavaScript. It is also meant to provide native solutions for styling and layout, so it will also output HTML and CSS--along with any other asset type you might find in a web application. Any abstractions we implement should retain the flexibility of outputting different asset types over time.


TODO more preamble

...

## Language Features



### Pattern Matching

We compile `match` expressions to `switch` statements.

```rust
match "hello" {
    
}