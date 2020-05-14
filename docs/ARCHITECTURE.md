## Language Design Goals

## Constraints

## Type System

### Primitive / Intrinsics

The type system has a handful of built-in types that can be used directly or composed together.

* `string` - the set of all possible strings
* `number` - the set of all possible numbers
* `bool` - `true` and `false`
* `Array<T>` - an heterogeneous array of items with type `T`
* `void` - the empty type
* `Option<T>` - a built-in enum type for representing optional data

`Option` is defined simply as:

```rust
enum Option<T> {
    Some(T),
    None
}
```

These types are intrinisic and *cannot be overwritten*. `void` is similar to Rust's unit type; it represents no value. If a function doesn't return a value its type is assumed to be `void`.

We may extend the numeric types in the future to support `BigInt` or other more specific numeric types like `float` and `int`.

### Composite Types

You can use the `type` keyword to declare _composite types_. 


### Tuples

You can use parens to define a tuple type.

```ts
type Coordinates = (number, number)
```

### Aliases

You can alias one type to another by simply referencing it in the type definition.

```ts
type Input = Coordinates
```

### Function 
A function type is a mapping of `Type` to `Type` using the `=>` token.

```ts
type Callback = Coordinates => void
```

### Records

> Note: records are the least thought out part of this. We will likely remove this feature in the future and use
> a `struct` and `impl` system like Rust's

You can define _record types_ using the object sytax

```ts
type Player = {
    position: Coordinates,
    active: bool,
}
```

A record type is considered _sealed_ and _complete_. You cannot instantiate an instance of a record type without declaring all the fields. Optional fields should use the built-in `Option` type


### Generics / Polymorphism

Composite types can be _generic_ over some type parameter. To make a type generic you use the `<>` syntax:

```ts
type Pair<T> = (T, T)
```

```ts
type Identity<T> = T => T
```


## Component System

## Design System

