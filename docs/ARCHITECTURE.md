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

### Schemes and Aliases

You can use the `type` keyword to declare both *type schemes* and *type aliases*. From a user's perspective there are almost never worth distinguishing, but they are semantically different for the type system.

A *type scheme* is a *polytype* which contains 

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


## Components

### Basics

Components are defined with the `component` keyword.

```ts
component Greeting() {
    // ...
}
```

Most of the syntax and semantics are the same as functions; you can define parameters, use generics, and add type annotations just like functions

```ts
component Map<T>(input: T) : View {
   // ...
}
```


### Composing Views

Components can return other components. In the simplest case this looks like a regular function call

```ts
component Greeting() {
    Text("Hello there")
}
```

In this case we assume text is a component with the signature `(content: string) -> Text`, where `Text` is the primitive component type for rendering text. You can also use the named argument syntax

```ts
Text(content: "Hello there")
```

This is mostly useful for components with many parameters.

In many cases we need to compose components and using _nesting_. In JSX this is what `children` was for

```jsx
<View>
  <Text>Hello there</Text>
</View>
```

In this example `View` nests `Text`; in other words this expresson is a `View` component that itself renders a `Text`.

For this case we use the _trailing closure_ syntax.

```ts
View {
    Text("Hello there")
}
```



## Design System

