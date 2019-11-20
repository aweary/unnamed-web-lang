# WebScript Informal Semantics

This is a description of how the language works, in informal terms.


## Primitives


### Strings

Strings work just like they do in JavaScript, except we only support double quotes.

```js
"hello, world"
```

They will map 1:1 to JavaScript strings, unless interning would be valuable; in that case they might map to an offset in a table. But semantically, same primitive value.

### Numbers

Numbers are all floating point, represented by the type `number`. We could have a `float` type in the future based on `BigInt` but I don't think thats necessary in `v1`.

```js
1;
1.40;
```

## Imports

Imports work much list ECMAScript imports. You 

```js
import map from "std"
```

## Exports
```js
// util.ws
export fn map<T, U>(item: List<T>) : U {
  // ...
} 
```


## Components

This is the major feature of the language. A component is like a function except it defines and returns UI. Specifically, it can define templates and CSS.

```jsx
component AlertBar(type: AlertBarType, title: string, body: string) {
  return (
    <div>
    </div>
  )
}
```


### Template Bytecode Generation

#### Static Templates

These are the easiest. They are pure functions of their props. They can be inlined or hoisted depending on what optimizes best for the scenario.



```jsx
component Button(label: string) {
  <button class="btn">{label}</button>
}
```

```js
function Button_COMPILED(label) {
  return [1, 2, 12, 23, label, ]
}
```




## Stateful and Effectual functions

##### Input
```js
// mouseMove is an effect from the std library
import {mouseEvent} from "std/events"

pub fn useMouse() : (number, number) {
  state x = 0;
  state y = 0;
  handle mouseEvent("mousemove") |event| {
    next x = event.pageX;
    next y = event.pageY;
  };
  (x, y)
}

pub fn useCounter() {
  state count = 0;
  // A lambda function
  let increment = || {
    next count += 1;
  }
  (state, increment)
}

fn Component() {
  let (x, y) = useMouse();
  state z = x + y;
  let (count, increment) = useCounter();
  <div>{x} {y} {count}</div>
}
```

##### Output

```js
import {html, render} from 'lit-html';
import {listen} from '@webscript/runtime';

let $t0 = (x, y) => html`<div>${x} ${y}</div>`;

function useMouse() {
  listen("mousemove", (e) => {

  })
}

function Component_COMPILED() {
  let x = 0;
  let y = 0;
  let count = 0;
  // Doesn't depend on any props, only state
  return $t0
}
```