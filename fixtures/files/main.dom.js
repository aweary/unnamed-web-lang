// DEVELOPMENT BUILD, WebScript v1.23.1591
import {  html } from "@webscript/runtime";

const increment = 3;

function $Counter($ctx) {
  let count = 0;
  let onPress = () => {
    count += 1;
    $ctx.render();
  };
  return () =>
    html`
      <button @onClick=${onPress}>${count}</button>
    `;
}
