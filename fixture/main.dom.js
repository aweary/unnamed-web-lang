// DEVELOPMENT BUILD, WebScript v1.23.1591
import { op } from "@webscript/runtime";

// You need to:
// - Load CSS styles for this that aren't already
// - Load JS for rendering
// - Load data for this instance
// - 

function $Button($label, $kind, $onPress) {
  const $program = [
    op.CreateButtonElement,
    op.SetAttribute,
    'aria-label',
    $label,
    op.AddListener,
    $onPress,
    op.AttachText,
    $label,
    op.Close,
  ];
  return function $Button_update($runtime) {
      $runtime.execute($program);
  }
}
