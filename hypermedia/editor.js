import {EditorState, EditorView, basicSetup} from "@codemirror/basic-setup"
import {javascript} from "@codemirror/lang-javascript"

addEventListener('htmx:afterSwap', () => {
  let editor = document.getElementById('editor');
  if (editor !== null) {
    let view = new EditorView({
      state: EditorState.create({extensions: [basicSetup, javascript()]}),
      parent: editor 
    });
  }
});
