import { EditorState, EditorView, basicSetup } from "@codemirror/basic-setup"
import { javascript } from "@codemirror/lang-javascript"

export default { view };

var view = 2;

addEventListener('htmx:afterSwap', () => {
  let editor = document.getElementById('editor');
  let editorContent = document.getElementById('editor-content');
  let content = "";
  if (editorContent) {
    content = editorContent.innerText;
  }
  if (editor !== null) {
    view = new EditorView({
      state: EditorState.create({
        extensions: [basicSetup, javascript()],
        doc: content
      }),
      parent: editor
    });
  }
});
