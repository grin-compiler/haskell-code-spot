<script context="module">
  import * as _CodeMirror   from '../public/codemirror/lib/codemirror.js';
  import * as _CodeMirrorHS from '../public/codemirror/mode/haskell/haskell.js';
</script>

<script>
  import { onMount, createEventDispatcher } from "svelte";

  const dispatch = createEventDispatcher();

  export let selectionsArray = [];
  export let firstLineNumber = 1;
  export let value = "";
  export let readonly = 'nocursor';
  export let errorLoc = null;
  // export let flex = false;
  export let lineNumbers  = true;
  export let tab          = true;

  export let cmdEnter   = null;
  export let ctrlEnter  = null;
  export let shiftEnter = null;
  export let cmdPeriod  = null;
  export let ctrlPeriod = null;
  export let cmdHiffen  = null;
  export let ctrlHiffen = null;
  export let cmdEqual   = null;
  export let ctrlEqual  = null;
  export let cmdOpenSquareBracket   = null;
  export let ctrlOpenSquareBracket  = null;
  export let cmdCloseSquareBracket  = null;
  export let ctrlCloseSquareBracket = null;
  export let cmdForwardSlash = null;
  export let ctrlForwardSlash = null;

  // [Original Comment] We have to expose set and update methods,
  // rather than making this state-driven through props,
  // because it's difficult to update an editor
  // without resetting scroll otherwise

  export function getEditor() {
    return editor;
  }

  export async function set(new_value, new_mode=mode, new_theme=theme) {
    if (new_mode !== mode || new_theme !== theme) {
      await createEditor((mode = new_mode), (theme = new_theme));
    }

    value = new_value;
    updating_externally = true;
    if (editor) {
      editor.setOption('firstLineNumber', 1);
      editor.setValue(value);
    }
    updating_externally = false;
  }

  export function update(new_value) {
    value = new_value;

    if (editor) {
      const { left, top } = editor.getScrollInfo();
      editor.setValue((value = new_value));
      editor.scrollTo(left, top);
    }
  }

  export function getValue() {
    if (editor) {
      return editor.getValue();
    }
  }

  export function getLine(lineIndex) {
    if (editor) {
      return editor.getLine(lineIndex);
    }
  }

  export function getSelection() {
    if (editor) {
      let expression = editor.getSelection();
      if (expression == "") {
        let cursorInfo = editor.getCursor();
        expression = editor.getDoc().getLine(cursorInfo.line);
      }
      return expression;
    }
  }

  export function getCursorPosition() {
    return editor? editor.getCursor() : undefined;
  }

  export function getRange(from, to) {
    return editor? editor.getRange(from, to) : undefined;
  }

  export function commentSelection() {
    if (editor) {
      let expression = editor.getSelection();
      if (expression == "") {
        let cursorInfo = editor.getCursor();
        expression = editor.getDoc().getLine(cursorInfo.line);
      }
      return expression;
    }
  }

  /*
   * Find code between dividers,
   * const divider = "__________";
  */
  export function getBlock() {

    if (editor) {
      let cursorInfo = editor.getCursor();
      //find post divider
      let line = cursorInfo.line;
      let linePost = editor.lastLine();

      while (line < linePost) {
        if (/___+/.test(editor.getLine(line))) {  // Test RegEx at least 3 underscores
          linePost = line - 1;
          break;
        }
        line++;
      }

      line = cursorInfo.line;
      let linePre = -1;
      while (line >= 0) {
        // console.log(editor2.getLine(line));
        if (/___+/.test(editor.getLine(line))) {
          linePre = line;
          break;
        }
        line--;
      }
      if (linePre > -1) {
        linePre++;
      }
      let code = editor.getRange({
        line: linePre,
        ch: 0
      }, {
        line: linePost + 1,
        ch: 0
      });

      return code;
    }
  }

  export function resize() {
    editor.refresh();
  }

  export function focus() {
    editor.focus();
  }

  let w;
  let h;
  let mode;
  //let theme = "cobalt";

  let theme = "default"; // working well!

  //let theme = "monokai";
  //let theme = "paraiso-light";
  //let theme = "paraiso-dark";
  //let theme = "3024-night";
  //let theme = "base16-dark";
  //let theme = "base16-light";
  //let theme = "bespin";

  //let theme = "lucario";

  //let theme = "material";

  //let theme = "material-darker";
  //let theme = "material-palenight";
  //let theme = "midnight";



  const modes = {
    js: {
      name: "javascript",
      json: false
    },
    json: {
      name: "javascript",
      json: true
    },
    ebnf: {
      name: "ebnf",
      base: "text/html"
    },
    svelte: {
      name: "handlebars",
      base: "text/html"
    },
    closure: {
      name: "clojure",
      base: "text/x-clojure"
    },
    asn : {
      name: "asn.1",
      base: "text/x-ttcn-asn"
    },
    sema: {
      name: "sema",
      base: "text/html"
    }
  };


  const refs = {};
  let editor;
  let updating_externally = false;
  let marker;
  let error_line;
  let destroyed = false;
  let CodeMirror;

  $: if (editor && w && h) {
    editor.refresh();
  }

  $: {
    if (marker) marker.clear();

    if (errorLoc) {
      const line = errorLoc.line - 1;
      const ch = errorLoc.column;

      marker = editor.markText(
        { line, ch },
        { line, ch: ch + 1 },
        {
          className: "error-loc"
        }
      );

      error_line = line;
    } else {
      error_line = null;
    }
  }

  let previous_error_line;
  $: if (editor) {
    if (previous_error_line != null) {
      editor.removeLineClass(previous_error_line, "wrap", "error-line");
    }

    if (error_line && error_line !== previous_error_line) {
      editor.addLineClass(error_line, "wrap", "error-line");
      previous_error_line = error_line;
    }
  }

  onMount(() => {
    if (_CodeMirror) {
      CodeMirror = _CodeMirror;
      createEditor(mode || "haskell", theme).then(() => {
        if (editor) {
          //editor.setOption('firstLineNumber', firstLineNumber || 1);
          editor.setValue(value || "");
        }
      });
    } else {
      alert("Error in codemirror import!");
    }

    return () => {
      destroyed = true;
      if (editor) editor.toTextArea();
    };
  });

  let first = true;

  async function createEditor(mode, theme) {
    if (destroyed || !CodeMirror) return;

    if (editor) editor.toTextArea();

    // console.log("createEditor:", theme);

    const opts = {
      lineNumbers,
      firstLineNumber: firstLineNumber,
      lineWrapping: true,
      indentWithTabs: true,
      indentUnit: 2,
      tabSize: 2,
      value: "",
      mode: modes[mode] || {
        name: mode
      },
      readOnly: readonly,
      autoCloseBrackets: true,
      autoCloseTags: true,
      extraKeys: {}
    };

    if(theme !== undefined)
      opts.theme = theme;


    if (!tab)
      opts.extraKeys = {
        Tab: tab,
        "Shift-Tab": tab,
      };

    if(cmdEnter)
      opts.extraKeys["Cmd-Enter"] = (cmdEnter);

    if(ctrlEnter)
      opts.extraKeys["Ctrl-Enter"] = (ctrlEnter);

    if(shiftEnter)
      opts.extraKeys["Shift-Enter"] = (shiftEnter);

    if(cmdPeriod)
      opts.extraKeys["Cmd-."] = (cmdPeriod);

    if(ctrlPeriod)
      opts.extraKeys["Ctrl-."] = (ctrlPeriod);

    if(cmdHiffen)
      opts.extraKeys["Cmd--"] = (cmdHiffen);

    if(ctrlHiffen)
      opts.extraKeys["Ctrl--"] = (ctrlHiffen);

    if(cmdEqual)
      opts.extraKeys["Cmd-="] = (cmdEqual);

    if(ctrlEqual)
      opts.extraKeys["Cmd-="] = (ctrlEqual);

    if(cmdCloseSquareBracket)
      opts.extraKeys["Cmd-]"] = (cmdCloseSquareBracket);

    if(cmdOpenSquareBracket)
      opts.extraKeys["Cmd-["] = (cmdOpenSquareBracket);

    if(ctrlCloseSquareBracket)
      opts.extraKeys["Ctrl-]"] = (ctrlCloseSquareBracket);

    if(ctrlOpenSquareBracket)
      opts.extraKeys["Ctrl-["] = (ctrlOpenSquareBracket);

    if(cmdForwardSlash)
      opts.extraKeys["Cmd-/"] = () => editor.execCommand('toggleComment');

    if(ctrlForwardSlash)
      opts.extraKeys["Ctrl-/"] = () => editor.execCommand('toggleComment');

    // if(ctrlForwardSlash)
    //   opts.extraKeys["Ctrl-/"] = (ctrlForwardSlash);

    // if(cmdEnter && !opts.extraKeys["Cmd-Enter"])
    //   opts.extraKeys["Cmd-Enter"] = (cmdEnter);


    // Creating a text editor is a lot of work, so we yield
    // the main thread for a moment. This helps reduce jank
    if (first) await sleep(50);

    if (destroyed) return;

    editor = CodeMirror.fromTextArea(refs.editor, opts);

    editor.setSize(null, 'auto');

/*
    editor.on("change", (instance, changeObj) => {
      if (!updating_externally) {
        // const value = instance.getValue();
        dispatch("change", { changeObj });
      }
    });

    editor.on("focus", (instance, event) => {
      if (!updating_externally) {
        dispatch("focus", { event });
      }
    });

    editor.on("blur", (instance, event) => {
      if (!updating_externally) {
        dispatch("blur", { event });
      }
    });

    editor.on("refresh", (instance, event) => {
      if (!updating_externally) {
        dispatch("refresh", { event });
      }
    });

    editor.on("gutterClick", (instance, line, gutter, clickEvent) => {
      if (!updating_externally) {
        dispatch("gutterClick", { line, gutter, clickEvent });
      }
    });

    editor.on("viewportChange", (instance, from, to) => {
      if (!updating_externally) {
        dispatch("viewportChange", { from, to });
      }
    });
*/
    if (first) await sleep(50);
    editor.refresh();

    first = false;
  }

  function sleep(ms) {
    return new Promise(fulfil => setTimeout(fulfil, ms));
  }

  $: {
    updating_externally = true;
    if (editor) {
      editor.setOption('firstLineNumber', firstLineNumber);
      editor.setValue(value);
      selectionsArray.forEach(s => {
        editor.getDoc().markText(s.anchor, s.head, {className: 'CodeMirror-selected'});
      });
      editor.getDoc().setSelections(selectionsArray);
    }
    updating_externally = false;
  }

</script>

<style>
  textarea {
    visibility: hidden;
  }

  pre {
    position: absolute;
    width: 100%;
    height: 100%;
    top: 0;
    left: 0;
    border: none;
    padding: 4px 4px 4px 60px;
    resize: none;
    font-family: var(--font-mono);
    font-size: 13px;
    line-height: 1.7;
    user-select: none;
    pointer-events: none;
    color: #ccc;
    tab-size: 2;
    -moz-tab-size: 2;
  }
</style>

<textarea {value}
          tabindex="0"
          bind:this={refs.editor}
          readonly />
{#if !CodeMirror}
  <pre>{value}</pre>
{/if}
