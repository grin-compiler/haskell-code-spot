<script>

  import { parseSrcLoc } from './ghc-srcloc.js';
  import CodeMirror from "./CodeMirror.svelte";

  // GHC eventlog related
  export let srcLocString;
  export let moduleName;
  export let functionLabel;

  // GHC-WPC related
  export let sourceCode;
  export let packageName;

  let originalSourceSpan = parseSrcLoc(srcLocString);

  let startLineNumber = originalSourceSpan.startLineNumber;
  let endLineNumber   = originalSourceSpan.endLineNumber;

  let sourceCodeLines = sourceCode.split('\n');
  let selectionsArray = [];

  function getSourceRange(startLine, endLine) {
    return sourceCodeLines.slice(startLine -1, endLine).join('\n');
  }

  function toggleSource() {
    if (startLineNumber !== originalSourceSpan.startLineNumber || endLineNumber !== originalSourceSpan.endLineNumber) {
      startLineNumber = originalSourceSpan.startLineNumber;
      endLineNumber   = originalSourceSpan.endLineNumber;
      selectionsArray = [];
    } else {
      startLineNumber = 1;
      endLineNumber   = sourceCodeLines.length;
    }
  }

  function extendStart() {
    startLineNumber -= 5;
    if (startLineNumber < 1) startLineNumber = 1;
  }

  function extendEnd() {
    endLineNumber += 5;
    if (endLineNumber >= sourceCodeLines.length) endLineNumber = sourceCodeLines.length;
  }

  $: if (startLineNumber !== originalSourceSpan.startLineNumber || endLineNumber !== originalSourceSpan.endLineNumber) {
    selectionsArray = [
      { anchor: {line:originalSourceSpan.startLineNumber - startLineNumber, ch: originalSourceSpan.startColumn-1}
      , head:   {line:originalSourceSpan.endLineNumber   - startLineNumber + 1, ch: originalSourceSpan.endColumn}
      }]
  } else {
    selectionsArray = [];
  }

</script>

<div style="background-image: linear-gradient(to right, #8e508a, #5f5286); padding: 0.4em; margin: 0.1em 0">
  <section>
    <button class="btn btn-secondary" on:click={toggleSource}><span style="color:lightgrey;">{moduleName}.</span><span style="color:white;">{functionLabel}</span></button>

    {#if packageName}
      <code class="my-right-note">
        <span class="my-right-note">{packageName}</span>
        <br>
        <span class="my-right-note">{srcLocString}</span>
      </code>
    {:else}
      <code style="float:right;font-size: 0.96em; color:lightgrey;padding: 0.5em; margin:0;">
        {srcLocString}
      </code>
    {/if}

    <button style="width:100%; margin: 0; padding: 0.1em 0.5em; border-radius: 0;border-radius: 3px 3px 0 0; text-align:right;" on:click={extendStart}>+5 lines</button>
    <CodeMirror
      {selectionsArray}
      value={getSourceRange(startLineNumber, endLineNumber)}
      firstLineNumber={startLineNumber}
    />
    <button style="width:100%; margin: 0; padding: 0.1em 0.5em;border-radius: 0;border-radius: 0 0 3px 3px; text-align:right;" on:click={extendEnd}>+5 lines </button>
  </section>
</div>

<style>
  .my-right-note {
    float: right;
    font-size: 0.96em;
    color: lightgrey;
    padding: 0 0.1em;
    margin: 0;
  }
</style>
