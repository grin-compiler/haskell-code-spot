<script>
  import { postData } from './post-data.js';
  import CodeMirror from "./CodeMirror.svelte";

  //let ghcStgApp = '';
  let ghcStgApp = '/home/csaba/haskell/grin-compiler/ghc-wpc-sample-programs/game-logic-experiment/.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/minigame/minigame.ghc_stgapp';
  let moduleMap;
  let currentModule = 'Main';
  let haskellSourceCode = 'haskell';
  let coreSourceCode = 'core';
  let ghcstgSourceCode = 'ghc-stg';
  let extstgSourceCode = 'ext-stg';
  let viewMode = "haskell";
  let sourceCode = '';

  function changeViewMode(vm) {
    viewMode = vm;
  };

  $: {
    switch (viewMode) {
      case 'haskell':
        sourceCode = haskellSourceCode;
        break;
      case 'core':
         sourceCode = coreSourceCode;
         break;
      case 'ghcstg':
        sourceCode = ghcstgSourceCode;
         break;
      case 'extstg':
        sourceCode = extstgSourceCode;
        break;
      default:
        sourceCode = '';
        break;
    }
  }

  $: if (ghcStgApp) {
    postData('http://localhost:3000/ext-stg/get-module-mapping', {ghcStgAppPath: ghcStgApp})
      .then(d => {moduleMap = d;}, _ => {moduleMap = null;});
  } else {
    moduleMap = null;
  }

  $: if (moduleMap && currentModule) {
    let files = ['module.hs', 'module.ghccore', 'module.ghcstg'];
    postData('http://localhost:3000/ext-stg/get-modpak-content', { modpakPath: moduleMap[currentModule].modpakPath, files: files })
      .then(d => {
        haskellSourceCode = d['module.hs'];
        coreSourceCode = d['module.ghccore'];
        ghcstgSourceCode = d['module.ghcstg'];
      }, _ => {
        haskellSourceCode = '';
        coreSourceCode = '';
        ghcstgSourceCode = '';
      });
  }
</script>

<nav class="ccs-nav">

  <label for="myfile-ghcstgapp">GHC-WPC ghc_stgapp file:</label>
  <input type="text" name="myfile-ghcstgapp" bind:value={ghcStgApp} title={ghcStgApp} style="flex-grow:1;">

</nav>

<div class="project-container">
  <div class="module-boxes">
    {#each Object.entries(moduleMap || {}).sort() as [mod_name, mod]}
      <span
        on:click={() => {currentModule = mod_name;}}
        class:selected={currentModule === mod_name}
      >{mod_name}</span>
    {/each}
  </div>
  <div class="module-source-view">
    <h3>{currentModule}</h3>
    <p>{moduleMap && currentModule && moduleMap[currentModule].packageName}</p>
    <nav>
      <button
        on:click={()=>changeViewMode("haskell")}
        class:selected={viewMode === "haskell"}
      >Haskell</button>
      <button
        on:click={()=>changeViewMode("core")}
        class:selected={viewMode === "core"}
      >Core</button>
      <button
        on:click={()=>changeViewMode("ghcstg")}
        class:selected={viewMode === "ghcstg"}
      >GHC-Stg</button>
      <button
        on:click={()=>changeViewMode("extstg")}
        class:selected={viewMode === "extstg"}
      >Ext-Stg</button>
    </nav>

    <CodeMirror value={sourceCode} />

  </div>
</div>

<style>
  .project-container {

    display: flex;
    flex-direction: row;
    align-items: stretch;
    margin: 0;
    overflow-y: auto;
  }

  .module-boxes {

    display: flex;
    flex-direction: column;

    height: auto;
    padding: 0.5em;
    background: green;
    overflow-y: auto;
  }

  .module-source-view {
    background: red;
    flex-basis: 80%;
    flex-grow: 1;
    flex-shrink: 2;

    display: flex;
    flex-direction: column;

    padding: 0.5em;

    color: white;
  }

  .ccs-nav {

    display: flex;
    align-items: center;

    background: #5f5286;
    color: white;
  }

  .ccs-nav > * {
    margin: 0.5em;
  }

  .selected {
    color: orange;
    background: black;
  }

  .hidden {
  }
</style>