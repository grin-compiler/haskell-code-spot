<script>
  import * as d3 from 'd3';
  import { postData } from './post-data.js';
  import { onMount } from 'svelte';
  import CostCentreStackHeap from './CostCentreStackHeap.svelte';
  import SourceRangeBox from './SourceRangeBox.svelte';

  export let eventlogData;

  let ghcStgApp = '/home/csaba/haskell/grin-compiler/ghc-wpc-sample-programs/game-logic-experiment/.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/minigame/minigame.ghc_stgapp';
  //let ghcStgApp = '';
  let moduleMap;
  let stackModuleSource = {};

  const errorCostCentre = {
    evSpec: {
      heapProfCostCentreId: 'invalid',
      heapProfLabel:  'invalid Cost Centre ID',
      heapProfModule: '',
      heapProfSrcLoc: 'could be a bug in our application or in GHC RTS',
      heapProfFlags: ''
    }
  };

  async function calculateCurrentStack() {
    if (!currentStack) return;
    switch (currentStack.evSpec.tag) {
      case 'HeapProfSampleCostCentre':
        currentStackData = currentStack.evSpec.heapProfStack.map(i => costCentreMap[i] || errorCostCentre);
        bgScale = d3.scaleOrdinal(colorScheme).domain(currentStack.evSpec.heapProfStack);
        break;
      case 'ProfSampleCostCentre':
        currentStackData = currentStack.evSpec.profCcsStack.map(i => costCentreMap[i] || errorCostCentre);
        bgScale = d3.scaleOrdinal(colorScheme).domain(currentStack.evSpec.profCcsStack);
        break;
      default:
        console.log('invalid stack:', currentStack);
        return;
    }

    // load stack module sources
    let oldModuleSource = stackModuleSource;
    stackModuleSource = {}
    if (moduleMap) {
      currentStackData.forEach(d => stackModuleSource[d.evSpec.heapProfModule] = null);
      for (let moduleName in stackModuleSource) {
        if (moduleName in oldModuleSource && oldModuleSource[moduleName]) {
          stackModuleSource[moduleName] = oldModuleSource[moduleName];
        } else if (moduleName in moduleMap) {
          stackModuleSource[moduleName] = postData('http://localhost:3000/ext-stg/get-source-code', { stgbinPath: moduleMap[moduleName].stgbinPath });
        }
      }
    }
  }

  async function getSourceCodeForModule(moduleName) {
    if (moduleName in stackModuleSource) {
      const res = await stackModuleSource[moduleName];
      return res.sourceCode;
    }
    return '<not found>';
  }

  function handleKeypress(event) {
    if (event.key === 'ArrowLeft') {
      sampleIndex -= event.shiftKey ? 300 : 1;
      if (sampleIndex < 0) sampleIndex = 0;
      currentStack = dataSamples[sampleIndex];
      event.preventDefault();
    }
    if (event.key === 'ArrowRight') {
      sampleIndex += event.shiftKey ? 300 : 1;
      if (sampleIndex >= dataSamples.length) sampleIndex = dataSamples.length-1;
      currentStack = dataSamples[sampleIndex];
      event.preventDefault();
    }
  }

  let index;

  let dataAll = [];
  let dataCCS = [];
  let dataSamples = [];
  let sampleIndex = 0;
  let costCentreMap = {};

  $: if (eventlogData) {
    dataAll = eventlogData.dat.events;
    dataCCS = dataAll.filter(e => e.evSpec.tag === 'HeapProfCostCentre');
    dataSamples = dataAll.filter(e => e.evSpec.tag === 'HeapProfSampleCostCentre' || e.evSpec.tag === 'ProfSampleCostCentre');
    costCentreMap = {};
    dataCCS.forEach(d => costCentreMap[d.evSpec.heapProfCostCentreId] = d);

    postData('http://localhost:3000/ext-stg/get-module-mapping', {ghcStgAppPath: ghcStgApp})
      .then(d => {moduleMap = d;}, _ => {moduleMap = null;})
      .finally(calculateCurrentStack);
  }

  const colorScheme = d3.schemeDark2;//d3.schemeTableau10;
  let bgScale = d3.scaleOrdinal(colorScheme).domain([]);
  let currentStack;
  let currentStackData = [];

  $: {
    currentStack = dataSamples[sampleIndex];
    if (currentStack) calculateCurrentStack();
  }

  // highlight on screen source boxes
  let sourceBoxMap = {};
  let nameBoxMap = {};

  function registerNameBox(node, i) {
    nameBoxMap[i] = node;
  }
  function registerSourceBox(node, i) {
    sourceBoxMap[i] = node;
  }

  function highlightName(idx, value) {
    if (value) {
      for (const i in nameBoxMap) {
        nameBoxMap[i].style.backgroundImage = (+i === idx) ? 'linear-gradient(#8e508a, #453b61)' : '';
      }
    } else {
      nameBoxMap[idx].style.backgroundImage = '';
    }
  }

  // scrolling and jumping
  function jumpToSourceBox(idx) {
    sourceBoxMap[idx].scrollIntoView({behavior: 'smooth', block: 'center'});
  }
</script>

<nav class="ccs-nav">

  <label for="myfile-ghcstgapp">GHC-WPC ghc_stgapp file:</label>
  <input type="text" name="myfile-ghcstgapp" bind:value={ghcStgApp} title={ghcStgApp} style="flex-grow:1;">

</nav>

<div on:keydown={handleKeypress} tabindex=0>
<div style="height:21vmin; width:100%; padding: 0; margin:0; position:sticky; top:0; z-index:100; background-color: white;">
  <CostCentreStackHeap {eventlogData} bind:currentStackIndex={sampleIndex} timeMarker={currentStack && currentStack.evTime} width="100%" height="100%"/>
</div>

<div class="ccs-container">
  <div class="name-boxes">
    <h4>
      Cost Centre Stack
    </h4>
    <div class="names-container">
      {#each currentStackData as cc, i}
        <div class="list-group-item text-light cc-name-box not-selected-name-box"
             use:registerNameBox={i}
             on:click={() => jumpToSourceBox(i)}
             data-box-index={i}
        >

            <span on:click={() => jumpToSourceBox(i)} style="color: lightgrey;">{cc.evSpec.heapProfModule}</span>
            <br>
            <span on:click={() => jumpToSourceBox(i)} style="color: white;">{cc.evSpec.heapProfLabel}</span>
        </div>
      {/each}

    </div>
  </div>

  <div class="source-boxes">
    {#each currentStackData as cc, i}
      {#await getSourceCodeForModule(cc.evSpec.heapProfModule)}
        <div class="spinner-border text-primary"></div>
      {:then sourceCode}
        <div use:registerSourceBox={i} data-box-index={i}
             on:mouseenter={() => {highlightName(i, true);}}
             on:mouseleave={() => {highlightName(i, false);}}
        >
          <SourceRangeBox
            srcLocString={cc.evSpec.heapProfSrcLoc}
            moduleName={cc.evSpec.heapProfModule}
            functionLabel={cc.evSpec.heapProfLabel}

            sourceCode={sourceCode}
            packageName={moduleMap && moduleMap[cc.evSpec.heapProfModule] && moduleMap[cc.evSpec.heapProfModule].packageName || ''}
          />
        </div>
      {/await}

    {/each}
  </div>
</div>
</div>

<style>

  .ccs-container {
    width: 100%;
    height: auto;

    display: flex;
    flex-direction: row;
    align-items: flex-start;
    margin: 0;
  }

  .names-container {
    overflow-y: scroll;
    margin: 0;
    padding: 0;
  }

  .name-boxes {
    flex-basis: 20%;

    display: flex;
    flex-direction: column;

    height: calc(100vh - 21vmin);

    position: sticky;
    top: 21vmin;

    padding: 0.5em;
  }

  .source-boxes {
    flex-basis: 80%;

    display: flex;
    flex-direction: column;

    padding: 0.5em;

    color: white;
  }

  .ccs-nav {
    width: 100%;
    height: auto;

    display: flex;
    align-items: center;

    background: #5f5286;
    color: white;
  }

  .ccs-nav > * {
    margin: 0.5em;
  }

  .cc-name-box {
    word-wrap: break-word;
    margin: 0;
    cursor: pointer;
    background-image: linear-gradient(#5f5286, #453b61);
  }

  .cc-name-box:hover {
    background-image: linear-gradient(#8e508a, #453b61);
  }

</style>
