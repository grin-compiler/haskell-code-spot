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

    console.log('calculate currentStackData', currentStackData);

    // load stack module sources
    let oldModuleSource = stackModuleSource;
    stackModuleSource = {}
    if (moduleMap) {
      currentStackData.forEach(d => stackModuleSource[d.evSpec.heapProfModule] = null);
      for (let moduleName in stackModuleSource) {
        if (moduleName in oldModuleSource && oldModuleSource[moduleName]) {
          stackModuleSource[moduleName] = oldModuleSource[moduleName];
        } else if (moduleName in moduleMap) {
          console.log('fetching', moduleName);
          stackModuleSource[moduleName] = postData('http://localhost:3000/ext-stg/get-source-code', { stgbinPath: moduleMap[moduleName].stgbinPath });
        }
      }
    }
  }

  async function getSourceCodeForModule(moduleName) {
    if (moduleName in stackModuleSource) {
      const res = await stackModuleSource[moduleName];
      //console.log('getSourceCodeForModule', moduleName, res);
      return res.sourceCode;
    }
    return '<not found>';
  }

  function handleKeypress(event) {
    console.log(event);

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
    //console.log(dataCCS);
    dataSamples = dataAll.filter(e => e.evSpec.tag === 'HeapProfSampleCostCentre' || e.evSpec.tag === 'ProfSampleCostCentre');
    //console.log(dataSamples);
    costCentreMap = {};
    dataCCS.forEach(d => costCentreMap[d.evSpec.heapProfCostCentreId] = d);
    //console.log('costCentreMap', costCentreMap);

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

  const options = {};
  const observer = new IntersectionObserver(function(entries, observer) {
    entries.forEach(e => {
      const idx = e.target.dataset.boxIndex;
      nameBoxMap[idx].style.backgroundImage = e.isIntersecting ? 'linear-gradient(#8e508a, #5f5286)' : 'linear-gradient(#5f5286, #453b61)';
    });
  }, options);

  function registerNameBox(node, i) {
    nameBoxMap[i] = node;
    node.style.backgroundImage = 'linear-gradient(#8e508a, #5f5286)';
    node.style.backgroundImage = 'linear-gradient(#5f5286, #453b61)';
  }
  function registerSourceBox(node, i) {
    sourceBoxMap[i] = node;
    observer.observe(node);
  }
</script>

<nav class="my-nav">

  <label for="myfile-ghcstgapp">GHC-WPC ghc_stgapp file:</label>
  <input type="text" name="myfile-ghcstgapp" bind:value={ghcStgApp} title={ghcStgApp} style="flex-grow:1;">

</nav>

<div on:keydown={handleKeypress} tabindex=0>
<div style="height:21vmin; width:100%; padding: 0; margin:0; position:sticky; top:0; z-index:100; background-color: white;">
  <CostCentreStackHeap {eventlogData} bind:currentStackIndex={sampleIndex} timeMarker={currentStack && currentStack.evTime} width="100%" height="100%"/>
</div>

<div class="my-container">
  <div class="one">
    <h4>
      Cost Centre Stack
    </h4>

    {#each currentStackData as cc, i}
      <div class="list-group-item text-light"
           style="word-wrap:break-word; margin: 0.0em; background-image: linear-gradient(#5f5286, #453b61)"
           use:registerNameBox={i}
           data-box-index={i}
      >

        <span style="color: lightgrey;">{cc.evSpec.heapProfModule}</span>
        <br>
        <span style="color: white;">{cc.evSpec.heapProfLabel}</span>

      </div>
    {/each}

    <pre>
    {JSON.stringify(currentStack || {}, null, ' ')}
    </pre>
  </div>

  <div class="two">
    {#each currentStackData as cc, i}
      {#await getSourceCodeForModule(cc.evSpec.heapProfModule)}
        <div class="spinner-border text-primary"></div>
      {:then sourceCode}
        <div use:registerSourceBox={i} data-box-index={i}>
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

  .my-container {
    width: 100%;
    height: auto;

    display: flex;
    flex-direction: row;
    margin: 0;
  }

  .one {
    flex-basis: 20%;

    display: flex;
    flex-direction: column;

    padding: 0.5em;
  }

  .two {
    flex-basis: 80%;

    display: flex;
    flex-direction: column;

    padding: 0.5em;

    color: white;
  }

  .my-nav {
    width: 100%;
    height: auto;

    display: flex;
    align-items: center;

    background-image: linear-gradient(#8e508a, #453b61);
    background: #5f5286;
    color: white;
  }

  .my-nav > * {
    margin: 0.5em;
  }

</style>
