<script>
  import * as d3 from 'd3';
  import { postData } from './post-data.js';
  import { onMount } from 'svelte';
  import CodeMirror from "./CodeMirror.svelte";
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

  const re1 = /.*:(?<line>\d+):(?<column>\d+)$/;
  const re2 = /.*:(?<line>\d+):(?<start_column>\d+)-(?<end_column>\d+)$/;
  const re3 = /.*:\((?<start_line>\d+),(?<start_column>\d+)\)-\((?<end_line>\d+),(?<end_column>\d+)\)$/;
/*

    isPointRealSpan
      path:line:column
      /.*:(?<line>\d+):(?<column>\d+)$/

    isOneLineRealSpan
      path:line:start_column-end_column
      /.*:(?<line>\d+):(?<start_column>\d+)-(?<end_column>\d+)$/

    generic
      path:(start_line,start_column)-(end_line,end_column)
      /.*:\((?<start_line>\d+),(?<start_column>\d+)\)-\((?<end_line>\d+),(?<end_column>\d+)\)$/

    EXAMPLE:
      src3.match(re4).groups

*/
  function getSrcLocLine(srcLoc) {
    let m;
    if (m = srcLoc.match(re1)) {
      // path:line:column
      return +m.groups.line;
    } else if (m = srcLoc.match(re2)) {
      // path:line:start_column-end_column
      return +m.groups.line;
    } else if (m = srcLoc.match(re3)) {
      // path:(start_line,start_column)-(end_line,end_column)
      return +m.groups.start_line;
    }

    return 1;
  }

  function sliceToSrcLoc(sourceCode, srcLoc) {
    const lines = sourceCode.split('\n');
    let m;
    if (m = srcLoc.match(re1)) {
      // path:line:column
      return lines.slice(+m.groups.line -1, +m.groups.line).join('\n');
    } else if (m = srcLoc.match(re2)) {
      // path:line:start_column-end_column
      return lines.slice(+m.groups.line -1, +m.groups.line).join('\n');
    } else if (m = srcLoc.match(re3)) {
      // path:(start_line,start_column)-(end_line,end_column)
      return lines.slice(+m.groups.start_line -1, +m.groups.end_line).join('\n');
    }

    return sourceCode;
  }

  async function getSourceCodeForModule(moduleName) {
    if (moduleName in stackModuleSource) {
      const res = await stackModuleSource[moduleName];
      //console.log('getSourceCodeForModule', moduleName, res);
      return res.sourceCode;
    }
    return '<not found>';
  }

/*
  const btn = () => {
    cm1.slice(src, 19, 29);
    //console.log(cm1.getEditor().getViewport());
    //cm1.getEditor().scrollIntoView({line:10, ch:1});
    //cm1.getEditor().getDoc().setCursor(44);
    //cm1.getEditor().getDoc().setSelection({line:19, ch:0},{line:28, ch:0});
    //cm1.getEditor().scrollIntoView({line:111, ch:1});
    //cm1.getEditor().getDoc().markText({line:51, ch:0},{line:63, ch:0}, {css: "background-color: #f00e0350"});
    //console.log(cm1.getEditor().getViewport());
    cm1.getEditor().setSize(null, 'auto');
  };
*/

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

  function bgImage2(i, ccid) {
    //return 'linear-gradient(#8e508a, #8e508a)';
    //return 'linear-gradient(#5f5286, #5f5286)';
//    return 'linear-gradient(#8e508a, #5f5286)';
    return 'linear-gradient(to right, #8e508a, #5f5286)';
    return 'linear-gradient(to right, #5f5286, #8e508a)';
  }

  function bgImage(i, ccid) {
    if (i < 4)
    //return 'linear-gradient(to right, #8e508a, #5f5286)';
    return 'linear-gradient(#8e508a, #5f5286)';
//    return 'linear-gradient(#8e508a, #453b61)';
//    return 'linear-gradient(#5f5286, #8e508a)';
    return 'linear-gradient(#5f5286, #453b61)'; // super
    
    return 'linear-gradient(#8e508a, #453b61)';
    return 'linear-gradient(#8e508a, #5f5286, #453b61)';
  }

  function bgColor(i, ccid) {

  //  if (i === 0)
      return '#5f5286';
      return '#453b61';

      return '#8e508a';
      //return 'darkgreen';



    if (i % 2 === 0)
      return 'orange';
    return '#8e508a';
    return '#453b61';
    return '#5f5286';

    /*
      453b61
      5f5286
      8e508a
    */
    //return 'orange';
    if (i % 2 === 0)
      return 'dodgerblue';
    return 'MediumSeaGreen';

    if (i % 2 === 0)
      return 'orange';
    return 'slateblue';

    if (i % 2 === 0)
      return '#453b61';
    return '#8e508a';
    return '#5f5286';
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
               class:font-weight-bolder="{i === index}"
               style="word-wrap:break-word;background-image: {bgImage(i, cc.evSpec.heapProfCostCentreId)}; margin: 0.0em;"
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
        <SourceRangeBox
          srcLocString={cc.evSpec.heapProfSrcLoc}
          moduleName={cc.evSpec.heapProfModule}
          functionLabel={cc.evSpec.heapProfLabel}

          sourceCode={sourceCode}
          packageName={moduleMap && moduleMap[cc.evSpec.heapProfModule] && moduleMap[cc.evSpec.heapProfModule].packageName || ''}
        />
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

    //background: orangered;
    //background: #5f5286;
/*
    return '#8e508a';
    return '#453b61';
    return '#5f5286';
*/
    background-image: linear-gradient(to right, #8e508a, #5f5286);
    background-image: linear-gradient(to right, #5f5286, #8e508a); /* ok */
    background-image: linear-gradient(to right, #453b61, #8e508a);
    background-image: linear-gradient(#453b61, #8e508a);
    background-image: linear-gradient(#8e508a, #453b61);
    background: #5f5286;
    color: white;
  }

  .my-nav > * {
    margin: 0.5em;
  }

</style>
