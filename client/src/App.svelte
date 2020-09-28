<script>
  import { postData }     from './post-data.js';
  import { onMount }      from 'svelte';
  import HeapSize         from './HeapSize.svelte';
  import HeapLive         from './HeapLive.svelte';
  import HeapAllocated    from './HeapAllocated.svelte';
  import ActiveThreads    from './ActiveThreads.svelte';
  import CostCentreStack  from './CostCentreStack.svelte';
  import ModuleBrowser    from './ModuleBrowser.svelte';

  let response;
  let restResponse = '';
  //let eventlogFilepath = '/home/csaba/haskell/grin-compiler/ghc-wpc-sample-programs/game-logic-experiment/.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/minigame/minigame.eventlog';
  let eventlogFilepath = '../data/grin.eventlog';
  let eventlogOffset = 0;
  let eventlogIndex = 100000;
  let eventKinds = [];
  let eventlog;
  let useEventlog = true;


  async function fetchEventData() {
    let data = await postData('http://localhost:3000/eventlog',{
      path: eventlogFilepath,
      offset: eventlogOffset,
//      idx: eventlogIndex,
      'event-type': eventKinds
    });
    restResponse = "<h3> got eventlog json, check console </h3>";
    console.log('response: ', data);
    eventlog = data;
  }

  let diagramOptions = [
    { text:   'Heap Size'
    , value:  'HeapSize'
    , eventlogFilter: ['HeapSize']
    },
    { text:   'Heap Live (Chart Zoo)'
    , value:  'HeapLive'
    , eventlogFilter: ['HeapLive', 'HeapSize']
    },
    { text:   'Heap Allocated'
    , value:  'HeapAllocated'
    , eventlogFilter: ['HeapAllocated']
    },
    { text:   'Active Threads'
    , value:  'ActiveThreads'
    , eventlogFilter: ['RunThread', 'StopThread']
    },
    { text:   'Cost Centre Stack'
    , value:  'CostCentreStack'
    , eventlogFilter: ['HeapSize', 'HeapLive', 'HeapProfCostCentre', 'HeapProfSampleCostCentre', 'ProfSampleCostCentre', 'GCWork']
    },
    { text:   'Module Browser'
    , value:  'ModuleBrowser'
    , eventlogFilter: null  // not all view requires eventlog data
    }
  ];

  let diagramMode = 'CostCentreStack';
  let diagramSelected;

  $: if (diagramSelected) {
    diagramMode = diagramSelected.value;
    eventKinds = diagramSelected.eventlogFilter;
    // not all view requires eventlog data
    useEventlog = eventKinds !== null;
    if (useEventlog) {
      fetchEventData();
    }
  }

</script>

<style>

  :global(html, body) {
    font-size: 12px;
    padding: 0;
  }

  :global(text.chart-title) {
    font-size: 1.7em;
    font-family: sans-serif;
  }

  :global(.line-path) {
    fill: none;
    stroke: maroon;
    stroke-width: 2;
    stroke-linejoin: round;
  }

  :global(.live-line-path) {
    fill: none;
    stroke: steelblue;
    stroke-width: 2;
    stroke-linejoin: round;
  }

  :global(.tick text) {
    font-size: 1.2em;
    font-family: sans-serif;
    fill: #635F5D;
  }

  :global(.tick line) {
    stroke: #C0C0BB;
  }

  .my-nav {
    width: 100%;
    height: auto;

    display: flex;
    align-items: center;

    background: #5f5286;
    color: white;
  }

  .my-nav > * {
    margin: 0.5em;
  }

  .my-title {
    padding: 0.1em 0.3em;
    margin: 0;
  }

  .fullscreen {
    height: 100vh;
    display: flex;
    flex-flow: column;
    align-items: stretch;
    background: yellow;
  }

  .top-container {
    padding: 0.5em;
  }

</style>
<div class:fullscreen="{diagramMode === 'ModuleBrowser'}" class="top-container">
<nav class="my-nav">

  <h3 class="my-title">
  Haskell Code Spot
  </h3>

  <select bind:value={diagramSelected}>
    {#each diagramOptions as diagramOption}
      <option value={diagramOption} selected={diagramMode === diagramOption.value}>
        {diagramOption.text}
      </option>
    {/each}
  </select>

  <label for="myfile">Eventlog file:</label>
  <input type="text" name="myfile" bind:value={eventlogFilepath} title={eventlogFilepath} style="flex-grow:1;">
  <button on:click={fetchEventData} disabled='{!useEventlog}'>Fetch Event Data</button>

</nav>

{#if diagramMode === 'HeapSize'}
<HeapSize eventlogData={eventlog}/>
{:else if diagramMode === 'HeapLive'}
<HeapLive eventlogData={eventlog}/>
{:else if diagramMode === 'HeapAllocated'}
<HeapAllocated eventlogData={eventlog}/>
{:else if diagramMode === 'ActiveThreads'}
<ActiveThreads eventlogData={eventlog}/>
{:else if diagramMode === 'CostCentreStack'}
<CostCentreStack eventlogData={eventlog}/>
{:else if diagramMode === 'ModuleBrowser'}
<ModuleBrowser/>
{/if}
</div>