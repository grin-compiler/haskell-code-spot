<script>
  import { postData }     from './post-data.js';
  import { onMount }      from 'svelte';
  import HeapSize         from './HeapSize.svelte';
  import HeapLive         from './HeapLive.svelte';
  import HeapAllocated    from './HeapAllocated.svelte';
  import ActiveThreads    from './ActiveThreads.svelte';
  import RuntimeProfiling from './RuntimeProfiling.svelte'
  import CostCentreStack  from './CostCentreStack.svelte';

  let response;
  let restResponse = '';
  let eventlogFilepath = '/home/csaba/haskell/grin-compiler/ghc-wpc-sample-programs/game-logic-experiment/.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/minigame/minigame.eventlog';
  let eventlogOffset = 0;
  let eventlogIndex = 100000;
  let eventKinds = [];
  let eventlog;

  onMount(() => fetchEventData());

  // function webSocketTest()
  // {
  //   var ws = new WebSocket("ws://localhost:3000");

  //   ws.onopen = () => {
  //     ws.send("initial from js");
  //   };

  //   ws.onmessage = evt => {
  //     var m = evt.data;
  //     response = "WS received: " + m;
  //     console.log( m );
  //   };

  //   ws.onclose = function() {
  //     //response = "ws closed";
  //     //alert("ws closed");
  //   };

  //   window.onbeforeunload = evt => {
  //     socket.close();
  //   };
  // }

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

  let diagramModes = ['HeapSize', 'HeapLive', 'HeapAllocated', 'ActiveThreads', 'RuntimeProfiling', 'CostCentreStack'];
  let diagramMode = 'CostCentreStack';
  const diagramModeElim = cases => d => {
    return cases[diagramMode];
  }

  let diagramOptions = [
    { id: 1, text: 'Heap Size', value: 'HeapSize' },
    { id: 2, text: 'Heap Live Chart Zoo', value: 'HeapLive' },
    { id: 3, text: 'Heap Allocated', value: 'HeapAllocated' },
    { id: 4, text: 'Active Threads', value: 'ActiveThreads' },
    { id: 5, text: 'Runtime profiling', value: 'RuntimeProfiling' },
    { id: 6, text: 'Cost Centre Stack', value: 'CostCentreStack' }
  ];
  let diagramSelected;

  function handleDiagramSubmit() {
    diagramMode = diagramSelected.value;
    eventKinds = diagramModeElim({
      HeapSize:         ['HeapSize'],
      HeapLive:         ['HeapLive', 'HeapSize'],
      HeapAllocated:    ['HeapAllocated'],
      ActiveThreads:    ['RunThread', 'StopThread'],
      RuntimeProfiling: ['HeapProfCostCentre', 'HeapProfSampleCostCentre', 'ProfSampleCostCentre'],
      CostCentreStack:  ['HeapSize', 'HeapLive', 'HeapProfCostCentre', 'HeapProfSampleCostCentre', 'ProfSampleCostCentre', 'GCWork']
    })(diagramMode);
    fetchEventData();
  }

</script>

<style>

  :global(html) {
    font-size: 12px;
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

</style>
<nav class="my-nav">

<h3 class="my-title">
Haskell Code Spot
</h3>

      <select bind:value={diagramSelected} on:change="{ () => handleDiagramSubmit(diagramSelected) }">
      {#each diagramOptions as diagramOption}
        <option value={diagramOption} selected={diagramMode === diagramOption.value}>
          {diagramOption.text}
        </option>
      {/each}
    </select>

  <label for="myfile">Eventlog file:</label>
  <input type="text" name="myfile" bind:value={eventlogFilepath} title={eventlogFilepath} style="flex-grow:1;">
  <button on:click={fetchEventData}>Fetch Event Data</button>

</nav>

{#if diagramMode == 'HeapSize'}
<HeapSize eventlogData={eventlog}/>
{:else if diagramMode == 'HeapLive'}
<HeapLive eventlogData={eventlog}/>
{:else if diagramMode == 'HeapAllocated'}
<HeapAllocated eventlogData={eventlog}/>
{:else if diagramMode == 'ActiveThreads'}
<ActiveThreads eventlogData={eventlog}/>
{:else if diagramMode == 'RuntimeProfiling'}
<RuntimeProfiling eventlogData={eventlog}/>
{:else if diagramMode == 'CostCentreStack'}
<CostCentreStack eventlogData={eventlog}/>
{/if}
