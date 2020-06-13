<script>
  import { onMount }    from 'svelte';
  import HeapSize       from './HeapSize.svelte';
  import HeapLive       from './HeapLive.svelte';
  import HeapAllocated  from './HeapAllocated.svelte';
  import ActiveThreads  from './ActiveThreads.svelte';
  import * as d3 from 'd3';
  import SourceView from './SourceView.svelte';


  let response;
  let restResponse = '';
  let eventlogFilepath = '../data/grin.eventlog';
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
    // let uri = `http://localhost:3000/eventlog/${btoa(eventlogFilepath)}`;
    let uri = `http://localhost:3000/eventlog/${btoa(eventlogFilepath)}?offset=0&idx=100000`;
    // let uri = `http://localhost:3000/eventlog/${btoa(eventlogFilepath)}?event-type=RunThread&event-type=StopThread`;
    console.log("send:", uri);
    let response = await fetch(uri);
    let data = await response.json();
    restResponse = "<h3> got eventlog json, check console </h3>";
    console.log('response: ', data);
    eventlog = data;
  }

  let diagramModes = ['HeapSize', 'HeapLive', 'HeapAllocated', 'ActiveThreads']
  let diagramMode = 'HeapSize';

  let diagramOptions = [
    { id: 1, text: 'Heap Size', value: 'HeapSize' },
    { id: 2, text: 'Heap Live', value: 'HeapLive' },
    { id: 3, text: 'Heap Allocated', value: 'HeapAllocated' },
    { id: 4, text: 'Active Threads', value: 'ActiveThreads' }
  ];
  let diagramSelected;
  function handleDiagramSubmit() {
    diagramMode = diagramSelected.value;
    eventlog = eventlog;
  }
</script>

<style>
  :global(rect) {
    fill: steelblue
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

  :global(circle) {
    fill: steelblue;
    opacity: 0.1;
  }

  :global(text) {
    font-size: 2em;
    font-family: sans-serif;
  }

  :global(.tick text) {
    font-size: 1.7em;
    font-family: sans-serif;
    fill: #635F5D;
  }

  :global(.tick line) {
    stroke: #C0C0BB;
  }
</style>

<SourceView/>

<div>
  <label for="myfile">Eventlog file path:</label>
  <input type="text" name="myfile" bind:value={eventlogFilepath}>
  <p>{eventlogFilepath}</p>
  <button on:click={fetchEventData}>Fetch Event Data</button>
</div>

<form on:change="{ () => handleDiagramSubmit(diagramSelected) }">
  <select bind:value={diagramSelected}>
    {#each diagramOptions as diagramOption}
      <option value={diagramOption}>
        {diagramOption.text}
      </option>
    {/each}
  </select>
</form>

{#if diagramMode == 'HeapSize'}
<HeapSize eventlogData={eventlog}/>
{:else if diagramMode == 'HeapLive'}
<HeapLive eventlogData={eventlog}/>
{:else if diagramMode == 'HeapAllocated'}
<HeapAllocated eventlogData={eventlog}/>
{:else if diagramMode == 'ActiveThreads'}
<ActiveThreads eventlogData={eventlog}/>
{/if}
