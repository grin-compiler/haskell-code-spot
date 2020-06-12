<script>
  import { onMount } from 'svelte';
  import * as d3 from 'd3';

  let name = 'world';
  let response;
  let restResponse = '';
  let eventlogFilepath = '/home/csaba/haskell/lambdacube-quake3/q3mapviewer.eventlog';
  // let eventlogFilepath = '/home/andorp/Sources/grin-tech/grin/grin.eventlog';
  let eventlog;

  onMount(() => restTest());

  function webSocketTest()
  {
    var ws = new WebSocket("ws://localhost:3000");

    ws.onopen = () => {
      ws.send("initial from js");
    };

    ws.onmessage = evt => {
      var m = evt.data;
      response = "WS received: " + m;
      console.log( m );
    };

    ws.onclose = function() {

      //response = "ws closed";
      //alert("ws closed");
    };

    window.onbeforeunload = evt => {
      socket.close();
    };
  }

  let el, el2, el3, el4;

  async function restTest() {
    let uri = `http://localhost:3000/eventlog/${btoa(eventlogFilepath)}?offset=0&idx=10000`;
    console.log("send:", uri);
    let response = await fetch(uri);
    let data = await response.json();
    restResponse = "<h3> got eventlog json, check console </h3>";
    console.log('response: ', data);
    eventlog = data;

    let evs = data.dat.events;
    render(evs);
    render2(evs);
    render3(evs);
    render4(evs);
  }


  const render = data => {

    data = data.filter(e => e.evSpec.tag === 'HeapSize');

    const svg     = d3.select(el);
    const width   = +svg.attr('width');
    const height  = +svg.attr('height');

    const xValue = d => d.evSpec.sizeBytes;
    const yValue = d => d.evTime/1000000000;
    const margin = { top: 40, right: 20, bottom: 20, left:50 };
    const innerWidth  = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;

    const xScale = d3.scaleLinear()
      .domain([0, d3.max(data, xValue)])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, yValue)])
      .range([0, innerHeight]);

    const g = svg.append('g')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

    const xAxis = d3.axisBottom(xScale)
      .tickFormat(d3.format('.3s'))
      .tickSize(-innerHeight);

    g.append('g').call(d3.axisLeft(yScale));
    g.append('g').call(xAxis)
      .attr('transform', `translate(0, ${innerHeight})`);

    g.selectAll('rect').data(data)
        .enter().append('rect')
          .attr('y', d => yScale(yValue(d)))
          .attr('width', d => xScale(xValue(d)))
          .attr('height', d => 0.1);

    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .text('Heap Size');
  };


  const render2 = data => {

    data = data.filter(e => e.evSpec.tag === 'HeapSize');

    const svg     = d3.select(el2);
    const width   = +svg.attr('width');
    const height  = +svg.attr('height');

    const yValue = d => d.evSpec.sizeBytes;
    const xValue = d => d.evTime/1000000000;
    const margin = { top: 40, right: 20, bottom: 30, left:70 };
    const innerWidth  = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;

    const xScale = d3.scaleLinear()
      .domain([0, d3.max(data, xValue)])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, yValue)])
      .range([innerHeight, 0]);

    const g = svg.append('g')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

    const xAxis = d3.axisBottom(xScale)
      .tickPadding(10)
      .tickSize(-innerHeight);

    const yAxis = d3.axisLeft(yScale)
      .tickFormat(d3.format('.3s'))
      .tickPadding(5)
      .tickSize(-innerWidth);

    g.append('g').call(yAxis);
    g.append('g').call(xAxis)
      .attr('transform', `translate(0, ${innerHeight})`);

    const lineGenerator = d3.line()
      .x(d => xScale(xValue(d)))
      .y(d => yScale(yValue(d)))
      .curve(d3.curveBasis);

    g.append('path')
      .attr('class', 'line-path')
      .attr('d', lineGenerator(data));

    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .text('Heap Size');
  };

  const render3 = data => {

    data = data.filter(e => e.evSpec.tag === 'HeapLive');

    const svg     = d3.select(el3);
    const width   = +svg.attr('width');
    const height  = +svg.attr('height');

    const yValue = d => d.evSpec.liveBytes;
    const xValue = d => d.evTime/1000000000;
    const margin = { top: 40, right: 20, bottom: 30, left:70 };
    const innerWidth  = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;

    const xScale = d3.scaleLinear()
      .domain([0, d3.max(data, xValue)])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, yValue)])
      .range([innerHeight, 0]);

    const g = svg.append('g')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

    const xAxis = d3.axisBottom(xScale)
      .tickPadding(10)
      .tickSize(-innerHeight);

    const yAxis = d3.axisLeft(yScale)
      .tickFormat(d3.format('.3s'))
      .tickPadding(5)
      .tickSize(-innerWidth);

    g.append('g').call(yAxis);
    g.append('g').call(xAxis)
      .attr('transform', `translate(0, ${innerHeight})`);

    const lineGenerator = d3.line()
      .x(d => xScale(xValue(d)))
      .y(d => yScale(yValue(d)))
      .curve(d3.curveBasis);

    g.append('path')
      .attr('class', 'line-path')
      .attr('d', lineGenerator(data));

    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .text('Heap Live');
  };

  const render4 = data => {

    data = data.filter(e => e.evSpec.tag === 'HeapAllocated');

    const svg     = d3.select(el4);
    const width   = +svg.attr('width');
    const height  = +svg.attr('height');

    const yValue = d => d.evSpec.allocBytes;
    const xValue = d => d.evTime/1000000000;
    const margin = { top: 40, right: 20, bottom: 30, left:70 };
    const innerWidth  = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;

    const xScale = d3.scaleLinear()
      .domain([0, d3.max(data, xValue)])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, yValue)])
      .range([innerHeight, 0]);

    const g = svg.append('g')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

    const xAxis = d3.axisBottom(xScale)
      .tickPadding(10)
      .tickSize(-innerHeight);

    const yAxis = d3.axisLeft(yScale)
      .tickFormat(d3.format('.3s'))
      .tickPadding(5)
      .tickSize(-innerWidth);

    g.append('g').call(yAxis);
    g.append('g').call(xAxis)
      .attr('transform', `translate(0, ${innerHeight})`);

    const lineGenerator = d3.line()
      .x(d => xScale(xValue(d)))
      .y(d => yScale(yValue(d)))
      .curve(d3.curveBasis);

    g.append('path')
      .attr('class', 'line-path')
      .attr('d', lineGenerator(data));

    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .text('Heap Allocated');
  };
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

<svg bind:this={el} width="960" height="500"></svg>

<svg bind:this={el2} width="960" height="500"></svg>

<svg bind:this={el3} width="960" height="500"></svg>

<svg bind:this={el4} width="960" height="500"></svg>

<h1>Hello {name}!</h1>

<input bind:value={name}>

<div>
  <button on:click={webSocketTest}>websocket test</button>
  <button on:click={restTest}>rest test</button>
</div>

<p>{response}</p>

<div>
<h2>rest response</h2>
{@html restResponse}
</div>

<div>
  <label for="myfile">Eventlog file path:</label>
  <input type="text" name="myfile" bind:value={eventlogFilepath}>
  <p>{eventlogFilepath}</p>
</div>



{#if 0 && eventlog && eventlog.header.eventTypes}
<ul>
  {#each eventlog.header.eventTypes as et}
    <li>
      {et.desc}
    </li>
  {/each}
</ul>
{/if}


{#if eventlog && eventlog.dat.events}
<ul>
  {#each eventlog.dat.events as e}
      <li>
        {e.evTime}: {e.evSpec.tag}
      </li>
  {/each}
</ul>
{/if}



