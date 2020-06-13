<script>
  import { onMount } from 'svelte';
  import * as d3 from 'd3';

  let elThreadInfo;

  const renderThreads = data => {
    if (elThreadInfo === undefined) {
      return;
    }

    const timescale = 1000000000;
    data = data.filter(e => (e.evSpec.tag === 'RunThread') || (e.evSpec.tag === 'StopThread'));

    let activeThreadsData = [];
    let activeThreads = [];
    data.forEach(e => {
      // e represents a RunThread or a StopThread.
      // Run adds, Stop removes the thread from the active threads array.
      const idx = activeThreads.indexOf(e.evSpec.thread);
      if (e.evSpec.tag === 'RunThread') {
        if (idx === -1) {
          activeThreads.push(e.evSpec.thread); // Add element
        }
      } else if (e.evSpec.tag === 'StopThread') {
        if (idx !== -1) {
          activeThreads.splice(idx, 1); // Remove element
        }
      }
      e.activeThreadCount = activeThreads.length;
    });

    const svg    = d3.select(elThreadInfo);
    const width  = +svg.attr('width');
    const height = +svg.attr('height');

    const yValue = d => d.activeThreadCount;
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
      .y(d => yScale(yValue(d)));

    g.append('path')
      .attr('class', 'line-path')
      .attr('d', lineGenerator(data));

    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -10)
      .attr('text-anchor', 'middle')
      .text('Number of active threads');

  };

  export let eventlogData = { dat: { events: [] }};
  $: {
    renderThreads(eventlogData.dat.events);
  }

  onMount(async () => {
    renderThreads(eventlogData.dat.events);
  })
</script>

<svg bind:this={elThreadInfo} width="960" height="500"></svg>
