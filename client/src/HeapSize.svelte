<script>
  import { onMount } from 'svelte';
  import * as d3 from 'd3';

  let el;
  let el2;

  const render = data => {

    if (el === undefined) {
      return;
    }

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

    if (el2 === undefined) {
      return;
    }

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

  export let eventlogData = { dat: { events: [] }};
  $: {
    render(eventlogData.dat.events);
    render2(eventlogData.dat.events);
  }

  onMount(async () => {
    render(eventlogData.dat.events);
    render2(eventlogData.dat.events);
  })

</script>

<svg bind:this={el} width="960" height="500"></svg>
<svg bind:this={el2} width="960" height="500"></svg>
