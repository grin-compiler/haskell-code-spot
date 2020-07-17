<script>
  import { onMount } from 'svelte';
  import * as d3 from 'd3';

  export let width   = 960;
  export let height  = 500;

  export let timeMarker;
  export let currentStackIndex;

  let el3;
  let xValue, xScale, innerHeight, g;

  const render3 = dataAll => {

    if (el3 === undefined) {
      return;
    }

    const data = dataAll.filter(e => e.evSpec.tag === 'HeapLive');

    const svg     = d3.select(el3);
    const svgRect = svg.node().getBoundingClientRect();
    svg.selectAll("*").remove();

    const width   = svgRect.width;
    const height  = svgRect.height;

    const yValue = d => d.evSpec.liveBytes;
    xValue = d => d.evTime/1000000000;
    const margin = { top: 35, right: 10, bottom: 25, left:50 };
    const innerWidth  = width - margin.left - margin.right;
    innerHeight = height - margin.top - margin.bottom;

    // show heap live
    xScale = d3.scaleLinear()
      .domain([0, d3.max(data, xValue)])
      .range([0, innerWidth]);

    const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, yValue)])
      .range([innerHeight, 0]);

    g = svg.append('g')
      .attr('transform', `translate(${margin.left}, ${margin.top})`);

    g.append('g')
      .attr('class', 'time-marker');

    const xAxis = d3.axisBottom(xScale)
      .tickPadding(3);
//      .tickSize(-innerHeight);

    const yAxis = d3.axisLeft(yScale)
      .ticks(5)
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
      .attr('y', -11)
      .attr('text-anchor', 'middle')
      .text('Heap Live');

    // show call stack event times
    const dataStack = dataAll.filter(e => e.evSpec.tag === 'ProfSampleCostCentre' || e.evSpec.tag === 'HeapProfSampleCostCentre');
    g.selectAll('circle').data(dataStack)
        .enter().append('circle')
          .style('stroke', 'blue')
          .style('fill', 'blue')
          .attr('cy', d => 0)
          .attr('cx', d => xScale(xValue(d)))
          .attr('r',  d => 4)
          .on('click', function(d, i) {
            currentStackIndex = i;
          });

    // show GC event times
    const dataGC = dataAll.filter(
      e =>  //e.evSpec.tag === 'StartGC'// ||
            e.evSpec.tag === 'GCWork'//  ||
            //e.evSpec.tag === 'GCIdle'//  ||
            //e.evSpec.tag === 'GCDone'//  ||
            //e.evSpec.tag === 'EndGC'
    );
    g.selectAll('line').data(dataGC)
        .enter().append('line')
          .style('stroke', 'green')
          .style('stroke-opacity', 0.5)
          .style('stroke-width', 1.5)
          .style('fill', 'none')
          .attr('y1', d => 0)
          .attr('x1', d => xScale(xValue(d)))
          .attr('y2', d => innerHeight)
          .attr('x2', d => xScale(xValue(d)));

    /*
      var yMin = yScale.domain()[0];
    */
/*
    // setup brushing
    g.call(
      d3.brushX()
//        .extent(xScale.domain())
    );
*/
    /*
    // setup zoom
    svg.call(d3.zoom().on('zoom', () => {
      g.attr('transform', d3.event.transform);
    }));
    */

  };


  export let eventlogData = { dat: { events: [] }};
  $: {
    console.log('redraw svg');
    render3(eventlogData.dat.events);
  }

  // render time marker
  $: if (timeMarker) {
    d3.select('g.time-marker')
      .selectAll('rect')
      .data([timeMarker])
      .join('rect')
        .style('stroke', 'none')
        .style('opacity', 0.3)
        .style('fill', 'red')
        .attr('y', d => -10)
        .attr('x', d => xScale(timeMarker/1000000000)-4)
        .attr('height', d => innerHeight+20)
        .attr('width', 8);
  }

  onMount(async () => {
    render3(eventlogData.dat.events);
  })
</script>

<svg bind:this={el3} {width} {height}></svg>
