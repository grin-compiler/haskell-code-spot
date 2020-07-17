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

    // title
    g.append('text')
      .attr('x', innerWidth / 2)
      .attr('y', -11)
      .attr('text-anchor', 'middle')
      .attr('class', 'chart-title')
      .text('Heap Live');

    const xAxis = d3.axisBottom(xScale)
      .tickPadding(3);
//      .tickSize(-innerHeight);

    const yAxis = d3.axisLeft(yScale)
      .ticks(5)
      .tickFormat(d3.format('.3s'))
      .tickPadding(5)
      .tickSize(-innerWidth);

    g.append('g').call(yAxis);
    const gAxisX = g.append('g').call(xAxis)
      .attr('transform', `translate(0, ${innerHeight})`);

    const lineGenerator = d3.line()
      .x(d => xScale(xValue(d)))
      .y(d => yScale(yValue(d)))
      .curve(d3.curveBasis);

    const gPlot = g.append('g');

    // cost centre stack time cursor
    gPlot.append('g')
      .attr('class', 'time-marker');

    gPlot.append('path')
      .attr('class', 'line-path')
      .datum(data)
      .attr('d', lineGenerator);

    // show call stack event times
    const dataStack = dataAll.filter(e => e.evSpec.tag === 'ProfSampleCostCentre' || e.evSpec.tag === 'HeapProfSampleCostCentre');
    gPlot.selectAll('circle').data(dataStack)
        .enter().append('circle')
          .style('stroke', 'blue')
          .style('fill', 'blue')
          .style('fill-opacity',  0.2)
          .style('stroke-opacity', 0.5)
          .style('stroke-width',  1)
          .attr('cy', d => -4)
          .attr('cx', d => xScale(xValue(d)))
          .attr('r',  d => 2);
;

    // show GC event times
    const dataGC = dataAll.filter(
      e =>  //e.evSpec.tag === 'StartGC'// ||
            e.evSpec.tag === 'GCWork'//  ||
            //e.evSpec.tag === 'GCIdle'//  ||
            //e.evSpec.tag === 'GCDone'//  ||
            //e.evSpec.tag === 'EndGC'
    );
    gPlot.selectAll('line').data(dataGC)
        .enter().append('line')
          .style('stroke', 'green')
          .style('stroke-opacity', 0.5)
          .style('stroke-width', 1.5)
          .style('fill', 'none')
          .attr('y1', d => 0)
          .attr('x1', d => xScale(xValue(d)))
          .attr('y2', d => innerHeight)
          .attr('x2', d => xScale(xValue(d)));

    // setup brushing
    const brush = d3.brushX()
      .extent([[0, -margin.top], [innerWidth, height]])
      .on("end", updateChart);

    gPlot.append("g")
      .attr("class", "brush")
      .call(brush);

    // setup clip
    // Add a clipPath: everything out of this area won't be drawn.
    const clip = g.append("defs").append("svg:clipPath")
      .attr("id", "clip")
      .append("svg:rect")
      .attr("width", innerWidth )
      .attr("height", height )
      .attr("x", 0)
      .attr("y", -margin.top);
    gPlot.attr("clip-path", "url(#clip)");

    gPlot.on('click', function() {
      const coords = d3.mouse(this);
      const evTime = Math.round(xScale.invert(coords[0]) * 1000000000 );
      let bisectEv = d3.bisector(function(d) { return d.evTime; }).right;
      const i = bisectEv(dataStack, evTime);
      currentStackIndex = (i > 0 && Math.abs(dataStack[i].evTime - evTime) > Math.abs(dataStack[i-1].evTime - evTime)) ? i-1 : i;
    });

/*
    // setup zoom
    svg.call(d3.zoom().on('zoom', () => {
      //g.attr('transform', d3.event.transform);
      let new_x_scale = d3.event.transform.rescaleX(xScale);

      gAxisX.transition()
        .duration(1000)
        .call(xAxis.scale(new_x_scale));
    }));

*/


    // A function that set idleTimeOut to null
    let idleTimeout
    function idled() { idleTimeout = null; }

    // A function that update the chart for given boundaries
    function updateChart() {

      const extent = d3.event.selection

      // If no selection, back to initial coordinate. Otherwise, update X axis domain
      if(!extent){
        if (!idleTimeout) return idleTimeout = setTimeout(idled, 200); // This allows to wait a little bit
        xScale.domain([0, d3.max(data, xValue)])
      }else{
        xScale.domain([ xScale.invert(extent[0]), xScale.invert(extent[1]) ])
        svg.select(".brush").call(brush.move, null) // This remove the grey brush area as soon as the selection has been done
      }

      // Update axis and circle position
      gAxisX.transition().duration(1000).call(d3.axisBottom(xScale));

      gPlot
        .selectAll("circle")
        .transition().duration(1000)
        .attr('cx', d => xScale(xValue(d)))

      gPlot
        .selectAll("line")
        .transition().duration(1000)
          .attr('y1', d => 0)
          .attr('x1', d => xScale(xValue(d)))
          .attr('y2', d => innerHeight)
          .attr('x2', d => xScale(xValue(d)));
      gPlot
        .select('.line-path')
        .transition().duration(1000)
        .attr('d', lineGenerator);

      g
        .selectAll("g.time-marker rect")
        .transition().duration(1000)
        .attr('x', d => xScale(timeMarker/1000000000)-3);
      }
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
        .style('opacity', 0.6)
        .style('fill', 'orangered')
        .attr('y', d => -10)
        .attr('x', d => xScale(timeMarker/1000000000)-3)
        .attr('height', d => innerHeight+10)
        .attr('width', 6);
  }

  onMount(async () => {
    render3(eventlogData.dat.events);
  })
</script>

<svg bind:this={el3} {width} {height}></svg>
