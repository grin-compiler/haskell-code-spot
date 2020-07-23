<script>
  import { onMount } from 'svelte';
  import * as d3 from 'd3';
  import * as c3 from 'c3';

  let el3;

  const renderD3 = data => {

    if (el3 === undefined) {
      return;
    }

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
      .attr('class', 'chart-title')
      .text('Heap Live with D3');
  };

  export let eventlogData = { dat: { events: [] }};
  let chart;

  function renderC3() {
    const heapLiveX = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapLive').map(d => d.evTime / 1000000000);
    const heapLive  = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapLive').map(d => d.evSpec.liveBytes);
    const heapSizeX = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapSize').map(d => d.evTime / 1000000000);
    const heapSize  = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapSize').map(d => d.evSpec.sizeBytes);
    heapLiveX.unshift('Heap Live X');
    heapLive.unshift('Heap Live');
    heapSizeX.unshift('Heap Size X');
    heapSize.unshift('Heap Size');
    chart = c3.generate({
      bindto: '#chart',
      data: {
        xs : {
          'Heap Live': 'Heap Live X',
          'Heap Size': 'Heap Size X'
        },
        columns: [
          heapLiveX, heapLive,
          heapSizeX, heapSize
        ],
        type: 'spline'
      },
      spline: {
        interpolation: {
          type: 'linear'
        }
      },
      /*
      legend: {
        position: 'inset'
      },
      */
      grid: {
        x: {
          show: true
        },
        y: {
          show: true
        }
      },
      zoom: {
        enabled: true
      },
      axis : {
        x : {
          type: 'indexed',
          tick: {
              fit: false,
              format: d => { return d3.format(".12~s")(d) + "s"; }
          }
        },
        y : {
          tick: {
              format: d => { return d3.format(".2s")(d).toUpperCase() + "B"; }
          }
        }
    }
    });
  }

  let plotly_data = [{
    x: ['giraffes', 'orangutans', 'monkeys'],
    y: [20, 14, 23],
    type: 'bar'
  }];

  let mounted = false;
  $: {
    renderD3(eventlogData.dat.events);
    renderC3();
    renderPlotly();
  }
  onMount(async () => {
    mounted = true;
    renderD3(eventlogData.dat.events);
    renderC3();
    renderPlotly();
  });

  function renderPlotly() {
    if (!mounted) return;
    const heapLiveX = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapLive').map(d => d.evTime / 1000000000);
    const heapLive  = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapLive').map(d => d.evSpec.liveBytes);
    const heapSizeX = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapSize').map(d => d.evTime / 1000000000);
    const heapSize  = eventlogData.dat.events.filter(e => e.evSpec.tag === 'HeapSize').map(d => d.evSpec.sizeBytes);

    let traceHeapLive = {
      x: heapLiveX,
      y: heapLive,
      mode: 'lines+markers',
      name: 'Heap Live',
      type: 'scatter'
    };

    let traceHeapSize = {
      x: heapSizeX,
      y: heapSize,
      mode: 'lines+markers',
      name: 'Heap Size',
      type: 'scatter'
    };

    let data = [traceHeapLive, traceHeapSize];

    Plotly.newPlot('myDiv', data);
  }
</script>

<svg bind:this={el3} width="960" height="500"></svg>
<section style="margin: 3em 0;">
  <h3 style="text-align: center;">Heap Live & Size <em>with C3</em></h3>
  <div id="chart"></div>
</section>

<section style="margin: 3em 0;">
  <h3 style="text-align: center;">Heap Live & Size <em>with plotly.js</em></h3>
  <div id="myDiv"></div>
</section>