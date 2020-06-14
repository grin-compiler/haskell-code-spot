<script>
  import { onMount } from 'svelte';
  import { postData } from './post-data.js';

  let sourceFileRoot = '/home/andorp/Sources/grin-tech/grin/grin/'
  let sourceFilePath = 'src/Pipeline/Pipeline.hs';
  let fullStackTrace = [];
  let moduleStackTrace = [];
  let currentProgramStep = 0;
  let content = `
Click on fetch file content...
  `;

  async function fetchFileContent() {
    let data = await postData('http://localhost:3000/fileview',{
      path: sourceFileRoot + sourceFilePath,
    });
    content = data.content;
  }

  const render = data => {
    fetchFileContent();
    calcFullStackTrace(data);
    calcActiveModuleStackTrace();
  };

  export let eventlogData = { dat: { events: [] }};
  $: {
    render(eventlogData.dat.events);
  }

  onMount(async () => {
    render(eventlogData.dat.events);
  })

  const calcFullStackTrace = data => {
    // Filter out cost center creation that is associated with the current file.
    const moduleCostCenterCreation = data.filter(e => (e.evSpec.tag === 'HeapProfCostCentre'));
    // Map cost center IDs to cost centers.
    const moduleCostCenters = new Map();
    moduleCostCenterCreation.forEach(e => {
      moduleCostCenters.set(e.evSpec.heapProfCostCentreId, e);
    });

    // Keep only locations in call-stack.
    const locationsInStack = stack => {
      let locations = [];
      stack.forEach(s => {
        const costCenter = moduleCostCenters.get(s);
        if (costCenter !== undefined && costCenter.evSpec.heapProfSrcLoc.includes(':')) {
          locations.push(costCenter.evSpec.heapProfSrcLoc);
        }});
      return locations;
    };

    // Call-stack of every interesting sampling event.
    const eventLocations = [];
    data.forEach(e => {
      let locations = [];
      if (e.evSpec.tag === 'HeapProfSampleCostCentre') {
        locations = locationsInStack(e.evSpec.heapProfStack);
      } else if (e.evSpec.tag === 'ProfSampleCostCentre') {
        locations = locationsInStack(e.evSpec.profCcsStack);
      }
      if (locations.length !== 0) {
        eventLocations.push(locations);
      }
    });
    fullStackTrace = eventLocations;
  };

  function calcActiveModuleStackTrace() {
    // Filter out events that mention  sourceFilePath
    let moduleStackTraceLocal = fullStackTrace.filter(stackTrace =>
      (stackTrace.findIndex(entry => entry.includes(sourceFilePath))) > (-1)
    );
    // Find the first element in the stack trace which starts with the current module.
    moduleStackTrace = moduleStackTraceLocal.map(xs => { return {
        srcLoc: (xs[xs.findIndex(entry => entry.includes(sourceFilePath))]),
        stackTrace: xs
      }});
  };

  function renderActiveFrameLoc(current) {
    if (moduleStackTrace[current] === undefined) {
      return '<???>';
    } else {
      return moduleStackTrace[current].srcLoc;
    }
  }

  function pad(num, size) {
      var s = "000000000" + num;
      return s.substr(s.length-size);
  }

  function createSourceRange(lineinfo) {
    const parts = lineinfo.split(':');
    if (parts.length == 3) {
      // src/Pipeline/Pipeline.hs:374:38-75
      const range = parts[2].split('-');
      return {
        line:  +parts[1],
        start: +range[0],
        end:   +range[1]
      };
    } else if (parts.length == 2) {
      // src/Pipeline/Pipeline.hs:(357,1)-(398,37)
      const range = parts[1].split('-');
      const blockStart = range[0].split(',');
      const blockEnd   = range[1].split(',');
      return {
        blockStartLine: blockStart[0].substring(1),
        blockStartChar: blockStart[1].substring(0,blockStart[1].length - 1),
        blockEndLine:   blockEnd[0].substring(1),
        blockEndChar:   blockEnd[1].substring(0,blockEnd[1].length - 1)
      };
    } else {
      return undefined;
    }
  }

  // Handle the src/Pipeline/Pipeline.hs:374:38-75 case
  function insertLineAnnotation(line, start, end, stacktrace) {
    const s = start - 1;
    const e = end + 1;
    return line.substring(0, s) + '<span style="color:red" title="' + stacktrace + '">' + line.substring(s, e) + '</span>' + line.substring(e);
  }

  // Handle the src/Pipeline/Pipeline.hs:(357,1)-(398,37) case
  function insertBlockAnnotation(line, start, end, stacktrace) {
    if (start !== undefined) {
      const s = start - 1;
      return line.substring(0,s) + '<span style="color:red" title="' + stacktrace + '">' + line.substring(s);
    } else if (end !== undefined) {
      const e = end + 1;
      return line.substring(0,e) + '</span>' + line.substring(e);
    } else {
      return line;
    }
  }

  function annotateContent(current) {
    if (moduleStackTrace[current] === undefined) {
      return content;
    }
    const stacktrace = moduleStackTrace[current].stackTrace.join('\n');
    const lines = content.split(/\r\n|\r|\n/);
    const padSize = ('' + lines.length).length;
    let annotatedContent = '';
    const sourceRange = createSourceRange(moduleStackTrace[current].srcLoc);
    lines.forEach((l, i) => {
      let line = l;
      if (sourceRange !== undefined) {
        if (sourceRange.line !== undefined) {
          if (i == sourceRange.line) {
            line = insertLineAnnotation(l, sourceRange.start, sourceRange.end, stacktrace);
          }
        } else if (sourceRange.blockStartLine !== undefined) {
          if (i == sourceRange.blockStartLine) {
            line = insertBlockAnnotation(l, sourceRange.blockStartChar, undefined, stacktrace);
          } else if (i == sourceRange.blockEndLine) {
            line = insertBlockAnnotation(l, undefined, sourceRange.blockEndChar, stacktrace);
          }
        }
      }
      annotatedContent += pad(i, padSize) + ' ' + line + '\r\n';
    });
    return annotatedContent;
  }
</script>

<style>
</style>

<div>
  <label for="sourcefile">Source file path:</label>
  <input type="text" name="sourcefile" bind:value={sourceFilePath}>
  <p>{sourceFilePath}</p>
  <button on:click={fetchFileContent}>Fetch File Content</button>
</div>

<div>
<label>
  <input type=number bind:value={currentProgramStep} min=0 max={moduleStackTrace.length - 1}>
  <input type=range  bind:value={currentProgramStep} min=0 max={moduleStackTrace.length - 1}>
</label>
<p>{renderActiveFrameLoc(currentProgramStep)}</p>
</div>

<code style="white-space: pre-wrap">
{@html (annotateContent(currentProgramStep))}
</code>
