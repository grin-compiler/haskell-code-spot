/*
    isPointRealSpan
      path:line:column
      /.*:(?<line>\d+):(?<column>\d+)$/

    isOneLineRealSpan
      path:line:start_column-end_column
      /.*:(?<line>\d+):(?<start_column>\d+)-(?<end_column>\d+)$/

    generic
      path:(start_line,start_column)-(end_line,end_column)
      /.*:\((?<start_line>\d+),(?<start_column>\d+)\)-\((?<end_line>\d+),(?<end_column>\d+)\)$/

    EXAMPLE:
      src3.match(re4).groups
*/

// source location parser
const re1 = /.*:(?<line>\d+):(?<column>\d+)$/;
const re2 = /.*:(?<line>\d+):(?<start_column>\d+)-(?<end_column>\d+)$/;
const re3 = /.*:\((?<start_line>\d+),(?<start_column>\d+)\)-\((?<end_line>\d+),(?<end_column>\d+)\)$/;

export function parseSrcLoc(srcLoc) {
  let m;
  if (m = srcLoc.match(re1)) {
    // path:line:column
    return {
      startLineNumber: +m.groups.line,
      endLineNumber:   +m.groups.line,
      startColumn: +m.groups.column,
      endColumn:   +m.groups.column
    };
  } else if (m = srcLoc.match(re2)) {
    // path:line:start_column-end_column
    return {
      startLineNumber: +m.groups.line,
      endLineNumber:   +m.groups.line,
      startColumn: +m.groups.start_column,
      endColumn:   +m.groups.end_column
    };
  } else if (m = srcLoc.match(re3)) {
    // path:(start_line,start_column)-(end_line,end_column)
    return {
      startLineNumber: +m.groups.start_line,
      endLineNumber:   +m.groups.end_line,
      startColumn: +m.groups.start_column,
      endColumn:   +m.groups.end_column
    };
  }
  return {
    startLineNumber: 1,
    endLineNumber: undefined, // marks the end of the document
    startColumn: 1,
    endColumn: 1
  };
}
