<script>
  let name = 'world';
  let response;
function webSocketTest()
{
  var ws = new WebSocket("ws://localhost:3000");

  ws.onopen = () => {
    ws.send("initial from js");
  };

  ws.onmessage = evt => {
    var m = evt.data;
    response = m;
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

</script>

<h1>Hello {name}!</h1>

<button on:click={webSocketTest}>websocket test</button>
<p>{response}</p>
