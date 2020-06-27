<script>
  import CodeMirror from "./CodeMirror.svelte";

  async function postData(url = '', data = {}) {
    const response = await fetch(url, {
      method: 'POST',
      cache: 'no-cache',
      body: JSON.stringify(data)
    });
    return response.json();
  }

  let filepath = '/home/csaba/haskell/grin-compiler/ghc-wpc-sample-programs/game-logic-experiment/.stack-work/dist/x86_64-linux/Cabal-3.2.0.0/build/minigame/minigame.ghc_stgapp';

  async function stgappTest() {
    let data = await postData('http://localhost:3000/ghc_stgapp', { path: filepath });
    console.log('response: ', data);
  }

  let cm1;

  const btn = () => {
    cm1.set('let f import = () => {} data MyData = T | F :: Int');
  };

</script>
<!--
<div>
  <label for="myfile">ghc_stgapp file path:</label>
  <input type="text" name="myfile" bind:value={filepath}>
  <p>{filepath}</p>

  <button on:click={stgappTest}>test ghc_stgapp</button>
</div>
-->
<CodeMirror bind:this={cm1} />
<button on:click={btn}>change value</button>
