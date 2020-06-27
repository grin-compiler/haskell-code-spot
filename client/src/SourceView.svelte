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

  let cm1, editor;

  const btn = () => {
    cm1.set(`let f import = () => {} data MyData = T | F :: Int
module GHCPrimOp where

import Data.Int
import Data.Word

-- 64 bit platform semantics
type PrimWord = Word64
type PrimInt  = Int64

data Value
  = CharV   Word32  -- HINT: utf32 encoding
  | IntV    PrimInt
  | WordV   PrimWord
  | FloatV  Float   -- 32 bit floating point
  | DoubleV Double  -- 64 bit floating point
  | TupleV  [Value] -- HINT: tuples can not contain tuples, just simple values i.e. int, word, char
  deriving (Eq, Ord, Show)

data PrimOp
  -- Char#
  = CharGtOp
  | CharGeOp
  | CharEqOp
  | CharNeOp
  | CharLtOp
  | CharLeOp
  | OrdOp
  -- Int#
  | IntAddOp
  | IntSubOp
  | IntMulOp
  | IntMulMayOfloOp
  | IntQuotOp
  | IntRemOp
  | IntQuotRemOp
  | AndIOp
  | OrIOp
  | XorIOp
  | NotIOp
  | IntNegOp
  | IntAddCOp
  | IntSubCOp
  | IntGtOp
  | IntGeOp
  | IntEqOp
  | IntNeOp
  | IntLtOp
  | IntLeOp
  | ChrOp
  | Int2WordOp
  | Int2FloatOp
  | Int2DoubleOp
  | Word2FloatOp
  | Word2DoubleOp
  | ISllOp
  | ISraOp
  | ISrlOp
  -- Word#
  | WordAddOp
  | WordAddCOp
  | WordSubCOp
  | WordAdd2Op
  | WordSubOp
  | WordMulOp
  | WordMul2Op
  | WordQuotOp
  | WordRemOp
  | WordQuotRemOp
  | WordQuotRem2Op
  | AndOp
  | OrOp
  | XorOp
  | NotOp
  | SllOp
  | SrlOp
  | Word2IntOp
  | WordGtOp
  | WordGeOp
  | WordEqOp
  | WordNeOp
  | WordLtOp
  | WordLeOp
  | PopCnt8Op
  | PopCnt16Op
  | PopCnt32Op
  | PopCnt64Op
  | PopCntOp
  | Pdep8Op   -- TODO
    `);
    //console.log(cm1.getEditor().getViewport());
    //cm1.getEditor().scrollIntoView({line:10, ch:1});
    //cm1.getEditor().getDoc().setCursor(44);
    cm1.getEditor().getDoc().setSelection({line:51, ch:0},{line:63, ch:0});
    //cm1.getEditor().scrollIntoView({line:111, ch:1});
    //cm1.getEditor().getDoc().markText({line:51, ch:0},{line:63, ch:0}, {css: "background-color: #f00e0350"});
    //console.log(cm1.getEditor().getViewport());
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
