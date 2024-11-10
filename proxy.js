import { execSync } from 'child_process';
import fs from 'node:fs';

// lazy platform shim
const provides = (getMod) => ({
  'wasi:cli/stdout@0.2.0': {
    'get-stdout': () => 0,
  },
  'wasi:io/streams@0.2.0' :{
    '[method]output-stream.blocking-write-and-flush': (_stream, memPtr, memLen, retPtr) => {
      const mod = getMod();
      const mem = mod.instance.exports.memory.buffer;
      const piece = new Buffer(mem.slice(memPtr, memPtr + memLen));
      console.log(piece.toString());
    },
  }
});



const rawAdapter = {
  encode(mod, x) {
    return x;
  },
  decode(mod, x) {
    return x;
  },
};

const encodeAdapter = {
  encode(mod, x) {
    if ((typeof x)  === 'number') {
      return (x << 4) | 0xF;
    }
    return x;
  },
  decode(mod, x) {
    if ((x & 0xF) === 0xF) {
      return (x >>> 4);
    }
    return x;
  },
};

export async function erwImport(modName, funcName, arity, raw) {
  const wasmBuffer = fs.readFileSync(`./esrc/${modName}.fat.wasm`);
  const mod = await WebAssembly.instantiate(wasmBuffer, provides(
    () => mod
  ));
  const func = mod.instance.exports[`${modName}#${funcName}_${arity}`];
  const adapter = raw ? rawAdapter : encodeAdapter;
  return (...args) => {
    if (args.length !== arity) {
      throw new TypeError(`Arity mismatch on ${modName}:${func_name}/${arity}, got ${args.length} args`);
    }
    const erArgs = [...args].map((arg) => adapter.encode(mod, arg));
    return adapter.decode(mod, func(...erArgs));
  }
}

export function erwCompile(modName) {
  execSync(`make ${modName}.fat.wasm`);
}
