import * as stdio from "./stdio.mts"
import * as mem from "./memory.mts"

let memory: mem.Memory;

function init_memory(instance: WebAssembly.Instance): void {
    const wasm_memory = instance.exports.mem as WebAssembly.Memory;
    const heap_start = instance.exports.__heap_start as WebAssembly.Global;
    memory = new mem.Memory(wasm_memory, heap_start.value);
}
let c = 0;
function report(): void {
    const blocks = memory.get_blocks();
    const free_blocks = blocks.reduce((acc: number, b: mem.Block) => b.free ? acc + 1 : acc, 0);
    const allocated_blocks = blocks.length - free_blocks;
    console.log(`number of blocks : ${blocks.length}`);
    console.log(`free blocks      : ${free_blocks}`);
    console.log(`allocated blocks : ${allocated_blocks}`);
    
    const total_memory = blocks.reduce((total, b) => total + b.size + 8, 0);
    console.log(`heap size : ${memory.get_heap_size()}`);
    console.log(`used heap : ${total_memory}`);
}

const encoder = new TextEncoder();

stdio.set_on_output(value => { Deno.stdout.writeSync(encoder.encode(value)); });

const importObj: WebAssembly.Imports = {
    std: {
        putchar: stdio.putchar
    },
    debug: {
        __dump: () => memory.dump(),
        __log: (i: number) => { Deno.stdout.writeSync(encoder.encode(`${i}\n`)); },
        __export: () => memory.export()
    }
};

if (Deno.args.length == 0) {
    console.log("no program passed as argument");
} else {
    const filename = Deno.args[0];
    const data = Deno.readFileSync(filename);
    WebAssembly.instantiate(data, importObj).then(result => {
        const instance = result.instance;
        init_memory(instance);
        const main = instance.exports.main as Function;
        try {
            main();
        } catch (error) {
            Deno.stdout.writeSync(encoder.encode(`An exception occured: ${error}\n`));
            memory.dump();
            memory.export();
        }
        report();
    });
}
