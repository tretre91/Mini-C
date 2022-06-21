import * as stdio from "./stdio.mts"

let instance: WebAssembly.Instance;

const importObj: WebAssembly.Imports = {
    std: {
        putchar: stdio.putchar
    },
    debug: {
        __dump: () => dump_memory(instance.exports.mem as WebAssembly.Memory),
        __log: (i: number) => { Deno.stdout.writeSync(encoder.encode(`${i}\n`)); }
    }
};


let count = 0;
function dump_memory(buffer: WebAssembly.Memory): void {
    const arr = new Uint8Array(buffer.buffer);
    Deno.writeFile("minic.dump" + count, arr);
    count++;
}

const encoder = new TextEncoder();

stdio.set_on_output(value => { Deno.stdout.writeSync(encoder.encode(value)); });

if (Deno.args.length == 0) {
    console.log("no program passed as argument");
} else {
    const filename = Deno.args[0];
    const data = Deno.readFileSync(filename);
    WebAssembly.instantiate(data, importObj).then(result => {
        instance = result.instance;
        const main = instance.exports.main as Function;
        try {
            main();
        } catch (_error) {
            Deno.stdout.writeSync(encoder.encode(`An exception occured: ${_error}\n`));
            dump_memory(instance.exports.mem as WebAssembly.Memory)
        }
    });
}
