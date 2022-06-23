import * as stdio from "./stdio.mts"

const importObj: WebAssembly.Imports = {
    std: {
        putchar: stdio.putchar
    }
};

const encoder = new TextEncoder();

stdio.set_on_output(value => { Deno.stdout.writeSync(encoder.encode(value)); });

if (Deno.args.length == 0) {
    console.log("no program passed as argument");
} else {
    const filename = Deno.args[0];
    const data = Deno.readFileSync(filename);
    WebAssembly.instantiate(data, importObj).then(result => {
        const main = result.instance.exports.main as Function;
        return main();
    });
}
