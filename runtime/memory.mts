export class Block {
    size = 0;
    free = false;
    data: Uint8Array = new Uint8Array;

    constructor(mem: Uint8Array, address: number) {
        const header = Block.get_size(mem.slice(address, address + 8));
        this.size = header & ~1;
        this.free = header % 2 === 1;
        this.data = mem.slice(address + 8, address + this.size + 8);
    }

    private static get_size(data: Uint8Array): number {
        const size = new DataView(data.buffer).getBigInt64(0, true);
        return Number(size);
    }
}

export class Memory {
    memory: WebAssembly.Memory;
    private heap_start: number;

    private static dump_count = 0;

    constructor(memory: WebAssembly.Memory, heap_start: number) {
        this.memory = memory;
        this.heap_start = heap_start;
    }

    /**
     * @returns The blocks currently composing the module's heapp
     */
    get_blocks() : Array<Block> {
        const heap = this.get_heap();
        const blocks: Array<Block> = [];
        for (let i = 0; i < heap.length;) {
            const b = new Block(heap, i);
            blocks.push(b);
            i += b.size + 8;
        }
        return blocks;
    }

    /**
     * @returns The module's memory as an Uint8Array
     */
    get_memory(): Uint8Array {
        return new Uint8Array(this.memory.buffer);
    }

    /**
     * @returns The module's heap as an Uint8Array
     */
    get_heap(): Uint8Array {
        return new Uint8Array(this.memory.buffer.slice(this.heap_start));
    }

    /**
     * @returns The heap's size in bytes
     */
    get_heap_size(): number {
        return this.memory.buffer.byteLength - this.heap_start;
    }

    /**
     * Dumps the contents of the module's memory into a file
     */
    dump(): void {
        console.log("called dump");
        Deno.writeFileSync("minic.dump" + Memory.dump_count++, this.get_memory());
    }

}
