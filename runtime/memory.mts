export class Block {
    address = 0;
    size = 0;
    free = false;
    previous = -1;
    next = -1;

    constructor(mem: Uint8Array, address: number, heap_start: number) {
        try {
            const header = Block.get_int64(mem.slice(address, address + 8));
            this.address = address + 8;
            this.size = header & ~1;
            this.free = header % 2 === 1;
            if (this.free) {
                this.previous = Block.get_int32(mem.slice(address + 8, address + 12));
                if (this.previous > 0) {
                    this.previous -= heap_start;
                }
                this.next = Block.get_int32(mem.slice(address + 12, address + 16));
                if (this.next > 0) {
                    this.next -= heap_start;
                }
            }
        } catch (error) {
            console.log(address);
            throw error;
        }
    }

    private static get_int64(data: Uint8Array): number {
        const size = new DataView(data.buffer).getBigInt64(0, true);
        return Number(size);
    }

    private static get_int32(data: Uint8Array): number {
        return new DataView(data.buffer).getInt32(0, true);
    }
}

export class Memory {
    memory: WebAssembly.Memory;
    private heap_start: number;

    private static dump_count = 0;
    private static export_count = 0;
    private encoder = new TextEncoder();

    constructor(memory: WebAssembly.Memory, heap_start: number) {
        this.memory = memory;
        this.heap_start = heap_start;
    }

    /**
     * @returns The blocks currently composing the module's heapp
     */
    get_blocks(): Array<Block> {
        const heap = this.get_heap();
        const blocks: Array<Block> = [];
        for (let i = 0; i < heap.length;) {
            const b = new Block(heap, i, this.heap_start);
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

    /**
     * Exports statistics about the memory in a json file
     */
    export(): void {
        const blocks = this.get_blocks();
        const free_blocks = blocks.reduce((acc: number, b: Block) => b.free ? acc + 1 : acc, 0);
        const allocated_blocks = blocks.length - free_blocks;
        const total_memory = blocks.reduce((total, b) => total + b.size + 8, 0);
        const s = JSON.stringify({
            blocks: blocks,
            nb_free: free_blocks,
            nb_allocated: allocated_blocks,
            size: this.get_heap_size(),
            total: total_memory
        });
        Deno.writeFileSync(`minic_report${Memory.export_count++}.json`, this.encoder.encode(s));
    }

}
