let output_fun: (value: string) => void;

export function set_on_output(fun: ((value: string) => void)): void {
    output_fun = fun;
}

export function putchar(c: number): number {
    output_fun(String.fromCharCode(c));
    return 0;
}
