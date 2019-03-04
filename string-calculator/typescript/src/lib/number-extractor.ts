const DELIMITER = /[,\n]/;

export class NumberExtractor {
    private readonly input: string;

    constructor(input: string) {
        this.input = input;
    }

    extract(): number[] {
        const number = this.input;
        if (!number) return [0];
        const match = number.match(/\/\/(.)\n(.*)$/);
        return match ? this.extractNumbers(match[2], match[1]) : this.extractNumbers(number);
    }

    private extractNumbers(number: string, delimiter: string | RegExp = DELIMITER): number[] {
        return number.split(delimiter).map(n => parseInt(n));
    }
}
