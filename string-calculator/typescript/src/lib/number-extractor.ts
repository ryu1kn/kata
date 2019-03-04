const DEFAULT_DELIMITER = /[,\n]/;

type Delimiter = string | RegExp;

export class NumberExtractor {
    private readonly input: string;

    constructor(input: string) {
        this.input = input;
    }

    extract(): number[] {
        return this.isEmpty ? [] : this.extractNumbers(this.numberPart, this.delimiter);
    }

    private get isEmpty(): boolean {
        return !this.input;
    }

    private get delimiterSpecified(): boolean {
        return this.input.startsWith('//');
    }

    private get delimiter(): Delimiter {
        return this.delimiterSpecified ? this.input.match(/\/\/(.)\n/)[1] : DEFAULT_DELIMITER;
    }

    private get numberPart(): string {
        return this.delimiterSpecified ? this.input.substring(this.input.indexOf('\n')) : this.input;
    }

    private extractNumbers(number: string, delimiter: Delimiter): number[] {
        return number.split(delimiter).map(n => parseInt(n));
    }
}
