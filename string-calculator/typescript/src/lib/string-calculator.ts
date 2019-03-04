const DELIMITER = /[,\n]/;

export class StringCalculator {
    add(number: string): number {
        if (!number) return 0;
        const match = number.match(/\/\/(.)\n(.*)$/);
        const numbers = match ? this.extractNumbers(match[2], match[1]) : this.extractNumbers(number);
        return this.sum(numbers);
    }

    private extractNumbers(number: string, delimiter: string | RegExp = DELIMITER): number[] {
        return number.split(delimiter).map(n => parseInt(n));
    }

    private sum(numbers: number[]): number {
        return numbers.reduce((sum, n) => sum + n, 0);
    }
}
