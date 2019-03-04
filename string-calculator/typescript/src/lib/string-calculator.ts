import {NumberExtractor} from './number-extractor';

export class StringCalculator {
    add(number: string): number {
        const numbers = new NumberExtractor(number).extract();
        return this.sum(numbers);
    }

    private sum(numbers: number[]): number {
        return numbers.reduce((sum, n) => sum + n, 0);
    }
}
