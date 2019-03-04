import {StringCalculator} from '../lib/string-calculator';
import {strictEqual} from 'assert';

describe('StringCalculator', () => {
    const calculator = new StringCalculator();

    it('receives no numbers', () => {
        strictEqual(calculator.add(''), 0);
    });

    it('receives 1 number', () => {
        strictEqual(calculator.add('1'), 1);
    });

    it('receives 2 numbers and adds them', () => {
        strictEqual(calculator.add('1,2'), 3);
    });

    it('accepts a new line as a delimiter instead of a comma', () => {
        strictEqual(calculator.add('1\n2'), 3);
        strictEqual(calculator.add('1\n2,3'), 6);
    });

    it('allows a delimiter to be specified as an input', () => {
        strictEqual(calculator.add('//;\n1;2'), 3);
        strictEqual(calculator.add('//;\n1;3'), 4);
    });
});
