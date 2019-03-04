import {StringCalculator} from '../lib/string-calculator';
import {strictEqual} from 'assert';

describe('StringCalculator', () => {
    const calculator = new StringCalculator();

    it('receives no numbers', () => {
        strictEqual(calculator.add(''), 0);
    });
});
