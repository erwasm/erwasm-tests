import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../loader';

describe('Test json decoder', async () => {
  erwCompile('esrc/jsone1_example');
  const { decode, test } = await erwImport('jsone1_example');

  it('should decode a number (constant 0)', () => {
    expect(decode(0)).toBe(5124);
  });

  it('should return dead bad if example doesnt match', () => {
    expect(test(0)).toBe(0xBAD_DEAD);
  });

  it('should decode and compare a string', () => {
    expect(test(1)).toBe(1);
  });

  it('should decode and compare a string in a list', () => {
    expect(test(2)).toBe(2);
  });

  it('should decode and return a number in a list', () => {
    expect(test(3)).toBe(335);
  });


  it('should decode and return a sum of numbers in a list', () => {
    expect(test(4)).toBe(1323);
  });

  it('should decode an object and return the number in X field', () => {
    expect(test(5)).toBe(523);
  });

  it('should decode an object and return the sum of two numbers', () => {
    expect(test(6)).toBe(23);
  });

  it('should decode an object and return the sum of two numbers (long keys)', () => {
    expect(test(7)).toBe(54);
  });

  it('should decode an object and return the sum of two numbers', () => {
    expect(test(8)).toBe(11);
  });
});
