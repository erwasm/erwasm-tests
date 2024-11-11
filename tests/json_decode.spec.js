import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../proxy';

describe('Test json decoder', () => {
  beforeAll(() => {
    erwCompile('esrc/jsone1_example');
  });

  it('should decode a number (constant 0)', async () => {
    const decode = await erwImport('jsone1_example', 'decode', 1);
    expect(decode(0)).toBe(5124);
  });

  it('should return dead bad if example doesnt match', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(0)).toBe(0xBAD_DEAD);
  });

  it('should decode and compare a string', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(1)).toBe(1);
  });

  it('should decode and compare a string in a list', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(2)).toBe(2);
  });

  it('should decode and return a number in a list', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(3)).toBe(335);
  });


  it('should decode and return a sum of numbers in a list', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(4)).toBe(1323);
  });

  it('should decode an object and return the number in X field', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(5)).toBe(523);
  });

  it('should decode an object and return the sum of two numbers', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(6)).toBe(23);
  });

  it('should decode an object and return the sum of two numbers (long keys)', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(7)).toBe(54);
  });

  it('should decode an object and return the sum of two numbers', async () => {
    const test = await erwImport('jsone1_example', 'test', 1);
    expect(test(8)).toBe(11);
  });

});
