import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

erwCompile('esrc/record_example');
const { example } = await erwImport('record_example');

const encoder = new TextEncoder();

describe('Record', async () => {

  function b(string) {
    return Array.from(Buffer.from(string));
  }

  it('should return default base price', () => {
    expect(example(1)).toBe(5);
  });

  it('should return default extra price', () => {
    expect(example(2)).toBe(23);
  });

  it('should give default prices based on the empty buffer', () => {
    const buffer = Array.from(Buffer.from(''));
    expect(example(buffer)).toBe(5 + 23);
  });

  it('should override base price', () => {
    const buffer = Array.from(Buffer.from('extra=3;'));
    expect(example(buffer)).toBe(5 + 3);
  });

  it('should parse the price from buffer', () => {
    const buffer = Array.from(Buffer.from('base=3;extra=9;'));
    expect(example(buffer)).toBe(3 + 9);
  });

});
