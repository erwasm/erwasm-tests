import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../loader';

describe('Test basic math', async () => {
  erwCompile('esrc/math_example');
  const { add: addRaw } = await erwImport('math_example', true);
  const { add } = await erwImport('math_example');

  it('should implement + operator for 8 bit numbers (raw)', () => {
    expect(addRaw(0x4F, 0x3F)).toBe(0x7F);
  });

  it('should return the number itself when adding to zero (raw)', () => {
    expect(addRaw(0x9F, 0x0F)).toBe(0x9F);
  });

  it('should implement + operator for 8 bit numbers', () => {
    expect(add(4, 3)).toBe(7);
  });

  it('should implement + operator for 8 bit numbers', () => {
    expect(add(4, 3)).toBe(7);
  });

  it('should return the number itself when adding to zero', () => {
    expect(add(9, 0)).toBe(9);
  });
});
