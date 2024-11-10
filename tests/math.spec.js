import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../proxy';

describe('Test basic math', () => {
  beforeAll(() => {
    erwCompile('esrc/math_example');
  });

  it('should implement + operator for 8 bit numbers (raw)', async () => {
    const add = await erwImport('math_example', 'add', 2, true);
    expect(add(0x4F, 0x3F)).toBe(0x7F);
  });

  it('should return the number itself when adding to zero (raw)', async () => {
    const add = await erwImport('math_example', 'add', 2, true);
    expect(add(0x9F, 0x0F)).toBe(0x9F);
  });

  it('should implement + operator for 8 bit numbers', async () => {
    const add = await erwImport('math_example', 'add', 2);
    expect(add(4, 3)).toBe(7);
  });

  it('should return the number itself when adding to zero', async () => {
    const add = await erwImport('math_example', 'add', 2);
    expect(add(9, 0)).toBe(9);
  });

});
