import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../loader';

describe('Test logic operators', async () => {
  erwCompile('esrc/logic_example');
  const { example } = await erwImport('logic_example');

  describe('andalso', () => {
    const table = [
      [0, 0, false],
      [0, 1, false],
      [1, 0, false],
      [1, 1, true],
    ];
    it.each(table)('should evaluate %s and %s to %s', (a, b, ret) => {
      expect(example(a, b, 'land'))
        .toBe(ret)
    });

    it('should stop evaluation as soon as false is found on the left side', () => {
      // 3 doesn't have an atom mapped to it
      expect(example(0, 3, 'land'))
        .toBe(false)
    });

  });

  describe('and', () => {
    const table = [
      [0, 0, false],
      [0, 1, false],
      [1, 0, false],
      [1, 1, true],
    ];
    it.each(table)('should evaluate %s and %s to %s', (a, b, ret) => {
      expect(example(a, b, 'and'))
        .toBe(ret)
    });

    it('should evaluate the right branch even if lest is arleady false', () => {
      // 3 doesn't have an atom mapped to it
      expect(() => example(0, 3, 'and'))
        .toThrow(/error/);
    });

  });

  describe('orelse', () => {
    const table = [
      [0, 0, false],
      [0, 1, true],
      [1, 0, true],
      [1, 1, true],
    ];
    it.each(table)('should evaluate %s and %s to %s', (a, b, ret) => {
      expect(example(a, b, 'lor'))
        .toBe(ret)
    });

    it('should stop evaluation as soon as true is found on the left side', () => {
      // 3 doesn't have an atom mapped to it
      expect(example(1, 3, 'lor'))
        .toBe(true)
    });

  });

  describe('or', () => {
    const table = [
      [0, 0, false],
      [0, 1, true],
      [1, 0, true],
      [1, 1, true],
    ];
    it.each(table)('should evaluate %s and %s to %s', (a, b, ret) => {
      expect(example(a, b, 'or'))
        .toBe(ret)
    });

    it('should evaluate the right branch even if lest is arleady true', () => {
      // 3 doesn't have an atom mapped to it
      expect(() => example(1, 3, 'or'))
        .toThrow(/error/);
    });
  });

});
