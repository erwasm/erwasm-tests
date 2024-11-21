import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

describe('Test atom encoder', async () => {
  erwCompile('esrc/atom_example');
  const { example: exampleRaw } = await erwImport('atom_example', true);
  const { example } = await erwImport('atom_example');

  it('should return internal representation of an atom (raw)', () => {
    expect(exampleRaw(0x1F))
      .toBe((9 << 6) | 0xB)
  });

  it('should return a symbol corresponding to an atom', () => {
    expect(example(1))
      .toBe(Symbol.for('atom_x'))
  });

  it('should return a symbol corresponding to an atom', () => {
    expect(example(11))
      .toBe(Symbol.for('atomName'))
  });

  it('should return a symbol corresponding to an atom', () => {
    expect(example(111))
      .toBe(Symbol.for('Atom.Dot.Name'))
  });

  it('should return a symbol corresponding to an atom', () => {
    expect(example(12))
      .toBe(Symbol.for('Otheratomname'))
  });

  it('should return a symbol corresponding to an atom', () => {
    expect(example(13))
      .toBe(Symbol.for('otheratomname'))
  });

  it('should check that atoms that differ in case are not equal', () => {
    expect(example(14))
      .toBe(false)
  });

  it('should return binary representation of an atom (ASCII)', () => {
    expect(string(example(3)))
      .toBe('atom_x');
  });

  it('should return binary representation of an atom defined in a dependency (ASCII)', () => {
    expect(string(example(4)))
      .toBe('outer_constant');
  });

  it('should return binary representation of an atom defined in main file, serialized in a dependency (ASCII)', () => {
    expect(string(example(5)))
      .toBe('atom_x');
  });

  it('should throw when encoding is not utf8', () => {
    expect(() => string(example(6)))
      .toThrow(/error/);
  });

});
