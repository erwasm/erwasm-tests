import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

erwCompile('esrc/trycatch_example');
const { match_fail, catch_fail, catch_some } = await erwImport('trycatch_example');

describe('Global exception handler', async () => {
  it('should return 0 when match succeeds', () => {
    expect(match_fail(0)).toBe(0);
  });

  it('should raise error badmatch on 1 = len([])', () => {
    expect(() => match_fail(1)).toThrow(/error/);
  });
});


describe('Caller exception handler', async () => {
  it('should return true when match succeeds', () => {
    expect(catch_fail(0)).toBe(true);
  });

  it('should return false when caller\'s exception handler for badarg catches it', () => {
    expect(catch_fail(1)).toBe(false);
  });

  it('should return weird when calle\'s catchall handler catches it', () => {
    expect(catch_fail([])).toBe(Symbol.for('weird'));
  });

});

describe('Caller exception with raise', async () => {
  it('should return true when match succeeds', () => {
    expect(catch_some(0)).toBe(true);
  });

  it('should return false when caller\'s exception handler for badarg catches it', () => {
    expect(catch_some(1)).toBe(false);
  });

  it('should throw exceptions not cough by caller', () => {
    expect(() => catch_some([])).toThrow(/throw/);
  });

});
