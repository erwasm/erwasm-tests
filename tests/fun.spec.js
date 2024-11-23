import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

describe('Func variable', async () => {
  erwCompile('esrc/atom_example');
  const { example } = await erwImport('fun_example');

  function call(n) {
    return Array.from(example(n));
  }

  it('should return a list', () => {
    expect(call(1)).toEqual([1,2,3]);
  });

  it('should return a list where everything is +1', () => {
    expect(call(5)).toEqual([2,3,4]);
  });

  it('should return a list where everything is +2', () => {
    expect(call(6)).toEqual([3,4,5]);
  });

  it('should return a list where everything is +7', () => {
    expect(call(7)).toEqual([8,9,10]);
  });

  it('should return a list in reverse', () => {
    expect(call(11)).toEqual([3,2,1]);
  });

  it('should return a +1 list in reverse', () => {
    expect(call(15)).toEqual([4,3,2]);
  });

  it('should return a +2 list in reverse', () => {
    expect(call(16)).toEqual([5,4,3]);
  });

  it('should return a +7 list in reverse', () => {
    expect(call(17)).toEqual([10,9,8]);
  });

}); 
