import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../proxy';

describe('Test json encoder', () => {
  beforeAll(() => {
    erwCompile('esrc/jsone2_example');
  });

  it.each([
    [0],
    [5],
    [10],
    [19761],
    [300400],
    [(1 << 28) - 1],
  ])('should encode %i in json', async (number) => {
    const encode = await erwImport('jsone2_example', 'encode', 1);
    expect(Buffer.from(encode(number)).toString('binary'))
      .toBe(String(number));
  });

  // TODO: fix the nagatives
  it.skip('should encode a negative number', async () => {
    const encode = await erwImport('jsone2_example', 'encode', 1);
    expect(Buffer.from(encode(-0x779)).toString('binary'))
      .toBe("-0x00000779");
  });

  // TODO: encode the normal integers, not hex
  it('should encode a string', async () => {
    const encode = await erwImport('jsone2_example', 'encode', 1);
    const data = Array.from(Buffer.from('Hi this is a string'));
    expect(Buffer.from(encode(data)).toString('binary'))
      .toBe('"Hi this is a string"');
  });

  it('should encode a list of strings (erlang constants)', async () => {
    const example = await erwImport('jsone2_example', 'example', 1);
    expect(example(1).toString('binary'))
      .toBe('["String 1","Another one","One more"]');
  });

  it('should encode a list of strings including the input string', async () => {
    const example = await erwImport('jsone2_example', 'example', 2);
    const data = Array.from(Buffer.from('User string'));
    expect(example(1, data).toString('binary'))
      .toBe('["String X","User string","Tail"]');
  });

  it('should encode a list of strings mixed with numbers', async () => {
    const example = await erwImport('jsone2_example', 'example', 2);
    expect(example(2, 35).toString('binary'))
      .toBe('["N1",2,35,"TailN"]');
  });

  it('should encode a string with Cyrillic character', async () => {
    const example = await erwImport('jsone2_example', 'example', 2);
    const data = Array.from(Buffer.from('Input ї'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\u0457"]`);
  });

  it('should encode a string with Hiragana character', async () => {
    const example = await erwImport('jsone2_example', 'example', 2);
    const data = Array.from(Buffer.from('Input つ'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\u3064"]`);
  });

  it('should encode a string with emoji as a surrogate pair', async () => {
    const example = await erwImport('jsone2_example', 'example', 2);
    const data = Array.from(Buffer.from('Input 🌞'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\ud83c\\udf1e"]`);
  });

  it('buffer should contain escape codes', async () => {
    const quote = await erwImport('jsone2_example', 'quote', 1);
    const output = quote(0x3031);
    expect(output.toString('utf8'))
      .toBe('"01"');
  });

});
