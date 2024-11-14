import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../loader';

describe('Test json encoder', async () => {
  erwCompile('esrc/jsone2_example');
  const { encode, example, quote } = await erwImport('jsone2_example');

  it.each([
    [0],
    [5],
    [10],
    [19761],
    [300400],
    [(1 << 28) - 1],
  ])('should encode %i in json', (number) => {
    expect(Buffer.from(encode(number)).toString('binary'))
      .toBe(String(number));
  });

  // TODO: fix the nagatives
  it.skip('should encode a negative number', () => {
    expect(Buffer.from(encode(-0x779)).toString('binary'))
      .toBe("-0x00000779");
  });

  it('should encode a string', () => {
    const data = Array.from(Buffer.from('Hi this is a string'));
    expect(Buffer.from(encode(data)).toString('binary'))
      .toBe('"Hi this is a string"');
  });

  it('should encode a list of strings (erlang constants)', () => {
    expect(example(1).toString('binary'))
      .toBe('["String 1","Another one","One more"]');
  });

  it('should encode a list of strings including the input string', () => {
    const data = Array.from(Buffer.from('User string'));
    expect(example(1, data).toString('binary'))
      .toBe('["String X","User string","Tail"]');
  });

  it('should encode a list of strings mixed with numbers', () => {
    expect(example(2, 35).toString('binary'))
      .toBe('["N1",2,35,"TailN"]');
  });

  it('should encode a string with Cyrillic character', () => {
    const data = Array.from(Buffer.from('Input ї'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\u0457"]`);
  });

  it('should encode a string with Hiragana character', () => {
    const data = Array.from(Buffer.from('Input つ'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\u3064"]`);
  });

  it('should encode a string with emoji as a surrogate pair', () => {
    const data = Array.from(Buffer.from('Input 🌞'));
    const output = example(3, data);
    expect(output.toString('utf8'))
      .toBe(`["Input \\ud83c\\udf1e"]`);
  });

  it('buffer should contain escape codes', () => {
    const output = quote(0x3031);
    expect(output.toString('utf8'))
      .toBe('"01"');
  });

  it('should encode erlang string literal as a list of ASCII codes because of erlang reasons', () => {
    const output = example(2);

    const expectedOutput = Array.from('ASCII')
      .map((symbol) => symbol.codePointAt(0));

    expect(output.toString('utf8'))
      .toEqual(JSON.stringify(expectedOutput));

    // same but hardcoded for visibility
    expect(output.toString('utf8')).toEqual('[65,83,67,73,73]');

  });

  it('should encode erlang string literal as a list of code points codes because of erlang reasons', () => {
    const output = example(22);

    const expectedOutput = Array.from('Ґанок')
      .map((symbol) => symbol.codePointAt(0));

    expect(output.toString('utf8'))
      .toEqual(JSON.stringify(expectedOutput));

    // same but hardcoded for visibility
    expect(output.toString('utf8')).toEqual('[1168,1072,1085,1086,1082]');
  });

  it('should encode erlang string literal inside a list as a list of ASCII codes because of erlang reasons', () => {
    const output = example(3);

    const expectedOutput = Array.from('ASCII')
      .map((symbol) => symbol.codePointAt(0));

    expect(output.toString('utf8'))
      .toEqual(JSON.stringify(['N1', expectedOutput]));

    // same but hardcoded for visibility
    expect(output.toString('utf8')).toEqual('["N1",[65,83,67,73,73]]');
  });

  it('should encode atom inside a list as a string', () => {
    const output = example(32);

    expect(output.toString('utf8')).toEqual('["N1","ascii"]');
  });

  it('should encode prop list as an object', () => {
    const output = example(4);

    expect(output.toString('utf8'))
      .toEqual('{"x":1}');
  });

  it('should NOT encode prop list with string keys as an object', () => {
    const output = example(5);

    expect(output.toString('utf8'))
      .toEqual('Encoding error');
  });

  it('should encode prop list with atom keys as an object', () => {
    const output = example(6);

    expect(output.toString('utf8'))
      .toEqual('{"xatom":1}');
  });
});
