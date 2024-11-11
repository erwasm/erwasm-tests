import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../proxy';

describe('UTF-8 decoder', () => {
  beforeAll(() => {
    erwCompile('esrc/unicode_example');
  });

  it('should return base plane (ASCII) codepoint as is', async () => {
    const decode_u8 = await erwImport('unicode_example', 'decode_u8', 1);
    expect(decode_u8(0x33)).toBe(0x33);
  });

  /*
  Python: 
  >>> 'ф'.encode('utf8')
  b'\xd1\x84'
  >>> hex(ord('ф'))
  '0x444'
  */
  it('should return codepoint for Cyrilic Ф', async () => {
    const decode_u8 = await erwImport('unicode_example', 'decode_u8', 2);
    expect(decode_u8(0xD1, 0x84)).toBe(0x444);
  });

  /*
  Python: 
  >>> 'つ'.encode('utf8')
  b'\xe3\x81\xa4'
  >>> hex(ord('つ'))
  '0x3064'
  */
  it('should return codepoint for Hiragana つ', async () => {
    const decode_u8 = await erwImport('unicode_example', 'decode_u8', 3);
    expect(decode_u8(0xE3, 0x81, 0xA4)).toBe(0x3064);
  });

  /*
  Python: 
  >>> '🌞'.encode('utf8')
  b'\xf0\x9f\x8c\x9e'
  >>> hex(ord('🌞'))
  '0x1f31e'
  */
  it('should return codepoint for sun emoji 🌞', async () => {
    const decode_u8 = await erwImport('unicode_example', 'decode_u8', 4);
    expect(decode_u8(0xF0, 0x9F, 0x8C, 0x9E)).toBe(0x1f31e);
  });

  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should decode "%s" codepoint from utf8 representation', async (symbol) => {
    const utf8Buffer = Array.from(Buffer.from(symbol));
    const decode_u8 = await erwImport('unicode_example', 'decode_u8_bin', 1);
    expect(decode_u8(utf8Buffer)).toBe(symbol.codePointAt(0));
  })

  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should encode "%s" into utf8 representation', async (symbol) => {
    const codePoint = symbol.codePointAt(0);
    const encode_u8 = await erwImport('unicode_example', 'encode_u8', 1);
    const jsBuffer = Buffer.from(symbol, 'utf8');
    expect(encode_u8(codePoint)).toEqual(jsBuffer);
  });

  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should encode "%s" into utf16 be representation', async (symbol) => {
    const codePoint = symbol.codePointAt(0);
    const encode_u16 = await erwImport('unicode_example', 'encode_u16', 1);
    const jsBufferLe = Buffer.from(symbol, 'utf16le');
    const jsBufferBe = new Buffer(jsBufferLe.length);

    let idx = 0;
    while(idx < jsBufferLe.length) {
      let pair = jsBufferLe.readUInt16LE(idx);
      jsBufferBe.writeUInt16BE(pair, idx);
      idx += 2;
    }

    const eBuffer = Buffer.from(encode_u16(codePoint));
    expect(eBuffer).toEqual(jsBufferBe);
  });

});
