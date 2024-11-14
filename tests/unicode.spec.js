import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from '../loader';

erwCompile('esrc/unicode_example');
const { decode_u8, encode_u8, decode_u8_bin, decode_u16, encode_u16 } = await erwImport('unicode_example');

describe('UTF-8 decoder', async () => {

  it('should return base plane (ASCII) codepoint as is', () => {
    expect(decode_u8(0x33)).toBe(0x33);
  });

  /*
  Python:
  >>> 'ф'.encode('utf8')
  b'\xd1\x84'
  >>> hex(ord('ф'))
  '0x444'
  */
  it('should return codepoint for Cyrilic Ф', () => {
    expect(decode_u8(0xD1, 0x84)).toBe(0x444);
  });

  /*
  Python:
  >>> 'つ'.encode('utf8')
  b'\xe3\x81\xa4'
  >>> hex(ord('つ'))
  '0x3064'
  */
  it('should return codepoint for Hiragana つ', () => {
    expect(decode_u8(0xE3, 0x81, 0xA4)).toBe(0x3064);
  });

  /*
  Python:
  >>> '🌞'.encode('utf8')
  b'\xf0\x9f\x8c\x9e'
  >>> hex(ord('🌞'))
  '0x1f31e'
  */
  it('should return codepoint for sun emoji 🌞', () => {
    expect(decode_u8(0xF0, 0x9F, 0x8C, 0x9E)).toBe(0x1f31e);
  });

  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should decode "%s" codepoint from utf8 representation', (symbol) => {
    const utf8Buffer = Array.from(Buffer.from(symbol));
    expect(decode_u8_bin(utf8Buffer)).toBe(symbol.codePointAt(0));
  })

});

describe('UTF-16 decoder', () => {
  /*
  Python:
  >>> 'ф'.encode('utf16').hex()[4:]
  '4404'
  >>> hex(ord('ф'))
  '0x444'
  */
  it('should return codepoint for Cyrilic Ф', () => {
    expect(decode_u16(0x44, 0x04)).toBe(0x444);
  });


  /*
  Python:
  >>> 'つ'.encode('utf16').hex()[4:]
  '6430'
  >>> hex(ord('つ'))
  '0x3064'
  */
  it('should return codepoint for Hiragana つ', () => {
    expect(decode_u16(0x64, 0x30)).toBe(0x3064);
  });

  /*
  >>> '🌞'.encode('utf16').hex()[4:]
  '3cd81edf'
  >>> hex(ord('🌞'))
  '0x1f31e'
  */
  it('should return codepoint for sun emoji 🌞' , () => {
    expect(decode_u16(0x3c, 0xd8, 0x1e, 0xdf))
      .toBe(0x1f31e);
  });

});

describe('UTF-8 encoder', () => {
  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should encode "%s" into utf8 representation', (symbol) => {
    const codePoint = symbol.codePointAt(0);
    const jsBuffer = Buffer.from(symbol, 'utf8');
    expect(encode_u8(codePoint)).toEqual(jsBuffer);
  });
});

describe('UTF-16 encoder', () => {
  it.each([
    ['X'],
    ['ї'],
    ['つ'],
    ['🌞']
  ])('should encode "%s" into utf16 be representation', (symbol) => {
    const codePoint = symbol.codePointAt(0);
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
