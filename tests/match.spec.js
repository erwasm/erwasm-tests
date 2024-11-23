import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

erwCompile('esrc/match_example');
const { matchBase, matchShort, matchLong, matchMiddle } = await erwImport('match_example');

const encoder = new TextEncoder();

describe('Binary matcher', async () => {

  function b(string) {
    return Array.from(Buffer.from(string));
  }

  describe('base', () => {
    it('should be equal to base string', () => {
      expect(matchBase(b('base'))).toBe(Symbol.for('base'));
    });

    it('should not be equal to the base string', () => {
      expect(matchBase(b('---'))).toBe(false);
    });
  });

  describe('short end', () => {
    it('should be equal to base x string', () => {
      expect(matchShort(b('base_x'))).toBe(Symbol.for('base_x'));
    });

    it('should be equal to base y string', () => {
      expect(matchShort(b('base_y'))).toBe(Symbol.for('base_y'));
    });

    it('should not be equal to the base string', () => {
      expect(matchShort(b('---'))).toBe(false);
    });
  });

  describe('long end', () => {
    it('should be equal to base one string', () => {
      expect(matchLong(b('base_1234567890'))).toBe(Symbol.for('base_one'));
    });

    it('should be equal to base zero string', () => {
      expect(matchLong(b('base_0000000001'))).toBe(Symbol.for('base_zero'));
    });

    it('should not be equal to the base string', () => {
      expect(matchLong(b('---'))).toBe(false);
    });
  });

  describe('variable in the middle', () => {
    it('should find variable in the middle', () => {
      expect(string(matchMiddle(b('base_X_1234567890')))).toBe('ok:X');
    });

    it('should reject if varible has 0x80 bit set', () => {
      //                          012345
      const buffer = Buffer.from('base_R_1234567890');
      buffer[5] = buffer[5] | 0x80;
      expect(string(matchMiddle(b(buffer)))).toBe('nop')
    });

    it('should not match to nonsense', () => {
      expect(matchMiddle(b('base_X_0000000001'))).toBe(false);
    });
  });
});
