import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile, string } from '../loader';

const encoder = new TextEncoder();

describe('Elixir', async () => {
  erwCompile('esrc/Elixir.Hi');
  const { hello } = await erwImport('Elixir.Hi');

  it('should return internal representation of an atom (raw)', () => {
    expect(Buffer.from(hello()).toString())
      .toBe('Hello, World!');
  });
});
