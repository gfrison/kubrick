// FFI implementation for bit operations and string hashing

/**
 * Left shift
 */
export const shl = (x) => (n) => (x << n) | 0;

/**
 * Signed right shift
 */
export const shr = (x) => (n) => (x >> n) | 0;

/**
 * Zero-fill right shift (unsigned)
 */
export const zshr = (x) => (n) => (x >>> n) | 0;

/**
 * XOR operation
 */
export const xor = (x) => (y) => (x ^ y) | 0;

/**
 * Integer multiplication with proper 32-bit truncation
 */
export const imul = (x) => (y) => Math.imul(x, y) | 0;

/**
 * String hash function using polynomial rolling hash
 * Similar to Java's String.hashCode()
 */
export const stringHash = (s) => {
  let h = 0;
  for (let i = 0; i < s.length; i++) {
    h = (31 * h + s.charCodeAt(i)) | 0;
  }
  // Apply MurmurHash3 finalizer for better distribution
  h ^= h >>> 16;
  h = Math.imul(h, 0x85ebca6b);
  h ^= h >>> 13;
  h = Math.imul(h, 0xc2b2ae35);
  h ^= h >>> 16;
  return h | 0;
};
