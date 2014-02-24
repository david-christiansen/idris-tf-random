#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "idr_mash.h"


void set_a (struct idr_block256 *blk, uint64_t a) {
  blk->a = a;
}

void set_b (struct idr_block256 *blk, uint64_t b) {
  blk->b = b;
}

void set_c (struct idr_block256 *blk, uint64_t c) {
  blk->c = c;
}

void set_d (struct idr_block256 *blk, uint64_t d) {
  blk->d = d;
}

uint64_t get_a (struct idr_block256 *blk) {
  return blk->a;
}

uint64_t get_b (struct idr_block256 *blk) {
  return blk->b;
}

uint64_t get_c (struct idr_block256 *blk) {
  return blk->c;
}

uint64_t get_d (struct idr_block256 *blk) {
  return blk->d;
}

struct idr_block256 *alloc_block () {
  return (struct idr_block256 *) (malloc (sizeof(struct idr_block256)));
}

void free_block (struct idr_block256 *blk) {
  free(blk);
}

struct idr_block256 *seed_block () {
  struct idr_block256 *seed = alloc_block();
  uint64_t seed_data[4];
  FILE* f = fopen("/dev/urandom", "rb");
  fread(seed_data, sizeof(uint64_t), 4, f);
  fclose(f);
  set_a(seed, seed_data[0]);
  set_b(seed, seed_data[1]);
  set_c(seed, seed_data[2]);
  set_d(seed, seed_data[3]);
  return seed;
}

void idr_Threefish_256_Process_Block(struct idr_block256 *key, struct idr_block256 *blk, struct idr_block256 *out) {
  uint64_t keyPtr[4];
  uint64_t blkPtr[4];
  uint64_t cryptPtr[4] = { 0LL, 0LL, 0LL, 0LL };

  keyPtr[0] = get_a(key);
  keyPtr[1] = get_b(key);
  keyPtr[2] = get_c(key);
  keyPtr[3] = get_d(key);

  blkPtr[0] = get_a(blk);
  blkPtr[1] = get_b(blk);
  blkPtr[2] = get_c(blk);
  blkPtr[3] = get_d(blk);

  Threefish_256_Process_Block((const u08b_t *)keyPtr, (const u08b_t *)blkPtr, (u08b_t *)cryptPtr, 0);

  set_a(out, cryptPtr[0]);
  set_b(out, cryptPtr[1]);
  set_c(out, cryptPtr[2]);
  set_d(out, cryptPtr[3]);
}
