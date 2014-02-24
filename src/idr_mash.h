#ifndef IDR_MASH_H
#define IDR_MASH_H

#include <stdint.h>
#include <threefish.h>

struct idr_block256 {
  uint64_t a;
  uint64_t b;
  uint64_t c;
  uint64_t d;
};

void set_a (struct idr_block256 *blk, uint64_t a);
void set_b (struct idr_block256 *blk, uint64_t b);
void set_c (struct idr_block256 *blk, uint64_t c);
void set_d (struct idr_block256 *blk, uint64_t d);

uint64_t get_a (struct idr_block256 *blk);
uint64_t get_b (struct idr_block256 *blk);
uint64_t get_c (struct idr_block256 *blk);
uint64_t get_d (struct idr_block256 *blk);

struct idr_block256 *alloc_block ();
void free_block (struct idr_block256 *blk);

struct idr_block256 *seed_block ();

void idr_Threefish_256_Process_Block(struct idr_block256 *key, struct idr_block256 *blk, struct idr_block256 *out);
#endif
