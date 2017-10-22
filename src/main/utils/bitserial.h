#ifndef BITSERIAL_H_
#define BITSERIAL_H_

#include <cstdint>

typedef struct matrix_t {
  int64_t *M;
  int rows, cols, bit_depth;
} matrix_t;

void print_matrix(matrix_t *M);
void free_matrix(matrix_t *M);
void test_matrix_stuff();
void fill_matrix(matrix_t *M, int64_t value);

matrix_t* extract_bitplane(matrix_t *M, int depth);
matrix_t* software_GEMM(matrix_t *W, matrix_t *A);


#endif // BITSERIAL_H_
