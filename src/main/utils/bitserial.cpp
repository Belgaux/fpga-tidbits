#include <iostream>
#include <cstdint>
#include <chrono>
#include <random>
#include <unistd.h>
#include <string.h>
#include <cstdlib>
#include <cstdio>
#include <bitset>

#include "bitserial.h"

using namespace std;

void free_matrix(matrix_t *M) {
  free(M->M);
  free(M);
}

void print_matrix(matrix_t *M) {
  for (int i = 0; i < M->rows; ++i) {
    for (int j = 0; j < M->cols; ++j) {
      int64_t n = M->M[i * M->cols + j];
      bitset<32> b(n);
      cout << b << "(" << n << ") ";
    }
    cout << endl;
  }
  cout << endl;
}

matrix_t* extract_bitplane(matrix_t *M, int depth) {
  matrix_t *r = (matrix_t *) malloc(sizeof(matrix_t));
  r->bit_depth = 1;
  r->rows = M->rows;
  r->cols = M->cols;
  r->M = (int64_t *) malloc(sizeof(int64_t) * r->rows * r->cols);

  for (int i = 0; i < M->rows; ++i) {
    for (int j = 0; j < M->cols; ++j) {
      int t = (1 << depth) & (M->M[i * M->cols + j]);
      r->M[i * r->cols + j] = (t > 0 ? 1 : 0);
    }
  }
  return r;
}

void fill_matrix(matrix_t *M, int64_t value)
{
  for (int i = 0; i < M->rows; ++i)
    for (int j = 0; j < M->cols; ++j)
      M->M[i*M->cols + j] = value;
}

matrix_t* software_GEMM(matrix_t *W, matrix_t *A) {
  matrix_t *result = (matrix_t *) malloc(sizeof(matrix_t));
  result->rows = W->rows;
  result->cols = 1;
  result->M = (int64_t *) calloc(result->rows, sizeof(int64_t));

  // BITSERIAL ALGORITHM
  for (int w_depth = 0; w_depth < W->bit_depth; ++w_depth) {
    for (int a_depth = 0; a_depth < A->bit_depth; ++a_depth) {
      
      int signW = w_depth == W->bit_depth - 1 ? -1 : 1;
      int signA = a_depth == A->bit_depth - 1 ? -1 : 1;
      int significance = (1 << (w_depth + a_depth));
      int alpha = significance * signW * signA;
      
      matrix_t *A_bitplane = extract_bitplane(A, a_depth);
      matrix_t *W_bitplane = extract_bitplane(W, w_depth);

      for (int i = 0; i < W->rows; ++i) {
        for (int j = 0; j < W->cols; ++j) {
          int64_t a = W_bitplane->M[i*W->cols + j];
          int64_t w = A_bitplane->M[j];
          int64_t aw = a & w;
          int64_t tmp = alpha * aw;
          result->M[i] += tmp;
        }
      }
     
      free_matrix(A_bitplane);
      free_matrix(W_bitplane);
    }
  }

  return result;
}

void test_matrix_stuff() {
#define N 128
  matrix_t *mat1 = (matrix_t *) malloc(sizeof(matrix_t));
  mat1->rows = 8;
  mat1->cols = N;
  mat1->bit_depth = 2;
  mat1->M = (int64_t *) malloc(sizeof(int64_t) * mat1->rows * mat1->cols);

  matrix_t *vec1 = (matrix_t *) malloc(sizeof(matrix_t));
  vec1->rows = N;
  vec1->cols = 1;
  vec1->bit_depth = 2;
  vec1->M = (int64_t *) malloc(sizeof(int64_t) * vec1->rows * vec1->cols);

  // Fill in matrix/vector values
  for (int i = 0; i < mat1->rows; ++i) {
    for (int j = 0; j < mat1->cols; ++j) {
      mat1->M[i * mat1->cols + j] = 1;
    }
  }
  for (int i = 0; i < vec1->rows; ++i)
    vec1->M[i] = 1;

  printf("W:\n");
  print_matrix(mat1);
  printf("A:\n");
  print_matrix(vec1);

  matrix_t *res = software_GEMM(mat1, vec1);
  printf("Result:\n");
  print_matrix(res);

  free_matrix(res);
  free_matrix(mat1);
  free_matrix(vec1);
}


