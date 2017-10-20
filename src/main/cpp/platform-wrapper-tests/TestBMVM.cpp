#include <iostream>
#include <cstdint>
#include <chrono>
#include <random>
#include <unistd.h>
#include <string.h>
#include <cstdlib>
#include <bitset>
#include <cstdio>

using namespace std;
#include "platform.h"

typedef struct matrix_t {
  int64_t *M;
  int rows, cols, bit_depth;
} matrix_t;

void free_matrix(matrix_t *M) {
  free(M->M);
  free(M);
}

void print_matrix(matrix_t *M) {
  for (int i = 0; i < M->rows; ++i) {
    for (int j = 0; j < M->cols; ++j) {
      int64_t n = M->M[i * M->cols + j];
      bitset<8> b(n);
      cout << b << "(" << n << ") ";
    }
    cout << endl;
  }
  cout << endl;
}

matrix_t* extract_bitplane(matrix_t *M, int depth) {
  matrix_t *r = (matrix_t*)malloc(sizeof(matrix_t));
  r->bit_depth = 1;
  r->rows = M->rows;
  r->cols = M->cols;
  r->M = (int64_t*) malloc(sizeof(int64_t) * r->rows * r->cols);
  for (int i = 0; i < M->rows; ++i) {
    for (int j = 0; j < M->cols; ++j) {
      int t = (1 << depth) & (M->M[i * M->cols + j]);
      r->M[i * r->cols + j] = (t > 0 ? 1 : 0);
    }
  }
  return r;
}

void software_GEMM(matrix_t *A, matrix_t *W) {
  matrix_t *result = (matrix_t*) malloc(sizeof(matrix_t));
  result->rows = W->rows;
  result->cols = 1;
  result->M = (int64_t*) malloc(sizeof(int64_t) * result->rows);
  for (int i = 0; i < result->rows; ++i)
    result->M[i] = 0;

  for (int w_depth = 0; w_depth < W->bit_depth; ++w_depth) {
    for (int a_depth = 0; a_depth < A->bit_depth; ++a_depth) {
      
      int signW = w_depth == W->bit_depth - 1 ? -1 : 1;
      int signA = a_depth == A->bit_depth - 1 ? -1 : 1;
      int significance = (1 << (w_depth + a_depth));
      printf("significance: %d\n", significance);
      
      int alpha = significance * signW * signA;
      printf("alpha: %d\n", alpha);
      
      // extract bitplane from A and W
      matrix_t *A_bitplane = extract_bitplane(A, a_depth);
      matrix_t *W_bitplane = extract_bitplane(W, w_depth);

      printf("A_bitplane: \n");
      print_matrix(A_bitplane);
      printf("%d %d %d\n", A_bitplane->M[0], A_bitplane->M[1], A_bitplane->M[2]);
      printf("W_bitplane: \n");
      print_matrix(W_bitplane);

      for (int i = 0; i < W->rows; ++i) {
        for (int j = 0; j < W->cols; ++j) {
          int64_t a = W_bitplane->M[i*W->cols + j];
          printf("a: %zu\n", a);
          int64_t w = A_bitplane->M[j];
          printf("w: %zu\n", w);
          int64_t aw = a & w;
          printf("a&w: %zu\n", aw);
          int64_t tmp = alpha * aw;
          printf("tmp: %zu\n", tmp);
          result->M[i] += tmp;
        }
      }
     
      printf("result:\n");
      print_matrix(result);     
          
      free_matrix(A_bitplane);
      free_matrix(W_bitplane);
    }
  }

  printf("result:\n");
  print_matrix(result);
  
  free(result->M);
  free(result);
}

void test_matrix_stuff() {
  matrix_t *mat1 = (matrix_t*) malloc(sizeof(matrix_t));
  mat1->rows = 3;
  mat1->cols = 3;
  mat1->bit_depth = 8;
  mat1->M = (int64_t*) malloc(sizeof(int64_t) * mat1->rows * mat1->cols);

  matrix_t *vec1 = (matrix_t*) malloc(sizeof(matrix_t));
  vec1->cols = 1;
  vec1->rows = mat1->rows;
  vec1->bit_depth = 8;
  vec1->M = (int64_t*) malloc(sizeof(int64_t) * vec1->rows * vec1->cols);

  for (int i = 0; i < mat1->rows; ++i) {
    for (int j = 0; j < mat1->cols; ++j) {
      mat1->M[i * mat1->cols + j] = j == 0 ? -1 : j+1;
    }
    vec1->M[i] = i==0 ? -1 : i+1;
  }

  print_matrix(mat1);
  print_matrix(vec1);
  software_GEMM(vec1, mat1);

}

/*
#include "TestBMVM.hpp"
// Testing reading from DRAM, multiplying vectors, writing back
void Run_AccelTest(WrapperRegDriver* platform) {
  TestBMVM t(platform);


  // Allocate DRAM memory
  void * dramBufferVector = platform->allocAccelBuffer(vectorBytes);
  void * dramBufferMatrix = platform->allocAccelBuffer(matrixBytes);
  void * dramBufferResult = platform->allocAccelBuffer(resBytes);
  
  // Copy vectors to DRAM
  platform->copyBufferHostToAccel(vector, dramBufferVector, vectorBytes);
  platform->copyBufferHostToAccel(matrix, dramBufferMatrix, matrixBytes);

  //Initialize 
  t.set_addrV((AccelDblReg) dramBufferVector);
  t.set_addrM((AccelDblReg) dramBufferMatrix);
  t.set_addrR((AccelDblReg) dramBufferResult);
  t.set_numRows(r);
  t.set_numCols(c);
  t.set_stride(stride);

  t.set_start(1);

  while (t.get_finished() != 1);

  uint64_t *result = new uint64_t[r];
  platform->copyBufferAccelToHost(dramBufferResult, result, resBytes);
  cout << "DRAM:" << endl;
  for (int i = 0; i < r; ++i)
    cout << result[i] << " ";
  cout << endl;

  int succ = memcmp(expectedResult, result, resBytes);
  cout << "memcmp: " << succ << endl;

  platform->deallocAccelBuffer(dramBufferVector);
  platform->deallocAccelBuffer(dramBufferMatrix);
  platform->deallocAccelBuffer(dramBufferResult);

  t.set_start(0);

}
*/

int main()
{
  WrapperRegDriver * platform = initPlatform();

  //Run_AccelTest(platform);
  test_matrix_stuff();

  deinitPlatform(platform);

  return 0;
}
