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
#include "platform.h"

#include "TestBitplane.hpp"
void Run_TestBitplane(WrapperRegDriver* platform) 
{
  TestBitplane t(platform);

  // Random 0/1 generator
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937_64 generator (seed);
  std::uniform_int_distribution<uint64_t> distribution(0, 1);

  // might use this loop for channels eventually?
  while (1) {
    uint32_t r, c;
    cout << "row, col:" << endl;
    cin >> r >> c;
    if (r == 0 || c == 0)
      break;
    uint32_t word_size = 64;
    uint32_t result_bytes = r * sizeof(int64_t);
    uint32_t vector_bytes = c * sizeof(int64_t);
    uint32_t matrix_bytes = c * r * sizeof(int64_t);
    uint32_t stride = vector_bytes;

    matrix_t *W = (matrix_t *) malloc(sizeof(matrix_t));
    W->rows = r;
    W->cols = c;
    W->bit_depth = 2;
    W->M = (int64_t *) malloc(sizeof(int64_t) * r * c);

    matrix_t *A = (matrix_t *) malloc(sizeof(matrix_t));
    A->rows = c;
    A->cols = 1;
    A->bit_depth = 2;
    A->M = (int64_t *) malloc(sizeof(int64_t) * c);

    matrix_t *R = (matrix_t *) malloc(sizeof(matrix_t));
    R->rows = W->rows;
    R->cols = 1;
    R->M = (int64_t *) calloc(R->rows, sizeof(int64_t));

    fill_matrix(W, -1);
    fill_matrix(A, 1);

    matrix_t *software_result = software_GEMM(W, A);
    printf("Software gemm (expected result):\n");
    for (int i = 0; i < software_result->rows; ++i)
      printf("%ld ", software_result->M[i]);
    printf("\n\n");


    // Allocate DRAM memory
    void * dram_buffer_vec = platform->allocAccelBuffer(vector_bytes);
    void * dram_buffer_mat = platform->allocAccelBuffer(matrix_bytes);
    void * dram_buffer_res = platform->allocAccelBuffer(result_bytes);

    // Copy vectors to DRAM
    platform->copyBufferHostToAccel(A->M, dram_buffer_vec, vector_bytes);
    platform->copyBufferHostToAccel(W->M, dram_buffer_mat, matrix_bytes);

    //Initialize 
    t.set_addrV((AccelDblReg) dram_buffer_vec);
    t.set_addrM((AccelDblReg) dram_buffer_mat);
    t.set_addrR((AccelDblReg) dram_buffer_res);
    t.set_numRows(r);
    t.set_numCols(c);
    t.set_stride(stride);
    t.set_m_depth(W->bit_depth);
    t.set_v_depth(A->bit_depth);

    t.set_start(1);

    while (t.get_finished() != 1);

    int64_t *result = (int64_t *) malloc(sizeof(int64_t) * R->rows);
    platform->copyBufferAccelToHost(dram_buffer_res, result, result_bytes);
    cout << "DRAM:" << endl;
    for (int i = 0; i < R->rows; ++i) {
      cout << result[i] << " ";
      R->M[i] += result[i];
    }
    cout << endl;
    free(result);

    platform->deallocAccelBuffer(dram_buffer_vec);
    platform->deallocAccelBuffer(dram_buffer_mat);
    platform->deallocAccelBuffer(dram_buffer_res);
    t.set_start(0);  

    
    cout << "FINAL RESULT" << endl;
    for (int i = 0; i < R->rows; ++i)
      cout << R->M[i] << " ";
    cout << endl;

    int succ = memcmp(software_result->M, R->M, result_bytes);
    cout << "memcmp: " << succ << endl;

    free_matrix(A);
    free_matrix(W);
    free_matrix(R);
    free_matrix(software_result);
  }
}

int main()
{
  WrapperRegDriver * platform = initPlatform();

  Run_TestBitplane(platform);

  deinitPlatform(platform);
  return 0;
}

