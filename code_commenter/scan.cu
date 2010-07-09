/*
 * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
 *
 * NVIDIA Corporation and its licensors retain all intellectual property and 
 * proprietary rights in and to this software and related documentation and 
 * any modifications thereto.  Any use, reproduction, disclosure, or distribution 
 * of this software and related documentation without an express license 
 * agreement from NVIDIA Corporation is strictly prohibited.
 * 
 */

#ifdef _WIN32
#  define NOMINMAX 
#endif

// includes, system
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>

// includes, project
#include <cutil_inline.h>

// includes, kernels
#include <scan_naive_kernel.cu>
#include <scan_workefficient_kernel.cu>
#include <scan_best_kernel.cu>

////////////////////////////////////////////////////////////////////////////////
// declaration, forward
void runTest( int argc, char** argv);

// regression test functionality
extern "C" 
unsigned int compare( const float* reference, const float* data, 
                      const unsigned int len);
extern "C" 
void computeGold( float* reference, float* idata, const unsigned int len);

////////////////////////////////////////////////////////////////////////////////
// Program main
////////////////////////////////////////////////////////////////////////////////
int 
main( int argc, char** argv) 
{
    runTest( argc, argv);
    cutilExit(argc, argv);
}

////////////////////////////////////////////////////////////////////////////////
//! Run a scan test for CUDA
////////////////////////////////////////////////////////////////////////////////
void
runTest( int argc, char** argv) 
{
    // use command-line specified CUDA device, otherwise use device with highest Gflops/s
    if( cutCheckCmdLineFlag(argc, (const char**)argv, "device") )
        cutilDeviceInit(argc, argv);
    else
        cudaSetDevice( cutGetMaxGflopsDeviceId() );

    unsigned int num_elements = 512;
    cutGetCmdLineArgumenti( argc, (const char**) argv, "n", (int*)&num_elements);

    unsigned int timer;
    cutilCheckError( cutCreateTimer(&timer));
    
    const unsigned int num_threads = num_elements / 2;
    const unsigned int mem_size = sizeof( float) * num_elements;

    // padding space is used to avoid shared memory bank conflicts
    unsigned int extra_space = num_elements / NUM_BANKS;

#ifdef ZERO_BANK_CONFLICTS
    extra_space += extra_space / NUM_BANKS;
#endif

    const unsigned int shared_mem_size = sizeof(float) * 
        (num_elements + extra_space);

    // allocate host memory to store the input data
    float* h_data = (float*) malloc( mem_size);
      
    // initialize the input data on the host to be integer values
    // between 0 and 1000
    for( unsigned int i = 0; i < num_elements; ++i) 
    {
        h_data[i] = floorf(1000*(rand()/(float)RAND_MAX));
    }

    // compute reference solution
    float* reference = (float*) malloc( mem_size);  
    computeGold( reference, h_data, num_elements);

    // allocate device memory input and output arrays
    float* d_idata;
    float* d_odata[3];
    cutilSafeCall( cudaMalloc( (void**) &d_idata, mem_size));
    cutilSafeCall( cudaMalloc( (void**) &(d_odata[0]), mem_size));
    cutilSafeCall( cudaMalloc( (void**) &(d_odata[1]), mem_size));
    cutilSafeCall( cudaMalloc( (void**) &(d_odata[2]), mem_size));

    // copy host memory to device input array
    cutilSafeCall( cudaMemcpy( d_idata, h_data, mem_size, cudaMemcpyHostToDevice) );

    // setup execution parameters
    // Note that these scans only support a single thread-block worth of data,
    // but we invoke them here on many blocks so that we can accurately compare
    // performance
#ifndef __DEVICE_EMULATION__
    dim3  grid(256, 1, 1);  
#else
    dim3  grid(1, 1, 1); // only one run block in device emu mode or it will be too slow
#endif
    dim3  threads(num_threads*2, 1, 1);

    // make sure there are no CUDA errors before we start
    cutilCheckMsg("Kernel execution failed");

    printf("Running parallel prefix sum (scan) of %d elements\n", num_elements);
    printf("Comparing 3 versions:\n\n");
  
    // execute the kernels
    unsigned int numIterations = 100;

    printf("1. scan_naive -- not work efficient (O(n log n) adds).\n");
    cutStartTimer(timer);
    for (unsigned int i = 0; i < numIterations; ++i)
    {
        scan_naive<<< grid, threads, 2 * shared_mem_size >>>
            (d_odata[0], d_idata, num_elements);
    }
    cudaThreadSynchronize();
    cutStopTimer(timer);
    printf("Average time: %f ms\n\n", cutGetTimerValue(timer) / numIterations);
    cutResetTimer(timer);

    threads.x /= 2;
    
    printf("2. scan_workefficient -- Work efficient (O(n) adds), but many bank conflicts.\n");
    cutStartTimer(timer);
    for (unsigned int i = 0; i < numIterations; ++i)
    {
        scan_workefficient<<< grid, threads, shared_mem_size >>>
            (d_odata[1], d_idata, num_elements);
    }
    cudaThreadSynchronize();
    cutStopTimer(timer);
    printf("Average time: %f ms\n\n", cutGetTimerValue(timer) / numIterations);
    cutResetTimer(timer);

    printf("3. scan_best -- work efficient with very few bank conflicts.\n");
    cutStartTimer(timer);
    for (unsigned int i = 0; i < numIterations; ++i)
    {
        scan_best<<< grid, threads, shared_mem_size >>>
            (d_odata[2], d_idata, num_elements);
    }
    cudaThreadSynchronize();
    cutStopTimer(timer);
    printf("Average time: %f ms\n\n", cutGetTimerValue(timer) / numIterations);
    cutResetTimer(timer);

    // check for any errors
    cutilCheckMsg("Kernel execution failed");

    for (int i = 0; i < 3; ++i) // check all 3 results
    {
        // copy result from device to host
        cutilSafeCall(cudaMemcpy( h_data, d_odata[i], sizeof(float) * num_elements, 
                                   cudaMemcpyDeviceToHost));

        // If this is a regression test write the results to a file
        if( cutCheckCmdLineFlag( argc, (const char**) argv, "regression")) 
        {
            // write file for regression test 
            cutWriteFilef( "./data/result.dat", h_data, num_elements, 0.0);
        }
        else 
        {
            // custom output handling when no regression test running
            // in this case check if the result is equivalent to the expected soluion
            
            // We can use an epsilon of 0 since values are integral and in a range 
            // that can be exactly represented
            float epsilon = 0.0f;
            unsigned int result_regtest = cutComparefe( reference, h_data, num_elements, epsilon);
            char* names[] = {"scan_naive", "scan_workefficient", "scan_best"};
            printf( "%s: Test %s\n", names[i], (1 == result_regtest) ? "PASSED" : "FAILED");
        }
    }

    printf("\nCheck out the CUDA Data Parallel Primitives Library for more on scan.\n");
    printf("http://www.gpgpu.org/developer/cudpp\n");

    // cleanup memory
    free( h_data);
    free( reference);
    cutilSafeCall(cudaFree(d_idata));
    cutilSafeCall(cudaFree(d_odata[0]));
    cutilSafeCall(cudaFree(d_odata[1]));
    cutilSafeCall(cudaFree(d_odata[2]));
    cutilCheckError(cutDeleteTimer(timer));

    cudaThreadExit();
}
