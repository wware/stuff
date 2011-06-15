/*
 * Fake values for convenient testing of defrag code.
 */

#include <stdio.h>

#define ALIGNMENT_BITS 2
#define portBYTE_ALIGNMENT      (1 << ALIGNMENT_BITS)
#define portBYTE_ALIGNMENT_MASK (portBYTE_ALIGNMENT - 1)
#define configUSE_MALLOC_FAILED_HOOK 0
#define configTOTAL_HEAP_SIZE 4096
#define pdFALSE 0
#define pdTRUE 1

typedef int portBASE_TYPE;

#define vTaskSuspendAll()
#define xTaskResumeAll()

#ifdef ENABLE_DEBUG
#define DBGPRINTF(x)          printf("%s %d: ", __FILE__, __LINE__); printf(x)
#define DBGPRINTF1(x,a)       printf("%s %d: ", __FILE__, __LINE__); printf(x, a)
#define DBGPRINTF2(x,a,b)     printf("%s %d: ", __FILE__, __LINE__); printf(x, a, b)
#define DBGPRINTF3(x,a,b,c)   printf("%s %d: ", __FILE__, __LINE__); printf(x, a, b, c)
#else
#define DBGPRINTF(x)
#define DBGPRINTF1(x,a)
#define DBGPRINTF2(x,a,b)
#define DBGPRINTF3(x,a,b,c)
#endif

