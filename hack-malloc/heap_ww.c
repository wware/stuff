/*
 * Copy the heap_2.c code from FreeRTOS, and then add the defrag stuff.
 * Apologies to FreeRTOS folks about violating some of your coding conventions.
 */

#define portBYTE_ALIGNMENT 4
#define portBYTE_ALIGNMENT_MASK ((1 << portBYTE_ALIGNMENT) - 1)
#define configUSE_MALLOC_FAILED_HOOK 0
#define configTOTAL_HEAP_SIZE 4096
#define pdFALSE 0
#define pdTRUE 1

#include <stdio.h>

typedef int portBASE_TYPE;

static void vTaskSuspendAll(void) { }
static void xTaskResumeAll(void) { }

static int defragEnabled = 0;

void enableDefrag(int yesPlease)
{
    defragEnabled = 1;
}

// VERBOSITY
#if 0
#define SHOW_FREE_LIST 1
#define COMMENT(x)     printf(x)
#define COMMENT1(x,a)  printf(x,a)
#else
#define SHOW_FREE_LIST 0
#define COMMENT(x)     ((void*) 0)
#define COMMENT1(x,a)  ((void*) 0)
#endif

/*
    FreeRTOS V7.0.1 - Copyright (C) 2011 Real Time Engineers Ltd.


    ***************************************************************************
     *                                                                       *
     *    FreeRTOS tutorial books are available in pdf and paperback.        *
     *    Complete, revised, and edited pdf reference manuals are also       *
     *    available.                                                         *
     *                                                                       *
     *    Purchasing FreeRTOS documentation will not only help you, by       *
     *    ensuring you get running as quickly as possible and with an        *
     *    in-depth knowledge of how to use FreeRTOS, it will also help       *
     *    the FreeRTOS project to continue with its mission of providing     *
     *    professional grade, cross platform, de facto standard solutions    *
     *    for microcontrollers - completely free of charge!                  *
     *                                                                       *
     *    >>> See http://www.FreeRTOS.org/Documentation for details. <<<     *
     *                                                                       *
     *    Thank you for using FreeRTOS, and thank you for your support!      *
     *                                                                       *
    ***************************************************************************


    This file is part of the FreeRTOS distribution.

    FreeRTOS is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License (version 2) as published by the
    Free Software Foundation AND MODIFIED BY the FreeRTOS exception.
    >>>NOTE<<< The modification to the GPL is included to allow you to
    distribute a combined work that includes FreeRTOS without being obliged to
    provide the source code for proprietary components outside of the FreeRTOS
    kernel.  FreeRTOS is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
    more details. You should have received a copy of the GNU General Public
    License and the FreeRTOS license exception along with FreeRTOS; if not it
    can be viewed here: http://www.freertos.org/a00114.html and also obtained
    by writing to Richard Barry, contact details for whom are available on the
    FreeRTOS WEB site.

    1 tab == 4 spaces!

    http://www.FreeRTOS.org - Documentation, latest information, license and
    contact details.

    http://www.SafeRTOS.com - A version that is certified for use in safety
    critical systems.

    http://www.OpenRTOS.com - Commercial support, development, porting,
    licensing and training services.
*/

/*
 * A sample implementation of pvPortMalloc() and vPortFree() that permits
 * allocated blocks to be freed, but does not combine adjacent free blocks
 * into a single larger block.
 *
 * See heap_1.c and heap_3.c for alternative implementations, and the memory
 * management pages of http://www.FreeRTOS.org for more information.
 */
#include <stdlib.h>

/* Defining MPU_WRAPPERS_INCLUDED_FROM_API_FILE prevents task.h from redefining
all the API functions to use the MPU wrappers.  That should only be done when
task.h is included from an application file. */
#define MPU_WRAPPERS_INCLUDED_FROM_API_FILE

//#include "FreeRTOS.h"
//#include "task.h"

#undef MPU_WRAPPERS_INCLUDED_FROM_API_FILE

/* The free list is actually two linked lists, both running through every unallocated block.
   The first list orders the free blocks by size, the number of bytes available for allocation
   starting with the smallest, and is used for parsimonious allocation. The second list orders
   free blocks by physical memory address, and is used to determine when a freed block is
   contiguous with one or two already free blocks, for purposes of defragmentation. */
typedef struct A_BLOCK_LINK
{
    struct A_BLOCK_LINK *pxNextSizeBlock;       /*<< The next free block by allocation size. */
    struct A_BLOCK_LINK *pxNextAddrBlock;       /*<< The next free block by physical memory address. */
    size_t xBlockSize;                          /*<< The size of the free block. */
} xBlockLink;

static xBlockLink xStartAddr;

/* Allocate the memory for the heap.  The struct is used to force byte
   alignment without using any non-portable code. */
static union xRTOS_HEAP
{
#if portBYTE_ALIGNMENT == 8
    volatile portDOUBLE dDummy;
#else
    volatile unsigned long ulDummy;
#endif
    unsigned char ucHeap[ configTOTAL_HEAP_SIZE ];
} xHeap;

static const unsigned short heapSTRUCT_SIZE = ( sizeof( xBlockLink ) + portBYTE_ALIGNMENT - ( sizeof( xBlockLink ) % portBYTE_ALIGNMENT ) );
#define heapMINIMUM_BLOCK_SIZE  ( ( size_t ) ( heapSTRUCT_SIZE * 2 ) )

/* Create a couple of list links to mark the start and end of the list. */
static xBlockLink xStartSize, xEnd;

/* Keeps track of the number of free bytes remaining, but says nothing about
   fragmentation. */
static size_t xFreeBytesRemaining = configTOTAL_HEAP_SIZE;

/* STATIC FUNCTIONS ARE DEFINED AS MACROS TO MINIMIZE THE FUNCTION CALL DEPTH. */

static void showFreeList(void)
{
    xBlockLink *pxIterator;
    printf("\nFree list sorted by size, first is xStartSize\n");
    for( pxIterator = &xStartSize;
         ;
         pxIterator = pxIterator->pxNextSizeBlock )
    {
        printf("Block %p, size %d\n", pxIterator, pxIterator->xBlockSize);
        if (pxIterator == &xEnd) break;
    }
    printf("Free list sorted by address, first is xStartAddr\n");
    for( pxIterator = &xStartAddr;
         ;
         pxIterator = pxIterator->pxNextAddrBlock )
    {
        printf("Block %p, size %d\n", pxIterator, pxIterator->xBlockSize);
        if (pxIterator == &xEnd) break;
    }
    printf("\n");
}

/*
 * Insert a block into the list of free blocks - which is ordered by size of
 * the block.  Small blocks at the start of the list and large blocks at the end
 * of the list.
 */
static void prvInsertBlockIntoFreeList(xBlockLink *pxBlockToInsert)
{
    xBlockLink *pxIterator;
    size_t xBlockSize;

    xBlockSize = pxBlockToInsert->xBlockSize;

    /* Iterate through the size-ordered list until a block is found that
       has a larger size than the block we are inserting. */
    for( pxIterator = &xStartSize;
         pxIterator->pxNextSizeBlock->xBlockSize < xBlockSize;
         pxIterator = pxIterator->pxNextSizeBlock )
    {
        /* There is nothing to do here - just iterate to the correct position. */
    }

    /* Update the size-ordered list to include the block being inserted in the correct position. */
    pxBlockToInsert->pxNextSizeBlock = pxIterator->pxNextSizeBlock;
    pxIterator->pxNextSizeBlock = pxBlockToInsert;

    /* Iterate through the address-ordered list until a block is found that
       has a larger size than the block we are inserting. */
    for ( pxIterator = &xStartAddr;
          pxIterator->pxNextAddrBlock != &xEnd &&
              (int) pxIterator->pxNextAddrBlock < (int) pxBlockToInsert;
          pxIterator = pxIterator->pxNextAddrBlock )
    {
        /* There is nothing to do here - just iterate to the correct position. */
    }

    /* Update the address-ordered list to include the block being inserted in the correct position. */
    pxBlockToInsert->pxNextAddrBlock = pxIterator->pxNextAddrBlock;
    pxIterator->pxNextAddrBlock = pxBlockToInsert;
#if SHOW_FREE_LIST
    showFreeList();
#endif
}

/*-----------------------------------------------------------*/

static void prvHeapInit(void)
{
    xBlockLink *pxFirstFreeBlock;

    /* xStartSize is used to hold a pointer to the first item in
       the list of free blocks.  The void cast is used to
       prevent compiler warnings. */
    xStartSize.pxNextSizeBlock = ( void * ) xHeap.ucHeap;
    xStartSize.xBlockSize = ( size_t ) 0;
    xStartAddr.pxNextAddrBlock = ( void * ) xHeap.ucHeap;

    /* xEnd is used to mark the end of the list of free blocks. */
    xEnd.xBlockSize = configTOTAL_HEAP_SIZE;
    xEnd.pxNextSizeBlock = NULL;
    xEnd.pxNextAddrBlock = NULL;

    /* To start with there is a single free block that
       is sized to take up the entire heap space. */
    pxFirstFreeBlock = ( void * ) &xHeap.ucHeap;
    pxFirstFreeBlock->xBlockSize = configTOTAL_HEAP_SIZE;
    pxFirstFreeBlock->pxNextSizeBlock = &xEnd;
    pxFirstFreeBlock->pxNextAddrBlock = &xEnd;

    if ((int)&xHeap >= (int)&xEnd) {
        printf("OUCH, xEnd must be higher in memory than the heap\n");
    }
    if ((int)&xHeap <= (int)&xStartAddr) {
        printf("OUCH, xStartAddr must be lower in memory than the heap\n");
    }
}

/*-----------------------------------------------------------*/

void *pvPortMalloc( size_t xWantedSize )
{
    xBlockLink *pxBlock;
    static portBASE_TYPE xHeapHasBeenInitialised = pdFALSE;
    void *pvReturn = NULL;

    vTaskSuspendAll();
    {
        /* If this is the first call to malloc then the heap will require
           initialisation to setup the list of free blocks. */
        if ( xHeapHasBeenInitialised == pdFALSE )
        {
            printf("Initializing heap\n");
            prvHeapInit();
            xHeapHasBeenInitialised = pdTRUE;
        }

        /* The wanted size is increased so it can contain a xBlockLink
           structure in addition to the requested amount of bytes. */
        if ( xWantedSize > 0 )
        {
            xWantedSize += heapSTRUCT_SIZE;

            /* Ensure that blocks are always aligned to the required number of bytes. */
            if( xWantedSize & portBYTE_ALIGNMENT_MASK )
            {
                /* Byte alignment required. */
                xWantedSize += ( portBYTE_ALIGNMENT - ( xWantedSize & portBYTE_ALIGNMENT_MASK ) );
            }
        }

        if ( ( xWantedSize > 0 ) && ( xWantedSize < configTOTAL_HEAP_SIZE ) )
        {
            /* Blocks are stored in byte order - traverse the list from the start
               (smallest) block until one of adequate size is found. */
            xBlockLink *pxPreviousSizeBlock = &xStartSize;
            pxBlock = xStartSize.pxNextSizeBlock;
            while ( ( pxBlock->xBlockSize < xWantedSize ) && ( pxBlock->pxNextSizeBlock ) )
            {
                pxPreviousSizeBlock = pxBlock;
                pxBlock = pxBlock->pxNextSizeBlock;
            }

            /* If we found the end marker then a block of adequate size was not found. */
            if ( pxBlock != &xEnd )
            {
                xBlockLink *pxPreviousAddrBlock = &xStartAddr;

                /* Return the memory space - jumping over the xBlockLink structure
                   at its start. */
                pvReturn = ( void * ) ( ( ( unsigned char * ) pxPreviousSizeBlock->pxNextSizeBlock ) +
                                        heapSTRUCT_SIZE );

                while ((int) pxPreviousAddrBlock->pxNextAddrBlock < (int) pxBlock)
                    pxPreviousAddrBlock = pxPreviousAddrBlock->pxNextAddrBlock;

                /* This block is being returned for use so must be taken our of the
                   list of free blocks. */
                pxPreviousSizeBlock->pxNextSizeBlock = pxBlock->pxNextSizeBlock;
                pxPreviousAddrBlock->pxNextAddrBlock = pxBlock->pxNextAddrBlock;

                /* If the block is larger than required it can be split into two. */
                if ( ( pxBlock->xBlockSize - xWantedSize ) > heapMINIMUM_BLOCK_SIZE )
                {
                    /* This block is to be split into two.  Create a new block
                       following the number of bytes requested. The void cast is
                       used to prevent byte alignment warnings from the compiler. */
                    xBlockLink *pxNewBlockLink = ( void * ) ( ( ( unsigned char * ) pxBlock ) + xWantedSize );

                    /* Calculate the sizes of two blocks split from the single block. */
                    pxNewBlockLink->xBlockSize = pxBlock->xBlockSize - xWantedSize;
                    pxBlock->xBlockSize = xWantedSize;

                    /* Insert the new block into the list of free blocks. */
                    prvInsertBlockIntoFreeList( ( pxNewBlockLink ) );
                    /* Splitting blocks incurs allocation cost of one struct size. */
                    xFreeBytesRemaining -= heapSTRUCT_SIZE;
                }

                xFreeBytesRemaining -= pxBlock->xBlockSize;
            }
        }
    }
    xTaskResumeAll();

#if configUSE_MALLOC_FAILED_HOOK == 1
    {
        if( pvReturn == NULL )
        {
            extern void vApplicationMallocFailedHook( void );
            vApplicationMallocFailedHook();
        }
    }
#endif

    return pvReturn;
}
/*-----------------------------------------------------------*/

#define START_OF_HEAP      ((xBlockLink*) &xHeap)
#define END_OF_HEAP        ((xBlockLink*) (((char*)&xHeap) + configTOTAL_HEAP_SIZE))
//#define END_OF_BLOCK(blk)  ((xBlockLink*) (((char*)blk) + heapSTRUCT_SIZE + blk->xBlockSize))
#define END_OF_BLOCK(blk)  ((xBlockLink*) (((char*)blk) + blk->xBlockSize))

static void remove_from_free_list(xBlockLink *victim, xBlockLink *previousByAddr)
{
    xBlockLink *previousBySize, *successorBySize;

    previousByAddr->pxNextAddrBlock = victim->pxNextAddrBlock;

    previousBySize = &xStartSize;
    while (previousBySize->pxNextSizeBlock != NULL) {
        successorBySize = previousBySize->pxNextSizeBlock;
        if (successorBySize->xBlockSize >= victim->xBlockSize)
            break;
        previousBySize = successorBySize;
    }
    previousBySize->pxNextSizeBlock = victim->pxNextSizeBlock;
}

void vPortFree( void *pv )
{
    unsigned char *puc = ( unsigned char * ) pv;
    xBlockLink *pxLink;

    if( pv )
    {
        /* The memory being freed will have an xBlockLink structure immediately
           before it. */
        puc -= heapSTRUCT_SIZE;

        /* This casting is to keep the compiler from issuing warnings. */
        pxLink = ( void * ) puc;

        vTaskSuspendAll();
        {
            if (defragEnabled) {
                xBlockLink *previousPrevious, *previous, *successor;

                previousPrevious = NULL;
                previous = &xStartAddr;
                while (previous->pxNextAddrBlock != &xEnd) {
                    successor = previous->pxNextAddrBlock;
                    if ((unsigned int) successor >= (unsigned int) pxLink)
                        break;
                    previousPrevious = previous;
                    previous = successor;
                }

                COMMENT1("pxLink %p\n", pxLink);
                COMMENT1("pxLink->xBlockSize %d\n", pxLink->xBlockSize);
                COMMENT1("END_OF_BLOCK(pxLink) %p\n", END_OF_BLOCK(pxLink));
                COMMENT1("successor %p\n", successor);
                COMMENT1("&xEnd %p\n", &xEnd);
                COMMENT1("successor != &xEnd ? %s\n",
                         (successor != &xEnd) ? "yes" : "no");
                COMMENT1("END_OF_BLOCK(pxLink) == successor ? %s\n",
                         (END_OF_BLOCK(pxLink) == successor) ? "yes" : "no");

                if (successor != &xEnd && END_OF_BLOCK(pxLink) == successor) {
                    COMMENT("contiguous with successor, so they can be merged\n");
                    remove_from_free_list(successor, previous);
                    pxLink->xBlockSize += successor->xBlockSize;
                }

                if (previous != &xStartAddr && END_OF_BLOCK(previous) == pxLink) {
                    COMMENT("contiguous with predecessor, so they can be merged\n");
                    remove_from_free_list(previous, previousPrevious);
                    previous->xBlockSize += pxLink->xBlockSize;
                    pxLink = previous;
                }
            }

            prvInsertBlockIntoFreeList( ( ( xBlockLink * ) pxLink ) );
            xFreeBytesRemaining += pxLink->xBlockSize;
        }
        xTaskResumeAll();
    }
}
/*-----------------------------------------------------------*/

size_t xPortGetFreeHeapSize( void )
{
    return xFreeBytesRemaining;
}
/*-----------------------------------------------------------*/

void vPortInitialiseBlocks( void )
{
    /* This just exists to keep the linker quiet. */
}
