//*----------------------------------------------------------------------------
//*         ATMEL Microcontroller Software Support  -  ROUSSET  -
//*----------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*----------------------------------------------------------------------------
//* File Name           : Debug.h
//* Object              : Debug menu
//* Creation            : JPP   02/Sep/2004
//*----------------------------------------------------------------------------

#ifndef dbgu_h
#define dbgu_h

#include <stdio.h>

#define AT91C_DBGU_BAUD 115200

//* ----------------------- External Function Prototype -----------------------

void AT91F_DBGU_Init(void);
void AT91F_DBGU_Printk(	char *buffer);
void AT91F_DBGU_Frame(	char *buffer);
int AT91F_DBGU_Get( char *val);
void AT91F_DBGU_scanf(char * type,unsigned int * val);

#endif /* dbgu_h */
