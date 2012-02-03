//*----------------------------------------------------------------------------
//*         ATMEL Microcontroller Software Support  -  ROUSSET  -
//*----------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*----------------------------------------------------------------------------
//* File Name           : Debug.c
//* Object              : Debug menu
//* Creation            : JPP   14/Sep/2004
//*----------------------------------------------------------------------------


// Include Standard files
#include "board.h"
#include "dbgu.h"
#define USART_SYS_LEVEL 4
/*---------------------------- Global Variable ------------------------------*/
//*--------------------------1--------------------------------------------------
//* \fn    AT91F_DBGU_Printk
//* \brief This function is used to send a string through the DBGU channel
//*----------------------------------------------------------------------------
void AT91F_DBGU_Ready(void)
{
	while (!(AT91C_BASE_DBGU->DBGU_CSR & AT91C_US_TXEMPTY));

}
//*----------------------------------------------------------------------------
//* Function Name       : Send_reset
//* Object              : Acknoledeg AIC and send reset
//*----------------------------------------------------------------------------
void Send_reset(void)
{
 void(*pfct)(void) = (void (*)(void) ) 0x00000000;

    // Acknoledge the interrupt
    // Mark the End of Interrupt on the AIC
     AT91C_BASE_AIC->AIC_EOICR=0;
     AT91F_DBGU_Ready();
    // Jump in reset
     pfct();
}

//*----------------------------------------------------------------------------
//* Function Name       : DBGU_irq_handler
//* Object              : C handler interrupt function called by the interrupts
//*                       assembling routine
//*----------------------------------------------------------------------------
void DBGU_irq_handler(void)
{
 char value;

       AT91F_DBGU_Get(&value);
       switch (value) {
       case '0': //* info
         AT91F_DBGU_Frame("Set Pull up\n\r");
         // Set
          AT91F_PIO_ClearOutput(AT91C_BASE_PIOA,AT91C_PIO_PA16);
       break;
       case '1': //* info
           AT91F_PIO_SetOutput(AT91C_BASE_PIOA,AT91C_PIO_PA16);
           AT91F_DBGU_Printk("Clear Pull up\n\r");
         // Reset Application
            Send_reset();
       break;
       default:
	   AT91F_DBGU_Printk("\n\r");
        break;
       }// end switch
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_DBGU_Init
//* \brief This function is used to send a string through the DBGU channel (Very low level debugging)
//*----------------------------------------------------------------------------
void AT91F_DBGU_Init(void)
{
    //* Open PIO for DBGU
        AT91F_DBGU_CfgPIO();
  //* Enable Transmitter & receivier
       ((AT91PS_USART)AT91C_BASE_DBGU)->US_CR = AT91C_US_RSTTX |AT91C_US_RSTRX;

    //* Configure DBGU
	AT91F_US_Configure (
		(AT91PS_USART) AT91C_BASE_DBGU,       // DBGU base address
		MCK,
		AT91C_US_ASYNC_MODE ,                 // Mode Register to be programmed
		AT91C_DBGU_BAUD ,                     // Baudrate to be programmed
		0);                                   // Timeguard to be programmed

    //* Enable Transmitter & receivier
       ((AT91PS_USART)AT91C_BASE_DBGU)->US_CR = AT91C_US_RXEN | AT91C_US_TXEN;

    //* Enable USART IT error and AT91C_US_ENDRX
    	AT91F_US_EnableIt((AT91PS_USART) AT91C_BASE_DBGU,AT91C_US_RXRDY);

    //* open interrupt
	AT91F_AIC_ConfigureIt ( AT91C_BASE_AIC, AT91C_ID_SYS, USART_SYS_LEVEL,
                                AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL, DBGU_irq_handler);
	AT91F_AIC_EnableIt (AT91C_BASE_AIC, AT91C_ID_SYS);
}
//*----------------------------------------------------------------------------
//* \fn    AT91F_DBGU_Printk
//* \brief This function is used to send a string through the DBGU channel (Very low level debugging)
//*----------------------------------------------------------------------------
void AT91F_DBGU_Printk(	char *buffer)
{
    while(*buffer != '\0') {
	while (!AT91F_US_TxReady((AT91PS_USART)AT91C_BASE_DBGU));
	AT91F_US_PutChar((AT91PS_USART)AT91C_BASE_DBGU, *buffer++);
    }
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_DBGU_Frame
//* \brief This function is used to send a string through the DBGU channel
//*----------------------------------------------------------------------------
void AT91F_DBGU_Frame(	char *buffer)
{
	unsigned char len;

	for (len =0; buffer[len] != '\0'; len++){}
	AT91F_US_SendFrame((AT91PS_USART)AT91C_BASE_DBGU, buffer,len,0,0);

}

//*----------------------------------------------------------------------------
//* \fn    AT91F_US_Get
//* \brief Get a Char to USART
//*----------------------------------------------------------------------------
 int AT91F_DBGU_Get( char *val)
{
    if ((AT91F_US_RxReady((AT91PS_USART)AT91C_BASE_DBGU)) == 0) return (false);
    else
    {
	*val= AT91F_US_GetChar((AT91PS_USART)AT91C_BASE_DBGU);
        return (true);
    }
}
//*----------------------------------------------------------------------------
//* \fn    AT91F_DBGU_scanf
//* \brief Get a string to USART manage Blackspace and echo
//*----------------------------------------------------------------------------
void AT91F_DBGU_scanf(char * type,unsigned int * val)
{//* Begin
    unsigned int read = 0;
    char buff[10];
    unsigned int nb_read =0;

    while( (read != 0x0D) & (nb_read != sizeof(buff)) ) {
        //* wait the USART Ready for reception
	 while((AT91C_BASE_DBGU->DBGU_CSR  & AT91C_US_RXRDY) == 0 ) ;
        //* Get a char
	read = AT91C_BASE_DBGU->DBGU_RHR ;
        buff[nb_read]= (char)read;
        //* Manage Blackspace
        while((AT91C_BASE_DBGU->DBGU_CSR & AT91C_US_TXRDY) ==0)  {}
        if ((char)read == 0x08) {
            if ( nb_read != 0 ) {
              nb_read--;
              AT91C_BASE_DBGU->DBGU_THR = read;
            }
        }
        else {
          //* echo
          AT91C_BASE_DBGU->DBGU_THR = read;
          nb_read++;
        }
    }
#if 0 /* linker hassles */
    //* scan the value
    sscanf(buff,type,val);
#else
    {
        const char *message = "Sorry, no sscanf";
        int i;
        for (i = 0; message[i] != '\0'; i++)
            buff[i] = message[i];
        buff[i] = '\0';
    }
#endif
}//* End

