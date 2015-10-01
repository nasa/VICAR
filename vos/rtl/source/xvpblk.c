#include "xvmaininc.h"
#include "ftnbridge.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if VMS_OS
#pragma nostandard		/* turn off portability check on PUBLICREF */
#endif

PUBLICREF int in_vicar;

#if VMS_OS
#pragma standard
#endif

/************************************************************************/
/* xvpblk - Return the address of the parameter block to be used by	*/
/*          the executive						*/
/*									*/
/* This routine must be used for VMS instead of a common block to allow	*/
/* the address of an area of memory internal to the VICAR shareable	*/
/* image to be passed outside of the shareable image.			*/
/*									*/
/* In addition, it sets the global variable in_vicar to TRUE so that	*/
/* other routines (specifically xvmessage) can tell we are under TAE.	*/
/* This may get reset later if we're in shell-VICAR.			*/
/************************************************************************/

#if RTL_USE_TAE

void FTN_NAME2(xvpblk, XVPBLK) (int **parb_addr)
{
   in_vicar = 1;
   *parb_addr = (int *) &parb;
}

void zvpblk(void **parb_addr)
{
   in_vicar = 1;
   *parb_addr = (void *)&parb;
}

#else

void FTN_NAME2(xvpblk, XVPBLK) (int **parb_addr)
{
   in_vicar = 0;
   *parb_addr = NULL;
}

void zvpblk(void **parb_addr)
{
   in_vicar = 0;
   *parb_addr = NULL;
}

#endif

