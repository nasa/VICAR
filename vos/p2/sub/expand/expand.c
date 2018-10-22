/******************************************************************************/
/*                                                                            */
/*  FORTRAN:                                                                  */
/*  call expand(buf, nsi, inc)                                                */
/*     logical*1 buf(1)                                                       */
/*     integer nsi, inc                                                       */
/*                                                                            */
/*  C:                                                                        */
/*  zexpand(buf, nsi, inc)                                                    */
/*     unsigned char buf[];                                                   */
/*     int nsi;                                                               */
/*     int inc;                                                               */
/*                                                                            */
/*  This function will magnify the length of a video line by an integral      */
/*  multiple.  Magnification is performed by replicating pixels.  The func-   */
/*  tion is useful in programs which blow up pictures for display.            */
/*                                                                            */
/*  The parameters are:                                                       */
/*     buf - a byte array containing the line of data to be expanded.  The    */
/*           array must be large enough to contain the expanded data.         */
/*     nsi - the number of input samples.                                     */
/*     inc - the magnification factor.                                        */
/*                                                                            */
/*  Cognizant Programmer:  Florance Moss                                      */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  10-21-92   ---   PDB-Ported to UNIX.  Converted from Fortran to C.        */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"

void zexpand(buf, nsi, inc)
unsigned char buf[];
int nsi, inc;
{
   int index, i, j;

   index = nsi * inc - 1;
   for (i = nsi-1; i >= 0; i--) {
      for (j = 0; j < inc; j++, index--) {
         buf[index] = buf[i];
      }
   }
}

void FTN_NAME2(expand, EXPAND) (buf, nsi, inc)
unsigned char buf[];
int *nsi, *inc;
{
   zexpand(buf, *nsi, *inc);
}

