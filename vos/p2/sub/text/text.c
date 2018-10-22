/******************************************************************************/
/*                                                                            */
/*  Fortran:                                                                  */
/*     integer inchr, line, size, dn                                          */
/*     character*inchr inbuf                                                  */
/*     byte outbuf(inchr*size)                                                */
/*     call text(inbuf, inchr, line, outbuf, size, dn)                        */
/*                                                                            */
/*  C:                                                                        */
/*     int inchr, line, size, dn;                                             */
/*     char inbuf[];                                                          */
/*     unsigned char outbuf[];                                                */
/*     text(inbuf, inchr, line, outbuf, size, dn);                            */
/*                                                                            */
/*  This is a text image generating routine, used to convert a string of text */
/*  into a dot matrix array.                                                  */
/*                                                                            */
/*  Each character is stored as a 7 line by 5 sample matrix.  To generate a   */
/*  text image, TEXT must be called 7 times--once for each image line needed. */
/*  Each output buffer also includes a blank space between each character,    */
/*  so you should actually allow for 6 samples per character in your output   */
/*  buffer.                                                                   */
/*                                                                            */
/*  Frankly, this isn't a particularly intelligent program and you must be    */
/*  careful to call it correctly.  It is your responsibility to allocate both */
/*  the input and output buffers, and your responsibility to maintain the line*/
/*  number count as you generate the 7 lines of the character string.  You    */
/*  must also set the size parameter correctly.  For normal character size,   */
/*  the size parameter = 6.  For magnified character size, see below.         */
/*                                                                            */
/*  This subroutine provides an option to generate larger size text by an     */
/*  integer magnification factor.  Unfortunately, the subroutine is still     */
/*  pretty stupid about this--you're not allowed to just pass in the mag-     */
/*  nification factor.  You have to multiply your magnification factor by     */
/*  6 (6 samples per character, counting the space that separates each char-  */
/*  acter) and pass that figure in using the size parameter.  You will also   */
/*  have to make sure that you have allocated enough space in your output     */
/*  buffer to handle the increased size.  This subroutine will handle expand- */
/*  ing each line, but your program will have to repeat each line according   */
/*  to the magnification factor you have specified.                           */
/*                                                                            */
/*  Finally, the dn parameter used to be optional.  Now that the program has  */
/*  been ported to Unix, it's a required parameter.  If you set it to 0, the  */
/*  subroutine will use a default value of 255 for the dn--otherwise, it will */
/*  use the value you specify.                                                */
/*                                                                            */
/*  Input Parameters:                                                         */
/*     inbuf  -- the array of ASCII characters to be converted                */
/*     inchr  -- the number of characters to be converted                     */
/*     line   -- the line number to be generated (0-6)                        */
/*     size   -- =6 for standard char size.  6*n for multiple size chars      */
/*     dn     -- DN value of the output characters                            */
/*                                                                            */
/*  Output Parameters:                                                        */
/*     outbuf -- the output array.  It must be at least inchr*size bytes long */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew  March 11, 1993                   */
/*  Converted to VMS by:   R. E. Alley  July 15, 1983                         */
/*  Original Author:       H. J. Frieden  June 2, 1977                        */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  07/10/93  81810  PDB - Removed LIB_LOCAL from ttext.imake.                */
/*  03/11/93   N/A   PDB - Ported to Unix and converted to C.                 */
/*  06/20/96   DFR   OAM - Added code to the Fortran bridge to                */
/*                         pass the entire string to ztext even if            */
/*                         padded with blanks on the right.                   */
/******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "text.h"
#include <stdlib.h>
#include <string.h>

FTN_NAME2(text, TEXT) (char *inbuf, int *inchr, int *line,
		unsigned char outbuf[],
		int *size, int *dn, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char *c_string;
   int  length,i;

   zsfor2len(length, inbuf, &inbuf, 6, 1, 1, dn);
   c_string = (char *) malloc(sizeof(char) * (length + 1));
   zsfor2c(c_string, length, inbuf, &inbuf, 6, 1, 1, dn);
   for ( i = strlen(c_string); i < length ; i++) c_string[i]=' ';
   c_string[i]='\n';
   ztext(c_string, *inchr, *line, outbuf, *size, *dn);
   free(c_string);
   return(1);
}

int ztext(inbuf, inchr, line, outbuf, size, dn)
char *inbuf;
int inchr, line;
unsigned char outbuf[];
int size, dn;
{
   int i, j, charindex, lineindex, magnify;
   unsigned char *ptr;

   magnify = size / (VT_NSAMPS+1);
   if (dn == 0)
      dn = 255;

   if (line > VT_NLINES-1 || line < 0) {
      for (i = 0; i < (VT_NSAMPS+1) * inchr; i++)
         outbuf[i] = 0;
   }
   else {
      ptr = (unsigned char *) outbuf;
      lineindex = line * VT_NSAMPS;
      for (i = 0; i < inchr; i++) {
         *ptr = 0;
         charindex = find_array_index(inbuf[i]);
         for (j = 0, ptr++; j < VT_NSAMPS; j++, ptr++) {
            if (textarr[charindex][lineindex + j] == 0)
               *ptr = 0;
            else
               *ptr = (unsigned char) dn;
         }
      }
   }

   if (magnify > 1)
      zexpand(outbuf, (VT_NSAMPS+1)*inchr, magnify);

   return (1);
}


/*  This routine takes an ASCII character value (0-127) and returns the index */
/*  to the character array in text.h.  Lower case letters are implicitly con- */
/*  verted to upper case because the array only contains upper case charac-   */
/*  ters.  Additionally, the Cent replaces '[', the VerticalBar replaces '\', */
/*  the Sigma replaces ']', and Delta replaces '^'.                           */

int find_array_index(charvalue)
unsigned char charvalue;
{
   int index;

   if (charvalue >= 128)
      charvalue -= 128;

   if (charvalue <= 64)
      index = charvalue - 1;
   else if (charvalue <= 96)
      index = charvalue - 65;
   else
      index = charvalue - 97;

   return (index);
}
