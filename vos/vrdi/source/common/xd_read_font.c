/*	XD_Read_Font - Reads a font description file
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:	September 30, 1986
 *
 *	Calling Sequence:
 *
 *		STATUS = XD_Read_Font()
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"


#include <stdio.h>

FUNCTION int XD_Read_Font()
   {
   int	i, j, nChars, iChar, nVecs;
   FILE	*FontFile;
   char	FileName[256];
   char dummy;
   float cWid;
   int ptrMD;
   float ptrX,ptrY;

   for ( i = 0; i < MAX_FONT_CHAR+1; i++ ) {
      if (xd_font_info.ptrX[i]) {
	 free( xd_font_info.ptrX[i] );
	 xd_font_info.ptrX[i] = 0;
	 }
      if (xd_font_info.ptrY[i]) {
	 free( xd_font_info.ptrY[i] );
	 xd_font_info.ptrY[i] = 0;
	 }
      if (xd_font_info.ptrMD[i]) {
	 free( xd_font_info.ptrMD[i] );
	 xd_font_info.ptrMD[i] = 0;
	 }
      xd_font_info.cWidths[i] = 0;
      xd_font_info.vCount[i] = 0;
      }

#if VMS_OS
   sprintf( FileName, "vrdi$fonts:%03d.fon\000", FONT_NUMBER );
#endif /* VMS_OS */
#if UNIX_OS
   sprintf( FileName, "%s/%03d.fon", getenv("VRDIFONTS"), FONT_NUMBER );
#endif /* UNIX_OS */
   FontFile = fopen( FileName, "r" );
   if (FontFile == NULL) {
      return  (NO_SUCH_FONT);
      }

   if (fscanf( FontFile, " %d \n", &nChars ) != 1) {
      fclose( FontFile );
      return (FONT_FILE_READ_ERROR);
      }

   for ( i = 0; i < nChars; i++ ) {
      if (fscanf( FontFile, " %d%c %d%c %f \n",
			&iChar, &dummy, &nVecs, &dummy, &cWid ) != 5) {
	 fclose( FontFile );
	 return (FONT_FILE_READ_ERROR);
	 }

      if ((iChar < 0) || (iChar > MAX_FONT_CHAR)) {
	 fclose( FontFile );
	 return (BAD_ASCII_CODE);
	 }
      if (nVecs < 0) {
	 fclose( FontFile );
	 return (BAD_VECTOR_COUNT);
	 }

      xd_font_info.ptrX[iChar] = (float *) malloc( nVecs * sizeof (float) );
      xd_font_info.ptrY[iChar] = (float *) malloc( nVecs * sizeof (float) );
      xd_font_info.ptrMD[iChar] = (char *) malloc( nVecs * sizeof (char) );
      if ((xd_font_info.ptrX[iChar] == 0) ||
	 (xd_font_info.ptrY[iChar] == 0) ||
	 (xd_font_info.ptrMD[iChar] == 0)) {
	    fclose( FontFile );
	    free( xd_font_info.ptrX[iChar] );
	    free( xd_font_info.ptrY[iChar] );
	    free( xd_font_info.ptrMD[iChar] );
	    return (FONT_TABLE_OVERFLOW);
	    }

      xd_font_info.vCount[iChar] = nVecs;
      xd_font_info.cWidths[iChar] = cWid;

      for ( j = 0; j < nVecs; j++ ) {
	 if (fscanf( FontFile, " %d%c %f%c %f \n",
	    &ptrMD, &dummy, &ptrX, &dummy, &ptrY ) != 5) {
	       fclose( FontFile );
	       xd_font_info.vCount[iChar] = 0;
	       xd_font_info.cWidths[iChar] = 0;
	       return (FONT_FILE_READ_ERROR);
	       }
	 xd_font_info.ptrMD[iChar][j] = (char)(ptrMD);
	 xd_font_info.ptrX[iChar][j] = ptrX;
	 xd_font_info.ptrY[iChar][j] = ptrY;
	 }
      }

   fclose( FontFile );

   return (SUCCESS);
   }
