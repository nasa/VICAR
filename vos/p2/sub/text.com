$!****************************************************************************
$!
$! Build proc for MIPL module text
$! VPACK Version 1.9, Monday, December 07, 2009, 16:38:15
$!
$! Execute by entering:		$ @text
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module text ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to text.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("text.imake") .nes. ""
$   then
$      vimake text
$      purge text.bld
$   else
$      if F$SEARCH("text.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake text
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @text.bld "STD"
$   else
$      @text.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create text.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack text.com -mixed -
	-s text.c text.h -
	-i text.imake -
	-t ttext.c ttextf.f ttext.imake ttext.pdf -
	-o text.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create text.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create text.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************/
/*  This is the include file that accompanies text.c.  Do you need any more   */
/*  documentation than that?                                                  */
/******************************************************************************/

#define VT_NLINES	7
#define VT_NSAMPS	5

unsigned char textarr[64][VT_NLINES * VT_NSAMPS] = {

/* 0.  A */
   0, 0, 1, 0, 0,
   0, 1, 0, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,

/* 1.  B */
   1, 1, 1, 1, 0,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   0, 1, 1, 1, 0,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   1, 1, 1, 1, 0,

/* 2.  C */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 3.  D */
   1, 1, 1, 1, 0,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   0, 1, 0, 0, 1,
   1, 1, 1, 1, 0,

/* 4.  E */
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 1,

/* 5.  F */
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,

/* 6.  G */
   0, 1, 1, 1, 1,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 1, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 1,

/* 7.  H */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,

/* 8.  I */
   1, 1, 1, 1, 1,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   1, 1, 1, 1, 1,

/* 9.  J */
   0, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 10.  K */
   1, 0, 0, 0, 1,
   1, 0, 0, 1, 0,
   1, 0, 1, 0, 0,
   1, 1, 0, 0, 0,
   1, 0, 1, 0, 0,
   1, 0, 0, 1, 0,
   1, 0, 0, 0, 1,

/* 11.  L */
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 1,

/* 12.  M */
   1, 0, 0, 0, 1,
   1, 1, 0, 1, 1,
   1, 0, 1, 0, 1,
   1, 0, 1, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,

/* 13.  N */
   1, 0, 0, 0, 1,
   1, 1, 0, 0, 1,
   1, 0, 1, 0, 1,
   1, 0, 0, 1, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,

/* 14.  O */
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 1,

/* 15.  P */
   1, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,

/* 16.  Q */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 1, 0, 1,
   1, 0, 0, 1, 0,
   0, 1, 1, 0, 1,

/* 17.  R */
   1, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 0,
   1, 0, 1, 0, 0,
   1, 0, 0, 1, 0,
   1, 0, 0, 0, 1, 

/* 18.  S */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   0, 1, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 1, 0,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 19.  T */
   1, 1, 1, 1, 1,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,

/* 20.  U */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 21.  V */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 0, 1, 0,
   0, 1, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,

/* 22.  W */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 1, 0, 1,
   1, 1, 0, 1, 1,
   1, 0, 0, 0, 1,

/* 23.  X */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,

/* 24.  Y */
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,

/* 25.  Z */
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 1,

/* 26.  Cent */
   0, 0, 1, 0, 0,
   0, 1, 1, 1, 0,
   1, 0, 1, 0, 0,
   1, 0, 1, 0, 0,
   1, 0, 1, 0, 0,
   0, 1, 1, 1, 0,
   0, 0, 1, 0, 0,

/* 27.  VerticalBar */
   0, 0, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 0, 0,

/* 28.  Sigma */
   0, 0, 0, 0, 0,
   0, 1, 1, 1, 1,
   1, 0, 0, 1, 0,
   1, 0, 0, 1, 0,
   1, 0, 0, 1, 0,
   0, 1, 1, 0, 0,
   0, 0, 0, 0, 0,

/* 29.  Delta */
   0, 0, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 1, 0,
   1, 0, 0, 0, 1,
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 0,

/* 30.  Underline */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   1, 1, 1, 1, 1,

/* 31.  Space */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 32.  Exclamation */
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 1, 0, 0,

/* 33.  Quote */
   0, 1, 0, 1, 0,
   0, 1, 0, 1, 0,
   0, 1, 0, 1, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 34.  Pound */
   0, 1, 0, 1, 0,
   0, 1, 0, 1, 0,
   1, 1, 0, 1, 1,
   0, 0, 0, 0, 0,
   1, 1, 0, 1, 1,
   0, 1, 0, 1, 0,
   0, 1, 0, 1, 0,

/* 35.  Dollar */
   0, 0, 1, 0, 0,
   0, 1, 1, 1, 1,
   1, 0, 1, 0, 0,
   0, 1, 1, 1, 0,
   0, 0, 1, 0, 1,
   1, 1, 1, 1, 0,
   0, 0, 1, 0, 0,

/* 36.  Percent */
   1, 1, 0, 0, 1,
   1, 1, 0, 0, 1,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 1, 1,
   1, 0, 0, 1, 1,

/* 37.  Ampersand */
   0, 1, 0, 0, 0,
   1, 0, 1, 0, 0,
   1, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 1, 0, 1,
   1, 0, 0, 1, 0,
   0, 1, 1, 0, 1,

/* 38.  Apostrophe */
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 39.  LeftParen */
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   0, 1, 0, 0, 0,
   0, 1, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 1, 0,

/* 40.  RightParen */
   0, 1, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 1, 0,
   0, 0, 0, 1, 0,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,

/* 41.  Asterisk */
   0, 0, 0, 0, 0,
   1, 0, 1, 0, 1,
   0, 1, 1, 1, 0,
   1, 1, 1, 1, 1,
   0, 1, 1, 1, 0,
   1, 0, 1, 0, 1,
   0, 0, 0, 0, 0,

/* 42.  Plus */
   0, 0, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   1, 1, 1, 1, 1,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 0, 0,

/* 43.  Comma */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,

/* 44.  Minus */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 45.  Period */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,

/* 46.  Slash */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 1,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 47.  Zero */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 48.  One */
   0, 0, 1, 0, 0,
   0, 1, 1, 0, 0,
   1, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   1, 1, 1, 1, 1,

/* 49.  Two */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 1,

/* 50.  Three */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 0, 1, 1, 0,
   0, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 51.  Four */
   0, 0, 0, 1, 0,
   0, 0, 1, 1, 0,
   0, 1, 0, 1, 0,
   1, 0, 0, 1, 0,
   1, 1, 1, 1, 1,
   0, 0, 0, 1, 0,
   0, 0, 0, 1, 0,

/* 52.  Five */
   1, 1, 1, 1, 1,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 0,
   0, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 53.  Six */
   0, 0, 1, 1, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 0, 0,
   1, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 54.  Seven */
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   0, 1, 0, 0, 0,
   0, 1, 0, 0, 0,

/* 55.  Eight */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 0,

/* 56.  Nine */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   1, 0, 0, 0, 1,
   0, 1, 1, 1, 1,
   0, 0, 0, 0, 1,
   0, 0, 0, 1, 0,
   0, 1, 1, 0, 0,

/* 57.  Colon */
   0, 0, 0, 0, 0,
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,
   0, 0, 0, 0, 0,
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,
   0, 0, 0, 0, 0,

/* 58.  Semicolon */
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,
   0, 0, 0, 0, 0,
   0, 1, 1, 0, 0,
   0, 1, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,

/* 59.  LessThan */
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 0, 0,
   0, 1, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 1, 0, 

/* 60.  Equal */
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 0,
   1, 1, 1, 1, 1,
   0, 0, 0, 0, 0,
   0, 0, 0, 0, 0,

/* 61.  GreaterThan */
   1, 0, 0, 0, 0,
   0, 1, 0, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 1, 0, 0, 0,
   1, 0, 0, 0, 0,

/* 62.  QuestionMark */
   0, 1, 1, 0, 0,
   1, 0, 0, 1, 0,
   0, 0, 0, 1, 0,
   0, 0, 1, 0, 0,
   0, 0, 1, 0, 0,
   0, 0, 0, 0, 0,
   0, 0, 1, 0, 0,

/* 63.  At */
   0, 1, 1, 1, 0,
   1, 0, 0, 0, 1,
   0, 0, 0, 0, 1,
   0, 1, 1, 0, 1,
   1, 0, 1, 0, 1,
   1, 0, 1, 0, 1,
   0, 1, 1, 1, 0
};
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create text.imake
/*  Imake file for VICAR subroutine TEXT  */

#define SUBROUTINE text

#define MODULE_LIST text.c
#define INCLUDE_LIST text.h

#define FTN_STRING

#define USES_ANSI_C

#define P2_SUBLIB

#define LIB_P2SUB
$ Return
$!#############################################################################
$Test_File:
$ create ttext.c
#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
   int size, line, inchr, test, dn;
   char *inbuf = {"ABCD2468[\\]^abcd"};
   unsigned char outbuf[72];

   zvmessage("\nTesting TEXT subroutine C interface.\n", "");

   inchr = 4;
   for (test = 0; test < 4; test++) {
      for (size = 1; size <= 3; size++) {
         dn = size * 35;
         for (line = 0; line < 7; line++) {
            ztext(inbuf+(inchr*test), inchr, line, outbuf, size*6, dn);
            print_buffer(outbuf, inchr*size*6);
         }
         zvmessage("", "");
      }
   }

   FTN_NAME(ttextf)();
   exit(0);   
}


print_buffer(buf, nsi)
unsigned char buf[];
int nsi;
{
   int i;
   unsigned char msg[81];

   for (i = 0; i < nsi; i++) {
      if (buf[i] == 0)
         msg[i] = ' ';
      else
         msg[i] = buf[i];
   }
   msg[nsi] = 0;
   zvmessage(msg, "");
}
$!-----------------------------------------------------------------------------
$ create ttextf.f
C  Subroutine to test the Fortran interface to TEXT

      subroutine ttextf()

      character*4 inbuf(4) /'ABCD', '2468', '[\]^', 'abcd'/
      character*80 msg
      byte outbuf(80)
      integer size, line, inchr, test, dn, i, nbytes

      call xvmessage(' ', ' ')
      call xvmessage('Testing TEXT subroutine Fortran interface.', ' ')
      call xvmessage(' ', ' ')

      inchr = 4
      do i = 1, 80
         outbuf(i) = 32
      enddo

      do test = 1, 4
         do size = 1, 3
            dn = size * 35
            nbytes = size * 6 * inchr
            do line = 0, 6
               call text(inbuf(test), inchr, line, outbuf, size*6, dn)
               do i = 1, nbytes
                  if (outbuf(i) .eq. 0) then
                     outbuf(i) = 32
                  endif
               enddo
               write(msg, 100) (outbuf(i), i = 1, nbytes)
               call xvmessage(msg, ' ')
            enddo
            call xvmessage(' ', ' ')
         enddo
      enddo

      return
 100  format(80a1)
      end
$!-----------------------------------------------------------------------------
$ create ttext.imake
/*  Imake file for test of VICAR subroutine TEXT  */

#define PROGRAM ttext

#define MODULE_LIST ttext.c ttextf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create ttext.pdf
process
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create text.hlp
1 Text

  Fortran:
     integer inchr, line, size, dn
     character*inchr inbuf
     byte outbuf(inchr*size)
     call text(inbuf, inchr, line, outbuf, size, dn)

  C:
     int inchr, line, size, dn;
     char inbuf[];
     unsigned char outbuf[];
     text(inbuf, inchr, line, outbuf, size, dn);

  This is a text image generating routine, used to convert a string of text
  into a dot matrix array.

  Each character is stored as a 7 line by 5 sample matrix.  To generate a
  text image, TEXT must be called 7 times--once for each image line needed.
  Each output buffer also includes a blank space between each character,
  so you should actually allow for 6 samples per character in your output
  buffer.

  Frankly, this isn't a particularly intelligent program and you must be
  careful to call it correctly.  It is your responsibility to allocate both
  the input and output buffers, and your responsibility to maintain the line
  number count as you generate the 7 lines of the character string.  You
  must also set the size parameter correctly.  For normal character size,
  the size parameter = 6.  For magnified character size, see below.

  This subroutine provides an option to generate larger size text by an
  integer magnification factor.  Unfortunately, the subroutine is still
  pretty stupid about this--you're not allowed to just pass in the mag-
  nification factor.  You have to multiply your magnification factor by
  6 (6 samples per character, counting the space that separates each char-
  acter) and pass that figure in using the size parameter.  You will also
  have to make sure that you have allocated enough space in your output
  buffer to handle the increased size.  This subroutine will handle expand-
  ing each line, but your program will have to repeat each line according
  to the magnification factor you have specified.

  Finally, the dn parameter used to be optional.  Now that the program has
  been ported to Unix, it's a required parameter.  If you set it to 0, the
  subroutine will use a default value of 255 for the dn--otherwise, it will
  use the value you specify.

  Input Parameters:
     inbuf  -- the array of ASCII characters to be converted
     inchr  -- the number of characters to be converted
     line   -- the line number to be generated (0-6)
     size   -- =6 for standard char size.  6*n for multiple size chars
     dn     -- DN value of the output characters

  Output Parameters:
     outbuf -- the output array.  It must be at least inchr*size bytes long


2 History

  Original Programmer:  H. J. Frieden  1977-06-02
  Converted to Vax by:  R. E. Alley    1983-07-15
  Ported to Unix:       Paul Bartholomew  03-12-93
  Source Language:  C
$ Return
$!#############################################################################
