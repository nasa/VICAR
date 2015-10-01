$!****************************************************************************
$!
$! Build proc for MIPL module txtsubs
$! VPACK Version 1.9, Monday, December 07, 2009, 16:40:27
$!
$! Execute by entering:		$ @txtsubs
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
$ write sys$output "*** module txtsubs ***"
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
$ write sys$output "Invalid argument given to txtsubs.com file -- ", primary
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
$   if F$SEARCH("txtsubs.imake") .nes. ""
$   then
$      vimake txtsubs
$      purge txtsubs.bld
$   else
$      if F$SEARCH("txtsubs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake txtsubs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @txtsubs.bld "STD"
$   else
$      @txtsubs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create txtsubs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack txtsubs.com -mixed -
	-s txtsubs.c -
	-i txtsubs.imake -
	-t ttxtsubs.c ttxtsubsf.f ttxtsubs.imake ttxtsubs.pdf tsttxtsub.pdf -
	-o txtsubs.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create txtsubs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************/
/*                                                                            */
/*  Original Programmer:  Kevin Hussey   July 15, 1985                        */
/*  Cognizant Programmer:  Allan Runkle  July 15, 1985                        */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  01-07-93   ---   PDB - Ported to UNIX.  Converted from Fortran to C.      */
/*  05-15-00  104468 AXC - Modified condition statements in functions         */
/*                         fntlchr and txttext to process blank/space         */
/*                         within input strings.                              */
/******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

#define MAXFBUF		4000
#define PI		3.14159265359
#define SIGN(value)	((value) < 0 ? -1 : 1)
#define TRUE		1
#define FALSE		0

unsigned char txfcolor;
int   txfhite;
int   txfcptr[128], txfclen[128];
int   txfmove[MAXFBUF];
float txfscale, txfcosa, txfsina;
float txfcwid[128];
float txfxvec[MAXFBUF];
float txfyvec[MAXFBUF];


/******************************************************************************/
/*                                                                            */
/*    Set the color value c used to write text.                               */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txtcolor(c)                                                    */
/*       integer*2 c                                                          */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxtcolor(c)                                                   */
/*       short c;                                                             */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        c      Color value (DN value)                                       */
/*                                                                            */
/*    Returned                                                                */
/*     Status    Description                                                  */
/*        0      Function not implemented                                     */
/*        1      Normal return (logical TRUE)                                 */
/*        2      Illegal color value                                          */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txtcolor, TXTCOLOR) (c)
short *c;
{
   return (ztxtcolor(*c));
}

int ztxtcolor(c)
short c;
{
   if (c < 0 || c > 255)
      return (2);

   txfcolor = (unsigned char) c;
   return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Reads font description file into internal font table                    */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txtfont(f)                                                     */
/*       integer*2 f                                                          */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxtfont(f)                                                    */
/*       short f;                                                             */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        f      Font type                                                    */
/*                                                                            */
/*    Returned                                                                */
/*     Status    Description                                                  */
/*        0      Function not implemented                                     */
/*        1      Normal return (logical TRUE)                                 */
/*        2      Illegal font type                                            */
/*        4      Font table too small                                         */
/*        6      End-of-file on font description file                         */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txtfont, TXTFONT) (f)
short *f;
{
   return (ztxtfont(*f));
}

int ztxtfont(f)
short f;
{
   int   i, nchars, ichar, nvecs, lasbuf, jj, status;
   short ifont;
   float width;
   char  fname[121];
   FILE  *file=NULL;

   if (f == 0)
      ifont = 1;
   else
      ifont = f;

   memset(txfcptr, 0, 128*sizeof(int));
   memset(txfclen, 0, 128*sizeof(int));
   memset(txfcwid, 0, 128*sizeof(float));

#if VMS_OS
   sprintf(fname, "vrdi$fonts:%03d.fon", ifont);
#else
   sprintf(fname, "%s/%03d.fon\000", getenv("VRDIFONTS"), ifont);
#endif
   file = fopen(fname, "r");
   if (file == NULL)
      return (2);

   lasbuf = 0;
   status = fscanf(file, "%d\n", &nchars);
   if (status != 1) {
      fclose(file);
      return (6);
   }

   for (i = 0; i < nchars; i++) {
      status = fscanf(file, "%d, %d, %f\n", &ichar, &nvecs, &width);
      if (status != 3) {
         fclose(file);
         return (6);
      }
      if (lasbuf + nvecs > MAXFBUF) {
         fclose(file);
         return (4);
      }

      txfclen[ichar] = nvecs;
      txfcwid[ichar] = width;
      txfcptr[ichar] = lasbuf;

      for (jj = 0; jj < nvecs; jj++, lasbuf++) {
         status = fscanf(file, "%d, %f, %f\n", &txfmove[lasbuf],
              &txfxvec[lasbuf], &txfyvec[lasbuf]);
         if (status != 3) {
            fclose(file);
            return (6);
         }
      }
   }
   fclose(file);
   return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Returns length of character string in pixels                            */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txtlength(l, n, a)                                             */
/*       integer*2 l, n                                                       */
/*       byte a(n)                                                            */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxtlength(l, n, a)                                            */
/*       short *l, n;                                                         */
/*       unsigned char a[n];                                                  */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        l      returned length of string                                    */
/*        n      Number of characters in string                               */
/*        a      Array of characters                                          */
/*                                                                            */
/*    Returned                                                                */
/*     Status    Description                                                  */
/*        0      Function not implemented                                     */
/*        1      Normal return (logical TRUE)                                 */
/*        2      Illegal character count                                      */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txtlength, TXTLENGTH) (l, n, a)
short *l, *n;
unsigned char a[];
{
   return (ztxtlength(l, *n, a));
}

int ztxtlength(l, n, a)
short *l, n;
unsigned char a[];
{
   if (n < 0)
      return (2);

   *l = (short) zfntlstr(n, a);
   return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Special function to determine the length of a string                    */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    length = fntlstr(n, str)                                                */
/*       integer*2 n                                                          */
/*       byte str(n)                                                          */
/*                                                                            */
/*    C:                                                                      */
/*    length = zfntlstr(n, str);                                              */
/*       short n;                                                             */
/*       unsigned char str[n];                                                */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(fntlstr, FNTLSTR) (n, str)
short *n;
unsigned char str[];
{
   return ((short) zfntlstr(*n, str));
}

int zfntlstr(n, str)
short n;
unsigned char str[];
{
   short i, len, ichar;

   for (len = 0, i = 0; i < n; i++) {
      ichar = (short) str[i];
      len += (short) zfntlchr(ichar) + 1;
   }
   return (len);
}


/******************************************************************************/
/*                                                                            */
/*    Special function to find the length of a character                      */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    length = fntlchr(ichar)                                                 */
/*       integer*2 ichar                                                      */
/*                                                                            */
/*    C:                                                                      */
/*    length = zfntlchr(ichar);                                               */
/*       short ichar;                                                         */
/*                                                                            */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(fntlchr, FNTLCHR) (ichar)
short *ichar;
{
   return (zfntlchr(*ichar));
}

int zfntlchr(ichar)
short ichar;
{
   int width;

   if (txfcptr[ichar] < 0)
      return (0);

   width = (short) (txfscale * (float) txfhite * txfcwid[ichar] + 0.5);
   return (width);
}


/******************************************************************************/
/*                                                                            */
/*    Sets angle above or below the X-axis text is to be written              */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txtrotate(a)                                                   */
/*       real a                                                               */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxtrotate(a);                                                 */
/*       float a;                                                             */
/*                                                        |+90                */
/*                                                        |                   */
/*                                                        |                   */
/*    Parameter  Description                              |                   */
/*                                                        |                   */
/*        a      Angle to rotate text       +-180 ----------------- 0         */
/*                                                        |                   */
/*                                                        |                   */
/*    Returned                                            |                   */
/*     Status    Description                              |                   */
/*                                                        |-90                */
/*       0       Function not implemented                                     */
/*       1       Normal return (logical TRUE)                                 */
/*       2       Illegal angle                                                */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txtrotate, TXTROTATE) (a)
float *a;
{
   return (ztxtrotate(*a));
}

int ztxtrotate(a)
float a;
{
   if (a < -180.0 || a > 180.0)
      return (2);

   txfsina = sin(a * PI / 180.0);
   txfcosa = cos(a * PI / 180.0);
   return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Sets the height and horizontal scale factor for text                    */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txtsize(h, s)                                                  */
/*       integer*2 h                                                          */
/*       real s                                                               */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxtsize(h, s);                                                */
/*       short h;                                                             */
/*       float s;                                                             */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        h      height of characters (in lines)                              */
/*        s      horizontal scale factor (width/height)                       */
/*                                                                            */
/*    Returned                                                                */
/*     Status    Description                                                  */
/*        0      Function not implemented                                     */
/*        1      Normal return (logical TRUE)                                 */
/*        2      Illegal height                                               */
/*        4      Illegal horizontal scale factor                              */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txtsize, TXTSIZE)(h, s)
short *h;
float *s;
{
   return (ztxtsize(*h, *s));
}

int ztxtsize(h, s)
short h;
float s;
{
   if (h < 0)
      return (2);

   if (s < 0.0)
      return (4);

    txfhite = h;
    txfscale = s;
    return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Write a line of text to an image                                        */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    status = txttext(i, nl, ns, x, y, loc, n, a, flag1)                     */
/*       byte i(ns, nl)                                                       */
/*       integer*2 nl, ns, x, y, loc, n                                       */
/*       byte a(n)                                                            */
/*       logical flag1                                                        */
/*                                                                            */
/*    C:                                                                      */
/*    status = ztxttext(i, nl, ns, x, y, loc, n, a, flag1)                    */
/*       unsigned char i[nl][ns];                                             */
/*       short nl, ns, x, y, loc, n;                                          */
/*       unsigned char a[n];                                                  */
/*       int *flag1;                                                          */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        i      name of array into which text is written                     */
/*        x      x-coordinate of text position                                */
/*        y      y-coordinate of text position                                */
/*       loc     orientation of text about (X,Y)                              */
/*               loc value  Description                                       */
/*                    1     x,y is lower left corner of text                  */
/*                    2     x,y is bottom center of text                      */
/*                    3     x,y is lower right corner of text                 */
/*        n      number of characters in text                                 */
/*        a      text string                                                  */
/*                                                                            */
/*    Returned                                                                */
/*     Status    Description                                                  */
/*        0      Function not implemented                                     */
/*        1      Normal return  (logical TRUE)                                */
/*        8      Coordinates are outside access window                        */
/*       10      Illegal LOC value                                            */
/*       12      Illegal character count                                      */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txttext, TXTTEXT) (i, nl, ns, x, y, loc, n, a, flag1)
unsigned char *i;
short *nl, *ns, *x, *y, *loc, *n;
unsigned char *a;
int   *flag1;
{
   return (ztxttext(i, *nl, *ns, *x, *y, *loc, *n, a, flag1));
}

int ztxttext(i, nl, ns, x, y, loc, n, a, flag1)
unsigned char *i;
short nl, ns, x, y, loc, n;
unsigned char *a;
int   *flag1;
{
   short ii, ichar, sx, sy;
   int   flag2;
   float dx, dy, len, temp;

   if (loc == 3) {
      len = (float) zfntlstr(n, a);
      temp = txfcosa * len;
      sx = (temp < 0) ? x - (int) (temp - 0.5) : x - (int) (temp + 0.5);
      temp = txfsina * len;
      sy = (temp < 0) ? y + (int) (temp - 0.5) : y + (int) (temp + 0.5);
   }
   else if (loc == 2) {
      len = (float) zfntlstr(n, a) / 2.0;
      temp = txfcosa * len;
      sx = (temp < 0) ? x - (int) (temp - 0.5) : x - (int) (temp + 0.5);
      temp = txfsina * len;
      sy = (temp < 0) ? y + (int) (temp - 0.5) : y + (int) (temp + 0.5);
   }
   else {
      sx = x;
      sy = y;
   }

   *flag1 = TRUE;

   for (ii = 0; ii < n; ii++) {
      ichar = (short) a[ii];
      if (txfcptr[ichar] >= 0) {
         ztxtchr(i, nl, ns, ichar, sx, sy, &flag2);
         len = zfntlchr(ichar);
         temp = txfcosa * (len + 1.0);
         sx = (temp < 0) ? sx + (short) (temp-0.5) : sx + (short) (temp+0.5);
         temp = txfsina * (len + 1.0);
         sy = (temp < 0) ? sy - (short) (temp-0.5) : sy - (short) (temp+0.5);
         if (flag2 == FALSE)
            *flag1 = FALSE;
      }
   }
   return (1);
}


/******************************************************************************/
/*                                                                            */
/*    Special subroutine to draw characters                                   */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    call txtchr(i, nl, ns, ichar, dx, dy, flag1)                            */
/*       byte i(ns,nl)                                                        */
/*       integer*2 nl, ns, ichar, dx, dy                                      */
/*       logical flag1                                                        */
/*                                                                            */
/*    C:                                                                      */
/*    ztxtchr(i, nl, ns, ichar, dx, dy, flag1);                               */
/*       unsigned char i[nl][ns];                                             */
/*       short nl, ns, ichar, dx, dy;                                         */
/*       int *flag1;                                                          */
/*                                                                            */
/******************************************************************************/

/* int FTN_NAME2(txtchr, TXTCHR)(i, nl, ns, ichar, dx, dy, flag1) */
   /** no values returned -- TLT059 **/
void FTN_NAME2(txtchr, TXTCHR) (i, nl, ns, ichar, dx, dy, flag1)
unsigned char *i;
short *nl, *ns, *ichar, *dx, *dy;
int   *flag1;
{
/*  return (ztxtchr(i, *nl, *ns, *ichar, *dx, *dy, flag1)); */
    /** no values returned -- TLT059 **/
  ztxtchr(i, *nl, *ns, *ichar, *dx, *dy, flag1);
}

/* int ztxtchr(i, nl, ns, ichar, dx, dy, flag1) */ 
   /** incorrect type definition -- no values returned -- TLT059 **/ 
ztxtchr(i, nl, ns, ichar, dx, dy, flag1)
unsigned char *i;
short nl, ns, ichar, dx, dy;
int   *flag1;
{
   int   px, py, j, ndraws, x, y, k, flag2;
   float ddx, ddy, temp;

   if (txfclen[ichar] > 0) {
      *flag1 = TRUE;
      ndraws = 0;
      for (j = txfcptr[ichar]; j <= txfcptr[ichar] + txfclen[ichar]; j++) {
         ddx = txfscale * txfhite * txfxvec[j];
         ddy = txfhite * txfyvec[j];
         temp = txfcosa * ddx - txfsina * ddy;
         px = (temp < 0) ? dx + (int) (temp - 0.5) : dx + (int) (temp + 0.5);
         temp = txfsina * ddx + txfcosa * ddy;
         py = (temp < 0) ? dy - (int) (temp - 0.5) : dy - (int) (temp + 0.5);

         if (txfmove[j] == 0) {
            x = px;
            y = py;
         }
         else {
            ztxvector(i, nl, ns, txfcolor, x, y, px, py, &flag2);
            if (flag2 == FALSE)
               flag1 = FALSE;
            x = px;
            y = py;
         }
      }
   }
}


/******************************************************************************/
/*                                                                            */
/*    Special subroutine that draws a vector into the IMAGE array             */
/*                                                                            */
/*    FORTRAN:                                                                */
/*    call txvector(i, nl, ns, v, x1, y1, x2, y2, flag)                       */
/*       byte i(ns, nl)                                                       */
/*       integer*2 nl, ns                                                     */
/*       byte v                                                               */
/*       integer*2 x1, y1, x2, y2                                             */
/*       logical flag                                                         */
/*                                                                            */
/*    C:                                                                      */
/*    ztxvector(i, nl, ns, v, x1, y1, x2, y2, flag);                          */
/*       unsigned char i[nl][ns];                                             */
/*       short nl, ns;                                                        */
/*       unsigned char v;                                                     */
/*       short x1, y1, x2, y2;                                                */
/*       int *flag;                                                           */
/*                                                                            */
/*    Parameter  Description                                                  */
/*        i      Image array into which text is drawn                         */
/*        v      Pixel value (dn)                                             */
/*        x1     Starting X coordinate                                        */
/*        y1     Starting Y coordinate                                        */
/*        x2     Ending X coordinate                                          */
/*        y2     Ending Y coordinate                                          */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txvector, TXVECTOR) (i, nl, ns, v, x1, y1, x2, y2, flag)
unsigned char *i;
short *nl, *ns;
unsigned char *v;
short *x1, *y1, *x2, *y2;
int *flag;
{
   return (ztxvector(i, *nl, *ns, *v, *x1, *y1, *x2, *y2, flag));
}

int ztxvector(i, nl, ns, v, x1, y1, x2, y2, flag)
unsigned char *i;
short nl, ns;
unsigned char v;
short x1, y1, x2, y2;
int *flag;
{
   int   ii, x, y, idx, idy, isx, isy, id, temp;
   int   xw, yw, incx, incy, interchange;
 
   *flag = TRUE;

/* clip vector to access window */

   if (!ztxvisible(nl, ns, x1, y1, x2, y2)) {
      *flag = FALSE;
      return;
   }

   idx = x2 - x1;
   idy = y2 - y1;
   isx = SIGN(idx);
   isy = SIGN(idy);
   idx = abs(idx);
   idy = abs(idy);

   if (idx > idy)
      interchange = FALSE;
   else {
      temp = idx;
      idx = idy;
      idy = temp;
      interchange = TRUE;
   }

   id = 2 * idy - idx;
   incx = idx * (-2);
   incy = idy * 2;

   xw = x1;
   yw = y1;

   for (ii = 0; ii < idx; ii++) {
      if (xw < 1)
         *flag = FALSE;
      else if (yw < 1)
         *flag = FALSE;
      else if (xw > ns)
         *flag = FALSE;
      else if (yw > nl)
         *flag = FALSE;
      else
         *(i + ((yw-1) * ns) + (xw-1)) = v;

      while (id >= 0) {
         if (interchange)
            xw += isx;
         else
            yw += isy;
         id = id + incx;
      }

      if (interchange)
         yw += isy;
      else
         xw += isx;
      id += incy;
   }
}


/******************************************************************************/
/*                                                                            */
/*    FORTRAN:                                                                */
/*    visible = txvisible(nl, ns, x1, y1, x2, y2)                             */
/*       integer nl, ns, x1, y1, x2, y2                                       */
/*                                                                            */
/*    C:                                                                      */
/*    visible = ztxvisible(nl, ns, x1, y1, x2, y2);                           */
/*       int nl, ns, x1, y1, x2, y2;                                          */
/*                                                                            */
/*                                                                            */
/******************************************************************************/

int FTN_NAME2(txvisible, TXVISIBLE) (nl, ns, x1, y1, x2, y2)
int *nl, *ns, *x1, *y1, *x2, *y2;
{
   return (ztxvisible(*nl, *ns, *x1, *y1, *x2, *y2));
}

int ztxvisible(nl, ns, x1, y1, x2, y2)
int nl, ns, x1, y1, x2, y2;
{
   int code1=0, code2=0, visible=0;

   if (x1 < 1)
      code1 |= 0x01;
   if (y1 < 1)
      code1 |= 0x02;
   if (x1 > ns)
      code1 |= 0x04;
   if (y1 > nl)
      code1 |= 0x08;

   if (x2 < 1)
      code2 |= 0x01;
   if (y2 < 1)
      code2 |= 0x02;
   if (x2 > ns)
      code2 |= 0x04;
   if (y2 > nl)
      code2 |= 0x08;

   if (code1 == 0 || code2 == 0 || !(code1 & code2))
      visible = 1;

   return (visible);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create txtsubs.imake
#define SUBROUTINE txtsubs

#define MODULE_LIST txtsubs.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create ttxtsubs.c
#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"

#define TEXT_HEIGHT		25
#define TEXT_LENGTH		79
#define DN			35

/*  Program to test subroutines in TXTSUBS.                                   */

main44()
{
   int   lc, flag, def, status, temp, cnt;
   short char_height, line, sample, text_loc, dn;
   short text_height, text_length, lth, msg_len, font;
   char  image_block[TEXT_LENGTH*TEXT_HEIGHT], buf[256], out_msg[16];
   float scale, angle;

   zvparm("FONT", &temp, &cnt, &def, 1, 1);
   font = (short) temp;
   zvparm("MSG", out_msg, &cnt, &def, 1, 15);
   zvparm("HEIGHT", &temp, &cnt, &def, 1, 1);
   char_height = (short) temp;
   zvparm("SCALE", &scale, &cnt, &def, 1, 1);
   zvparm("ANGLE", &angle, &cnt, &def, 1, 1);
   zvparm("X", &temp, &cnt, &def, 1, 1);
   sample = (short) temp;
   zvparm("Y", &temp, &cnt, &def, 1, 1);
   line = (short) temp;
   zvparm("LOC", &temp, &cnt, &def, 1, 1);
   text_loc = (short) temp;
   msg_len = (short) strlen(out_msg);

   text_height = TEXT_HEIGHT;
   text_length = TEXT_LENGTH;
   dn = DN;

   for (lc=0; lc < text_length*text_height; lc++)
      image_block[lc] = 32;

   zvmessage("C Interface:", "");
   status = ztxtfont(font);
   if (status != 1) {
      sprintf(buf, "Error (%d) reading font file %03d.FON.", status, font);
      zvmessage(buf, "");
   }
   if (status == 1) {
      status = ztxtsize(char_height, scale);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text size.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtrotate(angle);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text angle.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtcolor(dn);
      if (status != 1) {
         sprintf(buf, "Error (%d) setting text color.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxtlength(&lth, msg_len, out_msg);
      if (status != 1) {
         sprintf(buf, "Error (%d) getting length of text string.", status);
         zvmessage(buf, "");
      }
   }
   if (status == 1) {
      status = ztxttext(image_block, text_height, text_length, sample, line,
           text_loc, msg_len, out_msg, &flag);
      if (status != 1) {
         sprintf(buf, "Error (%d) writing text to image array.", status);
         zvmessage(buf, "");
      }
   }

   if (status == 1) {
      for (lc=0; lc < text_height; lc++) {
         memcpy(buf, &image_block[text_length*lc], text_length);
         buf[text_length] = '\0';
         zvmessage(buf, "");
      }
   }

   FTN_NAME(ttxtsubsf)(&text_height, &text_length, &char_height, &sample,
        &line, &scale, &dn, &angle, &text_loc, &font, out_msg, &msg_len,
        image_block);

   exit(0);
}
$!-----------------------------------------------------------------------------
$ create ttxtsubsf.f
C FORTRAN subroutine to test FORTRAN bridge to subroutines in TXTSUBS

      SUBROUTINE ttxtsubsf(text_height, text_length, char_height,
     &     sample, line, scale, dn, angle, text_loc, font, out_msg,
     &     cnt, image_block)

      integer*2 text_height, text_length, char_height
      integer*2 sample, line, text_loc, font, cnt, dn
      real scale, angle
      byte out_msg(cnt), image_block(text_length, text_height)

      integer i, j, lth, flag
      character*80 buf
      logical status, txtfont, txtsize, txtrotate, txtcolor
      logical txtlength, txttext

      do 10 i = 1, text_height
         do 10 j = 1, text_length
            image_block(j, i) = 32
   10 continue

      call xvmessage('Fortran Interface:', ' ')
      status = txtfont(font)
      if (status .ne. 1) then
         write(buf, 101) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtsize(char_height, scale)
      if (status .ne. 1) then
         write(buf, 102) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtrotate(angle)
      if (status .ne. 1) then
         write(buf, 103) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtcolor(dn)
      if (status .ne. 1) then
         write(buf, 104) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txtlength(lth, cnt, out_msg)
      if (status .ne. 1) then
         write(buf, 105) status
         call xvmessage(buf, ' ')
         goto 999
      endif
      status = txttext(image_block, text_height, text_length,
     &     sample, line, text_loc, cnt, out_msg, flag)
      if (status .ne. 1) then
         write(buf, 106) status
         call xvmessage(buf, ' ')
         goto 999
      endif

      do 20 i = 1, text_height
         write(buf, 100) (image_block(j, i), j = 1, text_length)
         call xvmessage(buf, ' ')
   20 continue

  100 format(79a1)
  101 format('Error (', i1, ') reading font file.')
  102 format('Error (', i1, ') setting text size.')
  103 format('Error (', i1, ') setting text angle.')
  104 format('Error (', i1, ') setting text color.')
  105 format('Error (', i1, ') getting length of text string.')
  106 format('Error (', i1, ') writing text to image array.')

  999 continue
      return
      end
$!-----------------------------------------------------------------------------
$ create ttxtsubs.imake
/*  Imake file for test of VICAR subroutine TXTSUBS */

#define PROGRAM ttxtsubs

#define MODULE_LIST ttxtsubs.c ttxtsubsf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define P2_SUBLIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create ttxtsubs.pdf
process
parm font	type=integer		default=4
parm msg	type=(string,10)	default="Hello"
parm height	type=integer		default=15
parm scale	type=real		default=1.25
parm angle	type=real		default=0
parm x		type=integer		default=1	valid=(1:80)
parm y		type=integer		default=20	valid=(1:25)
parm loc	type=integer		default=1
end-proc
.end
.end
$!-----------------------------------------------------------------------------
$ create tsttxtsub.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
ttxtsubs
ttxtsubs 106 FFFFFF
ttxtsubs 6 Good
ttxtsubs 7 Bye
ttxtsubs 7 Bye height=15 scale=1.25 angle=180 x=70 y=9
ttxtsubs 7 Bye height=15 scale=0.8 angle=90 x=40 y=25
ttxtsubs 7 Bye height=15 scale=0.8 angle=-90 x=40 y=1
ttxtsubs 4 Testing height=10 scale=1.25 angle=0.0 x=1 y=20 loc=1
ttxtsubs 4 Testing height=10 scale=2.0 angle=0.0 x=40 y=20 loc=2
ttxtsubs 4 Testing height=15 scale=1.0 angle=0.0 x=70 y=20 loc=3
ttxtsubs 999
ttxtsubs 4 Hello height=15 scale=1.0 angle=270
ttxtsubs 4 Hello height=10 scale=0.7 angle=45 x=20 y=25
ttxtsubs 4 Hello height=-2
ttxtsubs 4 Hello height=15 scale=-2.0
ttxtsubs 3 "Hi There" height=10 scale=1.0
ttxtsubs 3 Done height=15 scale=1.0
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create txtsubs.hlp
1 TXTSUBS

  LOGICAL FUNCTION TXTCOLOR(C)
  int ztxtcolor(c)
      Set the color value c used to write text.

      FORTRAN:
      status = txtcolor(c)
         integer*2 c

      C:
      status = ztxtcolor(c)
         short c;

      Parameter  Description
          c      Color value (DN value)

      Returned
       Status    Description
          0      Function not implemented
          1      Normal return (logical TRUE)
          2      Illegal color value


  LOGICAL FUNCTION TXTFONT(F)
  int ztxtfont(f)
      Reads font description file into internal font table

      FORTRAN:
      status = txtfont(f)
         integer*2 f

      C:
      status = ztxtfont(f)
         short f;

      Parameter  Description
          f      Font type

      Returned
       Status    Description
          0      Function not implemented
          1      Normal return (logical TRUE)
          2      Illegal font type
          4      Font table too small
          6      End-of-file on font description file


  LOGICAL FUNCTION TXTLENGTH(L, N, A)
  int ztxtlength(l, n, a)
      Returns length of character string in pixels

      FORTRAN:
      status = txtlength(l, n, a)
         integer*2 l, n
         byte a(n)

      C:
      status = ztxtlength(l, n, a)
         short *l, n;
         unsigned char a[n];

      Parameter  Description
          l      returned length of string
          n      Number of characters in string
          a      Array of characters

      Returned
       Status    Description
          0      Function not implemented
          1      Normal return (logical TRUE)
          2      Illegal character count


  INTEGER FUNCTION FNTLSTR(n, str)
  int zfntlstr(n, str)
      Special function to determine the length of a string

      FORTRAN:
      length = fntlstr(n, str)
         integer*2 n
         byte str(n)

      C:
      length = zfntlstr(n, str);
         short n;
         unsigned char str[n];


  INTEGER FUNCTION FNTLCHR(ICHAR)
  int zfntlchr(ichar)
      Special function to find the length of a character

      FORTRAN:
      length = fntlchr(ichar)
         integer*2 ichar

      C:
      length = zfntlchr(ichar);
         short ichar;


  LOGICAL FUNCTION TXTROTATE(A)
  int ztxtrotate(a)
      Sets angle above or below the X-axis text is to be written

      FORTRAN:
      status = txtrotate(a)
         real a

      C:
      status = ztxtrotate(a);
         float a;
                                                          |+90
                                                          |
                                                          |
      Parameter  Description                              |
          a      Angle to rotate text                     |
                                            +-180 ------------------ 0
                                                          |
      Returned                                            |
       Status    Description                              |
         0       Function not implemented                 |
         1       Normal return (logical TRUE)             |-90
         2       Illegal angle


  LOGICAL FUNCTION TXTSIZE(H, S)
  int ztxtsize(h, s)
      Sets the height and horizontal scale factor for text

      FORTRAN:
      status = txtsize(h, s)
         integer*2 h
         real s

      C:
      status = ztxtsize(h, s);
         short h;
         float s;

      Parameter  Description
          h      height of characters (in lines)
          s      horizontal scale factor (width/height)

      Returned
       Status    Description
          0      Function not implemented
          1      Normal return (logical TRUE)
          2      Illegal height
          4      Illegal horizontal scale factor


  LOGICAL FUNCTION TXTTEXT(I, NL, NS, X, Y, LOC, N, A, FLAG1)
  int ztxttext(i, nl, ns, x, y, loc, n, a, flag1)
      Write a line of text to an image

      FORTRAN:
      status = txttext(i, nl, ns, x, y, loc, n, a, flag1)
         byte i(ns, nl)
         integer*2 nl, ns, x, y, loc, n
         byte a(n)
         logical flag1

      C:
      status = ztxttext(i, nl, ns, x, y, loc, n, a, flag1)
         unsigned char i[nl][ns];
         short nl, ns, x, y, loc, n;
         unsigned char a[n];
         int *flag1;

      Parameter  Description
          i      name of array into which text is written
          x      x-coordinate of text position
          y      y-coordinate of text position
         loc     orientation of text about (X,Y)
                 loc value  Description
                      1     x,y is lower left corner of text
                      2     x,y is bottom center of text
                      3     x,y is lower right corner of text
          n      number of characters in text
          a      text string

      Returned
       Status    Description
          0      Function not implemented
          1      Normal return  (logical TRUE)
          8      Coordinates are outside access window
         10      Illegal LOC value
         12      Illegal character count


  SUBROUTINE TXTCHR(I, NL, NS, ICHAR, DX, DY, FLAG1)
  ztxtchr(i, nl, ns, ichar, dx, dy, flag1)
      Special subroutine to draw characters

      FORTRAN:
      call txtchr(i, nl, ns, ichar, dx, dy, flag1)
         byte i(ns,nl)
         integer*2 nl, ns, ichar, dx, dy
         logical flag1

      C:
      ztxtchr(i, nl, ns, ichar, dx, dy, flag1);
         unsigned char i[nl][ns];
         short nl, ns, ichar, dx, dy;
         int *flag1;


  SUBROUTINE TXVECTOR(I, NL, NS, V, X1, Y1, X2, Y2, FLAG)
  int ztxvector(i, nl, ns, v, x1, y1, x2, y2, flag)
      Special subroutine that draws a vector into the IMAGE array

      FORTRAN:
      call txvector(i, nl, ns, v, x1, y1, x2, y2, flag)
         byte i(ns, nl)
         integer*2 nl, ns
         byte v
         integer*2 x1, y1, x2, y2
         logical flag

      C:
      ztxvector(i, nl, ns, v, x1, y1, x2, y2, flag);
         unsigned char i[nl][ns];
         short nl, ns;
         unsigned char v;
         short x1, y1, x2, y2;
         int *flag;

      Parameter  Description
          i      Image array into which text is drawn
          v      Pixel value (dn)
          x1     Starting X coordinate
          y1     Starting Y coordinate
          x2     Ending X coordinate
          y2     Ending Y coordinate


  LOGICAL FUNCTION TXVISIBLE(NL, NS, X1, Y1, X2, Y2)
  int ztxvisible(nl, ns, x1, y1, x2, y2)
      Special function to determine if character will be visible in the image
      array.

      FORTRAN:
      visible = txvisible(nl, ns, x1, y1, x2, y2)
         integer nl, ns, x1, y1, x2, y2

      C:
      visible = ztxvisible(nl, ns, x1, y1, x2, y2);
         int nl, ns, x1, y1, x2, y2;


2 History

  Original Programmer:  Kevin Hussey  July 15, 1985
  Cognizant Programmer: Allan Runkle  July 15, 1985
  Source Language: Fortran
  Conversion Language:  C
  Ported to UNIX and converted to C:  Paul Bartholomew  January 8, 1993
  For details about specific modifications (if any), see the header of the
  source file.

2 Operation


$ Return
$!#############################################################################
