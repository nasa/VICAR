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
