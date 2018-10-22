/*  This program replaces the values in an array by means of a table    */
/*  lookup.  This program is used on the UNIX system because the        */
/*  original subroutine was written in assembly language and is not     */
/*  compilable in UNIX.                                                 */    

/*  Original Programmer: Damon D. Knight  1993-08-11                    */
/*  Current Cog. Progr.: Damon D. Knight  1993-08-11                    */

#include "xvmaininc.h"
#include "ftnbridge.h"

static void ztbl(unsigned char *buf, unsigned char *tab, int n );

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(tbl, TBL) ( buf, tab, n )
unsigned char *buf, *tab;
int *n;
{
     ztbl(buf, tab, *n);
     return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

static void ztbl(unsigned char *buf, unsigned char *tab, int n )
{
     int i, val;
     for (i=0; i<n; i++)
     {
          val= *buf;
          *buf = *(tab+val);
          buf++;
     }
}
