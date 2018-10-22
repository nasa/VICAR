/*---------------------------  prnt     ------------------------
 Print a string (TITLE) followed by an array BUF of N elements.  
 The data format (DCODE) of BUF may  be one of the following:

        0=HEX        1=BYTE        2=INTEGER*2     4=INTEGER*4
        7=REAL*4     8=REAL*8     10=COMPLEX*8    99=ASCII string

 REVISION HISTORY:                                          
   93-3-24  ..SP....  Added more significant digits (total=8) if 
                      only one value to print for float or double and absolute 
                      value between .0001 and 9999999.9.
   92-4-20  ..SP....  Made portable for UNIX - converted from Fortran
                      to C to handle CHARACTER titles, made TITLE a
                      required parameter, changed to use sprintf
                      instead of OUTCON, changed to use zvmessage
                      instead of QPRINT (pulled in code to handle
                      non-printable characters), added features from
                      old PRNT2: a) print title and value on same line
                      if room, b) if only one value to print for float or
                      double and absolute value between .1 and 9999.9999, 
                      use fixed point format.  (If more than one value to
                      print, exponential format is used to insure uniformity.)
   89-10-24 ..GMY...  Add ASCII string data format (DCODE=99)
   85-2-28 ...JRS...  ADDED COMPLEX DATA TYPE
   85-4-04 ...JRS...  CHANGED DCODE FROM 9 TO 10 FOR COMPLEX DATA TYPE 
                      TO BE CONSISTENT WITH OTHER VICAR ROUTINES
   84-9-5  ...LWK...  fix bugs in TITLE & # elements/line,
                      reduce hwd field 
   83-6-2  ...LWK...  increase halfword field to 6 chars
   83-4-18 ...LWK...  fixed 0 DCODE negative byte data  
   83-3-15 ...LWK...  fixed halfword OUTCON call 
   83-3-1  ...LWK...  fixed DCODE=0  

--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "prnt.h"

/*  Values of dcode - format of data in BUF buffer.  */

#define HEX    0
#define BYTE   1
#define HALF   2
#define FULL   4
#define REAL4  7
#define DOUB   8
#define COMP  10
#define ASCI  99

#define LINESIZ 132

static char blanks10[]= "          ";
static char pbuf[LINESIZ+1];
static char valstr[15];
void prnthex(unsigned char *bbuf, int ns, int maxcount);
int ndleft(double d);
void prnt_catchar(unsigned char *a);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(prnt, PRNT) (int *dcode, int *n, void *buf,
					char *title, ZFORSTR_PARAM)
#if 0
int *dcode;      /* format code of data to be printed   */
int *n;          /* number of data elements to print  */
void *buf;        /* array of data elements to be printed*/
char *title;    /* string to be printed in front of data */
#endif
{
   ZFORSTR_BLOCK
   char *c_string;
   int length;

   zsfor2len(length, title, &dcode, 4, 4, 1, title);  /* 4 args for prnt   */
   c_string = (char *)calloc(1,(length+1));	     /* title is 4th arg,  */
   zsfor2c(c_string, length, title, &dcode, 4, 4, 1, title);
						     /* title is 1st string  */

   zprnt( *dcode, *n, buf, c_string );

   free (c_string);

}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zprnt(dcode, n, buf, title)

int dcode;      /* format code of data to be printed   */
int n;          /* number of data elements to print  */
void *buf;        /* array of data elements to be printed*/
char *title;    /* string to be printed in front of data */
{
 static char stars10[]= " **********";
 char        fmt[10];
 int i, ns, nchars, nc, maxcount=0, ndright;
 unsigned char *bbuf = NULL;
 char 	       *cbuf = NULL;
 short         *hbuf = NULL;
 int           *ibuf = NULL;
 float         *rbuf = NULL;
 double        *dbuf = NULL;
 float         absval;
 double        dabsval;
/*  ==================================================================  */

 switch( dcode) {
 case HEX:	bbuf = (unsigned char *)buf;
                maxcount = 32;          /*  elements per line  */
		break;
 case BYTE:	bbuf = (unsigned char *)buf;
                maxcount = 30;          /*  elements per line  */
		break;
 case HALF:	hbuf = (short *)buf;
                maxcount = 20;          /*  elements per line  */
		break;                  
 case FULL:	ibuf = (int *)buf;
                maxcount = 10;          /*  elements per line  */
		break;
 case REAL4:	rbuf = (float *)buf;
                maxcount = 10;          /*  elements per line  */
		break;
 case DOUB:	dbuf = (double *)buf;
                maxcount = 10;          /*  elements per line  */
		break;
 case COMP:	rbuf = (float *)buf;    /*  2 elements per complex value  */
                maxcount = 10;          /*  elements per line  */
		break;
 case ASCI:   	cbuf = (char *)buf;
                maxcount = 120;          /*  elements per line  */
                break;
 default:       zvmessage("*** PRNT - Illegal DCODE","");
                zabend();
                break;
 }
 nchars = strlen(title);
 if (nchars <= 0)
    strcpy(pbuf, blanks10);
 else if (nchars ==1 && *title == ' ') {       /*  if no title desired  */
    strcpy(pbuf,blanks10);
    nchars = 0;
 }
 else if (nchars > LINESIZ){
    strncpy(pbuf, title, LINESIZ);
    pbuf[LINESIZ] = '\0';
    nchars = LINESIZ;
 }
 else
    strcpy( pbuf, title);

/*  throw away trailing . if present.  This was old terminator.  */

 if (nchars > 0)
    if (pbuf[nchars-1] == '.'){
       pbuf[nchars-1] = '\0';
       nchars = nchars - 1;
    }

 ns = n;
 if (dcode == COMP)  ns = 2*n;  /*  each complex treated as 2 reals.  */

/*  If there is a title, and  */
/*  if more than one value or long title, print title on separate line  */

 if (nchars > 0)
    if (n > 1 || nchars > 110){
       zvmessage( pbuf, "");
       strcpy( pbuf, blanks10);
    }

/*  Now print the values in buf.  */

 if (dcode == HEX)  prnthex(bbuf, ns, maxcount); /*  handle HEX separately  */
 else {
 for ( i=0; i<ns; i++ ){

     switch( dcode) {
     case BYTE:  sprintf( valstr, " %3u", *bbuf++);
		 break;
     case HALF:	 sprintf( valstr, "%6d", *hbuf++);
                 break;              /* no space if -10000 to -32768)  */
     case FULL:	 sprintf( valstr, " %10d", *ibuf++);
                 nc = strlen(valstr);
                 if (nc > 11 && ns > 1) strcpy(valstr, stars10); /* overflow */
		 break;
     case REAL4: if ( ns == 1 && .0001<= fabs(*rbuf) && fabs(*rbuf)<=9999999.9){
                     absval  = fabs(*rbuf);       /*  if only 1 value & in   */
                     ndright = 8-ndleft(absval);  /*  suitable range, use %f */
			/* Format the output in a 10 character field   */
			/* consisting of eight significant digits, a decimal */
			/* point, and a space for a possible minus sign.   */
			/* ndright= number of digits to right of decimal point*/
			/* 8 (digits) = ndright + ndleft  */
                     sprintf( fmt, " %%10.%df", ndright);  
                     sprintf( valstr, fmt, *rbuf++); 
                 }
                 else				
		     sprintf( valstr, " %10.3E", *rbuf++); 
 		 break;
     case DOUB:	 if ( ns == 1 && .0001<= fabs(*dbuf) && fabs(*dbuf)<=9999999.9){
                     dabsval  = fabs(*dbuf);      /*  if only 1 value & in   */
                     ndright = 8-ndleft(dabsval); /*  suitable range, use %f */
			/* Format the output in a 10 character field   */
			/* consisting of eight significant digits, a decimal */
			/* point, and a space for a possible minus sign.   */
			/* ndright= number of digits to right of decimal point*/
			/* 8 (digits) = ndright + ndleft  */

                     sprintf( fmt, " %%10.%df", ndright);  
                     sprintf( valstr, fmt, *dbuf++); 
                 }
                 else					   /*  & in suitable  */
		     sprintf( valstr, " %10.3E", *dbuf++); /*  range, use %f. */
		 break;
     case COMP:	 sprintf( valstr, " %10.3E", *rbuf++);  /* This is 4 signif.  */
		 break;	      /*  digits since 1 digit before decimal point.  */
     case ASCI:  sprintf( valstr, "%c", *cbuf++);
                 break;
     default:    zvmessage("*** PRNT - Illegal DCODE","");
                 zabend();
                 break;
     }
     strcat( pbuf, valstr);
     if ( i+1 == ns  || (i+1)%maxcount == 0 ){
        zvmessage( pbuf, "");
        strcpy( pbuf, blanks10);
     }
  }  /*  end for loop  */
  }  /*  end of else  */


}




/************************************************************************/
/* prnthex - print ns bytes in hex and ASCII.  Assumes pbuf already	*/
/*           contains title or blanks10.  */
/************************************************************************/

void prnthex(unsigned char *bbuf, int ns, int maxcount)     
#if 0
                      /*  pbuf declared externally  */
unsigned char *bbuf;  /*  pointer to data to be printed.  */
int ns;                       /* number of bytes to print   */
int maxcount;         /*  maximum number of bytes to print per line.  */
                      /*  each byte printed once in HEX, & once in ASCII.  */
#endif

{	
   int i, isav, j;
   unsigned char *abuf;
/*  ==================================================================  */

   abuf = bbuf;		/*  save start of line values.  */
   isav = 0;

   for (i=0; i<ns; i++ ){

       if ( i%4 == 0 ) strcat(pbuf, " "); /*  space between groups of 4 bytes */

       sprintf( valstr, "%02X", *bbuf++); /*  print each byte as 2 HEX digits */
       strcat(  pbuf,  valstr);
 
       if ( i+1 == ns  || (i+1)%maxcount == 0 ){  /*  if at end of line  */
         strcat( pbuf, "   ");			/*  add 3 blanks and   */
			/*  return to beginning of line and add on the ASCII */
         for ( j=isav; j < ns  && j < maxcount+isav; j++ )
             prnt_catchar( abuf++);

         abuf = bbuf;		/*  save start of next line values.  */
         isav = i+1;

         zvmessage( pbuf, "");
         strcpy( pbuf, blanks10);
       }
     }
}
/************************************************************************/
/* prnt_catchar - add a character (and null) to end of pbuf.		*/
/*  			nonprintable characters are converted.  	*/
/************************************************************************/

void prnt_catchar(unsigned char *a)     
/* a = character to be added to pbuf.  pbuf declared externally. */
{
   unsigned char c;
/*  ==================================================================  */
   c = *a;

   if ( isprint(c) )
     strncat(pbuf, (char*) a, 1);
   else if (c >= 127)
      strcat( pbuf, "#");      /*  for 127 to 255  */
   else
      strcat( pbuf, "|");      /*  for 0 to 31.  Include tabs, etc., as  */
}				/*  | for uniform spacing.  */
/************************************************************************/
/* ndleft - special purpose routine to see how many digits to the	*/
/*  	left of decimal point in positive value <= 9999999.9		*/
/*	This routine is on the conservative side to allow for the	*/
/*	possibility of sprintf rounding up to 1.0000 etc.		*/
/************************************************************************/

int ndleft(double d)     
{
/*  ==================================================================  */

   if      ( d <= .99999999)
      return 0;
   else if ( d <= 9.9999999)
      return 1;
   else if ( d <= 99.999999)
      return 2;
   else if ( d <= 999.99999)
      return 3;
   else if ( d <= 9999.9999)
      return 4;
   else if ( d <= 99999.999)
      return 5;
   else if ( d <= 999999.99)
      return 6;
   else 
      return 7;
}
