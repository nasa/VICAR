$!****************************************************************************
$!
$! Build proc for MIPL module prnt
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:18
$!
$! Execute by entering:		$ @prnt
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
$ write sys$output "*** module prnt ***"
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
$ write sys$output "Invalid argument given to prnt.com file -- ", primary
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
$   if F$SEARCH("prnt.imake") .nes. ""
$   then
$      vimake prnt
$      purge prnt.bld
$   else
$      if F$SEARCH("prnt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake prnt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @prnt.bld "STD"
$   else
$      @prnt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create prnt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack prnt.com -mixed -
	-s prnt.c -
	-i prnt.imake -
	-t tprnt.f tzprnt.c tprnt.imake tprnt.pdf tstprnt.pdf -
	-o prnt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create prnt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create prnt.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY prnt

   To Create the build file give the command:

	$ vimake prnt                     (VMS)
   or
	% vimake prnt                     (Unix)


*************************************************************************/

#define SUBROUTINE prnt

#define MODULE_LIST prnt.c

#define FTN_STRING
#define P1_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tprnt.f
      INCLUDE 'VICMAIN_FOR'

C TEST SUBROUTINE "PRNT" -- ALL DCODES, 2 N'S (LT/GT line SIZE)
C
      SUBROUTINE MAIN44
      BYTE      ABUF(256)
      INTEGER*2 HBUF(120)
      INTEGER*4 IBUF(30)
      REAL*4    RBUF(30),R
      REAL*8    DBUF(30),D
      COMPLEX   ZBUF(30) 
      INTEGER   CODE(7) / 0,  1,  2,  4,  7,  8, 10/
      INTEGER   SIZ(7)  /32, 30, 20, 10, 10, 10, 10/
      CHARACTER*8 TITL(7)/' DUMP   ',' BYTE   ',' HALFWD.',' FULLWD.',
     : ' REAL*4.',' REAL*8.','COMPLEX.'/
      CHARACTER*8 TITLE/'CHARACT.'/
      CHARACTER*160 LONGSTR
C
      DO I=1,120
	 ABUF(I)  = MOD(I-1,26) + 65
	 HBUF(I)  = -16705 + (I - 1)*200
      ENDDO
      abuf(1)=0
      abuf(2)=127
      abuf(3)=-1   ! Should be interperted as 255.

      DO I=1,30
         IBUF(I) = 1000*(I-10)
	 RBUF(I) = 1.E3*I
	 DBUF(I) = 1.D4*I
         R       = I - 1
         ZBUF(I) = CMPLX(R,0.0)
      ENDDO
      ibuf(1)= -(10**9)
C
      CALL XVMESSAGE('Test #1:  WITH TITLES',' ')
      DO D=1,7     !  TRY VARIOUS DCODES
	 DO NN=1,3,2         !  LINE LENGTH
	    N = NN*SIZ(D)
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, TITL(D))
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, TITL(D))
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, TITL(D))
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, TITL(D))
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, TITL(D))
	    CALL XVMESSAGE(' ',' ')
	 ENDDO
      ENDDO
      CALL XVMESSAGE('Test #2:  REPEAT WITHOUT TITLES',' ')

      DO D=1,7     !  TRY VARIOUS DCODES
	 DO NN=1,3,2         !  LINE LENGTH
	    N = NN*SIZ(D)
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, ' ')
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, ' ')
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, ' ')
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, ' ')
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, ' ')
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, ' ')
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, ' ')
	    CALL XVMESSAGE(' ',' ')
	 ENDDO
      ENDDO

      CALL XVMESSAGE('Test #3:  REPEAT WITH TITLES and N=1',' ')
      DO D=1,7     !  TRY VARIOUS DCODES
	    N = 1
	    IF (D.EQ.1)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.2)            CALL PRNT(CODE(D), N, ABUF, TITL(D))
	    IF (D.EQ.3)            CALL PRNT(CODE(D), N, HBUF, TITL(D))
	    IF (D.EQ.4)            CALL PRNT(CODE(D), N, IBUF, TITL(D))
	    IF (D.EQ.5)            CALL PRNT(CODE(D), N, RBUF, TITL(D))
	    IF (D.EQ.6)            CALL PRNT(CODE(D), N, DBUF, TITL(D))
	    IF (D.EQ.7)            CALL PRNT(CODE(D), N, ZBUF, TITL(D))
	    CALL XVMESSAGE(' ',' ')
      ENDDO


c  Repeat test cases in C to test C interface: zprnt

      CALL XVMESSAGE('REPEAT ABOVE TESTS FROM C LANGUAGE',' ')
      call tzprnt(CODE,SIZ,ABUF,HBUF,IBUF,RBUF,DBUF,ZBUF)

C LONG TITLE
      LONGSTR='123456789012345678901234567890123456789012345678901234567
     C890123456789072345678901234567890123456789012345678901234567890123
     C4567890123456789012345678907234567890'

      CALL XVMESSAGE(
     .  'The following PRNT output should all fit on one line', ' ')
      CALL PRNT(4, 1, IBUF, LONGSTR(1:110) )
      CALL XVMESSAGE(
     . 'Next title should be truncated to 132 chars. Data on 2nd line.',
     .  ' ')
      CALL PRNT(4, 1, IBUF, LONGSTR )

C     ....ASCII string

      CALL MVCL(LONGSTR, ABUF, 150)
      CALL PRNT(99,12,ABUF,' This is a short ASCII string:.')
      CALL PRNT(99,150,ABUF,' This is a longer ASCII string:.')

      CALL XVMESSAGE(
     . 'Test cases of a single real value of various magnitudes.', ' ')
      R = .0000123123123
      DO I = 1,15
         CALL PRNT(7,1,R,' R =')
         CALL PRNT(7,1,-R,'-R =')
         R = R*10.
      END DO
      D = .0000123123123D0
      DO I = 1,15
         CALL PRNT(8,1,D,' D =')
         CALL PRNT(8,1,-D,'-D =')
         D = D*10.
      END DO
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzprnt.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzprnt)(code,siz,abuf,hbuf,ibuf,rbuf,dbuf,zbuf) 

  int code[],siz[];
  unsigned char *abuf;
  short         *hbuf;
  int           *ibuf;
  float         *rbuf;
  double        *dbuf;
  float         *zbuf;
{
      int d, n, nn;

/*  ==================================================================  */



      zvmessage("Test #1:  with titles","");
      for ( d=0; d<7; d++ )
      {
        for ( nn=0; nn<3; nn=nn+2 )
        {
	    n = (nn+1)*siz[d];
	    if (d == 0)            zprnt(code[d], n, abuf, "DUMP ");
	    if (d == 1)            zprnt(code[d], n, abuf, "BYTE ");
	    if (d == 2)            zprnt(code[d], n, hbuf, "HALFWD ");
	    if (d == 3)            zprnt(code[d], n, ibuf, "FULLWD ");
	    if (d == 4)            zprnt(code[d], n, rbuf, "REAL*4 ");
	    if (d == 5)            zprnt(code[d], n, dbuf, "REAL*8 ");
	    if (d == 6)            zprnt(code[d], n, zbuf, "COMPLEX ");
	    zvmessage("","");
         }
      }
      zvmessage("Test #2:  repeat without titles","");

      for ( d=0; d<7; d++ )
      {
        for ( nn=0; nn<3; nn=nn+2 )
        {
	    n = (nn+1)*siz[d];
	    if (d == 0)            zprnt(code[d], n, abuf, "");
	    if (d == 1)            zprnt(code[d], n, abuf, "");
	    if (d == 2)            zprnt(code[d], n, hbuf, "");
	    if (d == 3)            zprnt(code[d], n, ibuf, "");
	    if (d == 4)            zprnt(code[d], n, rbuf, "");
	    if (d == 5)            zprnt(code[d], n, dbuf, "");
	    if (d == 6)            zprnt(code[d], n, zbuf, "");
	    zvmessage("","");
         }
      }

      zvmessage("Test #3:  repeat with titles AND n=1","");
      for ( d=0; d<7; d++ )
      {
	    n = 1;
	    if (d == 0)            zprnt(code[d], n, abuf, "DUMP ");
	    if (d == 1)            zprnt(code[d], n, abuf, "BYTE ");
	    if (d == 2)            zprnt(code[d], n, hbuf, "HALFWD ");
	    if (d == 3)            zprnt(code[d], n, ibuf, "FULLWD ");
	    if (d == 4)            zprnt(code[d], n, rbuf, "REAL*4 ");
	    if (d == 5)            zprnt(code[d], n, dbuf, "REAL*8 ");
	    if (d == 6)            zprnt(code[d], n, zbuf, "COMPLEX ");
	    zvmessage("","");
      }
}
$!-----------------------------------------------------------------------------
$ create tprnt.imake
/* Imake file for Test of VICAR subroutine prnt */

#define PROGRAM tprnt

#define MODULE_LIST tprnt.f tzprnt.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create tprnt.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstprnt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tprnt
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create prnt.hlp
1 PRNT

 Print a string (TITLE) followed by an array BUF of N elements.  
 The data format (DCODE) of BUF may  be one of the following:

        0=HEX        1=BYTE        2=INTEGER*2     4=INTEGER*4
        7=REAL*4     8=REAL*8     10=COMPLEX*8    99=ASCII string


  FORTRAN Calling Sequence:  CALL PRNT( DCODE, N, BUF ,TITLE)
  C Calling Sequence:        zprnt( dcode, n, buf ,title);

  Arguments: 

   DCODE  -  input, integer  -  data format code
   N  -  input, integer  -  number of elements to print
   BUF  -  input array, any valid data type
   TITLE - input, CHARACTER variable or quoted string for FORTRAN.
                  null-terminated string for C.  If no title is desired,
                  a ' ' may be passed from FORTRAN, or a "" may be passed
                  from C.


2 History

  Original Programmer:   Gary Yagi, 13 January 1977
  Conversion to VAX:     L. W. Kamp
  Conversion to Vicar2:  J. R. Stagner
  Cognizant Engineer:    Steve Pohorsky
  Source Language:       C
  Revisions:
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

2 OPERATION

  PRNT formats the output into lines of up to 132 characters and
  then calls zvmessage.  Here are some details:

  1) If there is a title and only one value to be printed (and the title
     is <= 100 characters), the title and value are printed on the same
     line.  Otherwise, assuming there is a title, it is printed (up to 132
     characters) on the first line and data from BUF starts on the second
     line of output.
  2) If there is more than one value (N>1), each line of data starts in
     column 11.  (Column 11 may be blank due to the single space used
     to separate values for DCODE = 1,4,7,8,10 or due to leading blanks
     in the output of the numeric values.)
  3) For DCODE=0, the HEX values on a line are printed first, with a
     space separating each eight HEX digits (2 HEX digits per unsigned byte).
     The HEX is followed by three blanks and the corresponding ASCII
     representation of each byte.  Non-printing values are handled as
     follows: unsigned bytes in the range from 127 to 255 are displayed
     as a '#' and unsigned bytes in the range from 0 to 31 are displayed
     as a '|'.
  4) For DCODE=2, PRNT allows six characters per value.  This provides
     spacing between values unless the values are in the range -10000 to
     -32768.  In the latter case, the minus sign also servers as the
     separator between values.  This allows 20 values per line.
  5) For DCODE=4, if N>1, PRNT allows 10 characters per value plus a separating
     space.  This accomodates all 32-bit signed integers except values in 
     the range -(2**31), i.e. -2,147,483,648, through -1,000,000,000.
     Ten '*'s are displayed for values in this range.  For N=1, PRNT will
     use extra characters if necessary to display any INTEGER*4 value.
  6) For DCODE=7 or 8, fixed point is used if the number of values to
     be printed is 1 and the absolute value of the number is in the
     range .0001 to 9999999.9 inclusive.  In this case, eight significant
     digits are printed.
     Otherwise, the format " %10.3E" is used.  The latter
     provides 4 significant digits since there is 1 digit to the left of the
     decimal point. 
  7) For DCODE=10, each complex value is displayed as a pair of real values.
     (The input format expected in BUF is also a pair of reals.)
  8) For DCODE=99, the ASCII data is expected in a BYTE array or equivalent
     form.  The data may not be in a FORTRAN quoted string or a CHARACTER 
     variable, which may be passed directly to XVMESSAGE.  DCODE 99 is
     maintained to provide backwards compatability.

2 ARGUMENTS

 DCODE (input): Data format code, as follows:

               DCODE         Array Type        Elements per line
               -----         ----------        -----------------
                 0       Hexadecimal and ASCII       32
                 1       Byte                        30
                 2       Integer*2                   20
                 4       Integer*4                   10
                 7       Real*4                      10
                 8       Real*8                      10
                10       Complex*8                    5
                99       ASCII string              up to 120

  N (input): number of elements to be printed.  For DCODE 0, 1, and 99,
             this is the number of bytes.

  BUF (input): this specifies the first element to be printed. The
               number of bytes per element is determined by DCODE.

   TITLE - input, CHARACTER variable or quoted string for FORTRAN.
                  null-terminated string for C.  If no title is desired,
                  a ' ' may be passed from FORTRAN, or a "" may be passed
                  from C.  Since PRNT used to expect a string
                  terminated by a period ("."), PRNT will ignore a period
                  at the end of the string (for backwards compatability).
$ Return
$!#############################################################################
