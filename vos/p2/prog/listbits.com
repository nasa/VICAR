$!****************************************************************************
$!
$! Build proc for MIPL module listbits
$! VPACK Version 1.9, Monday, December 07, 2009, 16:37:07
$!
$! Execute by entering:		$ @listbits
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module listbits ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to listbits.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("listbits.imake") .nes. ""
$   then
$      vimake listbits
$      purge listbits.bld
$   else
$      if F$SEARCH("listbits.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake listbits
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @listbits.bld "STD"
$   else
$      @listbits.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create listbits.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack listbits.com -mixed -
	-s listbits.c -
	-i listbits.imake -
	-p listbits.pdf -
	-t tstlistbits.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create listbits.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* VICAR PROGRAM LISTBITS -- lists the bits in an image area */
#include "vicmain_c"

void main44(void)
{
int i,j,count;
int iunit,sl,ss,nl,ns,nli,nsi,line,samp;
unsigned char ibuf[10000];	/* Input image line */
int space,blank=32;
unsigned char pbuf[4][8];
char zbuf[80];
 
/* Log LISTBITS startup message */
zifmessage ("LISTBITS version 02-May-94");

/* Set default error handling action */
zveaction ("SA", "");

/* Assign unit number for the file about to be read */ 
zvunit (&iunit,"INP",1,NULL);

/* Open input file using assigned unit number */
zvopen (iunit,NULL);

/* Request image size */
zvsize (&sl,&ss,&nl,&ns,&nli,&nsi);

/* Indicate whether a keyword was specified */
space  = zvptst("SPACE");

/* Loop through the file one line at a time, format and print results */
for (line=sl; line<sl+nl; line++) {

   /* Read a single line from operator specified input-image file */
   zvread(iunit,ibuf,"LINE",line,"SAMP",ss,"NSAMPS",ns, NULL);

   sprintf (zbuf, "Line=%10d", line);
   zvmessage (zbuf, "");

   for (j=0; j<8; j++){
      for (i=0; i<4; i++) {
         pbuf[i][j]= blank;
      }
   }

   for (j=0; j<sizeof(zbuf); j++) { 
      zbuf[j] = blank;
   }

   zbuf[sizeof(zbuf)-1] = '\0';

   /* Loop through the number of samples for the line being processed */
   for (i=0; i<ns; i+=4)	/* print the bits 4 bytes at a time */
       {

       /* Set 'samp' to starting sample number */
       samp = ss + i;

       /* Convert integer 'samp' to decimal ASCII and store in zbuf[0..3] */
       sprintf (zbuf, "%3d", samp);

       /* Clear zbuf 'NULL' character for next line to be printed */
       count = 3;
       zbuf[count] = ' ';

       /* Convert hexadecimal array ibuf[] into ASCII and store in zbuf[] */
       count = 4;
       zhexcon(&ibuf[i],&zbuf[4],&count);		/* hex number */

       if (i > ns-4) {			/* If on last four bytes... */
           for (j=0; j<8; j++) zbuf[j+2*(ns-i)+5] = ' '; /*blank*/
       }

       for (j=0; j<4; j++) {
	   listbyte(&ibuf[i+j],&pbuf[j][0]);
           zmve (1,8,&pbuf[j][0],&zbuf[17*j+13],1,2);
       }
       zvmessage (zbuf,""); 
       if (space) zvmessage ("0",""); 
       }
   }
zvmessage ("LISTBITS task completed","");
zvclose (iunit,NULL);
return;
}

/* LISTBYTE():
   Inspect each byte in the input buffer. Chech each bit position by checking
   magnitude of byte (modulo 128, 64, 32, 16, 8, 4, 2 & 0).  If byte value is 
   greater than or equal to checked value then reduce the value of the byte
   by the magnitude of the bit position being checked and return an ASCII '1',
   else return an ASCII '0' */ 

listbyte(ibuf,pbuf)
unsigned char *ibuf;
unsigned char *pbuf;
{
   int ival;
   unsigned char *qbuf;

   qbuf = pbuf;

   ival = *ibuf;

   /* If highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 128) {
      *qbuf=49;
      ival = ival - 128;
   } else {
      *qbuf=48;    
   }

   /* Bump output buffer pointer */
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 64) {
      *qbuf=49;
      ival = ival - 64;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 32) {
      *qbuf=49;
      ival = ival - 32;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 16) {
      *qbuf=49;
      ival = ival - 16;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 8) {
      *qbuf=49;
      ival = ival - 8;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 4) {
      *qbuf=49;
      ival = ival - 4;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 2) {
      *qbuf=49;
      ival = ival - 2;
   } else {
      *qbuf=48;    
   } 
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 1) {
      *qbuf=49;
   } else { 
      *qbuf=48;    
   }
   qbuf++;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create listbits.imake
#define PROGRAM listbits

#define MODULE_LIST listbits.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
$ Return
$!#############################################################################
$PDF_File:
$ create listbits.pdf
process help=*
PARM INP TYPE=STRING
PARM SIZE    TYPE=INTEGER COUNT=4   DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER COUNT=1   DEFAULT=1
PARM SS      TYPE=INTEGER COUNT=1   DEFAULT=1
PARM NL      TYPE=INTEGER COUNT=1   DEFAULT=0
PARM NS      TYPE=INTEGER COUNT=1   DEFAULT=0
PARM SPACE   TYPE=KEYWORD COUNT=0:1 VALID=SPACE DEFAULT=--

!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=("DN values",bits,binary)

END-PROC
.TITLE
Print the DN values of an image area in binary (ones and zeroes)
.help
PURPOSE:

LISTBITS prints the bits in an image area as strings of ones and zeroes.

EXECUTION:

Example 

	listbits  inp=pic user-parameters...

where pic is an image of arbitrary size and data format.

.page
The image area is printed line-by-line.  The data for each line is printed four
bytes at a time, displaying the starting byte, a hexadecimal representation,
and a bit representation, as in the following example:

	listbits  files.dat size=(2,1,1,25)

Line=         2
  1 0081CEE0 0 0 0 0 0 0 0 0  1 0 0 0 0 0 0 1  1 1 0 0 1 1 1 0  1 1 1 0 0 0 0 0
  5 00000000 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0
  9 0E50003D 0 0 0 0 1 1 1 0  0 1 0 1 0 0 0 0  0 0 0 0 0 0 0 0  0 0 1 1 1 1 0 1
 13 80000000 1 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0
 17 013AF600 0 0 0 0 0 0 0 1  0 0 1 1 1 0 1 0  1 1 1 1 0 1 1 0  0 0 0 0 0 0 0 0
 21 E4000087 1 1 1 0 0 1 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  1 0 0 0 0 1 1 1
 25 9B       1 0 0 1 1 0 1 1

The 'SPACE keyword may be used to introduce a blank line between each printed
line (double spacing).

.page
2  HISTORY:

	Original Programmer: Gary Yagi		January 10, 1990
	Current Cognizant Programmer: Gary Yagi

   Made portable for UNIX    Jim Turner (CRI)    30 March 1994
   Corrected tst pdf as per FR85764     (CRI)    08 March 1995

.LEVEL1
.VARIABLE INP
Input file name.
.VARIABLE SIZE
Standard vicar size field
Ex: SIZE=(sl,ss,nl,ns)
.VARIABLE SPACE
Keyword specifying double 
spacing.
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NL
INTEGER - Number of lines
.VARIABLE NS
INTEGER - Number of samples
.LEVEL2
.VARIABLE INP
INP specifies the input data set.
.VARIABLE SIZE
The SIZE parameter may be used when a sub-region of an image is to 
be printed; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples.
.VARIABLE SL
INTEGER - Starting line (see SIZE)
.VARIABLE SS
INTEGER - Starting sample (see SIZE)
.VARIABLE NL
INTEGER - Number of lines (see SIZE)
.VARIABLE NS
INTEGER - Number of samples (see SIZE)
.END
EXIT
$ Return
$!#############################################################################
$Test_File:
$ create tstlistbits.pdf
procedure
! TO RUN ON VMS, TYPE   TSTPSF
! TO RUN ON UNIX OR AXP, MOVE THE TEST FILE TO THE MACHINE FROM THE VAX
! IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
! tstpsf DIR=dirname
! where dirname = pathname of directory containing file with trailing / OR
!               = "" if in current directory.
refgbl $echo
refgbl $autousage
PARM DIR TYPE=STRING DEFAULT="SITOD1:[test_data.images]"
LOCAL INPIC TYPE=STRING
body
let _onfail="continue"
let $autousage = "none"
let $echo="yes"
let INPIC= "&DIR"//"gllssi.cmp"
listbits &INPIC (2,1,2,388)
end-proc
$ Return
$!#############################################################################
