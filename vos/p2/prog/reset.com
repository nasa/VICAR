$!****************************************************************************
$!
$! Build proc for MIPL module reset
$! VPACK Version 1.7, Thursday, June 30, 1994, 16:31:57
$!
$! Execute by entering:		$ @reset
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
$ write sys$output "*** module reset ***"
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
$ write sys$output "Invalid argument given to reset.com file -- ", primary
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
$   if F$SEARCH("reset.imake") .nes. ""
$   then
$      vimake reset
$      purge reset.bld
$   else
$      if F$SEARCH("reset.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake reset
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @reset.bld "STD"
$   else
$      @reset.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create reset.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack reset.com -
	-s reset.c -
	-i reset.imake -
	-p reset.pdf -
	-t tstreset.dat tstreset.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create reset.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>
#include "vicmain_c"

/*  Revision History                                                    */
/*    8-86   SP   changed fscanf to sscanf and changed                  */
/*                format specifier from "%5d" to "10%d"                 */
/*                to allow users to modify the first record in file.    */
/*                added code to check for first record having a         */
/*                different length than expected and to handle this.    */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/

void main44(void)
{
#define MAXR1    80             /* buffer length for first record.      */
#define LENGTH1  17             /* number of chars in first record      */
   
    FILE *unit, *ounit;	        /* C I/O control block			*/
    int file_number,stat;	/* NEXT FILE number and status return	*/
    int count;			/* COUNT from zvp			*/
    int i,j, length;              /* loop variable, first record length   */
    char c;
    char rec1[MAXR1];           /* buffer for first record in file.     */
    int rec_tmp[MAXR1];           /* buffer for first record in file.     */
    char file_name[41];		/* name of file to reset		*/
    char new_name[45];		/* process specifiec file_name		*/
    long off_dst,off_src;	/* used for fseek */
    int c_buf_size, c_buf_indx;

    zvp("INPUT",file_name,&count);	/* get name of input		*/
    zvfilename(file_name,new_name,43);

    unit = fopen(new_name,"r+");
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }

/*   Load first record into rec1 buffer.                                */

    for (i=0; (rec1[i] = getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;
    length = i;
    /* Start source offset at character right after the first \n. */
    off_src=i+1;

    stat = sscanf(rec1, "NEXT FILE = %10d", &file_number);
    if (stat != 1)
    {
	zvmessage(" Error reading number of next file","");
	zvmessage(" First line of input file must be of the form:","");
	zvmessage(" NEXT FILE = file_number","");
	zabend();
    }

    zvp("NEXTFILE",&file_number,&count);	/* Get next file number	*/

    if ( length >= LENGTH1 )      /* If no change in record length just */
    {                             /* change first record.               */
      rewind(unit);				/* Write it out		*/
      sprintf(rec1,"NEXT FILE = ");
      for(i=0; i< (length-LENGTH1); i++)
        strcat(rec1," ");
      sprintf(&rec1[12+length-LENGTH1],"%5d\n",file_number);
      stat = fprintf(unit,rec1);
      if (stat != length+1)
      {
 	zvmessage(" Write (update) error on input","");
	zabend();
      }
      fclose(unit);
    } else { 
/* sorry guys.  I tried to get VMS to use the same code as UNIX, but */
/* it just refused, and i tried 3 different approaches, so we're stuck */
/* with 2 seperate routines. */
#if VMS_OS
      /* open up another file and just copy into it - the reason this */
      /* works is because VMS just opens up another version. */
      ounit = fopen(new_name,"w");
      if (ounit == NULL)
      {
	zvmessage(" File open error.  Check specification.","");
	zabend();
      }
      sprintf(rec1,"NEXT FILE = %5d\n",file_number);

      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], ounit );          /* write out first record. */

      while ( (c=getc(unit)) != EOF )
        putc( c, ounit);                 /* copy the rest of the file. */

      fclose(unit);
      fclose(ounit);

#else
      /* determine size needed and fill circular buffer */
      c_buf_size = LENGTH1-length;
      c_buf_indx = 0;
      for(j=0; (j<c_buf_size) && (rec_tmp[c_buf_indx] = getc(unit)) != EOF;
		j++) {
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_src++;
      }
      /* off_scr is now sitting at the first read element (0) which will */
      /* be the next written over. */

      rewind(unit);
      /* write 1st record */
      sprintf(rec1,"NEXT FILE = %5d\n",file_number);
      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], unit );          /* write out first record. */
      off_dst = strlen(rec1);

      /* copy over remainder of file through circular buffer */
      /* until we hit end of file */
      fseek(unit,off_src,0);
      while ( (rec_tmp[c_buf_indx]=getc(unit)) != EOF )
      {
        fseek(unit,off_dst,0);
        /* write out oldest element */
        putc(rec_tmp[ (c_buf_indx+1) % (c_buf_size+1)] , unit);
        off_dst++;
        off_src++;
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        fseek(unit,off_src,0);
      }

      /* wrap up the file */
      c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
      for(j=0; j<c_buf_size ; j++) {
        putc(rec_tmp[c_buf_indx],unit);
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_dst++;
      }

      fclose(unit);
#endif
    }

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create reset.imake
#define PROGRAM reset

#define MODULE_LIST reset.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create reset.pdf
PROCESS HELP=*
  PARM INPUT TYPE=(STRING,40)
  PARM NEXTFILE TYPE=INT DEFAULT=1

!# annot function="VICAR Procedure Generation"
!# annot keywords=(SRCH,record,default,NXT,CNT)

END-PROC
.title
Resets the next file pointer of a SRCH list
.help
RESET takes an input which is a list of files in the format of
the output from the proc SRCH and resets the next file (the n in 
"NEXT FILE =     n", the first record) to the value specified by 
NEXTFILE, or 1 by default.  (This number is limited to 5 digits.)
.level1
.vari input
Input file name
.vari nextfile
New next file number
.level2
.vari input
INPUT is a file in the format written by the proc SRCH, containing
the line "NEXT FILE =     n" followed by a list of file names.
.vari nextfile
NEXTFILE is the new value for the "NEXT FILE =" field of the input
file.  This field is used to provide a value for the program NXT.
This file is limited to 5 digits.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstreset.dat
NEXT FILE =     1
A.DAT
B.DAT
C.DAT
D.DAT
E.DAT
F.DAT
$!-----------------------------------------------------------------------------
$ create tstreset.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
  ush cat tstreset.dat
  reset tstreset.dat 3
  ush cat tstreset.dat
  reset tstreset.dat
  ush cat tstreset.dat
else
  dcl type tstreset.dat
  reset tstreset.dat 3
  dcl type tstreset.dat
  reset tstreset.dat
  dcl type tstreset.dat
end-if
let $echo="no"
end-proc
$ Return
$!#############################################################################
