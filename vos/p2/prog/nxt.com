$!****************************************************************************
$!
$! Build proc for MIPL module nxt
$! VPACK Version 2.1, Monday, April 11, 2016, 22:17:27
$!
$! Execute by entering:		$ @nxt
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
$ write sys$output "*** module nxt ***"
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
$ write sys$output "Invalid argument given to nxt.com file -- ", primary
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
$   if F$SEARCH("nxt.imake") .nes. ""
$   then
$      vimake nxt
$      purge nxt.bld
$   else
$      if F$SEARCH("nxt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake nxt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @nxt.bld "STD"
$   else
$      @nxt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create nxt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack nxt.com -mixed -
	-s nxt.c -
	-i nxt.imake -
	-p nxt.pdf -
	-t tstnxt.pdf tstnxt.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create nxt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>
#include "taeextproto.h"
#include "parblk.inc"
#include "vicmain_c.h"
#include "zifmessage.h"

/*  Revision History                                                    */
/*    9-86   SP   changed fscanf to sscanf and changed                  */
/*                format specifier from "%5d" to "10%d"                 */
/*                to allow users to modify the first record in file.    */
/*                added code to check for first record having a         */
/*                different length than expected and to handle this.    */
/*    9-86   SP   modified to output to a TAE variable the number of the*/
/*                file (as done in program CNT) per Charlie Avis.       */
/*                note that this change the calling sequence of NXT by  */
/*                adding another required parameter, NXTFIL.            */
/*    2-87   SP   Added code to handle  NO RECORDS in SRCH file.        */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/
/*    8-97   RRD  Added first fseek call after #else to fix position    */
/*    7-1998 TIH  Changed constant value with FILENAME_LEN to fix       */
/*   10-2011 LWK  Increased pool size in q_init call from 500 to 1000   */
/*                to fix V-block overflow in 64-bit linux.              */
/*    12-09-2012 R. J. Bambery - fixed warnings with gcc 4.7.2          */
/*                moved variables c and ounit into #ifdef VMS block     */
 
void main44(void)
{

#define MAXR1    80             /* buffer length for first record.      */
#define LENGTH1  17             /* number of chars in first record      */
#define FILENAME_LEN	255
   
    FILE *unit;	        /* C I/O control block			*/
    int length;                 /* first record length                  */
    int count,def;		/* COUNT, DEF for XVPARM		*/
    struct PARBLK out_parb;	/* Local parameter block for XQ calls	*/
    int next_file;		/* Next file to be retrieved		*/
    int stat,i;			/* Return status ind, increment var	*/
    int j;                      /* loop variable, first record length   */
    int tape_pos;		/* File position number for tapes	*/
    int is_tape;		/* 1 if file is tape; 0 if not		*/
    char rec1[MAXR1];           /* buffer for first record in file.     */
    int rec_tmp[MAXR1];           /* buffer for first record in file.     */
    char file_name[FILENAME_LEN];	/* name of file to reset	*/
    char new_name[FILENAME_LEN];	/* process specific  file_name	*/
    char output_name[FILENAME_LEN];	/* Name of file for output	*/
    char output_name_final[FILENAME_LEN];	/* Name of file for output */
    char *name_ptr;
    char msg[80];		/* Message buffer for zvmessage		*/
    long off_dst,off_src;	/* used for fseek */
    int c_buf_size, c_buf_indx;

    zifmessage("*** nxt version 2016-03-07");
    zvparm("INPUT",file_name,&count,&def,1,0);
    zvfilename(file_name,new_name,FILENAME_LEN);

    unit = fopen(new_name,"r+");	/* Open the file		*/
    if (unit == NULL)
    {
	zvmessage("??E - File open error.  Check specification.","");
	zabend();
    }

     /*   Load first record into rec1 buffer.                           */
    for (i=0; (rec1[i] = (char)getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;
    length = i;
    /* Start source offset at character right after the first \n. */
    off_src=i+1;

    if ( strncmp(rec1,"NO RECORDS",10) == 0 )
    {
       stat = EOF;                   /* case of no records found by SRCH */
    } else {
      stat = sscanf(rec1, "NEXT FILE = %10d", &next_file);
      if (stat != 1)
      {
  	zvmessage("??E - Error reading number of next file","");
  	zvmessage(" First line of input file must be of the form:","");
  	zvmessage(" NEXT FILE = file_number","");
  	zabend();
      }
  
      if (next_file <= 0) next_file = 1;	/* Protect from user error */
  
      for (i = 1; i <= next_file; i++)
      {
  	stat = fscanf(unit,"%s",output_name);
  	if (stat == EOF) break;
  	if (stat != 1)
  	{
  	    zvmessage("??E - Read error on input","");
  	    zabend();
  	}
      }
    }

    if (stat == EOF)
    {
	strcpy(output_name_final,"END_OF_FILE");
	tape_pos = 0;
	is_tape = 0;
	zvmessage(" NXT encountered end of file","");
    }
    else if (output_name[0] == '?')	/* if tape is used */
    {
	for (i = 1; output_name[i] != '/'; i++)
	  ;
	strncpy(output_name_final,&output_name[1],(size_t)( i-1));
	output_name_final[i-1] = '\0';
	sscanf(&output_name[i+1],"%d",&tape_pos);
	is_tape = 1;
	sprintf(msg," Output %d is tape %.*s file %d",
		next_file,i-1,&output_name[1],tape_pos);
        zvmessage(msg,"");
    }
    else				/* situation normal */
    {
	strcpy(output_name_final,output_name);
	tape_pos = 0;
	is_tape = 0;
	sprintf(msg," Output %d is %s",next_file,output_name);
	zvmessage(msg,"");
    }
	
    q_init(&out_parb,1000,P_ABORT);	/* Initialize a local par block	*/

/* output variables ONAM, ISTAPE, and TAPEPOS to vblock			*/
    name_ptr=output_name_final;
    q_string(&out_parb,"ONAM",1,&name_ptr,P_ADD);
    q_intg(&out_parb,"ISTAPE",1,&is_tape,P_ADD);
    q_intg(&out_parb,"TAPEPOS",1,&tape_pos,P_ADD);
    q_intg(&out_parb,"NXTFIL",1,&next_file,P_ADD);

    zvq_out(&out_parb);		/* Output vblock to TM		*/

    next_file++;

    if ( strncmp(output_name_final,"END_OF_FILE",11) == 0)
    {
      fclose(unit);                     /* skip update if at end of file*/
    }
    else if ( length >= LENGTH1 )      /* If no change in record length just */
    {                             /* change first record.               */
      rewind(unit);				/* Write it out		*/
      sprintf(rec1,"NEXT FILE = ");
      for(i=0; i< (length-LENGTH1); i++)
        strcat(rec1," ");
      sprintf(&rec1[12+length-LENGTH1],"%5d\n",next_file);
      stat = fprintf(unit,rec1);
      if (stat != length+1)
      {
 	zvmessage("??E -  Write (update) error on input","");
	zabend();
      }
      fclose(unit);
    } else { 
      /* determine size needed and fill circular buffer */
      c_buf_size = LENGTH1-length;
      c_buf_indx = 0;
      fseek(unit,off_src,0);    /* fscanf moved position so go back */  
      for(j=0; (j<c_buf_size) && (rec_tmp[c_buf_indx] = getc(unit)) != EOF;
		j++) {
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_src++;
      }
      /* off_scr is now sitting at the first read element (0) which will */
      /* be the next written over. */

      rewind(unit);
      /* write 1st record */
      sprintf(rec1,"NEXT FILE = %5d\n",next_file);
      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], unit );          /* write out first record. */
      off_dst = (long int)strlen(rec1);

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
    }
}



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create nxt.imake
#define PROGRAM nxt

#define MODULE_LIST nxt.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* comment out on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create nxt.pdf
PROCESS HELP=*
  local a       type=string  count=1
  local (b,c,d) type = integer 
  PARM INPUT    TYPE=STRING COUNT=1
  PARM ONAM     TYPE=NAME  default = a
  PARM ISTAPE   TYPE=NAME  default = b
  PARM TAPEPOS  TYPE=NAME  default = c
  PARM NXTFIL   TYPE=NAME  default = d

!# annot function="VICAR Procedure Generation"
!# annot keywords=(SRCH,ONAM,ISTAPE,TAPEPOS,NXTFIL,"LIST.DAT",+
!#  "Output parameter","TAE variable",NXT,CNT,SRCH)
END-PROC
.TITLE
Returns data for next file in a SRCH list
.HELP

OPERATION

NXT reads a list file which was written by the program SRCH and 
returns information about the next file in the list.  The values 
are returned in ONAM, ISTAPE, TAPEPOS, and NXTFIL.  Upon completion, ONAM
contains the name of the disk file or tape position which is next in the list.

If the file is a tape, ISTAPE will be 1, otherwise it will be 0.

If ISTAPE is 1, then TAPEPOS will contain the file position number on 
the tape.

NXTFIL contains the (ordinal) number of the returned file within the list file.

NXT needs to have write access to the input file and to its directory because
it modifies the first record of the file.  The recommended method for accessing
a SRCH file owned by another user is to make a copy of it in one's account.


HISTORICAL FOOTNOTE

This program was written for processing files from a 9-track tape on
a VAX-VMS system in the 1980's. In those days disk space was limited,
so tape was the medium of archival storage and data interchange.

Tapes came in two forms; with and without IBM standard labels. Standard
label tape allowed file names to be associated with a position on the
tape. If the tape was not labeled then you could only access data
by knowing its position on the tape. This program performed this
function for you by relating an entry in an ascii text list of files
to a position on a tape and then creating a disk file with a filename.

The example, below shows historically how this was done under VAX-VMS.

Now this program gives you the ability to access files quickly from
a list, thus avoiding a human manual entry.
.page
EXAMPLE:

The following procedure could be used to get a list of the 
system label of each file in the list file LIST.DAT.

PROCEDURE
  LOCAL FILENAM TYPE=STRING
  LOCAL ISTAPE TYPE=INT
  LOCAL POSITION TYPE=INT
  LOCAL FILENUM TYPE=INT
BODY
  RESET LIST.DAT
  write ('resetting')
  LOOP
    NXT LIST.DAT FILENAM ISTAPE POSITION FILENUM
    IF (FILENAM = "END_OF_FILE") BREAK
    IF (ISTAPE)
        ALLOC MT T
	MOUNT T COMMENT="Please mount tape &FILENAM"
	LABEL-LIST &FILENAM/&POSITION 'SYS
      ELSE
        LABEL-LIST &FILENAM 'SYS
    END-IF
  END-LOOP
END-PROC

.page
REVISIONS:

  1986-09    SP  Changed fscanf to sscanf and changed                  
                 format specifier from "%5d" to "10%d"                 
                 to allow users to modify the first record in file.    
                 Added code to check for first record having a         
                 different length than expected and to handle this.    
                 Modified to output to a TAE variable the number of the
                 file (as done in program CNT) per Charlie Avis.       
                 Note that this change the calling sequence of NXT by  
                 adding another required parameter, NXTFIL.            
  1987-02    SP  Added code to handle  NO RECORDS in SRCH file.        
  1994-06    SVH Ported to UNIX - Steve Hwan				
  1996-06    OAM Included a REWIND statement and a loop in the VMS part
	         to avoid dropping filenames as it updates the NEXT FILE
                 number.  FR 89371.
  1997-08    RRD Added first fseek call after #else (the UNIX specific 
                 section) because a previous call to fscanf had moved 
                 the current position in the file and this fixes it.
  1998-07    TIH Changed constant value with FILENAME_LEN to fix       
  2011-10    LWK Increased pool size in q_init call from 500 to 1000   
                 to fix V-block overflow in 64-bit linux.             
  2012-12-09 RJB fixed warnings with gcc 4.7.2         
                 moved variables c and ounit into #ifdef VMS_OS block   
  2012-12-14 RJB Doc update.
  2016-03-07 WLB Switched to ANSI_C. Removed VMS residue. Migrated to MIPL.

.LEVEL1
.VARI INPUT 
List file
.VARI ONAM
Output -- the next name
in the list
.VARI ISTAPE
Output -- TRUE (1) if tape
.VARI TAPEPOS
Output -- File position number 
.vari nxtfil
Output -- Number of next
file in list.
.LEVEL2
.VARI INPUT
The input file which was generated by the program SRCH.  The file
contains a list of file names from which the next entry is to be
selected.
.VARI ONAM
After exiting NXT, the parameter name given for ONAM contains
the next filename in the list contained in INPUT.  If the file
is on a tape, then ONAM contains the name of the tape.

See the example under HELP NXT.
.VARI ISTAPE
ISTAPE is an output integer from NXT which is 1 (logical TRUE) if
the file given in ONAM is a tape, and 0 (logical FALSE) if it is not.

ISTAPE may be used like a logical expression in TCL, like

  IF (ISTAPE) ...

See the example under HELP NXT.
.VARI TAPEPOS
TAPEPOS indicates the file number on the tape when the output from
ONAM is a tape file (ISTAPE is 1).

See the example under HELP NXT.
.vari nxtfil
Upon completion of NXT, the parameter given for NXTFIL contains the
number of the next file to be referenced in the INPUT list (corresponding
to ONAM).
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstnxt.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1
local filnam    type=string count=1
local istape    type=int    count=1
local tapos     type=int    count=1
local filenum   type=int    count=1

! Aug 22, 2013 - RJB
! TEST SCRIPT FOR NXT
!
! Vicar Programs:
!       translog reset 
!
! External programs
!       <none>
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!            
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 

refgbl $echo


body
let _onfail="stop"

enable-log

!check to see if mipl or cartlab for test images
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/carto ct
else
!CARTLAB
    if (aftestdata = "") 
        ush ln -s ../test_data/vicar_test_images/testdata/carto ct
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto ct
    end-if
end-if

let _onfail="goto rm"

let $echo="yes"
ush cp ct/tstnxt.dat ./
ush cp ct/file_list.list ./ 
reset tstnxt.dat 3
reset file_list.list 1
loop
  nxt tstnxt.dat filnam istape tapos filenum
  if (filnam = "END_OF_FILE") break
  let $echo="no"
  write "filnam = &filnam"
  write "istape = &istape"
  write "tapos = &tapos"
  write "filenum = &filenum"
  let $echo="yes"
end-loop
!
!Now, using default parameters.....
!
!To test the fixes for the FR 89371,
!run this script on the Alphas 
!and inspect the output file:
!file_list.list
!
  nxt file_list.list
!
!To test for AR100461, add the current path in front
!of input filename, as: /project/it/tih/deliv20.01/ss/nxt/tstnxt.dat
rm>
ush rm ct

disable-log

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstnxt.log
ush cp ct/tstnxt.dat ./
ush cp ct/file_list.list ./
reset tstnxt.dat 3
Beginning VICAR task reset
reset file_list.list 1
Beginning VICAR task reset
loop
  nxt tstnxt.dat filnam istape tapos filenum
Beginning VICAR task nxt
*** nxt version 2016-03-07
 Output 3 is C.DAT
  if (filnam = "END_OF_FILE") break
  let $echo="no"
filnam = C.DAT
istape = 0
tapos = 0
filenum = 3
end-loop
  nxt tstnxt.dat filnam istape tapos filenum
Beginning VICAR task nxt
*** nxt version 2016-03-07
 Output 4 is tape JS4269 file 4
  if (filnam = "END_OF_FILE") break
  let $echo="no"
filnam = JS4269
istape = 1
tapos = 4
filenum = 4
end-loop
  nxt tstnxt.dat filnam istape tapos filenum
Beginning VICAR task nxt
*** nxt version 2016-03-07
 Output 5 is E.DAT
  if (filnam = "END_OF_FILE") break
  let $echo="no"
filnam = E.DAT
istape = 0
tapos = 0
filenum = 5
end-loop
  nxt tstnxt.dat filnam istape tapos filenum
Beginning VICAR task nxt
*** nxt version 2016-03-07
 Output 6 is F.DAT
  if (filnam = "END_OF_FILE") break
  let $echo="no"
filnam = F.DAT
istape = 0
tapos = 0
filenum = 6
end-loop
  nxt tstnxt.dat filnam istape tapos filenum
Beginning VICAR task nxt
*** nxt version 2016-03-07
 NXT encountered end of file
  if (filnam = "END_OF_FILE") break
 break
end-loop
  nxt file_list.list
Beginning VICAR task nxt
*** nxt version 2016-03-07
 Output 1 is 0400r.spk
ush rm ct
disable-log
$ Return
$!#############################################################################
