$!****************************************************************************
$!
$! Build proc for MIPL module ddd2vic
$! VPACK Version 1.9, Monday, December 07, 2009, 16:05:06
$!
$! Execute by entering:		$ @ddd2vic
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
$ write sys$output "*** module ddd2vic ***"
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
$ write sys$output "Invalid argument given to ddd2vic.com file -- ", primary
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
$   if F$SEARCH("ddd2vic.imake") .nes. ""
$   then
$      vimake ddd2vic
$      purge ddd2vic.bld
$   else
$      if F$SEARCH("ddd2vic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ddd2vic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ddd2vic.bld "STD"
$   else
$      @ddd2vic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ddd2vic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ddd2vic.com -mixed -
	-s ddd2vic.c -
	-p ddd2vic.pdf -
	-i ddd2vic.imake -
	-t tstddd2vic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ddd2vic.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

ddd2vic  -- 

Purpose: to convert ddd formatted MGS filesto VICAR format

20dec00 -bam- original version

******************************************************************************/

#include <math.h>
#include "vicmain_c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

FILE *fp,*fopen();              /* for file work */

int i_unit,o_unit;              /* for VICAR */
int status;

char message[80];               /* for user information output */
  
int error;                      /* return error flag */
int hstart,hend;                /* start and end of header data */

/***************************************************************************/
void main44(void)
{
  char verdat[11];              /* VERSION_DATE */
  int nl, ns;	        	/* actual dimensions */
   
  /* inform user of Program version & update VERSION_DATE for label */
  zvmessage("   ","");
  zvmessage("*** ddd2vic version 20-Dec-2000 ***","");
  zvmessage("   ","");
  strcpy( verdat, "2000-12-20");

  process_parms(&nl, &ns);      /* get nl,ns + data file names */
  if ( error == 1 ) return;     /* error handling files */

  process_data(nl,ns);          /* read and write ddd data */

  fclose(fp);
  status = zvclose(o_unit,NULL);

  return;
}


/****************************************************************************/
/* process user input parameters                                            */
/****************************************************************************/
process_parms(nl,ns)
  int *nl, *ns;	        	/* actual dimensions */
{
  int i,l,s;
  char file[100];

  error = 0;                    /* return error flag */

  zvp("INP",file,&i);	 	/* Get input file name	*/
  fp = fopen(file,"r");         /* open the input file */
  if ( fp == 0 ) 
      {
      sprintf ( message, "\nError opening input file\n\n " );
      zvmessage (message," ");
      error = 1;
      return 0;
      }

  find(&l, &s);  /* get input nl, and ns from ddd header */
  *nl = l;
  *ns = s;
  if ( error == 1 ) 
      {
      sprintf ( message, "\nError finding # of lines and samples\n\n " );
      zvmessage (message," ");
      error = 1;
      return 0;
      }

 
  zvp("OUT",file,&i);
  status = zvunit(&o_unit,"OUT",1,NULL);
  if ( status != 1 ) goto bad;
  status = zvopen(o_unit,"OP","WRITE","U_FORMAT","BYTE","O_FORMAT",
	"BYTE","U_NL",*nl,"U_NS",*ns,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  if ( status != 1 ) goto bad;
  return 0;

/* some type of problem with the output file */
bad: sprintf ( message, "\nError handling output file\n\n " );
     zvmessage (message," ");
     error = 1;
     return 0;
}


/****************************************************************************/
/* Find the number of lines and samples                                     */
/****************************************************************************/
find(l, s)
  int *l,*s;	        	/* actual dimensions */
{
 int i,j,k;
 int len;
 char header[1024];              /* ddd header */
 int nl,ns;

 len = 1024;
 status = fread(header,1,len,fp);

/* grab all the pertinent data from the header first */
 for ( i=0; i<len; i++ )
     {
     if ( header[i] == 'd' ) /* found the start of the header */
         {
         hstart = i;
         j = i;
         for ( k=j; k<len; k++ )
             {
             if ( header[k] == '0' ) /* found the end of the header */
                 {
                 hend = k - 1;
                 goto next;
                 } 
             }       
         }
     }

/* find the # of samples */
next:
 for ( i=0; i<len; i++ )
      {
      if (strncmp(&header[i],"cross",5)==0 ) /* find the samples */
          {          
          sscanf(&header[i+6], "%d", &ns);
          break;
          }
      }

/* find the # of lines */
 for ( i=0; i<len; i++ )
      {
      if (strncmp(&header[i],"down",4)==0 ) /* find the lines */
          {          
          sscanf(&header[i+5], "%d", &nl);
          break;
          }
      }
  *l = nl;
  *s = ns;
  return 0;
}


/****************************************************************************/
/* process data                                                             */
/****************************************************************************/
process_data(nl,ns)
  int nl,ns;	        	/* actual dimensions */
{
  int i;
  int lines;
  unsigned char *buf;           /* buffer for the data */
  int i_status, o_status;

/* allocate enough room for a line of data */
  buf = (unsigned char *)malloc(sizeof(char) * ns );  

  for ( i=0; i<nl; i++ ) /* read and write the data */
      {
      i_status = fread(buf,1,ns,fp);
      if ( i_status == EOF ) /* check for end of file */
          {
          lines = i;
          goto end;
          }      
      if ( i_status != ns ) /* check for end of file */
          {
          sprintf ( message, "\nError reading input\n\n " );
          zvmessage (message," ");
          sprintf ( message, "\nProgram terminated\n\n " );
          zvmessage (message," ");
          lines = i;
          goto end;
          }      
      o_status = zvwrit (o_unit, buf, NULL); /* and write it out */
      lines = i;
      }

end:
  status = zladd(o_unit,"SYSTEM","NL",&lines,"FORMAT","INT",NULL);
  return 0;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ddd2vic.pdf
PROCESS HELP=*
PARM INP    TYPE=STRING COUNT=1
PARM OUT    TYPE=STRING COUNT=1
END-PROC
.TITLE
VICAR2 program ddd2vic
            
.HELP
PURPOSE:

ddd2vic is a VICAR2 applications program which is used to convert Mars Global
Surveyor "ddd" formatted data into VICAR formatted data.  Information about the 
file which is held in the ddd header is placed in the "PROPERTY" region of the
VICAR header.
.page 
EXECUTION STATEMENT:
		  ddd2vic in out 

OPERATION:
ddd2vic first reads and parses the "ddd" header, extracting number of lines and
samples as well as other useful information. The data is then moved from the
"ddd" to VICAR file.  Finally, the VICAR header is updated with the information
removed from the "ddd" header.
WRITTEN BY: Steve Levoe ( original perl script )
            Barbara McGuffie - Vicar code

COGNIZANT PROGRAMMER: Barbara McGuffie

REVISION:  1.0 -- Original Version

.LEVEL1
.VARI INP
The input MGS 
"ddd" data set.
.VARI OUT
Output VICAR 
data set.
.LEVEL2 
.END 
.VARI INP
The input MGS "ddd" data set.
.VARI OUT
Output VICAR data set.
$ Return
$!#############################################################################
$Imake_File:
$ create ddd2vic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ddd2vic

   To Create the build file give the command:

		$ vimake ddd2vic			(VMS)
   or
		% vimake ddd2vic			(Unix)


************************************************************************/


#define PROGRAM	ddd2vic
#define R2LIB

#define MODULE_LIST ddd2vic.c

#define MAIN_LANG_C
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstddd2vic.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
write "		Test for program ddd2vic"
write ""
write ""
write "Generate a VICAR file"

if ($syschar(1) = "VAX_VMS")
   ddd2vic dev:[bam059.test]M12-963.DDD ddd.vic
else
   ddd2vic /home/bam/porting/sun-solr/M12-963.DDD ddd.vic
end-if

write "Now list out the labels and a chunk of the data"
   label-list ddd.vic
   list ddd.vic nl=10 ns=5

write "And compare this to the original perl script data"
if ($syschar(1) = "VAX_VMS")
   label-list dev:[bam059.test]M12.VIC
   list dev:[bam059.test]M12.VIC nl=10 ns=5
else
   label-list /home/bam/porting/sun-solr/M12.VIC
   list /home/bam/porting/sun-solr/M12.VIC nl=10 ns=5
end-if
if ($syschar(1) = "UNIX")
  ush rm ddd.vic
else
  dcl del ddd.vic.
end-if
end-proc
$ Return
$!#############################################################################
