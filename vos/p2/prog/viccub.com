$!****************************************************************************
$!
$! Build proc for MIPL module viccub
$! VPACK Version 1.9, Monday, December 07, 2009, 17:08:13
$!
$! Execute by entering:		$ @viccub
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
$ write sys$output "*** module viccub ***"
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
$ write sys$output "Invalid argument given to viccub.com file -- ", primary
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
$   if F$SEARCH("viccub.imake") .nes. ""
$   then
$      vimake viccub
$      purge viccub.bld
$   else
$      if F$SEARCH("viccub.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake viccub
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @viccub.bld "STD"
$   else
$      @viccub.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create viccub.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack viccub.com -mixed -
	-s viccub.c -
	-i viccub.imake -
	-p viccub.pdf -
	-t tstviccub.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create viccub.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void my_abort(char abort_message[80]);

void main44()
{
int lauf, lauf_nl;
int status;
int inunit, outunit;
int nl,ns, ni, nb, insampl, inlines;
char    format[10];
char    informat[10];
char    *buf;
char    outstring[80];

status=zvpcnt("INP", &ni);

status=zvunit(&inunit, "INP", 1, NULL);
status=zvopen(inunit, NULL);
if (status != 1) my_abort("can not open first input image");

status=zvget(inunit,"nl",&nl,"ns",&ns, "FORMAT", format, NULL);
status = zvclose(inunit,NULL);

if (!strcmp(format,"COMP")) 
   my_abort
      ("first input image has complex format, not supported");


status=zvunit(&outunit, "out", 1, NULL);
status=zvopen(outunit, 
       "OP", "WRITE",
       "O_FORMAT", format,
       "U_NS", ns, "U_NL", nl, "U_NB", ni,
       NULL);
if (status != 1) my_abort("can not open output image");

/* the largest image format is double */
buf=(char *)malloc(ns*sizeof(double));   
if(buf==NULL)
   my_abort("input images to large, memory problems !!");


lauf=0;

for (lauf=1; lauf <= ni; lauf++)
   {
   status=zvunit(&inunit, "INP", lauf, NULL);
   status=zvopen(inunit, NULL);
if (status != 1) 
   {
   sprintf(outstring,"can not open input image # %5d",lauf);
   my_abort(outstring);
   }

   status=zvget(inunit,"nl",&inlines,"ns",&insampl, "nb", &nb,
                "format", informat, NULL);

   if (nb != 1) 
      {
      sprintf(outstring,"input image # %5d has more than 1 band",lauf);
      my_abort(outstring);
      }
   if (insampl != ns)
      {
      sprintf(outstring,
         "input image # %5d has different number of samples",lauf);
      my_abort(outstring);
      }
   if (inlines != nl)
      {
      sprintf(outstring,
         "input image # %5d has different number of lines",lauf);
      my_abort(outstring);
      }
   if (strcmp(format,informat)) 
      {
      sprintf(outstring,
         "input image # %5d has different image format",lauf);
      my_abort(outstring);
      }

   for (lauf_nl=1; lauf_nl <= nl; lauf_nl++)         
      {
      status=zvread(inunit, buf,  NULL);
      status=zvwrit(outunit, buf, NULL);
      }
   zvclose(inunit, NULL);
   }

status=zvclose(outunit,NULL);
}

void my_abort(abort_message)

char abort_message[80];
{
   zvmessage("","");
   zvmessage("     ******* VICCUB error *******","");
   zvmessage(abort_message,"");
   zvmessage("","");
   zabend();
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create viccub.imake
#define PROGRAM viccub

#define P2LIB 

#define MODULE_LIST viccub.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create viccub.pdf
process help=*
PARM INP COUNT=(1:255)
PARM OUT
end-proc

.Title
 Program VICCUB

.HELP

PURPOSE

WRITTEN BY Thomas Roatsch, DLR    9-Apr-1997

VICCUB copies input images into 1 multi-band image.

All images must have the same size and the same format.
The maximum number of input images is 255.

.LEVEL1
.VARI INP
 Names of input files
.VARI OUT
 Name of output file
.End
$ Return
$!#############################################################################
$Test_File:
$ create tstviccub.pdf
procedure
refgbl $echo
refgbl $syschar
body
write "Test procedure for viccub"
write ""
write "generate 3 test files"
gen aaa nl=50 ns=30
gen bbb nl=50 ns=30
gen ccc nl=50 ns=30
write "create the multi band file"
viccub (aaa,bbb,ccc) multi
write ""
write "label of the multibandfile"
write ""
label-l multi
write ""
write "let's do the same with a full image"
write ""
write "generate 3 test files"
gen aaa nl=50 ns=30 format=full
gen bbb nl=50 ns=30 format=full
gen ccc nl=50 ns=30 format=full
write "create the multi band file"
viccub (aaa,bbb,ccc) multi
write ""
write "label of the multibandfile"
write ""
label-l multi
write ""
end-proc
$ Return
$!#############################################################################
