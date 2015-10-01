$!****************************************************************************
$!
$! Build proc for MIPL module translog
$! VPACK Version 1.7, Tuesday, April 05, 1994, 13:33:19
$!
$! Execute by entering:		$ @translog
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
$ write sys$output "*** module translog ***"
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
$ write sys$output "Invalid argument given to translog.com file -- ", primary
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
$   if F$SEARCH("translog.imake") .nes. ""
$   then
$      vimake translog
$      purge translog.bld
$   else
$      if F$SEARCH("translog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake translog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @translog.bld "STD"
$   else
$      @translog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create translog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack translog.com -
	-s translog.c -
	-i translog.imake -
	-p translog.pdf -
	-t tsttranslog.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create translog.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include  "xvmaininc.h"
#include  "taeconf.inp"
#include  "pgminc.inc"
#include  "symtab.inc"
#include  "parblk.inc"
#include <string.h>

/* This program uses TAE directly instead of the RTL for no	*/
/* particularly good reason.  Historical value, I guess.	*/

main()
{
   struct PARBLK           vblock;

   int                     status, i;
   char                    *inptr, *outptr;
   char                    output[128];
   char                    input[128];
   int lines, cols, type;

   struct VARIABLE *var, *p_find();

   t_pinit(&lines, &cols, &type);	/* avoid screen clear on startup */
   p_inim(&vblock, P_BYTES, P_ABORT);

   /* Fetch logical name from parameter block */

   var = p_find(&vblock, "INP");
   inptr = SVAL(*var, 0);

   /* Convert string to upper case (VMS only) */

#if VMS_OS
   for (i=0; i<strlen(inptr); i++)
      input[i] = toupper(inptr[i]);
#else
   strcpy(input, inptr);
#endif

   /* Get translation */

   do_trans(input, output);

   /* Do the TAE stuff to pass the name back to the TM process */

   /* Note:  This is incompatible with shell-VICAR, due to the direct	*/
   /* use of TAE.  zvq_out() requires initialization which is done by	*/
   /* vicmain_c.  Since this program does TAE directly, zvq_out() is	*/
   /* unusable and q_out() must be used instead.  I could recode the	*/
   /* startup to be standard... but since all scripting languages have	*/
   /* better facilities than this for translating env vars/log names,	*/
   /* it hardly seems worth it.  Small loss. (rgd 11/97)		*/

   q_init(&vblock, P_BYTES, P_ABORT);
   outptr = output;
   q_string(&vblock, "TRANS", 1, &outptr, P_ADD);
   q_out(&vblock);
}


/* This is the routine that actually does the logical/env var translation */

#if VMS_OS

#include <descrip.h>

do_trans(input,output)
char *input, *output;
{
   struct dsc$descriptor_s input_desc;
   struct dsc$descriptor_s output_desc;
   int                     output_length;
   int                     status;

/* Set up Descriptor of input logical name */

   input_desc.dsc$w_length   = strlen(input);
   input_desc.dsc$a_pointer  = input;
   input_desc.dsc$b_class    = DSC$K_CLASS_S;
   input_desc.dsc$b_dtype    = DSC$K_DTYPE_T;

/* Set up Descriptor of output equivalence string */

   output_desc.dsc$w_length  = 128;
   output_desc.dsc$a_pointer = output;
   output_desc.dsc$b_class   = DSC$K_CLASS_S;
   output_desc.dsc$b_dtype   = DSC$K_DTYPE_T;

/* Get translation */

   status = sys$trnlog(&input_desc, &output_length, &output_desc, 0, 0, 0);
   output_desc.dsc$w_length = output_length;

/* Check for error */

   if (status == 1)
      output[output_length] = '\0';
   else
      output[0] = '\0';

   return;
}

#else		/* Unix version */

#include <stdlib.h>

do_trans(input,output)
char *input, *output;
{
   char *p;

   p = (char *)getenv(input);
   if (p == NULL)
      output[0] = '\0';
   else
      strcpy(output, p);
}

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create translog.imake
#define PROGRAM translog
#define MODULE_LIST translog.c

#define MAIN_LANG_C
#define USES_C

#define R2LIB

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create translog.pdf
process help=*
parm  INP    type=(string, 63)
parm  TRANS  type=name

!# annot function="VICAR Procedure Generation"
!# annot keywords=(VMS,variable,translate,R2LIB,string,TAE,+
!# "Enviroment variables","Logical names","shell-VICAR")

end-proc
.title
Translates a logical name
.help
Program Description

This procedure returns the translation of an input logical name (VMS)
or environment variable (Unix).  (The example listed below will translate
the name R2LIB).  A null string is returned if no translation is possible.
The name is case-sensitive under Unix, since environment variables are
case-sensitive.  It is not case-sensitive under VMS.

Note:  This program must be used from TAE; it will not work with shell-VICAR
due to implementation details.  But since all scripting languages (except
PDFs!) have better facilities than this for accessing env vars/log names,
this program is not needed with shell-VICAR.

Calling Sequence:

           VICAR> local trans type=(string,128)

           VICAR> translog R2LIB trans
           VICAR> disp trans

Written By:            Glenn Dimit  (Nov. 27, 1984)
Cognizant Programmer:  Bob Deen     (Nov. 21, 1997)
.level1
.variable INP
Input name
.variable TRANS
Output equivalence of input's
name
.level2
.variable INP
Input logical name (VMS) or environment variable name (Unix).  Environment
variables should not have the leading "$".
.variable TRANS
Output equivalence of input's logical name (VMS) or environment variable
(Unix).
.end
$ Return
$!#############################################################################
$Test_File:
$ create tsttranslog.pdf
procedure
refgbl $echo
refgbl $syschar
local trans  type=(string,128)
body
let _onfail="continue"
let $echo="yes"
translog R2LIB trans=trans
disp trans
if ($syschar(1) = "UNIX")
   translog TAE trans
else
   translog $TAE trans
end-if
disp trans
translog xyzdasdfe trans
disp trans
end-proc
$ Return
$!#############################################################################
