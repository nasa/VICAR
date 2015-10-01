$!****************************************************************************
$!
$! Build proc for MIPL module username
$! VPACK Version 1.8, Wednesday, December 28, 1994, 14:25:06
$!
$! Execute by entering:		$ @username
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
$ write sys$output "*** module username ***"
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
$ write sys$output "Invalid argument given to username.com file -- ", primary
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
$   if F$SEARCH("username.imake") .nes. ""
$   then
$      vimake username
$      purge username.bld
$   else
$      if F$SEARCH("username.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake username
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @username.bld "STD"
$   else
$      @username.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create username.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack username.com -
	-s username_vms.c username_unix.c -
	-i username.imake -
	-p username.pdf username_vms.pdf username_unix.pdf -
	-t tstusername.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create username_vms.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* 02 JAN 1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING) */
#include  "vicmain_c"
#include  <descrip.h>
#include  <stdio.h>
/* #undef I_PGMINC */		/* TEMP KLUDGE to allow .INC instead of .CIN */
#include  "pgminc.inc"
#define XPRDIM	(5300/4)	/* Size of the parblock in ints */

void main44(void)
{
   char  *cuserid();
   static char string[L_cuserid] = "";
   char  *p_userid;
   int   vblock[XPRDIM];
   int   status;
   struct dsc$descriptor_s  userid_desc;

   zifmessage ("USERNAME version 02-JAN-95");

/* Get the user name */

   p_userid = cuserid(string);

/* Create a descriptior for the user id */

   userid_desc.dsc$w_length = strlen(p_userid);
   userid_desc.dsc$a_pointer = p_userid;
   userid_desc.dsc$b_class = DSC$K_CLASS_S;
   userid_desc.dsc$b_dtype = DSC$K_DTYPE_T;

/* Do the TAE stuff to pass the name back to the TM process */

     q_init(vblock, &XPRDIM, &P_ABORT);
     status = q_string(vblock, "NAME", 1, &p_userid, P_ADD);
     status = zvq_out(vblock); 
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create username_unix.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* 02 JAN 1995 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING) */
#include  <stdio.h>
#include  <stdlib.h>
#include  "vicmain_c"
#include  "pgminc.inc"
#define XPRDIM	(5300/4)	/* Size of the parblock in ints */

void main44(void)
{
   char  *cuserid();
   static char name_desc[5] ;
   char  *p_userid, *s;
   char   vblock[XPRDIM];
   int   xprdim, p_abort, count, status;

   zifmessage ("USERNAME version 02-JAN-95");
   xprdim = XPRDIM;
   p_abort = P_ABORT;
   count = 1;

/* Get the user name */

   p_userid = getenv("USER");

/* Do the TAE stuff to pass the name back to the TM process */

     q_init(vblock, &xprdim, &p_abort);
     status = q_string(vblock, "NAME", count, &p_userid, P_ADD);
     status = zvq_out(vblock); 
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create username.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM username

   To Create the build file give the command:

	$ vimake username                     (VMS)
   or
	% vimake username                     (Unix)


*************************************************************************/

#if VMS_OS
#define PROGRAM username_vms
#define MODULE_LIST username_vms.c 
#define CLEAN_OTHER_LIST username_unix.c
#else
#define PROGRAM username_unix
#define MODULE_LIST username_unix.c
#define CLEAN_OTHER_LIST username_vms.c
#endif

#define MAIN_LANG_C
#define USES_C

#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create username.pdf
procedure help=*
parm  NAME    type=name
refgbl $syschar
refgbl $echo
body
let $echo="no"
if ($syschar(1) = "VAX_VMS")
   username_vms name=@NAME
else
   username_unix name=@NAME
end-if

!# annot function="VICAR Procedure Generation"
!# annot keywords=(TAE,userid)

end-proc
.title
Returns current user ID
.help
 Program Description

 This procedure returns the current user name.

 Calling Sequence:

           TAE> local  userid  type=(string,12)

           TAE> username  userid
           TAE> disp userid

 Written By:            Glenn Dimit  (Nov. 15, 1984)
 Cognizant Programmer:  Glenn Dimit  (Nov. 15, 1984)
 Revisions:
 Made portable for UNIX ... V. Unruh ... (CRI) (Jan.  2, 1995)
.level1
.variable NAME
Output local vaiable name
.end
end-proc
$!-----------------------------------------------------------------------------
$ create username_vms.pdf
PROCESS 
PARM NAME NAME
END-PROC
$!-----------------------------------------------------------------------------
$ create username_unix.pdf
PROCESS 
PARM NAME NAME
END-PROC
$ Return
$!#############################################################################
$Test_File:
$ create tstusername.pdf
procedure
refgbl $echo
local userid  type=(string,12)
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
username userid
disp userid
end-proc
$ Return
$!#############################################################################

