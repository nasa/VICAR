$!****************************************************************************
$!
$! Build proc for MIPL module batch
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:47
$!
$! Execute by entering:		$ @batch
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
$ write sys$output "*** module batch ***"
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
$ write sys$output "Invalid argument given to batch.com file -- ", primary
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
$   if F$SEARCH("batch.imake") .nes. ""
$   then
$      vimake batch
$      purge batch.bld
$   else
$      if F$SEARCH("batch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake batch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @batch.bld "STD"
$   else
$      @batch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create batch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack batch.com -mixed -
	-s batch.c -
	-i batch.imake -
	-o batch.hlp -
	-t tbatch.f tbatch.imake tbatch.pdf tstbatch.scr
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create batch.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <zvproto.h>
int zbatch(void);

/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/

int FTN_NAME2(batch, BATCH) (void)
{
  return zbatch();
}

/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/

int zbatch(void)
{
   char runtype[32];
   int  count, batch;

   memset (runtype, 0, 32);

   zvp("$RUNTYPE", runtype, &count);

   if (strcmp(runtype,"INTERACTIVE") == 0)
      batch = 0;
   else if (strcmp(runtype,"BATCH") == 0)
      batch = 1;
   else if (strcmp(runtype,"ASYNC") == 0)
      batch = 2;
   else
      batch = -1;

   if (count != 1)
      batch = -1;

   return batch;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create batch.imake
#define SUBROUTINE batch

#define MODULE_LIST batch.c

#define USES_C

#define P2_SUBLIB
$ Return
$!#############################################################################
$Other_File:
$ create batch.hlp
VICAR subroutine BATCH

Purpose:

       Batch is a subroutine that returns an integer values indicating
   whether a TAE job is running interactively, asynchronously or in batch.


Operation:

       Batch uses zvp to obtain the value of the global variable
   $RUNTYPE and returns an integer depending on its value. Batch
   expects the $RUNTYPE global to be capitalized

  Returned values:   0  if  $RUNTYPE == INTERACTIVE
                     1  if  $RUNTYPE == BATCH
                     2  if  $RUNTYPE == ASYNC
                    99  if  $RUNTYPE has some undefined value

Calling sequence:

  Fortran:    call batch(i)

  C:          i = batch();

$ Return
$!#############################################################################
$Test_File:
$ create tbatch.f
c test pgm for function BATCH

      include 'VICMAIN_FOR'

      SUBROUTINE main44

      implicit integer (a-z)

      i = batch()
      if (i.eq.0) then
         call xvmessage(' Job is interactive', ' ')
      elseif (i.eq.1) then
         call xvmessage(' Job is batch', ' ')
      elseif (i.eq.2) then
         call xvmessage(' Job is asynchronous', ' ')
      else
         call xvmessage(' BATCH could not decide', ' ')
      endif

      return
      end
$!-----------------------------------------------------------------------------
$ create tbatch.imake

#define PROGRAM tbatch

#define MODULE_LIST tbatch.f

#define TEST

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$!-----------------------------------------------------------------------------
$ create tbatch.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstbatch.scr
! This is a SCRIPT file, not a PDF, because the commands must be run as
! separate processes and one must be a batch job.
!
! The output of this step should say it is interactive:
TBATCH
!
! The output of this step should say it is asynchronous:
TBATCH|RUN=ASYNC|
!
! This step should create a file called TBATCH.LOG which should state
! that the job is batch:
TBATCH|RUN=BATCH|
! 
$ Return
$!#############################################################################
