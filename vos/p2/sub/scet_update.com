$!****************************************************************************
$!
$! Build proc for MIPL module scet_update
$! VPACK Version 1.9, Monday, December 07, 2009, 16:35:22
$!
$! Execute by entering:		$ @scet_update
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
$ write sys$output "*** module scet_update ***"
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
$ write sys$output "Invalid argument given to scet_update.com file -- ", primary
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
$   if F$SEARCH("scet_update.imake") .nes. ""
$   then
$      vimake scet_update
$      purge scet_update.bld
$   else
$      if F$SEARCH("scet_update.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake scet_update
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @scet_update.bld "STD"
$   else
$      @scet_update.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create scet_update.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack scet_update.com -mixed -
	-s scet_update.c -
	-i scet_update.imake -
	-t tstscet_update.pdf -
	-o scet_update.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create scet_update.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/***********************************************************************/
/* SCET_UPDATE subroutine for detect/correct invalide SCET input.      */
/* Seet scet_update.hlp for detail.                                    */
/*                                                                     */
/* History:                                                            */
/*     Aug. 25, 1998  .T.Huang.   Initial release.                     */
/*                                                                     */
/***********************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "date_check.h"
#include <string.h>
#include <stdio.h>

int is_null ();
int time_checker ();

/* define FORTRAN interface */
void FTN_NAME2_(scet_update, SCET_UPDATE) (char *project, void *data, int *ind,
								ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length=5;

   zsfor2c(proj, length, project, &project, 3, 1, 1, ind);
   *ind = zscet_update (proj, data);
}


/* implementation of scet_update */
int zscet_update (project, data)
   char *project;
   int  data[80];
{
   int  ind;

   /* initialize temp time buffer */
   int temp[6];

/**
   zvmessage ("************* SCET_UPDATE Begin *************","");
   zvmessage ("scet_update::Checking SCET entry....","");
**/
   temp[0] = data[7];
   temp[1] = data[8];
   temp[2] = data[9];
   temp[3] = data[10];
   temp[4] = data[11];
   temp[5] = data[12];

   if (!is_null(temp))
   {
      /* checking and/update scet */
      if (time_checker (project,temp))
      {
         /* update input buffer scet entry */
         data[7] = temp[0];
         data[12] = temp[5];
      }
      else
         zabend ();
   }
   else
      zmabend ("scet_update::>>> Error, SCET cannot be NULL.");
/**  
   zvmessage ("",""); 
   zvmessage ("scet_update::Checking ERT entry.","");
**/
   temp[0] = data[65];
   temp[1] = data[66];
   temp[2] = data[67];
   temp[3] = data[68];
   temp[4] = data[69];
   temp[5] = -999;  /* NULL flag. ERT has no msec */

   /* allow ERT to be null */
   if (!is_null(temp))
   {
      temp[5] = 0;  /* set to zero, so date_check routine will ignor it */
      /* checking and/update scet, and allow for NULL date input */
      if (time_checker (project,temp))
         /* update input buffer ert entry */
         data[65] = temp[0];
      else
         zabend ();
   }
/**
   zvmessage ("************** SCET_UPDATE End **************","");
**/
   return 1;
}



/* subroutine to check for default date buffer 
   Return values:
      1  true
      0  false
*/
int is_null (tbuf)
   int tbuf[6];
{
   return (tbuf[0]==tbuf[1] &&
           tbuf[1]==tbuf[2] &&
           tbuf[2]==tbuf[3] &&
           tbuf[3]==tbuf[4] &&
           tbuf[4]==tbuf[5] &&
           tbuf[5]==-999);
}



/* subroutine to perform date checking and update when necessary. 
   Return values:
       1  Valid date
       0  Invalid date
*/
int time_checker (project,tbuf)
   char *project;
   int  tbuf[6];
{
   char msg[80];

   /* invoke date_check subroutine */
   if (!zchk_scet_date (tbuf))
      /* correct Voyager date */
      if (!strcmp(project,"VGR-1") || !strcmp(project,"VGR-2"))
      {
         sprintf (msg,"time_checker::Correcting %s year input...", project);
/**
         zvmessage (msg,"");
**/
         /* check for year between 2000 -- 2074 */
         if (tbuf[0] < 75 && tbuf[0] >= 0)
         {
            tbuf[0] = tbuf[0] + 2000;
            tbuf[5] = 0;
            /* performs final check on the corrected date */
            if (!zchk_scet_date (tbuf)) return 0;
         }

         else if (tbuf[0] > 75 && tbuf[0] < 100)
         {
            /* check for year between 1975 -- 1999 */
            tbuf[0] = tbuf[0] + 1900;
            tbuf[5] = 0;
            /* performs final check on the corrected date */
            if (!zchk_scet_date (tbuf)) return 0;
         }

         else
         {
            sprintf (msg, 
               "time_checker::>>> Invalid %s date format!!!", project);
/**
            zvmessage (msg,"");
**/
            return 0;
         }
      }

      else
      {
         sprintf (msg, "time_checker::>>> Invalid %s date format!!!", project);
         zvmessage (msg,"");
         return 0;
      }
   return 1;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create scet_update.imake
/* Imake file for VICAR subroutine SCET_UPDATE */
#define SUBROUTINE scet_update

#define MODULE_LIST  scet_update.c
#define USES_ANSI_C
#define P2_SUBLIB

$ Return
$!#############################################################################
$Test_File:
$ create tstscet_update.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
local path1 type=string init="wms_test_work:[testdata.mipl.vgr]"
local path2 type=string init="wms_test_work:[testdata.mipl.gll]"
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! TEST SCRIPT FOR THE PROGRAM gspice
if ($syschar(1) = "UNIX")
    let path1="/project/test_work/testdata/mipl/vgr/"
    let path2="/project/test_work/testdata/mipl/gll/"
end-if

write "Testing C-bridge:: getpc->getspice2->getlabcon->scet_update for VGR"
write "   VGR-1 Test "
getpc inp=&"path1"f1636832.fic target=io spicemode=remote

write " "
write "   VGR-2 Test"
getpc inp=&"path1"uvh.img target=io spicemode=remote

write " "
write "   GLL Test"
getpc inp=&"path2"venus.img spicemode=remote

write ""
write ""
write "Testing Fortran-bridge:: gspice->getspice3->getspice4->scet_update"
gspice spacecraft=gll target=venus scet=(1990,44,5,58,16,962) +
    camera=1 ckname=fare spicemode=remote

write ""
write "   GLL Test"
gspice inp=&"path2"venus.img +
    ckname=fare spicemode=remote

write ""
write "   VGR-1 Test"
gspice spacecraft=vgr-1 camera=7 target=jupiter scet=(79,58,7,42,59,0) +
    spicemode=remote

write ""
write "   VGR-1 Test"
gspice inp=&"path1"f1636832.fic +
   target=io spicemode=remote

write ""
write "   VGR-2 Test"
gspice inp=&"path1"uvh.img +
   target=io spicemode=remote

write ""
write ""
write "**** The following case will ABEND ****"
write ""
write "More than one problems with VGR scet date"
gspice spacecraft=vgr-1 camera=7 target=jupiter scet=(79,58,7,42,999,0) +
    spicemode=remote

write ""
write "Invalid GLL scet date"
gspice spacecraft=gll target=venus scet=(90,44,5,58,16,962) +
    camera=1 ckname=fare spicemode=remote

end-proc

$ Return
$!#############################################################################
$Other_File:
$ create scet_update.hlp
1 SCET_UPDATE

SCET_UPDATE is designed to assist in detect and/or correct invalid
Spacecraft Event Time (SCET) and Earth Receive Time (ERT) in a SPICE 
data buffer.  It does so by invoking DATE_CHECK subroutines to detect 
invalid date format.  For VGR-1 and VGR-2, the subroutine will attempt 
to convert the 2-digit year input into 4-digit using 100-year sliding 
window.  It also sets the MSEC field to 0, because this field is set to 
-999 for Voyager.

Design Logics:

   For SCET check and update:
      IF scet is not NULL (i.e. -999) THEN
         IF invalid scet format THEN
            IF project is Voyager THEN
               use 75 as the switch for 100-year window to update scet
               IF still has invalid format THEN
                  ABEND
            ELSE
              ABEND
      ELSE
         ABEND

   For ERT check and update:
      Same as SCET check and update, but it will not ABEND if the input
      date is NULL.


FORTRAN calling sequence:

   CHARACTER*5 PROJECT
   INTEGER*4 DATA[80] ! GETLABCON buffer. 
   INTEGER*4 IND  !return status = 1 when successful

   ...
   CALL SCET_UPDATE (PROJECT,DATA,IND)
   ...


C calling sequence:

   char project[5];
   int  data[80];  /* zgetlabcon buffer */
   int  ind;  /* return status = 1 when successful */

   ...
   ind = zscet_update (project,data);
   ...


2 History

   Original Programmer: Thomas Huang, August 25, 1998
   Source Lanugage: C
   Revision history: New

   October 29, 1998            Removed the 'zvmessage' calls.

$ Return
$!#############################################################################
