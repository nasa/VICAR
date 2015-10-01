$!****************************************************************************
$!
$! Build proc for MIPL module convisos
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:21
$!
$! Execute by entering:		$ @convisos
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
$ write sys$output "*** module convisos ***"
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
$ write sys$output "Invalid argument given to convisos.com file -- ", primary
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
$   if F$SEARCH("convisos.imake") .nes. ""
$   then
$      vimake convisos
$      purge convisos.bld
$   else
$      if F$SEARCH("convisos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake convisos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @convisos.bld "STD"
$   else
$      @convisos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create convisos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack convisos.com -mixed -
	-s convisos.c -
	-i convisos.imake -
	-t tconvisos.f tzconvisos.c tconvisos.imake tconvisos.pdf -
	   tstconvisos.pdf -
	-o convisos.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create convisos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*---------------------------  convisos     ------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <zvproto.h>

int zgllgcor(float* is_line,float* is_samp,float* os_line,float* os_samp,
	     int mode,int icam);
int zcasgcor(float* is_line,float* is_samp,float* os_line,float* os_samp,
	     int mode,int icam);
void ztritra(int* ind,float* conv, int nph,int npv,float* line,
	     float* samp,int mode);

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zconvisos(project,icam,is_line,is_samp,os_line,os_samp,
          mode,conv,nph,npv,ind)
char *project;
float *is_line,*is_samp, *os_line,*os_samp;
float *conv;
int   icam,mode,nph,npv,*ind; 
{
/*  ==================================================================  */
    *ind = 0;
    if (strncmp(project, "GLL", 3) == 0)
    {
        zgllgcor(is_line,is_samp,os_line,os_samp,mode,icam);
    }
    else if (strncmp(project, "CAS", 3) == 0)
    {
        zcasgcor(is_line,is_samp,os_line,os_samp,mode,icam);
    }
    else
    {
        if (mode == 0)
        {
          *is_line= *os_line;               /*  OS to IS  */
          *is_samp= *os_samp;
          ztritra(ind,conv,nph,npv,is_line,is_samp,mode);
        }
        else
        {
          *os_line= *is_line;              /*  IS to OS  */
          *os_samp= *is_samp;
          ztritra(ind,conv,nph,npv,os_line,os_samp,mode);
        }
    }
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(convisos, CONVISOS) (char *project, int *icam,
	float *is_line, float *is_samp, float *os_line, float *os_samp,
	int *mode, float *conv, int *nph, int *npv, int *ind, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length;
/*  ==================================================================  */

   length = 5;

/* 11 args for convisos, project is 1st arg and 1st string   */

   zsfor2c(proj,length,project,&project,11,1,1, ind); 

   zconvisos(proj,*icam,is_line,is_samp,os_line,os_samp,
              *mode,conv,*nph,*npv,ind);

}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create convisos.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY convisos

   To Create the build file give the command:

	$ vimake convisos                     (VMS)
   or
	% vimake convisos                     (Unix)


*************************************************************************/

#define SUBROUTINE convisos

#define MODULE_LIST convisos.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tconvisos.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER ICAM,MODE,NPV,NPH,IND,IPTR
      REAL IS_LINE, IS_SAMP, OS_LINE, OS_SAMP
      REAL RES(2,202),CONV(2216)
      CHARACTER*5 PROJECT

C  TEST1 FOR convisos
      CALL XVMESSAGE('TEST1 FOR convisos', ' ')
      PROJECT = 'VGR-2'
      ICAM = 7
      CALL GETRES(res,ICAM)
      CALL GEOMAV(conv,ICAM,RES)
      NPH = 24
      NPV = 23
      IND = 0
      IPTR=0
      MODE = 1
      IS_LINE = 400.
      IS_SAMP = 400.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL CONVISOS(PROJECT,ICAM,is_line,is_samp,os_line,os_samp,
     +                                  MODE,CONV(9),NPH,NPV,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

C  TEST2 FOR convisos

      CALL XVMESSAGE('TEST2 FOR convisos', ' ')
      OS_LINE = 498.56
      OS_SAMP = 498.59
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(9),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')

C  TEST3 FOR convisos

      CALL XVMESSAGE('TEST3 FOR convisos', ' ')
      IND = 0
      PROJECT = 'GLL'
      ICAM = 1
      MODE = 1
      IS_LINE = -100.
      IS_SAMP =    1.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

      CALL XVMESSAGE('TEST4 FOR convisos', ' ')
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')

C  TEST5 FOR convisos

      CALL XVMESSAGE('TEST5 FOR convisos', ' ')
      IND = 0
      PROJECT = 'CAS'
      ICAM = 2 
      MODE = 1
      IS_LINE =    1.
      IS_SAMP =    1.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

      CALL XVMESSAGE('TEST6 FOR convisos', ' ')
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')


      CALL XVMESSAGE(
     . 'Repeat TEST1 in C to test C interface: zconvisos', ' ')

      call tzconvisos(CONV(9))

      return
      END
$!-----------------------------------------------------------------------------
$ create tzconvisos.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzconvisos)(conv) 
float *conv;
{
      int icam,nph, npv, ind, mode;
      float is_line, is_samp, os_line, os_samp;
/*  ==================================================================  */

      icam = 7;
      nph = 24;
      npv = 23;
      ind = 0;
      is_line = 400.;
      is_samp = 400.;
      mode = 1;
      zprnt(7,1,&is_line,"IS_LINE =.");
      zprnt(7,1,&is_samp," IS_SAMP =.");
      zconvisos("VGR-2",icam,&is_line,&is_samp,&os_line,&os_samp,
                                    mode,conv,nph,npv,&ind);
      zprnt(7,1,&os_line," OS_LINE =.");
      zprnt(7,1,&os_samp," OS_SAMP =.");

}
$!-----------------------------------------------------------------------------
$ create tconvisos.imake
/* Imake file for Test of VICAR subroutine convisos */

#define PROGRAM tconvisos

#define MODULE_LIST tconvisos.f tzconvisos.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
#define LIB_MATH77
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tconvisos.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstconvisos.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tconvisos
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create convisos.hlp
1 CONVISOS    (CONVert Image Space - Object Space)

 PURPOSE:  Convert between image & object space coordinates.  This routine is
  applicable to all flight projects.

 FORTRAN Calling Sequence:

  CHARACTER*5 PROJECT 		!GLL,VGR-1,VGR-2,etc. (blank filled)
  INTEGER*4 ICAM		!Camera serial number
  REAL*4 IS_LINE,IS_SAMP	!Image space line-sample coordinates
  REAL*4 OS_LINE,OS_SAMP	!Object space line-sample coordinates
  INTEGER*4 MODE		!0=OS to IS, 1=IS to OS
  REAL*4 CONV(4,NPH,NPV)	!GEOMA tiepoint pairs
  INTEGER*4 NPH			!Number of tieponts/row
  INTEGER*4 NPV			!Number of tiepoints/column
  INTEGER*4 IND			!0=normal return, 1=abnormal return

  CALL CONVISOS(PROJECT,ICAM,is_line,is_samp,os_line,os_samp,
	MODE,CONV,NPH,NPV,ind)


 C Calling Sequence:

  char project[6];		/* GLL,VGR-1,VGR-2,etc. (null terminated)*/
  int icam;			/* Camera serial number			 */
  float is_line,is_samp;	/* Image space line,sample coordinates   */
  float os_line,os_samp;	/* Object space line,sample coordinates  */
  int mode;			/* 0 = OS to IS, 1 = IS to OS		 */
  float conv[npv][nph][4];	/* GEOMA tiepoint pairs			 */
  int nph;			/* Number of tiepoints/row		 */
  int npv;			/* Number of tiepoints/column		 */
  int ind;			/* 0=normal return, 1=abnormal return	 */

  zconvisos(project,icam,&is_line,&is_samp,&os_line,&os_samp,
	mode,conv,nph,npv,&ind);
 
2 Notes on argument list
  
  (IS_LINE,IS_SAMP) and (OS_LINE,OS_SAMP) are inputs or outputs depending
  on the value of MODE.  IND is output.  The remaining arguments are inputs.

  If GEOMAV is used to create CONV, the tiepoint pairs begin with word #9.

  PROJECT and ICAM are the values returned by GETPROJ.
  Valid values for PROJECT are CAS, GLL, VGR-1, VGR-2, VIKOR, MAR10, and MAR-9.
  If PROJECT is not CAS or GLL, ICAM is ignored.
  If PROJECT=GLL or CAS, CONV, NPH, NPV are ignored and IND is always 0.

  If IND=1 a warning message will be printed.

2 OPERATION

  CONVISOS converts image line-sample values from raw (image space) coordinates
  to geometrically corrected (object space) coordinates, or vice-versa.

  CONVISOS calls GLLGCOR for project=GLL, calls CASGCOR for project=CAS,
  but calls TRITRA for all other flight projects.

2 History

 Written By: Jean Lorre        10/1/89
 Cognizant Programmer: J Lorre
 Ported to UNIX: Steve Pohorsky
 Source Language:       C

 Revisions:
   05 Nov 01  VRH  Added Cassini.
   02 Sep 96  GMY  Added ICAM argument to handle SSI summation mode.
   1-15-93  ..SP....  Made portable for UNIX - converted from Fortran
                      to C ala VICAR Porting Guide to accomodate character
                      string argument.
$ Return
$!#############################################################################
