$!****************************************************************************
$!
$! Build proc for MIPL module logo
$! VPACK Version 1.9, Monday, December 07, 2009, 16:26:04
$!
$! Execute by entering:		$ @logo
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
$ write sys$output "*** module logo ***"
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
$ write sys$output "Invalid argument given to logo.com file -- ", primary
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
$   if F$SEARCH("logo.imake") .nes. ""
$   then
$      vimake logo
$      purge logo.bld
$   else
$      if F$SEARCH("logo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake logo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @logo.bld "STD"
$   else
$      @logo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create logo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack logo.com -mixed -
	-s logo.c -
	-i logo.imake -
	-t tlogo.f tzlogo.c tlogo.imake tlogo.pdf tstlogo.pdf -
	-o logo.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create logo.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            LOGO

     24 MAY 83   ...JHR...  INITIAL RELEASE
     01 NOV 88   ...SP....  CHANGED TO USE NEW JPL LOGO.

   THIS ROUTINE GENERATES LOGOS FOR USE WITH MASK PROGRAMS.
   ONE LINE IS GENERATED EACH TIME THE ROUTINE IS CALLED.
   THE IMAGE SIZE OCCUPIED BY EACH LOGO IS 64 LINES BY 64 SAMPLES.
   THE LOGOS ARE STORED HERE SUCH THAT EACH BYTE IS REPRESENTED BY
   A SINGLE BIT.

   ARGUMENTS IN THE CALLING SEQUENCE ARE:
     IDN     THE DN VALUE FOR THE LOGO
     LINE    THE RELATIVE LINE NUMBER (I.E., 1 - 64)
     ILOGO   SPECIFIES THE LOGO TO BE USED
             1 = JPL
             2 = MICKEY MOUSE
             3 = GODDARD
             4 = NASA
     BGR     RESERVED FOR POSSIBLE FUTURE COLOR LOGOS
             0 = BLACK AND WHITE
             1 = BLUE
             2 = GREEN
             3 = RED
     BUF     BUFFER IN WHICH LOGO IS RETURNED (BYTE)
*/    
#include "xvmaininc.h"
#include "ftnbridge.h"


/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(logo, LOGO) (idn,line,ilogo,bgr,buf)
     int *idn, *line, *ilogo, *bgr;
     unsigned char *buf;
{
   zlogo( *idn, *line, *ilogo, *bgr, buf);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zlogo(idn,line,ilogo,bgr,buf)
     int idn, line, ilogo, bgr;
     unsigned char *buf;
{
  int ibit,  ibyte, inbyte, ip2, obyte;
  static int ip[8] = { 1,2,4,8,16,32,64,128};
  unsigned char *pts;

/*    *** DATA FOR JPL LOGO ***  */

  static unsigned char pts1[64][8] = {
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0X0F,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0X8F,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XC0,0X0F,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XC0,0X07,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XC0,0X07,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XC0,0X0F,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XFF,0XFF,0XCF,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XDF,0XFF,0X8F,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XCF,0XFF,0X0F,0XC0,0X00 },
	{ 0X00,0X07,0XE7,0XC7,0XFC,0X0F,0XC0,0X00 },
	{ 0X00,0X0F,0XE7,0XC0,0X00,0X0F,0XC0,0X00 },
	{ 0X0F,0XFF,0XE7,0XC0,0X00,0X0F,0XFF,0XF0 },
	{ 0X1F,0XFF,0XC7,0XC0,0X00,0X0F,0XFF,0XF8 },
	{ 0X3F,0XFF,0X87,0XC0,0X00,0X07,0XFF,0XFC },
	{ 0X7F,0XFF,0X07,0XC0,0X00,0X03,0XFF,0XFE },
	{ 0XFF,0XFE,0X07,0XC0,0X00,0X01,0XFF,0XFF },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 }
  };

/*    *** DATA FOR MICKEY MOUSE LOGO ***  */

  static unsigned char pts2[64][8] = {
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X1E,0X00, 0X00 },
	{ 0X00, 0X00, 0XF8,0X00, 0X00, 0X7F,0XC0,0X00 },
	{ 0X00,0X03,0XFE,0X00, 0X00, 0XFF,0XF0,0X00 },
	{ 0X00,0X0F,0XFF,0X00,0X01,0XFF,0XF8,0X00 },
	{ 0X00,0X1F,0XFF,0X80,0X03,0XFF,0XFC,0X00 },
	{ 0X00,0X3F,0XFF,0X80,0X07,0XFF,0XFC,0X00 },
	{ 0X00,0X7F,0XFF,0XC0,0X07,0XFF,0XFE,0X00 },
	{ 0X00,0X7F,0XFF,0XC0,0X07,0XFF,0XFE,0X00 },
	{ 0X00,0X7F,0XFF,0XC0,0X07,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0XC0,0X07,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0XC0,0X07,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0XCF,0XE7,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0XFE,0X7F,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0XF8,0X3F,0XFF,0XFE,0X00 },
	{ 0X00,0XFF,0XFF,0X00,0X00,0XFF,0XFC,0X00 },
	{ 0X00,0XFF,0XFE,0X7C,0X7C,0X7F,0XFC,0X00 },
	{ 0X00,0X7F,0XFC,0X44,0X44,0X3F,0XF8,0X00 },
	{ 0X00,0X3F,0XFC,0XD8,0X36,0X3F,0XE0,0X00 },
	{ 0X00,0X1F,0XFC,0XAC,0XCA,0X1F,0XC0,0X00 },
	{ 0X00,0X0F,0XF8,0X42,0X84,0X1E,0X00, 0X00 },
	{ 0X00, 0X00, 0X78,0X42,0X84,0X1E,0X00, 0X00 },
	{ 0X00, 0X00, 0X78,0X42,0X84,0X0F,0X00, 0X00 },
	{ 0X00, 0X00, 0XF8,0X42,0X84,0X0F,0X00, 0X00 },
	{ 0X00, 0X00, 0XF8,0X4C,0X64,0X1F,0X00, 0X00 },
	{ 0X00, 0X00, 0XF8,0X5F,0XF4,0X1F,0X80,0X00 },
	{ 0X00,0X01,0XF8,0X5E,0XF4,0X1F,0X80,0X00 },
	{ 0X00,0X01,0XFC,0X3C,0X78,0X3F,0X80,0X00 },
	{ 0X00,0X01,0XFC,0X1B,0XB0,0X3F,0X80,0X00 },
	{ 0X00,0X01,0XE4,0X37,0XD8,0X27,0X80,0X00 },
	{ 0X00,0X03,0X80,0X6F,0XEC,0X00,0XC0,0X00 },
	{ 0X00,0X03,0X00,0X5F,0XF4,0X60,0XC0,0X00 },
	{ 0X00,0X02,0X0F,0X1F,0XF0,0X70,0X40,0X00 },
	{ 0X00,0X02,0X1F,0X1F,0XF0,0X78,0X40,0X00 },
	{ 0X00,0X02,0X1B,0X07,0XC0,0XC8,0X80,0X00 },
	{ 0X00,0X02,0X13,0X80,0X01,0X80,0X80,0X00 },
	{ 0X00,0X02,0X01,0X40,0X02,0X81,0X80,0X00 },
	{ 0X00,0X01,0X00,0XB0,0X0D,0X01,0X00,0X00 },
	{ 0X00,0X00,0X80,0X9F,0XF3,0X02,0X00,0X00 },
	{ 0X00,0X00,0X40,0X40,0X02,0X04,0X00,0X00 },
	{ 0X00,0X00,0X20,0X5F,0XF4,0X18,0X00,0X00 },
	{ 0X00,0X00,0X18,0XBF,0XFA,0X20,0X00,0X00 },
	{ 0X00,0X00,0X06,0XDF,0XF6,0XC0,0X00,0X00 },
	{ 0X00,0X00,0X01,0XCF,0XEF,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X63,0X98,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X30,0X30,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X0F,0XC0,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 }
  };

/*    *** DATA FOR GODDARD LOGO ***  */

  static unsigned char pts3[64][8] = {
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X80,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X80,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X80,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X80,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X01,0XC0,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X01,0XC0,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X03,0XE0,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X05,0XD0,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X09,0XC8,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X84,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X11,0XC4,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X10,0X04,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X7F,0XFF,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X7F,0XFF,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X50,0X05,0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0XD0,0X05,0X80,0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0XD0,0X05,0X80,0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X90,0X04,0X80,0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X90,0X04,0X80,0X00, 0X00 },
	{ 0X00, 0X00, 0X01,0X90,0X04,0XC0,0X00, 0X00 },
	{ 0X00, 0X00, 0X01,0X90,0X04,0XC0,0X00, 0X00 },
	{ 0X00, 0X00, 0X01,0X90,0X04,0XC0,0X00, 0X00 },
	{ 0X00, 0X00, 0X01,0X10,0X04,0X40,0X00, 0X00 },
	{ 0X00, 0X00, 0X03,0X10,0X04,0X60,0X00, 0X00 },
	{ 0X00, 0X00, 0X03,0X10,0X04,0X60,0X00, 0X00 },
	{ 0X00, 0X00, 0X03,0X10,0X84,0X60,0X00, 0X00 },
	{ 0X00, 0X00, 0X02,0X11,0XC4,0X20,0X00, 0X00 },
	{ 0X00, 0X00, 0X06,0X13,0XE4,0X30,0X00, 0X00 },
	{ 0X00, 0X00, 0X06,0X17,0XF4,0X30,0X00, 0X00 },
	{ 0X00, 0X00, 0X06,0X13,0XE4,0X30,0X00, 0X00 },
	{ 0X00, 0X00, 0X04,0X1F,0XFC,0X10,0X00, 0X00 },
	{ 0X00, 0X00, 0X0C,0X03,0XE0,0X18,0X00, 0X00 },
	{ 0X00, 0X00, 0X0C,0X03,0XE0,0X18,0X00, 0X00 },
	{ 0X00, 0X00, 0X0C,0X03,0XE0,0X18,0X00, 0X00 },
	{ 0X00, 0X00, 0X08,0X03,0XE0,0X08,0X00, 0X00 },
	{ 0X00, 0X00, 0X18,0X01,0XC0,0X0C,0X00, 0X00 },
	{ 0X00, 0X00, 0X18,0X01,0XC0,0X0C,0X00, 0X00 },
	{ 0X00, 0X00, 0X18,0X01,0XC0,0X0C,0X00, 0X00 },
	{ 0X00, 0X00, 0X10,0X01,0XC0,0X04,0X00, 0X00 },
	{ 0X00, 0X00, 0X30,0X01,0XC0,0X06,0X00, 0X00 },
	{ 0X00, 0X00, 0X30,0X01,0XC0,0X06,0X00, 0X00 },
	{ 0X00, 0X00, 0X30,0X00,0X00,0X06,0X00, 0X00 },
	{ 0X00, 0X00, 0X20,0X00,0X00,0X02,0X00, 0X00 },
	{ 0X00, 0X00, 0X60,0X00,0X00,0X03,0X00, 0X00 },
	{ 0X00, 0X00, 0X63,0XCE,0X7C,0XE3,0X00, 0X00 },
	{ 0X00, 0X00, 0X64,0X11,0X41,0X13,0X00, 0X00 },
	{ 0X00, 0X00, 0X64,0X08,0X41,0X03,0X00, 0X00 },
	{ 0X00, 0X00, 0X64,0XC4,0X71,0X03,0X00, 0X00 },
	{ 0X00, 0X00, 0X64,0X42,0X41,0X03,0X00, 0X00 },
	{ 0X00, 0X00, 0X64,0X51,0X41,0X13,0X00, 0X00 },
	{ 0X00, 0X00, 0X63,0XCE,0X40,0XE3,0X00, 0X00 },
	{ 0X00, 0X00, 0X60,0X00,0X00,0X03,0X00, 0X00 },
	{ 0X00, 0X00, 0X7F,0XFF,0XFF,0XFF,0X00, 0X00 },
	{ 0X00, 0X00, 0X7F,0XFF,0XFF,0XFF,0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 }
  };

/*    *** DATA FOR NASA LOGO ***  */

  static unsigned char pts4[64][8] = {
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X1C,0X0F,0X01,0XC0,0X0F,0XFC,0X03,0X80 },
	{ 0X3E,0X0F,0X03,0XE0,0X3F,0XFC,0X07,0XC0 },
	{ 0X7F,0X0F,0X07,0XF0,0X7F,0XFC,0X0F,0XE0 },
	{ 0XFF,0X8F,0X07,0XF0,0XFF,0XFC,0X0F,0XE0 },
	{ 0XF7,0X8F,0X0F,0X78,0XF8,0X00,0X1E,0XF0 },
	{ 0XF7,0X8F,0X0F,0X78,0XF0,0X00,0X1E,0XF0 },
	{ 0XF7,0X8F,0X0F,0X78,0XF0,0X00,0X1E,0XF0 },
	{ 0XF3,0XCF,0X0F,0X78,0XF8,0X00,0X1E,0XF0 },
	{ 0XF3,0XCF,0X1E,0X3C,0XFF,0XE0,0X3C,0X78 },
	{ 0XF3,0XCF,0X1E,0X3C,0X7F,0XF8,0X3C,0X78 },
	{ 0XF3,0XCF,0X1E,0X3C,0X3F,0XFC,0X3C,0X78 },
	{ 0XF3,0XCF,0X1E,0X3C,0X0F,0XFE,0X3C,0X78 },
	{ 0XF3,0XCF,0X3C,0X1E,0X00,0X3E,0X78,0X3C },
	{ 0XF1,0XEF,0X3C,0X1E,0X00,0X1E,0X78,0X3C },
	{ 0XF1,0XEF,0X3C,0X1E,0X00,0X1E,0X78,0X3C },
	{ 0XF1,0XEF,0X3C,0X1E,0X00,0X3E,0X78,0X3C },
	{ 0XF1,0XFF,0X78,0X0F,0XFF,0XFE,0XF0,0X1E },
	{ 0XF0,0XFE,0X78,0X0F,0XFF,0XFC,0XF0,0X1E },
	{ 0XF0,0X7C,0X78,0X0F,0XFF,0XF8,0XF0,0X1E },
	{ 0XF0,0X38,0X78,0X0F,0XFF,0XE0,0XF0,0X1E },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 },
	{ 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00, 0X00 }
  };

/*  ==================================================================  */  

  if (line < 1 || line >64)  zmabend("LOGO: invalid line value");
  if (bgr < 0)               zmabend("LOGO: invalid BGR value");

  switch (ilogo) {
  case 1:
    pts = pts1[line-1];  /*  pointer to data for given line & given logo  */
    break;
  case 2:
    pts = pts2[line-1];  /*  pointer to data for given line & given logo  */
    break;
  case 3:
    pts = pts3[line-1];  /*  pointer to data for given line & given logo  */
    break;
  case 4:
    pts = pts4[line-1];  /*  pointer to data for given line & given logo  */
    break;
  default:    
    zvmessage("*** LOGO: invalid ilogo value","");
    zabend();
    break;
  }

  obyte = 0;

  for (ibyte=0; ibyte<8; ibyte++ ) {
      inbyte = *pts++;    /*  grab byte from bit map  */
      for (ibit=0; ibit<8; ibit++ ) {
	ip2 = ip[7-ibit];
	if (inbyte >= ip2) {     /*  test for bit ON  */
	   buf[obyte] = idn;     /*  store value in output buffer  */
           inbyte = inbyte - ip2;  /*  subtract value of bit that was ON  */
	}
        obyte++;
      }
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create logo.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY logo

   To Create the build file give the command:

	$ vimake logo                     (VMS)
   or
	% vimake logo                     (Unix)


*************************************************************************/

#define SUBROUTINE logo

#define MODULE_LIST logo.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tlogo.f
C TO TEST SUBROUTINE LOGO
C TO TEST JPL LOGO WITH IDN=255
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      BYTE BUF(64)
      LOGICAL  XVPTST
      INTEGER ICODE

      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','O_FORMAT','BYTE',
     &            'U_FORMAT','BYTE','U_NL',64,'U_NS',64,' ')

      IF(XVPTST('JPL'))  ICODE=1
      IF(XVPTST('MICK')) ICODE=2
      IF(XVPTST('GODD')) ICODE=3
      IF(XVPTST('NASA')) ICODE=4

      DO I=1,64
         CALL ITLA(0,BUF,64)
         IF ( MOD(I,2) .EQ. 1) THEN
           CALL LOGO(255,I,ICODE,0,BUF)    ! ALTERNATE BETWEEN TRYING FROM
         ELSE 
           CALL TZLOGO(255,I,ICODE,0,BUF)  ! FORTRAN AND FROM C.
         END IF
         CALL XVWRIT(OUNIT,BUF,STAT,'NSAMPS',64,' ')
      ENDDO

      CALL XVCLOSE(OUNIT,STAT,' ')

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzlogo.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/*  To test zlogo from C, we virtually use the same code as the Fortran
    bridge in logo.c  */

void FTN_NAME(tzlogo)(idn,line,ilogo,bgr,buf)
     int *idn, *line, *ilogo, *bgr;
     unsigned char *buf;
{
   zlogo( *idn, *line, *ilogo, *bgr, buf);
}
$!-----------------------------------------------------------------------------
$ create tlogo.imake
/* Imake file for Test of VICAR subroutine logo */

#define PROGRAM tlogo

#define MODULE_LIST tlogo.f tzlogo.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tlogo.pdf
 PROCESS
 PARM OUT TYPE=STRING
 PARM NL TYPE=INTEGER DEFAULT=0
 PARM NS TYPE=INTEGER DEFAULT=0
 PARM LOGO KEYWORD VALID=(JPL,MICK,GODD,NASA)
 END-PROC
$!-----------------------------------------------------------------------------
$ create tstlogo.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
! THIS IS A TEST OF SUBROUTINE LOGO.
! LOGO GENERATES LOGO DISPLAYS FOR USE WITH MASK PROGAMS.
! ONE LINE IS GENERATED EACH TIME THE ROUTINE IS CALLED. 
! EACH LOGO OCCUPIES A 64 LINE BY 64 SAMPLE AREA.  
! THE LOGOS ARE STORED HERE SUCH THAT EACH BYTE IS REPRESENTED 
! BY A SINGLE BIT.
! CALL LOGO(IDN,LINE,ILOGO,BGR,BUF)
! WHERE
! IDN IS THE DN VALUE OF THE DISPLAYED LOGO (I.E. 0-255).
! LINE IS THE RELATIVE LINE NUMBER (I.E. 1-64)
! ILOGOS: SPECIFIES THE DESIRED LOGOS:.
!        1=JPL
!        2=MICKEY MOUSE
!        3=GODDARD
!        4=NASA
! BGR IS RESERVED FOR POSSIBLE FUTURE COLOR LOGOS:
!        0=BLACK
!        1=BLUE
!        2=GREEN
!        3=RED
!  BUF IS THE BUFFER IN WHICH THE REQUESTED LOGO LINE IS 
! RETURNED (LOGICAL *1).  IT MUST BE AT LEAST 64 BYTES IN LENGTH.
! TEST THE JPL LOGO WITH IDN=255.
tlogo A 64 64 'JPL
list A (1,1,64,30) 'ZERO
list A (1,31,64,30) 'ZERO
list A (1,61,64,4) 'ZERO
tlogo A 64 64 'MICK
list A (1,1,64,30) 'ZERO
list A (1,31,64,30) 'ZERO
list A (1,61,64,4) 'ZERO
tlogo A 64 64 'GODD
list A (1,1,64,30) 'ZERO
list A (1,31,64,30) 'ZERO
list A (1,61,64,4) 'ZERO
tlogo A 64 64 'NASA
list A (1,1,64,30) 'ZERO
list A (1,31,64,30) 'ZERO
list A (1,61,64,4) 'ZERO
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create logo.hlp
1  LOGO

 PURPOSE: LOGO generates logo displays for use in masking programs.

 Fortran calling sequence:  CALL LOGO(IDN,LINE,ILOGO,BGR,BUF)
 C Calling Sequence:        zlogo(idn,line,ilogo,bgr,buf);
 
  ( idn, line, ilogo, and bgr are passed by value for zlogo.)


 ARGUMENTS:  IDN, LINE, ILOGO, and BGR are input integers.
             BUF is an output buffer of type BYTE.

     IDN     THE DN VALUE FOR THE LOGO

     LINE    THE RELATIVE LINE NUMBER (I.E., 1 - 64)

     ILOGO   SPECIFIES THE LOGO TO BE USED
             1 = JPL
             2 = MICKEY MOUSE
             3 = GODDARD
             4 = NASA (worm logo)

     BGR     RESERVED FOR POSSIBLE FUTURE COLOR LOGOS
             IT SHOULD BE SET AT ZERO AT THE PRESENT TIME
             FUTURE VALUES ARE:
             0 = BLACK AND WHITE
             1 = BLUE
             2 = GREEN
             3 = RED

     BUF     BUFFER IN WHICH LOGO IS RETURNED (BYTE).
             IT MUST BE AT LEAST 64 BYTES IN LENGTH.

2  HISTORY

  Original Programmer: A. Gillespie
  Ported to UNIX:       Steve Pohorsky
  Source Language: C
  Current Cog Progr:    Steve Pohorsky

 REVISION HISTORY:                                          
   6-16-93  ..SP....  Made portable for UNIX.  Converted to C because
                      bit maps should be in hex, which ANSI Fortran
                      does not support.

2  OPERATION

     ALL LOGOS ARE STORED IN LOGO IN PACKED FORMAT, EACH BIT REPRESENTING A 
     BYTE (PIXEL).  FOR EACH CALL TO THE SUBROUTINE, THE SPECIFIED LINE OF THE 
     SPECIFIED LOGO IS UNPACKED FROM 8 BYTES TO THE OUTPUT SIZE OF 64 BYTES.
     ON BITS CAUSE THE CORRESPONDING BYTE IN BUF TO BE SET TO THE VALUE SPEC-
     FIED BY THE DN ARGUMENT WHILE OFF BITS CAUSE NO CHANGE TO THE CORRES-
     PONDING BYTE IN BUF.

2  EXAMPLE

      INTEGER OUNIT
      BYTE  BUF(64)
      CALL XVUNIT(OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','O_FORMAT','BYTE',
     &            'U_NL',NLO,'U_NS',NSO, ' ')
      DO I=1,64
         CALL ITLA(0,BUF,64)
         CALL LOGO(255,I,1,0,BUF)   ! JPL LOGO AT 255 DN.
         CALL XVWRIT(OUNIT,BUF,STAT,'NSAMPS',64, ' ')
      END DO
      CALL XVCLOSE(OUNIT,STAT, ' ')


$ Return
$!#############################################################################
