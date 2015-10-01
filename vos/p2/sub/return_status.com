$!****************************************************************************
$!
$! Build proc for MIPL module return_status
$! VPACK Version 1.9, Thursday, July 13, 2000, 16:29:04
$!
$! Execute by entering:		$ @return_status
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
$ write sys$output "*** module return_status ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to return_status.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("return_status.imake") .nes. ""
$   then
$      vimake return_status
$      purge return_status.bld
$   else
$      if F$SEARCH("return_status.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake return_status
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @return_status.bld "STD"
$   else
$      @return_status.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create return_status.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack return_status.com -
	-s return_status.c -
	-i return_status.imake -
	-t tst_return_status.c tst_return_status.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create return_status.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <string.h>

#include "return_status.h"

typedef	struct	{	char	*FullName;
			char	*Nmemonic;
			int	Value;
		} Tbl_typ;

static Tbl_typ	Codes[] = {
	{"NORMAL",		"N",		RTN_C_NORMAL },
	{"WARNING",		"W",		RTN_C_WARNING },
	{"INFO",		"I",		RTN_C_INFO },
	{"ERROR",		"E",		RTN_C_ERROR },
	{"DEBUG",		"D",		RTN_C_DEBUG },
	{"CRITICAL",		"C",		RTN_C_CRITICAL },
	{"RECOVER",		"R",		RTN_C_RECOVER },
	{"FATAL",		"F",		RTN_C_FATAL },
	{ "N/A",		"?",		-1} };

static Tbl_typ	SubSys[] = {
	{"Generic",		"GNRC",		RTN_S_GENERIC },
	{"General",		"GNRL",		RTN_S_GENERAL },
	{"VICAR RTL",		"VIC",		RTN_S_VICAR },
	{"Catalog_API",		"CAT",		RTN_S_CAT_API },
	{"Label_API",		"LBL",		RTN_S_LBL_API },
	{"Parameter",		"PARM",		RTN_S_PARAM },
	{"SFDU",		"SFDU",		RTN_S_SFDU },
	{"CHDO",		"CHDO",		RTN_S_CHDO },
	{"Telemetry",		"TLM",		RTN_S_TLM_PKT },
	{ "N/A",		"?",		-1} };

static Tbl_typ	Errors[] = {
	{"Undefined",			"Undef",	RTN_V_UNDEF },
	{"Invalid feature",		"Invld",	RTN_V_INVALID },
	{"Unsupported feature",		"Unsup",	RTN_V_UNSUPPORTED },
	{"Time-out expired",		"TmOut",	RTN_V_TIMEOUT },
	{"No more data available",	"EOD",		RTN_V_EOD },
	{"Allocate resource",		"Alloc",	RTN_V_ALLOCATE },
	{"Open a resource",		"Open",		RTN_V_OPEN },
	{"Create a resource",		"Creat",	RTN_V_CREATE },
	{"Attach a resource",		"Attch",	RTN_V_ATTACH },
	{"Resource exists",		"Exist",	RTN_V_EXISTS },
	{"Initialization",		"Init",		RTN_V_INITIALIZE },
	{"VICAR Run Time Library",	"V_RTL",	RTN_V_VICAR_RTL },
	{"Missing Value",		"M_Val",	RTN_V_MISSING_VALUE },
	{"Pathname Syntax",		"Path",		RTN_V_BAD_PATH },
	{"End of File",			"EOF",		RTN_V_EOF },
	{"Insufficient Data",		"Insuf",	RTN_V_INSUFF_DATA },
	{"Insufficient Memory",		"NoMem",	RTN_V_INSUFF_MEMORY },
	{"Too Much Data",		"Mucho",	RTN_V_MUCHO_DATA },
	{"Missing Data",		"NoDat",	RTN_V_MISSING_DATA },
	{"Array index bounds",		"Idx",		RTN_V_BOUND_ERROR },
	{"I/O error",			"IoErr",	RTN_V_IO_ERROR },
	{"Function not implemented",	"NoImp",	RTN_V_NOT_IMPLEMENTED },
	{"Data SFDU record",		"S_Dat",	RTN_V_SFDU_DATA },
	{"Informational SFDU record",	"S_Inf",	RTN_V_SFDU_INFO },
	{"Delimiter SFDU record",	"S_Del",	RTN_V_SFDU_DELIM },
	{"Error SFDU record",		"S_Err",	RTN_V_SFDU_ERROR },
	{"Unknown SFDU length",		"U_len",	RTN_V_SFDU_UNKWN_LEN},
	{"Resource not available",	"Avail",	RTN_V_NOT_AVAILABLE },
	{"Invalid argument value",	"InArg",	RTN_V_INVLD_ARG },
	{"Item not found",		"NoFnd",	RTN_V_NOT_FOUND },
	{"Inconsistent Usage",		"Incst",	RTN_V_INCONSISTENT },
	{"Unexpected Result",		"UnExp",	RTN_V_UNEXPECTED },
	{ "N/A",			"?",		-1} };

static char	MsgBuffer[256];

/******************************************************************************
 *
 *				err_msg
 *
 *	Formats a text string describing the status assocated with the
 *  specified 'ErrorNumber'.
 *
 *****************************************************************************/
const char	*err_msg(
	int	ErrorNumber,
	int	CodeMask,
	int	SubSysMask,
	int	NumberMask)
{ int	Idx;
  char	Temp[32];

  memset(MsgBuffer,0,sizeof(MsgBuffer));


  if (CodeMask)
  { for (Idx=0; (Codes[Idx].Value != -1 &&
                 RTN_CODE(ErrorNumber) != Codes[Idx].Value); Idx++);

    if (CodeMask & RTN_M_STRING)
       strcpy(Temp,Codes[Idx].FullName);
    else if (CodeMask & RTN_M_NMEMONIC)
         strcpy(Temp,Codes[Idx].Nmemonic);
    else if (CodeMask & RTN_M_VALUE)
            sprintf(Temp,"%d",RTN_CODE(ErrorNumber));

    strcat(MsgBuffer,Temp);

    if (SubSysMask || NumberMask) strcat(MsgBuffer,": ");
  }


  if (SubSysMask)
  { for (Idx=0; (SubSys[Idx].Value != -1 &&
                 RTN_SUBSYS(ErrorNumber) != SubSys[Idx].Value); Idx++);

    if (SubSysMask & RTN_M_NMEMONIC)
       sprintf(Temp,"[%s]",SubSys[Idx].Nmemonic);
    else if (SubSysMask & RTN_M_STRING)
            sprintf(Temp,"[%s]",SubSys[Idx].FullName);
    else if (SubSysMask & RTN_M_VALUE)
            sprintf(Temp,"[%d]",RTN_SUBSYS(ErrorNumber));

    strcat(MsgBuffer,Temp);

    if (NumberMask) strcat(MsgBuffer," ");
  }


  if (NumberMask)
  { for (Idx=0; (Errors[Idx].Value != -1 &&
                 RTN_NUMBER(ErrorNumber) != Errors[Idx].Value); Idx++);

    if (NumberMask & RTN_M_STRING) strcat(MsgBuffer,Errors[Idx].FullName);
    else if (NumberMask & RTN_M_NMEMONIC)
            strcat(MsgBuffer,Errors[Idx].Nmemonic);
    if (NumberMask & (RTN_M_STRING | RTN_M_NMEMONIC)) strcat(MsgBuffer," ");

    if (NumberMask & RTN_M_VALUE)
    { sprintf(Temp,"(%d)",RTN_NUMBER(ErrorNumber));
      strcat(MsgBuffer,Temp);
    }
  }


  return MsgBuffer;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create return_status.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE return_status
/*
/*   To Create the build file give the command:
/*
/*		$ vimake return_status			(VMS)
/*   or
/*		% vimake return_status			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE return_status		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST return_status.c


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_ANSI_C
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of return_status imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_return_status.c
/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>

#include "return_status.h"

main()
{ int	code,
	subsys,
	number;

  printf("\nStatic display options\n");
  for (number=0; number<30; number++)
  { code = number % 8;
    subsys = number % 10;

    printf("%s\n",
           err_msg(RTN_STAT_VALUE(number,subsys,code),
                   RTN_M_STRING,RTN_M_NMEMONIC,(RTN_M_STRING|RTN_M_VALUE)));
  }

  printf("\n\nMixing display options\n");

  for (number=0; number<30; number++)
  { code = number % 8;
    subsys = number % 10;

    printf("%s\n",err_msg(RTN_STAT_VALUE(number,subsys,code),
			((number+3)%5),((number+1)%5),(number%8)));
/***
			((number+3)%5),((number+1)%5),RTN_M_STRING));
			RTN_M_STRING,RTN_M_STRING,RTN_M_STRING));
			RTN_M_STRING,0,RTN_M_STRING));
			RTN_M_NMEMONIC,0,RTN_M_STRING));
***/
  }

  return number;
}
$!-----------------------------------------------------------------------------
$ create tst_return_status.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_return_status
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_return_status			(VMS)
/*   or
/*		% vimake tst_return_status			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define PROGRAM tst_return_status		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_return_status.c


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_ANSI_C
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_return_status imake file  **********/
$ Return
$!#############################################################################
