$!****************************************************************************
$!
$! Build proc for MIPL module dsnid
$! VPACK Version 1.9, Monday, December 07, 2009, 16:12:02
$!
$! Execute by entering:		$ @dsnid
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
$ write sys$output "*** module dsnid ***"
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
$ write sys$output "Invalid argument given to dsnid.com file -- ", primary
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
$   if F$SEARCH("dsnid.imake") .nes. ""
$   then
$      vimake dsnid
$      purge dsnid.bld
$   else
$      if F$SEARCH("dsnid.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dsnid
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dsnid.bld "STD"
$   else
$      @dsnid.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dsnid.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dsnid.com -mixed -
	-s dsnid.c -
	-i dsnid.imake -
	-t tdsnid.c tdsnid.imake tdsnid.pdf tstdsnid.pdf -
	-o dsnid.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dsnid.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 |  DSNID.C -- Returns the DSN Source Station ID # and station name          |
 |       given its GCF code. 						     |
 |  Reference: 820-13 (OPS-6-6 for Mark III and OPS-6-8 for Mark IV)         |
 |	       1839-10 A-13.						     |
 |
 |   Revision  History:
 |      Date       FR #            Description
 |  --------  -----  ------------------------------------------------------- 
 |   8-1-94        ---      GAC - Ported to Unix. 
 *===========================================================================*/
#include "mve.h"

int zdsnid(station_code,mrk3_station_id,mrk4_station_id,station_name)
int *station_code;	/* Input GCF source code */
int *mrk3_station_id;	/* DSN Mark III Source Station ID # (= 0 if unknown) */
int *mrk4_station_id;	/* DSN Mark  IV Source Station ID # (= 0 if unknown) */
char station_name[];	/* Source station name */
{
	int		i, icode, jcode;
static 	struct src
	{
	short	code;			/* GCF source code */
	char 	station_name[13];	/* Station name */
	short 	m3_station;  		/* Mark 3 station id */
	short 	m4_station;		/* Mark 4 station id */
	} 		dcode[] = {

  {0x13,	"MCCC SIM    ",	27, 81},
  { 0x14,	"Int MCCC    ",	99, 84},
  { 0x19,	"DSS-14      ",	14,  0},
  { 0x21,	"OCC CHLTN,UK",	53, 53},
  { 0x26,	"MIL 71      ",	71, 71},
  { 0x27,	"ESOCC       ",	54, 54},
  {0x29,	"DSS-11      ",	11, 0},
  {0x2D,	"PARKS, AUST ",	49, 49},
  {0x46,	"CAPE A0     ",	70, 73},
  {0x51,  "MGN BTG     ",  0,  0},
  {0x57,	"Goldstone-10",  0, 10},
  {0x67,	"WILHEIM     ",	67, 50},
  {0x76,	"DSS-62      ",	62, 62},
  {0x77,	"NOCC        ",	24, 24},
  {0x7A,	"DSS-63      ",	63,  0},
  {0x7C,	"GCF TEST    ",	20, 29},
  {0x7F,	"DSN GCF TEST",	20, 20},
  {0x81,	"Goldstone-12",	 0, 12},
  {0x82,	"Goldstone-14",	 0, 14},
  {0x83,	"Goldstone-15",	 0, 15},
  {0x84,	"Goldstone-16",	 0, 16},
  {0x85,  "JPL MCCC RT ",  0,  0},
  {0x86,	"DSS-43      ",	43,  0},
  {0x89,	"DSS-13      ",	13, 13},
  {0x8C,	"MCCC AMPTEE ",	 0,  0},
  {0x8E,	"MCCC/GSOC   ",	 0,  0},
  {0x90,  "Canberra-40 ",  0,  0},
  {0x91,  "JPL         ",  0,  0},
  {0x92,	"Canberra-42 ",	42, 42},
  {0x93,	"Canberra-43 ",  0, 43},
  {0x94,	"Canberra-45 ",  0, 45},
  {0x95,	"Canberra-46 ",  0, 46},
  {0xA4,	"DSN/MCCC    ",	98, 83},
  {0xA7,	"GSOC        ",	69, 52},
  {0xB7,	"DSS DRG-ATRS",	25, 25},
  {0xB8,	"DSS DRG-IDR ",	25, 25},
  {0xBC,	"NDPA        ",	22, 22},
  {0xBD,  "GLL Blocker ",  0,  0},
  {0xC2,	"DSS-44      ",	44, 44},
  {0xD2,  "VLA N.M.    ",  0, 19},
  {0xD7,  "Madrid-60   ",  0, 60},
  {0xDB,	"Goldstone-17",	 0, 16},
  {0xE5,	"Madrid-62   ",	 0, 62},
  {0xF3,	"Madrid-61   ",	 0, 61},
  {0xF4,	"Madrid-63   ",	 0, 63},
  {0xF6,  "Madrid-66   ",  0, 66},
  {0xFD,  "Madrid-65   ",  0, 65},
  {0,	"Unknown     ",	 0,  0}};

  icode = *station_code;
  for (i=0; i<256 ;i++)
  {
    jcode = (int) dcode[i].code;
    if (jcode == 0) break;
    if (icode == jcode)
    {
      if (mrk3_station_id) *mrk3_station_id = dcode[i].m3_station;
      if (mrk4_station_id) *mrk4_station_id = dcode[i].m4_station;
      if (station_name) zmve(  1, 12, dcode[i].station_name,    station_name , 1, 1 );
      return(1);
    }
  }

  if (mrk3_station_id) *mrk3_station_id = 0;
  if (mrk4_station_id) *mrk4_station_id = 0;
  if (station_name) zmve(  1, 12, dcode[i].station_name,    station_name,  1, 1 ); 
  return(0);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dsnid.imake
/* Imake file for Test of VICAR subroutine dsnid */

#define SUBROUTINE dsnid

#define MODULE_LIST dsnid.c  

#define MAIN_LANG_C
#define USES_C


#define P2_SUBLIB 		/* Enable during delivery */
/* #define LIB_LOCAL */		/* Disable during delivery */
#define LIB_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tdsnid.c
/*===========================================================================*
 |  TDSNID.C -- Routine to test GCFDCODE.C				     |
 *===========================================================================*/
main()
{
	int icode,mrk3_station_id,mrk4_station_id,ind;
	char name[13];

	name[12] = 0;
	for (icode=0; icode<256; icode++)
		{
		ind = zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
		if (ind == 1) printf(" %d   %s   %d   %d\n",icode,
			name,mrk3_station_id,mrk4_station_id);
		}
	icode = 1;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
	icode = 0;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
	icode = 257;
	zdsnid(&icode, &mrk3_station_id, &mrk4_station_id, name);
	printf(" %d   %s   %d   %d\n",icode, name,mrk3_station_id,mrk4_station_id);
}
$!-----------------------------------------------------------------------------
$ create tdsnid.imake
/* Imake file for Test of VICAR subroutine dsnid */

#define PROGRAM tdsnid

#define MODULE_LIST tdsnid.c  

#define MAIN_LANG_C
#define USES_C

#define P2_SUBLIB 		/* Enable during delivery */
/* #define LIB_LOCAL*/		/* Disable during delivery */
#define LIB_FORTRAN

#define LIB_TAE 
#define LIB_RTL 
#define LIB_P2SUB		/* Disabled for testing old code */
/*#define LIB_S2   	*/		/* Enable for testing old code */

$!-----------------------------------------------------------------------------
$ create tdsnid.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdsnid.pdf
procedure
refgbl $echo
body
let _onfail="continue"
!let $echo="yes"
tdsnid
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create dsnid.hlp

1 DSNID    

  Routine to return the DSN Source Station ID # and station name
  given its code as extracted from the GCF block.  Called by MIPL
  Real-Time Subsystem and VEDR.

  Reference: 820-13 (OPS-6-6 for Mark III and OPS-6-8 for Mark IV)
	     1839-10 A-13.

  CALLING SEQUENCE:
	status = zdsnid( icode , mark3_id , mark4_id , station_name)

	int 	icode, 
		mark3_id,
		mark4_id,
		status;
	char   	station_name[12];


         where....

	icode is the input DSN source code as extracted from the GCF block,
	mark3_id is the Mark III DSN source station ID # (=0 if unknown),
	mark4_id is the Mark  IV DSN source station ID # (=0 if unknown), and
	station_name is the station name (in ASCII) blank filled at the left.
	status is 1 if station_name exists for icode
$ Return
$!#############################################################################
