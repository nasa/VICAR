$!****************************************************************************
$!
$! Build proc for MIPL module getll
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:15
$!
$! Execute by entering:		$ @getll
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
$ write sys$output "*** module getll ***"
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
$ write sys$output "Invalid argument given to getll.com file -- ", primary
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
$   if F$SEARCH("getll.imake") .nes. ""
$   then
$      vimake getll
$      purge getll.bld
$   else
$      if F$SEARCH("getll.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getll
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getll.bld "STD"
$   else
$      @getll.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getll.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getll.com -mixed -
	-s getll.c -
	-p getll.pdf -
	-i getll.imake -
	-t tstgetll.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getll.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/****************************************************************************
 * Program GETLL (Get LINESAMP/LATLON)
 * 
 * This program will output the (LINE, SAMP) if input (LAT, LON) or
 *   output (LAT, LON) if input (LINE, SAMP); the 2 modes are distinguished
 *   by the presence of keyword 'TOLS (To LineSamp) meaning the first case.
 *
 * Hint: set window size to at least 90 column size to best view this file
 ****************************************************************************/

#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"
#include "spiceinc.h"
#include <string.h>

#define  SUCCESS  1

void main44(void)
{
  float MAP[40], CoordIn[2], CoordOut[2];
  int   Mode, Cnt, Def, Status, Dummy;

  switch (FillMAPBuffer(MAP))
  {
    case SUCCESS: break;
    case -1: case -2: case -3: case -4: case -5: case -6:
      zmabend("GETLL: Unable to determine MAP buffer for ZCONVEV, cannot continue!", "");
    default: 
      zvmessage ("GETLL: WARNING, unhandled return code for FillMAPBuffer", "");
      break;
  }

  zvparm ("COORDIN", CoordIn, &Cnt, &Def, 2, 0);

  if (zvptst("TOLS"))
  {
    Mode = 1;   /* CONVEV is to convert Lat/Lon to Line/Samp */
    zconvev(&Status, MAP, MAP, &CoordOut[0], &CoordOut[1],
                               &CoordIn[0],  &CoordIn[1], Mode, &Dummy);
  }
  else
  {
    Mode = 2;   /* CONVEV is to convert Line/Samp to Lat/Lon */
    zconvev(&Status, MAP, MAP, &CoordIn[0],  &CoordIn[1], 
                               &CoordOut[0], &CoordOut[1], Mode, &Dummy);
  }
  if (Status != 0)
    if (Mode == 1)
      zvmessage (" This LAT,LON coordinate is on other side of the planet","");
    else
      zvmessage (" This LINE,SAMP is off the planet", "");

  OutputCoord (Mode, CoordIn, CoordOut);
}

/*****************************************************************************
 * Function FillMAPBuffer
 * Purpose: Fill the input buffer with that of the standard float [40] MAP
 *          data buffer by obtaining information from GETSPICE2 and 
 *          GETCAMCON for CONVEV to be executed on geometrically corrected
 *          images (project type 8).  The function will also informs the user
 *          if it detects ZGETSPICE2 using different CKNAME than specified.
 *
 * Requirement: INP parameter from TAE must be a valid image file
 *              specification.
 *
 * Input  : None
 * Output : MAP data buffer for CONVEV to convert geometrically corrected
 *          images.  Please consult to CONVEV.HLP for the elements filled.
 * Return : Function execution status with the following codes:
 *             1  => Success
 *            -1  => File unit obtaining failure for INP
 *            -2  => File unit opening failure
 *            -3  => GETSPICE2 failed
 *            -4  => GETPROJ failed
 *            -5  => GETCAMCON failed
 *            -6  -> Project is not one of the type: GLL, VGR-1, VGR-2.
 *                   Theoretically, the program should work for all project
 *                     CONVEV can handle.  But such responsibility will not
 *                     be claimed unless needed, and unless other projects 
 *                     are tested.
 ****************************************************************************/
int FillMAPBuffer(
                  float MAP[40]   /* output */
                 )
{
  buf_union_typ SPICE;    /* Spice Buffer */
  char     Project[6];
  int      InUnit, Camera, Dum1, Dum2, Status, i;
  static int NO_ABEND = 0;
  char     MsgBuf[128], ActualCKName[LEN_SOURCE+1], 
           RequestedCKName[LEN_SOURCE+1];

  Status = zvunit(&InUnit, "INP", 1, NULL);
  zvsignal (InUnit,Status,NO_ABEND);
  if  (Status != SUCCESS) return -1;

  /* Get SPICE buffer */
  Status = zvopen(InUnit, "OP", "READ", NULL);
  zvsignal (InUnit, Status, NO_ABEND);
  if (Status != SUCCESS) return -2;

  if (zgetspice2(InUnit, 1, &SPICE) != SUCCESS)
  {
    zvmessage ("GETLL:ZGETSPICE2 failed.", "");
    return -3; 
  }

  /* Get Project and Camera S/N, also validates Project */
  zgetproj (InUnit, Project, &Camera, &Dum1, &Status);
  zvclose (InUnit,NULL);
  if (Status != 0) 
  {
    zvmessage ("GETLL:ZGETPROJ failed.", "");
    return -4;
  }
  else if (strncmp("GLL",Project,3) && strncmp("VGR-1",Project,5) && 
           strncmp("VGR-2", Project,5))
  {
    sprintf(MsgBuf,"GETLL: Input file is of invalid project type %s.",Project);
    zvmessage (MsgBuf, "");
    return -6;
  }

  /* Check requested and actual CKName used, if different inform the user */
  zvparm("CKNAME", RequestedCKName, &Dum1, &Dum2, 1, sizeof(RequestedCKName));
  for (i=0; RequestedCKName[i]; i++)
    RequestedCKName[i] = toupper(RequestedCKName[i]);

  if (!strncmp("GLL",Project,3))
  { /* for project "GLL", getspice2 only gives the Actual SOURCE in form of
     * CKID in intbuf[171], intbuf[10] contains the RequestedCKName only
     */ 
    if (CKID2CKName(SPICE.intbuf[0],&SPICE.intbuf[171], ActualCKName) < 0)
    { /* Fill with '?" if unknown */
      memset (ActualCKName, '?', LEN_SOURCE-1);
      ActualCKName[LEN_SOURCE-1] = '\0';
    }
  }
  else
  { /* for project Voyager, getspice will return the actual CKName used in
     * intbuf[10] exactly correct except SEDR is returned as "    "
     */
    memcpy (ActualCKName, &SPICE.intbuf[10],4);
    ActualCKName[4] = '\0';
    if (!strcmp(ActualCKName, "    ")) strcpy(ActualCKName, "SEDR");
  }
  if (strcmp(RequestedCKName, ActualCKName))
  {
    sprintf (MsgBuf, "CKName %s not available, %s used.", RequestedCKName,
             ActualCKName);
    zvmessage (MsgBuf, "");
  }

  /* Start Filling the MAP Buffer */
  zmve (8,9,&SPICE.doublebuf[58],MAP,1,1);         /* OM Matrix */
  zmve (8,3,&SPICE.doublebuf[21],&MAP[18],1,1);    /* RS Vector */

  MAP[24] = SPICE.doublebuf[14];            /* Polar Radius */
  MAP[25] = SPICE.doublebuf[12];            /* equatorial radius, */

  zgetcamcon (Project, Camera, &MAP[26], &MAP[27], &MAP[28],
                               &MAP[29], &Status);
  if (Status != 0)
  {
    zvmessage ("GETLL:ZGETCAMCON failed.", "");
    return -5;
  }

  MAP[37] = SPICE.doublebuf[26];      /* space to picture body center (KM) */
  Dum1 = 8;
  zmve(4, 1, &Dum1, &MAP[38], 1, 1); /* indicate Geometrically corrected */

  return SUCCESS; 
} 

/******************************************************************************
 * Function OutputCoord
 * Purpose: Output the processed coordinates to the standard console, and then
 *          the .PDF defined 2 dimensional real variable. 
 * Input: CONVEVMode => int, (1, or 2) refers to the mode that CONVEV was 
 *                      executed: LATLON to LINESAMP or LINESAMP to LATLON 
 *                      respectively.
 *        CoordIn    => float x 2, The input coordinate from the user.
 *        CoordOut   => float x 2, The output coordinate calculated by CONVEV
 * Output: none.
 * Return: none.
 *****************************************************************************/

OutputCoord (int CONVEVMode,    /* Input */
             float CoordIn[2],  /* Input */
             float CoordOut[2]  /* Input */
              )
{
  char MsgBuf[256];
  struct PARBLK ParBlk;
  double doubleout[2];

  /* Output to the standard console */
  zvmessage ("", "");
  if (CONVEVMode == 1)
    sprintf (MsgBuf, "(%f, %f) {LAT,LON} = (%f, %f) {LINE,SAMP}\n", 
             CoordIn[0], CoordIn[1], CoordOut[0], CoordOut[1]);
  else
    sprintf (MsgBuf, "(%f, %f) {LINE,SAMP} = (%f, %f) {LAT,LON}\n",
             CoordIn[0], CoordIn[1], CoordOut[0], CoordOut[1]);
  zvmessage (MsgBuf, "");

  /* Output to the TAE procedure's variables */
  doubleout[0] = CoordOut[0];
  doubleout[1] = CoordOut[1];
  q_init(&ParBlk,  P_BYTES, P_ABORT);
  q_real (&ParBlk, "COORDOUT", 2, doubleout, P_ADD);
  zvq_out (&ParBlk);
}

/*****************************************************************************
 * Function CKID2CKName
 * Purpose: Converts a C Kernel ID (CKID) to its common recognized name
 *          format (CKName)
 *
 * Method: This function works by opening the SPICE Kernel Database file
 *         for parsing.  If it finds CKID on any row's first column, then
 *         the 3rd column of the row is returned as the CKName
 *         
 * Usage : This function is useful in converting the 172nd element of the 4x200
 *         SPICE buffer returned from getspice95 to the common recognized SOURCE 
 *         name (DAVI, NAV, NAIF, etc)
 *
 * Input : SpacecraftID => (int), Spacecraft ID as returned by the 1st element
 *                         of the 4x200 SPICE buffer.
 *         CKID => (char []), C Kernel ID as returned by the 172nd element of
 *                 the 4x200 SPICE buffer.  The length of this "char array" is
 *                 defined by LEN_SPICE_ID in spiceinc.h. NOTE: this input is
 *                 not expected to be a "standard string" with NULL termination.
 *                 Thus one could simply pass the address of the 172nd element
 *                 of the SPICE buffer as getspice95 doesn't NULL terminate
 *                 any of its strings.
 * Output: CKName => (char []), Common recognized SPICE source name such as:
 *                     NAIF, AMOS, NEAR, NAV2, FAR, NAV, DAVI, etc.
 *                   the calling function should make sure to allocate the
 *                   string with length LEN_SOURCE+1 as defined in spiceinc.h
 *                   (+1 for NULL char)
 *                   This ouput will be NULL if function fails
 * Return: (int)  1 -> Success
 *               -1 -> Unable to obtain full path of the Kernel Database file
 *               -2 -> Unable to open Kernel Database file for read access
 *               -3 -> Kernel Database file format unknown
 *               -4 -> CKID not found in Kernel Database file
 *****************************************************************************/
int CKID2CKName (int  SpacecraftID,    /* Input  */
                 char CKID[],          /* Input  */
                 char CKName[]         /* Output */
                )
{
  FILE *KDbFILE;
  char KDbFname[256], KDbLine[256], ID[LEN_SPICE_ID+1], 
       Source[LEN_SOURCE+1];
  int  Dummy;

  CKName[0] = '\0';

  if (GetKernelDbFname(KDbFname, SpacecraftID) != SUCCESS) return -1;
  if ((KDbFILE=fopen(KDbFname, "r")) == NULL) return -2;

  while (!feof(KDbFILE))
  {
    if (fgets(KDbLine, 256, KDbFILE)==NULL) return -3;
    if (sscanf(KDbLine, "%s%i%s", ID, &Dummy, Source)!= 3) return -3;
    if (!strncmp(CKID,ID,LEN_SPICE_ID))
    {
      strcpy (&CKName[0], Source);
      break;
    }
  }

  fclose (KDbFILE);
  if (CKName)
    return SUCCESS;
  else
    return -4;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create getll.pdf
PROCESS help=*
LOCAL DUM1 REAL COUNT=2
PARM INP STRING
PARM COORDIN	TYPE=REAL 	 COUNT=2
PARM COORDOUT	TYPE=NAME 	                                DEFAULT=DUM1
PARM MODE	TYPE=KEYWORD     COUNT=1   VALID=(TOLS,TOLL)

PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1                        DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                        DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE

END-PROC
.TITLE
VICAR Program GETLL
.HELP
PURPOSE
GETLL stands for Get Linesamp/Latlon, it is a VICAR application program which 
converts the user input coordinate of a geometrically corrected input image 
from LINESAMP to LATLON; or vice versa.  The conversion result is output to the
screen and a TAE procedure's variable if defined.

This program replaces the unported GLL_LL and VGRLL.  (VGRLL actually only did
the MODE=TOLS, this program allows both.)

.page
OPERATION

    GETLL INP=InpFile COORDIN=(Line,Samp) COORDOUT=OutVar 'TOLL
or
    GETLL INP=InpFile COORDIN=(Lat,Lon) COORDOUT=OutVar 'TOLS

    where InpFile     is a Galileo image file name
          (Line,Samp) is a LINESAMP coordinate in type REAL
          (Lat, Lon)  is a LATLON Coordinate in type REAL
          OutVar      is a TAE procedure's variable of type=REAL and COUNT=2

.page
CKNAME

The program will inform the user of the CKNAME used if it detects the 
unavailability of the specified CKNAME.

.page
REVISION HISTORY

When       Who What
---------- --- ---------------------------------------------------------------
08/01/1996 SMC Initial Release

.level1
.var INP
Required string
Input image file name
.var COORDIN
Required 2D real
Input coordinate to be converted
.var COORDOUT
Required variable name
Variable for storing output
.var MODE
Required keyward
Mode of ouput
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created

.LEVEL2
.VARI INP
Input file name of a geometrically corrected image; must be of Galileo or 
Voyager.
.VARI COORDIN
Input coordinate in either (LINE, SAMP) or (LAT, LON) format.  The format of
the input is indicated to the program by the MODE keyword.  It is (LINE, SAMP)
if MODE=TOLL; (LAT, LON) if MODE=TOLS.
.VARI COORDOUT
Output variable for the converted input coordinated.  This output is of format
(LINE, SAMP) if MODE=TOLS, and (LAT, LON) if MODE=TOLL.
.VARI MODE
Indicates the mode of output.
.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME        C KERNEL
  --------      -------------
  DAVI          MIPS_DAVI.CK
  NAV           MIPS_NAV.CK
  FARE          MIPS_FARENC.CK
  NAV2          MIPS_NAV2.CK
  NEAR          MIPS_NEAR.CK
  AMOS          MIPS_AMOS.CK
  NAIF          the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID    CKNAME        C KERNEL
  ----    --------      -------------
  M906    DAVI          MIPS_DAVI.CK
  M905    NAV           MIPS_NAV.CK
  M904    FARE          MIPS_FARENC.CK
  M903    NAV2          MIPS_NAV2.CK
  M902    NEAR          MIPS_NEAR.CK
  M901    AMOS          MIPS_AMOS.CK
  varies  NAIF          there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.
.end


$ Return
$!#############################################################################
$Imake_File:
$ create getll.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM getll

   To Create the build file give the command:

		$ vimake getll			(VMS)
   or
		% vimake getll			(Unix)


************************************************************************/


#define PROGRAM	getll
#define R2LIB

#define MODULE_LIST getll.c 

#define MAIN_LANG_C
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_MATH77
#define LIB_FORTRAN
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstgetll.pdf
procedure
  RefGbl $echo
  RefGbl $SysChar
body
  Local CoordOut REAL COUNT=2
  Local GLLFile  STRING
  Local VGRFile  STRING

  Let $echo="no"
  If ($SysChar(1)="VAX_VMS")
    Let GLLFile="wms_test_work:[testdata.mipl.gll]ptp_gll_grn.img"
    Let VGRFile="wms_test_work:[testdata.mipl.vgr]f1636832.geo"
  Else
    Let GLLFile="/project/test_work/testdata/mipl/gll/ptp_gll_grn.img"
    Let VGRFile="/project/test_work/testdata/mipl/vgr/f1636832.geo"
  End-If

  write "===This section checks if the program correctly detects source"
  write "=================================================================="
  write "===Test Galileo Project with an existing source"
  Let $echo="yes"
 GETLL @GLLFile COORDIN=(-70,83) 'tols COORDOUT=CoordOut CKNAME=NAIF
  Let $echo="no"
  write "===CoordOut is &CoordOut"

  write "===Test Galileo Project with a non-existing source"
  Let $echo="yes"
 GETLL @GLLFile COORDIN=(-70,83) 'tols COORDOUT=CoordOut CKNAME=SMC
  Let $echo="no"
  write "===CoordOut is &CoordOut"

  write "===Test Voyager Project with an existing source" 
  Let $echo="yes"
 GETLL @VGRFile COORDIN=(16,214) 'tols COORDOUT=CoordOut CKNAME=FARE
  Let $echo="no"
  write "===CoordOut is &CoordOut"

  write "===Test Voyager Project with a non-existing source" 
  Let $echo="yes"
 GETLL @VGRFile COORDIN=(16,214) 'tols COORDOUT=CoordOut CKNAME=SMC
  Let $echo="no"
  write "===CoordOut is &CoordOut"
  write ""

  write "===This section checks if the program SEES the picture alright"
  write "=================================================================="

  write "===Test Galileo project, all points should be OFF planet"
  Let $echo="yes"
 GETLL @GLLFile (65,537) 'toll
 GETLL @GLLFile (57,500) 'toll
 GETLL @GLLFile (79,429) 'toll
 GETLL @GLLFile (173,294) 'toll
 GETLL @GLLFile (416,220) 'toll
 GETLL @GLLFile (688,477) 'toll
  Let $echo="no"
  write "===Test Galileo project, all points should be ON planet"
  Let $echo="yes"
 GETLL @GLLFile (78,533) 'toll
 GETLL @GLLFile (118,376) 'toll
 GETLL @GLLFile (299,235) 'toll
 GETLL @GLLFile (456,234) 'toll
 GETLL @GLLFile (634,362) 'toll
 GETLL @GLLFile (678,483) 'toll
  Let $echo="no"

  write "===Test Voyager project, all points should be OFF planet"
  Let $echo="yes"
 GETLL @VGRFile (277,735) 'toll
 GETLL @VGRFile (250,605) 'toll
 GETLL @VGRFile (277,486) 'toll
 GETLL @VGRFile (354,388) 'toll
 GETLL @VGRFile (537,319) 'toll
 GETLL @VGRFile (677,353) 'toll
  Let $echo="no"
  write "===Test Voyager project, all pointes should be ON planet"
 GETLL @VGRFile (333,796) 'toll
 GETLL @VGRFile (287,735) 'toll
 GETLL @VGRFile (257,621) 'toll
 GETLL @VGRFile (279,497) 'toll
 GETLL @VGRFile (353,394) 'toll
 GETLL @VGRFile (779,452) 'toll
end-proc
$ Return
$!#############################################################################
