$!****************************************************************************
$!
$! Build proc for MIPL module ns1
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:08
$!
$! Execute by entering:		$ @ns1
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module ns1 ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ns1.imake") .nes. ""
$   then
$      vimake ns1
$      purge ns1.bld
$   else
$      if F$SEARCH("ns1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ns1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ns1.bld "STD"
$   else
$      @ns1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ns1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ns1.com -
	-p ns1.pdf -
	-i ns1.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ns1.pdf
PROCEDURE	HELP=*

PARM	inp	TYPE=(STRING,40)		! Input dataset name
PARM	out	TYPE=(STRING,40)  COUNT=(1:8)	! Output dataset name(s)
PARM	chan	TYPE=INTEGER COUNT=(1:8) DEFAULT=(1,2,3,4,5,6,7,8)
						! Channels to be logged
PARM	date	TYPE=(STRING,15)		! Date of flight
PARM	site	TYPE=(STRING,40)		! Site name
PARM	flight	TYPE=(STRING,20)		! Flight number
PARM	line	TYPE=(STRING,10)		! Flight line number
PARM    run     TYPE=(STRING,10)  DEFAULT=1	! Flight run number
PARM	text	TYPE=STRING  COUNT=(1:8)+
		DEFAULT=("NS001  CHANNEL  1 OF  8   0.45 TO 0.52 MICRONS",+
			 "NS001  CHANNEL  2 OF  8   0.52 TO 0.60 MICRONS",+
			 "NS001  CHANNEL  3 OF  8   0.63 TO 0.69 MICRONS",+
			 "NS001  CHANNEL  4 OF  8   0.76 TO 0.90 MICRONS",+
			 "NS001  CHANNEL  5 OF  8   1.00 TO 1.30 MICRONS",+
			 "NS001  CHANNEL  6 OF  8   1.55 TO 1.75 MICRONS",+
			 "NS001  CHANNEL  7 OF  8   2.08 TO 2.35 MICRONS",+
			 "NS001  CHANNEL  8 OF  8   10.4 TO 12.5 MICRONS")
LOCAL	i	TYPE=INTEGER			! Channel number
LOCAL	j	TYPE=INTEGER			! Starting byte of input
LOCAL	n	TYPE=INTEGER INITIAL=1		! Index
LOCAL	nmax	TYPE=INTEGER			! # of channels to be logged
LOCAL	txt	TYPE=STRING
LOCAL   outds	TYPE=STRING

BODY

LET nmax=$COUNT(chan)

LOOP						! Log each requested channel
   LET outds=out(n)
   LET i=chan(n)
   LET j=750*i-699
   LET txt=text(i)
   COPY &inp &outds SS=&j NS=699
   LABEL-ADD &outds ITEMS="BANDPASS='&txt' DATE='&date' SITE='&site'"
   LABEL-ADD &outds ITEMS="FLIGHT='&flight' LINE='&line' RUN='&run'"
   IF (n=nmax) BREAK
   LET n=n+1
END-LOOP

END-PROC
.TITLE 
VICAR2 Procedure NS1
.HELP
     NS1 is a logging procedure for data acquired from the NS001
multispectral scanner. The NS001 scanner is an 8 channel instrument with 7
bands in visible and near-IR and a single thermal-IR band. Its bandpasses are
similar to the LANDSAT-TM system, with an added band at 1.0 to 1.3 microns. The
NS001 scanner was designed to be operated from an aircraft platform, and scenes
are typically acquired from a C-130 operated by NASA AMES. 
.PAGE
     This TAE procedure separates the input data file into separate files for
each band logged, and adds the standard annotation for NS001 data.The user may 
choose to log all bands, or only a subset of the bands.
     The user is required to specify the following information when running
NS001:
	the input dataset name
	the output dataset(s)
	the flight number, line/run number, and date of the data acquisition

Cognizant Programmer: Ron Alley		12/10/85

.LEVEL1
.VARIABLE INP
Input dataset name
.VARIABLE OUT
Output dataset name(s)
.VARIABLE CHAN
Channels to be logged
.VARIABLE DATE
Date of data acquisition
ddmmmyy
.VARIABLE SITE
Site name
.VARIABLE FLIGHT
NASA/AMES flight number
.VARIABLE LINE
Line number for this flight
.VARIABLE RUN
Run number for this flight line
.VARIABLE TEXT
Channel descriptions to be
entered into the VICAR label
.LEVEL2
.VARIABLE INP
The name of the input dataset
.VARIABLE OUT
The name(s) of the output dataset(s).
.VARIABLE CHAN
Channels to be logged. The default is to log all channels, in order. You
may select any subset to be logged; they will be output IN THE ORDER LISTED.
.VARIABLE DATE
Date of data acquisition. This is the date that the mission was flown. The
date is usually written on the tape label, or it can be retrieved from the
flight log. The preferred format is ddmmmyy, e.g. 01FEB85.
.VARIABLE SITE
This is the user specified site name.
.VARIABLE FLIGHT
NASA/AMES flight number. This is usually written on the tape label; otherwise
it can be obtained from the flight log.
.VARIABLE LINE
Line number. This is usually written on the external tape label.
.VARIABLE RUN
Run number for this flight line. This is usually written on the external
label of the tape.
.VARIABLE TEXT
Channel descriptions to be entered into the VICAR label. If you wish to add
annotation to individual channels, you may do so by entering the default string
followed by the desired additional information. You are urged to always include
the default string, unless the text is incorrect for this data.
$ Return
$!#############################################################################
$Imake_File:
$ create ns1.imake
#define  PROCEDURE ns1

#define R2LIB 
$ Return
$!#############################################################################
