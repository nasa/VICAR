$!****************************************************************************
$!
$! Build proc for MIPL module photfit2
$! VPACK Version 1.9, Thursday, September 04, 2014, 15:17:24
$!
$! Execute by entering:		$ @photfit2
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
$ write sys$output "*** module photfit2 ***"
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
$ write sys$output "Invalid argument given to photfit2.com file -- ", primary
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
$   if F$SEARCH("photfit2.imake") .nes. ""
$   then
$      vimake photfit2
$      purge photfit2.bld
$   else
$      if F$SEARCH("photfit2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake photfit2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @photfit2.bld "STD"
$   else
$      @photfit2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create photfit2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack photfit2.com -mixed -
	-p photfit2.pdf photfit2m.pdf photfit2m.mdf photfit2m_general.pdf -
	   photfit2_lambert.pdf photfit2_minnaert.pdf photfit2_irvine.pdf -
	   photfit2_veverka.pdf photfit2_buratti1.pdf photfit2_buratti2.pdf -
	   photfit2_buratti3.pdf photfit2_mosher.pdf -
	   photfit2_lumme_bowel_hg1.pdf photfit2_hapke_81_le2.pdf -
	   photfit2_hapke_81_cook.pdf photfit2_hapke_86_hg1.pdf -
	   photfit2_hapke_86_hg2.pdf photfit2_hapke_86_le2.pdf -
	   photfit2_hapke_hg1_dom.pdf photfit2_regner_hapke_hg1.pdf -
	-i photfit2.imake -
	-s photfit2.c -
	-t tstphotfit2.pdf tstphotfit2_linux.log tstphotfit2_sun.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create photfit2.pdf
  process help=*

 
	parm INP	type=(string,32) count=0:20	default=photcat.dat


	parm PHO_FUNC type=(string,32) count=1 		+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default=HAPKE_86_LE2

	PARM CLASS_ID TYPE=INTEGER COUNT=0:1 valid=(0:32767) DEFAULT=--

  	PARM NORM    TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=500
  	PARM RERUN   TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM MAXITER TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM NUMTEN  TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM PERCENT TYPE=REAL COUNT=1 VALID=(1.:100.) DEFAULT=90.
  	PARM TOLERANC TYPE=REAL COUNT=1 VALID=(0.:1.) DEFAULT=.02
  	PARM METROP  TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=0
  	PARM PRINT    TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT

       parm ALBEDO 		real count=0:1 			default=--
       parm MIN_ALBEDO 		real count=0:1 			default=--
       parm MAX_ALBEDO 		real count=0:1 			default=--
       parm T_ALBEDO 		real count=0:1 			default=--
       parm EXPONENT 		real count=0:1 valid=(0:1)	default=--
       parm MIN_EXPONENT 	real count=0:1 valid=(0:1)	default=--
       parm MAX_EXPONENT 	real count=0:1 valid=(0:1)	default=--
       parm T_EXPONENT 		real count=0:1 valid=(0:1)	default=--
       parm A_VEVERKA 		real count=0:1 			default=--
       parm MIN_A_VEVERKA 	real count=0:1 			default=--
       parm MAX_A_VEVERKA 	real count=0:1 			default=--
       parm T_A_VEVERKA 	real count=0:1 			default=--
       parm B_VEVERKA 		real count=0:1 			default=--
       parm MIN_B_VEVERKA 	real count=0:1 			default=--
       parm MAX_B_VEVERKA 	real count=0:1 			default=--
       parm T_B_VEVERKA 	real count=0:1 			default=--
       parm C_VEVERKA 		real count=0:1 			default=--
       parm MIN_C_VEVERKA 	real count=0:1 			default=--
       parm MAX_C_VEVERKA 	real count=0:1 			default=--
       parm T_C_VEVERKA 	real count=0:1 			default=--
       parm D_VEVERKA 		real count=0:1 			default=--
       parm MIN_D_VEVERKA 	real count=0:1 			default=--
       parm MAX_D_VEVERKA 	real count=0:1 			default=--
       parm T_D_VEVERKA 	real count=0:1 			default=--
       parm MO_EXP1 		real count=0:1 			default=--
       parm MIN_MO_EXP1 	real count=0:1 			default=--
       parm MAX_MO_EXP1 	real count=0:1 			default=--
       parm T_MO_EXP1 		real count=0:1 			default=--
       parm MO_EXP2 		real count=0:1 			default=--
       parm MIN_MO_EXP2 	real count=0:1 			default=--
       parm MAX_MO_EXP2 	real count=0:1 			default=--
       parm T_MO_EXP2 		real count=0:1 			default=--
       parm E_BURATTI 		real count=0:1 			default=--
       parm MIN_E_BURATTI 	real count=0:1 			default=--
       parm MAX_E_BURATTI 	real count=0:1 			default=--
       parm T_E_BURATTI 	real count=0:1 			default=--
       parm DEN_SOIL 		real count=0:1 			default=--
       parm MIN_DEN_SOIL 	real count=0:1 			default=--
       parm MAX_DEN_SOIL 	real count=0:1 			default=--
       parm T_DEN_SOIL 		real count=0:1 			default=--
       parm W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm MIN_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm MAX_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm T_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm HG1_SOIL 		real count=0:1 			default=--
       parm MIN_HG1_SOIL 	real count=0:1 			default=--
       parm MAX_HG1_SOIL 	real count=0:1 			default=--
       parm T_HG1_SOIL 		real count=0:1 			default=--
       parm HG2_SOIL 		real count=0:1 			default=--
       parm MIN_HG2_SOIL 	real count=0:1 			default=--
       parm MAX_HG2_SOIL 	real count=0:1 			default=--
       parm T_HG2_SOIL 		real count=0:1 			default=--
       parm HG_ASY_SOIL 	real count=0:1 			default=--
       parm MIN_HG_ASY_SOIL 	real count=0:1 			default=--
       parm MAX_HG_ASY_SOIL 	real count=0:1 			default=--
       parm T_HG_ASY_SOIL 	real count=0:1 			default=--
       parm LE1_SOIL 		real count=0:1 			default=--
       parm MIN_LE1_SOIL 	real count=0:1 			default=--
       parm MAX_LE1_SOIL 	real count=0:1 			default=--
       parm T_LE1_SOIL 		real count=0:1 			default=--
       parm LE2_SOIL 		real count=0:1 			default=--
       parm MIN_LE2_SOIL 	real count=0:1 			default=--
       parm MAX_LE2_SOIL 	real count=0:1 			default=--
       parm T_LE2_SOIL 		real count=0:1 			default=--
       parm H_SHOE 		real count=0:1 			default=--
       parm MIN_H_SHOE 		real count=0:1 			default=--
       parm MAX_H_SHOE 		real count=0:1 			default=--
       parm T_H_SHOE 		real count=0:1 			default=--
       parm B_SHOE 		real count=0:1 			default=--
       parm MIN_B_SHOE 		real count=0:1 			default=--
       parm MAX_B_SHOE 		real count=0:1 			default=--
       parm T_B_SHOE 		real count=0:1 			default=--
       parm H_CBOE 		real count=0:1 			default=--
       parm MIN_H_CBOE 		real count=0:1 			default=--
       parm MAX_H_CBOE 		real count=0:1 			default=--
       parm T_H_CBOE 		real count=0:1 			default=--
       parm B_CBOE 		real count=0:1 			default=--
       parm MIN_B_CBOE 		real count=0:1 			default=--
       parm MAX_B_CBOE 		real count=0:1 			default=--
       parm T_B_CBOE 		real count=0:1 			default=--
       parm THETA 		real count=0:1 			default=--
       parm MIN_THETA 		real count=0:1 			default=--
       parm MAX_THETA 		real count=0:1 			default=--
       parm T_THETA 		real count=0:1 			default=--
       parm COOK 		real count=0:1 			default=--
       parm MIN_COOK 		real count=0:1 			default=--
       parm MAX_COOK 		real count=0:1 			default=--
       parm T_COOK 		real count=0:1 			default=--
       parm TAU_ATM 		real count=0:1 			default=--
       parm MIN_TAU_ATM 	real count=0:1 			default=--
       parm MAX_TAU_ATM 	real count=0:1 			default=--
       parm T_TAU_ATM 		real count=0:1 			default=--
       parm W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm MIN_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm MAX_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm T_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm HG1_ATM 		real count=0:1 			default=--
       parm MIN_HG1_ATM 	real count=0:1 			default=--
       parm MAX_HG1_ATM 	real count=0:1 			default=--
       parm T_HG1_ATM 		real count=0:1 			default=--
       parm IRV_EXP1 		real count=0:1 			default=--
       parm MIN_IRV_EXP1 	real count=0:1 			default=--
       parm MAX_IRV_EXP1 	real count=0:1 			default=--
       parm T_IRV_EXP1 		real count=0:1 			default=--
       parm IRV_EXP2 		real count=0:1 			default=--
       parm MIN_IRV_EXP2 	real count=0:1 			default=--
       parm MAX_IRV_EXP2 	real count=0:1 			default=--
       parm T_IRV_EXP2 		real count=0:1 			default=--


  end-proc

.title
VICAR program PHOTFIT2 determines the coefficients of various photometric 
functions.

.help
PURPOSE:	Photfit2 is a VICAR program which determines the coefficients 
		of various photometric functions.  

	
FUNCTION:

  PHOTFIT2 reads IBIS1 and IBIS2 files which have been generated by the VICAR 
programs PHOTOM, HWPHOCAE/D, PHOTTEST2. PHOTOM or HWPHOCAE/D are used to collect photometric data from images (and their navigation). The IBIS files contain all 
the data required for PHOTFIT2. There is one input IBIS file for each picture. 
The output of PHOTFIT2 is listed coefficient values on screen [or an IBIS2 
photometric parameterfile (subtyp=phopar) - not yet implemented].

.page


EXECUTION:

  Be very conservative about the amount of data to include in the
IBIS files. PHOTFIT3 is a slow program. It is the quality of the
IBIS data points, not their quantity, which matters. You should
assure as broad a coverage of incidence, emission, and phase
angles as possible. If you have 10 points/ibis file
and 6 files (one for each phase angle) that should be sufficient.  
You might consider restricting points (from PHOTOM, HWPHOCA*) to areas of
consistent albedo based upon your theories of morphology.
  The Hapke and Veverka functions are EXTREMELY sensitive to
data which, because of a lack of broad ranges of incidence, emission,
and phase angles, or because of inclusion of various albedo's,
does not truly represent the function being fit. When this 
problem occurs the coefficients will become distorted in order
to accomodate erroneous data. Sometimes you can tell when the coefficients
returned lie up against the limits of their permitted ranges.
  Accuracy: Rerun PHOTFIT2 at least twice to assure that the function
has had sufficient time to find a stable minimum. If you cool the
solution too fast it will 'freeze' on the wall of the error minimum.
Comparing several answers will give a feeling of the precision of the 
result. 'Freezing' can be avoided by providing either a higher initial
'temperature' or more iterations.
  The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.page

USAGE:

  There are sets of four keywords which relate to each function:
These are the parmeters, T_*, MAX_*, MIN_*. These provide the:
1.  Initial position guess for the photometric parameters. This is not 
    important per-se but it assures you that Metropolis will investigate 
    this point (and remember what it found). 
2.  Range over which random guesses can be expected to vary at first:
    PARAM_NEW = T_* * tan( PI * ran_num + PI/2 ).
    This is the 'temperature' for each variable. As the system cools the range 
    will constrict gradually (T_NEW_* = T_OLD_* * scale, scale depends of 
    NUMTEN) until the temperatur is only 10**-MAXITER/NUMTEN times the inital 
    temperatur of the parameter.
3/4.Two limits MIN_*, MAX_* outside of which each of the photometric 
    parameters are stricly prohibited to fall. Each time these limits 
    are violated the attempted solution is abandoned and a new one is 
    attempted.  This will happen thousands of times, especially at first 
    when the temperatures are high.  
  
Each of these has defaults but you must not consider these as gospel.

The maxiter parameter proposals are a bit arbitrary. They will depend
upon experience. You will probably be able to reduce them greatly.


.page

METHOD:

  PHOTFIT2 uses the simulated annealing method  to solve for the function 
coefficients. This is verry inefficient solution method which is good for 
fitting complicated functions. The solution is arrived at through guessing. 
Initially guessing is performed over a large region using a random number
generator. As time progresses the "temperature" is reduced, 
constricting the range of guesses to an ever decreasing region
in the N solution space. Decisions are made at every step based upon
the Boltzmann probability of transitioning from one energy level
(error due to incorrect solution estimate) to the next. This is
essentially a chaotic downhill search scheme which mimics the
cooling history of a thermodynamic system.
  Because of the technique used there is no way to estimate the 
accuracy of the coefficients for a single solution. Rerun the program
several times. Note that the routine Metropolis will take
a different solution route each attempt (based upon the time of day)
so you cannot ever repeat yourself (or cheat).


.page

 STARTING PROGRAM (in tutor mode):

  There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  

The menu points have the following meanings:

1. Select the first menu point to input the general parameters for the program
   such as the names of input photometric catalog IBIS file, the type of 
   photometric funtion, the fit conditions and the kind of outputs. 

2. This point contains all function specific parameters (first guess, upper and 
   lower limits, first temperatur). The name of this menu point is changing 
   depending on your input of the parameter PHO_FUNC in the first menu point. 

3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save photfit2.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


.page

HELPS :

- You will get the common help contained in the ".mdf" file (photfit2.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (photfit2.pdf) 
  by processing of "help-help" applied to the program (should be verry 
  similary ones of photfit2.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.



.page

TESTING and EXAMPLES:

  You can test PHOTFIT2 with program PHOTTEST2:

  phottest out=phottest_m.dat PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.7 EXPONENT=0.6 +
  START=(10,10,10) DELTA=(30,30,180) SIGMA=0.000001 

  photfit2 inp=(phottest_m.dat,phottest_m.dat) PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.6 MIN_ALBEDO=0.0 MAX_ALBEDO=1.0 T_ALBEDO=0.1 +
  EXPONENT=0.6 MIN_EXPONENT=0.0 MAX_EXPONENT=1.0 T_EXPONENT=0.1 +
  NORM=25 RERUN=2 MAXITER=100 NUMTEN=25 METRO=20 PERCENT=90 TOLERANC=0.02 'PRINT 

  or, to actually run it you must generate a bunch of photom/hwphoca*
  files:

  photom INP=pix#1 out=ibis1    ( interactive job )
  photom INP=(pix#2,ibis1) out=ibis2 'batch   (batch mode)
  photom INP=(pix#3,ibis1) out=ibis3 'batch   (batch mode)
  photom INP=(pix#4,ibis1) out=ibis4 'batch   (batch mode)
  photom INP=(pix#5,ibis1) out=ibis5 'batch   (batch mode)
  photom INP=(pix#6,ibis1) out=ibis6 'batch   (batch mode)
  photfit2 INP=(ibis1,ibis2,ibis3,ibis4,ibis5,ibis6) +
           PHO_FUNC=HAPKE_86_HG1  MAXITER=20000

.page



INPUT

PHOTFIT2 accepts two types of input files - the old IBIS1 file and a IBIS2 file 
of the type=phocat.


IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values.


PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 



.page


SUBROUTINES REQUIRED TO RUN PROGRAM:	pho_routines package,
					PHOPDF package

INCLUDE FILES REQUIRED TO RUN PROGRAM:	pho.h,
					pho_global.pdf,
					ibisfile.h, ibisfile.h, 
					vicmain_c, defines.h, 
					math.h, time.h 

	

BACKGROUND AND REFERENCES :	Jean J. Lorre, 
				Function Minimization With Partially Corrected 
				Data Via Simulated Annealing,
				J. Soc. Ind. Appl. Math., 43 (1990), 123-127


SOFTWARE PLATFORM :		VICAR, TAE
				(AXP/SUNOS/SOLARIS/SGI)

HARDWARE PLATFORM :		No particular hardware required;
				tested on AXP/SUNOS/SOLARIS/SGI

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Programmer: J J Lorre,  Jan 10 1988
				rewritten in C: Friedel Oschuetz, Nov. '95,
Revisions:
 14 Jun 1996  GMY  Removes tabs from IMAKE file (DFR).

 25 Mar 1998  TXH  Modified to work with new IBIS prototypes (DFR).

  4 Sep 2014  WLB  Corrected several memory allocation bugs.

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)


.LEVEL1

.VARI INP
A
photometric catalog

.VARI PHO_FUNC
photometric function

.VARIABLE CLASS_ID
Class-id

.VARIABLE NORM
Causes subroutine Metropolis 
to renormalize itself.

.VARIABLE RERUN
Number of rerun of metropolis

.VARIABLE MAXITER
Specifies the total number of 
successful iterations.

.VARIABLE NUMTEN
Number of iterations
before temperature
drops by a factor of ten.

.VARIABLE PERCENT
Minimum acceptable # of points
with residuals below tolerance.

.VARIABLE TOLERANC
The I/F residual tolerance.

.VARIABLE PRINT
Screen output  
of the IBIS input files.

.VARIABLE METROP
List the iteration progress.

.VARI ALBEDO
Surface albedo

.var MIN_ALBEDO
Minimum of surface albedo

.var MAX_ALBEDO
Maximum of surface albedo

.var T_ALBEDO
Temperatur of Surface albedo

.var EXPONENT
Minnaert exponent

.var MIN_EXPONENT
Maximum of Minnaert exponent

.var MAX_EXPONENT
Maximum of Minnaert exponent

.var T_EXPONENT
Temperatur of Minnaert exponent

.VARI A_VEVERKA 
Veverka parameter

.VARI MIN_A_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_A_VEVERKA 
Maximum of Veverka parameter

.VARI T_A_VEVERKA 
Temperatur of Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI MIN_B_VEVERKA
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA
Maximum of Veverka parameter

.VARI T_B_VEVERKA
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MIN_D_VEVERKA
Minimum of Veverka parameter

.VARI MAX_D_VEVERKA
Maximum of Veverka parameter

.VARI T_D_VEVERKA
Temperatur of Veverka parameter

.VARI MO_EXP1
Mosher's exponent

.VARI MIN_MO_EXP1
Minimum of Mosher's exponent

.VARI MAX_MO_EXP1
Maximum of Mosher's exponent

.VARI T_MO_EXP1
Temperatur of Mosher's exponent

.VARI MO_EXP2
Mosher's exponent

.VARI MIN_MO_EXP2
Minimum of Mosher's exponent

.VARI MAX_MO_EXP2
Maximum of Mosher's exponent

.VARI T_MO_EXP2
Temperatur of Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI MIN_E_BURATTI
Minimum of Buratti's parameter

.VARI MAX_E_BURATTI
Maximum of Buratti's parameter

.VARI T_E_BURATTI
Temperatur of Buratti's parameter

.VARI DEN_SOIL
Density of the soil

.var MIN_DEN_SOIL
Minimum of density of the soil

.var MAX_DEN_SOIL
Maximum of density of the soil

.var T_DEN_SOIL
Temperatur of density of the soil

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.VARI HG1_SOIL
Henyey-Greenstein term

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG2_SOIL
Henyey-Greenstein term

.var MIN_HG2_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG2_SOIL
Maximum of Henyey-Greenstein term

.VARI T_HG2_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG_ASY_SOIL
Asymetry term of
Henyey-Greenstein

.VARI MIN_HG_ASY_SOIL
Minimum of asymetry parameter

.VARI MAX_HG_ASY_SOIL
Maximum of asymetry parameter

.VARI T_HG_ASY_SOIL
Temperatur of asymetry parameter

.VARI LE1_SOIL
Hapke parameter
First Legendre-Polynom

.VARI MIN_LE1_SOIL
Minimum of 
first Legendre-Polynom

.VARI MAX_LE1_SOIL
Maximum of 
first Legendre-Polynom

.VARI T_LE1_SOIL
Temperatur of 
first Legendre-Polynom

.VARI LE2_SOIL
Second Legendre-Polynom

.VARI MIN_LE2_SOIL
Minimum of 
second Legendre-Polynom

.VARI MAX_LE2_SOIL
Maximum of 
second Legendre-Polynom

.VARI T_LE2_SOIL
Temperatur of 
second Legendre-Polynom

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of width of opposition magnitude

.VARI H_CBOE
Width of opposition surge
due by coherent backscatter

.VARI MIN_H_CBOE
Minimum of width of opposition surge
due by coherent backscatter

.VARI MAX_H_CBOE
Maximum of width of opposition surge
due by coherent backscatter

.VARI T_H_CBOE
Temperatur of 
width of opposition surge
due by coherent backscatter

.VARI B_CBOE
Opposition magnitude
due by coherent backscatter

.VARI MIN_B_CBOE
Minimum of opposition magnitude
due by coherent backscatter

.VARI MAX_B_CBOE
Maximum of opposition magnitude
due by coherent backscatter

.VARI T_B_CBOE
Temperatur of 
opposition magnitude
due by coherent backscatter

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.VARI COOK
Hapke-Cook parameter

.VARI MIN_COOK
Minimum of Hapke-Cook parameter

.VARI MAX_COOK
Maximum of Hapke-Cook parameter

.VARI T_COOK
Temperatur of Hapke-Cook parameter

.VARI TAU_ATM
Atmospheric optical depth

.VARI MIN_TAU_ATM
Minimum of 
atmospheric optical depth

.VARI MAX_TAU_ATM
Maximum of 
atmospheric optical depth

.VARI T_TAU_ATM
Temperatur of  
atmospheric optical depth

.VARI W_ATM
Atmospheric single scattering albedo

.VARI MIN_W_ATM
Minimum of 
atmospheric single scattering albedo

.VARI MAX_W_ATM
Maximum of 
atmospheric single scattering albedo

.VARI T_W_ATM
Temperatur of 
atmospheric ingle scattering albedo

.VARI HG1_ATM
Atmospheric Henyey-Greenstein term

.VARI MIN_HG1_ATM
Minimum of 
atmospheric Henyey-Greenstein term

.VARI MAX_HG1_ATM
Maximum of 
atmospheric Henyey-Greenstein term

.VARI T_HG1_ATM
Temperatur of
atmospheric Henyey-Greenstein term

.vari IRV_EXP1
Irvine's first exponent

.vari MIN_IRV_EXP1
Minimum of Irvine's first exponent

.vari MAX_IRV_EXP1
Maximum of Irvine's first exponent

.vari T_IRV_EXP1
Temperatur of Irvine's first exponent

.vari IRV_EXP2
Irvine's second exponent

.vari MIN_IRV_EXP2
Minimum of Irvine's second exponent

.vari MAX_IRV_EXP2
Maximum of Irvine's second exponent

.vari T_IRV_EXP2
Temperatur of Irvine's second exponent

.VARIABLE CLASS_ID
Class-id

.VARI SAVE_PAR
file name for par-file


.LEVEL2

.VARI INP
File names of the input input IBIS photometric catalog files.
PHOTFIT2 accepts two types of files - the old IBIS1 file and a IBIS2 file of 
the type=phocat.

IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values.

.page

PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 

.VARI PHO_FUNC
Photometric function :

This parameter of the first menu point selects the menu point for input the 
photometry task:
When returning to the highest level of the menu (i.e. the PHOTFIT2.MDF-file) 
you will see that the third selection point has been changed according to your 
input of PHO_FUNC in the first menu point.

.VARIABLE CLASS_ID
The "phocat" file can contain data of different classes. The class_id numerates 
the photometric functions. For using different photometric functions or 
parameter sets. The program PHOTFIT2 will using the data of given class only 
(or all data if CLASS_ID is not given).

.VARIABLE NORM
Causes subroutine Metropolis to renormalize itself by recomputing the
Boltzmann coefficient. NORM=n causes renormalization each n successful
iterations. 

.VARIABLE RERUN
Number of rerun of metropolis. You can see the stability of the results. But be 
aware, the mean values and there deviations of the parameters are not real 
statistical values because every rerun of metropolis starts with the best fit 
of the run before.  
Default for RERUN is 1

.VARIABLE MAXITER
Specifies the number of successful iterations which Metropolis will
perform before ceasing in it's hunt for the coefficient values.
Usefully is for MAXITER is:
for MINNAERT 5000
for VEVERKA  20000
for HAPKE_* 20000
for HAPKE_* 20000 

.VARIABLE NUMTEN
Specifies the number of successful iterations which must be 
accumulated before the width of the solution generating  probability
function drops by a factor of ten. If for example MAXITER/NUMTEN
is 4.0 then the initial range specified by the temperatur
parameter (the starting temperature) is reduced by 4.0 orders
of magnitude (10000:1) by the time the iteration process has
ceased. 
Default for NUMTEN is: MAXITER/4

.VARIABLE PERCENT
The minimum acceptable # of points with residuals below tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.VARIABLE TOLERANC
The I/F residual tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.VARIABLE PRINT
Keyword for screen output of the IBIS input files.
NOPRINT deactivates the sceen output of IBIS input file.

.VARIABLE METROP
Causes subroutine Metropolis to list the iteration progress as it
converges upon the solution. METROP=n causes a printout each
n successful iterations. 

.VARI SAVE_PAR
This is the name for the TAE-parameter file containing all parameters 
needed to running the program. The default name is PHOTFIT2.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.
This parameter gives the albedo of the surface. 

.var MIN_ALBEDO
This parameter gives the absolut lower limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_ALBEDO
This parameter gives the absolut upper limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_ALBEDO
This parameter gives temperatur for the albedo of the surface. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI MIN_EXPONENT
This parameter gives the absolut lower limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_EXPONENT
This parameter gives the absolut upper limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_EXPONENT
This parameter gives temperatur for the Exponent - the geometrical constant k
of the Minnaert photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    EXPONENT_NEW = T_EXPONENT * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_EXPONENT_NEW_* = T_EXPONENT_OLD_* * scale, 
scale depends of NUMTEN.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_A_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_A_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_A_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    A_VEVERKA_NEW = T_A_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_A_VEVERKA_NEW_* = T_A_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI MIN_B_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_C_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MIN_MO_EXP1
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP1
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP1
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP2).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP1_NEW = T_MO_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP1_NEW_* = T_MO_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI MIN_MO_EXP2
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP2
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP2
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP1).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP2_NEW = T_MO_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP2_NEW_* = T_MO_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

.VARI DEN_SOIL
Specific volume density of the soil.

.var MIN_DEN_SOIL
This parameter gives the absolut lower limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_DEN_SOIL
This parameter gives the absolut upper limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_DEN_SOIL
This parameter gives temperatur for the specific volume density of the soil.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    DEN_SOIL_NEW = T_DEN_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_DEN_SOIL_NEW_* = T_DEN_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI MIN_HG2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG2_SOIL_NEW = T_HG2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG2_SOIL_NEW_* = T_HG2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG_ASY_SOIL
This parameter gives the asymmetry parameter (weight of the two terms in the 
Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MIN_HG_ASY_SOIL
This parameter gives the absolut lower limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MAX_HG_ASY_SOIL
This parameter gives the absolut upper limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG_ASY_SOIL
This parameter gives temperatur for the parameter of the asymmetry parameter (weight of the two terms in the Henyey-Greenstein soil phase function).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG_ASY_SOIL_NEW = T_HG_ASY_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG_ASY_SOIL_NEW_* = T_HG_ASY_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI MIN_H_CBOE
This parameter gives the absolut lower limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI MAX_H_CBOE
This parameter gives the absolut upper limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI T_H_CBOE
This parameter gives temperatur for the parameter of the coherent 
backscattering ( width of theopposition surge due to the backscatter ).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_CBOE_NEW = T_H_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_CBOE_NEW_* = T_H_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI MIN_B_CBOE
This parameter gives the absolut lower limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI MAX_B_CBOE
This parameter gives the absolut upper limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI T_B_CBOE
This parameter gives temperatur for the opposition magnitude coefficient of the 
coherent backscattering (height of opposition surge due to backscatter). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_CBOE_NEW = T_B_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_CBOE_NEW_* = T_B_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI MIN_COOK
This parameter gives the absolut lower limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_COOK
This parameter gives the absolut upper limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_COOK
This parameter gives temperatur for the parameter of the Cook's modification 
of the old Hapke function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    COOK_NEW = T_COOK * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_COOK_NEW_* = T_COOK_OLD_* * scale, 
scale depends of NUMTEN.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI MIN_TAU_ATM
This parameter gives the absolut lower limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_TAU_ATM
This parameter gives the absolut upper limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_TAU_ATM
This parameter gives temperatur for the optical depth of the atmosphere.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    TAU_ATM_NEW = T_TAU_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_TAU_ATM_NEW_* = T_TAU_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI MIN_W_ATM
This parameter gives the absolut lower limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_W_ATM
This parameter gives the absolut upper limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_W_ATM
This parameter gives temperatur for the single scattering albedo of the 
atmospheric aerosols.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_ATM_NEW = T_W_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_ATM_NEW_* = T_W_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI MIN_HG1_ATM
This parameter gives the absolut lower limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG1_ATM
This parameter gives the absolut upper limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG1_ATM
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein atmospheric phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_ATM_NEW = T_HG1_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_ATM_NEW_* = T_HG1_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP1
Irvine's first exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP1
This parameter gives the absolut lower limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP1
This parameter gives the absolut upper limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP1
This parameter gives temperatur for the Irvine's first exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
   IRV_EXP1_NEW = T_IRV_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP1_NEW_* = T_IRV_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP2
Irvine's second exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP2
This parameter gives the absolut lower limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP2
This parameter gives the absolut upper limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP2
This parameter gives temperatur for the Irvine's second exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    IRV_EXP2_NEW = T_IRV_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP2_NEW_* = T_IRV_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2m.pdf
  procedure option=selftutor help=*

  !*******************************************************************
  ! If OPTIONS=SELFTUTOR (see the first line of this procedure), the
  ! TAE Terminal Monitor (TM) does not do the tutoring, i.e. when a
  ! user requests t(utor) for a SELFTUTOR procedure, the procedure
  ! is executed immediately to perform its own version of tutor.
  !*******************************************************************


	!*************************************************************
	! The process of the main program part is named
	! PHOTFIT2 and has to be declared here.
	!*************************************************************

	process name=PHOTFIT2
	end-proc

	!*************************************************************
	! The following definitions/defaults will be used in 
	! the batch modus :
	!*************************************************************
	


  ! general input parameters :



	parm INP	type=(string,32) count=0:20	default=photcat.dat


	! photometric functions :

	parm PHO_FUNC type=(string,32) count=1 		+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default=HAPKE_86_LE2

	PARM CLASS_ID TYPE=INTEGER COUNT=0:1 valid=(0:32767) DEFAULT=--

  	PARM NORM    TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=500
  	PARM RERUN   TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM MAXITER TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM NUMTEN  TYPE=INTEGER COUNT=0:1 DEFAULT=--
  	PARM PERCENT TYPE=REAL COUNT=1 VALID=(1.:100.) DEFAULT=90.
  	PARM TOLERANC TYPE=REAL COUNT=1 VALID=(0.:1.) DEFAULT=.02

!  	PARM PLOT     TYPE=KEYWORD DEFAULT=NOPLOT	+ 
!		      VALID=(PLOT, NOPLOT) 
  	PARM PRINT    TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT
!  	PARM DISP     TYPE=KEYWORD VALID=(DISP,NODISP,WAITNOMORE) +
!    				DEFAULT=NODISP
  	PARM METROP  TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=0




  ! photometric parameters:

!       parm PHO_PAR_FILE	string  count=0:1 		default=--

       parm ALBEDO 		real count=0:1 			default=--
       parm MIN_ALBEDO 		real count=0:1 			default=--
       parm MAX_ALBEDO 		real count=0:1 			default=--
       parm T_ALBEDO 		real count=0:1 			default=--
       parm EXPONENT 		real count=0:1 valid=(0:1)	default=--
       parm MIN_EXPONENT 	real count=0:1 valid=(0:1)	default=--
       parm MAX_EXPONENT 	real count=0:1 valid=(0:1)	default=--
       parm T_EXPONENT 		real count=0:1 valid=(0:1)	default=--
       parm A_VEVERKA 		real count=0:1 			default=--
       parm MIN_A_VEVERKA 	real count=0:1 			default=--
       parm MAX_A_VEVERKA 	real count=0:1 			default=--
       parm T_A_VEVERKA 	real count=0:1 			default=--
       parm B_VEVERKA 		real count=0:1 			default=--
       parm MIN_B_VEVERKA 	real count=0:1 			default=--
       parm MAX_B_VEVERKA 	real count=0:1 			default=--
       parm T_B_VEVERKA 	real count=0:1 			default=--
       parm C_VEVERKA 		real count=0:1 			default=--
       parm MIN_C_VEVERKA 	real count=0:1 			default=--
       parm MAX_C_VEVERKA 	real count=0:1 			default=--
       parm T_C_VEVERKA 	real count=0:1 			default=--
       parm D_VEVERKA 		real count=0:1 			default=--
       parm MIN_D_VEVERKA 	real count=0:1 			default=--
       parm MAX_D_VEVERKA 	real count=0:1 			default=--
       parm T_D_VEVERKA 	real count=0:1 			default=--
       parm MO_EXP1 		real count=0:1 			default=--
       parm MIN_MO_EXP1 	real count=0:1 			default=--
       parm MAX_MO_EXP1 	real count=0:1 			default=--
       parm T_MO_EXP1 		real count=0:1 			default=--
       parm MO_EXP2 		real count=0:1 			default=--
       parm MIN_MO_EXP2 	real count=0:1 			default=--
       parm MAX_MO_EXP2 	real count=0:1 			default=--
       parm T_MO_EXP2 		real count=0:1 			default=--
       parm E_BURATTI 		real count=0:1 			default=--
       parm MIN_E_BURATTI 	real count=0:1 			default=--
       parm MAX_E_BURATTI 	real count=0:1 			default=--
       parm T_E_BURATTI 	real count=0:1 			default=--
       parm DEN_SOIL 		real count=0:1 			default=--
       parm MIN_DEN_SOIL 	real count=0:1 			default=--
       parm MAX_DEN_SOIL 	real count=0:1 			default=--
       parm T_DEN_SOIL 		real count=0:1 			default=--
       parm W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm MIN_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm MAX_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm T_W_SOIL 		real count=0:1 valid=(0:1)	default=--
       parm HG1_SOIL 		real count=0:1 			default=--
       parm MIN_HG1_SOIL 	real count=0:1 			default=--
       parm MAX_HG1_SOIL 	real count=0:1 			default=--
       parm T_HG1_SOIL 		real count=0:1 			default=--
       parm HG2_SOIL 		real count=0:1 			default=--
       parm MIN_HG2_SOIL 	real count=0:1 			default=--
       parm MAX_HG2_SOIL 	real count=0:1 			default=--
       parm T_HG2_SOIL 		real count=0:1 			default=--
       parm HG_ASY_SOIL 	real count=0:1 			default=--
       parm MIN_HG_ASY_SOIL 	real count=0:1 			default=--
       parm MAX_HG_ASY_SOIL 	real count=0:1 			default=--
       parm T_HG_ASY_SOIL 	real count=0:1 			default=--
       parm LE1_SOIL 		real count=0:1 			default=--
       parm MIN_LE1_SOIL 	real count=0:1 			default=--
       parm MAX_LE1_SOIL 	real count=0:1 			default=--
       parm T_LE1_SOIL 		real count=0:1 			default=--
       parm LE2_SOIL 		real count=0:1 			default=--
       parm MIN_LE2_SOIL 	real count=0:1 			default=--
       parm MAX_LE2_SOIL 	real count=0:1 			default=--
       parm T_LE2_SOIL 		real count=0:1 			default=--
       parm H_SHOE 		real count=0:1 			default=--
       parm MIN_H_SHOE 		real count=0:1 			default=--
       parm MAX_H_SHOE 		real count=0:1 			default=--
       parm T_H_SHOE 		real count=0:1 			default=--
       parm B_SHOE 		real count=0:1 			default=--
       parm MIN_B_SHOE 		real count=0:1 			default=--
       parm MAX_B_SHOE 		real count=0:1 			default=--
       parm T_B_SHOE 		real count=0:1 			default=--
       parm H_CBOE 		real count=0:1 			default=--
       parm MIN_H_CBOE 		real count=0:1 			default=--
       parm MAX_H_CBOE 		real count=0:1 			default=--
       parm T_H_CBOE 		real count=0:1 			default=--
       parm B_CBOE 		real count=0:1 			default=--
       parm MIN_B_CBOE 		real count=0:1 			default=--
       parm MAX_B_CBOE 		real count=0:1 			default=--
       parm T_B_CBOE 		real count=0:1 			default=--
       parm THETA 		real count=0:1 			default=--
       parm MIN_THETA 		real count=0:1 			default=--
       parm MAX_THETA 		real count=0:1 			default=--
       parm T_THETA 		real count=0:1 			default=--
       parm COOK 		real count=0:1 			default=--
       parm MIN_COOK 		real count=0:1 			default=--
       parm MAX_COOK 		real count=0:1 			default=--
       parm T_COOK 		real count=0:1 			default=--
       parm TAU_ATM 		real count=0:1 			default=--
       parm MIN_TAU_ATM 	real count=0:1 			default=--
       parm MAX_TAU_ATM 	real count=0:1 			default=--
       parm T_TAU_ATM 		real count=0:1 			default=--
       parm W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm MIN_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm MAX_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm T_W_ATM 		real count=0:1 valid=(0:1)	default=--
       parm HG1_ATM 		real count=0:1 			default=--
       parm MIN_HG1_ATM 	real count=0:1 			default=--
       parm MAX_HG1_ATM 	real count=0:1 			default=--
       parm T_HG1_ATM 		real count=0:1 			default=--
       parm IRV_EXP1 		real count=0:1 			default=--
       parm MIN_IRV_EXP1 	real count=0:1 			default=--
       parm MAX_IRV_EXP1 	real count=0:1 			default=--
       parm T_IRV_EXP1 		real count=0:1 			default=--
       parm IRV_EXP2 		real count=0:1 			default=--
       parm MIN_IRV_EXP2 	real count=0:1 			default=--
       parm MAX_IRV_EXP2 	real count=0:1 			default=--
       parm T_IRV_EXP2 		real count=0:1 			default=--


      !*******************************************************************
      ! local variable for the photometric parameters to enable different
      !	default values of the same parameter in different photometric 
      ! functions :
      !*******************************************************************

       local ALBEDO_count 	int 
       local ALBEDO_mic 	int 
       local ALBEDO_mac 	int 
       local ALBEDO_tc	 	int 
       local EXPONENT_count 	int 
       local EXPONENT_mic 	int 
       local EXPONENT_mac 	int 
       local EXPONENT_tc 	int 
       local A_VEVERKA_count 	int
       local A_VEVERKA_mic 	int
       local A_VEVERKA_mac 	int
       local A_VEVERKA_tc 	int
       local B_VEVERKA_count 	int
       local B_VEVERKA_mic 	int
       local B_VEVERKA_mac 	int
       local B_VEVERKA_tc 	int
       local C_VEVERKA_count	int
       local C_VEVERKA_mic	int
       local C_VEVERKA_mac	int
       local C_VEVERKA_tc	int
       local D_VEVERKA_count 	int 
       local D_VEVERKA_mic 	int 
       local D_VEVERKA_mac 	int 
       local D_VEVERKA_tc 	int 
       local MO_EXP1_count 	int 
       local MO_EXP1_mic 	int 
       local MO_EXP1_mac 	int 
       local MO_EXP1_tc 	int 
       local MO_EXP2_count 	int
       local MO_EXP2_mic 	int
       local MO_EXP2_mac 	int
       local MO_EXP2_tc 	int
       local E_BURATTI_count 	int
       local E_BURATTI_mic 	int
       local E_BURATTI_mac 	int
       local E_BURATTI_tc 	int
       local DEN_SOIL_count 	int
       local DEN_SOIL_mic 	int
       local DEN_SOIL_mac 	int
       local DEN_SOIL_tc 	int
       local W_SOIL_count 	int
       local W_SOIL_mic 	int
       local W_SOIL_mac 	int
       local W_SOIL_tc	 	int
       local HG1_SOIL_count 	int
       local HG1_SOIL_mic 	int
       local HG1_SOIL_mac 	int
       local HG1_SOIL_tc 	int
       local HG2_SOIL_count 	int
       local HG2_SOIL_mic 	int
       local HG2_SOIL_mac 	int
       local HG2_SOIL_tc 	int
       local HG_ASY_SOIL_cou 	int
       local HG_ASY_SOIL_mic 	int
       local HG_ASY_SOIL_mac 	int
       local HG_ASY_SOIL_tc 	int
       local LE1_SOIL_count 	int
       local LE1_SOIL_mic 	int
       local LE1_SOIL_mac 	int
       local LE1_SOIL_tc 	int
       local LE2_SOIL_count 	int
       local LE2_SOIL_mic 	int
       local LE2_SOIL_mac 	int
       local LE2_SOIL_tc 	int
       local H_SHOE_count 	int
       local H_SHOE_mic 	int
       local H_SHOE_mac 	int
       local H_SHOE_tc	 	int
       local B_SHOE_count 	int
       local B_SHOE_mic 	int
       local B_SHOE_mac 	int
       local B_SHOE_tc	 	int
       local H_CBOE_count 	int
       local H_CBOE_mic 	int
       local H_CBOE_mac 	int
       local H_CBOE_tc	 	int
       local B_CBOE_count 	int
       local B_CBOE_mic 	int
       local B_CBOE_mac 	int
       local B_CBOE_tc	 	int
       local THETA_count 	int
       local THETA_mic	 	int
       local THETA_mac	 	int
       local THETA_tc	 	int
       local COOK_count 	int
       local COOK_mic	 	int
       local COOK_mac	 	int
       local COOK_tc	 	int
       local TAU_ATM_count 	int
       local TAU_ATM_mic 	int
       local TAU_ATM_mac 	int
       local TAU_ATM_tc 	int
       local W_ATM_count 	int
       local W_ATM_mic	 	int
       local W_ATM_mac	 	int
       local W_ATM_tc	 	int
       local HG1_ATM_count 	int
       local HG1_ATM_mic 	int
       local HG1_ATM_mac 	int
       local HG1_ATM_tc 	int
       local IRV_EXP1_count	int
       local IRV_EXP1_mic	int
       local IRV_EXP1_mac	int
       local IRV_EXP1_tc	int
       local IRV_EXP2_count  	int
       local IRV_EXP2_mic  	int
       local IRV_EXP2_mac  	int
       local IRV_EXP2_tc  	int




  ! the name of the parameter file :

       parm SAVE_PAR	    type=(string,40) count=1    default=last.par
       parm MAIN_PROC_NAME	type=string count=1	default=PHOTFIT2M



  ! for running the main program: 

       parm readparam	keyword		 count=0:1	valid=read def=--

	!*******************************************************************
	! The globals "$MENUOPT" and "SYSCHAR" are used in this proc,
	! so we have to declare they here with the command REFGBL
	!*******************************************************************

	refgbl $menuopt
	refgbl $syschar

  body


  !*************************************************************************
  ! "pho_global.pdf" is being executed in the following line. 
  ! In this PDF the global PHO_FUNC_type is defined. 
  !*************************************************************************

  pho_global PHO_FUNC_type = "&PHO_FUNC" pho_PROC_NAME="&_PROC"



  ! for different photometric funtions :

  !*******************************************************************
  ! set the value oft local variable parameter-count to the beginning 
  ! parameter_count:
  !*******************************************************************

  let ALBEDO_count 	=$count(ALBEDO)
  let ALBEDO_mic 	=$count(MIN_ALBEDO)
  let ALBEDO_mac 	=$count(MAX_ALBEDO)
  let ALBEDO_tc 	=$count(T_ALBEDO)
  let EXPONENT_count 	=$count(EXPONENT)
  let EXPONENT_mic 	=$count(MIN_EXPONENT)
  let EXPONENT_mac 	=$count(MAX_EXPONENT)
  let EXPONENT_tc 	=$count(T_EXPONENT)
  let A_VEVERKA_count 	=$count(A_VEVERKA)
  let A_VEVERKA_mic 	=$count(MIN_A_VEVERKA)
  let A_VEVERKA_mac 	=$count(MAX_A_VEVERKA)
  let A_VEVERKA_tc 	=$count(T_A_VEVERKA)
  let B_VEVERKA_count 	=$count(B_VEVERKA)
  let B_VEVERKA_mic 	=$count(MIN_B_VEVERKA)
  let B_VEVERKA_mac 	=$count(MAX_B_VEVERKA)
  let B_VEVERKA_tc 	=$count(T_B_VEVERKA)
  let C_VEVERKA_count	=$count(C_VEVERKA)
  let C_VEVERKA_mic	=$count(MIN_C_VEVERKA)
  let C_VEVERKA_mac	=$count(MAX_C_VEVERKA)
  let C_VEVERKA_tc	=$count(T_C_VEVERKA)
  let D_VEVERKA_count 	=$count(D_VEVERKA)
  let D_VEVERKA_mic 	=$count(MIN_D_VEVERKA)
  let D_VEVERKA_mac 	=$count(MAX_D_VEVERKA)
  let D_VEVERKA_tc 	=$count(T_D_VEVERKA)
  let MO_EXP1_count 	=$count(MO_EXP1)
  let MO_EXP1_mic 	=$count(MIN_MO_EXP1)
  let MO_EXP1_mac 	=$count(MAX_MO_EXP1)
  let MO_EXP1_tc 	=$count(T_MO_EXP1)
  let MO_EXP2_count 	=$count(MO_EXP2)
  let MO_EXP2_mic 	=$count(MIN_MO_EXP2)
  let MO_EXP2_mac 	=$count(MAX_MO_EXP2)
  let MO_EXP2_tc 	=$count(T_MO_EXP2)
  let E_BURATTI_count 	=$count(E_BURATTI)
  let E_BURATTI_mic 	=$count(MIN_E_BURATTI)
  let E_BURATTI_mac 	=$count(MAX_E_BURATTI)
  let E_BURATTI_tc 	=$count(T_E_BURATTI)
  let DEN_SOIL_count 	=$count(DEN_SOIL)
  let DEN_SOIL_mic 	=$count(MIN_DEN_SOIL)
  let DEN_SOIL_mac 	=$count(MAX_DEN_SOIL)
  let DEN_SOIL_tc 	=$count(T_DEN_SOIL)
  let W_SOIL_count 	=$count(W_SOIL)
  let W_SOIL_mic 	=$count(MIN_W_SOIL)
  let W_SOIL_mac 	=$count(MAX_W_SOIL)
  let W_SOIL_tc 	=$count(T_W_SOIL)
  let HG1_SOIL_count 	=$count(HG1_SOIL)
  let HG1_SOIL_mic 	=$count(MIN_HG1_SOIL)
  let HG1_SOIL_mac 	=$count(MAX_HG1_SOIL)
  let HG1_SOIL_tc 	=$count(T_HG1_SOIL)
  let HG2_SOIL_count 	=$count(HG2_SOIL)
  let HG2_SOIL_mic 	=$count(MIN_HG2_SOIL)
  let HG2_SOIL_mac 	=$count(MAX_HG2_SOIL)
  let HG2_SOIL_tc 	=$count(T_HG2_SOIL)
  let HG_ASY_SOIL_cou 	=$count(HG_ASY_SOIL)
  let HG_ASY_SOIL_mic 	=$count(MIN_HG_ASY_SOIL)
  let HG_ASY_SOIL_mac 	=$count(MAX_HG_ASY_SOIL)
  let HG_ASY_SOIL_tc 	=$count(T_HG_ASY_SOIL)
  let LE1_SOIL_count 	=$count(LE1_SOIL)
  let LE1_SOIL_mic 	=$count(MIN_LE1_SOIL)
  let LE1_SOIL_mac 	=$count(MAX_LE1_SOIL)
  let LE1_SOIL_tc 	=$count(T_LE1_SOIL)
  let LE2_SOIL_count 	=$count(LE2_SOIL)
  let LE2_SOIL_mic 	=$count(MIN_LE2_SOIL)
  let LE2_SOIL_mac 	=$count(MAX_LE2_SOIL)
  let LE2_SOIL_tc 	=$count(T_LE2_SOIL)
  let H_SHOE_count 	=$count(H_SHOE)
  let H_SHOE_mic 	=$count(MIN_H_SHOE)
  let H_SHOE_mac 	=$count(MAX_H_SHOE)
  let H_SHOE_tc 	=$count(T_H_SHOE)
  let B_SHOE_count 	=$count(B_SHOE)
  let B_SHOE_mic 	=$count(MIN_B_SHOE)
  let B_SHOE_mac 	=$count(MAX_B_SHOE)
  let B_SHOE_tc 	=$count(T_B_SHOE)
  let H_CBOE_count 	=$count(H_CBOE)
  let H_CBOE_mic 	=$count(MIN_H_CBOE)
  let H_CBOE_mac 	=$count(MAX_H_CBOE)
  let H_CBOE_tc 	=$count(T_H_CBOE)
  let B_CBOE_count 	=$count(B_CBOE)
  let B_CBOE_mic 	=$count(MIN_B_CBOE)
  let B_CBOE_mac 	=$count(MAX_B_CBOE)
  let B_CBOE_tc 	=$count(T_B_CBOE)
  let THETA_count 	=$count(THETA)
  let THETA_mic 	=$count(MIN_THETA)
  let THETA_mac 	=$count(MAX_THETA)
  let THETA_tc	 	=$count(T_THETA)
  let COOK_count 	=$count(COOK)
  let COOK_mic	 	=$count(MIN_COOK)
  let COOK_mac	 	=$count(MAX_COOK)
  let COOK_tc	 	=$count(T_COOK)
  let TAU_ATM_count 	=$count(TAU_ATM)
  let TAU_ATM_mic 	=$count(MIN_TAU_ATM)
  let TAU_ATM_mac 	=$count(MAX_TAU_ATM)
  let TAU_ATM_tc 	=$count(T_TAU_ATM)
  let W_ATM_count 	=$count(W_ATM)
  let W_ATM_mic 	=$count(MIN_W_ATM)
  let W_ATM_mac 	=$count(MAX_W_ATM)
  let W_ATM_tc	 	=$count(T_W_ATM)
  let HG1_ATM_count 	=$count(HG1_ATM)
  let HG1_ATM_mic 	=$count(MIN_HG1_ATM)
  let HG1_ATM_mac 	=$count(MAX_HG1_ATM)
  let HG1_ATM_tc 	=$count(T_HG1_ATM)
  let IRV_EXP1_count	=$count(IRV_EXP1)
  let IRV_EXP1_mic	=$count(MIN_IRV_EXP1)
  let IRV_EXP1_mac	=$count(MAX_IRV_EXP1)
  let IRV_EXP1_tc	=$count(T_IRV_EXP1)
  let IRV_EXP2_count  	=$count(IRV_EXP2)
  let IRV_EXP2_mic  	=$count(MIN_IRV_EXP2)
  let IRV_EXP2_mac  	=$count(MAX_IRV_EXP2)
  let IRV_EXP2_tc  	=$count(T_IRV_EXP2)




    !*************************************************************************
    ! _TUTOR (type=integer) is an implicit local variable in a procedure
    ! with OPTIONS=SELFTUTOR (see first line of this proc). When TAE
    ! receives an initial tutor request for a procedure declared as selftutor,
    ! _TUTOR is set to one, otherwise it is set to zero.
    !*************************************************************************

    if (_tutor=1)

  	!*********************************************************************
  	! dummy files in case the procs aren't called :
  	! The save-variable (sub-)commmand is used to save the specified
  	! variables into the save file.
 	!
 	! Command:
 	! SAVE-VARIABLE FILE = save_file_name, VARIABLE = variable_list
  	!**********************************************************************

  	!**********************************************************************
  	! Saving the parameter-file with the photometric function parameters
  	! either the inputed parameters or if did not parameter input - the  
  	! default parameter for the actual photometric function :
  	!********************************************************************** 
	!**********************************************************************
	! At this position you can input function-specific default values 
	! for the function parameter for the tutor modus : 
	!**********************************************************************


  ! LAMBERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0
	if (ALBEDO_mic=0)				let MIN_ALBEDO=0.0
	if (ALBEDO_mac=0)				let MAX_ALBEDO=1.0
	if (ALBEDO_tc=0)				let T_ALBEDO=0.3

          save-var pho_lambert.par, (		+
				ALBEDO,		+
				MIN_ALBEDO,	+
				MAX_ALBEDO,	+
				T_ALBEDO	+
				)

	if (ALBEDO_count=0)	let ALBEDO=--
	if (ALBEDO_mic=0)	let MIN_ALBEDO=--
	if (ALBEDO_mac=0)	let MAX_ALBEDO=--
	if (ALBEDO_tc=0)	let T_ALBEDO=--


  ! MINNAERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0
	if (ALBEDO_mic=0)				let MIN_ALBEDO=0.0
	if (ALBEDO_mac=0)				let MAX_ALBEDO=1.0
	if (ALBEDO_tc=0)				let T_ALBEDO=0.3
	if (EXPONENT_count=0)				let EXPONENT=0.6
	if (EXPONENT_mic=0)				let MIN_EXPONENT=0.0
	if (EXPONENT_mac=0)				let MAX_EXPONENT=1.0
	if (EXPONENT_tc=0)				let T_EXPONENT=0.3

          save-var pho_minnaert.par, (		+
				ALBEDO,		+
				MIN_ALBEDO,	+
				MAX_ALBEDO,	+
				T_ALBEDO,	+
				EXPONENT,	+
				MIN_EXPONENT,	+
				MAX_EXPONENT,	+
				T_EXPONENT	+
				)


	if (ALBEDO_count=0)	let ALBEDO=--
	if (ALBEDO_mic=0)	let MIN_ALBEDO=--
	if (ALBEDO_mac=0)	let MAX_ALBEDO=--
	if (ALBEDO_tc=0)	let T_ALBEDO=--
	if (EXPONENT_count=0)	let EXPONENT=--
	if (EXPONENT_mic=0)	let MIN_EXPONENT=--
	if (EXPONENT_mac=0)	let MAX_EXPONENT=--
	if (EXPONENT_tc=0)	let T_EXPONENT=--

  ! IRVINE :

	if (EXPONENT_count=0)				let EXPONENT=0.9
	if (EXPONENT_mic=0)				let MIN_EXPONENT=0.0
	if (EXPONENT_mac=0)				let MAX_EXPONENT=1.0
	if (EXPONENT_tc=0)				let T_EXPONENT=0.3
	if (IRV_EXP1_count=0)				let IRV_EXP1=0.118
	if (IRV_EXP1_mic=0)				let MIN_IRV_EXP1=0.0
	if (IRV_EXP1_mac=0)				let MAX_IRV_EXP1=1.0
	if (IRV_EXP1_tc=0)				let T_IRV_EXP1=0.1
	if (IRV_EXP2_count=0)				let IRV_EXP2=0.0039
	if (IRV_EXP2_mic=0)				let MIN_IRV_EXP2=0.0
	if (IRV_EXP2_mac=0)				let MAX_IRV_EXP2=0.01
	if (IRV_EXP2_tc=0)				let T_IRV_EXP2=0.004

          save-var pho_irvine.par,  (		+
				EXPONENT,	+
				MIN_EXPONENT,	+
				MAX_EXPONENT,	+
				T_EXPONENT,	+
				IRV_EXP1,	+
				MIN_IRV_EXP1,	+
				MAX_IRV_EXP1,	+
				T_IRV_EXP1,	+
				IRV_EXP2,	+
				MIN_IRV_EXP2,	+
				MAX_IRV_EXP2,	+
				T_IRV_EXP2	+
				)

	if (EXPONENT_count=0)	let EXPONENT=--
	if (EXPONENT_mic=0)	let MIN_EXPONENT=--
	if (EXPONENT_mac=0)	let MAX_EXPONENT=--
	if (EXPONENT_tc=0)	let T_EXPONENT=--
	if (IRV_EXP1_count=0)	let IRV_EXP1=--
	if (IRV_EXP1_mic=0)	let MIN_IRV_EXP1=--
	if (IRV_EXP1_mac=0)	let MAX_IRV_EXP1=--
	if (IRV_EXP1_tc=0)	let T_IRV_EXP1=--
	if (IRV_EXP2_count=0)	let IRV_EXP2=--
	if (IRV_EXP2_mic=0)	let MIN_IRV_EXP2=--
	if (IRV_EXP2_mac=0)	let MAX_IRV_EXP2=--
	if (IRV_EXP2_tc=0)	let T_IRV_EXP2=--

  ! VEVERKA :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.997
	if (A_VEVERKA_mic=0) 				let MIN_A_VEVERKA=0.0
	if (A_VEVERKA_mac=0) 				let MAX_A_VEVERKA=1.5
	if (A_VEVERKA_tc=0) 				let T_A_VEVERKA=0.2
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (B_VEVERKA_mic=0) 				let MIN_B_VEVERKA=-3.0
	if (B_VEVERKA_mac=0) 				let MAX_B_VEVERKA=3.0
	if (B_VEVERKA_tc=0) 				let T_B_VEVERKA=0.05
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (C_VEVERKA_mic=0) 				let MIN_C_VEVERKA=-3.0
	if (C_VEVERKA_mac=0) 				let MAX_C_VEVERKA=3.0
	if (C_VEVERKA_tc=0) 				let T_C_VEVERKA=0.05
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (D_VEVERKA_mic=0) 				let MIN_D_VEVERKA=-2.0
	if (D_VEVERKA_mac=0) 				let MAX_D_VEVERKA=2.0
	if (D_VEVERKA_tc=0) 				let T_D_VEVERKA=0.02

          save-var pho_veverka.par, (		+
				A_VEVERKA,	+
				MIN_A_VEVERKA,	+
				MAX_A_VEVERKA,	+
				T_A_VEVERKA,	+
				B_VEVERKA,	+
				MIN_B_VEVERKA,	+
				MAX_B_VEVERKA,	+
				T_B_VEVERKA,	+
				C_VEVERKA,	+
				MIN_C_VEVERKA,	+
				MAX_C_VEVERKA,	+
				T_C_VEVERKA,	+
				D_VEVERKA,	+
				MIN_D_VEVERKA,	+
				MAX_D_VEVERKA,	+
				T_D_VEVERKA	+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (A_VEVERKA_mic=0) 	let MIN_A_VEVERKA=--
	if (A_VEVERKA_mac=0) 	let MAX_A_VEVERKA=--
	if (A_VEVERKA_tc=0) 	let T_A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (B_VEVERKA_mic=0) 	let MIN_B_VEVERKA=--
	if (B_VEVERKA_mac=0) 	let MAX_B_VEVERKA=--
	if (B_VEVERKA_tc=0) 	let T_B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (C_VEVERKA_mic=0) 	let MIN_C_VEVERKA=--
	if (C_VEVERKA_mac=0) 	let MAX_C_VEVERKA=--
	if (C_VEVERKA_tc=0) 	let T_C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (D_VEVERKA_mic=0) 	let MIN_D_VEVERKA=--
	if (D_VEVERKA_mac=0) 	let MAX_D_VEVERKA=--
	if (D_VEVERKA_tc=0) 	let T_D_VEVERKA=--

  ! BURATTI1 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (ALBEDO_mic=0) 				let MIN_ALBEDO=0.0
	if (ALBEDO_mac=0) 				let MAX_ALBEDO=1.0
	if (ALBEDO_tc=0) 				let T_ALBEDO=0.3
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (B_VEVERKA_mic=0) 				let MIN_B_VEVERKA=-3.0
	if (B_VEVERKA_mac=0) 				let MAX_B_VEVERKA=3.0
	if (B_VEVERKA_tc=0) 				let T_B_VEVERKA=0.05
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14
	if (E_BURATTI_mic=0) 				let MIN_E_BURATTI=0.0
	if (E_BURATTI_mac=0) 				let MAX_E_BURATTI=1.0
	if (E_BURATTI_tc=0) 				let T_E_BURATTI=0.3

          save-var pho_buratti1.par, (		+
				ALBEDO,		+
				MIN_ALBEDO,	+
				MAX_ALBEDO,	+
				T_ALBEDO,	+
				B_VEVERKA,	+
				MIN_B_VEVERKA,	+
				MAX_B_VEVERKA,	+
				T_B_VEVERKA,	+
				E_BURATTI,	+
				MIN_E_BURATTI,	+
				MAX_E_BURATTI,	+
				T_E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (ALBEDO_mic=0) 	let MIN_ALBEDO=--
	if (ALBEDO_mac=0) 	let MAX_ALBEDO=--
	if (ALBEDO_tc=0) 	let T_ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (B_VEVERKA_mic=0) 	let MIN_B_VEVERKA=--
	if (B_VEVERKA_mac=0) 	let MAX_B_VEVERKA=--
	if (B_VEVERKA_tc=0) 	let T_B_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--
	if (E_BURATTI_mic=0) 	let MIN_E_BURATTI=--
	if (E_BURATTI_mac=0) 	let MAX_E_BURATTI=--
	if (E_BURATTI_tc=0) 	let T_E_BURATTI=--

  ! BURATTI2 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (ALBEDO_mic=0) 				let MIN_ALBEDO=0.0
	if (ALBEDO_mac=0) 				let MAX_ALBEDO=1.0
	if (ALBEDO_tc=0) 				let T_ALBEDO=0.3
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (B_VEVERKA_mic=0) 				let MIN_B_VEVERKA=-3.0
	if (B_VEVERKA_mac=0) 				let MAX_B_VEVERKA=3.0
	if (B_VEVERKA_tc=0) 				let T_B_VEVERKA=0.05
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (C_VEVERKA_mic=0) 				let MIN_C_VEVERKA=-3.0
	if (C_VEVERKA_mac=0) 				let MAX_C_VEVERKA=3.0
	if (C_VEVERKA_tc=0) 				let T_C_VEVERKA=0.05
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14
	if (E_BURATTI_mic=0) 				let MIN_E_BURATTI=0.0
	if (E_BURATTI_mac=0) 				let MAX_E_BURATTI=1.0
	if (E_BURATTI_tc=0) 				let T_E_BURATTI=0.3

          save-var pho_buratti2.par, (		+
				ALBEDO,		+
				MIN_ALBEDO,	+
				MAX_ALBEDO,	+
				T_ALBEDO,	+
				B_VEVERKA,	+
				MIN_B_VEVERKA,	+
				MAX_B_VEVERKA,	+
				T_B_VEVERKA,	+
				C_VEVERKA,	+
				MIN_C_VEVERKA,	+
				MAX_C_VEVERKA,	+
				T_C_VEVERKA,	+
				E_BURATTI,	+
				MIN_E_BURATTI,	+
				MAX_E_BURATTI,	+
				T_E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (ALBEDO_mic=0) 	let MIN_ALBEDO=--
	if (ALBEDO_mac=0) 	let MAX_ALBEDO=--
	if (ALBEDO_tc=0) 	let T_ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (B_VEVERKA_mic=0) 	let MIN_B_VEVERKA=--
	if (B_VEVERKA_mac=0) 	let MAX_B_VEVERKA=--
	if (B_VEVERKA_tc=0) 	let T_B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (C_VEVERKA_mic=0) 	let MIN_C_VEVERKA=--
	if (C_VEVERKA_mac=0) 	let MAX_C_VEVERKA=--
	if (C_VEVERKA_tc=0) 	let T_C_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--
	if (E_BURATTI_mic=0) 	let MIN_E_BURATTI=--
	if (E_BURATTI_mac=0) 	let MAX_E_BURATTI=--
	if (E_BURATTI_tc=0) 	let T_E_BURATTI=--

  ! BURATTI3 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (ALBEDO_mic=0) 				let MIN_ALBEDO=0.0
	if (ALBEDO_mac=0) 				let MAX_ALBEDO=1.0
	if (ALBEDO_tc=0) 				let T_ALBEDO=0.3
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (B_VEVERKA_mic=0) 				let MIN_B_VEVERKA=-3.0
	if (B_VEVERKA_mac=0) 				let MAX_B_VEVERKA=3.0
	if (B_VEVERKA_tc=0) 				let T_B_VEVERKA=0.05
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (C_VEVERKA_mic=0) 				let MIN_C_VEVERKA=-3.0
	if (C_VEVERKA_mac=0) 				let MAX_C_VEVERKA=3.0
	if (C_VEVERKA_tc=0) 				let T_C_VEVERKA=0.05
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (D_VEVERKA_mic=0) 				let MIN_D_VEVERKA=-2.0
	if (D_VEVERKA_mac=0) 				let MAX_D_VEVERKA=2.0
	if (D_VEVERKA_tc=0) 				let T_D_VEVERKA=0.02
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14
	if (E_BURATTI_mic=0) 				let MIN_E_BURATTI=0.0
	if (E_BURATTI_mac=0) 				let MAX_E_BURATTI=1.0
	if (E_BURATTI_tc=0) 				let T_E_BURATTI=0.3

          save-var pho_buratti3.par, (		+
				ALBEDO,		+
				MIN_ALBEDO,	+
				MAX_ALBEDO,	+
				T_ALBEDO,	+
				B_VEVERKA,	+
				MIN_B_VEVERKA,	+
				MAX_B_VEVERKA,	+
				T_B_VEVERKA,	+
				C_VEVERKA,	+
				MIN_C_VEVERKA,	+
				MAX_C_VEVERKA,	+
				T_C_VEVERKA,	+
				D_VEVERKA,	+
				MIN_D_VEVERKA,	+
				MAX_D_VEVERKA,	+
				T_D_VEVERKA,	+
				E_BURATTI,	+
				MIN_E_BURATTI,	+
				MAX_E_BURATTI,	+
				T_E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (ALBEDO_mic=0) 	let MIN_ALBEDO=--
	if (ALBEDO_mac=0) 	let MAX_ALBEDO=--
	if (ALBEDO_tc=0) 	let T_ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (B_VEVERKA_mic=0) 	let MIN_B_VEVERKA=--
	if (B_VEVERKA_mac=0) 	let MAX_B_VEVERKA=--
	if (B_VEVERKA_tc=0) 	let T_B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (C_VEVERKA_mic=0) 	let MIN_C_VEVERKA=--
	if (C_VEVERKA_mac=0) 	let MAX_C_VEVERKA=--
	if (C_VEVERKA_tc=0) 	let T_C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (D_VEVERKA_mic=0) 	let MIN_D_VEVERKA=--
	if (D_VEVERKA_mac=0) 	let MAX_D_VEVERKA=--
	if (D_VEVERKA_tc=0) 	let T_D_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--
	if (E_BURATTI_mic=0) 	let MIN_E_BURATTI=--
	if (E_BURATTI_mac=0) 	let MAX_E_BURATTI=--
	if (E_BURATTI_tc=0) 	let T_E_BURATTI=--

  ! MOSHER :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.997
	if (A_VEVERKA_mic=0) 				let MIN_A_VEVERKA=0.0
	if (A_VEVERKA_mac=0) 				let MAX_A_VEVERKA=1.5
	if (A_VEVERKA_tc=0) 				let T_A_VEVERKA=0.2
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (B_VEVERKA_mic=0) 				let MIN_B_VEVERKA=-3.0
	if (B_VEVERKA_mac=0) 				let MAX_B_VEVERKA=3.0
	if (B_VEVERKA_tc=0) 				let T_B_VEVERKA=0.05
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (C_VEVERKA_mic=0) 				let MIN_C_VEVERKA=-3.0
	if (C_VEVERKA_mac=0) 				let MAX_C_VEVERKA=3.0
	if (C_VEVERKA_tc=0) 				let T_C_VEVERKA=0.05
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (D_VEVERKA_mic=0) 				let MIN_D_VEVERKA=-2.0
	if (D_VEVERKA_mac=0) 				let MAX_D_VEVERKA=2.0
	if (D_VEVERKA_tc=0) 				let T_D_VEVERKA=0.02
	if (MO_EXP1_count=0) 				let MO_EXP1=0.5
	if (MO_EXP1_mic=0) 				let MIN_MO_EXP1=0.0
	if (MO_EXP1_mac=0) 				let MAX_MO_EXP1=1.0
	if (MO_EXP1_tc=0) 				let T_MO_EXP1=0.5
	if (MO_EXP2_count=0) 				let MO_EXP2=0.1
	if (MO_EXP2_mic=0) 				let MIN_MO_EXP2=0.0
	if (MO_EXP2_mac=0) 				let MAX_MO_EXP2=1.0
	if (MO_EXP2_tc=0) 				let T_MO_EXP2=0.2

          save-var pho_mosher.par, (		+
				A_VEVERKA,	+
				MIN_A_VEVERKA,	+
				MAX_A_VEVERKA,	+
				T_A_VEVERKA,	+
				B_VEVERKA,	+
				MIN_C_VEVERKA,	+
				MAX_C_VEVERKA,	+
				T_C_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA,	+
				MIN_D_VEVERKA,	+
				MAX_D_VEVERKA,	+
				T_D_VEVERKA,	+
				MO_EXP1,	+
				MIN_MO_EXP1,	+
				MAX_MO_EXP1,	+
				T_MO_EXP1,	+
				MO_EXP2,	+
				MIN_MO_EXP2,	+
				MAX_MO_EXP2,	+
				T_MO_EXP2	+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (A_VEVERKA_mic=0) 	let MIN_A_VEVERKA=--
	if (A_VEVERKA_mac=0) 	let MAX_A_VEVERKA=--
	if (A_VEVERKA_tc=0) 	let T_A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (B_VEVERKA_mic=0) 	let MIN_B_VEVERKA=--
	if (B_VEVERKA_mac=0) 	let MAX_B_VEVERKA=--
	if (B_VEVERKA_tc=0) 	let T_B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (C_VEVERKA_mic=0) 	let MIN_C_VEVERKA=--
	if (C_VEVERKA_mac=0) 	let MAX_C_VEVERKA=--
	if (C_VEVERKA_tc=0) 	let T_C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (D_VEVERKA_mic=0) 	let MIN_D_VEVERKA=--
	if (D_VEVERKA_mac=0) 	let MAX_D_VEVERKA=--
	if (D_VEVERKA_tc=0) 	let T_D_VEVERKA=--
	if (MO_EXP1_count=0) 	let MO_EXP1=--
	if (MO_EXP1_mic=0) 	let MIN_MO_EXP1=--
	if (MO_EXP1_mac=0) 	let MAX_MO_EXP1=--
	if (MO_EXP1_tc=0) 	let T_MO_EXP1=--
	if (MO_EXP2_count=0) 	let MO_EXP2=--
	if (MO_EXP2_mic=0) 	let MIN_MO_EXP2=--
	if (MO_EXP2_mac=0) 	let MAX_MO_EXP2=--
	if (MO_EXP2_tc=0) 	let T_MO_EXP2=--

  ! LUMME_BOWEL_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.02
	if (DEN_SOIL_count=0) 				let DEN_SOIL=0.8
	if (DEN_SOIL_mic=0) 				let MIN_DEN_SOIL=0.0
	if (DEN_SOIL_mac=0) 				let MAX_DEN_SOIL=1.0
	if (DEN_SOIL_tc=0) 				let T_DEN_SOIL=0.2
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26
	if (HG1_SOIL_mic=0) 				let MIN_HG1_SOIL=-1.0
	if (HG1_SOIL_mac=0) 				let MAX_HG1_SOIL=1.0
	if (HG1_SOIL_tc=0) 				let T_HG1_SOIL=0.2

          save-var pho_lumme_bowel_hg1.par, (	+
				W_SOIL,		+
				MAX_W_SOIL,	+
				MIN_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MAX_H_SHOE,	+
				MIN_H_SHOE,	+
				T_H_SHOE,	+
				DEN_SOIL,	+
				MAX_DEN_SOIL,	+
				MIN_DEN_SOIL,	+
				T_DEN_SOIL,	+
				THETA,		+
				MAX_THETA,	+
				MIN_THETA,	+
				T_THETA,	+
				HG1_SOIL,	+
				MAX_HG1_SOIL,	+
				MIN_HG1_SOIL,	+
				T_HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (DEN_SOIL_count=0) 	let DEN_SOIL=--
	if (DEN_SOIL_mic=0) 	let MIN_DEN_SOIL=--
	if (DEN_SOIL_mac=0) 	let MAX_DEN_SOIL=--
	if (DEN_SOIL_tc=0) 	let T_DEN_SOIL=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--
	if (HG1_SOIL_mic=0) 	let MIN_HG1_SOIL=--
	if (HG1_SOIL_mac=0) 	let MAX_HG1_SOIL=--
	if (HG1_SOIL_tc=0) 	let T_HG1_SOIL=--

  ! HAPKE_81_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE1_SOIL_mic=0) 				let MIN_LE1_SOIL=-1.732
	if (LE1_SOIL_mac=0) 				let MAX_LE1_SOIL=1.732
	if (LE1_SOIL_tc=0) 				let T_LE1_SOIL=0.2
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3
	if (LE2_SOIL_mic=0) 				let MIN_LE2_SOIL=-2.0
	if (LE2_SOIL_mac=0) 				let MAX_LE2_SOIL=2.0
	if (LE2_SOIL_tc=0) 				let T_LE2_SOIL=0.2

          save-var pho_hapke_81_le2.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				LE1_SOIL,	+
				MIN_LE1_SOIL,	+
				MAX_LE1_SOIL,	+
				T_LE1_SOIL,	+
				LE2_SOIL,	+
				MIN_LE2_SOIL,	+
				MAX_LE2_SOIL,	+
				T_LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE1_SOIL_mic=0) 	let MIN_LE1_SOIL=--
	if (LE1_SOIL_mac=0) 	let MAX_LE1_SOIL=--
	if (LE1_SOIL_tc=0) 	let T_LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--
	if (LE2_SOIL_mic=0) 	let MIN_LE2_SOIL=--
	if (LE2_SOIL_mac=0) 	let MAX_LE2_SOIL=--
	if (LE2_SOIL_tc=0) 	let T_LE2_SOIL=--

  ! HAPKE_81_COOK :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.3
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE1_SOIL_mic=0) 				let MIN_LE1_SOIL=-1.732
	if (LE1_SOIL_mac=0) 				let MAX_LE1_SOIL=1.732
	if (LE1_SOIL_tc=0) 				let T_LE1_SOIL=0.2
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3
	if (LE2_SOIL_mic=0) 				let MIN_LE2_SOIL=-2.0
	if (LE2_SOIL_mac=0) 				let MAX_LE2_SOIL=2.0
	if (LE2_SOIL_tc=0) 				let T_LE2_SOIL=0.2
	if (COOK_count=0) 				let COOK=0.9
	if (COOK_mic=0) 				let MIN_COOK=0.0
	if (COOK_mac=0) 				let MAX_COOK=1.0
	if (COOK_tc=0) 					let T_COOK=0.2

          save-var pho_hapke_81_cook.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				LE1_SOIL,	+
				MIN_LE1_SOIL,	+
				MAX_LE1_SOIL,	+
				T_LE1_SOIL,	+
				LE2_SOIL,	+
				MIN_LE2_SOIL,	+
				MAX_LE2_SOIL,	+
				T_LE2_SOIL,	+
				COOK,		+
				MIN_COOK,	+
				MAX_COOK,	+
				T_COOK		+
				)


	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE1_SOIL_mic=0) 	let MIN_LE1_SOIL=--
	if (LE1_SOIL_mac=0) 	let MAX_LE1_SOIL=--
	if (LE1_SOIL_tc=0) 	let T_LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--
	if (LE2_SOIL_mic=0) 	let MIN_LE2_SOIL=--
	if (LE2_SOIL_mac=0) 	let MAX_LE2_SOIL=--
	if (LE2_SOIL_tc=0) 	let T_LE2_SOIL=--
	if (COOK_count=0) 	let COOK=--
	if (COOK_count=0) 	let MIN_COOK=--
	if (COOK_count=0) 	let MAX_COOK=--
	if (COOK_count=0) 	let T_COOK=--

  ! HAPKE_86_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (B_SHOE_mic=0) 				let MIN_B_SHOE=0.0
	if (B_SHOE_mac=0) 				let MAX_B_SHOE=10.0
	if (B_SHOE_tc=0) 				let T_B_SHOE=1.0
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26
	if (HG1_SOIL_mic=0) 				let MIN_HG1_SOIL=-1.0
	if (HG1_SOIL_mac=0) 				let MAX_HG1_SOIL=1.0
	if (HG1_SOIL_tc=0) 				let T_HG1_SOIL=0.2

          save-var pho_hapke_86_hg1.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				B_SHOE,		+
				MIN_B_SHOE,	+
				MAX_B_SHOE,	+
				T_B_SHOE,	+
				THETA,		+
				MIN_THETA,	+
				MAX_THETA,	+
				T_THETA,	+
				HG1_SOIL,	+
				MIN_HG1_SOIL,	+
				MAX_HG1_SOIL,	+
				T_HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (B_SHOE_mic=0) 	let MIN_B_SHOE=--
	if (B_SHOE_mac=0) 	let MAX_B_SHOE=--
	if (B_SHOE_tc=0) 	let T_B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--
	if (HG1_SOIL_mic=0) 	let MIN_HG1_SOIL=--
	if (HG1_SOIL_mac=0) 	let MAX_HG1_SOIL=--
	if (HG1_SOIL_tc=0) 	let T_HG1_SOIL=--

  ! HAPKE_86_HG2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (B_SHOE_mic=0) 				let MIN_B_SHOE=0.0
	if (B_SHOE_mac=0) 				let MAX_B_SHOE=10.0
	if (B_SHOE_tc=0) 				let T_B_SHOE=1.0
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.29
	if (HG1_SOIL_mic=0) 				let MIN_HG1_SOIL=-1.0
	if (HG1_SOIL_mac=0) 				let MAX_HG1_SOIL=1.0
	if (HG1_SOIL_tc=0) 				let T_HG1_SOIL=0.2
	if (HG2_SOIL_count=0) 				let HG2_SOIL=0.39
	if (HG2_SOIL_mic=0) 				let MIN_HG2_SOIL=-1.0
	if (HG2_SOIL_mac=0) 				let MAX_HG2_SOIL=1.0
	if (HG2_SOIL_tc=0) 				let T_HG2_SOIL=0.2
	if (HG_ASY_SOIL_cou=0) 				let HG_ASY_SOIL=1
	if (HG_ASY_SOIL_mic=0) 			       let MIN_HG_ASY_SOIL=-10.0
	if (HG_ASY_SOIL_mac=0) 				let MAX_HG_ASY_SOIL=10.0
	if (HG_ASY_SOIL_tc=0) 				let T_HG_ASY_SOIL=1.0

          save-var pho_hapke_86_hg2.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				B_SHOE,		+
				MIN_B_SHOE,	+
				MAX_B_SHOE,	+
				T_B_SHOE,	+
				THETA,		+
				MIN_THETA,	+
				MAX_THETA,	+
				T_THETA,	+
				HG1_SOIL,	+
				MIN_HG1_SOIL,	+
				MAX_HG1_SOIL,	+
				T_HG1_SOIL,	+
				HG2_SOIL,	+
				MIN_HG2_SOIL,	+
				MAX_HG2_SOIL,	+
				T_HG2_SOIL,	+
				HG_ASY_SOIL,	+
				MIN_HG_ASY_SOIL,+
				MAX_HG_ASY_SOIL,+
				T_HG_ASY_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (B_SHOE_mic=0) 	let MIN_B_SHOE=--
	if (B_SHOE_mac=0) 	let MAX_B_SHOE=--
	if (B_SHOE_tc=0) 	let T_B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--
	if (HG1_SOIL_mic=0) 	let MIN_HG1_SOIL=--
	if (HG1_SOIL_mac=0) 	let MAX_HG1_SOIL=--
	if (HG1_SOIL_tc=0) 	let T_HG1_SOIL=--
	if (HG2_SOIL_count=0) 	let HG2_SOIL=--
	if (HG2_SOIL_mic=0) 	let MIN_HG2_SOIL=--
	if (HG2_SOIL_mac=0) 	let MAX_HG2_SOIL=--
	if (HG2_SOIL_tc=0) 	let T_HG2_SOIL=--
	if (HG_ASY_SOIL_cou=0) 	let HG_ASY_SOIL=--
	if (HG_ASY_SOIL_mic=0) 	let MIN_HG_ASY_SOIL=--
	if (HG_ASY_SOIL_mac=0) 	let MAX_HG_ASY_SOIL=--
	if (HG_ASY_SOIL_tc=0) 	let T_HG_ASY_SOIL=--

  ! HAPKE_86_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (B_SHOE_mic=0) 				let MIN_B_SHOE=0.0
	if (B_SHOE_mac=0) 				let MAX_B_SHOE=10.0
	if (B_SHOE_tc=0) 				let T_B_SHOE=1.0
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.29
	if (LE1_SOIL_mic=0) 				let MIN_LE1_SOIL=-1.732
	if (LE1_SOIL_mac=0) 				let MAX_LE1_SOIL=1.732
	if (LE1_SOIL_tc=0) 				let T_LE1_SOIL=0.2
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.39
	if (LE2_SOIL_mic=0) 				let MIN_LE2_SOIL=-2.0
	if (LE2_SOIL_mac=0) 				let MAX_LE2_SOIL=2.0
	if (LE2_SOIL_tc=0) 				let T_LE2_SOIL=0.2

          save-var pho_hapke_86_le2.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				B_SHOE,		+
				MIN_B_SHOE,	+
				MAX_B_SHOE,	+
				T_B_SHOE,	+
				THETA,		+
				MIN_THETA,	+
				MAX_THETA,	+
				T_THETA,	+
				LE1_SOIL,	+
				MIN_LE1_SOIL,	+
				MAX_LE1_SOIL,	+
				T_LE1_SOIL,	+
				LE2_SOIL,	+
				MIN_LE2_SOIL,	+
				MAX_LE2_SOIL,	+
				T_LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (B_SHOE_mic=0) 	let MIN_B_SHOE=--
	if (B_SHOE_mac=0) 	let MAX_B_SHOE=--
	if (B_SHOE_tc=0) 	let T_B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE1_SOIL_mic=0) 	let MIN_LE1_SOIL=--
	if (LE1_SOIL_mac=0) 	let MAX_LE1_SOIL=--
	if (LE1_SOIL_tc=0) 	let T_LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--
	if (LE2_SOIL_mic=0) 	let MIN_LE2_SOIL=--
	if (LE2_SOIL_mac=0) 	let MAX_LE2_SOIL=--
	if (LE2_SOIL_tc=0) 	let T_LE2_SOIL=--

  ! HAPKE_HG1_DOM :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (B_SHOE_mic=0) 				let MIN_B_SHOE=0.0
	if (B_SHOE_mac=0) 				let MAX_B_SHOE=10.0
	if (B_SHOE_tc=0) 				let T_B_SHOE=1.0
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (HG1_SOIL_mic=0)				let MIN_HG1_SOIL=-1.0
	if (HG1_SOIL_mac=0)				let MAX_HG1_SOIL=1.0
	if (HG1_SOIL_tc=0)				let T_HG1_SOIL=0.2
	if (H_CBOE_count=0) 				let H_CBOE=0.06
	if (H_CBOE_mic=0) 				let MIN_H_CBOE=0.0
	if (H_CBOE_mac=0) 				let MAX_H_CBOE=2.0
	if (H_CBOE_tc=0) 				let T_H_CBOE=0.2
	if (B_CBOE_count=0) 				let B_CBOE=1.0
	if (B_CBOE_mic=0) 				let MIN_B_CBOE=1.0
	if (B_CBOE_mac=0) 				let MAX_B_CBOE=10.0
	if (B_CBOE_tc=0) 				let T_B_CBOE=1.0

          save-var pho_hapke_hg1_dom.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				B_SHOE,		+
				MIN_B_SHOE,	+
				MAX_B_SHOE,	+
				T_B_SHOE,	+
				THETA,		+
				MIN_THETA,	+
				MAX_THETA,	+
				T_THETA,	+
				HG1_SOIL,	+
				MIN_HG1_SOIL,	+
				MAX_HG1_SOIL,	+
				T_HG1_SOIL,	+
				H_CBOE,		+
				MIN_H_CBOE,	+
				MAX_H_CBOE,	+
				T_H_CBOE,	+
				B_CBOE,		+
				MIN_B_CBOE,	+
				MAX_B_CBOE,	+
				T_B_CBOE	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (B_SHOE_mic=0) 	let MIN_B_SHOE=--
	if (B_SHOE_mac=0) 	let MAX_B_SHOE=--
	if (B_SHOE_tc=0) 	let T_B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (HG1_SOIL_mic=0)	let MIN_HG1_SOIL=--
	if (HG1_SOIL_mac=0)	let MAX_HG1_SOIL=--
	if (HG1_SOIL_tc=0)	let T_HG1_SOIL=--
	if (H_CBOE_count=0) 	let H_CBOE=--
	if (H_CBOE_mic=0) 	let MIN_H_CBOE=--
	if (H_CBOE_mac=0) 	let MAX_H_CBOE=--
	if (H_CBOE_tc=0) 	let T_H_CBOE=--
	if (B_CBOE_count=0) 	let B_CBOE=--
	if (B_CBOE_mic=0) 	let MIN_B_CBOE=--
	if (B_CBOE_mac=0) 	let MAX_B_CBOE=--
	if (B_CBOE_tc=0) 	let T_B_CBOE=--

  ! REGNER_HAPKE_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (W_SOIL_mic=0) 				let MIN_W_SOIL=0.0
	if (W_SOIL_mac=0) 				let MAX_W_SOIL=1.0
	if (W_SOIL_tc=0) 				let T_W_SOIL=0.2
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (H_SHOE_mic=0) 				let MIN_H_SHOE=0.0
	if (H_SHOE_mac=0) 				let MAX_H_SHOE=2.0
	if (H_SHOE_tc=0) 				let T_H_SHOE=0.2
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (B_SHOE_mic=0) 				let MIN_B_SHOE=0.0
	if (B_SHOE_mac=0) 				let MAX_B_SHOE=10.0
	if (B_SHOE_tc=0) 				let T_B_SHOE=1.0
	if (THETA_count=0) 				let THETA=20.0
	if (THETA_mic=0) 				let MIN_THETA=0.0
	if (THETA_mac=0) 				let MAX_THETA=60.0
	if (THETA_tc=0) 				let T_THETA=6.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (HG1_SOIL_mic=0)				let MIN_HG1_SOIL=-1.0
	if (HG1_SOIL_mac=0)				let MAX_HG1_SOIL=1.0
	if (HG1_SOIL_tc=0)				let T_HG1_SOIL=0.2
	if (W_ATM_count=0)				let W_ATM=0.78
	if (W_ATM_mic=0)				let MIN_W_ATM=0.0
	if (W_ATM_mac=0)				let MAX_W_ATM=1.0
	if (W_ATM_tc=0)					let T_W_ATM=0.2
	if (TAU_ATM_count=0)				let TAU_ATM=0.05
	if (TAU_ATM_mic=0)				let MIN_TAU_ATM=0.0
	if (TAU_ATM_mac=0)				let MAX_TAU_ATM=10.0
	if (TAU_ATM_tc=0)				let T_TAU_ATM=0.1
	if (HG1_ATM_count=0)				let HG1_ATM=0.35
	if (HG1_ATM_mic=0)				let MIN_HG1_ATM=0.0
	if (HG1_ATM_mac=0)				let MAX_HG1_ATM=1.0
	if (HG1_ATM_tc=0)				let T_HG1_ATM=0.2

          save-var pho_regner_hapke_hg1.par, (	+
				W_SOIL,		+
				MIN_W_SOIL,	+
				MAX_W_SOIL,	+
				T_W_SOIL,	+
				H_SHOE,		+
				MIN_H_SHOE,	+
				MAX_H_SHOE,	+
				T_H_SHOE,	+
				B_SHOE,		+
				MIN_B_SHOE,	+
				MAX_B_SHOE,	+
				T_B_SHOE,	+
				THETA,		+
				MIN_THETA,	+
				MAX_THETA,	+
				T_THETA,	+
				HG1_SOIL,	+
				MIN_HG1_SOIL,	+
				MAX_HG1_SOIL,	+
				T_HG1_SOIL,	+
				W_ATM,		+
				MIN_W_ATM,	+
				MAX_W_ATM,	+
				T_W_ATM,	+
				TAU_ATM,	+
				MIN_TAU_ATM,	+
				MAX_TAU_ATM,	+
				T_TAU_ATM,	+
				HG1_ATM,	+
				MIN_HG1_ATM,	+
				MAX_HG1_ATM,	+
				T_HG1_ATM	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (W_SOIL_mic=0) 	let MIN_W_SOIL=--
	if (W_SOIL_mac=0) 	let MAX_W_SOIL=--
	if (W_SOIL_tc=0) 	let T_W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (H_SHOE_mic=0) 	let MIN_H_SHOE=--
	if (H_SHOE_mac=0) 	let MAX_H_SHOE=--
	if (H_SHOE_tc=0) 	let T_H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (B_SHOE_mic=0) 	let MIN_B_SHOE=--
	if (B_SHOE_mac=0) 	let MAX_B_SHOE=--
	if (B_SHOE_tc=0) 	let T_B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (THETA_mic=0) 	let MIN_THETA=--
	if (THETA_mac=0) 	let MAX_THETA=--
	if (THETA_tc=0) 	let T_THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (HG1_SOIL_mic=0)	let MIN_HG1_SOIL=--
	if (HG1_SOIL_mac=0)	let MAX_HG1_SOIL=--
	if (HG1_SOIL_tc=0)	let T_HG1_SOIL=--
	if (W_ATM_count=0)	let W_ATM=--
	if (W_ATM_mic=0)	let MIN_W_ATM=--
	if (W_ATM_mac=0)	let MAX_W_ATM=--
	if (W_ATM_tc=0)		let T_W_ATM=--
	if (TAU_ATM_count=0)	let TAU_ATM=--
	if (TAU_ATM_mic=0)	let MIN_TAU_ATM=--
	if (TAU_ATM_mac=0)	let MAX_TAU_ATM=--
	if (TAU_ATM_tc=0)	let T_TAU_ATM=--
	if (HG1_ATM_count=0)	let HG1_ATM=--
	if (HG1_ATM_mic=0)	let MIN_HG1_ATM=--
	if (HG1_ATM_mac=0)	let MAX_HG1_ATM=--
	if (HG1_ATM_tc=0)	let T_HG1_ATM=--



	!*******************************************************************
	! other dummy files in case the procs aren't called :
	!*******************************************************************


          save-var &"_PROC"_general.par, (	+
				INP,		+
				PHO_FUNC,	+
				CLASS_ID,	+
  				NORM,		+
				RERUN,		+
  				MAXITER,	+
  				NUMTEN,		+
  				PERCENT,	+
  				TOLERANC,	+
  				PRINT,		+
  				METROP		+
				)

	  save-var common_save_par.par, SAVE_PAR	

!	  save-var pho_par_file.par, PHO_PAR_FILE

	  save-var common_proc_name.par, MAIN_PROC_NAME





	  !****************************************************************
	  ! The string global variable $MENUOPT allows suppression of the
	  ! "Press RETURN key for menu" message. Hence, when typing
	  ! "VICAR> tutor PHOTFIT2" the menu can be entered directly 
	  ! without the need to press the RETURN key first.
	  !****************************************************************

	    let $menuopt="NO_PRESS_FOR_MENU"

	  !****************************************************************
	  ! "menu" puts VICAR into the Menu Mode, i.e. here we enter the
	  !  menu of 'PHOTFIT2.MDF'
	  !****************************************************************

	    menu &"_PROC".mdf

   end-if

    write "returned into main-program &_PROC"

! passing the parameters from tutor mode and then running the process :

	!***************************************************************
	! $COUNT returns the current number of elements for "readparam":
	! If the keyword parameter "readparam" has been set to 'read',
	! $count(readparam)=1, else $count(readparam)<>1 (see below).
	!***************************************************************

	if ($count(readparam)=1) 


		!****************************************************
		! The restore-parms (sub-)command is used to restore
		! only the parameters in the save file.
		!
		! Command:
		! RESTORE-PARMS FILE = save_file_name
		!****************************************************

		restore-parm common_save_par.par

		restore-parm &"_PROC"_general.par

!		if ("&PHO_FUNC" = "PAR_FILE" )			+ 
!					restore-parm pho_par_file.par
		if ("&PHO_FUNC" = "LAMBERT" )			+ 
					restore-parm pho_lambert.par
		if ("&PHO_FUNC" = "MINNAERT")			+  
					restore-parm pho_minnaert.par
		if ("&PHO_FUNC" = "IRVINE")			+  
					restore-parm pho_irvine.par
		if ("&PHO_FUNC" = "VEVERKA")			+ 
					restore-parm pho_veverka.par
		if ("&PHO_FUNC" = "BURATTI1")			+ 
					restore-parm pho_buratti1.par
		if ("&PHO_FUNC" = "BURATTI2")			+ 
					restore-parm pho_buratti2.par
		if ("&PHO_FUNC" = "BURATTI3")			+  
					restore-parm pho_buratti3.par
		if ("&PHO_FUNC" = "MOSHER") 			+
					restore-parm pho_mosher.par
		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1") 		+
					restore-parm pho_lumme_bowel_hg1.par
		if ("&PHO_FUNC" = "HAPKE_81_LE2")		+
  					restore-parm pho_hapke_81_le2.par
		if ("&PHO_FUNC" = "HAPKE_81_COOK")  		+
 					restore-parm pho_hapke_81_cook.par
		if ("&PHO_FUNC" = "HAPKE_86_HG1")		+
 					restore-parm pho_hapke_86_hg1.par
		if ("&PHO_FUNC" = "HAPKE_86_HG2")		+
 					restore-parm pho_hapke_86_hg2.par
		if ("&PHO_FUNC" = "HAPKE_86_LE2") 		+
 					restore-parm pho_hapke_86_le2.par
		if ("&PHO_FUNC" = "HAPKE_HG1_DOM") 		+
					restore-parm pho_hapke_hg1_dom.par
		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")  		+
					restore-parm pho_regner_hapke_hg1.par
                                                                             



		save-var "&SAVE_PAR", (		+
			INP,			+
			PHO_FUNC,		+
			CLASS_ID,		+
  			NORM,			+
			RERUN,			+
  			MAXITER,		+
  			NUMTEN,			+
  			PERCENT,		+
  			TOLERANC,		+
  			PRINT,			+
  			METROP,			+
			ALBEDO,			+ 
			MIN_ALBEDO,		+ 
			MAX_ALBEDO,		+ 
			T_ALBEDO,		+ 
			EXPONENT,		+
			MIN_EXPONENT,		+
			MAX_EXPONENT,		+
			T_EXPONENT,		+
      			A_VEVERKA, 		+
      			MIN_A_VEVERKA, 		+
      			MAX_A_VEVERKA, 		+
      			T_A_VEVERKA, 		+
       			B_VEVERKA, 		+
       			MIN_B_VEVERKA, 		+
       			MAX_B_VEVERKA, 		+
       			T_B_VEVERKA, 		+
       			C_VEVERKA, 		+
       			MIN_C_VEVERKA, 		+
       			MAX_C_VEVERKA, 		+
       			T_C_VEVERKA, 		+
       			D_VEVERKA,		+ 
       			MIN_D_VEVERKA,		+ 
       			MAX_D_VEVERKA,		+ 
       			T_D_VEVERKA,		+ 
       			MO_EXP1, 		+
       			MIN_MO_EXP1, 		+
       			MAX_MO_EXP1, 		+
       			T_MO_EXP1, 		+
       			MO_EXP2, 		+
       			MIN_MO_EXP2, 		+
       			MAX_MO_EXP2, 		+
       			T_MO_EXP2, 		+
       			E_BURATTI, 		+
       			MIN_E_BURATTI, 		+
       			MAX_E_BURATTI, 		+
       			T_E_BURATTI, 		+
       			DEN_SOIL, 		+
       			MIN_DEN_SOIL, 		+
       			MAX_DEN_SOIL, 		+
       			T_DEN_SOIL, 		+
       			W_SOIL, 		+
       			MIN_W_SOIL, 		+
       			MAX_W_SOIL, 		+
       			T_W_SOIL, 		+
       			HG1_SOIL, 		+
       			MIN_HG1_SOIL, 		+
       			MAX_HG1_SOIL, 		+
       			T_HG1_SOIL, 		+
       			HG2_SOIL, 		+
       			MIN_HG2_SOIL, 		+
       			MAX_HG2_SOIL, 		+
       			T_HG2_SOIL, 		+
       			HG_ASY_SOIL,		+
       			MIN_HG_ASY_SOIL,	+
       			MAX_HG_ASY_SOIL,	+
       			T_HG_ASY_SOIL,		+
       			LE1_SOIL, 		+
       			MIN_LE1_SOIL, 		+
       			MAX_LE1_SOIL, 		+
       			T_LE1_SOIL, 		+
       			LE2_SOIL, 		+
       			MIN_LE2_SOIL, 		+
       			MAX_LE2_SOIL, 		+
       			T_LE2_SOIL, 		+
       			H_SHOE, 		+
       			MIN_H_SHOE, 		+
       			MAX_H_SHOE, 		+
       			T_H_SHOE, 		+
       			B_SHOE, 		+
       			MIN_B_SHOE, 		+
       			MAX_B_SHOE, 		+
       			T_B_SHOE, 		+
       			H_CBOE, 		+
       			MIN_H_CBOE, 		+
       			MAX_H_CBOE, 		+
       			T_H_CBOE, 		+
       			B_CBOE, 		+
       			MIN_B_CBOE, 		+
       			MAX_B_CBOE, 		+
       			T_B_CBOE, 		+
       			THETA ,			+
       			MIN_THETA ,		+
       			MAX_THETA ,		+
       			T_THETA ,		+
       			COOK,			+
       			MIN_COOK,		+
       			MAX_COOK,		+
       			T_COOK,			+
       			TAU_ATM,		+
       			MIN_TAU_ATM,		+
       			MAX_TAU_ATM,		+
       			T_TAU_ATM,		+
       			W_ATM, 			+
       			MIN_W_ATM, 		+
       			MAX_W_ATM, 		+
       			T_W_ATM, 		+
       			HG1_ATM,		+
       			MIN_HG1_ATM,		+
       			MAX_HG1_ATM,		+
       			T_HG1_ATM,		+
       			IRV_EXP1, 		+
       			MIN_IRV_EXP1, 		+
       			MAX_IRV_EXP1, 		+
       			T_IRV_EXP1, 		+
       			IRV_EXP2, 		+  
       			MIN_IRV_EXP2, 		+  
       			MAX_IRV_EXP2, 		+  
       			T_IRV_EXP2, 		+  
			SAVE_PAR		+
 				)



		!*********************************************
		! The parameter and their values can bee 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*************************************************
		! ... and the main program PHOTFIT2 is run
		!*************************************************

		PHOTFIT2

	end-if



! running the process in the batch modus :

	if ($count(readparam) <> 1) 

		!***********************************************************
		! At this position you can input function-specific default 
		! values for the function parameter for the batch modus : 
		!***********************************************************



		if ( "&PHO_FUNC" = "LAMBERT" )
			if (ALBEDO_count=0)		let ALBEDO=0.5
			if (ALBEDO_mic=0)		let MIN_ALBEDO=0.0
			if (ALBEDO_mac=0)		let MAX_ALBEDO=1.0
			if (ALBEDO_tc=0)		let T_ALBEDO=0.3
		end-if

		if ("&PHO_FUNC" = "MINNAERT")
			if (ALBEDO_count=0)		let ALBEDO=0.5
			if (ALBEDO_mic=0)		let MIN_ALBEDO=0.0
			if (ALBEDO_mac=0)		let MAX_ALBEDO=1.0
			if (ALBEDO_tc=0)		let T_ALBEDO=0.3
			if (EXPONENT_count=0)		let EXPONENT=0.6
			if (EXPONENT_mic=0)		let MIN_EXPONENT=0.0
			if (EXPONENT_mac=0)		let MAX_EXPONENT=1.0
			if (EXPONENT_tc=0)		let T_EXPONENT=0.3
		end-if

		if ("&PHO_FUNC" = "IRVINE")
			if (EXPONENT_count=0)		let EXPONENT=0.9
			if (EXPONENT_mic=0)		let MIN_EXPONENT=0.0
			if (EXPONENT_mac=0)		let MAX_EXPONENT=1.0
			if (EXPONENT_tc=0)		let T_EXPONENT=0.3
			if (IRV_EXP1_count=0)		let IRV_EXP1=0.118
			if (IRV_EXP1_mic=0)		let MIN_IRV_EXP1=0.0
			if (IRV_EXP1_mac=0)		let MAX_IRV_EXP1=1.0
			if (IRV_EXP1_tc=0)		let T_IRV_EXP1=0.1
			if (IRV_EXP2_count=0)		let IRV_EXP2=0.0039
			if (IRV_EXP2_mic=0)		let MIN_IRV_EXP2=0.0
			if (IRV_EXP2_mac=0)		let MAX_IRV_EXP2=0.01
			if (IRV_EXP2_tc=0)		let T_IRV_EXP2=0.004
		end-if

		if ("&PHO_FUNC" = "VEVERKA")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.997
			if (A_VEVERKA_mic=0) 		let MIN_A_VEVERKA=0.0
			if (A_VEVERKA_mac=0) 		let MAX_A_VEVERKA=1.5
			if (A_VEVERKA_tc=0) 		let T_A_VEVERKA=0.2
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (B_VEVERKA_mic=0) 		let MIN_B_VEVERKA=-3.0
			if (B_VEVERKA_mac=0) 		let MAX_B_VEVERKA=3.0
			if (B_VEVERKA_tc=0) 		let T_B_VEVERKA=0.05
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (C_VEVERKA_mic=0) 		let MIN_C_VEVERKA=-3.0
			if (C_VEVERKA_mac=0) 		let MAX_C_VEVERKA=3.0
			if (C_VEVERKA_tc=0) 		let T_C_VEVERKA=0.05
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (D_VEVERKA_mic=0) 		let MIN_D_VEVERKA=-2.0
			if (D_VEVERKA_mac=0) 		let MAX_D_VEVERKA=2.0
			if (D_VEVERKA_tc=0) 		let T_D_VEVERKA=0.02
		end-if

		if ("&PHO_FUNC" = "BURATTI1")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (ALBEDO_mic=0) 		let MIN_ALBEDO=0.0
			if (ALBEDO_mac=0) 		let MAX_ALBEDO=1.0
			if (ALBEDO_tc=0) 		let T_ALBEDO=0.3
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (B_VEVERKA_mic=0) 		let MIN_B_VEVERKA=-3.0
			if (B_VEVERKA_mac=0) 		let MAX_B_VEVERKA=3.0
			if (B_VEVERKA_tc=0) 		let T_B_VEVERKA=0.05
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
			if (E_BURATTI_mic=0) 		let MIN_E_BURATTI=0.0
			if (E_BURATTI_mac=0) 		let MAX_E_BURATTI=1.0
			if (E_BURATTI_tc=0) 		let T_E_BURATTI=0.3
		end-if

		if ("&PHO_FUNC" = "BURATTI2")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (ALBEDO_mic=0) 		let MIN_ALBEDO=0.0
			if (ALBEDO_mac=0) 		let MAX_ALBEDO=1.0
			if (ALBEDO_tc=0) 		let T_ALBEDO=0.3
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (B_VEVERKA_mic=0) 		let MIN_B_VEVERKA=-3.0
			if (B_VEVERKA_mac=0) 		let MAX_B_VEVERKA=3.0
			if (B_VEVERKA_tc=0) 		let T_B_VEVERKA=0.05
			if (C_VEVERKA_count=0) 		let C_VEVERKA=-0.003
			if (C_VEVERKA_mic=0) 		let MIN_C_VEVERKA=-3.0
			if (C_VEVERKA_mac=0) 		let MAX_C_VEVERKA=3.0
			if (C_VEVERKA_tc=0) 		let T_C_VEVERKA=0.05
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
			if (E_BURATTI_mic=0) 		let MIN_E_BURATTI=0.0
			if (E_BURATTI_mac=0) 		let MAX_E_BURATTI=1.0
			if (E_BURATTI_tc=0) 		let T_E_BURATTI=0.3
		end-if

		if ("&PHO_FUNC" = "BURATTI3")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (ALBEDO_mic=0) 		let MIN_ALBEDO=0.0
			if (ALBEDO_mac=0) 		let MAX_ALBEDO=1.0
			if (ALBEDO_tc=0) 		let T_ALBEDO=0.3
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (B_VEVERKA_mic=0) 		let MIN_B_VEVERKA=-3.0
			if (B_VEVERKA_mac=0) 		let MAX_B_VEVERKA=3.0
			if (B_VEVERKA_tc=0) 		let T_B_VEVERKA=0.05
			if (C_VEVERKA_count=0) 		let C_VEVERKA=-0.003
			if (C_VEVERKA_mic=0) 		let MIN_C_VEVERKA=-3.0
			if (C_VEVERKA_mac=0) 		let MAX_C_VEVERKA=3.0
			if (C_VEVERKA_tc=0) 		let T_C_VEVERKA=0.05
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (D_VEVERKA_mic=0) 		let MIN_D_VEVERKA=-2.0
			if (D_VEVERKA_mac=0) 		let MAX_D_VEVERKA=2.0
			if (D_VEVERKA_tc=0) 		let T_D_VEVERKA=0.02
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
			if (E_BURATTI_mic=0) 		let MIN_E_BURATTI=0.0
			if (E_BURATTI_mac=0) 		let MAX_E_BURATTI=1.0
			if (E_BURATTI_tc=0) 		let T_E_BURATTI=0.3
		end-if

		if ("&PHO_FUNC" = "MOSHER")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.997
			if (A_VEVERKA__mic=0) 		let MIN_A_VEVERKA=0.0
			if (A_VEVERKA_mac=0) 		let MAX_A_VEVERKA=1.5
			if (A_VEVERKA_tc=0) 		let T_A_VEVERKA=0.2
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (B_VEVERKA_mic=0) 		let MIN_B_VEVERKA=-3.0
			if (B_VEVERKA_mac=0) 		let MAX_B_VEVERKA=3.0
			if (B_VEVERKA_tc=0) 		let T_B_VEVERKA=0.05
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (C_VEVERKA_mic=0) 		let MIN_C_VEVERKA=-3.0
			if (C_VEVERKA_mact=0) 		let MAX_C_VEVERKA=3.0
			if (C_VEVERKA_tc=0) 		let T_C_VEVERKA=0.05
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (D_VEVERKA_mic=0) 		let MIN_D_VEVERKA=-2.0
			if (D_VEVERKA_mac=0) 		let MAX_D_VEVERKA=2.0
			if (D_VEVERKA_tc=0) 		let T_D_VEVERKA=0.02
			if (MO_EXP1_count=0) 		let MO_EXP1=0.5
			if (MO_EXP1_mic=0) 		let MIN_MO_EXP1=0.0
			if (MO_EXP1_mac=0) 		let MAX_MO_EXP1=1.0
			if (MO_EXP1_tc=0) 		let T_MO_EXP1=0.5
			if (MO_EXP2_count=0) 		let MO_EXP2=0.1
			if (MO_EXP2_mic=0) 		let MIN_MO_EXP2=0.0
			if (MO_EXP2_mac=0) 		let MAX_MO_EXP2=1.0
			if (MO_EXP2_tc=0) 		let T_MO_EXP2=0.2
		end-if

		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.02
			if (DEN_SOIL_count=0) 		let DEN_SOIL=0.8
			if (DEN_SOIL_mic=0) 		let MIN_DEN_SOIL=0.0
			if (DEN_SOIL_mac=0) 		let MAX_DEN_SOIL=1.0
			if (DEN_SOIL_tc=0) 		let T_DEN_SOIL=0.2
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
			if (HG1_SOIL_mic=0) 		let MIN_HG1_SOIL=-1.0
			if (HG1_SOIL_mac=0) 		let MAX_HG1_SOIL=1.0
			if (HG1_SOIL_tc=0) 		let T_HG1_SOIL=0.2
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE1_SOIL_mic=0) 		let MIN_LE1_SOIL=-1.732
			if (LE1_SOIL_mac=0) 		let MAX_LE1_SOIL=1.732
			if (LE1_SOIL_tc=0) 		let T_LE1_SOIL=0.2
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
			if (LE2_SOIL_mic=0) 		let MIN_LE2_SOIL=-2.0
			if (LE2_SOIL_mac=0) 		let MAX_LE2_SOIL=2.0
			if (LE2_SOIL_tc=0) 		let T_LE2_SOIL=0.2
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_COOK")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.3
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE1_SOIL_mic=0) 		let MIN_LE1_SOIL=-1.732
			if (LE1_SOIL_mac=0) 		let MAX_LE1_SOIL=1.732
			if (LE1_SOIL_tc=0) 		let T_LE1_SOIL=0.2
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
			if (LE2_SOIL_mic=0) 		let MIN_LE2_SOIL=-2.0
			if (LE2_SOIL_mac=0) 		let MAX_LE2_SOIL=2.0
			if (LE2_SOIL_tc=0) 		let T_LE2_SOIL=0.2
			if (COOK_count=0) 		let COOK=0.9
			if (COOK_mic=0) 		let MIN_COOK=0.0
			if (COOK_mac=0) 		let MAX_COOK=1.0
			if (COOK_tc=0) 			let T_COOK=0.5
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (B_SHOE_mic=0) 		let MIN_B_SHOE=0.0
			if (B_SHOE_mac=0) 		let MAX_B_SHOE=10.0
			if (B_SHOE_tc=0) 		let T_B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
			if (HG1_SOIL_mic=0) 		let MIN_HG1_SOIL=-1.0
			if (HG1_SOIL_mact=0) 		let MAX_HG1_SOIL=1.0
			if (HG1_SOIL_tc=0) 		let T_HG1_SOIL=0.2
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (B_SHOE_mic=0) 		let MIN_B_SHOE=0.0
			if (B_SHOE_mac=0) 		let MAX_B_SHOE=10.0
			if (B_SHOE_tc=0) 		let T_B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.29
			if (HG1_SOIL_mic=0) 		let MIN_HG1_SOIL=-1.0
			if (HG1_SOIL_mac=0) 		let MAX_HG1_SOIL=1.0
			if (HG1_SOIL_tc=0) 		let T_HG1_SOIL=0.2
			if (HG2_SOIL_count=0) 		let HG2_SOIL=0.39
			if (HG2_SOIL_mic=0) 		let MIN_HG2_SOIL=-1.0
			if (HG2_SOIL_mac=0) 		let MAX_HG2_SOIL=1.0
			if (HG2_SOIL_tc=0) 		let T_HG2_SOIL=0.2
			if (HG_ASY_SOIL_cou=0) 		let HG_ASY_SOIL=1
			if (HG_ASY_SOIL_mic=0) 	       let MIN_HG_ASY_SOIL=-10.0
			if (HG_ASY_SOIL_mac=0) 		let MAX_HG_ASY_SOIL=10.0
			if (HG_ASY_SOIL_tc=0) 		let T_HG_ASY_SOIL=1.0
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (B_SHOE_mic=0) 		let MIN_B_SHOE=0.0
			if (B_SHOE_mac=0) 		let MAX_B_SHOE=10.0
			if (B_SHOE_tc=0) 		let T_B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.29
			if (LE1_SOIL_mic=0) 		let MIN_LE1_SOIL=-1.732
			if (LE1_SOIL_mac=0) 		let MAX_LE1_SOIL=1.732
			if (LE1_SOIL_tc=0) 		let T_LE1_SOIL=0.2
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.39
			if (LE2_SOIL_mic=0) 		let MIN_LE2_SOIL=-2.0
			if (LE2_SOIL_mac=0) 		let MAX_LE2_SOIL=2.0
			if (LE2_SOIL_tc=0) 		let T_LE2_SOIL=0.2
		end-if

		if ("&PHO_FUNC" = "HAPKE_HG1_DOM")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (B_SHOE_mic=0) 		let MIN_B_SHOE=0.0
			if (B_SHOE_mac=0) 		let MAX_B_SHOE=10.0
			if (B_SHOE_tc=0) 		let T_B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (HG1_SOIL_mic=0)		let MIN_HG1_SOIL=-1.0
			if (HG1_SOIL_mac=0)		let MAX_HG1_SOIL=1.0
			if (HG1_SOIL_tc=0)		let T_HG1_SOIL=0.2
			if (H_CBOE_count=0) 		let H_CBOE=0.06
			if (H_CBOE_mic=0) 		let MIN_H_CBOE=0.0
			if (H_CBOE_mac=0) 		let MAX_H_CBOE=2.0
			if (H_CBOE_tct=0) 		let T_H_CBOE=0.2
			if (B_CBOE_count=0) 		let B_CBOE=1.0
			if (B_CBOE_mic=0) 		let MIN_B_CBOE=1.0
			if (B_CBOE_mac=0) 		let MAX_B_CBOE=10.0
			if (B_CBOE_tc=0) 		let T_B_CBOE=1.0
		end-if

		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (W_SOIL_mic=0) 		let MIN_W_SOIL=0.0
			if (W_SOIL_mac=0) 		let MAX_W_SOIL=1.0
			if (W_SOIL_tc=0) 		let T_W_SOIL=0.2
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (H_SHOE_mic=0) 		let MIN_H_SHOE=0.0
			if (H_SHOE_mac=0) 		let MAX_H_SHOE=2.0
			if (H_SHOE_tc=0) 		let T_H_SHOE=0.2
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (B_SHOE_mic=0) 		let MIN_B_SHOE=0.0
			if (B_SHOE_mac=0) 		let MAX_B_SHOE=10.0
			if (B_SHOE_tc=0) 		let T_B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (THETA_mic=0) 		let MIN_THETA=0.0
			if (THETA_mac=0) 		let MAX_THETA=60.0
			if (THETA_tc=0) 		let T_THETA=6.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (HG1_SOIL_mic=0)		let MIN_HG1_SOIL=-1.0
			if (HG1_SOIL_mac=0)		let MAX_HG1_SOIL=1.0
			if (HG1_SOIL_tc=0)		let T_HG1_SOIL=0.2
			if (W_ATM_count=0)		let W_ATM=0.78
			if (W_ATM_mic=0)		let MIN_W_ATM=0.0
			if (W_ATM_mac=0)		let MAX_W_ATM=1.0
			if (W_ATM_tc=0)			let T_W_ATM=0.2
			if (TAU_ATM_count=0)		let TAU_ATM=0.05
			if (TAU_ATM_mic=0)		let MIN_TAU_ATM=0.0
			if (TAU_ATM_mac=0)		let MAX_TAU_ATM=10
			if (TAU_ATM_tc=0)		let T_TAU_ATM=0.1
			if (HG1_ATM_count=0)		let HG1_ATM=0.35
			if (HG1_ATM_mic=0)		let MIN_HG1_ATM=0.0
			if (HG1_ATM_mac=0)		let MAX_HG1_ATM=1.0
			if (HG1_ATM_tc=0)		let T_HG1_ATM=0.2
		end-if





		!*********************************************
		! The parameter and their values can bee 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*********************************************
		! ... and the main program TPHO_ROUTINES_C is run
		!*********************************************

		PHOTFIT2

	end-if



! delete the temporary .par files only for the tutor modud :

	if ($count(readparam) = 1)  

	    if ($syschar(1) = "UNIX")

	       ush /bin/rm -f photfit2m_general.par

	       ush /bin/rm -f pho_lambert.par;+
		   /bin/rm -f pho_minnaert.par;+
		   /bin/rm -f pho_irvine.par;+
		   /bin/rm -f pho_veverka.par;+
		   /bin/rm -f pho_buratti1.par;+
		   /bin/rm -f pho_buratti2.par;+
		   /bin/rm -f pho_buratti3.par;+
		   /bin/rm -f pho_mosher.par;+
		   /bin/rm -f pho_lumme_bowel_hg1.par;+
		   /bin/rm -f pho_hapke_81_le2.par;+
		   /bin/rm -f pho_hapke_81_cook.par;+
		   /bin/rm -f pho_hapke_86_hg1.par;+
		   /bin/rm -f pho_hapke_86_hg2.par;+
		   /bin/rm -f pho_hapke_86_le2.par;+
		   /bin/rm -f pho_hapke_hg1_dom.par;+
		   /bin/rm -f pho_regner_hapke_hg1.par

	       ush /bin/rm -f common_proc_name.par;+
		   /bin/rm -f common_save_par.par


	    else

              dcl if f$search ("photfit2m_general.par;*") .nes. "" 	+
			then delete photfit2m_general.par;* 

	      dcl if f$search ("pho_lambert.par;*") .nes. "" 		+
		 	then delete pho_lambert.par;*
	      dcl if f$search ("pho_minnaert.par;*") .nes. "" 		+
		 	then delete pho_minnaert.par;*
	      dcl if f$search ("pho_irvine.par;*") .nes. "" 		+
		 	then delete pho_irvine.par;*
	      dcl if f$search ("pho_veverka.par;*") .nes. "" 		+
		 	then delete pho_veverka.par;*
	      dcl if f$search ("pho_buratti1.par;*") .nes. "" 		+
		 	then delete pho_buratti1.par;*
	      dcl if f$search ("pho_buratti2.par;*") .nes. "" 		+
		 	then delete pho_buratti2.par;*
	      dcl if f$search ("pho_buratti3.par;*") .nes. "" 		+
		 	then delete pho_buratti3.par;*
	      dcl if f$search ("pho_mosher.par;*") .nes. "" 		+
		 	then delete pho_mosher.par;*
	      dcl if f$search ("pho_lumme_bowel_hg1.par;*") .nes. "" 	+
		 	then delete pho_lumme_bowel_hg1.par;*
	      dcl if f$search ("pho_hapke_81_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_le2.par;*
	      dcl if f$search ("pho_hapke_81_cook.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_cook.par;*
	      dcl if f$search ("pho_hapke_86_hg1.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg1.par;*
	      dcl if f$search ("pho_hapke_86_hg2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg2.par;*
	      dcl if f$search ("pho_hapke_86_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_le2.par;*
	      dcl if f$search ("pho_hapke_hg1_dom.par;*") .nes. "" 	+
		 	then delete pho_hapke_hg1_dom.par;*
	      dcl if f$search ("pho_regner_hapke_hg1.par;*") .nes. "" 	+
		 	then delete pho_regner_hapke_hg1.par;*

              dcl if f$search ("common_proc_name.par;*") .nes. "" 	+
		 	then delete common_proc_name.par;*
	      dcl if f$search ("common_save_par.par;*") .nes. "" 	+
		 	then delete common_save_par.par;*


	    end-if
	end-if

	! delete all photometrical globals:

!	delete-global pho_global 

  end-proc

.title
VICAR program PHOTFIT2 determines the coefficients of various photometric 
functions.

.help
PURPOSE:	PHOTFIT2 is a VICAR program which determines the coefficients 
		of various photometric functions.  

	
FUNCTION:

  PHOTFIT2 reads IBIS1 and IBIS2 files which have been generated by the VICAR 
programs PHOTOM, HWPHOCAE/D, PHOTTEST2. PHOTOM or HWPHOCAE/D are used to collect photometric data from images (and their navigation). The IBIS files contain all 
the data required for PHOTFIT2. There is one input IBIS file for each picture. 
The output of PHOTFIT2 is listed coefficient values on screen [or an IBIS2 
photometric parameterfile (subtyp=phopar) - not yet implemented].

.page


EXECUTION:

  Be very conservative about the amount of data to include in the
IBIS files. PHOTFIT3 is a slow program. It is the quality of the
IBIS data points, not their quantity, which matters. You should
assure as broad a coverage of incidence, emission, and phase
angles as possible. If you have 10 points/ibis file
and 6 files (one for each phase angle) that should be sufficient.  
You might consider restricting points (from PHOTOM, HWPHOCA*) to areas of
consistent albedo based upon your theories of morphology.
  The Hapke and Veverka functions are EXTREMELY sensitive to
data which, because of a lack of broad ranges of incidence, emission,
and phase angles, or because of inclusion of various albedo's,
does not truly represent the function being fit. When this 
problem occurs the coefficients will become distorted in order
to accomodate erroneous data. Sometimes you can tell when the coefficients
returned lie up against the limits of their permitted ranges.
  Accuracy: Rerun PHOTFIT2 at least twice to assure that the function
has had sufficient time to find a stable minimum. If you cool the
solution too fast it will 'freeze' on the wall of the error minimum.
Comparing several answers will give a feeling of the precision of the 
result. 'Freezing' can be avoided by providing either a higher initial
'temperature' or more iterations.
  The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.page

USAGE:

  There are sets of four keywords which relate to each function:
These are the parmeters, T_*, MAX_*, MIN_*. These provide the:
1.  Initial position guess for the photometric parameters. This is not 
    important per-se but it assures you that Metropolis will investigate 
    this point (and remember what it found). 
2.  Range over which random guesses can be expected to vary at first:
    PARAM_NEW = T_* * tan( PI * ran_num + PI/2 ).
    This is the 'temperature' for each variable. As the system cools the range 
    will constrict gradually (T_NEW_* = T_OLD_* * scale, scale depends of 
    NUMTEN) until the temperatur is only 10**-MAXITER/NUMTEN times the inital 
    temperatur of the parameter.
3/4.Two limits MIN_*, MAX_* outside of which each of the photometric 
    parameters are stricly prohibited to fall. Each time these limits 
    are violated the attempted solution is abandoned and a new one is 
    attempted.  This will happen thousands of times, especially at first 
    when the temperatures are high.  
  
Each of these has defaults but you must not consider these as gospel.

The maxiter parameter proposals are a bit arbitrary. They will depend
upon experience. You will probably be able to reduce them greatly.


.page

METHOD:

  PHOTFIT2 uses the simulated annealing method  to solve for the function 
coefficients. This is verry inefficient solution method which is good for 
fitting complicated functions. The solution is arrived at through guessing. 
Initially guessing is performed over a large region using a random number
generator. As time progresses the "temperature" is reduced, 
constricting the range of guesses to an ever decreasing region
in the N solution space. Decisions are made at every step based upon
the Boltzmann probability of transitioning from one energy level
(error due to incorrect solution estimate) to the next. This is
essentially a chaotic downhill search scheme which mimics the
cooling history of a thermodynamic system.
  Because of the technique used there is no way to estimate the 
accuracy of the coefficients for a single solution. Rerun the program
several times. Note that the routine Metropolis will take
a different solution route each attempt (based upon the time of day)
so you cannot ever repeat yourself (or cheat).


.page

 STARTING PROGRAM 

In the SHELL-VICAR :

	 PHOTFIT2 'INP=PHOTCAT.DAT PARAMS'
  (no default values for the photometric parameters!)



In the command modus :

	TAE> PHOTFIT2 INP=PHOTCAT.DAT PARAMS
   (no default values for the photometric parameters!)
 or
	TAE> PHOTFIT2M OUT=PHOCAT.DAT PARAMS  
  (it helds for every photometric function its own parameter defaults)



In the tutor modus  --> menu-driven parameter input :

	TAE> tutor PHOTFIT2M   
  (it helds for every photometric function its own parameter defaults)


tutor PHOTTESTM

  There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  

The menu points have the following meanings:

1. Select the first menu point to input the general parameters for the program
   such as the names of input photometric catalog IBIS file, the type of 
   photometric funtion, the fit conditions and the kind of outputs. 

2. This point contains all function specific parameters (first guess, upper and 
   lower limits, first temperatur). The name of this menu point is changing 
   depending on your input of the parameter PHO_FUNC in the first menu point. 

3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save photfit2m.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


.page

HELPS :

- You will get the common help contained in the ".mdf" file (photfit2m.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (photfit2m.pdf
  or photfit2.pdf) by processing of "help-help" applied to the program 
  (should be verry similary ones of photfit2m.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.



.page

TESTING and EXAMPLES:

  You can test PHOTFIT2 with program PHOTTEST2:

  phottest out=phottest_m.dat PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.7 EXPONENT=0.6 +
  START=(10,10,10) DELTA=(30,30,180) SIGMA=0.000001 

  photfit2 inp=(phottest_m.dat,phottest_m.dat) PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.6 MIN_ALBEDO=0.0 MAX_ALBEDO=1.0 T_ALBEDO=0.1 +
  EXPONENT=0.6 MIN_EXPONENT=0.0 MAX_EXPONENT=1.0 T_EXPONENT=0.1 +
  NORM=25 RERUN=2 MAXITER=100 NUMTEN=25 METRO=20 PERCENT=90 TOLERANC=0.02 'PRINT 

  or, to actually run it you must generate a bunch of photom/hwphoca*
  files:

  photom INP=pix#1 out=ibis1    ( interactive job )
  photom INP=(pix#2,ibis1) out=ibis2 'batch   (batch mode)
  photom INP=(pix#3,ibis1) out=ibis3 'batch   (batch mode)
  photom INP=(pix#4,ibis1) out=ibis4 'batch   (batch mode)
  photom INP=(pix#5,ibis1) out=ibis5 'batch   (batch mode)
  photom INP=(pix#6,ibis1) out=ibis6 'batch   (batch mode)
  photfit2 INP=(ibis1,ibis2,ibis3,ibis4,ibis5,ibis6) +
           PHO_FUNC=HAPKE_86_HG1  MAXITER=20000

.page



INPUT

PHOTFIT2 accepts two types of input files - the old IBIS1 file and a IBIS2 file 
of the type=phocat.


IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values.


PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 



.page


SUBROUTINES REQUIRED TO RUN PROGRAM:	pho_routines package,
					PHOPDF package

INCLUDE FILES REQUIRED TO RUN PROGRAM:	pho.h,
					pho_global.pdf,
					ibisfile.h, ibisfile.h, 
					vicmain_c, defines.h, 
					math.h, time.h 

	

BACKGROUND AND REFERENCES :	Jean J. Lorre, 
				Function Minimization With Partially Corrected 
				Data Via Simulated Annealing,
				J. Soc. Ind. Appl. Math., 43 (1990), 123-127


SOFTWARE PLATFORM :		VICAR, TAE
				(AXP/SUNOS/SOLARIS/SGI)

HARDWARE PLATFORM :		No particular hardware required;
				tested on AXP/SUNOS/SOLARIS/SGI

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Programmer: J J Lorre,  Jan 10 1988
				rewritten in C: Friedel Oschuetz, Nov. '95,

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)


.LEVEL1

.VARI INP
photometric catalog

.VARI PHO_FUNC
photometric function

.VARIABLE CLASS_ID
Class-id

.VARIABLE NORM
Causes subroutine Metropolis 
to renormalize itself.

.VARIABLE RERUN
Number of rerun of metropolis

.VARIABLE MAXITER
Specifies the total number of 
successful iterations.

.VARIABLE NUMTEN
Number of iterations
before temperature
drops by a factor of ten.

.VARIABLE PERCENT
Minimum acceptable # of points
with residuals below tolerance.

.VARIABLE TOLERANC
The I/F residual tolerance.

.VARIABLE PRINT
Screen output  
of the IBIS input files.

.VARIABLE METROP
List the iteration progress.

.VARI ALBEDO
Surface albedo

.var MIN_ALBEDO
Minimum of surface albedo

.var MAX_ALBEDO
Maximum of surface albedo

.var T_ALBEDO
Temperatur of Surface albedo

.var EXPONENT
Minnaert exponent

.var MIN_EXPONENT
Maximum of Minnaert exponent

.var MAX_EXPONENT
Maximum of Minnaert exponent

.var T_EXPONENT
Temperatur of Minnaert exponent

.VARI A_VEVERKA 
Veverka parameter

.VARI MIN_A_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_A_VEVERKA 
Maximum of Veverka parameter

.VARI T_A_VEVERKA 
Temperatur of Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI MIN_B_VEVERKA
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA
Maximum of Veverka parameter

.VARI T_B_VEVERKA
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MIN_D_VEVERKA
Minimum of Veverka parameter

.VARI MAX_D_VEVERKA
Maximum of Veverka parameter

.VARI T_D_VEVERKA
Temperatur of Veverka parameter

.VARI MO_EXP1
Mosher's exponent

.VARI MIN_MO_EXP1
Minimum of Mosher's exponent

.VARI MAX_MO_EXP1
Maximum of Mosher's exponent

.VARI T_MO_EXP1
Temperatur of Mosher's exponent

.VARI MO_EXP2
Mosher's exponent

.VARI MIN_MO_EXP2
Minimum of Mosher's exponent

.VARI MAX_MO_EXP2
Maximum of Mosher's exponent

.VARI T_MO_EXP2
Temperatur of Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI MIN_E_BURATTI
Minimum of Buratti's parameter

.VARI MAX_E_BURATTI
Maximum of Buratti's parameter

.VARI T_E_BURATTI
Temperatur of Buratti's parameter

.VARI DEN_SOIL
Density of the soil

.var MIN_DEN_SOIL
Minimum of density of the soil

.var MAX_DEN_SOIL
Maximum of density of the soil

.var T_DEN_SOIL
Temperatur of density of the soil

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.VARI HG1_SOIL
Henyey-Greenstein term

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG2_SOIL
Henyey-Greenstein term

.var MIN_HG2_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG2_SOIL
Maximum of Henyey-Greenstein term

.VARI T_HG2_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG_ASY_SOIL
Asymetry term of
Henyey-Greenstein

.VARI MIN_HG_ASY_SOIL
Minimum of asymetry parameter

.VARI MAX_HG_ASY_SOIL
Maximum of asymetry parameter

.VARI T_HG_ASY_SOIL
Temperatur of asymetry parameter

.VARI LE1_SOIL
Hapke parameter
First Legendre-Polynom

.VARI MIN_LE1_SOIL
Minimum of 
first Legendre-Polynom

.VARI MAX_LE1_SOIL
Maximum of 
first Legendre-Polynom

.VARI T_LE1_SOIL
Temperatur of 
first Legendre-Polynom

.VARI LE2_SOIL
Second Legendre-Polynom

.VARI MIN_LE2_SOIL
Minimum of 
second Legendre-Polynom

.VARI MAX_LE2_SOIL
Maximum of 
second Legendre-Polynom

.VARI T_LE2_SOIL
Temperatur of 
second Legendre-Polynom

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of width of opposition magnitude

.VARI H_CBOE
Width of opposition surge
due by coherent backscatter

.VARI MIN_H_CBOE
Minimum of width of opposition surge
due by coherent backscatter

.VARI MAX_H_CBOE
Maximum of width of opposition surge
due by coherent backscatter

.VARI T_H_CBOE
Temperatur of 
width of opposition surge
due by coherent backscatter

.VARI B_CBOE
Opposition magnitude
due by coherent backscatter

.VARI MIN_B_CBOE
Minimum of opposition magnitude
due by coherent backscatter

.VARI MAX_B_CBOE
Maximum of opposition magnitude
due by coherent backscatter

.VARI T_B_CBOE
Temperatur of 
opposition magnitude
due by coherent backscatter

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.VARI COOK
Hapke-Cook parameter

.VARI MIN_COOK
Minimum of Hapke-Cook parameter

.VARI MAX_COOK
Maximum of Hapke-Cook parameter

.VARI T_COOK
Temperatur of Hapke-Cook parameter

.VARI TAU_ATM
Atmospheric optical depth

.VARI MIN_TAU_ATM
Minimum of 
atmospheric optical depth

.VARI MAX_TAU_ATM
Maximum of 
atmospheric optical depth

.VARI T_TAU_ATM
Temperatur of  
atmospheric optical depth

.VARI W_ATM
Atmospheric single scattering albedo

.VARI MIN_W_ATM
Minimum of 
atmospheric single scattering albedo

.VARI MAX_W_ATM
Maximum of 
atmospheric single scattering albedo

.VARI T_W_ATM
Temperatur of 
atmospheric ingle scattering albedo

.VARI HG1_ATM
Atmospheric Henyey-Greenstein term

.VARI MIN_HG1_ATM
Minimum of 
atmospheric Henyey-Greenstein term

.VARI MAX_HG1_ATM
Maximum of 
atmospheric Henyey-Greenstein term

.VARI T_HG1_ATM
Temperatur of
atmospheric Henyey-Greenstein term

.vari IRV_EXP1
Irvine's first exponent

.vari MIN_IRV_EXP1
Minimum of Irvine's first exponent

.vari MAX_IRV_EXP1
Maximum of Irvine's first exponent

.vari T_IRV_EXP1
Temperatur of Irvine's first exponent

.vari IRV_EXP2
Irvine's second exponent

.vari MIN_IRV_EXP2
Minimum of Irvine's second exponent

.vari MAX_IRV_EXP2
Maximum of Irvine's second exponent

.vari T_IRV_EXP2
Temperatur of Irvine's second exponent

.VARIABLE CLASS_ID
Class-id

.VARI SAVE_PAR
file name for par-file


.LEVEL2

.VARI INP
File names of the input input IBIS photometric catalog files.
PHOTFIT2 accepts two types of files - the old IBIS1 file and a IBIS2 file of 
the type=phocat.

IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values.

.page

PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 

.VARI PHO_FUNC
Photometric function :

This parameter of the first menu point selects the menu point for input the 
photometry task:
When returning to the highest level of the menu (i.e. the PHOTFIT2.MDF-file) 
you will see that the third selection point has been changed according to your 
input of PHO_FUNC in the first menu point.

.VARIABLE CLASS_ID
The "phocat" file can contain data of different classes. The class_id numerates 
the photometric functions. For using different photometric functions or 
parameter sets. The program PHOTFIT2 will using the data of given class only 
(or all data if CLASS_ID is not given).

.VARIABLE NORM
Causes subroutine Metropolis to renormalize itself by recomputing the
Boltzmann coefficient. NORM=n causes renormalization each n successful
iterations. 

.VARIABLE RERUN
Number of rerun of metropolis. You can see the stability of the results. But be 
aware, the mean values and there deviations of the parameters are not real 
statistical values because every rerun of metropolis starts with the best fit 
of the run before.  
Default for RERUN is 1

.VARIABLE MAXITER
Specifies the number of successful iterations which Metropolis will
perform before ceasing in it's hunt for the coefficient values.
Usefully is for MAXITER is:
for MINNAERT 5000
for VEVERKA  20000
for HAPKE_* 20000
for HAPKE_* 20000 

.VARIABLE NUMTEN
Specifies the number of successful iterations which must be 
accumulated before the width of the solution generating  probability
function drops by a factor of ten. If for example MAXITER/NUMTEN
is 4.0 then the initial range specified by the temperatur
parameter (the starting temperature) is reduced by 4.0 orders
of magnitude (10000:1) by the time the iteration process has
ceased. 
Default for NUMTEN is: MAXITER/4

.VARIABLE PERCENT
The minimum acceptable # of points with residuals below tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.VARIABLE TOLERANC
The I/F residual tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.VARIABLE PRINT
Keyword for screen output of the IBIS input files.
NOPRINT deactivates the sceen output of IBIS input file.

.VARIABLE METROP
Causes subroutine Metropolis to list the iteration progress as it
converges upon the solution. METROP=n causes a printout each
n successful iterations. 

.VARI SAVE_PAR
This is the name for the TAE-parameter file containing all parameters 
needed to running the program. The default name is PHOTFIT2.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.
This parameter gives the albedo of the surface. 

.var MIN_ALBEDO
This parameter gives the absolut lower limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_ALBEDO
This parameter gives the absolut upper limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_ALBEDO
This parameter gives temperatur for the albedo of the surface. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI MIN_EXPONENT
This parameter gives the absolut lower limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_EXPONENT
This parameter gives the absolut upper limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_EXPONENT
This parameter gives temperatur for the Exponent - the geometrical constant k
of the Minnaert photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    EXPONENT_NEW = T_EXPONENT * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_EXPONENT_NEW_* = T_EXPONENT_OLD_* * scale, 
scale depends of NUMTEN.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_A_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_A_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_A_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    A_VEVERKA_NEW = T_A_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_A_VEVERKA_NEW_* = T_A_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI MIN_B_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_C_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MIN_MO_EXP1
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP1
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP1
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP2).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP1_NEW = T_MO_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP1_NEW_* = T_MO_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI MIN_MO_EXP2
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP2
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP2
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP1).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP2_NEW = T_MO_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP2_NEW_* = T_MO_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

.VARI DEN_SOIL
Specific volume density of the soil.

.var MIN_DEN_SOIL
This parameter gives the absolut lower limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_DEN_SOIL
This parameter gives the absolut upper limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_DEN_SOIL
This parameter gives temperatur for the specific volume density of the soil.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    DEN_SOIL_NEW = T_DEN_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_DEN_SOIL_NEW_* = T_DEN_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI MIN_HG2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG2_SOIL_NEW = T_HG2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG2_SOIL_NEW_* = T_HG2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG_ASY_SOIL
This parameter gives the asymmetry parameter (weight of the two terms in the 
Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MIN_HG_ASY_SOIL
This parameter gives the absolut lower limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MAX_HG_ASY_SOIL
This parameter gives the absolut upper limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG_ASY_SOIL
This parameter gives temperatur for the parameter of the asymmetry parameter (weight of the two terms in the Henyey-Greenstein soil phase function).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG_ASY_SOIL_NEW = T_HG_ASY_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG_ASY_SOIL_NEW_* = T_HG_ASY_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI MIN_H_CBOE
This parameter gives the absolut lower limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI MAX_H_CBOE
This parameter gives the absolut upper limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI T_H_CBOE
This parameter gives temperatur for the parameter of the coherent 
backscattering ( width of theopposition surge due to the backscatter ).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_CBOE_NEW = T_H_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_CBOE_NEW_* = T_H_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI MIN_B_CBOE
This parameter gives the absolut lower limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI MAX_B_CBOE
This parameter gives the absolut upper limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI T_B_CBOE
This parameter gives temperatur for the opposition magnitude coefficient of the 
coherent backscattering (height of opposition surge due to backscatter). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_CBOE_NEW = T_B_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_CBOE_NEW_* = T_B_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI MIN_COOK
This parameter gives the absolut lower limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_COOK
This parameter gives the absolut upper limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_COOK
This parameter gives temperatur for the parameter of the Cook's modification 
of the old Hapke function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    COOK_NEW = T_COOK * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_COOK_NEW_* = T_COOK_OLD_* * scale, 
scale depends of NUMTEN.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI MIN_TAU_ATM
This parameter gives the absolut lower limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_TAU_ATM
This parameter gives the absolut upper limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_TAU_ATM
This parameter gives temperatur for the optical depth of the atmosphere.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    TAU_ATM_NEW = T_TAU_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_TAU_ATM_NEW_* = T_TAU_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI MIN_W_ATM
This parameter gives the absolut lower limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_W_ATM
This parameter gives the absolut upper limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_W_ATM
This parameter gives temperatur for the single scattering albedo of the 
atmospheric aerosols.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_ATM_NEW = T_W_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_ATM_NEW_* = T_W_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI MIN_HG1_ATM
This parameter gives the absolut lower limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG1_ATM
This parameter gives the absolut upper limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG1_ATM
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein atmospheric phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_ATM_NEW = T_HG1_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_ATM_NEW_* = T_HG1_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP1
Irvine's first exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP1
This parameter gives the absolut lower limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP1
This parameter gives the absolut upper limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP1
This parameter gives temperatur for the Irvine's first exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
   IRV_EXP1_NEW = T_IRV_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP1_NEW_* = T_IRV_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP2
Irvine's second exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP2
This parameter gives the absolut lower limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP2
This parameter gives the absolut upper limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP2
This parameter gives temperatur for the Irvine's second exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    IRV_EXP2_NEW = T_IRV_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP2_NEW_* = T_IRV_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2m.mdf
.TITLE
VICAR Program '&"pho_PROC_NAME"'

.proc &"pho_PROC_NAME"_general
Enter general input parameters and select function of the photometry menu-point
(type RUN when done)

.proc photfit2_&PHO_FUNC_type
Enter parameters for the "&PHO_FUNC_type" photometric function 
(type RUN when done)

.proc common_save_par
Enter the name for the par-file where you want to save your parameters
(type RUN when done)

.proc common_proc_done
Run main photometry application program '&"pho_PROC_NAME"'



.help
Name of Program:	PHOTFIT2
	
PURPOSE:	Photfit2 is a VICAR program which determines the coefficients 
		of various photometric functions.  

	
FUNCTION:

  PHOTFIT2 reads IBIS1 and IBIS2 files which have been generated by the VICAR 
programs PHOTOM, HWPHOCAE/D, PHOTTEST2. PHOTOM or HWPHOCAE/D are used to collect photometric data from images (and their navigation). The IBIS files contain all 
the data required for PHOTFIT2. There is one input IBIS file for each picture. 
The output of PHOTFIT2 is listed coefficient values on screen [or an IBIS2 
photometric parameterfile (subtyp=phopar) - not yet implemented].

.page


EXECUTION:

  Be very conservative about the amount of data to include in the
IBIS files. PHOTFIT3 is a slow program. It is the quality of the
IBIS data points, not their quantity, which matters. You should
assure as broad a coverage of incidence, emission, and phase
angles as possible. If you have 10 points/ibis file
and 6 files (one for each phase angle) that should be sufficient.  
You might consider restricting points (from PHOTOM, HWPHOCA*) to areas of
consistent albedo based upon your theories of morphology.
  The Hapke and Veverka functions are EXTREMELY sensitive to
data which, because of a lack of broad ranges of incidence, emission,
and phase angles, or because of inclusion of various albedo's,
does not truly represent the function being fit. When this 
problem occurs the coefficients will become distorted in order
to accomodate erroneous data. Sometimes you can tell when the coefficients
returned lie up against the limits of their permitted ranges.
  Accuracy: Rerun PHOTFIT2 at least twice to assure that the function
has had sufficient time to find a stable minimum. If you cool the
solution too fast it will 'freeze' on the wall of the error minimum.
Comparing several answers will give a feeling of the precision of the 
result. 'Freezing' can be avoided by providing either a higher initial
'temperature' or more iterations.
  The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.page

USAGE:

  There are sets of four keywords which relate to each function:
These are the parmeters, T_*, MAX_*, MIN_*. These provide the:
1.  Initial position guess for the photometric parameters. This is not 
    important per-se but it assures you that Metropolis will investigate 
    this point (and remember what it found). 
2.  Range over which random guesses can be expected to vary at first:
    PARAM_NEW = T_* * tan( PI * ran_num + PI/2 ).
    This is the 'temperature' for each variable. As the system cools the range 
    will constrict gradually (T_NEW_* = T_OLD_* * scale, scale depends of 
    NUMTEN) until the temperatur is only 10**-MAXITER/NUMTEN times the inital 
    temperatur of the parameter.
3/4.Two limits MIN_*, MAX_* outside of which each of the photometric 
    parameters are stricly prohibited to fall. Each time these limits 
    are violated the attempted solution is abandoned and a new one is 
    attempted.  This will happen thousands of times, especially at first 
    when the temperatures are high.  
  
Each of these has defaults but you must not consider these as gospel.

The maxiter parameter proposals are a bit arbitrary. They will depend
upon experience. You will probably be able to reduce them greatly.


.page

METHOD:

  PHOTFIT2 uses the simulated annealing method  to solve for the function 
coefficients. This is verry inefficient solution method which is good for 
fitting complicated functions. The solution is arrived at through guessing. 
Initially guessing is performed over a large region using a random number
generator. As time progresses the "temperature" is reduced, 
constricting the range of guesses to an ever decreasing region
in the N solution space. Decisions are made at every step based upon
the Boltzmann probability of transitioning from one energy level
(error due to incorrect solution estimate) to the next. This is
essentially a chaotic downhill search scheme which mimics the
cooling history of a thermodynamic system.
  Because of the technique used there is no way to estimate the 
accuracy of the coefficients for a single solution. Rerun the program
several times. Note that the routine Metropolis will take
a different solution route each attempt (based upon the time of day)
so you cannot ever repeat yourself (or cheat).


.page

 STARTING PROGRAM 

In the SHELL-VICAR :

	 PHOTFIT2 'INP=PHOTCAT.DAT PARAMS'
  (no default values for the photometric parameters!)



In the command modus :

	TAE> PHOTFIT2 INP=PHOTCAT.DAT PARAMS
   (no default values for the photometric parameters!)
 or
	TAE> PHOTFIT2M OUT=PHOCAT.DAT PARAMS  
  (it helds for every photometric function its own parameter defaults)



In the tutor modus  --> menu-driven parameter input :

	TAE> tutor PHOTFIT2M   
  (it helds for every photometric function its own parameter defaults)


tutor PHOTTESTM

  There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  

The menu points have the following meanings:

1. Select the first menu point to input the general parameters for the program
   such as the names of input photometric catalog IBIS file, the type of 
   photometric funtion, the fit conditions and the kind of outputs. 

2. This point contains all function specific parameters (first guess, upper and 
   lower limits, first temperatur). The name of this menu point is changing 
   depending on your input of the parameter PHO_FUNC in the first menu point. 

3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save photfit2.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


.page

HELPS :

- You will get the common help contained in the ".mdf" file (photfit2m.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (photfit2m.pdf
  or photfit2.pdf) by processing of "help-help" applied to the program 
  (should be verry similary ones of photfit2m.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.



.page

TESTING and EXAMPLES:

   You can test PHOTFIT2 with program PHOTTEST2:

  phottest out=phottest_m.dat PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.7 EXPONENT=0.6 +
  START=(10,10,10) DELTA=(30,30,180) SIGMA=0.000001 

  photfit2 inp=(phottest_m.dat,phottest_m.dat) PHO_FUNC=MINNAERT CLASS_ID=2 +
  ALBEDO=0.6 MIN_ALBEDO=0.0 MAX_ALBEDO=1.0 T_ALBEDO=0.1 +
  EXPONENT=0.6 MIN_EXPONENT=0.0 MAX_EXPONENT=1.0 T_EXPONENT=0.1 +
  NORM=25 RERUN=2 MAXITER=100 NUMTEN=25 METRO=20 PERCENT=90 TOLERANC=0.02 'PRINT 

  or, to actually run it you must generate a bunch of photom/hwphoca*
  files:

  photom INP=pix#1 out=ibis1    ( interactive job )
  photom INP=(pix#2,ibis1) out=ibis2 'batch   (batch mode)
  photom INP=(pix#3,ibis1) out=ibis3 'batch   (batch mode)
  photom INP=(pix#4,ibis1) out=ibis4 'batch   (batch mode)
  photom INP=(pix#5,ibis1) out=ibis5 'batch   (batch mode)
  photom INP=(pix#6,ibis1) out=ibis6 'batch   (batch mode)
  photfit2 INP=(ibis1,ibis2,ibis3,ibis4,ibis5,ibis6) +
           PHO_FUNC=HAPKE_86_HG1  MAXITER=20000

.page


IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values as computed from the photometric 
		      function.


PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 



.page


SUBROUTINES REQUIRED TO RUN PROGRAM:	pho_routines package,
					PHOPDF package

INCLUDE FILES REQUIRED TO RUN PROGRAM:	pho.h,
					pho_global.pdf,
					ibisfile.h, ibisfile.h, 
					vicmain_c, defines.h, 
					math.h, time.h 

	

BACKGROUND AND REFERENCES :	Jean J. Lorre, 
				Function Minimization With Partially Corrected 
				Data Via Simulated Annealing,
				J. Soc. Ind. Appl. Math., 43 (1990), 123-127

SOFTWARE PLATFORM :		VICAR, TAE
				(AXP/SUNOS/SOLARIS/SGI)

HARDWARE PLATFORM :		No particular hardware required;
				tested on AXP/SUNOS/SOLARIS/SGI

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Programmer: J J Lorre,  Jan 10 1988
				rewritten in C: Friedel Oschuetz, Nov. '95,

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)


.page

VICAR PARAMETERS:

NOTE: All parameters (including those from the photometry subPDFs) are 
defined in the mainPDF (PHOTFIT2.pdf).
Every parameter of the photometric function has MIN_*, MAX_* and T_* for the 
minmum maximum and the couling temperatur(see obove).

PARAMETER:

INP
File names of the input input IBIS photometric catalog files.
PHOTFIT2 accepts two types of files - the old IBIS1 file and a IBIS2 file of 
the type=phocat.

IBIS1 FILE FORMAT:

There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values.

.page

PHOCAT FILE:

The structure of the IBIS2 file of type phocat is desined in such a way that 
tiepoint files can be extended and containing all collumns of the old IBIS1 
photometric catalog files. The program PHOTFIT2 used only one IMAGE_* group at 
time. but tiepoint files using some IMAGE_* groups containing informations 
relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g. 
CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 
(There are 19 columns in this file.)

abstract groups	      primitive groups    units	      formats  used in PHOTFIT2

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
				StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used

The "phocat" file can contain data of different classes (CLASS_ID). The program 
PHOTFIT2 will using the data of given class only (or all data if class is not 
given).
The program uses the value from the column "StandDev" (if given) for weigthing the reflectance value by fitting. 

PHO_FUNC
Photometric function :

This parameter of the first menu point selects the menu point for input the 
photometry task:
When returning to the highest level of the menu (i.e. the PHOTFIT2.MDF-file) 
you will see that the third selection point has been changed according to your 
input of PHO_FUNC in the first menu point.

CLASS_ID
The "phocat" file can contain data of different classes. The class_id numerates 
the photometric functions. For using different photometric functions or 
parameter sets. The program PHOTFIT2 will using the data of given class only 
(or all data if CLASS_ID is not given).

NORM
Causes subroutine Metropolis to renormalize itself by recomputing the
Boltzmann coefficient. NORM=n causes renormalization each n successful
iterations. 

RERUN
Number of rerun of metropolis. You can see the stability of the results. But be 
aware, the mean values and there deviations of the parameters are not real 
statistical values because every rerun of metropolis starts with the best fit 
of the run before.  
Default for RERUN is 1

MAXITER
Specifies the number of successful iterations which Metropolis will
perform before ceasing in it's hunt for the coefficient values.
Usefully is for MAXITER is:
for MINNAERT 5000
for VEVERKA  20000
for HAPKE_* 20000
for HAPKE_* 20000 

NUMTEN
Specifies the number of successful iterations which must be 
accumulated before the width of the solution generating  probability
function drops by a factor of ten. If for example MAXITER/NUMTEN
is 4.0 then the initial range specified by the temperatur
parameter (the starting temperature) is reduced by 4.0 orders
of magnitude (10000:1) by the time the iteration process has
ceased. 
Default for NUMTEN is: MAXITER/4

PERCENT
The minimum acceptable # of points with residuals below tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

TOLERANC
The I/F residual tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

PRINT
Keyword for screen output of the IBIS input files.
NOPRINT deactivates the sceen output of IBIS input file.

METROP
Causes subroutine Metropolis to list the iteration progress as it
converges upon the solution. METROP=n causes a printout each
n successful iterations. 

SAVE_PAR
This is the name for the TAE-parameter file containing all parameters 
needed to running the program. The default name is PHOTFIT2.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.
This parameter gives the albedo of the surface. 

MIN_ALBEDO
This parameter gives the absolut lower limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_ALBEDO
This parameter gives the absolut upper limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_ALBEDO
This parameter gives temperatur for the albedo of the surface. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

MIN_EXPONENT
This parameter gives the absolut lower limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_EXPONENT
This parameter gives the absolut upper limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_EXPONENT
This parameter gives temperatur for the Exponent - the geometrical constant k
of the Minnaert photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    EXPONENT_NEW = T_EXPONENT * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_EXPONENT_NEW_* = T_EXPONENT_OLD_* * scale, 
scale depends of NUMTEN.

A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

MIN_A_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_A_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_A_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    A_VEVERKA_NEW = T_A_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_A_VEVERKA_NEW_* = T_A_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

MIN_B_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_B_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_B_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.
Usually :
C_VEVERKA=1-A_VEVERKA

MIN_C_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_C_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

MIN_MO_EXP1
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_MO_EXP1
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_MO_EXP1
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP2).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP1_NEW = T_MO_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP1_NEW_* = T_MO_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

MIN_MO_EXP2
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_MO_EXP2
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_MO_EXP2
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP1).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP2_NEW = T_MO_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP2_NEW_* = T_MO_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

DEN_SOIL
Specific volume density of the soil.

MIN_DEN_SOIL
This parameter gives the absolut lower limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_DEN_SOIL
This parameter gives the absolut upper limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_DEN_SOIL
This parameter gives temperatur for the specific volume density of the soil.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    DEN_SOIL_NEW = T_DEN_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_DEN_SOIL_NEW_* = T_DEN_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

MIN_HG2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_HG2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_HG2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG2_SOIL_NEW = T_HG2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG2_SOIL_NEW_* = T_HG2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

HG_ASY_SOIL
This parameter gives the asymmetry parameter (weight of the two terms in the 
Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

MIN_HG_ASY_SOIL
This parameter gives the absolut lower limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

MAX_HG_ASY_SOIL
This parameter gives the absolut upper limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_HG_ASY_SOIL
This parameter gives temperatur for the parameter of the asymmetry parameter (weight of the two terms in the Henyey-Greenstein soil phase function).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG_ASY_SOIL_NEW = T_HG_ASY_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG_ASY_SOIL_NEW_* = T_HG_ASY_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.

For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.
 
H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

MIN_H_CBOE
This parameter gives the absolut lower limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

MAX_H_CBOE
This parameter gives the absolut upper limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

T_H_CBOE
This parameter gives temperatur for the parameter of the coherent 
backscattering ( width of theopposition surge due to the backscatter ).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_CBOE_NEW = T_H_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_CBOE_NEW_* = T_H_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

MIN_B_CBOE
This parameter gives the absolut lower limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

MAX_B_CBOE
This parameter gives the absolut upper limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

T_B_CBOE
This parameter gives temperatur for the opposition magnitude coefficient of the 
coherent backscattering (height of opposition surge due to backscatter). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_CBOE_NEW = T_B_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_CBOE_NEW_* = T_B_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

COOK
 Parameter of the Cook's modification of the old Hapke function.

MIN_COOK
This parameter gives the absolut lower limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_COOK
This parameter gives the absolut upper limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_COOK
This parameter gives temperatur for the parameter of the Cook's modification 
of the old Hapke function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    COOK_NEW = T_COOK * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_COOK_NEW_* = T_COOK_OLD_* * scale, 
scale depends of NUMTEN.

TAU_ATM
Optical depth of the atmosphere.

MIN_TAU_ATM
This parameter gives the absolut lower limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_TAU_ATM
This parameter gives the absolut upper limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_TAU_ATM
This parameter gives temperatur for the optical depth of the atmosphere.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    TAU_ATM_NEW = T_TAU_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_TAU_ATM_NEW_* = T_TAU_ATM_OLD_* * scale, 
scale depends of NUMTEN.

W_ATM
Single scattering albedo of the atmospheric aerosols.

MIN_W_ATM
This parameter gives the absolut lower limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_W_ATM
This parameter gives the absolut upper limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_W_ATM
This parameter gives temperatur for the single scattering albedo of the 
atmospheric aerosols.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_ATM_NEW = T_W_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_ATM_NEW_* = T_W_ATM_OLD_* * scale, 
scale depends of NUMTEN.

HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI MIN_HG1_ATM
This parameter gives the absolut lower limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_HG1_ATM
This parameter gives the absolut upper limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_HG1_ATM
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein atmospheric phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_ATM_NEW = T_HG1_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_ATM_NEW_* = T_HG1_ATM_OLD_* * scale, 
scale depends of NUMTEN.

IRV_EXP1
Irvine's first exponent - parameter of the Irvine photometric function.

MIN_IRV_EXP1
This parameter gives the absolut lower limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_IRV_EXP1
This parameter gives the absolut upper limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_IRV_EXP1
This parameter gives temperatur for the Irvine's first exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
   IRV_EXP1_NEW = T_IRV_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP1_NEW_* = T_IRV_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

IRV_EXP2
Irvine's second exponent - parameter of the Irvine photometric function.

MIN_IRV_EXP2
This parameter gives the absolut lower limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

MAX_IRV_EXP2
This parameter gives the absolut upper limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

T_IRV_EXP2
This parameter gives temperatur for the Irvine's second exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    IRV_EXP2_NEW = T_IRV_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP2_NEW_* = T_IRV_EXP2_OLD_* * scale, 
scale depends of NUMTEN.




PARAMETERS SORTED BY SUB-PDFS :

Name of PDF file	PDF parameters	default		comments
(will appear as a	within this	setting	
separate sub-menu)	particular 
			sub-menu			


1) GENERAL INPUT / OUTPUT :
	
photfit2m_general       INP

			PHO_FUNC	MINNAERT The photometric function
						(see PHOPDF)
									
						Permitted values :

						PAR_FILE, 
						LAMBERT, 
						MINNAERT, 
						IRVINE, 
						VEVERKA, 
						BURATTI1, 
						BURATTI2, 
						BURATTI3, 
						MOSHER, 
						LUMME_BOWEL_HG1, 
						HAPKE_81_LE2, 
						HAPKE_81_COOK, 
						HAPKE_86_HG1, 
						HAPKE_86_HG2, 
						HAPKE_86_LE2, 
						HAPKE_HG1_DOM, 
						REGNER_HAPKE_HG1, 
						ATMO_CORR_REGNER


2) PHOTOMETRY MENU :

Note: the subPDFs pertaining to photometry are deliverd to the VICAR system by 
PHOPDF.  They may not be changed, however, the default values may be set, reset 
or overwritten by the mainPDF (PHOTFIT2M in this case).  All PDF parameters 
must be defined in the main program's PDF in oder to access the different 
submenues correctly.  

actuell second menu	Name		default		Comment
(photom.function)	(funct.params)

PHO_PAR_FILE		PHO_PAR_FILE		parameters from IBIS 
						pho_par_file

LAMBERT			ALBEDO		0.5
			MIN_ALBEDO	0.0
			MAX_ALBEDO	1.0
			T_ALBEDO	0.3

MINNAERT		ALBEDO		0.5
			MIN_ALBEDO	0.0
			MAX_ALBEDO	1.0
			T_ALBEDO	0.3
			EXPONENT	0.6
			MIN_EXPONENT	0.0
			MAX_EXPONENT	1.0
			T_EXPONENT	0.3

IRVINE			EXPONENT	0.9
			MIN_EXPONENT	0.0
			MAX_EXPONENT	1.0
			T_EXPONENT	0.3
			IRV_EXP1	0.118
			MIN_IRV_EXP1	0.0
			MAX_IRV_EXP1	1.0
			T_IRV_EXP1	0.1	
			IRV_EXP2	0.0039
			MIN_IRV_EXP2	0.0
			MAX_IRV_EXP2	0.01
			T_IRV_EXP2	0.004

VEVERKA			A_VEVERKA	0.997
			MIN_A_VEVERKA	0.0
			MAX_A_VEVERKA	1.5
			T_A_VEVERKA	0.2
			B_VEVERKA	0.6
			MIN_B_VEVERKA	-3.0
			MAX_B_VEVERKA	3.0
			T_B_VEVERKA	0.05
			C_VEVERKA	0.003
			MIN_C_VEVERKA	-3.0
			MAX_C_VEVERKA	3.0
			T_C_VEVERKA	0.05
			D_VEVERKA	0.14
			MIN_D_VEVERKA	-2.0
			MAX_D_VEVERKA	2.0
			T_D_VEVERKA	0.02


BURATTI1		ALBEDO		0.5
			MIN_ALBEDO	0.0
			MAX_ALBEDO	1.0
			T_ALBEDO	0.3
			B_VEVERKA	0.6
			MIN_B_VEVERKA	-3.0
			MAX_B_VEVERKA	3.0
			T_B_VEVERKA	0.05
			E_BURATTI	0.14
			MIN_E_BURATTI	0.0
			MAX_E_BURATTI	1.0
			T_E_BURATTI	0.3

BURATTI2		ALBEDO		0.5
			MIN_ALBEDO	0.0
			MAX_ALBEDO	1.0
			T_ALBEDO	0.3
			B_VEVERKA	0.6
			MIN_B_VEVERKA	-3.0
			MAX_B_VEVERKA	3.0
			T_B_VEVERKA	0.05
			C_VEVERKA	0.003
			MIN_C_VEVERKA	-3.0
			MAX_C_VEVERKA	3.0
			T_C_VEVERKA	0.03
 			E_BURATTI	0.14
			MIN_E_BURATTI	0.0
			MAX_E_BURATTI	1.0
			T_E_BURATTI	0.3

BURATTI3		ALBEDO		0.5
			MIN_ALBEDO	0.0
			MAX_ALBEDO	1.0
			T_ALBEDO	0.3
			B_VEVERKA	0.6
			MIN_B_VEVERKA	-3.0
			MAX_B_VEVERKA	3.0
			T_B_VEVERKA	0.05
			C_VEVERKA	0.003
			MIN_C_VEVERKA	-3.0
			MAX_C_VEVERKA	3.0
			T_C_VEVERKA	0.03
			D_VEVERKA	0.14
			MIN_D_VEVERKA	-2.0
			MAX_D_VEVERKA	2.0
			T_D_VEVERKA	0.02
			E_BURATTI	0.14
			MIN_E_BURATTI	0.0
			MAX_E_BURATTI	1.0
			T_E_BURATTI	0.3

MOSHER			A_VEVERKA	0.5
			MIN_A_VEVERKA	0.0
			MAX_A_VEVERKA	1.5
			T_A_VEVERKA	0.2
			B_VEVERKA	0.6
			MIN_B_VEVERKA	-3.0
			MAX_B_VEVERKA	3.0
			T_B_VEVERKA	0.05
			C_VEVERKA	0.003
			MIN_C_VEVERKA	-3.0
			MAX_C_VEVERKA	3.0
			T_C_VEVERKA	0.03
			D_VEVERKA	0.14
			MIN_D_VEVERKA	-2.0
			MAX_D_VEVERKA	2.0
			T_D_VEVERKA	0.02
			MO_EXP1		0.5
			MIN_MO_EXP1	0.0
			MAX_MO_EXP1	1.0
			T_MO_EXP1	0.5
			MO_EXP2		0.1
			MIN_MO_EXP2	0.0
			MAX_MO_EXP2	1.0
			T_MO_EXP2	0.2

LUMME_BOWEL_HG1		W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			DEN_SOIL	0.8
			MIN_DEN_SOIL	0.0
			MAX_DEN_SOIL	1.0
			T_DEN_SOIL	0.2
			THETA		20
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			HG1_SOIL	-0.26
			MIN_HG1_SOIL	-1.0
			MAX_HG1_SOIL	1.0
			T_HG1_SOIL	0.2

HAPKE_81_LE2		W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			LE1_SOIL	0.3
			MIN_LE1_SOIL	-1.732
			MAX_LE1_SOIL	1.732
			T_LE1_SOIL	0.2
			LE2_SOIL	0.3
			MIN_LE2_SOIL	-2.0
			MAX_LE2_SOIL	2.0
			T_LE2_SOIL	0.2

HAPKE_81_COOK		W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			LE1_SOIL	0.3
			MIN_LE1_SOIL	-1.732
			MAX_LE1_SOIL	1.732
			T_LE1_SOIL	0.2
			LE2_SOIL	0.3
			MIN_LE2_SOIL	-2.0
			MAX_LE2_SOIL	2.0
			T_LE2_SOIL	0.2
			COOK		0.9
			MIN_COOK	0.0
			MAX_COOK	1.0
			T_COOK		0.5

HAPKE_86_HG1		W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			B_SHOE		2.0
			MIN_B_SHOE	0.0
			MAX_B_SHOE	10.0
			T_B_SHOE	1.0
			THETA		15.0
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			HG1_SOIL	-0.26
			MIN_HG1_SOIL	-1.0
			MAX_HG1_SOIL	1.0
			T_HG1_SOIL	0.2

HAPKE_86_HG2		W_SOIL		0.21
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.07
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			B_SHOE		2.0
			MIN_B_SHOE	0.0
			MAX_B_SHOE	10.0
			T_B_SHOE	1.0
			THETA		20.0
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			HG1_SOIL	-0.29
			MIN_HG1_SOIL	-1.0
			MAX_HG1_SOIL	1.0
			T_HG1_SOIL	0.2
			HG2_SOIL	0.39
			MIN_HG2_SOIL	-1.0
			MAX_HG2_SOIL	1.0
			T_HG2_SOIL	0.2
			HG_ASY_SOIL	1.0
			MIN_HG_ASY_SOIL	-10.0
			MAX_HG_ASY_SOIL	10.0
			T_HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.21
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			B_SHOE		2.0
			MIN_B_SHOE	0.0
			MAX_B_SHOE	10.0
			T_B_SHOE	1.0
			THETA		20.0
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			LE1_SOIL	0.3
			MIN_LE1_SOIL	-1.732
			MAX_LE1_SOIL	1.732
			T_LE1_SOIL	0.2
			LE2_SOIL	0.3
			MIN_LE2_SOIL	-2.0
			MAX_LE2_SOIL	2.0
			T_LE2_SOIL	0.2

HAPKE_HG1_DOM		W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			B_SHOE		1.0
			MIN_B_SHOE	0.0
			MAX_B_SHOE	10.0
			T_B_SHOE	1.0
			THETA		20.0
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			MIN_H_CBOE	0.0
			MAX_H_CBOE	2.0
			T_H_CBOE	0.2
			B_CBOE		1.0
			MIN_B_CBOE	1.0
			MAX_B_CBOE	10.0
			T_B_CBOE	1.0

ATMO_CORR_REGNER	W_SOIL		0.3
			MIN_W_SOIL	0.0
			MAX_W_SOIL	1.0
			T_W_SOIL	0.2
			H_SHOE		0.06
			MIN_H_SHOE	0.0
			MAX_H_SHOE	2.0
			T_H_SHOE	0.02
			B_SHOE		2.0
			MIN_B_SHOE	0.0
			MAX_B_SHOE	10.0
			T_B_SHOE	1.0
			THETA		15.0
			MIN_THETA	0.0
			MAX_THETA	60.0
			T_THETA		6.0
			HG1_SOIL	-0.26
			MIN_HG1_SOIL	-1.0
			MAX_HG1_SOIL	1.0
			T_HG1_SOIL	0.2
			W_ATM		0.78
			MIN_W_ATM	0.0
			MAX_W_ATM	1.0
			T_W_ATM		0.2
			TAU_ATM		0.05
			MIN_TAU_ATM	0.0
			MAX_TAU_ATM	10
			T_TAU_ATM	0.1
			HG1_ATM		0.35
			MIN_HG1_ATM	0.0
			MAX_HG1_ATM	1.0
			T_HG1_ATM	0.2

3) PAR-FILE NAME

pho_save_par		SAVE_PAR		Name of the TEA-parameter file 
						containing all parameters 
						needed to running the program.
						default:
						name of the main program 
						with the extention ".par"


4) RUN  MAIN PROGRAM

pho_proc_done		-			runs the program


GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

	Name		Type			Description

	PHO_FUNC_type 	string			It containes the names of the 
						valid photometric functions (to 
						pass into the menu).

	pho_PROC_NAME 	string			Name of the main program
						(PHOTFIT2M) 



										
.end
$!-----------------------------------------------------------------------------
$ create photfit2m_general.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! PHOTFIT2M_GENERAL.PDF
!
! This is the PDF for the first menu point of the MDF file.
! In this PDF file general parameters are defined like the names of 
! input/output files.
!
!-----------------------------------------------------------------------------

	!**********************************************************************
	! The global variables PHO_FUNC_type (and $MENUS) will be used in this
	! procedure, so they have to be declared here.
	! The global PHO_FUNC_type (the desired photometric function) will be
	! used to change the third menu point of the MDF file (see above)
	! according to the input to PHO_FUNC in this PDF.
	! The global $MENUS keeps the active stack of Menu Definition File
	! names. $MENUS(1) is the root menu name, $MENUS(2) the menu selected
	! from the root menu, and so on. The current menu stack can be
	! displayed with "DISPLAY $MENUS". Since there is only one MDF in
	! this demo, it doesn't make too much sense to have that global
	! included here. It is just referenced for completeness.
	!**********************************************************************

	refgbl PHO_FUNC_type 
	refgbl $menus

	parm MAIN_PROC_NAME string

	procedure name=photfit2m_general_sub help=*

		! dummy inputs :

		parm inp	type=(string,72) count=(0:20) default=--

		! photometric functions :

		parm PHO_FUNC type=(string,32) count=1 	+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1 	+
				) 	default="&PHO_FUNC_type"


		PARM CLASS_ID TYPE=INTEGER COUNT=0:1 valid=(0:32767) DEFAULT=--

  		PARM NORM    TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=500
  		PARM RERUN   TYPE=INTEGER COUNT=0:1 DEFAULT=1
  		PARM MAXITER TYPE=INTEGER COUNT=0:1 DEFAULT=--
  		PARM NUMTEN  TYPE=INTEGER COUNT=0:1 DEFAULT=--
  		PARM PERCENT TYPE=REAL COUNT=1 VALID=(1.:100.) DEFAULT=90.
  		PARM TOLERANC TYPE=REAL COUNT=1 VALID=(0.:1.) DEFAULT=.02

!  		PARM PLOT     TYPE=KEYWORD DEFAULT=PLOT	+ 
!			      VALID=(PLOT, NOPLOT) 
  		PARM PRINT    TYPE=KEYWORD DEFAULT=PRINT +
			      VALID=(PRINT, NOPRINT) 
!  		PARM DISP TYPE=KEYWORD VALID=(DISP,NODISP,WAITNOMORE) +
!    				DEFAULT=NODISP
  		PARM METROP  TYPE=INTEGER COUNT=1 VALID=(0:10000) DEFAULT=0



	body

	!*******************************************************************
	! "pho_global.pdf" is being executed in the following line. 
	! In this PDF, two globals are defined (PHO_FUNC_type, pho_PROC_NAME)
	!*******************************************************************
 
	pho_global PHO_FUNC_type="&PHO_FUNC"

	end-proc
body

	if (_tutor=1)

	   restore-parm common_proc_name.par

 	   tutor photfit2m_general_sub 				+
			|restore=&"MAIN_PROC_NAME"_general.par 	+
		 	 save=&"MAIN_PROC_NAME"_general.par|
	else
	   write " ************************************************"
	   write " "
	   write " This program works only when run from tutor mode"
           write " of other programs."
	   write " "
	   write " ************************************************"
	end-if

end-proc

.TITLE
VICAR sub-menu &"MAIN_PROC_NAME"_GENERAL

.HELP
PURPOSE:
This menu point is dedicated to input general parameters for the program such 
as the names of input photometric catalog IBIS file, the type of 
photometric funtion, the fit conditions and the kind of outputs.

NOTE : The parameter PHO_FUNC is in need to select the photometric function :  
When returning to the highest level of the menu you will see that the second 
selection point has been changed according to your input of PHO_FUNC in this 
menu.


.PAGE
Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.LEVEL1

.VARIABLE inp
  Input IBIS files, containing 
  the points generated by PHOTOM

.VARI PHO_FUNC
Photometric function type

.VARIABLE CLASS_ID
Class-id

.VARIABLE NORM
Causes subroutine Metropolis 
to renormalize itself.

.VARIABLE RERUN
Number of rerun of metropolis

.VARIABLE MAXITER
Specifies the total number of 
successful iterations.

.VARIABLE NUMTEN
Specifies the number of successful 
iterations which must be 
accumulated before the width of 
the solution generating  probability
function drops by a factor of ten.

.VARIABLE PERCENT
The minimum acceptable # of points
with residuals below tolerance.

.VARIABLE TOLERANC
The I/F residual tolerance.

!.VARIABLE PLOT
!Keyword for plotting the residuals.
!NOPLOT deactivates plotting.

.VARIABLE PRINT
Keyword for printing 
of the IBIS input files.
NOPRINT deactivates printing.

!.VARIABLE DISP
!Disp on image display?

.VARIABLE METROP
Causes subroutine Metropolis to 
list the iteration progress.

.LEVEL2

.VARI inp
File names of the input input IBIS photometric catalog files.
There are 18 columns in this file. 
All are not used exept for columns # 11, 12, 13, and 16. 
These columns contain :
	column # 11 = incidence angle (degrees),
	column # 12 = emission angle (degrees),
	column # 13 = phase angle (degrees)
	column # 16 = I/F reflectance values as computed from the photometric 
		      function.
	column # 18 = Class-ID

.VARI PHO_FUNC
Photometric function :

This parameter selects the menu point for input the photometry task:
   1. to run the program without using a photometric function, you have 
      to select "NONE"'
   2. to read in the photometric function and its associated parameters
      from a photometric parameter file, you have to select "PAR_FILE" and
   3. to put in the parameter by yourself from the tutor mode, 
      you have to select the desired photometric function.

When returning to the highest level of the menu (i.e. the MDF-file) you will
see that the third selection point has been changed according to your input of
PHO_FUNC in this menu.

.VARIABLE CLASS_ID
The class_id nummerates the photometric functions. For using different fotometric functions or parameter sets.

.VARIABLE NORM
Causes subroutine Metropolis to renormalize itself by recomputing the
Boltzmann coefficient. NORM=n causes renormalization each n successful
iterations. 

.VARIABLE RERUN
Number of rerun of metropolis.

.VARIABLE MAXITER
Specifies the number of successful iterations which Metropolis will
perform before ceasing in it's hunt for the coefficient values.
Usefully is for MAXITER is:
for MINNAERT 5000
for VEVERKA  20000
for HAPKE_* 20000
for HAPKE_* 20000 

.VARIABLE NUMTEN
Specifies the number of successful iterations which must be 
accumulated before the width of the solution generating  probability
function drops by a factor of ten. If for example MAXITER/NUMTEN
is 4.0 then the initial range specified by the MTEMP or HTEMP...
parameter (the starting temperature) is reduced by 4.0 orders
of magnitude (10000:1) by the time the iteration process has
ceased. 
Default for NUMTEN is: MAXITER/4

.VARIABLE PERCENT
The minimum acceptable # of points
with residuals below tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

.VARIABLE TOLERANC
The I/F residual tolerance.
The percent and tolerance keywords permit a solution that is found to
consist of a subset of all of the data points. If there are more than
percent of the points with I/F residuals below tolerance then the
remainder of the points can be ignored if they exceed tolerance.
If there are fewer than percent points with residuals below
tolerance then all of the points will be considered.

!.VARIABLE PLOT
!Keyword for plotting the residuals of the 
!fits on the input tiepoints.
!The default is to plot the residuals.
!NOPLOT deactivates printing of residuals.
!See PRINTRON to select device.

.VARIABLE PRINT
Keyword for printing of the IBIS input files.
NOPRINT deactivates printing of IBIS input file.

!.VARIABLE DISP
!Keyword to plot on image display.
!DISP       : Plot display.
!NODISP     : Without generted graphs.
!WAITNOMORE : All graph images will be save in the PostScript image file.

.VARIABLE METROP
Causes subroutine Metropolis to list the iteration progress as it
converges upon the solution. METROP=n causes a printout each
n successful iterations. 


.end
$!-----------------------------------------------------------------------------
$ create photfit2_lambert.pdf
procedure option=selftutor help=*

        parm ALBEDOt		real count=0:1 			def=0.5
        parm MIN_ALBEDOt	real count=0:1 			def=0.0
        parm MAX_ALBEDOt	real count=0:1 			def=1.0
        parm T_ALBEDOt		real count=0:1 			def=0.3
        parm ALBEDO		real count=0:1 valid=(0:1)     	def=--
        parm MIN_ALBEDO		real count=0:1 valid=(0:1)     	def=--
        parm MAX_ALBEDO		real count=0:1 valid=(0:1)     	def=--
        parm T_ALBEDO		real count=0:1 valid=(0:1)     	def=--

	PARMSET name=photfit2_lambert_sub help=*

             parm ALBEDO	real count=0:1 valid=(0:1)     	def=--
             parm MIN_ALBEDO	real count=0:1 valid=(0:1)     	def=--
             parm MAX_ALBEDO	real count=0:1 valid=(0:1)     	def=--
             parm T_ALBEDO	real count=0:1 valid=(0:1)     	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_lambert.par

	  if ($count(ALBEDO)=0)
	 						let ALBEDOt=0.5
	  else
		let ALBEDOt=&ALBEDO
	  end-if

	  if ($count(MIN_ALBEDO)=0)
	 						let MIN_ALBEDOt=0.0
	  else
		let MIN_ALBEDOt=&MIN_ALBEDO
	  end-if

	  if ($count(MAX_ALBEDO)=0)
	 						let MAX_ALBEDOt=1.0
	  else
		let MAX_ALBEDOt=&MAX_ALBEDO
	  end-if

	  if ($count(T_ALBEDO)=0)
	 						let T_ALBEDOt=0.3
	  else
		let T_ALBEDOt=&T_ALBEDO
	  end-if


	  tutor photfit2_lambert_sub 				+
		|restore=pho_lambert.par, save=pho_lambert.par| +
			ALBEDO=&ALBEDOt				+
			T_ALBEDO=&T_ALBEDOt			+
			MIN_ALBEDO=&MIN_ALBEDOt			+
			MAX_ALBEDO=&MAX_ALBEDOt

	  return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the Lambert function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_LAMBERT' sub-menu 

.help

PURPOSE:

This is the sub-menu associated with the Lambert photometric
function. This function needs just one input parameter (ALBEDO) and its absolute  limits.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page

MATHEMATICAL BACKGROUND :

Lambert's law does provide a reasonably good descrition of the reflectance of 
high-albedo surfaces, like snow, but the approximation is poor for dark 
surfaces.

The Lambert's law is based on the empirical observation that the brightnessses 
of many surfaces are nearly independent of the emmission angle and azimuthal 
angle and on the fact that the brightness of any surface must be proportional 
to cos(incidence angle).

bidirectional reflectance [1/str] :

r(i,e,g)=A*cos(i)


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.var ALBEDO
Surface albedo

.var MIN_ALBEDO
Minimum of surface albedo

.var MAX_ALBEDO
Maximum of surface albedo

.var T_ALBEDO
Temperatur of Surface albedo

.level2

.var ALBEDO
This parameter gives the albedo of the surface. 

.var MIN_ALBEDO
This parameter gives the absolut lower limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_ALBEDO
This parameter gives the absolut upper limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_ALBEDO
This parameter gives temperatur for the albedo of the surface. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.end

$!-----------------------------------------------------------------------------
$ create photfit2_minnaert.pdf
procedure option=selftutor help=*

    	parm ALBEDOt		real count=0:1 			def=0.5 
        parm MIN_ALBEDOt	real count=0:1 			def=0.0
        parm MAX_ALBEDOt	real count=0:1 			def=1.0
        parm T_ALBEDOt		real count=0:1 			def=0.3
 	parm EXPONENTt		real count=0:1 			def=0.6
 	parm MIN_EXPONENTt	real count=0:1 			def=0.0
 	parm MAX_EXPONENTt	real count=0:1 			def=1.0
 	parm T_EXPONENTt	real count=0:1 			def=0.3

    	parm ALBEDO	 	real count=0:1 			def=-- 
        parm MIN_ALBEDO		real count=0:1 valid=(0:1)     	def=--
        parm MAX_ALBEDO		real count=0:1 valid=(0:1)     	def=--
        parm T_ALBEDO		real count=0:1 valid=(0:1)     	def=--
 	parm EXPONENT	 	real count=0:1 			def=--
 	parm MIN_EXPONENT	real count=0:1 			def=--
 	parm MAX_EXPONENT	real count=0:1 			def=--
 	parm T_EXPONENT	 	real count=0:1 			def=--

	PARMSET name=photfit2_minnaert_sub help=*

    	   parm ALBEDO		real count=0:1 			def=-- 
           parm MIN_ALBEDO	real count=0:1 valid=(0:1)     	def=--
           parm MAX_ALBEDO	real count=0:1 valid=(0:1)     	def=--
           parm T_ALBEDO	real count=0:1 		     	def=--
 	   parm EXPONENT	real count=0:1 			def=--
 	   parm MIN_EXPONENT	real count=0:1 			def=--
 	   parm MAX_EXPONENT	real count=0:1 			def=--
 	   parm T_EXPONENT	real count=0:1 			def=--

	END-PROC

body

	if (_tutor=1)

	   restore-parm pho_minnaert.par


	   if ($count(ALBEDO)=0)
	 						let ALBEDOt=0.5
	   else
		let ALBEDOt=&ALBEDO
	   end-if


	  if ($count(MIN_ALBEDO)=0)
	 						let MIN_ALBEDOt=0.0
	  else
		let MIN_ALBEDOt=&MIN_ALBEDO
	  end-if

	  if ($count(MAX_ALBEDO)=0)
	 						let MAX_ALBEDOt=1.0
	  else
		let MAX_ALBEDOt=&MAX_ALBEDO
	  end-if

	  if ($count(T_ALBEDO)=0)
	 						let T_ALBEDOt=0.3
	  else
		let T_ALBEDOt=&T_ALBEDO
	  end-if


	   if ($count(EXPONENT)=0)
	 						let EXPONENTt=0.6
	   else
		let EXPONENTt=&EXPONENT
	   end-if

	   if ($count(MIN_EXPONENT)=0)
	 					let MIN_EXPONENTt=0.6
	   else
		let MIN_EXPONENTt=&MIN_EXPONENT
	   end-if

	   if ($count(MAX_EXPONENT)=0)
	 					let MAX_EXPONENTt=0.6
	   else
		let MAX_EXPONENTt=&MAX_EXPONENT
	   end-if

	   if ($count(T_EXPONENT)=0)
	 						let T_EXPONENTt=0.3
	   else
		let T_EXPONENTt=&T_EXPONENT
	   end-if


	   tutor photfit2_minnaert_sub 					+
		|restore=pho_minnaert.par, save=pho_minnaert.par|	+
			ALBEDO=&ALBEDOt					+
			MIN_ALBEDO=&MIN_ALBEDOt				+
			MAX_ALBEDO=&MAX_ALBEDOt				+
			T_ALBEDO=&T_ALBEDOt				+
			EXPONENT=&EXPONENTt				+
			T_EXPONENT=&T_EXPONENTt				+
			MIN_EXPONENT=&MIN_EXPONENTt			+
			MAX_EXPONENT=&MAX_EXPONENTt	
	
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the MINNAERT function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc
.title
'PHOTFIT2_MINNAERT' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to  
fit Minnaerts's photometric function. This function needs just two input 
parameters (Albedo, geometric exponent k) and there absolute limits.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :

It is found empirically that Minnaert's law approximately describes the 
variation of brightness of many surfaces over a limited range of angles.
But the general law breaks down completely at the limb of a planet.

Minnaert (1941) suggested generalizing Lambert's law so that the power emitted 
per unit solid angle per unit area of the surface be proportional 
(cos(i)*cos(e))**k [ Lambert's power is proportional to (cos(i)*cos(e))**1 ].

bidirectional reflectance [1/str] :

r(i,e,g)=ALBEDO*(cos(i)*cos(e))**EXPONENT/cos(e)

REFERENCE :
M. Minnaert, The reciprocity principle in Lunar photometry,
Astrophysical Journal, Vol. 93, No. 2, p. 403-410, 1941
.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.var ALBEDO
Surface albedo

.var MIN_ALBEDO
Minimum of surface albedo

.var MAX_ALBEDO
Maximum of surface albedo

.var T_ALBEDO
Temperatur of Surface albedo

.var EXPONENT
Minnaert exponent

.var MIN_EXPONENT
Maximum of Minnaert exponent

.var MAX_EXPONENT
Maximum of Minnaert exponent

.var T_EXPONENT
Temperatur of Minnaert exponent

.level2

.var ALBEDO
This parameter gives the albedo of the surface. 

.var MIN_ALBEDO
This parameter gives the absolut lower limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_ALBEDO
This parameter gives the absolut upper limit of the albedo of the surface. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_ALBEDO
This parameter gives temperatur for the albedo of the surface. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI MIN_EXPONENT
This parameter gives the absolut lower limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_EXPONENT
This parameter gives the absolut upper limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_EXPONENT
This parameter gives temperatur for the Exponent - the geometrical constant k
of the Minnaert photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    EXPONENT_NEW = T_EXPONENT * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_EXPONENT_NEW_* = T_EXPONENT_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_irvine.pdf
procedure option=selftutor help=*

        parm EXPONENTt		real count=0:1 			def=0.9
 	parm MIN_EXPONENTt	real count=0:1 			def=0.0
 	parm MAX_EXPONENTt	real count=0:1 			def=1.0
 	parm T_EXPONENTt	real count=0:1 			def=0.3
	parm IRV_EXP1t		real count=0:1 			def=0.118
	parm MIN_IRV_EXP1t	real count=0:1 			def=0.0
	parm MAX_IRV_EXP1t	real count=0:1 			def=1.0
	parm T_IRV_EXP1t	real count=0:1 			def=0.1
	parm IRV_EXP2t		real count=0:1 			def=0.0039
	parm MIN_IRV_EXP2t	real count=0:1 			def=0.0
	parm MAX_IRV_EXP2t	real count=0:1 			def=0.01
	parm T_IRV_EXP2t	real count=0:1 			def=0.004

        parm EXPONENT		real count=0:1 	def=--
 	parm MIN_EXPONENT	real count=0:1 	def=--
 	parm MAX_EXPONENT	real count=0:1 	def=--
        parm T_EXPONENT		real count=0:1 	def=--
	parm IRV_EXP1		real count=0:1  def=--
	parm MIN_IRV_EXP1	real count=0:1  def=--
	parm MAX_IRV_EXP1	real count=0:1  def=--
	parm T_IRV_EXP1		real count=0:1  def=--
	parm IRV_EXP2		real count=0:1  def=--
	parm MIN_IRV_EXP2	real count=0:1  def=--
	parm MAX_IRV_EXP2	real count=0:1  def=--
	parm T_IRV_EXP2		real count=0:1  def=--

	PARMSET name=photfit2_irvine_sub help=*

             parm EXPONENT	real count=0:1	def=--
 	     parm MIN_EXPONENT	real count=0:1 	def=--
 	     parm MAX_EXPONENT	real count=0:1 	def=--
             parm T_EXPONENT	real count=0:1	def=--
	     parm IRV_EXP1	real count=0:1	def=--
	     parm MIN_IRV_EXP1	real count=0:1	def=--
	     parm MAX_IRV_EXP1	real count=0:1	def=--
	     parm T_IRV_EXP1	real count=0:1	def=--
	     parm IRV_EXP2	real count=0:1  def=--
	     parm MIN_IRV_EXP2	real count=0:1  def=--
	     parm MAX_IRV_EXP2	real count=0:1  def=--
	     parm T_IRV_EXP2	real count=0:1  def=--

	end-proc

body

	if (_tutor=1)

	  restore-parm pho_irvine.par

	  if ($count(EXPONENT)=0)
	 					let EXPONENTt=0.9
	  else
	 	let EXPONENTt=&EXPONENT
	  end-if

	  if ($count(MIN_EXPONENT)=0)
	 					let MIN_EXPONENTt=0.6
	  else
		let MIN_EXPONENTt=&MIN_EXPONENT
	  end-if

	  if ($count(MAX_EXPONENT)=0)
	 					let MAX_EXPONENTt=0.6
	  else
		let MAX_EXPONENTt=&MAX_EXPONENT
	  end-if

	  if ($count(T_EXPONENT)=0)
	 						let T_EXPONENTt=0.3
	  else
		let T_EXPONENTt=&T_EXPONENT
	  end-if


	  if ($count(IRV_EXP1)=0)
	 					let IRV_EXP1t=0.118
	  else
	 	let IRV_EXP1t=&IRV_EXP1
	  end-if

	  if ($count(MIN_IRV_EXP1)=0)
	 					let MIN_IRV_EXP1t=0.0
	  else
	 	let MIN_IRV_EXP1t=&MIN_IRV_EXP1
	  end-if

	  if ($count(MAX_IRV_EXP1)=0)
	 					let MAX_IRV_EXP1t=1.0
	  else
	 	let MAX_IRV_EXP1t=&MAX_IRV_EXP1
	  end-if

	  if ($count(T_IRV_EXP1)=0)
	 					let T_IRV_EXP1t=0.1
	  else
	 	let T_IRV_EXP1t=&T_IRV_EXP1
	  end-if


	  if ($count(IRV_EXP2)=0)
	 					let IRV_EXP2t=0.0039
	  else
	 	let IRV_EXP2t=&IRV_EXP2
	  end-if

	  if ($count(MIN_IRV_EXP2)=0)
	 					let MIN_IRV_EXP2t=0.0
	  else
	 	let MIN_IRV_EXP2t=&MIN_IRV_EXP2
	  end-if

	  if ($count(MAX_IRV_EXP2)=0)
	 					let MAX_IRV_EXP2t=1.0
	  else
	 	let MAX_IRV_EXP2t=&MAX_IRV_EXP2
	  end-if

	  if ($count(T_IRV_EXP2)=0)
	 					let T_IRV_EXP2t=0.004
	  else
	 	let T_IRV_EXP2t=&T_IRV_EXP2
	  end-if


	  tutor photfit2_irvine_sub 				+
		|restore=pho_irvine.par, save=pho_irvine.par|	+
			EXPONENT=&EXPONENTt			+
			MIN_EXPONENT=&MIN_EXPONENTt		+
			MAX_EXPONENT=&MAX_EXPONENTt		+
			T_EXPONENT=&T_EXPONENTt			+
			IRV_EXP1=&IRV_EXP1t			+
			MIN_IRV_EXP1=&MIN_IRV_EXP1t		+
			MAX_IRV_EXP1=&MAX_IRV_EXP1t		+
			T_IRV_EXP1=&T_IRV_EXP1t			+
			IRV_EXP2=&IRV_EXP2t			+
			MIN_IRV_EXP2=&MIN_IRV_EXP2t		+
			MAX_IRV_EXP2=&MAX_IRV_EXP2t		+
			T_IRV_EXP2=&T_IRV_EXP2t			

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the Irvine function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_IRVINE' sub-menu 

.help
PURPOSE:

This is the sub-menu associated with Irvine's photometric
function. This function needs three input parameters and there absolute limits :
 EXPONENT,     IRV_EXP1,     IRV_EXP2.
 MIN_EXPONENT, MIN_IRV_EXP1, MIN_IRV_EXP2.
 MAX_EXPONENT, MAX_IRV_EXP1, MAX_IRV_EXP2.
 T_EXPONENT,   T_IRV_EXP1,   T_IRV_EXP2.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page



MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g)=( cos(i)*cos(e) )**EXPONENT) / cos(e) * (1-exp(-cos(i)*IRV_EXP1))/(1-exp(-cos(e)*IRV_EXP2))


REFERENCES :
old VICAR Photometry programs

.page
PROGRAMMER:

.page
Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.vari EXPONENT
Minnaert exponent

.vari MIN_EXPONENT
Minimum of Minnaert exponent

.vari MAX_EXPONENT
Maximum of Minnaert exponent

.vari T_EXPONENT
Temperatur of Minnaert exponent

.vari IRV_EXP1
Irvine's first exponent

.vari MIN_IRV_EXP1
Minimum of Irvine's first exponent

.vari MAX_IRV_EXP1
Maximum of Irvine's first exponent

.vari T_IRV_EXP1
Temperatur of Irvine's first exponent

.vari IRV_EXP2
Irvine's second exponent

.vari MIN_IRV_EXP2
Minimum of Irvine's second exponent

.vari MAX_IRV_EXP2
Maximum of Irvine's second exponent

.vari T_IRV_EXP2
Temperatur of Irvine's second exponent


.level2

.vari EXPONENT
Exponent - the geometrical constant k of the Minnaert part of the photometric 
function.

.VARI MIN_EXPONENT
This parameter gives the absolut lower limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_EXPONENT
This parameter gives the absolut upper limit of the Minnaert exponent - the 
geometrical constant k of the Minnaert photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_EXPONENT
This parameter gives temperatur for the Exponent - the geometrical constant k
of the Minnaert photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    EXPONENT_NEW = T_EXPONENT * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_EXPONENT_NEW_* = T_EXPONENT_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP1
Irvine's first exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP1
This parameter gives the absolut lower limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP1
This parameter gives the absolut upper limit of the Irvine's first exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP1
This parameter gives temperatur for the Irvine's first exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
   IRV_EXP1_NEW = T_IRV_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP1_NEW_* = T_IRV_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.vari IRV_EXP2
Irvine's second exponent - parameter of the Irvine photometric function.

.vari MIN_IRV_EXP2
This parameter gives the absolut lower limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari MAX_IRV_EXP2
This parameter gives the absolut upper limit of the Irvine's second exponent - 
parameter of the Irvine photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.vari T_IRV_EXP2
This parameter gives temperatur for the Irvine's second exponent - parameter 
of the Irvine photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    IRV_EXP2_NEW = T_IRV_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_IRV_EXP2_NEW_* = T_IRV_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.end

$!-----------------------------------------------------------------------------
$ create photfit2_veverka.pdf
procedure option=selftutor help=*

	parm A_VEVERKAt	 			real count=0:1 def=0.997
	parm MIN_A_VEVERKAt	 		real count=0:1 def=0.0
	parm MAX_A_VEVERKAt	 		real count=0:1 def=1.5
	parm T_A_VEVERKAt	 		real count=0:1 def=0.2
	parm B_VEVERKAt	 			real count=0:1 def=0.6 
	parm MIN_B_VEVERKAt	 		real count=0:1 def=-3.0 
	parm MAX_B_VEVERKAt	 		real count=0:1 def=3.0
	parm T_B_VEVERKAt	 		real count=0:1 def=0.05 
	parm C_VEVERKAt				real count=0:1 def=0.003 
	parm MIN_C_VEVERKAt			real count=0:1 def=-3.0 
	parm MAX_C_VEVERKAt			real count=0:1 def=3.0
	parm T_C_VEVERKAt			real count=0:1 def=0.05 
	parm D_VEVERKAt	 			real count=0:1 def=0.14 
	parm MIN_D_VEVERKAt	 		real count=0:1 def=-2.0 
	parm MAX_D_VEVERKAt	 		real count=0:1 def=2.0
	parm T_D_VEVERKAt	 		real count=0:1 def=0.02 

	parm A_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_A_VEVERKA	real count=0:1 	def=-- 
	parm MAX_A_VEVERKA	real count=0:1 	def=-- 
	parm T_A_VEVERKA	real count=0:1 	def=-- 
	parm B_VEVERKA	 	real count=0:1 	def=--
	parm MIN_B_VEVERKA	real count=0:1 	def=--
	parm MAX_B_VEVERKA	real count=0:1 	def=--
	parm T_B_VEVERKA	real count=0:1 	def=--
	parm C_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_C_VEVERKA	real count=0:1 	def=-- 
	parm MAX_C_VEVERKA	real count=0:1 	def=-- 
	parm T_C_VEVERKA	real count=0:1 	def=-- 
	parm D_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_D_VEVERKA	real count=0:1 	def=-- 
	parm MAX_D_VEVERKA	real count=0:1 	def=-- 
	parm T_D_VEVERKA	real count=0:1 	def=-- 

	procedure name=photfit2_veverka_sub help=*

		parm A_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_A_VEVERKA	real count=0:1 	def=-- 
		parm MAX_A_VEVERKA	real count=0:1 	def=-- 
		parm T_A_VEVERKA	real count=0:1 	def=-- 
		parm B_VEVERKA	 	real count=0:1 	def=--
		parm MIN_B_VEVERKA	real count=0:1 	def=--
		parm MAX_B_VEVERKA	real count=0:1 	def=--
		parm T_B_VEVERKA	real count=0:1 	def=--
		parm C_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_C_VEVERKA	real count=0:1 	def=-- 
		parm MAX_C_VEVERKA	real count=0:1 	def=-- 
		parm T_C_VEVERKA	real count=0:1 	def=-- 
		parm D_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_D_VEVERKA	real count=0:1 	def=-- 
		parm MAX_D_VEVERKA	real count=0:1 	def=-- 
		parm T_D_VEVERKA	real count=0:1 	def=-- 

	body
	end-proc

body

	if (_tutor=1)

	  restore-parm pho_veverka.par

	  if ($count(A_VEVERKA)=0)
	 					let A_VEVERKAt=0.997
	  else 
		let A_VEVERKAt=&A_VEVERKA
	  end-if

	  if ($count(MIN_A_VEVERKA)=0)
	 					let MIN_A_VEVERKAt=0.0
	  else 
		let MIN_A_VEVERKAt=&MIN_A_VEVERKA
	  end-if

	  if ($count(MAX_A_VEVERKA)=0)
	 					let MAX_A_VEVERKAt=1.5
	  else 
		let MAX_A_VEVERKAt=&MAX_A_VEVERKA
	  end-if

	  if ($count(T_A_VEVERKA)=0)
	 					let T_A_VEVERKAt=0.2
	  else 
		let T_A_VEVERKAt=&T_A_VEVERKA
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKAt=0.6
	  else
		let B_VEVERKAt=&B_VEVERKA
	  end-if

	  if ($count(MIN_B_VEVERKA)=0)
	 					let MIN_B_VEVERKAt=-3.0
	  else
		let MIN_B_VEVERKAt=&MIN_B_VEVERKA
	  end-if

	  if ($count(MAX_B_VEVERKA)=0)
	 					let MAX_B_VEVERKAt=3.0
	  else
		let MAX_B_VEVERKAt=&MAX_B_VEVERKA
	  end-if

	  if ($count(T_B_VEVERKA)=0)
	 					let T_B_VEVERKAt=0.05
	  else
		let T_B_VEVERKAt=&T_B_VEVERKA
	  end-if


	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKAt=0.003
	  else
		let C_VEVERKAt=&C_VEVERKA
	  end-if

	  if ($count(MIN_C_VEVERKA)=0)
	 					let MIN_C_VEVERKAt=-3.0
	  else
		let MIN_C_VEVERKAt=&MIN_C_VEVERKA
	  end-if

	  if ($count(MAX_C_VEVERKA)=0)
	 					let MAX_C_VEVERKAt=3.0
	  else
		let MAX_C_VEVERKAt=&MAX_C_VEVERKA
	  end-if

	  if ($count(T_C_VEVERKA)=0)
	 					let T_C_VEVERKAt=0.05
	  else
		let T_C_VEVERKAt=&T_C_VEVERKA
	  end-if


	  if ($count(D_VEVERKA)=0)
						let D_VEVERKAt=0.14
	  else
		let D_VEVERKAt=&D_VEVERKA
	  end-if

	  if ($count(MIN_D_VEVERKA)=0)
						let MIN_D_VEVERKAt=-2.0
	  else
		let MIN_D_VEVERKAt=&MIN_D_VEVERKA
	  end-if

	  if ($count(MAX_D_VEVERKA)=0)
						let MAX_D_VEVERKAt=2.0
	  else
		let MAX_D_VEVERKAt=&MAX_D_VEVERKA
	  end-if

	  if ($count(T_D_VEVERKA)=0)
						let T_D_VEVERKAt=0.02
	  else
		let T_D_VEVERKAt=&T_D_VEVERKA
	  end-if



	   tutor photfit2_veverka_sub 					+
		|restore=pho_veverka.par, save=pho_veverka.par|		+
			A_VEVERKA=&A_VEVERKAt				+
			MIN_A_VEVERKA=&MIN_A_VEVERKAt			+
			MAX_A_VEVERKA=&MAX_A_VEVERKAt			+
			T_A_VEVERKA=&T_A_VEVERKAt			+
			B_VEVERKA=&B_VEVERKAt				+
			MIN_B_VEVERKA=&MIN_B_VEVERKAt			+
			MAX_B_VEVERKA=&MAX_B_VEVERKAt			+
			T_B_VEVERKA=&T_B_VEVERKAt			+
			C_VEVERKA=&C_VEVERKAt				+
			MIN_C_VEVERKA=&MIN_C_VEVERKAt			+
			MAX_C_VEVERKA=&MAX_C_VEVERKAt			+
			T_C_VEVERKA=&T_C_VEVERKAt			+
			D_VEVERKA=&D_VEVERKAt				+
			MIN_D_VEVERKA=&MIN_D_VEVERKAt			+
			MAX_D_VEVERKA=&MAX_D_VEVERKAt			+
			T_D_VEVERKA=&T_D_VEVERKAt


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the VEVERKA function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_VEVERKA' sub-menu

.help

PURPOSE:

In this PDF, the user is asked for the only parameters and there absolute 
limits needed to fit Veverka's photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :

Exept close to zero phase, this expression is a good description of light 
scattered by low-albedo bodies of the solar system, such as the Moon and 
Mercury, for which only light that has been scattered once contributes 
significantly to the brightnes.

bidirectional reflectance [1/str] :

r(i,e,g) = ( cos(i) / (cos(e)+cos(e)) )  
	 * (A_VEVERKA + B_VEVERKA * phase + C_VEVERKA * exp(-DVEVERKA * phase))

usually :
C_VEVERKA=1-A_VEVERKA


.page
REFERENCE :
Joseph Veverka, J. Goguen, S. Young, J. Elliont, Scattering of light from 
particulate surfaces. 
I. A laboratory assessment of multiple-scattering effects.
Icarus, Vol. 34, p. 406-414


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI A_VEVERKA 
Veverka parameter

.VARI MIN_A_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_A_VEVERKA 
Maximum of Veverka parameter

.VARI T_A_VEVERKA 
Temperatur of Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI MIN_B_VEVERKA
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA
Maximum of Veverka parameter

.VARI T_B_VEVERKA
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MIN_D_VEVERKA
Minimum of Veverka parameter

.VARI MAX_D_VEVERKA
Maximum of Veverka parameter

.VARI T_D_VEVERKA
Temperatur of Veverka parameter

.level2

.VARI A_VEVERKA 
Parameter of the Veverka photometric function.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_A_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_A_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_A_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    A_VEVERKA_NEW = T_A_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_A_VEVERKA_NEW_* = T_A_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA
Parameter of the Veverka photometric function.

.VARI MIN_B_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka photometric function.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_C_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI D_VEVERKA
Parameter of the Veverka photometric function.

.VARI MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_buratti1.pdf
procedure option=selftutor help=*

	parm ALBEDOt	 	 real count=0:1 		def=0.5 
	parm MIN_ALBEDOt	 real count=0:1 		def=0.0 
	parm MAX_ALBEDOt	 real count=0:1 		def=1.0 
	parm T_ALBEDOt	 	 real count=0:1 		def=0.3 
	parm B_VEVERKAt	 	 real count=0:1 		def=0.6
	parm MIN_B_VEVERKAt	 real count=0:1 		def=-3.0
	parm MAX_B_VEVERKAt	 real count=0:1 		def=3.0
	parm T_B_VEVERKAt	 real count=0:1 		def=0.05
	parm E_BURATTIt	 	 real count=0:1 		def=0.14
	parm MIN_E_BURATTIt	 real count=0:1 		def=0.0
	parm MAX_E_BURATTIt	 real count=0:1 		def=1.0
	parm T_E_BURATTIt	 real count=0:1 		def=0.3

	parm ALBEDO	 	real count=0:1 	def=-- 
	parm MIN_ALBEDO	 	real count=0:1 	def=-- 
	parm MAX_ALBEDO	 	real count=0:1 	def=-- 
	parm T_ALBEDO	 	real count=0:1 	def=-- 
	parm B_VEVERKA		real count=0:1 	def=--
	parm MIN_B_VEVERKA	real count=0:1  def=--
	parm MAX_B_VEVERKA	real count=0:1  def=--
	parm T_B_VEVERKA	real count=0:1 	def=--
	parm E_BURATTI	 	real count=0:1 	def=--
	parm MIN_E_BURATTI	real count=0:1 	def=--
	parm MAX_E_BURATTI	real count=0:1 	def=--
	parm T_E_BURATTI	real count=0:1 	def=--

	PARMSET name=photfit2_buratti1_sub help=*

		parm ALBEDO	 	real count=0:1 	def=-- 
		parm MIN_ALBEDO	 	real count=0:1 	def=-- 
		parm MAX_ALBEDO	 	real count=0:1 	def=-- 
		parm T_ALBEDO	 	real count=0:1 	def=-- 
		parm B_VEVERKA		real count=0:1 	def=--
		parm MIN_B_VEVERKA	real count=0:1  def=--
		parm MAX_B_VEVERKA	real count=0:1  def=--
		parm T_B_VEVERKA	real count=0:1 	def=--
		parm E_BURATTI	 	real count=0:1 	def=--
		parm MIN_E_BURATTI	real count=0:1 	def=--
		parm MAX_E_BURATTI	real count=0:1 	def=--
		parm T_E_BURATTI	real count=0:1 	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti1.par


	  if ($count(ALBEDO)=0)
	 					let ALBEDOt=0.5
	  else
	 	let ALBEDOt=&ALBEDO
	  end-if

	  if ($count(MIN_ALBEDO)=0)
	 					let MIN_ALBEDOt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_ALBEDO
	  end-if

	  if ($count(MAX_ALBEDO)=0)
	 					let MAX_ALBEDOt=1.0
	  else
	 	let MAX_ALBEDOt=&MAX_ALBEDO
	  end-if

	  if ($count(T_ALBEDO)=0)
	 					let T_ALBEDOt=0.3
	  else
	 	let T_ALBEDOt=&ALBEDO
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKAt=0.6
	  else
		let B_VEVERKAt=&B_VEVERKA
	  end-if

	  if ($count(MIN_B_VEVERKA)=0)
	 					let MIN_B_VEVERKAt=-3.0
	  else
		let MIN_B_VEVERKAt=&MIN_B_VEVERKA
	  end-if

	  if ($count(MAX_B_VEVERKA)=0)
	 					let MAX_B_VEVERKAt=3.0
	  else
		let MAX_B_VEVERKAt=&MAX_B_VEVERKA
	  end-if

	  if ($count(T_B_VEVERKA)=0)
	 					let T_B_VEVERKAt=0.05
	  else
		let T_B_VEVERKAt=&T_B_VEVERKA
	  end-if


	  if ($count(E_BURATTI)=0)
	 					let E_BURATTIt=0.14
	  else
	 	let E_BURATTIt=&E_BURATTI
	  end-if

	  if ($count(MIN_E_BURATTI)=0)
	 					let MIN_E_BURATTIt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_E_BURATTI
	  end-if

	  if ($count(MAX_E_BURATTI)=0)
	 					let MAX_E_BURATTIt=1.0
	  else
	 	let MAX_E_BURATTIt=&MAX_E_BURATTI
	  end-if

	  if ($count(T_E_BURATTI)=0)
	 					let T_E_BURATTIt=0.14
	  else
	 	let T_E_BURATTIt=&T_E_BURATTI
	  end-if



	   tutor photfit2_buratti1_sub      				+
		|restore=pho_buratti1.par, save=pho_buratti1.par|	+
		ALBEDO=&ALBEDOt	       		       			+
		MIN_ALBEDO=&MIN_ALBEDOt	       		       		+
		MAX_ALBEDO=&MAX_ALBEDOt	       		       		+
		T_ALBEDO=&T_ALBEDOt	       		       		+
		B_VEVERKA=&B_VEVERKAt      				+
		MIN_B_VEVERKA=&MIN_B_VEVERKAt      			+
		MAX_B_VEVERKA=&MAX_B_VEVERKAt      			+
		T_B_VEVERKA=&T_B_VEVERKAt      				+
		E_BURATTI=&E_BURATTIt					+
		MIN_E_BURATTI=&MIN_E_BURATTIt				+
		MAX_E_BURATTI=&MAX_E_BURATTIt				+
		T_E_BURATTI=&T_E_BURATTIt

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_BURATTI1' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the only parameters and there limmits needed 
to fit Buratti's photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.


.page
MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) * cos(phase))/PI)

BURATTI1 approximation :
phi = 1 + B * phase

REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI MIN_ALBEDO
Minimum of isotropic/unisotropic 
reflectance weight 

.VARI MAX_ALBEDO
Maximum of isotropic/unisotropic 
reflectance weight 

.VARI T_ALBEDO
Temperatur of isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA 
Veverka parameter

.VARI MIN_B_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA 
Maximum of Veverka parameter

.VARI T_B_VEVERKA 
Temperatur of Veverka parameter

.VARI E_BURATTI
Buratti's parameter

.VARI MIN_E_BURATTI
Minimum of Buratti's parameter

.VARI MAX_E_BURATTI
Maximum of Buratti's parameter

.VARI T_E_BURATTI
Temperatur of Buratti's parameter

.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI MIN_ALBEDO
This parameter gives the absolut lower limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_ALBEDO
This parameter gives the absolut upper limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_ALBEDO
This parameter gives temperatur for the weight of unisotropic and isotropic 
reflectance.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA 
Parameter of the Veverka and Mosher photometric functions.

.VARI MIN_B_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka and Mosher 
photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_buratti2.pdf
procedure option=selftutor help=*

	parm ALBEDOt	 	 real count=0:1 		def=0.5 
	parm MIN_ALBEDOt	 real count=0:1 		def=0.0 
	parm MAX_ALBEDOt	 real count=0:1 		def=1.0 
	parm T_ALBEDOt	 	 real count=0:1 		def=0.3 
	parm B_VEVERKAt	 	 real count=0:1 		def=0.6
	parm MIN_B_VEVERKAt	 real count=0:1 		def=-3.0
	parm MAX_B_VEVERKAt	 real count=0:1 		def=3.0
	parm T_B_VEVERKAt	 real count=0:1 		def=0.05
	parm C_VEVERKAt	 	 real count=0:1 		def=-0.003
 	parm MIN_C_VEVERKAt	 real count=0:1 		def=-3.0
	parm MAX_C_VEVERKAt	 real count=0:1 		def=3.0
	parm T_C_VEVERKAt	 real count=0:1 		def=0.05
	parm E_BURATTIt	 	 real count=0:1			def=0.14
	parm MIN_E_BURATTIt	 real count=0:1 		def=0.0
	parm MAX_E_BURATTIt	 real count=0:1 		def=1.0
	parm T_E_BURATTIt	 real count=0:1 		def=0.3

	parm ALBEDO	 	real count=0:1 	def=-- 
	parm MIN_ALBEDO	 	real count=0:1 	def=-- 
	parm MAX_ALBEDO	 	real count=0:1 	def=-- 
	parm T_ALBEDO	 	real count=0:1 	def=-- 
	parm B_VEVERKA		real count=0:1 	def=--
	parm MIN_B_VEVERKA	real count=0:1  def=--
	parm MAX_B_VEVERKA	real count=0:1  def=--
	parm T_B_VEVERKA	real count=0:1 	def=--
	parm C_VEVERKA	 	real count=0:1 	def=--
	parm MIN_C_VEVERKA	real count=0:1 	def=--
	parm MAX_C_VEVERKA	real count=0:1 	def=--
	parm T_C_VEVERKA	real count=0:1 	def=--
	parm E_BURATTI	 	real count=0:1 	def=--
	parm MIN_E_BURATTI	real count=0:1 	def=--
	parm MAX_E_BURATTI	real count=0:1 	def=--
	parm T_E_BURATTI	real count=0:1 	def=--

	PARMSET name=photfit2_buratti2_sub help=*

		parm ALBEDO	 	real count=0:1 	def=-- 
		parm MIN_ALBEDO	 	real count=0:1 	def=-- 
		parm MAX_ALBEDO	 	real count=0:1 	def=-- 
		parm T_ALBEDO	 	real count=0:1 	def=-- 
		parm B_VEVERKA		real count=0:1 	def=--
		parm MIN_B_VEVERKA	real count=0:1  def=--
		parm MAX_B_VEVERKA	real count=0:1  def=--
		parm T_B_VEVERKA	real count=0:1 	def=--
		parm C_VEVERKA	 	real count=0:1 	def=--
		parm MIN_C_VEVERKA	real count=0:1 	def=--
		parm MAX_C_VEVERKA	real count=0:1 	def=--
		parm T_C_VEVERKA	real count=0:1 	def=--
		parm E_BURATTI	 	real count=0:1 	def=--
		parm MIN_E_BURATTI	real count=0:1 	def=--
		parm MAX_E_BURATTI	real count=0:1 	def=--
		parm T_E_BURATTI	real count=0:1 	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti2.par



	  if ($count(ALBEDO)=0)
	 					let ALBEDOt=0.5
	  else
	 	let ALBEDOt=&ALBEDO
	  end-if

	  if ($count(MIN_ALBEDO)=0)
	 					let MIN_ALBEDOt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_ALBEDO
	  end-if

	  if ($count(MAX_ALBEDO)=0)
	 					let MAX_ALBEDOt=1.0
	  else
	 	let MAX_ALBEDOt=&MAX_ALBEDO
	  end-if

	  if ($count(T_ALBEDO)=0)
	 					let T_ALBEDOt=0.3
	  else
	 	let T_ALBEDOt=&ALBEDO
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKAt=0.6
	  else
		let B_VEVERKAt=&B_VEVERKA
	  end-if

	  if ($count(MIN_B_VEVERKA)=0)
	 					let MIN_B_VEVERKAt=-3.0
	  else
		let MIN_B_VEVERKAt=&MIN_B_VEVERKA
	  end-if

	  if ($count(MAX_B_VEVERKA)=0)
	 					let MAX_B_VEVERKAt=3.0
	  else
		let MAX_B_VEVERKAt=&MAX_B_VEVERKA
	  end-if

	  if ($count(T_B_VEVERKA)=0)
	 					let T_B_VEVERKAt=0.05
	  else
		let T_B_VEVERKAt=&T_B_VEVERKA
	  end-if


	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKAt=-0.003
	  else
	 	let C_VEVERKAt=&C_VEVERKA
	  end-if

	  if ($count(MIN_C_VEVERKA)=0)
	 					let MIN_C_VEVERKAt=-3.0
	  else
	 	let MIN_C_VEVERKAt=&MIN_C_VEVERKA
	  end-if

	  if ($count(MAX_C_VEVERKA)=0)
	 					let MAX_C_VEVERKAt=3.0
	  else
	 	let MAX_C_VEVERKAt=&MAX_C_VEVERKA
	  end-if

	  if ($count(T_C_VEVERKA)=0)
	 					let T_C_VEVERKAt=0.05
	  else
	 	let T_C_VEVERKAt=&T_C_VEVERKA
	  end-if


	  if ($count(E_BURATTI)=0)
	 					let E_BURATTIt=0.14
	  else
	 	let E_BURATTIt=&E_BURATTI
	  end-if

	  if ($count(MIN_E_BURATTI)=0)
	 					let MIN_E_BURATTIt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_E_BURATTI
	  end-if

	  if ($count(MAX_E_BURATTI)=0)
	 					let MAX_E_BURATTIt=1.0
	  else
	 	let MAX_E_BURATTIt=&MAX_E_BURATTI
	  end-if

	  if ($count(T_E_BURATTI)=0)
	 					let T_E_BURATTIt=0.14
	  else
	 	let T_E_BURATTIt=&T_E_BURATTI
	  end-if



	 	
	   tutor photfit2_buratti2_sub      				+
		|restore=pho_buratti2.par, save=pho_buratti2.par|	+
		ALBEDO=&ALBEDOt	       		       			+
		MIN_ALBEDO=&MIN_ALBEDOt	       		       		+
		MAX_ALBEDO=&MAX_ALBEDOt	       		       		+
		T_ALBEDO=&T_ALBEDOt	       		       		+
		B_VEVERKA=&B_VEVERKAt      				+
		MIN_B_VEVERKA=&MIN_B_VEVERKAt      			+
		MAX_B_VEVERKA=&MAX_B_VEVERKAt      			+
		T_B_VEVERKA=&T_B_VEVERKAt      				+
		C_VEVERKA=&C_VEVERKAt					+
		MIN_C_VEVERKA=&MIN_C_VEVERKAt				+
		MAX_C_VEVERKA=&MAX_C_VEVERKAt				+
		T_C_VEVERKA=&T_C_VEVERKAt				+
		E_BURATTI=&E_BURATTIt					+
		MIN_E_BURATTI=&MIN_E_BURATTIt				+
		MAX_E_BURATTI=&MAX_E_BURATTIt				+
		T_E_BURATTI=&T_E_BURATTIt

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_BURATTI2' sub-menu'

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit Buratti's photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) 
* cos(phase))/PI)

BURATTI2 approximation : 
phi = 1 + phase * (B + phase * C)

REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI MIN_ALBEDO
Minimum of isotropic/unisotropic 
reflectance weight 

.VARI MAX_ALBEDO
Maximum of isotropic/unisotropic 
reflectance weight 

.VARI T_ALBEDO
Temperatur of isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA 
Veverka parameter

.VARI MIN_B_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA 
Maximum of Veverka parameter

.VARI T_B_VEVERKA 
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI E_BURATTI
Buratti's parameter

.VARI MIN_E_BURATTI
Minimum of Buratti's parameter

.VARI MAX_E_BURATTI
Maximum of Buratti's parameter

.VARI T_E_BURATTI
Temperatur of Buratti's parameter

.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI MIN_ALBEDO
This parameter gives the absolut lower limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_ALBEDO
This parameter gives the absolut upper limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_ALBEDO
This parameter gives temperatur for the weight of unisotropic and isotropic 
reflectance.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA 
Parameter of the Veverka and Mosher photometric functions.

.VARI MIN_B_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka and Mosher 
photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI MIN_C_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka, 
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka, 
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the parameter of the 
Veverka, Mosher and Buratti photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_buratti3.pdf
procedure option=selftutor help=*

	parm ALBEDOt	 	 real count=0:1 	def=0.5 
	parm MIN_ALBEDOt	 real count=0:1 	def=0.0 
	parm MAX_ALBEDOt	 real count=0:1 	def=1.0 
	parm T_ALBEDOt	 	 real count=0:1 	def=0.3 
	parm B_VEVERKAt	 	 real count=0:1 	def=0.6
	parm MIN_B_VEVERKAt	 real count=0:1 	def=-3.0
	parm MAX_B_VEVERKAt	 real count=0:1 	def=3.0
	parm T_B_VEVERKAt	 real count=0:1 	def=0.05
	parm C_VEVERKAt	 	 real count=0:1 	def=-0.003
 	parm MIN_C_VEVERKAt	 real count=0:1 	def=-3.0
	parm MAX_C_VEVERKAt	 real count=0:1 	def=3.0
	parm T_C_VEVERKAt	 real count=0:1 	def=0.05
	parm D_VEVERKAt	 	 real count=0:1 	def=0.14
	parm MIN_D_VEVERKAt	 real count=0:1 	def=-2.0
	parm MAX_D_VEVERKAt	 real count=0:1 	def=2.0
	parm T_D_VEVERKAt	 real count=0:1 	def=0.02
	parm E_BURATTIt	 	 real count=0:1 	def=0.14
	parm MIN_E_BURATTIt	 real count=0:1 	def=0.0
	parm MAX_E_BURATTIt	 real count=0:1 	def=1.0
	parm T_E_BURATTIt	 real count=0:1 	def=0.3

	parm ALBEDO	 	real count=0:1 	def=-- 
	parm MIN_ALBEDO	 	real count=0:1 	def=-- 
	parm MAX_ALBEDO	 	real count=0:1 	def=-- 
	parm T_ALBEDO	 	real count=0:1 	def=-- 
	parm B_VEVERKA		real count=0:1 	def=--
	parm MIN_B_VEVERKA	real count=0:1  def=--
	parm MAX_B_VEVERKA	real count=0:1  def=--
	parm T_B_VEVERKA	real count=0:1 	def=--
	parm C_VEVERKA	 	real count=0:1 	def=--
	parm MIN_C_VEVERKA	real count=0:1 	def=--
	parm MAX_C_VEVERKA	real count=0:1 	def=--
	parm T_C_VEVERKA	real count=0:1 	def=--
	parm D_VEVERKA	 	real count=0:1 	def=--
	parm MIN_D_VEVERKA	real count=0:1 	def=--
	parm MAX_D_VEVERKA	real count=0:1 	def=--
	parm T_D_VEVERKA	real count=0:1 	def=--
	parm E_BURATTI		real count=0:1 	def=--
	parm MIN_E_BURATTI	real count=0:1 	def=--
	parm MAX_E_BURATTI	real count=0:1 	def=--
	parm T_E_BURATTI	real count=0:1 	def=--

	PARMSET name=photfit2_buratti3_sub help=*

		parm ALBEDO	 	real count=0:1 	def=-- 
		parm MIN_ALBEDO	 	real count=0:1 	def=-- 
		parm MAX_ALBEDO	 	real count=0:1 	def=-- 
		parm T_ALBEDO	 	real count=0:1 	def=-- 
		parm B_VEVERKA		real count=0:1 	def=--
		parm MIN_B_VEVERKA	real count=0:1  def=--
		parm MAX_B_VEVERKA	real count=0:1  def=--
		parm T_B_VEVERKA	real count=0:1 	def=--
		parm C_VEVERKA	 	real count=0:1 	def=--
		parm MIN_C_VEVERKA	real count=0:1 	def=--
		parm MAX_C_VEVERKA	real count=0:1 	def=--
		parm T_C_VEVERKA	real count=0:1 	def=--
		parm D_VEVERKA	 	real count=0:1 	def=--
		parm MIN_D_VEVERKA	real count=0:1 	def=--
		parm MAX_D_VEVERKA	real count=0:1 	def=--
		parm T_D_VEVERKA	real count=0:1 	def=--
		parm E_BURATTI		real count=0:1 	def=--
		parm MIN_E_BURATTI	real count=0:1 	def=--
		parm MAX_E_BURATTI	real count=0:1 	def=--
		parm T_E_BURATTI	real count=0:1 	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti3.par

	  if ($count(ALBEDO)=0)
	 					let ALBEDOt=0.5
	  else
	 	let ALBEDOt=&ALBEDO
	  end-if

	  if ($count(MIN_ALBEDO)=0)
	 					let MIN_ALBEDOt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_ALBEDO
	  end-if

	  if ($count(MAX_ALBEDO)=0)
	 					let MAX_ALBEDOt=1.0
	  else
	 	let MAX_ALBEDOt=&MAX_ALBEDO
	  end-if

	  if ($count(T_ALBEDO)=0)
	 					let T_ALBEDOt=0.3
	  else
	 	let T_ALBEDOt=&ALBEDO
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKAt=0.6
	  else
		let B_VEVERKAt=&B_VEVERKA
	  end-if

	  if ($count(MIN_B_VEVERKA)=0)
	 					let MIN_B_VEVERKAt=-3.0
	  else
		let MIN_B_VEVERKAt=&MIN_B_VEVERKA
	  end-if

	  if ($count(MAX_B_VEVERKA)=0)
	 					let MAX_B_VEVERKAt=3.0
	  else
		let MAX_B_VEVERKAt=&MAX_B_VEVERKA
	  end-if

	  if ($count(T_B_VEVERKA)=0)
	 					let T_B_VEVERKAt=0.05
	  else
		let T_B_VEVERKAt=&T_B_VEVERKA
	  end-if



	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKAt=-0.003
	  else
	 	let C_VEVERKAt=&C_VEVERKA
	  end-if

	  if ($count(MIN_C_VEVERKA)=0)
	 					let MIN_C_VEVERKAt=-3.0
	  else
	 	let MIN_C_VEVERKAt=&MIN_C_VEVERKA
	  end-if

	  if ($count(MAX_C_VEVERKA)=0)
	 					let MAX_C_VEVERKAt=3.0
	  else
	 	let MAX_C_VEVERKAt=&MAX_C_VEVERKA
	  end-if

	  if ($count(T_C_VEVERKA)=0)
	 					let T_C_VEVERKAt=0.05
	  else
	 	let T_C_VEVERKAt=&T_C_VEVERKA
	  end-if


	  if ($count(D_VEVERKA)=0)
	 					let D_VEVERKAt=0.14
	  else
	 	let D_VEVERKAt=&D_VEVERKA
	  end-if

	  if ($count(MIN_D_VEVERKA)=0)
	 					let MIN_D_VEVERKAt=2.0
	  else
	 	let MIN_D_VEVERKAt=&MIN_D_VEVERKA
	  end-if

	  if ($count(MAX_D_VEVERKA)=0)
	 					let MAX_D_VEVERKAt=2.0
	  else
	 	let MAX_D_VEVERKAt=&MAX_D_VEVERKA
	  end-if

	  if ($count(T_D_VEVERKA)=0)
	 					let T_D_VEVERKAt=0.02
	  else
	 	let T_D_VEVERKAt=&T_D_VEVERKA
	  end-if


	  if ($count(E_BURATTI)=0)
	 					let E_BURATTIt=0.14
	  else
	 	let E_BURATTIt=&E_BURATTI
	  end-if

	  if ($count(MIN_E_BURATTI)=0)
	 					let MIN_E_BURATTIt=0.0
	  else
	 	let MIN_ALBEDOt=&MIN_E_BURATTI
	  end-if

	  if ($count(MAX_E_BURATTI)=0)
	 					let MAX_E_BURATTIt=1.0
	  else
	 	let MAX_E_BURATTIt=&MAX_E_BURATTI
	  end-if

	  if ($count(T_E_BURATTI)=0)
	 					let T_E_BURATTIt=0.14
	  else
	 	let T_E_BURATTIt=&T_E_BURATTI
	  end-if



	   tutor photfit2_buratti3_sub 					+
		|restore=pho_buratti3.par, save=pho_buratti3.par|	+
		ALBEDO=&ALBEDOt	       		       			+
		MIN_ALBEDO=&MIN_ALBEDOt	       		       		+
		MAX_ALBEDO=&MAX_ALBEDOt	       		       		+
		T_ALBEDO=&T_ALBEDOt	       		       		+
		B_VEVERKA=&B_VEVERKAt      				+
		MIN_B_VEVERKA=&MIN_B_VEVERKAt      			+
		MAX_B_VEVERKA=&MAX_B_VEVERKAt      			+
		T_B_VEVERKA=&T_B_VEVERKAt      				+
		C_VEVERKA=&C_VEVERKAt					+
		MIN_C_VEVERKA=&MIN_C_VEVERKAt				+
		MAX_C_VEVERKA=&MAX_C_VEVERKAt				+
		T_C_VEVERKA=&T_C_VEVERKAt				+
		D_VEVERKA=&D_VEVERKAt					+
		MIN_D_VEVERKA=&MIN_D_VEVERKAt				+
		MAX_D_VEVERKA=&MAX_D_VEVERKAt				+
		T_D_VEVERKA=&T_D_VEVERKAt				+
		E_BURATTI=&E_BURATTIt					+
		MIN_E_BURATTI=&MIN_E_BURATTIt				+
		MAX_E_BURATTI=&MAX_E_BURATTIt				+
		T_E_BURATTI=&T_E_BURATTIt

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI3 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_BURATTI3' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit Buratti's photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) 
* cos(phase))/PI)

BURATTI3 approximation : 
phi=1.0 + params[1] * phase + params[2] * (exp(-params[3] * phase) - 1.0)


REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI MIN_ALBEDO
Minimum of isotropic/unisotropic 
reflectance weight 

.VARI MAX_ALBEDO
Maximum of isotropic/unisotropic 
reflectance weight 

.VARI T_ALBEDO
Temperatur of isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA 
Veverka parameter

.VARI MIN_B_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA 
Maximum of Veverka parameter

.VARI T_B_VEVERKA 
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MIN_D_VEVERKA
Minimum of Veverka parameter

.VARI MAX_D_VEVERKA
Maximum of Veverka parameter

.VARI T_D_VEVERKA
Temperatur of Veverka parameter

.VARI E_BURATTI
Buratti's parameter

.VARI MIN_E_BURATTI
Minimum of Buratti's parameter

.VARI MAX_E_BURATTI
Maximum of Buratti's parameter

.VARI T_E_BURATTI
Temperatur of Buratti's parameter

.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI MIN_ALBEDO
This parameter gives the absolut lower limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_ALBEDO
This parameter gives the absolut upper limit of the weight of unisotropic and 
isotropic reflectance.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_ALBEDO
This parameter gives temperatur for the weight of unisotropic and isotropic 
reflectance.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    ALBEDO_NEW = T_ALBEDO * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_ALBEDO_NEW_* = T_ALBEDO_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA 
Parameter of the Veverka and Mosher photometric functions.

.VARI MIN_B_VEVERKA 
This parameter gives the absolut lower limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka 
and Mosher photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka and Mosher 
photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI MIN_C_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka, 
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka, 
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the parameter of the 
Veverka, Mosher and Buratti photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka, 
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka,
Mosher and Buratti photometric functions.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka, Mosher 
and Buratti photometric functions.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MIN_E_BURATTI
This parameter gives the absolut lower limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_E_BURATTI
This parameter gives the absolut upper limit of the Buratti's parameter for 
modification of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_E_BURATTI
This parameter gives temperatur for the Buratti's parameter for modification of 
the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    E_BURATTI_NEW = T_E_BURATTI * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_E_BURATTI_NEW_* = T_E_BURATTI_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_mosher.pdf
procedure option=selftutor help=*

	parm A_VEVERKAt	 			real count=0:1 def=0.997
	parm MIN_A_VEVERKAt	 		real count=0:1 def=0.0
	parm MAX_A_VEVERKAt	 		real count=0:1 def=1.5
	parm T_A_VEVERKAt	 		real count=0:1 def=0.2
	parm B_VEVERKAt	 			real count=0:1 def=0.6 
	parm MIN_B_VEVERKAt	 		real count=0:1 def=-3.0 
	parm MAX_B_VEVERKAt	 		real count=0:1 def=3.0
	parm T_B_VEVERKAt	 		real count=0:1 def=0.05 
	parm C_VEVERKAt				real count=0:1 def=0.003 
	parm MIN_C_VEVERKAt			real count=0:1 def=-3.0 
	parm MAX_C_VEVERKAt			real count=0:1 def=3.0
	parm T_C_VEVERKAt			real count=0:1 def=0.05 
	parm D_VEVERKAt	 			real count=0:1 def=0.14 
	parm MIN_D_VEVERKAt	 		real count=0:1 def=-2.0 
	parm MAX_D_VEVERKAt	 		real count=0:1 def=2.0
	parm T_D_VEVERKAt	 		real count=0:1 def=0.02 
	parm MO_EXP1t	 			real count=0:1 def=0.5
	parm MIN_MO_EXP1t	 		real count=0:1 def=0.0
	parm MAX_MO_EXP1t	 		real count=0:1 def=1.0
	parm T_MO_EXP1t	 			real count=0:1 def=0.5
	parm MO_EXP2t	 			real count=0:1 def=0.1
	parm MIN_MO_EXP2t	 		real count=0:1 def=0.0
	parm MAX_MO_EXP2t	 		real count=0:1 def=1.0
	parm T_MO_EXP2t	 			real count=0:1 def=0.2

	parm A_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_A_VEVERKA	real count=0:1 	def=-- 
	parm MAX_A_VEVERKA	real count=0:1 	def=-- 
	parm T_A_VEVERKA	real count=0:1 	def=-- 
	parm B_VEVERKA	 	real count=0:1 	def=--
	parm MIN_B_VEVERKA	real count=0:1 	def=--
	parm MAX_B_VEVERKA	real count=0:1 	def=--
	parm T_B_VEVERKA	real count=0:1 	def=--
	parm C_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_C_VEVERKA	real count=0:1 	def=-- 
	parm MAX_C_VEVERKA	real count=0:1 	def=-- 
	parm T_C_VEVERKA	real count=0:1 	def=-- 
	parm D_VEVERKA	 	real count=0:1 	def=-- 
	parm MIN_D_VEVERKA	real count=0:1 	def=-- 
	parm MAX_D_VEVERKA	real count=0:1 	def=-- 
	parm T_D_VEVERKA	real count=0:1 	def=-- 
	parm MO_EXP1	 	real count=0:1 	def=--
	parm MIN_MO_EXP1	real count=0:1 	def=--
	parm MAX_MO_EXP1	real count=0:1 	def=--
	parm T_MO_EXP1	 	real count=0:1 	def=--
	parm MO_EXP2	 	real count=0:1 	def=--
	parm MIN_MO_EXP2	real count=0:1 	def=--
	parm MAX_MO_EXP2	real count=0:1 	def=--
	parm T_MO_EXP2	 	real count=0:1 	def=--

	PARMSET name=photfit2_mosher_sub help=*

		parm A_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_A_VEVERKA	real count=0:1 	def=-- 
		parm MAX_A_VEVERKA	real count=0:1 	def=-- 
		parm T_A_VEVERKA	real count=0:1 	def=-- 
		parm B_VEVERKA	 	real count=0:1 	def=--
		parm MIN_B_VEVERKA	real count=0:1 	def=--
		parm MAX_B_VEVERKA	real count=0:1 	def=--
		parm T_B_VEVERKA	real count=0:1 	def=--
		parm C_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_C_VEVERKA	real count=0:1 	def=-- 
		parm MAX_C_VEVERKA	real count=0:1 	def=-- 
		parm T_C_VEVERKA	real count=0:1 	def=-- 
		parm D_VEVERKA	 	real count=0:1 	def=-- 
		parm MIN_D_VEVERKA	real count=0:1 	def=-- 
		parm MAX_D_VEVERKA	real count=0:1 	def=-- 
		parm T_D_VEVERKA	real count=0:1 	def=-- 
		parm MO_EXP1	 	real count=0:1 	def=--
		parm MIN_MO_EXP1	real count=0:1 	def=--
		parm MAX_MO_EXP1	real count=0:1 	def=--
		parm T_MO_EXP1	 	real count=0:1 	def=--
		parm MO_EXP2	 	real count=0:1 	def=--
		parm MIN_MO_EXP2	real count=0:1 	def=--
		parm MAX_MO_EXP2	real count=0:1 	def=--
		parm T_MO_EXP2	 	real count=0:1 	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_mosher.par


	  if ($count(A_VEVERKA)=0)
	 					let A_VEVERKAt=0.997
	  else 
		let A_VEVERKAt=&A_VEVERKA
	  end-if

	  if ($count(MIN_A_VEVERKA)=0)
	 					let MIN_A_VEVERKAt=0.0
	  else 
		let MIN_A_VEVERKAt=&MIN_A_VEVERKA
	  end-if

	  if ($count(MAX_A_VEVERKA)=0)
	 					let MAX_A_VEVERKAt=1.5
	  else 
		let MAX_A_VEVERKAt=&MAX_A_VEVERKA
	  end-if

	  if ($count(T_A_VEVERKA)=0)
	 					let T_A_VEVERKAt=0.2
	  else 
		let T_A_VEVERKAt=&T_A_VEVERKA
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKAt=0.6
	  else
		let B_VEVERKAt=&B_VEVERKA
	  end-if

	  if ($count(MIN_B_VEVERKA)=0)
	 					let MIN_B_VEVERKAt=-3.0
	  else
		let MIN_B_VEVERKAt=&MIN_B_VEVERKA
	  end-if

	  if ($count(MAX_B_VEVERKA)=0)
	 					let MAX_B_VEVERKAt=3.0
	  else
		let MAX_B_VEVERKAt=&MAX_B_VEVERKA
	  end-if

	  if ($count(T_B_VEVERKA)=0)
	 					let T_B_VEVERKAt=0.05
	  else
		let T_B_VEVERKAt=&T_B_VEVERKA
	  end-if


	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKAt=0.003
	  else
		let C_VEVERKAt=&C_VEVERKA
	  end-if

	  if ($count(MIN_C_VEVERKA)=0)
	 					let MIN_C_VEVERKAt=-3.0
	  else
		let MIN_C_VEVERKAt=&MIN_C_VEVERKA
	  end-if

	  if ($count(MAX_C_VEVERKA)=0)
	 					let MAX_C_VEVERKAt=3.0
	  else
		let MAX_C_VEVERKAt=&MAX_C_VEVERKA
	  end-if

	  if ($count(T_C_VEVERKA)=0)
	 					let T_C_VEVERKAt=0.05
	  else
		let T_C_VEVERKAt=&T_C_VEVERKA
	  end-if


	  if ($count(D_VEVERKA)=0)
						let D_VEVERKAt=0.14
	  else
		let D_VEVERKAt=&D_VEVERKA
	  end-if

	  if ($count(MIN_D_VEVERKA)=0)
						let MIN_D_VEVERKAt=-2.0
	  else
		let MIN_D_VEVERKAt=&MIN_D_VEVERKA
	  end-if

	  if ($count(MAX_D_VEVERKA)=0)
						let MAX_D_VEVERKAt=2.0
	  else
		let MAX_D_VEVERKAt=&MAX_D_VEVERKA
	  end-if

	  if ($count(T_D_VEVERKA)=0)
						let T_D_VEVERKAt=0.02
	  else
		let T_D_VEVERKAt=&T_D_VEVERKA
	  end-if


	  if ($count(MO_EXP1)=0)
	 					let MO_EXP1t=0.5
	  else
	 	let MO_EXP1t=&MO_EXP1
	  end-if

	  if ($count(MIN_MO_EXP1)=0)
	 					let MIN_MO_EXP1t=0.0
	  else
	 	let MIN_MO_EXP1t=&MIN_MO_EXP1
	  end-if

	  if ($count(MAX_MO_EXP1)=0)
	 					let MAX_MO_EXP1t=1.0
	  else
	 	let MAX_MO_EXP1t=&MAX_MO_EXP1
	  end-if

	  if ($count(T_MO_EXP1)=0)
	 					let T_MO_EXP1t=0.5
	  else
	 	let T_MO_EXP1t=&T_MO_EXP1
	  end-if


	  if ($count(MO_EXP2)=0)
	 					let MO_EXP2t=0.1
	  else
	 	let MO_EXP2t=&MO_EXP2
	  end-if

	  if ($count(MIN_MO_EXP2)=0)
	 					let MIN_MO_EXP2t=0.0
	  else
	 	let MIN_MO_EXP2t=&MIN_MO_EXP2
	  end-if

	  if ($count(MAX_MO_EXP2)=0)
	 					let MAX_MO_EXP2t=1.0
	  else
	 	let MAX_MO_EXP2t=&MAX_MO_EXP2
	  end-if

	  if ($count(T_MO_EXP2)=0)
	 					let T_MO_EXP2t=0.2
	  else
	 	let T_MO_EXP2t=&T_MO_EXP2
	  end-if




	   tutor photfit2_mosher_sub 					+
		|restore=pho_mosher.par, save=pho_mosher.par|		+
			A_VEVERKA=&A_VEVERKAt				+
			MIN_A_VEVERKA=&MIN_A_VEVERKAt			+
			MAX_A_VEVERKA=&MAX_A_VEVERKAt			+
			T_A_VEVERKA=&T_A_VEVERKAt			+
			B_VEVERKA=&B_VEVERKAt				+
			MIN_B_VEVERKA=&MIN_B_VEVERKAt			+
			MAX_B_VEVERKA=&MAX_B_VEVERKAt			+
			T_B_VEVERKA=&T_B_VEVERKAt			+
			C_VEVERKA=&C_VEVERKAt				+
			MIN_C_VEVERKA=&MIN_C_VEVERKAt			+
			MAX_C_VEVERKA=&MAX_C_VEVERKAt			+
			T_C_VEVERKA=&T_C_VEVERKAt			+
			D_VEVERKA=&D_VEVERKAt				+
			MIN_D_VEVERKA=&MIN_D_VEVERKAt			+
			MAX_D_VEVERKA=&MAX_D_VEVERKAt			+
			T_D_VEVERKA=&T_D_VEVERKAt			+
			MO_EXP1=&MO_EXP1t				+     
			MIN_MO_EXP1=&MIN_MO_EXP1t			+     
			MAX_MO_EXP1=&MAX_MO_EXP1t			+     
			T_MO_EXP1=&T_MO_EXP1t				+     
			MO_EXP2=&MO_EXP2t 				+   
			MIN_MO_EXP2=&MIN_MO_EXP2t			+     
			MAX_MO_EXP2=&MAX_MO_EXP2t 			+    
			T_MO_EXP2=&T_MO_EXP2t    



	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the MOSHER function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_MOSHER' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit Mosher's photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = (A + B * g + 
C * exp(- D * g)) * pow(cos(i) * cos(e), MO_EXP1 + MO_EXP2 * g) / cos(e)

with:
i 	incidense angle
e 	emission angle
g 	phase angle 
A 	A_VEVERKA
B 	B_VEVERKA
C 	C_VEVERKA
D 	D_VEVERKA	

REFERENCE :
old VICAR Photometry programs
!
.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI A_VEVERKA 
Veverka parameter

.VARI MIN_A_VEVERKA 
Minimum of Veverka parameter

.VARI MAX_A_VEVERKA 
Maximum of Veverka parameter

.VARI T_A_VEVERKA 
Temperatur of Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI MIN_B_VEVERKA
Minimum of Veverka parameter

.VARI MAX_B_VEVERKA
Maximum of Veverka parameter

.VARI T_B_VEVERKA
Temperatur of Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI MIN_C_VEVERKA
Minimum of Veverka parameter

.VARI MAX_C_VEVERKA
Maximum of Veverka parameter

.VARI T_C_VEVERKA
Temperatur of Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MIN_D_VEVERKA
Minimum of Veverka parameter

.VARI MAX_D_VEVERKA
Maximum of Veverka parameter

.VARI T_D_VEVERKA
Temperatur of Veverka parameter

.VARI MO_EXP1
Mosher's exponent

.VARI MIN_MO_EXP1
Minimum of Mosher's exponent

.VARI MAX_MO_EXP1
Maximum of Mosher's exponent

.VARI T_MO_EXP1
Temperatur of Mosher's exponent

.VARI MO_EXP2
Mosher's exponent

.VARI MIN_MO_EXP2
Minimum of Mosher's exponent

.VARI MAX_MO_EXP2
Maximum of Mosher's exponent

.VARI T_MO_EXP2
Temperatur of Mosher's exponent

.level2

.VARI A_VEVERKA 
Parameter of the Veverka photometric function.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_A_VEVERKA 
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_A_VEVERKA 
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_A_VEVERKA 
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    A_VEVERKA_NEW = T_A_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_A_VEVERKA_NEW_* = T_A_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_VEVERKA
Parameter of the Veverka photometric function.

.VARI MIN_B_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_B_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_VEVERKA_NEW = T_B_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_VEVERKA_NEW_* = T_B_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI C_VEVERKA
Parameter of the Veverka photometric function.
Usually :
C_VEVERKA=1-A_VEVERKA

.VARI MIN_C_VEVERKA
his parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_C_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_C_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    C_VEVERKA_NEW = T_C_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_C_VEVERKA_NEW_* = T_C_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI D_VEVERKA
Parameter of the Veverka photometric function.

.VARI MIN_D_VEVERKA
This parameter gives the absolut lower limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_D_VEVERKA
This parameter gives the absolut upper limit of the parameter of the Veverka photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_D_VEVERKA
This parameter gives temperatur for the parameter of the Veverka photometric function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    D_VEVERKA_NEW = T_D_VEVERKA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_D_VEVERKA_NEW_* = T_D_VEVERKA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MIN_MO_EXP1
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP1
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP2).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP1
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP2).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP1_NEW = T_MO_EXP1 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP1_NEW_* = T_MO_EXP1_OLD_* * scale, 
scale depends of NUMTEN.

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI MIN_MO_EXP2
This parameter gives the absolut lower limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_MO_EXP2
This parameter gives the absolut upper limit of the modification of the 
coefficient k in the Minnaert part of Mosher's photometric function (goes along 
with MO_EXP1).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_MO_EXP2
This parameter gives temperatur for the modification of the coefficient k in 
the Minnaert part of Mosher's photometric function (goes along with MO_EXP1).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    MO_EXP2_NEW = T_MO_EXP2 * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_MO_EXP2_NEW_* = T_MO_EXP2_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_lumme_bowel_hg1.pdf
procedure option=selftutor help=*

	parm W_SOILt 	 	 real count=0:1 	def=0.3
	parm MIN_W_SOILt 	 real count=0:1 	def=0.0
	parm MAX_W_SOILt 	 real count=0:1 	def=1.0
	parm T_W_SOILt 	 	 real count=0:1 	def=0.2
	parm H_SHOEt	 	 real count=0:1 	def=0.06
	parm MIN_H_SHOEt	 real count=0:1 	def=0.0
	parm MAX_H_SHOEt	 real count=0:1 	def=2.0
	parm T_H_SHOEt	 	 real count=0:1 	def=0.2
	parm DEN_SOILt	 	 real count=0:1 	def=0.8
	parm MIN_DEN_SOILt	 real count=0:1 	def=0.0
	parm MAX_DEN_SOILt	 real count=0:1 	def=1.0
	parm T_DEN_SOILt	 real count=0:1 	def=0.2
	parm THETAt	 	 real count=0:1 	def=20.0
	parm MIN_THETAt	 	 real count=0:1 	def=0.0
	parm MAX_THETAt	 	 real count=0:1 	def=60.0
	parm T_THETAt	 	 real count=0:1 	def=6.0
	parm HG1_SOILt	 	 real count=0:1 	def=-0.26
	parm MIN_HG1_SOILt	 real count=0:1 	def=-1.0
	parm MAX_HG1_SOILt	 real count=0:1 	def=1.0
	parm T_HG1_SOILt	 real count=0:1 	def=0.2

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm DEN_SOIL	 	real count=0:1 def=--
	parm MIN_DEN_SOIL	real count=0:1 def=--
	parm MAX_DEN_SOIL	real count=0:1 def=--
	parm T_DEN_SOIL	 	real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm HG1_SOIL	 	real count=0:1 def=--
	parm MIN_HG1_SOIL	real count=0:1 def=--
	parm MAX_HG1_SOIL	real count=0:1 def=--
	parm T_HG1_SOIL	 	real count=0:1 def=--

	PARMSET name=photfit2_lumme_bowel_hg1_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm DEN_SOIL	 	real count=0:1 def=--
		parm MIN_DEN_SOIL	real count=0:1 def=--
		parm MAX_DEN_SOIL	real count=0:1 def=--
		parm T_DEN_SOIL	 	real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm HG1_SOIL	 	real count=0:1 def=--
		parm MIN_HG1_SOIL	real count=0:1 def=--
		parm MAX_HG1_SOIL	real count=0:1 def=--
		parm T_HG1_SOIL	 	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)


	  restore-parm pho_lumme_bowel_hg1.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(DEN_SOIL)=0)
	 					let DEN_SOILt=0.8
	  else
		let DEN_SOILt=&DEN_SOIL
	  end-if

	  if ($count(MIN_DEN_SOIL)=0)
	 					let MIN_DEN_SOILt=0.0
	  else
		let MIN_DEN_SOILt=&MIN_DEN_SOIL
	  end-if

	  if ($count(MAX_DEN_SOIL)=0)
	 					let MAX_DEN_SOILt=1.0
	  else
		let MAX_DEN_SOILt=&MAX_DEN_SOIL
	  end-if

	  if ($count(T_DEN_SOIL)=0)
	 					let T_DEN_SOILt=0.2
	  else
		let T_DEN_SOILt=&DEN_SOIL
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 					let HG1_SOILt=-0.26
	  else
		let HG1_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG1_SOIL)=0)
	 					let MIN_HG1_SOILt=-1.0
	  else
		let MIN_HG1_SOILt=&MIN_HG1_SOIL
	  end-if

	  if ($count(MAX_HG1_SOIL)=0)
	 					let MAX_HG1_SOILt=1.0
	  else
		let MAX_HG1_SOILt=&MAX_HG1_SOIL
	  end-if

	  if ($count(T_HG1_SOIL)=0)
	 					let T_HG1_SOILt=0.2
	  else
		let T_HG1_SOILt=&T_HG1_SOIL
	  end-if



	   tutor photfit2_lumme_bowel_hg1_sub 			+
			|restore=pho_lumme_bowel_hg1.par,	+
			    save=pho_lumme_bowel_hg1.par|	+
	    		W_SOIL=&W_SOILt				+
	    		MIN_W_SOIL=&MIN_W_SOILt			+
	    		MAX_W_SOIL=&MAX_W_SOILt			+
	    		T_W_SOIL=&T_W_SOILt			+
	   		H_SHOE=&H_SHOEt				+ 
	   		MIN_H_SHOE=&MIN_H_SHOEt			+ 
	   		MAX_H_SHOE=&MAX_H_SHOEt			+ 
	   		T_H_SHOE=&T_H_SHOEt			+ 
			DEN_SOIL=&DEN_SOILt			+
			MIN_DEN_SOIL=&MIN_DEN_SOILt		+
			MAX_DEN_SOIL=&MAX_DEN_SOILt		+
			T_DEN_SOIL=&T_DEN_SOILt			+
	    		THETA=&THETAt				+ 
	    		MIN_THETA=&MIN_THETAt			+ 
	    		MAX_THETA=&MAX_THETAt			+ 
	    		T_THETA=&T_THETAt			+ 
	    		HG1_SOIL=&HG1_SOILt 			+
	    		MIN_HG1_SOIL=&MIN_HG1_SOILt		+ 
	    		MAX_HG1_SOIL=&MAX_HG1_SOILt		+ 
	    		T_HG1_SOIL=&T_HG1_SOILt
			

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the LUMME_BOWEL_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_LUMME_BOWEL_HG1' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Lumme-Bowell photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

 r(i,e,g)=...


REFERENCE :
Kari Lume, Edward Bowell, Radiative Transfer in Surfaces of 
	Atmosphereless Bodies
1. Theory,
	The Astronomical Jounal, Vol. 86, No. 11, p. 1694-1704, 1981
2. Interpretation of Phase curves,
	The Astronomical Jounal, Vol. 86, No. 11, p. 1705-1721, 1981
3. Interpretation of Lunar Photometry,
	The Astronomical Jounal, Vol. 87, No. 7, p. 1076-1082, 1982

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.var W_SOIL 
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.var DEN_SOIL
Density of the soil

.var MIN_DEN_SOIL
Minimum of density of the soil

.var MAX_DEN_SOIL
Maximum of density of the soil

.var T_DEN_SOIL
Temperatur of density of the soil

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.level2

.var W_SOIL 
Single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.var H_SHOE
Parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.var DEN_SOIL
Specific volume density of the soil.

.var MIN_DEN_SOIL
This parameter gives the absolut lower limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_DEN_SOIL
This parameter gives the absolut upper limit of the specific volume density of the soil.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_DEN_SOIL
This parameter gives temperatur for the specific volume density of the soil.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    DEN_SOIL_NEW = T_DEN_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_DEN_SOIL_NEW_* = T_DEN_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.var THETA
Average topographic slope angle of surface roughness at subresolution scale.

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.var HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_81_le2.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 		def=0.3
	parm MIN_W_SOILt 	real count=0:1 		def=0.0
	parm MAX_W_SOILt 	real count=0:1 		def=1.0
	parm T_W_SOILt 		real count=0:1 		def=0.2
	parm H_SHOEt		real count=0:1 		def=0.06
	parm MIN_H_SHOEt	real count=0:1 		def=0.0
	parm MAX_H_SHOEt	real count=0:1 		def=2.0
	parm T_H_SHOEt		real count=0:1 		def=0.2
	parm LE1_SOILt		real count=0:1 		def=0.3
	parm MIN_LE1_SOILt	real count=0:1 		def=-1.732
	parm MAX_LE1_SOILt	real count=0:1 		def=1.732
	parm T_LE1_SOILt	real count=0:1 		def=0.2
	parm LE2_SOILt		real count=0:1 		def=0.3
	parm MIN_LE2_SOILt	real count=0:1 		def=-2.0
	parm MAX_LE2_SOILt	real count=0:1 		def=2.0
	parm T_LE2_SOILt	real count=0:1 		def=0.2

	parm W_SOIL 		real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 		real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm LE1_SOIL		real count=0:1 def=--
	parm MIN_LE1_SOIL	real count=0:1 def=--
	parm MAX_LE1_SOIL	real count=0:1 def=--
	parm T_LE1_SOIL		real count=0:1 def=--
	parm LE2_SOIL		real count=0:1 def=--
	parm MIN_LE2_SOIL	real count=0:1 def=--
	parm MAX_LE2_SOIL	real count=0:1 def=--
	parm T_LE2_SOIL		real count=0:1 def=--

	PARMSET name=photfit2_hapke_81_le2_sub help=*

		parm W_SOIL 		real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 		real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm LE1_SOIL		real count=0:1 def=--
		parm MIN_LE1_SOIL	real count=0:1 def=--
		parm MAX_LE1_SOIL	real count=0:1 def=--
		parm T_LE1_SOIL		real count=0:1 def=--
		parm LE2_SOIL		real count=0:1 def=--
		parm MIN_LE2_SOIL	real count=0:1 def=--
		parm MAX_LE2_SOIL	real count=0:1 def=--
		parm T_LE2_SOIL		real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_81_le2.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(LE1_SOIL)=0)
	 					let LE1_SOILt=0.3
	  else
		let LE1_SOILt=&LE1_SOIL
	  end-if

	  if ($count(MIN_LE1_SOIL)=0)
	 					let MIN_LE1_SOILt=-1.732
	  else
		let MIN_LE1_SOILt=&MIN_LE1_SOIL
	  end-if

	  if ($count(MAX_LE1_SOIL)=0)
	 					let MAX_LE1_SOILt=1.732
	  else
		let MAX_LE1_SOILt=&MAX_LE1_SOIL
	  end-if

	  if ($count(T_LE1_SOIL)=0)
	 					let T_LE1_SOILt=0.2
	  else
		let T_LE1_SOILt=&T_LE1_SOIL
	  end-if


	  if ($count(LE2_SOIL)=0)
	 					let LE2_SOILt=0.3
	  else
		let LE2_SOILt=&LE2_SOIL
	  end-if

	  if ($count(MIN_LE2_SOIL)=0)
	 					let MIN_LE2_SOILt=-2.0
	  else
		let MIN_LE2_SOILt=&MIN_LE2_SOIL
	  end-if

	  if ($count(MAX_LE2_SOIL)=0)
	 					let MAX_LE2_SOILt=2.0
	  else
		let MAX_LE2_SOILt=&MAX_LE2_SOIL
	  end-if

	  if ($count(T_LE2_SOIL)=0)
	 					let T_LE2_SOILt=0.2
	  else
		let T_LE2_SOILt=&T_LE2_SOIL
	  end-if



	   tutor photfit2_hapke_81_le2_sub  				  +
		|restore=pho_hapke_81_le2.par, save=pho_hapke_81_le2.par| +
			W_SOIL=&W_SOILt				 	  +
	    		MIN_W_SOIL=&MIN_W_SOILt			 	  +
	    		MAX_W_SOIL=&MAX_W_SOILt			 	  +
			T_W_SOIL=&T_W_SOILt				  +
	   		H_SHOE=&H_SHOEt					  + 
	   		MIN_H_SHOE=&MIN_H_SHOEt				  + 
	   		MAX_H_SHOE=&MAX_H_SHOEt			 	  + 
	   		T_H_SHOE=&T_H_SHOEt				  + 
			LE1_SOIL=&LE1_SOILt				  +
			MIN_LE1_SOIL=&MIN_LE1_SOILt			  +
			MAX_LE1_SOIL=&MAX_LE1_SOILt			  +
			T_LE1_SOIL=&T_LE1_SOILt				  +
			LE2_SOIL=&LE2_SOILt				  +	
			MIN_LE2_SOIL=&MIN_LE2_SOILt			  +	
			MAX_LE2_SOIL=&MAX_LE2_SOILt			  +
			T_LE2_SOIL=&T_LE2_SOILt	

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_81_LE2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOfit_HAPHE_81_LE2' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Hapke-1981-two-term-Legendre-Polynomial photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...


REFERENCE :
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
1. Theory
   Journal of Geophysical Research Vol. 86, No.. B4, p. 3039-3054

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of single-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI LE1_SOIL
Hapke parameter
First Legendre-Polynom

.VARI MIN_LE1_SOIL
Minimum of 
first Legendre-Polynom

.VARI MAX_LE1_SOIL
Maximum of 
first Legendre-Polynom

.VARI T_LE1_SOIL
Temperatur of 
first Legendre-Polynom

.VARI LE2_SOIL
Second Legendre-Polynom

.VARI MIN_LE2_SOIL
Minimum of 
second Legendre-Polynom

.VARI MAX_LE2_SOIL
Maximum of 
second Legendre-Polynom

.VARI T_LE2_SOIL
Temperatur of 
second Legendre-Polynom

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.var H_SHOE
Parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_81_cook.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 		def=0.3
	parm MIN_W_SOILt 	real count=0:1 		def=0.0
	parm MAX_W_SOILt 	real count=0:1 		def=1.0
	parm T_W_SOILt 		real count=0:1 		def=0.2
	parm H_SHOEt		real count=0:1 		def=0.06
	parm MIN_H_SHOEt	real count=0:1 		def=0.0
	parm MAX_H_SHOEt	real count=0:1 		def=2.0
	parm T_H_SHOEt		real count=0:1 		def=0.2
	parm LE1_SOILt		real count=0:1 		def=0.3
	parm MIN_LE1_SOILt	real count=0:1 		def=-1.732
	parm MAX_LE1_SOILt	real count=0:1 		def=1.732
	parm T_LE1_SOILt	real count=0:1 		def=0.2
	parm LE2_SOILt		real count=0:1 		def=0.3
	parm MIN_LE2_SOILt	real count=0:1 		def=-2.0
	parm MAX_LE2_SOILt	real count=0:1 		def=2.0
	parm T_LE2_SOILt	real count=0:1 		def=0.2
	parm COOKt	 	real count=0:1 		def=0.9
	parm MIN_COOKt		real count=0:1 		def=0.0
	parm MAX_COOKt		real count=0:1 		def=1.0
	parm T_COOKt		real count=0:1 		def=0.5

	parm W_SOIL 		real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 		real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm LE1_SOIL		real count=0:1 def=--
	parm MIN_LE1_SOIL	real count=0:1 def=--
	parm MAX_LE1_SOIL	real count=0:1 def=--
	parm T_LE1_SOIL		real count=0:1 def=--
	parm LE2_SOIL		real count=0:1 def=--
	parm MIN_LE2_SOIL	real count=0:1 def=--
	parm MAX_LE2_SOIL	real count=0:1 def=--
	parm T_LE2_SOIL		real count=0:1 def=--
	parm COOK		real count=0:1 def=--
	parm MIN_COOK		real count=0:1 def=--
	parm MAX_COOK		real count=0:1 def=--
	parm T_COOK		real count=0:1 def=--

	PARMSET name=photfit2_hapke_81_cook_sub help=*

		parm W_SOIL 		real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 		real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm LE1_SOIL		real count=0:1 def=--
		parm MIN_LE1_SOIL	real count=0:1 def=--
		parm MAX_LE1_SOIL	real count=0:1 def=--
		parm T_LE1_SOIL		real count=0:1 def=--
		parm LE2_SOIL		real count=0:1 def=--
		parm MIN_LE2_SOIL	real count=0:1 def=--
		parm MAX_LE2_SOIL	real count=0:1 def=--
		parm T_LE2_SOIL		real count=0:1 def=--
		parm COOK		real count=0:1 def=--
		parm MIN_COOK		real count=0:1 def=--
		parm MAX_COOK		real count=0:1 def=--
		parm T_COOK		real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)


	  restore-parm pho_hapke_81_cook.par


	  if ($count(W_SOIL)=0)
	 						let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(LE1_SOIL)=0)
	 					let LE1_SOILt=0.3
	  else
		let LE1_SOILt=&LE1_SOIL
	  end-if

	  if ($count(MIN_LE1_SOIL)=0)
	 					let MIN_LE1_SOILt=-1.732
	  else
		let MIN_LE1_SOILt=&MIN_LE1_SOIL
	  end-if

	  if ($count(MAX_LE1_SOIL)=0)
	 					let MAX_LE1_SOILt=1.732
	  else
		let MAX_LE1_SOILt=&MAX_LE1_SOIL
	  end-if

	  if ($count(T_LE1_SOIL)=0)
	 					let T_LE1_SOILt=0.2
	  else
		let T_LE1_SOILt=&T_LE1_SOIL
	  end-if


	  if ($count(LE2_SOIL)=0)
	 					let LE2_SOILt=0.3
	  else
		let LE2_SOILt=&LE2_SOIL
	  end-if

	  if ($count(MIN_LE2_SOIL)=0)
	 					let MIN_LE2_SOILt=-2.0
	  else
		let MIN_LE2_SOILt=&MIN_LE2_SOIL
	  end-if

	  if ($count(MAX_LE2_SOIL)=0)
	 					let MAX_LE2_SOILt=2.0
	  else
		let MAX_LE2_SOILt=&MAX_LE2_SOIL
	  end-if

	  if ($count(T_LE2_SOIL)=0)
	 					let T_LE2_SOILt=0.2
	  else
		let T_LE2_SOILt=&T_LE2_SOIL
	  end-if


	  if ($count(COOK)=0)
	 						let COOKt=0.9
	  else
		let COOKt=&COOK
	  end-if

	  if ($count(MIN_COOK)=0)
	 						let MIN_COOKt=0.0
	  else
		let MIN_COOKt=&MIN_COOK
	  end-if

	  if ($count(MAX_COOK)=0)
	 						let MAX_COOKt=1.0
	  else
		let MAX_COOKt=&MAX_COOK
	  end-if

	  if ($count(T_COOK)=0)
	 						let T_COOKt=0.5
	  else
		let T_COOKt=&COOK
	  end-if



	   tutor photfit2_hapke_81_cook_sub 				    +
		|restore=pho_hapke_81_cook.par, save=pho_hapke_81_cook.par| +
			W_SOIL=&W_SOILt				  	    +
	    		MIN_W_SOIL=&MIN_W_SOILt			  	    +
	    		MAX_W_SOIL=&MAX_W_SOILt			  	    +
			T_W_SOIL=&T_W_SOILt				    +
	   		H_SHOE=&H_SHOEt				  	    + 
	   		MIN_H_SHOE=&MIN_H_SHOEt			  	    + 
	   		MAX_H_SHOE=&MAX_H_SHOEt			  	    + 
	   		T_H_SHOE=&T_H_SHOEt				    + 
			LE1_SOIL=&LE1_SOILt				    +
			MIN_LE1_SOIL=&MIN_LE1_SOILt			    +
			MAX_LE1_SOIL=&MAX_LE1_SOILt			    +
			T_LE1_SOIL=&T_LE1_SOILt			  	    +
			LE2_SOIL=&LE2_SOILt				    +	
			MIN_LE2_SOIL=&MIN_LE2_SOILt			    +	
			MAX_LE2_SOIL=&MAX_LE2_SOILt			    +
			T_LE2_SOIL=&T_LE2_SOILt			  	    +
			COOK=&COOKt					    +
			MIN_COOK=&MIN_COOKt				    +
			MAX_COOK=&MAX_COOKt				    +
			COOK=&COOKt	



	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_81_COOK function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_HAPKE_81_COOK' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Hapke-1981-two-term-Legendre-Polynomial bidirectional reflectance in 
the modification of Cook.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :


The Cook bidirectional reflectance is a modification to the 1981 Hapke one. 
The modification is in the redefining incidence and emission angles to their 
new values :
	cos(i) <-- sqrt(1-COOK*COOK*(1-cos(i)*cos(i)))
	cos(e) <-- sqrt(1-COOK*COOK*(1-cos(e)*cos(e)))



REFERENCE :
Old VICAR Photometry programs

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of single-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI LE1_SOIL
Hapke parameter

.VARI MIN_LE1_SOIL
Minimum of Hapke parameter

.VARI MAX_LE1_SOIL
Maximum of Hapke parameter

.VARI T_LE1_SOIL
Temperatur of Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI MIN_LE2_SOIL
Minimum of Hapke parameter

.VARI MAX_LE2_SOIL
Maximum of Hapke parameter

.VARI T_LE2_SOIL
Temperatur of Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI MIN_COOK
Minimum of Hapke-Cook parameter

.VARI MAX_COOK
Maximum of Hapke-Cook parameter

.VARI T_COOK
Temperatur of Hapke-Cook parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.var H_SHOE
Parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI MIN_COOK
This parameter gives the absolut lower limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_COOK
This parameter gives the absolut upper limit of the parameter of the Cook's 
modification of the old Hapke function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_COOK
This parameter gives temperatur for the parameter of the Cook's modification 
of the old Hapke function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    COOK_NEW = T_COOK * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_COOK_NEW_* = T_COOK_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_86_hg1.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 		def=0.3
	parm MIN_W_SOILt 	real count=0:1 		def=0.0
	parm MAX_W_SOILt 	real count=0:1 		def=1.0
	parm T_W_SOILt 		real count=0:1 		def=0.2
	parm H_SHOEt		real count=0:1 		def=0.06
	parm MIN_H_SHOEt	real count=0:1 		def=0.0
	parm MAX_H_SHOEt	real count=0:1 		def=2.0
	parm T_H_SHOEt		real count=0:1 		def=0.2
	parm B_SHOEt		real count=0:1 		def=2.0
	parm MIN_B_SHOEt	real count=0:1 		def=0.0
	parm MAX_B_SHOEt	real count=0:1 		def=10.0
	parm T_B_SHOEt		real count=0:1 		def=1.0
	parm THETAt	 	real count=0:1 		def=20.0
	parm MIN_THETAt		real count=0:1 		def=0.0
	parm MAX_THETAt		real count=0:1 		def=60.0
	parm T_THETAt		real count=0:1 		def=6.0
	parm HG1_SOILt		real count=0:1 		def=-0.26
	parm MIN_HG1_SOILt	real count=0:1 		def=-1.0
	parm MAX_HG1_SOILt	real count=0:1 		def=1.0
	parm T_HG1_SOILt	real count=0:1 		def=0.2

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm B_SHOE		real count=0:1 def=--
	parm MIN_B_SHOE		real count=0:1 def=--
	parm MAX_B_SHOE		real count=0:1 def=--
	parm T_B_SHOE		real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm HG1_SOIL	 	real count=0:1 def=--
	parm MIN_HG1_SOIL	real count=0:1 def=--
	parm MAX_HG1_SOIL	real count=0:1 def=--
	parm T_HG1_SOIL	 	real count=0:1 def=--

	PARMSET name=photfit2_hapke_86_hg1_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm B_SHOE		real count=0:1 def=--
		parm MIN_B_SHOE		real count=0:1 def=--
		parm MAX_B_SHOE		real count=0:1 def=--
		parm T_B_SHOE		real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm HG1_SOIL	 	real count=0:1 def=--
		parm MIN_HG1_SOIL	real count=0:1 def=--
		parm MAX_HG1_SOIL	real count=0:1 def=--
		parm T_HG1_SOIL	 	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_hg1.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(B_SHOE)=0)
	 					let B_SHOEt=2.0
	  else
		let B_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_B_SHOE)=0)
	 					let MIN_B_SHOEt=0.0
	  else
		let MIN_B_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_B_SHOE)=0)
	 					let MAX_B_SHOEt=1.0
	  else
		let MAX_B_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_B_SHOE)=0)
	 					let T_B_SHOEt=1.0
	  else
		let T_B_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 					let HG1_SOILt=-0.26
	  else
		let HG1_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG1_SOIL)=0)
	 					let MIN_HG1_SOILt=-1.0
	  else
		let MIN_HG1_SOILt=&MIN_HG1_SOIL
	  end-if

	  if ($count(MAX_HG1_SOIL)=0)
	 					let MAX_HG1_SOILt=1.0
	  else
		let MAX_HG1_SOILt=&MAX_HG1_SOIL
	  end-if

	  if ($count(T_HG1_SOIL)=0)
	 					let T_HG1_SOILt=0.2
	  else
		let T_HG1_SOILt=&T_HG1_SOIL
	  end-if



	   tutor photfit2_hapke_86_hg1_sub  				  +
		|restore=pho_hapke_86_hg1.par, save=pho_hapke_86_hg1.par| +
	    		W_SOIL=&W_SOILt				  	  +
	    		MIN_W_SOIL=&MIN_W_SOILt			  	  +
	    		MAX_W_SOIL=&MAX_W_SOILt			  	  +
	    		T_W_SOIL=&T_W_SOILt				  +
	   		H_SHOE=&H_SHOEt				  	  + 
	   		MIN_H_SHOE=&MIN_H_SHOEt			  	  + 
	   		MAX_H_SHOE=&MAX_H_SHOEt			  	  + 
	   		T_H_SHOE=&T_H_SHOEt				  + 
	   		B_SHOE=&B_SHOEt				  	  + 
	   		MIN_B_SHOE=&MIN_B_SHOEt			  	  + 
	   		MAX_B_SHOE=&MAX_B_SHOEt			  	  + 
	   		T_B_SHOE=&T_B_SHOEt				  + 
	    		THETA=&THETAt				  	  + 
	    		MIN_THETA=&MIN_THETAt			  	  + 
	    		MAX_THETA=&MAX_THETAt			  	  + 
	    		T_THETA=&T_THETAt				  + 
	    		HG1_SOIL=&HG1_SOILt 			  	  +
	    		MIN_HG1_SOIL=&MIN_HG1_SOILt			  + 
	    		MAX_HG1_SOIL=&MAX_HG1_SOILt 		  	  +
	    		T_HG1_SOIL=&T_HG1_SOILt 


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write "  This program is only intended to be run "
	   write "  as tutor from other programs needs "
	   write "  photometric function parameters "
	   write "  for the HAPKE_86_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_HAPKE-86_HG1' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Hapke-1986-one-term-Henyey-Greenstein photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of opposition magnitude

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_86_hg2.pdf
procedure option=selftutor help=*

	parm W_SOILt 	 	 real count=0:1 	def=0.21
	parm MIN_W_SOILt 	 real count=0:1 	def=0.0
	parm MAX_W_SOILt 	 real count=0:1 	def=1.0
	parm T_W_SOILt 	 	 real count=0:1 	def=0.2
	parm H_SHOEt	 	 real count=0:1 	def=0.07
	parm MIN_H_SHOEt	 real count=0:1 	def=0.0
	parm MAX_H_SHOEt	 real count=0:1 	def=2.0
	parm T_H_SHOEt	 	 real count=0:1 	def=0.2
	parm B_SHOEt	 	 real count=0:1		def=2.0
	parm MIN_B_SHOEt	 real count=0:1 	def=0.0
	parm MAX_B_SHOEt	 real count=0:1 	def=10.0
	parm T_B_SHOEt	 	 real count=0:1 	Def=1.0
	parm THETAt	  	 real count=0:1 	def=20.0
	parm MIN_THETAt	 	 real count=0:1 	def=0.0
	parm MAX_THETAt	 	 real count=0:1 	def=60.0
	parm T_THETAt	 	 real count=0:1 	def=6.0
	parm HG1_SOILt	 	 real count=0:1 	def=-0.29
	parm MIN_HG1_SOILt	 real count=0:1 	def=-1.0
	parm MAX_HG1_SOILt	 real count=0:1 	def=1.0
	parm T_HG1_SOILt	 real count=0:1 	def=0.2
	parm HG2_SOILt	 	 real count=0:1 	def=-0.29
	parm MIN_HG2_SOILt	 real count=0:1 	def=-1.0
	parm MAX_HG2_SOILt	 real count=0:1 	def=1.0
	parm T_HG2_SOILt	 real count=0:1 	def=0.2
	parm HG_ASY_SOILt 	 real count=0:1 	def=1.0
	parm MIN_HG_ASY_SOIt     real count=0:1 	def=-10.0
	parm MAX_HG_ASY_SOIt 	 real count=0:1 	def=10.0
	parm T_HG_ASY_SOILt 	 real count=0:1 	def=1.0

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm B_SHOE		real count=0:1 def=--
	parm MIN_B_SHOE		real count=0:1 def=--
	parm MAX_B_SHOE		real count=0:1 def=--
	parm T_B_SHOE		real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm HG1_SOIL	 	real count=0:1 def=--
	parm MIN_HG1_SOIL	real count=0:1 def=--
	parm MAX_HG1_SOIL	real count=0:1 def=--
	parm T_HG1_SOIL	 	real count=0:1 def=--
	parm HG2_SOIL	 	real count=0:1 def=--
	parm MIN_HG2_SOIL	real count=0:1 def=--
	parm MAX_HG2_SOIL	real count=0:1 def=--
	parm T_HG2_SOIL	 	real count=0:1 def=--
	parm HG_ASY_SOIL	real count=0:1 def=--
	parm MIN_HG_ASY_SOIL	real count=0:1 def=--
	parm MAX_HG_ASY_SOIL	real count=0:1 def=--
	parm T_HG_ASY_SOIL	real count=0:1 def=--

	PARMSET name=photfit2_hapke_86_hg2_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm B_SHOE		real count=0:1 def=--
		parm MIN_B_SHOE		real count=0:1 def=--
		parm MAX_B_SHOE		real count=0:1 def=--
		parm T_B_SHOE		real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm HG1_SOIL	 	real count=0:1 def=--
		parm MIN_HG1_SOIL	real count=0:1 def=--
		parm MAX_HG1_SOIL	real count=0:1 def=--
		parm T_HG1_SOIL	 	real count=0:1 def=--
		parm HG2_SOIL	 	real count=0:1 def=--
		parm MIN_HG2_SOIL	real count=0:1 def=--
		parm MAX_HG2_SOIL	real count=0:1 def=--
		parm T_HG2_SOIL	 	real count=0:1 def=--
	    	parm HG_ASY_SOIL	real count=0:1 def=--
	    	parm MIN_HG_ASY_SOIL	real count=0:1 def=--
	    	parm MAX_HG_ASY_SOIL	real count=0:1 def=--
	    	parm T_HG_ASY_SOIL	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_hg2.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.21
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.07
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(B_SHOE)=0)
	 					let B_SHOEt=2.0
	  else
		let B_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_B_SHOE)=0)
	 					let MIN_B_SHOEt=0.0
	  else
		let MIN_B_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_B_SHOE)=0)
	 					let MAX_B_SHOEt=10.0
	  else
		let MAX_B_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_B_SHOE)=0)
	 					let T_B_SHOEt=1.0
	  else
		let T_B_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 					let HG1_SOILt=-0.29
	  else
		let HG1_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG1_SOIL)=0)
	 					let MIN_HG1_SOILt=-1.0
	  else
		let MIN_HG1_SOILt=&MIN_HG1_SOIL
	  end-if

	  if ($count(MAX_HG1_SOIL)=0)
	 					let MAX_HG1_SOILt=1.0
	  else
		let MAX_HG1_SOILt=&MAX_HG1_SOIL
	  end-if

	  if ($count(T_HG1_SOIL)=0)
	 					let T_HG1_SOILt=0.2
	  else
		let T_HG1_SOILt=&T_HG1_SOIL
	  end-if


	  if ($count(HG2_SOIL)=0)
	 					let HG2_SOILt=0.39
	  else
		let HG2_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG2_SOIL)=0)
	 					let MIN_HG2_SOILt=-1.0
	  else
		let MIN_HG2_SOILt=&MIN_HG2_SOIL
	  end-if

	  if ($count(MAX_HG2_SOIL)=0)
	 					let MAX_HG2_SOILt=1.0
	  else
		let MAX_HG2_SOILt=&MAX_HG2_SOIL
	  end-if

	  if ($count(T_HG2_SOIL)=0)
	 					let T_HG2_SOILt=0.2
	  else
		let T_HG2_SOILt=&T_HG1_SOIL
	  end-if


	  if ($count(HG_ASY_SOIL)=0)
	 					let HG_ASY_SOIt=1
	  else
		let HG_ASY_SOILt=&HG_ASY_SOIL
	  end-if

	  if ($count(MIN_HG_ASY_SOIL)=0)
	 					let MIN_HG_ASY_SOIt=-10
	  else
		let MIN_HG_ASY_SOIt=&MIN_HG_ASY_SOIL
	  end-if

	  if ($count(MAX_HG_ASY_SOIL)=0)
	 					let MAX_HG_ASY_SOIt=10
	  else
		let MIN_HG_ASY_SOIt=&MAX_HG_ASY_SOIL
	  end-if

	  if ($count(T_HG_ASY_SOIL)=0)
	 					let T_HG_ASY_SOIt=1
	  else
		let T_HG_ASY_SOILt=&T_HG_ASY_SOIL
	  end-if




	   tutor photfit2_hapke_86_hg2_sub  				  +
		|restore=pho_hapke_86_hg2.par, save=pho_hapke_86_hg2.par| +
	    		W_SOIL=&W_SOILt				 	  +
	    		MIN_W_SOIL=&MIN_W_SOILt				  +
	    		MAX_W_SOIL=&MAX_W_SOILt			 	  +
	    		T_W_SOIL=&T_W_SOILt				  +
	   		H_SHOE=&H_SHOEt					  + 
	   		MIN_H_SHOE=&MIN_H_SHOEt				  + 
	   		MAX_H_SHOE=&MAX_H_SHOEt				  + 
	   		T_H_SHOE=&T_H_SHOEt				  + 
	   		B_SHOE=&B_SHOEt					  + 
	   		MIN_B_SHOE=&MIN_B_SHOEt				  + 
	   		MAX_B_SHOE=&MAX_B_SHOEt				  + 
	   		T_B_SHOE=&T_B_SHOEt				  + 
	    		THETA=&THETAt					  + 
	    		MIN_THETA=&MIN_THETAt				  + 
	    		MAX_THETA=&MAX_THETAt				  + 
	    		T_THETA=&T_THETAt				  + 
	    		HG1_SOIL=&HG1_SOILt 				  +
	    		MIN_HG1_SOIL=&MIN_HG1_SOILt			  + 
	    		MAX_HG1_SOIL=&MAX_HG1_SOILt 			  +
	    		T_HG1_SOIL=&T_HG1_SOILt				  +
	    		MIN_HG2_SOIL=&MIN_HG2_SOILt			  + 
	    		MAX_HG2_SOIL=&MAX_HG2_SOILt			  +
	    		T_HG2_SOIL=&T_HG2_SOILt				  +
	    		HG_ASY_SOIL=&HG_ASY_SOILt			  +	
	    		MIN_HG_ASY_SOIL=&MIN_HG_ASY_SOIt		  +	
	    		MAX_HG_ASY_SOIL=&MAX_HG_ASY_SOIt		  +	
	    		T_HG_ASY_SOIL=&T_HG_ASY_SOILt	
 

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_86_HG2 function"
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_HAPKE_86_HG2' 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and limits needed to fit 
the Hapke-1986-two-term-Henyey-Greenstein photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of opposition magnitude

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG2_SOIL
Hapke parameter

.var MIN_HG2_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG2_SOIL
Maximum of Henyey-Greenstein term

.VARI T_HG2_SOIL
Temperatur of Henyey-Greenstein term

.VARI HG_ASY_SOIL
Asymetry term of
Henyey-Greenstein

.VARI MIN_HG_ASY_SOIL
Minimum of asymetry parameter

.VARI MAX_HG_ASY_SOIL
Maximum of asymetry parameter

.VARI T_HG_ASY_SOIL
Temperatur of asymetry parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI MIN_HG2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Henyey-Greenstein soil particle phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG2_SOIL_NEW = T_HG2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG2_SOIL_NEW_* = T_HG2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG_ASY_SOIL
This parameter gives the asymmetry parameter (weight of the two terms in the 
Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MIN_HG_ASY_SOIL
This parameter gives the absolut lower limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
in the Henyey-Greenstein soil phase function).

.VARI MAX_HG_ASY_SOIL
This parameter gives the absolut upper limit of the asymmetry parameter (weight 
of the two terms in the Henyey-Greenstein soil phase function).
in the Henyey-Greenstein soil phase function).
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG_ASY_SOIL
This parameter gives temperatur for the parameter of the asymmetry parameter (weight of the two terms in the Henyey-Greenstein soil phase function).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG_ASY_SOIL_NEW = T_HG_ASY_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG_ASY_SOIL_NEW_* = T_HG_ASY_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_86_le2.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 		def=0.21
	parm MIN_W_SOILt 	real count=0:1 		def=0.0
	parm MAX_W_SOILt 	real count=0:1 		def=1.0
	parm T_W_SOILt 		real count=0:1 		def=0.2
	parm H_SHOEt		real count=0:1 		Def=0.07
	parm MIN_H_SHOEt	real count=0:1 		def=0.0
	parm MAX_H_SHOEt	real count=0:1 		def=2.0
	parm T_H_SHOEt		real count=0:1 		Def=0.2
	parm B_SHOEt		real count=0:1 		def=2.0
	parm MIN_B_SHOEt	real count=0:1 		def=0.0
	parm MAX_B_SHOEt	real count=0:1 		def=10.0
	parm T_B_SHOEt		real count=0:1 		def=1.0
	parm THETAt	 	real count=0:1 		def=20
	parm MIN_THETAt		real count=0:1 		def=0
	parm MAX_THETAt		real count=0:1 		def=60.0
	parm T_THETAt		real count=0:1 		def=6.0
	parm LE1_SOILt		real count=0:1 		def=0.29
	parm MIN_LE1_SOILt	real count=0:1 		def=-1.732
	parm MAX_LE1_SOILt	real count=0:1 		def=1.732
	parm T_LE1_SOILt	real count=0:1 		def=0.2
	parm LE2_SOILt	 	real count=0:1 		def=0.39
	parm MIN_LE2_SOILt	real count=0:1 		def=-2.0
	parm MAX_LE2_SOILt	real count=0:1 		def=2.0
	parm T_LE2_SOILt	real count=0:1 		def=0.2

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm B_SHOE		real count=0:1 def=--
	parm MIN_B_SHOE		real count=0:1 def=--
	parm MAX_B_SHOE		real count=0:1 def=--
	parm T_B_SHOE		real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm LE1_SOIL		real count=0:1 def=--
	parm MIN_LE1_SOIL	real count=0:1 def=--
	parm MAX_LE1_SOIL	real count=0:1 def=--
	parm T_LE1_SOIL		real count=0:1 def=--
	parm LE2_SOIL		real count=0:1 def=--
	parm MIN_LE2_SOIL	real count=0:1 def=--
	parm MAX_LE2_SOIL	real count=0:1 def=--
	parm T_LE2_SOIL		real count=0:1 def=--

	PARMSET name=photfit2_hapke_86_le2_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm B_SHOE		real count=0:1 def=--
		parm MIN_B_SHOE		real count=0:1 def=--
		parm MAX_B_SHOE		real count=0:1 def=--
		parm T_B_SHOE		real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm LE1_SOIL		real count=0:1 def=--
		parm MIN_LE1_SOIL	real count=0:1 def=--
		parm MAX_LE1_SOIL	real count=0:1 def=--
		parm T_LE1_SOIL		real count=0:1 def=--
		parm LE2_SOIL		real count=0:1 def=--
		parm MIN_LE2_SOIL	real count=0:1 def=--
		parm MAX_LE2_SOIL	real count=0:1 def=--
		parm T_LE2_SOIL		real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_le2.par


	  if ($count(W_SOIL)=0) 
						let W_SOILt=0.21
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if



	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.07
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if



	  if ($count(B_SHOE)=0)
	 					let B_SHOEt=2.0
	  else
		let B_SHOEt=&B_SHOE
	  end-if

	  if ($count(MIN_B_SHOE)=0)
	 					let MIN_B_SHOEt=0.0
	  else
		let MIN_B_SHOEt=&MIN_B_SHOE
	  end-if

	  if ($count(MAX_B_SHOE)=0)
	 					let MAX_B_SHOEt=10.0
	  else
		let MAX_B_SHOEt=&MAX_B_SHOE
	  end-if

	  if ($count(T_B_SHOE)=0)
	 					let T_B_SHOEt=1.0
	  else
		let T_B_SHOEt=&T_B_SHOE
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20.0
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(LE1_SOIL)=0)
	 					let LE1_SOILt=0.29
	  else
	 	let LE1_SOILt=&LE1_SOIL
	  end-if

	  if ($count(MIN_LE1_SOIL)=0)
	 					let MIN_LE1_SOILt=-1.732
	  else
		let MIN_LE1_SOILt=&MIN_LE1_SOIL
	  end-if

	  if ($count(MAX_LE1_SOIL)=0)
	 					let MAX_LE1_SOILt=1.732
	  else
		let MAX_LE1_SOILt=&MAX_LE1_SOIL
	  end-if

	  if ($count(T_LE1_SOIL)=0)
	 					let T_LE1_SOILt=0.2
	  else
		let T_LE1_SOILt=&T_LE1_SOIL
	  end-if


	  if ($count(LE2_SOIL)=0)
	 					let LE2_SOILt=0.39
	  else
	 	let LE2_SOILt=&LE2_SOIL
	  end-if

	  if ($count(MIN_LE2_SOIL)=0)
	 					let MIN_LE2_SOILt=-2.0
	  else
		let MIN_LE2_SOILt=&MIN_LE2_SOIL
	  end-if

	  if ($count(MAX_LE2_SOIL)=0)
	 					let MAX_LE2_SOILt=2.0
	  else
		let MAX_LE2_SOILt=&MAX_LE2_SOIL
	  end-if

	  if ($count(T_LE2_SOIL)=0)
	 					let T_LE2_SOILt=0.2
	  else
		let T_LE2_SOILt=&T_LE2_SOIL
	  end-if


	   tutor photfit2_hapke_86_le2_sub 				  +
		|restore=pho_hapke_86_le2.par,				  +
		 save=pho_hapke_86_le2.par|				  +
	    		W_SOIL=&W_SOILt				          +
	    		MIN_W_SOIL=&MIN_W_SOILt			          +
	    		MAX_W_SOIL=&MAX_W_SOILt			          +
	    		T_W_SOIL=&T_W_SOILt			 	  +
	   		H_SHOE=&H_SHOEt				          + 
	   		MIN_H_SHOE=&MIN_H_SHOEt			  	  + 
	   		MAX_H_SHOE=&MAX_H_SHOEt			  	  + 
	   		T_H_SHOE=&T_H_SHOEt				  + 
	   		B_SHOE=&B_SHOEt				  	  + 
	   		MIN_B_SHOE=&MIN_B_SHOEt			  	  + 
	   		MAX_B_SHOE=&MAX_B_SHOEt			  	  + 
	   		T_B_SHOE=&T_B_SHOEt				  + 
	    		THETA=&THETAt				  	  + 
	    		MIN_THETA=&MIN_THETAt			  	  + 
	    		MAX_THETA=&MAX_THETAt			  	  + 
	    		T_THETA=&T_THETAt				  + 
			LE1_SOIL=&LE1_SOILt				  +
			MIN_LE1_SOIL=&MIN_LE1_SOILt			  +
			MAX_LE1_SOIL=&MAX_LE1_SOILt			  +
			T_LE1_SOIL=&T_LE1_SOILt			  	  +
			LE2_SOIL=&LE2_SOILt				  +	
			MIN_LE2_SOIL=&MIN_LE2_SOILt			  +	
			MAX_LE2_SOIL=&MAX_LE2_SOILt			  +
			T_LE2_SOIL=&T_LE2_SOILt	
	
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_86_LE2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_HAPKE_86_LE2' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Hapke-1986-two-term-Legendre-Polynomial photometric function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = ...


REFERENCE :
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.VARI T_LE1_SOIL
Temperatur of single-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of width of opposition magnitude

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of width of topographic slope angle

.VARI LE1_SOIL
Hapke parameter
First Legendre-Polynom

.VARI MIN_LE1_SOIL
Minimum of 
first Legendre-Polynom

.VARI MAX_LE1_SOIL
Maximum of 
first Legendre-Polynom

.VARI T_LE1_SOIL
Temperatur of 
first Legendre-Polynom

.VARI LE2_SOIL
Second Legendre-Polynom

.VARI MIN_LE2_SOIL
Minimum of 
second Legendre-Polynom

.VARI MAX_LE2_SOIL
Maximum of 
second Legendre-Polynom

.VARI T_LE2_SOIL
Temperatur of 
second Legendre-Polynom

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.
 
.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term 
of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE1_SOIL_NEW = T_LE1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE1_SOIL_NEW_* = T_LE1_SOILE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI MIN_LE2_SOIL
This parameter gives the absolut lower limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_LE2_SOIL
This parameter gives the absolut upper limit of the parameter of the second 
term of the Legendre-Polynomial soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_LE2_SOIL
This parameter gives temperatur for the parameter of the second term of the 
Legendre-Polynomial soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    LE2_SOIL_NEW = T_LE2_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_LE2_SOIL_NEW_* = T_LE2_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_hapke_hg1_dom.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 			def=0.3
	parm MIN_W_SOILt 	real count=0:1 			def=0.0
	parm MAX_W_SOILt 	real count=0:1 			def=1.0
	parm T_W_SOILt 		real count=0:1 			def=0.2
	parm H_SHOEt		real count=0:1 			def=0.06
	parm MIN_H_SHOEt	real count=0:1 			def=0.0
	parm MAX_H_SHOEt	real count=0:1 			def=2.0
	parm T_H_SHOEt		real count=0:1 			def=0.2
	parm B_SHOEt		real count=0:1 			def=2.0
	parm MIN_B_SHOEt	real count=0:1 			def=0.0
	parm MAX_B_SHOEt	real count=0:1 			def=10.0
	parm T_B_SHOEt		real count=0:1 			def=1.0
	parm THETAt	 	real count=0:1 			def=20
	parm MIN_THETAt		real count=0:1 			def=0
	parm MAX_THETAt		real count=0:1 			def=60.0
	parm T_THETAt		real count=0:1 			def=6.0
	parm HG1_SOILt		real count=0:1 			def=-0.26
	parm MIN_HG1_SOILt	real count=0:1 			def=-1.0
	parm MAX_HG1_SOILt	real count=0:1 			def=1.0
	parm T_HG1_SOILt	real count=0:1 			def=0.2
    	parm H_CBOEt 		real count=0:1 			def=0.06
    	parm MIN_H_CBOEt 	real count=0:1 			def=0.0
    	parm MAX_H_CBOEt 	real count=0:1 			def=2.0
    	parm T_H_CBOEt 		real count=0:1 			def=0.2
    	parm B_CBOEt 		real count=0:1 			def=1.0
    	parm MIN_B_CBOEt 	real count=0:1 			def=0.0
    	parm MAX_B_CBOEt 	real count=0:1 			def=2.0
    	parm T_B_CBOEt 		real count=0:1 			def=0.2

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm B_SHOE		real count=0:1 def=--
	parm MIN_B_SHOE		real count=0:1 def=--
	parm MAX_B_SHOE		real count=0:1 def=--
	parm T_B_SHOE		real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm HG1_SOIL	 	real count=0:1 def=--
	parm MIN_HG1_SOIL	real count=0:1 def=--
	parm MAX_HG1_SOIL	real count=0:1 def=--
	parm T_HG1_SOIL	 	real count=0:1 def=--
    	parm H_CBOE 	 	real count=0:1 def=--
    	parm MIN_H_CBOE 	real count=0:1 def=--
    	parm MAX_H_CBOE 	real count=0:1 def=--
    	parm T_H_CBOE 	 	real count=0:1 def=--
    	parm B_CBOE 	 	real count=0:1 def=--
    	parm MIN_B_CBOE 	real count=0:1 def=--
    	parm MAX_B_CBOE 	real count=0:1 def=--
    	parm T_B_CBOE 	 	real count=0:1 def=--

	PARMSET name=pho_hapke_hg1_dom_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm B_SHOE		real count=0:1 def=--
		parm MIN_B_SHOE		real count=0:1 def=--
		parm MAX_B_SHOE		real count=0:1 def=--
		parm T_B_SHOE		real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm HG1_SOIL	 	real count=0:1 def=--
		parm MIN_HG1_SOIL	real count=0:1 def=--
		parm MAX_HG1_SOIL	real count=0:1 def=--
		parm T_HG1_SOIL	 	real count=0:1 def=--
    		parm H_CBOE 	 	real count=0:1 def=--
    		parm MIN_H_CBOE 	real count=0:1 def=--
    		parm MAX_H_CBOE 	real count=0:1 def=--
    		parm T_H_CBOE 	 	real count=0:1 def=--
    		parm B_CBOE 	 	real count=0:1 def=--
    		parm MIN_B_CBOE 	real count=0:1 def=--
    		parm MAX_B_CBOE 	real count=0:1 def=--
    		parm T_B_CBOE 	 	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_hg1_dom.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(B_SHOE)=0)
	 					let B_SHOEt=2.0
	  else
		let B_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_B_SHOE)=0)
	 					let MIN_B_SHOEt=0.0
	  else
		let MIN_B_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_B_SHOE)=0)
	 					let MAX_B_SHOEt=10.0
	  else
		let MAX_B_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_B_SHOE)=0)
	 					let T_B_SHOEt=1.0
	  else
		let T_B_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 					let HG1_SOILt=-0.26
	  else
		let HG1_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG1_SOIL)=0)
	 					let MIN_HG1_SOILt=-1.0
	  else
		let MIN_HG1_SOILt=&MIN_HG1_SOIL
	  end-if

	  if ($count(MAX_HG1_SOIL)=0)
	 					let MAX_HG1_SOILt=1.0
	  else
		let MAX_HG1_SOILt=&MAX_HG1_SOIL
	  end-if

	  if ($count(T_HG1_SOIL)=0)
	 					let T_HG1_SOILt=0.2
	  else
		let T_HG1_SOILt=&T_HG1_SOIL
	  end-if


	  if ($count(H_CBOE)=0)
	 					let H_CBOEt=0.06
	  else
		let H_SHOEt=&H_CBOE
	  end-if

	  if ($count(MIN_H_CBOE)=0)
	 					let MIN_H_CBOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_CBOE
	  end-if

	  if ($count(MAX_H_CBOE)=0)
	 					let MAX_H_CBOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_CBOE
	  end-if

	  if ($count(T_H_CBOE)=0)
	 					let T_H_CBOEt=0.2
	  else
		let T_H_SHOEt=&T_H_CBOE
	  end-if


	  if ($count(B_CBOE)=0)
	 					let B_CBOEt=1.0
	  else
		let B_CBOEt=&B_CBOE
	  end-if

	  if ($count(MIN_B_CBOE)=0)
	 					let MIN_B_CBOEt=0.0
	  else
		let MIN_B_CBOEt=&MIN_B_CBOE
	  end-if

	  if ($count(MAX_B_CBOE)=0)
	 					let MAX_B_CBOEt=10.0
	  else
		let MAX_B_CBOEt=&MAX_B_CBOE
	  end-if

	  if ($count(T_B_CBOE)=0)
	 					let T_B_CBOEt=10.0
	  else
		let T_B_CBOEt=&T_B_CBOE
	  end-if




	   tutor pho_hapke_hg1_dom_sub 					    +
		|restore=pho_hapke_hg1_dom.par, save=pho_hapke_hg1_dom.par| +
	    		W_SOIL=&W_SOILt					  +
	    		MIN_W_SOIL=&MIN_W_SOILt				  +
	    		MAX_W_SOIL=&MAX_W_SOILt				  +
	    		T_W_SOIL=&T_W_SOILt				  +
	   		H_SHOE=&H_SHOEt					  + 
	   		MIN_H_SHOE=&MIN_H_SHOEt				  + 
	   		MAX_H_SHOE=&MAX_H_SHOEt				  + 
	   		T_H_SHOE=&T_H_SHOEt				  + 
	   		B_SHOE=&B_SHOEt					  + 
	   		MIN_B_SHOE=&MIN_B_SHOEt				  + 
	   		MAX_B_SHOE=&MAX_B_SHOEt				  + 
	   		T_B_SHOE=&T_B_SHOEt				  + 
	    		THETA=&THETAt					  + 
	    		MIN_THETA=&MIN_THETAt				  + 
	    		MAX_THETA=&MAX_THETAt				  + 
	    		T_THETA=&T_THETAt				  + 
	    		HG1_SOIL=&HG1_SOILt 				  +
	    		MIN_HG1_SOIL=&MIN_HG1_SOILt			  + 
	    		MAX_HG1_SOIL=&MAX_HG1_SOILt 			  +
	    		T_HG1_SOIL=&T_HG1_SOILt	 			  +
	   		H_CBOE=&H_CBOEt					  + 
	   		MIN_H_CBOE=&MIN_H_CBOEt				  + 
	   		MAX_H_CBOE=&MAX_H_CBOEt				  + 
	   		T_B_CBOE=&T_B_CBOEt 				  +
	   		H_CBOE=&H_CBOEt					  + 
	   		MIN_B_CBOE=&MIN_B_CBOEt				  + 
	   		MAX_B_CBOE=&MAX_B_CBOEt				  + 
	   		T_H_CBOE=&T_H_CBOEt


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_HG1_DOM function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE_HG1_DOM' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the Hapke-1986-one-term-Henyey-Greenstein photometric function containing 
the coherent backscattering term in the form of Dominique 1992.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Deborah Dominique, A Simple Method for Comparing Shadow-Hiding and Coherent 
  Backscatter Models of the Opposition Effect
  The 24th Annual Meeting of the Division for Planetary Sciences, 
  12-16 October 1992, Munich, Germany, 
  Bulletin of the American Astronomical Society, 
  Annual Report of the AAS, DPS Abstracts, 
  DDA/HAD Abstracts, Vol. 24, No. 3, 1992, p.958
Bruce Hapke, Coherent Backscatter and the Radar Characteristics of Outer Planet 
  Satellites,
  Icarus, Vol. 88, 407-417, 1990
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
  3. Correction for Macroscopic Roughness
  Icarus, Vol. 59, p.41-59, 1984


PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of opposition magnitude

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.VARI H_CBOE
Width of opposition surge
due by coherent backscatter

.VARI MIN_H_CBOE
Minimum of width of opposition surge
due by coherent backscatter

.VARI MAX_H_CBOE
Maximum of width of opposition surge
due by coherent backscatter

.VARI T_H_CBOE
Temperatur of 
width of opposition surge
due by coherent backscatter

.VARI B_CBOE
Opposition magnitude
due by coherent backscatter

.VARI MIN_B_CBOE
Minimum of opposition magnitude
due by coherent backscatter

.VARI MAX_B_CBOE
Maximum of opposition magnitude
due by coherent backscatter

.VARI T_B_CBOE
Temperatur of 
opposition magnitude
due by coherent backscatter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI MIN_H_CBOE
This parameter gives the absolut lower limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI MAX_H_CBOE
This parameter gives the absolut upper limit of the parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI T_H_CBOE
This parameter gives temperatur for the parameter of the coherent 
backscattering ( width of theopposition surge due to the backscatter ).
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_CBOE_NEW = T_H_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_CBOE_NEW_* = T_H_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI MIN_B_CBOE
This parameter gives the absolut lower limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI MAX_B_CBOE
This parameter gives the absolut upper limit of the opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.VARI T_B_CBOE
This parameter gives temperatur for the opposition magnitude coefficient of the 
coherent backscattering (height of opposition surge due to backscatter). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_CBOE_NEW = T_B_CBOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_CBOE_NEW_* = T_B_CBOE_OLD_* * scale, 
scale depends of NUMTEN.

.end
$!-----------------------------------------------------------------------------
$ create photfit2_regner_hapke_hg1.pdf
procedure option=selftutor help=*

	parm W_SOILt 		real count=0:1 			def=0.3
	parm MIN_W_SOILt 	real count=0:1 			def=0.0
	parm MAX_W_SOILt 	real count=0:1 			def=1.0
	parm T_W_SOILt 		real count=0:1 			def=0.2
	parm H_SHOEt		real count=0:1 			def=0.06
	parm MIN_H_SHOEt	real count=0:1 			def=0.0
	parm MAX_H_SHOEt	real count=0:1 			def=2.0
	parm T_H_SHOEt		real count=0:1 			def=0.2
	parm B_SHOEt		real count=0:1 			def=2.0
	parm MIN_B_SHOEt	real count=0:1 			def=0.0
	parm MAX_B_SHOEt	real count=0:1 			def=10.0
	parm T_B_SHOEt		real count=0:1 			def=1.0
	parm THETAt	 	real count=0:1 			def=20
	parm MIN_THETAt		real count=0:1 			def=0
	parm MAX_THETAt		real count=0:1 			def=60.0
	parm T_THETAt		real count=0:1 			def=6.0
	parm HG1_SOILt		real count=0:1 			def=-0.26
	parm MIN_HG1_SOILt	real count=0:1 			def=-1.0
	parm MAX_HG1_SOILt	real count=0:1 			def=1.0
	parm T_HG1_SOILt	real count=0:1 			def=0.2
	parm W_ATMt		real count=0:1 			def=0.78
	parm MIN_W_ATMt		real count=0:1 			def=0.0
	parm MAX_W_ATMt		real count=0:1 			def=1
	parm T_W_ATMt		real count=0:1 			def=0.2
	parm TAU_ATMt		real count=0:1 			def=0.05
	parm MIN_TAU_ATMt	real count=0:1 			def=0.0
	parm MAX_TAU_ATMt	real count=0:1 			def=10
	parm T_TAU_ATMt		real count=0:1 			def=0.1
	parm HG1_ATMt		real count=0:1 			def=0.35
	parm MIN_HG1_ATMt	real count=0:1 			def=-1.0
	parm MAX_HG1_ATMt	real count=0:1 			def=1.0
	parm T_HG1_ATMt		real count=0:1 			def=0.2

	parm W_SOIL 	 	real count=0:1 def=--
	parm MIN_W_SOIL 	real count=0:1 def=--
	parm MAX_W_SOIL 	real count=0:1 def=--
	parm T_W_SOIL 	 	real count=0:1 def=--
	parm H_SHOE	 	real count=0:1 def=--
	parm MIN_H_SHOE	 	real count=0:1 def=--
	parm MAX_H_SHOE	 	real count=0:1 def=--
	parm T_H_SHOE	 	real count=0:1 def=--
	parm B_SHOE		real count=0:1 def=--
	parm MIN_B_SHOE		real count=0:1 def=--
	parm MAX_B_SHOE		real count=0:1 def=--
	parm T_B_SHOE		real count=0:1 def=--
	parm THETA	 	real count=0:1 def=--
	parm MIN_THETA	 	real count=0:1 def=--
	parm MAX_THETA	 	real count=0:1 def=--
	parm T_THETA	 	real count=0:1 def=--
	parm HG1_SOIL	 	real count=0:1 def=--
	parm MIN_HG1_SOIL	real count=0:1 def=--
	parm MAX_HG1_SOIL	real count=0:1 def=--
	parm T_HG1_SOIL	 	real count=0:1 def=--
	parm W_ATM		real count=0:1 def=--
	parm MIN_W_ATM		real count=0:1 def=--
	parm MAX_W_ATM		real count=0:1 def=--
	parm T_W_ATM		real count=0:1 def=--
	parm TAU_ATM	 	real count=0:1 def=--
	parm MIN_TAU_ATM	real count=0:1 def=--
	parm MAX_TAU_ATM	real count=0:1 def=--
	parm T_TAU_ATM	 	real count=0:1 def=--
	parm HG1_ATM	 	real count=0:1 def=--
	parm MIN_HG1_ATM	real count=0:1 def=--
	parm MAX_HG1_ATM	real count=0:1 def=--
	parm T_HG1_ATM	 	real count=0:1 def=--

	PARMSET name=photfit2_regner_hapke_hg1_sub help=*

		parm W_SOIL 	 	real count=0:1 def=--
		parm MIN_W_SOIL 	real count=0:1 def=--
		parm MAX_W_SOIL 	real count=0:1 def=--
		parm T_W_SOIL 	 	real count=0:1 def=--
		parm H_SHOE	 	real count=0:1 def=--
		parm MIN_H_SHOE	 	real count=0:1 def=--
		parm MAX_H_SHOE	 	real count=0:1 def=--
		parm T_H_SHOE	 	real count=0:1 def=--
		parm B_SHOE		real count=0:1 def=--
		parm MIN_B_SHOE		real count=0:1 def=--
		parm MAX_B_SHOE		real count=0:1 def=--
		parm T_B_SHOE		real count=0:1 def=--
		parm THETA	 	real count=0:1 def=--
		parm MIN_THETA	 	real count=0:1 def=--
		parm MAX_THETA	 	real count=0:1 def=--
		parm T_THETA	 	real count=0:1 def=--
		parm HG1_SOIL	 	real count=0:1 def=--
		parm MIN_HG1_SOIL	real count=0:1 def=--
		parm MAX_HG1_SOIL	real count=0:1 def=--
		parm T_HG1_SOIL	 	real count=0:1 def=--
		parm W_ATM		real count=0:1 def=--
		parm MIN_W_ATM		real count=0:1 def=--
		parm MAX_W_ATM		real count=0:1 def=--
		parm T_W_ATM		real count=0:1 def=--
		parm TAU_ATM	 	real count=0:1 def=--
		parm MIN_TAU_ATM	real count=0:1 def=--
		parm MAX_TAU_ATM	real count=0:1 def=--
		parm T_TAU_ATM	 	real count=0:1 def=--
		parm HG1_ATM	 	real count=0:1 def=--
		parm MIN_HG1_ATM	real count=0:1 def=--
		parm MAX_HG1_ATM	real count=0:1 def=--
		parm T_HG1_ATM	 	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_regner_hapke_hg1.par


	  if ($count(W_SOIL)=0)
	 					let W_SOILt=0.3
	  else
		let W_SOILt=&W_SOIL
	  end-if

	  if ($count(MIN_W_SOIL)=0)
	 					let MIN_W_SOILt=0.0
	  else
		let MIN_W_SOILt=&MIN_W_SOIL
	  end-if

	  if ($count(MAX_W_SOIL)=0)
	 					let MAX_W_SOILt=1.0
	  else
		let MAX_W_SOILt=&MAX_W_SOIL
	  end-if

	  if ($count(T_W_SOIL)=0)
	 					let T_W_SOILt=0.2
	  else
		let T_W_SOILt=&T_W_SOIL
	  end-if


	  if ($count(H_SHOE)=0)
	 					let H_SHOEt=0.06
	  else
		let H_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_H_SHOE)=0)
	 					let MIN_H_SHOEt=0.0
	  else
		let MIN_H_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_H_SHOE)=0)
	 					Let MAX_H_SHOEt=2.0
	  else
		let MAX_H_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_H_SHOE)=0)
	 					let T_H_SHOEt=0.2
	  else
		let T_H_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(B_SHOE)=0)
	 					let B_SHOEt=2.0
	  else
		let B_SHOEt=&H_SHOE
	  end-if

	  if ($count(MIN_B_SHOE)=0)
	 					let MIN_B_SHOEt=0.0
	  else
		let MIN_B_SHOEt=&MIN_H_SHOE
	  end-if

	  if ($count(MAX_B_SHOE)=0)
	 					let MAX_B_SHOEt=10.0
	  else
		let MAX_B_SHOEt=&MAX_H_SHOE
	  end-if

	  if ($count(T_B_SHOE)=0)
	 					let T_B_SHOEt=1.0
	  else
		let T_B_SHOEt=&T_H_SHOE
	  end-if


	  if ($count(THETA)=0)
	 					let THETAt=20
	  else
		let THETAt=&THETA
	  end-if

	  if ($count(MIN_THETA)=0)
	 					let MIN_THETAt=0.0
	  else
		let MIN_THETAt=&MIN_THETA
	  end-if

	  if ($count(MAX_THETA)=0)
	 					let MAX_THETAt=60.0
	  else
		let MAX_THETAt=&MAX_THETA
	  end-if

	  if ($count(T_THETA)=0)
	 					let T_THETAt=6.0
	  else
		let T_THETAt=&T_THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 					let HG1_SOILt=-0.26
	  else
		let HG1_SOILt=&HG1_SOIL
	  end-if

	  if ($count(MIN_HG1_SOIL)=0)
	 					let MIN_HG1_SOILt=-1.0
	  else
		let MIN_HG1_SOILt=&MIN_HG1_SOIL
	  end-if

	  if ($count(MAX_HG1_SOIL)=0)
	 					let MAX_HG1_SOILt=1.0
	  else
		let MAX_HG1_SOILt=&MAX_HG1_SOIL
	  end-if

	  if ($count(T_HG1_SOIL)=0)
	 					let T_HG1_SOILt=0.2
	  else
		let T_HG1_SOILt=&T_HG1_SOIL
	  end-if


	  if ($count(W_ATM)=0)
	 						let W_ATMt=0.78
	  else
		let W_ATMt=&W_ATM
	  end-if

	  if ($count(MIN_W_ATM)=0)
	 						let MIN_W_ATMt=0.0
	  else
		let MIN_W_ATMt=&MIN_W_ATM
	  end-if

	  if ($count(MAX_W_ATM)=0)
	 						let MAX_W_ATMt=1.0
	  else
		let MAX_W_ATMt=&MIN_W_ATM
	  end-if

	  if ($count(T_W_ATM)=0)
	 						let T_W_ATMt=0.2
	  else
		let T_W_ATMt=&T_W_ATM
	  end-if


	  if ($count(TAU_ATM)=0)
	 						let TAU_ATMt=0.05
	  else
		let TAU_ATMt=&TAU_ATM
	  end-if

	  if ($count(MIN_TAU_ATM)=0)
	 						let MIN_TAU_ATMt=0.0
	  else
		let MIN_TAU_ATMt=&MIN_TAU_ATM
	  end-if

	  if ($count(MAX_TAU_ATM)=0)
	 					let MAX_TAU_ATMt=10.0
	  else
		let TAU_ATMt=&TAU_ATM
	  end-if

	  if ($count(T_TAU_ATM)=0)
	 						let T_TAU_ATMt=0.1
	  else
		let T_TAU_ATMt=&T_TAU_ATM
	  end-if


	  if ($count(HG1_ATM)=0)
	 						let HG1_ATMt=0.35
	  else
		let HG1_ATMt=&HG1_ATM
	  end-if

	  if ($count(MIN_HG1_ATM)=0)
	 					let MIN_HG1_ATMt=-1.0
	  else
		let MIN_HG1_ATMt=&MIN_HG1_ATM
	  end-if

	  if ($count(MAX_HG1_ATM)=0)
	 						let MAX_HG1_ATMt=1.0
	  else
		let MAX_HG1_ATMt=&MAX_HG1_ATM
	  end-if

	  if ($count(T_HG1_ATM)=0)
	 						let T_HG1_ATMt=0.2
	  else
		let T_HG1_ATMt=&T_HG1_ATM
	  end-if



	   tutor photfit2_regner_hapke_hg1_sub 	   			+
		|restore=pho_regner_hapke_hg1.par, 			+
		    save=pho_regner_hapke_hg1.par| 			+
	    		W_SOIL=&W_SOILt					+
	    		MIN_W_SOIL=&MIN_W_SOILt				+
	    		MAX_W_SOIL=&MAX_W_SOILt				+
	    		T_W_SOIL=&T_W_SOILt				+
	   		H_SHOE=&H_SHOEt					+ 
	   		MIN_H_SHOE=&MIN_H_SHOEt				+ 
	   		MAX_H_SHOE=&MAX_H_SHOEt				+ 
	   		T_H_SHOE=&T_H_SHOEt				+ 
	   		B_SHOE=&B_SHOEt					+ 
	   		MIN_B_SHOE=&MIN_B_SHOEt				+ 
	   		MAX_B_SHOE=&MAX_B_SHOEt				+ 
	   		T_B_SHOE=&T_B_SHOEt				+ 
	    		THETA=&THETAt					+ 
	    		MIN_THETA=&MIN_THETAt				+ 
	    		MAX_THETA=&MAX_THETAt				+ 
	    		T_THETA=&T_THETAt				+ 
	    		HG1_SOIL=&HG1_SOILt 				+
	    		MIN_HG1_SOIL=&MIN_HG1_SOILt			+ 
	    		MAX_HG1_SOIL=&MAX_HG1_SOILt 			+
	    		T_HG1_SOIL=&T_HG1_SOILt	  			+
	    		W_ATM=&W_ATMt 					+
	    		MIN_W_ATM=&MIN_W_ATMt 				+
	    		MAX_W_ATM=&MAX_W_ATMt 				+
	    		T_TAU_ATM=&T_TAU_ATMt 				+
	    		W_ATM=&W_ATMt 					+
	    		MIN_TAU_ATM=&MIN_TAU_ATMt 			+
	    		MAX_TAU_ATM=&MAX_TAU_ATMt 			+
	    		T_W_ATM=&T_W_ATMt 				+
	    		HG1_ATM=&HG1_ATMt 				+
	    		MIN_HG1_ATM=&MIN_HG1_ATMt		 	+
	    		MAX_HG1_ATM=&MAX_HG1_ATMt			+
			T_HG1_ATM=&T_HG1_ATMt 

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the REGNER_HAPKE_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHOTFIT2_REGNER_HAPKE_HG1' sub-menu 

.help

PURPOSE:

In this PDF, the user is asked for the parameters and there limits needed to 
fit the combined photometric function of the system atmosphere/surface as 
proposed by P.Regner. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.page

MATHEMATICAL BACKGROUND :

The approximation from Van Blerkom for the atmosphere is combined with the 
Hapke-1986-one-term-Henyey-Greenstein photometric function for the surface.


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

  Peter Regner, Photometric Analysis for the Determination of Physical and 
  Structural Properties of the Martian Surface in the Oxia Palus Region, 
  Thesis University Munich, DLR-FB 90-29, 1990

 Van Blerkom, D. J., The effect of haze on the visibility of Martian surface 
 features, ICARUS,vol.14, pp. 235-244, 1971

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Single-scattering albedo

.var MIN_W_SOIL 
Minimum of single-scattering albedo

.var MAX_W_SOIL 
Maximum of single-scattering albedo

.var T_W_SOIL 
Temperatur of tingle-scattering albedo

.var H_SHOE
Width of opposition surge

.var MIN_H_SHOE
Minimum of width of opposition surge

.var MAX_H_SHOE
Maximum of width of opposition surge

.var T_H_SHOE
Temperatur of width of opposition surge

.VARI B_SHOE
Opposition magnitude

.VARI MIN_B_SHOE
Minimum of opposition magnitude

.VARI MAX_B_SHOE
Maximum of opposition magnitude

.VARI T_B_SHOE
Temperatur of opposition magnitude

.var THETA
Topographic slope angle

.var MIN_THETA
Minimum of topographic slope angle

.var MAX_THETA
Maximum of topographic slope angle

.var T_THETA
Temperatur of topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term

.var MIN_HG1_SOIL
Minimum of Henyey-Greenstein term

.var MAX_HG1_SOIL
Maximum of Henyey-Greenstein term

.var T_HG1_SOIL
Temperatur of Henyey-Greenstein term

.VARI W_ATM
Atmospheric single scattering albedo

.VARI MIN_W_ATM
Minimum of 
atmospheric single scattering albedo


.VARI MAX_W_ATM
Maximum of 
atmospheric single scattering albedo

.VARI T_W_ATM
Temperatur of 
atmospheric ingle scattering albedo

.VARI TAU_ATM
Atmospheric optical depth

.VARI MIN_TAU_ATM
Minimum of 
atmospheric optical depth

.VARI MAX_TAU_ATM
Maximum of 
atmospheric optical depth

.VARI T_TAU_ATM
Temperatur of  
atmospheric optical depth

.VARI HG1_ATM
Atmospheric Henyey-Greenstein term

.VARI MIN_HG1_ATM
Minimum of 
atmospheric Henyey-Greenstein term

.VARI MAX_HG1_ATM
Maximum of 
atmospheric Henyey-Greenstein term

.VARI T_HG1_ATM
Temperatur of
atmospheric Henyey-Greenstein term

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.var MIN_W_SOIL 
This parameter gives the absolut lower limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_W_SOIL 
This parameter gives the absolut upper limit of the single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_W_SOIL 
This parameter gives temperatur for the single-scattering albedo of the soil 
particles. It characterizes the efficiency of an average particle to scatter 
and absorb light. 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_SOIL_NEW = T_W_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_SOIL_NEW_* = T_W_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.var MIN_H_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_H_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_H_SHOE
This parameter gives temperatur for the parameter which characterizes the soil 
structure (angular width of the opposition surge due to shadowing). 
This parameter gives the range over which random guesses can be expected to 
vary at first:
    H_SHOE_NEW = T_H_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_H_SHOE_NEW_* = T_H_SHOE_OLD_* * scale, 
scale depends of NUMTEN.

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI MIN_B_SHOE
This parameter gives the absolut lower limit of the parameter which characterizes the opposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_B_SHOE
This parameter gives the absolut upper limit of the parameter which characterizes theopposition magnitude coefficient.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.
 
.VARI T_B_SHOE
This parameter gives temperatur for the parameter which characterizes the 
opposition magnitude coefficient.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    B_SHOE_NEW = T_B_SHOE * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_B_SHOE_NEW_* = T_B_SHOE_OLD_* * scale, 
scale depends of NUMTEN.
 
.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.var MIN_THETA
This parameter gives the absolut lower limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_THETA
This parameter gives the absolut upper limit of the average topographic slope angle of surface roughness at subresolution scale.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_THETA
This parameter gives temperatur for the average topographic slope angle of 
surface roughness at subresolution scale.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    THETA_NEW = T_THETA * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_THETA_NEW_* = T_THETA_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
One of the classical Hapke parameter. 

.var MIN_HG1_SOIL
This parameter gives the absolut lower limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var MAX_HG1_SOIL
This parameter gives the absolut upper limit of the parameter of the first term of the Henyey-Greenstein soil particle 
phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.var T_HG1_SOIL
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein soil particle phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_SOIL_NEW = T_HG1_SOIL * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_SOIL_NEW_* = T_HG1_SOIL_OLD_* * scale, 
scale depends of NUMTEN.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI MIN_W_ATM
This parameter gives the absolut lower limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_W_ATM
This parameter gives the absolut upper limit of the single scattering albedo of 
the atmospheric aerosols.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_W_ATM
This parameter gives temperatur for the single scattering albedo of the 
atmospheric aerosols.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    W_ATM_NEW = T_W_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_W_ATM_NEW_* = T_W_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI MIN_TAU_ATM
This parameter gives the absolut lower limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_TAU_ATM
This parameter gives the absolut upper limit of the optical depth of the 
atmosphere.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_TAU_ATM
This parameter gives temperatur for the optical depth of the atmosphere.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    TAU_ATM_NEW = T_TAU_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_TAU_ATM_NEW_* = T_TAU_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI MIN_HG1_ATM
This parameter gives the absolut lower limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI MAX_HG1_ATM
This parameter gives the absolut upper limit of the parameter of the first term 
of the Henyey-Greenstein atmospheric phase function.
If a sulution guess falls out-of-bonds then the attemp will be aborted and 
a new guess attempted.

.VARI T_HG1_ATM
This parameter gives temperatur for the parameter of the first term of the 
Henyey-Greenstein atmospheric phase function.
This parameter gives the range over which random guesses can be expected to 
vary at first:
    HG1_ATM_NEW = T_HG1_ATM * tan( PI * ran_num + PI/2 ).
As the system cools the range will constrict gradually :
	T_HG1_ATM_NEW_* = T_HG1_ATM_OLD_* * scale, 
scale depends of NUMTEN.

.end
$ Return
$!#############################################################################
$Imake_File:
$ create photfit2.imake
#define PROGRAM photfit2
#define MODULE_LIST photfit2.c
#define MAIN_LANG_C
#define USES_ANSI_C
#define P2LIB

/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define TEST
#define LIB_LOCAL
#define DEBUG
#define LIB_P2SUB_DEBUG

*******************************************/

#define LIB_P2SUB
#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$Source_File:
$ create photfit2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <time.h> 
#include "vicmain_c"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "pho.h"
#include "zmabend.h"
#include <string.h>
#include <stdlib.h>

/* Program PHOTFIT2  */

#define D_EPS 1.0e-10
#define TEMP_RANGE 0.5
#define phoCAT_TYPE_1 "PHOCAT1"
#define phoCAT_TYPE "phocat"
#define phoCAT_NAME_LENGTH 6
#ifndef phoFILENAME_LENGTH
#define phoFILENAME_LENGTH 72
#endif
#ifndef phoINP_FILE_NUM
#define phoINP_FILE_NUM 20
#endif

#define phoCAT_NAME_LENGTH 6
#ifndef phoINVALID_CLASS_ID
#define phoINVALID_CLASS_ID -1013
#endif
#ifndef phoROW_OUT_OF_RANGE
#define phoROW_OUT_OF_RANGE -1012
#endif
#ifndef phoNO_CLASS_MATCH
#define phoNO_CLASS_MATCH -1014
#endif
#ifndef phoBAD_POINT
#define phoBAD_POINT -1015
#endif

/* from pho_private.h */
typedef struct  {
	char func_name[phoMAX_FUNC_NAME_LENGTH+1];
	double func_params[phoMAX_PARAM_PER_FUNC];
	char flag_set[phoMAX_PARAM_PER_FUNC];
	char flag_chg[phoMAX_PARAM_PER_FUNC];
	char flag_func_set[1];
	char flag_func_chg[1];
	char flag_mode[phoMax_MODE_NAME_LENGTH+1];
	}
PHO_STRUCT;
/* from pho_routines.c */
extern int phoInit(PHO *pho_obj);
extern int phoGetParms(PHO pho_obj);
extern int phoGetFunc(PHO_STRUCT *pho, char func_name[phoMAX_FUNC_NAME_LENGTH+1]);
extern int phoGetKeys(PHO_STRUCT *pho, char keywds[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1], int *num_kwd);
extern int phoGetVal(PHO_STRUCT *pho, char key[phoMAX_KEYWD_LENGTH+1], double *val);
extern int phoBidiRef(PHO_STRUCT *pho_obj, PHO_ILLUM *illum, double *phoFuncVal);
extern int phoFree(PHO pho_obj);
extern int phoSetVal(PHO_STRUCT *pho, char key[phoMAX_KEYWD_LENGTH+1], double val);

/* from rangen.c */
extern void zrangen(long*idum,float*rand_num);

int phocat1Open(int VicarUnit, int *IbisUnit, int *nrow, int *ncol);
int phocatRead(int IbisUnit, int *row, int *class_id, PHO_ILLUM *illum, double *phoFuncVal, double *eps);
int phocat1Close(int VicarUnit, int IbisUnit);
int phoCost(PHO pho_obj, double *x[2],PHO_ILLUM *illum_array, double limits[2][phoMAX_PARAM_PER_FUNC],
	    int npts, double tolerance, double minData,	double *error);
int phoMetropolis(PHO pho_obj,double limits[2][phoMAX_PARAM_PER_FUNC],double range[phoMAX_PARAM_PER_FUNC],
		  double *x[2],PHO_ILLUM *illum_array,int npts,double tolerance,double minData,int numIter,
		  int numTen,int numNorm,int numPrint,double *xcost);

void main44()
{
  int cnt, l, i, j, num, status, catReadStatus, count, npts, tnpts=0, ipts=0, n=0, least;
  int row, nrow, allrow=0, ncol, inpCount;
  int class_id, pdfClass_id, classCount;
  int rerun, maxIter, numTen, numNorm, numPrint;
  float temp;
  double dval, dval1, *phoFuncVal, eps;
  double phoParMin[phoMAX_PARAM_PER_FUNC], phoParMax[phoMAX_PARAM_PER_FUNC];
  double limits[2][phoMAX_PARAM_PER_FUNC];
  double phoParT[phoMAX_PARAM_PER_FUNC], percent, toleranc; 
  double  *x[2]; 
  double xcost;
  double mean[phoMAX_PARAM_PER_FUNC], std_dev[phoMAX_PARAM_PER_FUNC];
  double *res[phoMAX_PARAM_PER_FUNC];
  double reflErr, sumerr=0;
  char cval1[133], msg[133];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 
  char keyListMin[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+4]; 
  char keyListMax[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+4]; 
  char keyListT[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+2];
  char fileName[phoINP_FILE_NUM][phoFILENAME_LENGTH+1];
  PHO pho_obj;
  PHO_ILLUM illum;
  int *VicarUnit, *IbisUnit;

  PHO_ILLUM *illum_array; 

  zvmessage(" program PHOTFIT2", "");
  zvmessage( " ", "");

  zveaction("sau","");

  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** main44: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("main44 abend");
  }

  status = phoInit( &pho_obj );
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoInit failed ***","");
	    		zmabend("photfit abend");
  }

/* get the photometric function and there first parameter guess from the PDF*/
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj );
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetParms failed ***","");
	    		zmabend("photfit abend");
  }

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetFunc failed ***","");
	    		zmabend("photfit abend");
  }
  strcpy( msg, " Function =" );
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
  if(status!=phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  strcpy( msg, " parameter number = " );
  sprintf( cval1, " %d", num);
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

/* allocate memory for the parameters, limits,temperaturs */

  status = phoGetKeys( pho_obj, keylist, &num);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  if ( zvptst( "PRINT" ) )
  {

    zvmessage( " ", "");
    zvmessage( " First guess and there limits: ", "");
    zvmessage( " ----------------------------- ", "");
    zvmessage( " ", "");
  }

/* get the first guess, there limits and temperaturs */

  for (i=0; i<num; i++) /* for all function parameters */
  {
    /* first parameter guess */
    status = phoGetVal( pho_obj, keylist[i], &dval1);
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
    }
    strcpy( keyListMin[i], "MIN_");
    strcat( keyListMin[i], keylist[i]); 

    /* parameter minimum */
    status = zvp( keyListMin[i], &temp, &count);
    status = zvpcnt( keyListMin[i], &count);
    if ( count != 0 )
    { 
	phoParMin[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Minimum limit was not spezified ***","");
	zmabend("photfit abend");
    }
	
    /* parameter maximum */
    strcpy( keyListMax[i], "MAX_");
    strcat( keyListMax[i], keylist[i]); 

    status = zvp( keyListMax[i], &temp, &count);
    status = zvpcnt( keyListMax[i], &count);
    if ( count != 0 )
    { 
	phoParMax[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Maximum limit was not spezified ***","");
	zmabend("photfit abend");
    }
	
    /* parameter temperatur */
    strcpy( keyListT[i], "T_");
    strcat( keyListT[i], keylist[i]); 

    status = zvp( keyListT[i], &temp, &count);
    status = zvpcnt( keyListT[i], &count);
    if ( count != 0 )
    { 
	phoParT[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Temperatur was not spezified ***","");
	zmabend("photfit abend");
    }

  /* Check first guess limits and temperaturs, and corrections if possible: */

    if ( phoParMin[i] > dval1 ) phoParMin[i] = dval1;
    if ( phoParMax[i] < dval1 ) phoParMax[i] = dval1;
    if ( phoParMin[i] > phoParMax[i] )
    {
	dval = phoParMin[i];
	phoParMin[i] = phoParMax[i];
	phoParMax[i] = dval;
    }
    if ( phoParMax[i] - phoParMin[i] > D_EPS )
    {
	if( phoParT[i] > TEMP_RANGE * ( phoParMax[i] - phoParMin[i] ) ) \
	   phoParT[i] = TEMP_RANGE * ( phoParMax[i] - phoParMin[i] );
    }
    else phoParT[i] = (double)0.0;
 
    /* screen output */

    if ( zvptst( "PRINT" ) )
    {

      strcpy( msg, "     " );
      strcat( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListMin[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParMin[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListMax[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParMax[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListT[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParT[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      zvmessage( " ", "");
    }

  } /* if for all function parameters */

/* get the other Parameters from the PDF : */

  status = zvpcnt( "CLASS_ID", &classCount);
  if (classCount == 1 )   status = zvp( "CLASS_ID", &pdfClass_id, &count);
  else pdfClass_id = -1;
  status = zvp( "RERUN", &rerun, &count);
  if ( count==0 || rerun<=0 ) rerun=1;
  status = zvp( "MAXITER", &maxIter, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for MAXITER " );
  status = zvp( "NUMTEN", &numTen, &count);
  if ( count==0 || numTen<=0 ) numTen = (int )floor(maxIter/4.0 + 0.5); 
  status = zvp( "NORM", &numNorm, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for NORM " );
  status = zvp( "METROP", &numPrint, &count);
  if ( count==0 || numPrint<=0 ) numPrint=0;
  status = zvp( "PERCENT", &temp, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for PERCENT " );
  percent = (double)temp;
  status = zvp( "TOLERANC", &temp, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for TOLERANC" );
  toleranc = (double)temp;

/* start IBIS */

  status = zvpcnt( "INP", &inpCount);
  if ( inpCount<= 0 ) zmabend( " photfit2 : INP file wasn't given ");
  VicarUnit = (int *)malloc( inpCount * sizeof(int) );  
		    if ( VicarUnit == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 5 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  IbisUnit = (int *)malloc( inpCount * sizeof(int) );  
		    if ( IbisUnit == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 6 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  status = zvparm( "INP", fileName, &count, &cnt, phoINP_FILE_NUM, (phoFILENAME_LENGTH+1) );


/* loop over all phocat files */

  for (j=1; j<=inpCount; j++)
  {
     status = zvunit( VicarUnit+j-1, "inp", j, NULL);
       if ( status != 1 ) zvsignal ( *(VicarUnit+j-1), status, 1 );

  /* open the photcat1 file : */

     status = phocat1Open( *(VicarUnit+j-1), IbisUnit+j-1, &nrow, &ncol );

  /* Screen output ? */

     if ( zvptst( "PRINT" ) )
     {
      	zvmessage( " ", "");
      	strcpy( msg, "  " );
      	sprintf( cval1, " %d", j);
      	strcat( msg, cval1);
      	strcat( msg, ".File: ");
      	strcat( msg, &fileName[j-1][0]);
      	zvmessage( msg, "");

      	if (classCount == 1 )  /* if CLASS_ID is given */
      	{
            zvmessage(\
        "   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS",\
        "");
      	} /* if CLASS_ID is given */
      	else /* else if CLASS_ID isn't given */
      	{
            zvmessage(\
        "   ROW     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS",\
        "");
      	}  /* else if CLASS_ID isn't given */

    } /*if ( zvptst( "PRINT" ) ) */

    /* read the phocat IBIS-file : */

    row = 1;	
    npts = 0;
    do		/* loop throw all rows */
    {
        class_id = pdfClass_id;
        catReadStatus = phocatRead( *(IbisUnit+j-1), &row, &class_id, &illum, \
                                   phoFuncVal, &eps);
        if ( catReadStatus == phoROW_OUT_OF_RANGE || \
	     catReadStatus == phoNO_CLASS_MATCH )
        {
             zvmessage("no more points found in this file","");
             break;
        }
        if (  catReadStatus == phoBAD_POINT ) 
    	{
             row = row + 1;
             continue;
    	}

    	/* Screen output ? */

        if (zvptst("PRINT")) 
    	{
      	    strcpy( msg, " " );
     	    sprintf( cval1, " %4i", row);
      	    strcat( msg, cval1);
	    if (classCount == 1 )
	    {
     	  	sprintf( cval1, " %10i", class_id);
      	  	strcat( msg, cval1);
	    } /* if (classCount == 1 ) */
      	    sprintf( cval1, " %10.3e", *phoFuncVal);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos(illum.cos.inc));
      	    sprintf( cval1, " %9.4f", dval1);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos( illum.cos.em ));
      	    sprintf( cval1, " %9.4f", dval1);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos( illum.cos.phas ));
     	    sprintf( cval1, " %9.4f", dval1); 
     	    strcat( msg, cval1);
            sprintf( cval1, " %10.3e", eps); 
            strcat( msg, cval1);

      	   zvmessage( msg, "");

    	} /*  if (zvptst("PRINT")) */

    	row = row + 1;
   	npts = npts + 1; 	 /* number of good points in this IBIS file */
    	tnpts = tnpts + 1; /* number of gool points in all all IBIS files */

    } while (row <= nrow ); /* loop throw all rows */

    if (zvptst("PRINT")) 
    {
      strcpy( msg, "  " );
      sprintf( cval1, " %d",j);
      strcat (msg, cval1);
      strcat( msg,".IBIS file contains");
      sprintf( cval1, "%5i",nrow);
      strcat( msg, cval1);
      strcat( msg, " points");
      zvmessage( msg, "");
      strcpy( msg, "   Number of points left from this file =" );
      sprintf( cval1, " %5i", npts);
      strcat( msg, cval1);
      zvmessage( msg, "");
    } /*  if (zvptst("PRINT")) */

    allrow = allrow + nrow;

    status = phocat1Close( *(VicarUnit+j-1), *(IbisUnit+j-1));

  } /* for (j=1; j<=inpCount; j++)  loop over all phocat files  */

  zvmessage( " ", "");
  zvmessage( " ", "");
  strcpy( msg, "  Total number of input points =" );
  sprintf( cval1, " %6i", allrow);
  strcat( msg, cval1);
  zvmessage( msg, "");
  strcpy( msg, "  Total number of points left =" );
  sprintf( cval1, " %6i", tnpts);
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

/* Prepering for parameter fitting */

/* allocate memory for the buffers for metropolis */
/*  for(i=0; i<5; i++ ) */
  for(i=0; i<2; i++ ) 
  {
    x[i] = (double *)malloc( tnpts * sizeof(double));
		    if ( x[i] == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 7 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  }

  illum_array = (PHO_ILLUM *)malloc(tnpts * sizeof(PHO_ILLUM));
		    if ( illum_array == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 8 failed ***","");
	    		free(illum_array);
	    		zmabend("photfit2 abend");
		    }

/* loop over all phocat files and fill the buffers for metropolis */
  ipts = 0;
  for (j=1; j<=inpCount; j++)
  {
    status = zvunit( VicarUnit+j-1, "inp", j, NULL);
      if ( status != 1 ) zvsignal ( *(VicarUnit+j-1), status, 1 );

  /* open the photcat1 file : */

    status = phocat1Open( *(VicarUnit+j-1), IbisUnit+j-1, &nrow, &ncol );

    /* read the phocat IBIS-file : */
    row = 1;	
    npts = 0;
    do		/* loop throw all rows */
    {
       class_id = pdfClass_id;
       catReadStatus = phocatRead( *(IbisUnit+j-1), &row, &class_id, &illum, \
                                   phoFuncVal, &eps);
       if ( catReadStatus == phoROW_OUT_OF_RANGE || \
	     catReadStatus == phoNO_CLASS_MATCH ) break;

       if (  catReadStatus == phoBAD_POINT ) 
       {
	  row = row + 1;
	  continue;
       }

       /* fill the buffers */
      
      illum_array[ipts] = illum; 
       *(x[0] + ipts) = *phoFuncVal;
       *(x[1] + ipts) = eps;

       ipts = ipts + 1;
       row = row + 1;
       npts = npts + 1;

   } while (row <= nrow ); /* loop throw all rows */

    status = phocat1Close( *(VicarUnit+j-1), *(IbisUnit+j-1));
  
  } /* for (j=1; j<=inpCount; j++)  lopp over all phocat files  */

  for (i=0; i<num; i++) /* for all function parameters */
  {
    limits[0][i] = phoParMin[i];
    limits[1][i] = phoParMax[i];
  } /* for all function parameters */

  if( rerun<1 ) rerun = 1;

  for (i=0; i<num; i++)  /* for all function parameters */
  {
    std_dev[i] = 0.0;
    mean[i] = 0.0;
    res[i] = (double *)malloc( rerun * sizeof(double));
		    if ( res[i] == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 9 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  } /* for all function parameters */

  for (l=1; l<=rerun; l++) /* for statistics */
  { 

    status = phoMetropolis( pho_obj, limits, phoParT, x, illum_array, tnpts, toleranc,
			    percent, maxIter, numTen, numNorm, numPrint, &xcost); 

    if ( zvptst( "PRINT" ) )
    {
      zvmessage( " ", "");
      sprintf( cval1, " %10i", l);
      strcpy( msg, cval1);
      strcat( msg, ". Metropolis-result  :" );
      zvmessage( msg, "");
    }

    /* solution statistics : */

    for (i=0; i<num; i++) /* for all function parameters */
    {
      /* first parameter guess */
      status = phoGetVal( pho_obj, keylist[i], &dval1);
      if(status!=phoSUCCESS)
      {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
      }

      mean[i] = mean[i] + dval1;
      *(res[i] + l-1) = dval1;

      if ( zvptst( "PRINT" ) )
      {
        strcpy( msg, " " );
        strcat( msg, keylist[i]);
        strcat( msg, " = ");
        sprintf( cval1, " %10.3e", dval1);
        strcat( msg, cval1);
        zvmessage( msg, "");
      } /*end PRINT */

    } /* for all function parameters */

    if ( zvptst( "PRINT" ) ) zvmessage( " ", "");

  } /* for statistics */

  for ( i=0; i<tnpts; i++)
  {
    status = phoBidiRef(pho_obj,&illum_array[i], phoFuncVal);
    reflErr= *(x[0] + i) - *phoFuncVal;
    sumerr = sumerr + fabs(reflErr);
    if(fabs(reflErr)< toleranc) n = n+1;
  }
  sumerr = sumerr/((double )tnpts);
  least = (int )floor(0.5 + percent * tnpts / 100. );

   
  zvmessage(" ", "");
  zvmessage("          Statistical results:", "");
  zvmessage("          --------------------", "");
  zvmessage(" ", "");

  zvmessage("   INC_ANG    EM_ANG  PHAS_ANG      I/F     I/F-FitVal ", "");

  for ( i=0; i<tnpts; i++)
  {
    illum = illum_array[i];
    status = phoBidiRef(pho_obj,&illum_array[i], phoFuncVal);
    reflErr= *(x[0] + i) - *phoFuncVal;
    dval1 = RETURN_DEGREES(acos( illum.cos.inc ));
    sprintf( cval1, " %9.4f", dval1);
    strcpy( msg, cval1);
    dval1 = RETURN_DEGREES(acos( illum.cos.em ));
    sprintf( cval1, " %9.4f", dval1);
    strcat( msg, cval1);
    dval1 = RETURN_DEGREES(acos( illum.cos.phas ));
    sprintf( cval1, " %9.4f", dval1);
    strcat( msg, cval1);
    sprintf( cval1, " %10.7f", *phoFuncVal);
    strcat( msg, cval1);
    sprintf( cval1, " %12.8f", reflErr);
    strcat( msg, cval1);
    if( n >= least)
    {
	if( fabs(reflErr)< toleranc)
	{
    	   strcat( msg, "   used ");
	  
	}
	else
	{
    	   strcat( msg, " unused ");
	}

    }
    else
    {
	strcat( msg, "   used ");
    }
    zvmessage(msg, "");
  }

  strcpy( msg, "        Mean I/F error per fitted point =");
  sprintf( cval1, " %12.8f", sumerr);
  strcat( msg, cval1);
  zvmessage(msg, "");

  zvmessage( " ", "");
  zvmessage( " MEAN VALUES (to thinking about the stability of fitting) :", "");

  for (i=0; i<num; i++) /* for all function parameters */
  {
    if(rerun>1) mean[i] = mean[i] / ( (double)rerun );
    else mean[i] = mean[i] / ((double)rerun);

    for (l=1; l<=rerun; l++)
    { 
      std_dev[i] = std_dev[i]						   \
                 + (mean[i] - *(res[i] + l-1)) * (mean[i] - *(res[i] + l-1));
    }

    if(rerun>1) std_dev[i] = sqrt(std_dev[i]) / ( (double)rerun - (double)1.0);
    else std_dev[i] = sqrt(std_dev[i]) / ( (double)rerun);

    strcpy( msg, " " );
    strcat( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", mean[i]);
    strcat( msg, cval1);
    strcat( msg, " +/-");
    sprintf( cval1, " %10.3e", std_dev[i]);
    strcat( msg, cval1);
    zvmessage( msg, "");
    
  }
  zvmessage( " ", "");
  zvmessage(" ", "");

 
  zvmessage("*****************************************************************************",
	    "");
  zvmessage("* ***************************************************************************",
	    "");
  zvmessage("* *", "");
  zvmessage("* *  RESULTING PARAMETERS OF BEST-FIT: ", "");
  zvmessage("* *", "");

  for (i=0; i<num; i++) /* for all function parameters */
  {
      status = phoGetVal( pho_obj, keylist[i], &dval1);
      if(status!=phoSUCCESS)
      {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
      }

      strcpy( msg, "* *      " );
      strcat( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");

  } /* for all function parameters */
  zvmessage("* *", "");
  zvmessage("* ***************************************************************************",
	    "");
  zvmessage("*****************************************************************************",
	    "");
  zvmessage(" ", "");

  status = phoFree( pho_obj);
  free(x[0]);
  free(x[1]);
  free(illum_array);
  free(VicarUnit);
  free(IbisUnit);
  free(phoFuncVal);
  for (i=0; i<num; i++)  /* for all function parameters */
  {
    free(res[i]);
  } /* for all function parameters */
  return;
}



/************************************************************************
*									*
*		phocat1Open						*
*									*
************************************************************************/

/* opens a photometric catalog file for read as comming from PHOTTEST2 */

int phocat1Open(
	int VicarUnit, 
	int *IbisUnit,
	int *nrow,
	int *ncol)

/********************************************************************
	* VicarUnit	IBIS Unit No.
	* IbisUnit	VICAR Unit No.
	* IbisBuff	IBIS buffer
*********************************************************************/
{

  int status, count;
  char *mode;
  char org[10]=IORG_COLUMN;
  char type[phoCAT_NAME_LENGTH+1];
  char version[10];
  status=1; 
  mode = IMODE_READ;


/* Open the file : */

  status = IBISFileOpen(VicarUnit, IbisUnit, mode, 0, 0, NULL, org ); 
  	if (status != 1) IBISSignalU( VicarUnit, status, 1 );

 
/* get the number of rows and columns */

  count = IBISFileGet( *IbisUnit, IFILE_NR, nrow, 1, 1, 0 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );

  count = IBISFileGet( *IbisUnit, IFILE_NC, ncol, 1, 1, 0 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );

  if ( *nrow < 1 )
   {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** phocat file is empty --> can't read ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
  } /* if ( *nrow < 1 ) */

  if ( *ncol < 17 )
   {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** phocat file hasn't enough coloumns ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
 } /* if ( *ncol < 17 ) */





/* check which IBIS-Version : */
  
  count = IBISFileGet( *IbisUnit, IFILE_VERSION, version, 1, 1, 9 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );



  if (strcmp(version,IVERSION_1)!=0) /*  for IBIS-2 */
  {

  /* Now check to see which subtype : */

    count = IBISFileGet( *IbisUnit, IFILE_TYPE, type, 1, 1, phoCAT_NAME_LENGTH+1 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );
    if(strcmp(type, phoCAT_TYPE)!=0 || strcmp(type,"tiepoint")!=0) /*if phocat*/
    {} /* if phocat */
    else /* unknown photometric catalog file */
    {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** file isn't a phocat --> can't read as such ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
    } /* else unknown photometric catalog file */
  } /* if (strcmp(version,IVERSION_1)!=0) */


  return status; 

}



/************************************************************************
*									*
*		phocatRead						*
*									*
************************************************************************/

/* reads contens into the illu-object and from a photometric catalog file as comming */
/* from PHOTTEST2 */

int phocatRead(
	int IbisUnit, 
	int *row,
	int *class_id, 
	PHO_ILLUM *illum, 
	double *phoFuncVal,
	double *eps)
{
  int status, rstatus, count, col, ncol, nrow, i, tclass_id;
  float ftemp;
  double temp, dtemp, caz;
  char type[phoCAT_NAME_LENGTH+1];
  char ctmp1[133];
  char version[10];

  status = 1;
  rstatus = 1;


 /* Check up the number of rows and columns */

  count = IBISFileGet( IbisUnit, IFILE_NR, &nrow, 1, 1, 0 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );

  count = IBISFileGet( IbisUnit, IFILE_NC, &ncol, 1, 1, 0 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );

 


 if ( nrow < 1 )
  {
	zvmessage(" ", "");
	zvmessage("***phocatRead error***","");
	zvmessage("*** phocat file is empty --> can't read ***","");
	status = IBISFileClose( IbisUnit, NULL );
   	if (status != 1) IBISSignal( IbisUnit, status, 1 );
  } /* if ( nrow < 1 ) */





/* check which IBIS-Version : */
  
  count = IBISFileGet( IbisUnit, IFILE_VERSION, version, 1, 1, 9 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );







  if (strcmp(version,IVERSION_1))  /* if IBIS-2 */
  {

  /* Check to see which subtype : */

    count = IBISFileGet( IbisUnit, IFILE_TYPE, type, 1, 1, phoCAT_NAME_LENGTH+1 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );




    if ( strcmp(type, phoCAT_TYPE) == 0 )   /* if phocat */
    {
  	illum->type.sunshadow = illNoShadow;
  	illum->type.viewshadow = illNoShadow;
  	illum->type.mode = illEllCos;

	strcpy( ctmp1, "CLASS_ID" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );



	if (*class_id >= 0 )  /* search for class-id */
	{
	  if ( *row <= 0) *row = 1;
	  if (*row > nrow ) return rstatus=phoROW_OUT_OF_RANGE;
	  for(i = *row;  i <= nrow;  i++ )
	  {
            status = IBISColumnRead(  IbisUnit, (char *)&tclass_id, col, i, 1);

   	       if (status != 1) IBISSignal( IbisUnit, status, 1 );
	    if( tclass_id == *class_id )
	    {
		*row = i;
		break;
	    } /* if( tclass_id == *class_id ) */
	  } /* for(i=*row; i <= nrow; i++ ) */
	  if( tclass_id != *class_id )
	  {
	     *row = nrow;
	     return rstatus=phoNO_CLASS_MATCH;
	  } /* if( tclass_id == *class_id ) */

	} /* if (*class_id >= 0 ) */



	else /* read for given row */
	{
          status = IBISColumnRead(  IbisUnit, (char *)class_id, col, *row, 1);

   	     if (status != 1) IBISSignal( IbisUnit, status, 1 );
	} /* else read for given row */




	strcpy( ctmp1, "IncidenceAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 90.0 || temp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )temp));
	illum->cos.inc = dtemp;

	strcpy( ctmp1, "EmissionAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 90.0 || temp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )temp));
	illum->cos.em = dtemp;

	strcpy( ctmp1, "PhaseAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 1800.0 || temp < 00.0 ) return phoBAD_POINT;
	illum->cos.phas = cos(RETURN_RADIANS((double )temp));

	   if (illum->cos.inc < 0 || illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em < 0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 ) return phoBAD_POINT;

	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - illum->cos.inc * illum->cos.inc) <= 0.0 || fabs(1.0 - 
illum->cos.em * illum->cos.em) <= 0.0 )
		caz = 1.0;
	   else  
		caz = ( illum->cos.phas - illum->cos.em * illum->cos.inc ) / sqrt( fabs(1. - illum->cos.inc * illum->cos.inc) * fabs(1. - illum->cos.em * illum->cos.em) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) return phoBAD_POINT;


	strcpy( ctmp1, "I/F" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)phoFuncVal, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );

	/* Check if point never was computed (off picture...).		*/
	/* PHOTOM tries to keep the 1:1 order between IBIS files	*/
	/* computed in BATCH mode. Points which cannot be computed	*/
	/* are set to zero.						*/

	if ( illum->cos.inc >= 1.0  - ANG_EPS && 	\
	     illum->cos.em >= 1.0  - ANG_EPS && 	\
	     illum->cos.phas >= 1.0  - ANG_EPS && 	\
	     *phoFuncVal == 0.0) return phoBAD_POINT;


	strcpy( ctmp1, "StandDev" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)eps, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );



    } /* if phocat */
    else
    {
	zvmessage(" ", "");
	zvmessage("***phocatRead error***","");
	zvmessage("*** file isn't a phocat --> can't read as such ***","");
	status = IBISFileClose( IbisUnit, NULL );
   	if (status != 1) IBISSignal( IbisUnit, status, 1 );
    } /* else no phocat*/

  } /* if IBIS-2 */




  else		/* for IBIS-1 */
  {
     	if ( ncol < 17 )
   	{
	   zvmessage(" ", "");
	   zvmessage("***phocatRead error***","");
	   zvmessage("*** IBIS_1 phocat file hasn't enough coloumns ***","");
	   status = IBISFileClose( IbisUnit, NULL );
   	   if (status != 1) IBISSignal( IbisUnit, status, 1 );
  	} /* if ( ncol < 17 ) */


  	illum->type.sunshadow = illNoShadow;
  	illum->type.mode = illEllCos;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 11, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 ); 
	if ( ftemp >= 90.0 || ftemp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )ftemp));
	illum->cos.inc = dtemp;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 12, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( ftemp >= 90.0 || ftemp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )ftemp));
	illum->cos.em = dtemp;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 13, *row, 1);

  	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( ftemp >= 180.0 || ftemp < 0.0 ) return phoBAD_POINT;
	illum->cos.phas = cos(RETURN_RADIANS((double )ftemp));


	   if (illum->cos.inc < 0 || illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em < 0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 ) return phoBAD_POINT;

	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - illum->cos.inc * illum->cos.inc) <= 0.0 || fabs(1.0 - 
illum->cos.em * illum->cos.em) <= 0.0 )
		caz = 1.0;
	   else  
		caz = ( illum->cos.phas - illum->cos.em * illum->cos.inc ) / sqrt( fabs(1. - illum->cos.inc * illum->cos.inc) * fabs(1. - illum->cos.em * illum->cos.em) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) return phoBAD_POINT;


        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 16, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	*phoFuncVal = (double)ftemp;

	/* Check if point never was computed (off picture...).		*/
	/* PHOTOM tries to keep the 1:1 order between IBIS files	*/
	/* computed in BATCH mode. Points which cannot be computed	*/
	/* are set to zero.						*/

	if ( illum->cos.inc >= 1.0  - ANG_EPS && 	\
	     illum->cos.em >= 1.0  - ANG_EPS && 	\
	     illum->cos.phas >= 1.0  - ANG_EPS && 	\
	     *phoFuncVal == 0.0) return phoBAD_POINT;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 18, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	*eps = (double)ftemp;

	*class_id = -1;

  } /* else IBIS-1 */

  return rstatus;
}






/************************************************************************
*									*
*		phocat1Close						*
*									*
************************************************************************/

/* closes a photometric catalog file as comming from PHOTOM */

int phocat1Close(
	int VicarUnit, 
	int IbisUnit)

/********************************************************************
	* VicarUnit	IBIS Unit No.
	* IbisUnit	VICAR Unit No.
*********************************************************************/
{
  int status;

/* Now we try to close-down the IBIS file */
 
 status = IBISFileClose(IbisUnit, ICLOSE_UDELETE);

/* ... and if something is wrong we issue an IBIS warning and
	finish the subroutine as normal */

  if (status != 1) IBISSignalU(VicarUnit, status, 0);

  return status;

}




/************************************************************************
*									*
*		phoMetropolis						*
*									*
************************************************************************/

/*  The technique was conceived of by Edward Teller and Nocholas
 *  Metropolis in 1953 and in my humble opinion is the most
 *  powerful numerical minimization method in existence.
 *  I have taken the liberty to modify it as i saw fit by incorporating
 *  ideas from several authors including a few of my own.
 *
 *  HISTORY
 *   Original Programmer:               J. J. Lorre
 *   Current Cognizant Programmer:      F.Oschuetz
 *   Documentation Author:              J. J. Lorre
 *   Revision:                          Claus Groebner 8.3.95 C-Version
 *					F.Oschuetz 7/95 photometrie things
 *   
 *   REFERENCES:     	Szu H. H., SPIE 698 Real Time Signal Processing
 *                  	#9 (1986) 59 "Non-Convex Optimization"
 *		    	J.J.Lorre, Function Minimisation Whith Partially 
 *			Correct Data,
 *			J. Soc.  Ind.Appl. Math. 43, 1990, 123-127
 *
 *   ATTENTION !! ALL ARRAY ARE INITIATED BY 1 TO MAXPAR-1
 *               for the compatibility to the Marquart-Levenberg functions.
 *
 *  PARAMETER:
 *
 *  PHO pho_obj			photometric object,:
 *			input	first guess;
 *			output	solution.
 *  double limits[2][]	input	Array of upper/lowest limit of parameters.
 *  double range[]	input	Bounds the jumps of solution (temperatur).
 *  double x[2][]	input 	Array of data : I/F, [sigma]:
 *  PHO_ILLUM *illum_array input Array of data illumination objects
 *  int npts		input	The number of data points in x.
 *  double tolerance	input	Tolerance value for residuals(data,phoFuncVal).
 *  double minData	input	Minimum % of data points to be used.
 *  int numIter		input	The total number of iterations permitted.
 *  int numTen		input	Number of iterations to reduce the error by 10.
 *  int numNorm		input	The number of iterations between normalizations.
 *  int numPrint	input	The number of iterations between printouts.
 *  double *xcost	output	Returns minimal cost.
 *
 *  Function value:	Status indicator. 
 */


int phoMetropolis(
	PHO pho_obj,
	double limits[2][phoMAX_PARAM_PER_FUNC],
	double range[phoMAX_PARAM_PER_FUNC],
	double *x[2],
	PHO_ILLUM *illum_array,
	int npts,
        double tolerance, 
	double minData,
	int numIter,
	int numTen,
	int numNorm,
	int numPrint,
	double *xcost)
{
  int status, rstatus;
  int i,ii, kk, fail, loop, loop1, loop2, loop3, numreset, ind;
  int narg;
  float randout;
  double temp[phoMAX_PARAM_PER_FUNC], aa[phoMAX_PARAM_PER_FUNC];
  double mincost, costsum, tmp, prob, boltzmann = 0.0, energy;
  double answer[phoMAX_PARAM_PER_FUNC], minx[phoMAX_PARAM_PER_FUNC];
  double scale, c1, c2, c3, maxCostDif;
  double  xrd2, pi2;
  double dval;
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char msg[133], cval1[133];
  time_t tim;
  long *seed1 = NULL;

  if (seed1 != NULL )  free(seed1);
	seed1 = (long *)malloc(sizeof(long));
	if (seed1 == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phoMetropolis error***","");
	  zvmessage("*** memory allocation 10 failed ***","");
	  free(seed1);
	  zmabend("phottest abend");
	}

  pi2   = (double)0.5 * M_PI;
  xrd2  = (double)1.0 /((double)1.0);
  maxCostDif = (double)50.0;
/*  maxCostDif = 50.0 * 0.0001 *tolerance*/;

/* get the first guess for the parameters */

  status = phoGetKeys( pho_obj, 0, &narg);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  status = phoGetKeys( pho_obj, keylist, &narg);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  for (i=0; i<narg; i++) /* for all function parameters */
  {
    status = phoGetVal( pho_obj, keylist[i], answer+i);
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
    }
  } /* for all function parameters */ 


/*  Compute a random number seed based on the time of day.  */

/*  if(!(time(&tim) % 2))  tim--; */
  tim = time(NULL);
  *seed1 = (long )tim;
  while( *seed1 > 714025) *seed1 = *seed1 - 714025;
  if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  zrangen( seed1, &randout ); 
/*  zget_ran( &tim, &randout );*/

/*  Compute the cost at position answer and assign to variable c1.  */

  if (!(ind=phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c1))) 
  {
    zvmessage( " phoMetropolis: Failure in phoCost at initial guess","");
    free(seed1);
    return rstatus=ind;
  }

/*  Save the start guess position in case it was chosen wisely.  */
/*  And set initial temperatures to the range estimates.  	 */

  mincost = c1;
  for (ii=0; ii<narg; ii++)  /* for all function parameters */
  {
    minx[ii] = answer[ii];
    temp[ii] = range[ii];
  } /* for all function parameters */

  fail = 0;	/* costneu-costalt > 50/0.0001/tolerance */
  loop1 = 0;	/* Number of lower costneu */
  loop2 = 0;	/* Accepted higher costneu */
  loop3 = 0;	/* Number of to big jumps */
  numreset = numTen/10;
  if ( numreset<= (int)0 ) numreset=1;
  costsum = numTen; 
  costsum = 1.0/costsum;
  scale = pow(0.1, costsum);
/*  minscale = pow (10.0, ((double )numIter)/((double )numTen) ); 
  if ( scale < minscale) scale = minscale;
*/
  if (numPrint!=0) 
  {
    zvmessage(" ", "");   
    zvmessage(" 		phoMetropolis :", "");   
    zvmessage(" ", "");   
    strcpy( msg, " scaling factor reduces temperatur = " );
    sprintf( cval1, " %11.6e", scale);
    strcat( msg, cval1);
    zvmessage(msg, "");   
    zvmessage(" ", "");   
/*    zvmessage(\
" Boltzmann     Temperatur", ""); */
    zvmessage(\
"  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds","");

    sprintf( cval1, " %12s", keylist[0]);
    strcpy( msg, cval1 );

    for (i=1; i<narg; i++) /* for all function parameters */
    {
	  sprintf( cval1, " %12s", keylist[i]);
	  strcat( msg, cval1);
    } /* for all function parameters */

    zvmessage( msg, "");

  }  /* for numPrint!=0 */

/*  MAIN LOOP: loop on number of successful changes in solution space. */

  for (loop = 1; loop <= numIter; loop++) /* main loop */
  {
  /*  Compute the delta_cost/temperature ratio for normalization of
   *  probabilities. Note that this is the Boltzman constant for this 'system'.
   */

    if ((loop % numNorm) == 1)	/* renormalisation */
    {
      costsum = tmp = (double)0.0;
      kk = 0;
      for (ii=0; ii<narg; ii++)  aa[ii] = answer[ii];

      for (ii=0; ii<narg; ii++) /* for all parameters */
      {
        tmp += temp[ii];

        aa[ii] = answer[ii] - temp[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if( !(status == phoSUCCESS || status == phoKEYWD_CHANGED) )
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 1 failed ***","");
	    		zmabend("photfit abend");
        }

	ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);

        if( ind == 1 ) /* if parameters valid */
        {
          kk++;
          costsum += fabs(c1 - c2);
/*          tmp += temp[ii] * temp[ii]; */
/*          tmp += temp[ii]; */

        } /* if parameters valid */

        aa[ii] = answer[ii] + temp[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 2 failed ***","");
	    		zmabend("photfit abend");
        }

	ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);

        if (ind == 1 ) /* if parameters valid */
        {
          kk++;
          costsum += fabs(c1 - c2);
/*          tmp += temp[ii] * temp[ii]; */ 
/*          tmp += temp[ii]; */
        } /* if parameters valid */

        aa[ii] = answer[ii]; /*reset the parameters as befor normalisation */

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 3 failed ***","");
	    		zmabend("photfit abend");
        }

      } /* for all parameters */

      if (kk == 0) /* if parameters valid */
      {
        zvmessage(" METROPOLIS: Failure in normalization procedure: ", "");
	zvmessage(" solution + - range out of bounds", "");
        free(seed1);
        return rstatus=ind;
      } /* if parameters valid */

      tmp = tmp / (double )narg; /* mean temperatur */
      costsum = costsum /(double )kk;  

      if (tmp < 1.0e-30)    /* prevent divison by 0.0 */
        boltzmann = 1.0e-30;
      else
        boltzmann = 5.0 * costsum/tmp; 

/*      boltzmann = 5.0 * costsum /(kk * temp[1]);  */

      if (numPrint!=0)
      { 
	strcpy (msg, "  Boltzmann = ");
	sprintf( cval1, " %10.6e", boltzmann);
	strcat( msg, cval1);
	strcat( msg, "  Temperatur = ");
 	sprintf( cval1, " %10.6e", tmp);
	strcat( msg, cval1);
/*	sprintf( cval1, " %10d", kk);
	strcat( msg, cval1);
 	sprintf( cval1, " %10.6e", 5.0*costsum);
	strcat( msg, cval1); */
    	zvmessage( msg, "");

      } /* if (numPrint!=0) */

    } /* END if ((loop % numNorm) == 1) renormalisation */ 

/*  Decrement the temperature according to the multiplicative cooling schedule.
    for (ii=0; ii<narg; ii++)  
    {
	temp[ii] *= scale;
  	if ( temp[ii] < range[ii] * minscale) temp[ii] = range[ii] * minscale; 
    }

    energy = boltzmann * temp[1];
*/
    tmp = (double)0.0;
    for (ii=0; ii<narg; ii++)
    {
      temp[ii] *= scale;
/*      if ( temp[ii] < range[ii]*minscale) temp[ii] = range[ii] * minscale; */

/*      tmp += temp[ii]*temp[ii]; */
      tmp += temp[ii]; 
    }
    energy = boltzmann * tmp / ((double )narg);

/*  Compute a solution space guess using a Cauchy-Lorentzian random
 *  probability distribution function.
 */

    do  /* while( ! ind ) */
    {
      for (ii=0; ii<narg; ii++) /* new function parameters */
      {
  	if( *seed1 > 714025) *seed1 = *seed1 - 714025;
  	if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  	zrangen( seed1, &randout );
	dval = randout;
        aa[ii] = temp[ii] * tan( dval * M_PI + pi2) + answer[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 4 failed ***","");
	    		zmabend("photfit abend");
        }
      } /* new function parameters */

      ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);
 
      if (ind!=1) /* wrong function parameters */
      {
	for (ii=0; ii<narg; ii++) /* for all function parameters */
	{
	  aa[ii] = answer[ii];
	  status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
          if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
          {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 5 failed ***","");
	    		zmabend("photfit abend");
          }
	} /* for all function parameters */
	loop3++;
      } /* wrong function parameters */
      else /* valid function parameters */
      {
        if(c2 < c1) 
	{
	/*  Accept lower cost position.
 	*  We always accept a downhill cost route if offered.
 	*/
          c1 = c2;
          for (ii=0; ii<narg; ii++)  /* for all function parameters */
	  {
	    answer[ii] = aa[ii];
	    status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
            if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
            {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 6 failed ***","");
	    		zmabend("photfit abend");
            }
	  } /* for all function parameters */
          loop1++;
        }/* Accept lower cost position. */
        else 
	{
/*  Compute probability of accepting higher cost position.
 *  This comes from the Boltzman probability of our system 
 *  transitioning from energy state c1 to energy state c2.
 */
          c3 = (c2 - c1)/energy;

          if (c3 > maxCostDif) { fail++; ind = 1; }
          else 
	  {
/*            prob = 1.0/(1.0 + exp(c3));  */
	    if (c3< 1.0e-15) prob=1.0;
            else prob = 1.0/exp(c3);

/*  Evaluate the probability by comparing it against chance.  */

	    if( *seed1 > 714025) *seed1 = *seed1 - 714025;
	    if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  	    zrangen( seed1, &randout );
	    dval = randout;
            if (prob > xrd2*dval ) /*  Accept higher cost position.  */
            {	
              c1 = c2;
              for (ii=0; ii<narg; ii++)
	      {
		answer[ii] = aa[ii];
		status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
                if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
                {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 7 failed ***","");
	    		zmabend("photfit abend");
                }
	      } 
              loop2++;
            }
            else { fail++; ind = 1; }	/*  Reject higher cost position.  */
          }
        }
      } /* END loop3++ */

    } while(! ind );

/*  Save the minimum cost and associated solution as we go.  */

    if (c1 < mincost) 
    {
      mincost = c1;
      for (ii=0; ii<narg; ii++) minx[ii] = answer[ii];
    }

/*  Reset the solution pointer to the minimum cost location every numreset
 *  successful iterations.
 */
    if (!(loop % numreset)) 
    {
/*  	strcpy( msg,"reset of parameters to ones of mincost = " );
	sprintf( cval1, " %10.6e", mincost);
	strcat( msg, cval1);
	zvmessage(msg,""); 
*/
      c1 = mincost;
      for (ii=0; ii<narg; ii++)
      {
	answer[ii] = minx[ii];
	status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 8 failed ***","");
	    		zmabend("photfit abend");
        }

      }
/*        sprintf( cval1, " %10.6e", answer[0]);
  	strcpy( msg, cval1 );
	for (ii=1; ii<narg; ii++)
	{
 	  sprintf( cval1, " %10.6e", answer[ii]);
	  strcat( msg, cval1);
	}
    	zvmessage( msg, "");
*/
/*      loop++; */
    }

/*  Print out a status every PRNT iterations. */

    if (numPrint!=0) 
    {
      if (!(loop % numPrint)) 
      {

	sprintf( cval1, " %10i", loop);
  	strcpy( msg, cval1 );
	sprintf( cval1, " %10.6e", c1);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop1);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop2);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", fail);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop3);
	strcat( msg, cval1);
    	zvmessage( msg, "");

	sprintf( cval1, " %10.6e", answer[0]);
  	strcpy( msg, cval1 );
	for (ii=1; ii<narg; ii++)
	{
 	  sprintf( cval1, " %10.6e", answer[ii]);
	  strcat( msg, cval1);
	}
    	zvmessage( msg, "");

        fail = loop1 = loop2 = loop3 = 0;
      }
    }
  } /*  END of MAIN LOOP  */

/*  Put minimum solution into "answer" & it's cost into xcost. */

  for (ii=0; ii<narg; ii++) 
  {
    answer[ii] = minx[ii];
    status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
    if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
    {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal  9 failed ***","");
	    		zmabend("photfit abend");
    }
/*    range[ii] = temp[ii]; */
  }

  *xcost = mincost;
/*  free(pkeylist);*/
 
  free(seed1);

  return rstatus=ind;
}




/************************************************************************
*									*
*		phoCost							*
*									*
************************************************************************/

/* The cost function has two levels in order to reject bad data point.
 * For each observable we define a residual er=| I/F(ii) - phofuncval |
 * We the count the number of points N for which er<tolerance where tolerance 
 * is a precision thresold on the residual er.
 * Then we define the cost as the sum of residuals weighted by the standard 
 * deviation of the data :
 *
 *			   { sum[er(ii)/sigma(ii)]/N,   ii[0,N]   for N>=minNum 
 * cost(all I/F,pho_obj) = {sum[er(ii)/sigma(ii)]/npts, ii[0,npts] for N<minNum
 * 
 * where minNum is the minimum number of points acceptable. The minimum of 
 * standard deviation will be be set to 0.0001*tolerance.
 */

int phoCost(
	PHO pho_obj,	 /* input photometric object */
	double *x[2],	 /* input Array of data :  I/F, [sigma] */
	PHO_ILLUM *illum_array, /* Array of data illumination objects */
	double limits[2][phoMAX_PARAM_PER_FUNC], /* parameters minima&maxima */
	int npts, 	 /* input Number of data points in x  */
        double tolerance, /* input Maximum of acceptable residual */
	double minData,	 /* input Percent of minimal acceptable data points */
	double *error) 
{
  int status, rstatus=1; /* TRUE */
  int narg;
  int i,ii, n, minNum;
  double er, sum, sigSum, sig, sigma, sig2, ref, minSigma, *phoFuncVal;
  double a[phoMAX_PARAM_PER_FUNC];
/*  char *pkeylist;*/
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  PHO_ILLUM illum;

  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** phoCost: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("phoCost abend");
  }

/* get the photometric parameters */

  status = phoGetKeys( pho_obj, 0, &narg);
  if(status!=1) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
		    }


/*  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
		    if ( pkeylist == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** memory allocation 11 failed ***","");
	    		free(pkeylist);
	    		zmabend("photfit abend");
		    }

  pkeylist = (char *)keylist;
*/
  status = phoGetKeys( pho_obj, keylist, &narg);
  if(status!=1) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
		    }
  for (i=0; i<narg; i++) /* for all function parameters */
  {
    status = phoGetVal( pho_obj, keylist[i], a+i);
  if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
		    }


/* strcpy( msg," min: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", limits[0][i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
  	  strcpy( msg," max: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", limits[1][i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
  	  strcpy( msg," par: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
*/

    if(a[i] < limits[0][i] || a[i] > limits[1][i]) 
    {
/*      free(pkeylist); */

/*	  strcpy(  msg, "abort : ");
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
	  strcat( msg, "Min_" );
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", limits[0][i]);
	  strcat( msg, cval1);
	  strcat( msg, " Max_" );
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", limits[1][i]);
	  strcat( msg, cval1);
    	  zvmessage( msg, "");
*/
      free(phoFuncVal);
      return rstatus=0;
    }

/*	  strcpy( msg, keylist[i] );
	  strcat( msg, " = ");
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
    	  zvmessage( msg, "");
*/
  } 
/*    	  zvmessage( " ", "");*/

  n = 0;
  *error = sum = 0.0;
  sig = sigSum = 0.0;
/*
  ref = 0.01 * limits[0][phoMAX_PARAM_PER_FUNC] * npts;
  tolerance = limits[1][phoMAX_PARAM_PER_FUNC];
*/
  minSigma = 0.0001 * tolerance;
  ref = npts * minData;
  if (ref < 0.0) minNum = ref - 0.5;
  else           minNum = ref + 0.5;

/*  illum->type.sunshadow = illNoShadow;
  illum->type.mode = illEllCos;
*/

  for (ii=0; ii<npts; ii++) 
  {
   illum = illum_array[ii];

    status = phoBidiRef( pho_obj, &illum, phoFuncVal );
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoFunc failed ***","");
	    		zmabend("photfit abend");
		    }

    er = (*(x[0]+ii) - *phoFuncVal) * (*(x[0]+ii) - *phoFuncVal);

    if ( fabs(*(x[1]+ii) ) < minSigma ) sigma = minSigma;
    else                                sigma = fabs(*(x[1]+ii));
    sig2 = sigma * sigma;

    *error = *error + er/sig2;
    sig = sig + 1.0 / sig2;
    if ( fabs(*(x[0]+ii) - *phoFuncVal) < tolerance ) 
    {
      sum = sum + er / sig2;
      sigSum = sigSum + 1.0 / sig2;
      n++;
    }

/*
    er = fabs( *(x[0]+ii) - *phoFuncVal) ;
    *error = *error + er;

    if ( er < tolerance ) 
    {
      sum = sum + er;
      n++;
    }
*/
  }


  if (n > minNum) *error = sqrt(sum / sigSum) ;
  else            *error = sqrt(*error / sig) ;

/* 	strcpy( msg, " *error = " );
	sprintf( cval1, " %10.6e", *error);
	strcat( msg, cval1);
    	zvmessage( msg, "");
*/

/*  if (n > minNum) *error = sum/(1.0 * n); 
  else            *error = *error / (1.0 * npts); 
 
 	strcpy( msg, " *error = " );
	sprintf( cval1, " %10.6e", *error);
	strcat( msg, cval1);
    	zvmessage( msg, "");
*/
/*  free(pkeylist); */
  free(phoFuncVal);
  return rstatus=1;
}


/************************************************************************
*									*
*		zget_ran						*
*									*
************************************************************************/

/* generates uniform deviation */


void zget_ran( 
	long *seed1, 
	float *randout)

{
  int i;
  srand((unsigned) *seed1);  
  i = ((unsigned int )rand());
  if (i >= 32767) *randout = (float) (i%32768)/32767.0;
  else *randout = (float) (i/32767.0);
  *seed1 = ((*seed1)*1103515245+12345);
  *seed1 = (unsigned int) (*seed1/65536)%32768;
  return;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Test_File:
$ create tstphotfit2.pdf
procedure
refgbl $echo

body
let _onfail="continue"
let $echo="yes"

phottest out=phottest_m.dat pho_func=MINNAERT class_id=2 albedo=0.7 +
   exponent=0.6 start=(10,10,10) delta=(30,30,180) sigma=0.000001 

photfit2 inp=(phottest_m.dat,phottest_m.dat) pho_func=MINNAERT +
   class_id=2 albedo=0.6 min_albedo=0.0 max_albedo=1.0 t_albedo=0.1 +
   exponent=0.6 min_exponent=0.0 max_exponent=1.0 t_exponent=0.1 +
   norm=25 rerun=2 maxiter=100 numten=25 metrop=20 percent=90 +
   toleranc=0.02 'print 

end-proc
$!-----------------------------------------------------------------------------
$ create tstphotfit2_linux.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

phottest out=phottest_m.dat pho_func=MINNAERT class_id=2 albedo=0.7  +
   exponent=0.6 start=(10,10,10) delta=(30,30,180) sigma=0.000001
Beginning VICAR task phottest
 
 CLASS_ID =    2
 Function = MINNAERT
    ALBEDO =   7.000e-01
    EXPONENT =   6.000e-01
 
 Number of points =          9
 
 sigma =  1.000000e-06
 
photfit2 inp=(phottest_m.dat,phottest_m.dat) pho_func=MINNAERT  +
   class_id=2 albedo=0.6 min_albedo=0.0 max_albedo=1.0 t_albedo=0.1  +
   exponent=0.6 min_exponent=0.0 max_exponent=1.0 t_exponent=0.1  +
   norm=25 rerun=2 maxiter=100 numten=25 metrop=20 percent=90  +
   toleranc=0.02 'print
Beginning VICAR task photfit2
 program PHOTFIT2
 
 Function =MINNAERT
 parameter number =  2
 
 
 First guess and there limits: 
 ----------------------------- 
 
     ALBEDO =   6.000e-01
     MIN_ALBEDO =   0.000e+00
     MAX_ALBEDO =   1.000e+00
     T_ALBEDO =   1.000e-01
 
     EXPONENT =   6.000e-01
     MIN_EXPONENT =   0.000e+00
     MAX_EXPONENT =   1.000e+00
     T_EXPONENT =   1.000e-01
 
 
   1.File: phottest_m.dat
   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS
     1          2  6.979e-01   10.0000   10.0000    1.7343  4.126e-08
     2          2  7.716e-01   10.0000   40.0000   30.1938  5.493e-07
     3          2  1.065e+00   10.0000   70.0000   60.1639 -1.306e-07
     4          2  6.002e-01   40.0000   10.0000   30.1938  3.432e-08
     5          2  6.637e-01   40.0000   40.0000    6.4231  1.135e-06
     6          2  9.163e-01   40.0000   70.0000   31.0354  1.703e-06
     7          2  3.700e-01   70.0000   10.0000   60.1639  2.334e-07
     8          2  4.091e-01   70.0000   40.0000   31.0354 -9.338e-07
     9          2  5.648e-01   70.0000   70.0000    9.3955  1.300e-06
   1.IBIS file contains    9 points
   Number of points left from this file =     9
 
   2.File: phottest_m.dat
   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS
     1          2  6.979e-01   10.0000   10.0000    1.7343  4.126e-08
     2          2  7.716e-01   10.0000   40.0000   30.1938  5.493e-07
     3          2  1.065e+00   10.0000   70.0000   60.1639 -1.306e-07
     4          2  6.002e-01   40.0000   10.0000   30.1938  3.432e-08
     5          2  6.637e-01   40.0000   40.0000    6.4231  1.135e-06
     6          2  9.163e-01   40.0000   70.0000   31.0354  1.703e-06
     7          2  3.700e-01   70.0000   10.0000   60.1639  2.334e-07
     8          2  4.091e-01   70.0000   40.0000   31.0354 -9.338e-07
     9          2  5.648e-01   70.0000   70.0000    9.3955  1.300e-06
   2.IBIS file contains    9 points
   Number of points left from this file =     9
 
 
  Total number of input points =     18
  Total number of points left =     18
 
 
 		phoMetropolis :
 
 scaling factor reduces temperatur =  9.120108e-01
 
  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds
       ALBEDO     EXPONENT
  Boltzmann =  3.710930e+00  Temperatur =  1.000000e-01
         20 3.617582e-02          3         10          7          5
 6.416308e-01 5.522417e-01
  Boltzmann =  1.042349e+01  Temperatur =  1.000000e-02
         40 1.125803e-03          7         11          2          1
 7.018494e-01 6.014766e-01
  Boltzmann =  3.352949e+00  Temperatur =  1.000000e-03
         60 2.253812e-04          2         11          7          0
 6.996185e-01 5.996683e-01
  Boltzmann =  4.295351e+00  Temperatur =  1.000000e-04
         80 1.349530e-04          5          6          9          0
 6.998027e-01 5.996718e-01
        100 1.329283e-04          3         12          5          0
 6.997942e-01 5.996771e-01
 
          1. Metropolis-result  :
 ALBEDO =   6.998e-01
 EXPONENT =   5.997e-01
 
 
 		phoMetropolis :
 
 scaling factor reduces temperatur =  9.120108e-01
 
  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds
       ALBEDO     EXPONENT
  Boltzmann =  4.382582e+00  Temperatur =  1.000000e-01
         20 1.329283e-04          2          9          9          1
 6.997942e-01 5.996771e-01
  Boltzmann =  3.421286e+00  Temperatur =  1.000000e-02
         40 1.329283e-04          1         12          7          2
 6.997942e-01 5.996771e-01
  Boltzmann =  3.760608e+00  Temperatur =  1.000000e-03
         60 1.129697e-04          1         14          5          0
 6.999984e-01 5.998457e-01
  Boltzmann =  3.283085e+00  Temperatur =  1.000000e-04
         80 4.028756e-05          5          6          9          0
 7.000015e-01 5.999469e-01
        100 1.339423e-05          5          5         10          0
 6.999769e-01 5.999686e-01
 
          2. Metropolis-result  :
 ALBEDO =   7.000e-01
 EXPONENT =   6.000e-01
 
 
          Statistical results:
          --------------------
 
   INC_ANG    EM_ANG  PHAS_ANG      I/F     I/F-FitVal 
   10.0000   10.0000    1.7343  0.6978377   0.00002237   used 
   10.0000   40.0000   30.1938  0.7716082   0.00001915   used 
   10.0000   70.0000   60.1639  1.0653412  -0.00000140   used 
   40.0000   10.0000   30.1938  0.6002046   0.00001451   used 
   40.0000   40.0000    6.4231  0.6636541   0.00001191   used 
   40.0000   70.0000   31.0354  0.9162915  -0.00000661   used 
   70.0000   10.0000   60.1639  0.3699891  -0.00000021   used 
   70.0000   40.0000   31.0354  0.4091018  -0.00000464   used 
   70.0000   70.0000    9.3955  0.5648372  -0.00001811   used 
   10.0000   10.0000    1.7343  0.6978377   0.00002237   used 
   10.0000   40.0000   30.1938  0.7716082   0.00001915   used 
   10.0000   70.0000   60.1639  1.0653412  -0.00000140   used 
   40.0000   10.0000   30.1938  0.6002046   0.00001451   used 
   40.0000   40.0000    6.4231  0.6636541   0.00001191   used 
   40.0000   70.0000   31.0354  0.9162915  -0.00000661   used 
   70.0000   10.0000   60.1639  0.3699891  -0.00000021   used 
   70.0000   40.0000   31.0354  0.4091018  -0.00000464   used 
   70.0000   70.0000    9.3955  0.5648372  -0.00001811   used 
        Mean I/F error per fitted point =   0.00001099
 
 MEAN VALUES (to thinking about the stability of fitting) :
 ALBEDO =   6.999e-01 +/-  1.292e-04
 EXPONENT =   5.998e-01 +/-  2.062e-04
 
 
*****************************************************************************
* ***************************************************************************
* *
* *  RESULTING PARAMETERS OF BEST-FIT: 
* *
* *      ALBEDO =   7.000e-01
* *      EXPONENT =   6.000e-01
* *
* ***************************************************************************
*****************************************************************************
 
end-proc
$!-----------------------------------------------------------------------------
$ create tstphotfit2_sun.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

phottest out=phottest_m.dat pho_func=MINNAERT class_id=2 albedo=0.7  +
   exponent=0.6 start=(10,10,10) delta=(30,30,180) sigma=0.000001
Beginning VICAR task phottest
 
 CLASS_ID =    2
 Function = MINNAERT
    ALBEDO =   7.000e-01
    EXPONENT =   6.000e-01
 
 Number of points =          9
 
 sigma =  1.000000e-06
 
photfit2 inp=(phottest_m.dat,phottest_m.dat) pho_func=MINNAERT  +
   class_id=2 albedo=0.6 min_albedo=0.0 max_albedo=1.0 t_albedo=0.1  +
   exponent=0.6 min_exponent=0.0 max_exponent=1.0 t_exponent=0.1  +
   norm=25 rerun=2 maxiter=100 numten=25 metrop=20 percent=90  +
   toleranc=0.02 'print
Beginning VICAR task photfit2
 program PHOTFIT2
 
 Function =MINNAERT
 parameter number =  2
 
 
 First guess and there limits: 
 ----------------------------- 
 
     ALBEDO =   6.000e-01
     MIN_ALBEDO =   0.000e+00
     MAX_ALBEDO =   1.000e+00
     T_ALBEDO =   1.000e-01
 
     EXPONENT =   6.000e-01
     MIN_EXPONENT =   0.000e+00
     MAX_EXPONENT =   1.000e+00
     T_EXPONENT =   1.000e-01
 
 
   1.File: phottest_m.dat
   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS
     1          2  6.979e-01   10.0000   10.0000    1.7343  4.126e-08
     2          2  7.716e-01   10.0000   40.0000   30.1938  5.493e-07
     3          2  1.065e+00   10.0000   70.0000   60.1639 -1.306e-07
     4          2  6.002e-01   40.0000   10.0000   30.1938  3.432e-08
     5          2  6.637e-01   40.0000   40.0000    6.4231  1.135e-06
     6          2  9.163e-01   40.0000   70.0000   31.0354  1.703e-06
     7          2  3.700e-01   70.0000   10.0000   60.1639  2.334e-07
     8          2  4.091e-01   70.0000   40.0000   31.0354 -9.338e-07
     9          2  5.648e-01   70.0000   70.0000    9.3955  1.300e-06
   1.IBIS file contains    9 points
   Number of points left from this file =     9
 
   2.File: phottest_m.dat
   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS
     1          2  6.979e-01   10.0000   10.0000    1.7343  4.126e-08
     2          2  7.716e-01   10.0000   40.0000   30.1938  5.493e-07
     3          2  1.065e+00   10.0000   70.0000   60.1639 -1.306e-07
     4          2  6.002e-01   40.0000   10.0000   30.1938  3.432e-08
     5          2  6.637e-01   40.0000   40.0000    6.4231  1.135e-06
     6          2  9.163e-01   40.0000   70.0000   31.0354  1.703e-06
     7          2  3.700e-01   70.0000   10.0000   60.1639  2.334e-07
     8          2  4.091e-01   70.0000   40.0000   31.0354 -9.338e-07
     9          2  5.648e-01   70.0000   70.0000    9.3955  1.300e-06
   2.IBIS file contains    9 points
   Number of points left from this file =     9
 
 
  Total number of input points =     18
  Total number of points left =     18
 
 
 		phoMetropolis :
 
 scaling factor reduces temperatur =  9.120108e-01
 
  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds
       ALBEDO     EXPONENT
  Boltzmann =  3.710930e+00  Temperatur =  1.000000e-01
         20 3.333243e-02          6          8          6          2
 7.355117e-01 6.038551e-01
  Boltzmann =  3.556785e+00  Temperatur =  1.000000e-02
         40 3.064059e-03          6          8          6          1
 6.950480e-01 5.926315e-01
  Boltzmann =  1.188322e+00  Temperatur =  1.000000e-03
         60 2.489434e-03          2         10          8          0
 6.959210e-01 5.940402e-01
  Boltzmann =  1.324970e+00  Temperatur =  1.000000e-04
         80 1.757190e-03          7          6          7          0
 6.969721e-01 5.959322e-01
        100 1.607768e-03          8          9          3          0
 6.973140e-01 5.961848e-01
 
          1. Metropolis-result  :
 ALBEDO =   6.973e-01
 EXPONENT =   5.962e-01
 
 
 		phoMetropolis :
 
 scaling factor reduces temperatur =  9.120108e-01
 
  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds
       ALBEDO     EXPONENT
  Boltzmann =  4.320741e+00  Temperatur =  1.000000e-01
         20 1.607768e-03          1          9         10          1
 6.973140e-01 5.961848e-01
  Boltzmann =  4.380977e+00  Temperatur =  1.000000e-02
         40 1.607768e-03          0         12          8          0
 6.973140e-01 5.961848e-01
  Boltzmann =  1.522696e+00  Temperatur =  1.000000e-03
         60 1.171299e-03          2          9          9          0
 6.979054e-01 5.975306e-01
  Boltzmann =  1.247023e+00  Temperatur =  1.000000e-04
         80 8.797200e-04          7          5          8          0
 6.984233e-01 5.982119e-01
        100 8.033942e-04          4          7          9          0
 6.985947e-01 5.981771e-01
 
          2. Metropolis-result  :
 ALBEDO =   6.986e-01
 EXPONENT =   5.982e-01
 
 
          Statistical results:
          --------------------
 
   INC_ANG    EM_ANG  PHAS_ANG      I/F     I/F-FitVal 
   10.0000   10.0000    1.7343  0.6964979   0.00136218   used 
   10.0000   40.0000   30.1938  0.7704734   0.00115393   used 
   10.0000   70.0000   60.1639  1.0653124   0.00002746   used 
   40.0000   10.0000   30.1938  0.5993220   0.00089720   used 
   40.0000   40.0000    6.4231  0.6629764   0.00068961   used 
   40.0000   70.0000   31.0354  0.9166792  -0.00039425   used 
   70.0000   10.0000   60.1639  0.3699791   0.00000981   used 
   70.0000   40.0000   31.0354  0.4092749  -0.00017772   used 
   70.0000   70.0000    9.3955  0.5658931  -0.00107400   used 
   10.0000   10.0000    1.7343  0.6964979   0.00136218   used 
   10.0000   40.0000   30.1938  0.7704734   0.00115393   used 
   10.0000   70.0000   60.1639  1.0653124   0.00002746   used 
   40.0000   10.0000   30.1938  0.5993220   0.00089720   used 
   40.0000   40.0000    6.4231  0.6629764   0.00068961   used 
   40.0000   70.0000   31.0354  0.9166792  -0.00039425   used 
   70.0000   10.0000   60.1639  0.3699791   0.00000981   used 
   70.0000   40.0000   31.0354  0.4092749  -0.00017772   used 
   70.0000   70.0000    9.3955  0.5658931  -0.00107400   used 
        Mean I/F error per fitted point =   0.00064291
 
 MEAN VALUES (to thinking about the stability of fitting) :
 ALBEDO =   6.980e-01 +/-  9.056e-04
 EXPONENT =   5.972e-01 +/-  1.409e-03
 
 
*****************************************************************************
* ***************************************************************************
* *
* *  RESULTING PARAMETERS OF BEST-FIT: 
* *
* *      ALBEDO =   6.986e-01
* *      EXPONENT =   5.982e-01
* *
* ***************************************************************************
*****************************************************************************
 
end-proc
$ Return
$!#############################################################################
