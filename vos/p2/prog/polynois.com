$!****************************************************************************
$!
$! Build proc for MIPL module polynois
$! VPACK Version 1.8, Wednesday, February 07, 1996, 16:45:27
$!
$! Execute by entering:		$ @polynois
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
$ write sys$output "*** module polynois ***"
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
$ write sys$output "Invalid argument given to polynois.com file -- ", primary
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
$   if F$SEARCH("polynois.imake") .nes. ""
$   then
$      vimake polynois
$      purge polynois.bld
$   else
$      if F$SEARCH("polynois.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polynois
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polynois.bld "STD"
$   else
$      @polynois.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polynois.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polynois.com -
	-s polynois.f -
	-i polynois.imake -
	-p polynois.pdf -
	-t tstpolynois.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polynois.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INCLUDE 'fortport'
C     PROGRAM TO PRODUCE DISCRETE LEVEL NOISE
C
      INTEGER PBUF(2,256)
      REAL*4  THRESH(256),RANNUM
      BYTE OUT(3600),LNOV(256)
      INTEGER LOG,IO
      EQUIVALENCE (NVAL,LOG)
C
      CALL IFMESSAGE('POLYNOIS version 05-SEP-94')
C
      CALL XVEACTION('SA',' ')
      CALL XVPARM('SEEDS',IO,ICNT,IDEF,2)
      IO = 0
      CALL XVPARM('NOISE',PBUF,ICNT,IDEF,512)
      NLEV=ICNT/2
      IF (NLEV * 2 .NE. ICNT) THEN
          CALL XVMESSAGE(
     + '*** ERROR-NOISE PARAMETERS MUST COME IN PAIRS',' ')
          CALL ABEND
      END IF
C     DETERMINE THE NOISE LEVELS AND PROBABILITIES
      VN=0.0
      DO I=1,NLEV
           NVAL=PBUF(1,I)
           IF((NVAL.LT.0).AND.(NVAL.GT.255)) THEN
               CALL XVMESSAGE(
     + '*** INVALID NOISE INTENSITY LEVEL',' ')
               CALL ABEND
           END IF
C     FIX INTEGER OVERFLOW PROBLEM i.e. log=140
C     (USE SUBROUTINE ITLA INSTEAD OF "=")
C          LNOV(I)=LOG
           CALL ITLA(LOG,LNOV(I),1)
C           LNOV(I)=INT2BYTE(LOG)
           THRESH(I)=PBUF(2,I)
           IF(THRESH(I).LE.0.0) THEN
              CALL XVMESSAGE('*** NONPOSITIVE NOISE PROBABILITY',' ')
              CALL ABEND
           END IF
           VN=VN+THRESH(I)
      END DO
C     DETERMINE NOISE PROB. AS RATIO AGAINST TOTAL ACCUMULATIVE PROBS.
      V=0.0
      DO I=1,NLEV
           V=V+THRESH(I)/VN
           THRESH(I)=V
      END DO
      CALL XVPARM('NSO',NSO,ICNT,IDEF,1)
      CALL XVPARM('NLO',NLO,ICNT,IDEF,1)
      IF((NSO.LE.0).OR.(NLO.LE.0)) THEN
          CALL XVMESSAGE('*** INVALID SIZE FIELD PARAMETERS',' ')
          CALL ABEND
      END IF
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NLO,'U_NS',
     +NSO,'O_FORMAT','BYTE','U_FORMAT','BYTE',' ') 
C     FOR THE FIRST PROB. GREATER THAN THE RANDOM NUMBER, THE
C     ASSOCIATED NOISE INTENSITY LEVEL IS GENERATED.
      DO I=1,NLO
         DO 10 J=1,NSO
                 CALL RANGEN(IO,RANNUM)
                 V=RANNUM
                   DO K=1,NLEV
                        IF(V.LT.THRESH(K)) GO TO 10
                   END DO 
                CALL XVMESSAGE('*** NOISE ENCODING ERROR',' ')
                CALL ABEND
   10    OUT(J)=LNOV(K)
         CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NSO,' ')
      END DO
      CALL XVMESSAGE('*** POLYNOIS TASK COMPLETED',' ')
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polynois.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polynois

   To Create the build file give the command:

		$ vimake polynois		(VMS)
   or
		% vimake polynois		(Unix)


************************************************************************/


#define PROGRAM	polynois
#define R2LIB

#define MODULE_LIST polynois.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polynois.pdf
 process help=*
 PARM OUT          TYPE=STRING       
 PARM NLO          TYPE=INTEGER                            DEFAULT=0
 PARM NSO          TYPE=INTEGER                            DEFAULT=0
 PARM SEEDS        TYPE=INTEGER      COUNT=2               DEFAULT=(0,0)
 PARM NOISE        TYPE=INTEGER      COUNT=(2:512)

!# annot function="Generating Synthetic Images"
!# annot keywords=(noise,level,number,ratio,RAND,output,picture)    
 END-PROC
.TITLE
 Generates a noise image of specified noise spectra
.HELP
 This program produces discrete level noise (0-255). The user
 must specify the noise intensity level and the noise probability.


 Original Programmer : T. C. RINDFLEISCH
 Current  Cognizant Programmer : F. MOSS

 Made Portable for UNIX: CRI   05-SEP-94

 Operation:

    The program must be supplied with at least one pair of 
 "NOISE" parameters. (i.e. noise=(10,1)) Each pair of integer
 parameters consists of a noise intensity level,as the first
 constant, and an associated noise probability, as the second
 constant. The initial seeds for a uniform random number
 generator can be optionally specified. If no value for the
 random number generator is provided, then the default value 
 of (0,0) is used.

    The probability for a noise intensity level is determined
 as a ratio of the input value and the total cumulative noise
 probability. For example, if three noise levels with noise
 probabilities of NOP1,NOP2,and NOP3, respectively, are required,
 then the actual noise probabilities are NOP1/ACC, NOP2/ACC,
 and NOP3/ACC, where ACC is (NOP1+NOP2+NOP3). After the interval
 between 0 and 1 is divided into subintervals proportional to the
 internal probabilities (in the order specified), the value returned
 by the random number generator, RAND, is checked. The associated noise 
 intensity level for the subinterval in which RAND falls is generated
 for the current output pixel.
   
    The noise intensity level is restricted to values equal to or
 between 0 and 255, while the noise probability must be greater
 than zero. A maximum of 256 pairs of integers can be specified
 whether or not the default value for the random number generator
 is used. The order of the pairs of integers is not important. 
 The maximum line length is 3600 bytes. Since there is no input
 the size field determines the number of lines and samples in 
 the output picture.

.LEVEL1
.VARIABLE OUT
 output data set
.VARIABLE NLO
 number of lines in the
 output picture
.VARIABLE NSO
 number of samples in
 the output picture
.VARIABLE SEEDS
 initial seeds for random
 number generator
.VARIABLE NOISE
 noise intensity and noise,this is a required parameter
 probability
.LEVEL2 
.VARIABLE OUT
 output data set
.VARIABLE NLO
 this keyword determines the number of lines in the output picture
.VARIABLE NSO
 this keyword determines the  number of samples in the output picture
.VARIABLE SEEDS
 seeds=(s1,s2)          this keyword supplies the initial value
 of seeds s1,s2 for the Fortran function RAN. RAN(s1,s2) 
 returns a pseudorandom number and s1 and s2 are integer 
 variables or array elements that contain the seeds for computing
 the random number. The values  of s1 and s2 are updated during
 the computation to contain the updated seed. The default value
 is (0,0).
.VARIABLE NOISE
 NOISE=(I1,P1,I2,P2,I3,P3,....): the first constant I1 is the noise
 intensity level and the second constant P1 is an associated noise
 probability and so on.... A minimum of 1 pair and a maximum of 256 
 pairs of integers can be specified. The "In" values are restricted
 to values equal to or between 0 and 255. The "Pn" must be greater
 than 0.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolynois.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write " THIS IS A TEST SCRIPT FOR THE PROGRAM - POLYNOIS"
polynois flo 256 256 NOISE=(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)
hist flo SIZE=(1,1,256,256) FORMAT=BYTE
end-proc
$ Return
$!#############################################################################
