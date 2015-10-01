$!****************************************************************************
$!
$! Build proc for MIPL module fracgen
$! VPACK Version 1.7, Wednesday, July 13, 1994, 07:39:58
$!
$! Execute by entering:		$ @fracgen
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
$ write sys$output "*** module fracgen ***"
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
$ write sys$output "Invalid argument given to fracgen.com file -- ", primary
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
$   if F$SEARCH("fracgen.imake") .nes. ""
$   then
$      vimake fracgen
$      purge fracgen.bld
$   else
$      if F$SEARCH("fracgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fracgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fracgen.bld "STD"
$   else
$      @fracgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fracgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fracgen.com -
	-s fracgen.f -
	-i fracgen.imake -
	-p fracgen.pdf -
	-t tstfracgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fracgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C------------------------------------------------------------------------------
C FRACGEN
C Revisions:
C       12 March 1990, Megan O'Shaughnessy
C          Changed directory in which the scratch file is created, from
C          the default directory to V2$SCRATCH:.  (The scratch file takes up
C          lots of space (~8200 blocks in my runs of FRACGEN,) and if the user
C          didn't have enough file quota the program would abend.)  Also, 
C          mention that the scratch file is created was added to the help file.
C 	2-AUG-1993 by G. A. Madrid Jr.
C	   Deleted call to OPENSCRATCH subroutine because a reference to a
C	   specific VMS filename was coded into the CALL statement, which is
C	   does not lead to very portable code.  Instead, a new parameter was
C	   made up which allows the user the option of specifying the name of
C	   scratch file to be used and its location, making it more convenient
C	   to use and more portable.  This new parameter is called IDSNAM.
C	   LGEOM has a parameter by that same name, and users can refer to LGEOM
C 	   to see what its functionality is.
C        5-MAY-1994 by C.R. Schenk (CRI)
C          Corrected logic for seed generation and replaced random number
C          generator code with a new FUNCTION RANGEN.
C          RANGEN is an optional random number generator which will
C          generate the same random number sequence regardless of the
C          host platform (when the same seed is used on both machines).
C          This is useful for performance verification of the "portable"
C          code.  
C------------------------------------------------------------------------------
	SUBROUTINE MAIN44

	IMPLICIT NONE
	INTEGER	UNIT, SCRUNIT, COUNT, DEF, STATUS, SCRNUM
	INTEGER	NL,NS, NLFFT,NSFFT, LINE, SAMP
	INTEGER	SIGN, LOG2
	INTEGER SEED, BUF

        REAL*4  RAND_NUM
	REAL	Y2,R2, FILTER, S, PH, AMP, MAX
	REAL	FRACDIM, BETA, POWER
	REAL	MINVAL,MAXVAL, MAXFORMAT, SLOPE,OFFSET, SCPOW
	REAL	RBUFFER(4096)

	COMPLEX	CBUFFER(4096)
	COMPLEX	COLBUF(4096)
	CHARACTER*8 FORMAT
	CHARACTER*256 IDSNAM

	CALL IFMESSAGE('FRACGEN  version 1-JULY-1994')
	CALL XVEACTION('SA',' ')


C		GET THE INPUT PARAMETERS
	CALL XVP ('NL', NL, COUNT)
	CALL XVP ('NS', NS, COUNT)
	NLFFT = 2**( LOG2(NL-1) + 1)
	NSFFT = 2**( LOG2(NS-1) + 1)

	CALL XVP ('FRACDIM', FRACDIM, COUNT)

	CALL XVPARM ('SEED', SEED, COUNT, DEF,1)
	IF (COUNT .EQ. 0) THEN
            CALL GET_SECONDS(BUF)
	    SEED = MOD(BUF,2**24)
        ENDIF

	CALL XVP ('POWER', SCPOW, COUNT)

	CALL XVP ('FORMAT', FORMAT, COUNT)
	IF (FORMAT(1:4) .EQ. 'HALF') THEN
	    MAXFORMAT = 32767.0
	ELSE IF (FORMAT(1:4) .EQ. 'REAL') THEN
	    MAXFORMAT = 1.0
	ELSE
	    MAXFORMAT = 255.0
	ENDIF


C		CREATE AND OPEN THE SCRATCH FILE
	SCRNUM = 1
C------------------------------------------------------------------------------
c Change made, 3-12-1990 by M. O'Shaughnessy
c 	Changed filename of scratch file from 'fracgen.scr' to
c 	'v2$scratch:fracgen.scr'
c Change made, 2-AUG-1993 by G. A. Madrid Jr.
c	Substituted the value of parameter IDSNAM for the specification
c	'v2$scratch:fracgen.scr' which would only work in VMS.
C------------------------------------------------------------------------------
	CALL XVPARM('IDSNAM',IDSNAM,COUNT,DEF,0)
	CALL XVUNIT(SCRUNIT,' ',SCRNUM,STATUS,'U_NAME',IDSNAM,' ')

	CALL XVOPEN(SCRUNIT,STATUS,'OP','WRITE','U_NL',NLFFT,
     +			'U_NS',NSFFT,'O_FORMAT','REAL',
     +                  'COND','LABELS',' ')
	CALL XVCLOSE(SCRUNIT,STATUS,' ')

	CALL XVUNIT(SCRUNIT,' ',SCRNUM,STATUS,'U_NAME',IDSNAM,' ')
	CALL XVOPEN(SCRUNIT,STATUS,'OP','UPDATE','U_NL',NLFFT,
     +			'U_NS',NSFFT,'I_FORMAT','REAL','U_FORMAT','REAL',
     +			'O_FORMAT','REAL','COND','LABELS',' ')

	BETA = 2*(3.0 - FRACDIM) + 1.0
	POWER = -(1.+BETA)/4.0

C		MAKE THE POWER LAW NOISE
	DO LINE = 1, NLFFT
	    SIGN = (-1)**LINE
	    Y2 = ( FLOAT(MIN(LINE,NLFFT+2-LINE) - 1) /NLFFT )**2
	    DO SAMP = 1,NSFFT/2
		SIGN = -SIGN
		R2 = ( FLOAT(SAMP - 1) /NSFFT )**2 + Y2
		IF (R2 .NE. 0.0) THEN
		    FILTER = R2**POWER
   		    CALL RANGEN(SEED,RAND_NUM)
		    S = RAND_NUM
		    IF (RAND_NUM.LT.1.0E-10) THEN
                        S = 1.0E-10
                    ENDIF
		    AMP = FILTER*SQRT(-LOG(S))
   		    CALL RANGEN(SEED,RAND_NUM)
		    PH = 2.*3.14159*RAND_NUM
		    IF (SIGN .LT. 0)  PH = -PH
		    CBUFFER(SAMP) = AMP*CEXP((0,1)*PH)
		ENDIF
	    ENDDO
	    CALL XVWRIT (SCRUNIT, CBUFFER, STATUS, 'LINE',LINE,' ')
	ENDDO


C		TRANSFORM THE NOISE
	CALL FFTVICARR (SCRUNIT, NLFFT, NSFFT, -1, COLBUF)


C		SCALE THE FRACTIONAL NOISE
	MINVAL = +1.0E20
	MAXVAL = -1.0E20
	DO LINE = 1,NL
	    CALL XVREAD (SCRUNIT, RBUFFER, STATUS, 'LINE',LINE,' ')
	    DO SAMP = 1,NS
		MINVAL = MIN( MINVAL, RBUFFER(SAMP) )
		MAXVAL = MAX( MAXVAL, RBUFFER(SAMP) )
	    ENDDO
	ENDDO



	CALL XVUNIT (UNIT, 'OUT',1, STATUS,' ')
	CALL XVOPEN (UNIT, STATUS, 
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT',FORMAT,
     +		'U_NL',NL, 'U_NS',NS,' ')


	SLOPE = MAXFORMAT/(MAXVAL-MINVAL)
	OFFSET = -MINVAL*SLOPE
	IF (FORMAT(1:4) .NE. 'REAL')  OFFSET = OFFSET + 0.5
	DO LINE = 1,NL
	    CALL XVREAD (SCRUNIT, RBUFFER, STATUS, 'LINE',LINE,' ')
	    DO SAMP = 1,NS
		RBUFFER(SAMP) = SLOPE*RBUFFER(SAMP) + OFFSET
		RBUFFER(SAMP) = MAXFORMAT*(RBUFFER(SAMP)/MAXFORMAT)**SCPOW
	    ENDDO
	    CALL XVWRIT (UNIT, RBUFFER, STATUS,' ')
	ENDDO

	CALL XVCLOSE (UNIT, STATUS,' ')
	CALL XVCLOSE (SCRUNIT, STATUS, 'CLOS_ACT','DELETE',' ')

	RETURN
	END


	SUBROUTINE FFTVICARC (UNIT, NLFFT, NSFFT, FFTSIGN)
C
C
C	FFTVICARC IS A TWO DIMENSIONAL FAST FOURIER TRANSFORM ROUTINE.
C	    IT DOES AN IN PLACE COMPLEX TO COMPLEX TRANSFORM
C	    ON AN EXISTING VICAR IMAGE.  THE COMPLEX IMAGE MUST BE ALREADY 
C	    OPENED FOR UPDATE WITH XVOPEN AND HAVE SIZE NLFFT BY NSFFT 
C	    WHICH MUST BE POWERS OF TWO.  THE SIGN (+1 OR -1) OF THE 
C	    TRANSFORM IS PASSED IN FFTSIGN.  THE MAXIMUM SIZE IN EITHER 
C	    DIRECTION IS 32768.
C	THE TRANSFORM DOES NOT TRANSPOSE THE IMAGE AND THE ZERO-FREQUENCY
C	    PIXEL IS AT THE FIRST LINE AND SAMPLE.
C
C
	IMPLICIT NONE
	INTEGER	UNIT, NLFFT, NSFFT, FFTSIGN
	INTEGER	MAXN, LINE, STATUS, BUFPTR
	INTEGER*2 BITREV(65536)
	COMPLEX BUFFER(256*256), PHASE(65536)

	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE


	MAXN = MAX(NLFFT,NSFFT)
	CALL MAKEBITREV (BITREV, MAXN)
	CALL MAKEPHASE (PHASE, FFTSIGN, MAXN)


	IF (NLFFT*NSFFT .GT. 256*256) THEN

	    DO LINE = 1, NLFFT
	        CALL XVREAD (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	        CALL FFTC1D (BUFFER, NSFFT)
	        CALL XVWRIT (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	    ENDDO

	    CALL FFTC1VD (UNIT, NSFFT, NLFFT)

	ELSE

	    DO LINE = 1, NLFFT
		BUFPTR = NSFFT*(LINE-1)+1
		CALL XVREAD (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE',LINE,' ')
		CALL FFTC1D (BUFFER(BUFPTR), NSFFT)
	    ENDDO
	    CALL FFTCV (BUFFER, NSFFT, NLFFT)
	    DO LINE = 1, NLFFT
		BUFPTR = NSFFT*(LINE-1)+1
		CALL XVWRIT (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE', LINE,' ')
	    ENDDO

	ENDIF

	RETURN
	END





	SUBROUTINE FFTVICARR (UNIT, NLFFT, NSFFT, FFTSIGN, COLBUF)
C
C
C	FFTVICARR IS A TWO DIMENSIONAL FAST FOURIER TRANSFORM ROUTINE.
C	    IT DOES AN IN PLACE REAL TO COMPLEX CONJUGATE OR COMPLEX
C	    CONJUGATE TO REAL TRANSFORM ON AN EXISTING VICAR IMAGE.  
C	    THE IMAGE MUST BE ALREADY OPENED FOR UPDATE WITH XVOPEN,
C	    WITH REAL FORMATS, AND HAVE REAL SIZE NLFFT BY NSFFT
C	    WHICH MUST BE POWERS OF TWO.  
C	    THE SIGN (+1 OR -1) OF THE TRANSFORM IS PASSED IN FFTSIGN.
C	    IF FFTSIGN = +1 THEN THE TRANSFORM IS REAL TO COMPLEX CONJUGATE
C	    AND IF FFTSIGN = -1 THEN THEN TRANSFORM IS THE OTHER WAY.
C	    COLBUF IS A COMPLEX ARRAY OF LENGTH EQUAL TO THE NUMBER OF ROWS.
C	      IT CONTAINS THE 'EXTRA' COLUMN FOR THE COMPLEX CONJUGATE FORMAT.
C	    THE MAXIMUM SIZE IN EITHER DIRECTION IS 32768.
C	THE TRANSFORM DOES NOT TRANSPOSE THE IMAGE AND THE ZERO-FREQUENCY
C	    PIXEL IS AT THE FIRST LINE AND SAMPLE.
C
C
	IMPLICIT NONE
	INTEGER	UNIT, NLFFT, NSFFT, FFTSIGN
	INTEGER	MAXN, LINE, STATUS, BUFPTR
	COMPLEX COLBUF(1)
	INTEGER*2 BITREV(65536)
	COMPLEX BUFFER(256*128), PHASE(65536)

	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE


	MAXN = MAX(NLFFT,NSFFT)
	CALL MAKEBITREV (BITREV, MAXN)
	CALL MAKEPHASE (PHASE, FFTSIGN, MAXN)


	IF (NLFFT*NSFFT .GT. 256*256) THEN 

	IF (FFTSIGN .EQ. +1) THEN
	    DO LINE = 1, NLFFT
		CALL XVREAD (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
		CALL FFTC1D (BUFFER, NSFFT/2)
		CALL FIXREAL (BUFFER, COLBUF(LINE), NSFFT/2, +1)
		CALL XVWRIT (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	    ENDDO
	    CALL FFTC1D (COLBUF, NLFFT)
	    CALL FFTC1VD (UNIT, NSFFT/2, NLFFT)
	ELSE
	    CALL FFTC1VD (UNIT, NSFFT/2, NLFFT)
	    CALL FFTC1D (COLBUF, NLFFT)
	    DO LINE = 1, NLFFT
		CALL XVREAD (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
		CALL FIXREAL (BUFFER, COLBUF(LINE), NSFFT/2, -1)
		CALL FFTC1D (BUFFER, NSFFT/2)
		CALL XVWRIT (UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	    ENDDO
	ENDIF

	ELSE

	IF (FFTSIGN .EQ. +1) THEN
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVREAD (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE',LINE,' ')
		CALL FFTC1D (BUFFER(BUFPTR), NSFFT/2)
		CALL FIXREAL (BUFFER(BUFPTR), COLBUF(LINE), NSFFT/2,+1)
	    ENDDO
	    CALL FFTCV (BUFFER, NSFFT/2, NLFFT)
	    CALL FFTC1D (COLBUF, NLFFT)
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVWRIT (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE',LINE,' ')
	    ENDDO
	ELSE
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVREAD (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE',LINE,' ')
	    ENDDO
	    CALL FFTC1D (COLBUF, NLFFT)
	    CALL FFTCV (BUFFER, NSFFT/2, NLFFT)
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL FIXREAL (BUFFER(BUFPTR), COLBUF(LINE), NSFFT/2, -1)
		CALL FFTC1D (BUFFER(BUFPTR), NSFFT/2)
		CALL XVWRIT (UNIT, BUFFER(BUFPTR), STATUS, 
     +		'LINE',LINE,' ')
	    ENDDO
	ENDIF

	ENDIF

	RETURN
	END





	SUBROUTINE MAKEBITREV(BITREV,NMAX)
	IMPLICIT NONE
	INTEGER	I,IREV,J,K,L,M,LMAX,NMAX
	INTEGER*2 BITREV(1)
	INTEGER LOG2

	LMAX=LOG2(NMAX)
	I=0
	DO L=0,LMAX
	    IREV=0
	    DO J=0,2**L-1
		I=I+1
		BITREV(I)=IREV
		DO K=1,L
		    M=2**(L-K)
		    IF (IREV.LT.M) GO TO 70
		    IREV=IREV-M
		END DO
70		IREV=IREV+M
	    END DO
	END DO

	RETURN
	END



	SUBROUTINE MAKEPHASE (PHASE, SIGN, NMAX)
	IMPLICIT NONE
	INTEGER	I,J,L,LMAX,NMAX,SIGN, LOG2
	COMPLEX	PHASE(1)

	LMAX=LOG2(NMAX)
	J=0
	DO L=0,LMAX
	    DO I=0,2**L-1
		J=J+1
		PHASE(J) = CEXP(SIGN*2*3.1415926535*(0,1)*FLOAT(I)/2**L)
	    END DO
	END DO

	RETURN
	END




	SUBROUTINE FFTC1D (DATA, N)
	IMPLICIT NONE
	INTEGER	I,J,K,N,LN,M0,M1,JMAX,OFF,IDXPH,POWER, LOG2
	INTEGER*2 BITREV(65536)
	COMPLEX	DATA(1),TMP,PHASE(65536)
	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE

	LN = LOG2(N)
	DO I = 0,N-1
	    J = BITREV(I+N)
	    IF (J .GT. I) THEN
		TMP = DATA(I+1)
		DATA(I+1) = DATA(J+1)
		DATA(J+1) = TMP
	    END IF
	END DO

	M0 = 1
	M1 = 2
	JMAX = N/2
	DO J = 1,JMAX
	    TMP = DATA(M1)
	    DATA(M1) = DATA(M0) - TMP
	    DATA(M0) = DATA(M0) + TMP
	    M0 = M0 + 2
	    M1 = M1 + 2
	END DO

	DO I = 1,LN-1
	    POWER = 2**I
	    OFF = 2*POWER
	    M0 = 0
	    M1 = POWER
	    JMAX = 2**(LN-I-1)
	    DO J = 1,JMAX
		IDXPH = OFF
		DO K = 0,POWER-1
		    M0 = M0 + 1
		    M1 = M1 + 1
		    TMP = PHASE(IDXPH)*DATA(M1)
		    IDXPH = IDXPH + 1
		    DATA(M1) = DATA(M0) - TMP
		    DATA(M0) = DATA(M0) + TMP
		END DO
		M0 = M0 + POWER
		M1 = M1 + POWER
	    END DO
	END DO

	RETURN
	END





	SUBROUTINE FIXREAL(DATA,COLBUF,N,DIR)
	IMPLICIT NONE
	INTEGER N,DIR,I,ICONJ,IDXPH
	REAL PHR(2),TMPR
	COMPLEX DATA(1),COLBUF, PHASE(65536), TMP0,TMP1, PH
	EQUIVALENCE (PHR(1),PH)
	COMMON  /PHASE/  PHASE

	IDXPH = 2*N+1
	ICONJ = N

	IF (DIR .EQ. +1) THEN
	    COLBUF = REAL(DATA(1))-AIMAG(DATA(1))
	    DATA(1) = REAL(DATA(1))+AIMAG(DATA(1))
	    DO I = 2,N/2+1
		PH = PHASE(IDXPH)
		IDXPH = IDXPH + 1
		TMPR = PHR(1)		! MULT BY i
		PHR(1) = -PHR(2)
		PHR(2) = TMPR
		TMP0 = DATA(I)+CONJG(DATA(ICONJ))
		TMP1 = PH*( DATA(I)-CONJG(DATA(ICONJ)) )
		DATA(I) = (TMP0-TMP1)/2
		DATA(ICONJ) = CONJG(TMP0+TMP1)/2
		ICONJ = ICONJ-1
	    ENDDO
	ELSE
	    DATA(1) = ( REAL(DATA(1)) + REAL(COLBUF) )
     +			+ (0,1)*( REAL(DATA(1)) - REAL(COLBUF) )
	    DO I = 2,N/2+1
		PH = PHASE(IDXPH)
		IDXPH = IDXPH + 1
		TMPR = PHR(1)		! MULT BY -i
		PHR(1) = PHR(2)
		PHR(2) = -TMPR
		TMP0 = DATA(I)+CONJG(DATA(ICONJ))
		TMP1 = PH*( DATA(I)-CONJG(DATA(ICONJ)) )
		DATA(I) = TMP0-TMP1
		DATA(ICONJ) = CONJG(TMP0+TMP1)
		ICONJ = ICONJ-1
	    ENDDO
	ENDIF

	RETURN
	END




	SUBROUTINE FFTC1VD (UNIT, NS, NL)
	IMPLICIT NONE
	INTEGER	I,J,K,C,NS,NL,LNL,ROW0,ROW1,JMAX,OFF
	INTEGER*2 BITREV(65536)
	COMPLEX	PHASE(65536)
	INTEGER	POWER,STAT, LOG2, UNIT
	COMPLEX	DATA0(32768),DATA1(32768),TMP0,TMP1,PH
	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE

	LNL=LOG2(NL)
	DO I=0,NL-1
	    J=BITREV(I+NL)
	    IF (J.GT.I) THEN
		CALL XVREAD(UNIT, DATA0, STAT, 'LINE',I+1,' ')
		CALL XVREAD(UNIT, DATA1, STAT, 'LINE',J+1,' ')
		CALL XVWRIT(UNIT, DATA0, STAT, 'LINE',J+1,' ')
		CALL XVWRIT(UNIT, DATA1, STAT, 'LINE',I+1,' ')
	    END IF
	END DO


	DO I=0,LNL-1
	    JMAX=2**(LNL-I-1)
	    POWER=2**I
	    OFF=2*POWER
	    DO J=0,JMAX-1
		ROW0=OFF*J
		ROW1=ROW0+POWER
		DO K=0,POWER-1
		    ROW0=ROW0+1
		    ROW1=ROW1+1
		    PH=PHASE(K+OFF)
		    CALL XVREAD(UNIT, DATA0, STAT, 'LINE',ROW0,' ')
		    CALL XVREAD(UNIT, DATA1, STAT, 'LINE',ROW1,' ')
		    DO C=1,NS
			TMP0=DATA0(C)
			TMP1=PH*DATA1(C)
			DATA0(C)=TMP0+TMP1
			DATA1(C)=TMP0-TMP1
		    END DO
		    CALL XVWRIT(UNIT, DATA0, STAT, 'LINE',ROW0,' ')
		    CALL XVWRIT(UNIT, DATA1, STAT, 'LINE',ROW1,' ')
		END DO
	    END DO
	END DO

	RETURN
	END




	SUBROUTINE FFTCV(DATA,NS,NL)
	IMPLICIT NONE
	INTEGER	I,J,K,C,NS,NL,LNL,M0,M1,JMAX,OFF, LOG2
	INTEGER POWER,IDXPH
	INTEGER*2 BITREV(65536)
	COMPLEX	DATA(1),TMP,PHASE(65536),PH
	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE

	LNL = LOG2(NL)
	DO I = 0,NL-1
	    J = BITREV(I+NL)
	    IF (J .GT. I) THEN
		M0 = NS*I
		M1 = NS*J
		DO C = 1,NS
		    M0 = M0 + 1
		    M1 = M1 + 1
		    TMP = DATA(M0)
		    DATA(M0) = DATA(M1)
		    DATA(M1) = TMP
		END DO
	    END IF
	END DO


	DO I = 0,LNL-1
	    JMAX = 2**(LNL-I-1)
	    POWER = 2**I
	    OFF = 2*POWER
	    M0 = 0
	    M1 = POWER*NS
	    DO J = 0,JMAX-1
		IDXPH = OFF
		DO K = 0,POWER-1
		    PH = PHASE(IDXPH)
		    IDXPH = IDXPH + 1
		    DO C = 1,NS
			M0 = M0 + 1
			M1 = M1 + 1
			TMP = PH*DATA(M1)
			DATA(M1) = DATA(M0) - TMP
			DATA(M0) = DATA(M0) + TMP
		    END DO
		END DO
		M0 = M0 + POWER*NS
		M1 = M1 + POWER*NS
	    END DO
	END DO

	RETURN
	END




	INTEGER FUNCTION LOG2(N)
	IMPLICIT NONE
	INTEGER	N
	LOG2 = 2
	DO WHILE (ISHFT(N,-LOG2) .GT. 0)
	    LOG2 = LOG2 + 1
	ENDDO
	LOG2 = LOG2 - 1
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fracgen.imake
#define  PROGRAM   fracgen

#define MODULE_LIST fracgen.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create fracgen.pdf
PROCESS HELP=*
PARM OUT TYPE=STRING
PARM NS TYPE=INTEGER DEFAULT=256 VALID=4:4096
PARM NL TYPE=INTEGER DEFAULT=256 VALID=4:4096
PARM FORMAT TYPE=STRING DEFAULT=BYTE
PARM FRACDIM  TYPE=REAL DEFAULT=2.15  VALID=(0.0:3.0)
PARM POWER   TYPE=REAL DEFAULT=1.0
PARM SEED TYPE=INTEGER DEFAULT=--     COUNT=(0:1)
PARM IDSNAM TYPE=STRING DEFAULT="fracgen_scr"

!# annot function="Generating Synthetic Images"
!# annot keywords=(image,elevation,brownian,motion)

END-PROC
.TITLE
Simulates elevation data via fractional brownian motion
.HELP
PURPOSE:

fracgen generates images which are similar to elevation images of mountainous
regions using the fractional Brownian motion process.

EXECUTION:

fracgen  fake.img NL=300 NS=150  FORMAT=HALF +
	 FRACDIM=2.30 POWER=2.0 SEED=32161267

This example, which uses all the parameters, generates a 128x128 image that has
a fractal dimension of 2.30 and starts the random number generator with that
particular seed.  The fractal dimension parameter governs the roughness of the
terrain:  try values between 2.0 and 2.5, the default is 2.15.  The elevations
are raised to the POWER power.  If the power is greater than one this has the
effect of smoothing out the valleys relative to the mountains.  Using a partic-
ular value for the SEED parameter allows the generation of the same terrain.
If the SEED is defaulted, a random value is used for the seed.  

fracgen fake.img

This example will generate a 256x256 image with fractal dimension 2.15.


OPERATION:

fracgen basically just filters white noise with a power law filter.  More
specifically it generates random phases and random amplitudes, multiplies
the amplitudes by a power law ( 1/f**(power),  f is frequency), and FFT's
them to get the fake elevation image.  The elevations are raised to the POWER
power and are scaled to fit into the format.  Because an FFT must be
performed this program can take a large amount of CPU time.

.page
NOTE:

fracgen creates a large (roughly 8000 blocks) temporary scratch file named
by the user but defaulted to fracgen.scr.  The file is automatically deleted 
on successful completion of the program.

RESTRICTIONS:

The maximum image size is 4096 in either direction.

Original Programmer:	Frank Evans
Cognizant Programmer:	Frank Evans

Revisions:              Megan O'Shaughnessy, 3-12-1990
                          Changed destination of temporary scratch file to
                          V2$SCRATCH.

			George A. Madrid Jr, 6-AUG-1993 
			  Changed file specification so that it can be set
			  by the user when running the pdf.  Put in parameter
			  IDSNAM to accomplish this. Ported to UNIX.

.LEVEL1
.VARIABLE OUT
Output image
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE FORMAT
The output format
.VARIABLE FRACDIM
The fractal dimension
.VARIABLE POWER
The exponent the elevation
is raised to.
.VARIABLE SEED
The random generator seed
.VARIABLE IDSNAM
Name of Intermediate Data Set
.LEVEL2
.VARIABLE OUT
The output file name
.VARIABLE NS
NS can be used in conjunction with NL in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of samples per line for output.
.VARIABLE NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
.VARIABLE FORMAT
The FORMAT parameter is defaulted to BYTE
.VARIABLE FRACDIM
The FRACDIM parameter is the fractal dimension parameter chosen to 
determine the roughness of a given terrain.  The default value is 
2.15.
.VARIABLE POWER
The POWER parameter is used to smooth out valleys relative to mountains
for a certain terrain.  The default value is 1.0.
.VARIABLE SEED
Using a particular value for the SEED parameter allows the generation of 
the same terrain.  If the SEED is defaulted, a random value is used for 
the seed.
.VARIABLE IDSNAM
IDSNAM is an optional parameter which can be used to specify the
name of the intermediate data set.  This is often used to change the
location of the data set.  This data set is usually slightly larger than
the output file, but may be up to 2 times as large in some cases.

For example, if the user's current directory is USERDISK:[USER],
and he/she does not have enough disk space for IDS, then he/she could
input IDSNAM="fracgen_scr" or some such name to ensure that
the IDS ends up in a valid directory with enough disk space to hold it.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfracgen.pdf
procedure help=*
!------------------------------------------------------------------------------
! tstfracgen.pdf  Unit test for the program fracgen
! written by: G. A. Madrid Jr.   3-AUG-1993
! ported to UNIX by: CRS (CRI)  1-JULY-1994
!------------------------------------------------------------------------------
refgbl $becho
refgbl $autousage

body
let $autousage = "none"
let $becho="yes"

write "THIS IS A TEST OF MODULE FRACGEN"
write "TESTING PROGRAM FRACGEN WITH DEFAULTS EXCEPT FOR SEED VALUE"
fracgen filea seed=32161267
write "LIST THE RESULT OF THE DEFAULT PARAMETERS"
list filea (1,1,5,10)
list filea (201,201,5,10)

write "CREATE AN IMAGE WHERE THE NUMBER OF LINES EXCEEDS THE NUMBER OF SAMPLES"
fracgen fileb nl=100 ns=50 seed=32161267
write "NUMBER OF LINES = 100; NUMBER OF SAMPLES = 50"
list fileb (1,1,5,10)
list fileb (50,25,5,10)

write "CREATE AN IMAGE WHERE THE NUMBER OF SAMPLES EXCEEDS THE NUMBER OF LINES"
fracgen filec nl=50 ns=100 seed=32161267
write "NUMBER OF LINES = 50; NUMBER OF SAMPLES = 100"
list filec (1,1,5,10)
list filec (25,50,5,10)

write "CREATE AN IMAGE WITH IMAGE FORMAT REAL"
fracgen filea format=real seed=32161267
write "LIST SOME DN VALUES OF THE IMAGE"
list filea (1,1,5,10)
list filea (201,201,5,10)

write "SET FRACTAL DIMENSION TO 2.30"
fracgen filea fracdim=2.30 seed=32161267
write "LIST THE RESULTS WITH PARAMETER FRACTAL DIMENSION = 2.30"
list filea (1,1,5,10)
list filea (201,201,5,10)

write "SET POWER VALUE TO 2.0"
fracgen filea power=2.0 seed=32161267
write "LIST THE RESULTS WITH PARAMETER POWER = 2.0"
list filea (1,1,5,10)
list filea (201,201,5,10)

write "CREATE AN IMAGE WITHOUT SPECIFYING A SEED VALUE"
fracgen nostart nl=256 ns=256 fracdim=2.30 power=2.0
write "LIST SOME REPRESENTATIVE DN VALUES"
list nostart (1,1,5,10)
list nostart (201,201,5,10)

write "CREATE AN IMAGE BUT ALSO SPECIFY A SEED VALUE"
fracgen start nl=256 ns=256 fracdim=2.30 power=2.0 seed=32161267
write "LIST SOME REPRESENTATIVE DN VALUES"
list start (1,1,5,10)
list start (201,201,5,10)

write "COMPARE THE PREVIOUS TWO IMAGES AND PRINT DIFFERENCES"
difpic inp=(nostart,start) out=xdiff

write "CREATE ANOTHER FILE WITH SAME SEED VALUE"
fracgen sstart nl=256 ns=256 fracdim=2.30 power=2.0 seed=32161267
write "LIST SOME REPRESENTATIVE DN VALUES"
list sstart (1,1,5,10)
list sstart (201,201,5,10)

write "NOW COMPARE THE TWO IMAGES WITH THE SAME SEED VALUE"
difpic inp=(start,sstart) out=xdiff

end-proc
$ Return
$!#############################################################################
