       PROGRAM  ccomp
C#######################################################################
C  NAME OF ROUTINE
C      ccomp (Convert Complex)
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      ccmp is a VICAR applications program which performs conversions
C      between comples pixel format and two real format images.  Two
C      types of transformations are possible (amplitude and phase) or
C      (real and imaginary).  The transformations may be done in either
C      direction:  a complex image to two real images, or two real
C      images to a complex image.
C
C  PR[CEPARED FOR USE ON MIPL SYSTEM BY
C      FRANK EVANS
C  ORIGINAL CCOMP PROGRAM BY
C      FRANK EVANS
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     4-94  CRI  MSTP (S/W CONVERSION) VICAR PORTING
C
C    CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      ccomp INP=a OUT=(b,c) optional parameters
C        or
C      ccomp INP=(b,c) OUT=a optional parameters
C
C      ccomp a (b,c) optional parameters
C        or
C      ccomp (b,c) a optional parameters
C
C       Here 'a' represents the input or output complex image file name,
C       'b' represents the input or output image real or amplitude file name.
C       'c' represents the input or output image imaginary or phase file name.
C         When (b,c) are inputs or outputs they are paired as:
C              (real,imaginary) or (amplitude,phase) 
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name(s).
C      OUT    - Output file name(s).
C      POLAR  - for amplitude and phase (default).
C      RECTANG- for real and imaginary.
C      FORWARD- for complex input (default).
C      INVERSE- for complex output.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file(s).
C  PROGRAM LIMITATIONS
C      1. The input image must be COMP data for keyword FORWARD.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44 
C
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by user to perform translation.
C#######################################################################

	IMPLICIT  NONE
	INTEGER*4 IN1UNIT, IN2UNIT, OUT1UNIT, OUT2UNIT, STATUS
	INTEGER*4 SL, SS, NL, NS, NLI, NSI, LINE, SAMP
	COMPLEX*8 COMPBUF(4096)
	REAL*4	  REAL1BUF(4096), REAL2BUF(4096)
	REAL*4	  AMP
	CHARACTER*8  INFORMAT
	LOGICAL*4 PHASEAMP
        LOGICAL*4 XVPTST

        CALL IFMESSAGE ('CCOMP version 18 Dec 2012 (64-bit) - rjb') 

        CALL XVEACTION ('SA',' ')

	PHASEAMP = XVPTST('POLAR')


	IF (XVPTST('INVERSE')) GOTO 2000



	CALL XVUNIT (IN1UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN (IN1UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'COMP',' ')		!formerly COMPLEX
	CALL XVSIZE (SL, SS, NL, NS, NLI, NSI)
	CALL XVGET (IN1UNIT, STATUS, 'FORMAT', INFORMAT, ' ')
	IF (INFORMAT(1:4) .NE. 'COMP') THEN
	    CALL MABEND (' Input must be complex ')
	ENDIF


	CALL XVUNIT (OUT1UNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN (OUT1UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT','REAL',
     +		'U_NL',NL, 'U_NS',NS, ' ')
	CALL XVUNIT (OUT2UNIT,'OUT',2,STATUS,' ')
	CALL XVOPEN (OUT2UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT','REAL',
     +		'U_NL',NL, 'U_NS',NS, ' ')


	DO LINE = SL, NL+SL-1
	    CALL XVREAD (IN1UNIT, COMPBUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    IF (PHASEAMP) THEN
		DO SAMP = 1, NS
		    AMP = CABS(COMPBUF(SAMP))
		    REAL1BUF(SAMP) = AMP
		    IF (AMP .EQ. 0) THEN
			REAL2BUF(SAMP) = 0.0
		    ELSE
			REAL2BUF(SAMP) = ATAN2( AIMAG(COMPBUF(SAMP)), 
     +					   REAL(COMPBUF(SAMP))    )
		    ENDIF
		ENDDO
	    ELSE
		DO SAMP = 1, NS
		    REAL1BUF(SAMP) = REAL(COMPBUF(SAMP))
		    REAL2BUF(SAMP) = AIMAG(COMPBUF(SAMP))
		ENDDO
	    ENDIF
	    CALL XVWRIT (OUT1UNIT, REAL1BUF, STATUS, ' ')
	    CALL XVWRIT (OUT2UNIT, REAL2BUF, STATUS, ' ')
	ENDDO

	CALL XVCLOSE (IN1UNIT,STATUS,' ')
	CALL XVCLOSE (OUT1UNIT,STATUS,' ')
	CALL XVCLOSE (OUT2UNIT,STATUS,' ')

	RETURN



2000	CONTINUE


	CALL XVUNIT(IN1UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN(IN1UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'REAL', ' ')
	CALL XVSIZE (SL, SS, NL, NS, NLI, NSI)

	CALL XVUNIT(IN2UNIT,'INP',2,STATUS,' ')
	CALL XVOPEN(IN2UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'REAL', ' ')


	CALL XVUNIT (OUT1UNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN (OUT1UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','COMP', 'O_FORMAT','COMP',		!formerly COMPLEX
     +		'U_NL',NL, 'U_NS',NS, ' ')


	DO LINE = SL, NL+SL-1
	    CALL XVREAD (IN1UNIT, REAL1BUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    CALL XVREAD (IN2UNIT, REAL2BUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    IF (PHASEAMP) THEN
		DO SAMP = 1, NS
		    COMPBUF(SAMP) = REAL1BUF(SAMP)*EXP((0,1)*REAL2BUF(SAMP))
		ENDDO
	    ELSE
		DO SAMP = 1, NS
                    COMPBUF(SAMP) = CMPLX(REAL1BUF(SAMP),REAL2BUF(SAMP)) 
		ENDDO
	    ENDIF
	    CALL XVWRIT (OUT1UNIT, COMPBUF, STATUS, ' ')
	ENDDO

	CALL XVCLOSE (IN1UNIT,STATUS,' ')
	CALL XVCLOSE (IN2UNIT,STATUS,' ')
	CALL XVCLOSE (OUT1UNIT,STATUS,' ')


	RETURN
	END
