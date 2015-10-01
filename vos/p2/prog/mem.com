$!****************************************************************************
$!
$! Build proc for MIPL module mem
$! VPACK Version 1.9, Wednesday, September 25, 2002, 12:06:40
$!
$! Execute by entering:		$ @mem
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
$ write sys$output "*** module mem ***"
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
$ write sys$output "Invalid argument given to mem.com file -- ", primary
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
$   if F$SEARCH("mem.imake") .nes. ""
$   then
$      vimake mem
$      purge mem.bld
$   else
$      if F$SEARCH("mem.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mem
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mem.bld "STD"
$   else
$      @mem.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mem.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mem.com -mixed -
	-s mem.f mem.inc -
	-i mem.imake -
	-p mem.pdf -
	-t tstmem.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mem.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM MEM
C
C	PERFORMS MAXIMUM ENTROPY DECONVOLUTION.
C
C
C	VERSION :	NEW		JANUARY 1986
C
c                B      ported 7,1998  J. Lorre
C	REVISION A:	ADDED DIRECT CONVOLUTION, FIX BUGS
C			KFE	FEBRUARY 1986
C
C
C	ORIGINAL PROGRAMMER :		K. F. EVANS
C
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	INCLUDE 'mem.inc'

	INTEGER	LOG2, SCRNUM
	REAL	MAXFORMAT
	CHARACTER*32 FORMAT
	CHARACTER*40 STRING
	LOGICAL	XVPTST
        integer junk1,junk2

	DATA	DATA/1/, PSF/2/, DEFAULT/3/, OUT/4/, XFER/5/, TMPFFT/6/,
     +      DEF/7/, CURRENT/8/, GRDE/9/, CNVSTP/9/, RESID/10/, STEP/11/ 

	
C ******************************** START OF EXECUTABLE CODE


C		GET THE INPUT PARAMETERS
	CALL XVP ('MAXITER', MAXITER, COUNT)
	CALL XVP ('RMSERROR', TARGETRMS, COUNT)
	CALL XVP ('MAXCRIT', MAXCRIT, COUNT)

	RESTART = XVPTST ('RESTART')


C		OPEN THE CONVOLVED (BLURRED) IMAGE
	CALL XVUNIT (UNIT(DATA), 'INP', 1, STATUS,' ')
	CALL XVOPEN (UNIT(DATA), STATUS, 'IO_ACT', 'SA',
     +		'OPEN_ACT', 'SA', 'U_FORMAT', 'REAL',' ')
	CALL XVSIZE (SL,SS,NL,NS,junk1,junk2)
	IF (NL .GT. 4096 .OR. NS .GT. 4096) THEN
	    CALL xvmessage ('Maximum image size exceeded.',' ')
	    CALL ABEND
	ENDIF
	NPIXELS = FLOAT(NL*NS)
	CALL XVGET (UNIT(DATA), STATUS, 'FORMAT', FORMAT,' ')
	IF (FORMAT(1:4) .EQ. 'HALF' .OR. FORMAT(1:4) .EQ. 'WORD') THEN
	    MAXFORMAT = 32767.0
	ELSE IF (FORMAT(1:4) .EQ. 'FULL') THEN
	    MAXFORMAT = 2147483647.0
	ELSE IF (FORMAT(1:4) .EQ. 'BYTE') THEN
	    MAXFORMAT = 255.0
	ENDIF



C		OPEN THE POINT SPREAD FUNCTION (PSF)
	CALL XVUNIT (UNIT(PSF), 'INP', 2, STATUS,' ')
	CALL XVOPEN (UNIT(PSF), STATUS, 'IO_ACT', 'SA', 'OPEN_ACT', 'SA',
     +		'U_FORMAT', 'REAL',' ')
	CALL XVGET (UNIT(PSF), STATUS, 'NL',NLPSF, 'NS',NSPSF,' ')

	IF (NLPSF .LE. 16 .AND. NSPSF .LE. 16)  THEN
	    SMALLPSF = .TRUE.
	ELSE
	    SMALLPSF = .FALSE.
	ENDIF


C		CALCULATE THE SIZE NEEDED FOR THE FFT 
C		    MUST BE BIG ENOUGH SO THAT THE PSF WON'T WRAP AROUND
	NLFFT = 2**( LOG2(NL+NLPSF-1) + 1 )
	NSFFT = 2**( LOG2(NS+NSPSF-1) + 1 )



C		CREATE AND OPEN THE SCRATCH FILES
	SCRNUM = 0
	IF (.NOT. SMALLPSF) THEN
	    CALL OPENSCRATCH (UNIT(XFER), 'MEM1', 
     +						SCRNUM,	NLFFT, NSFFT)
	    CALL OPENSCRATCH (UNIT(TMPFFT), 'MEM2', 
     +						SCRNUM, NLFFT, NSFFT)
	ENDIF
	CALL OPENSCRATCH (UNIT(DEF), 'MEM3',
     +						SCRNUM, NL, NS)
	CALL OPENSCRATCH (UNIT(GRDE), 'MEM4', 
     +						SCRNUM, NL, NS)
	CALL OPENSCRATCH (UNIT(RESID), 'MEM5', 
     +						SCRNUM, NL, NS)
	CALL OPENSCRATCH (UNIT(STEP), 'MEM6', 
     +						SCRNUM, NL, NS)
	CALL OPENSCRATCH (UNIT(CURRENT), 'MEM7', 
     +						SCRNUM, NL, NS)



	IF (SMALLPSF) THEN
	    CALL READPSF
	ELSE
C		MAKE THE TRANSFER FUNCTION FROM THE PSF
	    CALL MAKEXFER
	ENDIF
	CALL XVCLOSE (UNIT(PSF), STATUS,' ')



C		COPY OR MAKE THE DEFAULT IMAGE
	CALL MAKEDEFAULT



	IF (RESTART) THEN
C		COPY THE OLD OUTPUT INTO THE CURRENT IMAGE SCRATCH
	    CALL XVUNIT( UNIT(OUT), 'OUT',1, STATUS,' ')
	    CALL XVOPEN( UNIT(OUT), STATUS, 'OP', 'READ', 
     +			'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     +			'U_FORMAT', 'REAL',' ')
	    IMAGEMIN = 1.0E20
	    DO LINE = 1, NL
		CALL XVREAD(UNIT(OUT), IMAGE(1,CURRENT), STATUS, 'LINE',LINE,
     +                       ' ')
		DO SAMP = 1, NS
		    IMAGE(SAMP,CURRENT) = MAX( IMAGE(SAMP,CURRENT), 1.0E-8)
		    IMAGEMIN = MIN (IMAGEMIN, IMAGE(SAMP,CURRENT) )
		ENDDO
		CALL XVWRIT (UNIT(CURRENT), IMAGE(1,CURRENT), STATUS, 
     +							'LINE',LINE,' ')
	    ENDDO
	    CALL XVCLOSE (UNIT(OUT), STATUS,' ')

	ELSE
C		COPY THE DEFAULT INTO THE CURRENT IMAGE TO START WITH
	    IMAGEMIN = 1.0E20
	    DO LINE = 1, NL
 		CALL XVREAD(UNIT(DEF), IMAGE(1,CURRENT), STATUS, 'LINE',LINE,
     +                       ' ')
		DO SAMP = 1, NS
		    IMAGEMIN = MIN (IMAGEMIN, IMAGE(SAMP,CURRENT) )
		ENDDO
		CALL XVWRIT (UNIT(CURRENT), IMAGE(1,CURRENT), STATUS, 
     +					'LINE',LINE,' ')
	    ENDDO
	ENDIF



C		MAKE THE STARTING RESIDUALS FROM THE CURRENT IMAGE AND THE DATA
	CALL MAKEFIRSTRESID
	CALL XVCLOSE (UNIT(DATA), STATUS,' ')


	TARGETERROR = NPIXELS* TARGETRMS**2
	TOL = 0.10
	PSFVOLUME = 1.0
	MAXSTEPLEN = 2.0
	ITER = 0



C		CALCULATE THE STARTING ALPHA
	RMSERROR = SQRT(ERROR/NPIXELS)
	CALL xvmessage (' ',' ')
	WRITE (STRING,103) 'Starting RMS Error : ', RMSERROR
103	FORMAT (A,F12.4)
	CALL xvmessage (STRING,' ')

	CALL CONVIMAGE (RESID, GRDE, .TRUE.)
	CALL FIRSTALPHA
	IF (RESTART)  CALL UPDATEALPHA
C	TYPE *,'Starting alpha : ',ALPHA

	CALL xvmessage (' ',' ')


C		* * *	START OF MEM ITERATIONS   * * *

100	CONTINUE

	    ITER = ITER + 1

	    CALL CALCSTEP

	    CALL CONVIMAGE (STEP, CNVSTP, .FALSE.)
	    CALL CALCSTEPLEN

	    CALL TAKESTEP
	    CALL MAKENEWRESID
	    CALL CONVIMAGE (RESID, GRDE, .TRUE.)

	    CALL UPDATEALPHA

	    CALL TERMOUTPUT

	IF ( ITER .LT. MAXITER      .AND.
     +	     ((SOLUCRIT - MAXCRIT)/MAXCRIT .GT. TOL  .OR.
     +	      ABS((RMSERROR-TARGETRMS)/TARGETRMS) .GT. TOL) )  GOTO 100


	IF (  ((SOLUCRIT - MAXCRIT)/MAXCRIT .LE. TOL)  .AND.
     +	      (ABS((RMSERROR-TARGETRMS)/TARGETRMS) .LE. TOL) )  THEN
	    CALL xvmessage (' ',' ')
	    CALL xvmessage ('      * * *  CONVERGENCE ACHIEVED  * * *  '
     +				,' ')
	ENDIF

C		* * *	END OF MEM ITERATIONS   * * *



C		COPY CURRENT IMAGE SCRATCH INTO OUTPUT MEM IMAGE
	CALL XVUNIT( UNIT(OUT), 'OUT',1, STATUS,' ')
	CALL XVOPEN( UNIT(OUT), STATUS, 'OP', 'WRITE', 
     +		'U_NL', NL,  'U_NS', NS,
     +		'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     +		'U_FORMAT', 'REAL', 'O_FORMAT',FORMAT,' ')

	DO LINE = 1, NL
	    CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +			 'LINE',LINE, ' ')
	    IF (FORMAT(1:4) .NE. 'REAL') THEN
		DO SAMP = 1, NS
		    IMAGE(SAMP,CURRENT) = 
     +			   MIN( IMAGE(SAMP,CURRENT) + 0.5, MAXFORMAT)
		ENDDO
	    ENDIF
	    CALL XVWRIT (UNIT(OUT), IMAGE(1,CURRENT), STATUS,
     +			 'LINE',LINE,' ')
	ENDDO
	CALL XVCLOSE (UNIT(OUT), STATUS,' ')



C		CLOSE AND DELETE SCRATCH FILES
	IF (.NOT. SMALLPSF) THEN
	    CALL XVCLOSE (UNIT(TMPFFT), STATUS,'CLOS_ACT','DELETE',' ')
	    CALL XVCLOSE (UNIT(XFER), STATUS, 'CLOS_ACT','DELETE',' ')
	ENDIF
	CALL XVCLOSE (UNIT(GRDE), STATUS, 'CLOS_ACT','DELETE',' ')
	CALL XVCLOSE (UNIT(CURRENT), STATUS, 'CLOS_ACT','DELETE',' ')
	CALL XVCLOSE (UNIT(DEF), STATUS, 'CLOS_ACT','DELETE',' ')
	CALL XVCLOSE (UNIT(RESID), STATUS, 'CLOS_ACT','DELETE',' ')
	CALL XVCLOSE (UNIT(STEP), STATUS, 'CLOS_ACT','DELETE',' ')


	RETURN
	END




	SUBROUTINE OPENSCRATCH (UNIT, NAME, SCRNUM, NL, NS)
C
C	CREATES AND OPENS A REAL SCRATCH FILE WITH NAME (NAME) 
C	    OF SIZE NL BY NS AND RETURNS THE UNIT NUMBER
C
	IMPLICIT NONE
	INTEGER	UNIT, NL, NS
	CHARACTER*32 NAME
	INTEGER	STATUS, SCRNUM

	SCRNUM = SCRNUM + 1
	CALL XVUNIT (UNIT,'   ',SCRNUM,  STATUS, 'U_NAME',NAME,' ')
	CALL XVOPEN (UNIT, STATUS, 'OP','WRITE',
     +				'U_NL',NL, 'U_NS',NS,
     +				'O_FORMAT','REAL', 
     +				'COND','LABELS', 'OPEN_ACT','SA',' ')
	CALL XVCLOSE (UNIT, STATUS,' ')
	CALL XVUNIT (UNIT,'   ',SCRNUM,  STATUS, 'U_NAME',NAME,' ')
	CALL XVOPEN (UNIT, STATUS, 'OP','UPDATE',
     +			'U_NL',NL, 'U_NS',NS,
     +			'I_FORMAT','REAL', 'U_FORMAT', 'REAL',
     +			'O_FORMAT','REAL',
     +			'IO_ACT', 'SA', 'OPEN_ACT', 'SA',' ')

	RETURN
	END



	SUBROUTINE MAKEDEFAULT
	INCLUDE 'mem.inc'
	REAL	DATAFLUX, AVGLEVEL
	INTEGER	INPCOUNT
	CHARACTER*132	INPFILES(3)

C		OPEN AND COPY THE DEFAULT IMAGE IF THERE IS ONE
	CALL XVP ('INP', INPFILES, INPCOUNT)
	IF (INPCOUNT .EQ. 3) THEN
	    DEFFLAG = .TRUE.
	ELSE
	    DEFFLAG = .FALSE.
	ENDIF

	IF (DEFFLAG) THEN
	    CALL XVUNIT (UNIT(DEFAULT), 'INP', 3, STATUS,' ')
	    CALL XVOPEN (UNIT(DEFAULT), STATUS, 'IO_ACT', 'SA', 
     +		'OPEN_ACT', 'SA', 'U_FORMAT', 'REAL',' ')
	    DO LINE = 1, NL
		CALL XVREAD(UNIT(DEFAULT), IMAGE(1,DEF), STATUS,
     +						 'LINE',LINE,' ')
		DO SAMP = 1, NS
		    IMAGE(SAMP,DEF) = MAX( IMAGE(SAMP,DEF), 1.0E-8 )
		ENDDO
		CALL XVWRIT (UNIT(DEF), IMAGE(1,DEF), STATUS,
     +						 'LINE',LINE,' ')
	    ENDDO
	    CALL XVCLOSE (UNIT(DEFAULT), STATUS,' ')
	ELSE
	    DATAFLUX = 0.0
	    DO LINE = 1, NL
		CALL XVREAD(UNIT(DATA), IMAGE(1,DATA), STATUS, 
     +					'LINE',LINE+SL-1, 'SAMP',SS,' ')
		DO SAMP = 1, NS
		    DATAFLUX = DATAFLUX + IMAGE(SAMP,DATA)
		ENDDO
	    ENDDO
	    AVGLEVEL = DATAFLUX/FLOAT(NL*NS)
	    DO SAMP = 1, NS
		IMAGE(SAMP,DEF) = AVGLEVEL
	    ENDDO
	    DO LINE = 1, NL
		CALL XVWRIT (UNIT(DEF), IMAGE(1,DEF), STATUS,
     +						 'LINE',LINE,' ')
	    ENDDO
	ENDIF

	RETURN
	END





	SUBROUTINE MAKEFIRSTRESID
	INCLUDE 'mem.inc'

C		CONVOLVE THE IMAGE WITH THE PSF AND SUBTRACT THE DATA
C		    IN ORDER TO MAKE THE STARTING RESIDUALS
	CALL CONVIMAGE (CURRENT, RESID, .FALSE.)
	ERROR = 0.0
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(DATA), IMAGE(1,DATA), STATUS, 
     +				'LINE',LINE+SL-1, 'SAMP',SS,' ')
	    CALL XVREAD(UNIT(RESID), IMAGE(1,RESID), STATUS,
     +				 'LINE',LINE,' ')
	    DO SAMP = 1, NS
		IMAGE(SAMP,RESID) = IMAGE(SAMP,RESID) - IMAGE(SAMP,DATA)
		ERROR = ERROR + IMAGE(SAMP,RESID)**2
	    ENDDO
	    CALL XVWRIT (UNIT(RESID), IMAGE(1,RESID), STATUS,
     +						 'LINE',LINE,' ')
	ENDDO

	RETURN
	END



	SUBROUTINE FIRSTALPHA
	INCLUDE 'mem.inc'
	REAL	GRAD(3), GHDGE, GEDGE

	GHDGE = 0.0
	GEDGE = 0.0
	IF (RESTART) THEN
	    DO LINE = 1, NL
	        CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS, 
     +				'LINE',LINE,' ')
	        CALL XVREAD(UNIT(DEF), IMAGE(1,DEF), STATUS,
     +				 'LINE',LINE,' ')
	        CALL XVREAD(UNIT(GRDE),IMAGE(1,GRDE),STATUS,
     +				'LINE',LINE,' ')
	        DO SAMP = 1, NS
		    GRAD(H) = -LOG( IMAGE(SAMP,CURRENT)/IMAGE(SAMP,DEF) )
		    GRAD(E) = 2.0*IMAGE(SAMP,GRDE)
		    GHDGE = GHDGE + GRAD(H)*GRAD(E)
		    GEDGE = GEDGE + GRAD(E)**2
	        ENDDO
	    ENDDO
	    ALPHA = GHDGE/GEDGE
	ELSE
	    DO LINE = 1, NL
	        CALL XVREAD(UNIT(GRDE),IMAGE(1,GRDE),STATUS,
     +				'LINE',LINE,' ')
	        DO SAMP = 1, NS
		    GRAD(E) = 2.0*IMAGE(SAMP,GRDE)
		    GEDGE = GEDGE + GRAD(E)**2
	        ENDDO
	    ENDDO
	    ALPHA = 1.0/SQRT(GEDGE/NPIXELS)
	ENDIF

	RETURN
	END






	SUBROUTINE TAKESTEP
	INCLUDE 'mem.inc'

	IMAGEMIN = 1.0E20
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +			 'LINE',LINE, ' ')
	    CALL XVREAD(UNIT(STEP), IMAGE(1,STEP), STATUS,
     +			 'LINE',LINE,' ')
	    DO SAMP = 1, NS
		IMAGE(SAMP,CURRENT) = IMAGE(SAMP,CURRENT)
     +				+ STEPLEN * IMAGE(SAMP,STEP)
		IMAGEMIN = MIN (IMAGEMIN, IMAGE(SAMP,CURRENT) )
	    ENDDO
	    CALL XVWRIT (UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +			 'LINE',LINE, ' ')
	ENDDO

	RETURN
	END



	SUBROUTINE MAKENEWRESID
	INCLUDE 'mem.inc'

	ERROR = 0.0
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(RESID), IMAGE(1,RESID), STATUS,
     +				 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(CNVSTP),IMAGE(1,CNVSTP),STATUS,
     +				'LINE',LINE,' ')
	    DO SAMP = 1, NS
		IMAGE(SAMP,RESID) = IMAGE(SAMP,RESID)
     +				+ STEPLEN * IMAGE(SAMP,CNVSTP)
		ERROR = ERROR + IMAGE(SAMP,RESID)**2
	    ENDDO
	    CALL XVWRIT (UNIT(RESID), IMAGE(1,RESID), STATUS,
     +				'LINE',LINE,' ')
	ENDDO
	RMSERROR = SQRT(ERROR/NPIXELS)

	RETURN
	END



	SUBROUTINE TERMOUTPUT
	INCLUDE 'mem.inc'
	CHARACTER*60  STRING

	WRITE (STRING,101) ' Iteration ',ITER, ' of ', MAXITER
101	FORMAT (A,I3,A,I3)
	CALL xvmessage (STRING,' ')
	WRITE (STRING,103) '     RMS Error          : ',RMSERROR
103	FORMAT (A,F12.4)
	CALL xvmessage (STRING,' ')
	WRITE (STRING,105) '     Solution Criterion : ',SOLUCRIT
105	FORMAT (A,F8.4)
	CALL xvmessage (STRING,' ')

C	TYPE *,'     Step Length        : ',STEPLEN
C	TYPE *,'     Alpha              : ', ALPHA

	RETURN
	END






	REAL FUNCTION CALCJAY (LENGTH)
	INCLUDE 'mem.inc'
	REAL	LENGTH
	REAL	ENTROPY, OUTPIXEL

	ENTROPY = 0.0
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +				 'LINE',LINE, ' ')
	    CALL XVREAD(UNIT(STEP), IMAGE(1,STEP), STATUS,
     +				 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(DEF), IMAGE(1,DEF), STATUS,
     +				 'LINE',LINE,' ')
	    DO SAMP = 1, NS
		OUTPIXEL = IMAGE(SAMP,CURRENT)+LENGTH*IMAGE(SAMP,STEP)
		ENTROPY = ENTROPY - OUTPIXEL
     +			  *( LOG(OUTPIXEL/IMAGE(SAMP,DEF)) - 1.0 )
	    ENDDO
	ENDDO

	CALCJAY = ENTROPY - ALPHA*(E0 + 2*E1*LENGTH + E2*LENGTH**2)

	RETURN
	END



	SUBROUTINE CALCSTEPLEN
	INCLUDE 'mem.inc'
	REAL	LENLOW, LENUP, LENA, LENB
	REAL	JAYLOW, JAYUP, JAYA, JAYB, CALCJAY

	E0 = ERROR
	E1 = 0.0
	E2 = 0.0
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(RESID), IMAGE(1,RESID), STATUS,
     +				 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(CNVSTP),IMAGE(1,CNVSTP),STATUS,
     +				'LINE',LINE,' ')
	    DO SAMP = 1, NS
		E1 = E1 + IMAGE(SAMP,RESID) * IMAGE(SAMP,CNVSTP)
		E2 = E2 + IMAGE(SAMP,CNVSTP)**2
	    ENDDO
	ENDDO


	LENLOW = 0.0
	LENUP = MAXSTEPLEN

	LENA = LENUP - 0.618*(LENUP-LENLOW)
	JAYA = CALCJAY(LENA)
	LENB = LENLOW + 0.618*(LENUP-LENLOW)
	JAYB = CALCJAY(LENB)

	DO WHILE (ABS((LENB-LENA)/LENA) .GT. 0.05)
	    IF (JAYA .GT. JAYB) THEN
		LENUP = LENB
		JAYUP = JAYB
		LENB = LENA
		JAYB = JAYA
		LENA = LENUP - 0.618*(LENUP-LENLOW)
		JAYA = CALCJAY (LENA)
	    ELSE
		LENLOW = LENA
		JAYLOW = JAYA
		LENA = LENB
		JAYA = JAYB
		LENB = LENLOW + 0.618*(LENUP-LENLOW)
		JAYB = CALCJAY (LENB)
	    ENDIF
	ENDDO

	IF (JAYA .GT. JAYB)  THEN
	    STEPLEN = LENA
	ELSE
	    STEPLEN = LENB
	ENDIF

	MAXSTEPLEN = 2.0*STEPLEN

	RETURN
	END




	SUBROUTINE CALCSTEP 
	INCLUDE 'mem.inc'
	REAL	TMP, INVGRDGRDJ, DELPIX
	REAL	GRAD(3)

	TMP = 2.0*ALPHA*PSFVOLUME

	DO LINE = 1, NL
	    CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +					'LINE',LINE,' ')
	    CALL XVREAD(UNIT(DEF), IMAGE(1,DEF), STATUS,
     +					 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(GRDE), IMAGE(1,GRDE), STATUS,
     +					 'LINE',LINE,' ')
	    DO SAMP = 1, NS
		GRAD(H) = -LOG( IMAGE(SAMP,CURRENT)/IMAGE(SAMP,DEF) )
		GRAD(E) = 2.0*IMAGE(SAMP,GRDE)
		GRAD(J) = GRAD(H) - ALPHA*GRAD(E)
		INVGRDGRDJ = 1.0/( 1.0/IMAGE(SAMP,CURRENT) + TMP)
		DELPIX = INVGRDGRDJ * GRAD(J)
		DELPIX = MAX( DELPIX, 
     +			  (0.1*IMAGEMIN-IMAGE(SAMP,CURRENT))/MAXSTEPLEN )
		IMAGE(SAMP,STEP) = DELPIX
	    ENDDO
	    CALL XVWRIT (UNIT(STEP), IMAGE(1,STEP), STATUS,
     +					 'LINE',LINE,' ')
	ENDDO

	RETURN
	END




	SUBROUTINE UPDATEALPHA
	INCLUDE 'mem.inc'
	INTEGER	I1, I2
	REAL	TMP, INVGRDGRDJ
	REAL	GRAD(3)
	REAL	A, B, C, DELTAALPHA, ALPHAUPBND, ALPHALOWBND

	DO I1 = 1, 3
	    DO I2 = 1, 3
		GRADDOTGRAD(I1,I2) = 0.0
	    ENDDO
	ENDDO

	TMP = 2.0*ALPHA*PSFVOLUME

	DO LINE = 1, NL
	    CALL XVREAD(UNIT(CURRENT), IMAGE(1,CURRENT), STATUS,
     +					 'LINE',LINE, ' ')
	    CALL XVREAD(UNIT(DEF), IMAGE(1,DEF), STATUS,
     +					 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(GRDE), IMAGE(1,GRDE), STATUS,
     +					'LINE',LINE,' ')
	    DO SAMP = 1, NS
		GRAD(H) = -LOG( IMAGE(SAMP,CURRENT)/IMAGE(SAMP,DEF) )
		GRAD(E) = 2.0*IMAGE(SAMP,GRDE)
		GRAD(J) = GRAD(H) - ALPHA*GRAD(E)
		INVGRDGRDJ = 1.0/( 1.0/IMAGE(SAMP,CURRENT) + TMP)
		DO I1 = 1, 3
		    DO I2 = I1, 3
			GRADDOTGRAD(I1,I2) = GRADDOTGRAD(I1,I2) +
     +				GRAD(I1) * INVGRDGRDJ * GRAD(I2)
		    ENDDO
		ENDDO
	    ENDDO
	ENDDO

	DO I1 = 1, 3
	    DO I2 = I1, 3
		GRADDOTGRAD(I2,I1) = GRADDOTGRAD(I1,I2)
	    ENDDO
	ENDDO


	COMPOFGRADJ = GRADDOTGRAD(H,H) + ALPHA**2 *GRADDOTGRAD(E,E)
	IF (COMPOFGRADJ .GT. 0.0) THEN
	    SOLUCRIT = GRADDOTGRAD(J,J) / COMPOFGRADJ
	ELSE
	    SOLUCRIT = 0.0
	ENDIF



	IF (SOLUCRIT .LT. MAXCRIT) THEN

	    DELTAALPHA = (ERROR - TARGETERROR)/ GRADDOTGRAD(E,E)

C	    A = (1-MAXCRIT)* GRADDOTGRAD(E,E)
C	    B = -GRADDOTGRAD(J,E) - MAXCRIT*ALPHA*GRADDOTGRAD(E,E)
C	    C = GRADDOTGRAD(J,J) - MAXCRIT *COMPOFGRADJ

	    A = GRADDOTGRAD(E,E)		! Tim Cornwell's method
	    B = -GRADDOTGRAD(J,E)
	    C = GRADDOTGRAD(J,J) - MAXCRIT *COMPOFGRADJ
	
	    ALPHAUPBND = ( -B + SQRT(B**2 - A*C) )/ A
	    ALPHALOWBND = ( -B - SQRT(B**2 - A*C) )/ A

	    DELTAALPHA = MIN (MAX( DELTAALPHA, ALPHALOWBND), ALPHAUPBND)
	    ALPHA = ALPHA + DELTAALPHA
	    ALPHA = MAX( ALPHA, 0.0)

	ENDIF


	RETURN
	END


	



	SUBROUTINE READPSF
	INCLUDE 'mem.inc'

	REAL	VOLUME

	VOLUME = 0.0
	DO LINE = 1, NLPSF
	    CALL XVREAD(UNIT(PSF), IMAGE(1,PSF), STATUS,
     +						 'LINE',LINE,' ')
	    DO SAMP = 1, NSPSF
		PSFIMAGE(SAMP,LINE) = IMAGE(SAMP,PSF)
		VOLUME = VOLUME + PSFIMAGE(SAMP,LINE)
	    ENDDO
	ENDDO

	if (volume.eq.0.0) call mabend(' ** empty PSF file **')

	DO LINE = 1, NLPSF
	    DO SAMP = 1, NSPSF
		PSFIMAGE(SAMP,LINE)= PSFIMAGE(SAMP,LINE)/VOLUME
	    ENDDO
	ENDDO

	RETURN
	END







	SUBROUTINE MAKEXFER
	INCLUDE 'mem.inc'

	REAL	VOLUME
	COMPLEX CBUFFER(2048), TWOPII


	TWOPII = 2*3.1415926*(0,1)

	VOLUME = 0.0
	DO LINE = 1, NLPSF
	    CALL XVREAD(UNIT(PSF), IMAGE(1,PSF), STATUS,
     +					 'LINE',LINE,' ')
	    DO SAMP = 1, NSPSF
		VOLUME = VOLUME + IMAGE(SAMP,PSF)
	    ENDDO
	ENDDO
	VOLUME = VOLUME*NLFFT*NSFFT

	DO LINE = 1, NLPSF
	    CALL XVREAD(UNIT(PSF), IMAGE(1,PSF), STATUS,
     +					 'LINE',LINE,' ')
	    DO SAMP = 1, NSPSF
		IMAGE(SAMP,XFER)= IMAGE(SAMP,PSF)/VOLUME
	    ENDDO
	    DO SAMP = NSPSF+1, NSFFT
		IMAGE(SAMP,XFER) = 0.0
	    ENDDO
	    CALL XVWRIT (UNIT(XFER), IMAGE(1,XFER), STATUS,
     +					 'LINE',LINE,' ')
	ENDDO
	DO SAMP = 1, NSFFT
	    IMAGE(SAMP,XFER) = 0.0
	ENDDO
	DO LINE = NLPSF+1, NLFFT
	    CALL XVWRIT (UNIT(XFER), IMAGE(1,XFER), STATUS,
     +					 'LINE',LINE,' ')
	ENDDO


	CALL FFTVICARR (UNIT(XFER), NLFFT,NSFFT, +1, XFERCOLBUF)


	DO LINE = 1, NLFFT
	    CALL XVREAD(UNIT(XFER), CBUFFER, STATUS,
     +					 'LINE',LINE,' ')
	    DO SAMP = 1, NSFFT/2
		CBUFFER(SAMP) = CBUFFER(SAMP)
     +		 * CEXP(-TWOPII*( FLOAT(LINE-1)*INT(NLPSF/2.)/NLFFT 
     +			       +  FLOAT(SAMP-1)*INT(NSPSF/2.)/NSFFT ) )
	    ENDDO
	    CALL XVWRIT (UNIT(XFER), CBUFFER, STATUS, 'LINE',LINE)
	    XFERCOLBUF(LINE) = XFERCOLBUF(LINE)
     +			 * CEXP(-TWOPII*( FLOAT(LINE-1)*INT(NLPSF/2.)/NLFFT 
     +				       + FLOAT(NSFFT/2)*INT(NSPSF/2.)/NSFFT ) )

	ENDDO

	RETURN
	END






	SUBROUTINE CONVIMAGE (INIM, OUTIM, CONJGFLAG)
	INCLUDE 'mem.inc'

	INTEGER	INIM, OUTIM
	LOGICAL CONJGFLAG

	IF (SMALLPSF) THEN
	    CALL CONVDIRECT (INIM, OUTIM, CONJGFLAG)
	ELSE
	    CALL CONVFFT (INIM, OUTIM, CONJGFLAG)
	ENDIF

	RETURN
	END




	SUBROUTINE CONVDIRECT (INIM, OUTIM, CONJGFLAG)
C		CONVOLVES THE INPUT WITH THE PSF AND STORES THE
C		    RESULT IN OUTPUT.  PERFORMS CONVOLUTION DIRECTLY
C		    USING A CIRCULAR LINE BUFFER FOR THE IMAGE.
C		  CONJGFLAG IS TRUE IF THE PSF SHOULD BE FLIPPED 
C
	INCLUDE 'mem.inc'

	INTEGER	INIM, OUTIM
	LOGICAL CONJGFLAG
	INTEGER	LSTART, LEND, BUFLINE, SSTART, L, S, CONVLINE, SOFF, LOFF
	REAL	SUM

	SSTART = NSPSF/2 + 1
	IF (.NOT. CONJGFLAG .AND. (MOD( NSPSF, 2) .EQ. 0)) SSTART =
     +						 SSTART - 1
	LSTART = 1 - NLPSF/2
	IF (CONJGFLAG .AND. (MOD( NLPSF, 2) .EQ. 0)) LSTART =
     +						 LSTART + 1
	LEND = NL + LSTART - 1

	DO LINE = 2 - NLPSF , NL
	    BUFLINE = MOD( LINE + NLPSF - 2, NLPSF) + 1
	    IF (LINE .GE. LSTART .AND. LINE .LE. LEND) THEN
		CALL XVREAD(UNIT(INIM), CONVBUF(SSTART,BUFLINE),
     +			  STATUS, 'LINE',LINE-LSTART+1,' ')
	    ELSE
	        DO SAMP = 1, NS+NSPSF+2
		    CONVBUF(SAMP,BUFLINE) = 0.0
	        ENDDO
	    ENDIF

	    IF (LINE .GE. 1) THEN
		DO SAMP = 1, NS
		    SUM = 0.0
		    IF (CONJGFLAG) THEN
			SOFF = SAMP - 1
			LOFF = LINE - 2
			DO L = 1, NLPSF
			    CONVLINE = MOD(LOFF+L, NLPSF) + 1
			    DO S = 1, NSPSF
				SUM = SUM + PSFIMAGE(S,L)
     +				       *CONVBUF(SOFF+S,CONVLINE)
			    ENDDO
			ENDDO
		    ELSE
			SOFF = NSPSF + SAMP
			LOFF = NLPSF + LINE - 1
			DO L = 1, NLPSF
			    CONVLINE = MOD( LOFF-L, NLPSF) + 1
			    DO S = 1, NSPSF
				SUM = SUM + PSFIMAGE(S,L)
     +				       *CONVBUF(SOFF-S,CONVLINE)
			    ENDDO
			ENDDO
		    ENDIF
		    IMAGE(SAMP,OUTIM) = SUM
		ENDDO
		CALL XVWRIT (UNIT(OUTIM), IMAGE(1,OUTIM), STATUS, 
     +					'LINE',LINE,' ')
	    ENDIF
	ENDDO

	RETURN
	END




	SUBROUTINE CONVFFT (INIM, OUTIM, CONJGFLAG)
C		CONVOLVES THE INPUT WITH THE PSF AND STORES THE
C		    RESULT IN OUTPUT.  PERFORMS CONVOLUTION USING FFT'S.
C		  CONJGFLAG IS TRUE IF THE PSF SHOULD BE FLIPPED 
C
	INCLUDE 'mem.inc'

	INTEGER	INIM, OUTIM
	LOGICAL CONJGFLAG
	COMPLEX	FCOLBUF(4096), FBUFFER(2048), XFERBUFFER(2048)


C		COPY THE INPUT IMAGE INTO THE FFT SCRATCH FILE 
C		    AND PAD WITH ZEROS
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(INIM), IMAGE(1,INIM), STATUS,
     +						 'LINE',LINE,' ')
	    DO SAMP = 1, NS
		IMAGE(SAMP,TMPFFT) = IMAGE(SAMP,INIM)
	    ENDDO
	    DO SAMP = NS+1, NSFFT
		IMAGE(SAMP,TMPFFT) = 0.0
	    ENDDO
	    CALL XVWRIT(UNIT(TMPFFT),IMAGE(1,TMPFFT),STATUS,
     +						'LINE',LINE,' ')
	ENDDO
	DO LINE = NL+1, NLFFT
	    DO SAMP = 1, NSFFT
		IMAGE(SAMP,TMPFFT) = 0.0
	    ENDDO
	    CALL XVWRIT(UNIT(TMPFFT),IMAGE(1,TMPFFT),STATUS,
     +						'LINE',LINE,' ')
	ENDDO

C		FFT THE INPUT IMAGE
	CALL FFTVICARR (UNIT(TMPFFT), NLFFT,NSFFT, +1, FCOLBUF)

C		COMPLEX MULTIPLY THE TRANSFORM OF THE INPUT IMAGE 
C		    BY THE TRANSFER IMAGE
	DO LINE = 1, NLFFT
	    CALL XVREAD(UNIT(TMPFFT), FBUFFER, STATUS,
     +						 'LINE',LINE,' ')
	    CALL XVREAD(UNIT(XFER), XFERBUFFER, STATUS,
     +						 'LINE',LINE,' ')
	    DO SAMP = 1, NSFFT/2
		IF (CONJGFLAG)  XFERBUFFER(SAMP) =
     +					 CONJG(XFERBUFFER(SAMP))
		FBUFFER(SAMP) = FBUFFER(SAMP) * XFERBUFFER(SAMP)
	    ENDDO
	    CALL XVWRIT(UNIT(TMPFFT), FBUFFER, STATUS,
     +						 'LINE',LINE,' ')
	    IF (CONJGFLAG)  XFERCOLBUF(LINE) =
     +					 CONJG(XFERCOLBUF(LINE))
	    FCOLBUF(LINE) = FCOLBUF(LINE)* XFERCOLBUF(LINE)
	ENDDO

C		FFT BACK TO GET THE CONVOLVED IMAGE
	CALL FFTVICARR (UNIT(TMPFFT), NLFFT,NSFFT, -1, FCOLBUF)


C		COPY INTO THE OUTPUT FILE
	DO LINE = 1, NL
	    CALL XVREAD(UNIT(TMPFFT),IMAGE(1,TMPFFT),STATUS,
     +						'LINE',LINE,' ')
	    CALL XVWRIT(UNIT(OUTIM), IMAGE(1,TMPFFT), STATUS,
     +						 'LINE',LINE,' ')
	ENDDO


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
	INTEGER*2 BITREV(8192)
	COMPLEX BUFFER(512*256), PHASE(8192)

	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE


	MAXN = MAX(NLFFT,NSFFT)
	CALL MAKEBITREV (BITREV, MAXN)
	CALL MAKEPHASE (PHASE, FFTSIGN, MAXN)


	IF (NLFFT*NSFFT .GT. 512*512) THEN 

	IF (FFTSIGN .EQ. +1) THEN
	    DO LINE = 1, NLFFT
		CALL XVREAD(UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
		CALL FFTC1D (BUFFER, NSFFT/2)
		CALL FIXREAL (BUFFER, COLBUF(LINE), NSFFT/2, +1)
		CALL XVWRIT(UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	    ENDDO
	    CALL FFTC1D (COLBUF, NLFFT)
	    CALL FFTC1VD (UNIT, NSFFT/2, NLFFT)
	ELSE
	    CALL FFTC1VD (UNIT, NSFFT/2, NLFFT)
	    CALL FFTC1D (COLBUF, NLFFT)
	    DO LINE = 1, NLFFT
		CALL XVREAD(UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
		CALL FIXREAL (BUFFER, COLBUF(LINE), NSFFT/2, -1)
		CALL FFTC1D (BUFFER, NSFFT/2)
		CALL XVWRIT(UNIT, BUFFER, STATUS, 'LINE',LINE,' ')
	    ENDDO
	ENDIF

	ELSE

	IF (FFTSIGN .EQ. +1) THEN
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVREAD(UNIT, BUFFER(BUFPTR), STATUS,
     +						 'LINE',LINE,' ')
		CALL FFTC1D (BUFFER(BUFPTR), NSFFT/2)
		CALL FIXREAL (BUFFER(BUFPTR), COLBUF(LINE), NSFFT/2,+1)
	    ENDDO
	    CALL FFTCV (BUFFER, NSFFT/2, NLFFT)
	    CALL FFTC1D (COLBUF, NLFFT)
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVWRIT(UNIT, BUFFER(BUFPTR), STATUS,
     +						 'LINE',LINE,' ')
	    ENDDO
	ELSE
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL XVREAD(UNIT, BUFFER(BUFPTR), STATUS,
     +						 'LINE',LINE,' ')
	    ENDDO
	    CALL FFTC1D (COLBUF, NLFFT)
	    CALL FFTCV (BUFFER, NSFFT/2, NLFFT)
	    DO LINE = 1, NLFFT
		BUFPTR = (NSFFT/2)*(LINE-1)+1
		CALL FIXREAL (BUFFER(BUFPTR), COLBUF(LINE), NSFFT/2, -1)
		CALL FFTC1D (BUFFER(BUFPTR), NSFFT/2)
		CALL XVWRIT(UNIT, BUFFER(BUFPTR), STATUS,
     +						 'LINE',LINE,' ')
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
	INTEGER*2 BITREV(1)
	COMPLEX	DATA(1),TMP,PHASE(1)
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
	COMPLEX DATA(1),COLBUF, PHASE(1), TMP0,TMP1, PH
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
	INTEGER*2 BITREV(1)
	COMPLEX	PHASE(1)
	INTEGER	POWER,STAT, LOG2, UNIT
	COMPLEX	DATA0(4096),DATA1(4096),TMP0,TMP1,PH
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
	INTEGER*2 BITREV(1)
	COMPLEX	DATA(1),TMP,PHASE(1),PH
	COMMON	/BITREV/ BITREV
	COMMON  /PHASE/  PHASE

	LNL = LOG2(NL)
	DO I = 0,NL-1
	    J = BITREV(I+NL)
	    IF (J .GT. I) THEN
		M0 = NS*I
		M 1= NS*J
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
$!-----------------------------------------------------------------------------
$ create mem.inc
$ DECK/DOLLARS="$ VOKAGLEVE"
C	MEM INCLUDE FILE
	IMPLICIT NONE
	INTEGER	UNIT(11)
	INTEGER DATA, PSF, DEFAULT, OUT, XFER, TMPFFT
	INTEGER	DEF, CURRENT, GRDE, CNVSTP, RESID, STEP
	INTEGER	SL,SS, NL,NS, NLFFT,NSFFT, NLPSF,NSPSF
	REAL	IMAGE(4096,11)
	REAL	PSFIMAGE(16,16), CONVBUF(4096+18,16)
	COMPLEX	XFERCOLBUF(4096)
	INTEGER	LINE, SAMP, STATUS, COUNT
	REAL	ALPHA, ERROR, NPIXELS, RMSERROR, TARGETERROR, TARGETRMS
	REAL	TOL, SOLUCRIT, MAXCRIT, COMPOFGRADJ, GRADDOTGRAD(3,3)
	REAL	IMAGEMIN, MAXSTEPLEN, STEPLEN, PSFVOLUME
	REAL	E0, E1, E2
	INTEGER	MAXITER, ITER
	LOGICAL	DEFFLAG, RESTART, SMALLPSF
	INTEGER	H, E, J

	PARAMETER (H=1, E=2, J=3)

	COMMON	/MEM/
     +		UNIT, IMAGE,  SL,SS, NL,NS, NLFFT,NSFFT, NLPSF,NSPSF,
     +		DATA, PSF, DEFAULT, OUT, XFER, TMPFFT, DEF,
     +		CURRENT, GRDE, CNVSTP, RESID, STEP,
     +		PSFIMAGE, CONVBUF,
     +		XFERCOLBUF,
     +		ALPHA,   E0, E1, E2,
     +		ERROR, NPIXELS, RMSERROR, TARGETERROR, TARGETRMS,
     +		TOL, SOLUCRIT, MAXCRIT, COMPOFGRADJ, GRADDOTGRAD,
     +		IMAGEMIN, MAXSTEPLEN, STEPLEN,  PSFVOLUME,
     +		DEFFLAG, RESTART, SMALLPSF, ITER, MAXITER
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mem.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mem

   To Create the build file give the command:

		$ vimake mem			(VMS)
   or
		% vimake mem			(Unix)


************************************************************************/


#define PROGRAM	mem
#define R2LIB

#define MODULE_LIST mem.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define DEBUG		/* remove on delivery */

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mem.pdf
process help=*
PARM INP TYPE=(STRING,32) COUNT=2:3
PARM OUT TYPE=(STRING,32) COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM RMSERROR TYPE=REAL
PARM MAXITER TYPE=INTEGER DEFAULT=20  VALID=(0:100)
PARM MAXCRIT TYPE=REAL DEFAULT=0.3  VALID=(0.01:0.80)
PARM RESTART TYPE=KEYWORD COUNT=0:1 VALID=(RESTART) DEFAULT=--
END-PROC
.TITLE
VICAR2 Program MEM -- Maximum Entropy Method deconvolution
.HELP

PURPOSE

MEM performs a non-linear restoration (deconvolution) using the Maximum 
Entropy Method.  Deconvolution is the process of undoing convolution:  
given a degraded image that results from an image being convolved with a 
known point spread function (PSF) and having noise added, find the original 
image.  There are, in general, many solutions to the deconvolution problem;  
of these MEM selects the solution having the greatest entropy:

	H = Sum over all pixels ( -b*log(b/m) )

where b is the pixel brightness and m is the default image.



EXECUTION

Normally use:
MEM  (DEGRADED.IMG,PSF.IMG)  MEM.IMG  RMSERROR=? 

Using more parameters:
MEM  (DEGRADED.IMG,PSF.IMG,DEFAULT.IMG)  MEM.IMG  RMSERROR=?  +
				MAXITER=?  MAXCRIT=?


The images can be in any non-complex format;  the output image has
the same format as the degraded image.  Be sure that the maximum in the
degraded image is quite a bit less than the maximum pixel value, so
that the output image won't have to be clipped.  The degraded image should
be nearly all positive (because the output maximum entropy image is totally
positive), and the background level should be close to  zero.  A sub-image 
of the degraded image can be used by specifying the SIZE or (SL,SS,NL,NS) 
parameters.  The image size must be less than or equal to 4096 in both 
dimensions.


The point spread function (PSF) is the image that the degraded image 
has been convolved with.  In some cases it will be known exactly; in
others it must be estimated.  If the degraded image contains point sources
(stars) then one of these can be used for the PSF.  Use COPY to cutout the 
point source.  A lot of noise on the point source image will cause problems 
since it will not then be the true PSF.  In this case a model image can be 
made using SPOT or F2 to approximate the PSF.  If there are no point sources 
then an edge can sometimes be used to estimate the PSF.  If the degradation 
is known (or hypothesized) to be a linear smear at constant velocity then 
the PSF is a line;  look at the FFT of the degraded image in order figure out
the length and direction of the line.  MEM normalizes the volume of the PSF 
to unity so its height is unimportant.  The center of the PSF should be near 
the center of the PSF image.

Usually the default image can be left out.  In some cases better results
may be obtained by using a default image that represent apriori information
about what the image should look like.  For example, the maximum entropy
method may work better on planets if an image containing a disk of the 
correct size and position is used as the default.


The RMSERROR parameter gives the amount of noise in the data.  This can be 
estimated from the standard deviation of a blank part of the image (use
standard deviation from HIST).  Specifying too large a number will lead to 
an overly smooth image.  Too small a number will prevent convergence.  If 
the PSF is inexact or the problem too difficult the desired RMS error maybe
unobtainable.  In these cases use a higher value for RMS error.  If you can't
estimate RMSERROR then run MEM with some low value, and then use the RMS error 
that is obtained when MEM starts to slow down.

The MAXITER parameter is the maximum number of iterations the program will do.
If the solution is reached sooner MEM will stop before doing that number of
iterations.  Typically around 10 iterations are required.  More difficult 
problems and higher signal-to-noise cases will take more iterations.  Easy
problems can often be done in about 5 iterations.  There is little to be 
gained from doing more than about 30 to 40 iterations.

The MAXCRIT parameter specifies the maximum allowed solution criterion.  The
solution criterion is a measure of how close to the maximum entropy solution
the image is for the particular RMS error it is currently at.  Values near
zero indicate that it is at a solution; values near one indicate problems.
The default value for MAXCRIT can nearly always be used.

The MEM program can be restarted using a previous image by specifying the
'RESTART keyword.  The previous (and output) image is specified in the
output image slot.  The parameters and/or images can be changed when restarting
so that adjustments can be made along the way.


OPERATION

The MEM program starts with the default image and changes the image each
iteration to better fit the data.  If no image is specified the default 
image is a constant equal to the average pixel value in the input image.
The algorithm maximizes the entropy with the constraint of the data by
using the method of Lagrange multipliers.  The algorithm uses a gradient 
search method where the gradient direction is modified by an approximation
to the second derivative.  At each iteration the image should be at the 
maximum entropy solution for the particular RMS error that is has.
The program stops (converges) if the RMS error is within ten percent of
the desired error and the solution criterion is less than ten percent
greater than the MAXCRIT parameter.  This algorithm is a modification
of the one described in the first reference (Cornwell and Evans).

MEM performs convolutions using either FFT's or directly, depending on the
size of the PSF.  If the PSF is larger than 16 in either direction then the
FFT's are used.  The size of the FFT is the power of two that is larger than 
the input image size plus the size of the PSF image.  Thus it saves CPU time 
if this slightly less than a power of two rather than slightly more.  If the
convolutions are being performed directly then the PSF should be as small as
possible in order to save CPU time.  MEM creates a bunch of scratch file 
images of real format with the file name MEMn , where n is a digit from 1 to 7.
If the FFT's are being used seven scratch files are created, otherwise five 
are used.  The two FFT scratch files are the size of the FFT, and other five 
are the size of the output image.  If there is a crash or if you stop the 
program, the latest MEM image is in the scratch file MEM7 .  Most of the CPU
time is spent in the two convolutions that are performed for each iteration.  


THEORY

The entropy measure has its background in statistical and information
theoretical arguments, but these are not widely accepted.  Pragmatically,
the entropy can be thought of as a non-linear measure of distance from
the default image, which measures the lack of dispersion in pixel values.
MEM finds the image that has the maximum entropy while still fitting the
data to within a specified RMS error.  The RMS error is the root mean
square of the difference between the degraded input image and the MEM image
convolved with the PSF.  This formulation assumes that the noise has 
a gaussian distribution and is added in the image domain after the convolution.

The purpose of maximizing entropy is to try to recover some of information 
lost in the degradation process.  The information the entropy puts in
is not necessarily correct for all applications.  The method tends to
produce images with enhanced gaussian peaks on a flat zero background level.
Maximum entropy works better if the image fits this picture, and it can do
poorly if the image is a lot different.  For example, it will put oscillations 
in high flat plateaus.  Another important property is that the resolution 
varies depending on the signal to noise ratio, with greater resolution on 
brighter features.  Maximum entropy forces the image to be positive and 
slightly biased.  These properties are a result of maximizing the entropy 
with the constraint of the data, and are not caused by the particular 
algorithm that performs the maximization.  For more information on the 
properties of maximum entropy images and the technique in general see 
the second reference.

The maximum entropy method restoration can be used for resolution enhancement
and for deblurring.  The resolution cannot be increased very much, however,
without introducing artifacts, because some information is permanently lost.
The method performs better on deblurring problems.  Often maximum entropy
will perform much better than linear methods (such as RESTORW), but sometimes
the linear methods can do better.  In general, maximum entropy will excel
(compared to linear methods) in cases of low signal-to-noise.



REFERENCES

    Cornwell, T. J. and Evans, K. F., 1985.  "A New Simple Maximum Entropy
Deconvolution Algorithm",  Astronomy and Astrophysics, 143:77-83.

    Narayan, R. and Nityananda, R., 1986.  "Maximum Entropy Image Restoration
in Astronomy",  Annual Reviews of Astronomy and Astrophysics.





Cognizant Programmer:  L.W.Kamp

Original Programmer:  K. F. Evans	January 1986



.LEVEL1
.VARIABLE INP
1. The degraded image
2. The point spread (PSF) image
3. The default image
.VARIABLE OUT
The output MEM image
.VARIABLE SIZE
The standard Vicar size field
.VARIABLE SL
The starting line
.VARIABLE SS
The starting sample
.VARIABLE NL
The number of lines
.VARIABLE NS
The number of samples
.VARIABLE RMSERROR
The RMS error to fit to -
the noise level in the data
.VARIABLE MAXITER
The maximum number of iterations
.VARIABLE MAXCRIT
The maximum allowed solution
criterion.  Use default.
.VARIABLE RESTART
Use if restarting MEM

.LEVEL2
.VARIABLE INP
Either two or three input images are required.  The first one is the
degraded image that need restoring (deconvolution).  The second one
is the point spread function (PSF) that the original image was convolved
with.  The third image is the optional default image, which can be
a guess as to what the image should look like.  Most of the time the
default image can be left out.  See the help file for more information
about the degraded image and the PSF image.
.VARIABLE OUT
The output maximum entropy image.  Has the same format as the input image.
.VARIABLE SIZE
The standard Vicar size field.  Use to select the window out of the degraded
image to restore.  The default is to restore the whole image.
.VARIABLE SL
The starting line of the window.
.VARIABLE SS
The starting sample of the window.
.VARIABLE NL
The number of lines in the window.
.VARIABLE NS
The number of samples in the window.
.VARIABLE RMSERROR
The RMSERROR parameter gives the amount of noise in the data.  This can be 
estimated from the standard deviation of a blank part of the image (use
standard deviation from HIST).  Specifying too large a number will lead to 
an overly smooth image.  Too small a number will prevent convergence.  If 
you don't want to estimate RMSERROR then run MEM with some low value, and
then use the RMS error that is obtained when MEM starts to slow down.
.VARIABLE MAXITER
The MAXITER parameter is the maximum number of iterations the program will do.
If the solution is reached sooner MEM will stop before doing that number of
iterations.  Typically 10 to 15 iterations are required.  More difficult 
problems and higher signal-to-noise cases will take more iterations.  There 
little to be gained from doing more than 40 or 50 iterations.
.VARIABLE MAXCRIT
The MAXCRIT parameter specifies the maximum allowed solution criterion.  The
solution criterion is a measure of how close to the maximum entropy solution
the image is for the particular RMS error it is currently at.  Values near
zero indicate that it is at a solution; values near one indicate problems.
The default value for MAXCRIT can nearly always be used.
.VARIABLE RESTART
The MEM program can be restarted using a previous image by specifying the
'RESTART keyword.  The previous (and output) image is specified in the
output image slot.  The parameters and/or images can be changed when restarting
so that adjustments can be made along the way.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmem.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! generate a psf
gen out=x.img nl=64 ns=64 linc=0 sinc=0
qsar inp=x.img out=y.img area=(2,2,1,7,50,2,8,3,1,50)
fft22 y.img ffty.img
!
! generate an image
qsar inp=x.img out=z.img area=(32,32,5,5,200)
fft22 z.img fftz.img
!
! blur the image & add noise
wiener inp=(fftz.img,ffty.img) out=ffts.img 'direct
fft22 inp=ffts.img out=blur.img 'inverse format=half
gausnois out=nois.img nl=64 ns=64 mean=0 sigma=20 format=half
f2 inp=(blur.img,nois.img) out=bn.img function="in1+in2"
fft22 bn.img fftbn.img
!
! restore the image
wiener inp=(fftbn.img,ffty.img) out=fftss.img 'wiener sn=6
fft22 inp=fftss.img out=restored.img 'inverse format=half
!xvd restored.img
!
copy y.img psf.img nl=11 ns=11
mem inp=(bn.img,psf.img) out=rest.img rmserror=20 maxcrit=.1
!xvd rest.img
!
end-proc
$ Return
$!#############################################################################
