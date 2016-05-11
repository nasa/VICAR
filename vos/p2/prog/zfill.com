$!****************************************************************************
$!
$! Build proc for MIPL module zfill
$! VPACK Version 2.1, Friday, February 12, 2016, 13:04:56
$!
$! Execute by entering:		$ @zfill
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
$ write sys$output "*** module zfill ***"
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
$ write sys$output "Invalid argument given to zfill.com file -- ", primary
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
$   if F$SEARCH("zfill.imake") .nes. ""
$   then
$      vimake zfill
$      purge zfill.bld
$   else
$      if F$SEARCH("zfill.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zfill
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zfill.bld "STD"
$   else
$      @zfill.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zfill.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zfill.com -mixed -
	-s zfill.f -
	-i zfill.imake -
	-p zfill.pdf -
	-t tstzfill.pdf tstzfill.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zfill.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     VICAR PROGRAM "zfill"      S.Z. FRIEDMAN    MARCH 1983
C     MODIFIED FOR VAX CONVERSION BY ASM, 6 SEPT 1983
C     MODIFIED TO VICAR2 AND SPARSE/DENSE ALGORITHMS ADDED BY REA, 20 MAY 1986
C     MODIFIED TO BE ABLE TO BE ALSO RUN ON A BIL FILE WITH ANY NUMBER OF
C     BANDS, 6 JULY 1987
C     3-94   CRI   MSTP (S/W CONVERSION) VICAR PORTING
C     1/24/2001 AXC ADDED SUPPORT TO PRESERVE BINARY LABELS.
C**********************************************************************
      SUBROUTINE MAIN44
C
C  PROGRAM "zfill" USES A WINDOW OF NLW LINES BY NSW SAMPLES TO
C  FILL IN VOID AREAS OF AN IMAGE.  VOID AREAS ARE ASSUMED
C  TO BE ZERO DN, BUT CAN BE RESPECIFIED BY PARAMETER.  VOIDS ARE
C  FILLED WITH THE MEAN VALUE OF ALL 'NON-VOID DN' PIXELS IN
C  THE WINDOW.  A MASK SHOWING FILLED PIXELS CAN BE
C  GENERATED UPON REQUEST
C
C
C  TAE STATEMENT FORM:
C     zfill IN OUT SIZE=(SL,SS,NL,NS) PARAMETERS       OR
C     zfill IN (OUT,MASK) SIZE=(SL,SS,NL,NS) PARAMETERS
C
C  PARAMETERS:
C
C     NLW=n            NUMBER OF LINES IN WINDOW (DEFAULT=3)
C
C     NSW=m            NUMBER OF SAMPLES IN WINDOW (DEFAULT=3)
C
C     REPLACE=n        DEFINES DN(N) TO BE THE 'VOID' DN.  ALL PIXELS
C                      OF VALUE N WILL BE REPLACED BY THE MEAN OF ALL
C                      'NON-N' VALUES IN THE WINDOW.  (DEFAULT=0)
C
C     EXCLUDE=n        DN N WILL NOT BE USED IN THE INTERPOLATION
C                      OF VALUE FOR REPLACEMENT DN.  ITS FUNCTION
C                      IS SIMILAR TO AN AREA MASK OR BARRIER.
C                      (NO DEFAULT)
C
C     DENSE	       INVOKES THE ALGORITHM FOR USE WHEN THE INPUT
C		       IS MOSTLY FULL.
C
	EXTERNAL MAIN
	LOGICAL XVPTST
	CHARACTER*4 FORMAT
	CHARACTER*3 ORG
        INTEGER     BUFFER_SIZE
        PARAMETER   (BUFFER_SIZE=200000)
        COMMON /C2/ BUF
        INTEGER*2   BUF(BUFFER_SIZE)
	COMMON /C1/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK,NB,QBINARY,
     +              NLBI,NSBI 
	LOGICAL QDENSE/.FALSE./,QMASK/.FALSE./,QBINARY
C							 call for parameters
        CALL IFMESSAGE('ZFILL version 2016-02-12')
        CALL XVEACTION('SA',' ')
	CALL XVPARM('NLW',NLW,ICNT,IDEF,1)
	CALL XVPARM('NSW',NSW,ICNT,IDEF,1)
	CALL XVPARM('REPLACE',IREPLACE,ICNT,IDEF,1)
	CALL XVPARM('EXCLUDE',IEXCLUDE,ICNT,IDEF,1)
	QDENSE = XVPTST('DENSE')
        QBINARY = XVPTST('BINARY')
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT, ' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')

C                                                     call for binary label
        IF (QBINARY) THEN 
            CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF','OPEN_ACT',
     +                  ' ','IO_ACT','SA','COND','BINARY', ' ')
        ELSE 
            CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF', ' ')
        ENDIF

C end added statement

	CALL XVGET(INUNIT,ISTAT,'NLB',NLBI,'NBB',NSBI,
     +             'NB',NB,'ORG',ORG,'FORMAT',FORMAT,' ')
	IF (NB.GT.1 .AND. ORG.NE.'BIL') THEN
	   CALL XVMESSAGE('ONLY ONE BAND IS ALLOWED FOR A BSQ FILE',' ')
	   CALL ABEND
	ENDIF
        IF (ORG.EQ.'BIP') THEN
           CALL XVMESSAGE('CANNOT BE RUN ON A BIP FILE',' ')
           CALL ABEND
        ENDIF
	IF (FORMAT.NE.'BYTE' .AND. FORMAT.NE.'HALF') THEN
	   CALL XVMESSAGE('MUST BE BYTE OR HALFWORD',' ')
	   CALL ABEND
	ENDIF

	CALL XVPCNT('OUT',NOUT)

	IF (NOUT.EQ.2) THEN
	    IF (NB.GT.1) THEN
	       CALL XVMESSAGE('MASK CAN ONLY BE CREATED FOR A BSQ FILE',
     +                        ' ')
	       CALL XVMESSAGE('NO MASK WILL BE CREATED',' ')
	    ELSE
	       QMASK = .TRUE.
	       CALL XVUNIT(MASKUNIT,'OUT',2,ISTAT,' ')
	       CALL XVOPEN(MASKUNIT,ISTAT,'U_FORMAT','BYTE','O_FORMAT',
     +                'BYTE','OP','WRITE',' ')
	    ENDIF
	END IF

C					     check parameters, adjust if needed
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (ISL+NL-1 .GT. NLIN) NL=NLIN-ISL+1
	IF (ISS+NS-1 .GT. NSIN) NS=NSIN-ISS+1

	CALL 
     +   XVMESSAGE('INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE', 
     +   ' ')
        IF (NL.LE.0 .OR. NS.LE.0) THEN
	    CALL XVMESSAGE('SIZE FIELD ERROR',' ')
	    CALL ABEND
	END IF

        IF ( (( ISL .NE. 1 ) .OR. ( ISS .NE. 1 ) .OR. 
     *       ( NL .NE. NLIN) .OR. ( NS .NE. NSIN))
     *       .AND. QBINARY ) THEN
         NLBI = 0
         NSBI = 0
         QBINARY = .FALSE.
         CALL XVMESSAGE('Output is a sub-window of input, so', ' ')
         CALL XVMESSAGE('binary parts can not be copied.', ' ')
         CALL XVMESSAGE('Continuing with copy of non-binary parts.',' ')
         ENDIF

C                         copy binary label to the output image if requested
        IF (QBINARY) THEN
            CALL XVOPEN(IOUTUNIT,ISTAT,'U_FORMAT','HALF','OP','WRITE',
     +		        'U_NL',NL,'U_NS',NS,'U_NB',NB,'U_ORG',ORG,
     +		        'OPEN_ACT','SA','IO_ACT','SA','COND',
     +                  'BINARY','U_NBB',NSBI,'U_NLB',NLBI, ' ')
        ELSE 
	    CALL XVOPEN(IOUTUNIT,ISTAT,'U_FORMAT','HALF','OP','WRITE',
     +          ' ')
        ENDIF

C Copy binary labels if requested for BSQ image format    

        IF (QBINARY .AND. NLBI .GT.0 .AND. ORG .EQ. 'BSQ') THEN
C                                             read in binary header
            DO REC = 1,NLBI
               CALL XVREAD(INUNIT,BUF,ISTAT, ' ')
               CALL XVWRIT(IOUTUNIT,BUF,ISTAT,' ')
            ENDDO 
        ENDIF

        NLW = 2*(NLW/2) + 1
	NSW = 2*(NSW/2) + 1
C		           determine buffer sizes and call main through stacka
	N1 = NS+NSW-1
	N2 = NLW*NB
	INSIZE = 2*N1*N2
	IOUTSIZE = 2*NS
	IF (QMASK) THEN
	    MSIZE = NS
	ELSE
	    MSIZE = 4
	END IF
	CALL STACKA(8,MAIN,3,MSIZE,INSIZE,IOUTSIZE,N1,N2)
	 	

        CALL XVCLOSE(INUNIT,ISTAT, ' ')
        CALL XVCLOSE(IOUTUNIT,ISTAT, ' ')
        RETURN
        END
C
C**********************************************************************
C
	SUBROUTINE MAIN(MBUF,MSIZE,INBUF,INSIZE,OUTBUF,IOUTSIZE,N1,N2)
C
C  VICAR PROGRAM ZFILL      S.Z. FRIEDMAN    MARCH 1983
C
C  SUBROUTINE MAIN HANDLES ALL I/O, CALLS MEAN ANALYSIS ROUTINE
C
C
C     MBUF    OPTIONAL ARRAY FOR MASK OUTPUT DS FOR LINE N
C     INBUF   INPUT BUFFER
C     OUTBUF  OUTPUT ARRAY FOR LINE N
C
	INTEGER*2 INBUF(N1,N2), OUTBUF(NS)
	INTEGER*4 BN, ISUM(2000), NPTS(2000)
        CHARACTER*80 LBUF
	BYTE      MBUF(MSIZE)
        INTEGER   BUFFER_SIZE
        PARAMETER (BUFFER_SIZE=200000)
        COMMON /C2/ BUF
        INTEGER*2   BUF(BUFFER_SIZE)
	COMMON /C1/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK,NB,QBINARY,
     +              NLBI,NSBI
	LOGICAL   QDENSE,QMASK,QBINARY
C
C						  check if core is available
C
	IF (IOUTSIZE .LT. 2*NS) THEN
	    CALL XVMESSAGE('INSUFFICIENT MEMORY FOR STACKA OPERATION',
     +                     ' ')
	    CALL ABEND
	END IF
C
C							label processing
C
        WRITE(LBUF,1000) NLW, NSW, IREPLACE
1000    FORMAT('ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5)
	IF (IEXCLUDE.NE.-32768) THEN
            WRITE(LBUF,1010) NLW, NSW, IREPLACE, IEXCLUDE
1010        FORMAT('ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5,
     +              ', EXCLUDE =',I5)
	    LENGTH = 59
	ELSE
	    LENGTH = 43
	END IF
	CALL XLADD(IOUTUNIT,'HISTORY','PARMS',LBUF,ISTAT,'FORMAT',
     +		   'STRING','ULEN',LENGTH,' ')
        CALL XVMESSAGE(LBUF,' ')
C	CALL QPRINT(LBUF,LENGTH)
C
	IF(QMASK) THEN
	    CALL XLADD(MASKUNIT,'HISTORY','PARMS',LBUF,ISTAT,'FORMAT',
     +		       'STRING','ULEN',LENGTH,' ')
	    CALL XLADD(MASKUNIT,'HISTORY','MASK',
     +		       ' 0=CHANGED   255=NO CHANGE',ISTAT,'FORMAT',
     +		       'STRING',' ')
	END IF

C
C					Read in initial lines; set pointers
C
	NLW2 = NLW/2
	NSW2 = NSW/2
	LINE1 = MAX(1,ISL-NLW2)				! First line to be read
	LINEN = MIN(NLIN,ISL+NLW2)			! Last line to be read
	ISAMP1 = MAX(1,ISS-NSW2)			! First sample read
	ISAMPN = MIN(NSIN,ISS+NS-1+NSW2)		! Last sample read
	NSAMPS = ISAMPN-ISAMP1+1			! Number of samps read
	LOCL = MAX(1,NLW2-ISL+2)		! Loc of 1st line in INBUF
	LOCS = MAX(1,NSW2-ISS+2)		! Loc of 1st samp in INBUF
C
C					Initialize all values in INBUF to the
C					excluded value
	DO I=1,N2
	    DO J=1,N1
		INBUF(J,I) = IEXCLUDE
	    END DO
	END DO
C						Read input to fill INBUF
C
C   THIS LOOP IS CORRECT ONLY IF NB=1 WHEN ORG IS BSQ
C
        DO I=LINE1,LINEN
	   DO BN = 1,NB
	      NLWBN = NLW*(BN-1)
              IF (QBINARY) THEN 
                  CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),ISTAT,
     +                        'LINE',NLBI+I,'SAMP',NSBI+ISAMP1,
     +                        'NSAMPS',NSAMPS,'BAND',BN,' ')
              ELSE
                  CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),ISTAT,
     +                        'LINE',I,'SAMP',ISAMP1,'NSAMPS', 
     +                        NSAMPS,'BAND',BN,' ')
              ENDIF
           ENDDO
	   LOCL = LOCL+1
	ENDDO
	LINELOC = NLW2
	LINEN = LINEN+1
	LOCL = 1
	IF (QDENSE) THEN
C
C **********************************************************    DENSE ALGORITHM
C
	    DO I=1,NL
		IF (QMASK) CALL ITLA(255,MBUF,NS)
		LINELOC = LINELOC+1
		IF (LINELOC.GT.NLW) LINELOC=1
C							      check each sample
		DO BN = 1,NB
	    	   ISAMPLOC = NSW2
		   NLWBN = NLW*(BN-1)
	    	   DO J=1,NS
		      ISAMPLOC = ISAMPLOC+1
		      IF (INBUF(ISAMPLOC,NLWBN+LINELOC).EQ.IREPLACE) THEN
C
C						      compute replacement value
		         ISUM(BN) = 0
		         NPTS(BN) = 0
		         DO K=1,NLW
			    DO L=ISAMPLOC-NSW2,ISAMPLOC+NSW2
			       IF(INBUF(L,NLWBN+K).NE.IREPLACE .AND.
     +			       	  INBUF(L,NLWBN+K).NE.IEXCLUDE)      THEN
				  ISUM(BN) = ISUM(BN)+INBUF(L,NLWBN+K)
				  NPTS(BN) = NPTS(BN)+1
			       END IF
			    END DO
		    	 END DO
		    	 IF (NPTS(BN).NE.0) THEN
			    OUTBUF(J) = FLOAT(ISUM(BN))/FLOAT(NPTS(BN))+0.5
			    IF (QMASK) CALL ITLA(0,MBUF(J),1)
		    	 ELSE					! nothing in
			    OUTBUF(J) = IREPLACE		! window, leave
		    	 END IF					! as is
		      ELSE
		    	 OUTBUF(J) = INBUF(ISAMPLOC,NLWBN+LINELOC) ! replacement
		      END IF					   ! not needed
	    	   END DO
C							     write out results
                   IF (QBINARY) THEN
C                                                      read in binary prefix
                       CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',NLBI+I,
     +                             'SAMP',1,'NSAMPS',NSBI,'BAND',BN,' ')

C                                                      write out binary prefix 
                       CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'LINE',NLBI+I,
     +                             'NSAMPS',NSBI+NS,'BAND',BN,' ')

C                                                      write out pixel data
	               CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,'LINE',NLBI+I,  
     +                             'SAMP',NSBI+1,'NSAMPS',NSAMPS,
     +                             'BAND',BN,' ')
                   ELSE
	    	       CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')     !write out line
                   ENDIF
	    	   IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')
C								! read new line
	    	   IF (LINEN.LE.NLIN) THEN
                       IF (QBINARY) THEN 
                           CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                 ISTAT,'LINE',NLBI+LINEN, 
     +                                 'SAMP',NSBI+ISAMP1,'NSAMPS', 
     +                                 NSAMPS,'BAND',BN,' ')
                        ELSE
         		   CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),
     +                                 ISTAT,'LINE',LINEN,'SAMP',
     +                                 ISAMP1,'NSAMPS',NSAMPS,
     +                                 'BAND',BN,' ')
                       ENDIF
 	            ELSE
		      DO J=1,N1
		    	INBUF(J,NLWBN+LOCL) = IEXCLUDE
		      END DO
	    	    END IF
		ENDDO
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO
	ELSE
C
C*********************************************************** SPARSE ALGORITHM
C
	    DO II=1,NL
C						    initialize ISUM and NPTS
  	       DO BN = 1,NB

		  NLWBN = NLW*(BN-1)
		  ISUM(BN) = 0
		  NPTS(BN) = 0
		  DO I=1,NLW
		     DO J=1,NSW
		        IF (INBUF(J,NLWBN+I).NE.IREPLACE .AND.
     +			    INBUF(J,NLWBN+I).NE.IEXCLUDE) THEN
			    ISUM(BN) = ISUM(BN)+INBUF(J,NLWBN+I)
			    NPTS(BN) = NPTS(BN)+1
			END IF
		     END DO
		  END DO
		  ISAMPL = 1
		  ISAMPR = NSW+1
C							set pointers
		  IF (QMASK) CALL ITLA(255,MBUF,NS)
		  IF (BN.EQ.1)  LINELOC = LINELOC+1
		  IF (LINELOC.GT.NLW) LINELOC=1
	    	  ISAMPLOC = NSW2
C							check each sample
	    	  DO J=1,NS
		     ISAMPLOC = ISAMPLOC+1
		     IF (INBUF(ISAMPLOC,NLWBN+LINELOC).EQ.IREPLACE) THEN
		    	IF (NPTS(BN).NE.0) THEN		   ! compute new value
			    OUTBUF(J) = FLOAT(ISUM(BN))/FLOAT(NPTS(BN))+0.5
			    IF (QMASK) CALL ITLA(0,MBUF(J),1)
		    	ELSE
			    OUTBUF(J) = IREPLACE	   ! nothing in window;
		    	END IF				   ! leave as is
		     ELSE
		    	OUTBUF(J) = INBUF(ISAMPLOC,NLWBN+LINELOC) ! don't
		     END IF					  ! replace
C							update ISUM and NPTS
		     IF (J.NE.NS) THEN
			DO K=1,NLW
			    IF(INBUF(ISAMPL,NLWBN+K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPL,NLWBN+K).NE.IEXCLUDE)	THEN
				ISUM(BN) = ISUM(BN)-INBUF(ISAMPL,NLWBN+K)
				NPTS(BN) = NPTS(BN)-1
			    END IF
			    IF(INBUF(ISAMPR,NLWBN+K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPR,NLWBN+K).NE.IEXCLUDE)	THEN
				ISUM(BN) = ISUM(BN)+INBUF(ISAMPR,NLWBN+K)
				NPTS(BN) = NPTS(BN)+1
			    END IF
			END DO
			ISAMPL = ISAMPL+1
			ISAMPR = ISAMPR+1
		     END IF
                  
	    	  END DO
C							     write out results
                  IF (QBINARY) THEN
C                                                       read in binary prefix
                      CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',NLBI+II,
     +                            'SAMP',1,'NSAMPS',NSBI,'BAND',BN,' ')

C                                                       write out binary prefix 
                      CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'LINE',NLBI+II,
     +                            'NSAMPS',NSBI+NS,'BAND',BN,' ')

	              CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,'LINE',NLBI+II, 
     +                            'SAMP',NSBI+1,'NSAMPS',NSAMPS,
     +                            'BAND',BN,' ')
                  ELSE
                      CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')
                  ENDIF
       	          IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')

C							     read new line into
C							     INBUF
	    	  IF (LINEN.LE.NLIN) THEN
                      IF (QBINARY) THEN 
                          CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                ISTAT,'LINE',NLBI+LINEN, 
     +                                'SAMP',NSBI+ISAMP1,'NSAMPS',
     +                                NSAMPS,'BAND',BN,' ')
                      ELSE
                          CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                ISTAT,'LINE',LINEN,'SAMP', 
     +                                ISAMP1,'NSAMPS',NSAMPS, 
     +                                'BAND',BN,' ')
                      ENDIF
	            ELSE
		      DO J=1,N1
		    	INBUF(J,NLWBN+LOCL) = IEXCLUDE
		      END DO
	    	    END IF
		ENDDO
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO	
        END IF
        RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zfill.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM zfill

   To Create the build file give the command:

		$ vimake f2			(VMS)
   or
		% vimake f2			(Unix)


************************************************************************/


#define PROGRAM	zfill
#define R2LIB

#define MODULE_LIST zfill.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define DEBUG        /* comment out upon delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create zfill.pdf
process help=*
 PARM 	INP 	TYPE=STRING
 PARM 	OUT 	TYPE=STRING 	COUNT=1:2
 PARM 	SIZE 	TYPE=INTEGER 	COUNT=4 	DEFAULT=(1,1,0,0)
 PARM 	SL 	TYPE=INTEGER 			DEFAULT=1
 PARM 	SS 	TYPE=INTEGER 			DEFAULT=1
 PARM 	NL 	TYPE=INTEGER 			DEFAULT=0
 PARM 	NS 	TYPE=INTEGER 			DEFAULT=0
 PARM 	NLW 	TYPE=INTEGER 			DEFAULT=3
 PARM 	NSW 	TYPE=INTEGER 			DEFAULT=3
 PARM 	REPLACE TYPE=INTEGER 			DEFAULT=0
 PARM 	EXCLUDE TYPE=INTEGER 			DEFAULT=-32768
 PARM 	DENSE	TYPE=KEYWORD	COUNT=(0:1)	DEFAULT=--	VALID="DENSE"
 PARM   BINARY  TYPE=KEYWORD    VALID=(BINARY,NOBINARY) DEFAULT="NOBINARY"
 END-PROC
.TITLE
 ZFILL
.HELP
 PURPOSE:
      ZFILL uses a window of NLW lines by NSW samples to fill in void areas
 of an image.  Void areas are assumed to be zero DN, but can be respecified
 by parameter.  Voids are filled with the mean value of all 'non-void DN'
 pixels in the window.  A mask showing filled pixels can be generated on 
 request.
      The user can select between two algorithms in ZFILL. The results of the
 algorithms are identical, but computation times vary dramatically. The
 'sparse' algorithm is faster if the input image is mostly empty; the 'dense'
 algorithm is faster if the input image is mostly complete.
      This program can be run on a BSQ file with 1 band or a BIL file with
 any number of bands.  The input can either be byte or halfword data.

 RESTRICTION:
      IF user specifies BINARY keyword, ZFILL will only work on the same 
 platform and operation systems on which the input image was created.

.PAGE
 BINARY LABELS PROCESSING:
      If the user specifies to preserve binary labels, ZFILL will copy the 
 input image's binary labels and relaying them to the output image without 
 modifying any part of label's information.

.PAGE
 EXECUTION:

 Examples

	ZFILL A B 'DENSE

	This command will use the default window (3 by 3) to scan
	input image A for pixels with value 0.  For each such pixel,
	the average of the surrounding pixels will be calculated, and 
	substituted for the original value. The 'dense' algorithm has
	been chosen, for the input has only a few 0 values to be replaced.

	ZFILL A B REPL=10 EXCL=8

	This command will use the default window (3 by 3) to scan
	input image A for pixels with value 10.  For each such pixel,
	the average of the surrounding pixels (excluding those valued 8)
	will be calculated, and substituted for the original value.

	ZFILL A (B,MSK)  NLW=2  NSW=5

	This command will scan image A for occurrences of 0-valued pixels
	(zero is the default value for replacement).  These will again
	be replaced by the averages of the surrounding values.  Note
	the window size which is specified to be 2 by 5.  Even dimensions
	are automatically increased to the next odd integer, so the
	window size which will be actually used is NLW=3, NSW=5.
	Note that the command above produces an mask showing which
	pixels were changed; mask pixels have DN 0 if they were changed
	in the output image, and 255 otherwise.

.PAGE

 OPERATION:

      ZFILL contains two algorithms. In the 'sparse' (default) mode, the 
  data lines needed for the window of the first pixel are read in and averaged;
  after this point, as the window moves across the image, the left (and top) 
  values which move out of the window are subtracted from the accumulating
  variables, and the new pixels appearing at the bottom and right of the window
  are added in.  Any time a pixel needs to be replaced, the average may be 
  easily calculated from these variables.
      In the 'dense' mode, pixels are checked for replacement first; the
 replacement value is computed only if needed. The entire window must be 
 examined to compute the replacement value each time a pixel is replaced.
      As a rule of thumb, the default (sparse) algorithm is faster if the
 fraction of pixels to be replaced is more than 2/NSW. The 'dense' algorithm
 is faster otherwise.
      When the filling window extends beyond the image (not image window)
 boundaries, the outside pixels are treated as excluded values.

 WRITTEN BY:  S. Z. Friedman, March 1983
 COGNIZANT PROGRAMMER:  Ron Alley
 REVISION:

  1987-07-16 Ron Alley
  2001-01-24 AXC Added support to preserve binary labels per request by
                 T. Roatsch from DLR. Added a new keyword in the pdf to 
                 enable the new capability. Updated test pdf and help 
                 file.
  2016-02-12 WLB Added enable/disable-log to tst pdf.

.LEVEL1
.VARIABLE INP
 STRING - Input image file
.VARIABLE OUT
 STRING - Output image file
.VARIABLE SIZE
 INTEGER - Standard VICAR size field
.VARIABLE SL
 INTEGER - Starting line
.VARIABLE SS
 INTEGER - Starting sample
.VARIABLE NS
 INTEGER - Number of lines
.VARIABLE NL
 INTEGER - Number of samples
.VARIABLE NLW
 INTEGER - Window length
.VARIABLE NSW
 INTEGER - Window width
.VARIABLE REPLACE
 INTEGER - DN value to replace
.VARIABLE EXCLUDE
 INTEGER - DN value to exclude
.VARIABLE DENSE
 KEYWORD - Use to fill mostly 
 complete datasets.
.KEYWORD - BINARY
Copy binary labels?
.LEVEL2
.VARIABLE NLW
 The length in lines of the window used to scan the input image.
.VARIABLE NSW
 The width in pixels of the window used to scan the input image.
.VARIABLE REPLACE
 The value specified by REPL will be replaced with the average of the
 other pixels within the window.
.VARIABLE EXCLUDE
 The value specified by EXCL will be ignored in the calculation of the
 average of the pixels within a given window.
.VARIABLE DENSE
 This invokes an algorithm that is more efficient if the input image has
 relatively few pixels to be replaced. As a rule of thumb, use the dense
 algorithm if the fraction of pixels to be replaced is less than 2/NSW.
.vari BINARY
This keyword parameter may be used to tell COPY that binary labels
and prefixes must be copied along with image data.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstzfill.pdf
!
! ZFILL TEST SCRIPT
!
procedure
refgbl $autousage
refgbl $syschar
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

enable-log

local PATH TYPE=STRING 

#if ($SYSCHAR(1)="VAX_VMS")
#  let PATH = "WMS_TEST_WORK:[TESTDATA.GLL]"
#  dcl del diff.out;*
#  dcl del test*.img*
#else
  let PATH = "/project/test_work/testdata/gll/"
  ush unalias rm
#  ush rm -f diff?.out
  ush rm -f test*.img*
#end-if

!
!cellgen OUT=C1515 NL=15 NS=15 CSIZE=5
genthis OUT=C1515 NL=15 NS=15 DN=(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3, +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6, +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6, +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6, +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6, +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6, +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9, +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9, +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9, +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9, +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9)
!
zfill C1515 OUT 
list OUT
zfill C1515 OUT REPL=5 
list OUT
zfill C1515 OUT REPL=1 
list OUT
zfill C1515 OUT REPL=6 
list OUT
zfill C1515 OUT REPL=5 'DENSE
list OUT
zfill C1515 OUT NSW=3 NLW=3 REPL=5
list OUT
zfill C1515 OUT NSW=5 NLW=5 REPL=5
list OUT
zfill C1515 OUT NSW=5 NLW=5 REPL=5 EXCL=2
list OUT
zfill C1515 OUT NSW=7 NLW=6 REPL=5
list OUT
zfill C1515 OUT NSW=7 NLW=6 REPL=5 EXCL=2
list OUT
zfill C1515 OUT NSW=1 NLW=5 REPL=5
list OUT
zfill C1515 OUT NSW=5 NLW=1 REPL=5
list OUT
zfill C1515 OUT REPL=257 'DENSE
list OUT
zfill C1515 (OUT,MSK) REPL=6
list OUT
list MSK
zfill C1515 (OUT,MSK) REPL=6 NSW=5 NLW=5
list OUT
list MSK
zfill C1515 OUT REPL=5 SIZE=(5,5,4,4)
list OUT
zfill C1515 OUT REPL=5 SIZE=(4,4,7,7)
list OUT
!
let $echo="no"
write ""
write ""
write "***********************************************"
write "*      TEST CASE COMPARING BINARY LABELS      *"
write "*                                             *"
write "*      There shouldn't be any difference      *"
write "*    between input and output binary lables.  *"
write "*                                             *"
write "***********************************************"

let $echo="yes"
 adespike inp=&"PATH"s0539939965.r out=test1.img
 zfill inp=test1.img out=test2.img 'bin
#gedrlist|stdout=in.out| test1.img nlh=800
#gedrlist|stdout=out.out| test2.img nlh=800
# (LWK - 15Mar2010) GEDRLIST has been obsoleted, replace with:
 label-remove test1.img test3.img 'bin
 label-create test3.img test1.img nl=829 ns=1000
 label-remove test2.img test3.img 'bin
 label-create test3.img test2.img nl=829 ns=1000
 difpic (test1.img test2.img) size=(1 1 12 1000) 'mod
 difpic (test1.img test2.img) size=(30 1 800 200) 'mod
let $echo="no"
write "*****************************************************"
write "* There should be 0 differences in the above tests. *"
write "*****************************************************"
#if ($SYSCHAR(1)="VAX_VMS")
#    dcl diff/out=diff.out in.out out.out
#else
#    ush diff in.out out.out >diff.out
#end-if

#write ""
#write "****************************************************"
#write "* Files diff1 & diff2.out should contain no entry. *"
#write "****************************************************"
#let $echo="yes"
# typetext diff1.out
# typetext diff2.out
#let $echo="no"

disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstzfill.log
local PATH TYPE=STRING
  let PATH = "/project/test_work/testdata/gll/"
  ush unalias rm
  ush rm -f test*.img*
genthis OUT=C1515 NL=15 NS=15 DN=(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,  +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,  +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,  +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,  +
                                  1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,  +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,  +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,  +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,  +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,  +
                                  4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,  +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,  +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,  +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,  +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,  +
                                  7,7,7,7,7,8,8,8,8,8,9,9,9,9,9)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
zfill C1515 OUT
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    0
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      7       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     10       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   2   2   2   4   6   6   6   6   6
      7       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      8       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      9       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
     10       4   4   4   4   4   6   8   8   8   7   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT REPL=1
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    1
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   2   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   2   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   2   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   2   2   2   2   2   2   3   3   3   3   3
      5       4   4   4   4   3   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      7       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     10       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT REPL=6
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    6
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   4   3   3   3   3
      7       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
     10       4   4   4   4   4   5   5   5   5   5   7   9   9   9   9
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT REPL=5 'DENSE
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   2   2   2   4   6   6   6   6   6
      7       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      8       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      9       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
     10       4   4   4   4   4   6   8   8   8   7   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=3 NLW=3 REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   2   2   2   4   6   6   6   6   6
      7       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      8       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
      9       4   4   4   4   4   4   5   5   5   6   6   6   6   6   6
     10       4   4   4   4   4   6   8   8   8   7   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=5 NLW=5 REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  5, NSW =  5, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   2   2   3   4   6   6   6   6   6
      7       4   4   4   4   4   3   3   2   4   5   6   6   6   6   6
      8       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      9       4   4   4   4   4   5   6   8   7   7   6   6   6   6   6
     10       4   4   4   4   4   6   7   8   8   8   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=5 NLW=5 REPL=5 EXCL=2
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  5, NSW =  5, REPLACE =    5, EXCLUDE =    2
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   3   5   5   5   6   6   6   6   6
      7       4   4   4   4   4   3   3   5   5   5   6   6   6   6   6
      8       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      9       4   4   4   4   4   5   6   8   7   7   6   6   6   6   6
     10       4   4   4   4   4   6   7   8   8   8   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=7 NLW=6 REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  7, NSW =  7, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   2   2   3   3   4   6   6   6   6   6
      7       4   4   4   4   4   3   3   3   4   4   6   6   6   6   6
      8       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      9       4   4   4   4   4   6   6   7   7   7   6   6   6   6   6
     10       4   4   4   4   4   6   7   7   8   8   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=7 NLW=6 REPL=5 EXCL=2
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  7, NSW =  7, REPLACE =    5, EXCLUDE =    2
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   3   3   4   5   5   6   6   6   6   6
      7       4   4   4   4   4   3   3   4   5   5   6   6   6   6   6
      8       4   4   4   4   4   5   5   6   7   6   6   6   6   6   6
      9       4   4   4   4   4   6   6   7   7   7   6   6   6   6   6
     10       4   4   4   4   4   6   7   7   8   8   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=1 NLW=5 REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  5, NSW =  1, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   2   2   2   2   2   6   6   6   6   6
      7       4   4   4   4   4   2   2   2   2   2   6   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      9       4   4   4   4   4   8   8   8   8   8   6   6   6   6   6
     10       4   4   4   4   4   8   8   8   8   8   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT NSW=5 NLW=1 REPL=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  1, NSW =  5, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      7       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      8       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
      9       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
     10       4   4   4   4   4   4   4   5   6   6   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 OUT REPL=257 'DENSE
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =  257
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      7       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     10       4   4   4   4   4   5   5   5   5   5   6   6   6   6   6
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
zfill C1515 (OUT,MSK) REPL=6
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    6
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   4   3   3   3   3
      7       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
      8       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   5   6   6   6   6
     10       4   4   4   4   4   5   5   5   5   5   7   9   9   9   9
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
list MSK
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      2     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      3     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      4     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      5     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      6     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
      7     255 255 255 255 255 255 255 255 255 255   0 255 255 255 255
      8     255 255 255 255 255 255 255 255 255 255   0 255 255 255 255
      9     255 255 255 255 255 255 255 255 255 255   0 255 255 255 255
     10     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
     11     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     12     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     13     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     14     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     15     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
zfill C1515 (OUT,MSK) REPL=6 NSW=5 NLW=5
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  5, NSW =  5, REPLACE =    6
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      2       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      3       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      4       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      5       1   1   1   1   1   2   2   2   2   2   3   3   3   3   3
      6       4   4   4   4   4   5   5   5   5   5   4   3   3   3   3
      7       4   4   4   4   4   5   5   5   5   5   4   4   3   3   3
      8       4   4   4   4   4   5   5   5   5   5   5   5   6   6   6
      9       4   4   4   4   4   5   5   5   5   5   6   7   9   9   9
     10       4   4   4   4   4   5   5   5   5   5   7   8   9   9   9
     11       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     12       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     13       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     14       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
     15       7   7   7   7   7   8   8   8   8   8   9   9   9   9   9
list MSK
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7       9      11      13      15
   Line
      1     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      2     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      3     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      4     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      5     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      6     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
      7     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
      8     255 255 255 255 255 255 255 255 255 255   0   0 255 255 255
      9     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
     10     255 255 255 255 255 255 255 255 255 255   0   0   0   0   0
     11     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     12     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     13     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     14     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     15     255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
zfill C1515 OUT REPL=5 SIZE=(5,5,4,4)
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3
   Line
      1       1   2   2   2
      2       4   3   2   2
      3       4   4   5   5
      4       4   4   5   5
zfill C1515 OUT REPL=5 SIZE=(4,4,7,7)
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    5
list OUT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GENTHIS   User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
 Task:ZFILL     User:wlb       Date_Time:Fri Feb 12 13:03:04 2016
     Samp     1       3       5       7
   Line
      1       1   1   2   2   2   2   2
      2       1   1   2   2   2   2   2
      3       4   4   3   2   2   2   4
      4       4   4   4   5   5   5   6
      5       4   4   4   5   5   5   6
      6       4   4   4   5   5   5   6
      7       4   4   6   8   8   8   7
let $echo="no"


***********************************************
*      TEST CASE COMPARING BINARY LABELS      *
*                                             *
*      There shouldn't be any difference      *
*    between input and output binary lables.  *
*                                             *
***********************************************
 adespike inp=/project/test_work/testdata/gll/s0539939965.r out=test1.img
Beginning VICAR task adespike
ADESPIKE version 21-JUL-95
    TOTAL NUMBER OF PIXELS CORRECTED USING ADJACENT SCAN LINE TEST         1126
    TOTAL NUMBER OF PIXELS CORRECTED USING SAME SCAN LINE TEST            3083
 zfill inp=test1.img out=test2.img 'bin
Beginning VICAR task zfill
ZFILL version 2016-02-12
INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE
ZFILL:  NLW =  3, NSW =  3, REPLACE =    0
 label-remove test1.img test3.img 'bin
Beginning VICAR task label
LABEL version 15-Nov-2010
 label-create test3.img test1.img nl=829 ns=1000
Beginning VICAR task label
LABEL version 15-Nov-2010
 label-remove test2.img test3.img 'bin
Beginning VICAR task label
LABEL version 15-Nov-2010
 label-create test3.img test2.img nl=829 ns=1000
Beginning VICAR task label
LABEL version 15-Nov-2010
 difpic (test1.img test2.img) size=(1 1 12 1000) 'mod
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
 difpic (test1.img test2.img) size=(30 1 800 200) 'mod
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
let $echo="no"
*****************************************************
* There should be 0 differences in the above tests. *
*****************************************************
$ Return
$!#############################################################################
