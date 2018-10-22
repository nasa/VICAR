       PROGRAM  VLOOKUP

C     8-89  RGD  QUICK HACK TO LOOKUP FOR READING VIDS LOOKUP TABLES FOR VOYAGER
c    10-90  lwk  cleaned up program for P2SOR, following RGD's guidelines
C     2-94  SP   Made portable for UNIX.  Changed code to use 
C                BYTE2INT AND INT2BYTE for converting 
C                between BYTE and INTEGER.  Added XVEACTION and 
C                IFMESSAGE calls.  CHANGED TO USE IBIS2 CALLS.  Changed to
C                read just the specified columns of the LUT file.
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

      PARAMETER (MAXFIL=5)	! MAX. NUMBER OF INPUT/OUTPUT FILES OR BANDS
      PARAMETER (MAXSAMPS=1000000)  ! MAX NUMBER OF SAMPLES PER LINE.
      INTEGER PARM(2000), USE(MAXFIL)
      INTEGER*2 TABLE2(MAXFIL,256)
      BYTE IBUF(MAXSAMPS), OBUF(MAXSAMPS), OBUF2(MAXSAMPS), 
     & OBUF3(MAXSAMPS), TABLE(256,MAXFIL)
      INTEGER ODSN(MAXFIL), IDSN(MAXFIL)
      LOGICAL TBLF
      INTEGER*2 VIDSTABLE(256)
      INTEGER VCOLLEN, VNCOLS, STATUS, LUNIT
      CHARACTER*256 LUTFIL
      CHARACTER*8 FMT
C
C=================START OF EXECUTABLE CODE===============================     

      CALL IFMESSAGE('VLOOKUP version 4-MAR-1994')
      DNMIN = 0                ! BYTE OUTPUT CURRENTLY.
      DNMAX = 255
C
C  DEFAULTS
      DO I=1,MAXFIL
	USE(I) = I
      ENDDO
      TBLF=.FALSE.
      CALL ZIA(TABLE2,MAXFIL*128)
C
C  GET THE PARAMETERS ENTERED BY THE USER.

      CALL XVPCNT( 'INP', NI )
      CALL XVPCNT( 'OUT', NO )

C
C  'TABLE' (OLD 'PS') PARAMETER:
 
      PSNO = 0
      CALL XVPARM( 'TABLE', PSNO, ICOUNT, IDEF, 1 )
      IF (ICOUNT.GT.0)  THEN
         CALL PSTAB(PSNO,TABLE2)
      END IF
C
C  'COLUMN' (OLD 'USE') PARAMETER:

      CALL XVPARM( 'COLUMN', PARM, IVALS, IDEF, 0 )
      IF (IVALS.GT.0) THEN
	IVALS = MIN( IVALS, MAXFIL)
	DO I = 1, IVALS
	  USE(I) = PARM(I)
	ENDDO
      ENDIF
C
C  OPEN DATA SETS

      DO I=1,NI

	CALL XVUNIT( IDSN(I), 'INP', I, IND,' ')
	CALL XVOPEN( IDSN(I), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     1 'IO_ACT', 'SA',' ')

	CALL XVGET( IDSN(I), IND, 'FORMAT', FMT, 'NB', NBI,' ')
	IF (FMT.NE.'BYTE') CALL MABEND('ONLY BYTE DATA ALLOWED')
	IF (NBI.GT.1 .AND. NI.GT.1) THEN
	  CALL XVMESSAGE(
     .         'MULTIPLE INPUT FILES IMPLIES SINGLE-BAND MODE:',' ')
	  CALL XVMESSAGE('  BANDS BEYOND THE FIRST ARE IGNORED',' ')
	  NBI = 1
	ENDIF
	IF (NBI.GT.MAXFIL) THEN
	  CALL XVMESSAGE('INPUT HAS TOO MANY BANDS, EXCESS IGNORED',' ')
	  NBI = MAXFIL
	ENDIF

	IF (I.EQ.1) THEN
	  CALL XVSIZE( SL, SS, NL, NS, dummy1,dummy2)
	  CALL XVBANDS( SB, NB,DUMMY3)
	  IF (NB.GT.1 .AND. NO.GT.1) THEN
	    IF (NB.GT.NBI) THEN		! IF 'NB' SPECIFIED EXPLICITLY
	      CALL MABEND('ONLY ONE OUTPUT ALLOWED IF MULTI-BAND')
	    ELSE
	      CALL XVMESSAGE(
     .         'MULTIPLE OUTPUT FILES IMPLIES SINGLE-BAND MODE:',' ')
	      CALL XVMESSAGE('  BANDS BEYOND THE FIRST ARE IGNORED',' ')
	      NB = 1
	    ENDIF
	  ENDIF
	ELSE
	  CALL XVGET( IDSN(I), IND, 'NL', NLI, 'NS', NSI,' ')
	  IF (NLI .LT. SL+NL-1  .OR. NSI .LT. SS+NS-1) CALL MABEND(
     1  'ERROR: ALL INPUT IMAGES SHOULD HAVE SAME SIZE')
	ENDIF

		! IF SEQUENTIAL READS, MOVE TO JUST BEFORE 1ST LINE:
	IF (NBI.EQ.1.AND.SL.GT.1) 
     .      CALL XVREAD(IDSN(I),IBUF,IND,'LINE',SL-1,' ')

      ENDDO

      NINP = MAX( NI, NBI)
      NOUT = MAX( NO, NB)

C  READ THE TABLE FROM THE INPUT DATA SET (IF PROVIDED)

      TBLF = .FALSE.
      CALL XVPARM( 'LUTFILE', LUTFIL, ICOUNT, IDEF, 1)
      IF (ICOUNT.GT.0) THEN
	TBLF = .TRUE.
	CALL XVUNIT (LUNIT,'NONE',1,STATUS,'U_NAME',LUTFIL, ' ')

        CALL IBIS_FILE_OPEN(LUNIT, LIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(LUNIT, STATUS, 1)

C  get the # rows & columns of the input IBIS file:

	mcount = ibis_file_get( LIBIS,'nc', VNCOLS, 1,1 )
	if (mcount.lt.0) call ibis_signal( LIBIS, mcount, 1)
	mcount = ibis_file_get( LIBIS,'nr', VCOLLEN, 1,1 )
	if (mcount.lt.0) call ibis_signal( LIBIS, mcount, 1)

	IF (VNCOLS .LE. 0) THEN	! TABLE IS ALREADY 0 IF IT DOESN'T EXIST
	  CONTINUE	! (??)
	ELSE
	  DO ICOL = 1,MIN(NOUT,VNCOLS)   ! READ JUST DESIRED COLUMNS.
            call ibis_column_set(LIBIS,'u_format','HALF',USE(ICOL),
     .                           status)
            if (status .ne.1) call ibis_signal( LIBIS, status, 1)
	    call ibis_column_read( LIBIS, VIDSTABLE, 
     +			USE(ICOL), 1, MIN(VCOLLEN,256), status )
            if (status .ne.1) call ibis_signal( LIBIS, status, 1)
	    DO I = 1, MIN(VCOLLEN,256)
	      TABLE2(ICOL,I) = VIDSTABLE(I) ! SHUFFLE
	    ENDDO
	  ENDDO
	ENDIF


	call ibis_file_close( LIBIS, ' ', status ) 
        if (status .ne.1) call ibis_signal( LIBIS, status, 1)
      ENDIF

      IF (.NOT. TBLF .AND. PSNO .EQ. 0) 
     .    CALL MABEND('ERROR: NEITHER LUTFILE NOR TABLE ENTERED.')

      DO I=1,NO
	CALL XVUNIT( ODSN(I), 'OUT', I, IND ,' ')
	CALL XVOPEN( ODSN(I), IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     1 'IO_ACT', 'SA', 'U_NL', NL, 'U_NS', NS, 'U_NB', NB,' ')
      ENDDO
C
C  REFORMAT THE TABLE - CONVERT TO BYTE
      DO J=1,NOUT
       IF (TBLF)  THEN
         DO I=1,256
          ITEMP = MAX( DNMIN, TABLE2(J,I) )
          ITEMP = MIN( DNMAX, ITEMP )
	  TABLE(I,J) = INT2BYTE( ITEMP )
 	 ENDDO
       ELSE
         DO I=1,256
          ITEMP = MAX( DNMIN, TABLE2(USE(J),I) )  ! IF COLUMN AND TABLE
          ITEMP = MIN( DNMAX, ITEMP )             ! THEN USE(J) USED.
	  TABLE(I,J) = INT2BYTE( ITEMP )
 	 ENDDO
       END IF
      ENDDO
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP  - USE ARRAY ALREADY FACTORED IN.

      IF (NINP.EQ.1 .AND. NOUT.EQ.3) THEN	! IF PSEUDO-COLOR

	DO II=1,NL           				! LOOP OVER LINES.
	  CALL XVREAD( IDSN(1), IBUF, IND,' ')
					                        ! DO LOOKUP.
	  CALL LOOKUPLOOP3( IBUF(SS), NS, OBUF, TABLE(1,1), OBUF2,
     1  TABLE(1,2), OBUF3, TABLE(1,3))
	  IF (NO.EQ.3) THEN
	    CALL XVWRIT( ODSN(1), OBUF, IND,' ')
	    CALL XVWRIT( ODSN(2), OBUF2, IND,' ')
	    CALL XVWRIT( ODSN(3), OBUF3, IND,' ')
	  ELSE
	    CALL XVWRIT( ODSN(1), OBUF, IND, 'LINE', II, 'BAND', 1,' ')
	    CALL XVWRIT( ODSN(1), OBUF2, IND, 'LINE', II, 'BAND', 2,' ')
	    CALL XVWRIT( ODSN(1), OBUF3, IND, 'LINE', II, 'BAND', 3,' ')
	  ENDIF
	ENDDO

      ELSE

	DO II=1,NL		! LOOP OVER LINES.
	  DO J=1,NOUT			! LOOP OVER OUTPUT IMAGES.
	    IF (J.LE.NINP) THEN
	      IF (NBI.EQ.1) THEN
		CALL XVREAD( IDSN(J), IBUF, IND,' ')
	      ELSE
		CALL XVREAD( IDSN(1), IBUF, IND, 'LINE', SL-1+II,
     .                       'BAND', J,' ')
	      ENDIF
            ENDIF
            CALL LOOKUPLOOP (IBUF(SS),OBUF,TABLE(1,J),NS)  ! DO LOOKUP.
	    IF (NB.EQ.1) THEN
	      CALL XVWRIT( ODSN(J), OBUF, IND,' ')
	    ELSE
	      CALL XVWRIT( ODSN(1), OBUF, IND, 'LINE',II, 'BAND',J,' ')
	    ENDIF
	  ENDDO
	ENDDO
      ENDIF

      RETURN          ! NORMAL END.
      END

	SUBROUTINE PSTAB(NUM,TABLE)
C#######################################################################
C  NAME OF ROUTINE
C     PSTAB	    ( PSeudo color TABle)
C
C  PURPOSE
C      PSTAB places into the array TABLE the IDISPLAY pseudocolor table
C      that corresponds to the number NUM.
C      
C  ENVIRONMENT
C      UNIX or  VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    4-14-94   Grouped DATA statements after all other declarations per ANSI77.
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETER
C      NUM	     - THE NUMBER OF THE PSEUDOCOLOR TABLE DESIRED.
C  OUTPUT PARAMETER
C      TABLE ARRAY   - THE PSEUDOCOLOR TABLE ITSELF
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

        PARAMETER (MAXFIL=5)	! MAX. NUMBER OF INPUT/OUTPUT FILES OR BANDS
	INTEGER*2 TABLE(MAXFIL,256)
	INTEGER*2 PS(256,3,7)
	INTEGER*2 PS1(256,3)
	INTEGER*2 PS2(256,3)
	INTEGER*2 PS3(256,3)
	INTEGER*2 PS4(256,3)
	INTEGER*2 PS5(256,3)
	INTEGER*2 PS6(256,3)
	INTEGER*2 PS7(256,3)
	EQUIVALENCE (PS(1,1,1),PS1),(PS(1,1,2),PS2),(PS(1,1,3),PS3),
     +		    (PS(1,1,4),PS4),(PS(1,1,5),PS5),(PS(1,1,6),PS6),
     +		    (PS(1,1,7),PS7)

      DATA PS1       /128*0,32*84,96*255,				    !R
     +		     32*0,32*128,64*255,32*200,32*255,32*128,32*0,	    !G
     +		     96*255,160*0/					    !B
      DATA PS2	     /16*170,16*115,16*125,16*0,16*50,16*30,64*0,	    !R
     +		     16*200,80*255,
     +		     32*0,16*70,16*50,16*110,16*150,16*200,16*185,	    !G
     +		     16*225,48*255,16*215,16*160,16*100,16*0,
     +		     16*255,16*210,16*240,16*255,16*224,32*200,16*120,	    !B
     +		     16*100,16*0,16*150,80*0/
      DATA PS3      /104*0,8*90,8*130,16*220,48*255,8*200,8*220,            !R
     +		     8*255,8*230,8*245,32*255,
     +		     32*0,8*70,8*100,8*130,8*150,8*170,8*190,8*210,	    !G
     +		     8*220,8*200,8*180,16*170,8*200,8*220,8*240,8*255,
     +		     8*140,8*120,8*80,8*60,40*0,8*50,8*0,8*255,
     +		     8*0,8*160,8*200,8*255,8*230,8*200,8*170,8*150,	    !B
     +		     8*130,8*100,8*80,8*60,64*0,24*80,8*60,16*0,8*100,
     +		     8*120,8*150,8*180,16*255/
      DATA PS4      /112*0,16*130,80*255,16*230,32*255,			    !R
     +		     32*0,16*100,16*130,16*170,16*210,16*200,16*170,	    !G
     +		     16*220,16*255,16*140,16*80,64*0,
     +		     16*160,16*255,16*200,16*170,16*130,16*80,64*0,	    !B
     +		     32*80,16*0,16*100,16*150,16*255/
      DATA PS5      /128*0,128*255,					    !R
     +		     32*0,32*130,32*170,32*200,32*255,32*80,64*0,	    !G
     +		     32*255,32*170,32*130,64*0,32*80,32*0,32*150/	    !B
      DATA PS6      /126*0,130*255,					    !R
     +		     42*0,42*170,42*200,42*255,88*0,			    !G
     +		     42*255,42*130,126*0,46*150/			    !B
      DATA PS7      /128*0,128*255,					    !R
     +		     64*0,64*190,64*255,64*0,				    !G
     +		     64*255,64*100,128*0/				    !B
C
	DO I=1,256
	    TABLE(1,I) = PS(I,1,NUM)
	    TABLE(2,I) = PS(I,2,NUM)
	    TABLE(3,I) = PS(I,3,NUM)
	    TABLE(4,I) = 0
	    TABLE(5,I) = 0
	END DO
	RETURN
	END
