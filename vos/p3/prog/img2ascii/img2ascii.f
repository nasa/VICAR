C  PROGRAM IMG2ASCII

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM IMG2ASCII
C  PURPOSE ---
C
C	Convert the contents of an image to an ASCII file
C
C  REVISION HISTORY ---
C       23-Feb-2001  ...REA...  Bug fix to print last 12 samples of each line
C
C  INPUT ---
C	INP	File name of image
C
C  OUTPUT ---
C	OUT	An ASCII file
C
C  RESTRICTIONS ---
C
C
C  SUBROUTINES CALLED ---
C	ABEND		Stops execution and makes a clean exit
C	CLOSE		Normal FORTRAN file close
C	OPEN		Normal FORTRAN file open
C	XVMESSAGE       Print a message to the user
C	XVCLOSE		Close a file from within VICAR/TAE
C	XVOPEN		Open a file from within VICAR/TAE
C	XVP		Parameter acquisition routine
C	XVPARM		Extract parameter value
C	XVSIZE		Get window size of incomming image
C	XVUNIT		Get the unit number
C	XVWRIT		Writes an array of data to a file
C
C  COMMENTS ---
C
C)
C
C  MODE DECLARATIONS ---
	INTEGER LINE, COUNT, SL, SS, NL, NS
	INTEGER STAT, UNITIN, UNITOUT, I, J, IEND
	INTEGER STRIP(10000), ZMIN, ZMAX, NLI, NSI
	CHARACTER*72 STRING
	CHARACTER*40 OUTFILE
	CHARACTER*3  NOTE
        CHARACTER*3  STATUS
	LOGICAL TRIM, STATS

C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

C		+=================+
C		| INITIALIZATIONS |
C		+=================+

        data unitin/0/, stat/0/, sl/0/, ss/0/, nl/0/, ns/0/
        data nli/0/, nsi/0/, count/0/, strip/10000*0/
        data string/' '/, OUTFILE/' '/, NOTE/' '/,STATUS/' '/

	ZMIN = 32768
	ZMAX = -32767
	STATS = .FALSE.

        CALL IFMESSAGE ('IMG2ASCII version 16-Feb-2001')
        CALL XVEACTION ('SA',' ')

C	Open up the input image

	CALL XVUNIT (UNITIN, 'INP', 1, STAT,' ')
	CALL XVOPEN (UNITIN, STAT,'U_FORMAT','FULL',' ')
	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)
	CALL XVGET  (UNITIN, STAT, 'NL', NLI,  'NS', NSI,' ')

C	Check for too many lines and/or samples
	IF ((SL.GT.NLI).OR.(SS.GT.NSI)) THEN
	    CALL XVMESSAGE ('Invalid SIZE parameters entered !',' ')
	    GOTO 999
	END IF

	TRIM = .FALSE.
	IF ((SL+NL-1).GT.NLI) THEN
	    TRIM = .TRUE.
	    NL = NLI-SL + 1
	ENDIF

	IF ((SS+NS-1).GT.NSI) THEN
	    TRIM = .TRUE.
	    NS = NSI-SS + 1
	ENDIF

C	Open up the output text file

	CALL XVP('OUT',OUTFILE,COUNT)

C	Open an output text file (no VICAR label)

!       Obtain unit number for output file
	CALL XVUNIT (UNITOUT, 'OUT', 1, STAT,' ')

!       If output file currently exists, Open file as an existing 'OLD' file
	OPEN(UNIT=UNITOUT,FILE=OUTFILE,STATUS='OLD',ERR=10100)
        goto 10101

!       Else open output file as a 'NEW' file
10100	OPEN(UNIT=UNITOUT,FILE=OUTFILE,STATUS='NEW',ERR=999)

C		Should we give file info at the end ?

10101   continue
	CALL XVP('NOTES',NOTE,COUNT)
	IF(NOTE(1:2).EQ.'ON') STATS = .TRUE.

C	   ************ Begin EXECUTION *************

C------------------------ I M G 2 A S C I I ----------------------------

C	Program IMG2ASCII

	DO LINE = SL, NL+SL-1
	    CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
	    DO I=1, NS, 12
	      IEND = MIN(NS,I+11)
	      WRITE (UNITOUT,'(1X,12I6)') (STRIP(J),J=I,IEND)
	    END DO

C		Check for the min and max DN values
	    IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
	END DO

	IF (STATS) THEN
	  WRITE (STRING,'(A,I6,A,I6)') 'Minimum value: ', ZMIN, 
     *		' Maximum value: ', ZMAX
	  CALL XVMESSAGE (STRING,' ')
	  WRITE (STRING,'(A,I4,A,I4)') 
     *         'The output text file is dimensioned ',
     *		NL, ' by ', NS
	  CALL XVMESSAGE (STRING,' ')
	ELSE
	 IF (TRIM) THEN
	  CALL XVMESSAGE 
     *      ('SIZE field exceeded the image size! Output truncated',' ')
	  WRITE (STRING,'(A,I4,A,I4)') 
     *           'The output text file is dimensioned ',
     *		NL, ' by ', NS
	  CALL XVMESSAGE (STRING,' ')
	 END IF
	END IF

	CLOSE (UNIT=UNITOUT)
999     continue	
        CALL XVCLOSE(UNITIN,STAT,' ')

	RETURN
	END

C  -------------------------------------------------------------------

	SUBROUTINE MINMAX (STRIP,ZMIN,ZMAX,NS)

	INTEGER STRIP(*), ZMIN, ZMAX, NS, I

	DO 100 I = 1, NS
	    IF (STRIP(I).LT.ZMIN) ZMIN = STRIP(I)
	    IF (STRIP(I).GT.ZMAX) ZMAX = STRIP(I)
100	CONTINUE

	RETURN
	END
C  ------------------ E N D   O F   S O U R C E ----------------------
