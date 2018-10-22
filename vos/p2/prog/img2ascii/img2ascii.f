C  PROGRAM IMG2ASCII

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM IMG2ASCII
C  PURPOSE ---
C
C	Convert the contents of an image to an ASCII file
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
	INTEGER*4 COUNT, SL, SS, NL, NS
	INTEGER*4 STAT, UNITIN, UNITOUT, NLI, NSI
	REAL*4 STRIP(10000), ZMIN, ZMAX
	integer*4 icode
	CHARACTER*3  NOTE, STATUS
	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org,index,organiz

	CHARACTER*72 STRING
	CHARACTER*40 OUTFILE
	LOGICAL*4 TRIM, STATS, uselines

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

	ZMIN = 3.0e10
	ZMAX = -3.0e10
	STATS = .FALSE.

        CALL IFMESSAGE ('IMG2ASCII version 2015-08-10')
c        CALL XVEACTION ('SA',' ')

C	Open up the input image

	CALL XVUNIT (UNITIN, 'INP', 1, STAT,' ')
	CALL XVOPEN (UNITIN, STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET  (UNITIN, STAT, 'NL', NLI,  'NS', NSI,'FORMAT',format,'ORG',org,' ')
	icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend  
        endif
        call xvclose(unitin,stat,' ')
c
	call xvopen(unitin,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')
             

	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)

C	Check for too many lines and/or samples
	IF ((SL.GT.NLI).OR.(SS.GT.NSI)) THEN
	    CALL XVMESSAGE ('??E - Invalid SIZE parameters entered !',' ')
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

	CALL XVP('ORG',organiz,count)
	if (organiz.eq.'COLUMNS') then
		CALL XVP('INDEX',index,count)
		if (index.eq.'YES') uselines = .true.
	endif
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
	if (icode.eq.1 .or. icode.eq.2 .or. icode.eq.3) then 
		if (organiz.eq.'ROWS') then
   	      call row_integer (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats)
		else
	      call column_integer (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats,uselines)
		endif
	endif  ! if (icode.eq.1 .or. icode.eq.2 .or. icode.eq.3 .and. organiz.eq."rows")

	if (icode.eq.4) then
		if (organiz.eq.'ROWS') then
		call row_real (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats)
		else
		call column_real (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats,uselines)
		endif
	endif
	CLOSE (UNIT=UNITOUT)
999     continue	
        CALL XVCLOSE(UNITIN,STAT,' ')

	RETURN
	END
C  -------------------------------------------------------------------
	subroutine row_integer (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats)
c
c	routine to write out BYTE,HALF or FULL data in row format
c
	implicit none
	integer*4 sl,ss,nl,ns,nchunks,stat,line
	integer*4 unitin,unitout,i,j,chunks
	real*4 strip(10000),zmin,zmax
	logical*4 stats
	CHARACTER*72 STRING
c

        NCHUNKS = ((NS/12)+.5)
        J = MOD(NS,12)
        IF(J.EQ.0)J=12

        DO LINE = SL, NL+SL-1
            CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
            I = 1
            DO CHUNKS = 1, NCHUNKS

C               Here's what we do if we can fill up the entire text string

              IF ((CHUNKS.NE.NCHUNKS).OR.(J.EQ.12)) THEN
                WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6)),int(STRIP(I+7)),
     *          int(STRIP(I+8)),int(STRIP(I+9)),int(STRIP(I+10)),int(STRIP(I+11))
              ELSE

C               We need to handle the other 11 sub-strings

                GOTO (1,2,3,4,5,6,7,8,9,10,11) J

1               WRITE (STRING,'(I6)')
     *          int(STRIP(I))
                        GOTO 100
2               WRITE (STRING,'(I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1))
                        GOTO 100
3               WRITE (STRING,'(I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2))
                        GOTO 100
4               WRITE (STRING,'(I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3))
                        GOTO 100
5               WRITE (STRING,'(I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4))
                        GOTO 100
6               WRITE (STRING,'(I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5))
                        GOTO 100
7               WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6))
                        GOTO 100
8               WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6)),int(STRIP(I+7))
                        GOTO 100
9               WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6)),int(STRIP(I+7)),
     *          int(STRIP(I+8))
                        GOTO 100
10              WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6)),int(STRIP(I+7)),
     *          int(STRIP(I+8)),int(STRIP(I+9))
                        GOTO 100
11              WRITE (STRING,'(I6,I6,I6,I6,I6,I6,I6,I6,I6,I6,I6)')
     *          int(STRIP(I)),int(STRIP(I+1)),int(STRIP(I+2)),int(STRIP(I+3)),
     *          int(STRIP(I+4)),int(STRIP(I+5)),int(STRIP(I+6)),int(STRIP(I+7)),
     *          int(STRIP(I+8)),int(STRIP(I+9)),int(STRIP(I+10))
              END IF

100           continue
!!!!          The following call to XVMESSAGE was enabled during the 
!!!!          porting of IMG2ASCII to UNIX to facilitate in the verification 
!!!!          of output data accross the various platforms.  Upon
!!!!          completion of the porting process, the call to xvmessage
!!!!          was disabled.
!!!!          call xvmessage (string,' ')

              IF (CHUNKS.NE.NCHUNKS) THEN
                WRITE (UNITOUT,'(1X,A)') STRING
              ELSE
                WRITE (UNITOUT,'(1X,A)') STRING(1:6*J)
              END IF
              I = I + 12
            END DO

C               Check for the min and max DN values
            IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
        END DO

        IF (STATS) THEN
          WRITE (STRING,'(A,I6,A,I6)') 'Minimum value: ', int(ZMIN),
     *          ' Maximum value: ', int(ZMAX)
          CALL XVMESSAGE (STRING,' ')
          WRITE (STRING,'(A,I4,A,I4)')
     *         'The output text file is dimensioned ',
     *          NL, ' by ', NS
          CALL XVMESSAGE (STRING,' ')
	  call putparm (zmin,zmax)
        ENDIF
c        END IF



	return
	end
C  -------------------------------------------------------------------
        subroutine row_real (unitin,unitout,sl,ss,nl,ns,zmin,zmax,stats)
c
c       routine to write out BYTE,HALF or FULL data in row format
c
        implicit none
        integer*4 sl,ss,nl,ns,nchunks,stat,line
        integer*4 unitin,unitout,i,j,chunks
        real*4 strip(10000),zmin,zmax
        logical*4 stats
        CHARACTER*120 STRING
c

        NCHUNKS = ((NS/12)+.5)
        J = MOD(NS,12)
        IF(J.EQ.0)J=12

        DO LINE = SL, NL+SL-1
            CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
            I = 1
            DO CHUNKS = 1, NCHUNKS

C               Here's what we do if we can fill up the entire text string
              IF ((CHUNKS.NE.NCHUNKS).OR.(J.EQ.12)) THEN
                WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5)

                WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I+6),STRIP(I+7),STRIP(I+8),STRIP(I+9),
     *          STRIP(I+10),STRIP(I+11)
              ELSE

C               We need to handle the other 11 sub-strings

                GOTO (1,2,3,4,5,6,7,8,9,10,11) J

1               WRITE (STRING,'(e10.3)')
     *          STRIP(I)
                        GOTO 100
2               WRITE (STRING,'(e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1)
                        GOTO 100
3               WRITE (STRING,'(e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2)
                        GOTO 100
4               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3)
                        GOTO 100
5               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4)
                        GOTO 100
6               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5)
                        GOTO 100
7               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5),STRIP(I+6)
                        GOTO 100
8               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5),STRIP(I+6),STRIP(I+7)
                        GOTO 100
9               WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5),STRIP(I+6),STRIP(I+7),STRIP(I+8)
                        GOTO 100
10              WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5),STRIP(I+6),STRIP(I+7),STRIP(I+8),STRIP(I+9)
                        GOTO 100
11              WRITE (STRING,'(e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)')
     *          STRIP(I),STRIP(I+1),STRIP(I+2),STRIP(I+3),STRIP(I+4),
     *          STRIP(I+5),STRIP(I+6),STRIP(I+7),STRIP(I+8),STRIP(I+9),
     *          STRIP(I+10)
              END IF
100           continue
!!!!          The following call to XVMESSAGE was enabled during the 
!!!!          porting of IMG2ASCII to UNIX to facilitate in the verification 
!!!!          of output data accross the various platforms.  Upon
!!!!          completion of the porting process, the call to xvmessage
!!!!          was disabled.
!!!!          call xvmessage (string,' ')

              IF (CHUNKS.NE.NCHUNKS) THEN
                WRITE (UNITOUT,'(1X,A)') STRING
              ELSE
                WRITE (UNITOUT,'(1X,A)') STRING(1:10*J)
              END IF
              I = I + 12
            END DO

C               Check for the min and max DN values
            IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
        END DO

        IF (STATS) THEN
          WRITE (STRING,'(A,e10.3,A,e10.3)') 'Minimum value: ', ZMIN,
     *          ' Maximum value: ', ZMAX
          CALL XVMESSAGE (STRING,' ')
          WRITE (STRING,'(A,i4,A,i6)')
     *         'The output text file is dimensioned ',
     *          NL, ' by ', NS
          CALL XVMESSAGE (STRING,' ')
          call putparm (zmin,zmax)
         ENDIF

        return
        end
C  -------------------------------------------------------------------
	subroutine column_integer (unitin,unitout,sl,ss,nl,ns,
     * zmin,zmax,stats,uselines)
c
c       routine to write out BYTE,HALF or FULL data in column format
c	with or without line number
c
        implicit none
        integer*4 sl,ss,nl,ns,ans,stat,line
        integer*4 unitin,unitout,i,totlines
        real*4 strip(10000),zmin,zmax
        logical*4 uselines,stats
        CHARACTER*80 STRING
c
c	
	totlines = 0
        DO LINE = SL, NL+SL-1
            CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
		if (uselines) then
		   do i=ss,ns+ss-1
			totlines=totlines + 1
		   write (unitout,'(1x,i6,2x,i6)') totlines,int(strip(I))
		   enddo
		else
		   do i=ss,ns+ss-1
		      write (unitout,'(1x,i6)') int(strip(i))	
		   enddo
		endif
C               Check for the min and max DN values
            IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
	enddo
c
       IF (STATS) THEN
          WRITE (STRING,'(A,i6,A,i6)') 'Minimum value: ', int(ZMIN),
     *          ' Maximum value: ', int(ZMAX)
          CALL XVMESSAGE (STRING,' ')
	  ans=1
	  if (uselines) ans=2
              WRITE (STRING,'(A,i4,A,i6)')
     *         'The output text file is dimensioned ',
     *          NL, ' by ', ans
              CALL XVMESSAGE (STRING,' ')
	  call putparm (zmin,zmax)
	ENDIF
	return
	end
C  -------------------------------------------------------------------
        subroutine column_real (unitin,unitout,sl,ss,nl,ns,
     * zmin,zmax,stats,uselines)
c
c       routine to write out REAL data in column format
c       with or without line number
c
        implicit none
        integer*4 sl,ss,nl,ns,ans,stat,line
        integer*4 unitin,unitout,i,totlines
        real*4 strip(10000),zmin,zmax
        logical*4 uselines,stats
        CHARACTER*80 STRING
c
c	
	totlines = 0
        DO LINE = SL, NL+SL-1
           CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
                if (uselines) then
                   do i=ss,ns+ss-1
		     totlines = totlines + 1
                   write (unitout,'(1x,i6,2x,e10.3)') totlines,strip(I)
                   enddo
                else
                   do i=ss,ns+ss-1
                      write (unitout,'(1x,e10.3)') strip(i)
                   enddo
	        endif
C               Check for the min and max DN values
            IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
        enddo
c
       IF (STATS) THEN
          WRITE (STRING,'(A,e10.3,A,e10.3)') 'Minimum value: ', ZMIN,
     *          ' Maximum value: ', ZMAX
          CALL XVMESSAGE (STRING,' ')
          ans=1
          if (uselines) ans=2
          WRITE (STRING,'(A,i4,A,i2)')
     *         'The output text file is dimensioned ',
     *          NL, ' by ', ans
          CALL XVMESSAGE (STRING,' ')
	  call putparm (zmin,zmax)
         END IF

	return
	end
C  -------------------------------------------------------------------
	SUBROUTINE MINMAX (STRIP,ZMIN,ZMAX,NS)

	implicit none
	INTEGER*4  NS, I
	REAL*4 STRIP(*), ZMIN, ZMAX

	DO 100 I = 1, NS
	    IF (STRIP(I).LT.ZMIN) ZMIN = STRIP(I)
	    IF (STRIP(I).GT.ZMAX) ZMAX = STRIP(I)
100	CONTINUE

	RETURN
	END
C -------------------------------------------------------------------
      subroutine putparm (zmin,zmax)
c            
      implicit none
      integer*4 parb(1000),xcont,xadd,stat
      real*4 zmin,zmax
c
      call xqini (parb,1000,xcont)
      call xqreal (parb,'minval',1,zmin,xadd,stat)
      call xqreal (parb,'maxval',1,zmax,xadd,stat)
      call xvqout (parb,stat)
      call chkstat (stat,'??E - XVQout error',0,0,0)

	return
	end


C  ------------------ E N D   O F   S O U R C E ----------------------
