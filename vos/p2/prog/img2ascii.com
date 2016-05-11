$!****************************************************************************
$!
$! Build proc for MIPL module img2ascii
$! VPACK Version 2.1, Monday, August 10, 2015, 15:08:54
$!
$! Execute by entering:		$ @img2ascii
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
$ write sys$output "*** module img2ascii ***"
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
$ write sys$output "Invalid argument given to img2ascii.com file -- ", primary
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
$   if F$SEARCH("img2ascii.imake") .nes. ""
$   then
$      vimake img2ascii
$      purge img2ascii.bld
$   else
$      if F$SEARCH("img2ascii.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake img2ascii
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @img2ascii.bld "STD"
$   else
$      @img2ascii.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create img2ascii.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack img2ascii.com -mixed -
	-s img2ascii.f -
	-i img2ascii.imake -
	-p img2ascii.pdf -
	-t tstimg2ascii.pdf tstimg2ascii.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create img2ascii.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create img2ascii.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM img2ascii

   To Create the build file give the command:

		$ vimake img2ascii			(VMS)
   or
		% vimake img2ascii			(Unix)

************************************************************************/
#define PROGRAM	img2ascii
#define R2LIB
#define MODULE_LIST img2ascii.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create img2ascii.pdf
PROCESS		HELP=*
!
! IMG2ASCII - Create an ASCII text file of DN values from an image
!
PARM INP    TYPE=(STRING,99) COUNT=(0:1)    DEFAULT=--
PARM OUT    TYPE=(STRING,99) COUNT=(0:1)    DEFAULT=--
PARM SIZE   TYPE=INTEGER     COUNT=4    DEFAULT=(1,1,0,0)
PARM NOTES  TYPE=(STRING,3)  COUNT=1    VALID=("ON","OFF") DEFAULT="OFF"
PARM ORG    TYPE=STRING      COUNT=1    VALID=("COLUMNS","ROWS") DEFAULT="ROWS"
PARM INDEX  TYPE=STRING      COUNT=1    VALID=("YES","NO") DEFAULT="YES"
local dummy TYPE=REAL        count=1
PARM MINVAL TYPE=name        DEFAULT=dummy
PARM MAXVAl TYPE=name        DEFAULT=dummy


!# annot function="Vicar Data Conversion"
!# annot keywords=("ASCII file",DN,dimension,file,size,BYTE)

END-PROC
.TITLE
Converts image data to ASCII text file

.HELP
PURPOSE

	Create an ASCII file of values from a VICAR image

TAE COMMAND LINE FORMAT

	IMG2ASCII INP OUT PARMS

	Where valid parms are SIZE, NOTES, ORG and INDEX.

    SIZE allows the user to exract a portion of the image.
    If the user exceeds the image bounds then the listing
    will be trimmed to the actual number of samples.
    NOTES provides the maximum and minimum values of the
    extracted data. The values are returned in MIMVAL and
    MAXVAL.
    ORG allow the user to specify that the output will be
    placed in rows. The INDEX parameter allows that the
    output row data will have a sequence number to start
    each row.  Normally, the data will be placed in ROW
    order in blocks appropriate the the number of samples.
 
.PAGE
EXAMPLES

    IMG2ASCII INP=SURFACE.IMG OUT=SURFACE.TXT SIZE=(100,100,50,50) NOTES=ON

	In this example, IMG2ASCII produces an ASCII text file, SURFACE.TXT
of values from the VICAR image SURFACE.IMG starting at line and sample 100
and ending at line and sample 150. Upon program completion, with the
NOTES switch on, the min and max values encountered and the dimension of
the text file written are reported to the user.

.PAGE
OPERATION

	Only the DN values are output. The line and sample positions are not
output to the text file. The data maybe output in rows or columns.
In the case of columns, the user can request the 1-dimensional position
order of the data. This may be useful for plotting. The program will notify
the user as to the dimension of the newly created text file (in the case
that the user specifies a SIZE larger than the input picture the output
file will trim to the max line and samples of the input picture).

    Typically, this is used for small segments of images. A user might
extract a small seqment of a 10K by 10K image for later use in a
vicar procedure. The user can also get the maximum and minimum values
into output tcl variables.

    Img2ascii works on BYTE, HALF, FULL and REAL images.

RESTRICTIONS

	Converting binary VICAR images to ASCII text files will create
files that are much larger in disk storage space than the images. An image
100 square that uses 21 blocks will create an ASCII file of 134 blocks.

The maximum size of image is 10000 in the sample direction.

HISTORY

Revisions:

  1994-06-27  F. Moss      Add test pdf
  1994-09-05  J. Turner (CRI)      Made portable for UNIX
  2012-10-31  R. Bambery   Fix .HELP and .TITLE order in pdf
                           Added ORG and INDEX parameters to
                           control output format. Added return
                           parameters MINVAL and MAXVAL. Allow
                           BYTE, HALF, FULL and REAL images. 
  2015-03-09  W. Bunch     Fixed WRITE format error.
  2015-08-10  W. Bunch - replaced xqout call with xvqout call 
                         to pass out vars to shell vicar

.LEVEL1
.VARIABLE INP
A VICAR image
(BYTE or HALF)
.VARIABLE OUT
ASCII text file
(No VICAR label)
.VARIABLE SIZE
Normal VICAR size field
.VARIABLE NOTES
Switch (ON or OFF)
.VARIABLE ORG
ASCII Text file in Columns
 or Rows
.VARIABLE INDEX
If in Column format do
you want 2 columns, the
first being its order
and the second being the
value.
.LEVEL2
.VARIABLE NOTES
ON and OFF switch
indicating the min
and max values and
the dimension of 
the text file
written to disk.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstimg2ascii.pdf
procedure
local   maxval  type=real count=1
local   minval  type=real count=1
local   iminval type=integer count=1
local   imaxval type=integer count=1
! Oct 31, 2012 - RJB
! TEST SCRIPT FOR IMG2ASCII
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen typetext 
!
! External Programs:
!   <none>
! 
! External test data: 
!   <none>
!            
! Output: BYTE, HALF, FULL and REAL test images    
!
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
! Gen all images
gen byte1.img 100 100
gen byte2.img 50 50

gen half1.img 100 100 ival=300 format=HALF
gen half2.img  50  50 ival=300 format=HALF

gen full1.img 100 100 ival=36000 format=FULL
gen full2.img  50  50 ival=36000 format=FULL

gen real1.img 100 100 ival=3000. format=REAL
gen real2.img  50  50 ival=3000. format=REAL
!
! TEST 1 - BYTE - minimal parameters
!
img2ascii byte2.img bout2.txt
typetext bout2.txt
!
! TEST 2 - BYTE with SIZE parameter
!
img2ascii byte1.img bout1.txt size=(1,1,20,20)
typetext bout1.txt
!
! TEST 3 - BYTE with ORG=COLUMNS, no index
!
img2ascii byte1.img bout1a.txt size=(1,1,20,20) org=COLUMNS
typetext bout1a.txt
!
! TEST 4 - BYTE with ORG=COLUMNS, INDEX=yes
img2ascii byte1.img bout1b.txt size=(1,1,20,20) org=COLUMNS +
    index=yes
typetext bout1b.txt
!
! TEST 5 - BYTE with ORG=COLUMNS, INDEX=yes NOTES=on
!       with return values
!
img2ascii byte1.img bout1c.txt size=(1,1,20,20) org=COLUMNS +
    index=yes notes=on minval=minval maxval=maxval

let $echo="no"
let iminval = $fix(minval)
let imaxval = $fix(maxval)
write "maximum value = &imaxval  minimum value = &iminval"
let $echo="yes"
typetext bout1c.txt

!
!  HALF IMAGES
!
! TEST 6 - HALF - minimal parameters
!
img2ascii half2.img hout2.txt
typetext hout2.txt
!
! TEST 7 - HALF with SIZE parameter
!
img2ascii half1.img hout1.txt size=(1,1,20,20)
typetext hout1.txt
!
! TEST 8 - HALF with ORG=COLUMNS, no index
!
img2ascii half1.img hout1a.txt size=(1,1,20,20) org=COLUMNS
typetext hout1a.txt
!
! TEST 9 - HALF with ORG=COLUMNS, INDEX=yes
!
img2ascii half1.img hout1b.txt size=(1,1,20,20) org=COLUMNS +
    index=yes
typetext hout1b.txt
!
! TEST 10 - HALF with ORG=COLUMNS, INDEX=yes NOTES=on
!       with return values
!
img2ascii half1.img hout1c.txt size=(1,1,20,20) org=COLUMNS +
    index=yes notes=on minval=minval maxval=maxval
let $echo="no"
let iminval = $fix(minval)
let imaxval = $fix(maxval)
write "maximum value = &imaxval  minimum value = &iminval"
let $echo="yes"
typetext hout1c.txt
!
! FULL images
!
! TEST 11 - FULL - minimal parameters
!
img2ascii full2.img fout2.txt
typetext fout2.txt
!
! TEST 12 - FULL with SIZE parameter
!
img2ascii full1.img fout1.txt size=(1,1,20,20)
typetext fout1.txt
!
! TEST 13 - HALF with ORG=COLUMNS, no index
!
img2ascii full1.img fout1a.txt size=(1,1,20,20) org=COLUMNS
typetext fout1a.txt
!
! TEST 14 - HALF with ORG=COLUMNS, INDEX=yes
!
img2ascii full1.img fout1b.txt size=(1,1,20,20) org=COLUMNS +
    index=yes
typetext fout1b.txt
!
! TEST 15 - HALF with ORG=COLUMNS, INDEX=yes NOTES=on
!       with return values
!
img2ascii full1.img fout1c.txt size=(1,1,20,20) org=COLUMNS +
    index=yes notes=on minval=minval maxval=maxval
let $echo="no"
let iminval = $fix(minval)
let imaxval = $fix(maxval)
write "maximum value = &imaxval  minimum value = &iminval"
let $echo="yes"
typetext fout1c.txt
!
! REAL images
!
! TEST 16 - REAL - minimal parameters
!
img2ascii real2.img rout2.txt
typetext rout2.txt
!
! TEST 17 - REAL with SIZE parameter
!
img2ascii real1.img rout1.txt size=(1,1,20,20)
typetext rout1.txt
!
! TEST 18 - REAL with ORG=COLUMNS, no index
!
img2ascii real1.img rout1a.txt size=(1,1,20,20) org=COLUMNS
typetext rout1a.txt
!
! TEST 19 - REAL with ORG=COLUMNS, INDEX=yes
!
img2ascii real1.img rout1b.txt size=(1,1,20,20) org=COLUMNS +
    index=yes
typetext rout1b.txt
!
! TEST 20 - REAL with ORG=COLUMNS, INDEX=yes NOTES=on
!       with return values
!
img2ascii real1.img rout1c.txt size=(1,1,20,20) org=COLUMNS +
    index=yes notes=on minval=minval maxval=maxval
let $echo="no"
write "************************************************"
write "maximum value = &maxval  minimum value = &minval"
write "************************************************"
let $echo="yes"
typetext rout1c.txt
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstimg2ascii.log
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

gen byte1.img 100 100
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen byte2.img 50 50
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half1.img 100 100 ival=300 format=HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen half2.img  50  50 ival=300 format=HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full1.img 100 100 ival=36000 format=FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen full2.img  50  50 ival=36000 format=FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real1.img 100 100 ival=3000. format=REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen real2.img  50  50 ival=3000. format=REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
img2ascii byte2.img bout2.txt
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext bout2.txt
Beginning VICAR task typetext
      0     1     2     3     4     5     6     7     8     9    10    11
     12    13    14    15    16    17    18    19    20    21    22    23
     24    25    26    27    28    29    30    31    32    33    34    35
     36    37
      1     2     3     4     5     6     7     8     9    10    11    12
     13    14    15    16    17    18    19    20    21    22    23    24
     25    26    27    28    29    30    31    32    33    34    35    36
     37    38
      2     3     4     5     6     7     8     9    10    11    12    13
     14    15    16    17    18    19    20    21    22    23    24    25
     26    27    28    29    30    31    32    33    34    35    36    37
     38    39
      3     4     5     6     7     8     9    10    11    12    13    14
     15    16    17    18    19    20    21    22    23    24    25    26
     27    28    29    30    31    32    33    34    35    36    37    38
     39    40
      4     5     6     7     8     9    10    11    12    13    14    15
     16    17    18    19    20    21    22    23    24    25    26    27
     28    29    30    31    32    33    34    35    36    37    38    39
     40    41
      5     6     7     8     9    10    11    12    13    14    15    16
     17    18    19    20    21    22    23    24    25    26    27    28
     29    30    31    32    33    34    35    36    37    38    39    40
     41    42
      6     7     8     9    10    11    12    13    14    15    16    17
     18    19    20    21    22    23    24    25    26    27    28    29
     30    31    32    33    34    35    36    37    38    39    40    41
     42    43
      7     8     9    10    11    12    13    14    15    16    17    18
     19    20    21    22    23    24    25    26    27    28    29    30
     31    32    33    34    35    36    37    38    39    40    41    42
     43    44
      8     9    10    11    12    13    14    15    16    17    18    19
     20    21    22    23    24    25    26    27    28    29    30    31
     32    33    34    35    36    37    38    39    40    41    42    43
     44    45
      9    10    11    12    13    14    15    16    17    18    19    20
     21    22    23    24    25    26    27    28    29    30    31    32
     33    34    35    36    37    38    39    40    41    42    43    44
     45    46
     10    11    12    13    14    15    16    17    18    19    20    21
     22    23    24    25    26    27    28    29    30    31    32    33
     34    35    36    37    38    39    40    41    42    43    44    45
     46    47
     11    12    13    14    15    16    17    18    19    20    21    22
     23    24    25    26    27    28    29    30    31    32    33    34
     35    36    37    38    39    40    41    42    43    44    45    46
     47    48
     12    13    14    15    16    17    18    19    20    21    22    23
     24    25    26    27    28    29    30    31    32    33    34    35
     36    37    38    39    40    41    42    43    44    45    46    47
     48    49
     13    14    15    16    17    18    19    20    21    22    23    24
     25    26    27    28    29    30    31    32    33    34    35    36
     37    38    39    40    41    42    43    44    45    46    47    48
     49    50
     14    15    16    17    18    19    20    21    22    23    24    25
     26    27    28    29    30    31    32    33    34    35    36    37
     38    39    40    41    42    43    44    45    46    47    48    49
     50    51
     15    16    17    18    19    20    21    22    23    24    25    26
     27    28    29    30    31    32    33    34    35    36    37    38
     39    40    41    42    43    44    45    46    47    48    49    50
     51    52
     16    17    18    19    20    21    22    23    24    25    26    27
     28    29    30    31    32    33    34    35    36    37    38    39
     40    41    42    43    44    45    46    47    48    49    50    51
     52    53
     17    18    19    20    21    22    23    24    25    26    27    28
     29    30    31    32    33    34    35    36    37    38    39    40
     41    42    43    44    45    46    47    48    49    50    51    52
     53    54
     18    19    20    21    22    23    24    25    26    27    28    29
     30    31    32    33    34    35    36    37    38    39    40    41
     42    43    44    45    46    47    48    49    50    51    52    53
     54    55
     19    20    21    22    23    24    25    26    27    28    29    30
     31    32    33    34    35    36    37    38    39    40    41    42
     43    44    45    46    47    48    49    50    51    52    53    54
     55    56
     20    21    22    23    24    25    26    27    28    29    30    31
     32    33    34    35    36    37    38    39    40    41    42    43
     44    45    46    47    48    49    50    51    52    53    54    55
     56    57
     21    22    23    24    25    26    27    28    29    30    31    32
     33    34    35    36    37    38    39    40    41    42    43    44
     45    46    47    48    49    50    51    52    53    54    55    56
     57    58
     22    23    24    25    26    27    28    29    30    31    32    33
     34    35    36    37    38    39    40    41    42    43    44    45
     46    47    48    49    50    51    52    53    54    55    56    57
     58    59
     23    24    25    26    27    28    29    30    31    32    33    34
     35    36    37    38    39    40    41    42    43    44    45    46
     47    48    49    50    51    52    53    54    55    56    57    58
     59    60
     24    25    26    27    28    29    30    31    32    33    34    35
     36    37    38    39    40    41    42    43    44    45    46    47
     48    49    50    51    52    53    54    55    56    57    58    59
     60    61
     25    26    27    28    29    30    31    32    33    34    35    36
     37    38    39    40    41    42    43    44    45    46    47    48
     49    50    51    52    53    54    55    56    57    58    59    60
     61    62
     26    27    28    29    30    31    32    33    34    35    36    37
     38    39    40    41    42    43    44    45    46    47    48    49
     50    51    52    53    54    55    56    57    58    59    60    61
     62    63
     27    28    29    30    31    32    33    34    35    36    37    38
     39    40    41    42    43    44    45    46    47    48    49    50
     51    52    53    54    55    56    57    58    59    60    61    62
     63    64
     28    29    30    31    32    33    34    35    36    37    38    39
     40    41    42    43    44    45    46    47    48    49    50    51
     52    53    54    55    56    57    58    59    60    61    62    63
     64    65
     29    30    31    32    33    34    35    36    37    38    39    40
     41    42    43    44    45    46    47    48    49    50    51    52
     53    54    55    56    57    58    59    60    61    62    63    64
     65    66
     30    31    32    33    34    35    36    37    38    39    40    41
     42    43    44    45    46    47    48    49    50    51    52    53
     54    55    56    57    58    59    60    61    62    63    64    65
     66    67
     31    32    33    34    35    36    37    38    39    40    41    42
     43    44    45    46    47    48    49    50    51    52    53    54
     55    56    57    58    59    60    61    62    63    64    65    66
     67    68
     32    33    34    35    36    37    38    39    40    41    42    43
     44    45    46    47    48    49    50    51    52    53    54    55
     56    57    58    59    60    61    62    63    64    65    66    67
     68    69
     33    34    35    36    37    38    39    40    41    42    43    44
     45    46    47    48    49    50    51    52    53    54    55    56
     57    58    59    60    61    62    63    64    65    66    67    68
     69    70
     34    35    36    37    38    39    40    41    42    43    44    45
     46    47    48    49    50    51    52    53    54    55    56    57
     58    59    60    61    62    63    64    65    66    67    68    69
     70    71
     35    36    37    38    39    40    41    42    43    44    45    46
     47    48    49    50    51    52    53    54    55    56    57    58
     59    60    61    62    63    64    65    66    67    68    69    70
     71    72
     36    37    38    39    40    41    42    43    44    45    46    47
     48    49    50    51    52    53    54    55    56    57    58    59
     60    61    62    63    64    65    66    67    68    69    70    71
     72    73
     37    38    39    40    41    42    43    44    45    46    47    48
     49    50    51    52    53    54    55    56    57    58    59    60
     61    62    63    64    65    66    67    68    69    70    71    72
     73    74
     38    39    40    41    42    43    44    45    46    47    48    49
     50    51    52    53    54    55    56    57    58    59    60    61
     62    63    64    65    66    67    68    69    70    71    72    73
     74    75
     39    40    41    42    43    44    45    46    47    48    49    50
     51    52    53    54    55    56    57    58    59    60    61    62
     63    64    65    66    67    68    69    70    71    72    73    74
     75    76
     40    41    42    43    44    45    46    47    48    49    50    51
     52    53    54    55    56    57    58    59    60    61    62    63
     64    65    66    67    68    69    70    71    72    73    74    75
     76    77
     41    42    43    44    45    46    47    48    49    50    51    52
     53    54    55    56    57    58    59    60    61    62    63    64
     65    66    67    68    69    70    71    72    73    74    75    76
     77    78
     42    43    44    45    46    47    48    49    50    51    52    53
     54    55    56    57    58    59    60    61    62    63    64    65
     66    67    68    69    70    71    72    73    74    75    76    77
     78    79
     43    44    45    46    47    48    49    50    51    52    53    54
     55    56    57    58    59    60    61    62    63    64    65    66
     67    68    69    70    71    72    73    74    75    76    77    78
     79    80
     44    45    46    47    48    49    50    51    52    53    54    55
     56    57    58    59    60    61    62    63    64    65    66    67
     68    69    70    71    72    73    74    75    76    77    78    79
     80    81
     45    46    47    48    49    50    51    52    53    54    55    56
     57    58    59    60    61    62    63    64    65    66    67    68
     69    70    71    72    73    74    75    76    77    78    79    80
     81    82
     46    47    48    49    50    51    52    53    54    55    56    57
     58    59    60    61    62    63    64    65    66    67    68    69
     70    71    72    73    74    75    76    77    78    79    80    81
     82    83
     47    48    49    50    51    52    53    54    55    56    57    58
     59    60    61    62    63    64    65    66    67    68    69    70
     71    72    73    74    75    76    77    78    79    80    81    82
     83    84
     48    49    50    51    52    53    54    55    56    57    58    59
     60    61    62    63    64    65    66    67    68    69    70    71
     72    73    74    75    76    77    78    79    80    81    82    83
     84    85
     49    50    51    52    53    54    55    56    57    58    59    60
     61    62    63    64    65    66    67    68    69    70    71    72
     73    74    75    76    77    78    79    80    81    82    83    84
     85    86
img2ascii byte1.img bout1.txt size=(1,1,20,20)
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext bout1.txt
Beginning VICAR task typetext
      0     1     2     3     4     5     6     7
      1     2     3     4     5     6     7     8
      2     3     4     5     6     7     8     9
      3     4     5     6     7     8     9    10
      4     5     6     7     8     9    10    11
      5     6     7     8     9    10    11    12
      6     7     8     9    10    11    12    13
      7     8     9    10    11    12    13    14
      8     9    10    11    12    13    14    15
      9    10    11    12    13    14    15    16
     10    11    12    13    14    15    16    17
     11    12    13    14    15    16    17    18
     12    13    14    15    16    17    18    19
     13    14    15    16    17    18    19    20
     14    15    16    17    18    19    20    21
     15    16    17    18    19    20    21    22
     16    17    18    19    20    21    22    23
     17    18    19    20    21    22    23    24
     18    19    20    21    22    23    24    25
     19    20    21    22    23    24    25    26
img2ascii byte1.img bout1a.txt size=(1,1,20,20) org=COLUMNS
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext bout1a.txt
Beginning VICAR task typetext
      1       0
      2       1
      3       2
      4       3
      5       4
      6       5
      7       6
      8       7
      9       8
     10       9
     11      10
     12      11
     13      12
     14      13
     15      14
     16      15
     17      16
     18      17
     19      18
     20      19
     21       1
     22       2
     23       3
     24       4
     25       5
     26       6
     27       7
     28       8
     29       9
     30      10
     31      11
     32      12
     33      13
     34      14
     35      15
     36      16
     37      17
     38      18
     39      19
     40      20
     41       2
     42       3
     43       4
     44       5
     45       6
     46       7
     47       8
     48       9
     49      10
     50      11
     51      12
     52      13
     53      14
     54      15
     55      16
     56      17
     57      18
     58      19
     59      20
     60      21
     61       3
     62       4
     63       5
     64       6
     65       7
     66       8
     67       9
     68      10
     69      11
     70      12
     71      13
     72      14
     73      15
     74      16
     75      17
     76      18
     77      19
     78      20
     79      21
     80      22
     81       4
     82       5
     83       6
     84       7
     85       8
     86       9
     87      10
     88      11
     89      12
     90      13
     91      14
     92      15
     93      16
     94      17
     95      18
     96      19
     97      20
     98      21
     99      22
    100      23
    101       5
    102       6
    103       7
    104       8
    105       9
    106      10
    107      11
    108      12
    109      13
    110      14
    111      15
    112      16
    113      17
    114      18
    115      19
    116      20
    117      21
    118      22
    119      23
    120      24
    121       6
    122       7
    123       8
    124       9
    125      10
    126      11
    127      12
    128      13
    129      14
    130      15
    131      16
    132      17
    133      18
    134      19
    135      20
    136      21
    137      22
    138      23
    139      24
    140      25
    141       7
    142       8
    143       9
    144      10
    145      11
    146      12
    147      13
    148      14
    149      15
    150      16
    151      17
    152      18
    153      19
    154      20
    155      21
    156      22
    157      23
    158      24
    159      25
    160      26
    161       8
    162       9
    163      10
    164      11
    165      12
    166      13
    167      14
    168      15
    169      16
    170      17
    171      18
    172      19
    173      20
    174      21
    175      22
    176      23
    177      24
    178      25
    179      26
    180      27
    181       9
    182      10
    183      11
    184      12
    185      13
    186      14
    187      15
    188      16
    189      17
    190      18
    191      19
    192      20
    193      21
    194      22
    195      23
    196      24
    197      25
    198      26
    199      27
    200      28
    201      10
    202      11
    203      12
    204      13
    205      14
    206      15
    207      16
    208      17
    209      18
    210      19
    211      20
    212      21
    213      22
    214      23
    215      24
    216      25
    217      26
    218      27
    219      28
    220      29
    221      11
    222      12
    223      13
    224      14
    225      15
    226      16
    227      17
    228      18
    229      19
    230      20
    231      21
    232      22
    233      23
    234      24
    235      25
    236      26
    237      27
    238      28
    239      29
    240      30
    241      12
    242      13
    243      14
    244      15
    245      16
    246      17
    247      18
    248      19
    249      20
    250      21
    251      22
    252      23
    253      24
    254      25
    255      26
    256      27
    257      28
    258      29
    259      30
    260      31
    261      13
    262      14
    263      15
    264      16
    265      17
    266      18
    267      19
    268      20
    269      21
    270      22
    271      23
    272      24
    273      25
    274      26
    275      27
    276      28
    277      29
    278      30
    279      31
    280      32
    281      14
    282      15
    283      16
    284      17
    285      18
    286      19
    287      20
    288      21
    289      22
    290      23
    291      24
    292      25
    293      26
    294      27
    295      28
    296      29
    297      30
    298      31
    299      32
    300      33
    301      15
    302      16
    303      17
    304      18
    305      19
    306      20
    307      21
    308      22
    309      23
    310      24
    311      25
    312      26
    313      27
    314      28
    315      29
    316      30
    317      31
    318      32
    319      33
    320      34
    321      16
    322      17
    323      18
    324      19
    325      20
    326      21
    327      22
    328      23
    329      24
    330      25
    331      26
    332      27
    333      28
    334      29
    335      30
    336      31
    337      32
    338      33
    339      34
    340      35
    341      17
    342      18
    343      19
    344      20
    345      21
    346      22
    347      23
    348      24
    349      25
    350      26
    351      27
    352      28
    353      29
    354      30
    355      31
    356      32
    357      33
    358      34
    359      35
    360      36
    361      18
    362      19
    363      20
    364      21
    365      22
    366      23
    367      24
    368      25
    369      26
    370      27
    371      28
    372      29
    373      30
    374      31
    375      32
    376      33
    377      34
    378      35
    379      36
    380      37
    381      19
    382      20
    383      21
    384      22
    385      23
    386      24
    387      25
    388      26
    389      27
    390      28
    391      29
    392      30
    393      31
    394      32
    395      33
    396      34
    397      35
    398      36
    399      37
    400      38
img2ascii byte1.img bout1b.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext bout1b.txt
Beginning VICAR task typetext
      1       0
      2       1
      3       2
      4       3
      5       4
      6       5
      7       6
      8       7
      9       8
     10       9
     11      10
     12      11
     13      12
     14      13
     15      14
     16      15
     17      16
     18      17
     19      18
     20      19
     21       1
     22       2
     23       3
     24       4
     25       5
     26       6
     27       7
     28       8
     29       9
     30      10
     31      11
     32      12
     33      13
     34      14
     35      15
     36      16
     37      17
     38      18
     39      19
     40      20
     41       2
     42       3
     43       4
     44       5
     45       6
     46       7
     47       8
     48       9
     49      10
     50      11
     51      12
     52      13
     53      14
     54      15
     55      16
     56      17
     57      18
     58      19
     59      20
     60      21
     61       3
     62       4
     63       5
     64       6
     65       7
     66       8
     67       9
     68      10
     69      11
     70      12
     71      13
     72      14
     73      15
     74      16
     75      17
     76      18
     77      19
     78      20
     79      21
     80      22
     81       4
     82       5
     83       6
     84       7
     85       8
     86       9
     87      10
     88      11
     89      12
     90      13
     91      14
     92      15
     93      16
     94      17
     95      18
     96      19
     97      20
     98      21
     99      22
    100      23
    101       5
    102       6
    103       7
    104       8
    105       9
    106      10
    107      11
    108      12
    109      13
    110      14
    111      15
    112      16
    113      17
    114      18
    115      19
    116      20
    117      21
    118      22
    119      23
    120      24
    121       6
    122       7
    123       8
    124       9
    125      10
    126      11
    127      12
    128      13
    129      14
    130      15
    131      16
    132      17
    133      18
    134      19
    135      20
    136      21
    137      22
    138      23
    139      24
    140      25
    141       7
    142       8
    143       9
    144      10
    145      11
    146      12
    147      13
    148      14
    149      15
    150      16
    151      17
    152      18
    153      19
    154      20
    155      21
    156      22
    157      23
    158      24
    159      25
    160      26
    161       8
    162       9
    163      10
    164      11
    165      12
    166      13
    167      14
    168      15
    169      16
    170      17
    171      18
    172      19
    173      20
    174      21
    175      22
    176      23
    177      24
    178      25
    179      26
    180      27
    181       9
    182      10
    183      11
    184      12
    185      13
    186      14
    187      15
    188      16
    189      17
    190      18
    191      19
    192      20
    193      21
    194      22
    195      23
    196      24
    197      25
    198      26
    199      27
    200      28
    201      10
    202      11
    203      12
    204      13
    205      14
    206      15
    207      16
    208      17
    209      18
    210      19
    211      20
    212      21
    213      22
    214      23
    215      24
    216      25
    217      26
    218      27
    219      28
    220      29
    221      11
    222      12
    223      13
    224      14
    225      15
    226      16
    227      17
    228      18
    229      19
    230      20
    231      21
    232      22
    233      23
    234      24
    235      25
    236      26
    237      27
    238      28
    239      29
    240      30
    241      12
    242      13
    243      14
    244      15
    245      16
    246      17
    247      18
    248      19
    249      20
    250      21
    251      22
    252      23
    253      24
    254      25
    255      26
    256      27
    257      28
    258      29
    259      30
    260      31
    261      13
    262      14
    263      15
    264      16
    265      17
    266      18
    267      19
    268      20
    269      21
    270      22
    271      23
    272      24
    273      25
    274      26
    275      27
    276      28
    277      29
    278      30
    279      31
    280      32
    281      14
    282      15
    283      16
    284      17
    285      18
    286      19
    287      20
    288      21
    289      22
    290      23
    291      24
    292      25
    293      26
    294      27
    295      28
    296      29
    297      30
    298      31
    299      32
    300      33
    301      15
    302      16
    303      17
    304      18
    305      19
    306      20
    307      21
    308      22
    309      23
    310      24
    311      25
    312      26
    313      27
    314      28
    315      29
    316      30
    317      31
    318      32
    319      33
    320      34
    321      16
    322      17
    323      18
    324      19
    325      20
    326      21
    327      22
    328      23
    329      24
    330      25
    331      26
    332      27
    333      28
    334      29
    335      30
    336      31
    337      32
    338      33
    339      34
    340      35
    341      17
    342      18
    343      19
    344      20
    345      21
    346      22
    347      23
    348      24
    349      25
    350      26
    351      27
    352      28
    353      29
    354      30
    355      31
    356      32
    357      33
    358      34
    359      35
    360      36
    361      18
    362      19
    363      20
    364      21
    365      22
    366      23
    367      24
    368      25
    369      26
    370      27
    371      28
    372      29
    373      30
    374      31
    375      32
    376      33
    377      34
    378      35
    379      36
    380      37
    381      19
    382      20
    383      21
    384      22
    385      23
    386      24
    387      25
    388      26
    389      27
    390      28
    391      29
    392      30
    393      31
    394      32
    395      33
    396      34
    397      35
    398      36
    399      37
    400      38
img2ascii byte1.img bout1c.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes notes=on minval=minval maxval=maxval
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
Minimum value:      0 Maximum value:     38
The output text file is dimensioned   20 by      2
let $echo="no"
maximum value = 38  minimum value = 0
typetext bout1c.txt
Beginning VICAR task typetext
      1       0
      2       1
      3       2
      4       3
      5       4
      6       5
      7       6
      8       7
      9       8
     10       9
     11      10
     12      11
     13      12
     14      13
     15      14
     16      15
     17      16
     18      17
     19      18
     20      19
     21       1
     22       2
     23       3
     24       4
     25       5
     26       6
     27       7
     28       8
     29       9
     30      10
     31      11
     32      12
     33      13
     34      14
     35      15
     36      16
     37      17
     38      18
     39      19
     40      20
     41       2
     42       3
     43       4
     44       5
     45       6
     46       7
     47       8
     48       9
     49      10
     50      11
     51      12
     52      13
     53      14
     54      15
     55      16
     56      17
     57      18
     58      19
     59      20
     60      21
     61       3
     62       4
     63       5
     64       6
     65       7
     66       8
     67       9
     68      10
     69      11
     70      12
     71      13
     72      14
     73      15
     74      16
     75      17
     76      18
     77      19
     78      20
     79      21
     80      22
     81       4
     82       5
     83       6
     84       7
     85       8
     86       9
     87      10
     88      11
     89      12
     90      13
     91      14
     92      15
     93      16
     94      17
     95      18
     96      19
     97      20
     98      21
     99      22
    100      23
    101       5
    102       6
    103       7
    104       8
    105       9
    106      10
    107      11
    108      12
    109      13
    110      14
    111      15
    112      16
    113      17
    114      18
    115      19
    116      20
    117      21
    118      22
    119      23
    120      24
    121       6
    122       7
    123       8
    124       9
    125      10
    126      11
    127      12
    128      13
    129      14
    130      15
    131      16
    132      17
    133      18
    134      19
    135      20
    136      21
    137      22
    138      23
    139      24
    140      25
    141       7
    142       8
    143       9
    144      10
    145      11
    146      12
    147      13
    148      14
    149      15
    150      16
    151      17
    152      18
    153      19
    154      20
    155      21
    156      22
    157      23
    158      24
    159      25
    160      26
    161       8
    162       9
    163      10
    164      11
    165      12
    166      13
    167      14
    168      15
    169      16
    170      17
    171      18
    172      19
    173      20
    174      21
    175      22
    176      23
    177      24
    178      25
    179      26
    180      27
    181       9
    182      10
    183      11
    184      12
    185      13
    186      14
    187      15
    188      16
    189      17
    190      18
    191      19
    192      20
    193      21
    194      22
    195      23
    196      24
    197      25
    198      26
    199      27
    200      28
    201      10
    202      11
    203      12
    204      13
    205      14
    206      15
    207      16
    208      17
    209      18
    210      19
    211      20
    212      21
    213      22
    214      23
    215      24
    216      25
    217      26
    218      27
    219      28
    220      29
    221      11
    222      12
    223      13
    224      14
    225      15
    226      16
    227      17
    228      18
    229      19
    230      20
    231      21
    232      22
    233      23
    234      24
    235      25
    236      26
    237      27
    238      28
    239      29
    240      30
    241      12
    242      13
    243      14
    244      15
    245      16
    246      17
    247      18
    248      19
    249      20
    250      21
    251      22
    252      23
    253      24
    254      25
    255      26
    256      27
    257      28
    258      29
    259      30
    260      31
    261      13
    262      14
    263      15
    264      16
    265      17
    266      18
    267      19
    268      20
    269      21
    270      22
    271      23
    272      24
    273      25
    274      26
    275      27
    276      28
    277      29
    278      30
    279      31
    280      32
    281      14
    282      15
    283      16
    284      17
    285      18
    286      19
    287      20
    288      21
    289      22
    290      23
    291      24
    292      25
    293      26
    294      27
    295      28
    296      29
    297      30
    298      31
    299      32
    300      33
    301      15
    302      16
    303      17
    304      18
    305      19
    306      20
    307      21
    308      22
    309      23
    310      24
    311      25
    312      26
    313      27
    314      28
    315      29
    316      30
    317      31
    318      32
    319      33
    320      34
    321      16
    322      17
    323      18
    324      19
    325      20
    326      21
    327      22
    328      23
    329      24
    330      25
    331      26
    332      27
    333      28
    334      29
    335      30
    336      31
    337      32
    338      33
    339      34
    340      35
    341      17
    342      18
    343      19
    344      20
    345      21
    346      22
    347      23
    348      24
    349      25
    350      26
    351      27
    352      28
    353      29
    354      30
    355      31
    356      32
    357      33
    358      34
    359      35
    360      36
    361      18
    362      19
    363      20
    364      21
    365      22
    366      23
    367      24
    368      25
    369      26
    370      27
    371      28
    372      29
    373      30
    374      31
    375      32
    376      33
    377      34
    378      35
    379      36
    380      37
    381      19
    382      20
    383      21
    384      22
    385      23
    386      24
    387      25
    388      26
    389      27
    390      28
    391      29
    392      30
    393      31
    394      32
    395      33
    396      34
    397      35
    398      36
    399      37
    400      38
img2ascii half2.img hout2.txt
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext hout2.txt
Beginning VICAR task typetext
    300   301   302   303   304   305   306   307   308   309   310   311
    312   313   314   315   316   317   318   319   320   321   322   323
    324   325   326   327   328   329   330   331   332   333   334   335
    336   337
    301   302   303   304   305   306   307   308   309   310   311   312
    313   314   315   316   317   318   319   320   321   322   323   324
    325   326   327   328   329   330   331   332   333   334   335   336
    337   338
    302   303   304   305   306   307   308   309   310   311   312   313
    314   315   316   317   318   319   320   321   322   323   324   325
    326   327   328   329   330   331   332   333   334   335   336   337
    338   339
    303   304   305   306   307   308   309   310   311   312   313   314
    315   316   317   318   319   320   321   322   323   324   325   326
    327   328   329   330   331   332   333   334   335   336   337   338
    339   340
    304   305   306   307   308   309   310   311   312   313   314   315
    316   317   318   319   320   321   322   323   324   325   326   327
    328   329   330   331   332   333   334   335   336   337   338   339
    340   341
    305   306   307   308   309   310   311   312   313   314   315   316
    317   318   319   320   321   322   323   324   325   326   327   328
    329   330   331   332   333   334   335   336   337   338   339   340
    341   342
    306   307   308   309   310   311   312   313   314   315   316   317
    318   319   320   321   322   323   324   325   326   327   328   329
    330   331   332   333   334   335   336   337   338   339   340   341
    342   343
    307   308   309   310   311   312   313   314   315   316   317   318
    319   320   321   322   323   324   325   326   327   328   329   330
    331   332   333   334   335   336   337   338   339   340   341   342
    343   344
    308   309   310   311   312   313   314   315   316   317   318   319
    320   321   322   323   324   325   326   327   328   329   330   331
    332   333   334   335   336   337   338   339   340   341   342   343
    344   345
    309   310   311   312   313   314   315   316   317   318   319   320
    321   322   323   324   325   326   327   328   329   330   331   332
    333   334   335   336   337   338   339   340   341   342   343   344
    345   346
    310   311   312   313   314   315   316   317   318   319   320   321
    322   323   324   325   326   327   328   329   330   331   332   333
    334   335   336   337   338   339   340   341   342   343   344   345
    346   347
    311   312   313   314   315   316   317   318   319   320   321   322
    323   324   325   326   327   328   329   330   331   332   333   334
    335   336   337   338   339   340   341   342   343   344   345   346
    347   348
    312   313   314   315   316   317   318   319   320   321   322   323
    324   325   326   327   328   329   330   331   332   333   334   335
    336   337   338   339   340   341   342   343   344   345   346   347
    348   349
    313   314   315   316   317   318   319   320   321   322   323   324
    325   326   327   328   329   330   331   332   333   334   335   336
    337   338   339   340   341   342   343   344   345   346   347   348
    349   350
    314   315   316   317   318   319   320   321   322   323   324   325
    326   327   328   329   330   331   332   333   334   335   336   337
    338   339   340   341   342   343   344   345   346   347   348   349
    350   351
    315   316   317   318   319   320   321   322   323   324   325   326
    327   328   329   330   331   332   333   334   335   336   337   338
    339   340   341   342   343   344   345   346   347   348   349   350
    351   352
    316   317   318   319   320   321   322   323   324   325   326   327
    328   329   330   331   332   333   334   335   336   337   338   339
    340   341   342   343   344   345   346   347   348   349   350   351
    352   353
    317   318   319   320   321   322   323   324   325   326   327   328
    329   330   331   332   333   334   335   336   337   338   339   340
    341   342   343   344   345   346   347   348   349   350   351   352
    353   354
    318   319   320   321   322   323   324   325   326   327   328   329
    330   331   332   333   334   335   336   337   338   339   340   341
    342   343   344   345   346   347   348   349   350   351   352   353
    354   355
    319   320   321   322   323   324   325   326   327   328   329   330
    331   332   333   334   335   336   337   338   339   340   341   342
    343   344   345   346   347   348   349   350   351   352   353   354
    355   356
    320   321   322   323   324   325   326   327   328   329   330   331
    332   333   334   335   336   337   338   339   340   341   342   343
    344   345   346   347   348   349   350   351   352   353   354   355
    356   357
    321   322   323   324   325   326   327   328   329   330   331   332
    333   334   335   336   337   338   339   340   341   342   343   344
    345   346   347   348   349   350   351   352   353   354   355   356
    357   358
    322   323   324   325   326   327   328   329   330   331   332   333
    334   335   336   337   338   339   340   341   342   343   344   345
    346   347   348   349   350   351   352   353   354   355   356   357
    358   359
    323   324   325   326   327   328   329   330   331   332   333   334
    335   336   337   338   339   340   341   342   343   344   345   346
    347   348   349   350   351   352   353   354   355   356   357   358
    359   360
    324   325   326   327   328   329   330   331   332   333   334   335
    336   337   338   339   340   341   342   343   344   345   346   347
    348   349   350   351   352   353   354   355   356   357   358   359
    360   361
    325   326   327   328   329   330   331   332   333   334   335   336
    337   338   339   340   341   342   343   344   345   346   347   348
    349   350   351   352   353   354   355   356   357   358   359   360
    361   362
    326   327   328   329   330   331   332   333   334   335   336   337
    338   339   340   341   342   343   344   345   346   347   348   349
    350   351   352   353   354   355   356   357   358   359   360   361
    362   363
    327   328   329   330   331   332   333   334   335   336   337   338
    339   340   341   342   343   344   345   346   347   348   349   350
    351   352   353   354   355   356   357   358   359   360   361   362
    363   364
    328   329   330   331   332   333   334   335   336   337   338   339
    340   341   342   343   344   345   346   347   348   349   350   351
    352   353   354   355   356   357   358   359   360   361   362   363
    364   365
    329   330   331   332   333   334   335   336   337   338   339   340
    341   342   343   344   345   346   347   348   349   350   351   352
    353   354   355   356   357   358   359   360   361   362   363   364
    365   366
    330   331   332   333   334   335   336   337   338   339   340   341
    342   343   344   345   346   347   348   349   350   351   352   353
    354   355   356   357   358   359   360   361   362   363   364   365
    366   367
    331   332   333   334   335   336   337   338   339   340   341   342
    343   344   345   346   347   348   349   350   351   352   353   354
    355   356   357   358   359   360   361   362   363   364   365   366
    367   368
    332   333   334   335   336   337   338   339   340   341   342   343
    344   345   346   347   348   349   350   351   352   353   354   355
    356   357   358   359   360   361   362   363   364   365   366   367
    368   369
    333   334   335   336   337   338   339   340   341   342   343   344
    345   346   347   348   349   350   351   352   353   354   355   356
    357   358   359   360   361   362   363   364   365   366   367   368
    369   370
    334   335   336   337   338   339   340   341   342   343   344   345
    346   347   348   349   350   351   352   353   354   355   356   357
    358   359   360   361   362   363   364   365   366   367   368   369
    370   371
    335   336   337   338   339   340   341   342   343   344   345   346
    347   348   349   350   351   352   353   354   355   356   357   358
    359   360   361   362   363   364   365   366   367   368   369   370
    371   372
    336   337   338   339   340   341   342   343   344   345   346   347
    348   349   350   351   352   353   354   355   356   357   358   359
    360   361   362   363   364   365   366   367   368   369   370   371
    372   373
    337   338   339   340   341   342   343   344   345   346   347   348
    349   350   351   352   353   354   355   356   357   358   359   360
    361   362   363   364   365   366   367   368   369   370   371   372
    373   374
    338   339   340   341   342   343   344   345   346   347   348   349
    350   351   352   353   354   355   356   357   358   359   360   361
    362   363   364   365   366   367   368   369   370   371   372   373
    374   375
    339   340   341   342   343   344   345   346   347   348   349   350
    351   352   353   354   355   356   357   358   359   360   361   362
    363   364   365   366   367   368   369   370   371   372   373   374
    375   376
    340   341   342   343   344   345   346   347   348   349   350   351
    352   353   354   355   356   357   358   359   360   361   362   363
    364   365   366   367   368   369   370   371   372   373   374   375
    376   377
    341   342   343   344   345   346   347   348   349   350   351   352
    353   354   355   356   357   358   359   360   361   362   363   364
    365   366   367   368   369   370   371   372   373   374   375   376
    377   378
    342   343   344   345   346   347   348   349   350   351   352   353
    354   355   356   357   358   359   360   361   362   363   364   365
    366   367   368   369   370   371   372   373   374   375   376   377
    378   379
    343   344   345   346   347   348   349   350   351   352   353   354
    355   356   357   358   359   360   361   362   363   364   365   366
    367   368   369   370   371   372   373   374   375   376   377   378
    379   380
    344   345   346   347   348   349   350   351   352   353   354   355
    356   357   358   359   360   361   362   363   364   365   366   367
    368   369   370   371   372   373   374   375   376   377   378   379
    380   381
    345   346   347   348   349   350   351   352   353   354   355   356
    357   358   359   360   361   362   363   364   365   366   367   368
    369   370   371   372   373   374   375   376   377   378   379   380
    381   382
    346   347   348   349   350   351   352   353   354   355   356   357
    358   359   360   361   362   363   364   365   366   367   368   369
    370   371   372   373   374   375   376   377   378   379   380   381
    382   383
    347   348   349   350   351   352   353   354   355   356   357   358
    359   360   361   362   363   364   365   366   367   368   369   370
    371   372   373   374   375   376   377   378   379   380   381   382
    383   384
    348   349   350   351   352   353   354   355   356   357   358   359
    360   361   362   363   364   365   366   367   368   369   370   371
    372   373   374   375   376   377   378   379   380   381   382   383
    384   385
    349   350   351   352   353   354   355   356   357   358   359   360
    361   362   363   364   365   366   367   368   369   370   371   372
    373   374   375   376   377   378   379   380   381   382   383   384
    385   386
img2ascii half1.img hout1.txt size=(1,1,20,20)
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext hout1.txt
Beginning VICAR task typetext
    300   301   302   303   304   305   306   307
    301   302   303   304   305   306   307   308
    302   303   304   305   306   307   308   309
    303   304   305   306   307   308   309   310
    304   305   306   307   308   309   310   311
    305   306   307   308   309   310   311   312
    306   307   308   309   310   311   312   313
    307   308   309   310   311   312   313   314
    308   309   310   311   312   313   314   315
    309   310   311   312   313   314   315   316
    310   311   312   313   314   315   316   317
    311   312   313   314   315   316   317   318
    312   313   314   315   316   317   318   319
    313   314   315   316   317   318   319   320
    314   315   316   317   318   319   320   321
    315   316   317   318   319   320   321   322
    316   317   318   319   320   321   322   323
    317   318   319   320   321   322   323   324
    318   319   320   321   322   323   324   325
    319   320   321   322   323   324   325   326
img2ascii half1.img hout1a.txt size=(1,1,20,20) org=COLUMNS
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext hout1a.txt
Beginning VICAR task typetext
      1     300
      2     301
      3     302
      4     303
      5     304
      6     305
      7     306
      8     307
      9     308
     10     309
     11     310
     12     311
     13     312
     14     313
     15     314
     16     315
     17     316
     18     317
     19     318
     20     319
     21     301
     22     302
     23     303
     24     304
     25     305
     26     306
     27     307
     28     308
     29     309
     30     310
     31     311
     32     312
     33     313
     34     314
     35     315
     36     316
     37     317
     38     318
     39     319
     40     320
     41     302
     42     303
     43     304
     44     305
     45     306
     46     307
     47     308
     48     309
     49     310
     50     311
     51     312
     52     313
     53     314
     54     315
     55     316
     56     317
     57     318
     58     319
     59     320
     60     321
     61     303
     62     304
     63     305
     64     306
     65     307
     66     308
     67     309
     68     310
     69     311
     70     312
     71     313
     72     314
     73     315
     74     316
     75     317
     76     318
     77     319
     78     320
     79     321
     80     322
     81     304
     82     305
     83     306
     84     307
     85     308
     86     309
     87     310
     88     311
     89     312
     90     313
     91     314
     92     315
     93     316
     94     317
     95     318
     96     319
     97     320
     98     321
     99     322
    100     323
    101     305
    102     306
    103     307
    104     308
    105     309
    106     310
    107     311
    108     312
    109     313
    110     314
    111     315
    112     316
    113     317
    114     318
    115     319
    116     320
    117     321
    118     322
    119     323
    120     324
    121     306
    122     307
    123     308
    124     309
    125     310
    126     311
    127     312
    128     313
    129     314
    130     315
    131     316
    132     317
    133     318
    134     319
    135     320
    136     321
    137     322
    138     323
    139     324
    140     325
    141     307
    142     308
    143     309
    144     310
    145     311
    146     312
    147     313
    148     314
    149     315
    150     316
    151     317
    152     318
    153     319
    154     320
    155     321
    156     322
    157     323
    158     324
    159     325
    160     326
    161     308
    162     309
    163     310
    164     311
    165     312
    166     313
    167     314
    168     315
    169     316
    170     317
    171     318
    172     319
    173     320
    174     321
    175     322
    176     323
    177     324
    178     325
    179     326
    180     327
    181     309
    182     310
    183     311
    184     312
    185     313
    186     314
    187     315
    188     316
    189     317
    190     318
    191     319
    192     320
    193     321
    194     322
    195     323
    196     324
    197     325
    198     326
    199     327
    200     328
    201     310
    202     311
    203     312
    204     313
    205     314
    206     315
    207     316
    208     317
    209     318
    210     319
    211     320
    212     321
    213     322
    214     323
    215     324
    216     325
    217     326
    218     327
    219     328
    220     329
    221     311
    222     312
    223     313
    224     314
    225     315
    226     316
    227     317
    228     318
    229     319
    230     320
    231     321
    232     322
    233     323
    234     324
    235     325
    236     326
    237     327
    238     328
    239     329
    240     330
    241     312
    242     313
    243     314
    244     315
    245     316
    246     317
    247     318
    248     319
    249     320
    250     321
    251     322
    252     323
    253     324
    254     325
    255     326
    256     327
    257     328
    258     329
    259     330
    260     331
    261     313
    262     314
    263     315
    264     316
    265     317
    266     318
    267     319
    268     320
    269     321
    270     322
    271     323
    272     324
    273     325
    274     326
    275     327
    276     328
    277     329
    278     330
    279     331
    280     332
    281     314
    282     315
    283     316
    284     317
    285     318
    286     319
    287     320
    288     321
    289     322
    290     323
    291     324
    292     325
    293     326
    294     327
    295     328
    296     329
    297     330
    298     331
    299     332
    300     333
    301     315
    302     316
    303     317
    304     318
    305     319
    306     320
    307     321
    308     322
    309     323
    310     324
    311     325
    312     326
    313     327
    314     328
    315     329
    316     330
    317     331
    318     332
    319     333
    320     334
    321     316
    322     317
    323     318
    324     319
    325     320
    326     321
    327     322
    328     323
    329     324
    330     325
    331     326
    332     327
    333     328
    334     329
    335     330
    336     331
    337     332
    338     333
    339     334
    340     335
    341     317
    342     318
    343     319
    344     320
    345     321
    346     322
    347     323
    348     324
    349     325
    350     326
    351     327
    352     328
    353     329
    354     330
    355     331
    356     332
    357     333
    358     334
    359     335
    360     336
    361     318
    362     319
    363     320
    364     321
    365     322
    366     323
    367     324
    368     325
    369     326
    370     327
    371     328
    372     329
    373     330
    374     331
    375     332
    376     333
    377     334
    378     335
    379     336
    380     337
    381     319
    382     320
    383     321
    384     322
    385     323
    386     324
    387     325
    388     326
    389     327
    390     328
    391     329
    392     330
    393     331
    394     332
    395     333
    396     334
    397     335
    398     336
    399     337
    400     338
img2ascii half1.img hout1b.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext hout1b.txt
Beginning VICAR task typetext
      1     300
      2     301
      3     302
      4     303
      5     304
      6     305
      7     306
      8     307
      9     308
     10     309
     11     310
     12     311
     13     312
     14     313
     15     314
     16     315
     17     316
     18     317
     19     318
     20     319
     21     301
     22     302
     23     303
     24     304
     25     305
     26     306
     27     307
     28     308
     29     309
     30     310
     31     311
     32     312
     33     313
     34     314
     35     315
     36     316
     37     317
     38     318
     39     319
     40     320
     41     302
     42     303
     43     304
     44     305
     45     306
     46     307
     47     308
     48     309
     49     310
     50     311
     51     312
     52     313
     53     314
     54     315
     55     316
     56     317
     57     318
     58     319
     59     320
     60     321
     61     303
     62     304
     63     305
     64     306
     65     307
     66     308
     67     309
     68     310
     69     311
     70     312
     71     313
     72     314
     73     315
     74     316
     75     317
     76     318
     77     319
     78     320
     79     321
     80     322
     81     304
     82     305
     83     306
     84     307
     85     308
     86     309
     87     310
     88     311
     89     312
     90     313
     91     314
     92     315
     93     316
     94     317
     95     318
     96     319
     97     320
     98     321
     99     322
    100     323
    101     305
    102     306
    103     307
    104     308
    105     309
    106     310
    107     311
    108     312
    109     313
    110     314
    111     315
    112     316
    113     317
    114     318
    115     319
    116     320
    117     321
    118     322
    119     323
    120     324
    121     306
    122     307
    123     308
    124     309
    125     310
    126     311
    127     312
    128     313
    129     314
    130     315
    131     316
    132     317
    133     318
    134     319
    135     320
    136     321
    137     322
    138     323
    139     324
    140     325
    141     307
    142     308
    143     309
    144     310
    145     311
    146     312
    147     313
    148     314
    149     315
    150     316
    151     317
    152     318
    153     319
    154     320
    155     321
    156     322
    157     323
    158     324
    159     325
    160     326
    161     308
    162     309
    163     310
    164     311
    165     312
    166     313
    167     314
    168     315
    169     316
    170     317
    171     318
    172     319
    173     320
    174     321
    175     322
    176     323
    177     324
    178     325
    179     326
    180     327
    181     309
    182     310
    183     311
    184     312
    185     313
    186     314
    187     315
    188     316
    189     317
    190     318
    191     319
    192     320
    193     321
    194     322
    195     323
    196     324
    197     325
    198     326
    199     327
    200     328
    201     310
    202     311
    203     312
    204     313
    205     314
    206     315
    207     316
    208     317
    209     318
    210     319
    211     320
    212     321
    213     322
    214     323
    215     324
    216     325
    217     326
    218     327
    219     328
    220     329
    221     311
    222     312
    223     313
    224     314
    225     315
    226     316
    227     317
    228     318
    229     319
    230     320
    231     321
    232     322
    233     323
    234     324
    235     325
    236     326
    237     327
    238     328
    239     329
    240     330
    241     312
    242     313
    243     314
    244     315
    245     316
    246     317
    247     318
    248     319
    249     320
    250     321
    251     322
    252     323
    253     324
    254     325
    255     326
    256     327
    257     328
    258     329
    259     330
    260     331
    261     313
    262     314
    263     315
    264     316
    265     317
    266     318
    267     319
    268     320
    269     321
    270     322
    271     323
    272     324
    273     325
    274     326
    275     327
    276     328
    277     329
    278     330
    279     331
    280     332
    281     314
    282     315
    283     316
    284     317
    285     318
    286     319
    287     320
    288     321
    289     322
    290     323
    291     324
    292     325
    293     326
    294     327
    295     328
    296     329
    297     330
    298     331
    299     332
    300     333
    301     315
    302     316
    303     317
    304     318
    305     319
    306     320
    307     321
    308     322
    309     323
    310     324
    311     325
    312     326
    313     327
    314     328
    315     329
    316     330
    317     331
    318     332
    319     333
    320     334
    321     316
    322     317
    323     318
    324     319
    325     320
    326     321
    327     322
    328     323
    329     324
    330     325
    331     326
    332     327
    333     328
    334     329
    335     330
    336     331
    337     332
    338     333
    339     334
    340     335
    341     317
    342     318
    343     319
    344     320
    345     321
    346     322
    347     323
    348     324
    349     325
    350     326
    351     327
    352     328
    353     329
    354     330
    355     331
    356     332
    357     333
    358     334
    359     335
    360     336
    361     318
    362     319
    363     320
    364     321
    365     322
    366     323
    367     324
    368     325
    369     326
    370     327
    371     328
    372     329
    373     330
    374     331
    375     332
    376     333
    377     334
    378     335
    379     336
    380     337
    381     319
    382     320
    383     321
    384     322
    385     323
    386     324
    387     325
    388     326
    389     327
    390     328
    391     329
    392     330
    393     331
    394     332
    395     333
    396     334
    397     335
    398     336
    399     337
    400     338
img2ascii half1.img hout1c.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes notes=on minval=minval maxval=maxval
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
Minimum value:    300 Maximum value:    338
The output text file is dimensioned   20 by      2
let $echo="no"
maximum value = 338  minimum value = 300
typetext hout1c.txt
Beginning VICAR task typetext
      1     300
      2     301
      3     302
      4     303
      5     304
      6     305
      7     306
      8     307
      9     308
     10     309
     11     310
     12     311
     13     312
     14     313
     15     314
     16     315
     17     316
     18     317
     19     318
     20     319
     21     301
     22     302
     23     303
     24     304
     25     305
     26     306
     27     307
     28     308
     29     309
     30     310
     31     311
     32     312
     33     313
     34     314
     35     315
     36     316
     37     317
     38     318
     39     319
     40     320
     41     302
     42     303
     43     304
     44     305
     45     306
     46     307
     47     308
     48     309
     49     310
     50     311
     51     312
     52     313
     53     314
     54     315
     55     316
     56     317
     57     318
     58     319
     59     320
     60     321
     61     303
     62     304
     63     305
     64     306
     65     307
     66     308
     67     309
     68     310
     69     311
     70     312
     71     313
     72     314
     73     315
     74     316
     75     317
     76     318
     77     319
     78     320
     79     321
     80     322
     81     304
     82     305
     83     306
     84     307
     85     308
     86     309
     87     310
     88     311
     89     312
     90     313
     91     314
     92     315
     93     316
     94     317
     95     318
     96     319
     97     320
     98     321
     99     322
    100     323
    101     305
    102     306
    103     307
    104     308
    105     309
    106     310
    107     311
    108     312
    109     313
    110     314
    111     315
    112     316
    113     317
    114     318
    115     319
    116     320
    117     321
    118     322
    119     323
    120     324
    121     306
    122     307
    123     308
    124     309
    125     310
    126     311
    127     312
    128     313
    129     314
    130     315
    131     316
    132     317
    133     318
    134     319
    135     320
    136     321
    137     322
    138     323
    139     324
    140     325
    141     307
    142     308
    143     309
    144     310
    145     311
    146     312
    147     313
    148     314
    149     315
    150     316
    151     317
    152     318
    153     319
    154     320
    155     321
    156     322
    157     323
    158     324
    159     325
    160     326
    161     308
    162     309
    163     310
    164     311
    165     312
    166     313
    167     314
    168     315
    169     316
    170     317
    171     318
    172     319
    173     320
    174     321
    175     322
    176     323
    177     324
    178     325
    179     326
    180     327
    181     309
    182     310
    183     311
    184     312
    185     313
    186     314
    187     315
    188     316
    189     317
    190     318
    191     319
    192     320
    193     321
    194     322
    195     323
    196     324
    197     325
    198     326
    199     327
    200     328
    201     310
    202     311
    203     312
    204     313
    205     314
    206     315
    207     316
    208     317
    209     318
    210     319
    211     320
    212     321
    213     322
    214     323
    215     324
    216     325
    217     326
    218     327
    219     328
    220     329
    221     311
    222     312
    223     313
    224     314
    225     315
    226     316
    227     317
    228     318
    229     319
    230     320
    231     321
    232     322
    233     323
    234     324
    235     325
    236     326
    237     327
    238     328
    239     329
    240     330
    241     312
    242     313
    243     314
    244     315
    245     316
    246     317
    247     318
    248     319
    249     320
    250     321
    251     322
    252     323
    253     324
    254     325
    255     326
    256     327
    257     328
    258     329
    259     330
    260     331
    261     313
    262     314
    263     315
    264     316
    265     317
    266     318
    267     319
    268     320
    269     321
    270     322
    271     323
    272     324
    273     325
    274     326
    275     327
    276     328
    277     329
    278     330
    279     331
    280     332
    281     314
    282     315
    283     316
    284     317
    285     318
    286     319
    287     320
    288     321
    289     322
    290     323
    291     324
    292     325
    293     326
    294     327
    295     328
    296     329
    297     330
    298     331
    299     332
    300     333
    301     315
    302     316
    303     317
    304     318
    305     319
    306     320
    307     321
    308     322
    309     323
    310     324
    311     325
    312     326
    313     327
    314     328
    315     329
    316     330
    317     331
    318     332
    319     333
    320     334
    321     316
    322     317
    323     318
    324     319
    325     320
    326     321
    327     322
    328     323
    329     324
    330     325
    331     326
    332     327
    333     328
    334     329
    335     330
    336     331
    337     332
    338     333
    339     334
    340     335
    341     317
    342     318
    343     319
    344     320
    345     321
    346     322
    347     323
    348     324
    349     325
    350     326
    351     327
    352     328
    353     329
    354     330
    355     331
    356     332
    357     333
    358     334
    359     335
    360     336
    361     318
    362     319
    363     320
    364     321
    365     322
    366     323
    367     324
    368     325
    369     326
    370     327
    371     328
    372     329
    373     330
    374     331
    375     332
    376     333
    377     334
    378     335
    379     336
    380     337
    381     319
    382     320
    383     321
    384     322
    385     323
    386     324
    387     325
    388     326
    389     327
    390     328
    391     329
    392     330
    393     331
    394     332
    395     333
    396     334
    397     335
    398     336
    399     337
    400     338
img2ascii full2.img fout2.txt
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext fout2.txt
Beginning VICAR task typetext
  36000 36001 36002 36003 36004 36005 36006 36007 36008 36009 36010 36011
  36012 36013 36014 36015 36016 36017 36018 36019 36020 36021 36022 36023
  36024 36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035
  36036 36037
  36001 36002 36003 36004 36005 36006 36007 36008 36009 36010 36011 36012
  36013 36014 36015 36016 36017 36018 36019 36020 36021 36022 36023 36024
  36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036
  36037 36038
  36002 36003 36004 36005 36006 36007 36008 36009 36010 36011 36012 36013
  36014 36015 36016 36017 36018 36019 36020 36021 36022 36023 36024 36025
  36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037
  36038 36039
  36003 36004 36005 36006 36007 36008 36009 36010 36011 36012 36013 36014
  36015 36016 36017 36018 36019 36020 36021 36022 36023 36024 36025 36026
  36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038
  36039 36040
  36004 36005 36006 36007 36008 36009 36010 36011 36012 36013 36014 36015
  36016 36017 36018 36019 36020 36021 36022 36023 36024 36025 36026 36027
  36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039
  36040 36041
  36005 36006 36007 36008 36009 36010 36011 36012 36013 36014 36015 36016
  36017 36018 36019 36020 36021 36022 36023 36024 36025 36026 36027 36028
  36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040
  36041 36042
  36006 36007 36008 36009 36010 36011 36012 36013 36014 36015 36016 36017
  36018 36019 36020 36021 36022 36023 36024 36025 36026 36027 36028 36029
  36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041
  36042 36043
  36007 36008 36009 36010 36011 36012 36013 36014 36015 36016 36017 36018
  36019 36020 36021 36022 36023 36024 36025 36026 36027 36028 36029 36030
  36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042
  36043 36044
  36008 36009 36010 36011 36012 36013 36014 36015 36016 36017 36018 36019
  36020 36021 36022 36023 36024 36025 36026 36027 36028 36029 36030 36031
  36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043
  36044 36045
  36009 36010 36011 36012 36013 36014 36015 36016 36017 36018 36019 36020
  36021 36022 36023 36024 36025 36026 36027 36028 36029 36030 36031 36032
  36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044
  36045 36046
  36010 36011 36012 36013 36014 36015 36016 36017 36018 36019 36020 36021
  36022 36023 36024 36025 36026 36027 36028 36029 36030 36031 36032 36033
  36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045
  36046 36047
  36011 36012 36013 36014 36015 36016 36017 36018 36019 36020 36021 36022
  36023 36024 36025 36026 36027 36028 36029 36030 36031 36032 36033 36034
  36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046
  36047 36048
  36012 36013 36014 36015 36016 36017 36018 36019 36020 36021 36022 36023
  36024 36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035
  36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047
  36048 36049
  36013 36014 36015 36016 36017 36018 36019 36020 36021 36022 36023 36024
  36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036
  36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048
  36049 36050
  36014 36015 36016 36017 36018 36019 36020 36021 36022 36023 36024 36025
  36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037
  36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049
  36050 36051
  36015 36016 36017 36018 36019 36020 36021 36022 36023 36024 36025 36026
  36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038
  36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050
  36051 36052
  36016 36017 36018 36019 36020 36021 36022 36023 36024 36025 36026 36027
  36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039
  36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051
  36052 36053
  36017 36018 36019 36020 36021 36022 36023 36024 36025 36026 36027 36028
  36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040
  36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052
  36053 36054
  36018 36019 36020 36021 36022 36023 36024 36025 36026 36027 36028 36029
  36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041
  36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053
  36054 36055
  36019 36020 36021 36022 36023 36024 36025 36026 36027 36028 36029 36030
  36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042
  36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054
  36055 36056
  36020 36021 36022 36023 36024 36025 36026 36027 36028 36029 36030 36031
  36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043
  36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055
  36056 36057
  36021 36022 36023 36024 36025 36026 36027 36028 36029 36030 36031 36032
  36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044
  36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056
  36057 36058
  36022 36023 36024 36025 36026 36027 36028 36029 36030 36031 36032 36033
  36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045
  36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057
  36058 36059
  36023 36024 36025 36026 36027 36028 36029 36030 36031 36032 36033 36034
  36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046
  36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058
  36059 36060
  36024 36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035
  36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047
  36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059
  36060 36061
  36025 36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036
  36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048
  36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059 36060
  36061 36062
  36026 36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037
  36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049
  36050 36051 36052 36053 36054 36055 36056 36057 36058 36059 36060 36061
  36062 36063
  36027 36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038
  36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050
  36051 36052 36053 36054 36055 36056 36057 36058 36059 36060 36061 36062
  36063 36064
  36028 36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039
  36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051
  36052 36053 36054 36055 36056 36057 36058 36059 36060 36061 36062 36063
  36064 36065
  36029 36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040
  36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052
  36053 36054 36055 36056 36057 36058 36059 36060 36061 36062 36063 36064
  36065 36066
  36030 36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041
  36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053
  36054 36055 36056 36057 36058 36059 36060 36061 36062 36063 36064 36065
  36066 36067
  36031 36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042
  36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054
  36055 36056 36057 36058 36059 36060 36061 36062 36063 36064 36065 36066
  36067 36068
  36032 36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043
  36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055
  36056 36057 36058 36059 36060 36061 36062 36063 36064 36065 36066 36067
  36068 36069
  36033 36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044
  36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056
  36057 36058 36059 36060 36061 36062 36063 36064 36065 36066 36067 36068
  36069 36070
  36034 36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045
  36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057
  36058 36059 36060 36061 36062 36063 36064 36065 36066 36067 36068 36069
  36070 36071
  36035 36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046
  36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058
  36059 36060 36061 36062 36063 36064 36065 36066 36067 36068 36069 36070
  36071 36072
  36036 36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047
  36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059
  36060 36061 36062 36063 36064 36065 36066 36067 36068 36069 36070 36071
  36072 36073
  36037 36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048
  36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059 36060
  36061 36062 36063 36064 36065 36066 36067 36068 36069 36070 36071 36072
  36073 36074
  36038 36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049
  36050 36051 36052 36053 36054 36055 36056 36057 36058 36059 36060 36061
  36062 36063 36064 36065 36066 36067 36068 36069 36070 36071 36072 36073
  36074 36075
  36039 36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050
  36051 36052 36053 36054 36055 36056 36057 36058 36059 36060 36061 36062
  36063 36064 36065 36066 36067 36068 36069 36070 36071 36072 36073 36074
  36075 36076
  36040 36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051
  36052 36053 36054 36055 36056 36057 36058 36059 36060 36061 36062 36063
  36064 36065 36066 36067 36068 36069 36070 36071 36072 36073 36074 36075
  36076 36077
  36041 36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052
  36053 36054 36055 36056 36057 36058 36059 36060 36061 36062 36063 36064
  36065 36066 36067 36068 36069 36070 36071 36072 36073 36074 36075 36076
  36077 36078
  36042 36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053
  36054 36055 36056 36057 36058 36059 36060 36061 36062 36063 36064 36065
  36066 36067 36068 36069 36070 36071 36072 36073 36074 36075 36076 36077
  36078 36079
  36043 36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054
  36055 36056 36057 36058 36059 36060 36061 36062 36063 36064 36065 36066
  36067 36068 36069 36070 36071 36072 36073 36074 36075 36076 36077 36078
  36079 36080
  36044 36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055
  36056 36057 36058 36059 36060 36061 36062 36063 36064 36065 36066 36067
  36068 36069 36070 36071 36072 36073 36074 36075 36076 36077 36078 36079
  36080 36081
  36045 36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056
  36057 36058 36059 36060 36061 36062 36063 36064 36065 36066 36067 36068
  36069 36070 36071 36072 36073 36074 36075 36076 36077 36078 36079 36080
  36081 36082
  36046 36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057
  36058 36059 36060 36061 36062 36063 36064 36065 36066 36067 36068 36069
  36070 36071 36072 36073 36074 36075 36076 36077 36078 36079 36080 36081
  36082 36083
  36047 36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058
  36059 36060 36061 36062 36063 36064 36065 36066 36067 36068 36069 36070
  36071 36072 36073 36074 36075 36076 36077 36078 36079 36080 36081 36082
  36083 36084
  36048 36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059
  36060 36061 36062 36063 36064 36065 36066 36067 36068 36069 36070 36071
  36072 36073 36074 36075 36076 36077 36078 36079 36080 36081 36082 36083
  36084 36085
  36049 36050 36051 36052 36053 36054 36055 36056 36057 36058 36059 36060
  36061 36062 36063 36064 36065 36066 36067 36068 36069 36070 36071 36072
  36073 36074 36075 36076 36077 36078 36079 36080 36081 36082 36083 36084
  36085 36086
img2ascii full1.img fout1.txt size=(1,1,20,20)
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext fout1.txt
Beginning VICAR task typetext
  36000 36001 36002 36003 36004 36005 36006 36007
  36001 36002 36003 36004 36005 36006 36007 36008
  36002 36003 36004 36005 36006 36007 36008 36009
  36003 36004 36005 36006 36007 36008 36009 36010
  36004 36005 36006 36007 36008 36009 36010 36011
  36005 36006 36007 36008 36009 36010 36011 36012
  36006 36007 36008 36009 36010 36011 36012 36013
  36007 36008 36009 36010 36011 36012 36013 36014
  36008 36009 36010 36011 36012 36013 36014 36015
  36009 36010 36011 36012 36013 36014 36015 36016
  36010 36011 36012 36013 36014 36015 36016 36017
  36011 36012 36013 36014 36015 36016 36017 36018
  36012 36013 36014 36015 36016 36017 36018 36019
  36013 36014 36015 36016 36017 36018 36019 36020
  36014 36015 36016 36017 36018 36019 36020 36021
  36015 36016 36017 36018 36019 36020 36021 36022
  36016 36017 36018 36019 36020 36021 36022 36023
  36017 36018 36019 36020 36021 36022 36023 36024
  36018 36019 36020 36021 36022 36023 36024 36025
  36019 36020 36021 36022 36023 36024 36025 36026
img2ascii full1.img fout1a.txt size=(1,1,20,20) org=COLUMNS
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext fout1a.txt
Beginning VICAR task typetext
      1   36000
      2   36001
      3   36002
      4   36003
      5   36004
      6   36005
      7   36006
      8   36007
      9   36008
     10   36009
     11   36010
     12   36011
     13   36012
     14   36013
     15   36014
     16   36015
     17   36016
     18   36017
     19   36018
     20   36019
     21   36001
     22   36002
     23   36003
     24   36004
     25   36005
     26   36006
     27   36007
     28   36008
     29   36009
     30   36010
     31   36011
     32   36012
     33   36013
     34   36014
     35   36015
     36   36016
     37   36017
     38   36018
     39   36019
     40   36020
     41   36002
     42   36003
     43   36004
     44   36005
     45   36006
     46   36007
     47   36008
     48   36009
     49   36010
     50   36011
     51   36012
     52   36013
     53   36014
     54   36015
     55   36016
     56   36017
     57   36018
     58   36019
     59   36020
     60   36021
     61   36003
     62   36004
     63   36005
     64   36006
     65   36007
     66   36008
     67   36009
     68   36010
     69   36011
     70   36012
     71   36013
     72   36014
     73   36015
     74   36016
     75   36017
     76   36018
     77   36019
     78   36020
     79   36021
     80   36022
     81   36004
     82   36005
     83   36006
     84   36007
     85   36008
     86   36009
     87   36010
     88   36011
     89   36012
     90   36013
     91   36014
     92   36015
     93   36016
     94   36017
     95   36018
     96   36019
     97   36020
     98   36021
     99   36022
    100   36023
    101   36005
    102   36006
    103   36007
    104   36008
    105   36009
    106   36010
    107   36011
    108   36012
    109   36013
    110   36014
    111   36015
    112   36016
    113   36017
    114   36018
    115   36019
    116   36020
    117   36021
    118   36022
    119   36023
    120   36024
    121   36006
    122   36007
    123   36008
    124   36009
    125   36010
    126   36011
    127   36012
    128   36013
    129   36014
    130   36015
    131   36016
    132   36017
    133   36018
    134   36019
    135   36020
    136   36021
    137   36022
    138   36023
    139   36024
    140   36025
    141   36007
    142   36008
    143   36009
    144   36010
    145   36011
    146   36012
    147   36013
    148   36014
    149   36015
    150   36016
    151   36017
    152   36018
    153   36019
    154   36020
    155   36021
    156   36022
    157   36023
    158   36024
    159   36025
    160   36026
    161   36008
    162   36009
    163   36010
    164   36011
    165   36012
    166   36013
    167   36014
    168   36015
    169   36016
    170   36017
    171   36018
    172   36019
    173   36020
    174   36021
    175   36022
    176   36023
    177   36024
    178   36025
    179   36026
    180   36027
    181   36009
    182   36010
    183   36011
    184   36012
    185   36013
    186   36014
    187   36015
    188   36016
    189   36017
    190   36018
    191   36019
    192   36020
    193   36021
    194   36022
    195   36023
    196   36024
    197   36025
    198   36026
    199   36027
    200   36028
    201   36010
    202   36011
    203   36012
    204   36013
    205   36014
    206   36015
    207   36016
    208   36017
    209   36018
    210   36019
    211   36020
    212   36021
    213   36022
    214   36023
    215   36024
    216   36025
    217   36026
    218   36027
    219   36028
    220   36029
    221   36011
    222   36012
    223   36013
    224   36014
    225   36015
    226   36016
    227   36017
    228   36018
    229   36019
    230   36020
    231   36021
    232   36022
    233   36023
    234   36024
    235   36025
    236   36026
    237   36027
    238   36028
    239   36029
    240   36030
    241   36012
    242   36013
    243   36014
    244   36015
    245   36016
    246   36017
    247   36018
    248   36019
    249   36020
    250   36021
    251   36022
    252   36023
    253   36024
    254   36025
    255   36026
    256   36027
    257   36028
    258   36029
    259   36030
    260   36031
    261   36013
    262   36014
    263   36015
    264   36016
    265   36017
    266   36018
    267   36019
    268   36020
    269   36021
    270   36022
    271   36023
    272   36024
    273   36025
    274   36026
    275   36027
    276   36028
    277   36029
    278   36030
    279   36031
    280   36032
    281   36014
    282   36015
    283   36016
    284   36017
    285   36018
    286   36019
    287   36020
    288   36021
    289   36022
    290   36023
    291   36024
    292   36025
    293   36026
    294   36027
    295   36028
    296   36029
    297   36030
    298   36031
    299   36032
    300   36033
    301   36015
    302   36016
    303   36017
    304   36018
    305   36019
    306   36020
    307   36021
    308   36022
    309   36023
    310   36024
    311   36025
    312   36026
    313   36027
    314   36028
    315   36029
    316   36030
    317   36031
    318   36032
    319   36033
    320   36034
    321   36016
    322   36017
    323   36018
    324   36019
    325   36020
    326   36021
    327   36022
    328   36023
    329   36024
    330   36025
    331   36026
    332   36027
    333   36028
    334   36029
    335   36030
    336   36031
    337   36032
    338   36033
    339   36034
    340   36035
    341   36017
    342   36018
    343   36019
    344   36020
    345   36021
    346   36022
    347   36023
    348   36024
    349   36025
    350   36026
    351   36027
    352   36028
    353   36029
    354   36030
    355   36031
    356   36032
    357   36033
    358   36034
    359   36035
    360   36036
    361   36018
    362   36019
    363   36020
    364   36021
    365   36022
    366   36023
    367   36024
    368   36025
    369   36026
    370   36027
    371   36028
    372   36029
    373   36030
    374   36031
    375   36032
    376   36033
    377   36034
    378   36035
    379   36036
    380   36037
    381   36019
    382   36020
    383   36021
    384   36022
    385   36023
    386   36024
    387   36025
    388   36026
    389   36027
    390   36028
    391   36029
    392   36030
    393   36031
    394   36032
    395   36033
    396   36034
    397   36035
    398   36036
    399   36037
    400   36038
img2ascii full1.img fout1b.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext fout1b.txt
Beginning VICAR task typetext
      1   36000
      2   36001
      3   36002
      4   36003
      5   36004
      6   36005
      7   36006
      8   36007
      9   36008
     10   36009
     11   36010
     12   36011
     13   36012
     14   36013
     15   36014
     16   36015
     17   36016
     18   36017
     19   36018
     20   36019
     21   36001
     22   36002
     23   36003
     24   36004
     25   36005
     26   36006
     27   36007
     28   36008
     29   36009
     30   36010
     31   36011
     32   36012
     33   36013
     34   36014
     35   36015
     36   36016
     37   36017
     38   36018
     39   36019
     40   36020
     41   36002
     42   36003
     43   36004
     44   36005
     45   36006
     46   36007
     47   36008
     48   36009
     49   36010
     50   36011
     51   36012
     52   36013
     53   36014
     54   36015
     55   36016
     56   36017
     57   36018
     58   36019
     59   36020
     60   36021
     61   36003
     62   36004
     63   36005
     64   36006
     65   36007
     66   36008
     67   36009
     68   36010
     69   36011
     70   36012
     71   36013
     72   36014
     73   36015
     74   36016
     75   36017
     76   36018
     77   36019
     78   36020
     79   36021
     80   36022
     81   36004
     82   36005
     83   36006
     84   36007
     85   36008
     86   36009
     87   36010
     88   36011
     89   36012
     90   36013
     91   36014
     92   36015
     93   36016
     94   36017
     95   36018
     96   36019
     97   36020
     98   36021
     99   36022
    100   36023
    101   36005
    102   36006
    103   36007
    104   36008
    105   36009
    106   36010
    107   36011
    108   36012
    109   36013
    110   36014
    111   36015
    112   36016
    113   36017
    114   36018
    115   36019
    116   36020
    117   36021
    118   36022
    119   36023
    120   36024
    121   36006
    122   36007
    123   36008
    124   36009
    125   36010
    126   36011
    127   36012
    128   36013
    129   36014
    130   36015
    131   36016
    132   36017
    133   36018
    134   36019
    135   36020
    136   36021
    137   36022
    138   36023
    139   36024
    140   36025
    141   36007
    142   36008
    143   36009
    144   36010
    145   36011
    146   36012
    147   36013
    148   36014
    149   36015
    150   36016
    151   36017
    152   36018
    153   36019
    154   36020
    155   36021
    156   36022
    157   36023
    158   36024
    159   36025
    160   36026
    161   36008
    162   36009
    163   36010
    164   36011
    165   36012
    166   36013
    167   36014
    168   36015
    169   36016
    170   36017
    171   36018
    172   36019
    173   36020
    174   36021
    175   36022
    176   36023
    177   36024
    178   36025
    179   36026
    180   36027
    181   36009
    182   36010
    183   36011
    184   36012
    185   36013
    186   36014
    187   36015
    188   36016
    189   36017
    190   36018
    191   36019
    192   36020
    193   36021
    194   36022
    195   36023
    196   36024
    197   36025
    198   36026
    199   36027
    200   36028
    201   36010
    202   36011
    203   36012
    204   36013
    205   36014
    206   36015
    207   36016
    208   36017
    209   36018
    210   36019
    211   36020
    212   36021
    213   36022
    214   36023
    215   36024
    216   36025
    217   36026
    218   36027
    219   36028
    220   36029
    221   36011
    222   36012
    223   36013
    224   36014
    225   36015
    226   36016
    227   36017
    228   36018
    229   36019
    230   36020
    231   36021
    232   36022
    233   36023
    234   36024
    235   36025
    236   36026
    237   36027
    238   36028
    239   36029
    240   36030
    241   36012
    242   36013
    243   36014
    244   36015
    245   36016
    246   36017
    247   36018
    248   36019
    249   36020
    250   36021
    251   36022
    252   36023
    253   36024
    254   36025
    255   36026
    256   36027
    257   36028
    258   36029
    259   36030
    260   36031
    261   36013
    262   36014
    263   36015
    264   36016
    265   36017
    266   36018
    267   36019
    268   36020
    269   36021
    270   36022
    271   36023
    272   36024
    273   36025
    274   36026
    275   36027
    276   36028
    277   36029
    278   36030
    279   36031
    280   36032
    281   36014
    282   36015
    283   36016
    284   36017
    285   36018
    286   36019
    287   36020
    288   36021
    289   36022
    290   36023
    291   36024
    292   36025
    293   36026
    294   36027
    295   36028
    296   36029
    297   36030
    298   36031
    299   36032
    300   36033
    301   36015
    302   36016
    303   36017
    304   36018
    305   36019
    306   36020
    307   36021
    308   36022
    309   36023
    310   36024
    311   36025
    312   36026
    313   36027
    314   36028
    315   36029
    316   36030
    317   36031
    318   36032
    319   36033
    320   36034
    321   36016
    322   36017
    323   36018
    324   36019
    325   36020
    326   36021
    327   36022
    328   36023
    329   36024
    330   36025
    331   36026
    332   36027
    333   36028
    334   36029
    335   36030
    336   36031
    337   36032
    338   36033
    339   36034
    340   36035
    341   36017
    342   36018
    343   36019
    344   36020
    345   36021
    346   36022
    347   36023
    348   36024
    349   36025
    350   36026
    351   36027
    352   36028
    353   36029
    354   36030
    355   36031
    356   36032
    357   36033
    358   36034
    359   36035
    360   36036
    361   36018
    362   36019
    363   36020
    364   36021
    365   36022
    366   36023
    367   36024
    368   36025
    369   36026
    370   36027
    371   36028
    372   36029
    373   36030
    374   36031
    375   36032
    376   36033
    377   36034
    378   36035
    379   36036
    380   36037
    381   36019
    382   36020
    383   36021
    384   36022
    385   36023
    386   36024
    387   36025
    388   36026
    389   36027
    390   36028
    391   36029
    392   36030
    393   36031
    394   36032
    395   36033
    396   36034
    397   36035
    398   36036
    399   36037
    400   36038
img2ascii full1.img fout1c.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes notes=on minval=minval maxval=maxval
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
Minimum value:  36000 Maximum value:  36038
The output text file is dimensioned   20 by      2
let $echo="no"
maximum value = 36038  minimum value = 36000
typetext fout1c.txt
Beginning VICAR task typetext
      1   36000
      2   36001
      3   36002
      4   36003
      5   36004
      6   36005
      7   36006
      8   36007
      9   36008
     10   36009
     11   36010
     12   36011
     13   36012
     14   36013
     15   36014
     16   36015
     17   36016
     18   36017
     19   36018
     20   36019
     21   36001
     22   36002
     23   36003
     24   36004
     25   36005
     26   36006
     27   36007
     28   36008
     29   36009
     30   36010
     31   36011
     32   36012
     33   36013
     34   36014
     35   36015
     36   36016
     37   36017
     38   36018
     39   36019
     40   36020
     41   36002
     42   36003
     43   36004
     44   36005
     45   36006
     46   36007
     47   36008
     48   36009
     49   36010
     50   36011
     51   36012
     52   36013
     53   36014
     54   36015
     55   36016
     56   36017
     57   36018
     58   36019
     59   36020
     60   36021
     61   36003
     62   36004
     63   36005
     64   36006
     65   36007
     66   36008
     67   36009
     68   36010
     69   36011
     70   36012
     71   36013
     72   36014
     73   36015
     74   36016
     75   36017
     76   36018
     77   36019
     78   36020
     79   36021
     80   36022
     81   36004
     82   36005
     83   36006
     84   36007
     85   36008
     86   36009
     87   36010
     88   36011
     89   36012
     90   36013
     91   36014
     92   36015
     93   36016
     94   36017
     95   36018
     96   36019
     97   36020
     98   36021
     99   36022
    100   36023
    101   36005
    102   36006
    103   36007
    104   36008
    105   36009
    106   36010
    107   36011
    108   36012
    109   36013
    110   36014
    111   36015
    112   36016
    113   36017
    114   36018
    115   36019
    116   36020
    117   36021
    118   36022
    119   36023
    120   36024
    121   36006
    122   36007
    123   36008
    124   36009
    125   36010
    126   36011
    127   36012
    128   36013
    129   36014
    130   36015
    131   36016
    132   36017
    133   36018
    134   36019
    135   36020
    136   36021
    137   36022
    138   36023
    139   36024
    140   36025
    141   36007
    142   36008
    143   36009
    144   36010
    145   36011
    146   36012
    147   36013
    148   36014
    149   36015
    150   36016
    151   36017
    152   36018
    153   36019
    154   36020
    155   36021
    156   36022
    157   36023
    158   36024
    159   36025
    160   36026
    161   36008
    162   36009
    163   36010
    164   36011
    165   36012
    166   36013
    167   36014
    168   36015
    169   36016
    170   36017
    171   36018
    172   36019
    173   36020
    174   36021
    175   36022
    176   36023
    177   36024
    178   36025
    179   36026
    180   36027
    181   36009
    182   36010
    183   36011
    184   36012
    185   36013
    186   36014
    187   36015
    188   36016
    189   36017
    190   36018
    191   36019
    192   36020
    193   36021
    194   36022
    195   36023
    196   36024
    197   36025
    198   36026
    199   36027
    200   36028
    201   36010
    202   36011
    203   36012
    204   36013
    205   36014
    206   36015
    207   36016
    208   36017
    209   36018
    210   36019
    211   36020
    212   36021
    213   36022
    214   36023
    215   36024
    216   36025
    217   36026
    218   36027
    219   36028
    220   36029
    221   36011
    222   36012
    223   36013
    224   36014
    225   36015
    226   36016
    227   36017
    228   36018
    229   36019
    230   36020
    231   36021
    232   36022
    233   36023
    234   36024
    235   36025
    236   36026
    237   36027
    238   36028
    239   36029
    240   36030
    241   36012
    242   36013
    243   36014
    244   36015
    245   36016
    246   36017
    247   36018
    248   36019
    249   36020
    250   36021
    251   36022
    252   36023
    253   36024
    254   36025
    255   36026
    256   36027
    257   36028
    258   36029
    259   36030
    260   36031
    261   36013
    262   36014
    263   36015
    264   36016
    265   36017
    266   36018
    267   36019
    268   36020
    269   36021
    270   36022
    271   36023
    272   36024
    273   36025
    274   36026
    275   36027
    276   36028
    277   36029
    278   36030
    279   36031
    280   36032
    281   36014
    282   36015
    283   36016
    284   36017
    285   36018
    286   36019
    287   36020
    288   36021
    289   36022
    290   36023
    291   36024
    292   36025
    293   36026
    294   36027
    295   36028
    296   36029
    297   36030
    298   36031
    299   36032
    300   36033
    301   36015
    302   36016
    303   36017
    304   36018
    305   36019
    306   36020
    307   36021
    308   36022
    309   36023
    310   36024
    311   36025
    312   36026
    313   36027
    314   36028
    315   36029
    316   36030
    317   36031
    318   36032
    319   36033
    320   36034
    321   36016
    322   36017
    323   36018
    324   36019
    325   36020
    326   36021
    327   36022
    328   36023
    329   36024
    330   36025
    331   36026
    332   36027
    333   36028
    334   36029
    335   36030
    336   36031
    337   36032
    338   36033
    339   36034
    340   36035
    341   36017
    342   36018
    343   36019
    344   36020
    345   36021
    346   36022
    347   36023
    348   36024
    349   36025
    350   36026
    351   36027
    352   36028
    353   36029
    354   36030
    355   36031
    356   36032
    357   36033
    358   36034
    359   36035
    360   36036
    361   36018
    362   36019
    363   36020
    364   36021
    365   36022
    366   36023
    367   36024
    368   36025
    369   36026
    370   36027
    371   36028
    372   36029
    373   36030
    374   36031
    375   36032
    376   36033
    377   36034
    378   36035
    379   36036
    380   36037
    381   36019
    382   36020
    383   36021
    384   36022
    385   36023
    386   36024
    387   36025
    388   36026
    389   36027
    390   36028
    391   36029
    392   36030
    393   36031
    394   36032
    395   36033
    396   36034
    397   36035
    398   36036
    399   36037
    400   36038
img2ascii real2.img rout2.txt
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext rout2.txt
Beginning VICAR task typetext
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.303E+04                                                            
  0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.303E+04 0.303E+04                                                            
  0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04                                                            
  0.302E+04 0.302E+04 0.302E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.302E+04 0.302E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.302E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04                                                            
  0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.303E+04                                                            
  0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.303E+04 0.303E+04                                                            
  0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.302E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.305E+04
  0.302E+04 0.302E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.306E+04
  0.302E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04                                                            
  0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.303E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04                                                            
  0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04                                                            
  0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04                                                            
  0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.307E+04
  0.304E+04 0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.308E+04
  0.304E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.307E+04                                                            
  0.307E+04 0.307E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.307E+04 0.307E+04                                                            
  0.307E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.306E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.306E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.306E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.305E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.308E+04
  0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04 0.306E+04                                                            
  0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04 0.307E+04                                                            
  0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04 0.308E+04                                                            
  0.308E+04 0.309E+04
img2ascii real1.img rout1.txt size=(1,1,20,20)
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext rout1.txt
Beginning VICAR task typetext
  0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.301E+04 0.301E+04
  0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.301E+04 0.301E+04 0.301E+04
  0.300E+04 0.300E+04 0.300E+04 0.300E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.300E+04 0.300E+04 0.300E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.300E+04 0.300E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.300E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04
  0.301E+04 0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.301E+04 0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.301E+04 0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.301E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04
  0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.302E+04 0.303E+04
img2ascii real1.img rout1a.txt size=(1,1,20,20) org=COLUMNS
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext rout1a.txt
Beginning VICAR task typetext
      1   0.300E+04
      2   0.300E+04
      3   0.300E+04
      4   0.300E+04
      5   0.300E+04
      6   0.300E+04
      7   0.301E+04
      8   0.301E+04
      9   0.301E+04
     10   0.301E+04
     11   0.301E+04
     12   0.301E+04
     13   0.301E+04
     14   0.301E+04
     15   0.301E+04
     16   0.302E+04
     17   0.302E+04
     18   0.302E+04
     19   0.302E+04
     20   0.302E+04
     21   0.300E+04
     22   0.300E+04
     23   0.300E+04
     24   0.300E+04
     25   0.300E+04
     26   0.301E+04
     27   0.301E+04
     28   0.301E+04
     29   0.301E+04
     30   0.301E+04
     31   0.301E+04
     32   0.301E+04
     33   0.301E+04
     34   0.301E+04
     35   0.302E+04
     36   0.302E+04
     37   0.302E+04
     38   0.302E+04
     39   0.302E+04
     40   0.302E+04
     41   0.300E+04
     42   0.300E+04
     43   0.300E+04
     44   0.300E+04
     45   0.301E+04
     46   0.301E+04
     47   0.301E+04
     48   0.301E+04
     49   0.301E+04
     50   0.301E+04
     51   0.301E+04
     52   0.301E+04
     53   0.301E+04
     54   0.302E+04
     55   0.302E+04
     56   0.302E+04
     57   0.302E+04
     58   0.302E+04
     59   0.302E+04
     60   0.302E+04
     61   0.300E+04
     62   0.300E+04
     63   0.300E+04
     64   0.301E+04
     65   0.301E+04
     66   0.301E+04
     67   0.301E+04
     68   0.301E+04
     69   0.301E+04
     70   0.301E+04
     71   0.301E+04
     72   0.301E+04
     73   0.302E+04
     74   0.302E+04
     75   0.302E+04
     76   0.302E+04
     77   0.302E+04
     78   0.302E+04
     79   0.302E+04
     80   0.302E+04
     81   0.300E+04
     82   0.300E+04
     83   0.301E+04
     84   0.301E+04
     85   0.301E+04
     86   0.301E+04
     87   0.301E+04
     88   0.301E+04
     89   0.301E+04
     90   0.301E+04
     91   0.301E+04
     92   0.302E+04
     93   0.302E+04
     94   0.302E+04
     95   0.302E+04
     96   0.302E+04
     97   0.302E+04
     98   0.302E+04
     99   0.302E+04
    100   0.302E+04
    101   0.300E+04
    102   0.301E+04
    103   0.301E+04
    104   0.301E+04
    105   0.301E+04
    106   0.301E+04
    107   0.301E+04
    108   0.301E+04
    109   0.301E+04
    110   0.301E+04
    111   0.302E+04
    112   0.302E+04
    113   0.302E+04
    114   0.302E+04
    115   0.302E+04
    116   0.302E+04
    117   0.302E+04
    118   0.302E+04
    119   0.302E+04
    120   0.302E+04
    121   0.301E+04
    122   0.301E+04
    123   0.301E+04
    124   0.301E+04
    125   0.301E+04
    126   0.301E+04
    127   0.301E+04
    128   0.301E+04
    129   0.301E+04
    130   0.302E+04
    131   0.302E+04
    132   0.302E+04
    133   0.302E+04
    134   0.302E+04
    135   0.302E+04
    136   0.302E+04
    137   0.302E+04
    138   0.302E+04
    139   0.302E+04
    140   0.302E+04
    141   0.301E+04
    142   0.301E+04
    143   0.301E+04
    144   0.301E+04
    145   0.301E+04
    146   0.301E+04
    147   0.301E+04
    148   0.301E+04
    149   0.302E+04
    150   0.302E+04
    151   0.302E+04
    152   0.302E+04
    153   0.302E+04
    154   0.302E+04
    155   0.302E+04
    156   0.302E+04
    157   0.302E+04
    158   0.302E+04
    159   0.302E+04
    160   0.303E+04
    161   0.301E+04
    162   0.301E+04
    163   0.301E+04
    164   0.301E+04
    165   0.301E+04
    166   0.301E+04
    167   0.301E+04
    168   0.302E+04
    169   0.302E+04
    170   0.302E+04
    171   0.302E+04
    172   0.302E+04
    173   0.302E+04
    174   0.302E+04
    175   0.302E+04
    176   0.302E+04
    177   0.302E+04
    178   0.302E+04
    179   0.303E+04
    180   0.303E+04
    181   0.301E+04
    182   0.301E+04
    183   0.301E+04
    184   0.301E+04
    185   0.301E+04
    186   0.301E+04
    187   0.302E+04
    188   0.302E+04
    189   0.302E+04
    190   0.302E+04
    191   0.302E+04
    192   0.302E+04
    193   0.302E+04
    194   0.302E+04
    195   0.302E+04
    196   0.302E+04
    197   0.302E+04
    198   0.303E+04
    199   0.303E+04
    200   0.303E+04
    201   0.301E+04
    202   0.301E+04
    203   0.301E+04
    204   0.301E+04
    205   0.301E+04
    206   0.302E+04
    207   0.302E+04
    208   0.302E+04
    209   0.302E+04
    210   0.302E+04
    211   0.302E+04
    212   0.302E+04
    213   0.302E+04
    214   0.302E+04
    215   0.302E+04
    216   0.302E+04
    217   0.303E+04
    218   0.303E+04
    219   0.303E+04
    220   0.303E+04
    221   0.301E+04
    222   0.301E+04
    223   0.301E+04
    224   0.301E+04
    225   0.302E+04
    226   0.302E+04
    227   0.302E+04
    228   0.302E+04
    229   0.302E+04
    230   0.302E+04
    231   0.302E+04
    232   0.302E+04
    233   0.302E+04
    234   0.302E+04
    235   0.302E+04
    236   0.303E+04
    237   0.303E+04
    238   0.303E+04
    239   0.303E+04
    240   0.303E+04
    241   0.301E+04
    242   0.301E+04
    243   0.301E+04
    244   0.302E+04
    245   0.302E+04
    246   0.302E+04
    247   0.302E+04
    248   0.302E+04
    249   0.302E+04
    250   0.302E+04
    251   0.302E+04
    252   0.302E+04
    253   0.302E+04
    254   0.302E+04
    255   0.303E+04
    256   0.303E+04
    257   0.303E+04
    258   0.303E+04
    259   0.303E+04
    260   0.303E+04
    261   0.301E+04
    262   0.301E+04
    263   0.302E+04
    264   0.302E+04
    265   0.302E+04
    266   0.302E+04
    267   0.302E+04
    268   0.302E+04
    269   0.302E+04
    270   0.302E+04
    271   0.302E+04
    272   0.302E+04
    273   0.302E+04
    274   0.303E+04
    275   0.303E+04
    276   0.303E+04
    277   0.303E+04
    278   0.303E+04
    279   0.303E+04
    280   0.303E+04
    281   0.301E+04
    282   0.302E+04
    283   0.302E+04
    284   0.302E+04
    285   0.302E+04
    286   0.302E+04
    287   0.302E+04
    288   0.302E+04
    289   0.302E+04
    290   0.302E+04
    291   0.302E+04
    292   0.302E+04
    293   0.303E+04
    294   0.303E+04
    295   0.303E+04
    296   0.303E+04
    297   0.303E+04
    298   0.303E+04
    299   0.303E+04
    300   0.303E+04
    301   0.302E+04
    302   0.302E+04
    303   0.302E+04
    304   0.302E+04
    305   0.302E+04
    306   0.302E+04
    307   0.302E+04
    308   0.302E+04
    309   0.302E+04
    310   0.302E+04
    311   0.302E+04
    312   0.303E+04
    313   0.303E+04
    314   0.303E+04
    315   0.303E+04
    316   0.303E+04
    317   0.303E+04
    318   0.303E+04
    319   0.303E+04
    320   0.303E+04
    321   0.302E+04
    322   0.302E+04
    323   0.302E+04
    324   0.302E+04
    325   0.302E+04
    326   0.302E+04
    327   0.302E+04
    328   0.302E+04
    329   0.302E+04
    330   0.302E+04
    331   0.303E+04
    332   0.303E+04
    333   0.303E+04
    334   0.303E+04
    335   0.303E+04
    336   0.303E+04
    337   0.303E+04
    338   0.303E+04
    339   0.303E+04
    340   0.304E+04
    341   0.302E+04
    342   0.302E+04
    343   0.302E+04
    344   0.302E+04
    345   0.302E+04
    346   0.302E+04
    347   0.302E+04
    348   0.302E+04
    349   0.302E+04
    350   0.303E+04
    351   0.303E+04
    352   0.303E+04
    353   0.303E+04
    354   0.303E+04
    355   0.303E+04
    356   0.303E+04
    357   0.303E+04
    358   0.303E+04
    359   0.304E+04
    360   0.304E+04
    361   0.302E+04
    362   0.302E+04
    363   0.302E+04
    364   0.302E+04
    365   0.302E+04
    366   0.302E+04
    367   0.302E+04
    368   0.302E+04
    369   0.303E+04
    370   0.303E+04
    371   0.303E+04
    372   0.303E+04
    373   0.303E+04
    374   0.303E+04
    375   0.303E+04
    376   0.303E+04
    377   0.303E+04
    378   0.304E+04
    379   0.304E+04
    380   0.304E+04
    381   0.302E+04
    382   0.302E+04
    383   0.302E+04
    384   0.302E+04
    385   0.302E+04
    386   0.302E+04
    387   0.302E+04
    388   0.303E+04
    389   0.303E+04
    390   0.303E+04
    391   0.303E+04
    392   0.303E+04
    393   0.303E+04
    394   0.303E+04
    395   0.303E+04
    396   0.303E+04
    397   0.304E+04
    398   0.304E+04
    399   0.304E+04
    400   0.304E+04
img2ascii real1.img rout1b.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
typetext rout1b.txt
Beginning VICAR task typetext
      1   0.300E+04
      2   0.300E+04
      3   0.300E+04
      4   0.300E+04
      5   0.300E+04
      6   0.300E+04
      7   0.301E+04
      8   0.301E+04
      9   0.301E+04
     10   0.301E+04
     11   0.301E+04
     12   0.301E+04
     13   0.301E+04
     14   0.301E+04
     15   0.301E+04
     16   0.302E+04
     17   0.302E+04
     18   0.302E+04
     19   0.302E+04
     20   0.302E+04
     21   0.300E+04
     22   0.300E+04
     23   0.300E+04
     24   0.300E+04
     25   0.300E+04
     26   0.301E+04
     27   0.301E+04
     28   0.301E+04
     29   0.301E+04
     30   0.301E+04
     31   0.301E+04
     32   0.301E+04
     33   0.301E+04
     34   0.301E+04
     35   0.302E+04
     36   0.302E+04
     37   0.302E+04
     38   0.302E+04
     39   0.302E+04
     40   0.302E+04
     41   0.300E+04
     42   0.300E+04
     43   0.300E+04
     44   0.300E+04
     45   0.301E+04
     46   0.301E+04
     47   0.301E+04
     48   0.301E+04
     49   0.301E+04
     50   0.301E+04
     51   0.301E+04
     52   0.301E+04
     53   0.301E+04
     54   0.302E+04
     55   0.302E+04
     56   0.302E+04
     57   0.302E+04
     58   0.302E+04
     59   0.302E+04
     60   0.302E+04
     61   0.300E+04
     62   0.300E+04
     63   0.300E+04
     64   0.301E+04
     65   0.301E+04
     66   0.301E+04
     67   0.301E+04
     68   0.301E+04
     69   0.301E+04
     70   0.301E+04
     71   0.301E+04
     72   0.301E+04
     73   0.302E+04
     74   0.302E+04
     75   0.302E+04
     76   0.302E+04
     77   0.302E+04
     78   0.302E+04
     79   0.302E+04
     80   0.302E+04
     81   0.300E+04
     82   0.300E+04
     83   0.301E+04
     84   0.301E+04
     85   0.301E+04
     86   0.301E+04
     87   0.301E+04
     88   0.301E+04
     89   0.301E+04
     90   0.301E+04
     91   0.301E+04
     92   0.302E+04
     93   0.302E+04
     94   0.302E+04
     95   0.302E+04
     96   0.302E+04
     97   0.302E+04
     98   0.302E+04
     99   0.302E+04
    100   0.302E+04
    101   0.300E+04
    102   0.301E+04
    103   0.301E+04
    104   0.301E+04
    105   0.301E+04
    106   0.301E+04
    107   0.301E+04
    108   0.301E+04
    109   0.301E+04
    110   0.301E+04
    111   0.302E+04
    112   0.302E+04
    113   0.302E+04
    114   0.302E+04
    115   0.302E+04
    116   0.302E+04
    117   0.302E+04
    118   0.302E+04
    119   0.302E+04
    120   0.302E+04
    121   0.301E+04
    122   0.301E+04
    123   0.301E+04
    124   0.301E+04
    125   0.301E+04
    126   0.301E+04
    127   0.301E+04
    128   0.301E+04
    129   0.301E+04
    130   0.302E+04
    131   0.302E+04
    132   0.302E+04
    133   0.302E+04
    134   0.302E+04
    135   0.302E+04
    136   0.302E+04
    137   0.302E+04
    138   0.302E+04
    139   0.302E+04
    140   0.302E+04
    141   0.301E+04
    142   0.301E+04
    143   0.301E+04
    144   0.301E+04
    145   0.301E+04
    146   0.301E+04
    147   0.301E+04
    148   0.301E+04
    149   0.302E+04
    150   0.302E+04
    151   0.302E+04
    152   0.302E+04
    153   0.302E+04
    154   0.302E+04
    155   0.302E+04
    156   0.302E+04
    157   0.302E+04
    158   0.302E+04
    159   0.302E+04
    160   0.303E+04
    161   0.301E+04
    162   0.301E+04
    163   0.301E+04
    164   0.301E+04
    165   0.301E+04
    166   0.301E+04
    167   0.301E+04
    168   0.302E+04
    169   0.302E+04
    170   0.302E+04
    171   0.302E+04
    172   0.302E+04
    173   0.302E+04
    174   0.302E+04
    175   0.302E+04
    176   0.302E+04
    177   0.302E+04
    178   0.302E+04
    179   0.303E+04
    180   0.303E+04
    181   0.301E+04
    182   0.301E+04
    183   0.301E+04
    184   0.301E+04
    185   0.301E+04
    186   0.301E+04
    187   0.302E+04
    188   0.302E+04
    189   0.302E+04
    190   0.302E+04
    191   0.302E+04
    192   0.302E+04
    193   0.302E+04
    194   0.302E+04
    195   0.302E+04
    196   0.302E+04
    197   0.302E+04
    198   0.303E+04
    199   0.303E+04
    200   0.303E+04
    201   0.301E+04
    202   0.301E+04
    203   0.301E+04
    204   0.301E+04
    205   0.301E+04
    206   0.302E+04
    207   0.302E+04
    208   0.302E+04
    209   0.302E+04
    210   0.302E+04
    211   0.302E+04
    212   0.302E+04
    213   0.302E+04
    214   0.302E+04
    215   0.302E+04
    216   0.302E+04
    217   0.303E+04
    218   0.303E+04
    219   0.303E+04
    220   0.303E+04
    221   0.301E+04
    222   0.301E+04
    223   0.301E+04
    224   0.301E+04
    225   0.302E+04
    226   0.302E+04
    227   0.302E+04
    228   0.302E+04
    229   0.302E+04
    230   0.302E+04
    231   0.302E+04
    232   0.302E+04
    233   0.302E+04
    234   0.302E+04
    235   0.302E+04
    236   0.303E+04
    237   0.303E+04
    238   0.303E+04
    239   0.303E+04
    240   0.303E+04
    241   0.301E+04
    242   0.301E+04
    243   0.301E+04
    244   0.302E+04
    245   0.302E+04
    246   0.302E+04
    247   0.302E+04
    248   0.302E+04
    249   0.302E+04
    250   0.302E+04
    251   0.302E+04
    252   0.302E+04
    253   0.302E+04
    254   0.302E+04
    255   0.303E+04
    256   0.303E+04
    257   0.303E+04
    258   0.303E+04
    259   0.303E+04
    260   0.303E+04
    261   0.301E+04
    262   0.301E+04
    263   0.302E+04
    264   0.302E+04
    265   0.302E+04
    266   0.302E+04
    267   0.302E+04
    268   0.302E+04
    269   0.302E+04
    270   0.302E+04
    271   0.302E+04
    272   0.302E+04
    273   0.302E+04
    274   0.303E+04
    275   0.303E+04
    276   0.303E+04
    277   0.303E+04
    278   0.303E+04
    279   0.303E+04
    280   0.303E+04
    281   0.301E+04
    282   0.302E+04
    283   0.302E+04
    284   0.302E+04
    285   0.302E+04
    286   0.302E+04
    287   0.302E+04
    288   0.302E+04
    289   0.302E+04
    290   0.302E+04
    291   0.302E+04
    292   0.302E+04
    293   0.303E+04
    294   0.303E+04
    295   0.303E+04
    296   0.303E+04
    297   0.303E+04
    298   0.303E+04
    299   0.303E+04
    300   0.303E+04
    301   0.302E+04
    302   0.302E+04
    303   0.302E+04
    304   0.302E+04
    305   0.302E+04
    306   0.302E+04
    307   0.302E+04
    308   0.302E+04
    309   0.302E+04
    310   0.302E+04
    311   0.302E+04
    312   0.303E+04
    313   0.303E+04
    314   0.303E+04
    315   0.303E+04
    316   0.303E+04
    317   0.303E+04
    318   0.303E+04
    319   0.303E+04
    320   0.303E+04
    321   0.302E+04
    322   0.302E+04
    323   0.302E+04
    324   0.302E+04
    325   0.302E+04
    326   0.302E+04
    327   0.302E+04
    328   0.302E+04
    329   0.302E+04
    330   0.302E+04
    331   0.303E+04
    332   0.303E+04
    333   0.303E+04
    334   0.303E+04
    335   0.303E+04
    336   0.303E+04
    337   0.303E+04
    338   0.303E+04
    339   0.303E+04
    340   0.304E+04
    341   0.302E+04
    342   0.302E+04
    343   0.302E+04
    344   0.302E+04
    345   0.302E+04
    346   0.302E+04
    347   0.302E+04
    348   0.302E+04
    349   0.302E+04
    350   0.303E+04
    351   0.303E+04
    352   0.303E+04
    353   0.303E+04
    354   0.303E+04
    355   0.303E+04
    356   0.303E+04
    357   0.303E+04
    358   0.303E+04
    359   0.304E+04
    360   0.304E+04
    361   0.302E+04
    362   0.302E+04
    363   0.302E+04
    364   0.302E+04
    365   0.302E+04
    366   0.302E+04
    367   0.302E+04
    368   0.302E+04
    369   0.303E+04
    370   0.303E+04
    371   0.303E+04
    372   0.303E+04
    373   0.303E+04
    374   0.303E+04
    375   0.303E+04
    376   0.303E+04
    377   0.303E+04
    378   0.304E+04
    379   0.304E+04
    380   0.304E+04
    381   0.302E+04
    382   0.302E+04
    383   0.302E+04
    384   0.302E+04
    385   0.302E+04
    386   0.302E+04
    387   0.302E+04
    388   0.303E+04
    389   0.303E+04
    390   0.303E+04
    391   0.303E+04
    392   0.303E+04
    393   0.303E+04
    394   0.303E+04
    395   0.303E+04
    396   0.303E+04
    397   0.304E+04
    398   0.304E+04
    399   0.304E+04
    400   0.304E+04
img2ascii real1.img rout1c.txt size=(1,1,20,20) org=COLUMNS  +
    index=yes notes=on minval=minval maxval=maxval
Beginning VICAR task img2ascii
IMG2ASCII version 2015-08-10
Minimum value:  0.300E+04 Maximum value:  0.304E+04
The output text file is dimensioned   20 by  2
let $echo="no"
************************************************
maximum value = 3.038000000000e+03  minimum value = 3.000000000000e+03
************************************************
typetext rout1c.txt
Beginning VICAR task typetext
      1   0.300E+04
      2   0.300E+04
      3   0.300E+04
      4   0.300E+04
      5   0.300E+04
      6   0.300E+04
      7   0.301E+04
      8   0.301E+04
      9   0.301E+04
     10   0.301E+04
     11   0.301E+04
     12   0.301E+04
     13   0.301E+04
     14   0.301E+04
     15   0.301E+04
     16   0.302E+04
     17   0.302E+04
     18   0.302E+04
     19   0.302E+04
     20   0.302E+04
     21   0.300E+04
     22   0.300E+04
     23   0.300E+04
     24   0.300E+04
     25   0.300E+04
     26   0.301E+04
     27   0.301E+04
     28   0.301E+04
     29   0.301E+04
     30   0.301E+04
     31   0.301E+04
     32   0.301E+04
     33   0.301E+04
     34   0.301E+04
     35   0.302E+04
     36   0.302E+04
     37   0.302E+04
     38   0.302E+04
     39   0.302E+04
     40   0.302E+04
     41   0.300E+04
     42   0.300E+04
     43   0.300E+04
     44   0.300E+04
     45   0.301E+04
     46   0.301E+04
     47   0.301E+04
     48   0.301E+04
     49   0.301E+04
     50   0.301E+04
     51   0.301E+04
     52   0.301E+04
     53   0.301E+04
     54   0.302E+04
     55   0.302E+04
     56   0.302E+04
     57   0.302E+04
     58   0.302E+04
     59   0.302E+04
     60   0.302E+04
     61   0.300E+04
     62   0.300E+04
     63   0.300E+04
     64   0.301E+04
     65   0.301E+04
     66   0.301E+04
     67   0.301E+04
     68   0.301E+04
     69   0.301E+04
     70   0.301E+04
     71   0.301E+04
     72   0.301E+04
     73   0.302E+04
     74   0.302E+04
     75   0.302E+04
     76   0.302E+04
     77   0.302E+04
     78   0.302E+04
     79   0.302E+04
     80   0.302E+04
     81   0.300E+04
     82   0.300E+04
     83   0.301E+04
     84   0.301E+04
     85   0.301E+04
     86   0.301E+04
     87   0.301E+04
     88   0.301E+04
     89   0.301E+04
     90   0.301E+04
     91   0.301E+04
     92   0.302E+04
     93   0.302E+04
     94   0.302E+04
     95   0.302E+04
     96   0.302E+04
     97   0.302E+04
     98   0.302E+04
     99   0.302E+04
    100   0.302E+04
    101   0.300E+04
    102   0.301E+04
    103   0.301E+04
    104   0.301E+04
    105   0.301E+04
    106   0.301E+04
    107   0.301E+04
    108   0.301E+04
    109   0.301E+04
    110   0.301E+04
    111   0.302E+04
    112   0.302E+04
    113   0.302E+04
    114   0.302E+04
    115   0.302E+04
    116   0.302E+04
    117   0.302E+04
    118   0.302E+04
    119   0.302E+04
    120   0.302E+04
    121   0.301E+04
    122   0.301E+04
    123   0.301E+04
    124   0.301E+04
    125   0.301E+04
    126   0.301E+04
    127   0.301E+04
    128   0.301E+04
    129   0.301E+04
    130   0.302E+04
    131   0.302E+04
    132   0.302E+04
    133   0.302E+04
    134   0.302E+04
    135   0.302E+04
    136   0.302E+04
    137   0.302E+04
    138   0.302E+04
    139   0.302E+04
    140   0.302E+04
    141   0.301E+04
    142   0.301E+04
    143   0.301E+04
    144   0.301E+04
    145   0.301E+04
    146   0.301E+04
    147   0.301E+04
    148   0.301E+04
    149   0.302E+04
    150   0.302E+04
    151   0.302E+04
    152   0.302E+04
    153   0.302E+04
    154   0.302E+04
    155   0.302E+04
    156   0.302E+04
    157   0.302E+04
    158   0.302E+04
    159   0.302E+04
    160   0.303E+04
    161   0.301E+04
    162   0.301E+04
    163   0.301E+04
    164   0.301E+04
    165   0.301E+04
    166   0.301E+04
    167   0.301E+04
    168   0.302E+04
    169   0.302E+04
    170   0.302E+04
    171   0.302E+04
    172   0.302E+04
    173   0.302E+04
    174   0.302E+04
    175   0.302E+04
    176   0.302E+04
    177   0.302E+04
    178   0.302E+04
    179   0.303E+04
    180   0.303E+04
    181   0.301E+04
    182   0.301E+04
    183   0.301E+04
    184   0.301E+04
    185   0.301E+04
    186   0.301E+04
    187   0.302E+04
    188   0.302E+04
    189   0.302E+04
    190   0.302E+04
    191   0.302E+04
    192   0.302E+04
    193   0.302E+04
    194   0.302E+04
    195   0.302E+04
    196   0.302E+04
    197   0.302E+04
    198   0.303E+04
    199   0.303E+04
    200   0.303E+04
    201   0.301E+04
    202   0.301E+04
    203   0.301E+04
    204   0.301E+04
    205   0.301E+04
    206   0.302E+04
    207   0.302E+04
    208   0.302E+04
    209   0.302E+04
    210   0.302E+04
    211   0.302E+04
    212   0.302E+04
    213   0.302E+04
    214   0.302E+04
    215   0.302E+04
    216   0.302E+04
    217   0.303E+04
    218   0.303E+04
    219   0.303E+04
    220   0.303E+04
    221   0.301E+04
    222   0.301E+04
    223   0.301E+04
    224   0.301E+04
    225   0.302E+04
    226   0.302E+04
    227   0.302E+04
    228   0.302E+04
    229   0.302E+04
    230   0.302E+04
    231   0.302E+04
    232   0.302E+04
    233   0.302E+04
    234   0.302E+04
    235   0.302E+04
    236   0.303E+04
    237   0.303E+04
    238   0.303E+04
    239   0.303E+04
    240   0.303E+04
    241   0.301E+04
    242   0.301E+04
    243   0.301E+04
    244   0.302E+04
    245   0.302E+04
    246   0.302E+04
    247   0.302E+04
    248   0.302E+04
    249   0.302E+04
    250   0.302E+04
    251   0.302E+04
    252   0.302E+04
    253   0.302E+04
    254   0.302E+04
    255   0.303E+04
    256   0.303E+04
    257   0.303E+04
    258   0.303E+04
    259   0.303E+04
    260   0.303E+04
    261   0.301E+04
    262   0.301E+04
    263   0.302E+04
    264   0.302E+04
    265   0.302E+04
    266   0.302E+04
    267   0.302E+04
    268   0.302E+04
    269   0.302E+04
    270   0.302E+04
    271   0.302E+04
    272   0.302E+04
    273   0.302E+04
    274   0.303E+04
    275   0.303E+04
    276   0.303E+04
    277   0.303E+04
    278   0.303E+04
    279   0.303E+04
    280   0.303E+04
    281   0.301E+04
    282   0.302E+04
    283   0.302E+04
    284   0.302E+04
    285   0.302E+04
    286   0.302E+04
    287   0.302E+04
    288   0.302E+04
    289   0.302E+04
    290   0.302E+04
    291   0.302E+04
    292   0.302E+04
    293   0.303E+04
    294   0.303E+04
    295   0.303E+04
    296   0.303E+04
    297   0.303E+04
    298   0.303E+04
    299   0.303E+04
    300   0.303E+04
    301   0.302E+04
    302   0.302E+04
    303   0.302E+04
    304   0.302E+04
    305   0.302E+04
    306   0.302E+04
    307   0.302E+04
    308   0.302E+04
    309   0.302E+04
    310   0.302E+04
    311   0.302E+04
    312   0.303E+04
    313   0.303E+04
    314   0.303E+04
    315   0.303E+04
    316   0.303E+04
    317   0.303E+04
    318   0.303E+04
    319   0.303E+04
    320   0.303E+04
    321   0.302E+04
    322   0.302E+04
    323   0.302E+04
    324   0.302E+04
    325   0.302E+04
    326   0.302E+04
    327   0.302E+04
    328   0.302E+04
    329   0.302E+04
    330   0.302E+04
    331   0.303E+04
    332   0.303E+04
    333   0.303E+04
    334   0.303E+04
    335   0.303E+04
    336   0.303E+04
    337   0.303E+04
    338   0.303E+04
    339   0.303E+04
    340   0.304E+04
    341   0.302E+04
    342   0.302E+04
    343   0.302E+04
    344   0.302E+04
    345   0.302E+04
    346   0.302E+04
    347   0.302E+04
    348   0.302E+04
    349   0.302E+04
    350   0.303E+04
    351   0.303E+04
    352   0.303E+04
    353   0.303E+04
    354   0.303E+04
    355   0.303E+04
    356   0.303E+04
    357   0.303E+04
    358   0.303E+04
    359   0.304E+04
    360   0.304E+04
    361   0.302E+04
    362   0.302E+04
    363   0.302E+04
    364   0.302E+04
    365   0.302E+04
    366   0.302E+04
    367   0.302E+04
    368   0.302E+04
    369   0.303E+04
    370   0.303E+04
    371   0.303E+04
    372   0.303E+04
    373   0.303E+04
    374   0.303E+04
    375   0.303E+04
    376   0.303E+04
    377   0.303E+04
    378   0.304E+04
    379   0.304E+04
    380   0.304E+04
    381   0.302E+04
    382   0.302E+04
    383   0.302E+04
    384   0.302E+04
    385   0.302E+04
    386   0.302E+04
    387   0.302E+04
    388   0.303E+04
    389   0.303E+04
    390   0.303E+04
    391   0.303E+04
    392   0.303E+04
    393   0.303E+04
    394   0.303E+04
    395   0.303E+04
    396   0.303E+04
    397   0.304E+04
    398   0.304E+04
    399   0.304E+04
    400   0.304E+04
let $echo="no"
$ Return
$!#############################################################################
