$!****************************************************************************
$!
$! Build proc for MIPL module ressar75
$! VPACK Version 1.9, Wednesday, December 21, 2011, 14:13:46
$!
$! Execute by entering:		$ @ressar75
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
$ write sys$output "*** module ressar75 ***"
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
$ write sys$output "Invalid argument given to ressar75.com file -- ", primary
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
$   if F$SEARCH("ressar75.imake") .nes. ""
$   then
$      vimake ressar75
$      purge ressar75.bld
$   else
$      if F$SEARCH("ressar75.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ressar75
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ressar75.bld "STD"
$   else
$      @ressar75.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ressar75.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ressar75.com -mixed -
	-s ressar75.f -
	-i ressar75.imake -
	-p ressar75.pdf -
	-t tstressar75.pdf tstressar75.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ressar75.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	REVISION HISTORY
C	7-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER K,IJ,II,HI,LLIC,J,NLIC2,RECI,OREC,INDEX
      INTEGER S,L,LLINE,CONST,BLINE,STAT,IREC,NPIX
      INTEGER DCODE,ICAM,IN1,OUT1,NLOC,NLOCS
      LOGICAL DBUG,BLEM,RESEAU

      COMMON/CP/DCODE,ICAM,IN1,OUT1,NLOC,NLOCS,R,DBUG,BLEM,RESEAU

      INTEGER NLIC,NLINE,BI,NL,NS,NI,NO
      REAL*4 R,RBUF,LOC(2,413)
      INTEGER*2 A,PTBUF,VPTBUF,XBUF,YBUF,IORD,WORK(2,800)
      INTEGER*2 WORK1(800),WORK2(800),WORK3(800),WORKNDX(800)
      COMMON/C1/NLIC,NLINE,BI,NL,NS,NI,NO
      COMMON/C2/A(1204,1056),PTBUF(3,150),VPTBUF(3,90,2)
      COMMON/C3/XBUF(500),YBUF(500),RBUF(500),IORD(500)

      EQUIVALENCE (LOC,PTBUF),(WORK,A)

      COMMON/NPIX/NPIX,CONST

      CHARACTER*50 MSG
      CHARACTER*132 MSG1

      CALL IFMESSAGE('RESSAR75 version April 6, 1997')
      CALL XVEACTION('SA',' ')
      MSG(1:50)=' '
      MSG(1:7)='NUMBER='
      MSG(14:18)='LINE='
      MSG(25:31)='SAMPLE='
      MSG(38:44)='RADIUS='

      CALL RPARAM(LOC,RBUF,*999)
      IF (BLEM) CALL BLMLOC(LOC,RBUF,WORK,R,ICAM,NLOC,NLOCS,DBUG)
      NLOC=NLOC+1	! Dummy center included to space past end of picture
      LOC(1,NLOC)=9999

C          Convert centers to halfword and get line order
      DO K=1,NLOC
         XBUF(K)=LOC(2,K)+.5
         YBUF(K)=LOC(1,K)+.5
         WORK(1,K)=YBUF(K)
         WORK(2,K)=K
      ENDDO

      CALL SORTX(WORK,WORK1,WORK2,WORK3,WORKNDX,NLOC)
      
      if (nloc.gt.0) CALL MVE(2,NLOC,WORK(2,1),IORD,2,1)

      IF (DBUG) THEN
C         CALL PRNT(4,20,NLAB,'NLAB=.')
C         CALL PRNT(2,NLOC,IORD,'IORD.')
        WRITE(MSG1,401) IORD(1),IORD(3),IORD(5),IORD(7),IORD(9),
     &                  IORD(11),IORD(13),IORD(15)
401     FORMAT('IORD',5X,8(3X,I5))
        CALL XVMESSAGE(MSG1,' ')
        IJ = 17
        DO 420 II=1,(NLOC/8)-1
        WRITE(MSG1,405) IORD(IJ),IORD(IJ+2),IORD(IJ+4),IORD(IJ+6),
     &       IORD(IJ+8),IORD(IJ+10),IORD(IJ+12),IORD(IJ+14)
405     FORMAT(' ',8X,8(3X,I5))
        CALL XVMESSAGE(MSG1,' ')
        IJ = IJ + 16
420     CONTINUE
      ENDIF
      HI=1
      BI=0
      LLIC=0
C-----STORE RADIUS FOR EACH RESEAU MARK:1

      IF (NLOCS .GT. 0) THEN
        DO 450  II = 1,NLOCS
         RBUF(II) = R
450     CONTINUE
      ENDIF


c      IF (DBUG) CALL PRNT(7,NLOC,RBUF,'RADII=.')
      IF (DBUG) then
        write(msg1,550) rbuf(1),rbuf(2),rbuf(3),rbuf(4),rbuf(5),rbuf(6)
550     format('RADII=',3x,6(2x,f9.7))
        call xvmessage(msg1,' ')
        j = 7
        do 560 ii=1,16
          write(msg1,555) rbuf(j),rbuf(j+1),rbuf(j+2),rbuf(j+3),
     &                    rbuf(j+4),rbuf(j+5)
555       format(9x,6(2x,f9.7))
          call xvmessage(msg1,' ')
          j= j + 6
560     continue
        write(msg1,565) rbuf(j),rbuf(j+1)
565     format(9x,2x,f9.7,1x,f10.8)
        call xvmessage(msg1,' ')
      ENDIF
      NLIC2 = NLIC/2
      reci=0
      orec=0

      DO 100 INDEX=1,NLOC
      K=IORD(INDEX)
      S=XBUF(K)
      L=YBUF(K)
      R=NINT(RBUF(K))
      LLINE=L+NLIC2+CONST
      IF(LLINE.LT.1) GOTO 100
      IF(LLINE.EQ.LLIC) GOTO 88
      BLINE=LLIC+1

C          POSITION TO LINE L
      DO 80 NLINE=BLINE,LLINE
         BI=MOD(BI,NLIC)+1
         IF(NLINE.LE.NLIC) GOTO 48
            IF(NLINE.GT.NS+NLIC) GOTO 110
               orec=orec+1
	    CALL XVWRIT( OUT1, A(1,BI), STAT,' ')
   48    IF(NLINE.LE.NL) then
                 irec=irec+1
                CALL XVREAD( IN1, A(1,BI), STAT,' ')
         endif
   80 CONTINUE

   88 CONTINUE
      LLIC=LLINE
      CALL RESREM(A,PTBUF,NL,NS,NLIC,BI,L,S,R)
      IF (DBUG) THEN
         WRITE (MSG(8:12),'(I5)') K
         WRITE (MSG(19:23),'(I5)') L
         WRITE (MSG(32:36),'(I5)') S
         WRITE (MSG(45:49),'(F5.1)') R
         CALL XVMESSAGE(MSG(1:50),' ')
      ENDIF
  100 CONTINUE

  110 CONTINUE

C  WRITE CORRECTED PIXEL COUNT TO LABEL
      CALL XLADD( OUT1, 'HISTORY', 'PIX_CNT', NPIX, STAT, 
     . 'FORMAT', 'INT', 'ERR_ACT', 'SA',' ')
      WRITE(MSG1,800) NPIX
800   FORMAT('TOTAL # CORRECTED PIXELS =',7X,I8)
      CALL XVMESSAGE(MSG1,' ')
      CALL XVMESSAGE('RESSAR75 task completed',' ')
      RETURN

  999 CALL XVMESSAGE('***RESSAR75 task cancelled',' ')
      CALL ABEND
      END

      SUBROUTINE RPARAM(LOC,RBUF,*)
      IMPLICIT NONE
      INTEGER DCODE,ICAM,IN1,OUT1,NLOC,NLOCS
      LOGICAL DBUG,BLEM,RESEAU
      REAL*4 R
      COMMON/CP/DCODE,ICAM,IN1,OUT1,NLOC,NLOCS,R,DBUG,BLEM,RESEAU

      INTEGER NLIC,NLINE,BI,NL,NS,NI,NO
      COMMON/C1/NLIC,NLINE,BI,NL,NS,NI,NO

      INTEGER NPIX,CONST
      COMMON/NPIX/NPIX,CONST

      INTEGER STAT,IND,IVAL,ICOUNT,IDEF,IN2,NSA,N,I

      REAL*4 LOC(2,413),RBUF(500)
      INTEGER APAR(40)
      REAL*4 FPAR(27)
      CHARACTER*8 IFMT
      character*255 filename(2)
      LOGICAL XVPTST

C  **WARNING... IF NLIC IS CHANGED,VERTICAL FIDUCIAL MARK ROUTINE WILL
C  BE AFFECTED
      NLIC = 1056	!Number of lines in core (entire image)
      CONST = 11
      NPIX = 0

c     CALL XVP('INP',LOC,NI)	! Linux does not like this!  (lwk / aug2010)
c     CALL XVP('OUT',LOC,NO)
      CALL XVP('INP',filename,NI)
      CALL XVP('OUT',filename,NO)

      CALL XVUNIT(OUT1,'OUT',1,STAT,' ')
      CALL XVOPEN(OUT1,STAT,'U_FORMAT','HALF','OP','WRITE',' ')

      CALL XVUNIT(IN1,'INP',1,STAT,' ')
      CALL XVOPEN(IN1,STAT,'U_FORMAT','HALF',' ')

      CALL VOLABV2(IND,IN1,APAR)	!Read label to get camera S/N
      ICAM = APAR(2)
      IF(IND.GT.0) THEN
         CALL XVPARM('CAMERA',icam,ICOUNT,IDEF,1)
         IF (IDEF.EQ.1) THEN
            CALL XVMESSAGE('**Err reading camera s/n from label',' ')
            CALL XVMESSAGE('**Use CAMERA parameter',' ')            
            RETURN1
         ENDIF
      ENDIF
      CALL XVPARM('CAMERA',IVAL,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) ICAM=IVAL

      CALL XVGET(IN1,STAT,'FORMAT',IFMT,'NL',NL,'NS',NS,' ')
      IF (IFMT.EQ.'WORD'.OR.IFMT.EQ.'HALF') THEN
          DCODE = 2		!Halfword input
      ELSE
          DCODE = 1		!Byte input
      ENDIF

      CALL XVUNIT(IN2,'INP',2,STAT,' ')
      CALL XVOPEN(IN2,STAT,' ')
      CALL XVGET(IN2,STAT,'NS',NSA,' ')
      NLOC = NSA/2	     !Number of reseau marks
      NLOCS = NLOC
      CALL XVREAD(IN2,LOC,STAT,' ')
      CALL XVCLOSE(IN2,STAT,' ')

      CALL XVP('RADIUS',R,ICOUNT)

      CALL XVPARM('CENTERS',FPAR,ICOUNT,IDEF,27)
      IF (IDEF.EQ.0) THEN
          N = ICOUNT/3
          IF (ICOUNT.NE.3*N) GOTO 998
          DO I=1,ICOUNT,3
              NLOC = NLOC + 1
              LOC(1,NLOC) = FPAR(I)
              LOC(2,NLOC) = FPAR(I+1)
              RBUF(NLOC) = FPAR(I+2)
          ENDDO
      ENDIF

      RESEAU = .NOT.XVPTST('KEEPRESE')
      BLEM = .NOT.XVPTST('KEEPBLEM')
      DBUG = XVPTST('DBUG')

      IF (.NOT.RESEAU) THEN
          NLOC = 0
          NLOCS = 0
      ENDIF

      RETURN

  998 CALL XVMESSAGE('***Err in CENTERS parameter',' ')
      RETURN1
      END

      SUBROUTINE RESREM(A,PTBUF,NL,NS,NLIC,BI,L,S,R)
C          CIRCLE INTERPOLATION ROUTINE
      IMPLICIT NONE
      INTEGER NPIX,CONST
      INTEGER OFFSET,NLIC,SLINE,L,ELINE,MAX0,NL,MIN0
      INTEGER N,X,S,NS,Y,J,BI,LINE,DY,DX,SSAMP,ESAMP
      COMMON/NPIX/ NPIX,CONST
      INTEGER*2 A(1204,1056),PTBUF(3,150)
      INTEGER*4  MAX1

      REAL*4 R,R2,THETA,DTHETA,PI2

      DATA PI2/6.2831853072/

      MAX1=10000

      OFFSET=NLIC/2+CONST
      R2=R*R
C          COMPUTE STARTING AND ENDING LINE OF CIRCLE
      SLINE=L-R
      ELINE=L+R
      SLINE=MAX0(SLINE,2)
      ELINE=MIN0(ELINE,NL)
      IF(ELINE.LT.SLINE) GOTO 100
      N=0
      THETA=0.
      DTHETA=1./R
C          GET ALL SAMPLES AT RADIUS R FROM CENTER
   90 X=R*COS(THETA)+S+.5
      IF(X.LT.1.OR.X.GT.NS) GOTO 92
      Y=R*SIN(THETA)+L+.5
      IF(Y.LT.2.OR.Y.GT.NL) GOTO 92
      IF(N.NE.0)THEN
         IF(X.EQ.PTBUF(1,N).AND.Y.EQ.PTBUF(2,N)) GOTO 92
      ENDIF
      J=BI-OFFSET+Y-L
      IF(J.LT.1) J=J+NLIC
      IF(J.LT.1)THEN
            PRINT *,' ERROR'
      ENDIF
      N=N+1
      PTBUF(1,N)=X
      PTBUF(2,N)=Y
      PTBUF(3,N)=A(X,J)
   92 THETA=THETA+DTHETA
      IF(THETA.LT.PI2) GOTO 90

      IF(N.EQ.0) GOTO 100
      J=BI-OFFSET+SLINE-L
      IF(J.LT.1) J=J+NLIC
      IF(J.LT.1)THEN
            PRINT *,' ERROR'
      ENDIF

C          REPLACE PIXELS IN CIRCLE BY INTERPOLATING SAMPLES
C          ON CIRCUMFERENCE
      DO 95 LINE=SLINE,ELINE
         DY=LINE-L
         DX=SQRT(R2-DY*DY)
         SSAMP=S-DX
         ESAMP=S+DX
         IF(SSAMP.LT.1) SSAMP=1
         IF(ESAMP.GT.NS) ESAMP=NS
	 IF (ESAMP.GE.SSAMP) THEN
	   CALL EXTRAP(N,LINE,SSAMP,ESAMP,PTBUF,A(SSAMP,J),MAX1)
	   NPIX=NPIX+ESAMP-SSAMP
	 ENDIF
         J=MOD(J,NLIC)+1
   95 CONTINUE

  100 RETURN
      END
C Returns blemish locations (added on to LOC and RBUF)
C Outputs: LOC, RBUF
C Scratch buffer: BLEM
C
      SUBROUTINE BLMLOC(LOC,RBUF,BLEM,R,ICAM,NLOC,NLOCS,DBUG)
      IMPLICIT NONE
      INTEGER ICAM,NBLM,I,NLOC,NLOCS
      REAL*4 LOC(2,400),RBUF(500),BLEM(4,200),R,R1,R2
      LOGICAL DBUG
C      INTEGER APAR(40)

C      CALL XVUNIT( IN3, 'INP', 3, STAT)
C      CALL XVOPEN( IN3, STAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA')
C      CALL VOLABV2(IND,IN3,APAR)
C      NCAM = APAR(2)
C	IF(NCAM .NE. ICAM) THEN
C	      CALL XVMESSAGE('S/N DO NOT MATCH',' ')
C	      CALL PRNT(4,1,NCAM,'BLEM SN.')
C	      CALL PRNT(4,1,ICAM,'RES  SN.')
C	      CALL ABEND
C	ENDIF
C      CALL XVGET(IN3,STAT,'NS',NSB,'PIX_SIZE',PS)
C      NSB=NSB*PS
C      NBLM=NSB/16

      CALL VOBLEM(ICAM,NBLM,BLEM)

C        SAR PARAMETERS ARE RETURNED AS FLOATING POINT IN BUFFER BLEM 
C        IN SL,SS,NL,NS ORDER
      NBLM=NBLM/4
      IF (DBUG) CALL PRNT(4,1,NBLM,'NBLM.')

      DO I=1,NBLM
         R1=BLEM(3,I)/2.  ! NL/2
         R2=BLEM(4,I)/2.  ! NS/2
         LOC(1,NLOC+I)=BLEM(1,I)+R1 ! FIND CENTER LINE
         LOC(2,NLOC+I)=BLEM(2,I)+R2 ! FIND CENTER SAMPLE
         RBUF(NLOC+I) = SQRT(R1*R1+R2*R2) ! FIND RADIUS
      ENDDO

C      CALL XVREAD( IN3, BLEM, STAT)
C      IF (DBUG) CALL PRNT(7,NBLM*4,BLEM,'BLEM.')
C      CALL XVCLOSE( IN3, STAT)
C      CALL MVE(7,NLOCS,R,RBUF,1,1)
C      CALL MVE(7,NBLM,BLEM(4,1),RBUF(NLOC+1),4,1)
CC--- CALCULATE BLEMISH LOCATIONS
C      DO I=1,NBLM
C         M=BLEM(1,I)
C         LOC(1,NLOC+I)=BLEM(2,I)+LOC(1,M)
C         LOC(2,NLOC+I)=BLEM(3,I)+LOC(2,M)
C      ENDDO
      NLOC=NLOC+NBLM
      IF (DBUG) THEN
           CALL PRNT(4,1,NLOC,'NLOC.')
           CALL PRNT(7,NLOC*2,LOC,'LOCS.')
      ENDIF
      RETURN
      END


C	SUBROUTINE SORTX(BUF,N)
C-----THIS ROUTINE WILL SWAP THE HALFWORDS OF THE FULLWORD BUFFER
C-----SO THAT VAX WILL SORT LIKE THE IBM.
C	INTEGER*2 BUF(2,N),J
c	DO I=1,N
c           J=BUF(1,I)
c	   BUF(1,I)=BUF(2,I)
c	   BUF(2,I)=J
c        ENDDO
C	CALL I4SORT(BUF,BUF,N)
c	DO I=1,N
c	   J=BUFOUT(1,I)
c	   BUFOUT(1,I)=BUFOUT(2,I)
c	   BUFOUT(2,I)=J
c        ENDDO
c	RETURN
c	END



C********************************************C
C RESSAR75 halfword sort subroutine          C
C********************************************C
	SUBROUTINE SORTX(BUF,BUFVAL1,BUFVAL2,BUFVAL3,BUFNDX,N)
C
C
        INTEGER II,JJ,N
	INTEGER*2 BUF(2,N)
        INTEGER*2 BUFVAL1(N), BUFVAL2(N)
        INTEGER*4 BUFVAL3(N), BUFNDX(N)        

        DO 90055 II=1,N
        BUFVAL3(II) = ((BUF(1,II) * 32768) + BUF(2,II))
        BUFVAL2(II) = BUF(2,II)
        BUFVAL1(II) = BUF(1,II)
        BUFNDX(II)  = II
90055   CONTINUE


	CALL ISORTP(BUFVAL3,1,N,BUFNDX)

        DO  90065 II=1,N
        JJ = BUFNDX(II)
        BUF(1,II) = BUFVAL1(JJ)
        BUF(2,II) = BUFVAL2(JJ)
90065   CONTINUE


	RETURN
	END




      SUBROUTINE VOBLEM(SN,NUM,BUF)
C        VIKING ORBITER NOMINAL BLEMISH RETRIEVAL SUBROUTINE
      IMPLICIT NONE
      INTEGER SN,NUM
      INTEGER BUF(1)
      REAL SN4(96)
      REAL SN6(156)
      REAL SN7(152)
      REAL SN8(256)
      REAL SN8A(128)
      REAL SN8B(128)

      EQUIVALENCE(SN8(1),SN8A)
      EQUIVALENCE(SN8(129),SN8B)


      DATA SN4/
     *    25.5,  38.,  2.,2.,  84.5,1115.5,6.,4., 163.0,1000., 5.,5.,
     *   172.,  725.5, 5.,4., 379.5,  97.5,2.,2., 384.,  488.,5.,3.,
     *   700.5, 422.5, 6.,4., 714.,  351.5,9.,8., 762.5, 430., 4.,3.,
     *   783.5, 726.5, 4.,2., 806.5, 351., 4.,3., 821.5, 258.5,2.,2.,
     *   834.5, 945.,  6.,3., 846.,  645., 5.,3., 857.,  308., 5.,3.,
     *   895.5, 214.5, 2.,2., 921.5, 513., 4.,3., 981.5, 481., 6.,5.,
     *   998.,  425.,  5.,3.,1026.5, 403.5,2.,4.,1030.,  797., 7.,3.,
     *  1043.5,1007.5, 8.,4.,
     *   297.,  185.5, 3.,2.,1055., 1047., 3.,7./  ! ETR ADDITIONS

      DATA SN6/
     *    74.5, 315., 12.,5., 114.5,1033.5,6.,4., 128.,  600., 5.,5.,
     *   159., 1028.,  2.,5., 167.,  775., 2.,3., 181.,   59., 5.,4.,
     *   165.,  542.,  4.,5., 292.,  447., 3.,3., 338.,  995.5,3.,6.,
     *   377.5, 280.5, 2.,2., 385., 1129., 5.,7., 444., 1155., 3.,3.,
     *   453.5, 325.5,24.,6., 502.,  206.,19.,5., 615.,  178., 4.,3.,
     *   690., 1041.5, 5.,6., 704.,  363., 3.,3., 710.5, 363., 2.,3.,
     *   711.5, 653.,  4.,5., 788.,  432., 5.,5., 840.5, 263., 2.,3.,
     *   841.5, 198.,  2.,3., 843.5, 514., 6.,7., 860.,  132.5,4.,4.,
     *   950.,  684.,  7.,7., 975.,  955., 4.,3.,1012.5, 481.5,2.,2.,
     *  1014.5, 914.5, 6.,6.,1017.5, 209., 2.,3.,1025.5, 532., 2.,3.,
     *  1034.5, 384.,  2.,3.,
C               ETR ADDITIONS
     *    23.,  638.5, 3.,6., 435.,   72.5,6.,4., 451.5, 46.,  4.,3.,
     *   460.,  633.,  9.,3., 658.,  245.5,3.,2., 862.,1145.5, 3.,2.,
     *   877.5, 962.5, 2.,2., 894., 1179.5,3.,4./

      DATA SN7/
     *    42.5, 514.5, 4.,6.,  56., 891.5, 5.,8., 180., 220.,  5.,8.,
     *   216.,  979.,  5.,7., 222.5,454.5,30.,18.,253.5,790.5, 4.,4.,
     *   261.5, 165.,  4.,5., 263.5,714.5, 6.,6., 267.,1064.,  5.,9.,
     *   270.5,1101.5, 2.,4., 299.,1049.5, 3.,4., 331.5,537.5, 8.,8.,
     *   373.5, 447.5, 2.,4., 373.5,632.5, 4.,4., 400.,1025.5, 5.,6.,
     *   423.,  910.5, 3.,4., 473., 257.,  5.,5., 541.5,382.,  4.,3.,
     *   556.,  290.5, 5.,6., 627.5,874.,  4.,5., 629., 413.5, 3.,4.,
     *   682.,  755.,  3.,5., 731., 871.,  3.,3., 733.5,779.5, 4.,4.,
     *   807.,  227.,  3.,4., 870.5,978., 10.,11.,906., 894.,  5.,5.,
     *   930.,  887.5, 7.,6., 980.5,201.5, 4.,6.,1000., 592.5, 3.,4.,
C               ETR ADDITIONS
     *    50.5, 294.,  4.,3., 163. ,163.5, 7.,8., 208., 266.5,13.,8.,
     *   274.,  589.,  3.,3., 316.5,165.5, 4.,6., 414., 504.5, 3.,4.,
     *   695.,  646.,  2.,3.,1037.,1094. , 3.,2./

      DATA SN8A/
     *    70.5,1167.,  4.,5., 139., 807.5,5.,6.,  184.,150.,  3.,5.,
     *   192.5, 781.,  2.,5., 249., 478.5,5.,4.,  263.,237.,  3.,5.,
     *   270.,  625.,  3.,5., 310.5,217.5,2.,4.,  320.5,451., 6.,9., 
     *   337.,  510.,  5.,9., 338.5,853., 2.,3.,  339.,910.5, 3.,4.,
     *   342.,  351.,  5.,7., 345., 244.5,7.,8.,  350.5,182., 2.,5.,
     *   354.5, 816.5, 4.,4., 382., 660.5,1.,4.,  387.5,333.5,2.,4.,
     *   390.5, 288.5, 2.,2., 391., 520.,11.,15., 414.5,339., 4.,5.,
     *   455.5, 528.5, 2.,4., 457., 436.5,3.,4.,  478.5,610., 4.,5.,
     *   480.,  622.,  3.,7., 495.5,649., 2.,3.,  507.5,55.5, 2.,4.,
     *   527.5,1159.5, 2.,4., 565.5,271.5,4.,4.,  572.,111.5, 3.,6.,
     *   570.,  208.5, 5.,6., 605.5,542.5,2.,6./

      DATA SN8B/
     *   606.5,332., 4.,7.,
     *   628.,  961.5, 3.,4., 639.5,871.5,4.,8.,  650.5,441.5,2.,4.,
     *   662., 1110.5, 5.,6., 669.5,348., 2.,5.,  679.5,463., 6.,11.,
     *   690.5, 769.,  2.,5., 737.5,585., 2.,5.,  751.5,762.5,2.,6.,
     *   766.5, 671.5, 2.,6., 778.5,549.5,4.,6.,  784.5,885.5,2.,6.,
     *   800.5, 761.5, 2.,4., 824., 204., 3.,7.,  829.,584.,  5.,5.,
     *   861.5, 694.,  2.,5., 867.5,881., 2.,5.,  868.5,651., 2.,5., 
     *   871.,  482.5, 3.,4., 903.5,502.5,2.,6.,  951.,1129., 3.,5.,
     *   955.,  682.5, 3.,6., 988., 855., 3.,7.,  992.5,166.5,5.,8.,
     *  1006.5, 239.,  4.,9.,1023.5,263., 6.,13.,1035.5,760., 2.,7.,
     *  1038., 1135.,  7.,9., 348., 898.5,5.,10.,
     *   321.,  908.,  3.,3., 486., 432.5,2.,4./


      IF(SN.EQ.4)THEN
         NUM=96
         CALL GETBLEM(NUM,SN4,BUF)
      ELSE IF(SN.EQ.6)THEN
         NUM=156
         CALL GETBLEM(NUM,SN6,BUF)
      ELSE IF(SN.EQ.7)THEN
         NUM=152
         CALL GETBLEM(NUM,SN7,BUF)
      ELSE IF(SN.EQ.8)THEN
         NUM=256
         CALL GETBLEM(NUM,SN8,BUF)
      ELSE
         CALL PRNT(4,1,SN,'voblem:ILLEGAL SERIAL NUMBER,SN=.')
         CALL ABEND
      ENDIF
      RETURN
      END

      SUBROUTINE GETBLEM(NUM,INBUF,OUTBUF)
      IMPLICIT NONE
      INTEGER NUM,I
      REAL INBUF(4,NUM/4),OUTBUF(4,NUM/4)
C     INBUF(1,I)IS CENTER LINE OF BLEMISH
C     INBUF(2,I)IS CENTER SAMPLE
C     INBUF(3,I)IS NUMBER OF LINES
C     INBUF(4,I)IS NUMBER OF SAMPLES
      DO I=1,NUM/4
         OUTBUF(1,I)=INBUF(1,I)-INBUF(3,I)/2.
         OUTBUF(2,I)=INBUF(2,I)-INBUF(4,I)/2.
         OUTBUF(3,I)=INBUF(3,I)
         OUTBUF(4,I)=INBUF(4,I)
      ENDDO
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ressar75.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ressar75

   To Create the build file give the command:

		$ vimake ressar75			(VMS)
   or
		% vimake ressar75			(Unix)


************************************************************************/


#define PROGRAM	ressar75
#define R2LIB

#define MODULE_LIST ressar75.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77

/*#define DEBUG		/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ressar75.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=2
PARM OUT      TYPE=STRING
PARM CAMERA   TYPE=INTEGER	     VALID=(4:7)	DEFAULT=4
PARM RADIUS   TYPE=REAL		     VALID=(2.:10.)	DEFAULT=5.0
PARM CENTERS  TYPE=REAL    COUNT=3:18			DEFAULT=(0.,0.,0.)
PARM DBUG     TYPE=KEYWORD COUNT=0:1 VALID=DBUG		DEFAULT=--
PARM KEEPRESE TYPE=KEYWORD COUNT=0:1 VALID=KEEPRESE	DEFAULT=--
PARM KEEPBLEM TYPE=KEYWORD COUNT=0:1 VALID=KEEPBLEM	DEFAULT=--
END-PROC
.TITLE
"ressar75"  --  Viking Orbiter reseau removal
.HELP
PURPOSE:

"ressar75" is a VICAR applications program which removes the reseau marks
and camera blemishes from Viking Orbiter images.

EXECUTION:

	ressar75 INP=(A,RES) OUT=B user-parameters...
where

	A is the input Viking Orbiter image,
	RES is a reseau location record generated by program RESLOCVO
            containing the reseau locations for image A.
	B is the output image, with reseau and/or blemishes removed.

The input image must be in standard VO format (1204x1056 with no geometric
correction applied).  The entire image is processed (i.e. no size field is
accepted).  The input may be in byte or 16-bit integer format.  The output
will be in the same format as the input.

.page
OPERATION:

Each reseau mark is removed by interpolating over samples a specified radius
(see RADIUS parameter) from its center, as defined in the reseau location
record RES.  Each camera blemish (see VO Calibration Report) is similary
treated, with their centers and radii retrieved from tables built into the
program.

The keywords KEEPRESE and KEEPBLEM may be used to suppress removal of either
reseau or blemishes.

The CENTERS parameter may be used to specify circular areas of the image
to be interpolated over.

Because line 1 of all Viking Orbiter images is normally saturated, it is
not processed, and is simply copied to the output image.

.page
EXAMPLE:

	reslocvo A RES
	ressar75 (A,RES) B

Program "reslocvo" is used to locate the reseau and store the line-sample
coordinates of each reseau mark in RES.  "ressar75" is then used to remove
the reseau and camera blemishes.


TIMING: None available for the VAX

WRITTEN BY: Gary Yagi      Aug. 27, 1980
CONVERTED TO VAX BY: C.C.Avis   14 APR 83   
COGNIZANT PROGRAMMER: Joel Mosher 

REVISION HISTORY:
  7 APR 97  GMY   Allow processing when no flight label is present (87985)
  6 JUL 94  Meredith Cox (CRI) -- Made portable for UNIX
 10 Jul 91  CCA   Modified to call new VOLABV2
  4 Mar 90  GMY   Modest change to test file
 31 OCT 87  GMY   Convert parameter processor to VICAR2
  8 DEC 86  JAM   fix keepblem,keepreseau
  8 APR 86  JAM   I/O CONVERTED TO VICAR2
 14 APR 83  CCA   CONVERTED TO VAX
 13 JAN 81  CCA   IMPLEMENT 3RD INPUT FOR BLEM REMOVAL
 27 AUG 80  GMY   INITIAL RELEASE
.LEVEL1
.VARIABLE INP
 Input VO image
 and reseau location
 record.
.VARIABLE OUT
 Output image
.VARIABLE DBUG
 KEYWORD-OPTIONAL
 Enables diagnotic
 printout.
.VARIABLE RADIUS
 REAL--OPTIONAL
 Radius (size) of
 typical reseau mark.
.VARIABLE CAMERA
 INTEGER--OPTIONAL
 camera serial no. 
 of input frame
.VARIABLE KEEPRESE
 KEYWORD--OPTIONAL
 Suppresses removal
 of the reseau
.VARI KEEPBLEM
 KEYWORD--OPTIONAL
 Suppresses removal
 of camera blemishes
.VARIABLE CENTERS
 3 to 18 REAL numbers
 centers (line,sample)
 and radii of circles
 to be removed.
.LEVEL2
.VARIABLE INP
 STRING--REQUIRED
 Input VO image and
 reseau location record.
 E.g. INP=(IMG,RES)
.VARIABLE OUT
 STRING--REQUIRED
 Output image
.VARIABLE DBUG
 KEYWORD--OPTIONAL
 DBUG should only be specified when debugging the program. This
keyword will cause diagnostic messages to be printed. 
.VARIABLE RADIUS
 REAL--OPTIONAL
 Specifies the radius of the circle enclosing each reseau mark. The accepted
range is 2.0 <= RADIUS <=10.0. The default is RADIUS=5.  (This value is rounded
to the nearest integer by the program).
.VARIABLE CAMERA
 INTEGER--OPTIONAL
 Camera serial number of the input frame. This parameter may be used to
override the camera serial number present in the Viking picture label.
Valid values are:
	7=VO-1A		8=VO-2A
	4=VO-1B		6=VO-2B
.VARIABLE KEEPRESE
 KEYWORD--OPTIONAL
 KEEPRESE specifies the reseau are not to be removed. 
.VARIABLE KEEPBLEM
 KEYWORD--OPTIONAL
 KEEPBLEM specifies blemishes are not to be removed.
.VARIABLE CENTERS
 REAL--OPTIONAL
 Groups of three REAL numbers specifying the center and radius of a
circle to be removed. (line, sample, radius). The entire circle need
not lie completely within the image. 
 The accepted range for radius is:
	2.0 <= RADIUS <= 10.0
 No more than 6 centers may be defined. (A TAE LIMIT)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstressar75.pdf
procedure
refgbl $echo
refgbl $autousage
body
!let $autousage = "none"
let _onfail="continue"
Write " Test for VICAR program RESSAR75"
Write " "
let $echo="yes"
refgbl $syschar
local PATH TYPE=STRING
if ($syschar(1)="VAX_VMS")
  let path="WMS_TEST_WORK:[TESTDATA.VO]"
else
  let path="/project/test_work/testdata/vo/"
end-if 

reslocvo &"path"408b25.img res			!Find the reseau

list &"path"408b25.img (131,196,10,10)		!List reseau mark (before)
list &"path"408b25.img (381,485,10,10)		!List blemish (before)

! Test 1: Remove reseau marks, keep blemishes
ressar75 (&"path"408b25.img,res) a came=4 'keepblem
list a (131,196,10,10)				!List reseau mark (after)
list a (381,485,10,10)				!List blemish (after)

! Test 2: Remove both reseau marks and blemishes
ressar75 (&"path"408b25.img,res) a came=4 
list a (131,196,10,10)				!List reseau mark (after)
list a (381,485,10,10)				!List blemish (after)

! Test 3: Remove reseau marks and one extra spot.
ressar75 (&"path"408b25.img,res) a came=4 center=(331,537,10) 'keepblem
list a (131,196,10,10)				!List reseau mark (after)
list &"path"408b25.img (321,527,20,18)		!List spot (before)
list a (321,527,20,18)				!List spot (after)

! Test 4: Remove blemishes, keep reseau
ressar75 (&"path"408b25.img,res) a came=4 'keepres
list a (131,196,10,10)				!List reseau mark (after)
list a (381,485,10,10)				!List blemish (after)
end-proc
$!-----------------------------------------------------------------------------
$ create tstressar75.log_solos
tstressar75
 Test for VICAR program RESSAR75
 
refgbl $syschar
local PATH TYPE=STRING
if ($syschar(1)="VAX_VMS")
else
  let path="/project/test_work/testdata/vo/"
end-if
reslocvo /project/test_work/testdata/vo/408b25.img res
Beginning VICAR task reslocvo
RESLOCVO version 01-JULY-94
NLW= 5 NSW= 5 NHOR=19 NVER=19 INTERP=1
TOT BUF=       57117
MINIMUM CORRELATION VALUE=    0.55715299
AVERAGE VALUE=      0.69429654
RMS=  2.33
RESEAU MARK  13 HAD THE LARGEST RESIDUE OF  4.5 PIXELS ( 131.4,  83.3) NOM=( 133.5,  79.3)
MARK  13 CHANGED TO ( 131.7,  82.6)
RMS=  2.32
RESEAU MARK   1 HAD THE LARGEST RESIDUE OF  4.2 PIXELS (   2.3,  22.1) NOM=(   4.3,  18.4)
MARK   1 CHANGED TO (   2.4,  21.9)
RMS=  2.31
RESEAU MARK   2 HAD THE LARGEST RESIDUE OF  4.2 PIXELS (   5.6, 140.5) NOM=(   7.8, 136.9)
MARK   2 CHANGED TO (   5.8, 140.2)
RMS=  2.30
RESEAU MARK   1 HAD THE LARGEST RESIDUE OF  4.0 PIXELS (   2.4,  21.9) NOM=(   4.3,  18.4)
RESLOCVO task completed
list /project/test_work/testdata/vo/408b25.img (131,196,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
     Samp   196     198     200     202     204
   Line
    131      50  52  54  56  56  54  56  54  52  56
    132      52  58  58  60  58  56  54  54  52  56
    133      54  52  50  42  30  30  38  48  50  54
    134      54  50  38  16   0   0  12  34  44  48
    135      56  50  34   8   0   0   0  24  40  46
    136      54  48  36  14   0   0   6  30  44  48
    137      50  52  50  46  38  34  36  42  46  48
    138      52  52  54  54  50  50  48  48  48  46
    139      52  52  50  52  52  52  52  48  48  52
    140      52  52  54  50  48  48  48  48  50  48
list /project/test_work/testdata/vo/408b25.img (381,485,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
     Samp   485     487     489     491     493
   Line
    381      54  54  52  54  52  52  52  52  52  52
    382      52  56  54  52  54  50  52  54  52  54
    383      56  52  54  56  54  54  54  54  54  54
    384      54  56  54  54  54  52  54  54  52  52
    385      52  52  54  54  54  54  54  52  56  50
    386      54  54  54  54  54  52  54  54  54  50
    387      54  52  52  54  52  56  50  52  54  52
    388      52  50  56  52  52  52  52  56  52  54
    389      50  52  54  54  52  54  54  54  52  52
    390      52  52  52  52  50  54  54  54  52  52
ressar75 (/project/test_work/testdata/vo/408b25.img,res) a came=4 'keepblem
Beginning VICAR task ressar75
RESSAR75 version April 6, 1997
TOTAL # CORRECTED PIXELS =           7180
RESSAR75 task completed
list a (131,196,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:14 2011
     Samp   196     198     200     202     204
   Line
    131      50  52  52  52  52  52  52  52  52  56
    132      52  52  52  52  52  52  52  52  52  56
    133      53  52  52  52  52  51  51  51  51  54
    134      53  52  52  52  51  51  51  50  49  48
    135      52  52  52  51  51  51  50  49  48  46
    136      52  52  52  51  51  51  50  49  48  48
    137      50  51  51  51  51  51  50  48  46  48
    138      52  52  51  51  51  51  50  48  48  46
    139      52  52  50  52  52  52  52  48  48  52
    140      52  52  54  50  48  48  48  48  50  48
list a (381,485,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:14 2011
     Samp   485     487     489     491     493
   Line
    381      54  54  52  54  52  52  52  52  52  52
    382      52  56  54  52  54  50  52  54  52  54
    383      56  52  54  56  54  54  54  54  54  54
    384      54  56  54  54  54  52  54  54  52  52
    385      52  52  54  54  54  54  54  52  56  50
    386      54  54  54  54  54  52  54  54  54  50
    387      54  52  52  54  52  56  50  52  54  52
    388      52  50  56  52  52  52  52  56  52  54
    389      50  52  54  54  52  54  54  54  52  52
    390      52  52  52  52  50  54  54  54  52  52
ressar75 (/project/test_work/testdata/vo/408b25.img,res) a came=4
Beginning VICAR task ressar75
RESSAR75 version April 6, 1997
TOTAL # CORRECTED PIXELS =           7780
RESSAR75 task completed
list a (131,196,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:15 2011
     Samp   196     198     200     202     204
   Line
    131      50  52  52  52  52  52  52  52  52  56
    132      52  52  52  52  52  52  52  52  52  56
    133      53  52  52  52  52  51  51  51  51  54
    134      53  52  52  52  51  51  51  50  49  48
    135      52  52  52  51  51  51  50  49  48  46
    136      52  52  52  51  51  51  50  49  48  48
    137      50  51  51  51  51  51  50  48  46  48
    138      52  52  51  51  51  51  50  48  48  46
    139      52  52  50  52  52  52  52  48  48  52
    140      52  52  54  50  48  48  48  48  50  48
list a (381,485,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:15 2011
     Samp   485     487     489     491     493
   Line
    381      54  54  52  54  52  52  52  52  52  52
    382      52  56  54  53  52  50  52  54  52  54
    383      56  54  54  53  53  53  54  54  54  54
    384      54  54  54  53  53  53  54  54  52  52
    385      52  53  53  53  53  53  54  52  56  50
    386      54  54  53  53  53  52  54  54  54  50
    387      54  52  52  54  52  56  50  52  54  52
    388      52  50  56  52  52  52  52  56  52  54
    389      50  52  54  54  52  54  54  54  52  52
    390      52  52  52  52  50  54  54  54  52  52
ressar75 (/project/test_work/testdata/vo/408b25.img,res) a came=4 center=(331,537,10) 'keepblem
Beginning VICAR task ressar75
RESSAR75 version April 6, 1997
TOTAL # CORRECTED PIXELS =           7476
RESSAR75 task completed
list a (131,196,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:16 2011
     Samp   196     198     200     202     204
   Line
    131      50  52  52  52  52  52  52  52  52  56
    132      52  52  52  52  52  52  52  52  52  56
    133      53  52  52  52  52  51  51  51  51  54
    134      53  52  52  52  51  51  51  50  49  48
    135      52  52  52  51  51  51  50  49  48  46
    136      52  52  52  51  51  51  50  49  48  48
    137      50  51  51  51  51  51  50  48  46  48
    138      52  52  51  51  51  51  50  48  48  46
    139      52  52  50  52  52  52  52  48  48  52
    140      52  52  54  50  48  48  48  48  50  48
list /project/test_work/testdata/vo/408b25.img (321,527,20,18)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
     Samp   527     529     531     533     535     537     539     541     543
   Line
    321      54  56  56  54  54  58  56  54  58  58  58  60  58  56  56  58  60  60
    322      60  56  56  58  56  60  58  56  54  56  54  58  56  56  56  56  58  58
    323      54  58  54  56  56  56  58  58  58  58  54  58  58  56  56  56  58  58
    324      58  56  54  56  58  56  58  56  58  60  56  58  58  58  58  56  58  56
    325      58  56  54  54  56  54  58  60  56  56  56  56  58  58  56  56  58  54
    326      54  58  56  56  54  56  56  56  56  54  54  56  56  56  56  56  54  56
    327      58  56  58  54  54  56  58  56  54  58  58  54  56  56  56  58  56  56
    328      58  56  54  58  56  58  56  56  56  54  58  58  58  56  56  58  58  56
    329      58  58  56  58  58  56  58  56  56  54  56  54  54  54  56  56  58  58
    330      56  58  56  58  58  58  58  56  58  56  56  56  58  58  58  56  56  56
    331      58  56  58  56  56  56  56  56  56  54  58  56  58  56  56  58  58  58
    332      58  58  54  58  58  56  58  54  54  58  58  56  58  56  58  58  58  56
    333      58  56  58  58  56  56  56  58  58  54  56  56  56  56  56  56  58  58
    334      58  54  58  58  58  58  58  58  58  56  58  58  58  56  56  56  56  58
    335      58  54  56  56  58  58  58  56  60  58  54  56  56  56  58  56  56  58
    336      56  56  56  54  58  56  58  56  56  54  58  56  56  54  56  56  56  56
    337      56  56  58  58  54  54  58  58  58  60  58  56  58  54  58  54  56  56
    338      58  56  56  58  56  58  56  56  56  56  56  58  58  56  56  54  56  58
    339      56  58  58  56  56  54  58  54  58  58  58  58  58  58  58  54  54  54
    340      56  56  58  56  56  54  54  58  58  58  58  56  56  58  56  56  54  54
list a (321,527,20,18)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:16 2011
     Samp   527     529     531     533     535     537     539     541     543
   Line
    321      54  56  56  54  54  58  56  54  58  58  58  60  58  56  56  58  60  60
    322      60  56  56  58  56  60  58  57  57  58  58  58  58  57  56  56  58  58
    323      54  58  54  56  56  57  58  57  57  57  57  57  57  57  56  57  58  58
    324      58  56  54  56  56  57  57  57  57  57  57  57  57  57  57  57  57  56
    325      58  56  54  56  56  57  57  57  57  57  57  57  57  57  57  57  57  57
    326      54  58  56  56  56  56  57  57  57  57  57  57  57  57  57  56  56  56
    327      58  56  56  56  56  56  57  57  57  57  57  57  57  57  56  56  56  56
    328      58  57  57  56  56  57  57  57  57  57  57  57  57  56  56  56  56  56
    329      58  57  57  57  57  57  57  57  57  57  57  57  56  56  56  56  56  56
    330      56  57  57  57  57  57  57  57  56  56  56  56  56  56  56  56  56  56
    331      58  57  57  57  57  57  56  56  56  56  56  56  56  56  56  56  56  56
    332      58  57  57  57  56  56  56  56  56  56  56  56  56  56  56  56  56  56
    333      58  56  56  56  56  56  56  56  56  56  56  56  56  56  56  56  56  56
    334      58  54  55  56  56  56  56  56  56  56  56  56  56  56  56  56  56  56
    335      58  54  55  56  56  56  56  56  56  56  56  56  56  56  56  56  56  56
    336      56  56  56  57  56  56  56  56  56  56  56  56  56  56  56  56  56  56
    337      56  56  58  57  57  56  56  56  56  56  56  56  56  56  56  56  56  56
    338      58  56  56  58  57  56  56  56  56  56  56  56  56  56  55  55  56  58
    339      56  58  58  56  56  55  56  56  57  56  56  56  56  56  55  54  54  54
    340      56  56  58  56  56  54  54  58  57  56  56  56  56  56  56  56  54  54
ressar75 (/project/test_work/testdata/vo/408b25.img,res) a came=4 'keepres
Beginning VICAR task ressar75
RESSAR75 version April 6, 1997
TOTAL # CORRECTED PIXELS =            600
RESSAR75 task completed
list a (131,196,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:17 2011
     Samp   196     198     200     202     204
   Line
    131      50  52  54  56  56  54  56  54  52  56
    132      52  58  58  60  58  56  54  54  52  56
    133      54  52  50  42  30  30  38  48  50  54
    134      54  50  38  16   0   0  12  34  44  48
    135      56  50  34   8   0   0   0  24  40  46
    136      54  48  36  14   0   0   6  30  44  48
    137      50  52  50  46  38  34  36  42  46  48
    138      52  52  54  54  50  50  48  48  48  46
    139      52  52  50  52  52  52  52  48  48  52
    140      52  52  54  50  48  48  48  48  50  48
list a (381,485,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:RESSAR75  User:lwk       Date_Time:Wed Dec 21 14:12:17 2011
     Samp   485     487     489     491     493
   Line
    381      54  54  52  54  52  52  52  52  52  52
    382      52  56  54  53  52  50  52  54  52  54
    383      56  54  54  53  53  53  54  54  54  54
    384      54  54  54  53  53  53  54  54  52  52
    385      52  53  53  53  53  53  54  52  56  50
    386      54  54  53  53  53  52  54  54  54  50
    387      54  52  52  54  52  56  50  52  54  52
    388      52  50  56  52  52  52  52  56  52  54
    389      50  52  54  54  52  54  54  54  52  52
    390      52  52  52  52  50  54  54  54  52  52
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
