$!****************************************************************************
$!
$! Build proc for MIPL module tfilt
$! VPACK Version 1.9, Thursday, December 20, 2012, 16:28:31
$!
$! Execute by entering:		$ @tfilt
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
$ write sys$output "*** module tfilt ***"
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
$ write sys$output "Invalid argument given to tfilt.com file -- ", primary
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
$   if F$SEARCH("tfilt.imake") .nes. ""
$   then
$      vimake tfilt
$      purge tfilt.bld
$   else
$      if F$SEARCH("tfilt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tfilt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tfilt.bld "STD"
$   else
$      @tfilt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tfilt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tfilt.com -mixed -
	-s tfilt.f -
	-i tfilt.imake -
	-p tfilt.pdf -
	-t tsttfilt.pdf tsttfilt.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tfilt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      Include   'VICMAIN_FOR'
      Subroutine  Main44
      External    FiltB
      Common/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
c
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER totlen
c
      call xvmessage(' *** program TFILT version 19-Dec-2012 ***',' ')

      call FILTA
      L1 = Totlen * 4
      L2 = Totlen * 4
      L3 = Totlen * 4
      L4 = Totlen * 4
      L5 = Totlen * 4
      L6 = NPIX * 4
      LTOT = L1 + L2 + L3 + L4 + L5 + L6
      call STACKA(9,FiltB,6,L1,L2,L3,L4,L5,L6,LTOT)
c
999   Continue
c
      Return
      End
c
c********************************************************************
c
      Subroutine FiltB(SUMDN,L1,COUNT,L2,IN,L3,LAST,L4,NEXT,L5,OUT,L6,
     *LTOT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER COUNT(L2/4)
      real*4 SUMDN(L1/4), IN(L3/4), LAST(L4/4), NEXT(L5/4), OUT(NPIX)
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2,status,
     * wunit1,RUNIT,wunit2,wunit3
      real*4 thresh,dnsum,lower,upper

      if (L1+L2+L3+L4+L5+L6 .NE. LTOT) then
        call xvmessage('*** STACKA Error ***', ' ')
	call Abend
      endif
c
c   Copy RUNIT to wunit1 and wunit2.  Rearranging such that Line I of wunit1
c   Contins the Next (Highest) Line in the Filter Window for Line I of RUNIT,
c   and Line I of wunit2 the Last (Lowest) Line in the Window for Line I-1

c  band loop, over entire subroutine (not reflected in indentation!):
      do ib=1,nb

c  for multi-band case, re-initialize wunit2 and wunit1:
      if (ib.gt.1) then
        call xvclose( wunit2, status, ' ')
        call xvopen( wunit2, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
        call xvclose( wunit1, status, ' ')
        call xvopen( wunit1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      do LINE=2,NLW2
          IREC2 = NLW2+SLIN+1-LINE
          call xvread( RUNIT, IN, status, 'LINE',IREC2, 'SAMP',SSAM,
     &	   'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit2, IN, status, ' ')
      enddo

      NEND = NLO-NLW1
      call xvread( RUNIT, IN , status, 'LINE',SLIN, 'SAMP',SSAM,
     &	'NSAMPS',NSO, 'band', ib, ' ')
      LINE=1
      if (LINE.LE.NEND) call xvwrit( wunit2, IN, status,' ')  ! USU. DONE
      if (LINE.GT.NLW2) call xvwrit( wunit1,  IN, status,' ')  ! USU. SKIPPED
c
      do LINE=2,NLO
	 call xvread( RUNIT, IN, status, 'SAMP',SSAM, 'NSAMPS',NSO,
     &    'line', slin+line-1, 'band', ib, ' ')
	 if (LINE.LE.NEND) call xvwrit( wunit2, IN, status, ' ')
	 if (LINE.GT.NLW2) call xvwrit( wunit1, IN, status, ' ')
      enddo

      do I=1,NLW2-1
          LINE=NLO-1+SLIN-I
          call xvread( RUNIT, IN, status, 'LINE',LINE, 'SAMP',SSAM,
     &	  'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit1, IN, status, ' ')
      enddo
c
C  Close  Scratch DISK & Re-Open for INPUT:
c
      call xvclose( wunit2, status, ' ')
      call xvopen( wunit2, status, 'OP','READ','U_FORMAT','REAL',
     *   'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
c
C  Close  OUTPUT File  & Re-Open  for UPDATE
c
      call xvclose( wunit1, status, ' ')
      call xvopen( wunit1, status, 'OP','UPDATE', 'U_FORMAT','REAL',
     * 'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     * ' ')
c
c   Compute the Initial SUMDN and Count buffers from the 
c   first NLW  lines 
c   SUMDN = SUM of DN'S  GE  THRESH in each column 
c   COUNT = SUM of PIXELS GE THRESH in each column 

c     call ZIA(SUMDN, L1/4)
c     call ZIA(COUNT, L2/4)
      do i=1,l1/4
	sumdn(i) = 0.0
      enddo
      do i=1,l2/4
	count(i) = 0.0
      enddo
      do LINE=1,NLW
	  call xvread( wunit2, IN(NSW2), status, 'LINE',LINE,
     &	 'NSAMPS',NSO, ' ')
	call RFLCT(IN)
	do J=1,TOTLEN
	  if (IN(J) .GE. THRESH) then
            SUMDN(J) = SUMDN(J) + IN(J)
            COUNT(J) = COUNT(J) + 1
	  endif
	enddo
      enddo
c
c    Initialize the xvreads:
c
      call xvread( wunit1,NEXT(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call RFLCT(NEXT)
      call xvread( wunit2,LAST(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call RFLCT(LAST)
      call xvread( RUNIT,IN(NSW2),status,'LINE',SLIN,'NSAMPS',NSO,
     &      'SAMP',SSAM, 'band', ib, ' ')
      call RFLCT(IN)
c
C   MAIN  LINE Loop for  OUTPUT Picture
c
      do LINE = 1,NLO
          DNSUM = 0
          NSUM  = 0
c
c    Add up the First NSW DN:
c 
          DO J = 1,NSW
            DNSUM = DNSUM + SUMDN(J)
            NSUM  = NSUM  + COUNT(J)
 	  enddo
c
c    Apply the appropriate Filter to Current LINE 
c
         if (MODE .EQ. 1) call HPFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 2) call SDFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 3) call LPFILT(IN,OUT,SUMDN,COUNT)
         if (MODE .EQ. 4) call DFILT (IN,OUT,SUMDN,COUNT)
	 call xvwrit( wunit1, OUT, status, ' ')
         if (LINE .EQ. NLO) Go To 2000
c
c   Update SUMDN & COUNT buffers by deleting LAST line & Adding NEXT:
c
	 DO J = 1,TOTLEN
	   if (LAST(J) .GE. THRESH) then
	     SUMDN(J) = SUMDN(J) - LAST(J)
	     COUNT(J) = COUNT(J) - 1
	   endif
	   if (NEXT(J) .GE. THRESH) then
	     SUMDN(J) = SUMDN(J) + NEXT(J)
	     COUNT(J) = COUNT(J) + 1
	   endif
	 enddo

	 if (SSAM .EQ. 1) then	   ! READ NEXT LINE
	   call xvread( RUNIT, IN(NSW2), status,'NSAMPS',NSO,
     &      'line', slin+line, 'band', ib,  ' ')
	 else
	   call xvread( RUNIT, IN(NSW2), status, 'SAMP',SSAM,
     &	    'NSAMPS',NSO, 'line', slin+line, 'band', ib, ' ')
	 endif
c
         call RFLCT(IN)
c
c    LAST   Line in Window  
c
         call xvread( wunit2,LAST(NSW2),status, 'NSAMPS',NSO, ' ')
         call RFLCT(LAST)
c
c    NEXT  Line in Window  
c
         call xvread( wunit1,NEXT(NSW2),status,'NSAMPS',NSO, ' ')
         call RFLCT(NEXT)
c
      enddo
c
C   Close Files and Exit
c
2000  Continue

c  multi-band case:  copy wunit1 to band IB of wunit3
      if (nb.gt.1) then
        do il=1,nlo
          call xvread( wunit1, in, status, 'line', il, 'nsamps',nso,
     &     ' ')
          call xvwrit( wunit3, in, status, 'line', il, 'nsamps',nso,
     &     'band', ib, ' ')
        enddo
      endif

      enddo  ! END OF BAND LOOP

      call xvclose( RUNIT, status, ' ')
      call xvclose( wunit2, status, ' ')
      call xvclose( wunit1, status, ' ')
c
      Return 
      End
c
c*************************************************************
c
      Subroutine RFLCT(BUF)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 BUF(1)
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
      real*4 thresh,dnsum,lower,upper

c     call MVE(2,NSW1,BUF(NSW3),BUF(NSW1),1,-1)

      I = 1
      J = 1
      DO K=1,NSW1
        BUF(NSW1+J-1) = BUF(NSW3+I-1)
        I = I + 1
        J = J - 1
      ENDDO

c     call MVE(2,NSW1,BUF(TOTL2),BUF(TOTL1),-1,1)

      I = 1
      J = 1
      DO K=1,NSW1
        BUF(TOTL1+J-1) = BUF(TOTL2+I-1)
        I = I - 1
        J = J + 1
      ENDDO

      Return
      End
c
c******************************************************************
c
      Subroutine FILTA
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2,status,limits(2),
     * wunit1,RUNIT,wunit2,wunit3
      real*4 thresh,dnsum,lower,upper
      LOGICAL xvptst,HIGH,LOW,SCENE,DIVIDE

      call xvunit( RUNIT,'INP',1,status, ' ')
      call xvopen( RUNIT, status,'U_FORMAT','REAL','OPEN_ACT','SA',
     * 'IO_ACT','SA', ' ')
      call xvsize(SLIN, SSAM,NLO,NSO,NX,NS, ' ')
      call xvbands(sb,nb,idum)
      call xvget( RUNIT,status,'PIX_SIZE',N, ' ')
      if (n.gt.4) call mabend(' Input pixel size may not be > 4 bytes!')

      call xvpcnt('OUT',i,j)
      if (nb.gt.1 .and.	i.lt.3) call mabend(
     *	 ' 3 output files required when input is multi-band')

      if (nb.eq.1) then
	call xvunit( WUNIT1,'OUT',1,status,' ')
	call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      else
	call xvunit( WUNIT3, 'OUT', 1, status, ' ')
	call xvopen( WUNIT3, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
        call xvunit( WUNIT1, 'OUT', 3, status, ' ')
        call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','REAL',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      call xvunit( WUNIT2, 'OUT', 2, status, ' ')
      call xvopen( WUNIT2, status,'OP','WRITE','U_FORMAT','REAL',
     * 'O_FORMAT','REAL','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     * 'SA','IO_ACT','SA', ' ')

      KEY = 4-N
      call Prnt(4,1,N,' #BYTES/PXL=.')
      MODE = 4
      NLW  = NLO/10
      NSW  = NLW
      call xvparm('NLW', NLW, NNLW, NLWDF, 0)
      call xvparm('NSW', NSW, NNSW, NSWDF, 0)
      call xvparm('THRESH', THRESH, NTHRE, THREDF, 0)
      call xvparm('SCALE' ,SCALE, NSCAL, SCALDF, 0)
      call xvparm('DCTRAN', DCTRAN, NDCTR, DCTRDF, 0)
      call xvparm('BOOST', BOOST, NBOOS, BOOSDF, 0)
      call xvparm('DCLEVEL', DCLVL, NDCLE, DCLEDF, 0)
      call xvparm('OFFSET', OFFSET, NOFFS, OFSDF, 0)
      HIGH  =  xvptst('HIGH')
      LOW   =  xvptst('LOW')
      SCENE =  xvptst('SCENE')
      DIVIDE=  xvptst('DIVIDE')

      if (HIGH)   MODE = 1
      if (LOW)    MODE = 3
      if (SCENE)  MODE = 2
      if (DIVIDE) MODE = 4

      if ((NLW/2)*2 .EQ. NLW) NLW = NLW + 1
      if ((NSW/2)*2 .EQ. NSW) NSW = NSW + 1

      NPIX = NSO
      if (KEY .EQ. 0) then
        call xvparm( 'LIMITS', limits, i, j, 0)
	if (i.eq.0) then
	  lower = -1.0e10
	  upper = 1.0e10
	else
	  lower = limits(1)
	  upper = limits(2)
	endif
      elseif (key.eq.2) then
	LOWER = -32768
	UPPER = 32767
      else
	LOWER = 0
	UPPER = 255
      endif

      NLW2 = (NLW+1) / 2
      NLW1 = NLW2 - 1
      NSW2 = (NSW+1) / 2
      NSW1 = NSW2 - 1
      TOTLEN = NPIX +  2*NSW1
      NSW3 = NSW2 + 1
      TOTL1 = TOTLEN - NSW1 + 1
      TOTL2 = TOTLEN - NSW2
      call PRNT(4,1,NLW,' NLW=.')
      call PRNT(4,1,NSW,' NSW=.')
c
c     Get  Number  Pixels/Line
c   
      Return
      End
c
c******************************************************************
c
      Subroutine HPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
      real*4 IN(1),OUT(1),sumdn(1)
      INTEGER COUNT(1),UPLIM
      real*4 thresh,dnsum,lower,upper
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
      if (IN(NSW2) .LT. THRESH) Go To 90
      if (NSUM .NE. 0) X = DNSUM/float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = (IN(NSW2) - X) * BOOST + DCTRAN*IN(NSW2) + DCLVL
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif (X .LE. LOWER) then
	OUT(1) = LOWER
      else
	OUT(1) = X
      endif
      Go  To 100
90    OUT(1)=0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
c
      Do 200  J = LOWLIM,UPLIM
         JJ  = JJ + 1
         JK1 = J - NSW2
         JK2 = J + NSW1
         DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
         NSUM  = NSUM - COUNT(JK1) + COUNT(JK2)
         if (IN(J) .LT. THRESH) Go To 160
         if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
         if (NSUM .EQ. 0) X = DNSUM
         X = (IN(J) - X) * BOOST + DCTRAN * IN(J) + DCLVL
         if (X .GT. UPPER)  then
	   OUT(JJ)=UPPER
         elseif (X .LE. LOWER) then
	   OUT(JJ)=LOWER
         else
	   OUT(JJ) = X
         endif
c
         Go To  200
160      OUT(JJ)=0
200   Continue
c
      Return
      End
c
c******************************************************************
c
      Subroutine DFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2

      IF(IN(NSW2).LT.THRESH)GO TO 90
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/DNSUM
      IF(NSUM.EQ.0)X=FLOAT(NSUM)
      X = IN(NSW2)*(SCALE*X+DCTRAN)+OFFSET
      if (X.GT.UPPER) then
	OUT(1)=UPPER
      elseif (X.LE.LOWER) then
	OUT(1)=LOWER
      else
	OUT(1) = X
      endif
      GO TO 100
 90   OUT(1)=0
 100  CONTINUE
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      DO 200 J=LOWLIM,UPLIM
      JJ=JJ+1
      JK1=J-NSW2
      JK2=J+NSW1
      DNSUM=DNSUM-SUMDN(JK1)+SUMDN(JK2)
      NSUM=NSUM-COUNT(JK1)+COUNT(JK2)
      IF(IN(J).LT.THRESH)GO TO 160
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/DNSUM
      IF(DNSUM.EQ.0)X=FLOAT(NSUM)
      X=IN(J)*(SCALE*X+DCTRAN)+OFFSET
      if (X.GT.UPPER) then
	OUT(JJ)=UPPER
      elseif (X.LE.LOWER) then
	OUT(JJ)=LOWER
      else
	OUT(JJ) = X
      endif
      GO TO 150
 160  OUT(JJ)=0
 150  CONTINUE
 200  CONTINUE
      RETURN
      END
c
c******************************************************************
c
      Subroutine SDFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2

c     call MVE(2,NPIX,IFIX(DCLVL),OUT,0,1)
      J = 1
      DO K=1,NPIX
        OUT(J) = dclvl
        J = J + 1
      ENDDO
      if (IN(NSW2) .LT. THRESH) Go To  90
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      if (X .NE. 0) X = (IN(NSW2)-X)*SCALE/X + DCTRAN*IN(NSW2) + DCLVL
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif (X .LE. LOWER) then
	OUT(1)=LOWER
      else
	OUT(1) = X
      endif
      go to 100
90    OUT(1) = 0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      if (IN(J) .LT. THRESH) Go To  160
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      if (X .NE. 0) X = (IN(J)-X)*SCALE / X + DCTRAN*IN(J) + DCLVL
      if (X .GT. UPPER) then
	OUT(JJ)=UPPER
      elseif (X .LE. LOWER) then
	OUT(JJ)=LOWER
      else
	OUT(JJ) = X
      endif
      go to 150
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      RETURN
      END
c
c******************************************************************
c
      Subroutine  LPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
      real*4 IN(1),OUT(1),sumdn(1)
      real*4 thresh,dnsum,lower,upper
      INTEGER COUNT(1),UPLIM
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
c
      if (IN(NSW2) .LT. THRESH) Go To 90
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = BOOST*X + OFFSET
      if (X .GT. UPPER) then
	OUT(1) = UPPER
      elseif  (X .LE. LOWER) then
	OUT(1) = LOWER
      else
	OUT(1) = X
      endif
      Go To  100
90    OUT(1) = 0
100   Continue
c
c
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      if (IN(J) .LT. THRESH) Go To 160
      if (NSUM .NE. 0) X = DNSUM/Float(NSUM)
      if (NSUM .EQ. 0) X = DNSUM
      X = BOOST*X + OFFSET
      if (X .GT. UPPER) then
	OUT(JJ) = UPPER
      elseif (X .LE. LOWER) then
	OUT(JJ) = LOWER
      else
	OUT(JJ) = X
      endif
      Go to 150 
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tfilt.imake
#define  PROGRAM   tfilt

#define MODULE_LIST tfilt.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create tfilt.pdf
process help=*
PARM INP      TYPE = (STRING,80)
PARM OUT      TYPE = (STRING,80) count=2:3
PARM SIZE     TYPE = INTEGER  COUNT = 4  DEFAULT = (1,1,0,0)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM NLW      TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM NSW      TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM THRESH   TYPE=REAL  DEFAULT=0.0
PARM FILTER TYPE=KEYWORD COUNT=1 VALID=(HIGH,LOW,SCENE,DIVIDE) DEFAULT=DIVIDE
PARM SCALE    TYPE=REAL  DEFAULT=100.0
PARM DCTRAN   TYPE=REAL  DEFAULT=0.0
PARM BOOST    TYPE=REAL DEFAULT=1.0
PARM DCLEVEL  TYPE=REAL  DEFAULT=128.0
PARM OFFSET   TYPE=REAL DEFAULT=0.5
PARM LIMITS   TYPE=REAL COUNT=(0,2) DEFAULT=--
END-PROC
.TITLE
 tfilt  --  threshold filtering.
.HELP
PURPOSE:

tfilt is a VICAR program which performs various boxfilter type convolutional
filters.  It is a commonly used, especially in planetary image processing. 
tfilt is often used to bring out detail that is not noticeable in the original
image.  tfilt is able to ignore DN's below a threshold when computing the low
pass average and is thus free of ringing in certain cases.  (See the threshold
effects section below.)   Scaling allows the dynamic range of high pass filters
to be improved before integer truncation. 
.PAGE
EXECUTION:

tfilt is executed as a standard VICAR program.  The following 
VICAR command line formats show the most common usages:

      tfilt INP=a OUT=(b,x) SIZE=(sl,ss,nl,ns) optional parameters
      tfilt INP=a OUT=(b,x) SL=sl SS=ss NL=nl NS=ns optional parameters
      tfilt a (b,x) (sl,ss,nl,ns) optional parameters
      tfilt a (b,x) optional parameters

       Here 'a' represents the input image file name, 
       'x' represents a scratch file name, and
       'b' represents the output image file name.

If the primary input is a multi-band image, then three output files
are required, of which the last two are scratch files, e.g.:

      tfilt a (b,x,y) optional parameters

.PAGE
USER INFORMATION:

The size of the output image is determined by the the SIZE field if the SIZE
field is entered by the user.  If the SIZE field is not entered, the output
file is the same size as the input file. 

The data type of the input image may be any valid format of 4 or fewer bytes
per pixel.  The data type is obtained from the VICAR label of the input image.
The output image has the same data format as the input image. 

.page
Available Filter Types:

tfilt can be used to apply a high pass, a low pass, a scene dependent, or
a divide filer to an image.  Each type of filter can be thought of as 
emphasizing one aspect of the image and de-emphasizing another aspect of
the image.  tfilt has a variety of uses in image restoration and
image enhancement.  The major charactistics of each of these filters are
described below.

1)  The high pass filter emphasizes high frequency information and
    de-emphasizes low frequency information.  (High frequency means
    brightness changing rapidly as a function of distance.  Low frequency
    means brightness changing slowly as a function of distance.)  The high
    pass filter can be used to compensate for variations in illumination
    between different areas of the image due to sun angle differences.  It
    can also be used to make the picture sharper, to emphasize edges of
    features, and to enhance fine detail.  Often the high pass filter will
    bring out small scale structure that is not noticeable in the original
    image.  The high pass filter tends to remove variations due to color
    differences (albedo variations). The high pass filter can emphasize
    noise if present, so it is best to remove noise before performing a
    high pass filter.  If there are reseau marks in the image, they should
    be removed with program RESSAR77 or a similar program before a high
    pass filter is performed. 
    
2)  The low pass filter emphasizes low frequency information and
    de-emphasizes high frequency information.  The low pass filter is used
    mainly for smoothing the brightness variations in the image.  It can be
    used to smooth noise in a noisy or grainy image to make it less
    noticeable.  The low pass filter makes edges and features blurred or
    smoother. 
    
3)  The scene dependent filter is a variant of the high pass filter that
    can be useful in dealing with images having nonhomogeneous illumination
    or images with mixtures of areas with little high frequency detail and
    areas with a lot of high frequency detail.  It is called a scene
    dependent filter because it compensates for dark areas by dividing the
    high frequency information by the average brightness of the surrounding
    area.  Thus it emphasizes high frequency information especially in
    darker areas of the image, and de-emphasizes low frequency information
    especially in brighter areas of the image. The scene dependent filter
    has similar properties to the high pass filter (enhancing detail), but
    compensates more for the darker areas of the image. The MIPL facility
    at JPL has a display showing the use of the scene dependent filter on
    an image of Mercury with only part of the planet illumined by the sun. 
    The scene dependent filter improves feasture discriminability near the
    edge of the illumined portion of the planet. 
    
4)  The divide filter amplifies darker parts of the image and scales down
    brighter parts of the image, preserving the contrast and evening out
    the overall intensity across the image.  It is useful for bringing out
    subtle detail, especially in darker parts of the image. The divide
    filter can be used to compensate for variations in illumination between
    different areas of the image due to sun angle differences.  The divide
    filter tends to remove variations due to color differences (albedo
    variations). The divide filter tends to pass random noise without
    adding emphasis. 
.page
Selection of Filter Type:

The most commonly used of the four filter types is the high pass filter. This is
partly because people like to see a lot of detail and imaging systems often
attenuate higher frequency information.  (See reference 2.) The high pass
filter also compensates for variations in illumination in the image. The divide
filter is also commonly used.  It compensates for variations in illumination in
the image without emphasizing high frequency information.  It can bring out
detail in the image and in some cases produces a result quite similar to the
high pass filter.  The scene dependent filter is less commonly used.  It is
recommended when the results of the high pass filter are not satisfactory
because of inhomogeneity in the image.  The low pass filter has a very
different purpose from the other three filter types, namely smoothing the data.
VICAR program BOXFLT2 is also used for low pass filtering
and has some different methods for handling the edges of the image.
.page
Advantages and Disadvantages:

tfilt has both good and bad features.  It is fast for a filter program, 
If the threshold is chosen well, tfilt can do a good filtering job without 
causing noticeable ringing near edges.  It is not an optimal filter, though, 
and sometimes it can generate ringing problems.  This is because it uses a 
uniform set of filter weights.(See reference 1.)  The VICAR program FILTER is 
recommended for users wishing to do more optimal filtering.  FILTER can be 
much slower than tfilt, though. FILTER requires the user specify the filter 
weights.  There is, however, a default set of weights in FILTER.  VICAR 
procedure FILTER2 can be used to generate the weights alternatively.
 
.page
Threshold Effects:

Linear filtering with a uniform set of weights can often cause ringing
in the output image, especially around a sharp edge.  The threshold (THRESH)
parameter allows tfilt to filter without causing noticeable ringing in many
cases.  In planetary image processing, the most distinct edge in a picture
is often the boundary (limb) between a planet or moon (satelite) and the
black of outer space.  For such an image the threshold should be set 
slightly below the brightness of the planet so that basically all of the
planet is brighter than the threshold and the black of outer space is
darker than the threshold. Unfortunately, finding a threshold that minimizes
ringing can be a matter of trial and error.  A histogram may be of value in
picking an initial value. 
.page
Effects of Box Size:

For all filter types the box size (parameters NLW and NSW) affects the
output.  For a low pass filter a small box (3 by 3 to 11 by 11) is usually used.
For the other filter types a large box is generally used since this
produces a less grainy image and less noise enhancement than a smaller box.
A large box can cause greater ringing than a small box, though, especially
if the threshold parameter is not set optimally.  For an 800 by 800 
Voyager image a box size in the range 15 by 15 to 75 by 75 is common.
The exact size is not too important.  A square is commonly used for
symmetry.  Because of the algorithm used by tfilt, the box size does not affect
the execution time significantly.  (A 75 by 75 box took five percent more time
than a 5 by 5 box.) 
.page
Effects of the DC Transmission Factor (DCTRAN):

The DCTRAN parameter is used to soften the effect of the selected filter by
making the output image a combination of the filter output and the input
image.  DCTRAN can be used with all of the filter types except low pass.
DCTRAN is commonly set to make the output image about 70 percent from
the filter and 30 percent from the input image.  (For the high pass filter
you can try DCTRAN = 0.1 to 0.2.)  The DC in DCTRAN stands for direct
current.  This stems from the connection of filtering with signal processing
theory.  DCTRAN reflects the amount of the original signal that is
transmitted by the filter.  When DCTRAN is used with the high pass filter or 
scene dependent filter, the output image will retain some low 
frequency information, and albedo and illumination variations will still be
present to some degree. 
.page
Control of Dynamic Range:

Each of the filter types uses some parameters to control the dynamic
range of DNs in the output image.  The default values for these
parameters can sometimes produce a narrow dynamic range, especially
with byte data, resulting in image degradation when integer truncation
takes place.  Some recommendations on these parameters are given below
for byte data, but a lot depends on the input image, and some trial
and error may be appropriate until satisfactory results are produced.
(For trial and error, try tfilt on a small portion of the 
image and run VICAR program HIST with BINS=20 to check the dynamic
range of the output image.)

For the high pass filter, try BOOST=4, and default DCLEVEL.  Default DCTRAN if a
straight high pass filter is desired or try DCTRAN = .1 to .2 to soften 
the filter.

For the scene dependent filter, try SCALE in the range 100 to 300 and default
DCLEVEL. Default DCTRAN if a straight scene dependent filter is desired or try
DCTRAN = .1 to .2 to soften the filter. 

For the low pass filter the defaults for BOOST and OFFSET should be
satisfactory.

For the divide filter, try OFFSET in the range 0 to 50, and default DCLEVEL.  
Default DCTRAN if a straight divide filter is desired or try 
DCTRAN = .1 to .2 to soften the filter.
.page
EXAMPLES:

1.     tfilt (A,X) HIGH.IMG  'HIGH BOOST=4 THRESH=25  NLW=21 NSW=21 DCTRAN=.1

In this example tfilt is used to apply a high pass filter to image A, 
producing image HIGH.IMG.  A 21 by 21 box is used.

2.     tfilt (MIRANDA.IMG,X) DIV.IMG   'DIV SCALE=150 THRESH=25  NLW=21 NSW=21

In this example tfilt is used to apply a divide filter to image MIRANDA.IMG, 
producing image DIV.IMG.  A 21 by 21 box is used.
.PAGE
3.     tfilt (MIRANDA.IMG,X) SCENE.IMG 'SCENE SCALE=250 THRESH=25 NLW=21 NSW=21

In this example tfilt is used to apply a scene dependent filter to image 
MIRANDA.IMG, producing image SCENE.IMG.  A 21 by 21 box is used.

4.     tfilt (MIRANDA.IMG,X) LOW.IMG   'LOW THRESH=25  NLW=3 NSW=3

In this example tfilt is used to apply a low pass filter to image MIRANDA.IMG, 
producing image LOW.IMG.  A 3 by 3 box is used.

5.  The last example is the test procedure for tfilt.  This is
    a complete example that could be run by the user and that 
    demonstrates uses of the possible parameters.

    GEN OUT=GEN NL=20 NS=17 ! generate a picture with byte format
    GEN OUT=HST NL=20 NS=22 'HALF ! generate a scratch file with halfword format
    LIST INP=GEN 'ZEROES ! check input data
    tfilt INP=GEN OUT=(TEST1,IST)   !run tfilt - default all optional params.
    LIST INP=TEST1 'ZEROES
    tfilt INP=GEN OUT=(TEST2,IST)  ! run tfilt with byte
    LIST INP=TEST2 'ZEROES
    ! run tfilt with byte and nlw
    tfilt INP=GEN OUT=(TEST4,IST) NLW=3
    LIST INP=TEST4  'ZEROES
    ! run tfilt with byte, nls and nsw
    tfilt INP=GEN OUT=(TEST6,IST) NLW=2 NSW=3
    LIST INP=TEST6 'ZEROES
    ! run tfilt with byte, nls, nsw and thresh
    tfilt INP=GEN OUT=(TEST7,IST) NLW=2 NSW=3 THRESH=3
    LIST INP=TEST7 'ZEROES
    ! run tfilt with nlw, nsw, thresh and high
    tfilt INP=GEN OUT=(TEST8,IST) NLW=3 NSW=3 THRESH=3 'HIGH
    LIST INP=TEST8 'ZEROES
    ! run tfilt with thresh, scene
    tfilt INP=GEN OUT=(TEST9,IST) THRESH=2 'SCENE
    LIST INP=TEST9 'ZEROES
    tfilt INP=GEN OUT=(TEST10,IST) NLW=4 'LOW !run tfilt with nlw and low
    LIST INP=TEST10 'ZEROES
    tfilt INP=GEN OUT=(TEST11,IST) 'DIVIDE ! run tfilt with divide
    LIST INP=TEST11 'ZEROES
    ! run tfilt with nlw and high - define boost,dctran, dclevel
    tfilt INP=GEN OUT=(TEST12,IST) NLW=2 'HIGH BOOST=2 DCTRAN=0.1 DCLEVEL=50.0
    LIST INP=TEST12 'ZEROES
    !run tfilt with nlw, nsw, and low - define boost  and offset
    tfilt INP=GEN OUT=(TEST14,IST) NLW=2 NSW=4 'LOW BOOST=2.00 OFFSET=20.
    LIST INP=TEST14 'ZEROES
    ! run tfilt with halfword and nsw
    GEN OUT=HGEN NL=20 NS=22 'HALF !generate a picture with halfword format
    LIST INP=HGEN 'ZEROES
    tfilt INP=(HGEN,HST) OUT=(TEST3,IST)  ! run tfilt with halfword
    LIST INP=TEST3 'ZEROES
    tfilt INP=(HGEN,HST) OUT=(TEST5,IST) NSW=2
    LIST INP=TEST5 'ZEROES
    !run tfilt with halfword,nlw,nsw,thresh,scene,scale,dctran and dclevel
    tfilt INP=(HGEN,HST) OUT=(TEST13,IST) NLW=2 NSW=2 THRESH=3 'SCENE+
     SCALE=200.0 DCTRAN=0.3 DCLEVEL=10
    LIST INP=TEST13 'ZEROES
.page
 OPERATION
    
tfilt operates as a convolutional filter.  At each pixel it computes the
average of all DN's greater than the THRESH parameter within an area of
dimensions NLW by NSW centered on that pixel and performs the filter using this
value.  Values of pixels outside the image are obtained by reflection about the
image edges. 

If the local average is ADN, the pixel at the center of the averaged area is
DN, and the output pixel DN is OUT, then the four filters are: 
 
 HIGH     OUT=(DN-ADN)*BOOST+DCTRAN*DN+DCLEVEL
 SCENE    OUT=((DN-ADN)/ADN)*SCALE+DCTRAN*DN+DCLEVEL
 LOW      OUT=ADN*BOOST+OFFSET
 DIVIDE   OUT=(DN/ADN)*SCALE+DCTRAN*DN+OFFSET

Default values for the constants are:
 
 SCALE = 100.0,  DCTRAN = 0.0,  BOOST = 1.0,
 DCLEVEL= 128.0,  OFFSET = 0.5

The calculations are performed in floating-point arithmetic and then truncated
to the output format if necessary.  For SCENE and DIVIDE,
1.0 is used in the denominator if ADN is 0.  The results of the calculations
are checked for being in the valid range of DNs for the data type
of the image and are adjusted if invalid.  For byte data, DNs less
than 0 are set to 0, and DNs greater than 255 are set to 255.  For halfword
data, DNs less than -32768 are set to -32768, and DNs greater than 32767 are
set to 32767.  For fullword integer and real*4 data, the limits are set by
default to (-1.0E10, +1.0E10), but may be changed using the LIMITS parameter.
.PAGE
TIMING: 

tfilt takes about 23 CPU seconds on the VAX 8600 for a high pass filter 
on an 800 by 800 byte image.  Because of the algorithm used by tfilt,
the box size does not affect the execution time significantly.  (A 75 by
75 box took five percent more time than a 5 by 5 box.)
.page 

  REFERENCES:

   1.   K. R. Castleman, "Digital Image Processing",
        Prentice Hall, Inc., 1979, p. 199.

   2.   J. G. Moik, "Digital Processing of Remotely Sensed Images",
        NASA Publication SP-431, 1980, p. 130.


 WRITTEN BY: J. J. Lorre,                   Sept. 22, 1980
 COGNIZANT PROGRAMMER:  L W.Kamp
 REVISIONS:
  19Dec12 ...LWK...  Revised to support FULL and REAL data types;  parameter THRESH
                     behaves slight differently than before:  the default is 0 instead
                     of -1 and DNs < THRESH are excluded, not <= THRESH;  parameter
                     LIMITS added.
  05MAR00 ...LWK...  Revised to allow multispectral files
  Feb 21 96 ...FFM...  Renamed FILTER0 as FILTER. Obsolete programs FILTERAP &
                       FILTER0.(There is no need to have procedure FILTER
                       because there is no AP on any platform).
                       Modified HELP & TEST slightly, retested on alpha, andes,
                       solaris, & sunos.
  AUG 06 89 ...GMY...  Fix bug in SDFILT (given SIZE field specification)
  MAY-85   ...LWK... RENAMED tfilt FOR PROC TFILT
  OCT. 84  ...LWK... BUG FIXES & SPEED UP I/O BY OMITTING OPTIONALS
  OCT. 84  ...BXG... CONVERTED TO VICAR2
  JAN. 84  ...DFS... CONVERTED TO VAX
  NOV 24 82 ...JAM... THIS IS TFILT REWRITTEN IN FORTRAN. THE ORIGINAL 
		     TFILT WAS WRITTEN IN PL/I BY J.J.LORRE
.LEVEL1
.VARIABLE INP
input file
.VARIABLE OUT
output file and one or two 
(if multi-band) scratch files
.VARIABLE SIZE
 FOUR INTEGERS -
 VICAR size field
.VARIABLE SL
 INTEGER - starting line in
 input picture
.VARIABLE SS
 INTEGER - starting sample in
 input picture
.VARIABLE NL
 INTEGER - number of lines in
 input picture to process
.VARIABLE NS
 INTEGER - number of samples per
 line in input picture 
 to process
.VARIABLE NLW
 INTEGER - number of lines 
 in filter
.VARIABLE NSW
 INTEGER - number of pixels 
 per line in filter
.VARIABLE THRESH
 INTEGER - pixel threshold level
.VARIABLE FILTER
 KEYWORD - filter type
 Valid: HIGH,LOW,SCENE,
 DIVIDE
.VARIABLE SCALE
 REAL - DN scale factor
.VARIABLE DCTRAN
 REAL - DC transmission factor
.VARIABLE BOOST
 REAL - amplitude boost factor
.VARIABLE DCLEVEL
 REAL - additive constant
.VARIABLE OFFSET
 REAL - additive constant
.vari LIMITS
Allows setting of upper and
lower limits for FULL and 
REAL data.
.LEVEL2
.VARIABLE INP
 input file
.VARIABLE OUT
 output file and one or two scratch files

 If the primary input is multi-band, then three output files are required
 of which the last two are scratch files.  Otherwise, only one scratch
 file is required, which is the second output file.

 The first output file is always the output image.

.VARIABLE SIZE
 (number of lines,number of samples,
 starting line number,starting sample number)
 Specifies area of input image to process.
.VARI SL
Starting line.
.VARI SS
Starting sample.
.VARI NL
Number of lines.
.VARI NS
Number of samples per line.
.VARIABLE NLW
 NLW is an integer which specifies the number of lines dimension
 of the convolution window.

 The default is NLW=NL/10 where NL is the number of lines in the size field.
 If NLW is an even number (whether user-specified or default), then 1 is
 added to get an odd number.  E.g., for a 100x100 image, the default is
 NLW=11.
.VARIABLE NSW
 NSW is an integer which specifies the number of pixels dimension of the
 convolutional window.  The default is NL/10.

 If NLW is an even number (whether user-specified or default), then 1 is
 added to get an odd number.  
.VARIABLE THRESH
 THRESH is an integer which specifies a threshold
 level above which pixels will be accepted into
 the average (low pass).  When a pixel lies 
 below THRESH the output picture pixel will
 always be zero.  The default is THRESH=0,
 which, at least for byte, converts tfilt into
 a linear boxfilter.

(NOTE:  until 20-Dec-2012, the default for THRESH was
-1 and the criterion for exclusion was DN<=THRESH.  This
was changed when support for REAL*4 data was added, to
make the behaviour at THRESH=0 consistent with integer
data types.  As a result, to obtain the same results as
the older version of THRESH, 1 must be added to the old
value.)
.VARIABLE FILTER
 This parameter specifies the type of filter to be used.
 
 HIGH: performs a high pass filter using the following equation:
 	OUT = (DN-AVGDN)*BOOST+DCTRAN*DN+DCLEVEL
 
 SCENE: performs a SCENE filter using the following equation:
	OUT = (DN-AVGDN)/ADVDN*SCALE+DCTRAN*DN+DCLEVEL
 
 LOW:  performs a low pass filter using the following equation:
	OUT = AVGDN*BOOST+OFFSET
 
 DIVIDE: performs a divide filter using the following equation:
	OUT = DN/AVGDN*SCALE+DCTRAN*DN+OFFSET
	 This is the default filter.
.VARIABLE SCALE
 SCALE is a real number, used to scale output for SCENE and
 DIVIDE filters only.  The default is 100.
.VARIABLE DCTRAN
 DCTRAN is a real, DC transmission. Default = 0.0  (See the section on 
 DCTRAN in the main help section for tfilt.)
.VARIABLE BOOST
 BOOST is a real number, used to scale output for HIGH and LOW
 filters only.   Default is 1.0
.VARIABLE DCLEVEL
 DCLEVEL is a real additive factor in SCENE and HIGH filters only.
 Default is 128.0
.VARIABLE OFFSET
 OFFSET is a real additive factor used with
 LOW and DIVIDE filters only. (Default = 0.5)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttfilt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

GEN OUT=GEN NL=20 NS=17 ! generate a picture with byte format
LIST INP=GEN 'ZEROES ! check input data
tfilt INP=GEN OUT=(TEST1,X)  ! run tfilt - default all optional parameters
LIST INP=TEST1 'ZEROES
tfilt INP=GEN OUT=(TEST2,X) ! run tfilt with byte
LIST INP=TEST2 'ZEROES
! run tfilt with byte and nlw
tfilt INP=GEN OUT=(TEST4,X) NLW=3
LIST INP=TEST4  'ZEROES
! run tfilt with byte, nls and nsw
tfilt INP=GEN OUT=(TEST6,X) NLW=2 NSW=3
LIST INP=TEST6 'ZEROES
! run tfilt with byte, nls, nsw and thresh
tfilt INP=GEN OUT=(TEST7,X) NLW=2 NSW=3 THRESH=4
LIST INP=TEST7 'ZEROES
! run tfilt with nlw, nsw, thresh and high
tfilt INP=GEN OUT=(TEST8,X) NLW=3 NSW=3 THRESH=4 'HIGH
LIST INP=TEST8 'ZEROES
! run tfilt with thresh, scene
tfilt INP=GEN OUT=(TEST9,X) THRESH=3 'SCENE
LIST INP=TEST9 'ZEROES
tfilt INP=GEN OUT=(TEST10,X) NLW=4 'LOW !run tfilt with nlw and low
LIST INP=TEST10 'ZEROES
tfilt INP=GEN OUT=(TEST11,X) 'DIVIDE ! run tfilt with divide
LIST INP=TEST11 'ZEROES
! run tfilt with nlw and high - define boost,dctran, dclevel
tfilt INP=GEN OUT=(TEST12,X) NLW=2 'HIGH BOOST=2 DCTRAN=0.1 DCLEVEL=50.0
LIST INP=TEST12 'ZEROES
!run tfilt with nlw, nsw, and low - define boost  and offset
tfilt INP=GEN OUT=(TEST14,X) NLW=2 NSW=4 'LOW BOOST=2.00 OFFSET=20.
LIST INP=TEST14 'ZEROES

! run tfilt with halfword and nsw
GEN OUT=HGEN NL=20 NS=22 'HALF !generate a picture with halfword format
LIST INP=HGEN 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) ! run tfilt with halfword
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST5,X) NSW=2
LIST INP=TEST5 'ZEROES
!run tfilt with halfword, nlw, nsw, thresh, scene, scale, dctran and dclevel
tfilt INP=HGEN OUT=(TEST13,X) NLW=2 NSW=2 THRESH=4 'SCENE +
 SCALE=200.0 DCTRAN=0.3 DCLEVEL=10
LIST  INP=TEST13  'ZEROES

! test multispectral option
GEN OUT=MGEN NL=20 NS=22 NB=10 'HALF ! generate a halfword multispectral file 
tfilt INP=MGEN OUT=(TEST15,X,Y) NLW=2 NSW=2 THRESH=4 'LOW
LIST  INP=TEST15 'ZEROES sinc=2 binc=2

! TEST FOR LARGE HALFWORD FILE, FULL DATA RANGE
Gen hgen_1 nl=500 ns=8000 Linc=20 Sinc=5 'Half
tfilt hgen_1 (test1_1,x) 'LOW
List test1_1 (1,1,10,10)
List test1_1 (1,6545,20,20)

! run tfilt with fullword
GEN OUT=HGEN NL=20 NS=22 'full
LIST INP=HGEN 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X)
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'high
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'scene
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'low
LIST INP=TEST3 'ZEROES


! run tfilt with real:
GEN OUT=HGEN NL=20 NS=22 'real
LIST INP=HGEN 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X)
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'high
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'scene
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) 'low
LIST INP=TEST3 'ZEROES

! clean up:
ush rm -f ?
ush rm -f TEST?
ush rm -f TEST??
ush rm -f GEN
ush rm -f ?GEN
ush rm -f hgen_1
ush rm -f test1_1

end-proc
$!-----------------------------------------------------------------------------
$ create tsttfilt.log_solos
tsttfilt
GEN OUT=GEN NL=20 NS=17
Beginning VICAR task GEN
GEN Version 6
GEN task completed
LIST INP=GEN 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
      2       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      3       2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
      4       3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
      5       4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
      6       5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21
      7       6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22
      8       7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23
      9       8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
     10       9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25
     11      10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26
     12      11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27
     13      12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
     14      13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29
     15      14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
     16      15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     17      16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
     18      17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
     19      18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
     20      19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
tfilt INP=GEN OUT=(TEST1,X)
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST1 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:22 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0  60  75  82  86  88  90  91  92  93  94  94  95  95  95  96 100
      2      60 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      3      75 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      4      82 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      86 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST2,X)
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST2 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:23 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0  60  75  82  86  88  90  91  92  93  94  94  95  95  95  96 100
      2      60 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      3      75 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      4      82 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      86 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST4,X) NLW=3
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST4  'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:23 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0  60  75  82  86  88  90  91  92  93  94  94  95  95  95  96 100
      2      60 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      3      75 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      4      82 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      86 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST6,X) NLW=2 NSW=3
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST6 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:24 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0  60  75  82  86  88  90  91  92  93  94  94  95  95  95  96 100
      2      60 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      3      75 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      4      82 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      86 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST7,X) NLW=2 NSW=3 THRESH=4
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST7 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:25 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0  82  88  90  91  92  93  94  94  95  95  95  96 100
      2       0   0   0  86  95 100 100 100 100 100 100 100 100 100 100 100 104
      3       0   0  86  95 100 100 100 100 100 100 100 100 100 100 100 100 104
      4       0  86  95 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      82  95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST8,X) NLW=3 NSW=3 THRESH=4 'HIGH
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST8 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:25 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0 127 127 127 127 127 127 127 127 127 127 127 127 128
      2       0   0   0 127 127 128 128 128 128 128 128 128 128 128 128 128 128
      3       0   0 127 127 128 128 128 128 128 128 128 128 128 128 128 128 128
      4       0 127 127 128 128 128 128 128 128 128 128 128 128 128 128 128 128
      5     127 127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
      6     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
      7     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
      8     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
      9     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     10     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     11     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     12     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     13     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     14     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     15     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     16     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     17     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     18     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     19     127 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
     20     128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 129
tfilt INP=GEN OUT=(TEST9,X) THRESH=3 'SCENE
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST9 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:26 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0 105 113 116 118 119 120 121 121 122 122 123 123 123 128
      2       0   0 109 122 128 128 128 128 128 128 128 128 128 128 128 128 132
      3       0 109 122 128 128 128 128 128 128 128 128 128 128 128 128 128 131
      4     105 122 128 128 128 128 128 128 128 128 128 128 128 128 128 128 131
      5     113 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 131
      6     116 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 131
      7     118 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 131
      8     119 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
      9     120 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     10     121 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     11     121 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     12     122 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     13     122 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     14     123 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     15     123 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     16     123 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     17     124 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     18     124 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     19     124 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 130
     20     128 131 131 131 130 130 130 130 130 130 130 130 130 130 130 130 131
tfilt INP=GEN OUT=(TEST10,X) NLW=4 'LOW
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          5
 NSW=          3
LIST INP=TEST10 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:27 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       2   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      2       2   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      3       3   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  17
      4       4   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  18
      5       5   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  19
      6       6   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  20
      7       7   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  21
      8       8   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  22
      9       9   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  23
     10      10  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  24
     11      11  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  25
     12      12  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  26
     13      13  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  27
     14      14  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  28
     15      15  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  29
     16      16  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  30
     17      17  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  31
     18      18  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  32
     19      18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  33
     20      18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  33
tfilt INP=GEN OUT=(TEST11,X) 'DIVIDE
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST11 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:27 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0  60  75  82  86  88  90  91  92  93  94  94  95  95  95  96 100
      2      60 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      3      75 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      4      82 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 104
      5      86 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      6      88 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      7      90 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      8      91 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
      9      92 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     10      93 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     11      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     12      94 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 103
     13      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     14      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     15      95 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     16      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     17      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     18      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     19      96 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 102
     20     100 103 103 103 103 103 103 103 103 102 102 102 102 102 102 102 104
tfilt INP=GEN OUT=(TEST12,X) NLW=2 'HIGH BOOST=2 DCTRAN=0.1 DCLEVEL=50.0
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          3
LIST INP=TEST12 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:28 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1      47  48  48  48  49  49  49  49  49  49  49  49  49  49  50  50  51
      2      48  50  50  50  50  50  50  50  50  51  51  51  51  51  51  51  53
      3      48  50  50  50  50  50  50  50  51  51  51  51  51  51  51  51  53
      4      48  50  50  50  50  50  50  51  51  51  51  51  51  51  51  51  53
      5      49  50  50  50  50  50  51  51  51  51  51  51  51  51  51  51  53
      6      49  50  50  50  50  51  51  51  51  51  51  51  51  51  51  52  53
      7      49  50  50  50  51  51  51  51  51  51  51  51  51  51  52  52  53
      8      49  50  50  51  51  51  51  51  51  51  51  51  51  52  52  52  53
      9      49  50  51  51  51  51  51  51  51  51  51  51  52  52  52  52  53
     10      49  51  51  51  51  51  51  51  51  51  51  52  52  52  52  52  53
     11      49  51  51  51  51  51  51  51  51  51  52  52  52  52  52  52  53
     12      49  51  51  51  51  51  51  51  51  52  52  52  52  52  52  52  54
     13      49  51  51  51  51  51  51  51  52  52  52  52  52  52  52  52  54
     14      49  51  51  51  51  51  51  52  52  52  52  52  52  52  52  52  54
     15      50  51  51  51  51  51  52  52  52  52  52  52  52  52  52  52  54
     16      50  51  51  51  51  52  52  52  52  52  52  52  52  52  52  53  54
     17      50  51  51  51  52  52  52  52  52  52  52  52  52  52  53  53  54
     18      50  51  51  52  52  52  52  52  52  52  52  52  52  53  53  53  54
     19      50  51  52  52  52  52  52  52  52  52  52  52  53  53  53  53  54
     20      51  53  53  53  53  53  53  53  54  54  54  54  54  54  54  54  56
tfilt INP=GEN OUT=(TEST14,X) NLW=2 NSW=4 'LOW BOOST=2.00 OFFSET=20.
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          1
 NLW=          3
 NSW=          5
LIST INP=TEST14 'ZEROES
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:21 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1      23  24  25  27  29  31  33  35  37  39  41  43  45  47  49  50  50
      2      24  24  26  28  30  32  34  36  38  40  42  44  46  48  50  51  51
      3      26  26  28  30  32  34  36  38  40  42  44  46  48  50  52  53  53
      4      28  28  30  32  34  36  38  40  42  44  46  48  50  52  54  55  55
      5      30  30  32  34  36  38  40  42  44  46  48  50  52  54  56  57  57
      6      32  32  34  36  38  40  42  44  46  48  50  52  54  56  58  59  59
      7      34  34  36  38  40  42  44  46  48  50  52  54  56  58  60  61  61
      8      36  36  38  40  42  44  46  48  50  52  54  56  58  60  62  63  63
      9      38  38  40  42  44  46  48  50  52  54  56  58  60  62  64  65  65
     10      40  40  42  44  46  48  50  52  54  56  58  60  62  64  66  67  67
     11      42  42  44  46  48  50  52  54  56  58  60  62  64  66  68  69  69
     12      44  44  46  48  50  52  54  56  58  60  62  64  66  68  70  71  71
     13      46  46  48  50  52  54  56  58  60  62  64  66  68  70  72  73  73
     14      48  48  50  52  54  56  58  60  62  64  66  68  70  72  74  75  75
     15      50  50  52  54  56  58  60  62  64  66  68  70  72  74  76  77  77
     16      52  52  54  56  58  60  62  64  66  68  70  72  74  76  78  79  79
     17      54  54  56  58  60  62  64  66  68  70  72  74  76  78  80  81  81
     18      56  56  58  60  62  64  66  68  70  72  74  76  78  80  82  83  83
     19      58  58  60  62  64  66  68  70  72  74  76  78  80  82  84  85  85
     20      59  59  60  62  64  66  68  70  72  74  76  78  80  82  84  85  86
GEN OUT=HGEN NL=20 NS=22 'HALF
Beginning VICAR task GEN
GEN Version 6
GEN task completed
LIST INP=HGEN 'ZEROES
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     1     2     3     4     5     6     7     8     9    10    11    12    13    14
      2         1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
      3         2     3     4     5     6     7     8     9    10    11    12    13    14    15    16
      4         3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
      5         4     5     6     7     8     9    10    11    12    13    14    15    16    17    18
      6         5     6     7     8     9    10    11    12    13    14    15    16    17    18    19
      7         6     7     8     9    10    11    12    13    14    15    16    17    18    19    20
      8         7     8     9    10    11    12    13    14    15    16    17    18    19    20    21
      9         8     9    10    11    12    13    14    15    16    17    18    19    20    21    22
     10         9    10    11    12    13    14    15    16    17    18    19    20    21    22    23
     11        10    11    12    13    14    15    16    17    18    19    20    21    22    23    24
     12        11    12    13    14    15    16    17    18    19    20    21    22    23    24    25
     13        12    13    14    15    16    17    18    19    20    21    22    23    24    25    26
     14        13    14    15    16    17    18    19    20    21    22    23    24    25    26    27
     15        14    15    16    17    18    19    20    21    22    23    24    25    26    27    28
     16        15    16    17    18    19    20    21    22    23    24    25    26    27    28    29
     17        16    17    18    19    20    21    22    23    24    25    26    27    28    29    30
     18        17    18    19    20    21    22    23    24    25    26    27    28    29    30    31
     19        18    19    20    21    22    23    24    25    26    27    28    29    30    31    32
     20        19    20    21    22    23    24    25    26    27    28    29    30    31    32    33

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
     Samp      16    17    18    19    20    21    22
   Line
      1        15    16    17    18    19    20    21
      2        16    17    18    19    20    21    22
      3        17    18    19    20    21    22    23
      4        18    19    20    21    22    23    24
      5        19    20    21    22    23    24    25
      6        20    21    22    23    24    25    26
      7        21    22    23    24    25    26    27
      8        22    23    24    25    26    27    28
      9        23    24    25    26    27    28    29
     10        24    25    26    27    28    29    30
     11        25    26    27    28    29    30    31
     12        26    27    28    29    30    31    32
     13        27    28    29    30    31    32    33
     14        28    29    30    31    32    33    34
     15        29    30    31    32    33    34    35
     16        30    31    32    33    34    35    36
     17        31    32    33    34    35    36    37
     18        32    33    34    35    36    37    38
     19        33    34    35    36    37    38    39
     20        34    35    36    37    38    39    40
tfilt INP=HGEN OUT=(TEST3,X)
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          2
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0    60    75    82    86    88    90    91    92    93    94    94    95    95    95
      2        60   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      3        75   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      4        82   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      5        86   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      6        88   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      7        90   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      8        91   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      9        92   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     10        93   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     11        94   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     12        94   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     13        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     14        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     15        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     16        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     17        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     18        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     19        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     20       100   103   103   103   103   103   103   103   103   102   102   102   102   102   102

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
     Samp      16    17    18    19    20    21    22
   Line
      1        96    96    96    96    97    97   100
      2       100   100   100   100   100   100   103
      3       100   100   100   100   100   100   103
      4       100   100   100   100   100   100   103
      5       100   100   100   100   100   100   103
      6       100   100   100   100   100   100   103
      7       100   100   100   100   100   100   103
      8       100   100   100   100   100   100   102
      9       100   100   100   100   100   100   102
     10       100   100   100   100   100   100   102
     11       100   100   100   100   100   100   102
     12       100   100   100   100   100   100   102
     13       100   100   100   100   100   100   102
     14       100   100   100   100   100   100   102
     15       100   100   100   100   100   100   102
     16       100   100   100   100   100   100   102
     17       100   100   100   100   100   100   102
     18       100   100   100   100   100   100   102
     19       100   100   100   100   100   100   102
     20       102   102   102   102   102   102   103
tfilt INP=HGEN OUT=(TEST5,X) NSW=2
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          2
 NLW=          3
 NSW=          3
LIST INP=TEST5 'ZEROES
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:30 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0    60    75    82    86    88    90    91    92    93    94    94    95    95    95
      2        60   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      3        75   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      4        82   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      5        86   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      6        88   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      7        90   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      8        91   100   100   100   100   100   100   100   100   100   100   100   100   100   100
      9        92   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     10        93   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     11        94   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     12        94   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     13        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     14        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     15        95   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     16        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     17        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     18        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     19        96   100   100   100   100   100   100   100   100   100   100   100   100   100   100
     20       100   103   103   103   103   103   103   103   103   102   102   102   102   102   102

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:30 2012
     Samp      16    17    18    19    20    21    22
   Line
      1        96    96    96    96    97    97   100
      2       100   100   100   100   100   100   103
      3       100   100   100   100   100   100   103
      4       100   100   100   100   100   100   103
      5       100   100   100   100   100   100   103
      6       100   100   100   100   100   100   103
      7       100   100   100   100   100   100   103
      8       100   100   100   100   100   100   102
      9       100   100   100   100   100   100   102
     10       100   100   100   100   100   100   102
     11       100   100   100   100   100   100   102
     12       100   100   100   100   100   100   102
     13       100   100   100   100   100   100   102
     14       100   100   100   100   100   100   102
     15       100   100   100   100   100   100   102
     16       100   100   100   100   100   100   102
     17       100   100   100   100   100   100   102
     18       100   100   100   100   100   100   102
     19       100   100   100   100   100   100   102
     20       102   102   102   102   102   102   103
tfilt INP=HGEN OUT=(TEST13,X) NLW=2 NSW=2 THRESH=4 'SCENE  +
 SCALE=200.0 DCTRAN=0.3 DCLEVEL=10
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          2
 NLW=          3
 NSW=          3
LIST  INP=TEST13  'ZEROES
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     0     0     0   -24   -12    -8    -5    -2    -1     0     1     3     4     5
      2         0     0     0   -17     1    11    12    12    12    13    13    13    13    14    14
      3         0     0   -17     1    11    12    12    12    13    13    13    13    14    14    14
      4         0   -17     1    11    12    12    12    13    13    13    13    14    14    14    15
      5       -24     1    11    12    12    12    13    13    13    13    14    14    14    15    15
      6       -12    11    12    12    12    13    13    13    13    14    14    14    15    15    15
      7        -8    12    12    12    13    13    13    13    14    14    14    15    15    15    16
      8        -5    12    12    13    13    13    13    14    14    14    15    15    15    16    16
      9        -2    12    13    13    13    13    14    14    14    15    15    15    16    16    16
     10        -1    13    13    13    13    14    14    14    15    15    15    16    16    16    16
     11         0    13    13    13    14    14    14    15    15    15    16    16    16    16    17
     12         1    13    13    14    14    14    15    15    15    16    16    16    16    17    17
     13         3    13    14    14    14    15    15    15    16    16    16    16    17    17    17
     14         4    14    14    14    15    15    15    16    16    16    16    17    17    17    18
     15         5    14    14    15    15    15    16    16    16    16    17    17    17    18    18
     16         5    14    15    15    15    16    16    16    16    17    17    17    18    18    18
     17         6    15    15    15    16    16    16    16    17    17    17    18    18    18    19
     18         7    15    15    16    16    16    16    17    17    17    18    18    18    19    19
     19         8    15    16    16    16    16    17    17    17    18    18    18    19    19    19
     20        15    22    22    22    22    22    22    23    23    23    23    23    23    23    24

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:29 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
     Samp      16    17    18    19    20    21    22
   Line
      1         5     6     7     8     8     9    16
      2        14    15    15    15    16    16    22
      3        15    15    15    16    16    16    22
      4        15    15    16    16    16    16    22
      5        15    16    16    16    16    17    22
      6        16    16    16    16    17    17    23
      7        16    16    16    17    17    17    23
      8        16    16    17    17    17    18    23
      9        16    17    17    17    18    18    23
     10        17    17    17    18    18    18    23
     11        17    17    18    18    18    19    23
     12        17    18    18    18    19    19    23
     13        18    18    18    19    19    19    24
     14        18    18    19    19    19    19    24
     15        18    19    19    19    19    20    24
     16        19    19    19    19    20    20    24
     17        19    19    19    20    20    20    24
     18        19    19    20    20    20    21    24
     19        19    20    20    20    21    21    25
     20        24    24    24    24    24    25    28
GEN OUT=MGEN NL=20 NS=22 NB=10 'HALF
Beginning VICAR task GEN
GEN Version 6
GEN task completed
tfilt INP=MGEN OUT=(TEST15,X,Y) NLW=2 NSW=2 THRESH=4 'LOW
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          2
 NLW=          3
 NSW=          3
LIST  INP=TEST15 'ZEROES sinc=2 binc=2
Beginning VICAR task LIST

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 ***********
 Band =     1
 ***********
     Samp       1     3     5     7     9    11    13    15    17    19    21
   Line
      1         0     0     5     7     9    11    13    15    17    19    21
      2         0     0     5     7     9    11    13    15    17    19    21
      3         0     5     6     8    10    12    14    16    18    20    22
      4         0     5     7     9    11    13    15    17    19    21    23
      5         5     6     8    10    12    14    16    18    20    22    24
      6         6     7     9    11    13    15    17    19    21    23    25
      7         7     8    10    12    14    16    18    20    22    24    26
      8         8     9    11    13    15    17    19    21    23    25    27
      9         9    10    12    14    16    18    20    22    24    26    28
     10        10    11    13    15    17    19    21    23    25    27    29
     11        11    12    14    16    18    20    22    24    26    28    30
     12        12    13    15    17    19    21    23    25    27    29    31
     13        13    14    16    18    20    22    24    26    28    30    32
     14        14    15    17    19    21    23    25    27    29    31    33
     15        15    16    18    20    22    24    26    28    30    32    34
     16        16    17    19    21    23    25    27    29    31    33    35
     17        17    18    20    22    24    26    28    30    32    34    36
     18        18    19    21    23    25    27    29    31    33    35    37
     19        19    20    22    24    26    28    30    32    34    36    38
     20        19    20    22    24    26    28    30    32    34    36    38


 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 ***********
 Band =     3
 ***********
     Samp       1     3     5     7     9    11    13    15    17    19    21
   Line
      1         0     5     7     9    11    13    15    17    19    21    23
      2         0     5     7     9    11    13    15    17    19    21    23
      3         5     6     8    10    12    14    16    18    20    22    24
      4         6     7     9    11    13    15    17    19    21    23    25
      5         7     8    10    12    14    16    18    20    22    24    26
      6         8     9    11    13    15    17    19    21    23    25    27
      7         9    10    12    14    16    18    20    22    24    26    28
      8        10    11    13    15    17    19    21    23    25    27    29
      9        11    12    14    16    18    20    22    24    26    28    30
     10        12    13    15    17    19    21    23    25    27    29    31
     11        13    14    16    18    20    22    24    26    28    30    32
     12        14    15    17    19    21    23    25    27    29    31    33
     13        15    16    18    20    22    24    26    28    30    32    34
     14        16    17    19    21    23    25    27    29    31    33    35
     15        17    18    20    22    24    26    28    30    32    34    36
     16        18    19    21    23    25    27    29    31    33    35    37
     17        19    20    22    24    26    28    30    32    34    36    38
     18        20    21    23    25    27    29    31    33    35    37    39
     19        21    22    24    26    28    30    32    34    36    38    40
     20        21    22    24    26    28    30    32    34    36    38    40


 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 ***********
 Band =     5
 ***********
     Samp       1     3     5     7     9    11    13    15    17    19    21
   Line
      1         5     7     9    11    13    15    17    19    21    23    25
      2         6     7     9    11    13    15    17    19    21    23    25
      3         7     8    10    12    14    16    18    20    22    24    26
      4         8     9    11    13    15    17    19    21    23    25    27
      5         9    10    12    14    16    18    20    22    24    26    28
      6        10    11    13    15    17    19    21    23    25    27    29
      7        11    12    14    16    18    20    22    24    26    28    30
      8        12    13    15    17    19    21    23    25    27    29    31
      9        13    14    16    18    20    22    24    26    28    30    32
     10        14    15    17    19    21    23    25    27    29    31    33
     11        15    16    18    20    22    24    26    28    30    32    34
     12        16    17    19    21    23    25    27    29    31    33    35
     13        17    18    20    22    24    26    28    30    32    34    36
     14        18    19    21    23    25    27    29    31    33    35    37
     15        19    20    22    24    26    28    30    32    34    36    38
     16        20    21    23    25    27    29    31    33    35    37    39
     17        21    22    24    26    28    30    32    34    36    38    40
     18        22    23    25    27    29    31    33    35    37    39    41
     19        23    24    26    28    30    32    34    36    38    40    42
     20        23    24    26    28    30    32    34    36    38    40    42


 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 ***********
 Band =     7
 ***********
     Samp       1     3     5     7     9    11    13    15    17    19    21
   Line
      1         7     9    11    13    15    17    19    21    23    25    27
      2         8     9    11    13    15    17    19    21    23    25    27
      3         9    10    12    14    16    18    20    22    24    26    28
      4        10    11    13    15    17    19    21    23    25    27    29
      5        11    12    14    16    18    20    22    24    26    28    30
      6        12    13    15    17    19    21    23    25    27    29    31
      7        13    14    16    18    20    22    24    26    28    30    32
      8        14    15    17    19    21    23    25    27    29    31    33
      9        15    16    18    20    22    24    26    28    30    32    34
     10        16    17    19    21    23    25    27    29    31    33    35
     11        17    18    20    22    24    26    28    30    32    34    36
     12        18    19    21    23    25    27    29    31    33    35    37
     13        19    20    22    24    26    28    30    32    34    36    38
     14        20    21    23    25    27    29    31    33    35    37    39
     15        21    22    24    26    28    30    32    34    36    38    40
     16        22    23    25    27    29    31    33    35    37    39    41
     17        23    24    26    28    30    32    34    36    38    40    42
     18        24    25    27    29    31    33    35    37    39    41    43
     19        25    26    28    30    32    34    36    38    40    42    44
     20        25    26    28    30    32    34    36    38    40    42    44


 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:31 2012
 ***********
 Band =     9
 ***********
     Samp       1     3     5     7     9    11    13    15    17    19    21
   Line
      1         9    11    13    15    17    19    21    23    25    27    29
      2        10    11    13    15    17    19    21    23    25    27    29
      3        11    12    14    16    18    20    22    24    26    28    30
      4        12    13    15    17    19    21    23    25    27    29    31
      5        13    14    16    18    20    22    24    26    28    30    32
      6        14    15    17    19    21    23    25    27    29    31    33
      7        15    16    18    20    22    24    26    28    30    32    34
      8        16    17    19    21    23    25    27    29    31    33    35
      9        17    18    20    22    24    26    28    30    32    34    36
     10        18    19    21    23    25    27    29    31    33    35    37
     11        19    20    22    24    26    28    30    32    34    36    38
     12        20    21    23    25    27    29    31    33    35    37    39
     13        21    22    24    26    28    30    32    34    36    38    40
     14        22    23    25    27    29    31    33    35    37    39    41
     15        23    24    26    28    30    32    34    36    38    40    42
     16        24    25    27    29    31    33    35    37    39    41    43
     17        25    26    28    30    32    34    36    38    40    42    44
     18        26    27    29    31    33    35    37    39    41    43    45
     19        27    28    30    32    34    36    38    40    42    44    46
     20        27    28    30    32    34    36    38    40    42    44    46
Gen hgen_1 nl=500 ns=8000 Linc=20 Sinc=5 'Half
Beginning VICAR task Gen
GEN Version 6
GEN task completed
tfilt hgen_1 (test1_1,x) 'LOW
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          2
 NLW=         51
 NSW=         51
List test1_1 (1,1,10,10)
Beginning VICAR task List

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:32 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:33 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       319   319   319   320   320   321   322   323   325   327
      2       319   319   319   320   321   321   323   324   325   327
      3       320   320   321   321   322   323   324   325   326   328
      4       322   322   323   323   324   325   326   327   328   330
      5       325   325   325   326   326   327   328   330   331   333
      6       328   329   329   329   330   331   332   333   335   336
      7       333   333   333   334   334   335   336   338   339   341
      8       338   338   338   339   339   340   341   343   344   346
      9       344   344   344   345   345   346   347   349   350   352
     10       350   350   351   351   352   353   354   355   357   358
List test1_1 (1,6545,20,20)
Beginning VICAR task List

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:32 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:33 2012
     Samp    6545  6546  6547  6548  6549  6550  6551  6552  6553  6554  6555  6556  6557  6558  6559
   Line
      1     32709 32711 32713 32714 32716 32718 32719 32721 32723 32724     0     0     0     0     0
      2     32709 32711 32712 32714 32716 32718     0     0     0     0     0     0     0     0     0
      3     32709 32711     0     0     0     0     0     0     0     0     0     0     0     0     0
GEN OUT=HGEN NL=20 NS=22 'full
Beginning VICAR task GEN
GEN Version 6
GEN task completed
LIST INP=HGEN 'ZEROES
Beginning VICAR task LIST

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13
      6              5          6          7          8          9         10         11         12         13         14
      7              6          7          8          9         10         11         12         13         14         15
      8              7          8          9         10         11         12         13         14         15         16
      9              8          9         10         11         12         13         14         15         16         17
     10              9         10         11         12         13         14         15         16         17         18
     11             10         11         12         13         14         15         16         17         18         19
     12             11         12         13         14         15         16         17         18         19         20
     13             12         13         14         15         16         17         18         19         20         21
     14             13         14         15         16         17         18         19         20         21         22
     15             14         15         16         17         18         19         20         21         22         23
     16             15         16         17         18         19         20         21         22         23         24
     17             16         17         18         19         20         21         22         23         24         25
     18             17         18         19         20         21         22         23         24         25         26
     19             18         19         20         21         22         23         24         25         26         27
     20             19         20         21         22         23         24         25         26         27         28

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1             10         11         12         13         14         15         16         17         18         19
      2             11         12         13         14         15         16         17         18         19         20
      3             12         13         14         15         16         17         18         19         20         21
      4             13         14         15         16         17         18         19         20         21         22
      5             14         15         16         17         18         19         20         21         22         23
      6             15         16         17         18         19         20         21         22         23         24
      7             16         17         18         19         20         21         22         23         24         25
      8             17         18         19         20         21         22         23         24         25         26
      9             18         19         20         21         22         23         24         25         26         27
     10             19         20         21         22         23         24         25         26         27         28
     11             20         21         22         23         24         25         26         27         28         29
     12             21         22         23         24         25         26         27         28         29         30
     13             22         23         24         25         26         27         28         29         30         31
     14             23         24         25         26         27         28         29         30         31         32
     15             24         25         26         27         28         29         30         31         32         33
     16             25         26         27         28         29         30         31         32         33         34
     17             26         27         28         29         30         31         32         33         34         35
     18             27         28         29         30         31         32         33         34         35         36
     19             28         29         30         31         32         33         34         35         36         37
     20             29         30         31         32         33         34         35         36         37         38

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp           21         22
   Line
      1             20         21
      2             21         22
      3             22         23
      4             23         24
      5             24         25
      6             25         26
      7             26         27
      8             27         28
      9             28         29
     10             29         30
     11             30         31
     12             31         32
     13             32         33
     14             33         34
     15             34         35
     16             35         36
     17             36         37
     18             37         38
     19             38         39
     20             39         40
tfilt INP=HGEN OUT=(TEST3,X)
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0         60         75         82         86         88         90         91         92         93
      2             60        100        100        100        100        100        100        100        100        100
      3             75        100        100        100        100        100        100        100        100        100
      4             82        100        100        100        100        100        100        100        100        100
      5             86        100        100        100        100        100        100        100        100        100
      6             88        100        100        100        100        100        100        100        100        100
      7             90        100        100        100        100        100        100        100        100        100
      8             91        100        100        100        100        100        100        100        100        100
      9             92        100        100        100        100        100        100        100        100        100
     10             93        100        100        100        100        100        100        100        100        100
     11             94        100        100        100        100        100        100        100        100        100
     12             94        100        100        100        100        100        100        100        100        100
     13             95        100        100        100        100        100        100        100        100        100
     14             95        100        100        100        100        100        100        100        100        100
     15             95        100        100        100        100        100        100        100        100        100
     16             96        100        100        100        100        100        100        100        100        100
     17             96        100        100        100        100        100        100        100        100        100
     18             96        100        100        100        100        100        100        100        100        100
     19             96        100        100        100        100        100        100        100        100        100
     20            100        103        103        103        103        103        103        103        103        102

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1             94         94         95         95         95         96         96         96         96         97
      2            100        100        100        100        100        100        100        100        100        100
      3            100        100        100        100        100        100        100        100        100        100
      4            100        100        100        100        100        100        100        100        100        100
      5            100        100        100        100        100        100        100        100        100        100
      6            100        100        100        100        100        100        100        100        100        100
      7            100        100        100        100        100        100        100        100        100        100
      8            100        100        100        100        100        100        100        100        100        100
      9            100        100        100        100        100        100        100        100        100        100
     10            100        100        100        100        100        100        100        100        100        100
     11            100        100        100        100        100        100        100        100        100        100
     12            100        100        100        100        100        100        100        100        100        100
     13            100        100        100        100        100        100        100        100        100        100
     14            100        100        100        100        100        100        100        100        100        100
     15            100        100        100        100        100        100        100        100        100        100
     16            100        100        100        100        100        100        100        100        100        100
     17            100        100        100        100        100        100        100        100        100        100
     18            100        100        100        100        100        100        100        100        100        100
     19            100        100        100        100        100        100        100        100        100        100
     20            102        102        102        102        102        102        102        102        102        102

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
     Samp           21         22
   Line
      1             97        100
      2            100        103
      3            100        103
      4            100        103
      5            100        103
      6            100        103
      7            100        103
      8            100        102
      9            100        102
     10            100        102
     11            100        102
     12            100        102
     13            100        102
     14            100        102
     15            100        102
     16            100        102
     17            100        102
     18            100        102
     19            100        102
     20            102        103
tfilt INP=HGEN OUT=(TEST3,X) 'high
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:37 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1            126        127        127        127        127        127        127        127        127        127
      2            127        128        128        128        128        128        128        128        128        128
      3            127        128        128        128        128        128        128        128        128        128
      4            127        128        128        128        128        128        128        128        128        128
      5            127        128        128        128        128        128        128        128        128        128
      6            127        128        128        128        128        128        128        128        128        128
      7            127        128        128        128        128        128        128        128        128        128
      8            127        128        128        128        128        128        128        128        128        128
      9            127        128        128        128        128        128        128        128        128        128
     10            127        128        128        128        128        128        128        128        128        128
     11            127        128        128        128        128        128        128        128        128        128
     12            127        128        128        128        128        128        128        128        128        128
     13            127        128        128        128        128        128        128        128        128        128
     14            127        128        128        128        128        128        128        128        128        128
     15            127        128        128        128        128        128        128        128        128        128
     16            127        128        128        128        128        128        128        128        128        128
     17            127        128        128        128        128        128        128        128        128        128
     18            127        128        128        128        128        128        128        128        128        128
     19            127        128        128        128        128        128        128        128        128        128
     20            128        128        128        128        128        128        128        128        128        128

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:37 2012
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1            127        127        127        127        127        127        127        127        127        127
      2            128        128        128        128        128        128        128        128        128        128
      3            128        128        128        128        128        128        128        128        128        128
      4            128        128        128        128        128        128        128        128        128        128
      5            128        128        128        128        128        128        128        128        128        128
      6            128        128        128        128        128        128        128        128        128        128
      7            128        128        128        128        128        128        128        128        128        128
      8            128        128        128        128        128        128        128        128        128        128
      9            128        128        128        128        128        128        128        128        128        128
     10            128        128        128        128        128        128        128        128        128        128
     11            128        128        128        128        128        128        128        128        128        128
     12            128        128        128        128        128        128        128        128        128        128
     13            128        128        128        128        128        128        128        128        128        128
     14            128        128        128        128        128        128        128        128        128        128
     15            128        128        128        128        128        128        128        128        128        128
     16            128        128        128        128        128        128        128        128        128        128
     17            128        128        128        128        128        128        128        128        128        128
     18            128        128        128        128        128        128        128        128        128        128
     19            128        128        128        128        128        128        128        128        128        128
     20            128        128        128        128        128        128        128        128        128        128

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:37 2012
     Samp           21         22
   Line
      1            127        128
      2            128        128
      3            128        128
      4            128        128
      5            128        128
      6            128        128
      7            128        128
      8            128        128
      9            128        128
     10            128        128
     11            128        128
     12            128        128
     13            128        128
     14            128        128
     15            128        128
     16            128        128
     17            128        128
     18            128        128
     19            128        128
     20            128        129
tfilt INP=HGEN OUT=(TEST3,X) 'scene
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1             27         88        103        109        113        116        118        119        120        121
      2             88        128        128        128        128        128        128        128        128        128
      3            103        128        128        128        128        128        128        128        128        128
      4            109        128        128        128        128        128        128        128        128        128
      5            113        128        128        128        128        128        128        128        128        128
      6            116        128        128        128        128        128        128        128        128        128
      7            118        128        128        128        128        128        128        128        128        128
      8            119        128        128        128        128        128        128        128        128        128
      9            120        128        128        128        128        128        128        128        128        128
     10            121        128        128        128        128        128        128        128        128        128
     11            121        128        128        128        128        128        128        128        128        128
     12            122        128        128        128        128        128        128        128        128        128
     13            122        128        128        128        128        128        128        128        128        128
     14            123        128        128        128        128        128        128        128        128        128
     15            123        128        128        128        128        128        128        128        128        128
     16            123        128        128        128        128        128        128        128        128        128
     17            124        128        128        128        128        128        128        128        128        128
     18            124        128        128        128        128        128        128        128        128        128
     19            124        128        128        128        128        128        128        128        128        128
     20            128        131        131        131        130        130        130        130        130        130

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1            121        122        122        123        123        123        124        124        124        124
      2            128        128        128        128        128        128        128        128        128        128
      3            128        128        128        128        128        128        128        128        128        128
      4            128        128        128        128        128        128        128        128        128        128
      5            128        128        128        128        128        128        128        128        128        128
      6            128        128        128        128        128        128        128        128        128        128
      7            128        128        128        128        128        128        128        128        128        128
      8            128        128        128        128        128        128        128        128        128        128
      9            128        128        128        128        128        128        128        128        128        128
     10            128        128        128        128        128        128        128        128        128        128
     11            128        128        128        128        128        128        128        128        128        128
     12            128        128        128        128        128        128        128        128        128        128
     13            128        128        128        128        128        128        128        128        128        128
     14            128        128        128        128        128        128        128        128        128        128
     15            128        128        128        128        128        128        128        128        128        128
     16            128        128        128        128        128        128        128        128        128        128
     17            128        128        128        128        128        128        128        128        128        128
     18            128        128        128        128        128        128        128        128        128        128
     19            128        128        128        128        128        128        128        128        128        128
     20            130        130        130        130        130        130        129        129        129        129

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp           21         22
   Line
      1            124        128
      2            128        131
      3            128        130
      4            128        130
      5            128        130
      6            128        130
      7            128        130
      8            128        130
      9            128        130
     10            128        130
     11            128        130
     12            128        130
     13            128        130
     14            128        130
     15            128        129
     16            128        129
     17            128        129
     18            128        129
     19            128        129
     20            129        131
tfilt INP=HGEN OUT=(TEST3,X) 'low
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              1          2          3          4          5          6          7          8          9         10
      2              2          2          3          4          5          6          7          8          9         10
      3              3          3          4          5          6          7          8          9         10         11
      4              4          4          5          6          7          8          9         10         11         12
      5              5          5          6          7          8          9         10         11         12         13
      6              6          6          7          8          9         10         11         12         13         14
      7              7          7          8          9         10         11         12         13         14         15
      8              8          8          9         10         11         12         13         14         15         16
      9              9          9         10         11         12         13         14         15         16         17
     10             10         10         11         12         13         14         15         16         17         18
     11             11         11         12         13         14         15         16         17         18         19
     12             12         12         13         14         15         16         17         18         19         20
     13             13         13         14         15         16         17         18         19         20         21
     14             14         14         15         16         17         18         19         20         21         22
     15             15         15         16         17         18         19         20         21         22         23
     16             16         16         17         18         19         20         21         22         23         24
     17             17         17         18         19         20         21         22         23         24         25
     18             18         18         19         20         21         22         23         24         25         26
     19             19         19         20         21         22         23         24         25         26         27
     20             19         19         20         21         22         23         24         25         26         27

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1             11         12         13         14         15         16         17         18         19         20
      2             11         12         13         14         15         16         17         18         19         20
      3             12         13         14         15         16         17         18         19         20         21
      4             13         14         15         16         17         18         19         20         21         22
      5             14         15         16         17         18         19         20         21         22         23
      6             15         16         17         18         19         20         21         22         23         24
      7             16         17         18         19         20         21         22         23         24         25
      8             17         18         19         20         21         22         23         24         25         26
      9             18         19         20         21         22         23         24         25         26         27
     10             19         20         21         22         23         24         25         26         27         28
     11             20         21         22         23         24         25         26         27         28         29
     12             21         22         23         24         25         26         27         28         29         30
     13             22         23         24         25         26         27         28         29         30         31
     14             23         24         25         26         27         28         29         30         31         32
     15             24         25         26         27         28         29         30         31         32         33
     16             25         26         27         28         29         30         31         32         33         34
     17             26         27         28         29         30         31         32         33         34         35
     18             27         28         29         30         31         32         33         34         35         36
     19             28         29         30         31         32         33         34         35         36         37
     20             28         29         30         31         32         33         34         35         36         37

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:36 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp           21         22
   Line
      1             21         21
      2             21         21
      3             22         22
      4             23         23
      5             24         24
      6             25         25
      7             26         26
      8             27         27
      9             28         28
     10             29         29
     11             30         30
     12             31         31
     13             32         32
     14             33         33
     15             34         34
     16             35         35
     17             36         36
     18             37         37
     19             38         38
     20             38         39
GEN OUT=HGEN NL=20 NS=22 'real
Beginning VICAR task GEN
GEN Version 6
GEN task completed
LIST INP=HGEN 'ZEROES
Beginning VICAR task LIST

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     11       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     12       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     13       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     14       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
     15       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01
     16       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01
     17       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01
     18       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01
     19       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01
     20       1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      2       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      3       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
      4       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
      5       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01
      6       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01
      7       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01
      8       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01
      9       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01
     10       1.900E+01   2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01
     11       2.000E+01   2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01
     12       2.100E+01   2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01
     13       2.200E+01   2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01
     14       2.300E+01   2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01
     15       2.400E+01   2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01
     16       2.500E+01   2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01   3.400E+01
     17       2.600E+01   2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01   3.400E+01   3.500E+01
     18       2.700E+01   2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01   3.400E+01   3.500E+01   3.600E+01
     19       2.800E+01   2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01   3.400E+01   3.500E+01   3.600E+01   3.700E+01
     20       2.900E+01   3.000E+01   3.100E+01   3.200E+01   3.300E+01   3.400E+01   3.500E+01   3.600E+01   3.700E+01   3.800E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
     Samp            21          22
   Line
      1       2.000E+01   2.100E+01
      2       2.100E+01   2.200E+01
      3       2.200E+01   2.300E+01
      4       2.300E+01   2.400E+01
      5       2.400E+01   2.500E+01
      6       2.500E+01   2.600E+01
      7       2.600E+01   2.700E+01
      8       2.700E+01   2.800E+01
      9       2.800E+01   2.900E+01
     10       2.900E+01   3.000E+01
     11       3.000E+01   3.100E+01
     12       3.100E+01   3.200E+01
     13       3.200E+01   3.300E+01
     14       3.300E+01   3.400E+01
     15       3.400E+01   3.500E+01
     16       3.500E+01   3.600E+01
     17       3.600E+01   3.700E+01
     18       3.700E+01   3.800E+01
     19       3.800E+01   3.900E+01
     20       3.900E+01   4.000E+01
tfilt INP=HGEN OUT=(TEST3,X)
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:39 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       5.000E-01   6.050E+01   7.550E+01   8.232E+01   8.621E+01   8.874E+01   9.050E+01   9.180E+01   9.281E+01   9.360E+01
      2       6.050E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      3       7.550E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      4       8.232E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      5       8.621E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      6       8.874E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      7       9.050E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      8       9.180E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      9       9.281E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     10       9.360E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     11       9.425E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     12       9.479E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     13       9.524E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     14       9.562E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     15       9.595E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     16       9.624E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     17       9.650E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     18       9.673E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     19       9.693E+01   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     20       1.005E+02   1.039E+02   1.038E+02   1.036E+02   1.035E+02   1.034E+02   1.032E+02   1.031E+02   1.030E+02   1.029E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:39 2012
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       9.425E+01   9.479E+01   9.524E+01   9.562E+01   9.595E+01   9.624E+01   9.650E+01   9.673E+01   9.693E+01   9.711E+01
      2       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      3       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      4       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      5       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      6       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      7       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      8       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
      9       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     10       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     11       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     12       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     13       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     14       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     15       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     16       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     17       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     18       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     19       1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02   1.005E+02
     20       1.029E+02   1.028E+02   1.027E+02   1.026E+02   1.026E+02   1.025E+02   1.024E+02   1.024E+02   1.023E+02   1.023E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:39 2012
     Samp            21          22
   Line
      1       9.727E+01   1.005E+02
      2       1.005E+02   1.036E+02
      3       1.005E+02   1.035E+02
      4       1.005E+02   1.034E+02
      5       1.005E+02   1.032E+02
      6       1.005E+02   1.031E+02
      7       1.005E+02   1.030E+02
      8       1.005E+02   1.029E+02
      9       1.005E+02   1.029E+02
     10       1.005E+02   1.028E+02
     11       1.005E+02   1.027E+02
     12       1.005E+02   1.026E+02
     13       1.005E+02   1.026E+02
     14       1.005E+02   1.025E+02
     15       1.005E+02   1.024E+02
     16       1.005E+02   1.024E+02
     17       1.005E+02   1.023E+02
     18       1.005E+02   1.023E+02
     19       1.005E+02   1.022E+02
     20       1.022E+02   1.039E+02
tfilt INP=HGEN OUT=(TEST3,X) 'high
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.267E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02
      2       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      3       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      4       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      5       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      6       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      7       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      8       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      9       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     10       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     11       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     12       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     13       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     14       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     15       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     16       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     17       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     18       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     19       1.273E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     20       1.280E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02   1.273E+02
      2       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      3       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      4       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      5       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      6       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      7       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      8       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      9       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     10       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     11       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     12       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     13       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     14       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     15       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     16       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     17       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     18       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     19       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     20       1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02   1.287E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp            21          22
   Line
      1       1.273E+02   1.280E+02
      2       1.280E+02   1.287E+02
      3       1.280E+02   1.287E+02
      4       1.280E+02   1.287E+02
      5       1.280E+02   1.287E+02
      6       1.280E+02   1.287E+02
      7       1.280E+02   1.287E+02
      8       1.280E+02   1.287E+02
      9       1.280E+02   1.287E+02
     10       1.280E+02   1.287E+02
     11       1.280E+02   1.287E+02
     12       1.280E+02   1.287E+02
     13       1.280E+02   1.287E+02
     14       1.280E+02   1.287E+02
     15       1.280E+02   1.287E+02
     16       1.280E+02   1.287E+02
     17       1.280E+02   1.287E+02
     18       1.280E+02   1.287E+02
     19       1.280E+02   1.287E+02
     20       1.287E+02   1.293E+02
tfilt INP=HGEN OUT=(TEST3,X) 'scene
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.800E+01   8.800E+01   1.030E+02   1.098E+02   1.137E+02   1.162E+02   1.180E+02   1.193E+02   1.203E+02   1.211E+02
      2       8.800E+01   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      3       1.030E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      4       1.098E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      5       1.137E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      6       1.162E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      7       1.180E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      8       1.193E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      9       1.203E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     10       1.211E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     11       1.218E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     12       1.223E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     13       1.227E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     14       1.231E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     15       1.235E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     16       1.237E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     17       1.240E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     18       1.242E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     19       1.244E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     20       1.280E+02   1.314E+02   1.313E+02   1.311E+02   1.310E+02   1.309E+02   1.307E+02   1.306E+02   1.305E+02   1.304E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       1.218E+02   1.223E+02   1.227E+02   1.231E+02   1.235E+02   1.237E+02   1.240E+02   1.242E+02   1.244E+02   1.246E+02
      2       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      3       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      4       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      5       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      6       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      7       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      8       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
      9       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     10       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     11       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     12       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     13       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     14       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     15       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     16       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     17       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     18       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     19       1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02   1.280E+02
     20       1.304E+02   1.303E+02   1.302E+02   1.301E+02   1.301E+02   1.300E+02   1.299E+02   1.299E+02   1.298E+02   1.298E+02

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:40 2012
     Samp            21          22
   Line
      1       1.248E+02   1.280E+02
      2       1.280E+02   1.311E+02
      3       1.280E+02   1.310E+02
      4       1.280E+02   1.309E+02
      5       1.280E+02   1.307E+02
      6       1.280E+02   1.306E+02
      7       1.280E+02   1.305E+02
      8       1.280E+02   1.304E+02
      9       1.280E+02   1.304E+02
     10       1.280E+02   1.303E+02
     11       1.280E+02   1.302E+02
     12       1.280E+02   1.301E+02
     13       1.280E+02   1.301E+02
     14       1.280E+02   1.300E+02
     15       1.280E+02   1.299E+02
     16       1.280E+02   1.299E+02
     17       1.280E+02   1.298E+02
     18       1.280E+02   1.298E+02
     19       1.280E+02   1.297E+02
     20       1.297E+02   1.314E+02
tfilt INP=HGEN OUT=(TEST3,X) 'low
Beginning VICAR task tfilt
 *** program TFILT version 19-Dec-2012 ***
 #BYTES/PXL=          4
 NLW=          3
 NSW=          3
LIST INP=TEST3 'ZEROES
Beginning VICAR task LIST

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:41 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.833E+00   2.167E+00   3.167E+00   4.167E+00   5.167E+00   6.167E+00   7.167E+00   8.167E+00   9.167E+00   1.017E+01
      2       2.167E+00   2.500E+00   3.500E+00   4.500E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01
      3       3.167E+00   3.500E+00   4.500E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01
      4       4.167E+00   4.500E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01
      5       5.167E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01
      6       6.167E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01
      7       7.167E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01
      8       8.167E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01
      9       9.167E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01
     10       1.017E+01   1.050E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01
     11       1.117E+01   1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01
     12       1.217E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01
     13       1.317E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01
     14       1.417E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01
     15       1.517E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01
     16       1.617E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01
     17       1.717E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01
     18       1.817E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01
     19       1.917E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01
     20       1.950E+01   1.983E+01   2.083E+01   2.183E+01   2.283E+01   2.383E+01   2.483E+01   2.583E+01   2.683E+01   2.783E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:41 2012
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       1.117E+01   1.217E+01   1.317E+01   1.417E+01   1.517E+01   1.617E+01   1.717E+01   1.817E+01   1.917E+01   2.017E+01
      2       1.150E+01   1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01
      3       1.250E+01   1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01
      4       1.350E+01   1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01
      5       1.450E+01   1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01
      6       1.550E+01   1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01
      7       1.650E+01   1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01
      8       1.750E+01   1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01
      9       1.850E+01   1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01
     10       1.950E+01   2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01
     11       2.050E+01   2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01
     12       2.150E+01   2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01
     13       2.250E+01   2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01
     14       2.350E+01   2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01
     15       2.450E+01   2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01   3.350E+01
     16       2.550E+01   2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01   3.350E+01   3.450E+01
     17       2.650E+01   2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01   3.350E+01   3.450E+01   3.550E+01
     18       2.750E+01   2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01   3.350E+01   3.450E+01   3.550E+01   3.650E+01
     19       2.850E+01   2.950E+01   3.050E+01   3.150E+01   3.250E+01   3.350E+01   3.450E+01   3.550E+01   3.650E+01   3.750E+01
     20       2.883E+01   2.983E+01   3.083E+01   3.183E+01   3.283E+01   3.383E+01   3.483E+01   3.583E+01   3.683E+01   3.783E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Thu Dec 20 16:27:38 2012
 Task:TFILT     User:lwk       Date_Time:Thu Dec 20 16:27:41 2012
     Samp            21          22
   Line
      1       2.117E+01   2.150E+01
      2       2.150E+01   2.183E+01
      3       2.250E+01   2.283E+01
      4       2.350E+01   2.383E+01
      5       2.450E+01   2.483E+01
      6       2.550E+01   2.583E+01
      7       2.650E+01   2.683E+01
      8       2.750E+01   2.783E+01
      9       2.850E+01   2.883E+01
     10       2.950E+01   2.983E+01
     11       3.050E+01   3.083E+01
     12       3.150E+01   3.183E+01
     13       3.250E+01   3.283E+01
     14       3.350E+01   3.383E+01
     15       3.450E+01   3.483E+01
     16       3.550E+01   3.583E+01
     17       3.650E+01   3.683E+01
     18       3.750E+01   3.783E+01
     19       3.850E+01   3.883E+01
     20       3.883E+01   3.917E+01
ush rm -f ?
ush rm -f TEST?
ush rm -f TEST??
ush rm -f GEN
ush rm -f ?GEN
ush rm -f hgen_1
ush rm -f test1_1
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
