      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM ROTATE2
c     09 Jan 2013 ...lwk...   fixed OUT1/OUT2 assignments due to different behaviour
c                             with the -e compiler flag on Solaris
c     20 APR 2011 ...RJB...   Changed call qprint to call xvmessage under AFIDS
C      9 JUN 93   ...SP....   MADE PORTABLE FOR UNIX.  CHANGED TO USE XVEACTION
C                             INSTEAD OF XVCHECK.
C      4 NOV 88   ...SP....   ADDED CODE TO HANDLE CASE OF NLO=1 OR NSO=1.
C      1 MAY 84   ...CCA...   CONVERT TO VICAR2
C      1 JAN 84   ...CCA...   CONVERT TO VAX, NO VXCTL
C     25 JUL 72   ...ARG...   INITIAL VERSIONM
C      1 MAR 73   ...FGS...   CORRECT CENTER OPTION IN OUTPUT PICTURE
C     22 MAR 73   ...FGS...   INCLUDE FAKIBCOM
C     15 MAY 73   ...FGS...   DELETE PARAMETER DATA SET
C     27 JUN 75   ...DAH...   CHANGES FOR CONVERSION TO 360/OS
C     29 MAR 79   ...JAM...   INCORPORATE HALF AND SPLINE OPTIONS
C      1 APR 79   ...JAM...   SET DEFAULT ANGLE TO ZERO
C      1 APR 79   ...JAM...   REMOVE EQUIVALENCE BETWEEN KWD AND PAR
C
C ** PURPOSE... TO GENERATE GEOM PARAMETER SETS DESCRIBING PICTURE
C               ROTATIONS AND TO FETCH LGEOM. 
C
C ** TO  'GEOM' USE...
C     ROTATE2 INP SIZE=() LINE=X SAMP=X ANGL=X CENT=(L,S)
C
C         WHERE
C   LINE, SAMP, ARE THE CENTER OF ROTATION FOR THE INPUT PICTURE
C   THESE MAY BE DEFAULTED TO THE CENTER OF THE INPUT PICTURE
C
C   ANGL IS THE ROTATION IN DEGREES CLOCKWISE FROM 'UP'.
C
C   CENT IS THE LINE AND SAMPLE LOCATION OF THE CENTER OF ROTATION
C   IN THE OUTPUT PICTURE. IT CAN BE DEFAULTED TO THE CENTER OF THE
C   OUTPUT PICTURE.
C
C   THE KEYWORD 'NOIN' MAY BE USED FOR TRANSMISSION TO LGEOM
C
	implicit none
      	real*4 rparm(16),ll,ls,ang
	real*4 dl,ds,fl,fs,s,c,csi,cso,cli,clo
      	integer*4 sl,ss,nlo,nso,nli,nsi,stat,nlo2,nso2,icent,iline,isamp
	integer*4 iunit,icount,idef,idummy,i,npds
	logical*4 XVPTST
      	character*256 pds
      	character*8 format
      	character*132 out1
      	character*132 out2

c     	DATA OUT1/' REGION (    ,     ,     ,     ) OF THE INPUT PICTURE I
c    +S ROTATED         DEGREES ABOUT       ,        '/

C==================================================================
	call xvmessage ("ROTATE2 version 2015-12-16"," ")
      CALL XVEACTION('SA',' ') ! SET XV ERROR ACTION

      icent = 0
      iline = 0
      isamp = 0
      ang=0.
      call zia(RPARM,16)
C        OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,' ')
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      if (format.ne.'BYTE' .and. format.ne.'HALF') then
	call xvmessage('??E - ROTATE2 accepts BYTE and HALF data only',' ')
        call abend
      endif

C        GET SIZE INFORMATION 
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)

C        CLOSE INPUT DATA SET
      CALL XVCLOSE(IUNIT,STAT,' ')

C        GET NAME OF PARAMETER DATA SET
      CALL XVP('PDS',PDS,NPDS)

C           PROCESS PARAMETERS
C        'LINE'
      CALL XVPARM('LINE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) then
         cli = RPARM(1)
	 clo = cli - sl + 1
	 iline = 1
      endif

C        'SAMPLE'
      CALL XVPARM('SAMPLE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) then
	 csi = RPARM(1)
	 cso = csi - ss + 1
	 isamp = 1
      endif

C        'CENTER'
      CALL XVPARM('CENTER',RPARM,ICOUNT,IDEF,2)
      if (icount .ne. 0) then
	 clo = RPARM(1)
	 cso = RPARM(2)
	 icent = 1
      endif

C        'ANGLE'
      CALL XVPARM('ANGLE',RPARM,ICOUNT,IDEF,1)
      if (icount .ne. 0) ang = RPARM(1)

      OUT1(1:53) = ' REGION (    ,     ,     ,     ) OF THE INPUT PICTURE'
      OUT1(54:102) = ' IS ROTATED         DEGREES ABOUT       ,        '
      nli = min0(nli+1-sl,nlo)
      nsi = min0(nsi+1-ss,nso)
      write (out1(10:13),'(i4)') sl
      write (out1(16:19),'(i4)') ss
      write (out1(22:25),'(i4)') nli
      write (out1(28:31),'(i4)') nsi
      if (iline .eq. 0) cli=.5*(sl+nli)
      if (isamp .eq. 0) csi=.5*(ss+nsi)
      if (icent .eq. 0) clo=.5*(1+nlo)
      if (icent .eq. 0) cso=.5*(1+nso)
      write (out1(66:72),'(f7.1)') ang
      write (out1(87:92),'(f6.1)') cli
      write (out1(95:100),'(f6.1)') csi
      call xvmessage(out1(2:102),' ')
      write (out2,9900) clo,cso
9900  format (
     +' THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL
     +     ',F6.1,',  ',F6.1)
      call xvmessage(out2(2:85),' ')

      ang=(-ang)*3.14159/180.
      c=cos(ang)
      s=sin(ang)


      nlo2 = max( nlo, 2 )   ! IF NLO OR NSO IS 1, THEN USE 2 AS THE
      nso2 = max( nso, 2 )   ! ENDING TIEPOINT LOCATION SO RECTANGLE
                             ! WILL NOT BE DEGENERATE.

      ds=csi-cso
      dl=cli-clo
      fl=1.-clo
      fs=1.-cso
      ll=nlo2-clo
      ls=nso2-cso

C        Open the parameter data set
      i = 3
      IF (XVPTST('NOINTERP')) I = I + 1
      IF (FORMAT .EQ. 'HALF') I = I + 1
      CALL XVPOPEN(STAT,I,64,PDS,'SA',IDUMMY)

C        Write out the parameters
      CALL XVPOUT(STAT,'NAH',1,'INT',1)
      CALL XVPOUT(STAT,'NAV',1,'INT',1)

      RPARM(1) = 1
      RPARM(2) = 1
      RPARM(3) = S*FS+C*FL-FL+DL + 1
      RPARM(4) = C*FS-S*FL-FS+DS + 1
      RPARM(5) = 1
      RPARM(6) = NSO2
      RPARM(7) = S*LS+C*FL-FL+DL + 1
      RPARM(8) = C*LS-S*FL-LS+DS + NSO2
      RPARM(9) = NLO2
      RPARM(10) = 1
      RPARM(11) = S*FS+C*LL-LL+DL + NLO2
      RPARM(12) = C*FS-S*LL-FS+DS + 1
      RPARM(13) = NLO2
      RPARM(14) = NSO2
      RPARM(15) = S*LS+C*LL-LL+DL + NLO2
      RPARM(16) = C*LS-S*LL-LS+DS + NSO2

      CALL XVPOUT(STAT,'TIEPOINT',RPARM,'REAL',16)

      IF(XVPTST('NOINTERP')) THEN
         CALL XVPOUT(STAT,'INTERP','NOIN','STRING',1)
      ENDIF

      IF(FORMAT .EQ. 'HALF') THEN
         CALL XVPOUT(STAT,'FORMAT','HALF','STRING',1)
      ENDIF

C        Close the parameter data set
      CALL XVPCLOSE(STAT)

      return
      end
