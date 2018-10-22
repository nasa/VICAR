      INCLUDE 'VICMAIN_FOR'
C OSBLEMLOC --Converts image-space blemish location file to object-space
C
C          OSBLEMLOC ISBLEM OSBLEM
C
C ISBLEM and OSBLEM contain the following for each blemish:
C		blem(1,I) = index to reference reseau mark
C		blem(2,I) = line coordinate
C		blem(3,I) = sample coordinate
C		blem(4,I) = radius of blemish
c
c
c     May 1996 - ported.  BAM
c     03/2010  -lwk-  replaced testos() with xvhost()
c


      SUBROUTINE MAIN44
      COMMON/C1/BLEM(4,500),RES(2,203),OSRES(2,203),CONV(2216)
      INTEGER*4 CONV
      character*144 lab

      CALL XVMESSAGE('OSBLEMLOC version 19-Mar-2010',0)

      CALL XVUNIT(IUNIT,'INP',1,IND,0)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',0)
C
C     ...Get camera S/N from blemish file label
      CALL XLGET(iunit,'HISTORY','CAMNUM',icam,ind,'FORMAT','INT',0)
      call xvsignal(iunit,ind,1)
      CALL XLGET(iunit,'HISTORY','NUMBLEM',nblem,ind2,'FORMAT','INT',0)
      call xvsignal(iunit,ind,1)
      IF (IND.NE.1.OR.IND2.NE.1) THEN
         CALL VIC1LAB(iunit,IND,blem,lab,10)
         call chkstat(ind,'Error reading blemish file header',
     -                 0,0,0)
         read (lab,90001) icam
90001    format (104x,i1)
         CALL XVGET(iunit,IND,'NS',nsb,0)
         nblem = NSB/4			!Get number of blemishes
      ENDIF
C
      CALL XVREAD(IUNIT,blem,ind,0)	!Read in the blemishes (blem)
      CALL GETRESLOC(ICAM,res,ind)	!Get nominal I.S. reslocs for camera
      CALL VGROS(ICAM,osres)		!Get nominal O.S. reslocs
      CALL GEOMAV(conv,ICAM,RES)	!Generate GEOMA parameters (CONV)
      NHOR = CONV(3) + 1
      NVER = CONV(6) + 1
C
C     ...Compute blemish coordinates by adding offsets to reseau coordinates
C     ...convert to object space, and recompute offsets.
      DO I=1,NBLEM
         M = BLEM(1,I)			!Get index of reference reseau mark
         RLINE = RES(1,M) + BLEM(2,I)	!Compute image-space coordinates
         RSAMP = RES(2,M) + BLEM(3,I)
C		Convert to object-space
         CALL TRITRA(ind,CONV(9),NHOR,NVER,rline,rsamp,1)
         BLEM(2,I) = RLINE - OSRES(1,M)	!Compute offsets
         BLEM(3,I) = RSAMP - OSRES(2,M)
      ENDDO
C
C     ...Write object-space offsets to output file
c
      CALL XVUNIT(OUNIT,'OUT',1,IND,0)
      CALL XVOPEN(OUNIT,IND,'OP','WRITE',
     -                'OPEN_ACT','SA','IO_ACT','SA',0)
      CALL XVWRIT(OUNIT,BLEM,IND,0)

c      close files
      call xvclose(iunit,istatus,0)
      call xvclose(ounit,istatus,0)

      RETURN
      END



C Get nominal reseau locations for Reseau Location File
C Input: ICAM
C Output: RES(2,202)
C
      SUBROUTINE GETRESLOC(ICAM,res,ind)
      REAL*4 res(2,202)
      integer * 4 ind

      COMMON/CRES/IFRM,JCAM,IFILT,IYEAR,IDAY

      CHARACTER*256 RFNAME



c for porting     BAM

! the below cludge is for byte swapping between alpha and unix boxes
! for alphas
      character*4 nom(4),nomu(4) 
      data NOM/'NOM4','NOM5','NOM6','NOM7'/
! for unix
      DATA nomu/'4MON','5MON','6MON','7MON'/
! better kludge:
      character*10 intfmt, realfmt

        integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status
        integer icount
        integer nrows


C
C     ...Find and open Reseau Location File

      CALL XVP('RES',rfname,ICNT)		!Get file name

      CALL XVUNIT(IUNITR,'X',1,IND,'U_NAME',RFNAME,0)

      call ibis_file_open(iunitr,ibis,'read',409,99999,
     -                        format,0,status)
      if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)

      icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
      if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)

                                  !Point to nominals for ICAM
c     call testos(ios)       ! check to swap bytes for UNIX
c     if (ios .eq. 0) then
c         call xvmessage('The OS is ALPHA',' ')
c         call mvcl(nom(icam-3),ifrm,4) ! change char to integer
c     else if (ios .eq. 1) then
c         call xvmessage('The OS is UNIX',' ')
c         call mvcl(nomu(icam-3),ifrm,4) ! change char to integer
c     end if
c  testos only knows about VMS & Unix ... replace with:

      call xvhost('NATIVE', intfmt, realfmt, status)
      if (intfmt.eq.'LOW') then
          call mvcl(nom(icam-3),ifrm,4) ! change char to integer
      else
          call mvcl(nomu(icam-3),ifrm,4) ! change char to integer
      endif

      jcam = icam
      call getlocV2(ibis,nrows,ifrm,res,ind)
      if ( ind .ne. 0 ) go to 992
      call ibis_file_close(ibis,0,status)
      return
C
C     ...Error conditions
  992 CALL XVMESSAGE(' ***Err reading nominal reseau locations',0)
      CALL ABEND
      END
