C VICAR PROGRAM LTGEN
C Radiometric calibration routine to initialize the Light Transfer File.
C The output Light Transfer File will contain a header record, followed
C by one record for each exposure level in the light transfer (or
C reciprocity) sequence.  The LTF file format is documented in the
C MOMGEN source.
C
C        LTGEN INP LTF.DAT NI=5 'GRID EXPO=(e0,e1,e2,e3...) LIST=
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         IMPLICIT NONE

         REAL*4 EXPO(100)

         INTEGER*4 AREA(4,400)
         INTEGER*4 SL, SS, NL, NS, NLI2, NSI2, SLI, SSI, NLI, NSI
         INTEGER*4 SLBUF(4), NAREA, NEXP, NX, PAR(6400) 
         INTEGER*4 ICNT, IDEFL, NI, IUNI, STAT, CNT, I, K, M, N
         INTEGER*4 NIX, NLA, NSA, IND, L, OUNI
         INTEGER*4 IGRES, NGRID, LOFF, SOFF, NLG, MAX0, NSG  

         LOGICAL*4 XVPTST,DBUG

         character*80 lfname
         CHARACTER*120 MSG

        ni=0
	nexp=0
	do i=1,400
	   expo=0.0
        enddo 
         CALL IFMESSAGE('LTGEN Version 23-Nov-2012  (64-bit) - rjb')
 
c-------If a LIST of filenames is input, get the NI, NEXP and EXPO
         call xvparm('LIST',lfname,icnt,idefl,80)
         if (icnt .eq. 1) call dolist(lfname,expo,nexp,ni)
 
c-------Open the INP file to get size field and label
c-------Label will automatically be transfered to ouput file
         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLI2,NSI2) !Get size field of input image
 
         DBUG = XVPTST('DBUG')
         NAREA = 0
         CALL ZIA(AREA,1600)
         IF (DBUG) CALL XVMESSAGE ('Area size fields...',' ')
 
         CALL XVP('AREA',PAR,CNT)          !Get specified areas

C        retrive AREA values from user input
         IF (CNT .NE. 0) THEN
            NAREA = CNT/4
            I = 1
 
            DO K=1,NAREA
               SL = PAR(I)
               SS = PAR(I+1)
               NL = PAR(I+2)
               NS = PAR(I+3)
               CALL MVE(4,4,PAR(I),AREA(1,K),1,1)
               WRITE(MSG,501)AREA(1,K),AREA(2,K),AREA(3,K),AREA(4,K)
  501          FORMAT('  ',I6,I6,I6,I6)
               IF (DBUG) CALL XVMESSAGE(MSG,' ')
               IF ((SL .LT. 1) .OR. (SS .LT. 1) .OR.
     &             (NL .LT. 1) .OR. (NS .LT. 1)) GOTO 990
               IF ((SL+NL-1 .GT. NLI) .OR. (SS+NS-1 .GT. NSI)) GOTO 990
               I = I + 4
            ENDDO
         ENDIF
 
         CALL XVPARM('GRES',PAR,CNT,IGRES,6400)  !grid resolution
         IF ((PAR(1) .LE. 0) .OR. (PAR(2) .LE. 0) .OR. 
     &       (PAR(3) .LE. 0)) GOTO 960 
         NGRID = PAR(1)
         NL = PAR(2)
         NS = PAR(3)
         SLBUF(3) = PAR(2)
         SLBUF(4) = PAR(3)
 
         NLG = NLI/NGRID
         NSG = NSI/NGRID
         LOFF = MAX0((NLG-NL)/2,1)   !Offsets
         SOFF = MAX0((NSG-NS)/2,1)
 
         IF (XVPTST('GRID') .OR. (IGRES .EQ. 0)) THEN    !Generate grid
            DO N=1,NGRID
               SLBUF(1) = (N-1)*NLG+LOFF
 
               DO M=1,NGRID
                  NAREA = NAREA + 1
                  SLBUF(2) = (M-1)*NSG+SOFF
                  CALL MVE(4,4,SLBUF(1),AREA(1,NAREA),1,1)
                  WRITE(MSG,502)AREA(1,NAREA),AREA(2,NAREA),
     &                            AREA(3,NAREA),AREA(4,NAREA)
 502              FORMAT('  ',I6,I6,I6,I6)
                  IF (DBUG) CALL XVMESSAGE (MSG,' ')
               ENDDO
            ENDDO
         ENDIF
 
         IF (NAREA .EQ. 0) GOTO 980
         IF (NAREA .GT. 400) GOTO 981
         WRITE (MSG,503)NAREA
 503     FORMAT ('NUMBER OF AREAS     = ',I6)
         CALL XVMESSAGE (MSG,' ')

c-----If EXP is entered, then it will override that derived from LIST
         CALL XVP('EXPO',EXPO,NX)          !Get exposure numbers
 
c-----If no LIST entered (or exps not found in labels)
c-----& no EXPO parameter, then abend
         IF ((NEXP .eq. 0) .and. (NX .EQ. 0)) GOTO 970

         if (NX .ne. 0) NEXP = NX           !EXP entered, override

         WRITE (MSG,504)NEXP
 504     FORMAT ('NUMBER OF EXPOSURES = ',I6)
         CALL XVMESSAGE (MSG,' ')
 
         CALL PRNT(7,NEXP,EXPO,'EXPOSURES = .')
 
c-----If NI is entered, then it will override that derived from LIST
         CALL XVP('NI',NIX,CNT)            !get max num of input/exp
         IF ((NI .eq. 0) .and. (CNT .EQ. 0)) GOTO 971    !no LIST & no NI
         if (CNT .ne. 0) NI = NIX          !NI entered, override
 
         CALL PRNT(4,1,NI,'MAX FRAMES/LEVEL = .')

         NLA = NEXP                    !one record for each exposure
         NSA = 3*NI*NAREA+1            !3 stats for each image*area
                                       !reserve 1 place holder for NI
         IF (NSA .GT. 32768) GOTO 982        !VMS record size restriction
 
         CALL XVMESSAGE 
     &      ('WRITING HALFWORD LIGHT TRANSFER FILE WITH',' ')
         CALL PRNT(4,1,NLA,' NL (NREC) = .')
         CALL PRNT(4,1,NSA,' NS        = .')

C        write to output file 
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         CALL XVOPEN(OUNI,STAT,'U_NL',NLA,'U_NS',NSA,'OPEN_ACT',
     &               'SA','IO_ACT','SA','OP','WRITE','U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')

C        write NUM_AREAS and AREAS onto VICAR label
         CALL XLADD(OUNI,'HISTORY','NUM_AREAS',NAREA,IND,
     &              'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','AREAS',AREA,IND,
     &              'NELEMENT',4*NAREA,'FORMAT','INT',' ')

C        write NUM_EXPOS and EXPOSURES onto VICAR label
         CALL XLADD(OUNI,'HISTORY','NUM_EXPOS',NEXP,IND,
     &              'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','EXPOSURES',EXPO(1),IND,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')

C        generate place holders for MOMGEN
         CALL ZIA(PAR,NSA)

C        write one record for each exposure...
         DO L=1,NEXP                                     !Each line is a place  
            CALL XVWRIT(OUNI,PAR,STAT,'NSAMPS',NSA,' ')  !holders for momgen
         ENDDO

         CALL XVMESSAGE('LTGEN task completed',' ')
         RETURN
 
C        error conditions 
  960    CALL XVMESSAGE ('??E - Grid resolution must be positive',' ')
         GOTO 999 
  970    CALL XVMESSAGE ('??E - No exposures from file list and ',' ')
         CALL XVMESSAGE ('   no exposures specified',' ')
         CALL XVMESSAGE ('***Use LIST or EXPO parameters',' ')
         GOTO 999
  971    CALL XVMESSAGE ('??E - No file list and no NI specified',' ')
         CALL XVMESSAGE ('***Use LIST or NI parameters',' ')
         GOTO 999
  980    CALL XVMESSAGE ('??E - No areas specified',' ')
         CALL XVMESSAGE ('***Use AREA, GRID, or GRES parameter',' ')
         GOTO 999
  981    CALL XVMESSAGE ('??E - Maximum of 400 areas exceeded',' ')
         GOTO 999
  982    CALL XVMESSAGE ('??E - Output file record size exceeds 32768',
     1  ' ')
         CALL XVMESSAGE 
     &      ('*** Reduce number of areas (see HELP LTGEN)',' ')
         GOTO 999
  990    CALL XVMESSAGE ('??E - Illegal area',' ')
  999    CALL XVMESSAGE ('***LTGEN task cancelled',' ')
         CALL ABEND
      END

C************************************************************************
      subroutine dolist(lfname,e,nexp,imax)
C     This subroutine only executes when the LIST parameter is given.  
C     It reads the SRCH-format file, which it is sorted by increasing
C     exposure time.  For each file in the list, it retrives the 
C     EXPOSRUE_DURATION label from the CASSINI-ISS property label.
C     The result of this subroutine is an array of exposures values. 
c      
c-------assumes only that the list of frames is in order of
c-------increasing exposure and in SRCH-format
c-------lfname is the file name of the list file
c-------exposure is the list of unique exposures (get rid of exp - f77 conflict)
c-------nexp is the number of them
c-------ni is the maximum number of frames found per any exposure
         implicit none
        integer*4 nexp,imax,i,ist,iu,j,ninstance,noexp,num
         real*4 exposure(500),e(100)
         character*80 fn
         character*9 tasks(30)
         character*(*) lfname
         integer*4 n(100),INSTANCE(30)
 
         open(unit=99,file=lfname,status='OLD',err=999)
 
         read(99,fmt=1) fn                      !skip first line
 
c-------initialize flag for errors finding exposures in labels
         noexp=0
	do i=1,100
	  n(i)=0		!fix for 64-bit
	enddo
c-------Open all input files and extract EXP from labels, if present
         do 10 i=1,501
            read(99,fmt=1,end=11) fn
            if (i .eq. 501) go to 992
            NUM = i
            call xvunit(iu,'NONE',1,ist,'u_name',fn,' ')
            call xvopen(iu,ist,'OPEN_ACT','SA','IO_ACT','SA',' ')
c-------try Cassini style
            call xlget(iu,'PROPERTY','EXPOSURE_DURATION',exposure(i),ist,
     &               'FORMAT','REAL','PROPERTY','CASSINI-ISS',' ')
            if (ist .eq. 1) go to 9
c-------try Galileo style
            NINSTANCE = 30
            CALL XLHINFO(iu,TASKS,INSTANCE,NINSTANCE,IST,' ')
            call xlget(iu,'HISTORY','EXP',exposure(i),ist,
     &                 'FORMAT','REAL','HIST',TASKS(1),' ')
            if (ist .eq. 1) goto 9
            noexp=1                        !Exposure not found in label
    9       continue
 
            call xvclose(iu,ist,' ')
   10    continue
 
   11    call prnt(4,1,num,'NUMBER OF FILES  =.')
 
c-------find number of frames at each exposure time and store the
c-------unique exposure times for return to main program
         i=1
         j=1
         e(j)=exposure(i)
         n(j)=1
 
         do 15 i=2,num
            if (exposure(i) .eq. e(j)) then
               n(j) = n(j) + 1     ! increment number of identical exposures
               goto 15
            else
               j=j+1
               if (j .gt. 100) go to 993
               e(j)=exposure(i)         ! record unique exposure times
               n(j)=1
            end if
   15    continue
 
         nexp=j      ! store size of exposure array to return to main
 
c-------If error finding EXP in labels, set NEXP to zero as a flag
c-------for the main program
         if (noexp .eq. 1) NEXP=0
 
         call prnt(4,1,nexp,'NUMBER OF LEVELS =.')
 
         imax=0
         do i=1,num
            if (n(i) .gt. imax) imax=n(i)
         enddo
 
         call prnt(4,1,imax,'MAX FRAMES/LEVEL =.')
 
         return
 
    1    format(a)

  992    call xvmessage('??E - more than 500 filenames in list',' ')
         call abend
  993    call xvmessage ('??E - more than 100 exposure levels',' ')
         call abend
  999    call xvmessage ('??E - Error openning input list file',' ')
         call xvmessage (lfname,' ')
         call abend
      end

