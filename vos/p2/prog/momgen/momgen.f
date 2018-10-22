C VICAR program MOMGEN
C Radiometric calibration program.  Extracts moments from specified areas
C of input images and stores them in the Light Transfer File.
C
C        MOMGEN (E1,E2,...EN) LTFILE PARAMS
C        or
C        MOMGEN LIST LTFILE PARAMS
C
C LIGHT TRANSFER FILE FORMAT:
C
C The Light Transfer File (LTF) is in VICAR format, and consists of VICAR
C labels containing num_areas, num_expos and an array of the exposures,
C followed by one record for each exposure level of the light transfer or
C reciprocity sequence.
C
C The label array areas contains the size fields for each area
C specified (see LTGEN):
C
C       where   AREA(1,K)=starting line for area K
C               AREA(2,K)=starting sample
C               AREA(3,K)=number of lines
C               AREA(4,K)=number of samples
C
C The exposure time (msec)is in the label of the LTF file.
C Each exposure record contains the number of input frames at that
C exposure (NI), and the moments for each area specified:
C
C       NI,OUT(3*NI*NAREA)
C
C where the array OUT consists of moment information in the following order:
C
C        SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 1
C        SUM(1),SUM(2),...,SUM(NI)          sum of DNs for area 2
C          .    .          .
C          .    .          .
C        SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 1
C        SUM2(1),SUM2(2),...,SUM2(NI)       sum of squares for area 2
C          .    .          .
C          .    .          .
C        SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 1
C        SUMXY(1),SUMXY(2),...,SUMXY(NI)    sum of cross terms for area 2
C          .    .          .
C          .    .          .
C The exposure records are arranged in order of increasing exposure.
C If an extended exposure dark current record is present, it occurs
C first (with EXPO=-1.0), followed by the normal DC frame (EXPO=0.0),
C followed by the lowest to highest exposure levels.
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         IMPLICIT REAL*8 (A-H,O-Z)

         REAL*4 OUT(8192),EXPO,EXP(100)
         REAL*8 SUM(50),SUM2(50),SUMXY(50)

         INTEGER IUNI(50),OUNI,SS,SL,NS,NL,NLA,NSA,STAT,NAREA,NEXP
         INTEGER*4 AREA(4,400)
         integer*4 INSTANCE(30)
         INTEGER*2 E(1024,50)

         LOGICAL DBUG,XVPTST
      
         CHARACTER*1000 MSG
         character*9 tasks(30)
         character*80 lfname,fn
 
C       Maximum number samples in input image=1024
C       Maximum number of areas=400
C       Maximim number inputs=50


         CALL IFMESSAGE ('MOMGEN Version 19-MAR-1997')
 
         DBUG = XVPTST('DBUG')
         CALL XVP('LIST',lfname,NJ)                !LIST used or
         CALL XVPCNT('INP',NI)                     !  input frames
 
         IF (NI .NE. 0) THEN                     !Open input files
            OUT(1) = FLOAT(NI)
            DO I=1,NI
               CALL XVUNIT(IUNI(I),'INP',I,STAT,' ')
               CALL XVOPEN(IUNI(I),STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &                     'U_FORMAT','HALF',' ')
            ENDDO
         ELSE IF (NJ .NE. 0) THEN                !open input list
            open(unit=99,file=lfname,status='OLD',err=950)
            read(99,fmt=1,err=940) fn                     !skip first line
    1       format(a)
 
            DO i=1,51                             !open files in list
               read(99,fmt=1,end=11) fn
               if (i .ge. 51) goto 920
               NI = i
               OUT(1) = NI
               call xvunit(iuni(i),'NONE',I,ist,'u_name',fn,' ')
               CALL XVOPEN(IUNI(I),IST,'OPEN_ACT','SA','IO_ACT','SA',
     &                     'U_FORMAT','HALF',' ')
            END DO
   11       continue
         ELSE
            CALL XVMESSAGE('Specify input files as INP or LIST',' ')
            CALL ABEND
         END IF
 
         CALL XVP('EXPO',EXPO,ICNT)
         if (ICNT .eq. 1) then
            GOTO 12
         ELSE
c-------Get exposure time from the label of one frame
            NINSTANCE = 30
            CALL XLHINFO(iuni(1),TASKS,INSTANCE,NINSTANCE,IST,' ')
C-------Try Cassini type
            call xlget(iuni(1),'PROPERTY','EXPOSURE_DURATION',EXPO,ist,
     1                 'FORMAT','REAL','PROPERTY','CASSINI-ISS',' ')
            if (ist .eq. 1) goto 12
C-------Try Galileo type
            call xlget(iuni(1),'HISTORY','EXP',EXPO,ist,
     1                 'FORMAT','REAL','HIST',TASKS(1),' ')
            if (ist .eq. 1) goto 12
         END IF
 
c-------No exposure in labels either
         GOTO 960
 
12       continue            !found exposure

c-------Open Light Transfer File for update
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','UPDATE',' ')
         CALL XVGET(OUNI,STAT,'NL',NLA,'NS',NSA,' ')   !Get size of LTFILE

C        Read in area locations from LTFILE
         CALL XLGET(OUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET(OUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         NCH = MIN0(20*NI+32,132)
 
         CALL XVMESSAGE(' ',' ')
         CALL PRNT(7,1,EXPO, 'EXPOSURE TIME=  .')
         CALL PRNT(4,1,NI,   'INPUT FRAMES=   .')
         CALL PRNT(4,1,NAREA,'NUMBER OF AREAS=.')
 
C----------------------------------------------------
         IF (DBUG) THEN    !Print column headings for mean/sigma table
            MSG = ' '
            MSG='AREA  STARTING    SIZE OF    MEAN'
            MSG(30+10*NI:28+10*NI+7)='STANDARD'
            CALL XVMESSAGE(MSG,' ')
            MSG=' '
            MSG='NO.  COORDINATES   AREA      VALUES...'
            MSG(30+10*NI:28+10*NI+12)='DEVIATIONS...'
            CALL XVMESSAGE(MSG,' ')
            MSG=' '
            MSG='     (LINE,SAMP)  (NL,NS)'
            ICNT=33
            DO I=1,NI
               MSG(ICNT:ICNT)='M'
               MSG(ICNT+10*NI:ICNT+10*NI)='S'
               WRITE(MSG(ICNT+1:ICNT+1),9000)I
               WRITE(MSG(ICNT+1+10*NI:ICNT+1+10*NI),9000)I
 9000          FORMAT(I1)
               ICNT = ICNT+10
            ENDDO
 
            CALL XVMESSAGE(MSG,' ')
         ENDIF
 
         IB = 2 
 
C-----Main loop:  Read areas from inputs
         DO 100 K=1,NAREA              !Loop through each area
            SL = AREA(1,K)              !Load area size field
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
            CALL ZIA(SUM,2*NI)
            CALL ZIA(SUM2,2*NI)
            CALL ZIA(SUMXY,2*NI)
 
            DO L=1,NL
               LL = SL+L-1
               DO I=1,NI     !Read a line from each input frame
                  CALL XVREAD(IUNI(I),E(1,I),STAT,'LINE',LL,
     &                        'SAMP',SS,'NSAMPS',NS,' ')
               ENDDO
               DO I=1,NI
                  II = MOD(I,NI)+1
                  CALL MOMGEN(E(1,I),E(1,II),SUM(I),SUM2(I), 
     &                        SUMXY(I),NS)
               ENDDO
            ENDDO
 
            IF (DBUG) THEN          !Print mean and sigma of area
               MSG=' '
               WRITE(MSG,9001)K,SL,SS,NL,NS
 9001          FORMAT(I3,'  (',I4,',',I4,') (',I4,',',I4,')')
               N = NL*NS
               IJ = 38 
 
               DO I=1,NI
                  II = MOD(I,NI) + 1
                  R = SUM(I)/N
                  SIG = ((SUM2(I)+SUM2(II)-2*SUMXY(I))/N
     &                   -((SUM(I)-SUM(II))/N)**2)/2.D0
                  S = DSQRT(SIG)
                  WRITE(MSG(IJ-8:IJ),'(F8.2)') R
                  WRITE(MSG(IJ+10*NI-8:IJ+10*NI),'(F8.4)') S
                  IJ = IJ + 10
               ENDDO
 
               CALL XVMESSAGE(MSG,' ')
            ENDIF
 
C-----Load moments into output record...
            CALL MVE(-9,NI,SUM,OUT(IB),1,1)
            CALL MVE(-9,NI,SUM2,OUT(IB+NI),1,1)
            CALL MVE(-9,NI,SUMXY,OUT(IB+2*NI),1,1)
  100    IB = IB+3*NI
 
C-----Search LTFILE for exposure record....
c-----and fill with moments and quit
         CALL XLGET(OUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET(OUNI,'HISTORY','EXPOSURES',EXP(1),STAT,
     &             'NELEMENT',NEXP,'FORMAT','REAL',
     &             'HIST','LTGEN',' ')
         DO L=1,NEXP
            IF (EXP(L).EQ.EXPO) THEN
               CALL XVWRIT(OUNI,OUT,STAT,'LINE',L,'NSAMPS',NSA,' ')
               RETURN
            ENDIF
         ENDDO
 
c-----continues to here if correct exposure record not found
  920    call xvmessage('more than 50 filenames in LIST',' ')
         goto 999
  940    call xvmessage('***Invalid SRCH-format input',' ')
         call xvmessage(lfname,' ')
         goto 999
  950    call xvmessage('could not open input list file',' ')
         call xvmessage(lfname,' ')
         goto 999
  960    call xvmessage('EXPO not specified or in labels',' ')
         goto 999
  999    CALL XVMESSAGE('***MOMGEN task cancelled',' ')
         CALL ABEND
      END


      SUBROUTINE MOMGEN(X,Y,S,S2,SXY,NS)
         IMPLICIT NONE 
         REAL*8 DN, S, S2, SXY 
         INTEGER*2 X(1),Y(1)
         INTEGER J, NS
 
         DO J=1,NS
            DN = X(J)
            S = S + DN
            S2 = S2 + DN**2
            SXY = SXY + DN*Y(J)
         ENDDO
 
         RETURN
      END

