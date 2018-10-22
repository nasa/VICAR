      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c         original program called "sar", modified to blemvorb
C  *******************************************************************
C  *  SAR-SEGMENT AVERAGING ROUTINE- PRODUCED FOR JPL BY IBM/FSD     *
C  *  UNDER CONTRACT 951538  FEBRUARY, 1968                          *
C  *  SAR- INTERPOLATION METHOD MODIFIED      MARCH,1968             *
C  *******************************************************************
C  01-JULY-94 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
      COMMON/C1/ PARAMT,BYTES,AREA,TABLE
      INTEGER PARAMT(800),TABLE(15000),SL,SS,SS2,SEGT(4,100)
      INTEGER NB,HALFW,SN,LBUF(40)
      INTEGER*2 HAREA(4096),HBYTES(26000),AI1,AI2,AI3
      BYTE BYTES(52000),AREA(8192)
      LOGICAL XVPTST
      CHARACTER*32 FORMAT
      INTEGER*4 COUNT,DEF,LINE
      INTEGER TRN_BUF(12),STAT

      EQUIVALENCE (SEGT,PARAMT(11))

C  the segt table contains integer entries specifying sl,ss,nl,ns
C  for each segment to be replaced with a local average. the program
C  converts the last two entries to last line and last sample. the program
C  reads in lines sequentially,performing all work needed on a line, and
C  then writing it out. the line currently being worked on is kept in
C  'area'. if any vertical averaging is required,then two other lines must
C  be in core.these line pairs are kept in the array bytes. the line
C  numbers of any lines in core are kept in the table 'table'. the
C  position in'table' corresponds to the position in 'bytes'. each entry
C  in 'bytes' is ns bytes long. 
   
      DATA  NB/52000/
      DATA  HALFW/0/
      DATA  SN/0/
      DATA  AI1/0/
      DATA  AI2/0/
      DATA  AI3/0/

      CALL IFMESSAGE('BLEMVORB version 01-JULY-94')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(UNIT2,'INP',1,IND,' ')
      CALL XVOPEN(UNIT2,IND,'U_FORMAT','HALF',' ')
      CALL XVGET(UNIT2,IND,'FORMAT',FORMAT,' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF(FORMAT(1:4).EQ.'HALF')THEN
         HALFW=1
          CALL XVMESSAGE('SYSTM LABEL SAYS INPUT IS HALFWORD',' ')
      ELSE IF(FORMAT(1:4).EQ.'WORD')THEN
         HALFW=1
          CALL XVMESSAGE('SYSTM LABEL SAYS INPUT IS HALFWORD',' ')
      ELSE IF(FORMAT(1:4).EQ.'BYTE')THEN
         HALFW=0
         CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS BYTE',' ')
      ENDIF
      HALFW=1   ! for this test assume always halfword data
      JPARM=10

      IF(XVPTST('BYTE'))HALFW=0
      IF(XVPTST('HALF'))then
         HALFW=1
         SS=2*(SS/2)+1
         NS=2*(NS/2)
      endif
      CALL XVPARM('CAMERA',PARAMT,COUNT,DEF,800)
      IF(DEF.EQ.0)SN=PARAMT(1)
      CALL XVPARM('SN',PARAMT,COUNT,DEF,800)
      IF(DEF.EQ.0)SN=PARAMT(1)
      
      IF(FORMAT.EQ.'HALF')THEN
          SS=(SS+1)/2
          NS=NS/2
      ENDIF
c  check that parameters do not exceed input picture size
      IF(SL+NL-1.LE.NLI)GO TO 2
         CALL XVMESSAGE('REQUESTED AREA EXCEEDS INPUT PICTURE SIZE',' ')
         NL=NLI-SL+1
2     IF(SS+NS-1.LE.NSI)GO TO 4
         CALL XVMESSAGE('REQUESTED AREA EXCEEDS INPUT PICTURE SIZE',' ')
         NS=NSI-SS+1
4     LL=SL+NL-1
      SS=SS-1
      CALL ZIA(TABLE,100)
      CALL XVUNIT(UNIT1,'OUT',1,IND,' ')
      IF(SN.EQ.0)THEN
         CALL VOLABV2(IND,UNIT2,LBUF)
         IF(IND.EQ.0)THEN
            SN=LBUF(2)
            CALL PRNT(4,1,SN,' INPUT IS VO CAMERA S/N.')
         ELSE
            CALL PRNT(4,1,IND,' VOLAB ERROR,IND=.')
            CALL ABEND
         ENDIF
      ENDIF

      CALL XVTRANS_SET(TRN_BUF,'HALF','HALF',STAT)
      IF(STAT.NE.1)THEN
         CALL XVMESSAGE('BUFFER SETUP UNSUCCESSFUL',' ')
         CALL ABEND
      ENDIF

      CALL XVOPEN(UNIT1,IND,'OP','WRITE','U_FORMAT','HALF',' ')
      
      NAVAIL=1
c  check that number of parameters is a multiple of 4
      NENT=(NB/(2*NS))*2
      CALL XVPARM('AREA',PARAMT(11),NUM,DEF,800)
      IF(DEF.EQ.0)THEN
         CALL XVMESSAGE('AREA SPECIFIED',' ')
         IF(MOD(NUM,4).NE.0)THEN
            CALL XVMESSAGE
     *            ('NUMBER OF PARAMETERS NOT A MULTIPLE OF FOUR',' ')
            CALL ABEND
         ENDIF
c  check input parameter for validity and convert entries
         LSA=NS/(1+HALFW)
      ENDIF
c  get the blemishes
      CALL VOBLEM(SN,NUMADD,PARAMT(10+NUM+1))
      IF(MOD(NUMADD,4).NE.0)THEN
         CALL XVMESSAGE('ERROR IN VOBLEM, NOT A MULTIPLE OF 4',' ')
         CALL ABEND
      ENDIF
      NUM=NUM+NUMADD/4
      DO 10 J=1,NUM
         SEGT(3,J)=SEGT(1,J)+SEGT(3,J)-1
         SEGT(4,J)=SEGT(2,J)+SEGT(4,J)-1
         IF(SEGT(3,J).LE.NLI)GO TO 9
         SEGT(3,J)=NLI
         CALL XVMESSAGE('REQUESTED AREA EXCEEDS INPUT PICTURE SIZE',' ')
9        IF(SEGT(4,J).LE.NSI)GO TO 10
c  print warning about error and continue
            CALL XVMESSAGE
     *           ('REQUESTED AREA EXCEEDS INPUT PICTURE SIZE',' ')
            SEGT(4,J)=NSI
10    CONTINUE

c  the following loop completely processes 1 line on each iteration
      DO 200 LINE=SL,LL
         LN=LINE
         CALL XVREAD
     *        (UNIT2,AREA,IND,'LINE',LN,'SAMP',SS+1,'NSAMPS',NS,' ')
         CALL XVTRANS(TRN_BUF,AREA,HAREA,NS)
c  if no segment specified, just write out requested area
      IF(NUM.EQ.0)GO TO 55
c  check for any areas to be averaged in this line.
      DO 50  J=1,NUM
         IF(LINE.LT.SEGT(1,J))GO TO 50
c  allow segment with nl=0  one time
         IF(SEGT(1,J).GT.SEGT(3,J).AND.LINE.EQ.SEGT(1,J))GO TO 16
         IF(LINE.GT.SEGT(3,J))GO TO 50
c  this segment touches this line
16       CONTINUE
         SS2=SEGT(2,J)-SS  !/(1+HALFW))
         LS2=SEGT(4,J)-SS  !/(1+HALFW))
c  use horizontal averaging if number of lines in segment =0
         IF(SEGT(3,J).LT.SEGT(1,J))GO TO 45
c  this segment requires vertical averaging, are the needed lines
c  already  in core
         DO 25  L=1,NENT,2
            IF(SEGT(1,J)-1.EQ.TABLE(L).AND.
     *                 SEGT(3,J).EQ.TABLE(L+1)-1)GO TO 30
25       CONTINUE
c  lines needed must be read into 'bytes'
         IB=(NAVAIL-1)*NS+1
         TABLE(NAVAIL)=SEGT(1,J)-1
         TABLE(NAVAIL+1)=SEGT(3,J)+1
         NAVAIL=NAVAIL+2
         IF(NAVAIL.GT.NENT)NAVAIL=1
         LN=SEGT(1,J)-1
c  test for segment at top of picture
         IF(SEGT(1,J).LE.1)GO TO 215
            CALL XVREAD(UNIT2,BYTES(IB),IND,'LINE',LN,'SAMP',SS+1,
     *            'NSAMPS',NS,' ')
            CALL XVTRANS(TRN_BUF,BYTES(IB),HBYTES((IB+1)/2),NS)
215      LN=SEGT(3,J)+1
c  test for segment at bottom of picture
         IF(SEGT(3,J).GE.NL)GOTO 31
            CALL XVREAD(UNIT2,BYTES(IB+NS),IND,'LINE',LN,'SAMP',SS+1,
     *           'NSAMPS',NS,' ')
            CALL XVTRANS(TRN_BUF,BYTES(IB+NS),HBYTES((IB+NS+1)/2),NS)
c  needed lines are now in
            GO TO  31
30       IB=(L-1)*NS+1
c  average vertical area
c  calculate distances from top and bottom or set flags if at edge
31       DT=LINE-SEGT(1,J)+1
         IF(SEGT(1,J).LE.1)DT=-1.
         DB=SEGT(3,J)+1-LINE
         IF(SEGT(3,J).GE.NL)DB=-1.
c  calculate point on left of line
315      CONTINUE
         IF(HALFW.EQ.1)AI1=HAREA(SS2-1)
         PL=AI1
         PLF=1.0
         IF(SS2.LE.1)PLF=-1.0
c  set flag if no left sample
c  calculate  point on right of line
         IF(HALFW.EQ.1)AI1=HAREA(LS2+1)
         PR=AI1
         PRF=1.0
         IF(LS2.GE.LSA)PRF=-1.0
c  set flag if no right sample
c  now replace points in this line
         IK=(IB-1)/(1+HALFW)+SS2-1
         DO 40 IJ=SS2,LS2
            IK=IK+1
c  calculate one new point based on pl,pt,pr,pb.
            ANUM=0.
            ADEN=0.
            IF(PLF.LT.0.0)GO TO 32
              DL=IJ-SS2+1
              ANUM=PL/DL+ANUM
              ADEN=1./DL+ADEN
32          IF(PRF.LT.0.0)GO TO 34
               DR=LS2+1-IJ
               ANUM=PR/DR+ANUM
               ADEN=1./DR+ADEN
34          IF(DT.LT.0.)GO TO 36
               AI1=HBYTES(IK)
               PT=AI1
               ANUM=PT/DT+ANUM
               ADEN=1./DT+ADEN
36          IF(DB.LT.0.)GO TO 38
               AI1=HBYTES(IK+LSA)
               PB=AI1
               ANUM=PB/DB+ANUM
               ADEN=1./DB+ADEN
38          IF(ADEN.EQ.0.)ADEN=1.
            AI1=ANUM/ADEN+.5
            IF(HALFW.EQ.1)THEN
              HAREA(IJ)=AI1
              CALL XVTRANS(TRN_BUF,HAREA(IJ),AREA(IJ*2-1),1)
            ENDIF
40       CONTINUE
         GO TO 50
c  horizontal averaging on this segment
45       DT=-1.
         DB=-1.
         GOTO 315
c  end of a segment on one line
50       CONTINUE
c  end of all segments on this line
55       CONTINUE
         CALL XVWRIT(UNIT1,AREA,IND,' ')
200   CONTINUE

      CALL XVCLOSE(UNIT1,IND,' ')
      CALL XVCLOSE(UNIT2,IND,' ')

      RETURN

      END

      SUBROUTINE VOBLEM(SN,NUM,BUF)
c  viking orbiter nominal blemish retrieval subroutine
      IMPLICIT INTEGER (A-Z)
      INTEGER BUF(1)
      REAL SN4(96),SN6(156),SN7(152),SN8(256),SN8A(216),SN8B(40)
      EQUIVALENCE (SN8,SN8A),(SN8(217),SN8B)
c  wasatch has a limit on continuation statements making it necessary
c  to break up the data statement for SN8
      DATA SN4/
     *    25.5,  38.,  2.,2.,  84.5,1115.5,6.,4., 163.0,1000., 5.,5.,
     *   172.,  725.5, 5.,4., 379.5,  97.5,2.,2., 384.,  488.,5.,3.,
     *   700.5, 422.5, 6.,4., 714.,  351.5,9.,8., 762.5, 430., 4.,3.,
     *   783.5, 726.5, 4.,2., 806.5, 351., 4.,3., 821.5, 258.5,2.,2.,
     *   834.5, 945.,  6.,3., 846.,  645., 5.,3., 857.,  308., 5.,3.,
     *   895.5, 214.5, 2.,2., 921.5, 513., 4.,3., 981.5, 481., 6.,5.,
     *   998.,  425.,  5.,3.,1026.5, 403.5,2.,4.,1030.,  797., 7.,3.,
     *  1043.5,1007.5, 8.,4.,
     *   297.,  185.5, 3.,2.,1055., 1047., 3.,7./  ! etr additions
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
c  etr additions
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
c  etr additions
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
     *   570.,  208.5, 5.,6., 605.5,542.5,2.,6.,  606.5,332., 4.,7.,
     *   628.,  961.5, 3.,4., 639.5,871.5,4.,8.,  650.5,441.5,2.,4.,
     *   662., 1110.5, 5.,6., 669.5,348., 2.,5.,  679.5,463., 6.,11.,
     *   690.5, 769.,  2.,5., 737.5,585., 2.,5.,  751.5,762.5,2.,6.,
     *   766.5, 671.5, 2.,6., 778.5,549.5,4.,6.,  784.5,885.5,2.,6.,
     *   800.5, 761.5, 2.,4., 824., 204., 3.,7.,  829.,584.,  5.,5.,
     *   861.5, 694.,  2.,5., 867.5,881., 2.,5.,  868.5,651., 2.,5., 
     *   871.,  482.5, 3.,4., 903.5,502.5,2.,6.,  951.,1129., 3.,5./
      DATA SN8B/
     *   955.,  682.5, 3.,6., 988., 855., 3.,7.,  992.5,166.5,5.,8.,
     *  1006.5, 239.,  4.,9.,1023.5,263., 6.,13.,1035.5,760., 2.,7.,
     *  1038., 1135.,  7.,9., 348., 898.5,5.,10.,
c  etr additions
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
         CALL PRNT(4,1,SN,' voblem:ILLEGAL SERIAL NUMBER,SN=.')
         CALL ABEND
      ENDIF
      RETURN
      END

      SUBROUTINE GETBLEM(NUM,INBUF,OUTBUF)
      IMPLICIT INTEGER(A-Z)
      REAL INBUF(4,NUM/4)
      INTEGER OUTBUF(4,NUM/4)
c  inbuf(1,i)is center line of blemish
c  inbuf(2,i)is center sample
c  inbuf(3,i)is number of lines
c  inbuf(4,i)is number of samples
      DO I=1,NUM/4
         OUTBUF(1,I)=INBUF(1,I)-INBUF(3,I)/2.+.5
         OUTBUF(2,I)=INBUF(2,I)-INBUF(4,I)/2.+.5
         OUTBUF(3,I)=INBUF(3,I)
         OUTBUF(4,I)=INBUF(4,I)
      ENDDO
      RETURN
      END
