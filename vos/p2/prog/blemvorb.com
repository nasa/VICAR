$!****************************************************************************
$!
$! Build proc for MIPL module blemvorb
$! VPACK Version 1.8, Tuesday, March 07, 1995, 17:15:47
$!
$! Execute by entering:		$ @blemvorb
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
$ write sys$output "*** module blemvorb ***"
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
$ write sys$output "Invalid argument given to blemvorb.com file -- ", primary
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
$   if F$SEARCH("blemvorb.imake") .nes. ""
$   then
$      vimake blemvorb
$      purge blemvorb.bld
$   else
$      if F$SEARCH("blemvorb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake blemvorb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @blemvorb.bld "STD"
$   else
$      @blemvorb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create blemvorb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack blemvorb.com -
	-s blemvorb.f -
	-i blemvorb.imake -
	-p blemvorb.pdf -
	-t tstblemvorb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create blemvorb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create blemvorb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM blemvorb

   To Create the build file give the command:

		$ vimake blemvorb			(VMS)
   or
		% vimake blemvorb			(Unix)


************************************************************************/


#define PROGRAM	blemvorb
#define R2LIB

#define MODULE_LIST blemvorb.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create blemvorb.pdf
PROCESS HELP=*
PARM INP TYPE=STRING 
PARM OUT TYPE=STRING 
PARM SIZE TYPE=INTEGER   COUNT=0:4            DEFAULT=(1,1,0,0)
PARM SL   TYPE=INTEGER   COUNT=0:1            DEFAULT=1
PARM SS   TYPE=INTEGER   COUNT=0:1            DEFAULT=1
PARM NL   TYPE=INTEGER   COUNT=0:1            DEFAULT=0
PARM NS   TYPE=INTEGER   COUNT=0:1            DEFAULT=0
PARM HALF TYPE=KEYWORD   COUNT=0:1 VALID=HALF DEFAULT=--
PARM BYTE TYPE=KEYWORD   COUNT=0:1 VALID=BYTE DEFAULT=--
PARM CAMERA TYPE=INTEGER COUNT=0:1            DEFAULT=--
PARM SN     TYPE=INTEGER COUNT=0:1            DEFAULT=--
PARM AREA TYPE=INTEGER   COUNT=0:4            DEFAULT=--  ! keep area last
END-PROC
.TITLE
VICAR PROGRAM "blemvorb" -- Viking Orbiter blemish removal
.HELP
Removes blemishs, wrinkles and warts from image space Viking Orbiter images.

 EXECUTION:

  blemvorb input-file output-file 'HALF AREA=(i1,i2,i3,i4) 

 OPERATION:

  If no user parameters are specified, the label is scanned and the
camera serial number is retrieved,the blemishes are retrieved, and
the portion of the input image specified in SIZE is copied into the
output file. Additional blemishes can be removed from input image
using the removal option below. 
.page
Removal option:
   Form:

     AREA=(sl1,sb1,nl1,nb1,sl2,sb2,nl2,nb2,...,slk,sbk,nlk,nbk)
       where
    slk = starting line of rectangle k
    sbk = starting byte of rectangle k
    nlk = number lines in rectangle k
nbk = number of bytes in rectangle k

   Function:

These parameters cause the rectangular areas specified to be removed
from the picture and to be replaced with an average of nearby points.
Specifically, let R be the rectangle located between lines L1 and L2
and samples S1 and S2 and assume R is to be eliminated. Let M be the
line coordinate of a point within R, and let N be its coordinate. Let
X(A,B) be the data number of a point located at line A, sample B. 
.page
Then:              X(M,S1)   X(M,S2)   X(L1,N)  X(L2,N)
                   ------- + ------- + ------ + -------
                   (N-S1)    (S2-N)    (M-L1)   (L2-M)
  X(M,N)   =      ---------------------------------------
                      1          1       1        1
                   ------- + ------- + ------ + -------
                   (N-S1)     (S2-N)   (M-L1)   (L2-M)

 In other words, if (E,F) is within a rectangle to be eliminated, 

                    PL   PR   PT   PB
                    -- + -- + -- + --
                    DL   DR   DT   DB
   X(E,F)  =      ---------------------
                    1    1    1    1
                    -- + -- + -- + --
                    DL   DR   DT   DB

 where PL, PR, PT, and PB are the intensities of the nearest points
to the left, right, top, and bottom outside the rectangle and DL, DR,
DT, and DB are the distances to those points. If a rectangle to be
eliminated occurs at the edge of a picture, only those terms in the
above equations for which intensities exist in the input are used.
Thus, if the rectangle to be removed occurs in the lower left hand
corner of a picture, the terms dealing with L2 and S2 in the first
equation and those dealing with DL and DB in the DL and DB in the
second equation are not used. 
.page
RESTRICTIONS:

 The input image be be in raw, image space. It must not have had its
geometry changed in any way since being read off the EDR.

EXAMPLES:

 blemvorb A B
 This operation copies the picture in file A to file B and removes 
the blemishes

 blemvorb SIZE=(1,1,300,300) AREA=(1,1,25,25,101,51,25,30) inp=A out=B
 This operation copies the top left 300 x 300 square of the  picture
in file A into file B removing the 2 rectangles  specified by the
 AREA parameter.

 blemvorb SIZE=(75,99,300,400) 'HALF AREA=(100,200,10,5) inp=A out=B
 Given a halfword (16 bits per pixel) picture 400 lines by 500
 samples (1000 bytes). The output picture will contain the 300
 lines and 200 samples staring at line 75 and sample 50 and the
 10 line by 5 sample block starting at line 100 and sample 200
 (in the input picture) is to be averaged out. 

 HISTORY:
  Written BY:  Joel Mosher
  Cognizant Programmer: None
  Revision: new 29-MAR-1986 renamed from SAR to BLEMVORB
            1   10-JUL-1991 modified to call new VOLABV2
            2   01-JUL-1994 Made portable for UNIX  AS (CRI)
            3   07-MAR-1995 Fixed tst pdf as per FR 85747
.LEVEL1
.VARI INP
Viking Orbiter image
.VARI OUT
output image
.VARI SIZE
4 integers sl,ss,nl,ns
.VARI SL
integer starting line
.VARI SS
integer starting sample
.VARI NL
integer number of lines
.VARI NS
integer number of samples.
.VARI HALF
keyword halfword input format
.VARI BYTE
keyword byte input format
.VARI AREA
integer, multiples of 4
sl,ss,nl,ns locations of blemishes
.VARI CAMERA
integer VO camera serial number
.VARI SN
integer VO camera serial number
.LEVEL2
.VARI INP
  A Viking Orbiter image file is input
.VARI OUT
  A file to write the processed product into
.VARI SIZE
 SIZE=(sl,sb,nl,nb)
  The standard size field defining the area of the input picture 
.VARI SL
SL=sl
  Starting line of the area to be processed.
.VARI SS
 SS=ss
  Starting sample of the area to be processed
.VARI NL
NL=nl
  Number of lines in the area to be processed.
.VARI NS
NS=ns
  Number of samples in the area to be processed.
.VARI HALF
HALF indicates that the input and output files are in halfword format. 
The default is to read the input image label to obtain the format. 
.VARI BYTE
BYTE indicates that the input and output files are in byte format.
The default is to read the input image label to obtain the format. 
.VARI AREA 
 AREA=(sl1,sb1,nl1,nb1,...,slk,sbk,nlk,nbk)
  AREA specifes the starting line, starting sample, number of samples,
and number of lines to be eliminated. The number of values following
 AREA must be a multiple of 4. 
.VARI CAMERA
CAMERA=integer
 CAMERA specifies the camera serial number of the input image. The
default is to obtain CAMERA from the label. 
.VARI SN
SN=integer 
 SN specifies the camera serial number of the input image. The
default is to obtain SN from the label. 
.END 
$ Return
$!#############################################################################
$Test_File:
$ create tstblemvorb.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write "  "
write " The Test Data are handled for both VMS and UNIX in the PDF. "
write " At present, (May 1994) in order for UNIX to run the program, "
write " the data files MUST be copied to the LOCAL directory where the "
write " program resides. "
write "                                  OLD       NEW (VMS or UNIX execution)"
write "  SITOD1:[TEST_DATA.IMAGES.VIKING]218S01.IMG ==> 218s01.img "
write "  SITOD1:[TEST_DATA.IMAGES.VIKING]218S02.IMG ==> 218s02.img "
write "  "
write " This UNIX restriction on the data will be changed eventually. "
write " For ease of use, VMS will also get the Test Data from the LOCAL "
write " directory "
write "  "

write " TEST CAMERA S/N 7"
write " LIST THE AREA OF A BLEMISH BEFORE REMOVAL"
list 218s01.img SL=325 SS=530 NL=15 NS=15
write " REMOVE BLEMISHES"
blemvorb 218s01.img a
write "LIST THE REMOVED AREA"
list a SL=325 SS=530 NL=15 NS=15
write " USE THE SIZE PARAMETERS"
blemvorb 218s01.img b SL=325 SS=530 NL=15 NS=15
list b
write "OVERRIDE THE CAMERA SERIAL NUMBER AND SPECIFY AN AREA"
blemvorb 218s01.img c SN=4 AREA=(327,533,8,8)
list c SIZE=(325,530,15,15)
write " TEST CAMERA S/N 4"
write " LIST THE AREA OF A BLEMISH BEFORE REMOVAL"
list 218s02.img SL=975 SS=475 NL=15 NS=15
write " REMOVE BLEMISHES"
blemvorb 218s02.img d
write "LIST THE REMOVED AREA"
list d SL=975 SS=475 NL=15 NS=15
end-proc
$ Return
$!#############################################################################
