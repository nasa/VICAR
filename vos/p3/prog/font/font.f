C  REVISION HISTORY
C     2-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     3-95  rea  make "loc" parameter work; permit blanks within text strings
      PROGRAM font
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      PARAMETER (NP=30)      
      PARAMETER (MAX_BYTES=524 288)	! 0.5 MBytes
      BYTE IMAGE(MAX_BYTES)
      CHARACTER*100 TEXT(2000)
      BYTE TEXT1(100)
      INTEGER FONT(NP),TALL(NP),DN(NP),SIZE(4),START(60),THICK(NP),
     &        STATUS,TallCnt,WideCnt,LocCnt,RotCnt,DNCnt,FontCnt,
     &        LOC(NP),INU,OUTU,NC(1000),DEF,COUNT,SL,SS,
     &        ThickCnt,SCOUNT,TCOUNT,FAT,NL,NS,
     &        NLCHUNK,SLCHUNK
      REAL WIDE(NP), ROTATE(NP)
c....flag to determine if a text string has been written, in part or
c    whole, off the image.
      LOGICAL FLAG
      INTEGER F2HBUF(12)
      INTEGER*2 FONT1, DN1, TALL1, NL1, NS1, ISS1, ISL1, NC1, LOC1
      LOGICAL TXTTEXT, TXTFONT, TXTSIZE, TXTCOLOR, STATUS1
C 
      CALL XVMESSAGE('FONT version 3-MAR-95',' ')
      call XVTRANS_SET(F2HBUF,'FULL','HALF',STATUS)
      CALL XVUNIT(INU,'INP',1,STATUS,' ')
      CALL XVUNIT(OUTU,'OUT',1,STATUS,' ')
C  
c...get the number of lines and samples of the input image from its internal
c   control block
      CALL XVOPEN(INU,STATUS,' ')
      CALL XVGET(INU,STATUS,'NL',NL,'NS',NS,' ')
 
      CALL XVPARM('SIZE',SIZE,COUNT,DEF,4)
      IF (DEF.EQ.1) THEN
         SIZE(3)=NL
         SIZE(4)=NS
         SIZE(1)=1
         SIZE(2)=1
      ENDIF

      CALL XVOPEN(OUTU,STATUS,'OP','WRITE',
     &     'U_NL',SIZE(3),'U_NS',SIZE(4),' ')
 
      CALL XVPARM('POSITION',START,SCOUNT,DEF,60)
      CALL XVPARM('TEXT',TEXT,TCOUNT,DEF,30)
      IF ( SCOUNT .NE. 2*TCOUNT ) THEN
         CALL XVMESSAGE( 'Number of TEXT strings does not match ',' ' )
         CALL XVMESSAGE( 'number of POSITIONS.',' ' )
         CALL ABEND
      END IF
 
      CALL XVPARM('TALL',TALL,TallCnt,DEF,NP)
      CALL XVPARM('WIDE',WIDE,WideCnt,DEF,NP)
      CALL XVPARM('LOC',LOC,LocCnt,DEF,NP)
      CALL XVPARM('ROTATE',ROTATE,RotCnt,DEF,NP)
      CALL XVPARM('DN',DN,DNCnt,DEF,NP)
      CALL XVPARM('FONT',FONT,FontCnt,DEF,NP)
      CALL XVPARM('THICK',THICK,ThickCnt,DEF,NP)

c...read the DESIRED PORTION OF THE input image into THE output image....

      DO I=1,SIZE(3)
         CALL XVREAD(INU,IMAGE(1),STATUS,'LINE',SIZE(1)+I-1,'SAMP',
     &        SIZE(2),'NSAMPS',SIZE(4),' ')
         CALL XVWRIT(OUTU,IMAGE(1),STATUS,'LINE',I,'SAMP',SIZE(2),
     &        'NSAMPS',SIZE(4),' ')
      ENDDO
      CALL XVCLOSE(INU,STATUS,' ')
      CALL XVCLOSE(OUTU,STATUS,' ')

c......both input and output data sets were closed above and the output
c      is now being re-opened for both reading and writing.     

      CALL XVOPEN(OUTU,STATUS,'OP','UPDATE',' ')

C	Divide the image into "chucks" where each chunk contains
C	NS samples and NLCHUNK lines.

      NLCHUNK = MAX_BYTES/SIZE(4)
      NLCHUNK = MIN(NLCHUNK,SIZE(3))

C	Now for each chunk....
C	  read it from the image
C	  write all strings into that chunk (sort of)
C	  write chunck to image

      DO SLCHUNK = 1,SIZE(3),NLCHUNK

C	   Calculate chunk boundary

	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+1)
	 NS = SIZE(4)
	 SS = 1
	 SL = SLCHUNK

C	   Read data from image file

	 DO J = 1,NL
            CALL XVREAD(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
	 END DO

C	   Draw each text string to image...
         DO I=1,TCOUNT
            DO K = 1, 100
              TEXT1(K) = 0
            END DO
            CALL MVCL(TEXT(I),TEXT1(1),100)
            NC(I) = 100
            DO K = 100,1,-1
              IF (TEXT1(K) .NE. 32) GO TO 99
              NC(I) = NC(I) - 1
            END DO
99        CONTINUE

c...Add missing parameters
            IF ( I .GT. TallCnt  ) TALL(I)   = TALL  (TallCnt)
            IF ( I .GT. WideCnt  ) WIDE(I)   = WIDE  (WideCnt)
            IF ( I .GT. LocCnt   ) LOC(I)    = LOC   (LocCnt)
            IF ( I .GT. RotCnt   ) ROTATE(I) = ROTATE(RotCnt)
            IF ( I .GT. DNCnt    ) DN(I)     = DN    (DNCnt)
            IF ( I .GT. FontCnt  ) FONT(I)   = FONT  (FontCnt)
            IF ( I .GT. ThickCnt ) THICK(I)  = THICK (ThickCnt)

C...SET STARTING COORDDINATE REFERENCE TO OUTPUT IMAGE..
            II = I*2-1
            JJ = II+1
            START(II)=START(II)-SIZE(1)+1
            START(JJ)=START(JJ)-SIZE(2)+1

C	      Skip if no characters in string

            IF(NC(I).EQ.0) GOTO 1

C	      Set up all parameters for this string

            CALL XVTRANS(F2HBUF,FONT(I),FONT1,1)
            STATUS1 = TXTFONT(FONT1)
            CALL XVTRANS(F2HBUF,TALL(I),TALL1,1)
            STATUS1 = TXTSIZE(TALL1,WIDE(I))
            CALL TXTROTATE(ROTATE(I))
            CALL XVTRANS(F2HBUF,DN(I),DN1,1)
            STATUS1 = TXTCOLOR(DN1)
 
c...the starting line and sample within the chunk are calculated
c   and the subroutine that writes the text is called after we
c   assign the thickness of each line in the characters, based on
c   the users request. Thickening is done be re-writing the string with
c   changing the iss and isl as needed.

            FAT=THICK(I)
            FLAG = .TRUE.
            DO J=1,FAT
               ISL=START(II)-SL+1-(J-1)
               DO K=1,FAT
                  ISS=START(JJ)-SS+K
                  CALL XVTRANS(F2HBUF,NL,NL1,1)
                  CALL XVTRANS(F2HBUF,NS,NS1,1)
                  CALL XVTRANS(F2HBUF,ISS,ISS1,1)
                  CALL XVTRANS(F2HBUF,ISL,ISL1,1)
                  CALL XVTRANS(F2HBUF,LOC(I),LOC1,1)
                  CALL XVTRANS(F2HBUF,NC(I),NC1,1)
              status1 = TXTTEXT(IMAGE,NL1,NS1,ISS1,ISL1,LOC1,NC1,
     &                 TEXT1(1),FLAG)
               ENDDO
            ENDDO

 1          CONTINUE
         END DO 
            ! Text drawing loop

c....data to now be placed in the output data set....but in order to
c    to write into the output in the update mode a XVREAD must be performed
c    on the data set to be written to before the XVWRIT can be.

         DO J=1,NL
            CALL XVWRIT(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
         ENDDO
      
      ENDDO          ! Chunking loop

      CALL XVCLOSE(OUTU,STATUS,' ')
c.........all format statements below except for # 1.
 2    FORMAT( ' PART OR ALL OF TEXT STRING',I3,
     &     ' WAS WRITTEN OFF THE IMAGE')
 4    FORMAT( ' TEXT STRING',I3,
     &     ' EXCEEDS PROGRAMS SIZE LIMITATIONS.')
 
 9999 RETURN
      END
