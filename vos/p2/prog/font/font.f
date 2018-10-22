C  REVISION HISTORY
C     2-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     7-96  HBM  FIX HANDLING OF BLANKS WITHIN TEXT
C     9-09  SRL  removed intial copy of the file to output, 
C           then close and then ropen to update mode.
C           Compressed files don't work in update mode. 
C           Now it will Read, modify, write
C           Add a call to xveaction so IO errors are reported
C           It is still unreliable when using the size option
c       Aug 21, 2008 - R.J. Bambery - added error processing
c
c	
c       routine calls certain routines from txtsubs.com
c       fonts are defined in environment variable VRDIFONTS
c	if it cannot find fonts it returns -2 in status1 = TXTFONT(FONT1)
c
	PROGRAM font
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
	integer*4 np,max_bytes
      PARAMETER (NP=30)      
      PARAMETER (MAX_BYTES=524 288)	! 0.5 MBytes
      byte IMAGE(MAX_BYTES)
      byte TEXT1(100)
	integer*2 FONT1, DN1, TALL1, NL1, NS1, ISS1, ISL1, NC1
	integer*4 i,j,k,ii,jj,isl,iss
	integer*4 FONT(NP),TALL(NP),DN(NP),SIZE(4),START(2*NP),THICK(NP)
	integer*4 LOC(NP),NC(NP)
	integer*4 ThickCnt,TallCnt,WideCnt,LocCnt,RotCnt,DNCnt,FontCnt
	integer*4 INU,OUTU,DEF,COUNT,SL,SS,NL,NS
	integer*4 status,SCOUNT,TCOUNT,FAT,NLCHUNK,SLCHUNK
	integer*4 f2hbuf(12)
	integer*4  status1
	integer*4 TXTTEXT, TXTFONT, TXTSIZE, TXTCOLOR,txtrotate
      INTEGER*4 OUTU2			!fixed NLOC(NP)
      INTEGER*4 SLOUT
	LOGICAL*4 FLAG
      real*4 WIDE(NP), ROTATE(NP)
        character*100 TEXT(NP)
        character*120 outline
c....flag to determine if a text string has been written, in part or
c    whole, off the image.
C 
      call ifmessage('FONT version Feb 14, 2010 (64-bit) - RJB')
	  call xveaction ('S', 'ERROR ***') 
C     ading the 'A' flag will cause an abort on error
C      WRITE (*,'(A)') 'FONT ##################'
      call xvtrans_set(f2hbuf,'FULL','HALF',STATUS)
      call xvunit(inu,'INP',1,STATUS,' ')
      call xvunit(outu,'OUT',1,STATUS,' ')
	  CALL XVUNIT(OUTU2,'cc',1,STATUS,' ')
C	  WRITE (*,'(A,I3,A,I3)') 'XVUNIT OUTU2=',OUTU2,' status ',STATUS
C  
c...get the number of lines and samples of the input image from its internal
c   control block
      call xvopen(inu,status,' ')
      call xvget(inu,status,'NL',NL,'NS',NS,' ')
 
      call xvparm('SIZE',SIZE,COUNT,DEF,4)
      IF (DEF.EQ.1) THEN         
         SIZE(1)=1
         SIZE(2)=1
	 SIZE(3)=NL
         SIZE(4)=NS
      ENDIF
C     SIZE(1)=StartLine
C     SIZE(2)=StartSample
C     SIZE(3)=NL 
C     SIZE(4)=NS
	  	  
      call xvopen(outu,status,'OP','WRITE',
     &     'U_NL',SIZE(3),'U_NS',SIZE(4),' ')
 
      call xvparm('POSITION',START,SCOUNT,DEF,2*NP)
      call xvparm('TEXT',TEXT,TCOUNT,DEF,NP)
      IF ( SCOUNT .NE. 2*TCOUNT ) THEN
         CALL XVMESSAGE( '??E -Number of TEXT strings does not match ',' ' )
         CALL XVMESSAGE( 'number of POSITIONS.',' ' )
         CALL ABEND
      END IF
 
      call xvparm('TALL',TALL,TallCnt,DEF,NP)
      call xvparm('WIDE',WIDE,WideCnt,DEF,NP)
      call xvparm('LOC',LOC,LocCnt,DEF,NP)
      call xvparm('ROTATE',ROTATE,RotCnt,DEF,NP)
      call xvparm('DN',DN,DNCnt,DEF,NP)
      call xvparm('FONT',FONT,FontCnt,DEF,NP)
      call xvparm('THICK',THICK,ThickCnt,DEF,NP)

c...read the DESIRED PORTION OF THE input image into THE output image....
C      WRITE (*,'(A,I6,A,I6)') 'FONT NL=',SIZE(3) ,' NS=',SIZE(4)
 
C	read and write to output image removed

C	Divide the image into "chunks" where each chunk contains
C	NS samples and NLCHUNK lines.


      NLCHUNK = MAX_BYTES/SIZE(4)
      NLCHUNK = MIN(NLCHUNK,SIZE(3))
	  
C	  WRITE (*,'(A,I6)') '  NLCHUNK=',NLCHUNK

C	Now for each chunk....
C	  read it from the image
C	  write all strings into that chunk (sort of)
C	  write chunk to image

C      DO SLCHUNK = 1,SIZE(3),NLCHUNK
C                    from, to, increment
      DO SLCHUNK = SIZE(1),SIZE(3)+SIZE(1),NLCHUNK
C	  DO SLCHUNK = SIZE(2),SIZE(3),NLCHUNK+SIZE(3)
	  
C	   Calculate chunk boundary

C	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+1)
C 	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+SIZE(1)+1) C one extra ??
	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+SIZE(1))
	 NS = SIZE(4)
      	SS = 1
	  SL = SLCHUNK
C	  SL = SLCHUNK + SIZE(2)
	  SLOUT = SL - SIZE(1) + 1
C     start at 1 not 0 ??
C      SLOUT = SL
C	 WRITE (*,'(A)') '***** loop start ******** ' 
C	 WRITE (*,'(A)') '** ' 
C	 WRITE (*,'(A,I6,A,I6)') '**   CHUNK SLCHUNK=',SLCHUNK,' NLCHUNK=',NLCHUNK
C	 WRITE (*,'(A,I6,A,I6, A,I6,A,I6)') '**   startLine SIZE(1)=',SIZE(1),' startSample SIZE(2)=',SIZE(2),'  NL SIZE(3)=',SIZE(3),' NS SIZE(4)=',SIZE(4)
C	 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6)') '**   SS=',SS,' SL=',SL,' SLOUT=',SLOUT,'  NL=',NL,' NS=',NS,' TCOUNT=',TCOUNT
C	 WRITE (*,'(A)') '** ' 
C	 WRITE (*,'(A)') '************************** ' 
C	   Read data from image file

	 DO J = 1,NL
	 
	    
C	    IF ( J .EQ. 1) THEN
C		 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   READ J=',J,' IMAGE(',((J-1)*SIZE(2)+1),') LINE=',(SL+J-1),' SAMP=',SIZE(2),' NSAMPS=',NS
C		ELSEIF ( J .EQ. NL) THEN
C		 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   READ J=',J,' IMAGE(',((J-1)*SIZE(2)+1),') LINE=',(SL+J-1),' SAMP=',SIZE(2),' NSAMPS=',NS
C		ENDIF
C      This is the original READ, it takes SIZE into account to resize the image if it is requested
            CALL XVREAD(INU,IMAGE((J-1)*NS+1),STATUS,'LINE',SL+J-1,
     &     	'SAMP',SIZE(2),'NSAMPS',SIZE(4),' ')

C      This is the READ for the second loop
C            CALL XVREAD(INU,IMAGE((J-1)*NS+1),STATUS,
C     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
	 END DO

C	   Draw each text string to image...
         DO I=1,TCOUNT
            DO K = 1, 100
              TEXT1(K) = 0
            END DO
            CALL MVCL(TEXT(I),TEXT1(1),100)
            NC(I) = 0
            DO K = 1, 100
               IF (TEXT1(K) .NE. 32) NC(I) = K
            END DO
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
C      START is Position - could have a flag, either adjust to output coordinates
C      or leave in input image coordinates
C      since we are doing sizing on the fly. add SIZE to the coordinate
C      don't do this? the number is in output coordinates already
C            START(II)=START(II)-SIZE(1)+1
C            START(JJ)=START(JJ)-SIZE(2)+1
		
C      convert position from 			
		   START(II)=START(II)+SIZE(1)+1
C           START(JJ)=START(JJ)+SIZE(2)+1

C	      Skip if no characters in string
C          WRITE (*,'(A,A,A)') '  TEXT(I)=',TEXT(I),' <*'
C          WRITE (*,'(A,I2,A,I6,A,I3)') '   I=',I,' K=',K,'    NC(I)=',NC(I)
		  
            IF(NC(I).EQ.0) GOTO 1			
			
C			WRITE (*,'(A,A,A,I6,A,I6,A)') '   WRITING  TEXT(I)=',TEXT(I),' START(II)=',START(II),' START(JJ)=',START(JJ),'  <*******'
C	      Set up all parameters for this string

            call xvtrans(f2hbuf,FONT(I),FONT1,1)
            status1 = TXTFONT(FONT1)
c       0=function not implemented, 2=illegal font type, 4=font table too small
c       6=end of file on font description file
            if (status1.ne.1) then
                write(outline,10110) status1
10110 format ('??E txtfont return = ',i3)
                call xvmessage(outline,' ' )
                call abend
            endif
c            CALL XVTRANS(F2HBUF,FONT(I),FONT1,1)
c           STATUS1 = TXTFONT(FONT1)
            call xvtrans(f2hbuf,TALL(I),TALL1,1)
            status1 = TXTSIZE(TALL1,WIDE(I))
c       0=function not implemented, 2=Illegal height, 4=Illegal horizontal scale factor
            if (status1.ne.1) then
                write(outline,10120) status1
10120 format ('??E txtsize return = ',i3)
                call xvmessage(outline,' ' )
                call abend
            endif
c            CALL XVTRANS(F2HBUF,TALL(I),TALL1,1)
c            STATUS1 = TXTSIZE(TALL1,WIDE(I))
            status1 = TXTROTATE(ROTATE(I))
c       0=function not implemented, 2=illegal angle
            if (status1.ne.1) then
                write(outline,10130) status1
10130 format ('??E txtrotate return = ',i3)
                call xvmessage(outline,' ' )
                call abend
            endif
c            CALL TXTROTATE(ROTATE(I))
            call xvtrans(f2hbuf,DN(I),DN1,1)
            status1 = TXTCOLOR(DN1)
c       0=function not implemented, 2=illegal color value
            if (status1.ne.1) then
                write(outline,10140) status1
10140 format ('??E txtcolor return = ',i3)
                call xvmessage(outline,' ' )
                call abend
            endif
c            CALL XVTRANS(F2HBUF,DN(I),DN1,1)
c            STATUS1 = TXTCOLOR(DN1)
 
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
                  call xvtrans(f2hbuf,nl,nl1,1)
                  call xvtrans(f2hbuf,ns,ns1,1)
                  call xvtrans(f2hbuf,iss,iss1,1)
                  call xvtrans(f2hbuf,isl,isl1,1)
                  call xvtrans(f2hbuf,nc(i),nc1,1)
		  status1 = TXTTEXT(IMAGE,NL1,NS1,ISS1,ISL1,LOC(I),NC1,
     &                 TEXT1(1),FLAG)
c       0=function not implemented, 8=Coordinates are outside access window
c       10=Illegal LOC (orientation) value, 12=Illegal character count
            if (status1.ne.1) then
                write(outline,10150) status1
10150 format ('??E txttext return = ',i3)
                call xvmessage(outline,' ' )
                call abend
            endif
               ENDDO
            ENDDO


 1          CONTINUE
         ENDDO 
            ! Text drawing loop

c....data to now be placed in the output data set....but in order to
c    to write into the output in the update mode a XVREAD must be performed
c    on the data set to be written to before the XVWRIT can be.
	
         DO J=1,NL
C		   IF (J .EQ. 1)  THEN
C		     WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   J=',J,'  WRITE IMAGE(',((J-1)*NS+1),') LINE=',SLOUT+J-1,' SAMP=',SS,' NSAMPS=',NS
C           ELSEIF (J .EQ. NL) THEN
C		    WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   J=',J,'  WRITE IMAGE(',((J-1)*NS+1),') LINE=',SLOUT+J-1,' SAMP=',SS,' NSAMPS=',NS
C		   ENDIF
            CALL XVWRIT(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SLOUT+J-1,'SAMP',SS,'NSAMPS',NS,' ')
C	         WRITE (*,'(A,I1,A,I4)') '     XVWRIT status ',STATUS,' J=',J
         ENDDO
      
C	 WRITE (*,'(A)') 'XXXX loop END XXXXXX ' 
C	 WRITE (*,'(A)') 'XX ' 
C	 WRITE (*,'(A,I6,A,I6)') 'XX   CHUNK SLCHUNK=',SLCHUNK,' NLCHUNK=',NLCHUNK
C	 WRITE (*,'(A,I6,A,I6, A,I6,A,I6)') 'XX   startLine SIZE(1)=',SIZE(1),' startSample SIZE(2)=',SIZE(2),'  NL SIZE(3)=',SIZE(3),' NS SIZE(4)=',SIZE(4)
C	 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6)') 'XX   SS=',SS,' SL=',SL,' SLOUT=',SLOUT,'  NL=',NL,' NS=',NS,' TCOUNT=',TCOUNT
C	 WRITE (*,'(A)') 'XX ' 
C	 WRITE (*,'(A)') 'XXXXXXXXXXXXXXXXXXXXXX ' 
      ENDDO          ! Chunking loop

C		WRITE (*,'(A)') 'After Chunking loop. Close files '
	  CALL XVCLOSE(INU,STATUS,' ')
      CALL XVCLOSE(OUTU,STATUS,' ')
c.........all format statements below except for # 1.
 2    FORMAT( '??W - PART OR ALL OF TEXT STRING',I3,
     &     ' WAS WRITTEN OFF THE IMAGE')
 4    FORMAT( '??E - TEXT STRING',I3,
     &     ' EXCEEDS PROGRAMS SIZE LIMITATIONS.')
 
 9999 RETURN
      END
