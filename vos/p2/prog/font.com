$!****************************************************************************
$!
$! Build proc for MIPL module font
$! VPACK Version 1.9, Thursday, April 16, 2015, 17:15:43
$!
$! Execute by entering:		$ @font
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
$ write sys$output "*** module font ***"
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
$ write sys$output "Invalid argument given to font.com file -- ", primary
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
$   if F$SEARCH("font.imake") .nes. ""
$   then
$      vimake font
$      purge font.bld
$   else
$      if F$SEARCH("font.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake font
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @font.bld "STD"
$   else
$      @font.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create font.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack font.com -mixed -
	-s font.f -
	-i font.imake -
	-p font.pdf -
	-t tstfont.pdf tstfont4c.pdf tstfont2.pdf tstfont2.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create font.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create font.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM font

   To Create the build file give the command:

		$ vimake font			(VMS)
   or
		% vimake font			(Unix)


************************************************************************/


#define PROGRAM	font
#define R2LIB

#define MODULE_LIST font.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define DEBUG*/
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create font.pdf
PROCESS HELP=*
PARM INP    TYPE=STRING  COUNT=1
PARM OUT    TYPE=STRING  COUNT=1
PARM SIZE   TYPE=INTEGER COUNT=4      DEFAULT=(1,1,0,0)
PARM FONT   TYPE=INTEGER COUNT=(0:30) VALID=(1:131) DEFAULT=3
PARM TALL   TYPE=INTEGER COUNT=(0:30) DEFAULT=15
PARM WIDE   TYPE=REAL    COUNT=(0:30) VALID=(.5:2.) DEFAULT=.8
PARM THICK  TYPE=INTEGER COUNT=(0:30) VALID=(1:100) DEFAULT=1
PARM POSITION  TYPE=INTEGER COUNT=(0:60) DEFAULT=(1,1)
PARM TEXT   TYPE=(STRING,100)  COUNT=(0:30) DEFAULT=" "
PARM ROTATE TYPE=REAL    COUNT=(0:30) VALID=(-180.:180.) DEFAULT=0.
PARM DN     TYPE=INTEGER COUNT=(0:30) VALID=(0:255) DEFAULT=255
PARM LOC    TYPE=INTEGER COUNT=(0:30) VALID=(1,2,3) DEFAULT=1
PARM PARMS  TYPE=(STRING,40) COUNT=0:1 DEFAULT=--
END-PROC
.TITLE
VICAR2 program "font"--
       Writes text on an image allowing choices of FONT type, size and dn.
.HELP
PURPOSE:

"font" is a VICAR2 applications program which may be used to write text onto
any  VICAR2 byte image with a choice of seven available FONT styles.  Along
with  varying  the  FONT  style  the user can specify the height, width, dn
value  and  line  thickness  of each character and whether to left justify,
right  justify  or  center  the  text string above a specified point.  "font"
operates  in  a  completely  different  manner  than  "textad" and is no way
related.

.page 
EXECUTION STATEMENT:
		  font in out SIZE=(SL,SS,NL,NS) PARAMS
				    or
			    font in out PARMS

OPERATION:

"font" first  checks  the user entered parameters to insure that they are of
the  correct  format  and  number.   The  area  of  the input image that is
specified  by  the size field is copied from the input to the output image.
If  no  size field is specified, the entire input is copied to output.  The
input  image  is  closed and all subsequent work is performed on the output
image.   Due  to  the  fact that the various characters within a particular
FONT  have different sizes, it becomes extremely difficult to determine the
exact  dimensions  of  a text string.  (If exact text string dimensions and
character  positioning  are  required,  the user is encouraged to preform a
test  on  a  image  and  then  use IDISPLAY to measure the results.) If any
portion  of  a  text string is to written off the output image, the user is
notified  and  given  the  string number.  That portion of the string which
will  fit  in  the  output image is written and subsequent strings are then
processed.

EXAMPLES:
	font in out SIZE=(1,1,400,600) POSITION=(100,20,130,20) +
	     TEXT=("How goes it Laddy?","Just fine, Thanks!")
 
In  the  above  example,  both  text strings would be written in the output
image  with all six parameters being defaulted.  The results would have the
following  characteristics:  Both text strings, would be written in a Roman
style  FONT  (FONT=3),  the upper case letters are 15 pixels tall (tall=15)
and  12  pixels  wide (wide=.8,width is based on the height selected), both
upper  and lower case letters are allowed, the DN of the pixels in the text
is  255  (dn=255), the thickness of lines in the characters is 1 (thick=1),
and  the  starting  line  and  sample  coordinates given (POSITION(1,2) and
POSITION(3,4) indicated where the bottom left of the first character in the
text  strings TEXT(1,2) is to be written (loc=1).  The output data set will
be 400 lines by 600 samples.

      font in out SIZE=(100,100,300,500) POSITION=(300,20,350,20) +
	  TEXT=("How goes it Bobby?","None of your business.") +
			FONT=(11) TALL=(40)

The  example  above  illustrates  three  features  of program "font" that are
important to understand:

1) The size field determines the portion of the input data set that will be
copied  to  the  output  and consequently determines the size of the output
image.

2)  The  line  and sample coordinates given for text positioning (values in
POSITION(1,2)  POSITION(3,4))  are locations in the input image.  POSITIONs
are  always  related  to  the output image.  This means that the first text
string  TEXT(1) ("How goes it Bobby?") will start at line 200 and sample 20
in the output image.

3)  The  number  of values for the parameters TEXT, FONT, WIDE, THICK, etc.
need  not  match  exactly.   If  there are more TEXT strings than there are
values  for  FONT,  WIDE, THICK, etc., the last value given is used for all
remaining  strings.   Thus  in this example FONT=11 and TALL=40 is used for
both strings.

The program calls certain routines from txtsubs.com, from p2 library,
namely TXTTEXT, TXTFONT, TXTSIZE, TXTCOLOR, TXTROTATE.
It uses the font files located by the environment variable VRDIFONTS.

If VRDIFONTS not set or if it cannot find fonts it returns
-2 from status1 = TXTFONT(FONT1)

ERROR REPORTING
    Now reports error returns for certain internal routines


1 - TXTFONT - ??E txtfont return = n
    where, 
        n = 0 - function not implemented
        n = 2 - illegal font type (VRDIFONTS environment variable not set or fonts not found)
        n = 4 - font table too small
        n = 6 - end of file on font description file

2 - TXTSIZE - ??E txtsize return = n
    where,
        n = 0 - function not implemented
        n = 2 - Illegal height
        n = 4 - Illegal horizontal scale factor

3 - TXTROTATE - ??E txtrotate return = n
    where,
        n = 0 - function not implemented
        n = 2 - illegal angle

4 - TXTCOLOR - ??E txtcolor return = n
    where,
        n = 0 - function not implemented
        n = 2 - illegal color value
 
5 - TXTTEXT - ??E txttext return = n
    where,
        n = 0 - function not implemented
        n = 8 - Coordinates are outside access window
        n = 10 - Illegal LOC (orientation) value
        n = 12 - Illegal character count
.page
TIMING:

WRITTEN BY: Bob Mortensen and Kevin Hussey

           (Parameter modification by Rich Walker AUG85)

COGNIZANT PROGRAMMER: R. J. Bambery

REVISIONS:
1.0 -- Original Version
2.0 -- Added ROTATE parameter and removed many restrictions
1995-01-02 V. Unruh (CRI) Made portable for UNIX
1996-07    HBM Fix handling of blanks within text
2008-08-21 R.J. Bambery - grabbed from VICAR V36 (Jun 27, 2007)
           Added error message support for txt calls
           (split off version for RJB)
2009-09-28 SRL  removed intial copy of the file to output, 
           then close and then ropen to update mode.
           Compressed files don't work in update mode. 
           Now it will Read, modify, write
           Add a call to xveaction so IO errors are reported
           It is still unreliable when using the size option
2010-02-14 R. J. Bambery - combined Aug 21, 2008 error handling
           with Steve Levoe's Sep 29, 2009 fixes, made 64-bit compliant
           with Linux, MacOSX/PowerPC and MacOSX/Intel

.LEVEL1
.VARI INP
The data set into which the
text is to be scribed

Example: FONTIN.DAT
.VARI OUT
Output data set containing
image input plus text added.

Example: FONTOUT.DAT
.VARI SIZE
Output data set dimensions.

Example: SIZE=(1,1,400,3360)
.VARI FONT
An integer code referring to
the nth (based on position)
text string letter type.

Example: FONT=(1,1,1,3,2,1,2)
.VARI TALL
The number of pixels tall the
characters of the nth text
are to be.

Example: 
TALL=(20,20,20,40,10,10,20)
.VARI WIDE
The number of pixels wide of
the characters in the nth
string will be wide(n)*tall(n).

Example: 
WIDE=(.8,.8,.5,.8,.9,.3,.6,.9)
.VARI THICK
The thickness in pixels of
the characters in the nth
text string

Example: THICK=(1,2,3,2,2,3,4,2)
.VARI POSITION
The sl and ss locations of
the nth text string given.
There should be 2*n entries
for this.

Example: 
POSITION=(201,301,501,701, +
          101,101,1001,551)
.VARI TEXT
The text strings to be
written into the output
image.

Example:  
TEXT=("This is string 1", +
      "This is string 2")
.VARI ROTATE
The angle to rotate the text
string using the given position
as the center of rotation.

Example:
ROTATE=(-90,0,180)
.VARI DN
The DN values of the pixels
of each of the strings to be
written.

Example: DN=(255,254,253)
.VARI LOC
This parameter is used as a
code to indicate left
justified, centered, or right
justification of each string.

Example:  LOC=(1,2,3,2,3)
.vari parms
This parameter is created in
order to handle more than one
datum passed to font by conlab

.LEVEL2 
.END 
$ Return
$!#############################################################################
$Test_File:
$ create tstfont.pdf
procedure
refgbl $autousage
body
let $autousage="none"
refgbl $echo
!
!
write "This is a unit test for the program FONT."
write "FONT places text into a VICAR file."
write "The program is simple and is simply tested."
!
write " "
write "A file is generated with FONT and five strings"
write "are placed into the file with various parameters."
write " "
write "The tester is left to verify with the VIDS command,"
write "JSHOW-CURSOR, that the strings are correct."
write " "
write "The correct values are evident from the parameters"
write "to FONT in the test."
write " "
write "This test will use the JDISP command so the tester"
write "must allocate a device and invoke VIDS. "
!
let $echo="yes"
gen a 512 512 ival=128 sinc=0 linc=0
!
font a b font=(1,2,3,4,5) tall=(10,15,20,25,40) +
         wide=(1.,.8,.7,.5,2.) thick=(1,2,3,4,5) +
         position=(20,20 20,200 100,50, 300,100 400,140) +
         text=("one little dog","two","three","four","five") +
         rotate=(0,0,10,80,0) +
         dn=(200,10,100,40,255)
!
let $echo="NO"
!
end-proc
$!-----------------------------------------------------------------------------
$ create tstfont4c.pdf
procedure
parm      nl  integer  def=512
parm      ns  integer  def=512

local      url  integer
local      urs  integer
local      ull  integer
local      uls  integer
local      lll  integer
local      lls  integer
local      lrl  integer
local      lrs  integer
local      cl  integer
local      cs  integer
local      newns  integer
local      newnl  integer

local  cmprs  string  ! system variable indicating file compression

refgbl $autousage
body
let $autousage="none"
refgbl $echo
!
!
write "This is a unit test for the program FONT."
write "Compress the file before calling font."
write "FONT places text into a VICAR file."
write "The program is simple and is simply tested."
write "This tests if the label strings have any order dependance."
!
write "create a file &ns x &nl "
!
let $echo="yes"
gen a &"nl" &"ns" ival=128 sinc=1 linc=1

lab2tcl    inp=a v1=cmprs keyword=compress type=system
write         "a COMPRESS = &cmprs "
if (cmprs = "NONE")
	write "file is NOT COMPRESSED - COMPRESSing as c"
	comprs    a c compress=BASIC
else 	
    write "file is already COMPRESSED - copying to c"
	copy      a c	
end-if

lab2tcl    inp=c v1=cmprs keyword=compress type=system
write         "c COMPRESS = &cmprs "

let ull = 50
let uls = 50

let url = 50
let urs = (&ns - 50)

let lrl = (&nl - 50)
let lrs = (&ns - 50)

let lll = (&nl - 50)
let lls = 50

let cl = (&nl / 2)
let cs = (&ns / 2)



write "lrl=&lrl lrs=&lrs "
! position is (line,sample)
font c b   font=(1,2,3,4,5) tall=(10,15,20,25,40) +
         wide=(1.,.8,.7,.5,2.) thick=(1,2,3,4,5) +
         position=(&lrl,&lrs &url,&urs &cl,&cs &lll,&lls &ull,&uls) +
         text=("LR","UR","&cl x &cs","LL","UL") +
         rotate=(0,0,10,80,0) +
         dn=(200,10,100,40,255) 
! test using a sub area, this will probably break it

!
font c d   font=(1,2,3,4,5) tall=(10,15,20,25,40) +
         wide=(1.,.8,.7,.5,2.) thick=(1,2,3,4,5) +
         position=(&lrl,&lrs &url,&urs &cl,&cs &lll,&lls &ull,&uls) +
         text=("LR","UR","&cl x &cs","LL","UL") +
         rotate=(0,0,10,80,0) +
         dn=(200,10,100,40,255) 

! cutout a portion of the original image using size()
let newns = (&ns - (&ns/4))
let newnl = (&nl - (&nl/4))

let urs = (&newns - 50)

let lrl = (&newnl - 50)
let lrs = (&newns - 50)

let lll = (&newnl - 50)
let lls = 50

let cl = (&newnl / 2)
let cs = (&newns / 2)
! In font I may need to adjust positions based on start X annd Y
font c e  size=(50,50,&newnl,&newns) font=(1,2,3,4,5) tall=(10,15,20,25,40) +
         wide=(1.,.8,.7,.5,2.) thick=(1,2,3,4,5) +
         position=(&lrl,&lrs &url,&urs &cl,&cs &lll,&lls &ull,&uls) +
         text=("LR","UR","&cl x &cs","LL","UL") +
         rotate=(0,0,10,80,0) +
         dn=(200,10,100,40,255) 
let $echo="NO"
!
end-proc
$!-----------------------------------------------------------------------------
$ create tstfont2.pdf
procedure                           !sumcoolpseudo.pdf
local version type=string count=1 init="Version - Mon Feb 23 2015 WLB"
!Generates  Capital M and little y images in a variety of conditions

!   font parameters 
!       position is bottom left corner of the first letter of the string
!       tall is height in pixels 
!       wide is % of height in pixels (if tall is 15 a wide of 0.8 is 12)
!       thick is thickness in pixels
!       font is font number
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 
!
parm dest   type=string count=1 def="./"
refgbl $echo
refgbl $SFI
refgbl $SKEY
 
! Operational History:
! Dec 07, 2007 - RJB - initial release
! Wed Apr 15 2015 - WLB - added difpic comparison of test output to 
! ease testing.
!
BODY
!!!!!!
let $echo="no"

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
local afidsroot type=string
local aftestdata type=string
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata

ush rm -f gl

if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/gll gl
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/gll gl
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/gll gl
    end-if
end-if

write "###============================================================="
write "### >>> tstfont2.pdf"
write "### tsttestfont2.pdf | &version"
let _onfail = "continue"               !if command abends

let $echo="yes"

gen &"dest"black100x100.byte nl=100  ns=100  format=byte +
    linc=0 sinc=0 ival=0

!!!!!!!!!!!!!!!!!!!!! FIRST TEST (Tall 10 - font 1)
! SANS SERIF
!Barely readable 
!top of M at line 39 - bottom at line 49
!left of M at sample 52 - right at sample 59
font &"dest"black100x100.byte &"dest"M_50.50_10_0.8_2_1 +
    text = ("M") +
    position = (50,50) +
    tall = 10 +
    wide = 0.8 + 
    thick = 2 +
    font = 1
difpic (&"dest"M_50.50_10_0.8_2_1, gl/M_50.50_10_0.8_2_1)

!top of y is at line 42 - bottom at line 53
!left of y at sample 51 - right at sample 56
!   top of y is 3 pixels down from top of M - bottom of y is 4 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_10_0.8_2_1 +
    text = ("y") +
    position = (50,50) +
    tall = 10 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"y_50.50_10_0.8_2_1, gl/y_50.50_10_0.8_2_1)

!Barely readable
!top of M at line 39 - bottom of y is a line 53
font &"dest"black100x100.byte &"dest"My_50.50_10_0.8_2_1 +
    text = ("My") +
    position = (50,50) +
    tall = 10 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"My_50.50_10_0.8_2_1, gl/My_50.50_10_0.8_2_1)

!!!!!!!!!!!!!!!!!!!!! SECOND TEST (Tall 20 - font 1)
! SANS SERIF
! READABLE
!top of M at line 29 - bottom at line 49
!left of M at sample 53 - right at sample 66
font &"dest"black100x100.byte &"dest"M_50.50_20_0.8_2_1 +
    text = ("M") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"M_50.50_20_0.8_2_1, gl/M_50.50_20_0.8_2_1)

!top of y at line 36 - bottom at line 57
!left of y at sample 52 - right at sample 62
!  top is 3 pixels down from top of M - bottom of y is 3 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_20_0.8_2_1 +
    text = ("y") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"y_50.50_20_0.8_2_1, gl/y_50.50_20_0.8_2_1)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_20_0.8_2_1 +
    text = ("My") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"My_50.50_20_0.8_2_1, gl/My_50.50_20_0.8_2_1)

!!!!!!!!!!!!!!!!!!!!! THIRD TEST (Tall 20 - font 2)
! SANS SERIF
! READABLE
!top of M at line 29 - bottom at line 50
!left of M at sample 53 - right at sample 66
font &"dest"black100x100.byte &"dest"M_50.50_20_0.8_2_2 +
    text = ("M") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 2
difpic (&"dest"M_50.50_20_0.8_2_2, gl/M_50.50_20_0.8_2_2)

!top of y at line 36 - bottom at line 57
!left of y at sample 52 - right at sample 62
!  top is 7 pixels down from top of M - bottom of y is 7 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_20_0.8_2_2 +
    text = ("y") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 2
difpic (&"dest"y_50.50_20_0.8_2_2, gl/y_50.50_20_0.8_2_2)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_20_0.8_2_2 +
    text = ("My") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 2
difpic (&"dest"My_50.50_20_0.8_2_2, gl/My_50.50_20_0.8_2_2)

!!!!!!!!!!!!!!!!!!!!! FOURTH TEST (Tall 20 - font 3)
! READABLE - SERIF
!top of M at line 29 - bottom at line 50
!left of M at sample 52 - right at sample 71
font &"dest"black100x100.byte &"dest"M_50.50_20_0.8_2_3 +
    text = ("M") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 3
difpic (&"dest"M_50.50_20_0.8_2_3, gl/M_50.50_20_0.8_2_3)

!top of y at line 35 - bottom at line 56
!left of y at sample 51 - right at sample 66
!  top is 6 pixels down from top of M - bottom of y is 6 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_20_0.8_2_3 +
    text = ("y") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 3
difpic (&"dest"y_50.50_20_0.8_2_3, gl/y_50.50_20_0.8_2_3)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_20_0.8_2_3 +
    text = ("My") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 3
difpic (&"dest"My_50.50_20_0.8_2_3, gl/My_50.50_20_0.8_2_3)

!!!!!!!!!!!!!!!!!!!!! FIFTH TEST (Tall 20 - font 4)
! SERIF
! READABLE - less than font 2
!top of M at line 29 - bottom at line 50
!left of M at sample 52 - right at sample 71
font &"dest"black100x100.byte &"dest"M_50.50_20_0.8_2_4 +
    text = ("M") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"M_50.50_20_0.8_2_4, gl/M_50.50_20_0.8_2_4)

!top of y at line 35 - bottom at line 56
!left of y at sample 51 - right at sample 66
!  top is 6 pixels down from top of M - bottom of y is 6 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_20_0.8_2_4 +
    text = ("y") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"y_50.50_20_0.8_2_4, gl/y_50.50_20_0.8_2_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_20_0.8_2_4 +
    text = ("My") +
    position = (50,50) +
    tall = 20 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"My_50.50_20_0.8_2_4, gl/My_50.50_20_0.8_2_4)

!!!!!!!!!!!!!!!!!!!!! SIXTH TEST (Tall 30 - font 1)
! SANS SERIF
! READABLE
!top of M at line 19 - bottom at line 49
!left of M at sample 55 - right at sample 74
font &"dest"black100x100.byte &"dest"M_50.50_30_0.8_2_1 +
    text = ("M") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"M_50.50_30_0.8_2_1, gl/M_50.50_30_0.8_2_1)

!top of y at line 29 - bottom at line 60
!left of y at sample 52 - right at sample 67
!  top is 10 pixels down from top of M - bottom of y is 11 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_30_0.8_2_1 +
    text = ("y") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"y_50.50_30_0.8_2_1, gl/y_50.50_30_0.8_2_1)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_30_0.8_2_1 +
    text = ("My") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"My_50.50_30_0.8_2_1, gl/My_50.50_30_0.8_2_1)

!!!!!!!!!!!!!!!!!!!!! SEVENTH TEST (Tall 30 - font 4)
! SERIF
! READABLE - getting holes in letter - use thick =3 for solid characters
!top of M at line 19 - bottom at line 50
!left of M at sample 54 - right at sample 81
font &"dest"black100x100.byte &"dest"M_50.50_30_0.8_2_4 +
    text = ("M") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"M_50.50_30_0.8_2_4, gl/M_50.50_30_0.8_2_4)

!top of y at line 28 - bottom at line 59
!left of y at sample 52 - right at sample 74 
!  top is 11 pixels down from top of M - bottom of y is 9 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_30_0.8_2_4 +
    text = ("y") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"y_50.50_30_0.8_2_4, gl/y_50.50_30_0.8_2_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_30_0.8_2_4 +
    text = ("My") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"My_50.50_30_0.8_2_4, gl/My_50.50_30_0.8_2_4)

!!!!!!!!!!!!!!!!!!!!! EIGHTH TEST (Tall 30 - font 4 thick 3)
! SERIF
! READABLE - 1st size where thick=3 is OK
!top of M at line 18 - bottom at line 50
!left of M at sample 54 - right at sample 82
font &"dest"black100x100.byte &"dest"M_50.50_30_0.8_3_4 +
    text = ("M") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 3 +
    font = 4
difpic (&"dest"M_50.50_30_0.8_3_4, gl/M_50.50_30_0.8_3_4)

!top of y at line 26 - bottom at line 59
!left of y at sample 52 - right at sample 75
!  top is 12 pixels down from top of M - bottom of y is 9 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_30_0.8_3_4 +
    text = ("y") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 3 +
    font = 4
difpic (&"dest"y_50.50_30_0.8_3_4, gl/y_50.50_30_0.8_3_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_30_0.8_3_4 +
    text = ("My") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 3 +
    font = 4
difpic (&"dest"My_50.50_30_0.8_3_4, gl/My_50.50_30_0.8_3_4)

!!!!!!!!!!!!!!!!!!!!! NINTH TEST (Tall 15 - font 1 - thick 2)
! SANS SERIF
! READABLE
!top of M at line 34 - bottom at line 49
!left of M at sample 52 - right at sample 62
font &"dest"black100x100.byte &"dest"M_50.50_15_0.8_2_1 +
    text = ("M") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"M_50.50_15_0.8_2_1, gl/M_50.50_15_0.8_2_1)

!top of y at line 39 - bottom at line 54
!left of y at sample 51 - right at sample 59
!  top is 5 pixels down from top of M - bottom of y is 5 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_15_0.8_2_1 +
    text = ("y") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"y_50.50_15_0.8_2_1, gl/y_50.50_15_0.8_2_1)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_15_0.8_2_1 +
    text = ("My") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 1
difpic (&"dest"My_50.50_15_0.8_2_1, gl/My_50.50_15_0.8_2_1)

!!!!!!!!!!!!!!!!!!!!! TENTH TEST (Tall 15 - font 4 - thick 2)
! SERIF
! READABLE
!top of M at line 34 - bottom at line 50
!left of M at sample 52 - right at sample 66
font &"dest"black100x100.byte &"dest"M_50.50_15_0.8_2_4 +
    text = ("M") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"M_50.50_15_0.8_2_4, gl/M_50.50_15_0.8_2_4)

!top of y at line 39 - bottom at line 55
!left of y at sample 51 - right at sample 62
!  top is 5 pixels down from top of M - bottom of y is 5 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_15_0.8_2_4 +
    text = ("y") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"y_50.50_15_0.8_2_4, gl/y_50.50_15_0.8_2_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_15_0.8_2_4 +
    text = ("My") +
    position = (50,50) +
    tall = 15 +
    wide = 0.8 +
    thick = 2 +
    font = 4
difpic (&"dest"My_50.50_15_0.8_2_4, gl/My_50.50_15_0.8_2_4)

!!!!!!!!!!!!!!!!!!!!! ELEVENTH TEST (Tall 15 - font 4 - thick 2 - wide 0.6)
! SERIF
! BARELY READABLE
!top of M at line 34 - bottom at line 50
!left of M at sample 51 - right at sample 62
font &"dest"black100x100.byte &"dest"M_50.50_15_0.6_2_4 +
    text = ("M") +
    position = (50,50) +
    tall = 15 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"M_50.50_15_0.6_2_4, gl/M_50.50_15_0.6_2_4)

!top of y at line 39 - bottom at line 55
!left of y at sample 51 - right at sample 59
!  top is 5 pixels down from top of M - bottom of y is 5 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_15_0.6_2_4 +
    text = ("y") +
    position = (50,50) +
    tall = 15 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"y_50.50_15_0.6_2_4, gl/y_50.50_15_0.6_2_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_15_0.6_2_4 +
    text = ("My") +
    position = (50,50) +
    tall = 15 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"My_50.50_15_0.6_2_4, gl/My_50.50_15_0.6_2_4)

!!!!!!!!!!!!!!!!!!!!! TWELFTH TEST (Tall 30 - font 3 - thick 4 - wide 0.8)
! SERIF
! READABLE
!top of M at line 17 - bottom at line 50
!left of M at sample 54 - right at sample 83
font &"dest"black100x100.byte &"dest"M_50.50_30_0.8_4_3 +
    text = ("M") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 4 +
    font = 3
difpic (&"dest"M_50.50_30_0.8_4_3, gl/M_50.50_30_0.8_4_3)

!top of y at line 26 - bottom at line 59
!left of y at sample 52 - right at sample 76
!  top is 9 pixels down from top of M - bottom of y is 9 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_30_0.8_4_3 +
    text = ("y") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 4 +
    font = 3
difpic (&"dest"y_50.50_30_0.8_4_3, gl/y_50.50_30_0.8_4_3)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_30_0.8_4_3 +
    text = ("My") +
    position = (50,50) +
    tall = 30 +
    wide = 0.8 +
    thick = 4 +
    font = 3
difpic (&"dest"My_50.50_30_0.8_4_3, gl/My_50.50_30_0.8_4_3)

!!!!!!!!!!!!!!!!!!!!! THIRTEENTH TEST (Tall 20 - font 4 - thick 2 - wide 0.6)
! SERIF
! READABLE 
!top of M at line 29 - bottom at line 50
!left of M at sample 52 - right at sample 66
font &"dest"black100x100.byte &"dest"M_50.50_20_0.6_2_4 +
    text = ("M") +
    position = (50,50) +
    tall = 20 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"M_50.50_20_0.6_2_4, gl/M_50.50_20_0.6_2_4)

!top of y at line 35 - bottom at line 56
!left of y at sample 51 - right at sample 62
!  top is 6 pixels down from top of M - bottom of y is 6 pixels below bottom of M
font &"dest"black100x100.byte &"dest"y_50.50_20_0.6_2_4 +
    text = ("y") +
    position = (50,50) +
    tall = 20 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"y_50.50_20_0.6_2_4, gl/y_50.50_20_0.6_2_4)

!Readable
font &"dest"black100x100.byte &"dest"My_50.50_20_0.6_2_4 +
    text = ("My") +
    position = (50,50) +
    tall = 20 +
    wide = 0.6 +
    thick = 2 +
    font = 4
difpic (&"dest"My_50.50_20_0.6_2_4, gl/My_50.50_20_0.6_2_4)

ush rm gl

theend>

end-proc
$!-----------------------------------------------------------------------------
$ create tstfont2.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

###=============================================================
### >>> tstfont2.pdf
### tsttestfont2.pdf | Version - Mon Feb 23 2015 WLB
gen ./black100x100.byte nl=100  ns=100  format=byte      l+
inc=0 sinc=0 ival=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
font ./black100x100.byte ./M_50.50_10_0.8_2_1      text =+
 ("M")      positi+
on = (50,50)      tall =+
 10      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_10_0.8_2_1, gl/M_50.50_10_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_10_0.8_2_1      text =+
 ("y")      positi+
on = (50,50)      tall =+
 10      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_10_0.8_2_1, gl/y_50.50_10_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_10_0.8_2_1      text =+
 ("My")      positi+
on = (50,50)      tall =+
 10      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_10_0.8_2_1, gl/My_50.50_10_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_20_0.8_2_1      text =+
 ("M")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_20_0.8_2_1, gl/M_50.50_20_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_20_0.8_2_1      text =+
 ("y")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_20_0.8_2_1, gl/y_50.50_20_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_20_0.8_2_1      text =+
 ("My")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_20_0.8_2_1, gl/My_50.50_20_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_20_0.8_2_2      text =+
 ("M")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 2
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_20_0.8_2_2, gl/M_50.50_20_0.8_2_2)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_20_0.8_2_2      text =+
 ("y")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 2
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_20_0.8_2_2, gl/y_50.50_20_0.8_2_2)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_20_0.8_2_2      text =+
 ("My")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 2
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_20_0.8_2_2, gl/My_50.50_20_0.8_2_2)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_20_0.8_2_3      text =+
 ("M")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_20_0.8_2_3, gl/M_50.50_20_0.8_2_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_20_0.8_2_3      text =+
 ("y")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_20_0.8_2_3, gl/y_50.50_20_0.8_2_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_20_0.8_2_3      text =+
 ("My")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_20_0.8_2_3, gl/My_50.50_20_0.8_2_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_20_0.8_2_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_20_0.8_2_4, gl/M_50.50_20_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_20_0.8_2_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_20_0.8_2_4, gl/y_50.50_20_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_20_0.8_2_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_20_0.8_2_4, gl/My_50.50_20_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_30_0.8_2_1      text =+
 ("M")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_30_0.8_2_1, gl/M_50.50_30_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_30_0.8_2_1      text =+
 ("y")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_30_0.8_2_1, gl/y_50.50_30_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_30_0.8_2_1      text =+
 ("My")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_30_0.8_2_1, gl/My_50.50_30_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_30_0.8_2_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_30_0.8_2_4, gl/M_50.50_30_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_30_0.8_2_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_30_0.8_2_4, gl/y_50.50_30_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_30_0.8_2_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_30_0.8_2_4, gl/My_50.50_30_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_30_0.8_3_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 3      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_30_0.8_3_4, gl/M_50.50_30_0.8_3_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_30_0.8_3_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 3      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_30_0.8_3_4, gl/y_50.50_30_0.8_3_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_30_0.8_3_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 3      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_30_0.8_3_4, gl/My_50.50_30_0.8_3_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_15_0.8_2_1      text =+
 ("M")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_15_0.8_2_1, gl/M_50.50_15_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_15_0.8_2_1      text =+
 ("y")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_15_0.8_2_1, gl/y_50.50_15_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_15_0.8_2_1      text =+
 ("My")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 1
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_15_0.8_2_1, gl/My_50.50_15_0.8_2_1)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_15_0.8_2_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_15_0.8_2_4, gl/M_50.50_15_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_15_0.8_2_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_15_0.8_2_4, gl/y_50.50_15_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_15_0.8_2_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.8      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_15_0.8_2_4, gl/My_50.50_15_0.8_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_15_0.6_2_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_15_0.6_2_4, gl/M_50.50_15_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_15_0.6_2_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_15_0.6_2_4, gl/y_50.50_15_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_15_0.6_2_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 15      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_15_0.6_2_4, gl/My_50.50_15_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_30_0.8_4_3      text =+
 ("M")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 4      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_30_0.8_4_3, gl/M_50.50_30_0.8_4_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_30_0.8_4_3      text =+
 ("y")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 4      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_30_0.8_4_3, gl/y_50.50_30_0.8_4_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_30_0.8_4_3      text =+
 ("My")      positi+
on = (50,50)      tall =+
 30      wide =+
 0.8      thick +
= 4      font =+
 3
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_30_0.8_4_3, gl/My_50.50_30_0.8_4_3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./M_50.50_20_0.6_2_4      text =+
 ("M")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./M_50.50_20_0.6_2_4, gl/M_50.50_20_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./y_50.50_20_0.6_2_4      text =+
 ("y")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./y_50.50_20_0.6_2_4, gl/y_50.50_20_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
font ./black100x100.byte ./My_50.50_20_0.6_2_4      text =+
 ("My")      positi+
on = (50,50)      tall =+
 20      wide =+
 0.6      thick +
= 2      font =+
 4
Beginning VICAR task font
FONT version Feb 14, 2010 (64-bit) - RJB
difpic (./My_50.50_20_0.6_2_4, gl/My_50.50_20_0.6_2_4)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
ush rm gl
end-proc
$ Return
$!#############################################################################
