C      PROGRAM  AVERAGE
C#######################################################################
C  NAME OF ROUTINE
C      "AVERAGE"  ( AVERAGE images )
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     3-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      average INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      average INP=(a...) OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      average (a...) b (sl,ss,nl,ns) optional parameters
C      average (a...) b optional parameters
C
C       Here (a...) represents a list of 2 to 48 file names.
C       b represents an output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file names.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C               The same SIZE parameters apply to each of the input
C               image files.
C  SUBROUTINES CALLED
C      MAIN44 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C      MAIN44   (name of top level subroutine by VICAR convention)
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM AVERAGE.
C      
C  CALLED BY
C      "average"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      IMPLICIT INTEGER (A-Z)
	implicit none
      INCLUDE 'fortport'
      integer*4 temp
c      BYTE IBUF(120000),OBUF(20000)
      integer*4 idsn(48), is(48)
	integer*4 ind,ni,sl,ss,nl,ns,nli,nsi,el
	integer*4 i,ii,j,isp,outfile,icode
	real*8 dibuf(120000),dobuf(20000)
	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org

C
C
C
C=================START OF EXECUTABLE CODE===============================     
C
      call ifmessage ('AVERAGE  16-MAY-2011')
      call xveaction ('SA', ' ')
      call xvpcnt( 'INP', ni )

         call xvunit( idsn(1), 'INP', 1, ind, ' ' )
         call xvopen( idsn(1), ind, 'OP', 'READ', ' ' )
	call xvget(idsn(1),ind,'FORMAT',format,'ORG',org,' ')
         call xvsize( sl, ss, nl, ns, nli, nsi )   ! GET SIZE PARAMETER.

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(idsn(1),ind,' ')

C
C  OPEN DATA SETS
      DO I=1,NI
         call xvunit( idsn(i), 'INP', i, ind, ' ' )
         call xvopen( idsn(i), ind, 'OP', 'READ', 
     & 		'I_FORMAT',fmt(icode),'U_FORMAT','DOUB',' ' )
      END DO

         call xvunit( outfile, 'OUT', 1, ind, ' ' )
         call xvopen( outfile, ind, 'OP', 'WRITE',
     .         'O_FORMAT',fmt(icode),'U_FORMAT','DOUB', 
     .         'U_NL', nl, 'U_NS', ns, ' ' )
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP

      EL = SL+NL-1

      DO 491 II=SL,EL           ! LOOP OVER LINES.
              IF ( II .EQ. SL )   THEN
                 ISP = 1
                 DO I = 1, NI

                  call xvread( idsn(i), dibuf(isp), ind, 'LINE', 
     .                  SL, ' ' )
                  ISP = ISP + NSI
                 END DO
              ELSE
                 ISP = 1
                 DO I = 1, NI

                  call xvread( idsn(i), dibuf(isp), ind, ' ' )
                  ISP = ISP + NSI
                 END DO
              END IF
                 IS(1) = SS
                 DO I = 2,NI
                    IS(I) = IS(I-1) + NSI
                 END DO

                 DO  J = 1, NS              ! THE SAMPLE LOOP.
                     TEMP = 0
                     DO I = 1, NI
c                        TEMP = TEMP + BYTE2INT( IBUF(  IS(I) ) )
			 temp = temp + dibuf(  is(i) )
                        IS(I) = IS(I) + 1
                     END DO
                     TEMP = TEMP / NI              ! AVERAGE
c                     OBUF(J) = INT2BYTE(TEMP)      ! LOW ORDER BYTE.   
		     dobuf(J) = temp	
                 END DO

                 call xvwrit( outfile, dobuf, ind, ' ' )

491   CONTINUE

      RETURN          ! NORMAL END.
      END

