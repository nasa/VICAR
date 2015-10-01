$!****************************************************************************
$!
$! Build proc for MIPL module vlookup
$! VPACK Version 1.9, Monday, December 07, 2009, 17:08:23
$!
$! Execute by entering:		$ @vlookup
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
$ write sys$output "*** module vlookup ***"
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
$ write sys$output "Invalid argument given to vlookup.com file -- ", primary
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
$   if F$SEARCH("vlookup.imake") .nes. ""
$   then
$      vimake vlookup
$      purge vlookup.bld
$   else
$      if F$SEARCH("vlookup.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vlookup
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vlookup.bld "STD"
$   else
$      @vlookup.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vlookup.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vlookup.com -mixed -
	-s vlookup.f vlookuploop.c -
	-i vlookup.imake -
	-p vlookup.pdf -
	-t tstvlookup.pdf timevlookup.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vlookup.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  VLOOKUP

C     8-89  RGD  QUICK HACK TO LOOKUP FOR READING VIDS LOOKUP TABLES FOR VOYAGER
c    10-90  lwk  cleaned up program for P2SOR, following RGD's guidelines
C     2-94  SP   Made portable for UNIX.  Changed code to use 
C                BYTE2INT AND INT2BYTE for converting 
C                between BYTE and INTEGER.  Added XVEACTION and 
C                IFMESSAGE calls.  CHANGED TO USE IBIS2 CALLS.  Changed to
C                read just the specified columns of the LUT file.
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

      PARAMETER (MAXFIL=5)	! MAX. NUMBER OF INPUT/OUTPUT FILES OR BANDS
      PARAMETER (MAXSAMPS=1000000)  ! MAX NUMBER OF SAMPLES PER LINE.
      INTEGER PARM(2000), USE(MAXFIL)
      INTEGER*2 TABLE2(MAXFIL,256)
      BYTE IBUF(MAXSAMPS), OBUF(MAXSAMPS), OBUF2(MAXSAMPS), 
     & OBUF3(MAXSAMPS), TABLE(256,MAXFIL)
      INTEGER ODSN(MAXFIL), IDSN(MAXFIL)
      LOGICAL TBLF
      INTEGER*2 VIDSTABLE(256)
      INTEGER VCOLLEN, VNCOLS, STATUS, LUNIT
      CHARACTER*256 LUTFIL
      CHARACTER*8 FMT
C
C=================START OF EXECUTABLE CODE===============================     

      CALL IFMESSAGE('VLOOKUP version 4-MAR-1994')
      DNMIN = 0                ! BYTE OUTPUT CURRENTLY.
      DNMAX = 255
C
C  DEFAULTS
      DO I=1,MAXFIL
	USE(I) = I
      ENDDO
      TBLF=.FALSE.
      CALL ZIA(TABLE2,MAXFIL*128)
C
C  GET THE PARAMETERS ENTERED BY THE USER.

      CALL XVPCNT( 'INP', NI )
      CALL XVPCNT( 'OUT', NO )

C
C  'TABLE' (OLD 'PS') PARAMETER:
 
      PSNO = 0
      CALL XVPARM( 'TABLE', PSNO, ICOUNT, IDEF, 1 )
      IF (ICOUNT.GT.0)  THEN
         CALL PSTAB(PSNO,TABLE2)
      END IF
C
C  'COLUMN' (OLD 'USE') PARAMETER:

      CALL XVPARM( 'COLUMN', PARM, IVALS, IDEF, 0 )
      IF (IVALS.GT.0) THEN
	IVALS = MIN( IVALS, MAXFIL)
	DO I = 1, IVALS
	  USE(I) = PARM(I)
	ENDDO
      ENDIF
C
C  OPEN DATA SETS

      DO I=1,NI

	CALL XVUNIT( IDSN(I), 'INP', I, IND,' ')
	CALL XVOPEN( IDSN(I), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     1 'IO_ACT', 'SA',' ')

	CALL XVGET( IDSN(I), IND, 'FORMAT', FMT, 'NB', NBI,' ')
	IF (FMT.NE.'BYTE') CALL MABEND('ONLY BYTE DATA ALLOWED')
	IF (NBI.GT.1 .AND. NI.GT.1) THEN
	  CALL XVMESSAGE(
     .         'MULTIPLE INPUT FILES IMPLIES SINGLE-BAND MODE:',' ')
	  CALL XVMESSAGE('  BANDS BEYOND THE FIRST ARE IGNORED',' ')
	  NBI = 1
	ENDIF
	IF (NBI.GT.MAXFIL) THEN
	  CALL XVMESSAGE('INPUT HAS TOO MANY BANDS, EXCESS IGNORED',' ')
	  NBI = MAXFIL
	ENDIF

	IF (I.EQ.1) THEN
	  CALL XVSIZE( SL, SS, NL, NS, dummy1,dummy2)
	  CALL XVBANDS( SB, NB,DUMMY3)
	  IF (NB.GT.1 .AND. NO.GT.1) THEN
	    IF (NB.GT.NBI) THEN		! IF 'NB' SPECIFIED EXPLICITLY
	      CALL MABEND('ONLY ONE OUTPUT ALLOWED IF MULTI-BAND')
	    ELSE
	      CALL XVMESSAGE(
     .         'MULTIPLE OUTPUT FILES IMPLIES SINGLE-BAND MODE:',' ')
	      CALL XVMESSAGE('  BANDS BEYOND THE FIRST ARE IGNORED',' ')
	      NB = 1
	    ENDIF
	  ENDIF
	ELSE
	  CALL XVGET( IDSN(I), IND, 'NL', NLI, 'NS', NSI,' ')
	  IF (NLI .LT. SL+NL-1  .OR. NSI .LT. SS+NS-1) CALL MABEND(
     1  'ERROR: ALL INPUT IMAGES SHOULD HAVE SAME SIZE')
	ENDIF

		! IF SEQUENTIAL READS, MOVE TO JUST BEFORE 1ST LINE:
	IF (NBI.EQ.1.AND.SL.GT.1) 
     .      CALL XVREAD(IDSN(I),IBUF,IND,'LINE',SL-1,' ')

      ENDDO

      NINP = MAX( NI, NBI)
      NOUT = MAX( NO, NB)

C  READ THE TABLE FROM THE INPUT DATA SET (IF PROVIDED)

      TBLF = .FALSE.
      CALL XVPARM( 'LUTFILE', LUTFIL, ICOUNT, IDEF, 1)
      IF (ICOUNT.GT.0) THEN
	TBLF = .TRUE.
	CALL XVUNIT (LUNIT,'NONE',1,STATUS,'U_NAME',LUTFIL, ' ')

        CALL IBIS_FILE_OPEN(LUNIT, LIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(LUNIT, STATUS, 1)

C  get the # rows & columns of the input IBIS file:

	mcount = ibis_file_get( LIBIS,'nc', VNCOLS, 1,1 )
	if (mcount.lt.0) call ibis_signal( LIBIS, mcount, 1)
	mcount = ibis_file_get( LIBIS,'nr', VCOLLEN, 1,1 )
	if (mcount.lt.0) call ibis_signal( LIBIS, mcount, 1)

	IF (VNCOLS .LE. 0) THEN	! TABLE IS ALREADY 0 IF IT DOESN'T EXIST
	  CONTINUE	! (??)
	ELSE
	  DO ICOL = 1,MIN(NOUT,VNCOLS)   ! READ JUST DESIRED COLUMNS.
            call ibis_column_set(LIBIS,'u_format','HALF',USE(ICOL),
     .                           status)
            if (status .ne.1) call ibis_signal( LIBIS, status, 1)
	    call ibis_column_read( LIBIS, VIDSTABLE, 
     +			USE(ICOL), 1, MIN(VCOLLEN,256), status )
            if (status .ne.1) call ibis_signal( LIBIS, status, 1)
	    DO I = 1, MIN(VCOLLEN,256)
	      TABLE2(ICOL,I) = VIDSTABLE(I) ! SHUFFLE
	    ENDDO
	  ENDDO
	ENDIF


	call ibis_file_close( LIBIS, ' ', status ) 
        if (status .ne.1) call ibis_signal( LIBIS, status, 1)
      ENDIF

      IF (.NOT. TBLF .AND. PSNO .EQ. 0) 
     .    CALL MABEND('ERROR: NEITHER LUTFILE NOR TABLE ENTERED.')

      DO I=1,NO
	CALL XVUNIT( ODSN(I), 'OUT', I, IND ,' ')
	CALL XVOPEN( ODSN(I), IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     1 'IO_ACT', 'SA', 'U_NL', NL, 'U_NS', NS, 'U_NB', NB,' ')
      ENDDO
C
C  REFORMAT THE TABLE - CONVERT TO BYTE
      DO J=1,NOUT
       IF (TBLF)  THEN
         DO I=1,256
          ITEMP = MAX( DNMIN, TABLE2(J,I) )
          ITEMP = MIN( DNMAX, ITEMP )
	  TABLE(I,J) = INT2BYTE( ITEMP )
 	 ENDDO
       ELSE
         DO I=1,256
          ITEMP = MAX( DNMIN, TABLE2(USE(J),I) )  ! IF COLUMN AND TABLE
          ITEMP = MIN( DNMAX, ITEMP )             ! THEN USE(J) USED.
	  TABLE(I,J) = INT2BYTE( ITEMP )
 	 ENDDO
       END IF
      ENDDO
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP  - USE ARRAY ALREADY FACTORED IN.

      IF (NINP.EQ.1 .AND. NOUT.EQ.3) THEN	! IF PSEUDO-COLOR

	DO II=1,NL           				! LOOP OVER LINES.
	  CALL XVREAD( IDSN(1), IBUF, IND,' ')
					                        ! DO LOOKUP.
	  CALL LOOKUPLOOP3( IBUF(SS), NS, OBUF, TABLE(1,1), OBUF2,
     1  TABLE(1,2), OBUF3, TABLE(1,3))
	  IF (NO.EQ.3) THEN
	    CALL XVWRIT( ODSN(1), OBUF, IND,' ')
	    CALL XVWRIT( ODSN(2), OBUF2, IND,' ')
	    CALL XVWRIT( ODSN(3), OBUF3, IND,' ')
	  ELSE
	    CALL XVWRIT( ODSN(1), OBUF, IND, 'LINE', II, 'BAND', 1,' ')
	    CALL XVWRIT( ODSN(1), OBUF2, IND, 'LINE', II, 'BAND', 2,' ')
	    CALL XVWRIT( ODSN(1), OBUF3, IND, 'LINE', II, 'BAND', 3,' ')
	  ENDIF
	ENDDO

      ELSE

	DO II=1,NL		! LOOP OVER LINES.
	  DO J=1,NOUT			! LOOP OVER OUTPUT IMAGES.
	    IF (J.LE.NINP) THEN
	      IF (NBI.EQ.1) THEN
		CALL XVREAD( IDSN(J), IBUF, IND,' ')
	      ELSE
		CALL XVREAD( IDSN(1), IBUF, IND, 'LINE', SL-1+II,
     .                       'BAND', J,' ')
	      ENDIF
            ENDIF
            CALL LOOKUPLOOP (IBUF(SS),OBUF,TABLE(1,J),NS)  ! DO LOOKUP.
	    IF (NB.EQ.1) THEN
	      CALL XVWRIT( ODSN(J), OBUF, IND,' ')
	    ELSE
	      CALL XVWRIT( ODSN(1), OBUF, IND, 'LINE',II, 'BAND',J,' ')
	    ENDIF
	  ENDDO
	ENDDO
      ENDIF

      RETURN          ! NORMAL END.
      END

	SUBROUTINE PSTAB(NUM,TABLE)
C#######################################################################
C  NAME OF ROUTINE
C     PSTAB	    ( PSeudo color TABle)
C
C  PURPOSE
C      PSTAB places into the array TABLE the IDISPLAY pseudocolor table
C      that corresponds to the number NUM.
C      
C  ENVIRONMENT
C      UNIX or  VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    4-14-94   Grouped DATA statements after all other declarations per ANSI77.
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETER
C      NUM	     - THE NUMBER OF THE PSEUDOCOLOR TABLE DESIRED.
C  OUTPUT PARAMETER
C      TABLE ARRAY   - THE PSEUDOCOLOR TABLE ITSELF
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

        PARAMETER (MAXFIL=5)	! MAX. NUMBER OF INPUT/OUTPUT FILES OR BANDS
	INTEGER*2 TABLE(MAXFIL,256)
	INTEGER*2 PS(256,3,7)
	INTEGER*2 PS1(256,3)
	INTEGER*2 PS2(256,3)
	INTEGER*2 PS3(256,3)
	INTEGER*2 PS4(256,3)
	INTEGER*2 PS5(256,3)
	INTEGER*2 PS6(256,3)
	INTEGER*2 PS7(256,3)
	EQUIVALENCE (PS(1,1,1),PS1),(PS(1,1,2),PS2),(PS(1,1,3),PS3),
     +		    (PS(1,1,4),PS4),(PS(1,1,5),PS5),(PS(1,1,6),PS6),
     +		    (PS(1,1,7),PS7)

      DATA PS1       /128*0,32*84,96*255,				    !R
     +		     32*0,32*128,64*255,32*200,32*255,32*128,32*0,	    !G
     +		     96*255,160*0/					    !B
      DATA PS2	     /16*170,16*115,16*125,16*0,16*50,16*30,64*0,	    !R
     +		     16*200,80*255,
     +		     32*0,16*70,16*50,16*110,16*150,16*200,16*185,	    !G
     +		     16*225,48*255,16*215,16*160,16*100,16*0,
     +		     16*255,16*210,16*240,16*255,16*224,32*200,16*120,	    !B
     +		     16*100,16*0,16*150,80*0/
      DATA PS3      /104*0,8*90,8*130,16*220,48*255,8*200,8*220,            !R
     +		     8*255,8*230,8*245,32*255,
     +		     32*0,8*70,8*100,8*130,8*150,8*170,8*190,8*210,	    !G
     +		     8*220,8*200,8*180,16*170,8*200,8*220,8*240,8*255,
     +		     8*140,8*120,8*80,8*60,40*0,8*50,8*0,8*255,
     +		     8*0,8*160,8*200,8*255,8*230,8*200,8*170,8*150,	    !B
     +		     8*130,8*100,8*80,8*60,64*0,24*80,8*60,16*0,8*100,
     +		     8*120,8*150,8*180,16*255/
      DATA PS4      /112*0,16*130,80*255,16*230,32*255,			    !R
     +		     32*0,16*100,16*130,16*170,16*210,16*200,16*170,	    !G
     +		     16*220,16*255,16*140,16*80,64*0,
     +		     16*160,16*255,16*200,16*170,16*130,16*80,64*0,	    !B
     +		     32*80,16*0,16*100,16*150,16*255/
      DATA PS5      /128*0,128*255,					    !R
     +		     32*0,32*130,32*170,32*200,32*255,32*80,64*0,	    !G
     +		     32*255,32*170,32*130,64*0,32*80,32*0,32*150/	    !B
      DATA PS6      /126*0,130*255,					    !R
     +		     42*0,42*170,42*200,42*255,88*0,			    !G
     +		     42*255,42*130,126*0,46*150/			    !B
      DATA PS7      /128*0,128*255,					    !R
     +		     64*0,64*190,64*255,64*0,				    !G
     +		     64*255,64*100,128*0/				    !B
C
	DO I=1,256
	    TABLE(1,I) = PS(I,1,NUM)
	    TABLE(2,I) = PS(I,2,NUM)
	    TABLE(3,I) = PS(I,3,NUM)
	    TABLE(4,I) = 0
	    TABLE(5,I) = 0
	END DO
	RETURN
	END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vlookuploop.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

#if 0
   Fortran code was commented out, left as documentation, and recoded in
   C for better performance.

c      SUBROUTINE LOOKUPLOOP (IBUF, OBUF, TABLE, NS)
C#######################################################################
C  NAME OF ROUTINE
C     LOOKUPLOOP    ( table LOOKUP LOOP)
C
C  PURPOSE
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     7-83  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED TO FORTRAN
C                FROM IBM ASSEMBLER.
C    10-84  SP   CHANGED TO USE BYTE ARRAYS AND IZEXT.
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETERS
C      IBUF ARRAY    - DATA NUMBERS FOR THE LINE OF THE INPUT IMAGE.
C                      ( IBUF(I) FOR I = 1 TO NS. )
C      TABLE ARRAY   - LOOKUP TABLE. ( TABLE(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).
C      NS            - NUMBER OF SAMPLES (DATA NUMBERS) IN THE LINE.
C  OUTPUT PARAMETERS
C      OBUF ARRAY    - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE.
C                      ( OBUF(I) FOR I = 1 TO NS. )
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cc      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
c
c      BYTE     IBUF(NS),   OBUF(NS),  TABLE(256)
c
cC=================START OF EXECUTABLE CODE===============================     
c
c      DO  I = 1, NS
c
c         OBUF(I) = TABLE(BYTE2INT( IBUF(I) ) + 1) ! CONVERT IBUF(I) TO INTEGER.
c 
c      END DO
c
c      RETURN
c      END
#endif

void FTN_NAME(lookuploop)(ibuf, obuf, table, ns)
unsigned char *ibuf;     /* input buffer   */
unsigned char *obuf;     /* output buffer   */
unsigned char table[256];/* lookup table   */
int *ns;                 /* number of samples*/
{
  unsigned char *end, *p;

   p   = obuf;
   end = (unsigned int)(*ns) + ibuf;       
   for (; ibuf < end; )
     *p++ = table[ *ibuf++ ];
}

#if 0
   Fortran code was commented out, left as documentation, and recoded in
   C for better performance.


      SUBROUTINE LOOKUPLOOP3 (IBUF, NS, OBUF, TABLE, OBUF2, TABLE2, 
     .                        OBUF3, TABLE3 )
C#######################################################################
C  NAME OF ROUTINE
C     LOOKUPLOOP3    ( table LOOKUP LOOP 3 outputs) 
C
C  PURPOSE
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
C  PREPARED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     OCT 1984
C
C  ENVIRONMENT
C      UNIX or  VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETERS
C      IBUF ARRAY    - DATA NUMBERS FOR THE LINE OF THE INPUT IMAGE.
C                      ( IBUF(I) FOR I = 1 TO NS. )
C      TABLE ARRAY   - LOOKUP TABLE. ( TABLE(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 1.
C      TABLE2 ARRAY  - LOOKUP TABLE. ( TABLE2(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 2.
C      TABLE3 ARRAY  - LOOKUP TABLE. ( TABLE3(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 3.
C      NS            - NUMBER OF SAMPLES (DATA NUMBERS) IN THE LINE.
C  OUTPUT PARAMETERS
C      OBUF ARRAY    - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 1.
C                      ( OBUF(I) FOR I = 1 TO NS. )
C      OBUF2 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 2.
C                      ( OBUF2(I) FOR I = 1 TO NS. )
C      OBUF3 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 3.
C                      ( OBUF3(I) FOR I = 1 TO NS. )
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
c
c      BYTE  IBUF(NS), OBUF(NS), TABLE(256), OBUF2(NS), TABLE2(256),
c     .           OBUF3(NS), TABLE3(256)
c
cC=================START OF EXECUTABLE CODE===============================     
c
c      DO  I = 1, NS
c
c         J = BYTE2INT( IBUF(I) ) + 1        ! CONVERT IBUF(I) TO INTEGER
c         OBUF(I) =  TABLE(J)
c         OBUF2(I) = TABLE2(J)
c         OBUF3(I) = TABLE3(J)
c 
c      END DO
c
c      RETURN
c      END
#endif

void FTN_NAME(lookuploop3)(ibuf, ns, obuf, table, obuf2, table2, obuf3, table3)
unsigned char *ibuf;     		/* input buffer   */
unsigned char *obuf, *obuf2, *obuf3;    /* output buffers   */
unsigned char table[256], table2[256], table3[256];  /* lookup tables   */
int *ns;                 		/* number of samples*/
{
  unsigned char *end, *p, *p2, *p3;

   p   = obuf;
   p2  = obuf2;
   p3  = obuf3;
   end = (unsigned int)(*ns) + ibuf;       
   for (; ibuf < end; ) {
     *p++ = table[ *ibuf ];
     *p2++ = table2[ *ibuf ];
     *p3++ = table3[ *ibuf++ ];
   }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vlookup.imake
#define  PROGRAM   vlookup

#define MODULE_LIST vlookup.f vlookuploop.c

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create vlookup.pdf

process help=*
!  FILE NAMES      INPUT AND OUTPUT FILE NAMES ARE OPTIONAL
!
PARM INP     TYPE=STRING   COUNT=1:5
PARM OUT     TYPE=STRING   COUNT=(0:5)   DEFAULT=--
PARM LUTFILE TYPE=STRING   COUNT=(0:1)   DEFAULT=--
!
PARM SIZE    TYPE=INTEGER  COUNT=(0,4)   DEFAULT=--
PARM SL      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--
PARM SS      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--
PARM NL      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--
PARM NS      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--
PARM SB      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--
PARM NB      TYPE=INTEGER  COUNT=(0:1)   DEFAULT=-- valid=(0:5)
!
PARM TABLE   TYPE=INTEGER  COUNT=(0:1)	 DEFAULT=--	    VALID=(1:7)
PARM COLUMN  TYPE=INTEGER  COUNT=(1:5)   DEFAULT=(1,2,3,4,5)  VALID=(1:5)
!# parm inp(2-5) hints=default
!# parm out(2-5) hints=noconnection
!
END-PROC
.TITLE
VLOOKUP
.HELP
 PURPOSE:

 VLOOKUP generates output images from input images using data number
 mappings defined in a multi-channel lookup table.  The user can specify 
 the table by giving its location (if it is contained in a file) or by
 naming a standard pseudocolor transformation. 

 This program performs the same function as an older program, LOOKUP; 
 the difference is that VLOOKUP uses IBIS format table files, as does
 VIDS, whereas LOOKUP uses the special format files created by IDX. 
 Because the IBIS files can be edited and viewed by other programs,
 the CHANGE and LIST functions of LOOKUP are not present in VLOOKUP.
 THE PROGRAM TEMPORARILY (UNTIL VIDS IS FIXED TO GENERATE IBIS2 FORMAT
 TABLE FILES) CANNOT HANDLE LOOKUP TABLE FILES UNLESS THEY ARE GENERATED ON
 THE VAX.
 THE PROGRAM HAS BEEN FIXED SO THAT THE TABLE PARAMETER FUNCTIONS CORRECTLY.

 EXECUTION:

 In the table format used by VLOOKUP and VIDS, a lookup table is made up
 of several independent channels, each of which is a "column" in the IBIS
 tabular format.  Most tables created by VIDS will have one (monochrome) or
 three (color or pseudocolor) channels.  Each channel defines a mapping or
 discrete transfer function to be applied to an input image. Some of these
 channels may be undefined depending on the application. 

 VLOOKUP also supports multi-band images in both input or output.  However,
 multi-band and single-band images may not be mixed, and only a single
 multi-band image (each) may be specified for input and output.  I.e., the
 user may supply 5 input images either as 5 separate files, or as one file
 with 5 bands -- NOT as one file with 3 bands and two files with one band
 (or one with 3 bands and one with 2.)

 The current limit on input and output images (or bands) is five.  This
 can easily be changed by revising one PARAMETER in the program and
 recompiling.

 All input images must have the same size.  All output images will have
 the size specified for the SIZE field, or if defaulted, will have the same
 size as the first input image.

 In the most straightforward use of VLOOKUP, the number of input images
 is equal to the number of output images and the default channel
 assignments are used.  In this case, the first channel in the table
 is used to map the first input image to the first output image, the
 second channel in the table is used to map the second input image to 
 the second output image, and so on.

 The user may select to use different channels (than the default) with
 the images by using the COLUMN parameter.  The user may also specify
 fewer input images than output images.  In this case, the last input
 image is mapped through the remaining channels to create the appropriate
 number of output images.  Thus, one input image could be mapped through
 three independent channels (transfer functions) to three output images.
 This feature allows simple production of Pseudo Color pictures.

 When the user specifies a lookup table in a file, VLOOKUP requires that
 the file be in the VIDS lookup table format, which is an IBIS tabular file
 made up of a number of columns with 256 entries per column, stored in REAL*4
 format.  See the VIDS documentation for details on the file format.
.PAGE
 TAE COMMAND LINE FORMAT

 The following command line shows the normal format:

 VLOOKUP INP=(a...) OUT=(b...) LUTFILE=c  [optional parameters]

  Here (a...) represents a list of one to five input image file names,
  (b...) is optional and represents a list of one to five output image
  file names, and c is an optional lookup table file (if omitted, then TABLE
  must be specified). 

.PAGE
EXAMPLES

1.    VLOOKUP INP=L1 OUT=L2 LUTFILE=T1 COLUMN=3

      In this example, channel 3 of file T1 is used to map
      image file L1 to image file L2.  

2.    VLOOKUP IN (RED,GREEN,BLUE) TABLE=3

      In this example, the standard pseudocolor transformation table 3 is
      used to map image file IN to files RED, GREEN, and BLUE.  (The color
      assignments must be in this order to get results corresponding to what
      is seen in IDX.)
.PAGE

 RESTRICTIONS
 1. The input and output images MUST BE BYTE data.
 2. Maximum number of samples is 1000000 per line.

 HISTORY:

  Aug.1989 --RGD-- original VLOOKUP, based on program LOOKUP
  Oct.1990 --LWK-- cleaned up the program, eliminating all paramters
		   except USE (renamed COLUMN) and PS (renamed TABLE);
		   also enabled multispectral file input.
  Feb.1993 --SP--- Made portable for UNIX.  Changed max samples to 80000.
                   The program has been fixed so that the TABLE parameter 
                   functions correctly.  Added message for case of a multiband
                   input and multiple outputs: "Bands beyond the first one are
                   ignored."  Used IBIS2 calls to access LUTFILE.

  Jan.2002 --FS_DLR--- Changed max samples to 1000000.

 COGNIZANT PROGRAMMER:  Steven Pohorsky

.LEVEL1
.VARIABLE INP
Input image file names
.VARIABLE OUT
Output image file names
.VARIABLE SIZE
Standard Vicar size field
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE SB
Starting band
.VARIABLE NB
Number of bands
.vari LUTFILE
.VARIABLE TABLE
Number of the pseudocolor
table in lieu of LUTFILE
.VARIABLE COLUMN
Nth value is the column for
the nth output image.
.LEVEL2
.VARIABLE INP
 This specifies one to five input image files.

.VARIABLE OUT
 This specifies zero to five output image files.

.VARIABLE SIZE
 The standard SIZE parameter is:  (SL,SS,NL,NS)
 You can enter SL, SS, NL, and NS together as SIZE, OR enter the SL, SS, NL,
 and NS parameters separately.

 By default, the entire input image is used if these parameters are not
 entered.

.VARIABLE SL
 See Help SIZE

.VARIABLE SS
 See Help SIZE

.VARIABLE NL
 See Help SIZE

.VARIABLE NS
 See Help SIZE

.VARIABLE SB
 Starting band to use in a multi-band input image.

.VARIABLE NB
 Number of bands for a multi-band output image.

.VARIABLE LUTFILE
 This specifies the optional lookup table file.  If not entered, then the
 lookup table must be specified using the TABLE parameter.

.VARIABLE TABLE
 This specifies that the lookup table will be the standard table of the
 number specified.  As a historical note, the tables in this program are
 the pseudocolor tables of program IDX (previously, IDISPLAY).

.VARIABLE COLUMN
 This specifies which column of the table corresponds to which output image.
 The nth value entered is the column for the nth output image.  If both
 COLUMN and TABLE are used, then COLUMN changes what part of the standard
 table is used for each output accordingly.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstvlookup.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "none"

!
!  THIS IS A TEST OF PROGRAM VLOOKUP

! Generate the lookup tables with standard IBIS calls

!
!  a one-column lut:
ibis-gen lut1 nc=1 nr=256
mf lut1 fun=("c1=index-1","c1=(c1-30)*255/70","c1=max(min(c1,255),0)")

! replaces:
!jstretch-linear 30 100 red
!jsave-stretch lut1 red

!
!  a 3-column lut -- standard for (pseudo-)color
ibis-gen lut2 nc=3 nr=256
mf lut2 fun=("c1=index-1", +
   "c2 = (c1-20)*255/100","c2=max(min(c2,255),0)",+
    "c3 = (c1-10)*255/150","c3=max(min(c3,255),0)")

! replaces:
!jstretch-linear 20 120 green
!jstretch-linear 10 150 blue
!jsave-stretch lut2 (red,green,blue)

!
!  a 4-column lut
ibis-gen lut3 nc=4 nr=256 
mf lut3 fun=("c1=index-1","c2=c1","c3=c1", +
    "c4 = (c1-5)*255/85","c4=max(min(c4,255),0)")

! replaces:
!jdef-plane x 4
!jstretch-linear 5 90 x
!jsave-stretch lut3 (red,green,blue,x)
!

gen a nl=5 ns=10 ival=10 sinc=5 linc=15
list a
copy a b
copy a c
copy a d
!
! and a multi-band one:
gen f nl=5 ns=10 nb=3 ival=5 sinc=5 linc=20 binc=10
!
!  simplest case:  1 input, 1 output:
vlookup a a1 lut1
list a1
!
!   4 input images and 4 output images:
vlookup inp=(a,b,c,d) out=(a1,b1,c1,d1) lutfil=lut3
list a1
list b1
list c1
list d1
!
!  test the 'size' and 'column' parameters:
vlookup inp=(a,b,c,d) out=(a1,b1,c1,d1) lutfil=lut3 +
       size=(1,1,3,10)    column=(2,1,3,4)  
list a1
list b1
list c1
list d1
!
!  test the special "pseudo-color" case:  1 input, 3 outputs:
vlookup a (a1,b1,c1) lut3
list a1
list b1
list c1
!
!  test 'nb' parameter: same case as above but one multi-band file:
vlookup a a1 lut3 nb=3
list a1
!
!  test multi-band input:  bands beyond band one are ignored
vlookup f (a1,b1,c1) lut3   
list a1
list b1
list c1

! test table
!
vlookup a a1 table=6 
list a1
!    new tests
vlookup a a1 table=6 column=3
list a1
!
!  test multi-band input:  
vlookup f g lut3  
list g
end-proc
$!-----------------------------------------------------------------------------
$ create timevlookup.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM VLOOKUP
!
!
! Generate the lookup tables with standard IBIS calls

!
!  a one-column lut:
ibis-gen lut1 nc=1 nr=256
mf lut1 fun=("c1=index-1","c1=(c1-30)*255/70","c1=max(min(c1,255),0)")

! replaces:
!jstretch-linear 30 100 red
!jsave-stretch lut1 red

!
!  a 3-column lut -- standard for (pseudo-)color
ibis-gen lut2 nc=3 nr=256
mf lut2 fun=("c1=index-1", +
   "c2 = (c1-20)*255/100","c2=max(min(c2,255),0)",+
    "c3 = (c1-20)*255/100","c3=max(min(c3,255),0)")

! replaces:
!jstretch-linear 20 120 green
!jstretch-linear 10 150 blue
!jsave-stretch lut2 (red,green,blue)

!  make a big image
gen a nl=2000 ns=2000 ival=10 sinc=5 linc=15
!
!  simplest case:  1 input, 1 output:
vlookup a a1 lut1
usage
!
!  simplest case:  1 input, 1 output: without IBIS2
vlookup a a1 table=2
usage
!
!  test the special "pseudo-color" case:  1 input, 3 outputs:
vlookup a (a1,b1,c1) lut2
usage
!
end-proc
$ Return
$!#############################################################################
