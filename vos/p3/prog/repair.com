$!****************************************************************************
$!
$! Build proc for MIPL module repair
$! VPACK Version 1.8, Wednesday, April 09, 2003, 16:44:38
$!
$! Execute by entering:		$ @repair
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
$ write sys$output "*** module repair ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to repair.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("repair.imake") .nes. ""
$   then
$      vimake repair
$      purge repair.bld
$   else
$      if F$SEARCH("repair.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake repair
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @repair.bld "STD"
$   else
$      @repair.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create repair.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack repair.com -
	-s repair.f -
	-p repair.pdf -
	-i repair.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create repair.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	13 OCT 89 ...REA... Added BSQ, BIL capability
C	 1 MAY 91 ...REA... Convert to UNIX/VICAR
C	17 JAN 03 ...REA... MODULO keyword added
C        9 APR 03 ...REA... Fix bug when SL<>1, and search area defaulted
C        9 APR 03 ...REA... Fix bug in rounding, when  input is REAL
C
	EXTERNAL SAR,RPR,SAR_MOD,RPR_MOD
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),MODVAL(2)
	LOGICAL XVPTST,QMV,QZOK
	CHARACTER*3 ORG
	CHARACTER*8 FMT
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	CALL XVMESSAGE('REPAIR Version April 9, 2003',' ')
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBI)
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,'ORG',ORG,' ')
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
	IF (ORG .EQ. 'BIP') THEN
      	    CALL XVMESSAGE(' REPAIR cannot process BIP files',' ')
	    CALL ABEND
	ENDIF
	IF (NB.EQ.1) THEN
	    ORG = 'BSQ'
	ELSE
	    ORG = 'BIL'
	END IF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE','U_ORG',ORG,' ')
C								get parameters
	QZOK = XVPTST('ZOK')
	QMV = XVPTST('MV')
	CALL XVPARM('MODULO',MODVAL,ICNT,IDEF,2)
	MOD1 = MODVAL(1)
	MOD2 = MODVAL(2)
	CALL XVPARM('CORR',CLEVEL,ICNT,IDEF,0)
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,0)
	IF (IDEF.NE.1) QMV=.TRUE.
	CALL XVPARM('VARIANCE',VAR,ICNT,IDEF,0)
	IF (IDEF.NE.1) QMV=.TRUE.
C			    process the 'area', 'badline' & 'lineset' parameters
	CALL PRCESS(ISL,ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA)
C
	II = 4*NS*NB
	JJ = 3*II
	IF (XVPTST('ALL')) THEN
	    IF (MOD2 .GT. 1) THEN
		CALL STACKA(6,SAR_MOD,2,II,JJ,MOD1,MOD2)
	    ELSE
		CALL STACKA(4,SAR,2,II,JJ)
	    END IF
	ELSE 
C								   report limits
	    WRITE (PRT,100) CLEVEL
  100	    FORMAT(' Correlation tolerance =',F6.3)
	    CALL XVMESSAGE(PRT,' ')
	    IF (QMV) THEN
		WRITE (PRT,200) XMEAN
  200		FORMAT(' Mean tolerance =',F10.2)
		CALL XVMESSAGE(PRT,' ')
		WRITE (PRT,300) VAR
  300		FORMAT(' Variance tolerance =',F11.2)
		CALL XVMESSAGE(PRT,' ')
	    END IF
	    IF (MOD2 .GT. 1) THEN
		CALL STACKA(6,RPR_MOD,2,II,JJ,MOD1,MOD2)
	    ELSE
		CALL STACKA(4,RPR,2,II,JJ)
	    END IF
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR(OBUF,II,BUF,JJ)
C
C	This routine replaces all areas specified, without any statistical
C	testing. Its function is the same as the old VICAR program SAR.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
		    END DO
		    LINE = LINE+1
		END DO
		ISLGOOD = ISLBAD+NLBUF(I)
		IF (ISLGOOD.LE.NL) THEN
C							Get the next good line
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,2),ISTAT,'LINE',
     +			   ISLGOOD,'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
		    END DO
		    NLBAD = NLBUF(I)
C							Fix all lines in set
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL AVE(BUF(1,1,1),BUF(1,1,2),OBUF,NS*NB,
     +				     X,ROUND)
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			END DO
		    ELSE
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    DO II=1,NB
				CALL XVREAD(INUNIT,OBUF(1,II),ISTAT,
     +					    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
				CALL AVE(BUF(ISSBUF(I),II,1),
     +				    BUF(ISSBUF(I),II,2),
     +				    OBUF(ISSBUF(I),II),NSBUF(I),X,ROUND)
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+1
			END DO
		    END IF
		ELSE
C						Bad lines go to the bottom of
C						the image, extend last good line
		    NLBAD = NLBUF(I)
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			END DO
		    ELSE
			DO J=1,NLBAD
			    DO II=1,NB
				CALL XVREAD(INUNIT,OBUF(1,II),ISTAT,
     +					    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
				CALL MVE(7,NSBUF(I),BUF(ISSBUF(I),II,1),
     +					 OBUF(ISSBUF(I),II),1,1)
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+1
			END DO
		    END IF
		END IF
		LINE = ISLGOOD
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    DO II=1,NB
		CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,'BAND',II,' ')
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    END DO
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR(OBUF,II,BUF,JJ)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBAD(16)
	LOGICAL DIFFER,QBAD
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
C
	LAST = 1		! LAST is the index of last good line
	LINX = 2		! LINX is the index to the test line
	NEXT = 3		! NEXT is the index to the next line
	NBAD = 0
	LASTGOOD = 0
	IAREA = 0
	IELTEST = -1
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,LINX),ISTAT,'LINE',ISL,
     +		        'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'LINE',ISL+1,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	CALL MVE(7,NS*NB,BUF(1,1,NEXT),BUF(1,1,LAST),1,1)
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    IF (LINE .NE. IEL) THEN
		DO II=1,NB
		    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'SAMP',ISS,
     +				'NSAMPS',NS,'BAND',II,'LINE',LINE+1,' ')
		END DO
	    ELSE
		CALL MVE(7,NS*NB,BUF(1,1,LAST),BUF(1,1,NEXT),1,1)
	    END IF
C						Get the next area to be tested
	    IF (LINE.GT.IELTEST) THEN
		IAREA = IAREA+1
		DO WHILE (NLBUF(IAREA).LE.0 .AND. IAREA.LT.NAREA)
		    IAREA = IAREA+1
		END DO
		IF (IAREA.LE.NAREA) THEN
		    ISLTEST = ISLBUF(IAREA)
		    ISSTEST = ISSBUF(IAREA)
		    IELTEST = ISLTEST + NLBUF(IAREA) -1
		    NSTEST = NSBUF(IAREA)
		ELSE
		    ISLTEST = 999999
		    IELTEST = 999999
		END IF
	    END IF
C
	    IF (LINE.GE.ISLTEST) THEN
		QBAD  = DIFFER(BUF(ISSTEST,1,LAST),BUF(ISSTEST,1,LINX),
     +			       BUF(ISSTEST,1,NEXT),NSTEST,NB)
	    ELSE
		QBAD = .FALSE.
	    END IF

C
	    IF (QBAD) THEN
		IF (NBAD .EQ. 0) CALL XVMESSAGE(
     +			 ' The following lines were repaired:',' ')
		NBAD = NBAD + 1
		IF (NBAD .GT. 16) THEN
		    WRITE (PRT,100) (IBAD(I),I=1,16)
  100		    FORMAT(16I5)
		    CALL XVMESSAGE(PRT,' ')
		    NBAD = 1
		END IF
		IBAD(NBAD) = LINE
		N = LINX
		LINX = NEXT
		NEXT = N
	    ELSE
C						If good, write out line and any
C						repaired lines that are needed.
C						Update pointers.
		IF (LASTGOOD.NE.LINE-1) THEN
		    NLBAD = LINE-LASTGOOD-1
		    DO J=1,NLBAD
			X = FLOAT(J)/FLOAT(NLBAD+1)
			CALL AVE(BUF(1,1,LAST),BUF(1,1,LINX),OBUF,NS*NB,
     +				 X,ROUND)
			DO II=1,NB
			    CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					'NSAMPS',NS,' ')
			END DO
		    END DO
		END IF
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LINX),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
		LASTGOOD = LINE
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = IEL-LASTGOOD
	    DO J=1,N
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LAST),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
	    END DO
	END IF
	WRITE (PRT,100) (IBAD(I),I=1,NBAD)
	CALL XVMESSAGE(PRT,' ')
	IF (NBAD .EQ. 0) CALL XVMESSAGE(' No bad lines found',' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR_MOD(OBUF,II,BUF,JJ,MOD1,MOD2)
C
C	This routine replaces all lines where MOD(LINE,MOD2) = MOD1 in the
C	areas specified, without any statistical testing. Its function is 
C	the same as the old VICAR program SAR.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
		    END DO
		    LINE = LINE+1
		END DO
C							Loop through all lines 
C							of this set
		IELBAD = ISLBAD + NLBUF(I) - 1
		DO WHILE (LINE .LE. IELBAD)
		    IF (MOD(LINE,MOD2) .NE. MOD1) THEN
			DO II=1,NB
			    CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			    CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
			END DO
			LINE = LINE + 1
		    ELSE
C							Bad line encountered
			IF (LINE .LT. IEL) THEN
C						    Read the following good line
			    DO II=1,NB
				CALL XVREAD(INUNIT,BUF(1,II,2),ISTAT,
     +					    'LINE',LINE+1,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
			    END DO
C								Fix the bad line
			    IF (NSBUF(I).EQ.NS) THEN
				CALL AVE(BUF(1,1,1),BUF(1,1,2),OBUF,
     +					 NS*NB,0.5,ROUND)
				DO II=1,NB
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    ELSE
				DO II=1,NB
				    CALL XVREAD(INUNIT,OBUF(1,II),
     +					   ISTAT,'LINE',LINE,'SAMP',ISS,
     +					   'NSAMPS',NS,'BAND',II,' ')
				    CALL AVE(BUF(ISSBUF(I),II,1),
     +					     BUF(ISSBUF(I),II,2),
     +					     OBUF(ISSBUF(I),II),
     +					     NSBUF(I),0.5,ROUND)
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    END IF
C							Write out the good line
C							that follows
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,BUF(1,II,2),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+2
			ELSE
C						  Bad line is last line in image
			    IF (NSBUF(I).EQ.NS) THEN
				DO II=1,NB
				    CALL XVWRIT(IOUTUNIT,BUF(1,II,1),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    ELSE
				DO II=1,NB
				    CALL XVREAD(INUNIT,OBUF(1,II),
     +					   ISTAT,'LINE',LINE,'SAMP',ISS,
     +					   'NSAMPS',NS,'BAND',II,' ')
				    CALL MVE(7,NSBUF(I),
     +					     BUF(ISSBUF(I),II,1),
     +					     OBUF(ISSBUF(I),II),1,1)
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    END IF
			    LINE = LINE+1
			END IF
		    END IF
		END DO
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    DO II=1,NB
		CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,'BAND',II,' ')
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    END DO
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR_MOD(OBUF,II,BUF,JJ,MOD1,MOD2)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBAD(16)
	LOGICAL DIFFER,QBAD
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
C
	LAST = 1		! LAST is the index of last good line
	LINX = 2		! LINX is the index to the test line
	NEXT = 3		! NEXT is the index to the next line
	NBAD = 0
	LASTGOOD = 0
	IAREA = 0
	IELTEST = -1
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,LINX),ISTAT,'LINE',ISL,
     +		        'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'LINE',ISL+1,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	CALL MVE(7,NS*NB,BUF(1,1,NEXT),BUF(1,1,LAST),1,1)
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    IF (LINE .NE. IEL) THEN
		DO II=1,NB
		    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'SAMP',ISS,
     +				'NSAMPS',NS,'BAND',II,'LINE',LINE+1,' ')
		END DO
	    ELSE
		CALL MVE(7,NS*NB,BUF(1,1,LAST),BUF(1,1,NEXT),1,1)
	    END IF
C						Get the next area to be tested
	    IF (LINE.GT.IELTEST) THEN
		IAREA = IAREA+1
		DO WHILE (NLBUF(IAREA).LE.0 .AND. IAREA.LT.NAREA)
		    IAREA = IAREA+1
		END DO
		IF (IAREA.LE.NAREA) THEN
		    ISLTEST = ISLBUF(IAREA)
		    ISSTEST = ISSBUF(IAREA)
		    IELTEST = ISLTEST + NLBUF(IAREA) -1
		    NSTEST = NSBUF(IAREA)
		ELSE
		    ISLTEST = 999999
		    IELTEST = 999999
		END IF
	    END IF
C
	    IF (LINE.GE.ISLTEST .AND. MOD(LINE,MOD2).EQ.MOD1) THEN
		QBAD  = DIFFER(BUF(ISSTEST,1,LAST),BUF(ISSTEST,1,LINX),
     +			       BUF(ISSTEST,1,NEXT),NSTEST,NB)
	    ELSE
		QBAD = .FALSE.
	    END IF

C
	    IF (QBAD) THEN
		IF (NBAD .EQ. 0) CALL XVMESSAGE(
     +			 ' The following lines were repaired:',' ')
		NBAD = NBAD + 1
		IF (NBAD .GT. 16) THEN
		    WRITE (PRT,100) (IBAD(I),I=1,16)
  100		    FORMAT(16I5)
		    CALL XVMESSAGE(PRT,' ')
		    NBAD = 1
		END IF
		IBAD(NBAD) = LINE
		N = LINX
		LINX = NEXT
		NEXT = N
	    ELSE
C						If good, write out line and any
C						repaired lines that are needed.
C						Update pointers.
		IF (LASTGOOD.NE.LINE-1) THEN
		    NLBAD = LINE-LASTGOOD-1
		    DO J=1,NLBAD
			X = FLOAT(J)/FLOAT(NLBAD+1)
			CALL AVE(BUF(1,1,LAST),BUF(1,1,LINX),OBUF,NS*NB,
     +				 X,ROUND)
			DO II=1,NB
			    CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					'NSAMPS',NS,' ')
			END DO
		    END DO
		END IF
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LINX),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
		LASTGOOD = LINE
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = IEL-LASTGOOD
	    DO J=1,N
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LAST),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
	    END DO
	END IF
	WRITE (PRT,100) (IBAD(I),I=1,NBAD)
	CALL XVMESSAGE(PRT,' ')
	IF (NBAD .EQ. 0) CALL XVMESSAGE(' No bad lines found',' ')
	RETURN
	END
C*************************************************************************
	SUBROUTINE PRCESS(ISL,ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NX)
C
C	Routine to gather and collate the  AREA, LINESET, and BADLINE 
C	parameters. This includes sorting the regions by SL, and combining
C	overlapping regions.
C
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBUF(400)
C
	NX = 0
C						   Get regions specified by AREA
	CALL XVPARM('AREA',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,4).NE.0) THEN
		CALL XVMESSAGE(
     +		' AREA parameter values must be in sets of 4',' ')
		CALL ABEND
	    ENDIF
	    DO I=1,ICNT,4
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = IBUF(I+1)-ISS+1
		NLBUF(NX) = IBUF(I+2)
		NSBUF(NX) = IBUF(I+3)
	    END DO
	END IF
C						  Get lines specified by LINESET
	CALL XVPARM('LINESET',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,2).NE.0) THEN
		CALL XVMESSAGE(
     +		' LINESET parameter values must be in sets of 2',' ')
		CALL ABEND
	    ENDIF
	    DO I=1,ICNT,2
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = IBUF(I+1)
		NSBUF(NX) = NS
	    END DO
	END IF
C						  Get lines specified by BADLINE
	CALL XVPARM('BADLINE',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    DO I=1,ICNT
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = 1
		NSBUF(NX) = NS
	    END DO
	END IF
C
	IF (NX.LE.0) THEN
	    NX = 1
	    ISLBUF(1) = ISL
	    ISSBUF(1) = ISS
	    NLBUF(1) = NL
	    NSBUF(1) = NS
	ELSE IF (NX.GT.1) THEN
C					put the areas into line ascending order
	    DO J=1,NX-1
		DO K=J+1,NX
		    IF(ISLBUF(J).GT.ISLBUF(K))  THEN
			N1 = ISLBUF(J)
			N2 = ISSBUF(J)
			N3 = NLBUF(J)
			N4 = NSBUF(J)
			ISLBUF(J) = ISLBUF(K)
			ISSBUF(J) = ISSBUF(K)
			NLBUF(J) = NLBUF(K)
			NSBUF(J) = NSBUF(K)
			ISLBUF(K) = N1
			ISSBUF(K) = N2
			NLBUF(K)= N3
			NSBUF(K)= N4
		    END IF
		END DO
	    END DO
	END IF
C						  take care of overlapping areas
	DO J=1,NX-1
	    IF (NLBUF(J).NE.0) THEN
		IEL = ISLBUF(J)+NLBUF(J)-1
		K = J+1
		DO WHILE (IEL.GE.ISLBUF(K) .AND. K.LE.NX)
		    IEL = MAX(IEL,ISLBUF(K)+NLBUF(K)-1)
		    NLBUF(J) = IEL-ISLBUF(J)+1
		    NLBUF(K) = 0
		    ISSBUF(J) = MIN(ISSBUF(J),ISSBUF(K))
		    IES1 = MAX(NSBUF(J)+ISSBUF(J),NSBUF(K)+ISSBUF(K))
		    NSBUF(J) = IES1-ISSBUF(J)
		    K = K+1
		END DO
	    END IF
	END DO
C
	RETURN
	END
C***********************************************************************
	SUBROUTINE AVE(BUF1,BUF2,OBUF,NS,X,ROUND)
C
C	This routine does a linear interpolation between BUF1 and BUF2,
C	putting the result in OBUF. X is the fractional weight to be given
C	BUF2.
C
	REAL BUF1(NS),BUF2(NS),OBUF(NS)
C
	Y = 1.0-X
	DO I=1,NS
	    OBUF(I) = Y*BUF1(I) + X*BUF2(I) + ROUND
	END DO
	RETURN
	END
C***********************************************************************
	LOGICAL FUNCTION DIFFER(BUF1,BUF2,BUF3,NS,NB)
C
C	This routine computes the mean and variance of BUF2, and the
C	correlation coefficient between BUF1 and BUF2. It then verifies
C	that the two lines in BUF1 and BUF2 are similar, within the limits
C	specified by the TESTVALS
C		BUF1	Input array, containing the last good line.
C		BUF2	Input array, containing the test line.
C		BUF3	Input array, containing the next line.
C		NS	Input, the number of pixels on each line.
C		NB	Input, the number of channels for each line
	REAL*8 SUMX,SUMY,SUMSQX,SUMSQY,XY
	REAL BUF1(NS,NB),BUF2(NS,NB),BUF3(NS,NB)
	LOGICAL QMV,QZOK
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	PIXELS = NS*NB
	SUMX = 0.0
	SUMY = 0.0
	SUMSQX = 0.0
	SUMSQY = 0.0
	XY = 0.0
C								gather raw stats
	DO II=1,NB
	    DO I=1,NS
		X = BUF2(I,II)
		Y = BUF1(I,II)
		SUMX = SUMX + X
		SUMY = SUMY + Y
		SUMSQX = SUMSQX + X*X
		SUMSQY = SUMSQY + Y*Y
		XY = XY + X*Y
	    END DO
	END DO
C
	AVGX = SUMX/PIXELS
	AVGY = SUMY/PIXELS
	VARX = SUMSQX/PIXELS - AVGX*AVGX
	VARY = SUMSQY/PIXELS - AVGY*AVGY
	IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
	    IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		RHO = 1.0
	    ELSE
		RHO = 0.0
	    END IF
	ELSE
	    RHO = (XY/PIXELS-AVGX*AVGY)/SQRT(VARX*VARY)
	END IF
C						determine whether the test line
C						is unlike the reference line
	IF (RHO.LT.CLEVEL) THEN
	    DIFFER = .TRUE.
	ELSE IF (QMV .AND. (ABS(AVGX-AVGY).GE.XMEAN 
     +			    .OR. ABS(VARX-VARY).GE.VAR)) THEN
	    DIFFER = .TRUE.
	ELSE
	    DIFFER = .FALSE.
	END IF
C
C					If the line is different than the last
C					good line, compare to the average of the
C					next line and the last good line.
	IF (DIFFER) THEN
C
	    SUMY = 0.0
	    SUMSQY = 0.0
	    XY = 0.0
	    DO II=1,NB
		DO I=1,NS
		    Y = (BUF1(I,II)+BUF3(I,II))/2.0
		    SUMY = SUMY + Y
		    SUMSQY = SUMSQY + Y*Y
		    XY = XY + BUF2(I,II)*Y
		END DO
	    END DO
C
	    AVGY = SUMY/PIXELS
	    VARY = SUMSQY/PIXELS - AVGY*AVGY
	    IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
		IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		    RHO = 1.0
		ELSE
		    RHO = 0.0
		END IF
	    ELSE
		RHO = (XY/PIXELS-AVGX*AVGY)/SQRT(VARX*VARY)
	    END IF
C						determine whether the test line
C						is unlike the possible new line
	    IF (RHO.GE.CLEVEL) THEN
		IF (.NOT.QMV .OR. (ABS(AVGX-AVGY).LE.XMEAN .AND. 
     +		    ABS(VARX-VARY).LE.VAR))    DIFFER = .FALSE.
	    END IF
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create repair.pdf
process help=*
 PARM INP         TYPE=STRING
 PARM OUT         TYPE=STRING
 PARM SIZE        TYPE=INTEGER         COUNT=0:4        DEFAULT=--
 PARM SL          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM SS          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM NL          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM NS          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM MODE	  TYPE=KEYWORD	VALID=(MV,CORR,ALL)	DEFAULT=CORR
 PARM AREA	  TYPE=INTEGER		COUNT=(0:300)	DEFAULT=--
 PARM BADLINE	  TYPE=INTEGER		COUNT=(0:100)	DEFAULT=--
 PARM LINESET	  TYPE=INTEGER		COUNT=(0:200)	DEFAULT=--
 PARM MODULO      TYPE=INTEGER          COUNT=2         DEFAULT=(0,1)
 PARM CORR	  TYPE=REAL	VALID=(-1.0:1.0)	DEFAULT=0.7
 PARM MEAN	  TYPE=REAL				DEFAULT=10.0
 PARM VARIANCE	  TYPE=REAL				DEFAULT=200.0
 PARM ZOK	  TYPE=KEYWORD	VALID=ZOK COUNT=(0:1)	DEFAULT=--
 END-PROC
.TITLE
 REPAIR --  Find and replace bad lines (or line segments) in images
.HELP
      REPAIR identifies bad lines (lines that are inconsistent with the
surrounding lines) and replaces them by interpolation of the nearest good 
lines. If the locations of the bad lines or line segments are known, the user 
may list the offending lines and use the ALL mode to repair just those lines.
If the locations of the bad lines are unknown, the program decides which lines
are bad by computing the interline correlation (and optionally the differences
in the means and the variances).
      REPAIR can now repair multichannel data in BSQ or BIL organizations. All
channels are tested in aggregate, and if the line is found to be bad, the line
is replaced in all channels. No specific channel repairs are presently 
implemented.
.PAGE
 OPERATION
      There are three modes of operation to REPAIR. The CORR and MV modes 
differ only in the tests employed to determine bad lines, but the ALL mode 
does no statistical testing, and is more closely related to the earlier VICAR 
program SAR. 
      In the ALL mode, lines or regions are specified by the user via the AREA,
LINESET, BADLINE, and/or MODULO parameters. These regions are replaced by means
of a linear interpolation, using the lines immediately above and below the 
region. Interpolation is performed in the line direction only, and the area 
outside the specified regions is unchanged.
      In the CORR and MV modes, the AREA, LINESET, BADLINE and MODULO 
parameters are used to identify regions to be examined for bad lines. If all 
three parameters are defaulted, the entire image is tested. Each line that is 
to be checked is tested by the following procedure:
.PAGE
      A. The line is compared to two reference lines: the last good line, and 
         the average of the next line and the last good line.
      B. If the correlation coefficients between the test line and both of the
         reference lines is less than the value of CORR, then the line is 
         considered bad. There is one exception to this test. If the ZOK (Zero
         OK) keyword is in effect, lines that are all zeroes are passed as
         good. The correlation coefficient is undefined for lines of constant 
         value, requiring this special case. A correlation coefficient of 0.0 
         is assigned to all other cases involving lines of constant DN.
      C. If the MV (Mean and Variance) mode is in effect, then two additional 
         tests are employed. The test line is considered bad if its mean is 
         different than the means of the reference lines by more than the value 
         of MEAN, or if its variance is different than both the reference line 
         variances by more than the value of VARIANCE.
 If a line is found to be bad, the entire line is replaced by a linear
 interpolation of the last good line and the next good line. 
.PAGE
      This algorithm is fairly sensitive to the values of the CORR, MEAN, and
 VARIANCE parameters. It may be necessary to run this program more than once
 in order to find an appropriate set of values. In general, the CORR and 
 VARIANCE values provide the best tests for random noise and hashed lines. The
 test of the means should only rarely be needed to reject a line.
      The coordinates used in the AREA parameter (SL,SS,NL,NS,...) refer to the
 input image, not the output image. This is important only if, in the size
 field, the starting line or starting sample is not one. 
      The user should also be aware that if two regions specified by AREA, 
 LINESET, or BADLINE contain the same line, the two regions will be combined 
 into one larger region that contains both original regions. This can cause
 problems, especially in the ALL mode, where more pixels may be modified than
 were intended.
      This program will run on byte, halfword, fullword, or real data. Up to
 100 regions may by given by each of the AREA, LINESET, and BADLINE parameters.
 If either MEAN or VARIANCE is not defaulted, the MV mode is automatically
 used.
.PAGE
 ORIGINAL PROGRAMMER:  John Addington
 
 CURRENT COGNIZANT PROGRAMMER: Ron Alley

 REVISION:  14 January, 2003  (MODULO option added)
.LEVEL1
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
 output image window
 (SL,SS,NL,NS)
.VARIABLE SL
 starting line
.VARIABLE SS
 starting sample
.VARIABLE NL
 number of lines
.VARIABLE NS
 number of samples
.VARIABLE MODE
CORR, MV, or ALL
.VARIABLE AREA
Sets of (SL,SS,NL,NS) to
be tested for bad lines
.VARIABLE LINESET
Sets of lines to be tested
(SL,NL,SL,NL,...)
.VARIABLE MODULO
Limit lines to those that
are N1 MODULO N2
.VARIABLE BADLINE
Lines to be tested
.VARIABLE CORR
Tolerance level for
interline correlation.
.VARIABLE MEAN
Tolerance level for
difference in means.
.VARIABLE VARIANCE
Tolerance level for
difference in variances.
.VARIABLE ZOK
Are lines of Zero DN OK?
.LEVEL2
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
 image size (SL,SS,NL,NS)
.VARIABLE SL
 The first line of the input image to be output.
.VARIABLE SS
 The first pixel of each input line to be output.
.VARIABLE NL
 The number of lines to be output.
.VARIABLE NS
 The number of pixels to be output for each line.
.VARIABLE MODE
      The program REPAIR operates in three different modes: CORR (the default),
 MV, and ALL. 
      If ALL is specified, no statistical checking is performed, and all 
 regions specified by the AREA, LINESET, BADLINE and MODULO parameters are 
 replaced by interpolating the nearest two lines.
      In the CORRelation mode, a line is considered to be a good line if the
 correlation coefficient between it and the last good line is greater than the
 threshold value specified by CORR. The line is also considered good if its
 correlation with the average of the last good line and the next line is above
 the threshold.
      If MV is specified, two tests are made, in addition to the correlation
 test. The difference in means must be less than the value specified by MEAN,
 and the difference in variances must be less than the value specified by
 VARIANCE. If a line passes all three tests, it is considered good.
.VARIABLE AREA
      The AREA, LINESET, BADLINE and MODULO parameters are used to limit the 
 portion of the image in which lines may be replaced. BADLINE is used to 
 identify individual lines to be checked. LINESET is used to specify sets of 
 lines, in the format (SL1,NL1,SL2,NL2,...), when the general region of concern
 is known, but not the exact location. The MODULO parameter is used to test
 and/or replace bad lines that may occur in a periodic fashion.  For example,
 MODULO=(4,20) could be used to test/replace every 20th line, starting with
 Line 4. Unlike the other parameters, MODULO acts as an additional restriction.
 If more than one of AREA, LINESET, and BADLINE are specified, then a line
 is tested/replaced if it satisfies any of the parameters, but if MODULO and
 another parameter is specified, then the line must meet both criteria to be
 tested/replaced. The AREA parameter allows the user to select a portion of 
 the lines to be examined. The values for AREA must be specified in sets of 
 four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      AREA, LINESET, and BADLINE accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE LINESET
      The AREA, LINESET, BADLINE and MODULO parameters are used to limit the 
 portion of the image in which lines may be replaced. BADLINE is used to 
 identify individual lines to be checked. LINESET is used to specify sets of 
 lines, in the format (SL1,NL1,SL2,NL2,...), when the general region of concern
 is known, but not the exact location. The MODULO parameter is used to test
 and/or replace bad lines that may occur in a periodic fashion.  For example,
 MODULO=(4,20) could be used to test/replace every 20th line, starting with
 Line 4. Unlike the other parameters, MODULO acts as an additional restriction.
 If more than one of AREA, LINESET, and BADLINE are specified, then a line
 is tested/replaced if it satisfies any of the parameters, but if MODULO and
 another parameter is specified, then the line must meet both criteria to be
 tested/replaced. The AREA parameter allows the user to select a portion of 
 the lines to be examined. The values for AREA must be specified in sets of 
 four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      AREA, LINESET, and BADLINE accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE BADLINE
      The AREA, LINESET, BADLINE and MODULO parameters are used to limit the 
 portion of the image in which lines may be replaced. BADLINE is used to 
 identify individual lines to be checked. LINESET is used to specify sets of 
 lines, in the format (SL1,NL1,SL2,NL2,...), when the general region of concern
 is known, but not the exact location. The MODULO parameter is used to test
 and/or replace bad lines that may occur in a periodic fashion.  For example,
 MODULO=(4,20) could be used to test/replace every 20th line, starting with
 Line 4. Unlike the other parameters, MODULO acts as an additional restriction.
 If more than one of AREA, LINESET, and BADLINE are specified, then a line
 is tested/replaced if it satisfies any of the parameters, but if MODULO and
 another parameter is specified, then the line must meet both criteria to be
 tested/replaced. The AREA parameter allows the user to select a portion of 
 the lines to be examined. The values for AREA must be specified in sets of 
 four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      AREA, LINESET, and BADLINE accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE MODULO
      The AREA, LINESET, BADLINE and MODULO parameters are used to limit the 
 portion of the image in which lines may be replaced. BADLINE is used to 
 identify individual lines to be checked. LINESET is used to specify sets of 
 lines, in the format (SL1,NL1,SL2,NL2,...), when the general region of concern
 is known, but not the exact location. The MODULO parameter is used to test
 and/or replace bad lines that may occur in a periodic fashion.  For example,
 MODULO=(4,20) could be used to test/replace every 20th line, starting with
 Line 4. Unlike the other parameters, MODULO acts as an additional restriction.
 If more than one of AREA, LINESET, and BADLINE are specified, then a line
 is tested/replaced if it satisfies any of the parameters, but if MODULO and
 another parameter is specified, then the line must meet both criteria to be
 tested/replaced. The AREA parameter allows the user to select a portion of 
 the lines to be examined. The values for AREA must be specified in sets of 
 four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      AREA, LINESET, and BADLINE accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE CORR
      The parameter CORR sets the minimum value of the correlation coefficient
 between lines for a line to be considered good. Correlation coefficients may
 range from 1.0 (two lines that match perfectly) to -1.0 (one line perfectly
 matches the complement of the other).
.VARIABLE MEAN
       The value of MEAN is the maximum allowed difference between the means
 of two lines for a line to be considered good.
.VARIABLE VARIANCE
       The value of VARIANCE is the maximum allowed difference between the 
 variances of two lines for a line to be considered good. The variance is the
 square of the standard deviation.
.VARIABLE ZOK
       The keyword ZOK is used when lines that are entirely 0 DN may be
present in the picture, and the user does not want them replaced. 
.END
$ Return
$!#############################################################################
$Imake_File:
$ create repair.imake
#define  PROGRAM   repair

#define MODULE_LIST repair.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
