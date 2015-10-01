$!****************************************************************************
$!
$! Build proc for MIPL module ibisfil
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:45
$!
$! Execute by entering:		$ @ibisfil
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
$ write sys$output "*** module ibisfil ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ibisfil.imake") .nes. ""
$   then
$      vimake ibisfil
$      purge ibisfil.bld
$   else
$      if F$SEARCH("ibisfil.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisfil
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisfil.bld "STD"
$   else
$      @ibisfil.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisfil.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisfil.com -
	-s ibisfil.f -
	-i ibisfil.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisfil.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE RDFIL (UNIT,INST,CLEN,NCOL,NOFILE)
C
	IMPLICIT INTEGER(A-Z)
	LOGICAL NOFILE
	DIMENSION BUF(128)
C
	CALL XVUNIT (UNIT,'INP',INST,STATUS,' ')
	NOFILE = STATUS.NE.1
	IF (.NOT.NOFILE) THEN
	  CALL XVOPEN (UNIT,STATUS,'OP','UPDATE',
     +				'OPEN_ACT','SA', 'IO_ACT','SA',' ')
	  CALL XVREAD (UNIT,BUF,STATUS,' ')
	  CLEN = BUF(1)
	  CALL XVGET (UNIT,STATUS,'NL',NBLK,' ')
	  NRCOL = (CLEN+127)/128
	  NCOL = (NBLK-1)/MAX(NRCOL,1)
	ENDIF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE WRFIL (UNIT,INST,CLEN,NCOL,NOFILE)
C
	IMPLICIT INTEGER(A-Z)
	LOGICAL NOFILE
	DIMENSION BUF(128)
C
	NLO = 1+NCOL*((CLEN+127)/128)
	CALL XVUNIT (UNIT,'OUT',INST,STATUS,' ')
	NOFILE = STATUS.NE.1
	IF (.NOT.NOFILE) THEN
	  CALL XVOPEN (UNIT,STATUS,'OP','WRITE','U_NL',NLO,
     *         'U_NS',512,'O_FORMAT','BYTE','U_FORMAT','BYTE',
     *         'OPEN_ACT','SA', 'IO_ACT','SA', ' ')
	  BUF(1) = CLEN
	  DO I=2,128
	    BUF(I) = 0
	  ENDDO
	  CALL XVWRIT (UNIT,BUF,STATUS,' ')
          BUF(1)=0
          DO I=2,NLO
             CALL XVWRIT(UNIT,BUF,STATUS,' ')
          ENDDO
          CALL XVCLOSE(UNIT,STATUS,' ')
          CALL XVOPEN(UNIT,STATUS,'OP','UPDATE',
     +			'OPEN_ACT','SA', 'IO_ACT','SA',' ')
       	ENDIF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GETCOL (UNIT,ICOL,CLEN,COL)
	IMPLICIT INTEGER(A-Z)
	REAL*4 COL(*)
C
	NREC = (CLEN+127)/128
	REC = 2+(ICOL-1)*NREC
	PTR = 1
	DO I=1,NREC
	  CALL XVREAD (UNIT,COL(PTR),STATUS,'LINE',REC,' ')
	  REC = REC+1
	  PTR = PTR+128
	ENDDO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PUTCOL(UNIT,ICOL,CLEN,COL)
	IMPLICIT INTEGER(A-Z)
	REAL*4 COL(*)
C
	NREC = (CLEN+127)/128
	REC = 2+(ICOL-1)*NREC
	PTR = 1
	DO I=1,NREC
	  CALL XVWRIT (UNIT,COL(PTR),STATUS,'LINE',REC,' ')
	  REC = REC+1
	  PTR = PTR+128
	ENDDO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GETREC (UNIT,DCOL,COLS,DATA,REC,CLEN,A)
C SERIAL READ ONLY
	IMPLICIT INTEGER(A-Z)
	DIMENSION COLS(*),DATA(*),A(128,*)
C
	PTR = MOD(REC-1,128)+1
	IF (PTR.EQ.1) THEN
	  OFFSET = (REC+127)/128
	  NRCOL = (CLEN+127)/128
	  DO IX=1,DCOL
	    R = NRCOL*(COLS(IX)-1)+OFFSET+1
	    CALL XVREAD (UNIT,A(1,IX),STATUS,'LINE',R,' ')
	  ENDDO
	ENDIF
	DO I=1,DCOL
	  DATA(I) = A(PTR,I)
	ENDDO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PUTREC (UNIT,DCOL,COLS,DATA,REC,CLEN,A)
C SERIAL WRITE ONLY
	IMPLICIT INTEGER(A-Z)
	DIMENSION COLS(*),DATA(*),A(128,*)
C
	PTR = MOD(REC-1,128)+1
	DO I=1,DCOL
	  A(PTR,I) = DATA(I)
	ENDDO
	IF (PTR.EQ.128.OR.REC.EQ.CLEN) THEN
C	  IF  (PTR.EQ.128.OR.REC.NE.CLEN) THEN
	  IF  (PTR.NE.128.AND.REC.EQ.CLEN) THEN
	    PTR1 = PTR+1
	    DO IX=1,DCOL
	      DO I=PTR1,128
		A(I,IX) = 0
	      ENDDO
	    ENDDO
	  ENDIF
	  OFFSET = (REC+127)/128
	  NRCOL = (CLEN+127)/128
	  DO IX=1,DCOL
	    R = NRCOL*(COLS(IX)-1)+OFFSET+1
	    CALL XVWRIT (UNIT,A(1,IX),STATUS,'LINE',R,' ')
	  ENDDO
	ENDIF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibisfil.imake
#define SUBROUTINE ibisfil

#define MODULE_LIST ibisfil.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
