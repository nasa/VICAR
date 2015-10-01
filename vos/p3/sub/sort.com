$!****************************************************************************
$!
$! Build proc for MIPL module sort
$! VPACK Version 1.8, Wednesday, September 27, 1995, 13:36:57
$!
$! Execute by entering:		$ @sort
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
$ write sys$output "*** module sort ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sort.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("sort.imake") .nes. ""
$   then
$      vimake sort
$      purge sort.bld
$   else
$      if F$SEARCH("sort.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sort
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sort.bld "STD"
$   else
$      @sort.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sort.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sort.com -
	-s sort.f -
	-i sort.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sort.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C******************************************************************************
      SUBROUTINE SORTIN(IBUF,NUM)
C
C	This routine sorts the NUM element integer*4 array, IBUF, in
C	ascending order.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      INTEGER*4 IBUF(NUM)
      LOGICAL MORE_TO_DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(IBUF(LOC1) .GT. IBUF(LOC2)) THEN
                      IHOLD = IBUF(LOC1)
                      IBUF(LOC1) = IBUF(LOC2)
                      IBUF(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C******************************************************************************
      SUBROUTINE SORTR(BUF,NUM)
C
C	This routine sorts the NUM element real*4 array, BUF, in
C	ascending order.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      REAL*4 BUF(NUM)
      LOGICAL MORE_TO_DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(BUF(LOC1) .GT. BUF(LOC2)) THEN
                      HOLD = BUF(LOC1)
                      BUF(LOC1) = BUF(LOC2)
                      BUF(LOC2) = HOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C*************************************************************
      SUBROUTINE INDSRTF(IBUF,INDEX,NUM)
C
C	This routine provides an INDexed SoRT of Fullword data. That is, a
C	NUM element buffer, IBUF, is input.  A NUM element buffer, INDEX, is
C	output, where IBUF(INDEX(I)) is the I'th element of the sorted
C	sequence of IBUF.  The IBUF array itself remains unchanged.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      INTEGER*4 IBUF(NUM),INDEX(NUM)
      LOGICAL MORE_TO_DO
C						initialize the index array
      DO I=1,NUM
          INDEX(I) = I
      END DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(IBUF(INDEX(LOC1)) .GT. IBUF(INDEX(LOC2))) THEN
                      IHOLD = INDEX(LOC1)
                      INDEX(LOC1) = INDEX(LOC2)
                      INDEX(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C*************************************************************
      SUBROUTINE INDSRTR(BUF,INDEX,NUM)
C
C	This routine provides an INDexed SoRT of Real*4 data. That is, a
C	NUM element buffer, BUF, is input.  A NUM element buffer, INDEX, is
C	output, where BUF(INDEX(I)) is the I'th element of the sorted
C	sequence of BUF.  The BUF array itself remains unchanged.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      REAL*4 BUF(NUM)
      INTEGER*4 INDEX(NUM)
      LOGICAL MORE_TO_DO
C						initialize the index array
      DO I=1,NUM
          INDEX(I) = I
      END DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(BUF(INDEX(LOC1)) .GT. BUF(INDEX(LOC2))) THEN
                      IHOLD = INDEX(LOC1)
                      INDEX(LOC1) = INDEX(LOC2)
                      INDEX(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sort.imake
#define SUBROUTINE sort

#define MODULE_LIST sort.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
