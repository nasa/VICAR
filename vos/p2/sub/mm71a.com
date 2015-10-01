$!****************************************************************************
$!
$! Build proc for MIPL module mm71a
$! VPACK Version 1.5, Thursday, October 22, 1992, 13:03:29
$!
$! Execute by entering:		$ @mm71a
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module mm71a ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("mm71a.imake") .nes. ""
$   then
$      vimake mm71a
$      purge mm71a.bld
$   else
$      if F$SEARCH("mm71a.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mm71a
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mm71a.bld "STD"
$   else
$      @mm71a.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mm71a.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mm71a.com -
	-s mm71a.f -
	-i mm71a.imake -
	-t tmm71a.f tmm71a.imake tmm71a.pdf tstmm71a.pdf -
	-o mm71a.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mm71a.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MM71A(RESTAB)
c
C     11 JUNE 80   ...JAM...    INITIAL RELEASE
C      6 OCT  90   ...CCA...    EBCDIC TO ASCII
c     22 Oct  92   ...WPL...    Ported for UNIX Conversion
c
      Integer    IJ
      REAL*4     RESTAB(840)
      REAL*4     TX1(840)
      BYTE       BNAH(8)
      INTEGER    INAH
      BYTE       BNAV(8)
      INTEGER    INAV
      BYTE       BTIE(8)
c     REAL*4     TYY(8)
c     REAL*4 TYY(8)/'2048414E'X,'20202020'X,'0000000F'X,'2056414E'X,
c    *              '20202020'X,'0000000C'X,'50454954'X,'544E494F'X/
      REAL TY1(72)/
     *  43.997, 18.646, 45.805, 29.865, 42.023, 71.206, 39.910, 75.227,
     *  42.023, 71.206, 39.910, 75.227, 42.140,204.422, 28.843,190.280,
     *  42.140,204.422, 28.843,190.280, 42.446,340.199, 21.381,307.852,
     *  42.446,340.199, 21.381,307.852, 42.056,475.275, 15.425,425.528,
     *  42.056,475.275, 15.425,425.528, 42.094,610.311, 11.164,542.024,
     *  42.094,610.311, 11.164,542.024, 42.540,745.151,  8.551,656.848,
     *  42.540,745.151,  8.551,656.848, 43.161,879.657,  9.034,766.587,
     *  43.161,879.657,  9.034,766.587, 42.538,935.245, 10.612,808.743,
     *  88.037, 16.248, 83.902, 26.477, 88.037, 16.248, 83.902, 26.477/
      REAL TY2(80)/
     *  86.908,137.437, 73.252,130.466, 86.908,137.437, 73.252,130.466,
     *  86.444,272.418, 64.905,247.283, 86.444,272.418, 64.905,247.283,
     *  86.641,407.628, 58.673,365.260, 86.641,407.628, 58.673,365.260,
     *  86.562,542.745, 53.868,482.723, 86.562,542.745, 53.868,482.723,
     *  86.619,677.199, 50.178,599.083, 86.619,677.199, 50.178,599.083,
     *  87.784,811.569, 48.086,712.498, 87.784,811.569, 48.086,712.498,
     *  88.026,934.552, 49.440,810.364, 88.026,934.552, 49.440,810.364,
     * 131.656, 15.430,121.999, 23.089,129.621, 70.160,116.329, 70.104,
     * 129.621, 70.160,116.329, 70.104,129.531,204.736,107.926,186.253,
     * 129.531,204.736,107.926,186.253,130.197,340.290,101.473,304.747/
      REAL TY3(80)/
     * 130.197,340.290,101.473,304.747,129.978,475.053, 96.413,422.914,
     * 129.978,475.053, 96.413,422.914,130.439,610.345, 92.472,540.427,
     * 130.439,610.345, 92.472,540.427,130.240,745.200, 88.526,657.033,
     * 130.240,745.200, 88.526,657.033,130.741,880.452, 86.232,769.522,
     * 130.741,880.452, 86.232,769.522,131.296,934.290, 88.268,811.985,
     * 199.551, 16.008,182.145, 19.769,199.551, 16.008,182.145, 19.769,
     * 198.128,138.361,173.621,125.341,198.128,138.361,173.621,125.341,
     * 197.749,272.204,167.004,243.060,197.749,272.204,167.004,243.060,
     * 197.754,407.245,161.838,361.541,197.754,407.245,161.838,361.541,
     * 197.686,542.745,157.593,479.734,197.686,542.745,157.593,479.734/
      REAL TY4(80)/
     * 197.911,677.050,153.751,597.367,197.911,677.050,153.751,597.367,
     * 198.320,811.623,150.223,713.059,198.320,811.623,150.223,713.059,
     * 199.053,933.685,149.393,814.174,199.053,933.685,149.393,814.174,
     * 266.010, 15.070,242.290, 16.449,264.883, 69.806,238.486, 63.689,
     * 264.883, 69.806,238.486, 63.689,265.048,205.142,231.743,181.318,
     * 265.048,205.142,231.743,181.318,264.920,339.707,226.874,299.924,
     * 264.920,339.707,226.874,299.924,264.977,475.136,222.593,418.580,
     * 264.977,475.136,222.593,418.580,264.930,610.293,218.895,537.015,
     * 264.930,610.293,218.895,537.015,265.472,745.338,215.113,654.993,
     * 265.472,745.338,215.113,654.993,265.304,880.371,211.023,770.689/
      REAL TY5(80)/
     * 265.304,880.371,211.023,770.689,265.572,935.591,210.518,816.363,
     * 333.976, 15.490,304.434, 14.135,333.976, 15.490,304.434, 14.135,
     * 333.068,137.838,297.547,120.295,333.068,137.838,297.547,120.295,
     &
     * 332.695,272.493,292.241,238.240,332.695,272.493,292.241,238.240,
     * 332.502,407.509,288.114,356.681,332.502,407.509,288.114,356.681,
     * 332.419,542.916,284.222,475.395,332.419,542.916,284.222,475.395,
     * 332.515,677.162,280.317,594.058,332.515,677.162,280.317,594.058,
     * 332.743,812.029,276.111,711.553,332.743,812.029,276.111,711.553,
     * 332.692,934.744,272.405,816.052,332.692,934.744,272.405,816.052,
     * 401.198, 15.954,366.579, 11.822,400.041, 69.918,362.876, 59.291/
      REAL TY6(80)/
     * 400.041, 69.918,362.876, 59.291,399.849,204.651,357.081,176.881,
     * 399.849,204.651,357.081,176.881,399.814,339.794,353.268,294.838,
     * 399.814,339.794,353.268,294.838,400.000,475.000,349.721,413.382,
     * 400.000,475.000,349.721,413.382,399.945,610.107,345.681,532.603,
     * 399.945,610.107,345.681,532.603,400.130,744.969,341.580,651.621,
     * 400.130,744.969,341.580,651.621,400.150,880.192,336.729,768.911,
     * 400.150,880.192,336.729,768.911,399.822,935.892,334.291,815.740,
     * 468.711, 17.086,429.221, 10.512,468.711, 17.086,429.221, 10.512,
     * 467.712,135.404,423.059,116.213,467.712,135.404,423.059,116.213,
     * 467.453,272.773,418.538,233.550,467.453,272.773,418.538,233.550/
      REAL TY7(80)/
     * 467.431,407.184,414.814,351.763,467.431,407.184,414.814,351.763,
     * 467.458,543.026,411.050,470.595,467.458,543.026,411.050,470.595,
     * 467.268,676.982,406.776,589.814,467.268,676.982,406.776,589.814,
     * 467.013,812.122,401.721,708.246,467.013,812.122,401.721,708.246,
     * 467.131,933.931,395.935,813.426,467.131,933.931,395.935,813.426,
     * 536.306, 16.826,491.864,  9.202,535.113, 69.947,488.374, 56.382,
     * 535.113, 69.947,488.374, 56.382,534.867,203.824,483.907,172.299,
     * 534.867,203.824,483.907,172.299,534.897,339.726,479.896,290.182,
     * 534.897,339.726,479.896,290.182,534.919,474.895,476.374,408.650,
     * 534.919,474.895,476.374,408.650,535.090,609.898,472.424,527.747/
      REAL TY8(80)/
     * 535.090,609.898,472.424,527.747,534.622,745.020,467.419,647.288,
     * 534.622,745.020,467.419,647.288,535.252,879.663,461.157,765.163,
     * 535.252,879.663,461.157,765.163,535.539,934.441,457.579,811.111,
     * 603.292, 18.153,553.997,  9.893,603.292, 18.153,553.997,  9.893,
     * 602.514,135.176,549.045,113.464,602.514,135.176,549.045,113.464,
     * 602.216,272.998,545.212,229.588,602.216,272.998,545.212,229.588,
     * 602.390,407.157,541.432,347.342,602.390,407.157,541.432,347.342,
     * 602.118,542.958,537.428,465.953,602.118,542.958,537.428,465.953,
     * 601.955,677.123,532.505,585.104,601.955,677.123,532.505,585.104,
     * 601.393,811.371,525.894,703.472,601.393,811.371,525.894,703.472/
      REAL TY9(80)/
     * 601.061,933.723,517.223,807.790,601.061,933.723,517.223,807.790,
     * 670.661, 18.015,616.130, 10.584,669.360, 70.015,613.587, 55.535,
     * 669.360, 70.015,613.587, 55.535,670.119,203.287,610.314,169.641,
     * 670.119,203.287,610.314,169.641,669.945,339.670,606.733,286.230,
     * 669.945,339.670,606.733,286.230,669.976,474.699,602.726,404.305,
     * 669.976,474.699,602.726,404.305,670.008,609.843,598.188,523.109,
     * 670.008,609.843,598.188,523.109,669.859,744.735,591.988,642.273,
     * 669.859,744.735,591.988,642.273,670.647,879.450,583.012,759.165,
     * 670.647,879.450,583.012,759.165,669.737,934.857,576.867,804.469,
     * 714.067, 18.933,655.207, 12.706,714.067, 18.933,655.207, 12.706/
      REAL TY10(80)/
     * 713.537,135.679,652.185,112.814,713.537,135.679,652.185,112.814,
     * 713.484,272.648,649.287,227.329,713.484,272.648,649.287,227.329,
     * 713.652,407.237,645.567,344.237,713.652,407.237,645.567,344.237,
     * 713.435,542.786,641.088,462.374,713.435,542.786,641.088,462.374,
     * 713.347,676.926,635.107,581.132,713.347,676.926,635.107,581.132,
     * 712.890,810.795,626.475,698.744,712.890,810.795,626.475,698.744,
     * 713.019,933.463,614.460,801.580,713.019,933.463,614.460,801.580,
     * 757.550, 20.134,694.283, 14.827,757.470, 69.834,692.952, 57.291,
     * 757.470, 69.834,692.952, 57.291,757.995,200.792,691.886,168.789,
     * 757.995,200.792,691.886,168.789,757.636,339.796,688.216,284.656/
      REAL TY11(40)/
     * 757.636,339.796,688.216,284.656,757.933,474.702,684.593,401.756,
     * 757.933,474.702,684.593,401.756,757.906,609.801,678.847,520.324,
     * 757.906,609.801,678.847,520.324,757.937,744.535,671.404,638.821,
     * 757.937,744.535,671.404,638.821,758.418,879.437,659.496,754.716,
     * 758.418,879.437,659.496,754.716,757.549,933.735,652.054,798.691/

      EQUIVALENCE  (TX1(1), BNAH(1))
      Equivalence  (TX1(3), INAH)
      Equivalence  (TX1(4), BNAV(1))
      EQuivalence  (TX1(6), INAV)
      Equivalence  (TX1(7), BTIE(1))

      Equivalence  (TX1(9),TY1(1)),(TX1(81),TY2(1)),
     &(TX1(161),TY3(1)),
     &(TX1(241),TY4(1)),(TX1(321),TY5(1)),(TX1(401),TY6(1)),
     &(TX1(481),TY7(1)),(TX1(561),TY8(1)),(TX1(641),TY9(1)),
     &(TX1(721),TY10(1)),(TX1(801),TY11(1))

      Call MVCL('NAH     ',BNAH, 8)
      INAH = 15
      Call MVCL('NAV     ',BNAV, 8)
      INAV = 12
      Call MVCL('TIEPOINT',BTIE, 8)

      Do  20  IJ = 1, 840
        RESTAB(IJ) = TX1(IJ)
20    Continue

      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mm71a.imake
/* Imake file for VICAR subroutine MM71A  */

#define SUBROUTINE  mm71a

#define MODULE_LIST  mm71a.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tmm71a.f
      INCLUDE 'VICMAIN_FOR'
c
C  PROGRAM TMM71A
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE MM71A.
C  MM71A PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL MARINER 9 DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  MM71A RETURNS DATA FOR THE "A" CAMERA.

      SUBROUTINE MAIN44
      REAL*4  BUF(840)

      CALL MM71A(BUF)

      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c     CALL PRNT(0,32,BUF,'.')
      Call Prnt(99, 8, BUF(1), ' FIRST 2 BUF = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' NEXT  2 BUF = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' NEXT  2 BUF = .')
c
      CALL QPRINT(' GEOMA PARAMETERS:',18)
      CALL PRNT(7,80,BUF(81),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(161),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(241),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(321),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(401),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(481),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(561),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(641),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(721),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,40,BUF(801),'.')

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tmm71a.imake
/* IMAKE file for Test of VICAR subroutine  MM71A  */

#define PROGRAM  tmm71a

#define MODULE_LIST tmm71a.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/*  #define   LIB_LOCAL  */  /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tmm71a.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstmm71a.pdf
Procedure
Refgbl $Echo
Body
Let _onfail="Continue"
Let $echo="yes"
! THIS IS A TEST OF SUBROUTINE MM71A.
! MM71A PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
! NOMINAL MARINER 9 DISTORTION CORRECTION DATA IN GEOMA FORMAT.
! MM71A RETURNS DATA FOR THE "A" CAMERA.  THE DATA IS RETURNED
! IN AN 840 ELEMENT ARRAY.  THE VALUES ARE INITIALIZED IN THE
! SUBROUTINE.
TMM71A
Let $echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create mm71a.hlp
1  MM71A

2  Calling Sequence:  

     CALL MM71A(BUF)

2  PURPOSE

       To provide the calling program a buffer containing nominal Mariner 9
       distortion correction data in GEOMA format.


2  OPERATION

       The data in the array is similar to the format as the parameter 
       dataset which can be input to GEOMA.  The difference between the
       two formats is in the first word.  This subroutine begins with NAH
       and the first word in the GEOMA dataset is the number of words (840)
       following the first word.


2  ARGUMENTS


    CALL MM71A(BUF)

      where:  

       BUF is an 840 word array of GEOMA parameters returned.

       MM71A should be called to get data for the "A: camera and MM71b for
       the "B" camera.

2  HISTORY

       Original Programmer: Unknown
       Current Cognizant Programmer: CHARLES AVIS
       Source Language: Fortran
       Revision:    Jul-28  1980
		    Oct-06  1990  ...CCA... EBCDIC TO ASCII  
                    Oct-22  1992  ...WPL... Ported for UNIX Conversion
$ Return
$!#############################################################################
