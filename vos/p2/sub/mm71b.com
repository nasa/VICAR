$!****************************************************************************
$!
$! Build proc for MIPL module mm71b
$! VPACK Version 1.5, Friday, October 23, 1992, 13:46:00
$!
$! Execute by entering:		$ @mm71b
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
$ write sys$output "*** module mm71b ***"
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
$   if F$SEARCH("mm71b.imake") .nes. ""
$   then
$      vimake mm71b
$      purge mm71b.bld
$   else
$      if F$SEARCH("mm71b.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mm71b
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mm71b.bld "STD"
$   else
$      @mm71b.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mm71b.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mm71b.com -
	-s mm71b.f -
	-i mm71b.imake -
	-t tmm71b.f tmm71b.imake tmm71b.pdf tstmm71b.pdf -
	-o mm71b.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mm71b.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MM71B(RESTAB)
c
c     11 JUNE 80   ...JAM...     INITIAL RELEASE
c      6  OCT 90   ...CCA...     EBCDIC TO ASCII
c     23  Oct 92   ...WPL...     Ported for UNIX Conversion
c
      REAL*4   RESTAB(840),TESTAB(840)
      BYTE     BH(8)
      BYTE     BV(8)
      BYTE     BTIE(8)
      INTEGER  IH,  IV
c      REAL TXX(8)/
c     *Z2048414E,Z20202020,Z0000000F,Z2056414E,Z20202020,Z0000000C,
c     *Z50454954,Z544E494F/
      Real TX2(72)/
     *  42.352, 14.217, 27.015, 18.290, 41.942, 69.337, 20.276, 62.380,
     *  41.942, 69.337, 20.276, 62.380, 41.071,205.043, 11.609,176.869,
     *  41.071,205.043, 11.609,176.869, 40.263,339.276,  7.755,294.436,
     *  40.263,339.276,  7.755,294.436, 41.082,474.925,  6.418,412.438,
     *  41.082,474.925,  6.418,412.438, 40.482,610.410,  5.662,530.096,
     *  40.482,610.410,  5.662,530.096, 40.138,746.603,  5.901,647.031,
     *  40.138,746.603,  5.901,647.031, 40.975,881.827,  8.702,760.730,
     *  40.975,881.827,  8.702,760.730, 43.945,937.744, 11.046,806.424,
     *  84.600, 13.062, 63.493, 14.789, 84.600, 13.062, 63.493, 14.789/
      REAL TX3(80)/
     *  84.600,136.986, 53.869,117.704, 84.600,136.986, 53.869,117.704,
     *  84.386,271.971, 49.362,234.892, 84.386,271.971, 49.362,234.892,
     *  84.131,407.232, 47.492,353.352, 84.131,407.232, 47.492,353.352,
     *  83.963,543.080, 46.784,471.983, 83.963,543.080, 46.784,471.983,
     *  83.400,678.416, 46.345,589.675, 83.400,678.416, 46.345,589.675,
     *  83.116,813.947, 47.011,706.071, 83.116,813.947, 47.011,706.071,
     *  84.557,939.006, 50.014,809.414, 84.557,939.006, 50.014,809.414,
     * 131.649, 12.359,103.468, 12.286,130.706, 69.990, 99.295, 59.350,
     * 130.706, 69.990, 99.295, 59.350,131.030,204.431, 94.017,175.377,
     * 131.030,204.431, 94.017,175.377,130.765,339.617, 91.809,293.974/
      REAL TX4(80)/
     * 130.765,339.617, 91.809,293.974,130.478,475.208, 91.007,412.731,
     * 130.478,475.208, 91.007,412.731,130.211,610.509, 90.809,531.110,
     * 130.211,610.509, 90.809,531.110,130.003,746.128, 90.812,648.975,
     * 130.003,746.128, 90.812,648.975,130.353,881.896, 91.889,764.831,
     * 130.353,881.896, 91.889,764.831,131.128,939.024, 92.980,811.906,
     * 197.079, 13.631,161.927, 10.779,197.079, 13.631,161.927, 10.779,
     * 197.091,136.911,157.080,115.463,197.091,136.911,157.080,115.463,
     * 196.825,271.769,154.695,233.964,196.825,271.769,154.695,233.964,
     * 196.757,407.167,153.984,353.017,196.757,407.167,153.984,353.017,
     & 196.584,542.909,153.954,472.047,196.584,542.909,153.954,472.047/
      REAL TX5(80)/
     * 196.153,678.528,153.884,590.766,196.153,678.528,153.884,590.766,
     * 196.077,813.666,153.813,708.668,196.077,813.666,153.813,708.668,
     * 197.030,938.427,154.932,814.393,197.030,938.427,154.932,814.393,
     * 266.203, 12.501,224.884,  8.773,266.256, 69.057,223.271, 56.641,
     * 266.256, 69.057,223.271, 56.641,266.084,204.241,220.683,174.053,
     * 266.084,204.241,220.683,174.053,266.344,339.444,220.089,293.231,
     * 266.344,339.444,220.089,293.231,266.027,475.060,219.866,412.341,
     * 266.027,475.060,219.866,412.341,265.691,610.616,220.007,531.426,
     * 265.691,610.616,220.007,531.426,265.392,745.945,220.068,650.227,
     * 265.392,745.945,220.068,650.227,265.515,881.523,219.769,768.005/
      REAL TX6(80)/
     * 265.515,881.523,219.769,768.005,266.431,938.719,220.383,816.381,
     * 332.215, 12.623,285.841,  8.264,332.215, 12.623,285.841,  8.264,
     * 332.124,136.497,283.615,114.420,332.124,136.497,283.615,114.420,
     * 332.095,271.836,282.717,233.150,332.095,271.836,282.717,233.150,
     * 332.081,407.220,282.782,352.360,332.081,407.220,282.782,352.360,
     * 331.788,543.110,282.967,471.835,331.788,543.110,282.967,471.835,
     * 331.876,678.398,283.437,590.871,331.876,678.398,283.437,590.871,
     * 331.460,813.982,283.000,709.917,331.460,813.982,283.000,709.917,
     * 332.284,939.079,282.835,817.871,332.284,939.079,282.835,817.871,
     * 399.858, 12.205,348.794,  8.255,399.879, 68.952,348.355, 56.241/
      REAL TX7(80)/
     * 399.879, 68.952,348.355, 56.241,400.038,204.222,347.325,173.431,
     * 400.038,204.222,347.325,173.431,400.000,339.557,347.145,292.401,
     * 400.000,339.557,347.145,292.401,400.000,475.000,347.775,411.550,
     * 400.000,475.000,347.775,411.550,399.714,610.712,348.061,531.153,
     * 399.714,610.712,348.061,531.153,399.572,746.059,347.936,650.352,
     * 399.572,746.059,347.936,650.352,399.336,881.955,346.960,769.241,
     * 399.336,881.955,346.960,769.241,399.866,938.896,346.788,818.361,
     * 467.860, 12.942,412.747,  9.245,467.860, 12.942,412.747,  9.245,
     * 467.565,136.694,411.684,114.351,467.565,136.694,411.684,114.351,
     * 467.726,271.783,411.647,232.513,467.726,271.783,411.647,232.513/
      REAL TX8(80)/
     * 467.697,407.174,412.027,351.578,467.697,407.174,412.027,351.578,
     * 467.538,542.909,412.399,470.975,467.538,542.909,412.399,470.975,
     * 467.412,678.151,412.710,590.368,467.412,678.151,412.710,590.368,
     * 467.346,813.904,411.968,710.058,467.346,813.904,411.968,710.058,
     * 467.645,938.534,410.742,818.352,467.645,938.534,410.742,818.352,
     * 534.403, 12.163,475.201, 10.734,533.461, 69.361,474.436, 57.245,
     * 533.461, 69.361,474.436, 57.245,533.853,204.281,474.558,173.374,
     * 533.853,204.281,474.558,173.374,533.746,339.464,474.847,291.868,
     * 533.746,339.464,474.847,291.868,533.628,474.866,475.178,410.822,
     * 533.628,474.866,475.178,410.822,533.845,610.521,475.796,530.241/
      REAL TX9(80)/
     * 533.845,610.521,475.796,530.241,533.590,745.482,475.228,649.786,
     * 533.590,745.482,475.228,649.786,533.325,881.386,473.261,769.133,
     * 533.325,881.386,473.261,769.133,533.737,938.785,472.697,818.343,
     * 602.911, 12.987,539.651, 12.722,602.911, 12.987,539.651, 12.722,
     * 602.852,136.926,540.193,115.615,602.852,136.926,540.193,115.615,
     * 602.994,271.689,540.687,232.605,602.994,271.689,540.687,232.605,
     * 602.905,407.176,541.114,351.179,602.905,407.176,541.114,351.179,
     * 602.892,542.889,541.414,470.319,602.892,542.889,541.414,470.319,
     * 602.812,678.242,541.123,589.921,602.812,678.242,541.123,589.921,
     * 602.980,813.273,539.894,709.525,602.980,813.273,539.894,709.525/
      REAL TX10(80)/
     * 602.712,938.696,536.652,817.834,602.712,938.696,536.652,817.834,
     * 668.971, 12.741,600.604, 15.710,668.725, 69.002,601.695, 60.977,
     * 668.725, 69.002,601.695, 60.977,668.814,204.284,603.111,174.761,
     * 668.814,204.284,603.111,174.761,669.038,339.546,603.948,292.102,
     * 669.038,339.546,603.948,292.102,668.906,474.868,604.132,410.475,
     * 668.906,474.868,604.132,410.475,669.243,610.324,604.298,529.668,
     * 669.243,610.324,604.298,529.668,668.974,745.460,602.803,649.194,
     * 668.974,745.460,602.803,649.194,669.023,881.061,599.349,767.793,
     * 669.023,881.061,599.349,767.793,669.408,938.428,597.609,816.328,
     * 714.908, 12.958,642.570, 18.701,714.908, 12.958,642.570, 18.701/
      REAL TX11(80)/
     * 715.173,136.625,646.228,118.528,715.173,136.625,646.228,118.528,
     * 715.494,271.669,647.803,233.653,715.494,271.669,647.803,233.653,
     * 715.341,407.049,648.346,351.240,715.341,407.049,648.346,351.240,
     * 715.516,542.542,648.367,470.022,715.516,542.542,648.367,470.022,
     * 715.167,678.165,647.135,589.361,715.167,678.165,647.135,589.361,
     * 715.412,813.367,644.354,708.469,715.412,813.367,644.354,708.469,
     * 715.303,938.496,639.081,815.323,715.303,938.496,639.081,815.323,
     * 758.058, 14.391,680.538, 22.691,758.586, 69.724,683.822, 65.799,
     * 758.586, 69.724,683.822, 65.799,758.646,204.944,687.897,176.899,
     * 758.646,204.944,687.897,176.899,758.640,339.736,689.200,292.891/
      REAL TX12(40)/
     * 758.640,339.736,689.200,292.891,758.868,474.849,689.564,410.525,
     * 758.868,474.849,689.564,410.525,759.194,610.536,688.801,529.366,
     * 759.194,610.536,688.801,529.366,758.817,745.433,686.188,648.625,
     * 758.817,745.433,686.188,648.625,759.276,880.891,680.775,765.836,
     * 759.276,880.891,680.775,765.836,758.455,937.947,677.056,813.319
     */
      Equivalence  (TESTAB(1),BH(1))
      Equivalence  (TESTAB(3),IH)
      Equivalence  (TESTAB(4),BV(1))
      Equivalence  (TESTAB(6),IV)
      Equivalence  (TESTAB(7),BTIE(1))

      Equivalence (TESTAB(9),TX2(1)),(TESTAB(81),TX3(1)),
     & (TESTAB(161),TX4(1)),(TESTAB(241),TX5(1)),(TESTAB(321),TX6(1)),
     & (TESTAB(401),TX7(1)),(TESTAB(481),TX8(1)),(TESTAB(561),TX9(1)),
     &(TESTAB(641),TX10(1)),(TESTAB(721),TX11(1)),(TESTAB(801),TX12(1))

      Call MVCL('NAH     ',BH, 8)
      IH = 15
      Call MVCL('NAV     ',BV, 8)
      IV = 12
      Call MVCL('TIEPOINT', BTIE, 8)

      Do 20  N = 1, 840
         RESTAB(N) =  TESTAB(N)
20    Continue

      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mm71b.imake
/* Imake file for VICAR subroutine MM71B  */

#define SUBROUTINE  mm71b

#define MODULE_LIST  mm71b.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tmm71b.f
      INCLUDE 'VICMAIN_FOR'
c
C  PROGRAM TMM71B
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE MM71B.
C  MM71B PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL MARINER 9 DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  MM71B RETURNS DATA FOR THE "B" CAMERA.
c
      Subroutine  Main44
      REAL*4  BUF(840)

      Call  MM71B(BUF)
      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c     CALL PRNT(0,32,BUF)
      Call Prnt(99, 8, BUF(1), ' First 2 BUFs = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' Next  2 BUFs = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' Next  2 BUFs = .')
     
      CALL QPRINT(' GEOMA PARAMETERS:',18)
      CALL PRNT(7,80,BUF(81), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(161), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(241), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(321), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(401), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(481), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(561), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(641), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(721), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,40,BUF(801), '.')

      Return
      End  
$!-----------------------------------------------------------------------------
$ create tmm71b.imake
/* IMAKE file for Test of VICAR subroutine  MM71B  */

#define PROGRAM  tmm71b

#define MODULE_LIST tmm71b.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/* #define   LIB_LOCAL  */  /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tmm71b.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstmm71b.pdf
Procedure
Refgbl $echo
Body
Let _onfail="Continue"
Let $echo="Yes"
! THIS IS A TEST OF SUBROUTINE MM71B.
! MM71B PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
! NOMINAL MARINER 9 DISTORTION CORRECTION DATA IN GEOMA FORMAT.
! MM71B RETURNS DATA FOR THE "B" CAMERA.  THE DATA IS RETURNED
! IN AN 840 ELEMENT ARRAY.  THE VALUES ARE INITIALIZED IN THE
! SUBROUTINE.
TMM71B
Let  $echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create mm71b.hlp
1  MM71B

2  PURPOSE

      To provide the calling program a buffer containing nominal Mariner 9
      distortion correction data in GEOMA format.

2  CALLING SEQUENCE:

      CALL MM71B(BUF)

2  ARGUMENTS

       BUF is an 840 word array of GEOMA parameters returned.

       MM71A should be called to get data for the "A: camera and MM71b for
       the "B" camera.

2  OPERATION

       The data in the array is similar to the format as the parameter 
       dataset which can be input to GEOMA.  The difference between the
       two formats is in the first word.  This subroutine begins with NAH
       and the first word in the GEOMA dataset is the number of words (840)
       following the first word.

2  HISTORY

       Original Programmer: Unkown
       Current Cognizant Programmer: CHARLES AVIS
       Source Language: Fortran
       Revision:    
                    Jul-28  1980
                    Oct-06  1990             EBCDIC TO ASCII
                    Oct-23  1992  ...WPL...  Ported for UNIX Conversion   
$ Return
$!#############################################################################
