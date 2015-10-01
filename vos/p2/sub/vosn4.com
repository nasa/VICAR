$!****************************************************************************
$!
$! Build proc for MIPL module vosn4
$! VPACK Version 1.5, Wednesday, November 04, 1992, 09:41:31
$!
$! Execute by entering:		$ @vosn4
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
$ write sys$output "*** module vosn4 ***"
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
$   if F$SEARCH("vosn4.imake") .nes. ""
$   then
$      vimake vosn4
$      purge vosn4.bld
$   else
$      if F$SEARCH("vosn4.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vosn4
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vosn4.bld "STD"
$   else
$      @vosn4.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vosn4.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vosn4.com -
	-s vosn4.f -
	-i vosn4.imake -
	-t tvosn4.f tvosn4.imake tvosn4.pdf tstvosn4.pdf -
	-o vosn4.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vosn4.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE VOSN4(RESTAB)
c
C    20 MAY 80      ...JAM...   INITIAL RELEASE
c    Nov-04-1992    ...WPL...   Ported for UNIX Conversion
c
      REAL*4 RESTAB(800)
C
C     VIKING 1976 CALIBRATION SPC 1 B   S/N 4
C     GENERATED MARCH 1976 BY GARY YAGI
c
      REAL      VI1B(800)
      BYTE      BH(8)
      INTEGER   IH
      BYTE      BV(8)
      INTEGER   IV
      BYTE      BTIE(8)
c
c      ,SN41(74)/'NAH ','    ',Z00000015,'NAV ','    ',
c     &Z00000008,'TIEP','OINT',
c
      REAL  SN41(66)/
     *  45.437,  30.065,   5.400,  23.700,  45.437,  30.065,
     *   5.400,  23.700,  44.387, 148.970,   6.864, 141.596,
     *  44.387, 148.970,   6.864, 141.596,  44.010, 267.773,
     *   8.551, 258.590,  44.010, 267.773,   8.551, 258.590,
     *  43.851, 386.943,   9.599, 375.239,  43.851, 386.943,
     *   9.599, 375.239,  43.801, 505.756,  10.582, 491.153,
     *  43.801, 505.756,  10.582, 491.153,  43.885, 624.490,
     *  11.518, 606.812,  43.885, 624.490,  11.518, 606.812,
     *  44.097, 743.745,  12.317, 722.583,  44.097, 743.745,
     *  12.317, 722.583,  44.092, 862.778,  12.814, 838.547,
     *  44.092, 862.778,  12.814, 838.547,  44.849, 981.452/
      REAL*4 SN42(66)/
     *  14.380, 953.933,  44.849, 981.452,  14.380, 953.933,
     *  44.938,1100.451,  14.622,1070.021,  44.938,1100.451,
     *  14.622,1070.021,  44.702,1219.427,  15.000,1185.400,
     *  44.702,1219.427,  15.000,1185.400, 177.382,  29.946,
     * 133.500,  24.400, 176.947,  89.563, 134.188,  83.563,
     * 176.947,  89.563, 134.188,  83.563, 176.845, 208.299,
     * 136.093, 200.559, 176.845, 208.299, 136.093, 200.559,
     * 176.519, 327.323, 137.512, 316.854, 176.519, 327.323,
     * 137.512, 316.854, 176.383, 446.310, 138.533, 433.138,
     * 176.383, 446.310, 138.533, 433.138, 176.451, 565.084,
     * 139.517, 548.852, 176.451, 565.084, 139.517, 548.852/
      REAL*4 SN43(66)/
     * 176.758, 684.420, 140.488, 664.558, 176.758, 684.420,
     * 140.488, 664.558, 177.058, 802.799, 141.100, 779.580,
     * 177.058, 802.799, 141.100, 779.580, 176.475, 922.667,
     * 141.477, 896.094, 176.475, 922.667, 141.477, 896.094,
     * 177.148,1040.684, 142.568,1010.801, 177.148,1040.684,
     * 142.568,1010.801, 177.674,1160.161, 143.560,1126.639,
     * 177.674,1160.161, 143.560,1126.639, 177.557,1219.144,
     * 143.800,1183.500, 310.378,  29.698, 263.500,  24.800,
     * 310.378,  29.698, 263.500,  24.800, 309.466, 148.886,
     * 264.579, 142.490, 309.466, 148.886, 264.579, 142.490,
     * 309.676, 267.537, 266.586, 258.705, 309.676, 267.537/
      REAL*4 SN44(66)/
     * 266.586, 258.705, 309.539, 386.543, 267.685, 375.005,
     * 309.539, 386.543, 267.685, 375.005, 309.107, 506.483,
     * 267.909, 491.989, 309.107, 506.483, 267.909, 491.989,
     * 309.606, 624.741, 269.321, 606.549, 309.606, 624.741,
     * 269.321, 606.549, 309.621, 744.060, 269.783, 722.360,
     * 309.621, 744.060, 269.783, 722.360, 309.470, 863.006,
     * 270.441, 837.836, 309.470, 863.006, 270.441, 837.836,
     * 309.642, 982.300, 270.931, 953.546, 309.642, 982.300,
     * 270.931, 953.546, 309.902,1100.661, 272.021,1068.338,
     * 309.902,1100.661, 272.021,1068.338, 310.368,1219.720,
     * 273.500,1183.200, 310.368,1219.720, 273.500,1183.200/
      REAL*4 SN45(66)/
     * 443.332,  29.651, 393.500,  25.200, 442.616,  88.883,
     * 393.787,  83.608, 442.616,  88.883, 393.787,  83.608,
     * 442.770, 208.046, 395.654, 200.805, 442.770, 208.046,
     * 395.654, 200.805, 442.426, 327.588, 396.651, 317.534,
     * 442.426, 327.588, 396.651, 317.534, 442.463, 446.648,
     * 397.689, 433.538, 442.463, 446.648, 397.689, 433.538,
     * 442.271, 565.819, 398.259, 549.334, 442.271, 565.819,
     * 398.259, 549.334, 442.279, 684.900, 398.736, 664.960,
     * 442.279, 684.900, 398.736, 664.960, 442.260, 803.592,
     * 399.596, 779.964, 442.260, 803.592, 399.596, 779.964,
     * 442.510, 922.322, 400.410, 894.995, 442.510, 922.322/
      REAL*4 SN46(66)/
     * 400.410, 894.995, 442.410,1041.064, 401.272,1010.031,
     * 442.410,1041.064, 401.272,1010.031, 442.305,1159.719,
     * 401.694,1125.039, 442.305,1159.719, 401.694,1125.039,
     * 442.901,1219.714, 403.000,1182.500, 576.029,  29.970,
     * 523.500,  25.400, 576.029,  29.970, 523.500,  25.400,
     * 575.234, 149.240, 524.242, 143.259, 575.234, 149.240,
     * 524.242, 143.259, 574.980, 268.023, 525.307, 259.134,
     * 574.980, 268.023, 525.307, 259.134, 574.880, 387.133,
     * 526.423, 375.535, 574.880, 387.133, 526.423, 375.535,
     * 574.973, 506.017, 527.451, 491.133, 574.973, 506.017,
     * 527.451, 491.133, 575.000, 625.000, 528.001, 606.535/
      REAL*4 SN47(66)/
     * 575.000, 625.000, 528.001, 606.535, 575.152, 743.918,
     * 528.709, 721.821, 575.152, 743.918, 528.709, 721.821,
     * 575.144, 862.799, 529.271, 836.900, 575.144, 862.799,
     * 529.271, 836.900, 575.025, 981.935, 529.833, 952.180,
     * 575.025, 981.935, 529.833, 952.180, 574.786,1100.485,
     * 530.542,1067.192, 574.786,1100.485, 530.542,1067.192,
     * 575.645,1219.615, 532.500,1182.000, 575.645,1219.615,
     * 532.500,1182.000, 708.550,  30.495, 652.700,  25.200,
     * 708.154,  89.728, 652.836,  84.137, 708.154,  89.728,
     * 652.836,  84.137, 708.115, 208.392, 654.504, 201.002,
     * 708.115, 208.392, 654.504, 201.002, 708.108, 327.384/
      REAL*4 SN48(66)/
     * 655.683, 317.126, 708.108, 327.384, 655.683, 317.126,
     * 708.007, 446.559, 656.604, 433.137, 708.007, 446.559,
     * 656.604, 433.137, 707.959, 565.365, 657.150, 548.400,
     * 707.959, 565.365, 657.150, 548.400, 708.147, 684.225,
     * 657.770, 663.714, 708.147, 684.225, 657.770, 663.714,
     * 707.934, 803.586, 658.465, 779.024, 707.934, 803.586,
     * 658.465, 779.024, 708.222, 922.270, 659.269, 893.891,
     * 708.222, 922.270, 659.269, 893.891, 708.257,1041.080,
     * 659.879,1008.946, 708.257,1041.080, 659.879,1008.946,
     * 708.324,1159.972, 661.056,1124.003, 708.324,1159.972,
     * 661.056,1124.003, 708.498,1219.728, 662.000,1181.400/
      REAL*4 SN49(66)/
     * 841.045,  30.243, 782.000,  24.900, 841.045,  30.243,
     * 782.000,  24.900, 841.092, 149.167, 783.343, 142.342,
     * 841.092, 149.167, 783.343, 142.342, 840.704, 267.862,
     * 784.562, 258.581, 840.704, 267.862, 784.562, 258.581,
     * 840.628, 386.782, 785.626, 374.516, 840.628, 386.782,
     * 785.626, 374.516, 840.702, 505.922, 786.525, 490.229,
     * 840.702, 505.922, 786.525, 490.229, 840.627, 624.752,
     * 786.959, 605.297, 840.627, 624.752, 786.959, 605.297,
     * 840.693, 743.566, 787.700, 720.237, 840.693, 743.566,
     * 787.700, 720.237, 840.721, 862.493, 788.382, 835.617,
     * 840.721, 862.493, 788.382, 835.617, 840.850, 981.764/
      REAL*4 SN410(66)/
     * 789.097, 950.795, 840.850, 981.764, 789.097, 950.795,
     * 841.106,1100.532, 790.561,1065.879, 841.106,1100.532,
     * 790.561,1065.879, 841.030,1219.735, 791.500,1180.700,
     * 841.030,1219.735, 791.500,1180.700, 974.164,  30.652,
     * 912.000,  24.100, 973.422,  89.877, 912.205,  83.235,
     * 973.422,  89.877, 912.205,  83.235, 973.334, 208.500,
     * 913.532, 199.858, 973.334, 208.500, 913.532, 199.858,
     * 973.376, 327.409, 914.638, 316.002, 973.376, 327.409,
     * 914.638, 316.002, 973.488, 446.202, 915.660, 431.573,
     * 973.488, 446.202, 915.660, 431.573, 973.489, 565.175,
     * 916.474, 546.759, 973.489, 565.175, 916.474, 546.759/
      REAL*4 SN411(66)/
     * 973.584, 684.296, 916.999, 662.111, 973.584, 684.296,
     * 916.999, 662.111, 973.619, 803.050, 917.689, 777.008,
     * 973.619, 803.050, 917.689, 777.008, 973.841, 921.900,
     * 918.406, 891.951, 973.841, 921.900, 918.406, 891.951,
     * 973.641,1040.748, 919.022,1007.147, 973.641,1040.748,
     * 919.022,1007.147, 973.488,1159.499, 920.314,1122.090,
     * 973.488,1159.499, 920.314,1122.090, 973.786,1219.545,
     * 921.300,1179.900,1106.762,  30.905,1041.000,  23.200,
     *1106.762,  30.905,1041.000,  23.200,1106.127, 149.187,
     *1041.654, 140.476,1106.127, 149.187,1041.654, 140.476,
     *1106.186, 267.937,1042.912, 256.964,1106.186, 267.937/
      REAL*4 SN412(66)/
     *1042.912, 256.964,1106.278, 386.906,1044.146, 372.761,
     *1106.278, 386.906,1044.146, 372.761,1106.302, 505.818,
     *1045.035, 488.238,1106.302, 505.818,1045.035, 488.238,
     *1106.120, 624.625,1045.449, 603.386,1106.120, 624.625,
     *1045.449, 603.386,1106.194, 743.814,1046.090, 718.261,
     *1106.194, 743.814,1046.090, 718.261,1106.124, 862.583,
     *1046.623, 833.460,1106.124, 862.583,1046.623, 833.460,
     *1106.190, 981.363,1047.468, 948.360,1106.190, 981.363,
     *1047.468, 948.360,1106.183,1100.275,1048.415,1063.888,
     *1106.183,1100.275,1048.415,1063.888,1106.475,1219.783,
     *1050.300,1179.300,1106.475,1219.783,1050.300,1179.300/
c
c 
      EQUIVALENCE  (VI1B(1), BH(1))
      Equivalence  (VI1B(3), IH)
      Equivalence  (VI1B(4), BV(1))
      EQuivalence  (VI1B(6), IV)
      Equivalence  (VI1B(7), BTIE(1))

      EQUIVALENCE (VI1B(9),SN41(1)),(VI1B(75),SN42(1)),
     & (VI1B(141),SN43(1)),(VI1B(207),SN44(1)),(VI1B(273),SN45(1)),
     & (VI1B(339),SN46(1)),
     &(VI1B(405),SN47(1)),(VI1B(471),SN48(1)),(VI1B(537),SN49(1)),
     &(VI1B(603),SN410(1)),(VI1B(669),SN411(1)),(VI1B(735),SN412(1))
c
c      CALL MVL(VI1B,RESTAB,3200)
c
      Call MVCL('NAH     ',BH, 8)
      IH = 21
      Call MVCL('NAV     ',BV, 8)
      IV =  8
      Call MVCL('TIEPOINT',BTIE, 8)

      Do  20  IJ = 1, 800
        RESTAB(IJ) = VI1B(IJ)
20    Continue

      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vosn4.imake
/* Imake file for VICAR subroutine VOSN4  */

#define SUBROUTINE  vosn4

#define MODULE_LIST  vosn4.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tvosn4.f
      INCLUDE 'VICMAIN_FOR'
c
      SUBROUTINE MAIN44
c
C  PROGRAM TVOSN4
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE VOSN4.
C  VOSN4 PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  VOSN4 RETURNS DATA FOR THE CAMERA.
c
      REAL*4  BUF(800)
c
      CALL VOSN4(BUF)
c
      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c
c     CALL PRNT(7,8,BUF)
c
      Call Prnt(99, 8, BUF(1), ' FIRST 2 BUF = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' NEXT  2 BUF = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' NEXT  2 BUF = .')
c
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
c
      Return
      End
c
C *** START PDF ***
CPROCESS
CEND-PROC
C *** END PDF ***
$!-----------------------------------------------------------------------------
$ create tvosn4.imake
/* IMAKE file for Test of VICAR subroutine  VOSN4  */

#define PROGRAM  tvosn4

#define MODULE_LIST tvosn4.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/* #define   LIB_LOCAL   */     /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tvosn4.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstvosn4.pdf
Procedure
Refgbl $echo
Body
Let  _onfail="Continue"
Let  $echo="Yes"
! THIS IS A TEST OF SUBROUTINE VOSN4.
! VOSN4 PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
! NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA FORMAT.
! VOSN4 RETURNS DATA FOR THE CAMERA.  THE DATA IS RETURNED
! IN AN 800 ELEMENT ARRAY.  THE VALUES ARE INITIALIZED IN THE
! SUBROUTINE.
TVOSN4
Let  $echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create vosn4.hlp
1 VOSN4

2  PURPOSE

     To provide the calling program a buffer containing nominal Viking
     Orbiter distortion correction data in the GEOMA format.

2  CALLING SEQUENCE

     CALL VOSN4(BUF)


     BUF    is an 800 word array of GEOMA parameters returned.

     VOSN4 should be called to get data for the camera serial number 4.
     VOSN6 should be called to get data for the camera serial number 6.
     VOSN7 should be called to get data for the camera serial number 7.
     VOSN8 should be called to get data for the camera serial number 8.

2  OPERATION

     The data in the array is similar to the format as the parameter
     file which can be input to GEOMA.  The difference between the
     two formats is in the first word.  This subroutine begins with
     NAH and the first word in the GEOMA dataset is the number of words
     (800) following the first word.

2  HISTORY

     Original Programmer:  Gary Yagi
     Current Cognizant Programmer:  Joel Mosher
     Source Language:  Fortran
     Latest Revision: 1, 24-JUN-1985

     Ported for UNIX Conversion:  W.P. Lee;  November 4, 1992 
$ Return
$!#############################################################################
