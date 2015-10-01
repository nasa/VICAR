$!****************************************************************************
$!
$! Build proc for MIPL module vosn7
$! VPACK Version 1.5, Friday, November 13, 1992, 08:59:34
$!
$! Execute by entering:		$ @vosn7
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
$ write sys$output "*** module vosn7 ***"
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
$   if F$SEARCH("vosn7.imake") .nes. ""
$   then
$      vimake vosn7
$      purge vosn7.bld
$   else
$      if F$SEARCH("vosn7.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vosn7
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vosn7.bld "STD"
$   else
$      @vosn7.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vosn7.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vosn7.com -
	-s vosn7.f -
	-i vosn7.imake -
	-t tvosn7.f tvosn7.imake tvosn7.pdf tstvosn7.pdf -
	-o vosn7.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vosn7.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE VOSN7(RESTAB)
c
C    20 MAY 80   ...JAM...   INITIAL RELEASE
c    13 Nov 92   ...WPL...   Ported for UNIX Conversion
c
      REAL*4 RESTAB(800)
c
C     VIKING 1976 CALIBRATION SPC 1 A   S/N 7
C     GENERATED MARCH 1976 BY GARY YAGI
c
      REAL      VI1A(800)
      BYTE      BH(8)
      INTEGER   IH
      BYTE      BV(8)
      INTEGER   IV
      BYTE      BTIE(8)
c
c      REAL SN71(74)/'NAH ','    ',Z00000015,'NAV ','    ',
c     &Z00000008,'TIEP','OINT',
c
       Real*4  SN71(66)/
     *  43.872,  29.778,   4.700,  18.400,  43.872,  29.778,
     *   4.700,  18.400,  43.957, 149.591,   8.063, 137.563,
     *  43.957, 149.591,   8.063, 137.563,  43.605, 268.910,
     *  10.791, 255.211,  43.605, 268.910,  10.791, 255.211,
     *  43.764, 387.476,  12.982, 372.052,  43.764, 387.476,
     *  12.982, 372.052,  43.626, 506.148,  14.852, 488.412,
     *  43.626, 506.148,  14.852, 488.412,  43.694, 624.992,
     *  16.602, 604.774,  43.694, 624.992,  16.602, 604.774,
     *  43.802, 744.143,  18.284, 720.982,  43.802, 744.143,
     *  18.284, 720.982,  43.826, 862.859,  19.701, 837.374,
     *  43.826, 862.859,  19.701, 837.374,  43.909, 981.875/
      REAL*4 SN72(66)/
     *  21.165, 953.850,  43.909, 981.875,  21.165, 953.850,
     *  43.968,1100.418,  22.219,1069.952,  43.968,1100.418,
     *  22.219,1069.952,  43.889,1219.626,  23.000,1187.000,
     *  43.889,1219.626,  23.000,1187.000, 176.619,  30.421,
     * 132.000,  20.200, 176.528,  90.300, 133.625,  79.813,
     * 176.528,  90.300, 133.625,  79.813, 176.609, 208.844,
     * 136.774, 197.025, 176.609, 208.844, 136.774, 197.025,
     * 176.656, 328.186, 139.225, 313.980, 176.656, 328.186,
     * 139.225, 313.980, 176.688, 446.950, 141.430, 430.528,
     * 176.688, 446.950, 141.430, 430.528, 176.777, 565.592,
     * 143.148, 546.938, 176.777, 565.592, 143.148, 546.938/
      REAL*4 SN73(66)/
     * 176.678, 684.699, 144.796, 663.090, 176.678, 684.699,
     * 144.796, 663.090, 176.882, 803.634, 146.438, 779.000,
     * 176.882, 803.634, 146.438, 779.000, 176.545, 922.471,
     * 148.009, 895.099, 176.545, 922.471, 148.009, 895.099,
     * 176.744,1041.385, 149.483,1011.321, 176.744,1041.385,
     * 149.483,1011.321, 176.773,1160.184, 150.759,1127.313,
     * 176.773,1160.184, 150.759,1127.313, 177.073,1219.925,
     * 151.500,1185.600, 309.951,  29.895, 260.500,  20.500,
     * 309.951,  29.895, 260.500,  20.500, 309.558, 149.340,
     * 263.567, 139.429, 309.558, 149.340, 263.567, 139.429,
     * 309.698, 268.325, 266.168, 256.325, 309.698, 268.325/
      REAL*4 SN74(66)/
     * 266.168, 256.325, 309.584, 387.553, 268.291, 372.943,
     * 309.584, 387.553, 268.291, 372.943, 309.723, 506.270,
     * 270.438, 489.188, 309.723, 506.270, 270.438, 489.188,
     * 309.558, 625.096, 271.867, 605.412, 309.558, 625.096,
     * 271.867, 605.412, 309.589, 744.234, 273.548, 721.103,
     * 309.589, 744.234, 273.548, 721.103, 309.604, 862.924,
     * 275.303, 836.868, 309.604, 862.924, 275.303, 836.868,
     * 309.675, 982.056, 276.765, 952.737, 309.675, 982.056,
     * 276.765, 952.737, 309.722,1100.785, 278.449,1068.617,
     * 309.722,1100.785, 278.449,1068.617, 310.035,1220.439,
     * 280.200,1184.700, 310.035,1220.439, 280.200,1184.700/
      REAL*4 SN75(66)/
     * 442.489,  29.854, 389.000,  21.300, 442.244,  90.117,
     * 390.535,  81.563, 442.244,  90.117, 390.535,  81.563,
     * 442.652, 208.930, 393.455, 198.815, 442.652, 208.930,
     * 393.455, 198.815, 442.165, 328.321, 395.537, 315.471,
     * 442.165, 328.321, 395.537, 315.471, 442.301, 446.937,
     * 397.560, 431.599, 442.301, 446.937, 397.560, 431.599,
     * 442.376, 565.631, 399.647, 547.379, 442.376, 565.631,
     * 399.647, 547.379, 442.310, 684.649, 401.044, 663.287,
     * 442.310, 684.649, 401.044, 663.287, 442.309, 803.584,
     * 402.677, 779.014, 442.309, 803.584, 402.677, 779.014,
     * 442.394, 922.362, 404.601, 894.448, 442.394, 922.362/
      REAL*4 SN76(66)/
     * 404.601, 894.448, 442.342,1041.461, 405.974,1010.357,
     * 442.342,1041.461, 405.974,1010.357, 442.433,1160.192,
     * 407.943,1126.061, 442.433,1160.192, 407.943,1126.061,
     * 442.596,1220.236, 408.500,1184.300, 575.314,  29.956,
     * 518.000,  22.300, 575.314,  29.956, 518.000,  22.300,
     * 574.854, 149.407, 520.397, 140.887, 574.854, 149.407,
     * 520.397, 140.887, 575.127, 268.302, 522.912, 257.464,
     * 575.127, 268.302, 522.912, 257.464, 575.115, 387.455,
     * 524.972, 373.689, 575.115, 387.455, 524.972, 373.689,
     * 575.132, 506.195, 526.827, 489.643, 575.132, 506.195,
     * 526.827, 489.643, 575.000, 625.000, 528.497, 605.391/
      REAL*4 SN77(66)/
     * 575.000, 625.000, 528.497, 605.391, 575.128, 744.146,
     * 530.363, 720.708, 575.128, 744.146, 530.363, 720.708,
     * 575.165, 863.038, 531.721, 836.463, 575.165, 863.038,
     * 531.721, 836.463, 574.998, 981.984, 533.414, 952.011,
     * 574.998, 981.984, 533.414, 952.011, 575.100,1100.807,
     * 534.890,1067.676, 575.100,1100.807, 534.890,1067.676,
     * 575.174,1220.150, 536.800,1183.700, 575.174,1220.150,
     * 536.800,1183.700, 707.736,  30.070, 646.500,  22.500,
     * 707.648,  90.337, 647.760,  82.514, 707.648,  90.337,
     * 647.760,  82.514, 707.721, 208.951, 650.453, 199.380,
     * 707.721, 208.951, 650.453, 199.380, 707.800, 327.992/
      REAL*4 SN78(66)/
     * 652.578, 315.670, 707.800, 327.992, 652.578, 315.670,
     * 707.864, 446.806, 654.525, 431.580, 707.864, 446.806,
     * 654.525, 431.580, 707.922, 565.646, 656.503, 547.186,
     * 707.922, 565.646, 656.503, 547.186, 707.897, 684.597,
     * 657.758, 662.784, 707.897, 684.597, 657.758, 662.784,
     * 707.835, 803.564, 659.379, 778.202, 707.835, 803.564,
     * 659.379, 778.202, 707.905, 922.242, 660.963, 893.574,
     * 707.905, 922.242, 660.963, 893.574, 707.853,1041.406,
     * 662.381,1009.360, 707.853,1041.406, 662.381,1009.360,
     * 707.832,1160.039, 663.980,1124.795, 707.832,1160.039,
     * 663.980,1124.795, 707.868,1220.128, 665.000,1183.000/
      REAL*4 SN79(66)/
     * 840.287,  30.352, 775.500,  22.500, 840.287,  30.352,
     * 775.500,  22.500, 840.279, 149.788, 777.935, 140.696,
     * 840.279, 149.788, 777.935, 140.696, 840.642, 268.622,
     * 780.366, 257.168, 840.642, 268.622, 780.366, 257.168,
     * 840.321, 387.630, 782.357, 373.098, 840.321, 387.630,
     * 782.357, 373.098, 840.393, 506.188, 783.953, 488.765,
     * 840.393, 506.188, 783.953, 488.765, 840.344, 624.940,
     * 785.522, 604.296, 840.344, 624.940, 785.522, 604.296,
     * 840.374, 744.118, 787.092, 719.586, 840.374, 744.118,
     * 787.092, 719.586, 840.222, 862.783, 788.291, 835.176,
     * 840.222, 862.783, 788.291, 835.176, 839.783, 981.652/
      REAL*4 SN710(66)/
     * 789.442, 950.479, 839.783, 981.652, 789.442, 950.479,
     * 840.361,1100.575, 791.344,1066.328, 840.361,1100.575,
     * 791.344,1066.328, 840.294,1220.126, 793.200,1182.300,
     * 840.294,1220.126, 793.200,1182.300, 973.325,  30.596,
     * 905.500,  22.000, 973.003,  90.610, 906.000,  81.681,
     * 973.003,  90.610, 906.000,  81.681, 973.002, 209.055,
     * 908.035, 198.276, 973.002, 209.055, 908.035, 198.276,
     * 973.143, 327.982, 910.256, 314.437, 973.143, 327.982,
     * 910.256, 314.437, 973.048, 446.804, 911.847, 430.193,
     * 973.048, 446.804, 911.847, 430.193, 973.034, 565.610,
     * 913.404, 545.576, 973.034, 565.610, 913.404, 545.576/
      REAL*4 SN711(66)/
     * 972.682, 684.342, 914.434, 660.772, 972.682, 684.342,
     * 914.434, 660.772, 973.078, 803.453, 916.073, 776.388,
     * 973.078, 803.453, 916.073, 776.388, 972.929, 922.062,
     * 917.048, 891.750, 972.929, 922.062, 917.048, 891.750,
     * 973.050,1041.296, 918.735,1007.551, 973.050,1041.296,
     * 918.735,1007.551, 972.937,1159.860, 920.335,1123.126,
     * 972.937,1159.860, 920.335,1123.126, 972.991,1220.111,
     * 921.500,1181.600,1106.081,  30.514,1034.500,  20.200,
     *1106.081,  30.514,1034.500,  20.200,1105.663, 149.765,
     *1035.902, 138.329,1105.663, 149.765,1035.902, 138.329,
     *1105.630, 268.396,1037.671, 254.917,1105.630, 268.396/
      REAL*4 SN712(66)/
     *1037.671, 254.917,1105.692, 387.437,1039.452, 370.964,
     *1105.692, 387.437,1039.452, 370.964,1105.773, 506.244,
     *1040.987, 486.535,1105.773, 506.244,1040.987, 486.535,
     *1105.662, 624.975,1042.429, 601.848,1105.662, 624.975,
     *1042.429, 601.848,1105.272, 744.196,1043.012, 717.438,
     *1105.272, 744.196,1043.012, 717.438,1105.644, 862.669,
     *1044.599, 832.670,1105.644, 862.669,1044.599, 832.670,
     *1105.617, 981.620,1045.776, 948.289,1105.617, 981.620,
     *1045.776, 948.289,1104.999,1100.401,1046.639,1064.126,
     *1104.999,1100.401,1046.639,1064.126,1105.630,1219.913,
     *1049.500,1180.700,1105.630,1219.913,1049.500,1180.700/
c
c
      EQUIVALENCE  (VI1A(1), BH(1))
      Equivalence  (VI1A(3), IH)
      Equivalence  (VI1A(4), BV(1))
      EQuivalence  (VI1A(6), IV)
      Equivalence  (VI1A(7), BTIE(1))
c
      EQUIVALENCE (VI1A(9),SN71(1)),(VI1A(75),SN72(1)),
     & (VI1A(141),SN73(1)),(VI1A(207),SN74(1)),(VI1A(273),SN75(1)),
     & (VI1A(339),SN76(1)),
     & (VI1A(405),SN77(1)),(VI1A(471),SN78(1)),(VI1A(537),SN79(1)),
     & (VI1A(603),SN710(1)),(VI1A(669),SN711(1)),(VI1A(735),SN712(1))
c
c
      Call MVCL('NAH     ',BH, 8)
      IH = 21
      Call MVCL('NAV     ',BV, 8)
      IV =  8
      Call MVCL('TIEPOINT',BTIE, 8)
c
      Do  20  IJ = 1, 800
        RESTAB(IJ) = VI1A(IJ)
20    Continue
c
c     CALL MVL(VI1A,RESTAB,3200)
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vosn7.imake
/* Imake file for VICAR subroutine VOSN7  */

#define SUBROUTINE  vosn7

#define MODULE_LIST  vosn7.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tvosn7.f
      INCLUDE 'VICMAIN_FOR'
c
      Subroutine  Main44
c
C  PROGRAM TVOSN7
C
C   THIS IS A TESTPROGRAM FOR SUBROUTINE VOSN7.
C   VOSN7 PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C   NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA
C   FORMAT.  VOSN7 RETURNS DATA FOR THE CAMERA.
c
      REAL*4  BUF(800)
c
      CALL VOSN7(BUF)
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
C PROCESS
C END-PROC
C *** END PDF ***
$!-----------------------------------------------------------------------------
$ create tvosn7.imake
/* IMAKE file for Test of VICAR subroutine  VOSN7  */

#define PROGRAM  tvosn7

#define MODULE_LIST tvosn7.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/*  #define   LIB_LOCAL    */   /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tvosn7.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstvosn7.pdf
Procedure
Refgbl $Echo
Body
Let _Onfail="Continue"
Let $Echo="Yes"
! THIS IS A TEST OF SUBROUTINE VOSN7.
! VOSN7 PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
! NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA FORMAT.
! VOSN7 RETURNS DATA FOR THE CAMERA.  THE DATA IS RETURNED
! IN AN 800 ELEMENT ARRAY.  THE VALUES ARE INITIALIZED IN THE
! SUBROUTINE.
TVOSN7
Let $Echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create vosn7.hlp
1 VOSN7

2  PURPOSE

     To provide the calling program a buffer containing nominal Viking
     Orbiter distortion correction data in the GEOMA format.

2  CALLING SEQUENCE

     CALL VOSN7(BUF)

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
     Latest Revision: 1, 28 July 1980

     Ported for UNIX Conversion:   W.P. Lee,  Nov-13-1992
$ Return
$!#############################################################################
