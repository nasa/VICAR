$!****************************************************************************
$!
$! Build proc for MIPL module voos
$! VPACK Version 1.7, Friday, August 27, 1993, 14:23:07
$!
$! Execute by entering:		$ @voos
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
$ write sys$output "*** module voos ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to voos.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("voos.imake") .nes. ""
$   then
$      vimake voos
$      purge voos.bld
$   else
$      if F$SEARCH("voos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake voos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @voos.bld "STD"
$   else
$      @voos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create voos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack voos.com -
	-s voos.f zvoos.c -
	-i voos.imake -
	-t tvoos.f tzvoos.c tvoos.imake tvoos.pdf tstvoos.pdf -
	-o voos.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create voos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c**********************************************************
c Subroutine VOOS(icam,loc)
c
C Routine to return Viking Orbiter object space reseau
c
c**********************************************************
C
      SUBROUTINE VOOS(ICAM,LOC)
c
      IMPLICIT INTEGER (A-Z)
      REAL*4 LOC(2,103)    !Returned reseau (line,sample) coordinates
      REAL*4 ALOC(2,103,8) !Temporary buffer
      REAL*4 OS4A(2,54),OS6A(2,54),OS7A(2,54),OS8A(2,54)
      REAL*4 OS4B(2,49),OS6B(2,49),OS7B(2,49),OS8B(2,49)
      EQUIVALENCE (ALOC(1,1,4),OS4A),(ALOC(1,55,4),OS4B)
      EQUIVALENCE (ALOC(1,1,6),OS6A),(ALOC(1,55,6),OS6B)
      EQUIVALENCE (ALOC(1,1,7),OS7A),(ALOC(1,55,7),OS7B)
      EQUIVALENCE (ALOC(1,1,8),OS8A),(ALOC(1,55,8),OS8B)
c-----------------------
C  OBJECT SPACE COORDINATES FOR S/N 4  (L,S)
c
      DATA OS4A /          !First half of coordinates for s/n 4
c
     *  45.113,  29.807,  44.368, 149.043,  43.987, 267.811,
     *  43.813, 386.999,  43.810, 505.788,  43.894, 624.531,
     *  44.099, 743.795,  44.124, 862.805,  44.867, 981.512,
     *  44.977,1100.491,  44.761,1219.436, 177.386,  29.650,
     * 176.970,  89.625, 176.820, 208.309, 176.522, 327.354,
     * 176.388, 446.331, 176.459, 565.104, 176.784, 684.447,
     * 177.090, 802.826, 176.519, 922.689, 177.207,1040.703,
     * 177.728,1160.186, 176.791,1218.905, 310.290,  29.701,
     * 309.435, 148.896, 309.664, 267.553, 309.530, 386.558,
     * 309.113, 506.506, 309.617, 624.750, 309.641, 744.080,
     * 309.501, 863.014, 309.677, 982.325, 309.958,1100.665,
     * 310.401,1219.759, 444.261,  29.468, 443.081,  89.383,
     * 442.750, 208.045, 442.418, 327.595, 442.466, 446.646,
     * 442.286, 565.821, 442.301, 684.905, 442.287, 803.596,
     * 442.552, 922.323, 442.458,1041.073, 442.355,1159.723,
     * 442.676,1220.154, 575.969,  29.995, 575.203, 149.230,
     * 574.969, 268.019, 574.872, 387.129, 574.966, 506.028,
     * 575.000, 625.000, 575.173, 743.917, 575.174, 862.786/
c
      DATA OS4B /        !Second half of coordinates for s/n 4
c
     * 575.062, 981.943, 574.840,1100.465, 575.691,1219.638,
     * 708.814,  31.239, 708.137,  89.698, 708.095, 208.374,
     * 708.100, 327.374, 708.027, 446.526, 708.791, 564.615,
     * 708.187, 684.188, 707.960, 803.578, 708.264, 922.247,
     * 708.304,1041.067, 708.390,1159.944, 708.469,1219.865,
     * 840.979,  30.182, 841.078, 149.147, 840.688, 267.830,
     * 840.621, 386.759, 840.709, 505.905, 840.617, 624.737,
     * 840.716, 743.552, 840.752, 862.461, 840.889, 981.748,
     * 841.160,1100.503, 841.076,1219.730, 974.146,  30.633,
     * 973.387,  89.829, 973.314, 208.467, 973.365, 327.373,
     * 973.486, 446.156, 973.500, 565.135, 973.605, 684.252,
     * 973.643, 803.007, 973.877, 921.853, 973.684,1040.715,
     * 973.546,1159.413, 973.840,1220.031,1106.727,  30.863,
     *1106.107, 149.144,1106.175, 267.889,1106.270, 386.867,
     *1106.308, 505.782,1106.133, 624.580,1106.220, 743.790,
     *1106.157, 862.541,1106.232, 981.333,1106.240,1100.246,
     *1106.520,1219.724/
c-----------------------
C  OBJECT SPACE COORDINATES FOR S/N 6  (L,S)
c
      DATA OS6A /         !First half of coordinates for s/n 6
c
     *  44.623,  30.413,  44.970, 149.758,  44.568, 268.816,
     *  44.228, 387.450,  44.288, 506.355,  43.896, 625.286,
     *  44.027, 744.115,  44.392, 862.834,  44.365, 981.715,
     *  44.581,1100.578,  44.492,1219.696, 177.527,  30.442,
     * 177.297,  90.296, 177.102, 208.683, 176.763, 327.835,
     * 176.720, 446.553, 176.711, 565.677, 176.951, 684.485,
     * 176.726, 803.241, 176.943, 922.071, 177.037,1040.777,
     * 177.444,1159.667, 177.652,1219.015, 310.331,  30.342,
     * 309.840, 149.349, 309.760, 268.345, 309.636, 387.298,
     * 309.635, 505.960, 309.595, 625.100, 309.436, 744.006,
     * 309.651, 862.618, 309.475, 981.526, 309.993,1100.315,
     * 310.331,1219.240, 442.867,  30.286, 442.820,  90.119,
     * 442.762, 208.905, 442.528, 328.079, 442.466, 446.823,
     * 442.327, 565.313, 442.317, 684.424, 442.194, 803.198,
     * 442.515, 922.059, 442.641,1040.809, 442.839,1159.795,
     * 442.830,1219.189, 575.665,  30.505, 575.299, 149.573,
     * 575.253, 268.394, 575.104, 387.293, 575.115, 505.865,
     * 575.000, 625.000, 575.105, 744.026, 575.165, 862.782/
c
      DATA OS6B /        !Second half of coordinates for s/n 6
c
     * 575.272, 981.671, 574.959,1100.387, 575.804,1219.188,
     * 708.405,  30.519, 708.156,  90.189, 708.091, 208.926,
     * 708.039, 327.708, 708.094, 446.658, 707.974, 565.467,
     * 708.068, 684.584, 708.082, 803.490, 708.103, 922.166,
     * 707.737,1040.778, 707.966,1159.557, 708.319,1218.819,
     * 840.961,  30.599, 840.708, 149.595, 840.838, 268.258,
     * 840.724, 387.209, 840.644, 505.979, 840.694, 624.937,
     * 840.671, 743.881, 840.668, 862.543, 840.594, 980.987,
     * 840.708,1099.933, 841.030,1218.788, 973.877,  30.426,
     * 973.458,  90.373, 973.491, 209.172, 973.428, 327.733,
     * 973.435, 446.866, 973.394, 565.512, 973.548, 684.233,
     * 973.550, 803.201, 973.471, 921.861, 973.478,1040.916,
     * 973.112,1159.799, 973.968,1218.653,1106.913,  30.349,
     *1105.978, 149.493,1106.062, 268.240,1106.159, 387.390,
     *1106.190, 505.956,1106.201, 624.665,1106.300, 743.615,
     *1106.115, 862.621,1105.965, 981.838,1105.823,1100.562,
     *1106.749,1219.129/
c-----------------------
C  OBJECT SPACE COORDINATES FOR S/N 7  (L,S)
c
      DATA OS7A /         !First half of coordinates for s/n 7
c
     *  43.872,  29.778,  43.963, 149.589,  43.604, 268.910,
     *  43.764, 387.476,  43.626, 506.149,  43.694, 624.992,
     *  43.803, 744.143,  43.827, 862.859,  43.909, 981.875,
     *  43.969,1100.419,  43.890,1219.627, 176.619,  30.421,
     * 176.563,  90.287, 176.608, 208.844, 176.656, 328.186,
     * 176.688, 446.950, 176.777, 565.592, 176.678, 684.699,
     * 176.882, 803.634, 176.546, 922.471, 176.745,1041.386,
     * 176.774,1160.184, 177.074,1219.926, 309.951,  29.896,
     * 309.557, 149.340, 309.698, 268.325, 309.583, 387.553,
     * 309.723, 506.271, 309.558, 625.096, 309.589, 744.234,
     * 309.604, 862.925, 309.676, 982.057, 309.723,1100.785,
     * 310.036,1220.439, 442.489,  29.854, 442.243,  90.117,
     * 442.651, 208.930, 442.165, 328.322, 442.300, 446.938,
     * 442.376, 565.631, 442.310, 684.649, 442.309, 803.585,
     * 442.394, 922.362, 442.343,1041.461, 442.433,1160.192,
     * 442.597,1220.236, 575.313,  29.956, 574.853, 149.407,
     * 575.126, 268.302, 575.115, 387.455, 575.132, 506.195,
     * 575.000, 625.000, 575.128, 744.146, 575.165, 863.038/
c
      DATA OS7B /        !Second half of coordinates for s/n 7
c
     * 574.998, 981.984, 575.100,1100.807, 575.175,1220.150,
     * 707.736,  30.069, 707.648,  90.336, 707.721, 208.951,
     * 707.800, 327.992, 707.864, 446.806, 707.922, 565.646,
     * 707.897, 684.597, 707.835, 803.564, 707.906, 922.241,
     * 707.853,1041.406, 707.833,1160.039, 707.869,1220.128,
     * 840.287,  30.351, 840.279, 149.787, 840.642, 268.622,
     * 840.320, 387.630, 840.393, 506.188, 840.344, 624.939,
     * 840.374, 744.118, 840.222, 862.782, 839.783, 981.651,
     * 840.362,1100.574, 840.295,1220.125, 973.325,  30.596,
     * 973.003,  90.609, 973.001, 209.054, 973.143, 327.981,
     * 973.048, 446.804, 973.034, 565.610, 972.682, 684.341,
     * 973.078, 803.452, 972.930, 922.062, 973.051,1041.296,
     * 972.938,1159.860, 972.992,1220.110,1106.081,  30.514,
     *1105.663, 149.764,1105.630, 268.396,1105.692, 387.437,
     *1105.774, 506.243,1105.662, 624.974,1105.273, 744.195,
     *1105.645, 862.668,1105.618, 981.619,1105.000,1100.400,
     *1105.631,1219.912/
c-----------------------
C  OBJECT SPACE COORDINATES FOR S/N 8  (L,S)
c
      DATA OS8A /         !First half of coordinates for s/n 8
c
     *  43.852,  30.093,  44.106, 148.877,  43.887, 268.209,
     *  43.787, 387.288,  43.904, 506.074,  43.746, 625.065,
     *  43.556, 744.246,  43.829, 862.991,  43.945, 981.924,
     *  44.180,1100.764,  44.337,1220.333, 177.166,  30.062,
     * 177.279,  89.487, 176.920, 208.586, 176.521, 327.883,
     * 176.098, 446.685, 176.094, 565.399, 176.191, 684.589,
     * 176.131, 803.572, 176.414, 922.592, 176.595,1041.531,
     * 177.044,1160.402, 177.447,1219.905, 309.972,  30.026,
     * 309.812, 148.937, 309.194, 267.848, 309.110, 387.044,
     * 308.952, 505.857, 309.021, 624.906, 309.192, 744.176,
     * 309.135, 862.903, 309.416, 982.140, 309.864,1100.952,
     * 310.287,1220.129, 442.800,  29.932, 442.523,  89.563,
     * 442.404, 208.465, 441.885, 327.534, 441.922, 446.419,
     * 442.233, 565.654, 442.094, 684.674, 441.931, 803.595,
     * 442.009, 922.387, 442.046,1041.450, 442.369,1160.410,
     * 442.820,1220.175, 575.597,  30.298, 575.561, 149.143,
     * 575.382, 268.157, 574.837, 387.025, 574.854, 505.977,
     * 575.000, 625.000, 574.840, 744.058, 575.130, 863.009/
c
      DATA OS8B /       !Second half of coordinates for s/n 8
c
     * 575.090, 982.037, 575.312,1100.951, 575.702,1220.061,
     * 708.374,  30.650, 708.255,  89.936, 708.357, 208.795,
     * 708.178, 327.842, 707.970, 446.483, 708.034, 565.541,
     * 707.966, 684.661, 707.943, 803.584, 708.055, 922.331,
     * 708.223,1041.439, 708.469,1160.228, 708.524,1219.790,
     * 841.106,  30.772, 840.889, 149.202, 840.955, 268.243,
     * 840.944, 387.264, 840.825, 506.072, 840.784, 624.985,
     * 840.974, 744.166, 840.725, 862.894, 840.854, 981.837,
     * 840.965,1100.649, 841.066,1219.487, 974.009,  30.782,
     * 973.815,  90.133, 974.180, 208.987, 974.104, 328.174,
     * 973.765, 446.862, 973.831, 565.706, 973.812, 684.658,
     * 973.762, 803.410, 973.856, 922.351, 974.052,1041.128,
     * 973.984,1159.959, 974.016,1219.425,1106.427,  31.247,
     *1106.561, 149.616,1106.545, 268.457,1106.587, 387.428,
     *1106.679, 506.205,1106.587, 625.070,1106.529, 743.957,
     *1106.562, 862.567,1106.638, 981.516,1106.624,1100.319,
     *1106.891,1219.303/
c
c-----------------
c
      IF ((ICAM.EQ.4).OR.(ICAM.EQ.6).OR.(ICAM.EQ.7).OR.(ICAM.EQ.8)) THEN
	 CALL MVE(7,206,ALOC(1,1,ICAM),LOC,1,1)
      ELSE
	 CALL PRNT(4,1,ICAM,' ***Invalid camera S/N=.')
         CALL ABEND
      ENDIF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zvoos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of VOOS                                         */
/************************************************************************/

void zvoos(icam,oloc)
 int    icam;  /* Input VO camera serial number. */
 void  *oloc;  /* Output object space reseau locations stored
		  as (line,sample) pairs. */

{
FTN_NAME(voos)(&icam,oloc);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create voos.imake
/* Imake file for VICAR subroutine VOOS */

#define SUBROUTINE voos

#define MODULE_LIST voos.f zvoos.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tvoos.f
C--------------------------------------------------------------
C THIS IS A TEST OF MODULE VOOS
C 
C PORTED TO UNIX 8/24/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c      
      real*4 loc(2,103)
c
      call xvmessage('**************fortran callable***********',' ')
c
c  test 1
c
      call voos(4,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-1B:')
c
c  test 2
c
      call voos(6,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2B:')
c
c  test 3
c
      call voos(7,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-1A:')
c
c  test 4
c
      call voos(8,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2A:')
c
c  test 5 - test C version
c
      call xvmessage('**************C callable***********',' ')
c
      call tzvoos(8,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2A:')
c
c  test 6 - incorrect ICAM
c
      call xvmessage('Should call ABEND',' ')
      call  tzvoos(2,loc)
c
      return
      end
$!-----------------------------------------------------------------------------
$ create tzvoos.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVOOS.F */
/************************************************************************/

void FTN_NAME(tzvoos)(icam,oloc)
 int   *icam;  /* Input VO camera serial number. */
 void  *oloc;  /* Output object space reseau locations stored
		  as (line,sample) pairs. */

{
       zvoos(*icam,oloc);
}
$!-----------------------------------------------------------------------------
$ create tvoos.imake
/* Imake file for Test of VICAR subroutine voos */

#define PROGRAM tvoos

#define MODULE_LIST tvoos.f tzvoos.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
$!-----------------------------------------------------------------------------
$ create tvoos.pdf
!*****************************************************************************
! TVOOS.PDF - pdf for test program TVOOS.F for the subroutine VOOS
!*****************************************************************************
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstvoos.pdf
!****************************************************************************
! TSTVOOS.PDF, unit test procedure for subroutine VOOS.F
!
!THIS IS A TEST OF MODULE VOOS
! note to testers:  Due to rounding practices, the fortran output might 
!                   differ amongst the VAX, SGI, SUN, and HP operating systems.
!                   However a difference of 1 in the last digit of a floating
!                   point number is acceptable.
!****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

tvoos

end-proc
.title TSTVOOS.PDF - unit test for subroutine VOOS
.end
$ Return
$!#############################################################################
$Other_File:
$ create voos.hlp
1 VOOS

  VOOS returns the object space reseau locations for a specified Viking
  Orbiter (VO) camera.

  Fortran Calling Sequence:  CALL VOOS(ICAM,OLOC)

      INTEGER*4 ICAM       Input VO camera serial number.
      REAL*4 OLOC(2,103)   Output object space reseau locations stored
		           as (line,sample) pairs.

  C Calling Sequence:  zvoos(icam,oloc);

      int icam;            Input VO camera serial number.
      float oloc[102][1];  Output object space reseau locations stored
		           as (line,sample) pairs.

  The Viking Orbiter camera serial numbers are:
	7=VO-1A		8 = VO-2A
	4=VO-1B		6 = VO-2B
	
2 History

  Original Programmer: Gary Yagi, 15 May 1990
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN
  Revisions: 
	1) FFM ...11/27/91... Renamed TEST PDF, also modified TEST PDF
                                (FR 63843)
	2) TTM ...08/24/93... Ported to UNIX
$ Return
$!#############################################################################
