$!****************************************************************************
$!
$! Build proc for MIPL module reslocvo
$! VPACK Version 1.7, Thursday, June 09, 1994, 10:50:36
$!
$! Execute by entering:		$ @reslocvo
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
$ write sys$output "*** module reslocvo ***"
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
$ write sys$output "Invalid argument given to reslocvo.com file -- ", primary
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
$   if F$SEARCH("reslocvo.imake") .nes. ""
$   then
$      vimake reslocvo
$      purge reslocvo.bld
$   else
$      if F$SEARCH("reslocvo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake reslocvo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @reslocvo.bld "STD"
$   else
$      @reslocvo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create reslocvo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack reslocvo.com -
	-s reslocvo.f -
	-i reslocvo.imake -
	-p reslocvo.pdf -
	-t tstreslcv.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create reslocvo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR PROGRAM RESLOCVO
C Finds reseau locations on Viking Orbiter images.
C
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    JANUARY 1985
C  FOR
C      MIPL DEVELOPMENT

C  Derived from original RESLOC and parts of RLGT by
C      Gary Yagi with modifications by Charlie Avis

C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR EXECUTIVE       FORTRAN-77
C  REVISION HISTORY
C      5-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C     
C  VICAR Program to locate reseaux on Viking Orbiter pictures
C  Use "@RESLOCVO DOC" to generate Programmer's Guide.
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C8/PTS(4,103),LOC2(2,103),DUMMY1
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,103),LOC(2,103)

      REAL*8 COEF(20),MOM(28),UMOM(20),A(100)
      CHARACTER*32 PARMFILE		!Output GEOMA parameter filename
      INTEGER IWORK(1)
      BYTE DUMMY1(77940)
      character*32 XWORK(1)

      EQUIVALENCE (PTS,IWORK)

      CALL IFMESSAGE ('RESLOCVO version 01-JULY-94')
      CALL XVEACTION('SA',' ')

      CALL XVUNIT(IUNIT,'INP',1,IND,' ')

C           Open input image and determine size...
      CALL XVOPEN(IUNIT,IND,'U_FORMAT','HALF',' ')
      CALL RESPAR(IUNIT,*990)			!Process user parameters...
      CALL XVGET(IUNIT,IND,'NL',NLI,'NS',NSI,' ')

      SL = 1
      SSAMP = 1
      NL = NLI
      NS = NSI
      CALL XVP('INP',XWORK,NI)		!Get number of inputs
      IF(NI.GT.1) THEN			!Get nominals from second input
         CALL XVUNIT(NOMUNIT,'INP',2,IND,' ')
         CALL XVOPEN(NOMUNIT,IND,' ')
         CALL XVREAD(NOMUNIT,LOC,IND,' ')
      ELSE
         CALL GETRES(LOC,ICAM)		!Get nominals from built-in tables
      ENDIF

      NRES = 103			!Total # of reseau marks on camera
      CALL MVE(7,2*NRES,-99.,OLOC,0,1)
      IF(DBUG.EQ.1) THEN
          CALL XVMESSAGE('Nominal locations',' ')
          CALL PU75(LOC,1)
      ENDIF

      CALL RCOR(IUNIT,*990)			!Find reseau by correlation
      CALL RESFIT(OLOC,LOC,LOC2,PTS,COEF,MOM,UMOM,A,IFIT,TOL,DBUG)
      IF(PRINT.EQ.1) THEN
          CALL XVMESSAGE('Output reseau locations',' ')
          CALL PU75(OLOC,1)
      ENDIF

C          Write reslocs in first output file
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUNIT,IND,'OP','WRITE',
     &   'U_FORMAT','REAL','O_FORMAT','REAL',
     &   'U_NL',1,'U_NS',2*NRES,' ')
      CALL XLADD(OUNIT,'HISTORY','RESLOCVO','RESLOCVO COORDINATES',
     &   IND,'FORMAT','STRING',' ')
      CALL XVWRIT(OUNIT,OLOC,IND,'NSAMPS',2*NRES,' ')
      CALL XVCLOSE(OUNIT,IND,' ')

C          Output GEOMA parameters...
      CALL XVPARM('GEOPAR',PARMFILE,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
          CALL GEOPAR(OLOC,IWORK,NRES,ICAM)
          CALL XVPOPEN(IND,3,800,PARMFILE,'SA',IUOUT)
          CALL XVPOUT(IND,'NAH',21,'INT',1)
          CALL XVPOUT(IND,'NAV',8,'INT',1)
          CALL XVPOUT(IND,'TIEPOINT',IWORK,'REAL',792)
          CALL XVPCLOSE(IND)
      ENDIF

      CALL XVMESSAGE('RESLOCVO task completed',' ')
      RETURN

C          UNEXPECTED EOF
  990 CALL XVMESSAGE('**RESLOCVO task cancelled',' ')
      CALL ABEND
      END

C********************************************C
C RESLOCVO parameter processor subroutine    C
C********************************************C
      SUBROUTINE RESPAR(IUNIT,*)
      IMPLICIT INTEGER(A-Z)

      COMMON/CP/IBUG
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/CPAR/NLW,NSW,NHOR,NVER,INTERP,SIGMA,CTHRE
      REAL*4 SIGMA,CTHRE,TOL
      INTEGER APAR(40)

      IBUG = 0
C           Get frame number and camera serial number...
      IFRM = 0
      CALL VOLABV2(IND,IUNIT,APAR)
      IF (IND.EQ.20)  GOTO 1          ! NOT VIKING LABEL.
      IF (IND.NE.0)  THEN
          CALL XVMESSAGE('***Error reading input label',' ')
          GOTO 990
      END IF
      IFRM = APAR(5)
      ICAM = APAR(2)
    1 CALL XVPARM('CAMERA',IVAL,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) ICAM=IVAL
      CALL XVPARM('FRAME',IVAL,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) IFRM=IVAL
      IF (ICAM.EQ.5) GOTO 981

C        Correlation parameters...
      CALL XVP('NLW',NLW,ICOUNT)	!Size of correlation window
      CALL XVP('NSW',NSW,ICOUNT)	!is NLW x NSW
      CALL XVP('NVER',NVER,ICOUNT)	!Search area is NVER x NHOR
      CALL XVP('NHOR',NHOR,ICOUNT)
      CALL XVP('SIGMA',SIGMA,ICOUNT)	!Sigma of Gaussian shape function
      CALL XVP('CTHRESH',CTHRE,ICOUNT)  !Correlation threshold
      INTERP = 1 - XVPTST('NOIN')	!Find mark to sub-pixel accuracy
      CALL XVP('TOLER',TOL,ICOUNT)	!Maximum pixel distance from nominal

      CALL XVP('FIT',FIT,ICOUNT)	!Constraints applied to linear fit

C         Controls diagnostic printout...
      DBUG = XVPTST('DBUG')
      PRINT = XVPTST('PRINT')
      IF (DBUG.EQ.1) THEN
          PRINT=1
          CALL PRNT(4,11,IFRM,' IFRM=.')
      ENDIF
      RETURN

  981 CALL XVMESSAGE('***INVALID CAMERA S/N',' ')
  990 RETURN1
      END

C*********************************************************C
C RESLOCVO returns nominal reseau locations subroutine    C
C*********************************************************C
C
C Returns nominal reseau locations in CLOCS for camera ICAM.
C
      SUBROUTINE GETRES(CLOCS,ICAM)
      REAL*4 CLOCS(2,103)
      REAL*4 CLACS(2,103,4)
      EQUIVALENCE (CLACS,CL1),(CLACS(1,1,2),CL2),(CLACS(1,1,3),CL3)
      EQUIVALENCE (CLACS(1,1,4),CL4)
      EQUIVALENCE (CLACS(1,52,1),KL1),(CLACS(1,52,2),KL2)
      EQUIVALENCE (CLACS(1,52,3),KL3),(CLACS(1,52,4),KL4)

      REAL*4 CL1(2,51)
      REAL*4 KL1(2,52)
      REAL*4 CL2(2,51)
      REAL*4 KL2(2,52)
      REAL*4 CL3(2,51)
      REAL*4 KL3(2,52)
      REAL*4 CL4(2,51)
      REAL*4 KL4(2,52)


C  S/N 7 NOMINAL RESEAU LOCATIONS (LINE,SAMP)
      DATA CL1/
     *   4.312,  18.436,   7.794, 136.913,  10.607, 254.690,
     *  12.833, 371.908,  14.699, 488.553,  16.550, 604.858,
     *  18.204, 721.033,  19.683, 837.399,  21.216, 953.746,
     *  22.474,1070.401,  23.243,1187.142, 131.762,  20.187,
     * 133.505,  79.281, 136.644, 197.074, 139.212, 314.177,
     * 141.427, 430.760, 143.221, 547.136, 144.872, 663.264,
     * 146.585, 779.255, 148.269, 895.179, 149.731,1011.269,
     * 151.270,1127.364, 151.893,1185.693, 260.414,  20.438,
     * 263.552, 139.422, 266.276, 256.475, 268.487, 373.219,
     * 270.429, 489.542, 272.176, 605.431, 273.951, 721.241,
     * 275.656, 837.006, 277.411, 952.772, 278.936,1068.664,
     * 280.744,1184.744, 389.065,  21.189, 390.612,  81.484,
     * 393.528, 198.751, 395.770, 315.526, 397.935, 431.701,
     * 399.737, 547.692, 401.502, 663.469, 403.305, 778.991,
     * 405.011, 894.607, 406.563,1010.362, 408.432,1125.900,
     * 409.195,1184.294, 518.218,  22.140, 520.725, 140.728,
     * 523.336, 257.473, 525.589, 373.626, 527.610, 489.464/

      DATA KL1/
     * 529.081, 605.348, 530.787, 720.817, 532.458, 836.481,
     * 534.211, 951.921, 535.754,1067.561, 537.646,1183.645,
     * 646.869,  22.291, 648.279,  82.354, 650.871, 199.289,
     * 653.205, 315.584, 655.233, 431.490, 656.805, 547.245,
     * 658.477, 662.617, 660.178, 778.076, 661.740, 893.451,
     * 663.340,1009.113, 665.030,1124.593, 665.997,1182.896,
     * 776.021,  22.241, 778.534, 140.508, 780.973, 257.006,
     * 782.942, 372.975, 784.638, 488.632, 786.390, 604.040,
     * 787.904, 719.481, 789.277, 834.863, 790.430, 950.498,
     * 792.460,1066.019, 794.347,1182.147, 906.174,  21.692,
     * 906.672,  81.131, 909.003, 198.109, 911.022, 314.263,
     * 912.697, 429.923, 914.325, 545.388, 915.407, 660.459,
     * 917.155, 776.122, 918.250, 891.464, 920.036,1007.261,
     * 921.593,1122.753, 922.798,1181.398,1035.325,  19.842,
     *1036.847, 138.202,1038.637, 254.697,1040.461, 370.685,
     *1042.106, 486.313,1043.260, 601.528,1044.438, 716.742,
     *1045.858, 832.334,1047.211, 947.818,1048.061,1063.639,
     *1050.948,1180.449/


C  S/N 4 NOMINAL RESEAU LOCATIONS (LINE,SAMP)
      DATA CL2/
     *   4.541,  23.839,   6.040, 141.605,   7.811, 258.683,
     *   8.999, 375.354,   9.966, 491.239,  10.811, 606.899,
     *  11.837, 722.627,  12.539, 838.622,  13.582, 953.977,
     *  14.466,1070.015,  14.690,1185.448, 132.679,  24.464,
     * 133.654,  83.602, 135.500, 200.538, 136.842, 317.027,
     * 137.930, 433.212, 138.866, 548.968, 139.888, 664.750,
     * 140.660, 779.973, 141.014, 896.120, 141.877,1010.875,
     * 143.435,1126.529, 143.528,1183.472, 262.719,  24.787,
     * 263.933, 142.421, 265.805, 258.976, 266.941, 375.133,
     * 267.969, 491.628, 268.648, 606.878, 269.406, 722.465,
     * 270.022, 837.624, 270.640, 953.135, 271.937,1067.969,
     * 273.267,1183.096, 392.758,  25.111, 392.893,  83.845,
     * 394.717, 200.829, 395.836, 317.387, 397.045, 433.222,
     * 397.739, 549.068, 398.578, 664.596, 399.468, 779.448,
     * 400.047, 894.803, 400.577,1010.031, 401.685,1124.783,
     * 402.805,1182.321, 522.797,  25.235, 523.535, 142.559,
     * 524.813, 259.157, 525.896, 375.214, 526.823, 490.921/

      DATA KL2/
     * 527.476, 606.646, 528.280, 721.882, 528.886, 836.787,
     * 529.712, 952.007, 530.494,1067.002, 532.344,1181.745,
     * 652.036,  24.960, 652.407,  83.866, 653.782, 200.806,
     * 655.092, 316.888, 655.972, 432.825, 656.667, 548.190,
     * 657.429, 663.610, 658.204, 778.524, 659.004, 893.648,
     * 659.770,1008.665, 661.017,1123.651, 661.883,1181.069,
     * 781.375,  24.584, 782.721, 141.867, 784.099, 258.321,
     * 785.156, 374.102, 786.006, 489.824, 786.647, 605.039,
     * 787.531, 719.858, 788.125, 834.995, 788.820, 950.527,
     * 790.011,1065.549, 791.422,1180.294, 911.414,  23.708,
     * 911.710,  82.758, 912.972, 199.603, 914.281, 315.606,
     * 915.257, 431.091, 915.955, 546.542, 916.729, 661.707,
     * 917.706, 776.266, 918.176, 891.524, 918.865,1006.676,
     * 920.243,1121.643, 921.260,1179.417,1040.452,  22.733,
     *1041.492, 140.045,1042.561, 256.557,1043.581, 372.511,
     *1044.578, 487.683,1045.400, 602.577,1045.828, 717.782,
     *1046.570, 832.847,1047.281, 947.745,1048.362,1063.462,
     *1050.299,1178.742/

C  S/N 8 NOMINAL RESEAU LOCATIONS (LINE,SAMP)
      DATA CL3/
     *   2.407,  25.276,   6.107, 142.049,   9.145, 258.721,
     *  11.659, 374.948,  13.761, 490.764,  15.447, 606.469,
     *  17.021, 721.952,  18.468, 837.585,  19.780, 953.109,
     *  20.851,1068.644,  21.626,1184.167, 130.989,  25.397,
     * 132.102,  83.831, 135.219, 200.396, 137.794, 316.489,
     * 139.593, 432.203, 141.525, 547.548, 143.205, 662.966,
     * 144.560, 778.418, 146.181, 893.573, 147.484,1008.999,
     * 148.792,1123.948, 150.105,1181.386, 259.570,  25.418,
     * 262.550, 142.296, 264.629, 258.078, 266.892, 373.810,
     * 268.747, 489.201, 270.602, 604.484, 272.426, 719.751,
     * 273.548, 834.835, 274.962, 950.130, 276.629,1065.218,
     * 278.186,1179.507, 388.952,  25.539, 390.139,  84.200,
     * 392.734, 200.372, 394.517, 315.792, 396.602, 431.346,
     * 398.438, 546.463, 399.851, 661.622, 401.498, 776.617,
     * 402.717, 891.694, 404.289,1006.765, 405.867,1121.563,
     * 407.167,1178.727, 517.734,  25.961, 520.386, 142.313,
     * 522.521, 257.909, 524.300, 373.465, 526.010, 488.685/

      DATA KL3/
     * 527.675, 603.809, 529.057, 718.676, 530.625, 833.861,
     * 531.950, 948.775, 533.668,1063.694, 535.848,1178.648,
     * 646.915,  25.581, 648.018,  83.943, 650.438, 200.180,
     * 652.318, 315.799, 653.791, 430.974, 655.579, 546.085,
     * 656.838, 661.030, 658.480, 775.938, 659.707, 890.877,
     * 661.472,1005.940, 663.189,1120.676, 664.429,1177.468,
     * 775.897,  25.102, 778.041, 141.555, 780.219, 257.504,
     * 781.768, 372.943, 783.466, 488.140, 784.682, 603.148,
     * 786.174, 718.111, 787.605, 833.090, 788.881, 948.126,
     * 790.663,1063.022, 792.510,1177.290, 904.978,  24.222,
     * 906.042,  82.772, 908.006, 199.020, 909.652, 314.651,
     * 911.318, 430.123, 912.701, 545.201, 913.945, 660.207,
     * 915.328, 775.146, 916.676, 890.263, 918.205,1005.254,
     * 919.800,1120.029, 920.591,1177.111,1033.759,  23.343,
     *1035.471, 139.952,1037.008, 255.971,1038.517, 371.500,
     *1040.023, 486.840,1041.361, 601.883,1042.582, 716.880,
     *1043.720, 831.897,1044.958, 946.995,1046.377,1062.003,
     *1048.671,1176.032/
     *

C  S/N 6 NOMINAL RESEAU LOCATIONS (LINE,SAMP)
      DATA CL4/
     *   5.275,  22.742,   7.288, 140.696,   8.703, 257.307,
     *   9.924, 373.137,  11.265, 488.854,  12.283, 604.528,
     *  13.641, 719.816,  15.477, 835.627,  17.016, 951.923,
     *  18.559,1068.416,  20.206,1184.980, 135.380,  23.041,
     * 135.554,  82.308, 136.994, 198.624, 138.007, 314.823,
     * 139.220, 430.659, 140.381, 546.502, 141.815, 661.889,
     * 142.705, 777.654, 144.119, 893.774, 145.727,1009.702,
     * 147.812,1125.903, 149.109,1183.680, 265.485,  23.541,
     * 266.145, 140.635, 267.349, 257.052, 268.289, 373.044,
     * 269.209, 488.703, 270.101, 604.618, 271.002, 720.437,
     * 272.498, 836.243, 273.633, 951.968, 275.784,1068.368,
     * 278.213,1184.080, 394.889,  23.840, 395.322,  82.356,
     * 396.593, 199.489, 397.533, 315.345, 398.343, 431.689,
     * 398.821, 546.838, 399.725, 662.699, 400.677, 778.657,
     * 401.901, 894.654, 403.721,1010.792, 405.660,1126.836,
     * 406.816,1184.480, 524.694,  24.140, 525.594, 141.415,
     * 526.471, 257.519, 527.000, 373.659, 527.775, 489.552/

      DATA KL4/
     * 528.229, 605.500, 529.139, 721.497, 530.118, 837.500,
     * 531.429, 953.349, 532.759,1069.520, 535.420,1185.081,
     * 654.298,  24.239, 654.566,  82.557, 655.393, 199.621,
     * 655.937, 315.956, 656.584, 432.006, 656.963, 547.981,
     * 657.840, 663.841, 658.600, 780.035, 659.720, 895.728,
     * 660.599,1011.541, 662.551,1127.829, 664.024,1185.281,
     * 783.902,  23.839, 784.455, 141.576, 784.893, 257.734,
     * 785.384, 374.022, 785.823, 490.158, 786.297, 606.088,
     * 786.942, 721.982, 787.810, 838.024, 788.807, 954.068,
     * 790.338,1070.266, 792.628,1185.981, 913.806,  23.138,
     * 913.643,  82.273, 913.972, 199.469, 914.383, 315.713,
     * 914.694, 432.222, 915.000, 548.225, 915.683, 664.133,
     * 916.361, 780.386, 917.183, 896.402, 918.472,1012.549,
     * 919.903,1128.780, 921.732,1186.482,1043.110,  21.938,
     *1042.724, 139.748,1042.943, 256.948,1043.306, 373.669,
     *1043.547, 489.401,1043.911, 605.700,1044.561, 721.552,
     *1045.151, 837.932,1046.127, 954.443,1047.691,1070.930,
     *1050.837,1187.382/

      NRES = 103
      ICAMI=1
      IF (ICAM .EQ. 4)  ICAMI=2
      IF (ICAM .EQ. 8)  ICAMI=3
      IF (ICAM .EQ. 6)  ICAMI=4
      CALL MVE(7,2*NRES,CLACS(1,1,ICAMI),CLOCS,1,1)
      RETURN
      END

C********************************************C
C RESLOCVO correlation subroutine            C
C********************************************C
C
      SUBROUTINE RCOR(IUNIT,*)
      IMPLICIT INTEGER(A-V)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C8/ILINE(2,103),WORK(80000)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,103),LOC(2,103)
      COMMON/CPAR/NLW,NSW,NHOR,NVER,INTERP,SIGMA,CTHRE
 
      INTEGER*4 MAXBUF/80000/
      INTEGER*2 ILINE
      REAL*4 LOC,OLOC
      CHARACTER*132 MSG
      BYTE WORK


      CALL ZIA(WORK,MAXBUF/4)
      NLIC = NVER + NLW - 1
      NSIC = NHOR + NSW - 1
C          GET INITIAL LINE (ILINE) OF EACH CORRELATION AREA WHILE
C        INSURING THAT THE AREA REMAINS WITHIN THE PICTURE
      DO 40 LRES=1,NRES
      L = LOC(1,LRES) - NLIC/2 + .5
      L = MAX0(L,1)
      L = MIN0(L,NLI-NLIC+1)
      ILINE(1,LRES) = L
      ILINE(2,LRES) = LRES
   40 CONTINUE

C          ELEMINATE MARKS ALONG EDGES OF PICTURE
      DO 41 LRES=1,11
      ILINE(1,LRES) = NLI
      ILINE(1,LRES+92) = NLI
   41 CONTINUE

      DO LRES= 12,81,23
      ILINE(1,LRES) = NLI
      ILINE(1,LRES+11) = NLI       
      ILINE(1,LRES+12) = NLI
      ILINE(1,LRES+22) = NLI
      END DO 

      CALL SORTX(ILINE,NRES)

      WRITE (MSG,9900) NLW,NSW,NHOR,NVER,INTERP
9900  FORMAT (' NLW=',I2,' NSW=',I2,' NHOR=',I2,' NVER=',I2,' INTERP=',
     +I1)
      CALL XVMESSAGE(MSG(2:39),' ')
C          ASSIGN WORK SPACE
      I = 1
      IRHO = I
      I = I + 4*NHOR*NVER
      IAP = I
      I = I + 4*NSW*NLW
      IS2 = I
      I = I + 4*NSIC
      IS = I
      I = I + 2*NSIC
      IB = I
      I = I + 2*NSI*NLIC
      IA = I
      I = I + 2*NSW*NLW
      
      WRITE(MSG,9990) I
9990  FORMAT('TOT BUF=     ',I7)
      CALL XVMESSAGE(MSG,' ')

C      CALL PRNT(4,1,I,' TOT BUF=.')

      IF(I.GT.MAXBUF) GOTO 990
 
      CALL RCOR3(IUNIT,WORK(IA),WORK(IB),WORK(IS),WORK(IS2),WORK(IAP),
     &WORK(IRHO),NHOR,NVER,NLW,NSW,NSI,SIGMA,CTHRE,INTERP,*999)
      CALL FILLOC(LOC,OLOC)

      RETURN



  990 CALL XVMESSAGE('BUFFER REQUIREMENTS EXCEED CORE ALLOCATION',' ')
      CALL XVMESSAGE('TRY REDUCING NVER, NHOR, NLW, OR NSW',' ')
  999 RETURN1

      END

C********************************************C
C RESLOCVO halfword sort subroutine          C
C********************************************C
	SUBROUTINE SORTX(BUF,N)
C
C
	INTEGER*2 BUF(2,103)
        INTEGER*2 BUFVAL1(103), BUFVAL2(103)
        INTEGER*4 BUFVAL3(103), BUFNDX(103)        

        DO 90055 II=1,103
        BUFVAL3(II) = ((BUF(1,II) * 32768) + BUF(2,II))
        BUFVAL2(II) = BUF(2,II)
        BUFVAL1(II) = BUF(1,II)
        BUFNDX(II)  = II
90055   CONTINUE


	CALL ISORTP(BUFVAL3,1,103,BUFNDX)

        DO  90065 II=1,103
        JJ = BUFNDX(II)
        BUF(1,II) = BUFVAL1(JJ)
        BUF(2,II) = BUFVAL2(JJ)
90065   CONTINUE



	RETURN
	END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE RCOR3(IUNIT,A,B,JSUM,JMOM,AP,R,NHOR,NVER,NLW,NSW,NSIN,
     &          SIGMA,CTHRE,INTERP,*)
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C8/ILINE(2,103),WORK(80000)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,103),LOC(2,103)
      INTEGER   IUNIT
      INTEGER*2 A(NSW,NLW),B(NSIN,1),JSUM(1),ILINE
      INTEGER*4 JMOM(1)
      REAL*4 AP(NSW,NLW),R(NHOR,NVER),RMAX,SCALE,CTHRE,CMIN,CSUM
      REAL*4 LOC,OLOC,X,Y,DX,DY
      CHARACTER*132 MSG
      CHARACTER*132 MSG1
      CHARACTER*132 MSG2
      CHARACTER*132 MSG3
      BYTE WORK
      
      MSG = '(SL,SS)=(***,***)'
      MSG1= 'RES MARK *** CANNOT BE FOUND'
      MSG2= 'CORRELATION MATRIX FOR MARK *** (****.*,****.*)'
      MSG3= 'RES MARK *** CORR=0.**'


      IF(DBUG.EQ.1) CALL XVMESSAGE('RCOR3 HERE',' ')
      NLIC = NVER + NLW - 1
      NSIC = NHOR + NSW - 1
      NCOR = NHOR*NVER
      NLWH = NLW/2
      NSWH = NSW/2
      CALL GAUSS(0.,0.,SIGMA,A,NLW,NSW)
      CALL OPCON(NLW,NSW)
      CALL DTREND(A,AP,NLW,NSW)
      IF(DBUG.EQ.0) GOTO 8
      DO 3 J=1,NLW
    3 CALL PRNT(2,NSW,A(1,J),'0.')
      DO 6 J=1,NLW
    6 CALL PRNT(7,NSW,AP(1,J),' .')
    8 CONTINUE
      KOUNT = 0
      CSUM = 0.
      CMIN = 1.
      BI = 0
      IRES = 0
      LLIC = 0
C          LLIC = LAST LINE IN CORE

   10 IRES = IRES + 1

      L = ILINE(1,IRES)


      IF(L.EQ.NLI) GOTO 100
      LRES = ILINE(2,IRES)
      BLINE = L
      ELINE = L + NLIC - 1
      IF(ELINE.EQ.LLIC) GOTO 15
      IF(BLINE.LE.LLIC) BLINE=LLIC+1

C          READ IN REQUIRED LINES
      DO 12 LLIC=BLINE,ELINE
         BI = MOD(BI,NLIC) + 1
         CALL XVREAD(IUNIT,B(1,BI),IND,'LINE',LLIC,' ')
12    CONTINUE

      LLIC = ELINE

C          GET LOCATION OF LEFTMOST SAMPLE IN CORRELATION AREA
   15 S = LOC(2,LRES) - NSIC/2 + .5
      BJ = MOD(BI,NLIC) + 1
      BK = MOD(BJ+NLW-2,NLIC) + 1

C          COMPUTE VERTICAL MOMENTS
      CALL MVE(2,NSIC,0,JSUM,0,1,1)
      CALL ZIA(JMOM,NSIC)
      J = BJ

      DO 17 K=1,NLW
      CALL MOMGEN(B(S,J),JSUM,JMOM,K,NSIC)
   17 J = MOD(J,NLIC) + 1

C     IF(DBUG.EQ.0) GOTO 19
C     CALL OUTCON(L,MSG(13),3)
C     CALL OUTCON(S,MSG(17),3)
C     CALL QPRINT(MSG,18)
C     J = BJ
C     DO 18 I=1,NLIC
C     CALL PRNT2(2,NSIC,B(S,J),'0.')
C  18 J = MOD(J,NLIC) + 1
C     CALL PRNT2(2,NSIC,JSUM,' JSUM=.')
C     CALL PRNT2(4,NSIC,JMOM,' JMOM=.')
C  19 CONTINUE
      CALL ZIA(R,NCOR)
      I = BK
      J = BJ

C          COMPUTE CORRELATION MATRIX R(NHOR,NVER)
      DO 40 JJ=1,NVER
      CALL CONVLD(AP,B(S,1),NLW,NSW,JSUM,JMOM,J,R(1,JJ),NHOR,NSIN,NLIC)
      IF(JJ.EQ.NVER) GOTO 40
C          UPDATE VERTICAL MOMENTS
      I = MOD(I,NLIC) + 1
      CALL UPMOMV(B(S,I),B(S,J),JSUM,JMOM,NSIC,NLW)
      J = MOD(J,NLIC) + 1
      IF(DBUG.EQ.1) THEN
         CALL PRNT(2,NSIC,B(S,I),'0.')
         CALL PRNT(2,NSIC,JSUM,' JSUM=.')
         CALL PRNT(4,NSIC,JMOM,' JMOM=.')
      ENDIF
   40 CONTINUE

      CALL MAXR(R,NCOR,RMAX,I)
      IF(RMAX.LT.CMIN) CMIN=RMAX
      KOUNT = KOUNT + 1
      CSUM = CSUM + RMAX

      IF(DBUG.EQ.0) GOTO 42
      WRITE (MSG3,9910) LRES,RMAX
9910  FORMAT (' RES MARK ',I3,' CORR=',F4.2)
      CALL XVMESSAGE(MSG3(2:23),' ')

   42 IF(RMAX.LT.CTHRE) GOTO 992
      K = MOD(I-1,NHOR) + 1
      J = (I-1)/NHOR + 1
      DX = K
      DY = J
      IF(INTERP.EQ.0) GOTO 60
      IF(K.EQ.1.OR.J.EQ.1) GOTO 990
      IF(K.EQ.NHOR.OR.J.EQ.NVER) GOTO 990
      CALL QXLOC(R(K-1,J-1),X,Y,NHOR,*55)
      IF(ABS(X).GT.1.OR.ABS(Y).GT.1.) GOTO 55
      DX = DX + X
      DY = DY + Y
      GOTO 60
   55 CALL XVMESSAGE('FIT ERROR',' ')

   60 DY = L + DY + (NLWH-1)
      DX = S + DX + (NSWH-1)
      OLOC(1,LRES) = DY
      OLOC(2,LRES) = DX
      GOTO 10

  100 WRITE(MSG,105) CMIN
  105 FORMAT('MINIMUM CORRELATION VALUE=  ',F12.8)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CSUM/KOUNT
  110 FORMAT('AVERAGE VALUE=    ',F12.8)
      CALL XVMESSAGE(MSG,' ')

      RETURN

990   WRITE (MSG1(11:13),'(I3)') LRES
      CALL XVMESSAGE(MSG1(2:29),' ')
      CALL XVMESSAGE('CORRELATION MAX LIES ON MARGIN OF SEARCH AREA',
     X   ' ')
      GOTO 998

992   WRITE (MSG1(11:13),'(I3)') LRES
      CALL XVMESSAGE(MSG1(2:29),' ')
      CALL PRNT(7,1,RMAX,' CORRELATION MAX TOO LOW=.')

  998 IF(DBUG.EQ.0) GOTO 10
      WRITE (MSG2,9920) LRES,DY,DX
9920  FORMAT ('CORRELATION MATRIX FOR MARK ',I3,' (',F6.1,',',F6.1,')')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2(2:48),' ')
      CALL PRNT(7,1,RMAX,' RMAX=.')
      CALL PRNT(4,1,J,' J=.')
      CALL PRNT(4,1,K,' K=.')
      IF(RMAX.LT.1.E-08) GOTO 10
      SCALE = 255./RMAX
      CALL SPRNT(R,JMOM,SCALE,NHOR,NVER)
      GOTO 10

      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE GAUSS(X0,Y0,SIGMA,A,NLW,NSW)
      INTEGER*2 A(NSW,NLW)

      S = -1./(2.*SIGMA*SIGMA)
      NLWH = NLW/2 + 1
      NSWH = NSW/2 + 1

      DO 10 J=1,NLW
      Y = Y0 + (J-NLWH)
      Y2 = Y*Y
      DO 10 I=1,NSW
      X = X0 + (I-NSWH)
      X2 = X*X
   10 A(I,J) = 255.*(1.-EXP(S*(X2+Y2)))

      RETURN
      END

C********************************************C
C RESLOCVO initialize CONVOLD subroutine     C
C********************************************C
      SUBROUTINE OPCON(NLW,NSW)
C ROUTINE TO INITIALIZE CONVOLD
	IMPLICIT REAL*8 (D-F)
	COMMON/CP/IBUG
	COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,dummy2

      AREA = NLW*NSW
      D1 = 6./(AREA*(NSW*NSW-1))
      D2 = 6./(AREA*(NLW*NLW-1))
      D3 = -1./(AREA*(NSW-1)*(NLW-1))
      D4 = 7.*AREA - (NLW+NSW+5)
      FNLWP1 = NLW + 1
      FNSWP1 = NSW + 1
      FNLWM1 = NLW - 1
      FNSWM1 = NSW - 1

      RETURN
      END

C********************************************C
C RESLOCVO detrend area subroutine           C
C********************************************C
      SUBROUTINE DTREND(A,AP,NLW,NSW)
C ROUTINE TO DETREND AREA A
	IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,SSUMA2
	INTEGER*2 A(NSW,NLW)
	REAL*4 AP(NSW,NLW)

      SSUM = 0.D0
      ZI = 0.D0
      ZJ = 0.D0

      DO 16 J=1,NLW
	N = 0
	MOM = 0
	DO 10 I=1,NSW
	N = N + A(I,J)
10   	MOM = MOM + I * A(I,J)
      SSUM = SSUM + N
      ZI = ZI + MOM
   16 ZJ = ZJ + J * N

      C1 = D1*(2.*ZI - FNSWP1*SSUM)
      C2 = D2*(2.*ZJ - FNLWP1*SSUM)
      C3 = D3*(6.*(FNLWM1*ZI+FNSWM1*ZJ) - D4*SSUM)
      SSUMA2 = 0.D0

C          AP(I,J) = A(I,J) - (C1*I+C2*J+C3)
      DO 17 J=1,NLW
      C3 = C3 + C2
      C4 = C3
      DO 17 I=1,NSW
      C4 = C4 + C1
      C5 = A(I,J) - C4
      AP(I,J) = C5
   17 SSUMA2 = SSUMA2 + C5*C5

      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
	SUBROUTINE MOMGEN(BUF,SUM,MOM,J,NS)
C-----THIS ROUTINE WAS WRITTEN FROM THE COMMENTS IN GARY'S 
C-----ASSEMBLER CODE.
	INTEGER*2 BUF(NS),SUM(NS)
	INTEGER*4 MOM(NS)

	DO 16 I=1,NS
	SUM(I) = SUM(I) + BUF(I)
	MOM(I) = MOM(I) + BUF(I)*J
16	CONTINUE

	RETURN
	END

C********************************************C
C RESLOCVO detrend area subroutine           C
C********************************************C
      SUBROUTINE CONVLD(AD,B,NLW,NSW,JSUM,JMOM,BK,R,NHOR,NSIN,NLIC)
C ROUTINE TO DETREND AREA B AND CONVOLVE IT WITH AREA A (ALREADY DETREND
	IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,SSUMA2
	REAL*4 AD(NSW,NLW),BP,R(NHOR)
	INTEGER*2 JSUM(NSIN),B(NSIN,1)
	INTEGER*4 JMOM(NSIN),BK
      SSUM = 0.D0
      ZI = 0.D0
      ZJ = 0.D0

      DO 20 I=1,NSW
      SSUM = SSUM + JSUM(I)
      ZI = ZI + I*JSUM(I)
   20 ZJ = ZJ + JMOM(I)

      DO 30 II=1,NHOR
C          DETREND RIGHT AREA
      C1 = D1*(2.*ZI - FNSWP1*SSUM)
      C2 = D2*(2.*ZJ - FNLWP1*SSUM)
      C3 = D3*(6.*(FNLWM1*ZI+FNSWM1*ZJ) - D4*SSUM)
      SSUMAB = 0.D0
      SSUMB2 = 0.D0
      LL = BK
C           BP = B(I,J) - (C1*I+C2*J+C3)
      DO 25 J=1,NLW
      C3 = C3 + C2
      C4 = C3
      DO 24 I=1,NSW
      C4 = C4 + C1
      BP = B(II+I-1,LL) - C4
      SSUMAB = SSUMAB + AD(I,J)*BP
   24 SSUMB2 = SSUMB2 + BP*BP
   25 LL = MOD(LL,NLIC) + 1

      IF(SSUMB2.GT.1.D-10.AND.SSUMAB.GT.0.D0) R(II) = (SSUMAB*SSUMAB)/
     &                                                (SSUMA2*SSUMB2)
      ZI = ZI + NSW*JSUM(II+NSW) - SSUM
      ZJ = ZJ + JMOM(II+NSW) - JMOM(II)
   30 SSUM = SSUM + JSUM(II+NSW) - JSUM(II)

      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
	SUBROUTINE UPMOMV(BI,BJ,JSUM,JMOM,NS,NLW)
C-----THIS ROUTINE HAS BEEN WRITTEN FROM THE COMMENTS
C-----IN GARY'S ASSEMBLER CODE.
	INTEGER*2 BI(NS),BJ(NS),JSUM(NS)
	INTEGER*4 JMOM(NS)

	DO 10 I=1,NS
	IB = BI(I)
	ISUM = JSUM(I)
	JSUM(I) = JSUM(I) + BI(I) - BJ(I)	
	JMOM(I) = JMOM(I) + BI(I)*NLW - ISUM
10	CONTINUE

	RETURN
	END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
	SUBROUTINE MAXR(RBUF,N,RMAX,INDEX)
C-----THIS ROUTINE WILL FIND THE MAXIMUM VALUE (RMAX) IN A
C-----R*4 BUFFER (RBUF) OF N ELEMENTS.  IT WILL RETURN 
C-----THAT VALUE AND ITS INDEX.
C-----THIS SUBROUTIONE WAS WRITTEN FROM GARY'S
C-----COMMENTS IN THE ASSEMBLER CODE.
	REAL*4 RBUF(N)

	RMAX = RBUF(1)
	INDEX = 1

	DO 2 I=1,N
	IF(RMAX .GT. RBUF(I)) GO TO 2
	INDEX = I
	RMAX = RBUF(INDEX)
2	CONTINUE

	RETURN
	END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
C Routine to approximate a 3x3 area by a quadratic and
C return the location of the extreme value.
C quadratic is of the form
C          Q(X,Y) = DXX*X**2+DXY*X*Y+DYY*Y**2+EX*X+EY*Y+F
C
      SUBROUTINE QXLOC(Q,X,Y,NHOR,*)
      REAL*4 Q(NHOR,*)

      DXX = 0.
      DYY = 0.
C          COMPUTE 2ND DERIVATIVES
      DO 10 I=1,3
      DXX = DXX + Q(1,I) - 2.*Q(2,I) + Q(3,I)
   10 DYY = DYY + Q(I,1) - 2.*Q(I,2) + Q(I,3)

      DXX = DXX/6.
      DYY = DYY/6.
      DXY = .25*(Q(1,1)+Q(3,3) - Q(1,3)-Q(3,1))
      EX = (Q(3,1)+Q(3,3) - Q(1,1)-Q(1,3))/6.
      EY = (Q(1,3)+Q(3,3) - Q(1,1)-Q(3,1))/6.
C          LOCATE EXTREMUM
      D = 4.*DXX*DYY - DXY*DXY
      IF(D.EQ.0.) RETURN1
      X = (EY*DXY-2.*EX*DYY)/D
      Y = (EX*DXY-2.*EY*DXX)/D
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE SPRNT(R,BUF,SCALE,NHOR,NVER)
      LOGICAL*1 BUF(1)
      INTEGER S,SI,SS
      REAL*4 R(NHOR,NVER)
      CHARACTER*133 SMSG
      CHARACTER*132 LMSG
      DATA LMSG/'       .'/
      DATA SMSG/'      SAMP                                             
     +                                                                  
     +            '/

C          LIST OUT CORRELATION MATRIX IN VERTICAL STRIPS, 30 ELEMENTS
C   PER LINE
      DO 74 SI=1,NHOR,30
      SS = SI - 1
      NS = MIN0(NHOR-SS,30)
      IF (NS.LT.30) SMSG(11:133) = ' '
C          PRINT SAMPLE HEADING
      DO 72 S=1,NS
72    WRITE (SMSG(4*S+11-1:4*S+11),'(I2)') SS+S
      CALL XVMESSAGE(SMSG(2:133),' ')

      DO 74 L=1,NVER
      WRITE (LMSG(6:7),'(I2)') L
      CALL SFIX(1,NS,R(SI,L),BUF,SCALE,.5)
      CALL XVMESSAGE('0',' ')
   74 CALL PRNT(1,NS,BUF,LMSG)

      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
	SUBROUTINE SFIX(DCODE,NS,R,IBUF,SCALE,OFFSET)
C-----THIS ROUTINE WAS WRITTEN ESSENTIALLY FROM THE COMMENTS
C-----IN GARY'S ASSEMBLER CODE.
	REAL*4 R(NS)
	BYTE   IBUF(NS)
	INTEGER LOW,HIGH

        LOW = 0
        HIGH = 255
	DO 10 I=1,NS
	K = SCALE * R(I) + OFFSET
	IF(K .LT. LOW) K = LOW
	IF(K .GT. HIGH) K = HIGH
10	CALL MVE(-5,1,K,IBUF(I),1,1)
	RETURN
	END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
C  SUBROUTINE FILLOC
C  PURPOSE
C      FILLOC FILLS IN MISSING RESEAU LOCATIONS BY INTERPOLATION FROM
C      NEIGHBORING RESEAU MARKS.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    JANUARY 1985
C  FOR
C      MIPL DEVELOPMENT
C
C  DERIVED FROM ORIGINAL FILLOC SUBROUTINE BY
C      GARY YAGI
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-85  SP   CONVERTED FOR VIKING ORBITER RESEAU.
C     1-85  SP   ADDED WARNING MESSAGE IF NOT ALL RESEAU LOCATIONS INTERPOLATED
C     7-91 CCA   MODIFIED TO CALL NEW VOLABV2
C     5-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)  
C  INPUT PARAMETERS ( all parameters are REAL*4 )
C      ILOC(2,NRES_PAR)  - NOMINAL RESEAU LOCATIONS (LINE AND SAMPLE) FOR THE
C       array              NUMBER OF RESEAU MARKS.
C      OLOC(2,NRES_PAR)  - OUTPUT RESEAU LOCATIONS (LINE AND SAMPLE) FOR THE
C       array              NUMBER OF RESEAU MARKS.
C  OUTPUT PARAMETERS
C      OLOC(2,NRES_PAR)  - OUTPUT RESEAU LOCATIONS (LINE AND SAMPLE) FOR THE
C       array              NUMBER OF RESEAU MARKS WITH MISSING LOCATIONS (-99)
C                          FILLED IN BY INTERPOLATION.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      SUBROUTINE FILLOC(ILOC,OLOC)
      PARAMETER (NRES_PAR=103)    ! NUMBER OF RESEAU MARKS FOR VIKING ORBITER.

      REAL*4 ILOC(2,NRES_PAR),  OLOC(2,NRES_PAR)
      INTEGER*2 L1(3,3)
      INTEGER*2 L2(3,4)
      INTEGER*2 L3(4,4)
      INTEGER*2 IBUF(7)

      DATA L1/25,13,36,   48,59,36,   71,82,59/
      DATA L2/13,24, 1,   36,24,47,   59,47,70,  82,93,70 /
      DATA L3/ 1, 2,12,13,    11,10,22,23,
     .                   93,94,81,82,   103,91,92,102   /

C=================START OF EXECUTABLE CODE===============================     

      LOOP = 0
      MIN = 2

C          SCAN FOR MISSING LOCS
    1 CONTINUE
      DO 3 I=1, NRES_PAR
      IF(ILOC(1,I).EQ.-99.) GOTO 3
      IF(OLOC(1,I).EQ.-99.) GOTO 5
    3 CONTINUE

      RETURN

    5 LOOP = LOOP + 1
      IF(LOOP.GT.5)  THEN
        CALL XVMESSAGE
     X       ('WARNING: SOME RESEAU LOCATIONS WERE NOT FOUND',' ')
        RETURN
      END IF
      IF(LOOP.GT.2) MIN=1

      DO 10 J=1,3
      IBUF(1) = L1(1,J)                    ! PROCESS RESEAU MARKS 25-33,
      IBUF(2) = L1(2,J)                    ! 48-56, & 71-79.
      IBUF(3) = L1(3,J)
      IBUF(4) = IBUF(1) + 1
      IBUF(5) = IBUF(2) + 1
      IBUF(6) = IBUF(3) + 1
      IBUF(7) = IBUF(1) - 1
      M = 9                                ! FILL1 OPERATES ON A ROW OF M 
   10 CALL FILL1(IBUF,7,M,ILOC,OLOC,MIN)   ! RESEAU MARKS.

      DO J=1,4
      IBUF(1) = L2(1,J)                   ! PROCESS RESEAU MARKS 13-22,
      IBUF(2) = L2(2,J)                   ! 36-45, 59-68, & 82-91.
      IBUF(3) = L2(3,J)
      IBUF(4) = IBUF(1) + 1
      IBUF(5) = IBUF(2) + 1
      IBUF(6) = IBUF(3) + 1
      IBUF(7) = IBUF(1) - 1
      M = 10
      CALL FILL1(IBUF,7,M,ILOC,OLOC,MIN)
      END DO

      IBUF(1) = 2                         ! PROCESS RESEAU MARKS 2-10.
      IBUF(2) = 1
      IBUF(3) = 3
      IBUF(4) = 13
      IBUF(5) = 14
      M = 9
      CALL FILL1(IBUF,5,M,ILOC,OLOC,MIN)

      IBUF(1) = 94                        ! PROCESS RESEAU MARKS 94-102.
      IBUF(2) = 93
      IBUF(3) = 95
      IBUF(4) = 82
      IBUF(5) = 83
      M = 9
      CALL FILL1(IBUF,5,M,ILOC,OLOC,MIN)

      DO I0= 24,70,23
      IBUF(1) = I0                        ! PROCESS RESEAU MARKS 24,47,70
      IBUF(2) = I0 - 12
      IBUF(3) = I0 + 11
      IBUF(4) = IBUF(3) + 1
      IBUF(5) = I0 + 1
      IBUF(6) = IBUF(2) + 1
      CALL FILL1(IBUF,6,1,ILOC,OLOC,MIN)
      END DO

      DO I0= 34,80,23
      IBUF(1) = I0                        ! PROCESS RESEAU MARKS 34,57,80.
      IBUF(2) = I0 + 12
      IBUF(3) = I0 - 11
      IBUF(4) = IBUF(3) - 1
      IBUF(5) = I0 - 1
      IBUF(6) = IBUF(2) - 1
      CALL FILL1(IBUF,6,1,ILOC,OLOC,MIN)
      END DO

      DO I0= 12,81,23
      IBUF(1) = I0                        ! PROCESS RESEAU MARKS 12,35,58,81
      IBUF(2) = I0 + 12
      IBUF(3) = I0 - 11
      IBUF(4) = I0 + 1
      CALL FILL1(IBUF,4,1,ILOC,OLOC,MIN)
      END DO

      DO I0= 23,92,23
      IBUF(1) = I0                        ! PROCESS RESEAU MARKS 23,46,69,92.
      IBUF(2) = I0 - 12
      IBUF(3) = I0 + 11
      IBUF(4) = I0 - 1
      CALL FILL1(IBUF,4,1,ILOC,OLOC,MIN)
      END DO

      DO J=1,4
      IBUF(1) = L3(1,J)                   ! PROCESS RESEAU MARKS 1,11,93,103.
      IBUF(2) = L3(2,J) 
      IBUF(3) = L3(3,J)
      IBUF(4) = L3(4,J)
      M = 1
      CALL FILL1(IBUF,4,M,ILOC,OLOC,MIN)
      END DO

      GOTO 1

      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE FILL1(IBUF,N,M,ILOC,OLOC,MIN)
      REAL*4 ILOC(2,*),OLOC(2,*)
      INTEGER*2 IBUF(N)

      DO 15 JJ=1,M
      I0 = IBUF(1) + JJ - 1              ! PROCESS A ROW OF M RESEAU STARTING 
      IF(OLOC(1,I0).NE.-99.) GOTO 15     ! WITH IBUF(1).
      KNT = 0
      DX = 0.
      DY = 0.

      DO 10 J=2,N
      I = IBUF(J) + JJ - 1
      IF(OLOC(1,I).LT.0.) GOTO 10
      KNT = KNT + 1
      DY = DY + OLOC(1,I) - ILOC(1,I)
      DX = DX + OLOC(2,I) - ILOC(2,I)
   10 CONTINUE

      IF(KNT.LT.MIN) GOTO 15           ! DO NOT INTERPOLATE UNLESS ENOUGH 
      OLOC(1,I0) = ILOC(1,I0) + DY/KNT ! LOCATIONS.
      OLOC(2,I0) = ILOC(2,I0) + DX/KNT
   15 CONTINUE

      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
C Routine to fit reslocs (loc) to nominals (ref)
C
      SUBROUTINE RESFIT(LOC,REF,WORK,PTS,COEF,MOM,UMOM,A,IFIT,TOL,IBUG)
      REAL*4 LOC(2,202),REF(2,202),WORK(2,202),PTS(4,202)
      CHARACTER*132 MSG
      REAL*8 COEF(20),MOM(28),UMOM(20),A(100)
      CHARACTER*132 MSG1
      CHARACTER*132 RMSG

      NRES = 103
      NPOW = MAX0(IFIT-7,1)
      IMAX0 = 0
      ILOOP = 0
      IFLAG = 0

    5 ILOOP = ILOOP + 1

      DO 10 I=1,NRES
      PTS(2,I) = LOC(1,I)
      PTS(1,I) = LOC(2,I)
      PTS(4,I) = REF(1,I)
      PTS(3,I) = REF(2,I)
      IF(PTS(1,I).NE.-99.) GOTO 10
      IFLAG = 1
      CALL MVE(7,2,PTS(3,I),PTS,1,1)
   10 CONTINUE

      IF(IFLAG.EQ.1) GOTO 50

      CALL LFIT(IFIT,PTS,NRES,COEF,MOM,UMOM,A,*100)
      CALL RMSFIT(NPOW,PTS,NRES,COEF,RMS,RMAX,IMAX,1)
      IF(IMAX.EQ.IMAX0) GOTO 50
      IMAX0 = IMAX
      WRITE (RMSG,9930) RMS
9930  FORMAT (' RMS=',F6.2)
      CALL XVMESSAGE(RMSG(2:11),' ')
      WRITE (MSG,9940) IMAX,RMAX,LOC(1,IMAX),LOC(2,IMAX),REF(1,IMAX),
     +REF(2,IMAX)
9940  FORMAT (' RESEAU MARK ',I3,' HAD THE LARGEST RESIDUE OF ',F4.1,
     +' PIXELS (',F6.1,',',F6.1,') NOM=(',F6.1,',',F6.1,')')
      CALL XVMESSAGE(MSG(2:91),' ')
      IF(RMAX.LT.TOL) GOTO 90
      LOC(1,IMAX) = -99.
      LOC(2,IMAX) = -99.
      CALL FILLOC(REF,LOC)
      WRITE (MSG1,9950) IMAX,LOC(1,IMAX),LOC(2,IMAX)
9950  FORMAT (' MARK ',I3,' CHANGED TO (',F6.1,',',F6.1,')')
      CALL XVMESSAGE(MSG1(2:36),' ')
      GOTO 5

   50 CALL XVMESSAGE(
     &	'**WARNING--MAXIMUM OUTPUT ERROR EXCEEDS TOLERANCE',' ')

   90 IF(IBUG.EQ.0) RETURN

      DO 95 I=1,NRES
      WORK(1,I) = PTS(4,I) - REF(1,I)
   95 WORK(2,I) = PTS(3,I) - REF(2,I)

      CALL XVMESSAGE('DIFFERENCES FROM NOMINALS',' ')
      CALL PU75(WORK,1)

  100 RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE LFIT(IFIT,PTS,N,COEF,MOM,UMOM,A,*)
      COMMON/CP/IBUG
      REAL*4 PTS(4,1)
      REAL*8 COEF(1),MOM(1),UMOM(1),A(1),EPS
      DATA EPS/1.D-15/

      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)/2
      IF(NU.GT.N) GOTO 999
      CALL MOMENT(NPOW,PTS,N,MOM,UMOM,0)
      IF(IBUG.EQ.0) GOTO 10
      CALL XVMESSAGE('MOMENTS=',' ')
      CALL PMOM(2*NPOW,MOM)
      CALL XVMESSAGE('UMOMENTS=',' ')
      CALL PMOM(NPOW,UMOM)
      CALL PMOM(NPOW,UMOM(NU+1))
   10 IF(IFIT.LT.8) CALL CLFIT(IFIT,COEF,MOM,UMOM,IER)
      IF(IFIT.GE.8) CALL LSPFIT(NPOW,COEF,MOM,UMOM,A,EPS,IER)
      IF(IER.NE.0) GOTO 998
      RETURN

  998 CALL PRNT(4,1,IER,'0**FIT ERR=.')
  999 CALL XVMESSAGE('REQUESTED FIT IS UNDERCONSTRAINED',' ')
      CALL XVMESSAGE('REDUCE FIT OR ADD MORE POINTS',' ')
      RETURN1
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE MOMENT(NPOW,PTS,N,MOM,UMOM,MODE)
      IMPLICIT REAL*8 (R-Z)
      REAL*4 PTS(4,1)
      REAL*8 MOM(1),UMOM(1)

      MPOW = 2*NPOW
      NU = (NPOW+1)*(NPOW+2)/2
      NM = (MPOW+1)*(MPOW+2)/2
      U = 0.D0
      V = 0.D0
      IF(MODE.NE.0) GOTO 5
      CALL MVE(8,NM,U,MOM,0,1,1)
      CALL MVE(8,2*NU,U,UMOM,0,1,1)
    5 CONTINUE

      DO 10 K=1,N
      XI = PTS(2,K)
      YI = PTS(1,K)
      UI = PTS(4,K)
      VI = PTS(3,K)
      U = U + UI
      V = V + VI
      L = 1

      DO 10 I=1,MPOW
      IP1 = I + 1
      IPOW = I
      Z = XI**I
      DO 10 J=1,IP1
      L = L + 1
      IF(J.EQ.1) GOTO 8
      IF(J.EQ.IP1) GOTO 7
      Z = XI**IPOW*YI**(I-IPOW)
      GOTO 8
    7 Z = YI**I
    8 MOM(L) = MOM(L) + Z
      IF(I.GT.NPOW) GOTO 10
      UMOM(L) = UMOM(L) + UI*Z
      UMOM(NU+L) = UMOM(NU+L) + VI*Z
   10 IPOW = IPOW - 1

      MOM(1) = MOM(1) + N
      UMOM(1) = UMOM(1) + U
      UMOM(NU+1) = UMOM(NU+1) + V
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE PMOM(NPOW,MOM)
      CHARACTER*132 MSG
      REAL*8 MOM(1)

      MSG(1:132) = ' '
      MSG(1:1) = '0'
      L = 1
      WRITE (MSG(2:10),'(F9.1)') MOM
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG(2:10),' ')

      DO 20 I=1,NPOW
      M = 1
      IP1 = I + 1
      DO 10 J=1,IP1
      L = L + 1
      M = M + 9
10    WRITE (MSG(M-8:M),'(F9.1)') MOM(L)
20    CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG(2:M),' ')

      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE LSPFIT(NPOW,COEF,MOM,UMOM,A,EPS,IER)
      REAL*8 COEF(1),MOM(1),UMOM(1),A(1)

C          GENERATE A-ARRAY FROM MOMENTS
      NU = (NPOW+1)*(NPOW+2)/2
      CALL MVE(8,NU,MOM,A,1,1)
      II = NU
      KX = 1

      DO 20 K=1,NPOW
      KP1 = K + 1
      KPOW = K
      LPOW = 0
      DO 20 L=1,KP1
      KX = KX + 1
      II = II + 1
      A(II) = MOM(KX)
      IBEG = II

      DO 10 I=1,NPOW
      IP1 = I + 1
      IPOW = I
      JPOW = 0
      DO 10 J=1,IP1
      IX = IPOW + KPOW
      IY = JPOW + LPOW
      IX = IX + IY
      IX = (IX*(IX+1))/2 + 1 + IY
      II = II + 1
      A(II) = MOM(IX)
      IPOW = IPOW - 1
   10 JPOW = JPOW + 1

      KPOW = KPOW - 1
   20 LPOW = LPOW + 1

      CALL MVE(8,2*NU,UMOM,COEF,1,1)
      CALL DGELG(COEF,A,NU,2,EPS,IER)
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE CLFIT(IFIT,COEF,MOM,UMOM,IER)
      IMPLICIT REAL*8 (A-F,R-Z)
      REAL*8 COEF(6),MOM(6),UMOM(6),N
      REAL*8 EPS
      INTEGER ROTATE,SCALE,OFFSET
      DATA EPS/1.D-15/

C LEAST SQUARE LINEAR FIT ROUTINE
C FINDS COEFFICIENTS A,B,E,F SUCH THAT
C          U = A*X + B*Y + E
C          V =-B*X + A*Y + F

C         IFIT  ROTATE SCALE  OFFSET
C          1      0      0      1
C          2      0      1      0
C          3      0      1      1
C          4      1      0      0
C          5      1      0      1
C          6      1      1      0
C          7      1      1      1
      IER = -1
      IF(IFIT.LT.0.OR.IFIT.GT.7) RETURN
      N = MOM(1)
      IF(N.LE..9) RETURN
      X = MOM(2)
      Y = MOM(3)
      X2 = MOM(4)
      Y2 = MOM(6)
      U = UMOM(1)
      XU = UMOM(2)
      YU = UMOM(3)
      V = UMOM(4)
      XV = UMOM(5)
      YV = UMOM(6)
C          INITIALIZE WITH IDENTITY TRANSORMATION
      A = 1.D0
      B = 0.D0
      E = 0.D0
      F = 0.D0
      OFFSET = MOD(IFIT,2)
      SCALE = MOD(IFIT/2,2)
      ROTATE = IFIT/4
      R1 = XU + YV
      R2 = YU - XV
      R = X2 + Y2
      IF(OFFSET.EQ.0) GOTO 5
      R1 = N*R1 - (X*U+Y*V)
      R2 = N*R2 - (Y*U-X*V)
      R = N*R - (X*X+Y*Y)
    5 IF(SCALE.EQ.0) R=DSQRT(R1*R1+R2*R2)
      IF(ROTATE+SCALE.EQ.0) GOTO 10
      IF(DABS(R).LE.EPS) RETURN
      A = R1/R
      IF(ROTATE.EQ.1) B=R2/R
   10 IF(OFFSET.EQ.0) GOTO 15
      E = (U - A*X - B*Y)/N
      F = (V + B*X - A*Y)/N
   15 COEF(1) = E
      COEF(2) = A
      COEF(3) = B
      COEF(4) = F
      COEF(5) = -B
      COEF(6) = A
      IER = 0
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE RMSFIT(NPOW,PTS,N,COEF,RMS,DMAX,IMAX,MODE)
      REAL*4 PTS(4,1)
      REAL*8 DRMS,COEF(1)

      DRMS = 0.D0
      DMAX = 0.D0
      IMAX = 0

      DO 20 K=1,N
      X = PTS(2,K)
      Y = PTS(1,K)
      CALL PTRAN(NPOW,COEF,X,Y,U,V)
      DU = U - PTS(4,K)
      DV = V - PTS(3,K)
      DU = DU*DU + DV*DV
      IF(DMAX.GE.DU) GOTO 18
      DMAX = DU
      IMAX = K
   18 DRMS = DRMS + DU
      IF(MODE.NE.1) GOTO 20
      PTS(4,K) = U
      PTS(3,K) = V
   20 CONTINUE

      DMAX = SQRT(DMAX)
      RMS = DSQRT(DRMS/N)
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
      SUBROUTINE PTRAN(NPOW,COEF,X0,Y0,U0,V0)
      REAL*8 COEF(1),X,Y,U,V,Z

      NU = (NPOW+1)*(NPOW+2)/2
      X = X0
      Y = Y0
      U = COEF(1)
      V = COEF(NU+1)
      L = 1

      DO 10 I=1,NPOW
      IP1 = I + 1
      IPOW = I
      Z = X**I
      DO 10 J=1,IP1
      L = L + 1
      IF(J.EQ.1) GOTO 8
      IF(J.EQ.IP1) GOTO 7
      Z = X**IPOW*Y**(I-IPOW)
      GOTO 8
    7 Z = Y**I
    8 U = U + COEF(L)*Z
      V = V + COEF(NU+L)*Z
   10 IPOW = IPOW - 1

      U0 = U
      V0 = V
      RETURN
      END

C********************************************C
C RESLOCVO subroutine                        C
C********************************************C
C Routine to generate geoma parameters (returned in WORK)
      SUBROUTINE GEOPAR(BLOCS,WORK,NRES,ICAM)
      REAL*4 BLOCS(2,103),WORK(1)
      REAL*4 CLUCS(2,103,4)
      EQUIVALENCE (CLUCS,CL1),(CLUCS(1,1,2),CL2),(CLUCS(1,1,3),CL3)
      EQUIVALENCE (CLUCS(1,1,4),CL4)
C  OBJECT SPACE COORDINATES FOR S/N 7  (L,S)
      REAL*4 CL1(2,103)
      REAL*4 CL2(2,103)
      REAL*4 CL3(2,103)
      REAL*4 CL4(2,103)
      REAL*4 CL1_A(103),CL1_B(103)      
      REAL*4 CL2_A(103),CL2_B(103)      
      REAL*4 CL3_A(103),CL3_B(103)      
      REAL*4 CL4_A(103),CL4_B(103)      
      EQUIVALENCE (CL1(1,1),CL1_A), (CL1(2,52),CL1_B)
      EQUIVALENCE (CL2(1,1),CL2_A), (CL2(2,52),CL2_B)
      EQUIVALENCE (CL3(1,1),CL3_A), (CL3(2,52),CL3_B)
      EQUIVALENCE (CL4(1,1),CL4_A), (CL4(2,52),CL4_B)

      DATA CL1_A/
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
     * 575.000/


      DATA CL1_B/
     * 625.000, 575.128, 744.146, 575.165, 863.038,
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

C  OBJECT SPACE COORDINATES FOR S/N 4  (L,S)
      DATA CL2_A/
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
     * 575.000/



      DATA CL2_B/
     * 625.000, 575.173, 743.917, 575.174, 862.786,
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

C  OBJECT SPACE COORDINATES FOR S/N 8  (L,S)
      DATA CL3_A/
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
     * 575.000/


      DATA CL3_B/
     * 625.000, 574.840, 744.058, 575.130, 863.009,
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

C  OBJECT SPACE COORDINATES FOR S/N 6  (L,S)
      DATA CL4_A/
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
     * 575.000/

      DATA CL4_B/
     * 625.000, 575.105, 744.026, 575.165, 862.782,
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


      ICAMI = 1
      IF (ICAM.EQ.4) ICAMI=2
      IF (ICAM.EQ.8) ICAMI=3
      IF (ICAM.EQ.6) ICAMI=4
      J = 0
C
      DO 104 N1=1,NRES,23
      N2 = N1 + 10
      M1 = N1 + 11
      M2 = N1 +21
C
      DO 102 I=N1,N2
      WORK(J+1)=CLUCS(1,I,ICAMI)
      WORK(J+2)=CLUCS(2,I,ICAMI)
      WORK(J+3)=BLOCS(1,I)
      WORK(J+4)=BLOCS(2,I)
      WORK(J+5)=CLUCS(1,I,ICAMI)
      WORK(J+6)=CLUCS(2,I,ICAMI)
      WORK(J+7)=BLOCS(1,I)
      WORK(J+8)=BLOCS(2,I)
  102 J = J + 8
C
      DO 103 I=M1,M2
      WORK(J+1)=CLUCS(1,I,ICAMI)
      WORK(J+2)=CLUCS(2,I,ICAMI)
      WORK(J+3)=BLOCS(1,I)
      WORK(J+4)=BLOCS(2,I)
      WORK(J+5)=CLUCS(1,I+1,ICAMI)
      WORK(J+6)=CLUCS(2,I+1,ICAMI)
      WORK(J+7)=BLOCS(1,I+1)
      WORK(J+8)=BLOCS(2,I+1)
  103 J = J + 8
C
  104 CONTINUE
C
      RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create reslocvo.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM reslocvo

   To Create the build file give the command:

		$ vimake reslocvo			(VMS)
   or
		% vimake reslocvo			(Unix)


************************************************************************/


#define PROGRAM	reslocvo
#define R2LIB

#define MODULE_LIST reslocvo.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/*#define LIB_LOCAL*/
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create reslocvo.pdf
process help=*
PARM INP     TYPE=STRING  COUNT=1:2
PARM OUT     TYPE=STRING  COUNT=1
PARM GEOPAR  TYPE=STRING  COUNT=0:1 		  DEFAULT=""
PARM CAMERA  TYPE=INTEGER COUNT=0:1 VALID=4:8	  DEFAULT=4
PARM DBUG    TYPE=KEYWORD COUNT=0:1 VALID=DBUG	  DEFAULT=--
PARM PRINT   TYPE=KEYWORD COUNT=0:1 VALID=PRINT	  DEFAULT=--
PARM NOIN    TYPE=KEYWORD COUNT=0:1 VALID=NOIN	  DEFAULT=--
PARM NVER    TYPE=INTEGER COUNT=0:1               DEFAULT=19
PARM NHOR    TYPE=INTEGER COUNT=0:1               DEFAULT=19
PARM NLW     TYPE=INTEGER COUNT=0:1               DEFAULT=5
PARM NSW     TYPE=INTEGER COUNT=0:1               DEFAULT=5
PARM SIGMA   TYPE=REAL	  COUNT=0:1   		  DEFAULT=1.
PARM FRAME   TYPE=INTEGER COUNT=0:1   		  DEFAULT=0
PARM CTHRESH TYPE=REAL    COUNT=0:1		  DEFAULT=.5
PARM FIT     TYPE=INTEGER COUNT=0:1 VALID=1:8	  DEFAULT=8
PARM TOLER   TYPE=REAL    COUNT=0:1		  DEFAULT=4.
END-PROC
.TITLE
"reslocvo"
.HELP
PURPOSE:

"reslocvo" will locate the reseau on Viking Orbiter images.  The (line,sample)
coordinates of each reseau mark are output as a reseau location record in a
MARK format.  The reseau location record may be input to the program RESSAR75
to cosmetically remove the reseau from the image.  "reslocvo" will optionally 
generate GEOMA parameters for removing the geometric distortions.

.PAGE
TAE COMMAND LINE FORMAT

	reslocvo INP=(IMG,NOM) OUT=LOC optional parameters
where
	IMG is the input Viking Orbiter image,
	NOM is an optional input reseau location record containing the
	  nominal reseau location,
	and LOC is the output reseau location record.

.PAGE
OPERATION:

The Viking Orbiter cameras include a reseau pattern consisting of 103 reseau
marks embedded on the face-plate of the vidicon.  The position of these
marks will vary from image-to-image due to time, temperature, and scene
dependent geometric camera distortions.

The location of the reseau requires a set of nominal locations to be
used as starting values. The nominal locations may be optionally input
as the second input file.  If the nominals are not input in this way, they
are retrieved from tables built into the program. 

"reslocvo" will search for each reseau mark in an NHOR x NVER area
centered about its nominal location.  Each mark is found by using an
NLW x NSW correlation window. As this window scans over the search area,
the underlying area is compared with a reseau shape function
(constant for all the marks) of the form: 
   	f(x,y) = 255(1 - exp(-(x**2 + y**2)/2s**2)
For a 9 x 9 window, the shape function is:

	254 254 254 254 254 254 254 254 254 254
	254 254 254 253 252 253 254 254 254 254
	254 254 250 234 220 234 250 254 254 254
	254 253 234 161 100 161 234 253 254 254
	254 252 220 100   0 100 220 252 254 254
	254 253 234 161 100 161 234 253 254 254
	254 254 250 234 220 234 250 254 254 254
	254 254 254 253 252 253 254 254 254 254
	254 254 254 254 254 254 254 254 254 254

Let A(i,j) represent the shape function, and B(i,j) be the area of the
picture being compared. Let uA, uB, tA, tB be the corresponding means and
standard deviations. The correlation function is a normalized variance of
the form: 
     p = (SIGMA(Aij-uA)(Bij-uB))/(N*tA*tB)
where N = NLW*NSW and the summation is performed over an NLW by NSW
window. 

Note: Marks near the boundaries of the picture are not located. Coordinates
for these marks are calculated by interpolating over its nearest neighbors.

If an output parameter file is specified via the GEOPAR parameter, then
GEOMA parameters are output to this file.  The GEOMA parameters can then
be input the program GEOMA to remove the geometric distortions.

.PAGE
EXAMPLE

	reslocvo INP=A OUT=RES GEOPAR=GEOM.PAR
	ressar75 (A,RES) B
	geoma B C PARMS=GEOM.PAR

"reslocvo" is used to locate the reseau in the frame stored in data set A. 
The resulting reseau locations (RES) are input to program RESSAR75 which
removes the reseau.  GEOMA parameters are also generated (GEOM.PAR) and
are input to program GEOMA to remove the camera distortions.

RESTRICTIONS:

The constants NLW, NSW, NHOR, NVER must satisfy the following: 
     1. Each must be odd;
     2. 6(NSW*NLW+NHOR+NSW) + 4*NHOR*NVER + 2408(NLW+NVER) <= 82414
        If NLW = NSW and NHOR = NVER, then the following
        simplified approximate formula may be used:
                NLW + NVER <= 32
.page
TIMING:

The execution time of "reslocvo" is directly proportional to the window and
search dimensions NLW, NSW, NHOR, NVER. 

WRITTEN BY:                Steve Pohorsky		17 Jan 1985
COGNIZANT PROGRAMMER:      Gary Yagi
REVISIONS:
 26 May 94  Meredith Cox (CRI) -- Made portable for UNIX 
 20 Oct 87  Gary Yagi   -- Added GEOMA parameter output
 10 Oct 87  Gary Yagi	-- Parameter processing converted to Vicar2
 25 Jun 86  Joel Mosher -- I/O converted to Vicar2
.LEVEL1
.VARIABLE INP
 1 image file optionally
 followed by a
 nominal reseau
 location record.
.VARIABLE OUT
 Reseau location record.
.VARIABLE GEOPAR
 STRING--OPTIONAL
 Output file to contain
 GEOMA parameters.
.VARIABLE CAMERA
 integer - camera serial number
 override
.VARIABLE DBUG
 Keyword - Valid value = DBUG.
 Causes diagnostics to be
 printed.
.VARIABLE PRINT
 Keyword - Valid value = PRINT
 Generates listing of reseaus.
.VARIABLE NOIN
 Keyword - Valid value =NOIN
 Suppresses the interpolation
 of correlation maximum.
.VARIABLE NVER
 integer - height of the search
 area
.VARIABLE NHOR
 integer - width of the search
 area
.VARIABLE NLW
 integer - height of correlation
 area
.VARIABLE NSW
 integer - width of the
 correlation area
.VARIABLE SIGMA
 real - standard deviation
 constant for reseau shape
 function.
.VARIABLE FRAME
 integer - frame number override
.VARIABLE CTHRESH
 REAL - Optional -CORRELATION THRESHOLD
.VARIABLE FIT
 INTEGER - Optional-SELECT FIT TYPE
.VARIABLE TOLER
 REAL - Optional - MAX LOCATION ERROR
.LEVEL2
.VARIABLE INP
One picture file generated by the Viking Orbiter logging program (VOLOG). 
This frame should be an unprocessed byte image (i.e. No stretching,
filtering or geom allowed).  This can be followed by a reseau location
record containing the nominal reseau locations; 
 .VARIABLE OUT
A reseau record containing the (LINE,SAMPLE) coordinates for each
reseau mark. This file contains just 1 line of 824 bytes. The output has a
VICAR label. 
.VARIABLE GEOPAR
Optional output file to contain GEOMA parameters for removing the geometric
distortions.
.VARIABLE CAMERA
Specifies the camera serial number for the input picture.  This
parameter may be used to override the camera information in the Viking
Orbiter label.  The valid camera serial numbers are:
	7=VO-1A		8=VO-2A
	4=VO-1B		6=VO-2B
.VARIABLE DBUG
Causes diagnostic messages to be printed.
.VARIABLE PRINT
Causes a listing of the reseau locations to be generated.
.VARIABLE NOIN
The program will normally attempt to calculate the reseau locations to
sub-pixel accuracy.  This keyword suppresses interpolation of the
correlation maximum.
.VARIABLE NVER
 NVER is an integer specifying the height of the search area. (must be
odd) (Default =19) 
.VARIABLE NHOR
 NHOR  is an integer specifying the width of the search area. (must be
odd) (Default = 19) 
.VARIABLE NLW
 NLW is an integer specifying the height of the correlation area. (must be
odd) (Default = 5) 
.VARIABLE NSW
 NSW is an integer specifying the width of the correlation area. (must be
odd) (Default = 5) 
.VARIABLE SIGMA
 SIGMA is a real number specifying the standard deviation constant used in
the reseau shape function. (Default = 1.0) 
.VARIABLE FRAME
 FRAME  is an integer specifying the frame number for the input picture. 
This parameter will override the frame number in the Viking Orbiter label.
.VARIABLE CTHRESH
 A THRESHOLD ON CORRELATION VALUES.
 Default is CTHRESH=0.5.
.VARIABLE FIT
 VALID = 1:8
 Default is FIT = 8.
 Specitfies the type of fit to be performed.  Meanings are :

         IFIT  ROTATE SCALE  OFFSET
          1      0      0      1
          2      0      1      0
          3      0      1      1
          4      1      0      0
          5      1      0      1
          6      1      1      0
          7      1      1      1

.VARIABLE TOLER
Default is TOLER = 4.0.  Maximum allowed error in calculated location
relative to  the nominal location. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstreslcv.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
Write " "
Write " The Test Data are handled for both VMS and UNIX in this PDF. "
Write " At present (May 1994), in order to run the program, the"
Write " following data files MUST be copied to the LOCAL directory"
Write " where the program resides: "
Write "                OLD     NEW  (VMS or UNIX execution)"
Write "  VIKING:218S01.IMG ==> 218s01.img "
Write " "
Write " This UNIX restriction on the data will be changed eventually. "
Write " "
Write " "
!DCL ASSIGN VIKING:218S01.IMG IMG1
!dcl assign v2$scratch: s
reslocvo 218s01.img resloca
fixloc resloca
WRITE "THE NEXT RUN WILL TEST THE REST OF THE KEYWORDS."
reslocvo (218s01.img,resloca) resloca2 SIGMA=1.2 FIT=7 TOLE=5. FRAME=1 'NOIN +
          CTHR=0.6 NVER=21 NHOR=21 NLW=7 NSW=7 GEOPAR=geopar
fixloc resloca2
!geoma 218s01.img OUT PARMS=geopar
!list OUT (501,501,10,10)
end-proc
$ Return
$!#############################################################################
