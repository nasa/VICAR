$!****************************************************************************
$!
$! Build proc for MIPL module wavlen
$! VPACK Version 1.8, Friday, June 29, 2001, 18:57:54
$!
$! Execute by entering:		$ @wavlen
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
$ write sys$output "*** module wavlen ***"
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
$ write sys$output "Invalid argument given to wavlen.com file -- ", primary
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
$   if F$SEARCH("wavlen.imake") .nes. ""
$   then
$      vimake wavlen
$      purge wavlen.bld
$   else
$      if F$SEARCH("wavlen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake wavlen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @wavlen.bld "STD"
$   else
$      @wavlen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create wavlen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack wavlen.com -
	-s wavlen.f -
	-i wavlen.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create wavlen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE WAVLEN(NAME,JSTART,NCHAN,WAVES)
      CHARACTER*20 NAME
      REAL WAVES(NCHAN)
      REAL DATABASE(224,7)
      REAL AVIRISA(90)/
     + 0.3969,0.4068,0.4166,0.4265,0.4364,0.4462,0.4561,0.4660,0.4758,
     + 0.4857,0.4956,0.5054,0.5153,0.5251,0.5350,0.5449,0.5547,0.5646,
     + 0.5745,0.5843,0.5942,0.6041,0.6139,0.6238,0.6337,0.6435,0.6534,
     + 0.6633,0.6731,0.6830,0.6929,0.7027,0.6747,0.6844,0.6940,0.7037,
     + 0.7133,0.7230,0.7327,0.7423,0.7520,0.7616,0.7713,0.7810,0.7906,
     + 0.8003,0.8099,0.8196,0.8293,0.8389,0.8486,0.8582,0.8679,0.8776,
     + 0.8872,0.8969,0.9065,0.9162,0.9259,0.9355,0.9452,0.9548,0.9645,
     + 0.9742,0.9838,0.9935,1.0031,1.0128,1.0225,1.0321,1.0418,1.0514,
     + 1.0611,1.0708,1.0804,1.0901,1.0997,1.1094,1.1191,1.1287,1.1384,
     + 1.1480,1.1577,1.1674,1.1770,1.1867,1.1963,1.2060,1.2156,1.2253/
      REAL AVIRISB(134)/
     + 1.2350,1.2446,1.2543,1.2639,1.2736,1.2833,1.2446,1.2545,1.2644,
     + 1.2743,1.2841,1.2940,1.3039,1.3138,1.3237,1.3336,1.3435,1.3534,
     + 1.3632,1.3731,1.3830,1.3929,1.4028,1.4127,1.4226,1.4324,1.4423,
     + 1.4522,1.4621,1.4720,1.4819,1.4918,1.5016,1.5115,1.5214,1.5313,
     + 1.5412,1.5511,1.5610,1.5709,1.5807,1.5906,1.6005,1.6104,1.6203,
     + 1.6302,1.6401,1.6499,1.6598,1.6697,1.6796,1.6895,1.6994,1.7093,
     + 1.7192,1.7290,1.7389,1.7488,1.7587,1.7686,1.7785,1.7884,1.7982,
     + 1.8081,1.8180,1.8279,1.8378,1.8477,1.8576,1.8674,1.8300,1.8399,
     + 1.8499,1.8598,1.8697,1.8796,1.8895,1.8995,1.9094,1.9193,1.9292,
     + 1.9391,1.9491,1.9590,1.9689,1.9788,1.9887,1.9987,2.0086,2.0185,
     + 2.0284,2.0383,2.0483,2.0582,2.0681,2.0780,2.0879,2.0978,2.1078,
     + 2.1177,2.1276,2.1375,2.1474,2.1574,2.1673,2.1772,2.1871,2.1970,
     + 2.2070,2.2169,2.2268,2.2367,2.2466,2.2566,2.2665,2.2764,2.2863,
     + 2.2962,2.3062,2.3161,2.3260,2.3359,2.3458,2.3558,2.3657,2.3756,
     + 2.3855,2.3954,2.4053,2.4153,2.4252,2.4351,2.4450,2.4549/
      REAL TIMS(6)/8.384,8.64,9.152,9.984,10.624,11.264/
      REAL GEOSCAN(24)/
     + 0.522,0.583,0.645,0.693,0.717,0.740,0.830,0.873,
     + 0.915,0.955,2.044,2.088,2.136,2.176,2.220,2.264,
     + 2.308,2.352, 8.64,9.17, 9.7  ,10.22,10.75,11.28/
      REAL AV87A(97)/0.4000,0.4098,0.4196,0.4294,0.4392,0.4490,0.4588,
     + 0.4686,0.4784,0.4882,0.4980,0.5078,0.5176,0.5274,0.5372,0.5470,
     + 0.5568,0.5666,0.5764,0.5862,0.5960,0.6058,0.6156,0.6254,0.6352,
     + 0.6450,0.6548,0.6646,0.6744,0.6842,0.6940,0.7038,0.7236,0.7234,
     + 0.7332,0.7430,0.7528,0.7626,0.7724,0.7822,0.7920,0.8018,0.8116,
     + 0.8214,0.8312,0.8410,0.8508,0.8606,0.8704,0.8802,0.8900,0.8998,
     + 0.9096,0.9194,0.9292,0.9390,0.9488,0.9586,0.9684,0.9782,0.9880,
     + 0.9978,1.0076,1.0174,1.0272,1.0370,1.0468,1.0566,1.0664,1.0762,
     + 1.0860,1.0958,1.1056,1.1154,1.1252,1.1350,1.1448,1.1546,1.1644,
     + 1.1742,1.1840,1.1938,1.2036,1.2134,1.2232,1.2330,1.2428,1.2526,
     + 1.2624,1.2722,1.2820,1.2918,1.3016,1.3114,1.3212,1.3310,1.3408/
      REAL AV87B(113)/
     + 1.3506,1.3604,1.3702,1.3800,1.3898,1.3996,1.4094,1.4192,1.4290,
     + 1.4388,1.4486,1.4584,1.4682,1.4780,1.4878,1.4976,1.5074,1.5172,
     + 1.5270,1.5368,1.5466,1.5564,1.5662,1.5760,1.5858,1.5956,1.6054,
     + 1.6152,1.6250,1.6348,1.6446,1.6544,1.6642,1.6740,1.6838,1.6936,
     + 1.7034,1.7132,1.7230,1.7328,1.7426,1.7524,1.7622,1.7720,1.7818,
     + 1.7916,1.8014,1.8112,1.8210,1.8308,1.8406,1.8504,1.8602,1.8700,
     + 1.8798,1.8896,1.8994,1.9092,1.9190,1.9288,1.9386,1.9484,1.9582,
     + 1.9680,1.9778,1.9876,1.9974,2.0072,2.0170,2.0268,2.0366,2.0464,
     + 2.0562,2.0660,2.0758,2.0856,2.0954,2.1052,2.1150,2.1248,2.1346,
     + 2.1444,2.1542,2.1640,2.1738,2.1836,2.1934,2.2032,2.2130,2.2228,
     + 2.2326,2.2424,2.2522,2.2620,2.2718,2.2816,2.2914,2.3012,2.3110,
     + 2.3208,2.3306,2.3404,2.3502,2.3600,2.3698,2.3796,2.3894,2.3992,
     + 2.4090,2.4188,2.4286,2.4384,2.4482/
      REAL TM(6)/0.48,0.55,0.66,0.83,1.55,2.22/
      REAL AV90A(159)/
     + 0.4004,0.4102,0.4200,0.4299,0.4397,0.4495,0.4593,0.4691,0.4789,
     + 0.4887,0.4985,0.5084,0.5182,0.5280,0.5378,0.5476,0.5574,0.5672,
     + 0.5770,0.5868,0.5967,0.6065,0.6163,0.6261,0.6359,0.6457,0.6555,
     + 0.6653,0.6751,0.6850,0.6948,0.7046,0.6806,0.6902,0.6998,0.7094,
     + 0.7190,0.7286,0.7382,0.7478,0.7574,0.7670,0.7766,0.7862,0.7958,
     + 0.8054,0.8150,0.8246,0.8342,0.8438,0.8534,0.8630,0.8726,0.8822,
     + 0.8918,0.9014,0.9110,0.9206,0.9302,0.9398,0.9494,0.9590,0.9686,
     + 0.9782,0.9878,0.9974,1.0070,1.0166,1.0262,1.0358,1.0454,
     + 1.0550,1.0646,1.0742,1.0838,1.0934,1.1030,1.1126,1.1222,
     + 1.1318,1.1414,1.1510,1.1606,1.1702,1.1798,1.1894,1.1990,
     + 1.2086,1.2182,1.2278,1.2374,1.2470,1.2566,1.2662,1.2758,
     + 1.2854,1.2428,1.2527,1.2626,1.2725,1.2823,1.2922,1.3021,
     + 1.3120,1.3219,1.3318,1.3417,1.3516,1.3615,1.3714,1.3813,
     + 1.3912,1.4011,1.4110,1.4209,1.4308,1.4407,1.4506,1.4605,
     + 1.4704,1.4802,1.4901,1.5000,1.5099,1.5198,1.5297,1.5396,
     + 1.5495,1.5594,1.5693,1.5792,1.5891,1.5990,1.6089,1.6188,
     + 1.6287,1.6386,1.6485,1.6584,1.6683,1.6782,1.6880,1.6979,
     + 1.7078,1.7177,1.7276,1.7375,1.7474,1.7573,1.7672,1.7771,
     + 1.7870,1.7969,1.8068,1.8167,1.8266,1.8365,1.8464,1.8563/
      REAL AV90B(65)/
     + 1.8662,1.8307,1.8406,1.8506,1.8605,1.8705,1.8804,1.8904,
     + 1.9003,1.9103,1.9202,1.9301,1.9401,1.9500,1.9600,1.9699,
     + 1.9799,1.9898,1.9998,2.0097,2.0196,2.0296,2.0395,2.0495,
     + 2.0594,2.0694,2.0793,2.0893,2.0992,2.1092,2.1191,2.1290,
     + 2.1390,2.1489,2.1589,2.1688,2.1788,2.1887,2.1987,2.2086,
     + 2.2185,2.2285,2.2384,2.2484,2.2583,2.2683,2.2782,2.2882,
     + 2.2981,2.3081,2.3180,2.3279,2.3379,2.3478,2.3578,2.3677,
     + 2.3777,2.3876,2.3976,2.4075,2.4174,2.4274,2.4373,2.4473,
     + 2.4572/    
      REAL MASTERTIR(10)/
     +  7.7700, 8.1842, 8.6268, 9.0954, 9.7266,10.1380,10.6812,11.3702,
     + 12.1734,12.9267/
C
      EQUIVALENCE (DATABASE(1,1),AVIRISA(1)),(DATABASE(91,1),AVIRISB(1))
      EQUIVALENCE (DATABASE(1,2),TIMS(1))
      EQUIVALENCE (DATABASE(1,3),GEOSCAN(1))
      EQUIVALENCE (DATABASE(1,4),AV87A(1)),    (DATABASE(98,4),AV87B(1))
      EQUIVALENCE (DATABASE(1,5),TM(1))
      EQUIVALENCE (DATABASE(1,6),AV90A(1)),   (DATABASE(160,6),AV90B(1))
      EQUIVALENCE (DATABASE(1,7),MASTERTIR(1))
C
      IF (NAME .EQ. 'AVIRIS') THEN
	  IBASE = 1
      ELSE IF (NAME .EQ. 'TIMS') THEN
	  IBASE = 2
      ELSE IF (NAME .EQ. 'GEOSCAN') THEN
          IBASE = 3
      ELSE IF (NAME .EQ. '87AVIRIS') THEN
          IBASE = 4
      ELSE IF (NAME .EQ. 'TM') THEN
          IBASE = 5
      ELSE IF (NAME .EQ. '90AVIRIS') THEN
	  IBASE = 6
      ELSE IF (NAME .EQ. 'MASTERTIR') THEN
	  IBASE = 7
      ELSE
	  DO I=1,NCHAN
	      WAVES(I) = JSTART + I - 1
	  END DO
	  RETURN
      END IF
C
      K = JSTART
C
      DO I = 1,NCHAN
          WAVES(I) = DATABASE(K,IBASE)
          K = K + 1
      END DO
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create wavlen.imake
#define SUBROUTINE wavlen

#define MODULE_LIST wavlen.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
