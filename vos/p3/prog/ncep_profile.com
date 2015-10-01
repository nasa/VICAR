$!****************************************************************************
$!
$! Build proc for MIPL module ncep_profile
$! VPACK Version 1.8, Monday, March 07, 2005, 19:54:00
$!
$! Execute by entering:		$ @ncep_profile
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
$ write sys$output "*** module ncep_profile ***"
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
$ write sys$output "Invalid argument given to ncep_profile.com file -- ", primary
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
$   if F$SEARCH("ncep_profile.imake") .nes. ""
$   then
$      vimake ncep_profile
$      purge ncep_profile.bld
$   else
$      if F$SEARCH("ncep_profile.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ncep_profile
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ncep_profile.bld "STD"
$   else
$      @ncep_profile.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ncep_profile.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ncep_profile.com -
	-s ncep_profile.f -
	-p ncep_profile.pdf -
	-i ncep_profile.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ncep_profile.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C   8 May 97   ...rea...   Initial release
C  12 Oct 00   ...rea...   Accomodate new format
C  22 Apr 04   ...rea...   Fix bug in adding relative humidity from std model
C                          atmospheres to Oct, 2000 format data (bad index)
C  17 Feb 05   ...rea...   Add DATE parameter
C   8 Mar 05   ...rea...   Change name from EMC_INTERP to NCEP_PROFILE, add
C                          ELEV keyword
C
C     NCEP_PROFILE is a program to perform an 8 point weighted interpolation
C     (in both space and time) of NCEP GDAS global assimilation model data.
C
C     The input files are in the JPL abridged format.  This format is 45, 77,
C     or 141 channels of halfword data in BSQ format, 181 lines by 360 samples.
C     The upper left corner is 90 degrees North, 0 degrees East/West.
C     Prior to October 1, 2000, the 77 channels have the following meanings:
C
C     Channel  Attribute                      Units        Scaling Factor
C      1-16    Geopotential height            meters         1.0
C     17-32    Temperature                    deg C        100.0
C     33-39    Relative Humidity              percent        1.0
C        40    Surface Pressure               millibars     10.0
C        41    Geopotential height at surface meters         1.0
C        42    Pressure reduced to MSL        millibars     10.0
C        43    Pressure at tropopause         millibars     10.0
C        44    Temperature at tropopause      deg C        100.0
C        45    Precipitable water in column   mm            10.0
C     46-61    Zonal Wind                     m/sec        100.0
C     62-77    Meridional Wind                m/sec        100.0
C              (Channels 46-77 are absent, prior to April 1, 1999.)
C
C     For data October 1, 2000 and later, the format is:
C
C 1-26    geopotential height at 26 isobaric levels 1000-10 mb    m         1.0
C 27-52   temperature at 26 isobaric levels 1000-10 mb            deg C   100.0
C 53-78   zonal wind at 26 isobaric levels 1000-10 mb             m/s     100.0
C 79-104  meridional wind at 26 isobaric levels 1000-10 mb        m/s     100.0
C 105-125 relative humidity at 21 isobaric levels 1000-100 mb     %         1.0
C 126     surface pressure                                        mb       10.0
C 127     surface geopotential height                             m         1.0
C 128     surface temperature                                     deg C   100.0
C 129     temperature at 2 meters above surface                   deg C   100.0
C 130     relative humidity at 2 meters above surface             %         1.0
C 131     pressure at tropopause                                  mb       10.0
C 132     geopotential height at tropopause                       m         1.0
C 133     temperature at tropopause                               deg C   100.0
C 134     precipitable water                                      cm     1000.0
C 135     pressure reduced to MSL                                 mb       10.0
C 136     Land / Sea Mask                                         none      0/1
C 137     Ice Concentration                                       none      0/1
C 138     Cloud Cover                                             %         1.0
C 139     Snow Depth                                              kg/m^2    1.0
C 140     geopotential height anomaly at 500 meters               m         1.0
C 141     geopotential height anomaly at 1000 meters              m         1.0
C
	REAL BUFIN(360),PROFILES(141,8),BUFOUT(141)
        REAL SCALE(141)/26*1.0, 78*100.0, 21*1.0, 10.0, 1.0, 100.0,
     +                  100.0, 1.0, 10.0, 1.0, 100.0, 1000.0, 10.0,
     +                  6*1.0/
	LOGICAL XVPTST,QPRT,QTABFILE
	CHARACTER*80 MSG,TABNAME
	CHARACTER*30 DATE
	CHARACTER*5 NORTHSOUTH1,NORTHSOUTH2
	CHARACTER*4 EASTWEST1,EASTWEST2
C							  Open input data sets
	CALL XVEACTION('SA',' ')
	CALL XVUNIT(INP1,'INP',1,ISTAT,0)
	CALL XVOPEN(INP1,ISTAT,'U_FORMAT','REAL',' ')
	CALL XVUNIT(INP2,'INP',2,ISTAT,0)
	CALL XVOPEN(INP2,ISTAT,'U_FORMAT','REAL',' ')
C								Get parameters
	CALL XVGET(INP1,ISTAT,'NB',NB,' ')
	CALL XVPARM('TIME',TIME,ICNT,IDEF,1)
	CALL XVPARM('LAT',XLAT,ICNT,IDEF,1)
	CALL XVPARM('LONG',XLONG,ICNT,IDEF,1)
	CALL XVPARM('DATE',DATE,ICNT,IDEF,1)
	CALL XVPARM('ELEV',ELEV,ICNT,IDEF,1)
	IF (ICNT .EQ. 0) THEN
	    ELEV = -99999.0
	END IF
	QPRT = XVPTST('PRINTALL')
	CALL XVPARM('TABLE',TABNAME,ICNT,IDEF,1)
	QTABFILE = ICNT .EQ. 1
	MODEL = 6
	IF (XVPTST('TROP')) MODEL=1
	IF (XVPTST('MLS')) MODEL=2
	IF (XVPTST('MLW')) MODEL=3
	IF (XVPTST('SAS')) MODEL=4
	IF (XVPTST('SAW')) MODEL=5
C					       Compute line, sample, and weights
	XLINE = 91.0 - XLAT
	LINE = INT(XLINE)
	FRACLAT = XLINE - FLOAT(LINE)
	XSAMP = XLONG + 1.0
	IF (XSAMP .LT. 0.0) XSAMP = XSAMP + 360.0
	ISAMP = INT(XSAMP)
	FRACLONG = XSAMP - FLOAT(ISAMP)
	IF (ISAMP.EQ.0 .OR. ISAMP.EQ.360) THEN
	    ISAMP1 = 360
	    ISAMP2 = 1
	ELSE
	    ISAMP1= ISAMP
	    ISAMP2= ISAMP + 1
	END IF
	XTIME = MOD(TIME,6.0)
	DAYFRAC = XTIME / 6.0
	X = DAYFRAC
	Y = FRACLAT
	Z = FRACLONG
	WT1 = (1.0-X) * (1.0-Y) * (1.0-Z)
	WT2 = (1.0-X) * (1.0-Y) *    Z
	WT3 = (1.0-X) *    Y    * (1.0-Z)
	WT4 = (1.0-X) *    Y    *    Z
	WT5 =    X    * (1.0-Y) * (1.0-Z)
	WT6 =    X    * (1.0-Y) *    Z
	WT7 =    X    *    Y    * (1.0-Z)
	WT8 =    X    *    Y    *    Z
C						Report the interpolation weights
	CALL XVMESSAGE('The interpolation weights are:',' ')
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('first file                     second file',' ')
	CALL XVMESSAGE('        SAMPLE                          SAMPLE',
     +			' ')
	WRITE(MSG,300) ISAMP,ISAMP+1,ISAMP,ISAMP+1
  300	FORMAT('   LINE',I10,I10,I20,I10)
	CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,310) LINE,WT1,WT2,WT5,WT6
  310	FORMAT(I7,2F10.5,10X,2F10.5)
	CALL XVMESSAGE(MSG,' ')
	WRITE(MSG,310) LINE+1,WT3,WT4,WT7,WT8
	CALL XVMESSAGE(MSG,' ')
	CALL XVMESSAGE(' ',' ')
C								Read in the data
	DO IBAND=1,NB
	    CALL XVREAD(INP1,BUFIN,ISTAT,'LINE',LINE,'BAND',IBAND,' ')
	    PROFILES(IBAND,1) = BUFIN(ISAMP1)
	    PROFILES(IBAND,2) = BUFIN(ISAMP2)
	    CALL XVREAD(INP1,BUFIN,ISTAT,'LINE',LINE+1,'BAND',IBAND,' ')
	    PROFILES(IBAND,3) = BUFIN(ISAMP1)
	    PROFILES(IBAND,4) = BUFIN(ISAMP2)
	    CALL XVREAD(INP2,BUFIN,ISTAT,'LINE',LINE,'BAND',IBAND,' ')
	    PROFILES(IBAND,5) = BUFIN(ISAMP1)
	    PROFILES(IBAND,6) = BUFIN(ISAMP2)
	    CALL XVREAD(INP2,BUFIN,ISTAT,'LINE',LINE+1,'BAND',IBAND,' ')
	    PROFILES(IBAND,7) = BUFIN(ISAMP1)
	    PROFILES(IBAND,8) = BUFIN(ISAMP2)
	END DO
C								Rescale the data
	IF (NB .LT. 141) THEN
	    DO IX=1,8
		DO IBAND=17,32
		    PROFILES(IBAND,IX) = PROFILES(IBAND,IX) / 100.0
		END DO
		PROFILES(40,IX) = PROFILES(40,IX) / 10.0
		PROFILES(42,IX) = PROFILES(42,IX) / 10.0
		PROFILES(43,IX) = PROFILES(43,IX) /10.0
		PROFILES(44,IX) = PROFILES(44,IX) / 100.0
		PROFILES(45,IX) = PROFILES(45,IX) / 10.0
	    END DO
	ELSE
	    DO IX=1,8
		DO IBAND=1,NB
		    PROFILES(IBAND,IX) = PROFILES(IBAND,IX)/SCALE(IBAND)
		END DO
	    END DO
	END IF
C							    Do the interpolation
	DO IBAND=1,NB
	    BUFOUT(IBAND) = WT1*PROFILES(IBAND,1) +
     +			    WT2*PROFILES(IBAND,2) +
     +			    WT3*PROFILES(IBAND,3) +
     +			    WT4*PROFILES(IBAND,4) +
     +			    WT5*PROFILES(IBAND,5) +
     +			    WT6*PROFILES(IBAND,6) +
     +			    WT7*PROFILES(IBAND,7) +
     +			    WT8*PROFILES(IBAND,8)
	END DO
C							      Print the profiles
	IF (QPRT) THEN
C							compute grid point time
	    ITIME = NINT(TIME-XTIME)
	    WRITE (MSG,500) ITIME
  500	    FORMAT(' At  ',I2,':00 UT')
	    CALL XVMESSAGE(MSG,' ')
C							compute grid lat/long
	    LAT1 = 91 - LINE
	    LAT2 = LAT1 - 1
	    IF (LAT1 .GE. 0) THEN
		NORTHSOUTH1 = 'North'
	    ELSE
		NORTHSOUTH1 = 'South'
		LAT1 = -LAT1
	    END IF
	    IF (LAT2 .GE. 0) THEN
		NORTHSOUTH2 = 'North'
	    ELSE
		NORTHSOUTH2 = 'South'
		LAT2 = -LAT2
	    END IF
	    LONG1 = ISAMP1 - 1
	    LONG2 = ISAMP2 - 1
	    IF (LONG1 .LE. 180) THEN
		EASTWEST1 = 'East'
	    ELSE
		EASTWEST1 = 'West'
		LONG1 = 360 - LONG1
	    END IF
	    IF (LONG2 .LE. 180) THEN
		EASTWEST2 = 'East'
	    ELSE
		EASTWEST2 = 'West'
		LONG2 = 360 - LONG2
	    END IF
C								first 2 profiles
	    WRITE (MSG,510) LAT1,NORTHSOUTH1,LONG1,EASTWEST1,
     +			    LAT1,NORTHSOUTH1,LONG2,EASTWEST2
  510	    FORMAT(' At',I3,' ',A5,I5,' ',A4,20X,'At',I3,' ',A5,I5,' ',
     +		   A4)
	    CALL XVMESSAGE(MSG,' ')
	    CALL PRINTHDR
	    CALL PRINTTABLE(PROFILES(1,1),PROFILES(1,2),NB,MSG)
C								second 2 profile
	    WRITE (MSG,510) LAT2,NORTHSOUTH2,LONG1,EASTWEST1,
     +			    LAT2,NORTHSOUTH2,LONG2,EASTWEST2
	    CALL XVMESSAGE(MSG,' ')
	    CALL PRINTHDR
	    CALL PRINTTABLE(PROFILES(1,3),PROFILES(1,4),NB,MSG)
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' ',' ')
C								third 2 profiles
	    ITIME = MOD(ITIME+6,24)
	    WRITE (MSG,500) ITIME
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,510) LAT1,NORTHSOUTH1,LONG1,EASTWEST1,
     +			    LAT1,NORTHSOUTH1,LONG2,EASTWEST2
	    CALL XVMESSAGE(MSG,' ')
	    CALL PRINTHDR
	    CALL PRINTTABLE(PROFILES(1,5),PROFILES(1,6),NB,MSG)
C								last 2 profiles
	    WRITE (MSG,510) LAT2,NORTHSOUTH2,LONG1,EASTWEST1,
     +			    LAT2,NORTHSOUTH2,LONG2,EASTWEST2
	    CALL XVMESSAGE(MSG,' ')
	    CALL PRINTHDR
	    CALL PRINTTABLE(PROFILES(1,7),PROFILES(1,8),NB,MSG)
	END IF
C							 Print the interpolation
	IHOUR = INT(TIME)
	MIN = NINT(60.0*MOD(TIME,1.0))
	WRITE (MSG,600) IHOUR,MIN,DATE
  600	FORMAT(' At  ',I2,':',I2.2,' UT   ',A30)
	CALL XVMESSAGE(MSG,' ')
C
	IF (XLAT .GE. 0.0) THEN
	    NORTHSOUTH1 = 'North'
	ELSE
	    NORTHSOUTH1 = 'South'
	    XLAT = -XLAT
	END IF
	IF (XLONG .GE. 0.0) THEN
	    EASTWEST1 = 'East'
	ELSE
	    EASTWEST1 = 'West'
	    XLONG = -XLONG
	END IF
	WRITE (MSG,700) XLAT,NORTHSOUTH1,XLONG,EASTWEST1
  700	FORMAT(' At',F9.4,' ',A5,F10.4,' ',A4)
	CALL XVMESSAGE(MSG,' ')
	CALL XVMESSAGE('Pressure Altitude Temperature Relative',' ')
	CALL XVMESSAGE('  (mb)   (meters)   (deg C)   Humidity',' ')
	CALL PRINTTABLE2(BUFOUT,NB,MSG)
C						           Output the rstab file
	IF (QTABFILE) THEN
	    OPEN (77,FILE=TABNAME)
	    WRITE (77,600) IHOUR,MIN,DATE
	    WRITE (77,700) XLAT,NORTHSOUTH1,XLONG,EASTWEST1
	    CALL PRINTTABLE3(BUFOUT,MODEL,NB,ELEV)
	    CLOSE (77)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PRINTHDR
C
	CALL XVMESSAGE('Pressure Altitude Temperature Relative    Pressu
     +re Altitude Temperature Relative',' ')
	CALL XVMESSAGE('  (mb)   (meters)   (deg C)   Humidity      (mb)
     +   (meters)   (deg C)   Humidity',' ')
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PRINTTABLE(ARR1,ARR2,NB,MSG)
C
	REAL ARR1(*),ARR2(*)
	INTEGER IP26(26)/1000,975,950,925,900,850,800,750,700,650,600,
     +			  550,500,450,400,350,300,250,200,150,100, 70,
     +			   50, 30, 20, 10/
	INTEGER IPRESS(16)/1000,925,850,700,500,400,300,250,200,150,
     +			   100,70,50,30,20,10/
	CHARACTER*80 MSG
C
	IF (NB .LT. 141) THEN
	    DO I=1,7
		WRITE (MSG,20) IPRESS(I),ARR1(I),ARR1(I+16),ARR1(I+32),
     +			       IPRESS(I),ARR2(I),ARR2(I+16),ARR2(I+32)
   20		FORMAT(I6,F10.1,F10.2,F10.1,I12,F10.1,F10.2,F10.1)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    DO I=8,16
		WRITE (MSG,30) IPRESS(I),ARR1(I),ARR1(I+16),
     +			       IPRESS(I),ARR2(I),ARR2(I+16)
   30		FORMAT(I6,F10.1,F10.2,I22,F10.1,F10.2)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    WRITE (MSG,40) ARR1(40),ARR2(40)
   40	    FORMAT('Surface Pressure (mb)',F16.1,5X,
     +		   'Surface Pressure (mb)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,50) ARR1(41),ARR2(41)
   50	    FORMAT('Surface Elevation (m)',F16.1,5X,
     +		   'Surface Elevation (m)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,60) ARR1(42),ARR2(42)
   60	    FORMAT('Pressure Reduced to MSL (mb)',F9.1,5X,
     +		   'Pressure Reduced to MSL (mb)',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,70) ARR1(43),ARR2(43)
   70	    FORMAT('Pressure @tropopause (mb)',F12.1,5X,
     +		   'Pressure @tropopause (mb)',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,80) ARR1(44),ARR2(44)
   80	    FORMAT('Temperature @tropopause (deg C)',F7.2,4X,
     +		   'Temperature @tropopause (deg C)',F7.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,90) ARR1(45),ARR2(45)
   90	    FORMAT('Column Water (mm)',F20.1,5X,
     +		   'Column Water (mm)',F20.1)
	    CALL XVMESSAGE(MSG,' ')
	ELSE
	    DO I=1,21
		WRITE (MSG,120) IP26(I),ARR1(I),ARR1(I+26),ARR1(I+104),
     +			       IP26(I),ARR2(I),ARR2(I+26),ARR2(I+104)
  120		FORMAT(I6,F10.1,F10.2,F10.1,I12,F10.1,F10.2,F10.1)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    DO I=22,26
		WRITE (MSG,130) IP26(I),ARR1(I),ARR1(I+26),
     +			       IP26(I),ARR2(I),ARR2(I+26)
  130		FORMAT(I6,F10.1,F10.2,I22,F10.1,F10.2)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    WRITE (MSG,140) ARR1(126),ARR2(126)
  140	    FORMAT('Surface Pressure (mb)',F16.1,5X,
     +		   'Surface Pressure (mb)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,150) ARR1(127),ARR2(127)
  150	    FORMAT('Surface Elevation (m)',F16.1,5X,
     +		   'Surface Elevation (m)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,152) ARR1(128),ARR2(128)
  152	    FORMAT('Surface Temperature (C)',F15.2,4X,
     +		   'Surface Temperature (C)',F15.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,154) ARR1(130),ARR2(130)
  154	    FORMAT('Surface Relative Humidity (%)',F8.1,5X,
     +		   'Surface Relative Humidity (%)',F8.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,170) ARR1(131),ARR2(131)
  170	    FORMAT('Pressure @tropopause (mb)',F12.1,5X,
     +		   'Pressure @tropopause (mb)',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,175) ARR1(132),ARR2(132)
  175	    FORMAT('Altitude @tropopause (m) ',F12.1,5X,
     +		   'Altitude @tropopause (m) ',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,180) ARR1(133),ARR2(133)
  180	    FORMAT('Temperature @tropopause (deg C)',F7.2,4X,
     +		   'Temperature @tropopause (deg C)',F7.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,190) ARR1(134),ARR2(134)
  190	    FORMAT('Column Water (cm)',F21.2,4X,
     +		   'Column Water (cm)',F21.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,195) ARR1(135),ARR2(135)
  195	    FORMAT('Pressure Reduced to MSL (mb)',F9.1,5X,
     +		   'Pressure Reduced to MSL (mb)',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,197) ARR1(138),ARR2(138)
  197	    FORMAT('Cloud Cover (%)             ',F9.1,5X,
     +		   'Cloud Cover (%)             ',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	END IF
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' ',' ')
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PRINTTABLE2(BUFOUT,NB,MSG)
C
	REAL BUFOUT(*)
	INTEGER IP26(26)/1000,975,950,925,900,850,800,750,700,650,600,
     +			  550,500,450,400,350,300,250,200,150,100, 70,
     +			   50, 30, 20, 10/
	INTEGER IPRESS(16)/1000,925,850,700,500,400,300,250,200,150,
     +			   100,70,50,30,20,10/
	CHARACTER*80 MSG
C
	IF (NB .LT.141) THEN
	    DO I=1,7
		WRITE (MSG,20) IPRESS(I),BUFOUT(I),BUFOUT(I+16),
     +			       BUFOUT(I+32)
   20		FORMAT(I6,F10.1,F10.2,F10.1)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    DO I=8,16
		WRITE (MSG,30) IPRESS(I),BUFOUT(I),BUFOUT(I+16)
   30		FORMAT(I6,F10.1,F10.2)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    WRITE (MSG,40) BUFOUT(40)
   40	    FORMAT('Surface Pressure (mb)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,50) BUFOUT(41)
   50	    FORMAT('Surface Elevation (m)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,60) BUFOUT(42)
   60	    FORMAT('Pressure Reduced to MSL (mb)',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,70) BUFOUT(43)
   70	    FORMAT('Pressure @tropopause (mb)',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,80) BUFOUT(44)
   80	    FORMAT('Temperature @tropopause (deg C)',F7.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,90) BUFOUT(45)
   90	    FORMAT('Column Water (mm)',F20.1)
	    CALL XVMESSAGE(MSG,' ')
	ELSE
	    DO I=1,21
		WRITE (MSG,120) IP26(I),BUFOUT(I),BUFOUT(I+26),
     +				BUFOUT(I+104)
  120		FORMAT(I6,F10.1,F10.2,F10.1)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    DO I=22,26
		WRITE (MSG,130) IP26(I),BUFOUT(I),BUFOUT(I+26)
  130		FORMAT(I6,F10.1,F10.2)
		CALL XVMESSAGE(MSG,' ')
	    END DO
	    WRITE (MSG,140) BUFOUT(126)
  140	    FORMAT('Surface Pressure (mb)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,150) BUFOUT(127)
  150	    FORMAT('Surface Elevation (m)',F16.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,152) BUFOUT(128)
  152	    FORMAT('Surface Temperature (C)',F15.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,154) BUFOUT(130)
  154	    FORMAT('Surface Relative Humidity (%)',F8.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,170) BUFOUT(131)
  170	    FORMAT('Pressure @tropopause (mb)',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,175) BUFOUT(132)
  175	    FORMAT('Altitude @tropopause (m) ',F12.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,180) BUFOUT(133)
  180	    FORMAT('Temperature @tropopause (deg C)',F7.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,190) BUFOUT(134)
  190	    FORMAT('Column Water (cm)',F21.2)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,195) BUFOUT(135)
  195	    FORMAT('Pressure Reduced to MSL (mb)',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,197) BUFOUT(138)
  197	    FORMAT('Cloud Cover (%)             ',F9.1)
	    CALL XVMESSAGE(MSG,' ')
	END IF
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' ',' ')
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PRINTTABLE3(BUFOUT,MODEL,NB,ELEV)
C
	REAL BUFOUT(*)
C
	REAL TEMP(6)/
     +        -82.5,    -82.7,   -54.6,   -82.8,   -54.7,     -78.1/
	REAL RH(6,10)/
     +         13.6,     21.3,    15.6,     9.30,   17.7,      28.2,
     +	        7.84,     9.11,    3.93,    1.65,    5.87,     15.2,
     +          8.04,     2.77,    2.13,    0.764,   2.13,      3.96,
     +         18.3,      1.24,    1.56,    0.502,   1.53,      1.33,
     +          5.01,     0.659,   1.23,    0.390,   1.30,      0.909,
     +          1.13,     0.370,   0.917,   0.294,   1.11,      0.624,
     +          0.240,    0.167,   0.561,   0.145,   0.880,     0.275,
     +		0.105,    0.0853,  0.364,   0.0722,  0.646,     0.151,
     +          0.0201,   0.0186,  0.138,   0.0167,  0.183,     0.0450,
     +          0.0001298,0.000118,0.000023,0.000125,0.0000056,0.000091/
C
	REAL PRESS27(27)/1000.0,975.0,950.0,925.0,900.0,850.0,800.0,
     +			  750.0,700.0,650.0,600.0,550.0,500.0,450.0,
     +			  400.0,350.0,300.0,250.0,200.0,150.0,100.0,
     +			   70.0, 50.0, 30.0, 20.0, 10.0,0.01/
	REAL PRESS(17)/1000.0,925.0,850.0,700.0,500.0,400.0,300.0,250.0,
     +		       200.0,150.0,100.0,70.0,50.0,30.0,20.0,10.0,0.01/
C
	WRITE (77,*) 'Altitude Pressure Temperature Relative'
	WRITE (77,*) '  (km)     (mb)     (deg C)   Humidity'
C
	J = 0
	IF (NB .LT. 141) THEN
	    IF (ELEV .GT. -9000.0) THEN
		J = 1
		DO WHILE (J.LT.16 .AND. ELEV.GT.BUFOUT(J+1))
		    J = J + 1
		END DO
		X = (BUFOUT(J+1)-ELEV) / (BUFOUT(J+1)-BUFOUT(J))
		XELEV = ELEV / 1000.0
		XPRESS = X*PRESS(J) + (1.0-X)*PRESS(J+1)
		XTEMP = X*BUFOUT(J+16) + (1.0-X)*BUFOUT(J+17)
		XRH = X*BUFOUT(J+32) + (1.0-X)*BUFOUT(J+33)
		WRITE (77,20) XELEV,XPRESS,XTEMP,XRH
   20		FORMAT(F10.4,F8.2,F9.2,F13.6)
		IF (ELEV .LT. BUFOUT(1)) J = 0
	    END IF
	    DO I=J+1,7
		WRITE (77,20) BUFOUT(I)/1000.0,PRESS(I),BUFOUT(I+16),
     +			      BUFOUT(I+32)
	    END DO
	    J = MAX(J+1,8)
	    DO I=J,16
		WRITE (77,20) BUFOUT(I)/1000.0,PRESS(I),BUFOUT(I+16),
     +			      RH(MODEL,I-7)
	    END DO
	    WRITE (77,20) 100.0,PRESS(17),TEMP(MODEL),RH(MODEL,10)
	ELSE
	    IF (ELEV .GT. -9000.0) THEN
		J = 1
		DO WHILE (J.LT.26 .AND. ELEV.GT.BUFOUT(J+1))
		    J = J + 1
		END DO
		X = (BUFOUT(J+1)-ELEV) / (BUFOUT(J+1)-BUFOUT(J))
		XELEV = ELEV / 1000.0
		XPRESS = X*PRESS27(J) + (1.0-X)*PRESS27(J+1)
		XTEMP = X*BUFOUT(J+26) + (1.0-X)*BUFOUT(J+27)
		XRH = X*BUFOUT(J+104) + (1.0-X)*BUFOUT(J+105)
		WRITE (77,20) XELEV,XPRESS,XTEMP,XRH
		IF (ELEV .LT. BUFOUT(1)) J = 0
	    END IF
	    DO I=J+1,21
		WRITE (77,20) BUFOUT(I)/1000.0,PRESS27(I),BUFOUT(I+26),
     +			      BUFOUT(I+104)
	    END DO
	    J = MAX(J+1,22)
	    DO I=J,26
		WRITE (77,20) BUFOUT(I)/1000.0,PRESS27(I),BUFOUT(I+26),
     +			      RH(MODEL,I-17)
	    END DO
	    WRITE (77,20) 100.0,PRESS27(27),TEMP(MODEL),RH(MODEL,10)
	END IF
	CLOSE(77)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ncep_profile.pdf
Process help=*
PARM INP      (STRING,40) COUNT=2
PARM TABLE    (STRING,40) COUNT=(0:1) DEFAULT=--
PARM TIME     REAL      VALID=(0.0:24.0)
PARM LAT      REAL      VALID=(-90:90)
PARM LONG     REAL      VALID=(-180:180)
PARM ELEV     REAL      COUNT=(0,1) DEFAULT=--
PARM DATE     (STRING,30) DEFAULT=" "
PARM MODEL    KEYWORD   VALID=("MLS","MLW","SAS","SAW","TROP","USS") DEFAULT=USS
PARM PRINTALL KEYWORD   VALID=(PRINTALL,NOPRINT)     DEFAULT=NOPRINT       
End-proc
 
.TITLE
VICAR Program NCEP_PROFILE
.HELP
PURPOSE:
 
      NCEP_PROFILE is a VICAR program to perform an 8 point weighted
interpolation (in both space and time) of NCEP GDAS global assimilation 
model data.
 
.LEVEL1
.VARI INP
Input VICAR labelled NCEP global
data files.
(1) Before desired time
(2) After desired time
.VARI TABLE
profile in radiosonde tablular
(rstab) format, suitable for
MODIN, TIMSCAL2, etc
.VARI TIME
Desired time, measured as GMT
in hours and fraction of hours.
.VARI LAT
Latitude (North positive)
.VARI LONG
Longitude (East positive)
.VARI DATE
Date to be added to table header
.VARI ELEV
Desired surface elevation,
in meters
.VARI MODEL
MODTRAN model to span to top
of atmosphere. Used only if a
file is output.
Valid: MLS,MLW,SAS,SAW,TROP,USS
.VARI PRINTALL
Print all 8 source profiles?
.LEVEL2
.VARI INP
      The variable INP specifies the names of the two NCEP global data files
to be used in the interpolation.  The first file should be for the snapshot
immediately preceeding the desired time, and the second file should be for
the snapshot immediately after the desired time.  The input files should be
VICAR labelled, and in the JPL abridged format.  This format is halfword data
in BSQ format, 181 lines by 360 samples.  The number of channels varies with
the date of assimilation.  The upper left corner is 90 degrees North, 0 
degrees East/West.  The attributes assigned to each channel are described on
the following pages.
.PAGE
Prior to October 1, 2000:

Channel  Attribute                      Units        Scaling Factor
 1-16    Geopotential height            meters         1.0
17-32    Temperature                    deg C        100.0
33-39    Relative Humidity              percent        1.0
   40    Surface Pressure               millibars     10.0
   41    Geopotential height at surface meters         1.0
   42    Pressure reduced to MSL        millibars     10.0
   43    Pressure at tropopause         millibars     10.0
   44    Temperature at tropopause      deg C        100.0
   45    Precipitable water in column   mm            10.0
46-61    Zonal Wind                     m/sec        100.0 (after 3/31/99)
62-77    Meridional Wind                m/sec        100.0 (after 3/31/99)
.PAGE
October 1, 2000 and later:

Channel Attribute                                             Units   Factor
------- ----------------------------------------------------  ------  ------
1-26    geopotential height at 26 isobaric levels 1000-10 mb   m         1.0
27-52   temperature at 26 isobaric levels 1000-10 mb           deg C   100.0
53-78   zonal wind at 26 isobaric levels 1000-10 mb            m/s     100.0
79-104  meridional wind at 26 isobaric levels 1000-10 mb       m/s     100.0
105-125 relative humidity at 21 isobaric levels 1000-100 mb    %         1.0
126     surface pressure                                       mb       10.0
127     surface geopotential height                            m         1.0
128     surface temperature                                    deg C   100.0
129     temperature at 2 meters above surface                  deg C   100.0
130     relative humidity at 2 meters above surface            %         1.0
131     pressure at tropopause                                 mb       10.0
132     geopotential height at tropopause                      m         1.0
133     temperature at tropopause                              deg C   100.0
134     precipitable water                                     cm     1000.0
135     pressure reduced to MSL                                mb       10.0
136     Land / Sea Mask                                        none      0/1
137     Ice Concentration                                      none      0/1
138     Cloud Cover                                            %         1.0
139     Snow Depth                                             kg/m^2    1.0
140     geopotential height anomaly at 500 meters              m         1.0
141     geopotential height anomaly at 1000 meters             m         1.0
.VARI TABLE
      If the parameter TABLE is used, a table in the format appropriate for
use as input for or TIMSCAL2 (parameter RSTABLE in TIMSCAL2) is generated,
and stored with this file name.
.VARI TIME
      The variable TIME specifies the time (in hours and fraction of hours
past midnight) for the desired interpolation.  The time should be expressed
as GMT/UT, not as local time.
.VARI LAT
      The variable LAT specifies the latitude (in degrees and fraction of
degrees North) for the desired interpolation. The range of valid values is
from -90.0 (South Pole) to +90.0 (North Pole).
.VARI LONG
      The variable LONG specifies the longitude (in degrees and fraction of
degrees East longitude) for the desired interpolation.  Note that all 
locations in the Western Hemisphere have negative values.  The valid range
of values is +/- 180.0
.VARI DATE
This optional parameter allows the user to supply a date to be placed in the
output profile's header text.  It is not used in the computation of the profile,
and it is the responsibility of the user to assure that the date given is
consistent with the input files.
.VARI ELEV
This optional parameter allows the user to specify the desired surface 
elevation, in meters.  This elevation will be used to compute the first
layer in the interpolated profile. If ELEV is defaulted, then the elevation
of the first layer will be the computed interpolated elevation that 
corresponds to the 1000 millibar level.
.VARI MODEL
      If a tablar output file is generated, relative humidities for
the upper layers and a top of atmosphere layer must be supplied from another
source.  This parameter allows the user to specify any of the 6 MODTRAN model
atmospheres to complete the profiles. The meanings of the valid keywords are
as follows:

           MLS  - Midlatitude Summer
           MLW  - Midlatitude Winter
           SAS  - Subarctic Summer
           SAW  - Subarctic Winter
           TROP - Tropical
           USS  - 1976 United States Standard Atmosphere
.VARI PRINTALL
      If this keyword is invoked, the 8 original profiles that are used to
compose the interpolated profile are also printed.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create ncep_profile.imake
#define  PROGRAM   ncep_profile

#define MODULE_LIST ncep_profile.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
