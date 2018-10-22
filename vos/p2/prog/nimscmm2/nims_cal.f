C*******************************************************************
      SUBROUTINE NIMS_GET_CAL(CALFILE,IR)

C_TITLE  NIMS_GET_CAL Read calibration file into memory

C_VARS
      INCLUDE 'calfile.fin'	! NIMS Calibration file structure
C					   in COMMON /CALFILE/
C_ARGS

      CHARACTER*64 CALFILE		!I  Calibration file name
      INTEGER*4    IR			!O  Return code: 0 OK, -1 parm error

C_DESC  Read calibration file into memory.

C_LIMS  Temporary version reads temporary unlabelled calibration file
C	with known fixed length.  (To be replaced by length read from label
C	of ISIS table file, and dynamic memory allocation.)

C_CALLS U_ERROR, U_FILL1

C_KEYS  NIMS, RAD, CAL

C_HIST	05sep90  Bob Mehlman UCLA/IGPP  Temporary E1 version -- Fortran I/O
C	07dec93  RMX  Renamed from CALGET
C	08dec93  RMX  Added file I/O error code and failover to old CAL file.
C	09dec93  RMX  Zero out buffer extension if old-style cal file.
c       13aug96  lwk  used getlun for unit #
c       11nov96  lwk  removed hard-coded unit 5/6 for screen i/o
c	10aug02  lwk  removed READONLY param from all OPEN statements for Linux

C_END

      REAL*4 CALBUF(LENCAL)			! TEMP fixed file length
      EQUIVALENCE (CALBUF,CF_THBOUND)
C      DATA IIN,IOUT/5,6/

C  Open calibration file, read into COMMON
      call getlun(ical)
      OPEN (ICAL,FILE=CALFILE,STATUS='OLD',FORM='UNFORMATTED',
     1      IOSTAT=IROPEN)
      IF (IROPEN.NE.0) CALL U_ERROR('GETCAL-OPENERR',	 ! Open error
     1 'Error opening calibration file',-101,2,IR)	 ! Give up

      LROW=LENCAL
      READ (ICAL,IOSTAT=IRREAD1) (CALBUF(i),i=1,lencal)
      IF (IRREAD1.NE.0) THEN
	LROW=3302
	REWIND ICAL			! Assume old-style (short) cal file
	READ (ICAL,IOSTAT=IRREAD2) (CALBUF(I),I=1,LROW)
	IF (IRREAD2.NE.0) CALL U_ERROR('GETCAL-READERR', ! Error on old file too
     1   'Error reading calibration file',-102,2,IR)	 ! Give up
	CF_TFPA0 = 124.0
	CF_TFPA = 124.0
	CF_TGRAT = 124.0
	CALL U_FILL1(0,4*(LENCAL-LROW),CALBUF(LROW+1))	 ! Clear extended buffer
c	WRITE (IOUT,921)
c 921	FORMAT (' Found old-style (short) calibration file. Proceeding.')
      ENDIF

c      WRITE (IOUT,922) LROW
c 922  FORMAT (' CAL file read, length',I6)
      CLOSE (ICAL)
      IR=0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_GET_CAL_A(CALFILE,IR)

C_TITLE  NIMS_GET_CAL_A Read ascii calibration file into memory

C_VARS
      INCLUDE 'calfile.fin'	! NIMS Calibration file structure
C					   in COMMON /CALFILE/
C_ARGS

      CHARACTER*64 CALFILE		!I  Calibration file name
      INTEGER*4    IR			!O  Return code: 0 OK, -1 parm error

C_DESC  Read calibration file into memory.

C_LIMS  ASCII cal file version
C	Error checks only for file OPEN and READ of first line

C_CALLS U_ERROR, U_FILL1

C_KEYS  NIMS, RAD, CAL

C_HIST	05sep90  Bob Mehlman UCLA/IGPP  Temporary E1 version -- Fortran I/O
C	07dec93  RMX  Renamed from CALGET
C	08dec93  RMX  Added file I/O error code and failover to old CAL file.
C	09dec93  RMX  Zero out buffer extension if old-style cal file.
C	18jul94  RMX  Changed NIMS$INC to ISIS$INC.
C	31jul02  RMX  ASCII cal file version
C	08aug02  lwk  added cf_thrump/cf_gndep, initialized calbuf to zero,
c		commented out override of CF_TFPA0

C_END

      PARAMETER (NDET=17)     ! # NIMS detectors
      PARAMETER (NDETX=20)    ! # NIMS detectors, counting thermal dets twice
      PARAMETER (NPHGP=31)    ! # physical grating positions

      REAL*4 CALBUF(LENCAL)			! TEMP fixed file length
      EQUIVALENCE (CALBUF,CF_THBOUND)
      REAL*4 THOFF(3)
      DATA ICAL,IIN,IOUT /4,5,6/

c  initialize the whole buffer:
	do i=1,lencal
	  calbuf(i) = 0.0
	enddo

C  Open ASCII calibration file
      OPEN (ICAL,FILE=CALFILE,STATUS='OLD',FORM='FORMATTED',
     1      IOSTAT=IROPEN)
      IF (IROPEN.NE.0) CALL U_ERROR('GETCAL-OPENERR',	! Open error
     1 'Error opening calibration file',-101,2,IR)	! Give up

C  Read header items
      READ (ICAL,920,IOSTAT=IRREAD) CF_IGS,CF_ICHM,CF_IPGPF,CF_IPGPL !int items
 920  FORMAT (4I5)
      IF (IRREAD.NE.0) CALL U_ERROR('GETCAL-READERR',  ! Read error
     1   'Error reading calibration file',-102,2,IR)	! Give up
      READ (ICAL,921) 					! spare line
 921  FORMAT (52X)
      READ (ICAL,922) CF_PSHIFT, CF_AINFL,		! real items
     1                CF_TFPA0, CF_TFPA, CF_TGRAT
 922  FORMAT (5F10.3)
      READ (ICAL,921)					! spare line

c      CF_TFPA0 = 124.0					! phase 2 overrides
c  CF_TFPA0 was the FPA temperature at which the ground calibration data
c  were taken, so should not be overriden unless special case (old-style
c  pre-temperature-correction cal file?)
      CF_TFPA = 124.0
      CF_TGRAT = 124.0

C  Read thermal detector boundaries, offsets and ratios
      DO I=1,3
        READ (ICAL,925) CF_THBOUND(I), THOFF(I), CF_THRAT(I)
 925    FORMAT (3F10.3)
	DO J=1,31					! fill phys gr positions
	  CF_THOFF(I,J) = THOFF(I)
	ENDDO
      ENDDO

C  Read chopper mode factors by detector: break points, lower & upper ratios
      DO IDET=1,NDET
        READ (ICAL,928) JDET, CF_CHBP(IDET), (CF_CHRAT(IDET,I),I=1,2)
 928    FORMAT (I5,3F10.3)
      ENDDO

C  Read corrected ground cal wavelengths, sensitivities, temperature gradients
        DO IDETX=1,NDETX
          DO IPHGP=1,NPHGP
            READ (ICAL,930) JDET, JPHGP, CF_WAVE(IDETX,IPHGP),
     1        CF_SENS(IDETX,IPHGP), CF_TGRAD(IDETX,IPHGP,1)
 930        FORMAT (2I5,3E14.7)
          ENDDO
        ENDDO

C  Read band-dependent scaling base & multiplier pairs (for ISIS tube gen only)
      DO IDET=1,NDET
        DO IPHGP=1,NPHGP
          READ (ICAL,935) JDET, JPHGP, CF_VBASE(IDET,IPHGP),
     1                                 CF_VMULT(IDET,IPHGP)
 935      FORMAT (2I5,F12.2,F12.6)
        ENDDO
      ENDDO

c  make up something for CF_THRUMP & CF_GNDEP arrays:
	do i=1,17
	  do j=1,40
	    cf_thrump(j,i) = 1.0
	  enddo
	enddo
	do i=1,4
	  do j=1,14
	    cf_gndep(j,i) = 1.0
	  enddo
	enddo

      WRITE (IOUT,950)
 950  FORMAT (' ASCII Cal file read into COMMON /CALFILE/')
      CLOSE (ICAL)
      IR=0
      RETURN
      END


C*******************************************************************
      SUBROUTINE NIMS_CHECK_DARK(IDTAB,DTAB,NTROWS,UPTYPE,KRIM,KMF,
     1 DAVE,IR)

C_TITLE  NIMS_CHECK_DARK  Check for dark value changes, update if necessary

C_VARS
      INCLUDE 'darkwork.fin'	! Working dark values, etc. in
C					   COMMON /DARKWORK/ including DN0 array

C_ARGS					!  Dark file record parameters
      PARAMETER (NDHEAD=32)		!   Row header contains RIM,MF,MODE,GAIN
      PARAMETER (NDARK=NDETD*NMPD)
      PARAMETER (NDROW=NDHEAD+NDARK)

      INTEGER*4 IDTAB(NDROW,NTROWS)	!I  Integer form of dark table
      REAL*4    DTAB(NDROW,NTROWS)	!I  Real form of dark table --
C					     must be same array as IDTAB
      INTEGER*4 NTROWS			!I  # rows in dark table
      INTEGER*4 UPTYPE			!I  Dark value update type:  0 none
C					     1 nearest  2 latest  3 interpolate
      INTEGER*4 KRIM			!I  Current spacecraft clock RIM
      INTEGER*4 KMF			!I  Current s/c clock minor frame (0-90)
      REAL*4    DAVE(NDETD)		!O  Average dark values, per detector
      INTEGER*4 IR			!O  Return code: 0: OK,
C					     1: more than one table row

C_DESC  Check for dark value changes, updating working dark value set from
C	multi-row dark file data by method requested:  nearest value, latest
C	value, or interpolation.  This routine should be called for each
C	RIM of data.

C	Dark file must have been previously read into array IDTAB/DTAB
C	(possibly allocated dynamically) in its entirety, using DRKGET
C	routine or other method.

C_LIMS  This version is restricted to dark tables with one row.

C_CALL  U_MOVE1

C_KEYS  NIMS, QUBE, RAD, CAL, DARK

C_HIST	14aug90  Bob Mehlman UCLA/IGPP  Interface specs
C	17aug90  RMX  PRELIMINARY VERSION: assumes 1-row dark table
C	07sep90  RMX  Table changed from grpos-based to mp-in-frame-based
C	07dec93  RMX  Renamed from DRKCHK
C	22apr94  RMX  Return array of average dark values, per dectector
c       19feb00  lwk  save DAVE to common for changes to nims_comp_rad

C_END

      real*4    davesave(ndetd)		!O  Average dark values, per detector
      common/dsave/ davesave

      LOGICAL LFIRST /.TRUE./

      IF (LFIRST) THEN
	LFIRST=.FALSE.
	NDROWS=NTROWS					! Set up header
	IDR1=1
	IDR2=1
	IDRIM1=IDTAB(1,1)
	IDRIM2=IDRIM1
	IDMF1=IDTAB(2,1)
	IDMF2=IDMF1
	KDRIM=KRIM
	KDMF=KMF
	CALL U_MOVE1(4*NDARK,IDTAB(NDHEAD+1,1),DN0)	! Move in dark table
	DO IDET=1,NDETD					! Compute averages/det
	  DAVE(IDET)=0.0
	  DO IMP=1,NMPD
	    DAVE(IDET)=DAVE(IDET)+DN0(IDET,IMP)
	  ENDDO
	  DAVE(IDET)=DAVE(IDET)/NMPD
	  davesave(idet) = dave(idet)
	ENDDO
	IF (NDROWS.NE.1) THEN
c	  TYPE 9							! TEMP
c 9	  FORMAT (' Multi-row dark table encountered, only 1st used')	! TEMP
	  IR=1
	  RETURN
	ENDIF
      ENDIF

      IR=0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_GET_DARK(DARKFILE,LROW,NROWS,DARKBUF,IR)

C_TITLE  NIMS_GET_DARK  Read dark table file into memory

      PARAMETER (LROWTEMP=712)		! TEMP dark table row length (longwords)
C_ARGS
      CHARACTER*64 DARKFILE		!I  Dark file name
      INTEGER*4    LROW			!O  Length of row of dark table (bytes)
      INTEGER*4    NROWS		!O  Number of rows in dark table
      REAL*4       DARKBUF(LROWTEMP)	!O  Buffer for darktable (TEMP 1 row)
      INTEGER*4    IR			!O  Return code: 0 OK, -1 parm error

C_DESC  Read dark table file into memory.

C_LIMS  Temporary version reads temporary unlabelled single row dark value file

C_CALLS U_ERROR

C_KEYS  NIMS, RAD, DARK

C_HIST	05sep90  Bob Mehlman UCLA/IGPP  Temporary E1 version -- Fortran I/O
C	07dec93  RMX  Renamed from DRKGET
C	08dec93  RMX  Added file I/O error code
c       13aug96  lwk  used getlun for unit #
c       11nov96  lwk  removed hard-coded unit 5/6 for screen i/o

C_END

c      DATA IIN,IOUT/5,6/

      NROWS=1						! TEMP
      LROW=LROWTEMP					! TEMP

      call getlun(idark)
      OPEN (IDARK,FILE=DARKFILE,STATUS='OLD',FORM='UNFORMATTED',
     1 IOSTAT=IROPEN)
      IF (IROPEN.NE.0) CALL U_ERROR('GETDARK-OPENERR',	! Open error
     1   'Error opening dark file',-201,2,IR)		! Give up

      READ (IDARK,IOSTAT=IRREAD) (DARKBUF(i),i=1,LROWTEMP)
      IF (IRREAD.NE.0) CALL U_ERROR('GETDARK-READERR',	! Read error
     1 'Error reading dark file',-202,2,IR)		! Give up

c      WRITE (IOUT,950) NROWS,LROW
c 950  FORMAT (' DARK file opened, rows',I3,'  length',I5)
      CLOSE (IDARK)
      IR=0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_GET_DARK_A(DARKFILE,LROW,NROWS,DARKBUF,IR)

C_TITLE  NIMS_GET_DARK_A  Read ascii dark table file into memory

      PARAMETER (LROWTEMP=712)		! TEMP dark table row length (longwords)
C_ARGS
      CHARACTER*64 DARKFILE		!I  Dark file name
      INTEGER*4    LROW			!O  Length of row of dark table (bytes)
      INTEGER*4    NROWS		!O  Number of rows in dark table
      REAL*4       DARKBUF(LROWTEMP)	!O  Buffer for darktable (TEMP 1 row)
      INTEGER*4    IR			!O  Return code: 0 OK, -1 parm error

C_DESC  Read dark table file into memory.

C_LIMS  Temporary version reads temporary unlabelled single row dark value file
C	ASCII version does the same; guess it's not temporary.

C_CALLS U_ERROR, U_MOVE1

C_KEYS  NIMS, RAD, DARK

C_HIST	05sep90  Bob Mehlman UCLA/IGPP  Temporary E1 version -- Fortran I/O
C	07dec93  RMX  Renamed from DRKGET
C	08dec93  RMX  Added file I/O error code
C	24jul02  RMX  ASCII dark file version
C	01aug02  RMX  Shorter record length (122) dropping spare from header
C	03aug02  RMX  Corrected error in U_MOVE1 call
C_END

      INTEGER*4 IHEAD(32) /32*0/
      PARAMETER (NDET=17)
      PARAMETER (NMPF=40)
      REAL*4 DTAB(NDET,NMPF)
      DATA IDARK,IIN,IOUT /7,5,6/

      NROWS=1						! TEMP
      LROW=LROWTEMP					! TEMP

      OPEN (IDARK,FILE=DARKFILE,STATUS='OLD',FORM='FORMATTED',
     1 IOSTAT=IROPEN)
      IF (IROPEN.NE.0) CALL U_ERROR('GETDARK-OPENERR',	! Open error
     1   'Error opening dark file',-201,2,IR)		! Give up

      READ (IDARK,920,IOSTAT=IRREAD) (IHEAD(I),I=1,17)
 920  FORMAT (17I7)
      IF (IRREAD.NE.0) CALL U_ERROR('GETDARK-READERR',	! Read error
     1 'Error reading dark file',-202,2,IR)		! Give up

      DO J=1,NMPF
        READ (IDARK,921,IOSTAT=IRREAD) (DTAB(I,J),I=1,NDET)
 921    FORMAT (17F7.3)
        IF (IRREAD.NE.0) CALL U_ERROR('GETDARK-READERR', ! Read error
     1   'Error reading dark file',-202,2,IR)		 ! Give up
      ENDDO

C  Move IHEAD and DTAB into DARKBUF
      CALL U_MOVE1(4*32,IHEAD,DARKBUF)
      CALL U_MOVE1(4*NDET*NMPF,DTAB,DARKBUF(33))

      WRITE (IOUT,950) NROWS,LROW
 950  FORMAT (' DARK file opened, rows',I3,'  length',I5)
      CLOSE (IDARK)
      IR=0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_CHECK_RAD(EDRMSC,HRSHSK,HRSBKG,IR)

C_TITLE  NIMS_CHECK_RAD  Check for changes to working calibration data in RIM

C_VARS
      INCLUDE 'calwork.fin'	! NIMS working calibration data
C					   in COMMON /CALWORK/
      INCLUDE 'calfile.fin'	! NIMS Calibration file structure
C					   in COMMON /CALFILE/
C_ARGS
      BYTE EDRMSC(64,91)		!I  First 64 bytes of each EDR record
C					     in RIM, incl. time & error info,
C					     NIMS LRS, LRS engineering & AACS
      BYTE HRSHSK(6,10,91)		!I  NIMS HRS Hskpng, 6 bytes per RTI
C					     for entire RIM of observation
      INTEGER*2 HRSBKG(4,10,91)		!I  NIMS HRS Bkgrnd, 4 items per RTI
C					     for entire RIM of observation
      INTEGER*4 IR			!O  Return code: 0 OK, others <tbd>

C_DESC  Before second and subsequent RIMs of observation, modify working
C	calibration tables if gain state or chopper mode have changed, or if
C	relevant instrument temperatures have changed too much.  Requires
C	housekeeping and engineering data for first RIM of observation,
C	from which all relevant temperatures, etc. will be extracted.

C_LIMS  Instrument mode is assumed constant -- all "observations" must be
C	single mode.

C_KEYS  NIMS, QUBE, RAD, CAL

C_HIST	25may90  Bob Mehlman UCLA/IGPP  Interface specs
C	20jun90  RMX  revised specs
C	01aug90  RMX  dummy version, does nothing
C	07dec93  RMX  Renamed from RADCHK, still does nothing.

C_END

      IR=0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_SET_RAD(IOP,INMODE,IGPOFF,IGPOS1,IGPINC,NGPOS,
     1 EDRMSC,HRSHSK,HRSBKG,OVTEMP,WAVEL,SENSV,RBASE,RMULT,pshift,
     2 ainfl, xnull, xlsat, xhsat, IR)

C_TITLE  NIMS_SET_RAD  Set up for radiance computations on observation

C_VARS
      INCLUDE 'calwork.fin'	! NIMS working calibration data
C					   in COMMON /CALWORK/
      INCLUDE 'calfile.fin'	! NIMS Calibration file structure
C					   in COMMON /CALFILE/
C_ARGS
      INTEGER*4 IOP			!I  Requested operation
C					     0:  radiance comp, band-dep scaling
C					     1:  wavelength computation only
C					     2:  radiance comp, uniform scaling
      INTEGER*4 INMODE			!I  NIMS instrument mode (0-15)
C					     Should be determined by automatic
C					     mode detection routine
C					     with possible input override.
C					     Negative value for STOP/SLIDE mode
C					     chooses STOP, positive for SLIDE.
C					     Map modes are 1 full, 3 long,
C					     5 short, 8 bandedge, 10 stop/slide.
C					     Add 1 for spectrometer modes.
C					     7 for fixed grating map.
      INTEGER*4 IGPOFF			!I  Grating position offset
C					     0 <= IGPOFF <= 7  (normally 4)
      INTEGER*4 IGPOS1			!I  Starting logical grating position  
      INTEGER*4 IGPINC			!I  Grating position increment
C						Defaults to standard mode value
      INTEGER*4 NGPOS			!I  Number of grating positions
C						Defaults to standard mode value
      BYTE EDRMSC(64,91)		!I  First 64 bytes of each EDR record in
C					     1st RIM, incl. time & error info,
C					     NIMS LRS, LRS engineering & AACS
      BYTE HRSHSK(6,10,91)		!I  NIMS HRS Hskpng, 6 bytes per RTI
C					     for first RIM of observation
      INTEGER*2 HRSBKG(4,10,91)		!I  NIMS HRS Bkgrnd, 4 items per RTI
C					     for entire RIM of observation
      REAL*4 OVTEMP(6)			!I  Override temperatures (if not zero)
C					     in order FPA, RadShield, Telescope,
C					     Grating, Chopper, Electronics.
      REAL*4 WAVEL(408)			!O  NIMS wavelengths, in microns,
C					     derived from grating temperature
C					     at beginning of observation.
C					     (Length is mode-dependent, <= 408)
C					     Ordered (ngp,17).  Some overlap
C					     is present between detectors.
      REAL*4 SENSV(408)			!O  NIMS sensitivities,
C					     in RWC units of DN/uW/cm^2/st/u.
C					     These may be used to convert
C					     radiance back to linearized,
C					     chopper-mode-corrected DN,
C					     but *not* to raw DN.
C					     (Length & order same as WAVEL.)
      REAL*4 RBASE(*)			!O  Base for 16-bit scaled radiance &
      REAL*4 RMULT(*)			!O  Multiplier for 16-bit scaled
C					     radiance -- to be used for
C					     conversion of REAL*4 radiance to
C					     INTEGER*2 for output to cube file.
C					    True value = base +
C						    (multiplier * stored value)
C					     # values <= 408 if IOP = 0
C					     # values  = 1 if IOP = 2
C					     Ignored if IOP = 1
      real*4 pshift			!I/O if -999. on input, then just 
c					     returned so that NIMSCMM can put it
c					     put it in the label;  otherwise,
c					     use input value
      real*4 ainfl			!I  grating step inflation parameter,
c					     see NIMS_WAVE;  currently this is
c					     input by user, in the future it 
c					     be "I/O" as pshift
c
      real*4 xnull, xlsat, xhsat	!I  ISIS special values
c
      INTEGER*4 IR			!O  Return code: 0 OK, -1 parm error
C					     1 some sensitivities zero (warning)

C_DESC  Compute working calibration tables by interpolation in calibration
C	file and application of FPA temperature gradient prior to radiance
C	computations for observation.  Also computes NIMS wavelengths and
C	band-dependent radiance scaling factors.

C	To be called at beginning of observation.  Requires engineering data
C	for first full RIM of observation, from which all relevant temperatures
C	will be extracted, unless override temperatures are passed via the
C	calibration file.  Other housekeeping data is passed in and may
C	eventually be used.

C	Routine can handle both constant Base and Multiplier (for all
C	wavelengths) for computing 16-bit radiance, OR band-dependent
C	Base and Multiplier, depending on IOP.

C_LIMS  Wavelengths determined from NIMS grating temperature at start of data,
C	unless override temperature is passed in calibration file.

C	Temperature gradient correction based on NIMS FPA temp at start of
C	data, unless override temperature is passed by argument or value
C	is unreasonable.  In latter case, calibration file default used.

C	Interpolation in sensitivity tables for only those physical
C	grating positions available; extrapolation beyond.  (Eventually
C	data may be available for all physical grating positions.)

C	Bandedge and Stop/Slide modes not yet supported.

C	Presently assumes gain- and mirror-position-dependence always 1.0.
C	Separate calibration files required for each gain and, for safety,
C	each chopper mode (of each gain).

C_CALL  NIMS_WAVE function (RWC, as modified by RMX), NIMS_ENGR

C_KEYS  NIMS, QUBE, RAD, CAL

C_HIST	25may90  Bob Mehlman UCLA/IGPP  Interface specs
C	20jun90  RMX  Revised specs
C	14aug90  RMX  Grating offset argument added
C	27aug90  RMX  Wavelength function added, interpolation in wavelength
C		       for sensitivities added.
C	10sep90  RMX  Extrapolation used outside range of physical grating
C		       positions for which sensitivity data is known.
C	14sep90  RMX  MIPL-delivery version
C	03apr91  RMX  IOP argument, wavelength-only and band-dependent
C		       scaling options added (gain state 2 only)
C	02feb93  RMX  Corrected wavelength computation to support start logical
C			grating positions other than 0.  Removed special fixed
C			map code for same.  Added arguments for start logical
C			grating position, grating position increment and number
C			of grating positions.  Allowed safe mode (fixed spect).
C			Name of RWC WAVEF subroutine changed to NIMS_WAVE.
C	03feb93  RMX  Corrected radiance scaling setup for non-long-map modes.
C	24may93  RMX  DON'T interpolate if either sensitivity is zero or
C			negative -- set result to zero.
C	03nov93  RMX  Added linear FPA temperature dependence of sensitivity,
C			obtaining ENGR temps from EDRMSC array, with default
C			in calibration file.
C	10nov93  RMX  Fixed bug (found by LWK) in above-table extrapolation.
C			Added warning return code if any zero sensitivities.
C	12nov93  RMX  Fixed bug affecting only cases of FPA temp gradient zero.
C	07dec93  RMX  Renamed from RADSET.
C		      Added OVTEMP argument for override temperatures.
C	24apr94  RMX  Added setup for 63-hertz chopper mode ratios and
C			breakpoints, removed old CHMDEP setup.  Gain state
C			and chopper mode now input from cal file.  Modified
C			form of temperature gradient correction to RWC's.
C			Added output sensitivity vector argument.
C	23sep97  lwk  don't compute SENSV when IDET > NDET
C	 5apr98  lwk  allow user specification of PSHIFT
C_END

      PARAMETER (NPGP=31)			! # physical grating positions
      REAL NIMS_WAVE				! Wavelength function 
C  Temperature extraction
      BYTE ENGR(2,91)				! Engineering bytes from EDR
      INTEGER TDN(7,6)				! Extracted TEMP DNs
      INTEGER ODN(4)				! Other ENGR DNs
      REAL TEMP(7,7)				! Computed temperatures
      REAL RESIST(7,4)				! Computed resistances
      REAL*8 SUMTEMP(6) /6*0.0D+00/		! Temperature sums
      INTEGER KTEMP(6) /6*0/			! Temperature counts
      REAL AVETEMP(6) /6*0.0/			! Average temperatures
C  Mode tables
      INTEGER*4 NGPA(16)    / 1,12,12,24,24,6,6,1,2,2,0,0,0,0,0,0/ ! # gr pos
      INTEGER*4 IGPDELA(16) / 0, 2, 2, 1, 1,4,4,0,0,0,0,0,0,0,0,0/ ! gr incr

C  Parameter checks
      SLIDE=.TRUE.
      IF (INMODE.LT.0) SLIDE=.FALSE.
      IMODE=IABS(INMODE)
      IF ((IMODE.LT.0.OR.IMODE.GT.15).OR.
     1    (IGPOFF.LT.0.OR.IGPOFF.GT.7).OR.
     2    (IGPOS1.LT.0.OR.IGPOS1.GT.31).OR.
     3    (IGPINC.LT.0.OR.IGPINC.GT.30).OR.
     4    (IOP.LT.0.OR.IOP.GT.2)) THEN
	IR=-1
	RETURN
      ENDIF
      IR = 0						! Default return code

      if (pshift.gt.-900.) then
	cf_pshift = pshift		! use user-specified value
      else
	pshift = cf_pshift		! return default value for label item
      endif

C  Set mode-dependent parameters, using defaults if necessary
      NGP=NGPOS
      IF (NGP.EQ.0) NGP=NGPA(IMODE+1)
      IGPDEL=IGPINC
      IF (IGPDEL.EQ.0) IGPDEL=IGPDELA(IMODE+1)
      IGPFIX=IGPOS1				! set start gr pos
C...modify for bandedge and stop/slide modes
C  Pass gain state and chopper mode
      IGS=CF_IGS				! For gain dependence, someday
      ICHM=CF_ICHM				! TEMP safety feature
      IF (OVTEMP(4).GT.0.0.AND.OVTEMP(1).GT.0.0) GO TO 20 ! Overrides provided
C  Extract NIMS engineering temperatures from EDRMSC data
      DO J=1,91
	CALL U_MOVE1(2,EDRMSC(39,J),ENGR(1,J))
      ENDDO
C  Compute NIMS temperatures, etc.
      CALL NIMS_ENGR(ENGR,TDN,ODN,TEMP,RESIST)
C  Compute average temperatures
        DO J=1,6                                        ! Sum temps if DN >0 but
          DO I=1,7                                      ! allow 0 FPA DN if real
            IF (TDN(I,J).GT.0.OR.(J.EQ.1.AND.TDN(I,2).GT.0)) THEN
              SUMTEMP(J) = SUMTEMP(J) + TEMP(I,J)
              KTEMP(J) = KTEMP(J) + 1
            ENDIF
          ENDDO
          IF (KTEMP(J).GT.0) AVETEMP(J) = SUMTEMP(J) / KTEMP(J)
        ENDDO
C  Find current FPA temperature, check for reasonableness, default if necessary
      TFPA=AVETEMP(1)
      IF (TFPA.LT.50.0.OR.TFPA.GT.200.0) TFPA=CF_TFPA	! Cal file default
C  Find current grating temperature, check, default if necessary
C      TGR = 124.0					! Old default
      TGR=AVETEMP(4)
      IF (TGR.LT.100.0.OR.TGR.GT.200.0) TGR=CF_TGRAT	! Cal file default
C  Override temperatures
 20   IF (OVTEMP(1).GT.0.0) TFPA=OVTEMP(1)		! Override FPA temp
      IF (OVTEMP(4).GT.0.0) TGR=OVTEMP(4)		! Override grating temp

C  Compute working wavelength set for COMMON, and more-or-less
C   ordered wavelength vector for caller to put in cube label
      IPHGP1=IGPOFF+IGPOS1				! Start physical gr pos
      DO IDET=1,NDET
	DO IGP=1,NGP
	  GP=IPHGP1+(IGP-1)*IGPDEL		! Physical grating position
	  WAVE(IDET,IGP)=NIMS_WAVE(IDET,GP,CF_PSHIFT,TGR,AINFL)	! Wavelength
	  K=(IDET-1)*NGP+IGP
	  WAVEL(K)=WAVE(IDET,IGP)
	ENDDO
      ENDDO
C  Quit if only want wavelengths
      IF (IOP.EQ.1) GO TO 80

C  Use wavelengths to interpolate for sensitivities(det,lgp)
      DELTAT=TFPA-CF_TFPA0	! FPA temperature difference from base
      DO IDET=1,NDET+NDTH	! Loop on detectors (18-20: thermal upper range)
	LDET=IDET			! Wavelength index same
	IF (IDET.GT.NDET) LDET=IDET-3	!  except for thermal upper range
	DO IGP=1,NGP		!  Loop on logical grating positions
	  WAVEX=WAVE(LDET,IGP)  ! Wavelength
	  DO KPGP=CF_IPGPF,CF_IPGPL ! Search for bracketing wavelengths
	    IF (WAVEX.LT.CF_WAVE(IDET,KPGP)) GO TO 30
	  ENDDO
	  KPGP=CF_IPGPL+1	! Off table end, set index 1 past
C   Compute working sensitivity -- temporarily only values at logical grating
C    positions are available; eventually interpolate among values for all
C    physical grating positions, and extrapolate at both ends.
 30	  IF (KPGP.EQ.CF_IPGPF) THEN		! Below table: extrapolate
	    K1=KPGP
	    K2=KPGP+1
	  ELSE IF (KPGP.EQ.CF_IPGPL+1) THEN	! Above table: extrapolate
	    K1=KPGP-2
	    K2=KPGP-1
	  ELSE				! Within table: do linear interpolation
	    K1=KPGP-1
	    K2=KPGP
	  ENDIF
	  ALPHA1=CF_TGRAD(IDET,K1,1)	!  Temperature gradient coeffs
	  ALPHA2=CF_TGRAD(IDET,K2,1)	!   (2nd coeff not currently used)
	  WAVE1 = CF_WAVE(IDET,K1) 
	  WAVE2 = CF_WAVE(IDET,K2)
	  F2 = (WAVEX - WAVE1) / (WAVE2 - WAVE1)
	  F1 = 1.0 - F2
	  SENS1 = CF_SENS(IDET,K1) * (1.0 + ALPHA1*DELTAT)	! Apply temp-
	  SENS2 = CF_SENS(IDET,K2) * (1.0 + ALPHA2*DELTAT)	!  dependence
	  IF (SENS1.EQ.0.0.OR.SENS2.EQ.0.0) THEN ! Check for bad sensitivities
	    F1 = 0.0				 ! Set for zero interpolation
	    F2 = 0.0
	    RET = 1				 ! Set warning return code
	  ENDIF
	  SENS(IDET,IGP) = F1 * SENS1
     1			 + F2 * SENS2
	  IF (IDET.LE.NDET) THEN
	    K=(IDET-1)*NGP+IGP
	    SENSV(K)=SENS(IDET,IGP)		 ! Sensitivity vector for label
	  ENDIF
	ENDDO
      ENDDO

C  Copy thermal offset data for logical grating positions
      DO I=1,NDTH
	DO IGP=1,NGP
	  THOFF(I,IGP)=CF_THOFF(I,CF_IPGPF-1+IGP)
	ENDDO
      ENDDO
C  Copy thermal range boundary points
      DO I=1,NDTH
	THBOUND(I)=CF_THBOUND(I)
      ENDDO
C  Copy mirror position throughput factors
      NMPF=2*NMP
      DO IDET=1,NDET
	DO IMPF=1,NMPF
	  THRUMP(IMPF,IDET)=CF_THRUMP(IMPF,IDET)
	ENDDO
      ENDDO
C  Copy gain factors
      DO IDET=1,NDNTH
	DO I=1,4
	  GNDEP(IDET,I)=CF_GNDEP(IDET,I)
	ENDDO
      ENDDO
C  Copy 63-hertz chopper mode breakpoints and ratios
      DO IDET=1,NDET
	CHBP(IDET)=CF_CHBP(IDET)
	DO I=1,2
	  CHRAT(IDET,I)=CF_CHRAT(IDET,I)
	ENDDO
	IF (IDET.GE.15)					   ! Linearize thermal
     1   CHRAT(IDET,2) = CHRAT(IDET,2) * CF_THRAT(IDET-14) !  high range factor
      ENDDO

C  ISIS special values are passed in from calling program (NIMSCMM2), in order
C  to support platform-dependent sets:
	radnull = xnull
	radlisat = xlsat
	radhisat = xhsat
c  (radnsens is disabled)

C  OLD CODE:
C  Set special values for REAL*4 radiance array (for possible use by caller)
c      RADNULL = 'FFFFFFFF'X				! ISIS REAL*4 null
c      RADLISAT = 'FFFDFFFF'X				! 	  low instr sat
c      RADHISAT = 'FFFCFFFF'X				! 	 high instr sat
c      RADNSENS = 'FFF1FFFF'X				!  rad if no sens avail
c (NEVER USED:)
C      RADBDARK = 'FFF0FFFF'X				!  rad if DN below dark

C  Return scaling parameters
      IF (IOP.EQ.2) THEN				! Uniform scaling:
	RBASE(1) = CF_UBASE				!  move info from
     	RMULT(1) = CF_UMULT				!  cal file to caller
	RADBASE = CF_UBASE				!  & to COMMON /CALWORK/
	RADMULT = CF_UMULT
      ELSE						! Variable scaling:
	RADMULT = 0.					!  flag in /CALWORK/
	K = 0
	DO IDET=1,NDET
	  KGP = IGPOFF + IGPOS1 + 1
	  DO IGP=1,NGP					! Move info to
	    VARBASE(IDET,IGP) = CF_VBASE(IDET,KGP)	!  COMMON /CALWORK/
	    VARMULT(IDET,IGP) = CF_VMULT(IDET,KGP)
	    K = K + 1
	    RBASE(K) = VARBASE(IDET,IGP)		!  and to caller
	    RMULT(K) = VARMULT(IDET,IGP)
	    KGP = KGP + IGPDEL
	  ENDDO
	ENDDO
      ENDIF

 80   RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_COMP_RAD(IGP,INCR,IDN,RAD,IR)

C_TITLE  NIMS_COMP_RAD  Compute radiances from NIMS raw data numbers for 1 nit

C_VARS
      INCLUDE 'calwork.fin'	! NIMS Working calibration data
C					   in COMMON /CALWORK/
      INCLUDE 'darkwork.fin'	! Working dark values
C					   in COMMON /DARKWORK/
C_ARGS
      INTEGER*4 IGP		    !I  Logical grating position index
C					 (lgp + 1)  (1-24)
      INTEGER*4 INCR		    !I  Increment between DNs to be processed
C					 in IDN and RAD arrays (may be 1)
      INTEGER*2 IDN(INCR,NMP,NDET)  !I  NIMS raw data numbers (may optionally
C					 include ISIS special values)
      REAL*4    RAD(INCR,NMP,NDET)  !O  NIMS radiances (may overlay IDN)
      INTEGER*4 IR		    !O  Return code:  0 OK  others <tbd>

C_DESC  Compute radiances from NIMS raw data numbers for 17 detectors at 20
C	mirror positions, one NIT's worth of data (collected in 1/3 second),
C	given current best dark values and calibration data.  Special
C	DN conditions recognized:  high & low instrument saturation,
C	DN less than dark value, invalid DN, ISIS INT*2 special values
C	(though absence of latter should cause no problems).

C	If data from 63-hertz chopper mode, bilinear DN-dark ratios between
C	63-hertz and reference chopper modes are applied depending on whether
C	particular DNs are above or below breakpoint values for particular
C	detectors.  (Except that thermal detector ratios are *simply* linear
C	in the low and high ranges.)  This procedure is followed for chopper
C	reference mode too, for simplicity, but the ratios in the calibration
C	file are all 1.0s.  Separate calibration files will continue to be
C	maintained for the two chopper modes.

C  <Radiance algorithm (as in RADGEN.TXT) to be included here.>

C_LIMS  Data numbers assumed 16-bit signed integers in range 0 to 1023.

C_KEYS  NIMS, QUBE, RAD, CAL, DARK

C_HIST  25may90  Bob Mehlman  UCLA/IGPP  Interface specs
C	20jun90  RMX  Revised specs
C	19jul90  RMX  ORIGINAL VERSION, no mirror position dependence yet,
C		       nor chopper mode nor gain dependence either
C	28aug90  RMX  Mirpos throughput, gain & chopper mode dependence added.
C	14sep90  RMX  MIPL delivery version, with special value detection.
C	03apr91  RMX  Special value for negative radiance dropped.
C	08jun91  RMX  Corrected index for mirror-position-throughput factor
C			by adding IMPOFF to IMP.
C	10nov93  RMX  Output special pixel value if sensitivity unknown.
C	07dec93  RMX  Renamed from RADVAL.
C	24apr94  RMX  Added code for 63-hertz chopper mode data: applied per-
C			detector bilinear 63-hertz-to-reference (DN - dark)
C			ratios depending on whether DN is below or above
C			per-detector breakpoint, except that thermal detector
C			ratios are *simply* linear in the low and high ranges.
C			High range thermal is linearized beforehand.
C			Removed old chopper mode dependence factor.
C			Changed 0 DNs from INSTR_LOW_SAT to NULL special value.
C	13nov96  lwk  put check for INSTR_LOW_SAT before that for negative DNs
C	 8feb00  lwk  added IGP=-1 flag for array without MP
C	 9jun01  lwk  fixed bug (which only appears on speedy!) when idet<idth1
C_END

C  DN special values (see CALWORK.FIN for radiance special values)
      INTEGER*2 NHISAT /1023/				! NIMS high sat DN
      INTEGER*2 INULL /-32768/				! ISIS INT*2 null
      INTEGER*2 ILOSAT /-32766/				!	  low instr sat
      INTEGER*2 IHISAT /-32765/				!	 high instr sat

      real*4    dave(ndetd)		!O  Average dark values, per detector
      common/dsave/ dave

      igpp = igp
      if (igp.lt.0) then
	impoff=0
	igpp = 1
      elseIF (MOD(IGP,2).EQ.0) THEN			! Compute mirpos offset
	IMPOFF=NMP					!  into frame for
      ELSE						!  dark value lookup
	IMPOFF=0
      ENDIF

      DO IDET=1,NDET
	CHBPI=CHBP(IDET)				! 63-hertz DN-dark
	CHRAT1=CHRAT(IDET,1)				!  breakpoint and
	CHRAT2=CHRAT(IDET,2)				!  bilinear ratios
	CHHRBASE=CHBPI*(CHRAT1-CHRAT2)			!  high range base
	IDTH=IDET-IDTH1+1				! For thermal rng check
	DO IMP=1,NMP
	  if (igp.lt.0 .and. imp.gt.1) go to 100
          KDN = IDN(1,IMP,IDET)				! Input INT*2 DN
	  IF (KDN.EQ.NHISAT.OR.KDN.EQ.IHISAT) THEN	! High instrument
	    RADX = RADHISAT				!  saturation
	  ELSE IF (KDN.EQ.ILOSAT) THEN			! Low instrument
	    RADX = RADLISAT				!  saturation (unlikely)
	  ELSE IF (KDN.LE.0.OR.KDN.GT.NHISAT) THEN	! Missing or illegal
	    RADX = RADNULL				!  DN, declare invalid
	  ELSE
	    DN = KDN					! Valid DN
	    !IF (IDET.LT.IDTH1.OR.DN.LT.THBOUND(IDTH)) THEN ! Nonthermal dets
	    ! above gives error on speedy when idet<idth1, replace with:
            dnlim = 0
            if (idet.ge.idth1) dnlim = thbound(idth)
            if (idet.lt.idth1.or.dn.lt.dnlim) then
	      if (igp.lt.0) then
	        dnx = dn - dave(idet)
	      else
	        DNX = DN - DN0(IDET,IMP+IMPOFF)	! & thermal lower rng: DN - dark
	      endif
	      IF (DNX.LE.CHBPI) THEN			! Apply 63hz mode ratios
	        DNX = DNX * CHRAT1			!  low range
	      ELSE
	        DNX = DNX * CHRAT2 + CHHRBASE		!  high range 
	      ENDIF					! Ref mode equivalent
	    ELSE				! Thermal dets, upper range:
	      DNX = DN - THOFF(IDTH,IGPp)		!  DN - offset
	      DNX = DNX * CHRAT2			! Apply 63hz mode ratio
	    ENDIF
	    SENSIT = SENS(IDET,IGPp)			! Chopper ref mode sens
	    IF (SENSIT.GT.1.0E-6) THEN
	      SENSX = 1.0/SENSIT			! Sensitivity factor
	    ELSE
	      SENSX = 0.0				! Missing (?) sens
	    ENDIF
	    IF (IDET.LT.IDTH1)				! Non-thermal
     1	      SENSX = SENSX * GNDEP(IDET,IGS)  		!  gain dependence
	    SENSX = SENSX * THRUMP(IMP+IMPOFF,IDET)	! Mirror pos thruput
            RADX = SENSX * DNX				! Radiance
	    IF (RADX.EQ.0.0) RADX = RADNSENS		! Sens was missing
	  ENDIF						!  DN < dark value?  NO!
          RAD(1,IMP,IDET) = RADX			! Output REAL*4 radiance
	ENDDO
100	continue
      ENDDO

      IR=0
      RETURN
      END

C*******************************************************************
      REAL FUNCTION NIMS_WAVE(IDET, P, DELSTP, TGR, AINFL)

C_TITLE NIMS_WAVE  Wavelength of NIMS detector at given grating position

C_ARGS
      INTEGER IDET    	!I  Detector number (1-17)
      REAL P       	!I  Physical grating position, a non-integer,
C			     equal to FLOAT(IGPOFF + IGP).  As usual,
C			     0 < IGPOFF < 7 and 0 < IGP < 23.
      REAL DELSTP  	!I  Deviation of grating position from nominal of
C			     given spectral line.  Nominal corresponds to
C			     the slit function measurements which were used
C			     to derive the parameters EPS, PHIZ, F.
C			     (Spring, 1985, DOY 112-114).
C			    In particular, OPCAL at that time showed a maximum
C			     at P = 6.632 +/- 0.034 at an SE telescope temp
C			     of c.a. 138 K.  The OPCAL intensity was c.a. 
C			     443.8 (1 RIM average). If, for the same conditions
C			     OPCAL is max at p = 7.632, then DELSTP is +1.000
C			     and PHIZ = PHIZ0 - EPS*DELSTP. (I.E. PHIZ has
C			     decreased and it takes more grating rotation to
C			     get the same wavelength.
      REAL TGR		!I  Grating temperature in degrees Kelvin 	    RMX
      REAL AINFL        !I  Inflation factor to account for increase in
C                            grating step size found during the mission.
C                            The step size is recomputed as (1. + AINFL) * EPS
C     REAL NIMS_WAVE	!O  Computed wavelength in micrometers		    RMX

C_DESC	Compute wavelength of detector at given grating position
C	for NIMS instrument P/F (FPA #2)

C	We include, for completeness, thermal contraction and refractive
C	effects.  NIMS_WAVE is given in microns.  We assume that the center  RMX
C	of det 10 is on the optical axis (XOFF = 0.0) since deviations
C	would be mimicked by changes in PHIZ.

C	Cincinnati measurements of FPA good to 0.0005" or 0.01 mm (or better).
C	Detectors 3,7,9 corrected slightly in position (see 4/28/87 work)

C_KEYS	NIMS, CALIBRATION

C_HIST
C	22 APR 1987  Bob Carlson  Adapted from WF, to replace WLNGTH
C	28 APR 1987  RWC  Slight changes in pos of 3,7,9 and in parameters.
C	27 AUG 1990  R Mehlman  Modified to add dependence on grating temp.
C			Name changed from WAVE to WAVEF to avoid confusion.
C	03 NOV 1992  RMX  Renamed from WAVEF.  Documentation to ISIS standard.
C       04 NOV 1998  RWC  Added step-size inflation parameter AINFL 
C	11 FEB 1999  RMX  Corrected AINFL documentation
C_END

      DIMENSION X(17)   ! Room temp positions for FPA #2, measured at CE
      DATA X/  12.400,  10.152,  15.794,  13.531,  11.275,  9.012,
     +          6.750,   4.506,   2.227,   0.000,  -2.253, -4.496,
     +         -6.739,  -8.981, -11.227, -13.457, -15.690/

      GAMMA = 0.2570      ! 1/2 angle of spectrometer, radians, from ICD
      GC = 39.051         ! Room temp grating constant, lines/mm, from PE
      EPS0 = 0.26569338/1000.         ! From calibration, rad/step	    RMX
      TAU = -.00020       ! Temperature dependence of grating stepsize	    RMX
      TGR0 = 124.0	  ! Grating temperature offset, degrees K	    RMX
      EPST = EPS0*(1.0+TAU*(TGR-TGR0)) ! Grating stepsize		    RMX
      EPS = (1. + AINFL)*EPST          ! Correcting for inflation of step size
      PHIZ0 = 0.058257226 ! Grating rotation at P=0
      F = 201.2029        ! Focal length
      TCA = 0.99929       ! Thermal contraction for alumina FPA substrate
      TCP = 0.99960       !    "         "       "  pyrex grating  "
      T = 3.85            ! Total thickness of sapphire windows, mm, +-.5
      AN = 1.692          ! Mean index of refraction of sapphire
      S = TCP*(1000./GC)  ! Ruling spacing in microns at 140 K 
      PHIZ = PHIZ0 - EPS*DELSTP
      M = 1               ! Assume first order
      IF(IDET.LT.3) M = 2 ! 2nd order for silicon
      REF = 1. + (1.-1./AN)*(T/F)  ! Rel increase in angle due to refraction
      CHI = ATAN(REF*TCA*X(IDET)/F)
      PHI = PHIZ + EPS*P  ! Total rotation angle of grating
      ALPHA = GAMMA + PHI ! Angle of incidence
      BETA = -(GAMMA + CHI - PHI)  ! Angle of diffraction
      NIMS_WAVE = S*(SIN(ALPHA) + SIN(BETA))/FLOAT(M)			  ! RMX
      RETURN
      END


C*******************************************************************
      SUBROUTINE NIMS_CDS2OHMS(ICHNL,NDATA,OHMS,IR)

C_TITLE NIMS_CDS2OHMS  Compute resistance for given CDS channel and data number

C_ARGS
      INTEGER ICHNL		!I  Channel number (see DESC below)
      INTEGER NDATA		!I  Data number for channel ICHNL (range 0-255)
      REAL    OHMS		!O  Resistance corresponding to ICHNL and NDATA
      INTEGER IR		!O  Return code:  0 OK, -1 ICHNL invalid

C_DESC  This subroutine applies to the following CDS channels:

C     ICHNL  CDS CHANNEL  LOCATION  SENSOR  SE TERMINALS
C     =====  ===========  ========  ======  ============
C       1       E-1911      Shield   15319     L - M
C       2       E-1912    Telescope  15322     F - G
C       3       E-1913    Gr. Mech.  15323     D - E
C       4       E-1914      Chopper    ?       B - C
C       5       E-1915  Electronics  UA-75     N - d

C   Values for E-1910 and E-1916, both of which relate
C   to the FPA temperature, are handled in subroutine NIMS_FPATMP.
C   In this routine, we use the given CDS coefficients to 
C   determine a resistance.  In the first four cases, the
C   answer is found by solving a cubic equation.  The 
C   electronics temperature is linearly related to the DN 
C   value.

C   Temperature may be computed from OHMS by routine NIMS_OHMS2TMP.

C   NOTE: We are solving the equation
C    DN = C3*OHMS^3 + C2*OHMS^2 + C1*OHMS + C0
C   The C's are from GLL 625-308

C   REFERENCES: GLL Telemetry Conversion Handbook, 625-308
C               Appendices A,B of NIMS calibration document.

C_CALLS U_FIND_ROOT3

C_HIST	16-AUG-88  R Carlson  JPL  ORIGINAL VERSION
C	20-FEB-93  R Mehlman  UCLA/IGPP  Renamed from CDS2OHMS, renamed U_ROOT3
C			and changed argument order, error parameter added,
C			internal documentation put in ISIS format.
C_END

      DIMENSION C0(5), C1(5), C2(5), C3(5)  ! CDS coefficients
      DATA C0/  -53.2668, -52.8356, -51.8332, -51.1541, 
     &          -209.424/
      DATA C1/  0.745103, 0.742733, 0.737517, 0.746692,
     &          0.719420/
      DATA C2/-0.537878E-03, -0.525888E-03, -0.512928E-03,
     &        -0.520592E-03, -0.000000E-03/
      DATA C3/ 0.251498E-06,  0.237182E-06,  0.226003E-06,
     &         0.204892E-06,  0.000000E-06/
      EPS = 0.001  ! Error parameter

      IF(ICHNL.LT.1) GO TO 750   ! Out of range
      IF(ICHNL.GT.5) GO TO 750   ! Out of range
      DN = FLOAT(NDATA)
      RTRY = (DN - C0(ICHNL))/C1(ICHNL)  ! Linear approximation
      IF(ICHNL.EQ.5) GO TO 800  ! Linear is adequate for chnl 5
      CALL U_FIND_ROOT3(C3(ICHNL),C2(ICHNL),C1(ICHNL),C0(ICHNL)-DN,EPS,
     1 RTRY,OHMS)
      GO TO 900
  750 IR = -1
C  750 TYPE *,'ICHNL out of range'
      RETURN
  800 OHMS = RTRY   ! For channel 5
  900 IR = 0
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_ENGR(ENGR,TDN,ODN,TEMP,RESIST)

C_TITLE NIMS_ENGR  Extract NIMS Engineering DNs, compute temperatures, etc.

C_ARGS
      BYTE ENGR(2,91)			!I  Engineering items, 2/frame over RIM
      INTEGER TDN(7,6)			!O  Temperature DNs, 6 kinds, 7 per RIM
C					     1 Focal Plane Assembly
C					     2 Radiator Shield
C					     3 Telescope
C					     4 Grating
C					     5 Chopper
C					     6 Electronics
      INTEGER ODN(4)			!O  Other 4 items, DNs, 1 per RIM
C					     1 Flexprint temperature
C					     2 RCT-Pt temperature
C					     3 RCT-Ni temperature
C					     4 RCT-reference resistance
      REAL*4 TEMP(7,7)			!O  Temperatures (deg K) corresponding
C					     to TDN, plus alternate TFPA (from
C					     composite Pt + Flexprint (SS)),
C					     7 per RIM
      REAL*4 RESIST(7,4)		!O  Resistances (ohms), 7 per RIM
C					     1 RKA:   R across terminals K,a
C					     2 RHPRK: RH + RK
C					     3 RHK:   R across H,K (RH+RK+Rpt)
C					     4 RPT:   R of Pt sensor

C_DESC  Extract NIMS Engineering DNs from RIM's worth of the 2 Engineering
C	bytes on the EDR; compute corresponding temperatures and resistances.
C	Item ID's, names and frequencies are:
C	  E-1910  Focal Plane Temperature		7 per RIM
C	  E-1911  Radiator Shield Temperature		7 per RIM
C	  E-1912  Telescope Temperature			7 per RIM
C	  E-1913  Grating Mechanism Temperature		7 per RIM
C	  E-1914  Optical Chopper Temperature		7 per RIM
C	  E-1915  Electronics Chassis Temperature	7 per RIM
C	  E-1916  Flexprint Temperature			1 per RIM
C	  E-1945  RCT-NIMS Temperature-Pt		1 per RIM
C	  E-1946  RCT-NIMS Temperature-Ni		1 per RIM
C	  E-1947  RCT-NIMS Reference R(esistance)	1 per RIM
C	Reference: GLL 3-280 Table A2.2.8 (Engineering Measurements)
C	See NIMS_* subroutines for details of temperature computations.

C_CALLS U_FILL1, U_MOVE1 (isislib)
C	NIMS_FPATMP, NIMS_CDS2OHMS, NIMS_OHMS2TMP (nimslib)

C_HIST	20feb93, R Mehlman, UCLA/IGPP  ORIGINAL VERSION

C_END

      INTEGER TFRAME(6) /0,0,1,1,2,2/	! Temperature DN frames (mod 13, 7/RIM)
      INTEGER TBYTE(6)  /1,2,1,2,1,2/	! Temperature DN bytes
      INTEGER OFRAME(4) /56,29,58,85/   ! Other item DN frames (of 0-90)
      INTEGER OBYTE(4)  / 1, 1, 1, 1/   ! Other item DN bytes
      REAL    EPS /.001/		! Epsilon for NIMS_FPATMP

      DO J=1,6							! Temp. DNs
	IFRAME = TFRAME(J) + 1
	DO I=1,7
	  CALL U_FILL1( 0, 4, TDN(I,J))
	  CALL U_MOVE1( 1, ENGR( TBYTE(J), IFRAME), TDN(I,J) )
	  IFRAME = IFRAME + 13
	ENDDO
      ENDDO

      DO I=1,4							! Other DNs
	CALL U_FILL1( 0, 4, ODN(I))
	CALL U_MOVE1( 1, ENGR( OBYTE(I), OFRAME(I) + 1), ODN(I) )
      ENDDO

      DO I=1,7							! Compute:
	CALL NIMS_FPATMP( TDN(I,1), ODN(1), EPS, RESIST(I,1),	!  FPA temp, R's
     1   RESIST(I,2), RESIST(I,3), RESIST(I,4), TEMP(I,7), TEMP(I,1) )
	DO J=2,6			  			!  Other temps
	  CALL NIMS_CDS2OHMS( J-1, TDN(I,J), TOHM, IR)
	  CALL NIMS_OHMS2TMP( J-1, TOHM, TCEN, TEMP(I,J), IR)
	ENDDO
      ENDDO

      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_FPATMP(N1910,N1916,EPS,RKA,RHPRK,RHK,RPT,
     1 TPTSS,TPT)

C_TITLE NIMS_FPATMP  Compute FPA temperature from CDS engineering data

C_ARGS
      INTEGER	N1910		!I  Data number for CDS channel E-1910
      INTEGER	N1916		!I  Same for E-1915 (flexprint)
      REAL	EPS		!I  Error parameter for cubic root routine
      REAL	RKA		!O  Resistance across terminals K,a
      REAL	RHPRK		!O  RH + RK
      REAL	RHK		!O  Resistance across H,K (RH + RK _ RPt)
      REAL	RPT		!O  Resistance of Pt sensor (#15604, FPA 002)
      REAL	TPTSS		!O  FPA temperature from composite Pt +
C					flexprint (SS), in K
      REAL	TPT		!O  FPA temperature from Pt, in K (USE THIS)

C_DESC	Computes FPA temperature from CDS engineering data for P/F unit.

C	The principal output FPA temperature is TPT, which uses just the
C	platinum sensor after subtracting out the simultaneously measured
C	flexprint resistance.  The other output FPA temperature, TPTSS,
C	assumes the flexprint resistance is also temperature sensitive
C	(slightly).  The combination of the two is a "resistance thermometer".
C	TPT and TPTSS should agree, providing the optics are cold.

C	References: Appendix A, B OF calibration document,
C                   GLL Telemetry Conversion Handbook, 625-308

C_CALLS U_FIND_ROOT3

C_HIST	04-SEP-85  R Carlson  JPL  ORIGINAL VERSION
C	20-FEB-93  R Mehlman  UCLA/IGPP  Renamed from FPATMP, renamed U_ROOT3
C			and changed argument order, internal documentation put
C			in ISIS format.
C_END

      C01910 = -0.100047619E+03    ! CDS coeff, E-1910
      C11910 =  0.680503115E+00
      C21910 = -0.342859039E-03
      C31910 =  0.925943766E-07
      C01916 = -0.246681101E+03    ! For E-1916
      C11916 =  0.721500002E+00
      RF = 413.91                  ! Ohms, fixed R in FPA
      ETA = 0.98698                ! (RH + RK)/(RK + Ra)
      A0PT = -76.64005             ! For Pt R, R=A0 + A1*T + A2*T**2
      A1PT =  2.265692
      A2PT = -5.6604762E-04
      A0SS =  70.83                ! For SS flexprint resistance
      A1SS =  0.41932
      A2SS = -1.8061E-03
      B0 =  34.20093               ! For PT, T = B0 + B1*R + B2*R**2
      B1 =  0.4473015
      B2 =  6.119418E-05
C
      A0 = A0PT + A0SS             ! For composite Pt + SS
      A1 = A1PT + A1SS
      A2 = A2PT + A2SS
      DN1910 = FLOAT(N1910)
      DN1916 = FLOAT(N1916)
      RKA = (DN1916 - C01916)/C11916  ! E-1916 is linear
      RHPRK = ETA*(RKA - RF)          !  from least squares linear fit
      RTRY = (DN1910 - C01910)/C11910 ! Initial guess at RHK
      CALL U_FIND_ROOT3(C31910,C21910,C11910,C01910-DN1910,EPS,RTRY,RHK)
      C = A0 - RHK                    ! For quadratic equation 
      B = A1                          ! For composite RSS + RPt  
      A = A2
      TPTSS = (-B + SQRT(B**2-4.*A*C))/(2.*A)
      RPT = RHK - RHPRK
      TPT = B0 + B1*RPT + B2*RPT**2
      GO TO 800
C     DATA IOLU /6/
C     WRITE(IOLU,200) 
C     WRITE(IOLU,201) RKA
C     WRITE(IOLU,202) RHPRK
C     WRITE(IOLU,203) RHK
C     WRITE(IOLU,204) RPT
C     WRITE(IOLU,205) TPTSS
C     WRITE(IOLU,206) TPT
C     WRITE(IOLU,207) N1910
C     WRITE(IOLU,208) N1916
C     WRITE(IOLU,209)
C 200 FORMAT(1H1,5X,'NIMS P/F FPA Temperature From CDS Values')
C 201 FORMAT(1H0,5X,'R(K-a)...............................',F7.2)
C 202 FORMAT(1H ,5X,'R(H) + R(K)..........................',F7.2)
C 203 FORMAT(1H ,5X,'R(H-K)...............................',F7.2)
C 204 FORMAT(1H ,5X,'R(Pt)................................',F7.2)
C 205 FORMAT(1H ,5X,'Temperature(Pt+SS)...................',F7.2)
C 206 FORMAT(1H ,5X,'Temperature(Pt)......................',F7.2)
C 207 FORMAT(1H ,5X,'DN (E-1910)..........................',I4)
C 208 FORMAT(1H ,5X,'DN (E-1916)..........................',I4)
C 209 FORMAT(1H)
  800 CONTINUE
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_OHMS2TMP(ICHNL,OHMS,TCEN,TKEL,IR)

C_TITLE NIMS_OHMS2TMP  Compute NIMS P/F temperatures from measured resistances

C_ARGS
      INTEGER	ICHNL		!I  Channel number (see DESC below)
      REAL	OHMS		!I  Resistance for channel ICHNL
      REAL	TCEN		!O  Temperature (degrees Centigrade)
      REAL	TKEL		!O  Temperature (degrees Kelvin)
      INTEGER	IR		!O  Return code:  0 OK, -1 invalid channel

C_DESC	This routine is for the shield, telescope, grating, chopper, and
C	electronics.  (Use NIMS_FPATMP for focal plane temperatures.)
C	Uses measured resistance and corresponding Callander - van Dusen
C	coefficients to figure out temperatures.  Channel identification
C	given  below.  The identity of the chopper sensor was not recorded
C	by Bulova, so we use parameters for 15323 (grating mechanism).

C   NO. LOCATION       TERMINALS     CDS CHANNEL    SENSOR
C   === ========       =========     ===========    ======
C    1  Shield           L - M          E-1911       15319
C    2  Telescope        F - G          E-1912       15322
C    3  Grating Mech.    D - E          E-1913       15323
C    4  Opt. Chopper     B - C          E-1914         ?
C    5  Electronics      N - d          E-1915        UA75

C_CALLS NIMS_PT_RES_THERM

C_HIST	25-AUG-88  R Carlson  JPL  Adapted from INSTMP
C	01-FEB-93  R Mehlman  UCLA/IGPP  Renamed from OHMS2TMP, renamed
C			subroutine, error parameter added, internal
C			documentation put in ISIS format.

C_END

      DIMENSION RZ(5), ALPHA(5), DELTA(5), BETA(5)  ! COEFFICIENTS
      DATA RZ/   497.793,
     &           498.235,
     &           498.434,
     &           498.434,
     &           500.089/
      DATA ALPHA/0.0038896874,
     &           0.0038918771,
     &           0.0038922303,
     &           0.0038922303,
     &           0.0038830074/
      DATA DELTA/1.386378,
     &           1.403585,
     &           1.396086,
     &           1.396086,
     &           1.386402/
      DATA BETA/ 0.2247264,
     &           0.2131825,
     &           0.2143358,
     &           0.2143358,
     &           0.2327845/

  100 I = ICHNL
      IF(I.LT.(1)) GO TO 900  ! Out of range
      IF(I.GT.(5)) GO TO 900  ! Out of range
      R = OHMS
      CALL NIMS_PT_RES_THERM(R,RZ(I),ALPHA(I),DELTA(I),BETA(I),
     1 TCEN,TKEL)
      IR = 0
      RETURN
  900 IR = -1
      RETURN
      END

C*******************************************************************
      SUBROUTINE NIMS_PT_RES_THERM(R,RZ,ALPHA,DELTA,BETA,TCEN,TKEL)

C_TITLE NIMS_PT_RES_THERM  Compute Pt resistance thermometer temperatures

C_ARGS
	REAL	R		!I  Measured resistance
	REAL	RZ		!I  Resistance at 0 degrees C
	REAL	ALPHA		!I  | Constants of
	REAL	DELTA		!I  | equation described
	REAL	BETA		!I  | in DESC below
	REAL	TCEN		!O  Temperature in degrees Centigrade
	REAL	TKEL		!O  Temperature in degrees Kelvin

C_DESC	Computes NIMS Platinum resistance thermometer temperatures from 
C	measured resistance.  Uses Callandar - van Dusen equation, measured
C	value of resistance (R), and previously determined constants
C	(RZ - resistance at 0 deg C, ALPHA, DELTA, BETA) to determine
C	temperature in Centigrade and Kelvin (TCEN,TKEL).  For TAU = TCEN/100,
C	the empirical Callandar - van Dusen equation is:

C         R/RZ = 1 + ALPHA[100*TAU - DELTA*TAU*(TAU-1)
C                                  - BETA*(TAU-1)*TAU**3]

C   	with BETA = 0 if TCEN > 0. (Note that the value of BETA supplied
C	to this routine is finite and for TCEN < 0; it is ignored as
C	appropriate.)  The program first solves the quadratic equation
C	(BETA = 0).  For RHO = R/RZ > 1, the resulting TAU gives the
C	temperatures.  For RHO < 1, the BETA term gives a quartic equation,
C	and the above-derived TAU is used as a starting point to solve
C	this quartic.

C_CALLS U_FIND_ROOT4

C_HIST	24-APR-87  R Carlson  JPL  ORIGINAL VERSION
C	20-FEB-93  R Mehlman  UCLA/IGPP  Renamed from PRT, renamed U_ROOT4 and
C			changed argument order, internal documentation put in
C			ISIS format.
C_END

      RHO = R/RZ
      A = -ALPHA*DELTA         ! Coeff in A*X**2 + B*X + C
      B = ALPHA*(100.+DELTA)   ! Linear coefficient
      C = 1. - RHO             ! C
      TAU = (-B + SQRT(B**2-4.*A*C))/(2.*A)  ! Quadratic sol'n
      IF(RHO.GE.(1.00)) GO TO 800            ! for TCEN > 0
      TAUS = TAU               ! Initial approximation for TCEN < 0
      D = ALPHA*BETA           ! Cubic coefficient
      E = -D                   ! Coefficient of TAU**4
      EPS = 0.0001             ! Iterate to 0.01 degree
      CALL U_FIND_ROOT4(E,D,A,B,C,EPS,TAUS,TAU)
  800 TCEN = 100.*TAU          ! In Centigrade
      TKEL = TCEN + 273.15     ! Ice point = 0 deg C = 273.15 K
      RETURN
      END

C*******************************************************************
      SUBROUTINE U_FIND_ROOT3(A3,A2,A1,A0,EPS,XTRY,X)

C_TITLE U_FIND_ROOT3  Find root of cubic equation by Newton's method
C_ARGS
      REAL	A3		!I  Coefficients of cubic equation
      REAL	A2		!I
      REAL	A1		!I
      REAL	A0		!I
      REAL	EPS		!I  Rough accuracy
      REAL	XTRY		!I  First guess as to where root is
      REAL	X		!O  Result

C_DESC	Find root of cubic equation: 0 = A3*X**3 + A2*X**2 + A1*X + A0
C	using Newton's method.

C_HIST	??-JUL-85  R Carlson  JPL  ORIGINAL VERSION
C	01-FEB-93  R Mehlman  UCLA/IGPP  Renamed from ROOT3, argument order
C			and internal documentation to ISIS standard.
C_END

      XNEW = XTRY
  100 XOLD = XNEW
      Y = A3*XOLD**3 + A2*XOLD**2 + A1*XOLD + A0
      YP = 3.*A3*XOLD**2 + 2.*A2*XOLD + A1
      XNEW = XOLD - (Y/YP)
      IF(ABS(XNEW-XOLD).GT.EPS) GO TO 100
      X = XNEW
      RETURN
      END

C*******************************************************************
      SUBROUTINE U_FIND_ROOT4(A4,A3,A2,A1,A0,EPS,XTRY,X)

C_TITLE U_FIND_ROOT4  Find root of quartic equation by Newton's method

C_ARGS
      REAL	A4		!I  Coefficients of quartic equation
      REAL	A3		!I
      REAL	A2		!I
      REAL	A1		!I
      REAL	A0		!I
      REAL	EPS		!I  Rough accuracy
      REAL	XTRY		!I  First guess as to where root is
      REAL	X		!O  Result


C_DESC	Find root of quartic equation:

C		 0 = A4*X**4 + A3*X**3 + A2*X**2 + A1*X + A0

C	using Newton's method.

C_HIST	22-APR-87  R Carlson  JPL  Adapted from ROOT3.
C	01-FEB-93  R Mehlman  UCLA/IGPP  Renamed from ROOT4, argument order
C			and internal documentation to ISIS standard.
C_END

      XNEW = XTRY
  100 XOLD = XNEW
      Y = A4*XOLD**4 + A3*XOLD**3 + A2*XOLD**2 + A1*XOLD + A0
      YP = 4.*A4*XOLD**3 + 3.*A3*XOLD**2 + 2.*A2*XOLD + A1
      XNEW = XOLD - (Y/YP)
      IF(ABS(XNEW-XOLD).GT.EPS) GO TO 100
      X = XNEW
      RETURN
      END


C*******************************************************************

	SUBROUTINE U_ERROR(ERRKEY,ERRMES,ERRVAL,ACTION,RET)

C_TITLE  U_ERROR  ISIS error message handling facility

C_ARGS
	CHARACTER*(*) ERRKEY  !I Error key, 17 characters or less in length
C				  Form "pgm/rtne-msgid" --> $SKEY under TAE.
	CHARACTER*(*) ERRMES  !I Error message
	INTEGER*4     ERRVAL  !I Error value -- unique number
C				  for $SFI under TAE or $STATUS under VMS.
	INTEGER*4     ACTION  !I Action code
C				  ACTION=1, return after logging error message
C				  ACTION=2, terminate pgm after logging error
	INTEGER*4     RET     !I Return code
C				  RET=0, success
C				  RET not 0, U_ERROR could not log error msg
C_DESC
C   See ISIS Programming Manual entry for
C    [ISIS.SUBS.ISISLIB.UTILIB.VMS]U_ERROR.FOR for details.

C_KEYS STRING, SESSION_LOG, TERMINAL, VMS

C_HIST	16nov87  Eric Eliason, USGS   Original specs
C	29may88  Bob Mehlman, UCLA/IGPP  Dummy interim version
C	31jul90  Kris Becker, USGS  ISIS Build 2 version, updated documentation
C	16sep90  KJB, Added check in ERRKEY for length, modified ACTION descr.
C	08dec93  Lucas Kamp, JPL/MIPS  Simulated routine for Vicar

C_END

c  This routine is needed to support the error-handling code in the
c  external NIMSCAL s/w -- cannot be delivered external because it
c  makes VICAR calls

c  TBD:  find some way to pass ERRVAL to TAE

	call xvmessage( errmes, errkey)
	if (action.eq.2) call abend
	ret = 0
	return
	end


C*******************************************************************
        subroutine u_fill1( b, n, buf)
c  11dec94  -lwk-  simulate ISIS routine for ported NIMS_CAL s/w
        byte b, buf(1)
        call mve( 1, n, b, buf, 0, 1)   ! use the VICAR call
        return
        end


C*******************************************************************
        subroutine u_move1( n, buf1, buf2)
c  11dec94  -lwk-  simulate ISIS routine for ported NIMS_CAL s/w
        byte buf1(1), buf2(1)
        call mve( 1, n, buf1, buf2, 1, 1)   ! use the VICAR call
        return
        end


C*******************************************************************
	SUBROUTINE xNIMS_GET_CAL( bFILNAM, i, istat)
c  this is part of the "C-bridge" nonsense needed for porting code ...
c  it is called by zNIMS_GET_CAL.c (in-line to NIMSCMM.C)
	byte bfilnam(1)
	character*64 filnam

	if (i.gt.64) call xvmessage('** NIMS CAL filename too long')
	filnam = ' '
	call mvlc( bfilnam, filnam, i)
	call NIMS_GET_CAL( filnam, istat)
	return
	end


C*******************************************************************
	SUBROUTINE xNIMS_GET_CAL_A( bFILNAM, i, istat)
c  this is part of the "C-bridge" nonsense needed for porting code ...
c  it is called by zNIMS_GET_CAL_A.c (in-line to NIMSCMM.C)
	byte bfilnam(1)
	character*64 filnam

	if (i.gt.64) call xvmessage('** NIMS CAL filename too long')
	filnam = ' '
	call mvlc( bfilnam, filnam, i)
	call NIMS_GET_CAL_A( filnam, istat)
	return
	end


C*******************************************************************
	SUBROUTINE xNIMS_GET_DARK( bFILNAM, i, idrk_NS, idrk_NL, DRKBUF,
     1   istat)
c  this is part of the "C-bridge" nonsense needed for porting code ...
c  it is called by zNIMS_GET_DARK.c (in-line to NIMSCMM.C)
	byte bfilnam(1)
	character*64 filnam

	if (i.gt.64) call xvmessage('** NIMS Dark filename too long')
	filnam = ' '
	call mvlc( bfilnam, filnam, i)
	call nims_get_dark( filnam, idrk_ns, idrk_nl, drkbuf, istat)
	return
	end


C*******************************************************************
	SUBROUTINE xNIMS_GET_DARK_A( bFILNAM, i, idrk_NS, idrk_NL, DRKBUF,
     1   istat)
c  this is part of the "C-bridge" nonsense needed for porting code ...
c  it is called by zNIMS_GET_DARK_A.c (in-line to NIMSCMM.C)
	byte bfilnam(64)
	character*64 filnam
	real*4 drkbuf(712)

	if (i.gt.64) call xvmessage('** NIMS Dark filename too long')
	filnam = ' '
	call mvlc( bfilnam, filnam, i)
	call nims_get_dark_a( filnam, idrk_ns, idrk_nl, drkbuf, istat)
	return
	end

