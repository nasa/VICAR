$!****************************************************************************
$!
$! Build proc for MIPL module minmap
$! VPACK Version 1.5, Friday, December 10, 1993, 12:53:38
$!
$! Execute by entering:		$ @minmap
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
$ write sys$output "*** module minmap ***"
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
$   if F$SEARCH("minmap.imake") .nes. ""
$   then
$      vimake minmap
$      purge minmap.bld
$   else
$      if F$SEARCH("minmap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake minmap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @minmap.bld "STD"
$   else
$      @minmap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create minmap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack minmap.com -
	-s minmap.f -
	-p minmap.pdf -
	-i minmap.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create minmap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	External GSFIT
        Parameter (NV=4,NP=3)
	Dimension P(NV,NP),X(NP),Y(NV),RMIN(NP),RMAX(NP)
	Dimension OBS(6),WVCENT(6),OUTBUF(10000,NP)
	Dimension FILTER(158,6),WVLN(158)
	Integer*2 INBUF(10000,6)
	Logical IPROG,XVPTST
	Common/BOX1/RMIN,RMAX
	Common/RAWFIL/FILTER,WVLN,ICALDATE
	Common/TIMS/WVCENT
	Common/DATA1/OBS
	Data RMIN/0.0,0.0,0.0/
	Data RMAX/1.0,0.0,4.0/

C------------------------------------------------------------------------
C                           Format Block
C------------------------------------------------------------------------
100	Format(/,' Progress Report for MINMAP ',/)
101	Format('+ Current Output Line: ',I10,
     +         '   Elapsed Time: ',F9.2,'   seconds')
102	Format(I2,1X,I2,1X,I2)

C--------------------------------------------------------------------------
C			Open datasets
C--------------------------------------------------------------------------
	Call XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	Call XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	Call XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	Call XVUNIT(IOUTUNIT1,'OUT',1,ISTAT,' ')
	Call XVOPEN(IOUTUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',
     +		    'U_NL',NL,'U_NS',NS,'U_NB',3,'U_ORG','BIL',' ')

C--------------------------------------------------------------------------
C	            Get the User's Parameters
C--------------------------------------------------------------------------
	Call XVPARM('SCALE',USCAL,ICNT,IDEF,0)
	Call XVPARM('RND_SEED',ISEED,ICNT,IDEF,0)
	ISEED=-ISEED
	IPROG=XVPTST('PROGRESS')

C---------------------------------------------------------------------------
C                  Calculate the TIMS band centers
C---------------------------------------------------------------------------
	Call BDCENT(IUNIT1,WVCENT)

C---------------------------------------------------------------------------
C 			Start Line Loop
C---------------------------------------------------------------------------
	If(IPROG) Write(6,100)
	ICOUNT=1
	IEL = ISL+NL-1
	Do ILIN=ISL,IEL	
				
C---------------------------------------------------------------------------
C            Read in a line of imagery
C---------------------------------------------------------------------------
	    Do IBND=1,6
		Call XVREAD(INUNIT,INBUF(1,IBND),ISTAT,'LINE',ILIN,'BAND',
     +			    IBND,'SAMP',ISS,'NSAMPS',NS,' ')
	    EndDo

C----------------------------------------------------------------------------
C                        Start Sample Loop
C
C	Read in emissivities, convert to "reflectance"
C----------------------------------------------------------------------------
	    JCOUNT=1
	    Do IPIX=1,NS
	       Do IBND=1,6
	          OBS(IBND) = 0.0
		  OBS(IBND) = FLOAT(INBUF(IPIX,IBND))/USCAL
	          OBS(IBND) = 1.0 - OBS(IBND)
	       EndDo

C------------------------------------------------------------------------------
C                      Bracket the position of the maximum
C------------------------------------------------------------------------------
            INDXHI=0
            RTEST=-1e6
            Do IBND=1,6
	       If(OBS(IBND).GT.RTEST) Then
                  RTEST=OBS(IBND)
                  INDXHI=IBND
               EndIf
            EndDo
            If(INDXHI.LE.3) Then	
	       RMIN(2)=8.0
	       RMAX(2)=10.0
            EndIf
	    If(INDXHI.GE.4) Then
	       RMIN(2)=9.0
	       RMAX(2)=12.0
	    EndIf

C-------------------------------------------------------------------------------
C	         Solve for the three model parameters
C 
C                Initialize the vertex matrix
C-------------------------------------------------------------------------------
	     Do IVRT=1,NV		 
	        Do IPRM=1,NP		 
                   P(IVRT,IPRM)=0.0 		
                   P(IVRT,IPRM)=RAN1(ISEED)		
                EndDo
             EndDo

C-------------------------------------------------------------------------------
C            Fill the model vector
C------------------------------------------------------------------------------
	     Do IVRT=1,NV
	        Do IPRM=1,NP
		   X(IPRM)=0.0
	           X(IPRM)=P(IVRT,IPRM)
                EndDo
		Y(IVRT)=0.0
                Y(IVRT)=GSFIT(X)
	     EndDo

C-------------------------------------------------------------------------------
C            Perform the inversion
C-------------------------------------------------------------------------------
	     NDIM=NP
	     Call AMOEBA(P,Y,NV,NP,NDIM,GSFIT) 

C------------------------------------------------------------------------
C	Decode the model parameter estimates returned from AMOEBA
C-----------------------------------------------------------------------
             Do IPRM=1,NP
	        X(IPRM)=0.0					
	        X(IPRM)=P(1,IPRM)
                X(IPRM)=X(IPRM)*(RMAX(IPRM)-RMIN(IPRM))+RMIN(IPRM)
                IF(IPRM.EQ.1) X(IPRM)=1.0-X(IPRM)
                OUTBUF(IPIX,IPRM)=ABS(X(IPRM))
              EndDo  
            EndDo


C------------------------------------------------------------------------------
C                    Write to the Output File
C-------------------------------------------------------------------------------
	     Do IPRM=1,NP
	     Call XVWRIT(IOUTUNIT1,OUTBUF(1,IPRM),ISTAT,' ')
	     EndDo
	        If(IPROG) Then
	           Write(6,101) ICOUNT
	           JCOUNT=JCOUNT+1
	        EndIf
	     If(IPROG) ICOUNT=ICOUNT+1
	EndDo

C-------------------------------------------------------------------------------
C                           Closing Business
C-------------------------------------------------------------------------------
	Call XLADD(IUNIT2,'HISTORY','LBL1','MINIMUM FEATURE MAP',
     +             ISTAT,'FORMAT','STRING','ULEN',19,' ')
	Call XVCLOSE(IUNIT1,ISTAT,' ')
	Call XVCLOSE(IUNIT2,ISTAT,' ')
	Return
	End




C------------------------------------------------------------------------------
C		           ----- TOOLKIT -----
C------------------------------------------------------------------------------
      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FUNK)
      PARAMETER (NMAX=20,ALPHA=1.0,BETA=0.5,GAMMA=2.0)
      PARAMETER (ITMAX=1000,FTOL=1.0E-6)
      DIMENSION P(MP,NP),Y(MP),PR(NMAX),PRR(NMAX),PBAR(NMAX)

      MPTS=NDIM+1
      ITER=0
1     ILO=1


      IF(Y(1).GT.Y(2))THEN
        IHI=1
        INHI=2
      ELSE
        IHI=2
        INHI=1
      ENDIF
      DO 11 I=1,MPTS
        IF(Y(I).LT.Y(ILO)) ILO=I
        IF(Y(I).GT.Y(IHI))THEN
          INHI=IHI
          IHI=I
        ELSE IF(Y(I).GT.Y(INHI))THEN
          IF(I.NE.IHI) INHI=I
        ENDIF
11    CONTINUE

C------------------------------------------------------------
C	Test the range between low and high vertices
C------------------------------------------------------------
	RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
	If(RTOL.LT.FTOL.OR.ITER.EQ.ITMAX) Then
            Return
      	ENDIF

      ITER=ITER+1
      DO 12 J=1,NDIM
        PBAR(J)=0.
12    CONTINUE
      DO 14 I=1,MPTS
        IF(I.NE.IHI)THEN
          DO 13 J=1,NDIM
            PBAR(J)=PBAR(J)+P(I,J)
13        CONTINUE
        ENDIF
14    CONTINUE
      DO 15 J=1,NDIM
        PBAR(J)=PBAR(J)/NDIM
        PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15    CONTINUE
      YPR=FUNK(PR)
      IF(YPR.LE.Y(ILO))THEN
        DO 16 J=1,NDIM
          PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16      CONTINUE
        YPRR=FUNK(PRR) 
        IF(YPRR.LT.Y(ILO))THEN
          DO 17 J=1,NDIM
            P(IHI,J)=PRR(J)
17        CONTINUE
          Y(IHI)=YPRR
        ELSE 
          DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18        CONTINUE
          Y(IHI)=YPR
        ENDIF
      ELSE IF(YPR.GE.Y(INHI))THEN
        IF(YPR.LT.Y(IHI))THEN
          DO 19 J=1,NDIM
            P(IHI,J)=PR(J)
19        CONTINUE
          Y(IHI)=YPR
        ENDIF
        DO 21 J=1,NDIM
          PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21      CONTINUE
        YPRR=FUNK(PRR)
        IF(YPRR.LT.Y(IHI))THEN
          DO 22 J=1,NDIM
            P(IHI,J)=PRR(J)
22        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 24 I=1,MPTS
            IF(I.NE.ILO)THEN
              DO 23 J=1,NDIM
                PR(J)=0.5*(P(I,J)+P(ILO,J))
                P(I,J)=PR(J)
23            CONTINUE
              Y(I)=FUNK(PR)
            ENDIF
24        CONTINUE
        ENDIF
      ELSE
        DO 25 J=1,NDIM
          P(IHI,J)=PR(J)
25      CONTINUE
        Y(IHI)=YPR
      ENDIF
      GO TO 1
      END




        FUNCTION GSFIT(X)
	Parameter (PI=3.141592654,NP=3,NV=6)
	Dimension X(NP),Y(NP),RMIN(NP),RMAX(NP),OBS(NV),WAVE(NV)
	Common/DATA1/OBS
	Common/BOX1/RMIN,RMAX
	Common/TIMS/WAVE

C----------------------------------------------------------------------
C	Decode the normalized model parameters
C----------------------------------------------------------------------
	Do i=1,NP
	   Y(i)=X(i)*(RMAX(i)-RMIN(i))+RMIN(i)
        EndDo
C-----------------------------------------------------------------------
C	Calculate the sum of the squares of the residuals
C-----------------------------------------------------------------------
        SUMSQ=0.
	Do i=1,NV
           REFL=GAUSS3(Y,WAVE(i))
	   RSD=OBS(i)-REFL
	   SUMSQ=SUMSQ+(RSD*RSD)
        EndDo
       
C-----------------------------------------------------------------------
C	Calculate the fit parameter
C----------------------------------------------------------------------- 
	GSFIT=SQRT(SUMSQ)
	Return
	End	
      


	FUNCTION GAUSS3(RMOD,WV)
C-------------------------------------------------------------------
C	Calculate the Gaussian Emittance, given the 
C	three model parameters and a wavelength
C--------------------------------------------------------------------
	Parameter(PI=3.14159265,N=3,FACT=1.0E-6)
	Dimension RMOD(N)
	A1=(WV-RMOD(2))/(RMOD(3)+FACT)	
	A2=PI*(A1**2.0)
	GAUSS3=RMOD(1)*EXP(-A2)
	Return
	End	



      FUNCTION RAN1(IDUM)
C-----------------------------------------------------------------
C	Improved random number generator, lifted from
C	Numerical Recipes. Set IDUM to any negative
C	number to initialize the sequence.
C-----------------------------------------------------------------
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF /0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97.OR.J.LT.1)PAUSE
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END


	Subroutine BDCENT(IPIX,WAVE)
C---------------------------------------------------------------------
C	Calculates the centers of the appropriate TIMS bands
C---------------------------------------------------------------------
	Real WAVE(6),FILTER(158,6),WVLN(158)
	CHARACTER*100 LABEL
	Common/RAWFIL/FILTER,WVLN,ICALDATE

        Call XVPARM('DATE',IDATE,ICNT,IDEF,0)
        If(IDATE.EQ.-1) Then
C
C		Check to see if the date is already in the label
C
	CALL XLGET(IPIX,'HISTORY','INFO1',LABEL,ISTAT,
     &		   'HIST','TIMSLOG','FORMAT','STRING',0)
	IF (ISTAT.LT.0) CALL XLGET(IPIX,'HISTORY','INFO1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',0)
	IF (ISTAT.LT.0) CALL XLGET(IPIX,'HISTORY','LAB1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',0)
	IF (LABEL(6:6).EQ.'D') THEN
	    READ (LABEL,80,err=95) MONTH,IDAY,IYEAR
   80	    FORMAT (14X,I2,1X,I2,1X,I2)
	ELSE
	    READ (LABEL,90,err=95) MONTH,IDAY,IYEAR
   90	    FORMAT (17X,I2,1X,I2,1X,I2)
	END IF
	IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	CONTINUE
	EndIf
        Call GETFIL(IDATE,0)
	Do IBND=1,6
	   PSUM=0.
	   FSUM=0.
	   WVLEN=7.488
	   Do INDX=1,158
              PSUM=PSUM+(FILTER(INDX,IBND)*WVLEN)
	      FSUM=FSUM+FILTER(INDX,IBND)
	      WVLEN=WVLEN+0.032
	   EndDo
	   WAVE(IBND)=PSUM/FSUM
	EndDo
	Return
	End
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create minmap.pdf
process help=*
  PARM INP  TYPE=(STRING,40)
  PARM OUT  TYPE=(STRING,40)
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM SL INTEGER DEFAULT=1
  PARM SS INTEGER DEFAULT=1
  PARM NL INTEGER DEFAULT=0
  PARM NS INTEGER DEFAULT=0
  PARM SCALE REAL DEFAULT=10000.
  PARM RND_SEED INTEGER DEFAULT=12345
  PARM DATE INTEGER DEFAULT=-1
  PARM REPORT KEYWORD DEFAULT=NOPROG VALID=(PROGRESS,NOPROG)		
END-PROC
.title
VICAR2 PROCEDURE MINMAP
.help
MINMAP is a procedure designed to find the depth and wavelength-
positon of the minimum feature in TIMS emissivity spectra. This
is accomplished by fitting a simple, 3-parameter Gaussian function
to the emissivity spectrum at every pixel. 

The output of this procedure is a 3-band image in REAL format. The
The first band of the contains the depth of the emissivity minimum,
which is an approximation of the minimum emissivity in the spectrum.
The second band contains the wavelength-position of the minimum, given
in micrometers. Note that this value may not correspond to the central
wavelength of any TIMS channel. The third band contains the area enclosed
by the Gaussian function, again given in units of micrometers (recall that
emissivity has no units).
.page
The input to MINMAP can be any six-band image file that contains estimates
of emissivity, and is in HALFWORD format. The default input consists of
emissivity estimates that have been multiplied by 10,000 (as produced by
the MODEL EMITTANCE and BBFIT options in TIMSEMIS), but the user can 
specify any scaling factor between 1 and 32,000. Regardless of the scaling
factor, the minimum emissivity estimate at a pixel CANNOT BE LESS THAN ZERO. 
The output values for the minimum emissivity will always range between 
zero and one.
.level1
.vari inp
Input file name
.vari out
output file name
.vari size
Window into input
.vari sl
Starting line
= size(1)
.vari ss
Starting sample
= size(2)
.vari nl
Number of lines
= size(3)
.vari ns
Number of samples
= size(4)
.vari SCALE
value used to scale
emissivity estimates
to HALFWORD
.VARI RND_SEED
Seed for the random
number generator
(5 digit integer)
.VARI DATE
optional over-ride
of image label date
(yymmdd)
.VARI REPORT
Report progress of 
procedure
.level2
.vari inp
NAME OF THE INPUT FILE.
.vari out
NAME OF THE OUTPUT FILE. THIS
IS A 5-BAND BIL REAL-FORMAT
FILE.
.vari size
THE SIZE PARAMETER DETERMINES THE BOUNDARIES 
IN THE INPUT FILE FROM WHICH THE MINMAP IS 
TO TAKE PLACE.  IT IS SPECIFIED AS  
(SL,SS,NL,NS), WHERE
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.vari SCALE
This parameter allows the user to specify the value that was
used to scale the emissivity estimates into HALFWORD pixels.
The default is 10,000, but any value between 1 and 32,000 is
acceptable. It is assumed that the minimum values in the 
estimated spectra were NOT LESS THAN ZERO.
.vari RND_SEED
This parameter allows the user to specify the "seed" used
to initialize the random number generator. If this is not
changed between runs of MINMAP, the random numbers used to
initialize the iterative curve-fitting algorithm will not
not change between the runs. The final fit should be 
independent of the starting point for the algorithm, but
it is good practice to change RND_SEED before the start
of each run. RND_SEED must be specified as a 5 digit integer.
.vari DATE
This parameter allows the user to over-ride the
date present in the VICAR label of the TIMS image.
The format for entering an alternate date is:
                  YYMMDD
$ Return
$!#############################################################################
$Imake_File:
$ create minmap.imake
#define  PROGRAM   minmap

#define MODULE_LIST minmap.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
#define LIB_VMS
$ Return
$!#############################################################################
