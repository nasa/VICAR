$!****************************************************************************
$!
$! Build proc for MIPL module filter2
$! VPACK Version 1.8, Thursday, February 29, 1996, 10:51:28
$!
$! Execute by entering:		$ @filter2
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module filter2 ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to filter2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("filter2.imake") .nes. ""
$   then
$      vimake filter2
$      purge filter2.bld
$   else
$      if F$SEARCH("filter2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake filter2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @filter2.bld "STD"
$   else
$      @filter2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create filter2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack filter2.com -
	-i filter2.imake -
	-p filter2.pdf -
	-t tstfilter2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create filter2.imake
#define PROCEDURE filter2
#define R2LIB
#define LIB_LOCAL
$ Return
$!#############################################################################
$PDF_File:
$ create filter2.pdf
procedure help=*
PARM	INP	TYPE=STRING	COUNT=(0:1)			DEFAULT=--
PARM	OUT	TYPE=STRING	COUNT=(0:1)			DEFAULT=--
PARM	SIZE	TYPE=INTEGER	COUNT=(0:4)			DEFAULT=--
PARM 	SL	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	SS	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	NL	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	NS	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	WTMAX	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	PRINT	TYPE=KEYWORD	COUNT=(0:1)	VALID="PRINT"	DEFAULT=--
PARM	RECT	TYPE=KEYWORD	COUNT=(0:1)	VALID="RECT"	DEFAULT=--
PARM	NLW	TYPE=INTEGER	COUNT=(0:1)	VALID=(1:1025)	DEFAULT=--
PARM	NSW	TYPE=INTEGER	COUNT=(0:1)	VALID=(1:1025)	DEFAULT=--
PARM	SCALE	TYPE=INTEGER	COUNT=(0:2)			DEFAULT=--
PARM	DIVIDE	TYPE=INTEGER	COUNT=(0:1)			DEFAULT=--
PARM	MTF	TYPE=REAL	COUNT=(0:66)			DEFAULT=--
PARM	HIA	TYPE=REAL	COUNT=(0:32)			DEFAULT=--
PARM	HIF	TYPE=REAL	COUNT=(0:32)			DEFAULT=--
PARM	VIA	TYPE=REAL	COUNT=(0:32)			DEFAULT=--
PARM	VIF	TYPE=REAL	COUNT=(0:32)			DEFAULT=--
PARM	SN	TYPE=REAL	COUNT=(0:1)			DEFAULT=--
PARM	RANGE	TYPE=INTEGER	COUNT=(0:2)			DEFAULT=--
PARM	PSF	TYPE=REAL	COUNT=(0:32)			DEFAULT=--
PARM    OFORM   KEYWORD  COUNT=0:1   VALID=(BYTE,HALF)  DEFAULT=--
BODY
FIL2  WTMAX=@WTMAX    PRINT=@PRINT    RECT=@RECT      +
      NLW=@NLW        NSW=@NSW        SCALE=@SCALE    +
      DIVIDE=@DIVIDE  MTF=@MTF        HIA=@HIA      HIF=@HIF     +
      VIA=@VIA        VIF=@VIF        SN=@SN        RANGE=@RANGE +
      PSF=@PSF
IF ($COUNT(INP)=0 OR $COUNT(OUT)=0) RETURN
FILTER   &INP   &OUT   PARMS=filtparms
END-PROC
.TITLE
VICAR PROCEDURE FILTER2
.HELP
PURPOSE
FILTER2 is a VICAR applications procedure which calculates filter
weights from point spread functions, modulation transfer functions,
and optical transfer functions.  It then applies the weights in the
spatial domain.
EXECUTION
The following is the execution statement format for FILTER2:
	FILTER2 IN OUT SIZE PARAMS
where IN, OUT, SIZE and PARAMS are parameters discussed in their
respective parameter sections.
OPERATION:
FILTER2 distinguishes between symmetrical and asymmetrical OTFs in the
following way.  There are two accumulation buffers, one for horizontal
OTFs fed by the MTF, HIA, PSF, and HIF keywords and the other for vertical
OTFs fed by the VIA and VIF keywords.  When both buffers contain OTFs,
they are taken in pairs starting with the first entries to those buffers,
and an asymmetrical two-dimensional OTF is generated from which one set
of weights is created.  These weights are written to the SYS000 along
with NLW, NSW, SCALE, DIVIDE, and the first RANGE data, if any.  The 
program then returns to the buffers for the next pair, etc., until the
buffers are exhausted.  If one buffer is exhausted before the other, 
FILTEr2 will assume that the user wants a symmetrical two-dimensional
OTF.  Thus, as many weight matrices are created as there are OTFs in the 
most populated buffer.  
For one-dimensional weight vectors,  NLW or NSW  must equal unity.
The OTFs must, however, be found in the horizontal buffer.
If there are no input files, the procedure will compute the weights and
print them.
FILTER2 (specifically program FIL2) uses Fast Fourier Transformations
to compute the weights.  for one-dimensional weights, a single transform
is used of length greater than the desired weight matrix but within the 
range 128 to 1024 elements.  For two-dimensional weight matrices, a 64x64
transform is used in all cases regardless of the dimensions.  The trans-
form is constructed from the following equation:
                   M-1 N-1
	WT  = 1/N   E   E  OTF  exp(-2#i[Km/M - ln/N] )
         kl        m=0 n=0   mn

or, in the one-dimensional case
                   N-1
	WT  = 1/N   E  OTF  exp( -2#i ln/N )
          l        n=0    n

where    E means summation and # is pi.

Interpolation of the one-dimensional OTF's into a two-dimensional
surface is performed with three different algorithms.  All interpolation
is performed upon the final OTF, i.e., after any reciprocals have been 
performed.
SYMMETRICAL MODE ALGORITHM
	If the symmetrical mode is used (only one OTF buffer), then
	regardless of the shape of the OTF, the surface is set equal
	to the amplitude of the OTF at the equivalent frequency position
	as measured radially outward from the DC term.
ASYMMETRICAL MODE ALGORITHM, RECT
	If the asymmetrical mode is used (both OTF buffers are populated)
	then the output surface is set equal to the smaller of the two 
	closest one-dimensional OTF values.
ASYMMETRICAL MODE ALGORITHM
	If the asymmetrical mode is used and the OTFs are greater than 
	unity in places, the surface is set equal to a combination of the
	amplitude of both OTFs at the equivalent radial frequency.  Each
	amplitude is weighted based on the square of the distance of the
	point from the horizontal or vertical.
The OTF computed from the raw weights does not necessarily agree with the OTF 
desired.  FILTER2 automatically adjusts the central weight and the DIVIDE
parameter so that the resultant OTF is equal to the input OTF at zero 
frequency.  Adjusting the central weight does not distort the OTF but
merely displaces it up or down by a constant.  The DIVIDE parameter mul-
tiplies the OTF so that there is no amplitude distortion due to scaling
the weights to large integers.  The formulae are:
	new central wt = wt sum - old central wt + OTF(0)*64*64
	DIVIDE = SCALE * 64*64 / abs(largest wt)
where SCALE is the real-to-integer conversion factor, usually set to 32000.
EXAMPLES:
FILTER2 IN OUT NLW=15 NSW=15 MTF=(1.,0.,.4,.2,.1,.5) SN=7.0
WRITTEN BY: j. j. lorre        2 DEC 1974
CURRENT COGNIZANT PROGRAMMER:  charlie avis
REVISION: 11 MAR 1975
          29 FEB 1996 Made portable f.f.Moss
.LEVEL1
.VARI INP
The input image file
.VARI OUT
The output filtered image file
.VARI SIZE
Vicar size field
.VARI SL
size field starting line
.VARI SS
Size field starting sample
.VARI NL
Size field number of lines
.VARI NS
Size field number of samples
.VARI WTMAX
Maximum weight value
Valid: HALF.
.VARI PRINT
Prints the computed OTF
.VARI RECT
Indicates RECT algorithm
.VARI DIVIDE
Scaling paramater upon output
.VARI NLW
Size of weight matrix in lines
.VARI NSW
Size of weight matrix in samples
.VARI SCALE
Defines the linear output scaling
.VARI MTF
Input pairs of amplitude and freq
.VARI HIA
Horizontal amplitude values
.VARI HIF
Horizontal frequency values
.VARI VIA
Vertical amplitude values
.VARI VIF
Vertical frequency values
.VARI SN
Signal-to-noise ratio
.VARI RANGE
Dn interval over which weights apply
.VARI PSF
Input point spread function
.vari OFORM
Output data format.
(Default: input format)
.LEVEL2
.VARI INP
A VICAR labelled image file
.VARI OUT
A file to write the filtered product into
.VARI SIZE
The standard size field defining the area of the input picture that is to
be filtered.
.VARI SL
Starting line of the area to be filtered.
.VARI SS
Starting sample of the area to be filtered
.VARI NL
Number of lines in the area to be filtered.
.VARI NS
Number of samples in the area to be filtered.
.VARI WTMAX
The magnitude of the largest weight to be used in the filtering. Default=32000
.VARI FORMAT
This parameter has one valid keyword value: HALF.
HALF indicates that both input and output files are in halfword format. 
Default is the format that is read from the label.
.VARI PRINT
The OTF as recomputed from the weights is to be printed out. Default is it 
will be printed.
.VARI RECT
Use the RECT algorithm to computed the OTF.  Default is the RECT algorithm 
will not be used.
.VARI DIVIDE
This is used in the final transformation equation.  Each output point 
is scaled by OUT = A + B*dn / DIVIDE
where A and B are defined by SCALE.  The default is that the sum of the 
weights is used.  If this sum is zero, then 1 is used.
.VARI SCALE
This keyword specifies the application of a linear transformation to each
output point .Default is SCALE=(A,B)=(0,1).
.VARI NLW
The extent of the weight matrix in lines. Must be odd.  For 1-dim filter,
1 < NLW < 1025. For 2-dim filter, 1 < NLW < 65.  No default
.VARI NSW
The extent of the weight matrix in samples. Must be odd.  For 1-dim filter,
1 < NSW < 1025. For 2-dim filter, 1 < NSW < 65.  No default
.VARI MTF
N pairs of real numbers may be input representing the (amplitude,frequency)
values of the OTF. The last frequency value must be .5. N must be LE 33.
No default.
.VARI HIA
N real numbers may be input representing the horizontal amplitude values of
the OTF at the frequencies specified by HIF. 2 < N < 33. No default.
.VARI HIF
N real numbers may be input representing the horizontal frequencies cor-
responding to the HIA amplitude values. 2 < N < 33. No default.
.VARI VIA
N real numbers may be input representing the vertical amplitude values of
the OTF at the frequencies specified by VIF. 2 < N < 33. No default.
.VARI VIF
N real numbers may be input representing the vertical frequencies cor-
responding to the VIA amplitude values. 2 < N < 33. No default.
.VARI SN
The signal-to-noise ratio used in the transformation of the OTF into its
Wiener reciprocal.  The reciprocal will correct the image for degradation 
due to the input OTF.  The expression is

 	OTF   =  OTF   / (OTF  **2 + 1/SN**2 )
           out      in      in

SN also serves as the flag to perform the reciprocal.  Default is no reciprocal.
.VARI NORECIPR
Indicates no reciprocal is to be taken.  Default a reciprocal is taken.
.VARI RANGE
The pair of values (a,b) specifies the range of dn over which the filter
weights are to be applied.
.VARI PSF
N real numbers may be input which represent the Point Spread Function or the
right side of the Line Spread Function.  The central weight is first.  The 
OTF will be computed from this.  1 < N < 33.  No default.
.vari OFORM
This specifies the data format of the output.  Valid: BYTE, HALF.

Default is that the output has the same format as the input.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstfilter2.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "THIS IS A TEST OF MODULE FILTER2"
WRITE "FIL2 WILL GENERATE AND PASS PARAMETERS TO PROGRAM"
WRITE "FILTER VIA A THE PROCEDURE FILTER2."
WRITE "THE IMPORTANT RESULT OF EACH CASE IS THE PRINTOUT"
WRITE "IDENTIFIED AS 'TOP LEFT WEIGHT QUADRANT'.  EACH"
WRITE "CASE WILL HAVE ITS IBM EQUIVALENT LISTED HERE."
WRITE ""
WRITE "FIRST, ONLY WEIGHTS BEING GENERATED, NO ACTUAL FILTERING."
FILTER2 NLW=1 NSW=11  +
       PSF=(      +
       1.,.99,.9617,.9175,.8592,.7975,.7292,.6617,   +
       .5967,.5308,.4717,.415,.3633,.315,.27, +
       .2292,.1933,.1592,.1325,.105,.0842  )
WRITE "IBM WEIGHTS:  2216  2388  2550  2673  2752  32000"
WRITE ""
WRITE "FILTER A GEN'D IMAGE   1-DIM FILTER"
GEN OUT=F.INP NL=50 NS=50
GEN OUT=G.FIL NL=50 NS=50
LIST F.INP SIZE=(1,1,10,10)
FILTER2 F.INP G.FIL NLW=1 NSW=11 sn=10. +
       PSF=(      +
       1.,.99,.9617,.9175,.8592,.7975,.7292,.6617,   +
       .5967,.5308,.4717,.415,.3633,.315,.27, +
       .2292,.1933,.1592,.1325,.105,.0842  )
WRITE "IBM WEIGHTS:  1160  3172  5607  7512  8795  -32000"
LIST G.FIL SIZE=(1,1,10,10)
WRITE ""
WRITE "DO A 2-DIM FILTER"
FILTER2 F.INP G.FIL NLW=11 NSW=11 sn=6. +
       PSF=(      +
       1.,.99,.9617,.9175,.8592,.7975,.7292,.6617,   +
       .5967,.5308,.4717,.415,.3633,.315,.27, +
       .2292,.1933,.1592,.1325,.105,.0842  )
WRITE "IBM WEIGHTS:  117   195    388    409    366    352"
WRITE "              195   419    352    405    578    640"
WRITE "              388   352    512    767    668    604"
WRITE "              409   405    767    555    813   1072"
WRITE "              366   578    668    813   1227    298"
WRITE "              352   640    604   1072    298 -32000"
WRITE ""
LIST G.FIL SIZE=(1,1,10,10)
WRITE "NOW ANOTHER 2-DIM FILTER"
FILTER2 F.INP G.FIL SN=6. NLW=11 NSW=11  +
       MTF=(      +
       1.,0.,.99,.025,.9617,.05,.9175,.075                     +
                                  .8592,.1,.7975,.125,.7292,.15,.6617,.175   +
       .5967,.2,.5308,.225,.4717,.25,.415,.275,.3633,.3,.315,.325,.27,.35 +
       .2292,.375,.1933,.4,.1592,.425,.1325,.45,.105,.475,.0842,.5  )
WRITE "IBM WEIGHTS:"
WRITE "              58   -57    -59    87    152    154"
WRITE "             -57   -11    153   -57   -255   -287"
WRITE "             -59   153   -197  -150    361    534"
WRITE "              87   -57   -150   636    -89   -804"
WRITE "             152  -255    361   -89  -2403  -2423"
WRITE "             154  -287    534  -804  -2423  32000"
LIST G.FIL SIZE=(1,1,10,10)
end-proc
$ Return
$!#############################################################################
