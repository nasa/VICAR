$!****************************************************************************
$!
$! Build proc for MIPL module eigen
$! VPACK Version 1.8, Thursday, October 24, 1996, 15:20:40
$!
$! Execute by entering:		$ @eigen
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
$ write sys$output "*** module eigen ***"
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
$ write sys$output "Invalid argument given to eigen.com file -- ", primary
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
$   if F$SEARCH("eigen.imake") .nes. ""
$   then
$      vimake eigen
$      purge eigen.bld
$   else
$      if F$SEARCH("eigen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake eigen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @eigen.bld "STD"
$   else
$      @eigen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create eigen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack eigen.com -
	-i eigen.imake -
	-p eigen.pdf -
	-t tsteigen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create eigen.imake
#define PROCEDURE eigen
#define R2LIB
$ Return
$!#############################################################################
$PDF_File:
$ create eigen.pdf
procedure help=*
PARM INP     TYPE=STRING     COUNT=(0:10)                 DEFAULT=--
PARM OUT     TYPE=STRING     COUNT=(0:10)                 DEFAULT=--
PARM SIZE    TYPE=INTEGER    COUNT=4                      DEFAULT=(1,1,0,0)
PARM SL	     TYPE=INTEGER   				  DEFAULT=1
PARM SS	     TYPE=INTEGER				  DEFAULT=1
PARM NL      TYPE=INTEGER				  DEFAULT=0
PARM NS      TYPE=INTEGER				  DEFAULT=0
PARM LINC    TYPE=INTEGER    COUNT=(0:1)                  DEFAULT=--
PARM INC     TYPE=INTEGER                                 DEFAULT=1
PARM CORR    TYPE=KEYWORD    COUNT=(0:1)   VALID="CORR"   DEFAULT=--
PARM DSTRETCH TYPE=KEYWORD   COUNT=(0:1) VALID="DSTRETCH" DEFAULT=--
PARM DSCALE  TYPE=REAL       COUNT=(0:32)                 DEFAULT=--
PARM EXCLUDE TYPE=REAL	     COUNT=(0:1)   		  DEFAULT=--
PARM MSS     TYPE=INTEGER    COUNT=(0:1)   VALID=(0:32)   DEFAULT=--
PARM USE     TYPE=INTEGER    COUNT=(0:32)  VALID=(0:32)   DEFAULT=--
PARM MSSO    TYPE=INTEGER    COUNT=(0:1)   VALID=(0:32)   DEFAULT=--
PARM OUTPUT  TYPE=INTEGER    COUNT=(0:32)  VALID=(0:32)   DEFAULT=--
PARM SAVE    TYPE=STRING     				  DEFAULT="EMATRIX"
PARM TSAVE   TYPE=STRING     				  DEFAULT="TMATRIX"
PARM AREA    TYPE=INTEGER    COUNT=(0:200)                DEFAULT=--
PARM NVAR    TYPE=INTEGER    COUNT=(0:1)                  DEFAULT=--
PARM DATA    TYPE=REAL       COUNT=(0:500)                DEFAULT=--
PARM SCALE   TYPE=REAL       COUNT=(0,1:96)                DEFAULT=--
PARM PRESET  TYPE=KEYWORD    COUNT=(0:1)  VALID="PRESET"   DEFAULT=--
PARM GAIN    TYPE=REAL       COUNT=(0,1:64)         DEFAULT=--
PARM OFFSET  TYPE=REAL       COUNT=(0,1:64)         DEFAULT=--
PARM SAMPLE  TYPE=REAL                              DEFAULT=10.
PARM PERCENT TYPE=REAL                              DEFAULT=1.
PARM LPERCENT TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM HPERCENT TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM SPREAD  TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM CENTER  TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM FORMAT  TYPE=KEYWORD COUNT=(0:1) VALID=(BYTE,HALF,FULL,REAL) DEFAULT=--
PARM NOAP KEYWORD VALID=NOAP COUNT=0:1 DEFAULT=NOAP
BODY

EIGENVEC @INP @OUT @SIZE SL=@SL SS=@SS NL=@NL NS=@NS LINC=@LINC INC=@INC +
	EXCLUDE=@EXCLUDE MSS=@MSS USE=@USE MSSO=@MSSO OUTPUT=@OUTPUT +
	SAVE=@SAVE TSAVE=@TSAVE AREA=@AREA NVAR=@NVAR DATA=@DATA +
	CORR=@CORR DSTRETCH=@DSTRETCH DSCALE=@DSCALE
IF ($COUNT(OUT)=0) RETURN
XFORM @INP @OUT @SIZE SL=@SL SS=@SS NL=@NL NS=@NS PARMS=@SAVE +
	MSS=@MSS MSSO=@MSSO USE=@USE SCALE=@SCALE SAMPLE=@SAMPLE +
	GAIN=@GAIN OFFSET=@OFFSET PRESET=@PRESET LINC=@LINC AREA=@AREA +
	PERCENT=@PERCENT LPERCENT=@LPERCENT HPERCENT=@HPERCENT +
	SPREAD=@SPREAD CENTER=@CENTER EXCLUDE=@EXCLUDE FORMAT=@FORMAT +
        NOAP=@NOAP
END-PROC
.TITLE
Procedure EIGEN -- Principal component analysis using eigenvectors.
.HELP
PURPOSE:
	EIGEN will compute the principal component (aka the eigenvector or
Karhunen-Loeve) transformation matrix of up to 32 input channels.  
The covariance (or, optionally, correlation) matrix, the transformation
matrix of eigen-vectors, and the eigen-values are printed.  If outputs are
provided, XFORM is executed to perform the transformation.  EIGEN can also
compute the eigen-vectors for non-image data input via the parameters NVAR
and DATA.

NOTE:
	The present version of EIGEN has been modified extensively from the
version used prior to September, 1985.  It has been changed from VICAR1 to
VICAR2, and converted into a procedure.  This procedure executes the program
EIGENVEC, which performs the function of the previous EIGEN program, then,
if output datasets are provided, it executes XFORM to perform the transforma-
tion.
	The meaning of parameters SAVE and TSAVE has been changed completely.
The eigenvector matrix and its transpose are now automatically saved as VICAR2
parameter datasets.  The names of these parameter datasets can be provided by
the user via the SAVE and TSAVE parameters, and by default they are temporary
datasets with the names EMATRIX and TMATRIX, respectively. Note that the
datasets no longer appear in the output dataset field.
	In addition, the color decorrelation stretch algorithm (formerly the
VICAR program DSTRETCH) has been implemented within EIGEN, via the parameter
DSTRETCH.
.PAGE
OPERATION:

	The measure of inter-dimensional correlation in the multi-variate
system is usually defined by the covariance matrix of the multi-variate
data.  The linear transformation that diagonalizes the covariance matrix 
can also be applied to the original data and produce a multi-variate 
system with an inter-dimensional correlation of zero, i.e.; completely
uncorrelated multi-variate data.  The linear transformation that
accomplishes this is the matrix of eigen-vectors or characteristic vectors.

	A common application of this transformation is to reduce the 
dimensionality of a multi-variate system.  The objective is to summarize
most of the variance, or information content, in a system with a 
lessor number of 'artificial' variates, i.e.; principal components. 
Effectively, by uncorrelating the system, we are compressing most of the
information into a system with lower dimensionality. 

	Assume an n-variate system ( n channels of data ).  Let K be the 
n x n covariance matrix of this data and A be the matrix of eigen-vectors
of K.  Associated with each eigen-vector A(j) there is an eigen-value or 
characteristic root, e(j).

	Let A(j) = (a(1j),a(2j),...,a(nj)) be the eigen-vector corresponding 
	to the jth largest eigen-value.

	Let X = (x(1),x(2),...,x(n)) be the n-variate observation 
	( n-dimensional pixel ).

	Then the jth principle component is: v(j) = A(j)  X = a(ij)x(i)

NOTE:  The jth eigen-value is actually the variance in the jth 
principle componemt dimension.  Therefore, the eigen-values are useful 
as a measure the information content that can be expected in the output 
pictures.


RESTRICTIONS:

EIGEN can handle up to 32 channels of data. However, the number of input and
output datasets is limited to 10 datasets. MSS format is needed for greater
than 10 input or output channels. The use of the array processor by
XFORM causes additional constraints in the combinations of inputs, outputs,
and number of samples. Consult the XFORM documentation for details.

The use of the LINC parameter will drastically reduce running time 
and, therefore, is recommended for large images.

The auto-scale mode of XFORM performs erratically if the number of inputs
and outputs is large and the eigen-value for the output being
auto-scaled is small.  There is no precise rule for this, but for byte 
data be skeptical of auto-scaling if:

			(# of outputs)SQRT(# of inputs)
	EIGEN-VALUE <  ---------------------------------
					3




EXAMPLES:

	EIGEN IN (V1,V2,V3,V4) (10,15,350,400) MSS=6 PERCENT=1.0 EXCLUDE=0

    The input dataset contains 6 channels in mss format.  Any pixels with 
    DN = 0 in all channels will be ignored.  XFORM will perform the 
    transformation and allow 1% saturation of the output pictures which 
    are the first 4 principle components.


	EIGEN (A,B,C,D,E) (V1,V2,V3) MSSO=(3,4,5) 'CORR AREA=(101,1,200,200)

    The input data is in separate datasets.  Principal components 3,4,
    and 5 only are to be output.  The correlation matrix is to be used 
    for computing the transformation and only the area specified is 
    used for computing the transformation and the auto-scaling. 

	EIGEN  NVAR=3  DATA=(1.,2.,3.,4.,5.,6.,7.,8.,9.)

    The multi-variate data is input through the parameter field.
    EIGEN will print the covariance matrix and the eigen-vectors for this data.
.page
WRITTEN BY:  Ron Alley,  Sept. 1985

CURRENT COGNIZANT PROGRAMMER:  Ron Alley
Made Portable : Florance Moss, Feb, 1996
                Change default to NOAP to make it work on ALPHA
.LEVEL1
.VARIABLE INP
input data sets
.VARIABLE OUT
output data sets
.VARIABLE SIZE
The standard Vicar size
 field (sl,ss,nl,ns)
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE LINC
Compute transform
from every nth line.
.VARIABLE INC
Compute transform from every
nth line and nth sample
.VARIABLE CORR
Compute eigenvectors from
the correlation matrix
.VARIABLE DSTRETCH
Perform the decorrelation 
stretch transformation, rather
than the eigenvector 
transformation.
.VARIABLE DSCALE
(Used only with DSTRETCH)
Adjust the variance equalization
scaling factors by the specified
values.
.VARIABLE EXCLUDE
Pixels with this DN in all
bands will be excluded from
all computations.
.VARIABLE MSS
The number of input bands,
when the input is in MSS format.
.VARIABLE USE
When input is MSS and not all
bands are to be used, use these
bands.
.VARIABLE MSSO
The number of output components,
if the output is to be in MSS
format.
.VARIABLE OUTPUT
The components to be output.
Default is to output the first
components, in order.
.VARIABLE SAVE
The name for the parameter 
dataset to hold the eigenvector
matrix.
.VARIABLE TSAVE
The name for the parameter
dataset to hold the transpose
of the eigenvector matrix.
.VARIABLE AREA
The subareas to be used to
compute statistics. Up to 50
regions (SL,SS,NL,NS) may be
entered. Default is to use
the entire image.
.VARIABLE NVAR
FOR NON-IMAGE DATA ONLY
The number of dimensions
to the data.
.VARIABLE DATA
FOR NON-IMAGE DATA ONLY
Up to 500 data values for
computing statistics.
.VARIABLE SCALE
(BANDi,GAINi,OFFSETi,...)
Specify the gain and the
offset for individual outputs
.VARIABLE PRESET
GAIN=1, OFFSET=0 for all outputs
.VARIABLE GAIN
(BANDi,GAINi,...)
Specify the gains for 
individual outputs
.VARIABLE OFFSET
(BANDi,OFFSETi,...)
Specify the offsets for 
individual outputs
.VARIABLE SAMPLE
(PARM FOR AUTO-SCALE MODE)
Specify certain percent
of the image to be sampled
.VARIABLE PERCENT
(PARM FOR AUTO-SCALE MODE)
Percent saturation in
output histogram
.VARIABLE LPERCENT
Percent saturation at the
lower end in output histogram
.VARIABLE HPERCENT
(PARM FOR AUTO-SCALE MODE)
Percent saturation at the
higher end in output histogram
.VARIABLE SPREAD
(PARM FOR AUTO-SCALE MODE)
Specify the spread in output
histograms
.VARIABLE CENTER
(PARM FOR AUTO-SCALE MODE)
Specify the center in output
histogram
.VARIABLE FORMAT
Output data format.
Valid: BYTE, HALF, FULL, REAL.
.vari NOAP
 Don't use AP in XFORM step.
.LEVEL2
.VARIABLE INP
Input datasets containing registered images, or a single input dataset 
in MSS format.
.VARIABLE OUT
Output datasets containing transformed images, or a single output
dataset in MSS format.
.VARIABLE SIZE
The standard Vicar size field ( starting_line, starting_sample, 
number_of_lines, number_of_samples).
.VARIABLE LINC
Statistics are gathered using only every n'th line of the image. If
the auto-scaling option of XFORM is invoked, every n'th line is used to
determine the scaling parameters.
.VARIABLE INC
Statistics are gathered using only every n'th line and n'th sample of the
image. If different line and sample increments are desired, use INC to
specify the sample increment and use LINC to override the INC line increment
value.
.VARIABLE CORR
The correlation matrix is used for computing eigenvectors instead of the
covariance matrix.
.VARIABLE DSTRETCH
When the DSTRETCH keyword is used, the output will be a set of images that
have been modified by a color decorrelation stretch transformation, rather
than the eigenvector transformation.
.VARIABLE DSCALE
This parameter has effect only when used with the DSTRETCH parameter. The
DSTRETCH operation consists of a rotation into principal component space,
rescaling to cause variance equalization among components, and the reverse
rotation out of principal component space. DSCALE applies an additional
scaling at the variance equalization stage. This can be used to suppress
components known to be noisy, at the expense of re-introducing some 
correlation among bands.
.VARIABLE EXCLUDE
A pixel is excluded from use in determining the transformation if the 
DN is equal to 'EXCLUDE' in each channel. If the auto-scaling option of 
XFORM is used, these pixels will also be excluded from the computation of
scaling parameters.
.VARIABLE MSS
Denotes the input data is in MSS format and contains 'MSS' number of 
channels of data.
.VARIABLE USE
If the input is in MSS format, use only the listed channels. The default
is that all channels are used.
.VARIABLE MSSO
Denotes that that output is to be in MSS format, and will contain 'MSSO'
components (bands) of data.
.VARIABLE OUTPUT
The output will consist of the specified components. The default is to
output the first n components, where n is the number of output datasets,
or for MSS format output, the value of MSSO.
.VARIABLE SAVE
The eigenvector transformation matrix will be saved as a VICAR2 parameter
dataset with the name denoted by the SAVE parameter.
.VARIABLE TSAVE
The transpose of the eigenvector transformation matrix will be saved as a
VICAR2 parameter dataset with the name denoted by the TSAVE parameter. This
matrix produces the rotation necessary to return to the original coordinate
system.
.VARIABLE AREA
Sets of (Starting_line, Starting_sample, Number_of_lines, Number_of_samples)
are given to define subareas used to generate the image statistics. If
auto-scaling is used in XFORM, only the first subarea listed will be used
to gather the auto-scaling statistics. The default is that the entire image 
is sampled.
.VARIABLE NVAR
Integer specifying the dimensionality of the data.  This parameter is 
only used when multi-variate data is input through the parameter field
(DATA).
.VARIABLE DATA
1 to 500 data values may be input through the parameter field. That is, the
number of case times NVAR must be no greater than 500. No input data sets 
are used in this mode.  The covariance matrix and the eigenvectors for this 
set of data are computed and printed.
.VARIABLE SCALE
SCALE=(I1,P1,Q1,I2,P2,Q2,...)   The output image specified by band number I 
is scaled by offset P and gain Q. The outputs not specified by I will be 
defaulted to the preset value.
.VARIABLE PRESET
An offset of P = 0.5 (integer data) or 0 (real data) and gain Q = 1 is
applied to all output bands.
.VARIABLE GAIN
    GAIN=Q                   For all no output bands
 or GAIN=(I1,Q1,I2,Q2,...)   Each output band specified by I is scaled 
			     by  gain Q. The output bands not specified by 
			     I will be defaulted to the PRESET value.
.VARIABLE OFFSET
    OFFSET=P                 For all no output bands
 or OFFSET=(I1,P1,I2,P2,...) Each output band specified  by I is scaled by
			     offset P. The output bands not specified by I 
			     will be defaulted to the PRESET value.
.VARIABLE SAMPLE
In the auto-scale phase of XFORM, SAMP percent of the picture is to be
sampled in the line direction only; i.e., LINC=100/sample. (default is 10%,
if AREA and LINC are not specified.)
.VARIABLE PERCENT
This denotes the desired percent saturation in the output images,
 one-half at each end. (default=1.0)
.VARIABLE LPERCENT
This denotes the desired percent saturation in the output images at the
dark end. (default=0.5)
.VARIABLE HPERCENT
This denotes the desired percent saturation in the output images at the
bright end. (default=0.5)
.VARIABLE SPREAD
The desired range of the output histograms. (default=full range of the 
data type)
.VARIABLE CENTER
The desired center of the output histograms. (default=spread/2 for byte data, 
=0.0 otherwise.)
.VARIABLE FORMAT
 Valid keyword values: BYTE, HALF, FULL, REAL.
 This specifies the output data format. 
.vari NOAP
This keyword is only meaningful if the proc invokes XFORM, i.e., if an
output file has been specified.

Specifying NOAP indicates that the Array Processor program XFORMAP should
not be used by the XFORM proc.  Instead, program XFORMEM will be invoked.

Default is to invoke XFORMAP if the AP is available and if NS is a power
of 2 between 2 and 16384; otherwise to use XFORMEM.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsteigen.pdf
procedure
  refgbl $echo
  RefGbl $SysChar
body
  Local Path TYPE=STRING
  IF ($SysChar(1)="VAX_VMS")
    Let Path="wms_test_work:[testdata.mipl.vgr]"
  Else
    Let Path="/project/test_work/testdata/mipl/vgr/"
  End-If

  let $echo="yes"
WRITE " this is a test for procedure eigen"
WRITE " the input byte data (uvb,viob,blub) and halfword data (uvh,vioh,bluh)" 
WRITE " are copied from /project/it/testdata/mipl/vgr for unix platforms, then"
WRITE " ftp the data to ALPHA using binary option for vms testing"
WRITE " test byte input"
WRITE " test parm inc"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) inc=10
WRITE " test parm linc"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) linc=20
WRITE " test parm area and xform output"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) out=(x,y,z) +
 area=(20,1,10,20,40,11,10,20)
WRITE " test parm exclud"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) exclud=11 +
 area=(20,1,10,20,40,11,10,20)
WRITE " test parm corr"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) 'corr inc=50
WRITE " test parm size"
eigen inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) size=(11,11,20,20)
WRITE " test halfword data"
WRITE " test parm inc"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) 'half inc=100
WRITE " test parm linc"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) 'half linc=20
WRITE " test parm area"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) 'half +
      area=(400,1,10,10)
WRITE " test parm exclud"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) 'half exclud=12 +
 area=(101,11,10,10,201,11,10,10)
WRITE " test parm corr"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) 'half inc=50 'corr
WRITE " test parm size and xform mode (parm 'out')"
eigen inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) out=(x,y,z) 'half +
 size=(100,101,30,100)

WRITE " test parm data"
eigen nvar=3 data=(1.,33.,179.,991.,127.3,0.,341.0,987.6,99.3,111.,123.,+
 50.,333.,15.,999.,1234.,378.,111.1,255.,678.,1911.)
! 
! test mss mode
mss inp=(&"Path"uvh.img, &"Path"vioh.img, &"Path"bluh.img) out=mssh
mss inp=(&"Path"uvb.img, &"Path"viob.img, &"Path"blub.img) out=mssb
WRITE " test mss mode of byte data"
WRITE " test parm inc"
eigen  mssb mss=3 inc=100
WRITE " test parm linc"
eigen  mssb mss=3 linc=20
WRITE " test parm area"
eigen  mssb mss=3 area=(101,1,30,30)
WRITE " test parm use"
eigen  mssb mss=3 use=(1,3) linc=100
WRITE " test parm corr and area"
eigen  mssb mss=3 'corr area=(101,1,30,30,201,11,30,30)
WRITE " test parm exclud and size"
eigen  mssb mss=3 exclud=8 size=(11,11,30,30)
WRITE " test mss mode of halfword data"
WRITE " test parm inc"
eigen  mssh mss=3 'half inc=100
WRITE " test parm linc"
eigen  mssh mss=3 'half linc=20 
WRITE " test parm area"
eigen  mssh mss=3 'half area=(101,1,30,30)
WRITE " test parm use"
eigen  mssh mss=3 'half use=(1,3) linc=100
WRITE " test parm corr and area"
eigen  mssh mss=3 'half 'corr area=(101,1,30,30,201,11,30,30)
WRITE " test parms byte and size and xform mode"
eigen  mssh (x,y) mss=3 'byte size=(11,11,30,30)
end-proc
$ Return
$!#############################################################################
