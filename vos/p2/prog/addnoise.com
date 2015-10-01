$!****************************************************************************
$!
$! Build proc for MIPL module addnoise
$! VPACK Version 1.9, Wednesday, October 17, 2012, 15:05:44
$!
$! Execute by entering:		$ @addnoise
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
$ write sys$output "*** module addnoise ***"
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
$ write sys$output "Invalid argument given to addnoise.com file -- ", primary
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
$   if F$SEARCH("addnoise.imake") .nes. ""
$   then
$      vimake addnoise
$      purge addnoise.bld
$   else
$      if F$SEARCH("addnoise.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake addnoise
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @addnoise.bld "STD"
$   else
$      @addnoise.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create addnoise.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack addnoise.com -mixed -
	-s addnoise.f -
	-i addnoise.imake -
	-p addnoise.pdf -
	-t tstaddnoise.pdf tstaddnoise.log_rjb tstaddnoise.log_solos -
	   tstaddnoise.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create addnoise.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44
c
c
	implicit none
	real*4	inbuf(32768)
	real*4	sigma,x,y,u,ranx,rany,gain,pixel_rate
	integer*4 nl,ns,cnt,status,line,samp,i,j,bit_hit
	integer*4 inunit,outunit,seed,bits_per_pixel,rate
	integer*8 dseed
	logical*4 add_noise,shot_noise,bit_noise
	character*8 format
	character*80 msg
c
	pixel_rate = 0.0
	call ifmessage ('ADDNOISE - 18-JUN-2012')	
c set parameters
	call xvp('GAIN',gain,cnt)
	if(cnt.eq.0)shot_noise=.false.
	if(cnt.eq.1)then
	  shot_noise=.true.
	  gain=sqrt(1.0/gain)
	endif
	call xvp('RATE',rate,cnt)
	if(cnt.eq.0)bit_noise=.false.
	if(cnt.eq.1)bit_noise=.true.	
	call xvp('SIGMA',sigma,cnt)
	if(cnt.eq.0)add_noise=.false.
	if(cnt.eq.1)add_noise=.true.
	call xvp('bits',bits_per_pixel,cnt)	
	call xvp('SEED',seed,cnt)
	dseed = seed			!convert to integer*8 (long int in randgen)
	if (cnt .eq. 0)  then
		call get_seconds(dseed)
	end if
	if(add_noise.or.shot_noise.or.bit_noise)then
	else
	  call xvmessage('??E - Must select a noise model',' ')
	  call abend
	endif
	
	if(bit_noise)then
	  pixel_rate=float(rate)/float(bits_per_pixel) ! pixel hit rate
	endif

c open input
	call xvunit(inunit,'INP',1,status,' ')
	call xvopen(inunit,status, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','READ',  'U_FORMAT','REAL',' ')
        call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
        write (msg,10100) format
10100 format ('Input format is ',a8)
	call xvmessage(msg,' ')
c open output
	call xvunit(outunit,'OUT',1,status,' ')
	call xvopen(outunit,status, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','WRITE',  'U_FORMAT','REAL',' ')

c introduce noise
        cnt=0
	do line = 1,nl
	    call xvread(inunit,inbuf,status,' ')
	    do samp = 1,ns
	    
	      if (shot_noise)then
		call rangen(dseed,ranx)
		call rangen(dseed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		x=max(inbuf(samp),0.0)
		inbuf(samp) = gain*sqrt(x)*u*0.7979 + inbuf(samp)
              endif
              	    
	      if (add_noise)then
		call rangen(dseed,ranx)
		call rangen(dseed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		inbuf(samp) = sigma*u + inbuf(samp)
              endif
              	    
	      if (bit_noise)then
	        call rangen(dseed,ranx)
	        if (ranx*pixel_rate.lt.1.0)then ! probability of pixel hit
	          call rangen(dseed,ranx)
	          bit_hit=nint(ranx*bits_per_pixel+0.5)-1 ! the bit # hit
	          i=nint(inbuf(samp))
	          call bits(i,bit_hit,bit_hit,j) ! j is the bit value
	          if (j.eq.0)then
	            inbuf(samp)=i+2**bit_hit
	          else
	            inbuf(samp)=i-2**bit_hit
	          endif
	          cnt=cnt+1
	        endif
              endif
                            
	    enddo
	    if (format.eq.'BYTE')then
	      do samp=1,ns
	        if (inbuf(samp).lt.0.0)inbuf(samp)=0.0
	        if (inbuf(samp).gt.255.0)inbuf(samp)=255.0
	      enddo
	    endif
	    call xvwrit(outunit,inbuf,status,' ')
	enddo
	if (bit_noise) then
	    write (msg,10200) cnt
10200 format (i8,' pixels hit by bit errors')
	call xvmessage (msg,' ')
	endif
	call xvclose(inunit,status,' ')
	call xvclose(outunit,status,' ')
	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create addnoise.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM addnoise

   To Create the build file give the command:

		$ vimake addnoise			(VMS)
   or
		% vimake addnoise			(Unix)


************************************************************************/


#define PROGRAM	addnoise

#define MODULE_LIST addnoise.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create addnoise.pdf
PROCESS HELP=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIGMA TYPE=REAL DEFAULT=-- COUNT=0:1
PARM GAIN TYPE=REAL DEFAULT=-- COUNT=0:1
PARM RATE TYPE=INTEGER DEFAULT=-- COUNT=0:1
PARM BITS TYPE=INTEGER DEFAULT=8 COUNT=0:1
PARM SEED TYPE=INTEGER DEFAULT=-- COUNT=0:1
END-PROC

.TITLE
Create noise in images via a noise model.

.HELP
PURPOSE:

Addnoise creates three types of noise:
1. Additive gaussian noise.
2. Shot noise (multiplicative ccd type noise).
3. Bit error noise.

EXECUTION:

addnoise inp=a out=b sigma=10.            Additive gaussian noise 
addnoise inp=a out=b gain=30.             ccd shot noise
addnoise inp=a out=b rate=100 bits=8      bit error noise

The SEED parameter is provided so the same random patterns can be generated.
If it is defaulted the seed for the random number generator comes from the
system time.

Note, more than one noise can be simulated at one time. If you specify "sigma"
and "gain" and "rate" for example, you'll get all three. The ordering of
these is:
first  shot noise
second additive noise
third  bit noise

METHOD:

Additive noise:
dn_out= dn_in + sigma

Shot noise:
dn_out = sqrt(1/gain) * sqrt(dn_in) * x + dn_in
where x is a random gaussian distribution with a mean deviation of 1.0

Bit error noise:
A pixel rate is computed from the bit rate.
A random number selects the pixels to hit.
Another random number selects the bit to flip.

LIMITATIONS:

    1. The maximum ns of image is 32768

HISTORY:

Original Programmer :	Jean Lorre  11-1-1998

6-18-2012 - Ray Bambery - gfortran 4.6.3 revealed that
                    rangen parameters are (long,float) so
                    created integer*8 dseed replacement for
                    integer*4 seed. get_seconds parameter 
                    is also integer*8, converted all internal
                    write (*,*) to xvmessage

.LEVEL1
.VARIABLE IN
Input image 
.VARIABLE OUT
Output image
.VARIABLE SIGMA
The standard deviation
of the distribution
for additive noise.
.VARIABLE GAIN
Ccd gain constant
in electrons/dn.
.VARIABLE RATE
Bit error rate.
.VARIABLE BITS
Bits per pixel.
.VARIABLE SEED
The starting seed for the
random number generator

.LEVEL2
.VARIABLE IN
Input image
 
.VARIABLE OUT
Output image

.VARIABLE SIGMA
Triggers additive noise to be created.
The standard deviation of the distribution for additive noise.

.VARIABLE GAIN
Triggers shot noise to be created.
Ccd gain constant in electrons/dn.
Typical values for galileo were gain=30 in gain state 4 and gain=170 in
gain state 2.

.VARIABLE RATE
Triggers bit error noise to be created.
For example a bit error rate of 100 means one bit in 100 is hit on the average.

Note, the algorithm does not permit a pixel to be hit more than once.
Note, see the BITS keyword.

.VARIABLE BITS
Bits per pixel. Defaults to 8. Only used in the bit error "RATE" mode.
This is the lower number of bits which can be changed per word. 

Note: bits does not default to the format type. If you have HALF data but
only 12 bits/pixel in a 16 bit field you must set bits=12 not to 16.
Avoid changing the sign bit by setting bits to one less than the word length.

.VARIABLE SEED
The starting seed for the random number generator. Defaults to time of day.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaddnoise.pdf
procedure
refgbl $echo

! Jun 19, 2012 - RJB
! TEST SCRIPT FOR ADDNOISE
! Vicar Programs:
!   gen hist xvd                   
!
! parameters:
!   <none>
!
! Requires NO external test data: 
!
! 17Oct2012: added SEED parameter to each invocation of addnoise so
! that output is constant for testing;  added clean-up of files at end.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
body
let _onfail="stop"
let $echo="yes"
!
gen out=a.img nl=100 ns=100 ival=100 linc=0 sinc=0
hist a.img
let $echo="no"
xvd a.img
let $echo="yes"
addnoise inp=a.img out=b1.img sigma=10.0 seed=8331591
hist b1.img
let $echo="no"
xvd b1.img
let $echo="yes"
addnoise inp=a.img out=b2.img gain=30.0 seed=8331592
hist b2.img
let $echo="no"
xvd b2.img
let $echo="yes"
addnoise inp=a.img out=b3.img rate=100 seed=8331593
hist b3.img
let $echo="no"
xvd b3.img
let $echo="yes"
!
addnoise inp=a.img out=c.img sigma=10.0 gain=30 rate=100 seed=8331594
hist c.img
let $echo="no"
xvd c.img

! clean up
ush rm a.img
ush rm b1.img
ush rm b2.img
ush rm b3.img
ush rm c.img

end-proc
$!-----------------------------------------------------------------------------
$ create tstaddnoise.log_rjb
tstaddnoise
gen out=a.img nl=100 ns=100 ival=100 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist a.img
Beginning VICAR task hist
*** HIST version Jun 24, 2011 (64-bit) -  RJB ***

Bin Width =        1.0
        100*   10000   **************************************************  1

AVERAGE GRAY LEVEL=100.0000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=     1000
MIN. DN=       100
MAX. DN=       100

let $echo="no"
addnoise inp=a.img out=b1.img sigma=10.0
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012 (64-bit) - rjb
Input format is BYTE
RANGEN::: Random Seed value of 1340914147 adjusted to 15514083
hist b1.img
Beginning VICAR task hist
*** HIST version Jun 24, 2011 (64-bit) -  RJB ***

Bin Width =        1.0
         58*       1
         62*       2
         64*       1
         65        1
         67*       2
         68        4
         69        3
         70        6
         71       10   *
         72       11   *
         73       10   *
         74        7
         75       23   **
         76       30   ***
         77       28   ***
         78       36   ****
         79       44   *****
         80       63   *******
         81       81   *********
         82       84   **********
         83      108   ************
         84      110   *************
         85      139   ****************
         86      166   *******************
         87      184   *********************
         88      179   *********************
         89      208   ************************
         90      251   *****************************
         91      279   *********************************
         92      298   ***********************************
         93      337   ****************************************
         94      365   *******************************************
         95      329   ***************************************
         96      420   **************************************************  2
         97      386   *********************************************
         98      386   *********************************************
         99      408   ************************************************
        100      396   ***********************************************
        101      421   **************************************************  1
        102      374   ********************************************
        103      374   ********************************************
        104      340   ****************************************
        105      330   ***************************************
        106      317   *************************************
        107      288   **********************************
        108      263   *******************************
        109      281   *********************************
        110      233   ***************************
        111      197   ***********************
        112      182   *********************
        113      176   ********************
        114      144   *****************
        115      115   *************
        116      116   *************
        117       95   ***********
        118       55   ******
        119       64   *******
        120       45   *****
        121       54   ******
        122       35   ****
        123       18   **
        124       17   **
        125       20   **
        126       10   *
        127        6
        128        7
        129        5
        130        5
        131        5
        132        3
        133        5
        135*       2
        136        1
        138*       1

AVERAGE GRAY LEVEL=99.57160
STANDARD DEVIATION=10.04829
NUMBER ELEMENTS=     1000
MIN. DN=        58
MAX. DN=       138

let $echo="no"
addnoise inp=a.img out=b2.img gain=30.0
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012 (64-bit) - rjb
Input format is BYTE
RANGEN::: Random Seed value of 1340914147 adjusted to 15514083
hist b2.img
Beginning VICAR task hist
*** HIST version Jun 24, 2011 (64-bit) -  RJB ***

Bin Width =        1.0
         94*       5
         95       31
         96      156   ***
         97      652   ************
         98     1570   *******************************
         99     2586   **************************************************  1
        100     2511   **************************************************  2
        101     1621   ********************************
        102      655   *************
        103      177   ***
        104       32
        105        4

AVERAGE GRAY LEVEL=99.51190
STANDARD DEVIATION=1.491596
NUMBER ELEMENTS=     1000
MIN. DN=        94
MAX. DN=       105

let $echo="no"
addnoise inp=a.img out=b3.img rate=100
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012 (64-bit) - rjb
Input format is BYTE
RANGEN::: Random Seed value of 1340914147 adjusted to 15514083
     816 pixels hit by bit errors
hist b3.img
Beginning VICAR task hist
*** HIST version Jun 24, 2011 (64-bit) -  RJB ***

Bin Width =        1.0
         36*      98   *****************************************
         68*      98   *****************************************
         96*      97   *****************************************
        100*    9184   **************************************************  1
        101      102   *******************************************
        102       79   *********************************
        108*     107   *********************************************
        116*     117   *************************************************
        228*     118   **************************************************  2

AVERAGE GRAY LEVEL=100.8296
STANDARD DEVIATION=15.70656
NUMBER ELEMENTS=     1000
MIN. DN=        36
MAX. DN=       228

let $echo="no"
addnoise inp=a.img out=c.img sigma=10.0 gain=30 rate=100
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012 (64-bit) - rjb
Input format is BYTE
RANGEN::: Random Seed value of 1340914147 adjusted to 15514083
     826 pixels hit by bit errors
hist c.img
Beginning VICAR task hist
*** HIST version Jun 24, 2011 (64-bit) -  RJB ***

Bin Width =        1.0
         16*       1
         17        1
         19*       1
         20        2
         21        1
         22        1
         23        1
         24        4
         25        1
         26        5
         27        3
         28        3
         29        5
         30        7
         31        5
         32        2
         33        1
         34        4
         35        5
         36        5
         37        4
         38        4
         39        5
         40        5
         41        3
         42        2
         43        5
         44        1
         45        2
         46        1
         47        2
         48        2
         49        3
         50        1
         52*       4
         53        1
         56*       1
         57        2
         59*       1
         63*       1
         64        5
         65        8   *
         66        7
         67       14   *
         68        6
         69       11   *
         70       15   *
         71       11   *
         72       15   *
         73       17   **
         74       25   ***
         75       30   ***
         76       26   ***
         77       34   ****
         78       44   *****
         79       54   ******
         80       52   ******
         81       67   ********
         82       80   **********
         83       95   ************
         84      131   ****************
         85      154   *******************
         86      157   ********************
         87      161   ********************
         88      210   **************************
         89      212   ***************************
         90      242   *******************************
         91      244   *******************************
         92      277   ***********************************
         93      272   **********************************
         94      357   *********************************************
         95      376   ************************************************
         96      342   *******************************************
         97      390   **************************************************  2
         98      372   ***********************************************
         99      356   *********************************************
        100      401   **************************************************  1
        101      352   *********************************************
        102      377   ************************************************
        103      349   ********************************************
        104      322   *****************************************
        105      329   ******************************************
        106      291   *************************************
        107      309   ***************************************
        108      260   *********************************
        109      242   *******************************
        110      201   *************************
        111      202   *************************
        112      175   **********************
        113      173   **********************
        114      143   ******************
        115      131   ****************
        116      100   ************
        117      116   **************
        118       81   **********
        119       68   ********
        120       61   *******
        121       49   ******
        122       35   ****
        123       24   ***
        124       27   ***
        125       28   ***
        126       17   **
        127       20   **
        128        8   *
        129        4
        130        3
        131        4
        132        6
        135*       1
        136        2
        199*       1
        203*       1
        205*       1
        211*       1
        212        3
        213        1
        214        3
        215        3
        216        4
        217        2
        218        2
        219        4
        220        1
        221        2
        222        4
        223        1
        224        2
        225        6
        226        4
        227        8   *
        228        5
        229        6
        230        5
        231        3
        232        2
        233        3
        234        3
        235        5
        236        5
        237        1
        238        3
        239        2
        242*       3
        243        2
        244        3
        245        3
        246        1
        249*       1
        251*       2

AVERAGE GRAY LEVEL=100.2761
STANDARD DEVIATION=18.46545
NUMBER ELEMENTS=     1000
MIN. DN=        16
MAX. DN=       251

let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstaddnoise.log_solos
tstaddnoise
gen out=a.img nl=100 ns=100 ival=100 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist a.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
        100*   10000   **************************************************  1

AVERAGE GRAY LEVEL=100.0000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=     1000
MIN. DN=       100
MAX. DN=       100

let $echo="no"
addnoise inp=a.img out=b1.img sigma=10.0 seed=8331591
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
hist b1.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         59*       1
         60        1
         64*       2
         66*       1
         68*       4
         69        1
         70        2
         71        4
         72       16   *
         73       12   *
         74       12   *
         75       23   **
         76       18   **
         77       28   ***
         78       42   *****
         79       36   ****
         80       48   *****
         81       90   **********
         82       89   **********
         83      102   ************
         84      128   ***************
         85      137   ****************
         86      145   *****************
         87      191   ***********************
         88      207   ************************
         89      221   **************************
         90      251   ******************************
         91      283   **********************************
         92      295   ***********************************
         93      307   ************************************
         94      332   ****************************************
         95      365   *******************************************
         96      414   *************************************************
         97      391   ***********************************************
         98      416   **************************************************  1
         99      415   **************************************************  2
        100      373   ********************************************
        101      406   ************************************************
        102      383   **********************************************
        103      397   ***********************************************
        104      351   ******************************************
        105      375   *********************************************
        106      328   ***************************************
        107      293   ***********************************
        108      294   ***********************************
        109      243   *****************************
        110      240   ****************************
        111      199   ***********************
        112      172   ********************
        113      148   *****************
        114      129   ***************
        115      133   ****************
        116       80   *********
        117       84   **********
        118       75   *********
        119       53   ******
        120       43   *****
        121       31   ***
        122       34   ****
        123       27   ***
        124       19   **
        125       18   **
        126       14   *
        127        6
        128        4
        129        2
        130        5
        131        3
        132        2
        134*       2
        135        3
        136        1

AVERAGE GRAY LEVEL=99.47080
STANDARD DEVIATION=9.855453
NUMBER ELEMENTS=     1000
MIN. DN=        59
MAX. DN=       136

let $echo="no"
addnoise inp=a.img out=b2.img gain=30.0 seed=8331592
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
hist b2.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         94*       4
         95       20
         96      159   ***
         97      651   ************
         98     1594   *******************************
         99     2602   **************************************************  1
        100     2568   **************************************************  2
        101     1592   ******************************
        102      625   ************
        103      159   ***
        104       21
        105        5

AVERAGE GRAY LEVEL=99.49250
STANDARD DEVIATION=1.464016
NUMBER ELEMENTS=     1000
MIN. DN=        94
MAX. DN=       105

let $echo="no"
addnoise inp=a.img out=b3.img rate=100 seed=8331593
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
     753 pixels hit by bit errors
hist b3.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         36*      87   ****************************************
         68*      91   ******************************************
         96*      90   *****************************************
        100*    9247   **************************************************  1
        101      108   **************************************************  2
        102       93   *******************************************
        108*      94   *******************************************
        116*      94   *******************************************
        228*      96   ********************************************

AVERAGE GRAY LEVEL=100.5998
STANDARD DEVIATION=14.32062
NUMBER ELEMENTS=     1000
MIN. DN=        36
MAX. DN=       228

let $echo="no"
addnoise inp=a.img out=c.img sigma=10.0 gain=30 rate=100 seed=8331594
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
     809 pixels hit by bit errors
hist c.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
          7*       1
         17*       1
         18        1
         19        1
         22*       2
         23        4
         24        1
         25        2
         26        2
         27        2
         28        3
         29        6
         30        2
         31        7
         32        1
         33        3
         34        3
         35        2
         36        6
         37        4
         38        1
         39        7
         40        8   *
         41        3
         43*       1
         44        4
         45        1
         46        1
         47        3
         48        1
         49        2
         50        1
         51        2
         52        2
         53        1
         54        1
         55        2
         59*       1
         61*       1
         63*       1
         64        6
         65        8   *
         66        4
         67        8   *
         68        3
         69       14   *
         70       12   *
         71       11   *
         72       14   *
         73       15   *
         74       22   **
         75       28   ***
         76       40   *****
         77       29   ***
         78       50   ******
         79       50   ******
         80       54   ******
         81       69   ********
         82       90   ***********
         83       86   ***********
         84      111   **************
         85      148   *******************
         86      153   *******************
         87      177   **********************
         88      192   ************************
         89      245   *******************************
         90      222   ****************************
         91      281   ************************************
         92      317   ****************************************
         93      280   ***********************************
         94      315   ****************************************
         95      330   ******************************************
         96      367   ***********************************************
         97      346   ********************************************
         98      388   *************************************************
         99      389   **************************************************  2
        100      372   ***********************************************
        101      391   **************************************************  1
        102      340   *******************************************
        103      380   ************************************************
        104      319   *****************************************
        105      301   **************************************
        106      316   ****************************************
        107      281   ************************************
        108      255   ********************************
        109      250   ********************************
        110      258   *********************************
        111      206   **************************
        112      191   ************************
        113      146   ******************
        114      160   ********************
        115      127   ****************
        116      110   **************
        117       88   ***********
        118       76   *********
        119       53   ******
        120       57   *******
        121       49   ******
        122       42   *****
        123       37   ****
        124       30   ***
        125       30   ***
        126       15   *
        127       11   *
        128        5
        129        5
        130        7
        131        5
        132        1
        133        3
        134        1
        135        1
        136        1
        139*       2
        140        1
        203*       1
        205*       1
        208*       1
        209        3
        211*       1
        212        1
        213        1
        214        2
        215        2
        218*       2
        219        3
        220        4
        221        3
        222        3
        223        8   *
        224        1
        225        2
        226        2
        227        7
        228        3
        229        5
        230        3
        231        4
        232        3
        233        4
        234        5
        235        3
        236        2
        237        5
        238        1
        239        3
        240        2
        241        6
        243*       3
        245*       2
        250*       1
        253*       1
        254        1

AVERAGE GRAY LEVEL=100.3028
STANDARD DEVIATION=18.04219
NUMBER ELEMENTS=     1000
MIN. DN=         7
MAX. DN=       254

let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstaddnoise.log_linux
tstaddnoise
gen out=a.img nl=100 ns=100 ival=100 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist a.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
        100*   10000   **************************************************  1

AVERAGE GRAY LEVEL=100.0000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=     1000
MIN. DN=       100
MAX. DN=       100

let $echo="no"
addnoise inp=a.img out=b1.img sigma=10.0 seed=8331591
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
hist b1.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         62*       1
         63        1
         65*       1
         66        1
         67        1
         68        3
         69        2
         70        3
         71        9   *
         72        8
         73       12   *
         74       12   *
         75       19   **
         76       24   **
         77       41   ****
         78       41   ****
         79       37   ****
         80       51   ******
         81       67   ********
         82       80   *********
         83       97   ***********
         84      134   ****************
         85      145   *****************
         86      164   *******************
         87      169   ********************
         88      212   *************************
         89      251   ******************************
         90      272   ********************************
         91      262   *******************************
         92      272   ********************************
         93      314   **************************************
         94      371   ********************************************
         95      338   ****************************************
         96      390   ***********************************************
         97      402   ************************************************
         98      377   *********************************************
         99      420   **************************************************  1
        100      413   **************************************************  2
        101      382   **********************************************
        102      394   ***********************************************
        103      410   *************************************************
        104      399   ************************************************
        105      336   ****************************************
        106      318   **************************************
        107      289   **********************************
        108      272   ********************************
        109      261   *******************************
        110      204   ************************
        111      195   ***********************
        112      183   **********************
        113      173   ********************
        114      126   ***************
        115       98   ***********
        116      120   **************
        117       82   *********
        118       71   ********
        119       48   *****
        120       56   ******
        121       26   ***
        122       32   ***
        123       25   ***
        124       21   **
        125       12   *
        126       10   *
        127        9   *
        128       12   *
        129        6
        130        3
        131        1
        132        5
        134*       2
        138*       2

AVERAGE GRAY LEVEL=99.46610
STANDARD DEVIATION=9.883615
NUMBER ELEMENTS=     1000
MIN. DN=        62
MAX. DN=       138

let $echo="no"
addnoise inp=a.img out=b2.img gain=30.0 seed=8331592
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
hist b2.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         94*       5
         95       27
         96      146   **
         97      636   ************
         98     1568   ******************************
         99     2606   **************************************************  2
        100     2610   **************************************************  1
        101     1563   *****************************
        102      653   ************
        103      152   **
        104       29
        105        5

AVERAGE GRAY LEVEL=99.50670
STANDARD DEVIATION=1.466000
NUMBER ELEMENTS=     1000
MIN. DN=        94
MAX. DN=       105

let $echo="no"
addnoise inp=a.img out=b3.img rate=100 seed=8331593
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
     815 pixels hit by bit errors
hist b3.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         36*     121   **************************************************  2
         68*     108   ********************************************
         96*      94   **************************************
        100*    9185   **************************************************  1
        101       91   *************************************
        102       86   ***********************************
        108*     104   ******************************************
        116*     103   ******************************************
        228*     108   ********************************************

AVERAGE GRAY LEVEL=100.4991
STANDARD DEVIATION=15.51822
NUMBER ELEMENTS=     1000
MIN. DN=        36
MAX. DN=       228

let $echo="no"
addnoise inp=a.img out=c.img sigma=10.0 gain=30 rate=100 seed=8331594
Beginning VICAR task addnoise
ADDNOISE - 18-JUN-2012
Input format is BYTE
     815 pixels hit by bit errors
hist c.img
Beginning VICAR task hist
*** HIST version 13 Jul 2012 ***

Bin Width =        1.0
         18*       1
         19        1
         20        1
         22*       3
         23        4
         24        2
         25        3
         26        3
         27        4
         28        3
         29        3
         30        7
         31        4
         32        4
         33        4
         34        3
         35        7
         36        2
         37        2
         38        4
         39        5
         40        1
         41        5
         42        3
         43        2
         44        2
         45        2
         46        1
         47        2
         48        3
         49        2
         50        4
         51        3
         52        2
         53        2
         58*       1
         59        1
         60        2
         61        1
         62        1
         64*       5
         65        2
         66        4
         67       11   *
         68        7
         69       10   *
         70       12   *
         71       11   *
         72       16   **
         73       15   *
         74       30   ***
         75       26   ***
         76       33   ****
         77       49   ******
         78       39   *****
         79       53   ******
         80       69   ********
         81       75   *********
         82       97   ************
         83       99   ************
         84      122   ***************
         85      142   ******************
         86      158   ********************
         87      169   **********************
         88      208   ***************************
         89      231   ******************************
         90      243   *******************************
         91      258   *********************************
         92      307   ***************************************
         93      273   ***********************************
         94      322   *****************************************
         95      339   ********************************************
         96      339   ********************************************
         97      355   **********************************************
         98      405   **************************************************  1
         99      384   **************************************************  2
        100      370   ************************************************
        101      370   ************************************************
        102      343   ********************************************
        103      367   ***********************************************
        104      337   *******************************************
        105      326   ******************************************
        106      330   ******************************************
        107      295   **************************************
        108      271   ***********************************
        109      228   *****************************
        110      221   ****************************
        111      206   **************************
        112      175   **********************
        113      171   **********************
        114      134   *****************
        115      126   ****************
        116      109   **************
        117       76   *********
        118       75   *********
        119       67   ********
        120       41   *****
        121       53   ******
        122       33   ****
        123       35   ****
        124       26   ***
        125       26   ***
        126       15   *
        127       15   *
        128        5
        129        7
        130        2
        131        1
        132        1
        134*       1
        136*       2
        137        1
        140*       1
        146*       1
        163*       1
        202*       1
        204*       2
        207*       2
        209*       2
        210        2
        211        1
        213*       1
        214        1
        215        2
        217*       3
        218        1
        219        2
        220        4
        221        4
        222        4
        223        2
        224        1
        225        1
        226        5
        227        8   *
        228        4
        230*       7
        231        4
        232        3
        233        5
        234        6
        235        3
        236        1
        237        1
        238        2
        239        2
        241*       5
        242        2
        243        5
        244        1
        245        1
        247*       2
        250*       1
        252*       4

AVERAGE GRAY LEVEL=100.0644
STANDARD DEVIATION=18.32424
NUMBER ELEMENTS=     1000
MIN. DN=        18
MAX. DN=       252

let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
