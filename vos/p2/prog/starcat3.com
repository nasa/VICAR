$!****************************************************************************
$!
$! Build proc for MIPL module starcat3
$! VPACK Version 1.9, Monday, December 07, 2009, 17:04:10
$!
$! Execute by entering:		$ @starcat3
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
$ write sys$output "*** module starcat3 ***"
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
$ write sys$output "Invalid argument given to starcat3.com file -- ", primary
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
$   if F$SEARCH("starcat3.imake") .nes. ""
$   then
$      vimake starcat3
$      purge starcat3.bld
$   else
$      if F$SEARCH("starcat3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake starcat3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @starcat3.bld "STD"
$   else
$      @starcat3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create starcat3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack starcat3.com -mixed -
	-s starcat3.f -
	-i starcat3.imake -
	-p starcat3.pdf -
	-t tststarcat3.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create starcat3.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM STARCAT3 - read input image (star field), locate stars, and store
C                     data in catalog for each star.
C
C   Revision History
C    8-97    SP   Changed to round up in computing auto threshold.  Commented
C                 out auto-threshold parameter processing.  (Ray said it did
C                 not work, and I was not able to tweak it to work OK.)
C                 Moved routine LABPROC behind CENTFIND to avoid Alpha compiler
C                 warnings.  This did not eliminate the warnings, which appear
C                 to be a compiler flaw, since ISL is an argument (warning)
C                 as is NL.
C                 Changed calls to CHKSTAT to use 5 arguments, calling ABEND
C                 in most cases.  (Because of OPEN_ACT and IO_ACT, the program
C                 will usually ABEND on errors, even without CHKSTAT.)
C                 Changed ".eq." TO ".ge." in check for saturation in CENTFIND.
C                 Changed MAXI default from 4.0E9 to 1.0E38 to better allow for
C                 saturation in fullword images.
C                 Changed calls to MVE with a first argument (DCODE) of 8,
C                 to use 0d0 (double precision 0) instead of 0.
C                 Corrected last argument in all calls to XVPARM.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
c
	implicit none
c
c	this routine allows for up to maxobjs dimension on the number 
c	of objects detected. That parameter is declared in subroutine
c	centfind.  To increase it reset that parmeter.
c
	integer*4 maxdn,ithr,iback,minpix,maxpix
	integer*4 lospike,hispike,minns,maxns,minnl,maxnl,nods,ibis
	integer*4 saominp,astminp,nlin1,isl,iss,columns,rows
	integer*4 imindn,imaxdn
	integer*4 numobj,astcnt,saocnt,nlimg,nsimg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal,nascii
	logical*4 asteroid,idef,iprnt,iauto,spike
	real*4 sens,saoratio,astratio,astmaxw,astmaxl,maxsum,minsum
	character*8 fmt
	character*35 version
c
	data version/' *** STARCAT3 9-11-97 SXP*** '/
c
	call xvmessage (version,' ')
c
	call parmproc (iuni,ouni,nods,sl,ss,nl,ns,
     1 nsimg,nlimg,fmt,ibis,iprnt,
     2 maxdn,iauto,sens,ithr,idef,iback,maxsum,minsum,
     3 minpix,maxpix,lospike,hispike,spike,minns,maxns,minnl,maxnl,
     4 asteroid,saominp,astminp,saoratio,astratio,astmaxw,astmaxl)
c
	call centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,fmt,
     2 maxdn,iauto,sens,ithr,idef,
     3 maxsum,minsum,spike,
     4 iback,imindn,imaxdn,minpix,maxpix,lospike,hispike,minns,maxns,
     5 minnl,maxnl,asteroid,saominp,astminp,saoratio,astratio,
     6 astmaxw,astmaxl,fullrecord,realrecord,asciirecord,nfull,
     7 nreal,nascii)
c
	call labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,
     1 nlin1,iss,isl,numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	call putparm (numobj)
	CALL XVCLOSE(IUNI,STAT,' ')
	call ibis_file_set(ibis,'nr',nlin1,stat)
	if (stat.ne.1) call ibis_signal(ibis,stat,1)
	call ibis_file_close (ibis,' ',stat)
	if (stat.ne.1) call ibis_signal_u (ouni(1),stat,1)
	if (nods.gt.1) call xvclose (ouni(2),stat,' ')
	if (nods.eq.3) call xvclose (ouni(3),stat,' ')
	return
	END
c========================================================================
	subroutine parmproc (iuni,ouni,nods,sl,ss,nl,ns,
     1 nsimg,nlimg,fmt,ibis,iprnt,
     2 maxdn,iauto,sens,ithr,idef,iback,maxsum,minsum,
     3 minpix,maxpix,lospike,hispike,spike,minns,maxns,minnl,maxnl,
     4 asteroid,saominp,astminp,saoratio,astratio,astmaxw,astmaxl)
c
c	parameter processor 
c
	implicit none
c
	common /catalog/ttype,ibfmt,tunits
c
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
c
	integer*4 nlimg,nsimg,nods,iback
	integer*4 maxdn,ibis,lospike,hispike,spikeval
	integer*4 ithr,minpix,minint,maxint,threshin
	integer*4 minns,maxns,maxpix,minnl,maxnl,count,def
	integer*4 saominp,astminp,minintg,maxintg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat,cnt
c	integer*4 sizefield(4),slparm,ssparm,nlparm,nsparm
	logical*4 xvptst,asteroid,idef,iprnt,iauto,spike
	real*4 saoratio,astratio,astmaxw,astmaxl
	real*4 sens,maxsum,minsum
	character*8 fmt
	character*256 fnam(3)
	character*132 outline
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
	data MININTG/-2147483648/		!MINIMUM INTEGER VALUE (I*4)
	data MAXINTG/2147483647/			!MAXIMUM INTEGER VALUE (I*4)
c
	CALL XVPARM ('OUT',FNAM,nods,DEF,3)
	CALL XVUNIT (IUNI,'INP',1,STAT,' ')
	call chkstat (stat,'XVunit error on 1st input file', 1,0,0)!ABEND if err
	CALL XVOPEN (IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1 'U_FORMAT','FULL',' ')
	call chkstat(stat,'XVopen error on 1st input file',1,0,0) !ABEND if err
	call xvget (iuni,stat,'NL',nlimg,'NS',nsimg,'FORMAT',fmt,' ')
c				!get size and format of input image 
	if (fmt.eq.'REAL'.or.fmt.eq.'DOUB'.or.fmt.eq.'COMP') then
		write (outline,10020) fmt
10020 format ('Format ',a8,' not supported by STARCAT3')
		call xvmessage (outline,' ')
		call abend
	endif
c
	call xvsize (sl,ss,nl,ns,nlimg,nsimg)
        if (ns .gt. 32767)  call mabend( 
     .     'ERROR: NS of input image > 32767')

C	First open catalog, then, if desired, an output image containing
C	the objects filled in with white DN=255 and an output image containing
C	the flag values, i.e., the object numbers as they ar found.
C
C					!ouni(1) is starcat catalog
	call xvunit(ouni(1),'OUT',1,stat,' ')
	call chkstat(stat,'XVopen error on output catalog file',1,0,0)!ABEND if error.
	if (nods.gt.1) then		!SKIP IF ONLY ONE OUTPUT
c					!ouni(2) is an image file with objects at dn=255
		CALL XVUNIT(ouni(2),'OUT',2,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat(stat,'XVunit error on 2nd output image',1,0,0)
		CALL XVOPEN(ouni(2),stat,'U_NL',nl,'U_NS',ns,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','BYTE',
     2	'O_FORMAT','BYTE',' ')
		call chkstat (stat,'XVopen error on 2nd output image', 1,0,0)
	endif
	if (nods.eq.3) then		!SKIP IF ONLY ONE OUTPUT
c				!OUNI(3) is flag image file (objects as they are numbered)
		CALL XVUNIT(ouni(3),'OUT',3,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat (stat,'XVunit error on 3rd output image', 1,0,0)
		CALL XVOPEN(ouni(3),stat,'U_NL',nl,'U_NS',ns,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','HALF',
     2	'O_FORMAT','HALF',' ')
		call chkstat (stat,'XVopen error on 3rd output image', 1,0,0)
	endif
C
C	Saturation value, maxd, allows the user to mark an object as
c	saturated but still count as an object while hispike says if value
c	exceeds hispike then do not count it as an object
c
	if (fmt.eq.'BYTE') maxdn=255
	if (fmt.eq.'HALF') maxdn=32767
	if (fmt.eq.'FULL') maxdn=maxintg
	call xvp('MAXD',maxdn,cnt)		!saturation - still an object
	if (fmt.eq.'HALF') then
		if (maxdn.lt.32767) then
		write (outline,10070) maxdn 
10070 format ('STARCAT3: MAXD (saturation) for a halfword image = ',i8)
		call xvmessage (outline,' ')
		endif
	endif
	if (fmt.eq.'FULL') then
		if (maxdn.lt.maxintg) then
		write (outline,10080) maxdn 
10080 format ('STARCAT3: MAXD (saturation) for a fullword image = ',i10)
		call xvmessage (outline,' ')
		endif
	endif
c
	iauto=.false.		!'AUTO' NO AUTOMATIC THRESHOLDING
CCCCCCCCC	if (xvptst('AUTO'))iauto=.true.	!check for autothresholding 
	sens=1.0					!'SENS' 1 SIGMA
CCCCCCCC	if (iauto) call xvp('SENS',sens,cnt)	!autothresholding sensitivity
C					!Number of sigma
	idef=.true.		!flag to indicate whether threshold is given
	ithr=0				!'THRE' DEFAULT THRESHOLD DN=0 for BYTE
	if (fmt.eq.'HALF') ithr=-32768		!default for 'HALF'
	if (fmt.eq.'FULL') ithr=minintg		!default for 'FULL'
	if (.not.iauto) then
		call xvp('THRE',threshin,cnt)		!THRESHOLD DN
		if(cnt.gt.0) then
			idef=.false.
			ithr=threshin
		endif
	endif
	iback=0				!'BACK' BACKGROUND DN=0
	call xvp('BACK',iback,cnt)	!BACKGROUND DN LEVEL
	iprnt=.false.		!'PRINT' DON'T PRINT CATALOG TO SESSION LOG
	if (xvptst('LIST'))iprnt=.true.	!1= PRINT CATALOG TO SESSION LOG
c
c  the following are single pixel rejection criteria parameters
c	maxint & minint
c
	if (fmt.eq.'BYTE') then
		maxint=0
		if (maxdn.eq.255) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	if (fmt.eq.'HALF') then
		maxint=-32768
		if (maxdn.eq.32767) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	if (fmt.eq.'FULL') then
		maxint=-65536
		if (maxdn.eq.65535) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	call xvp('SPIKE',spikeval,cnt)		!PIXEL MAXIMUM VALUE
	if (cnt.eq.0) then
		hispike=maxint 
	else
		hispike=spikeval
		spike=.true.
	endif
	if (spike) then
		maxdn=hispike-1
		write (outline,10100) hispike,maxdn 
10100 format ('STARCAT3: SPIKE=',i10,' MAXD=',i10,' reset MAXD=SPIKE-1')
		call xvmessage (outline,' ')
	endif
	if (fmt.eq.'BYTE') minint=0
	if (fmt.eq.'HALF') minint=-32768
	if (fmt.eq.'FULL') minint=minintg

	call xvp('LOVAL',lospike,cnt)			!PIXEL MINIMUM INTENSITY

	if (cnt.eq.0) 	then
		lospike=minint
	else
		if (.not.iauto) then
			if (lospike.ge.ithr) then
				if (ithr.eq.minintg) then
					lospike=ithr
				else
					lospike=ithr-1
				endif
			write (outline,10110) lospike,ithr 
10110 format ('STARCAT3: LOVAL=',i10,' THRE=',i10,' reset LOVAL=THRE-1')
			call xvmessage (outline,' ')
			endif
		endif
	endif
c
c  the following are object rejection criteria parameters
c	maxsum & minsum; maxpix & minpix; maxns & minns; maxnl & minnl
c
	maxsum=1.0e38
	call xvp('MAXI',maxsum,cnt)	!object maximum sum, in counts
	minsum=1.0
	call xvp('MINI',minsum,cnt)	!object minimum sum, in counts
	maxpix=maxintg
	call xvp('MAXP',maxpix,cnt)	!OBJECT MAXIMUM AREA, in pixels
	minpix=3
	call xvp('MINP',minpix,cnt)	!OBJECT MINIMUM AREA, in pixels
	minns=1	
	call xvp('MINS',minns,cnt)	!OBJECT MINIMUM SAMPLE DIMENSION
	maxns=1000000	
	call xvp('MAXS',maxns,cnt)	!OBJECT MAXIMUM SAMPLE DIMENSION
	minnl=1
	call xvp('MINL',minnl,cnt)	!OBJECT MINIMUM LINE DIMENSION
	maxnl=1000000
	call xvp('MAXL',maxnl,cnt)	!OBJECT MAXIMUM LINE DIMENSION
C asteroid option
	asteroid=.false.
	asteroid=xvptst('ASTEROID')
	if (asteroid) then
		call xvparm('SAOMINP',saominp,count,def,0)
		call xvparm('ASTMINP',astminp,count,def,0)
		call xvparm('SAORATIO',saoratio,count,def,0)
		call xvparm('ASTRATIO',astratio,count,def,0)
		call xvparm('ASTMAXW',astmaxw,count,def,0)
		call xvparm('ASTMAXL',astmaxl,count,def,0)
	endif
	return
	end
c============================================================================
	subroutine centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,fmt,
     2 maxdn,iauto,sens,ithr,idef,
     3 maxsum,minsum,spike,
     4 iback,imindn,imaxdn,minpix,maxpix,lospike,hispike,minns,maxns,
     5 minnl,maxnl,asteroid,saominp,astminp,saoratio,astratio,
     6 astmaxw,astmaxl,fullrecord,realrecord,asciirecord,nfull,
     7 nreal,nascii)
c
C*******************
C    Find all objects in field which meet certain criteria specified
C    in input parameters
C*******************
c
	implicit none
	include 'errdefs'
c
	common /catalog/ttype,ibfmt,tunits
c
	integer*4 maxobjs	!if maxobjs > 32767 make sure I*2 variables
	PARAMETER (maxobjs=2000)		!change nbin below
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
	byte outdata(32767)
	integer*2 flag(32767,2)
	integer*2 isls(maxobjs),isss(maxobjs),el(maxobjs),es(maxobjs)
	integer*2 cntr(maxobjs,2)
	integer*4 l,nl,ns,line,iss,isl,iuni,stat
	integer*4 j,ss,nods,numobj,ii
	integer*4 newc,nolc,n1,nbin,oldid,isw,numsat
	integer*4 npix,ithr,i,iback,maxdn,n2,num,newid,nlin1
	integer*4 saocnt,astcnt,sl,ouni(3),minpix
	integer*4 hispike,lospike,maxpix,minns,columns,rows,ibis
	integer*4 maxns,minnl,maxnl,saominp,astminp,imindn,imaxdn
	integer*4 fullrecord,realrecord,asciirecord,abendc,rejobj
	integer*4 nfull,nreal,nascii,scversion,scunum,maxfreq
	integer*4 id(maxobjs),npixs(maxobjs),satcnt(maxobjs),ibuf(7)
	integer*4 in(32767),rejcnt(maxobjs)
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65536:65535)
	logical*4 asteroid,idef,iprnt,iauto,spike
        real   rthresh
	real*4 saoratio,astratio
	real*4 astmaxw,astmaxl,sens,mean,sigma,median,mode,risump,xsigma
	real*4 ricens,maxsum,minsum
	real*4 rbuf(11),sumdn(maxobjs)
	real*8 l2,ij,iumpjl,iumpj2,iumpl2,rl,r8isump,rsumpl
	real*8 cenx(maxobjs),ceny(maxobjs),sumpjl(maxobjs)
        real*8 sumpj2(maxobjs)
	real*8 sumpl2(maxobjs)
	character*8 fmt
	character*16 openmode,iborg,scunits(50)
	character*20 abuf(3)
	character*150 heading
	character*150 outline
c
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
c	data heading
c    1/'  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--
c    2ENTSAMP CENTLINE              MAJ AXIS   MIN AXIS   RATIO  ROTANGL
c    3E'/
	heading = '  NUMBER INTENSITY    AREA       SL       ' //
     *      'SS       NL       NS CENTSAMP CENTLINE              ' //
     *      'MAJ AXIS   MIN AXIS   RATIO  ROTANGLE'
c
c
c The starcat catalog is an IBIS-2 tabular file which has the following format.
c
c
c	***************************************************************
c	descriptor	description			format	units
c	________________________________________________________
c	entry		catalog entry number	full		<none>		
c	DN-bkg		sum of DNs - background	real		counts
c	tot pix		total pixel count 		full		<none>
c	sl			starting line			full		<none>
c	ss			starting sample		full		<none>
c	nl			number of lines		full		<none>
c	ns			number of samples		full		<none>
c	centsamp		center sample			real		<none>
c	centline		center line			real		<none>
c	maj axis		major axis length		real		<none>
c	min axis		minor axis length		real		<none>
c	ratio		ratio minor/major axis	real		<none>
c	rotangle		rotation angle of ellip	real		degrees
c	calalog		source catalog			ascii-A20	<none>
c	ID			catalog ID			ascii-A16	<none>
c	RA			right ascension		real		hours
c	dec			declination			real		degrees
c	mag calc		calculated magnitude	real		<none>
c	mag ref		catalog magnitude		real		<none>
c	class		classification			ascii-A8	<none>
c	sat pixs		# saturated pixels		full		<none>
c	***************************************************************
c
C	The old STARCAT catalog format was the following
C
C*************************
C     IBUF(1)=SUM(DN-BACK)			I*4
C     IBUF(2)=SUM PIXELS				I*4
C     IBUF(3)=SL  					I*4
C     IBUF(4)=SS  					I*4
C     IBUF(5)=NL  					I*4
C     IBUF(6)=NS  					I*4
C     IBUF(7)=CENTROID SAMPLE  		R*4
C     IBUF(8)=CENTROID LINE  			R*4
C     IBUF(9)=major axis length  		R*4
C     IBUF(10)=minor axis length  		R*4
C     IBUF(11)=object identifier	  	R*4 - ("asteroid" option only)
C     IBUF(12)=rotation angle degrees 	R*4
C**********************
c	
	nbin=2000		!MAX NUMBER OF OBJECTS PER 2 LINES - same as maxobjs
	iborg='ROW'
	columns=21
	rows=maxobjs		!initial value - will be increased if needed
	openmode='WRITE'
	scversion=2			!as of 1/21/95 - 21 column (type 2)
	call scopen (ouni(1),ibis,columns,rows,ibfmt,iborg,ttype,
     1 tunits,scunits,scunum,fullrecord,realrecord,asciirecord,
     2 nfull,nreal,nascii,openmode,scversion)
c
	if (iauto) then
		call autot (iuni,fmt,sl,ss,nl,ns,iback,ohist,
     1	hist,fhist,mean,sigma,maxfreq,imindn,imaxdn,median,mode,
     2	xsigma)
		write (outline,10300) imindn
10300 format ('MIN_DN = ',i7,' for image')
		call xvmessage (outline,' ')
c note: imaxdn has no effect on maxdn specified in parmeters
		write (outline,10310) imaxdn
10310 format ('MAX_DN = ',i7,' for image')
		call xvmessage (outline,' ')
		write (outline,10320) mean
10320 format ('MEAN   = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10330) sigma
10330 format ('SIGMA  = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10335) median
10335 format ('MEDIAN = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10337) mode
10337 format ('MODE   = ',f9.2,' for image')
		call xvmessage (outline,' ')
c	call window into image

		rthresh=mean+sens*xsigma
                if ( rthresh .GT. INT(rthresh) )  then
                   ithr = INT(rthresh) + 1   ! Round up to next integer
                else
                   ithr = INT(rthresh)
                end if
		lospike=ithr-1
		write (outline,10340) sens
10340 format ('SENSITIVITY = ',f9.2,' as given')
		call xvmessage (outline,' ')
		write (outline,10345) xsigma
10345 format ('XSIGMA = ',f9.2,' as computed')
		call xvmessage (outline,' ')
		write (outline,10350) ithr
10350 format ('AUTOTHRESHOLD (MEAN+SENSITIVITY*XSIGMA) = ',i7)
		call xvmessage (outline,' ')
		write (outline,10355) lospike
10355 format ('LOVAL  = ',i8)
		call xvmessage(outline,' ')
	else
		write (outline,10360) ithr
10360 format ('THRESH = ',i6,' as given')
		call xvmessage (outline,' ')
	endif
c
c**			debugging purposes only
c**	if (.not.idef) then
c**		ithr=mean+sigma
c**	endif
c
c	print out heading
	if (iprnt) then
		call xvmessage (heading,' ')    !align with PRINZ.
	endif

	NLIN1=0			!output catalog line counter
	OLDID=1			!initialize ID 
	ISW=1			!initialize flag buffer to buffer #1 
	NPIX=NS			!NUMBER OF PIXELS =  NUMBER OF SAMPLES/LINE
	NUMOBJ=0			!RUNNING COUNTER OF REAL OBJECTS FOUND
	NOLC=0
	NEWC=0
	saocnt=0		!no asteroid candidate
	astcnt=0		!no sao catalog star candidate
	ISL=SL			!input starting line
	ISS=SS			!input starting sample
C*******************
C     CLEAR FLAG BUFFERS
C
C	The FLAG buffer is a 2 line array equal in width to the image
C	which associates an object ID with each sample. ISW indicates
C	the current and previous line in order to find contiguous pixels
C	for each object.
C*******************
	CALL MVE(2,NPIX,0,FLAG(1,1),0,1)
	CALL MVE(2,NPIX,0,FLAG(1,2),0,1)
	CALL MVE(7,NBIN,0.0,sumdn,0,1)
	CALL MVE(4,NBIN,0,NPIXS,0,1)
	CALL MVE(8,NBIN,0d0,CENX,0,1)
	CALL MVE(8,NBIN,0d0,CENY,0,1)
	CALL MVE(8,NBIN,0d0,sumpl2,0,1)
	CALL MVE(8,NBIN,0d0,sumpj2,0,1)
	CALL MVE(8,NBIN,0d0,sumpjl,0,1)
	call mve(4,nbin,0,satcnt,0,1)
	call mve(4,nbin,0,rejcnt,0,1)
	CALL MVE(-6,NBIN,32767,ISLS,0,1)
	CALL MVE(-6,NBIN,32767,ISSS,0,1)
	CALL MVE(2,NBIN,0,EL,0,1)
	CALL MVE(2,NBIN,0,ES,0,1)
	CALL MVE(2,NBIN,0,CNTR(1,1),0,1)
	CALL MVE(2,NBIN,0,CNTR(1,2),0,1)
c
c------> MAIN LOOP: Read input in line by line, find objects <------------
	DO 1000 L=1,NL
c				!print out a summary every 500 lines
	if (mod(L,500).eq.0) then
		write (outline,10410) l,numobj
10410 format ('Line# = ',i7,'        #Objects = ',i7)
		call xvmessage(outline,' ')
	endif
	LINE=L+ISL-1
c	if (line.eq.596) then
c		call xvmessage ('Here',' ')
c	endif
C     read the picture to process.
C	INPUT IMAGE DATA WILL BE IN BUFFER IN(NS)
C
	call xvread(iuni,in,stat,'LINE',line,'SAMP',iss,'NSAMPS',ns,' ')
	if (stat.eq.END_OF_FILE) GO TO 1004
	call chkstat(stat,'XVread error on input image', 1,0,0) !ABEND if error.
	if (nods.gt.1) call mve (1,ns,0,outdata,0,1)    !0's to output
C*******************
C     SEARCH IN(1) to in(npix) FOR AN OBJECT .ge. threshold
C*******************
      NEWC=0
      N1=1				!initialize starting samp position
c  Now start search on the line, l
c--> Find all objects on one line loop (goes to 200)
c
c  I think this algorithm will have some problems if mean is lt 0
c  It was originally written for byte images where lowest dn was 0
c
105	continue
	ricens=0.0			!this initializes centroid function
	risump=0.0			!init sum of dns
	iumpj2=0
	iumpjl=0
	iumpl2=0
	numsat=0			!initialize saturation counter for object
	rejobj=0			!valid object (for minint comparison)
      do 110 j=n1,npix
		if (in(j).ge.ithr) go to 120	!image > threshold?
110   continue
C*******************
C     finished line!
C*******************
      go to 200
120   continue
	N1=J				!mark starting samp position of object
      L2=L*L
      do 130 j=n1,npix
		i=in(j)-iback
      	if (in(j).lt.ithr) go to 140	!image < threshold?
C*******************
C     Have object - Update centroid numerator
C	- 1st moment of marginal distribution in x [sample direction]
C*******************
c	check for low value pixel modification  criteria
		if (in(j).lt.lospike) i=lospike-iback	!if dn lt lospike, then make lospike
		if (in(j).ge.maxdn) then
			i=maxdn
			numsat=numsat+1
		endif
		if (spike) then
			if (in(j).ge.hispike) rejobj=1  !reject object after enclosure
		endif
		ij=dble(i*j)			!1st moment in sample
		ricens=ricens+sngl(ij)
		risump=risump+float(i)		!add new intensity to sum dn's
		iumpj2=iumpj2+ij*j		!2nd moment in sample
		iumpjl=iumpjl+ij*L
		iumpl2=iumpl2+i*L2		!2nd moment in line
130   continue

      J=NPIX+1				!at end of line plus 1
140   N2=J-1				!mark ending position of object
C*******************
C     FILL IN STAR AREA ON 2ND OUTPUT FILE if greater than min ns
C	NOTE:  This occasionally marks certain rejected objects - esp single pix.
C*******************
	if ((n2-n1+1).ge.minns) then
		do ii=n1,n2
			outdata(ii)=255
		enddo
	endif
C*******************
C     FIND ID OF THIS OBJECT FROM PREVIOUS FLAG LINE
C	CURRENT  FLAG LINE = FLAG(NS,ISW)
C	PREVIOUS FLAG LINE = FLAG(NS,3-ISW)
C*******************
c
c**** got to figure out where to check rejobj
c****
      CALL MATCH (FLAG(1,3-ISW),N1,N2,NUM,ID,NPIX)
      NEWID=ID(1)
      IF(NUM.LT.2) GO TO 101
C*******************
C     IF MANY IDs,  CONDENSE TO ONE ID
C*******************
	CALL JJLCONCAT(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1               sumpj2,sumpjl,sumpl2,satcnt,rejcnt,NUM,ID)
      GO TO 102
101   continue
C*******************
C     IF NO ID,  CREATE ONE
C     UPDATE NEW FLAG BUFFER
C     UPDATE NEW COUNTER
C     UPDATE THE STATISTICS BUFFERS
C*******************
	if (num.eq.0) then
		CALL FIND (NBIN,NEWID,sumdn,OLDID)
	endif
102	continue
c	now have our id
	CALL MVE (-6,N2-N1+1,NEWID,FLAG(N1,ISW),0,1)
      NEWC=NEWC+1
      CNTR(NEWC,ISW)=NEWID
	sumdn(newid)=sumdn(newid)+risump	!sum of intensities
	NPIXS(NEWID)=NPIXS(NEWID)+N2-N1+1	!number of pixels in area
	CENX(NEWID)=CENX(NEWID)+dble(ricens)	!centroid in x-direction
	r8isump=dble(risump)
	rl=dble(l)
	rsumpl=r8isump*rl
      CENY(NEWID)=CENY(NEWID)+rsumpl		!centroid in y-direction
         SUMPJ2(NEWID) = SUMPJ2(NEWID)+IUMPJ2
         SUMPJL(NEWID) = SUMPJL(NEWID)+IUMPJL
         SUMPL2(NEWID) = SUMPL2(NEWID)+IUMPL2
      IF (ISLS(NEWID).GT.L) ISLS(NEWID)=L
      IF (ISSS(NEWID).GT.N1) ISSS(NEWID)=N1
      IF (ES(NEWID).LT.N2) ES(NEWID)=N2
      EL(NEWID)=L
	satcnt(newid) = satcnt(newid)+numsat	!number of saturated pixels
	rejcnt(newid) = rejcnt(newid)+rejobj
C*******************
C     SEND BACK FOR MORE DATA ON SAME LINE
C*******************
      N1=N2+2			!skip at least two pixels for next object
      if (n1.lt.npix) go to 105	!loop back if not at end of line
C--> FOUND ALL OBJECTS on one line - loop back to 105
200   CONTINUE			!formerly 103
C*******************
C     WRITE OUT 2ND and 3RD OUTPUT FILES
C*******************
	if (nods.gt.1) then
		call xvwrit(ouni(2),outdata,stat,'LINE',l,'NSAMPS',ns,
     1	' ')
		call chkstat (stat,'XVwrit error on 2nd output image', 1,0,0)
	endif
	if (nods.eq.3) then
		call xvwrit(ouni(3),flag(1,isw),stat,'LINE',l,'NSAMPS',
     1	ns,' ')
		call chkstat (stat,'XVwrit error on 3rd output image', 1,0,0)
	endif
C*******************
C     SEARCH BOTH FLAG BUFFERS FOR
C     TERMINATED STARS AND WRITE TO DATA SET
C*******************
      CALL EOL(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,MINPIX,satcnt,rejcnt,rbuf,abuf,IBUF,
     2 NPIX,NLIN1,NL,L,IPRNT,CNTR(1,3-ISW),
     3 CNTR(1,ISW),NOLC,NEWC,numobj,maxdn,
     4 MAXPIX,minsum,maxsum,MINNS,MAXNS,
     5 MINNL,MAXNL,OUNI,ibis,rows,
     6 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     7 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     8 nascii,fmt)
C*******************
C     ZERO THE OLD FLAG LINE
C     AND SWITCH FLAGS
C*******************
      CALL MVE(2,NPIX,0,FLAG(1,3-ISW),0,1)
      ISW=3-ISW
      NOLC=NEWC
1000   CONTINUE
c------> END MAIN LOOP: Read input completely, found objects <------------
C
1004   CONTINUE
C	Update nr of catalog file if greater that opening value
	if (nlin1.gt.rows) then
		rows=rows+1000
		call ibis_file_set(ibis,'nr',rows,stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendc)
	endif
c
	write (outline,10500) numobj
10500 format ('*** Total of ',i10,' stars found ***')
	call xvmessage (outline,' ')
	return
	end
c****************************************************************************
	SUBROUTINE FIND (NBIN,NEWID,sumdn,OLDID)
C*******************
C     TO FIND A NEW BIN POSITION
C     NBIN=LENGTH OF BIN BUFFERS
C     NEWID=NEW BIN VALUE RETURNED
C     sumdn=SUM DN BUFFER
C     OLDID=LAST FOUND I.D.
C*******************
	implicit none
	INTEGER*4 OLDID
	integer*4 j,nbin,newid
	real*4 sumdn(1)
	character*100 outline
c
      DO 30 J=OLDID,NBIN
		if (sumdn(j).eq.0.0) go to 40
30    CONTINUE
      DO 35 J=1,OLDID
		if (sumdn(j).eq.0.0) go to 40
35    CONTINUE
	write (outline,10100) nbin
10100 format ('**SUBROUTINE FIND - All ',i4,' bins filled')
	call xvmessage (outline,' ')
	CALL ABEND
c
40    NEWID=J
      OLDID=J
      RETURN
      END
c****************************************************************************
	SUBROUTINE JJLCONCAT(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,
     1 CENY,sumpj2,sumpjl,sumpl2,satcnt,rejcnt,NUM,ID)
C*******************
C     ROUTINE TO CONCATENATE MANY I.D. S
C	NUM = Number of objects located
C*******************
	implicit none
	INTEGER*2 ISLS(1),ISSS(1),EL(1),ES(1)
	INTEGER*4 OLDID,NPIXS(1),ID(1),satcnt(1),rejcnt(1)
	integer*4 newid,num,j
	real*4 sumdn(1)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
c
      NEWID=ID(1)
      DO 10 J=2,NUM
      OLDID=ID(J)
	sumdn(newid)=sumdn(newid)+sumdn(oldid)
	sumdn(oldid)=0.0
	satcnt(newid)=satcnt(newid)+satcnt(oldid)
	satcnt(oldid)=0
	rejcnt(newid)=rejcnt(newid)+rejcnt(oldid)
	rejcnt(oldid)=0
      NPIXS(NEWID)=NPIXS(NEWID)+NPIXS(OLDID)
      NPIXS(OLDID)=0
      IF(ISLS(NEWID).GT.ISLS(OLDID)) ISLS(NEWID)=ISLS(OLDID)
      ISLS(OLDID)=32767
      IF(ISSS(NEWID).GT.ISSS(OLDID)) ISSS(NEWID)=ISSS(OLDID)
      ISSS(OLDID)=32767
      IF(EL(NEWID).LT.EL(OLDID)) EL(NEWID)=EL(OLDID)
      EL(OLDID)=0
      IF(ES(NEWID).LT.ES(OLDID)) ES(NEWID)=ES(OLDID)
      ES(OLDID)=0
      CENX(NEWID)=CENX(NEWID)+CENX(OLDID)
      CENX(OLDID)=0.0
      CENY(NEWID)=CENY(NEWID)+CENY(OLDID)
      CENY(OLDID)=0.0
          SUMPJ2(NEWID)=SUMPJ2(NEWID)+SUMPJ2(OLDID)
          SUMPJ2(OLDID)=0.0
          SUMPJL(NEWID)=SUMPJL(NEWID)+SUMPJL(OLDID)
          SUMPJL(OLDID)=0.0
          SUMPL2(NEWID)=SUMPL2(NEWID)+SUMPL2(OLDID)
          SUMPL2(OLDID)=0.0
10    continue
      RETURN
      END
c============================================================================
      subroutine labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,nlin1,iss,isl,
     1 numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	implicit none
c
	integer*4 istat,ouni(3),nods,sl,ss,nl,ns,iback,nlin1
	integer*4 imindn,imaxdn,isl,iss,numobj,saocnt,astcnt
	integer*4 ithr,nlimg,nsimg,j,maxnl,maxns,maxdn
	logical*4 idef,iauto,asteroid
	real*4 mean,sigma,sens
	character*8 threshold,autothresh,man,default
	character*8 fmt,area,FULL,SUBAREA
	character*28 filen(3)
	character*35 version
c
	data man/'ENTERED '/,default/'DEFAULT '/
	data autothresh/'AUTO    '/
	data filen	/'STARCAT TYPE   2 CATALOG    ',
     1			 'STARCAT FILLED DISPLAY FILE ',
     2			 'STARCAT FLAGGED OBJECT FILE '/
c
	FULL='FULL    '
	SUBAREA='SUBAREA '

	AREA=FULL
	if (nl.ne.nlimg) AREA=SUBAREA
	if (ns.ne.nsimg) AREA=SUBAREA
	IF (ISL.NE.1) AREA=SUBAREA
	IF (ISS.NE.1) AREA=SUBAREA
	if (iauto) then
		threshold=autothresh
	else
		if (idef) then
			threshold=default
		else
			threshold=man
		endif
	endif
	do j=1,nods
		call xladd(ouni(j),'HISTORY','DATASET',filen(j),istat,
     1 'FORMAT','STRING',' ')
		call xladd(ouni(j),'HISTORY','CREATOR',version,istat,
     1 'FORMAT','STRING',' ')
		call xladd(ouni(j),'HISTORY','IMGFMT',fmt,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','THRESHOLD',threshold,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','THRESHVAL',ithr,istat,
     1 'FORMAT','INT',' ')
		if (iauto) then
			call xladd(ouni(j),'HISTORY','MIN_DN',imindn,istat,
     1 'FORMAT','INT',' ')
			call xladd(ouni(j),'HISTORY','MAX_DN',imaxdn,istat,
     1 'FORMAT','INT',' ')
			call xladd (ouni(j),'HISTORY','MEAN',mean,istat,
     1 		'FORMAT','REAL',' ')
			call xladd (ouni(j),'HISTORY','SIGMA',sigma,istat,
     1 		'FORMAT','REAL',' ')
			call xladd(ouni(j),'HISTORY','SENSITIVITY',sens,istat,
     1		'FORMAT','REAL',' ')
c
		endif
		call xladd (ouni(j),'HISTORY','BKGVALUE',iback,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','SATURAT_VAL',maxdn,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','MAX_LINESIZE',maxnl,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','MAX_SAMPSIZE',maxns,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','TAB_AREA',area,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','SL_IMAGE',SL,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','SS_IMAGE',SS,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','NL_IMAGE',NL,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','NS_IMAGE',NS,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','OBJECTS',numobj,istat,
     1 'FORMAT','INT',' ')
c
		if (asteroid) then
			call xladd(ouni(j),'HISTORY','ASTEROID',astcnt,istat,
     1	'FORMAT','INT',' ')
			call xladd(ouni(j),'HISTORY','SAOSTARS',saocnt,istat,
     1	'FORMAT','INT',' ')
		endif
c
	enddo

	return
	end
c****************************************************************************
	SUBROUTINE MATCH (FLAG,N1,N2,NUM,ID,NS)
C*******************
C     RETURN I.D. OF OBJECT FOUND ON PREVIOUS LINE
C     FLAG=OLD FLAG BUFFER
C     N1=BEGIN  N2=END  OF OBJECT
C     NUM = NUMBER OF OBJECTS LOCATED
C     ID=I.D. OF OBJECTS LOCATED
C*******************
	implicit none
	INTEGER*2 FLAG(1)
	INTEGER*4 ID(1)
	integer*4 m1,m2,n1,n2,ns,num,j,k,n
c
      M1=N1-1				!point 1 pixel to left of object
      M2=N2+1				!point 1 pixel to right of object
      IF(M1.LT.1) M1=1
      IF(M2.GT.NS) M2=NS
      NUM=0				!running counter of objects being checked
      K=0
C*******************
C     FIND ALL FLAGS - search for this object # in previous line flag buffer
C*******************
      DO 10 J=M1,M2
      	IF(FLAG(J).EQ.K) GO TO 10
      	IF(FLAG(J).EQ.0) GO TO 10
      	K=FLAG(J)
      	NUM=NUM+1
      	ID(NUM)=K
10    CONTINUE
      IF(NUM.LT.2) RETURN
C*******************
C     REJECT DUPLICATES
C*******************
	call isort (id,1,num)
      N=1
      K=ID(1)
      DO 20 J=2,NUM
      	IF(K.EQ.ID(J)) GO TO 20
      	N=N+1
      	ID(N)=ID(J)
      	K=ID(J)
20    CONTINUE
      NUM=N
      RETURN
      END
c****************************************************************************
	SUBROUTINE EOL(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,MINPIX,satcnt,rejcnt,rbuf,abuf,
     2 IBUF,NS,NLIN1,NL,LINE,IPRNT,OLCNTR,NECNTR,NOLC,
     3 NEWC,numobj,maxdn,MAXPIX,minsum,maxsum,MINNS,
     4 MAXNS,MINNL,MAXNL,OUNI,ibis,rows,
     5 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     6 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     7 nascii,fmt)
C*******************
C     SEARCH FOR FINISHED OBJECTS IN JUST COMPLETED LINE
C*******************
	implicit none
	INTEGER*2 OLCNTR(1),NECNTR(1)
	INTEGER*2 ISLS(1),ISSS(1),EL(1),ES(1)
	integer*4 j,k,nlin1,ibis,rows,maxdn
	integer*4 newc,newid,nolc,nl,ns,numobj
	integer*4 line,maxpix,minpix
	integer*4 maxnl,minnl,maxns,minns
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal,nascii
	integer*4 ibuf(nfull)
	integer*4 npixs(1),satcnt(1),rejcnt(1)
	integer*4 ouni(3)
	logical*4 asteroid,iprnt
	real*4 astmaxl,astmaxw,astratio,saoratio,maxsum,minsum
	real*4 rbuf(nreal),sumdn(1)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
	character*8 fmt
	character*20 abuf(3)
c
C*******************
C     SEARCH OLCNTR FOR IDs NOT IN NECNTR
C*******************
      IF(LINE.EQ.NL) GO TO 51
      IF(NOLC.EQ.0) RETURN
      IF(NEWC.EQ.0) GO TO 51
      DO 10 J=1,NOLC
 	     NEWID=OLCNTR(J)
      	DO 20 K=1,NEWC
	     	 IF(NEWID.EQ.NECNTR(K)) GO TO 10
20    CONTINUE
C*******************
C     STAR NOT CONTINUED
C*******************
	CALL FIN(NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,MINPIX,satcnt,rejcnt,rows,abuf,
     2 IBUF,rbuf,NLIN1,IPRNT,numobj,maxdn,
     3 MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
10    CONTINUE
      RETURN
C
C*******************
C     FINISH OFF EVERYTHING - JUST FINISHED LINE
C*******************
51    IF(NOLC.EQ.0) RETURN
      DO 50 J=1,NOLC
      NEWID=OLCNTR(J)
	CALL FIN(NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,MINPIX,satcnt,rejcnt,rows,abuf,
     2 IBUF,rbuf,NLIN1,IPRNT,numobj,maxdn,
     3 MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
50    CONTINUE
	RETURN
	END
c****************************************************************************
	SUBROUTINE FIN (NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,
     2 MINPIX,satcnt,rejcnt,rows,abuf,ibuf,rbuf,NLIN1,IPRNT,numobj,
     3 maxdn,MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
C*******************
C     ROUTINE TO TERMINATE AN OBJECT AND WRITE TO CATALOG
C     ibuf=catalog I*4 data
c	rbuf=catalog R*4 data
c	abuf=catalog ASCII data
C*******************
	implicit none
	integer*2 ISLS(1),ISSS(1),EL(1),ES(1)
	integer*4 npixs(1),satcnt(1),rejcnt(1)
	integer*4 ouni(3),stat
	integer*4 il,ind,is,ibis,abendc,rows,maxdn
	integer*4 maxnl,minnl,maxns,minns,maxpix,minpix
	integer*4 newid,nlin1,nrot,numobj,nfull,nreal,nascii
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord
	integer*4 ibuf(nfull)
	logical*4 asteroid,iprnt
	real*4 rbuf(nreal),sumdn(1)
	real*4 mu20,mu02,mu11,rad,ratio,minsum,maxsum
	real*4 r,astratio,saoratio,theta,astmaxw,astmaxl
	real*4 amatrx(2,2),dmatrx(2),vmatrx(2,2)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
	real*8 sump
	character*8 fmt
	character*16 abuf1
	character*20 abuf2,abuf(3)
c
	data rad/57.29577/
	data abuf1/'                '/
	data abuf2/'                    '/
c
c  first, check out object rejection criteria
      IF (NPIXS(NEWID).EQ.0) RETURN		!no object if no area
      IF (NPIXS(NEWID).LT.MINPIX) GO TO 100	!no object if lt min # pixels
      IF (NPIXS(NEWID).GT.MAXPIX) GO TO 100	!no object if gt max pixels
      IL=EL(NEWID)-ISLS(NEWID)+1
      IS=ES(NEWID)-ISSS(NEWID)+1
      IF (IL.LT.MINNL) GO TO 100		!no object if lt min lines
      IF (IL.GT.MAXNL) GO TO 100		!no object if gt max line
      IF (IS.LT.MINNS) GO TO 100		!no onject if lt min samples
      IF (IS.GT.MAXNS) GO TO 100		!no object if gt max samples
	if (fmt.eq.'BYTE') then
		if (sumdn(newid).le.0.0) go to 100		!may have to worry about
	endif
c**      IF (ISUM(NEWID).LT.MININT) GO TO 100		!dn sums lt 0.0
c**      IF (ISUM(NEWID).GT.MAXINT) GO TO 100
	if (sumdn(newid).lt.minsum) go to 100	!no obj if lt min sumdn
	if (sumdn(newid).gt.maxsum) go to 100	!no obj if gt max sumdn
	if (rejcnt(newid).gt.1) go to 100		!no object if 1 dn gt maxint
c
c	OBJECT IS REAL!!
c
	numobj=numobj+1
	sump=dble(sumdn(newid))
C					!Catalog entries
	ibuf(1)=nlin1+1			!1 - entry number
	rbuf(1)=sumdn(newid)		!2 - sum of DNs of object (intensity)
      IBUF(2)=NPIXS(NEWID)		!3 - number of pixels in area
      IBUF(3)=ISLS(NEWID)		!4 - SL of object from size field
      IBUF(4)=ISSS(NEWID)		!5 - SS of object from size field
      IBUF(5)=IL			!6 - NL of object from size field
      IBUF(6)=IS			!7 - NS of object from size field
      RBUF(2)=CENX(NEWID)/sump		!8 - centroid sample position
      RBUF(3)=CENY(NEWID)/sump		!9 - centroid line position
	ibuf(7)=satcnt(newid)		!21 - # of saturated pixels
c
      MU20=SUMPJ2(NEWID)/SUMP-(CENX(NEWID)/(SUMP))**2.
      MU02=SUMPL2(NEWID)/SUMP-(CENY(NEWID)/(SUMP))**2.
      MU11=SUMPJL(NEWID)/SUMP-CENY(NEWID)*CENX(NEWID)/(SUMP)**2.
c
      amatrx(1,1)=mu20
      amatrx(2,2)=mu02
      amatrx(1,2)=mu11
      amatrx(2,1)=mu11
      call JACOBI(amatrx,2,2,dmatrx,vmatrx,nrot,ind)
      if (ind.eq.0) then
        call EIGSRT(dmatrx,vmatrx,2,2)
        theta=-rad*atan2(vmatrx(2,1),vmatrx(1,1))
c						!finish catalog entries
        RBUF(4) = sqrt(abs(dmatrx(1)))		!10 - major axis of object
        RBUF(5) = sqrt(abs(dmatrx(2)))		!11 - minor axis of object
	  ratio=rbuf(5)/rbuf(4)		   	! minor/major axes
        RBUF(6) = ratio				!12 - minor/major ratio
        RBUF(7) = THETA				!13 - rotation of major axis
      else
         numobj=numobj-1
         goto 100
C*** from an earlier incarnation of this program
c        IF ((MU20.NE.MU02).OR.(MU11.NE.0)) THEN
c           THETA=-ATAN2(2.*MU11,MU20-MU02)*rad/2
c        ELSE
c           THETA=-9999.
c        ENDIF
c        RBUF(3) = MU20
c        RBUF(4) = MU02
c        RBUF(5) = MU11
c        RBUF(6) = THETA
c***
      endif

      if (asteroid) then  			! logic for asteroid program
		if (rbuf(4).eq.0.0) rbuf(4)=.00001
		ratio=rbuf(5)/rbuf(4)  	 	! minor/major axes
      	   if((int(rbuf(1)).ge.saominp).and.
     1 		(ratio.gt.saoratio)) then	!if intensity & area > then
			goto 180    			! SAO star candidate
         	   else if((int(rbuf(1)).ge.astminp).and.
     1        (ratio.lt.astratio).and.   	! asteroid candidates
     2        (rbuf(5).le.astmaxw)) then
		  	  r=sqrt(float(ibuf(5)**2+ibuf(6)**2)) 	        	! length
		if (r.lt.astmaxl) goto 190  		! accept object
         endif
         	numobj=numobj-1   				! reject object
         	goto 190
180		continue
c**	RBUF(ISTART+11)=1.0			!11- #1=SAO candidate
		abuf(1)='starcat3'
		abuf(3)='star'
		saocnt=saocnt+1
		go to 195
190		continue
c**	RBUF(ISTART+11)=2.0			!11- #2=asteroid candidate
		abuf(1)='starcat3'
		abuf(3)='asteroid'
		astcnt=astcnt+1
195		continue
      endif
200   continue
c 	skip printing if 'LIST' not given
	if (iprnt) CALL PRINZ(IBUF,RBUF,nfull,nreal)
C*******************
C     LOGIC FOR STATISTICS DATA SET
C*******************
	NLIN1=NLIN1+1		!bump catalog line counter
c
c	go write out catalog
c
	if (nlin1.gt.rows) then
		rows=rows+1000
		call ibis_file_set(ibis,'nr',rows,stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendc)
	endif
c				!catalog
	call ibis_record_write (fullrecord,ibuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
	call ibis_record_write (realrecord,rbuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
	call ibis_record_write (asciirecord,abuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
C*******************
C     CLEAR OUT BINS
C*******************
100	CONTINUE
	sumdn(newid)=0.0
	NPIXS(NEWID)=0
	ISLS(NEWID)=32767
	ISSS(NEWID)=32767
	EL(NEWID)=0
	ES(NEWID)=0
	CENX(NEWID)=0.0
	CENY(NEWID)=0.0
	SUMPJ2(NEWID)=0.0
	SUMPL2(NEWID)=0.0
	SUMPJL(NEWID)=0.0
	satcnt(newid)=0
	rejcnt(newid)=0
	RETURN
	END
c****************************************************************************
      SUBROUTINE PRINZ(IBUF,RBUF,nfull,nreal)
c
c	prints out catalog to log and terminal if 'LIST' keyword given
c
	implicit none
	integer*4 nfull,nreal
	INTEGER*4 IBUF(nfull),i,j
	REAL*4 RBUF(nreal)
	character*150 outline
c
	write (outline,10100) ibuf(1),rbuf(1),(ibuf(i),i=2,6),
     1	(rbuf(j),j=2,7)   !include rotangle after ratio.
10100 format(i8,f9.0,5i9,2f9.2,13x,4f9.2)
	call xvmessage(outline,' ')
      RETURN
      END
c****************************************************************************
      SUBROUTINE JACOBI(A,N,NP,D,V,NROT,ind)
c Computes eigenvalues & eigenvectors.
c A=n by n matrix of real symmetric values in an np by np matrix.
c D=returns n eigenvalues of A.
c V=returns normalized eigenvectors of A in NP by NP matrix.
c NROT=# jacobi rotations required.
c ind= 0(normal) 1(abnormal)
	implicit none
	integer*4 i,ind,ip,iq,j,n,np,nrot
	real*4 A(NP,NP),D(NP),V(NP,NP),B(100),Z(100)
	real*4 c,g,h,s,sm,t,tau,theta
	real*4 tresh
c
c	integer*4 nmax
c	PARAMETER (NMAX=100)
c
      ind=0
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+G.EQ.ABS(D(IP)))
     *         .AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
      ind=1				!1 = error
      RETURN
      END
c****************************************************************************
      SUBROUTINE EIGSRT(D,V,N,NP)
c sorts the eigenvalues into ASCENDING order & moves the
c eigenvector columns in the same order.
	implicit none
	integer*4 i,j,k,n,np 
	real*4 D(NP),V(NP,NP),p
c
      DO 13 I=1,N-1
        K=I
        P=D(I)
        DO 11 J=I+1,N
          IF(D(J).GE.P)THEN
            K=J
            P=D(J)
          ENDIF
11      CONTINUE
        IF(K.NE.I)THEN
          D(K)=D(I)
          D(I)=P
          DO 12 J=1,N
            P=V(J,I)
            V(J,I)=V(J,K)
            V(J,K)=P
12        CONTINUE
        ENDIF
13    CONTINUE
      RETURN
      END
c****************************************************************************
	subroutine autot (iunit,fmt,sl,ss,nl,ns,iback,ohist,
     1 hist,fhist,mean,sigma,maxfreq,mindn,maxdn,median,mode,xsigma)
c	subroutine autot (iunit,iunitb,nids,fmt,sl,ss,nl,ns,iback,ohist,
c 	1 hist,fhist,mean,sigma,maxfreq,mindn,maxdn,median,mode,xsigma)
c
c	computes a general threshold for image
c	
	implicit none
	integer*4 inpbuf(32767)
	integer*4 iunit,sl,ss,nl,ns,maxfreq,mindn,maxdn,npts,i
	integer*4 sumcts,iback,npixels,jmaxdn,jmean
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65538:65537)
	real*4 mean,sigma,median,mode,xsigma
	logical*4 medianfound
	character*8 fmt
	character*100 outline
c
	medianfound=.false.
	npts=nl*ns	
	if (fmt.eq.'BYTE') then
		call comphist(iunit,sl,ss,nl,ns,iback,ohist,
     1	inpbuf)
		call histat(ohist,npts,mean,sigma,maxfreq)
		mode=0.0
		sumcts=ohist(0)
		median=0.0
		do i=1,255
			if (ohist(i).eq.maxfreq) mode=i
			if (.not.medianfound) then
				sumcts=sumcts+ohist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
			if (ohist(i).ne.0) go to 10
		enddo
		write (outline,10100)
10100 format ('**SUBROUTINE AUTOT -  All values zero in minimum search')
		call xvmessage (outline,' ')
		call abend
c
10	continue
		mindn=i
		do i=255,1,-1
			if (ohist(i).ne.0) go to 20
		enddo	
		write (outline,10100)
		call xvmessage (outline,' ')
		call abend
c
20	continue
		maxdn=i
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma1 (ohist,mindn,jmean,jmaxdn,npixels,xsigma)
		return
	endif
c
	if (fmt.eq.'HALF') then
		call comphist2(iunit,sl,ss,nl,ns,iback,hist,
     1	inpbuf)
		call histat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
		mode=hist(-32768)
		sumcts=hist(-32768)
		median=0.0
		do i=-32768,32767
			if (hist(i).eq.maxfreq) mode=i
			if (.not.medianfound) then
				sumcts=sumcts+hist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
		enddo
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma2 (hist,mindn,jmean,jmaxdn,npixels,xsigma)
	endif
	if (fmt.eq.'FULL') then
		call comphist4(iunit,sl,ss,nl,ns,iback,fhist,
     1	inpbuf)
		call histat4(fhist,npts,mean,sigma,mindn,maxdn,maxfreq)
		mode=fhist(-65536)
		sumcts=fhist(-65536)
		median=0.0
		do i=-65538,65537
			if (fhist(i).eq.maxfreq) then
				mode=i
			endif
			if (.not.medianfound) then
				sumcts=sumcts+fhist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
		enddo
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma4 (fhist,mindn,jmean,jmaxdn,npixels,xsigma)
	endif
	return	
	end
c=============================================================================
	SUBROUTINE COMPHIST(iunit,SL,SS,NL,NS,iback,ohist,inpbuf)
C Compute a histogram of a byte image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c	and subtracts background if required
c
	implicit none
      INTEGER*4 OHIST(256)
      INTEGER*4 INPBUF(32768)		!only difference from library routine
      INTEGER*4 SL,SS,EL,nl,ns,iunit,iback
	integer*4 line,ind

      CALL ZIA(OHIST,256)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         CALL HSUB(1,NS,INPBUF,OHIST)
      ENDDO

      RETURN
      END
c============================================================================
	SUBROUTINE COMPHIST2(iunit,SL,SS,NL,NS,iback,hist,inpbuf)
C Compute a histogram of a halfword image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c
	implicit none
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 INPBUF(32768)		!only difference from library routine
      INTEGER*4 SL,SS,EL,nl,ns,iunit,iback,line,ind,j,idn

      CALL ZIA(HIST,65536)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS

            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
c=============================================================================
	SUBROUTINE COMPHIST4(iunit,SL,SS,NL,NS,iback,hist,inpbuf)
C
C Compute a histogram of a halfword image.
c	modeled after comphist2 in standard vicar subroutine library
c	limited to +/- 65535 for unsigned 16-bit CCD work
C
	implicit none
      INTEGER*4 HIST(-65536:65535)
      INTEGER*4 INPBUF(32767)
      INTEGER*4 SL,SS,nl,ns,EL,line,j,iunit,ind,idn,iback
c
      CALL ZIA(HIST,131072)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS
            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
c============================================================================
      SUBROUTINE histat4(FHIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C
C   Compute mean and standard deviation (for halfword inputs only).
C
c	modified standard routine histat2
c
	implicit none
	integer*4 mindn,maxdn,maxfreq,npts,j,npixels
      INTEGER*4 FHIST(-65536:65535)
      REAL*4 MEAN,SIGMA,dn
      REAL*8 DMEAN,DSIGMA
c
      DN = 0.0			!DN value
      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MINDN = 65535
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN		!Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; set max. and min. DN to 
	MAXDN = MINDN		!32767; then exit program.
        GOTO 15
      ENDIF 

      DO 10 J=-65536,65535
      NPIXELS = FHIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.LT.MINDN) MINDN=J
      IF (J.NE.-65536.AND.J.NE.65535.AND.NPIXELS.GT.MAXFREQ)
     &		 MAXFREQ=NPIXELS
   10 CONTINUE

      MAXDN = DN + 0.5
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
c==========================================================================
	subroutine newsigma1 (ohist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 ohist(256),temphist(256)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=ohist(i)
	enddo
	do i=midval+1,highval
		index=midval-i
		temphist(i)=ohist(midval+index)
	enddo

	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c==========================================================================
	subroutine newsigma2 (hist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 hist(-32768:32767),temphist(-32768:32767)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=hist(i)
	enddo
	do i=midval+1,highval
		index=midval-i
		temphist(i)=hist(midval+index)
	enddo

	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c==========================================================================
	subroutine newsigma4 (fhist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 fhist(-65536:65535),temphist(-65536:65535)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c	character*80 outline
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=fhist(i)
	enddo
	do i=midval+1,highval
		index=i-midval
		index=midval-index
		temphist(i)=fhist(index)
c		write (outline,10010) i, index, midval+index
c10010 format (i6,2x,i6,2x,i6)
c		call xvmessage(outline,' ')
	enddo
	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c===========================================================================
	subroutine putparm (numobj)
c
	implicit none
	integer*4 numobj
	integer*4 parb(500),xcont,xadd,stat
c
	call xqini (parb,500,xcont)
	call xqintg (parb,'OBJECTS',1,numobj,xadd,stat)
	call xvqout (parb,stat)
	call chkstat (stat,' xvqout error', 1,0,0)    !ABEND if error.
c
	return
	end
c===========================================================================
	subroutine scopen (unit,ibis,columns,rows,ibfmt,iborg,ttype,
     1 tunits,scunits,scunum,fullrecord,realrecord,asciirecord,
     2 nfull,nreal,nascii,openmode,scversion)
c
c	version - 1/21/95
c	a general routine to open starcat catalog in ibis2 format
c	for 'READ', 'WRITE, or 'UPDATE'
c
c The starcat catalog is an IBIS-2 tabular file which has the following format.
c	for type 1
c
c	starcat catalog type is declared in variable scversion
c
c	type 1 has 20 columns
c	***************************************************************
c	descriptor	description		format		units
c	entry		catalog entry number	full(1)		<none>		
c	DN-bkg		sum of DNs - background	real(1)		counts
c	tot pix		total pixel count 	full(2)		<none>
c	sl		starting line		full(3)		<none>
c	ss		starting sample		full(4)		<none>
c	nl		number of lines		full(5)		<none>
c	ns		number of samples	full(6)		<none>
c	centsamp	center sample		real(2)		<none>
c	centline	center line		real(3)		<none>
c	maj axis	major axis length	real(4)		<none>
c	min axis	minor axis length	real(5)		<none>
c	ratio		ratio minor/major axis	real(6)		<none>
c	rotangle	rotation angle of ellip	real(7)		degrees
c	catalog		source catalog		ascii(1)-'A16'	<none>
c	ID		catalog ID		ascii(2)-'A20'	<none>
c	RA		right ascension		real(8)		hours
c	dec		declination		real(9)		degrees
c	mag calc	calculated magnitude	real(10)	<none>
c	mag ref		catalog magnitude	real(11)	<none>
c	class		classification		ascii(3)-'A8'	<none>
c	***************************************************************
c
c	type 2 adds 1 more column:
c	sat pix	# saturated pixels		full(7)   <none>
c	***************************************************************
c
c	type 3 adds 4 more columns:
c	***************************************************************
c	descriptor 	description		format		units
c	inten		intensity		full(8)		<none>		
c	eccent		eccentricity		real(12)	<none>
c	stddev		standard deviation	real(13)	<none>
c	amplitud	amplitude		real(14)	<none>	
c	***************************************************************
c
	implicit none
c
	integer*4 unit,ibis,columns,rows,stat,count,abendcode
	integer*4 fullrecord,realrecord,asciirecord,ibis_file_get
	integer*4 ibis_group_new,scversion,i,j,k,l
	integer*4 ibis_column_find,collist(50),num,scunum,sctnum
	integer*4 nfull,nreal,nascii,iscptr
	character*5 ibfmt(columns)
	character*8 ttype(columns),openmode,sctypes(50)
	character*16 iborg,tunits(columns),scunits(50)
	character*16 cattype,catfile,tempunit
	character*80 outline
c
	data catfile/'STARCAT TYPE   1'/
c
	abendcode=0
	iborg='ROW'
c
	if (openmode(1:4).eq.'READ'.or.openmode(1:6).eq.'UPDATE') then
		call ibis_file_open (unit,ibis,openmode,columns,rows,ibfmt,
     1	 iborg,stat)
		if (stat.ne.1) call ibis_signal_u(unit,stat,1)
		count=ibis_file_get(ibis,'nr',rows,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'nc',columns,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'type',cattype,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		if (catfile(1:7).ne.cattype(1:7)) then
			write (outline,10100) cattype
10100 format ('Catalog of type ',a16,' not starcat catalog')
			call xvmessage (outline,' ')
			call abend
		endif
		read (cattype(14:16),10110) scversion
10110 format (i3)
		count=ibis_file_get(ibis,'formats',ibfmt,1,columns)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		scunum=ibis_file_get(ibis,'units',scunits,1,columns)
		if (scunum.lt.0) call ibis_signal(ibis,scunum,abendcode)
		do i=1,scunum
			num=ibis_column_find(ibis,'unit',scunits(i),
     1		collist,1,50)
			if (num.lt.0) call ibis_signal(ibis,num,abendcode)
			do j=1,num
				tunits(collist(j))=scunits(i)
			enddo
		enddo
		count=ibis_file_get(ibis,'groups',ttype,1,columns)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'org',iborg,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		nfull=0
		nreal=0
		nascii=0
		nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
		if (nfull.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,fullrecord,'format:full',0,0,
     1	 'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
		if (nreal.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,realrecord,'format:real',0,0,
     1	 'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
		if (nascii.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,asciirecord,'format:ascii',0,0,
     1	 'a20',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
	endif
c
	if (openmode(1:5).eq.'WRITE') then
		call ibis_file_open(unit,ibis,'WRITE',columns,rows,
     1	ibfmt,iborg,stat)
		if (stat.ne.1) call ibis_signal_u (unit,stat,1)
		if (scversion.ne.1) then
			write(outline,10110) scversion
			catfile(14:16)=outline(1:3)
		endif
		call ibis_file_set(ibis,'type',catfile,stat)
		if (stat.lt.0) call ibis_signal(ibis,stat,abendcode)
		nfull=0
		nreal=0
		nascii=0
		nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
		if (nfull.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,fullrecord,'format:full',0,0,
     1	 'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
		if (nreal.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,realrecord,'format:real',0,0,
     1	 'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
		if (nascii.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,asciirecord,'format:ascii',0,0,
     1	 'a20',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
c
c  check if inherit properties of existing catalog
c
		sctnum=ibis_file_get(ibis,'groups',sctypes,1,columns)
		if (sctnum.lt.0) call ibis_signal(ibis,sctnum,abendcode)
		if (sctnum.eq.0) then		!only do if no inherit
c**			write (outline,10150) sctnum
c**10150 format ('Subroutine SCOPEN - Number of starcat types = ',i3)
c**			call xvmessage (outline,' ')
c**			call abend
			do i=1,columns
			    count=ibis_group_new(ibis,'group',ttype(i),i,
     1			1,' ')
			    if (count.lt.0) call ibis_signal(ibis,count,
     1			abendcode)
			enddo
		endif
		scunum=ibis_file_get(ibis,'units',scunits,1,columns)
		if (scunum.lt.0) call ibis_signal(ibis,scunum,abendcode)
c
c   The following test is put in here to set if new catalog inherits
c   properties of an existing catalog, i.e., INP=<old catalog>,
c   OUT=<new catalog> if xvselpi is not done.
c
		if (scunum.gt.0) then
			do i=1,scunum
				num=ibis_column_find(ibis,'unit',scunits(i),
     1			collist,1,50)
				if (num.lt.0) call ibis_signal(ibis,num,
     1			abendcode)
				do j=1,num
					tunits(collist(j))=scunits(i)
				enddo
			enddo
		elseif (scunum.eq.0) then
		    	tempunit=tunits(1)
			i=1
			iscptr=1
50	continue
			scunits(iscptr)=tempunit
			collist(1)=i
			num=1
			do k=i+1,columns
			    if (scunits(iscptr).eq.tunits(k)) then 
				num=num+1
				collist(num)=k
			    endif
			enddo	
			count=ibis_group_new(ibis,'unit',scunits(iscptr),
     1		collist,num,' ') 
			if (count.lt.0) call ibis_signal(ibis,count,abendcode)
			do l=1,columns
			    collist(l)=0
			enddo
70	continue
			i=i+1
			if (i.gt.columns) go to 80
			do j=1,iscptr
			    if (tunits(i).eq.scunits(j)) go to 70
			    do k=j,iscptr
				if (tunits(i).eq.scunits(k)) go to 70
			    enddo
			enddo
			tempunit=tunits(i)
			iscptr=iscptr+1
			go to 50
		endif
80	continue
	endif
c
	return
	end
c**************************************************************************
	block data
c
c	catalog in ibis-2 format
c	type 1 has 20 columns - type 2 has 21
c
	common /catalog/ttype,ibfmt,tunits
c
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
c
	data ttype(1) /'Entry   '/
	data ttype(2) /'DN-bkg  '/
	data ttype(3) /'tot pix '/
	data ttype(4) /'sl      '/
	data ttype(5) /'ss      '/
	data ttype(6) /'nl      '/
	data ttype(7) /'ns      '/
	data ttype(8) /'centsamp'/
	data ttype(9) /'centline'/
	data ttype(10)/'maj axis'/
	data ttype(11)/'min axis'/
	data ttype(12)/'ratio   '/
	data ttype(13)/'rotangle'/
	data ttype(14)/'Catalog '/
	data ttype(15)/'ID      '/
	data ttype(16)/'RA      '/
	data ttype(17)/'dec     '/
	data ttype(18)/'mag calc'/
	data ttype(19)/'mag ref '/
	data ttype(20)/'class   '/
	data ttype(21)/'sat pix '/
c
	data ibfmt(1) /'FULL'/			!entry
	data ibfmt(2) /'REAL'/			!sum (DN-bkg)
	data ibfmt(3) /'FULL'/			!sum pixels
	data ibfmt(4) /'FULL'/			!sl
	data ibfmt(5) /'FULL'/			!ss
	data ibfmt(6) /'FULL'/			!nl
	data ibfmt(7) /'FULL'/			!ns
	data ibfmt(8) /'REAL'/			!centroid sample
	data ibfmt(9) /'REAL'/			!centroid line
	data ibfmt(10)/'REAL'/			!major axis length
	data ibfmt(11)/'REAL'/			!minor axis length
	data ibfmt(12)/'REAL'/			!minor/major axis ratio
	data ibfmt(13)/'REAL'/			!rotation angle
	data ibfmt(14)/'A16 '/			!catalog
	data ibfmt(15)/'A20 '/			!catalog identifier
	data ibfmt(16)/'REAL'/			!right ascension
	data ibfmt(17)/'REAL'/			!declination
	data ibfmt(18)/'REAL'/			!calculated magnitude
	data ibfmt(19)/'REAL'/			!reference magnitude
	data ibfmt(20)/'A8  '/			!class (star,galaxy,asteroid,
						!planet,moon,stdstar,comet,tstdstar)
	data ibfmt(21)/'FULL'/			!# of saturated pixels
c
	data tunits(1) /'<none>          '/
	data tunits(2) /'counts          '/
	data tunits(3) /'<none>          '/
	data tunits(4) /'<none>          '/
	data tunits(5) /'<none>          '/
	data tunits(6) /'<none>          '/
	data tunits(7) /'<none>          '/
	data tunits(8) /'<none>          '/
	data tunits(9) /'<none>          '/
	data tunits(10)/'<none>          '/
	data tunits(11)/'<none>          '/
	data tunits(12)/'<none>          '/
	data tunits(13)/'degrees         '/
	data tunits(14)/'<none>          '/
	data tunits(15)/'<none>          '/
	data tunits(16)/'hours           '/
	data tunits(17)/'degrees         '/
	data tunits(18)/'<none>          '/
	data tunits(19)/'<none>          '/
	data tunits(20)/'<none>          '/
	data tunits(21)/'<none>          '/
c
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create starcat3.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM starcat3

   To Create the build file give the command:

		$ vimake starcat3			(VMS)
   or
		% vimake starcat3			(Unix)


************************************************************************/


#define PROGRAM	starcat3
#define R2LIB

#define MODULE_LIST starcat3.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport errdefs

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create starcat3.pdf
process help=*
LOCAL OBJS TYPE=INTEGER COUNT=1
PARM INP  TYPE=STRING  
PARM OUT  TYPE=STRING  COUNT=(1:3) 
PARM SL   TYPE=INTEGER DEFAULT=1
PARM SS   TYPE=INTEGER DEFAULT=1
PARM NL   TYPE=INTEGER DEFAULT=0
PARM NS   TYPE=INTEGER DEFAULT=0
PARM SIZE TYPE=INTEGER COUNT=4     DEFAULT=(1,1,0,0)
PARM LIST TYPE=KEYWORD COUNT=(0:1) VALID=(LIST,NOLIST) DEFAULT=--
PARM BACK TYPE=INTEGER COUNT=(0:1) DEFAULT=0
PARM THRE TYPE=INTEGER COUNT=1
PARM MAXD TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM SPIKE TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM LOVAL TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM MINI TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM MAXI TYPE=REAL COUNT=(0:1) DEFAULT=1.0E38
PARM MINP TYPE=INTEGER COUNT=(0:1) DEFAULT=4
PARM MAXP TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM MINS TYPE=INTEGER COUNT=(0:1) DEFAULT=2
PARM MAXS TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM MINL TYPE=INTEGER COUNT=(0:1) DEFAULT=2
PARM MAXL TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM OBJECTS TYPE=NAME DEFAULT=OBJS
!!!!!!!PARM AUTO TYPE=KEYWORD COUNT=(0:1) VALID=(AUTO,NOAUTO) DEFAULT=--
!!!!!!!PARM SENS TYPE=REAL COUNT=(0:1) DEFAULT=1.0
PARM ASTEROID TYPE=KEYWORD VALID=(ASTEROID,NOAST) DEFAULT=NOAST 
PARM SAOMINP TYPE=INTEGER DEFAULT=400
PARM ASTMINP TYPE=INTEGER DEFAULT=35
PARM SAORATIO TYPE=REAL   DEFAULT=0.7
PARM ASTRATIO TYPE=REAL   DEFAULT=0.27
PARM ASTMAXW TYPE=REAL   DEFAULT=1.6
PARM ASTMAXL TYPE=REAL   DEFAULT=35.
END-PROC
.TITLE
VICAR PROGRAM STARCAT3
.HELP
PURPOSE
  STARCAT3 is a VICAR applications program which locates and integrates
  features consisting of contiguous pixels above a given threshold in 
  an image and places the results into a catalog file.  A primary use of
  STARCAT3 is to locate and catalog stars or other objects in astronomy
  imaging or camera calibration.


OPERATION

  STARCAT3 locates and associates all contiguous pixels above a user
  specified threshold into single objects.
  
  STARCAT3 uses the first moment of the marginal distribution with a static
  threshold to detect objects. The marginal distribution is the algorthim
  described in Auer and Altena (Reference 1) with the modification described
  by Stone (Reference 2).

  The AUTO and SENS parameters have been disabled because of difficulties,
and the THRE parameter is now required.

DATA FILES

  The input image is a starfield. It may be from any type of sensor such
  as a CCD, vidicon, or digitized photographic plate or film.  (The input
  image may contain experimentally produced light sources, as in camera
  calibration.)
  Valid formats are BYTE, HALF and FULL.

  The primary output is a 21 column IBIS-2 tabular file catalog described 
  in detail below. This catalog format is not compatible with the
  catalogs produced by STARCAT and STARCAT2. Those catalogs were based
  on a non-transportable format to UNIX systems (because it equivalenced
  real and integer variables). The catalog produced by STARCAT and STARCAT2
  is described below for illustrative purposes.
  
  An optional second output data set can be produced where located
  features are highlighted. This data set will have the same dimensions
  as the original data set but will always be of BYTE data type. The data
  set will have only two values, 0 and 255, where detected features are
  at DN=255 (white) with all data below the threshhold as DN=0 (black).
  THis data set is intended to approximately reflect the actual results,
  the primary output (star catalog).  Occasionally the second data set
  includes rejected objects (that are not containe in the primary output.)
  The second data set will not show portions of objects where there are
  less than "the MINS parameter" number of pixels in a given line.
  This is because of algorithmic limitations.  (Pixels in the second data
  set are marked (line by line) right after the input image lines are
  scanned, which is before most object rejection criteria can be applied.
  The program, as an approximation, marks groups of consecutive pixels
  (in an image line) containing at least MINS pixels.)
  
  An optional third output is a halfword data set having the object numbers
  found on each line as they are found before rejection criteria are applied
  This is the so-called the flag buffer output.


PROGRAM PARAMETERS

  Parameters are provided to manage program flow and to provide object 
  rejection criteria. The use of the rejection parameters can be highly
  subjective and depends heavily on the scene content. For this reason
  the second and third output files provide the user a visual examination
  of the effectiveness of STARCAT3.

  STARCAT3 is normally used to detect stars in an astronomical observation.
  The sensors used have been vidicons, CCDs and digitized photographic 
  negatives. Vidicon and CCD data sets are normally radiometrically 
  corrected before using STARCAT3 by subtracting darks, biases and dividing
  by flat fields.

  Optimal results requires the image to have as flat a background in the data
  set as possible since only one threshhold value is allowed for STARCAT3 --
  You do not want to detect objects which are really only due to subtle
  variations in the background noise. 

  Alternatively, should the user have no dark file, flatfield to correct
  the data, the user can subtract a constant background value with the
  parameter BACK. BACK is a value corresponding to the median dn value
  of the histogram of the image. BACK is important for the summation of the
  dn values within the object to make magnitude estimations.

  Photographic negatives which have been digitized for this operation
  typically do not have background data sets. If you have a
  particularly noisy, grainy or spotty photographic negative, the
  user can generate a pseudo background by one of two methods: 
  1) Run VICAR program MEDIAN on the original data set with a very large
  kernel - something on the order of 300 x 300 which will remove high
  frequency data (the stars) or with a kernel 4 to 5 times the size of the
  largest real object. Then subtract the pseudo background from the
  orginal data with F2.
  VICAR program MEDIAN is not too swift since it is a sorting algorithm
  and can take many minutes to run on a large (2K x 2K is large in this sense)
  data set.
  2) Run VICAR program SIZE with the interpolate option on the original
  data down to between 4 to 64 times smaller than the original data set and
  and then expand the data set with program SIZE back up to its original size
  with the interpolate option.

  BACK PARAMETER

  BACK specifies the true zero intensity level in DNs of the input image.
  It is usually the median value of the histogram of the corrected image data. 
  VICAR programs HIST or ORDSTAT provides the mean
  as a returnable parameter in a PDF. Note that HIST returns the mean as
  a real variable and ORDSTAT returns it as an integer. Since starcat3
  expects integer values for BACK then if HIST is used to provide an input
  to STARCAT3 it should be converted float to integer within the pdf.

  Object intensities are computed after background subtraction as:

                 I=SUM(DN    -BACK)
                    ij   INij

  Default; BACK=0.

  PARAMETERS THRE, AUTO, SENS

  The selection of a good threshold is the key to successful STARCAT3
  execution. If you only wish to see the brightest objects then the
  threshold can be somewhat arbitrary, but if you wish to detect fainter
  objects then care must be taken and it becomes more and more important
  to have a flattened background.

  THRE must be entered.

  A good place to start is some multiple of the standard deviation from
  the histogram of the image. A reasonable value to start with is about
  3 sigma above the mean.

  Alternatively, the user can display the image and select a DN value
  that lies somewhat above background and start from there.

The AUTO and SENS parameters have been disabled because of difficulties.
The associated documentation is preserved as background information.
------
  If KEYWORD "AUTO" is specified then the program computes a local
  threshold based upon the average DN in the image plus the standard 
  deviation (sigma).  The sensitivity factor, SENS, is the number of
  sigma to multiply by according to the equation 

	autothreshold = mean + sensitivity*std_deviation

  If you start with the AUTO option then choose SENS=3.0. Default is 1.0
  though.
-----------

  MAXD PARAMETER

  MAXD is the saturation value in the image. By default in STARCAT3
  a BYTE input data, MAXD is set to 255, a HALF data set it is set to 32767
  and a FULL data set it is to 2,147,483,647. However, not all sensors
  have dynamic ranges this high for a given data format. MAXD gives the
  user the ability to set the correct saturation value for the sensor.

  Internally MAXD is the highest value that STARCAT3 will accept for a
  valid object. Note that SPIKE (below) indicates that if a pixel of SPIKE
  or higher is detected within an object boundary then that object is to be
  ignored in generating the starcat catalog.  Internally STARCAT3 sets a
  value for SPIKE to be 1 DN above the saturation value unless the saturation
  value is the highest integer value for the data type. For example, if the
  image format is 'BYTE' and the saturation value MAXD is 255, then any
  value entered for SPIKE will be ignored.

  The same thing holds true if the data format is 'HALF' and the saturation
  value is 32767 or if the data format is 'FULL' and the saturation value
  is 65535. However, if the user enters a MAXD value less than the highest
  value for the data format and less than the SPIKE value then
  the program will reset the MAXD value to 1 less than the SPIKE value.
  (It assumes that the value the user gives for SPIKE is correct).

  The check for saturation is applied before background subtraction
  since saturation is independent of background.

  STARCAT3 will report the number of pixels within an object that have 
  reached the saturated value in column 21. 

  SPIKE AND LOVAL PARAMETERS (OBJECT REJECTION CRITERIUM and bad pixel handling)

  These two values are not exactly complementary as are the four pairs
  below. So read and understand the following.

  SPIKE is the value which, if a DN value is detected which is greater or
  equal to SPIKE is found within the boundary of an object, it will cause
  the object to be rejected as a valid object. Note the SPIKE value
  comparison is applied before background subtraction. 

  LOVAL is the value which, if a DN value is detected which is lower than
  LOVAL within the boundary of an object, it will cause the DN value to be
  replaced by THRE in the summation of the DNs within the object. This will
  prevent an artificially low DN value (such as a bad pixel value) from being
  subtracted from the value to be stored in column 2 of the catalog.


  OBJECT REJECTION CRITERIA
  
  The other parameters associated with the rejection of objects are
  complementary:
  MINI, MAXI - Intensities of objects, minimum and maximum
  MINP, MAXP - Number of Pixels (Area), minimum and maximum
  MINL, MAXL - Number of Lines of object, minimum and maximum
  MINS, MAXS - Number of Samples of object, minimum and maximum

  If the value is greater than the maximum the object is rejected. If the
  value is less than the minimum the object is also rejected.
 
  LIST PARAMETER

  The optional KEYWORD "LIST" allows the catalog to be printed to
  the screen or session log as the catalog is being created.

  OBJECTS PARAMETER
 
  The program will return the number of objects found in this parameter.


  ASTEROID PARAMETERS

  The KEYWORD "ASTEROID" causes the following 6 keywords to be activated:
  SAOMINP,ASTMINP,SAORATIO,ASTRATIO,ASTMAXW,ASTMAXL

  The ASTEROID parameter was designed for streak detection on old Palomar
  48-inch plates. Those plates were exposed long enough such that asteroids
  would show up as long streaks and these parameters allow automated tentative
  identification.  When the ASTEROID option is activated the catalog will
  add "asteroid" or "star" in column 20 (classification) and "starcat3
  for column 14 (catalog).

ALGORITHM

  Objects are associated together by maintaining a flag buffer (one line) 
  which contains zeroes except where 'an above threshold event' has occured.
  Event pixels are set to a unique ID value or flag. Internally the program
  allows up to 2000 objects to be active for two lines. This value is
  maxobjs and is a parameter in subrouine centfind. When the next image
  line is processed and a section through an object is located, each
  pixel in that section is compared with the three closest values in
  the flag buffer for the preceeding line. If the flag has been set
  to a non-zero ID, that ID is propagated downward and the object
  section is integrated in with that ID. If the section spans more
  than an ID, the IDs are concatenated. Sections which have no
  flagged values in the preceeding line are considered new objects
  and flagged values which are not propagated downwards are considered
  complete. Catalog entries are made only after an object is completed.
  Parameters are provided to restrict the areas, intensities, and 
  dimensions of objects which are placed into the catalog. 


STARCAT CATALOG
  
  Information about each object is entered into a catalog which
  contains 21 descriptors for each entry. Each entry is stored in the
  order as they were completed. The catalog is organized  as follows:


  The starcat catalog is an IBIS-2 tabular file which has the following format.
  (Note that the centsamp column comes before the centline column.)

	***************************************************************
    	descriptor	description		     format		units
	----------	-----------		     ------		-----
      1	entry     catalog entry number	full(1)		<none>		
      2	DN-bkg    sum of DNs - background	real(1)		counts
      3	sumpix    number of pixels 	     full(2)		<none>
      4	sl		starting line		     full(3)		<none>
      5	ss		starting sample          full(4)		<none>
      6	nl		number of lines		full(5)		<none>
      7	ns		number of samples        full(6)		<none>
      8	centsamp	center sample            real(2)		<none>
      9	centline	center line              real(3)		<none>
     10	maj axis	major axis length        real(4)		<none>
     11	min axis	minor axis length        real(5)		<none>
     12	ratio     ratio minor/major axis	real(6)		<none>
     13	rotangle  rotation angle of ellip	real(7)		degrees
     14	catalog   source catalog           ascii(1)-'A16'	<none>
     15	ID		catalog ID               ascii(2)-'A20'	<none>
     16	RA		right ascension          real(8)		hours
     17	dec		declination              real(9)		degrees
     18	mag calc	calculated magnitude     real(10)       <none>
     19	mag ref   catalog magnitude        real(11)       <none>
     20	class     classification           ascii(3)-'A8'	<none>
     21    satpix    number of saturated pix  full(7)        <none>
	***************************************************************

  Prior to Jan 21, 1995, STARCAT3 produced STARCAT catalogs of TYPE 1
  which had only the first 20 columns. However, the addition of column
  21 allows better analysis of starfield for use in other VICAR Astrometry
  and photometry programs.
  

  STARCAT3 fills in columns 1 to 13 and 21 for each object it detects.
  The major and minor axis lengths are somewhat proportional to (but much less
  than) the measurements of an elliptical approximation of the object.
  If the ASTEROID parameter is active it will fill in columns 14 and 20.
  Column 14 (catalog) will contain "starcat3" and column 20 (class)
  will contain either "asteroid" or "star".
 

OLDER STARCAT and STARCAT2 catalogs

  The format of the catalogs created by STARCAT and STARCAT2 are as follows:
  One record is 48 bytes. These programs use an unportable equivalencing
  of fortran real*4 and integer*4 data types.

  WORD #  CONTENTS        	TYPE	COMMENTS

    1     SUM of DN - BKG      I*4  represents total intensity of object
    2     SUM pixels      	I*4	area covered by object
    3     SL area         	I*4	with respect to size field
    4     SS area         	I*4	with respect to size field
    5     NL area         	I*4	with respect to size field
    6     NS area         	I*4	with respect to size field
    7     Centroid sample 	R*4	center of object in sample
    8     Centroid line   	R*4	center of object in line
    9     major axis length  	R*4	major axis length - right ascension
   10     minor axis length  	R*4	minor axis length - declination
   11     Used for ASTEROID    R*4     1.0= SAO star, 2.0= asteroid
   12     rotation angle	     R*4	about major axis, degrees

There is no delivered program to interconvert the catalogs at this time.
See the cognizant programmer for current information on related programs,
ORDSTAT,...

DIFFERENCES FROM STARCAT2

Besides differences in the catalog format, there are a few differences
between starcat2 (unported program) and starcat3.
  1.  Use of a second input file for a background data set was dropped
      because of too many problems.  See the cognizant programmer for 
      alternatives.
  2.  The TBL parameter was dropped.  It was available in starcat2 to
      compensate for non-linear imaging systems, which are no longer common.
  3.  The computation for SUM of DN - BKG is slightly different when there
      are saturated pixels (dn >= MAXD).  For saturated pixels, starcat3
      does not subtract the background, but the background was subtracted
      in starcat2.

LIMITATIONS:

  1. Maximum of 2000 objects for the flag buffer.  (This means a maximum of
     2000 objects on any one line of the image.)
  2. Input data files are limited to BYTE, HALF and FULL.
  3. Maximum number of samples (pixels) per line is 32767.

PRECISION
  See under DIFFERENCES FROM STARCAT2.

REFERENCES

  1. L.H. Auer and W.F. van Altena, The Astronomical Journal, Vol83, 531 (1978)
  2. Ronald C. Stone, The Astronomical Journal, Vol 97, 1227 (1989)


ERROR MESSAGES

XVunit error on 1st input file
XVopen error on 1st input file
XVopen error on output catalog file
XVunit error on 2nd output image
XVopen error on 2nd output image
XVunit error on 3rd output image
XVopen error on 3rd output image

1) Format XXXXXX not supported by STARCAT3
	- program does not support REAL, DOUB or COMP images

2) **SUBROUTINE FIND - All 2000 bins filled
	- exceeded maximum number of objects per line (2000) - the only
	  way to change this limit is to change parameter maxobjs in the
	  main routine, recompile and relink the program..

3) **SUBROUTINE AUTOT -  All values zero in minimum search

4) Catalog of type ',a16,' not starcat catalog
	- Cannot be fixed

PRECISION
  The agreement of results from tststarcat3.pdf between different makes of
computers is limited to about six significant digits by the fact that the 
array sumdn in STARCAT3 and the variables MEAN and STDEV in IBISSTAT are
all single precision.  (Double precision can reduce disagreements if ever
an issue, especially in computations of sums, means, and standard deviations.)

PROGRAM HISTORY:
  30 Sep   1997...S.Pohorsky....Changed default for MAXI to 1.0E38.  Disabled
                                AUTO and SENS at Ray Bambery's suggestion.  As
                                a result, THRE is now a required parameter.
                                Changed ".eq." TO ".ge." in check for saturation
                                in CENTFIND.  Changed PDF default for MAXD to --
                                to match HELP.
                                Changed MAXI default from 4.0E9 to 1.0E38 to 
                                better allow for saturation in fullword images.

  20 Feb   1996...R.J.Bambery...Added OBJECTS return value
  22 Feb   1995...R.J.Bambery...Removed optional background data set as
						second input -- it caused too many coding
						problems
   6 Feb   1995...R.J.Bambery...Added SPIKE and LOVAL parameters - changed
						 PRINT to LIST to conform to other programs
						 in this series
  21 Jan	1995...R.J.Bambery...Changed catalogs to TYPE 2 by adding column
						 21 (# of saturated pixels). Improved flow
						 by better defining MAXD, MINI & MAXI
   5 Jan   1995...R.J.Bambery...parameterize maxobjs and set nbin to 2000
  19 Dec   1994...R.J.Bambery...remove error messages on UNIX systems
   2 Nov   1994...R.J.Bambery...No longer outlines objects in output image
				Makes detected objects white on black bkg
  26 Aug   1994...R.J.Bambery...Added ability to process FULL data sets
   5 Jul   1994...R.J.Bambery...Fixed bug in output byte file from half
				files for rmindn
  28 Mar   1994...R.J.Bambery...Completed port
  31 Jan   1994...R.J.Bambery...Made portable using new IBIS-2 format
				for catalog
   9 FEB   1993...R.J.Bambery...Fixed bug that didn't give correct NL to
				catalog
  28 DEC   1992...R.J.BAMBERY...Modified to run under UNIX
  15 AUG   1990...R.J.BAMBERY...Add AUTO and SENS parameters	
   1 JUL   1988...J.J.LORRE.....added 4 fields into catalog:
                                major,minor,angle values
                                Background file.
  06 FEB   1985...M.E.MORRILL...NOW EXTRACTS VICAR1 AND VICAR2 LABELS
  25 SEPT  1984...M.E.MORRILL...CONVERSION TO VAX VICAR*2
  23 AUG   1984...M.E.MORRILL...CONVERSION TO VAX VICAR1*
  13 OCT   1982...J.J.LORRE.....NEW KEYWORDS: MAXP,MINI,MAXI,MINL
                                MAXL,MAXS,LINE,OVER,UNDER
  23 AUG   1982...R.D.TOAZ......INCREASED EFFICIENCY OF DUPE REJECTION
  10 NOV   1978...J.J.LORRE.....ACCOMODATE VICAR-77 LABEL
  17 JAN   1978...J.J.LORRE.....REVISIONS AND DOCUMENTATION
  12 SEPT  1977...J.J.LORRE.....INITIAL RELEASE

EXECUTION:
  
  starcat3 INP=SP.IMG OUT=SP.CAT PARAMS

  starcat3 INP=SP.IMG OUT=(SP.CAT,SP.OUT) PARAMS

  starcat3 INP=SP.IMG OUT=(SP.CAT,SP.OUT,SP.FLAG) PARAMS

  starcat3 INP=SP.IMG OUT=(SP.CAT,SP.OUT,SP.FLAG) PARAMS

Parameters are defined in the TUTOR mode. SIZE is optional and
can be used to restrict the search area in the input image within
which objects are to be located.


.LEVEL1
.VARIABLE INP
 An input image.

.VARIABLE OUT
 Output 1 = CATALOG
 Output 2 = BYTE image of
 size equal to the input
 (all objects set to DN=255).
 Output 3 = "flag" image
 SEE HELP FOR CAT FORMAT.

.VARIABLE SIZE
 The standard Vicar size field.
 Restricts the search area

.VARIABLE SL
 Starting line of search area

.VARIABLE SS
 Starting sample of search area

.VARIABLE NL
 Number of lines in search area

.VARIABLE NS
 Number of samples in search area

.VARIABLE LIST
 KEYWORD-OPTIONAL
 Print catalog to session
 log/terminal.

.VARIABLE BACK
 INTEGER-OPTIONAL
 Background DN level. 
 Default; BACK=0.

.VARIABLE MAXD
 INTEGER-OPTIONAL
 Maximum DN for an object
 (Saturation)
 Defaults:
 MAXD=255 for BYTE images
 MAXD=32767 for HALF images
 MAXD=2147483647 for FULL images

.VARIABLE SPIKE
 INTEGER-OPTIONAL
 Reject object if DN value
 greater or equal to
 SPIKE value found in object

.VARIABLE LOVAL
 INTEGER-OPTIONAL
 If DN value < LOVAL found in
 object, replace it with 
 THRE value.

.VARIABLE AUTO
 KEYWORD-OPTIONAL
DISABLED.    Previously:
 Compute a global threshold for
 the objects
 If AUTO=AUTO then THRE is
 ignored.

.VARIABLE SENS
 REAL-OPTIONAL
DISABLED.    Previously:
 Number of sigma to add to the
 autothreshold if AUTO=AUTO
 Ignored if AUTO=NOAUTO
 Default; SENS=1.0

.VARIABLE THRE
 INTEGER-OPTIONAL
 Threshold DN for real objects.
 Lowest value in image format
 After background subtraction

.VARIABLE MINI
 REAL-OPTIONAL
 Ignore objects whose sums of 
 DNs are less than this value
 Default; MINI=-2,147,483,648

.VARIABLE MAXI
 REAL-OPTIONAL
 Ignore objects whose sums of
 DNs are greater than this
 value.
 Default; MAXI=1.0E38

.VARIABLE MINP
 INTEGER-OPTIONAL
 Object minimum area in pixels.
 Default; MINP=4.

.VARIABLE MAXP
 INTEGER-OPTIONAL
 Object maximum area in pixels.
 Default; MAXP=Infinity.
 (+2,147,483,647)

.VARIABLE MINL
 INTEGER-OPTIONAL
 Minimum lines in object.
 Default; MINL=2.

.VARIABLE MINS
 INTEGER-OPTIONAL
 Minimum samples in object
 Default; MINS=2.

.VARIABLE MAXL
 INTEGER-OPTIONAL
 Maximum lines in object.
 Default; MAXL=1000000

.VARIABLE MAXS
 INTEGER-OPTIONAL
 Maximum samples in object
 Default; MAXS=1000000

.VARIABLE OBJECTS
 NAME
 Return of number of
 objects found to
 script

.VARIABLE ASTEROID
 KEYWORD-OPTIONAL
 The Asteroid keyword
 causes the following 
 6 keywords to be activated.
 These are:
 SAOMINP,ASTMINP,SAORATIO,
 ASTRATIO,ASTMAXW,ASTMAXL

 VALID=(ASTEROID,NOAST)
 DEFAULT; NOAST 

.VARIABLE SAOMINP
 INTEGER-OPTIONAL
 Candidate STARCAT SAO stars
 must have at least SAOMINP
 pixels.
 DEFAULT; SAOMINP=400

.VARIABLE ASTMINP
 INTEGER-OPTIONAL
 Candidate STARCAT asteroids
 must have at least ASTMINP
 pixels.  
 DEFAULT; ASTMINP=35

.VARIABLE SAORATIO
 REAL-OPTIONAL
 Candidate STARCAT SAO stars
 must have at least a narrow
 axis to wide axis ratio of
 SAORATIO.
 DEFAULT; SAORATIO=0.7

.VARIABLE ASTRATIO
 REAL-OPTIONAL
 Candidate STARCAT asteroids
 must have less than a narrow
 axis to wide axis ratio of
 ASTRATIO.
 DEFAULT; ASTRATIO=0.27

.VARIABLE ASTMAXW
 REAL-OPTIONAL
 The maximum allowed width for
 an object to be considered an
 asteroid candidate in the
 STARCAT catalogue.
 DEFAULT; ASTMAXW=1.6

.VARIABLE ASTMAXL
 REAL-OPTIONAL
 The maximum allowed length for
 an object to be considered an
 asteroid candidate in the
 STARCAT catalogue.
 DEFAULT; ASTMAXL=35.

.LEVEL2
.VARIABLE INP
 An input image data set

.VARIABLE OUT
 Primary output is a CATALOG data set described in help.

 Secondary output is an image of size equal to the input with
 a DN=255 area for each object found.
 THis data set is intended to approximately reflect the actual results,
 the primary output (star catalog).  Occasionally the second data set
 includes rejected objects (that are not containe in the primary output.)
 The second data set will not show portions of objects where there are
 less than "the MINS parameter" number of pixels in a given line.
 This is because of algorithmic limitations.  (Pixels in the second data
 set are marked (line by line) right after the input image lines are
 scanned, which is before most object rejection criteria can be applied.
 The program, as an approximation, marks groups of consecutive pixels
 (in an image line) containing at least MINS pixels.)

 Third output is the flag buffer containing the object IDs of all objects
 found during its search before rejection criteria are applied. Thus, it
 contains real and suspected objects.

.VARIABLE SIZE
 The standard Vicar size field.
 Restricts the search area.
 Default; (1,1,0,0) - Full size of input image.

.VARIABLE SL
 Starting line of search area.
 Default;  1   

.VARIABLE SS
 Starting sample of search area.
 Default;  1

.VARIABLE NL
 Number of lines of search area.
 Default;  NL of input image

.VARIABLE NS
 Number of samples of search area.
 Default;  NS of input image

.VARIABLE LIST
 KEYWORD-OPTIONAL
 Prints out catalog information for each object found on the terminal
 and the session log.

.VARIABLE MAXD
 INTEGER-OPTIONAL
 Specifies maximum input DN in the input image. The saturation value.
 32,767 for halfword images.
 2,147,483,647 for fullword images.
 Default; MAXD=255.

.VARIABLE SPIKE
 INTEGER-OPTIONAL
  SPIKE is the value which, if a DN value is detected which is greater or
  equal to SPIKE is found within the boundary of an object, it will cause
  the object to be rejected as a valid object. Note the SPIKE value
  comparison is applied before background subtraction. 

.VARIABLE LOVAL
 INTEGER-OPTIONAL
  LOVAL is the value which, if a DN value is detected which is lower than
  LOVAL within the boundary of an object, it will cause the DN value to be
  replaced by THRE in the summation of the DNs within the object. This will
  prevent an artificially low DN value (such as a bad pixel value) from being
  subtracted from the value to be stored in column 2 of the catalog.

.VARIABLE AUTO
 KEYWORD-OPTIONAL
DISABLED.    Previously:
 Computes a global threshold value. If set to AUTO=AUTO, ignores THRE and
 uses SENS parameter. 

.VARIABLE SENS
 REAL-OPTIONAL
DISABLED.    Previously:
 Number of sigma to add to the mean to get the autothreshold
 Ignored if AUTO=NOAUTO.
 Default; SENS=1.0 

.VARIABLE THRE
 INTEGER-REQUIRED
 Specifies the DN in input image at and above which pixels are
 considered real events. All DNs below THRE are ignored.

.VARIABLE BACK
 INTEGER-OPTIONAL
 Specifies the true zero intensity level in DNs of the
 input image. BACK is subtracted from each object DN.
 Object intensities are

 computed as:   I=SUM(DN   -BACK )
                   ij   INij

 Default; BACK=0.

.VARIABLE MINP
 INTEGER-OPTIONAL
 Specifies the least number of pixels which an object can 
 contain and still be considered a real object. All objects
 with less than MINP pixels are marked in optional output
 data set but are not entered in the Catalog.
 Default; MINP=4.

.VARIABLE MAXP
 INTEGER-OPTIONAL
 Specifies the maximum number of pixels that an object may
 contain and still be placed in the catalog.
 Default; MAXP=Infinity (2,147,483,647).

.VARIABLE MINI
 REAL-OPTIONAL
 Ignore objects whose sums of  DNs are less than this value
 Default; MINI=-Infinity.
 (-2,147,483,648)

.VARIABLE MAXI
 REAL-OPTIONAL
 Ignore objects whose sums of DNs are greater than this value.
 Default; MAXI=Infinity (1.0E38).

.VARIABLE MINL
 INTEGER-OPTIONAL
 Specifies the minimun number of lines that an object may span
 and be placed in the catalog.
 Default; MINL=2.

.VARIABLE MAXL
 INTEGER-OPTIONAL
 Specifies the maximum number of lines that an object may span
 and be placed in the catalog.
 Default; MAXL=Infinity (1,000,000).

.VARIABLE MINS
 INTEGER-OPTIONAL
 Specifies the minimun number of samples that an object may span
 and be placed in the catalog.
 Default; MINS=2.

.VARIABLE MAXS
 INTEGER-OPTIONAL
 Specifies the maximum number of samples that an object may span
 and be placed in the catalog.
 Default; MAXS=Infinity (1,000,000).

.VARIABLE ASTEROID
 KEYWORD-OPTIONAL
 The Asteroid keyword causes the following 
 6 keywords to be activated. These are:
 SAOMINP,ASTMINP,SAORATIO,ASTRATIO,ASTMAXW,ASTMAXL
 VALID=(ASTEROID,NOAST)
 DEFAULT; NOAST

.VARIABLE SAOMINP
 INTEGER-OPTIONAL
 Candidate STARCAT SAO stars must have
 at least SAOMINP pixels.  

.VARIABLE ASTMINP
 INTEGER-OPTIONAL
 Candidate STARCAT asteroids must have
 at least ASTMINP pixels.  

.VARIABLE SAORATIO
 REAL-OPTIONAL
 Candidate STARCAT SAO stars must have
 at least a narrow axis to wide axis
 ratio of SAORATIO.

.VARIABLE ASTRATIO
 REAL-OPTIONAL
 Candidate STARCAT asteroids must have
 less than a narrow axis to wide axis
 ratio of ASTRATIO.

.VARIABLE ASTMAXW
 REAL-OPTIONAL
 The maximum allowed width for an object
 to be considered an asteroid candidate
 in the STARCAT catalogue.

.VARIABLE ASTMAXL
 REAL-OPTIONAL 
 The maximum allowed length for an object
 to be considered an asteroid candidate
 in the STARCAT catalogue.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststarcat3.pdf
procedure
LOCAL INPIC   TYPE=STRING
local objs type=integer 
refgbl $syschar
refgbl $echo
! this test script exercises the following parameters
!  INP, OUT, SIZE, SL, SS, NL, NS, LIST, BACK, THRE, MINI, MAXI,
!  MINP, MAXP, MAXS, MINS SPIKE
!
! Does not test ASTEROID parameters or AUTO or SENSE paramteres
 
body
let _onfail="continue"
let $echo="yes"

!   TEST FILE ala starcat2 test pdf USING SPOT
spot OUT=sp1.dat NL=40 NS=40 SHAPE=CONICAL
spot OUT=sp2.dat NL=40 NS=40 SHAPE=CONICAL dnmax=200
spot OUT=sp3.dat NL=40 NS=40 SHAPE=CONICAL dnmax=150
spot OUT=sp4.dat NL=40 NS=40 SHAPE=CONICAL dnmax=100
fastmos inp=(sp1.dat,sp2.dat,sp3.dat,sp4.dat) out=sp.dat +
 nl=80 ns=80 off2=(1,41) off3=(41,1) off4=(41,41)
list sp.dat 
starcat3 INP=sp.dat OUT=(sp.cat,spb.dat) 'list back=10 thre=1
ibis-list sp.cat
list spb.dat 
starcat3 INP=(sp.dat) OUT=(sp.cat,spb.dat) 'list thre=1 +
 maxd=255 back=10 minp=4 maxp=1000000 mini=1 maxi=1000000 +
 minl=1 mins=1 maxl=1000000 maxs=1000000 
ibis-list sp.cat
!
write "Try some rectangular stars of known size and position"
!
gen star0 nl=10 ns=100 ival=0 sinc=0 linc=0 
qsar star0 star.boxes AREA=( 4 11 3 5 200, 4 31 3 10 200, 4 51 6 5 200, +
                             4 71 6 10 200, 4 91 3 5 100)
list star.boxes
starcat3 star.boxes out=sc thre=10 'list
ibis-list sc
!
! Now a real astronomy image.
if ($syschar(1) = "UNIX")
   LET INPIC ="/project/test_work/testdata/gll/s233m.vic"
else 
   LET INPIC ="WMS_TEST_WORK:[TESTDATA.GLL]s233m.vic"
end-if

starcat3 &INPIC out=s233m.cat thre=5940
write "List enough of the output to see if format agrees with HELP."
ibis-list s233m.cat nr=50

starcat3 &INPIC out=s233m.cat2 thre=5940 back=0 objects=objs +
	maxd=32767
write "number of stars = &objs"
write " "
write "Use ibisstat to compare IBIS interface file: like comparing histograms."
write "*****s mean value exceeds field width; good enough for our purposes" 
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")
write "The next ibisstat should produce the same results as the previous one."
ibisstat s233m.cat2  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")

! test second output

starcat3 &INPIC out=(s233m.cat,s233m.byte) thre=5940 back=0 objects=objs +
      maxd=32767
write "number of stars = &objs"
write "The next ibisstat should produce the same results as the previous one."
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")
list s233m.byte (1,1,300,60)
cform &INPIC s233m.raw irange=(5939,6194) orange=(0,255) oform=byte
stretch s233m.raw starstr linear=(0,1)
write "Should be 5714 differences. These are points excluded by default MINP=4,"
write "MINL=2,MINS=2"
difpic (starstr s233m.byte) stardiff
write "Should see only very small-size spots. Please note lines skipped if zero"
list stardiff ns=30 nl=500

starcat3 &INPIC out=(s233m.cat,s233m.byte) thre=5940 back=0 objects=objs +
      maxd=32767 mins=6 minl=6 maxl=50  maxs=50 SPIKE=30000
write "number of stars = &objs"
!Should get subset of previous ibis-list with cols 6&7 in range (6 to 50).
ibis-list s233m.cat nc=9
!Should be like previous LIST but only medium size stars.
list s233m.byte (1,1,300,60)

!test third output
starcat3 &INPIC out=(s233m.cat2,s233m.byte2,s233m.flag) +
      thre=5940 back=0 objects=objs +
      maxd=32767 mins=6 minl=6 maxl=50  maxs=50 SPIKE=30000
write "number of stars = &objs"
write "The next ibisstat should produce DIFFERENT from the previous one."
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")
write "The next ibisstat should produce the same results as the previous one."
ibisstat s233m.cat2  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")
!Should get 0 differences.
difpic (s233m.byte2 s233m.byte)
!Flag file should be same on all platforms (computers).
!This has data which is used internally in the pgm. list s233m.flag nl=15 ns=300
hist s233m.flag nlines=20

! test small image area
starcat3 &INPIC out=s233m.cat size=(1,1,256,256) thre=5940 
write "The next ibisstat should produce DIFFERENT from the previous one."
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")
! same for sl, ss, nl, ns
starcat3 &INPIC out=s233m.cat sl=1 ss=1 nl=256 ns=256 thre=5940 
write "The next ibisstat should produce the same results as the previous one."
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")

!list stars
starcat3 &INPIC out=s233m.cat sl=1 ss=1 nl=256 ns=256 thre=5940 list=list
write "The next ibisstat should produce the same results as the previous one."
ibisstat s233m.cat  cols=(2,9,8) colnames=("DN-B_SUM","CENTLINE","CENTSAMP")

write "Divide the set of stars into two subsets and test sum of two sets = 417"
starcat3 &INPIC out=s233m.cat thre=5940 back=0 objects=objs +
	maxd=32767  MAXI=30000.
write "number of stars = &objs"
starcat3 &INPIC out=s233m.cat thre=5940 back=0 objects=objs +
	maxd=32767  MINI=30001.
write "number of stars = &objs"

! Divide the set of stars into two subsets and test addition
write "Divide the set of stars into two subsets and test sum of two sets = 417"
starcat3 &INPIC out=s233m.cat thre=5940 back=0 objects=objs +
	maxd=32767 MINP=21
write "number of stars = &objs"
starcat3 &INPIC out=s233m.cat thre=5940 back=0 objects=objs +
	maxd=32767  MAXP=20
write "number of stars = &objs"


end-proc
$ Return
$!#############################################################################
