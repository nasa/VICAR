$!****************************************************************************
$!
$! Build proc for MIPL module starcat3
$! VPACK Version 2.1, Monday, March 07, 2016, 12:41:15
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
	-t tststarcat3.pdf tststarcat3.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create starcat3.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44
c
	implicit none
c
c	this routine allows for up to maxobjs dimension on the number 
c	of objects detected. That parameter is declared in subroutine
c	centfind.  To increase it reset that parmeter.
c
c       Made compatible with Linux and MacOSX 64-bit compilers
C       01-30-2010 - R. J. Bambery
c
	integer*4 maxdn,ithr,iback,minpix,maxpix
	integer*4 lospike,hispike,minns,maxns,minnl,maxnl,nods,ibis
	integer*4 saominp,astminp,nlin1,isl,iss,columns,rows
	integer*4 imindn,imaxdn,sloff,ssoff
	integer*4 numobj,astcnt,saocnt,nlimg,nsimg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat,listval
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal,nascii
	logical*4 asteroid,idef,iprnt,iauto,spike,list,window
	real*4 sens,saoratio,astratio,astmaxw,astmaxl,maxsum,minsum
	character*8 fmt
	character*50 version
c
	data version/'*** STARCAT3 version 2016-03-07'/

c
	call xvmessage (version,' ')
c
	call parmproc (iuni,ouni,nods,sl,ss,nl,ns,
     1 nsimg,nlimg,fmt,iprnt,list,listval,window,sloff,ssoff,
     2 maxdn,iauto,sens,ithr,idef,iback,maxsum,minsum,
     3 minpix,maxpix,lospike,hispike,spike,minns,maxns,minnl,maxnl,
     4 asteroid,saominp,astminp,saoratio,astratio,astmaxw,astmaxl)
c
	call centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,list,listval,fmt,
     2 maxdn,iauto,sens,ithr,
     3 maxsum,minsum,spike,window,sloff,ssoff,
     4 iback,imindn,imaxdn,minpix,maxpix,lospike,hispike,minns,maxns,
     5 minnl,maxnl,asteroid,saominp,astminp,saoratio,astratio,
     6 astmaxw,astmaxl,fullrecord,realrecord,asciirecord,nfull,
     7 nreal,nascii)
c
	call labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,
     1 iss,isl,numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	call putparm (numobj)
	call xvclose(iuni,stat,' ')
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
     1 nsimg,nlimg,fmt,iprnt,list,listval,window,sloff,ssoff,
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
	integer*4 maxdn,lospike,hispike,spikeval
	integer*4 ithr,minpix,minint,maxint,threshin
	integer*4 minns,maxns,maxpix,minnl,maxnl,count
	integer*4 saominp,astminp,minintg,maxintg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat,cnt,listval
	integer*4 sloff,ssoff
c	integer*4 sizefield(4),slparm,ssparm,nlparm,nsparm
	logical*4 xvptst,asteroid,idef,iprnt,iauto,spike,list
	logical*4 window
	real*4 saoratio,astratio,astmaxw,astmaxl
	real*4 sens,maxsum,minsum
	character*8 fmt,listout
	character*130 fnam(3)
	character*132 outline
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
	data MININTG/-2147483648/		!MINIMUM INTEGER VALUE (I*4)
	data MAXINTG/2147483647/			!MAXIMUM INTEGER VALUE (I*4)
c
c	call xvparm ('OUT',fnam,nods,def,3)
	call xvp ('OUT',fnam,nods)
	call xvunit (iuni,'INP',1,stat,' ')
	call chkstat (stat,'??E XVunit error on 1st input file',1,0,0)
	call xvopen (iuni,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1 'U_FORMAT','FULL',' ')
	call chkstat (stat,'??E XVopen error on 1st input file',1,0,0)
c This gets the size of the image, the window given by size or sl,ss,nl,ns parameter
C  and might not be smaller
	call xvget (iuni,stat,'NL',nlimg,'NS',nsimg,'FORMAT',fmt,' ')
c				!get size and format of input image 
	if (fmt.eq.'REAL'.or.fmt.eq.'DOUB'.or.fmt.eq.'COMP') then
		write (outline,10020) fmt
10020 format ('??E Format ',a8,' not supported by STARCAT3')
		call xvmessage (outline,' ')
		call abend
	endif
c
	call xvsize (sl,ss,nl,ns,nlimg,nsimg)

	window=.false.
	if (nl.ne.nlimg .or. ns.ne.nsimg) window=.true.
	if (window) then				!64-bit
		sloff = sl 
		ssoff = ss 
c		print *, "sloff, ssoff = ",sloff,ssoff
	else
		sloff = 0
		ssoff = 0
	endif

C	First open catalog, then, if desired, an output image containing
C	the objects filled in with white DN=255 and an output image containing
C	the flag values, i.e., the object numbers as they ar found.
C
C					!ouni(1) is starcat catalog
	call xvunit(ouni(1),'OUT',1,stat,' ')
	call chkstat (stat,'??E XVopen error on output catalog file',1,0,0)
c from 10-05-2009 open outputs to be same size as input
	if (nods.gt.1) then		!SKIP IF ONLY ONE OUTPUT
c					!ouni(2) is an image file with objects at dn=255
		call xvunit(ouni(2),'OUT',2,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat (stat,'??E XVunit error on 2nd output image',1,0,0)
		call xvopen(ouni(2),stat,'U_NL',nlimg,'U_NS',nsimg,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','BYTE',
     2	'O_FORMAT','BYTE',' ')
		call chkstat (stat,'??E XVopen error on 2nd output image',1,0,0)
	endif
	if (nods.eq.3) then		!SKIP IF ONLY ONE OUTPUT
c				!OUNI(3) is flag image file (objects as they are numbered)
		call xvunit(ouni(3),'OUT',3,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat (stat,'??E XVunit error on 3rd output image',1,0,0)
		call xvopen(ouni(3),stat,'U_NL',nlimg,'U_NS',nsimg,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','HALF',
     2	'O_FORMAT','HALF',' ')
		call chkstat (stat,'??E XVopen error on 3rd output image',1,0,0)
	endif
c	added LIST options 9-5-2004
        list=.false.
	iprnt=.false.
        listval=0
        call xvp('LIST',listout,cnt)
        if (listout.eq.'LIST') then	!!1= PRINT CATALOG TO SESSION LOG
                list=.true.
		iprnt=.true.		!'PRINT' DON'T PRINT CATALOG TO SESSION LOG
                listval=3
        endif
        if (listout.eq.'LIST1') then
                list=.true.		!list out certain messages to log
                listval=1
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
	    if (list.and.listval.eq.1) then
		if (maxdn.lt.32767) then
		write (outline,10070) maxdn 
10070 format ('STARCAT3: MAXD (saturation) for a halfword image = ',i8)
		call xvmessage (outline,' ')
		endif
	    endif
	endif		!'HALF'
	if (fmt.eq.'FULL') then
	   if (list.and.listval.eq.1) then
		if (maxdn.lt.maxintg) then
		write (outline,10080) maxdn 
10080 format ('STARCAT3: MAXD (saturation) for a fullword image = ',i10)
		call xvmessage (outline,' ')
		endif
	   endif
	endif	!	!'FULL'
c
	iauto=.false.		!'AUTO' NO AUTOMATIC THRESHOLDING
	if (xvptst('AUTO'))iauto=.true.	!check for autothresholding 
	sens=1.0					!'SENS' 1 SIGMA
	if (iauto) call xvp('SENS',sens,cnt)	!autothresholding sensitivity
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
c
c  the following are single pixel rejection criteria parameters
c	maxint & minint
c
	maxint=0
	if (fmt.eq.'BYTE') then
c		maxint=0
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
	    if (list.and.listval.eq.1) then
		write (outline,10100) hispike,maxdn 
10100 format ('STARCAT3: SPIKE=',i10,' MAXD=',i10,' reset MAXD=SPIKE-1')
		call xvmessage (outline,' ')
	   endif
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
c       8-97 SP Changed MAXI default from 4.0E9 to 1.0E38 to better allow for
c               saturation in fullword images.
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
C asteroid option  - streaks
	asteroid=.false.
	asteroid=xvptst('ASTEROID')
	if (asteroid) then
		call xvp('SAOMINP',saominp,count)
		call xvp('ASTMINP',astminp,count)
		call xvp('SAORATIO',saoratio,count)
		call xvp('ASTRATIO',astratio,count)
		call xvp('ASTMAXW',astmaxw,count)
		call xvp('ASTMAXL',astmaxl,count)
	endif
	return
	end
c============================================================================
	subroutine labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,
     1 iss,isl,numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	implicit none
c
	integer*4 istat,ouni(3),nods,sl,ss,nl,ns,iback
	integer*4 imindn,imaxdn,isl,iss,numobj,saocnt,astcnt
	integer*4 ithr,nlimg,nsimg,j,maxnl,maxns,maxdn
	logical*4 idef,iauto,asteroid
	real*4 mean,sigma,sens
	character*8 threshold,autothresh,man,default
	character*8 fmt,area,full,subarea
	character*28 filen(3)
	character*35 version
c
	data man/'ENTERED '/,default/'DEFAULT '/
	data autothresh/'AUTO    '/
	data filen	/'STARCAT TYPE   2 CATALOG    ',
     1			 'STARCAT FILLED DISPLAY FILE ',
     2			 'STARCAT FLAGGED OBJECT FILE '/
c
	full='FULL    '
	subarea='SUBAREA '

	area=full
	if (isl.ne.1) area=subarea
	if (iss.ne.1) area=subarea
	if (nl.ne.nlimg) area=subarea
	if (ns.ne.nsimg) area=subarea
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
	    call xladd(ouni(j),'HISTORY','DATASET',filen(j),istat,'FORMAT','STRING',' ')
  	    call xladd(ouni(j),'HISTORY','CREATOR',version,istat,'FORMAT','STRING',' ')
	    call xladd(ouni(j),'HISTORY','IMGFMT',fmt,istat,'FORMAT','STRING',' ')
	    call xladd (ouni(j),'HISTORY','THRESHOLD',threshold,istat,'FORMAT','STRING',' ')
	    call xladd (ouni(j),'HISTORY','THRESHVAL',ithr,istat,'FORMAT','INT',' ')
 	    if (iauto) then
		call xladd(ouni(j),'HISTORY','MIN_DN',imindn,istat,'FORMAT','INT',' ')
		call xladd(ouni(j),'HISTORY','MAX_DN',imaxdn,istat,'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','MEAN',mean,istat,'FORMAT','REAL',' ')
		call xladd (ouni(j),'HISTORY','SIGMA',sigma,istat,'FORMAT','REAL',' ')
 		call xladd(ouni(j),'HISTORY','SENSITIVITY',sens,istat,'FORMAT','REAL',' ')
c
	    endif
	    call xladd (ouni(j),'HISTORY','BKGVALUE',iback,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','SATURAT_VAL',maxdn,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','MAX_LINESIZE',maxnl,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','MAX_SAMPSIZE',maxns,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','TAB_AREA',area,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','SL_IMAGE',1,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','SS_IMAGE',1,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','NL_IMAGE',nlimg,istat,'FORMAT','INT',' ')
	    call xladd (ouni(j),'HISTORY','NS_IMAGE',nsimg,istat,'FORMAT','INT',' ')
            call xladd (ouni(j),'HISTORY','SL_WINDOW',sl,istat,'FORMAT','INT',' ')
            call xladd (ouni(j),'HISTORY','SS_WINDOW',ss,istat,'FORMAT','INT',' ')
            call xladd (ouni(j),'HISTORY','NL_WINDOW',nl,istat,'FORMAT','INT',' ')
            call xladd (ouni(j),'HISTORY','NS_WINDOW',ns,istat,'FORMAT','INT',' ')

	    call xladd (ouni(j),'HISTORY','OBJECTS',numobj,istat,'FORMAT','INT',' ')
c
	    if (asteroid) then
	        call xladd(ouni(j),'HISTORY','ASTEROID',astcnt,istat,'FORMAT','INT',' ')
	        call xladd(ouni(j),'HISTORY','SAOSTARS',saocnt,istat,'FORMAT','INT',' ')
	    endif
c
	enddo

	return
	end
c============================================================================
	subroutine centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,list,listval,fmt,
     2 maxdn,iauto,sens,ithr,
     3 maxsum,minsum,spike,window,sloff,ssoff,
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
	integer*4 flag(32767,2)
	integer*4 isls(maxobjs),isss(maxobjs),el(maxobjs),es(maxobjs)
	integer*4 cntr(maxobjs,2)
	integer*4 l,nl,ns,line,iss,isl,iuni,stat
	integer*4 i,j,ss,nods,numobj,ii,listval,sloff,ssoff
	integer*4 newc,nolc,n1,nbin,oldid,isw,numsat
	integer*4 npix,ithr,iback,maxdn,n2,num,newid,nlin1
	integer*4 saocnt,astcnt,sl,ouni(3),minpix
	integer*4 hispike,lospike,maxpix,minns,columns,rows,ibis
	integer*4 maxns,minnl,maxnl,saominp,astminp,imindn,imaxdn
	integer*4 fullrecord,realrecord,asciirecord,abendc,rejobj
	integer*4 nfull,nreal,nascii,scversion,scunum,maxfreq
	integer*4 id(maxobjs),npixs(maxobjs),satcnt(maxobjs),ibuf(7)
	integer*4 in(32767),rejcnt(maxobjs)
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65536:65535)
	logical*4 asteroid,iprnt,iauto,spike,list,window
	real*4 saoratio,astratio
	real*4 astmaxw,astmaxl,sens,mean,sigma,median,mode,risump,xsigma
	real*4 ricens,maxsum,minsum
	real*4 rbuf(11),sumdn(maxobjs)
	real*8 l2,ij,iumpjl,iumpj2,iumpl2,rl,r8isump,rsumpl
	real*8 cenx(maxobjs),ceny(maxobjs),sumpjl(maxobjs),sumpj2(maxobjs)
	real*8 sumpl2(maxobjs),rthresh
	character*8 fmt
	character*16 openmode,iborg,scunits(50)
	character*20 abuf(3)
	character*70 headings2
	character*100 headings1
	character*150 outline
c
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
	data headings1
     1/'  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--
     2ENTSAMP CENTLINE'/
	data headings2
     1/'    MAJ AXIS   MIN AXIS   RATIO  ROTANGLE'/
c
c
c The starcat catalog is an IBIS-2 tabular file which has the following format.
c
c
c	***************************************************************
c	descriptor	description			format	units
c	________________________________________________________
c1	entry		catalog entry number		full		<none>		
c2	DN-bkg		sum of DNs - background		real		counts
c3	tot pix		total pixel count 		full		<none>
c4	sl		starting line			full		<none>
c5	ss		starting sample			full		<none>
c6	nl		number of lines			full		<none>
c7	ns		number of samples		full		<none>
c8	centsamp	centroid sample			real		<none>
c9	centline	centroid line			real		<none>
c10	maj axis	major axis length		real		<none>
c11	min axis	minor axis length		real		<none>
c12	ratio		ratio minor/major axis		real		<none>
c13	rotangle	rotation angle of ellip		real		degrees
c14	calalog		source catalog			ascii-A20	<none>
c15	ID		catalog ID			ascii-A16	<none>
c16	RA		right ascension			real		hours
c17	dec		declination			real		degrees
c18	mag calc	calculated magnitude		real		<none>
c19	mag ref		catalog magnitude		real		<none>
c20	class		classification			ascii-A8	<none>
c21	sat pixs	# saturated pixels		full		<none>
c	***************************************************************
c
C	The old STARCAT catalog format was the following
C
C*************************
C     IBUF(1)=SUM(DN-BACK)			I*4
C     IBUF(2)=SUM PIXELS			I*4
C     IBUF(3)=SL  				I*4
C     IBUF(4)=SS  				I*4
C     IBUF(5)=NL  				I*4
C     IBUF(6)=NS  				I*4
C     IBUF(7)=CENTROID SAMPLE  			R*4
C     IBUF(8)=CENTROID LINE  			R*4
C     IBUF(9)=major axis length  		R*4
C     IBUF(10)=minor axis length  		R*4
C     IBUF(11)=object identifier	  	R*4 - ("asteroid" option only)
C     IBUF(12)=rotation angle degrees 		R*4
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

c               SP added rounding here (circa 1997)
c		ithr=mean+sens*xsigma
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
	   if (list.and.listval.eq.1) then
		write (outline,10360) ithr
10360 format ('THRESH = ',i6,' as given')
		call xvmessage (outline,' ')
	   endif
	endif
c
c**			debugging purposes only
c**	if (.not.idef) then
c**		ithr=mean+sigma
c**	endif
c
c	print out headings
	if (iprnt) then
		write (outline,10400) headings1
10400 format (a80)
		call xvmessage (outline,' ')
		write (outline,10400) headings2
		call xvmessage (outline,' ')
	endif

	nlin1=0			!output catalog line counter
	oldid=1			!initialize ID 
	isw=1			!initialize flag buffer to buffer #1 
	npix=ns			!NUMBER OF PIXELS =  NUMBER OF SAMPLES/LINE
	numobj=0			!RUNNING COUNTER OF REAL OBJECTS FOUND
	nolc=0
	newc=0
	saocnt=0		!no asteroid candidate
	astcnt=0		!no sao catalog star candidate
	isl=sl			!input starting line
	iss=ss			!input starting sample
C*******************
C     CLEAR FLAG BUFFERS
C
C	The FLAG buffer is a 2 line array equal in width to the image
C	which associates an object ID with each sample. ISW indicates
C	the current and previous line in order to find contiguous pixels
C	for each object.
C*******************
c	call mve(4,npix,0,flag(1,1),0,1)
c	call mve(4,npix,0,flag(1,2),0,1)
c	call mve(7,nbin,0,sumdn,0,1)
c	call mve(4,nbin,0,npixs,0,1)
c	call mve(8,nbin,0,cenx,0,1)
c	call mve(8,nbin,0,ceny,0,1)
c	call mve(8,nbin,0,sumpl2,0,1)
c	call mve(8,nbin,0,sumpj2,0,1)
c	CALL mve(8,nbin,0,sumpjl,0,1)
c	call mve(4,nbin,0,satcnt,0,1)
c	call mve(4,nbin,0,rejcnt,0,1)
c	call mve(-4,nbin,32767,isls,0,1)
c	call mve(-4,nbin,32767,isss,0,1)
c	call mve(4,nbin,0,el,0,1)
c	call mve(4,nbin,0,es,0,1)
c	call mve(4,nbin,0,cntr(1,1),0,1)
c	call mve(4,nbin,0,cntr(1,2),0,1)
	do i=1,maxobjs
	   cenx(i) = 0.0d0
	   ceny(i) = 0.0d0
	   sumpjl(i) = 0.0d0
	   sumpj2(i) = 0.0d0
	   sumpl2(i) = 0.0d0
	   satcnt(i) = 0.0
	   rejcnt(i) = 0.0
	   sumdn(i) = 0.0
	   id(i) = 0
	   npixs(i) = 0
	   isls(i) = 32767
	   isss(i) = 32767
	   el(i) = 0
	   es(i) = 0
	   flag(i,1) = 0
	   flag(i,2) = 0
	   cntr(i,1) = 0
	   cntr(i,2) = 0
	enddo
c
c------> MAIN LOOP: Read input in line by line, find objects <------------
	do 1000 l=1,nl
c				!print out a summary every 500 lines
	    if (list.and.listval.eq.1) then
		if (mod(l,500).eq.0) then
		    write (outline,10410) l,numobj
10410 format ('Line# = ',i7,'        #Objects = ',i7)
		    call xvmessage(outline,' ')
		endif
	    endif
	    line=l+isl-1
c	if (line.eq.596) then
c		call xvmessage ('Here',' ')
c	endif
C     read the picture to process.
C	INPUT IMAGE DATA WILL BE IN BUFFER IN(NS)
C
	    call xvread(iuni,in,stat,'LINE',line,'SAMP',iss,'NSAMPS',ns,' ')
	    if (stat.eq.END_OF_FILE) go to 1004
	    call chkstat(stat,'??E XVread error on input image',1,0,0)
	    if (nods.gt.1) call mve (1,ns,0,outdata,0,1)    !0's to output
C*******************
C     SEARCH IN(1) to in(npix) FOR AN OBJECT .ge. threshold
C*******************
           newc=0
           n1=1				!initialize starting samp position
c  Now start search on the line, l
c--> Find all objects on one line loop (goes to 200)
c
c  I think this algorithm will have some problems if mean is lt 0
c  It was originally written for byte images where lowest dn was 0
c
105	  continue
 	  ricens=0.0			!this initializes centroid function
	  risump=0.0			!init sum of dns
	  iumpj2=0
	  iumpjl=0
	  iumpl2=0
	  numsat=0			!initialize saturation counter for object
	  rejobj=0			!valid object (for minint comparison)
          do 110 j=n1,npix
		if (in(j).ge.ithr) go to 120	!image > threshold?
110       continue
C*******************
C     finished line!
C*******************
          go to 200
120       continue
	  n1=j				!mark starting samp position of object
          l2=l*l
          do 130 j=n1,npix
	      i=in(j)-iback
      	      if (in(j).lt.ithr) go to 140	!image < threshold?
C*******************
C     Have object - Update centroid numerator
C	- 1st moment of marginal distribution in x [sample direction]
C*******************
c	check for low value pixel modification  criteria
		if (in(j).lt.lospike) i=lospike-iback	!if dn lt lospike, then make lospike
		if (in(j).eq.maxdn) then
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
		iumpjl=iumpjl+ij*l
		iumpl2=iumpl2+i*l2		!2nd moment in line
130       continue

          j=npix+1				!at end of line plus 1
140       n2=j-1				!mark ending position of object
C*******************
C     FILL IN STAR AREA ON 2ND OUTPUT FILE if greater than min ns
C	NOTE:  This occasionally marks certain rejected objects - esp single pix.
C*******************
	  if ((n2-n1+1).ge.minns) then
	     do ii=n1,n2
		outdata(ii)=-1			!outdata(ii)=255 - signed byte integer
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
      call match (flag(1,3-isw),n1,n2,num,id,npix)
      newid=id(1)
      if (num.lt.2) go to 101
C*******************
C     IF MANY IDs,  CONDENSE TO ONE ID
C*******************
	call jjlconcat(sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1               sumpj2,sumpjl,sumpl2,satcnt,rejcnt,num,id)
      go to 102
101   continue
C*******************
C     IF NO ID,  CREATE ONE
C     UPDATE NEW FLAG BUFFER
C     UPDATE NEW COUNTER
C     UPDATE THE STATISTICS BUFFERS
C*******************
	if (num.eq.0) then
		call find (nbin,newid,sumdn,oldid)
	endif
102	continue
c	now have our id
	call mve (-4,n2-n1+1,newid,flag(n1,isw),0,1)
        newc=newc+1
        cntr(newc,isw)=newid
	sumdn(newid)=sumdn(newid)+risump	!sum of intensities
	npixs(newid)=npixs(newid)+n2-n1+1	!number of pixels in area
	cenx(newid)=cenx(newid)+dble(ricens)	!centroid in x-direction
	r8isump=dble(risump)
	rl=dble(l)
	rsumpl=r8isump*rl
        ceny(newid)=ceny(newid)+rsumpl		!centroid in y-direction
        sumpj2(newid) = sumpj2(newid)+iumpj2
        sumpjl(newid) = sumpjl(newid)+iumpjl
        sumpl2(newid) = sumpl2(newid)+iumpl2
        if (isls(newid).gt.l) isls(newid)=l
        if (isss(newid).gt.n1) isss(newid)=n1
        if (es(newid).lt.n2) es(newid)=n2
        el(newid)=l
	satcnt(newid) = satcnt(newid)+numsat	!number of saturated pixels
	rejcnt(newid) = rejcnt(newid)+rejobj
C*******************
C     SEND BACK FOR MORE DATA ON SAME LINE
C*******************
        n1=n2+2			!skip at least two pixels for next object
        if (n1.lt.npix) go to 105	!loop back if not at end of line
C--> FOUND ALL OBJECTS on one line - loop back to 105
200   continue			!formerly 103
C*******************
C     WRITE OUT 2ND and 3RD OUTPUT FILES
C*******************
	if (nods.gt.1) then
	    call xvwrit(ouni(2),outdata,stat,'LINE',l,'NSAMPS',ns,' ')
	    call chkstat (stat,'??E XVwrit error on 2nd output image',1,0,0)
	endif
	if (nods.eq.3) then
	    call xvwrit(ouni(3),flag(1,isw),stat,'LINE',l,'NSAMPS',ns,' ')
		call chkstat (stat,'??E XVwrit error on 3rd output image',1,0,0)
	endif
C*******************
C     SEARCH BOTH FLAG BUFFERS FOR
C     TERMINATED STARS AND WRITE TO DATA SET
C*******************

c        if (nlin1 .eq. 0 .or. nlin1 .eq. 1 ) then
c       write (outline,11120) nlin1, newid,cenx(newid),ceny(newid)
c11120 format('main = ',i3,2x,i8,2x,f24.8,2x,f24.8)
c        call xvmessage(outline,' ')

c        endif


      call eol(sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1 sumpj2,sumpjl,sumpl2,minpix,satcnt,rejcnt,rbuf,abuf,ibuf,
     2 nlin1,nl,l,iprnt,cntr(1,3-isw),cntr(1,isw),nolc,
     3 newc,numobj,maxpix,minsum,maxsum,minns,
     4 maxns,minnl,maxnl,ouni,ibis,rows,window,sloff,ssoff,
     5 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     6 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     7 fmt)
C*******************
C     ZERO THE OLD FLAG LINE
C     AND SWITCH FLAGS
C*******************
      call mve(4,npix,0,flag(1,3-isw),0,1)
      isw=3-isw
      nolc=newc
1000   continue
c------> END MAIN LOOP: Read input completely, found objects <------------
C
1004   continue
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
	subroutine find (nbin,newid,sumdn,oldid)
C*******************
C     TO FIND A NEW BIN POSITION
C     NBIN=LENGTH OF BIN BUFFERS
C     NEWID=NEW BIN VALUE RETURNED
C     sumdn=SUM DN BUFFER
C     OLDID=LAST FOUND I.D.
C*******************
	implicit none
	integer*4 oldid
	integer*4 j,nbin,newid
	real*4 sumdn(*)
	character*100 outline
c
      DO 30 J=OLDID,NBIN
		if (sumdn(j).eq.0.0) go to 40
30    CONTINUE
      DO 35 J=1,OLDID
		if (sumdn(j).eq.0.0) go to 40
35    CONTINUE
	write (outline,10100) nbin
10100 format ('??E subroutine find - All ',i4,' bins filled')
	call xvmessage (outline,' ')
	call abend
c
40    newid=j
      oldid=j
      return
      end
c****************************************************************************
	subroutine jjlconcat(sumdn,npixs,isls,isss,el,es,cenx,
     1 ceny,sumpj2,sumpjl,sumpl2,satcnt,rejcnt,num,id)
C*******************
C     ROUTINE TO CONCATENATE MANY I.D. S
C	NUM = Number of objects located
C*******************
	implicit none
	integer*4 isls(*),isss(*),el(*),es(*)
	integer*4 oldid,npixs(*),id(*),satcnt(*),rejcnt(*)
	integer*4 newid,num,j
	real*4 sumdn(*)
	real*8 cenx(*),ceny(*),sumpj2(*),sumpjl(*),sumpl2(*)
c
        newid=id(1)
        do 10 j=2,num
            oldid=id(j)
	    sumdn(newid)=sumdn(newid)+sumdn(oldid)
	    sumdn(oldid)=0.0
	    satcnt(newid)=satcnt(newid)+satcnt(oldid)
	    satcnt(oldid)=0
	    rejcnt(newid)=rejcnt(newid)+rejcnt(oldid)
	    rejcnt(oldid)=0
            npixs(newid)=npixs(newid)+npixs(oldid)
            npixs(oldid)=0
            if (isls(newid).gt.isls(oldid)) isls(newid)=isls(oldid)
            isls(oldid)=32767
            if (isss(newid).gt.isss(oldid)) isss(newid)=isss(oldid)
            isss(oldid)=32767
            if (el(newid).lt.el(oldid)) el(newid)=el(oldid)
            el(oldid)=0
            if (es(newid).lt.es(oldid)) es(newid)=es(oldid)
            es(oldid)=0
            cenx(newid)=cenx(newid)+cenx(oldid)
            cenx(oldid)=0.0
            ceny(newid)=ceny(newid)+ceny(oldid)
            ceny(oldid)=0.0
            sumpj2(newid)=sumpj2(newid)+sumpj2(oldid)
            sumpj2(oldid)=0.0
            sumpjl(newid)=sumpjl(newid)+sumpjl(oldid)
            sumpjl(oldid)=0.0
            sumpl2(newid)=sumpl2(newid)+sumpl2(oldid)
            sumpl2(oldid)=0.0
10    continue
      return
      end
c****************************************************************************
	subroutine match (flag,n1,n2,num,id,ns)
C*******************
C     RETURN I.D. OF OBJECT FOUND ON PREVIOUS LINE
C     FLAG=OLD FLAG BUFFER
C     N1=BEGIN  N2=END  OF OBJECT
C     NUM = NUMBER OF OBJECTS LOCATED
C     ID=I.D. OF OBJECTS LOCATED
C*******************
	implicit none
	integer*4 flag(*)
	integer*4 id(*)
	integer*4 m1,m2,n1,n2,ns,num,j,k,n
c
        m1=n1-1				!point 1 pixel to left of object
        m2=n2+1				!point 1 pixel to right of object
        if (m1.lt.1) m1=1
        if (m2.gt.ns) m2=ns
        num=0				!running counter of objects being checked
        k=0
C*******************
C     FIND ALL FLAGS - search for this object # in previous line flag buffer
C*******************
        do 10 j=m1,m2
      	    if (flag(j).eq.k) go to 10
      	    if (flag(j).eq.0) go to 10
      	    k=flag(j)
      	    num=num+1
      	    id(num)=k
10    continue
      if (num.lt.2) return
C*******************
C     REJECT DUPLICATES
C*******************
      call isort (id,1,num)
      n=1
      k=id(1)
      do 20 j=2,num
      	  if (k.eq.id(j)) go to 20
          n=n+1
      	  id(n)=id(j)
      	  k=id(j)
20    continue
      num=n
      return
      end
c****************************************************************************
	subroutine eol(sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1 sumpj2,sumpjl,sumpl2,minpix,satcnt,rejcnt,rbuf,abuf,ibuf,
     2 nlin1,nl,line,iprnt,olcntr,necntr,nolc,
     3 newc,numobj,maxpix,minsum,maxsum,minns,
     4 maxns,minnl,maxnl,ouni,ibis,rows,window,sloff,ssoff,
     5 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     6 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     7 fmt)
C*******************
C     SEARCH FOR FINISHED OBJECTS IN JUST COMPLETED LINE
C*******************
	implicit none
	integer*4 olcntr(*),necntr(*)
	integer*4 isls(*),isss(*),el(*),es(*)
	integer*4 j,k,nlin1,ibis,rows
	integer*4 newc,newid,nolc,nl,numobj
	integer*4 line,maxpix,minpix,sloff,ssoff
	integer*4 maxnl,minnl,maxns,minns
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal
	integer*4 ibuf(nfull)
	integer*4 npixs(*),satcnt(*),rejcnt(*)
	integer*4 ouni(3)
	logical*4 asteroid,iprnt,window
	real*4 astmaxl,astmaxw,astratio,saoratio,maxsum,minsum
	real*4 rbuf(nreal),sumdn(*)
	real*8 cenx(*),ceny(*),sumpj2(*),sumpjl(*),sumpl2(*)
	character*8 fmt
	character*20 abuf(3)
c	character*100 outline
c
C*******************
C     SEARCH OLCNTR FOR IDs NOT IN NECNTR
C*******************
        if (line.eq.nl) go to 51
        if (nolc.eq.0) return
        if (newc.eq.0) go to 51
        do 10 j=1,nolc
 	     newid=olcntr(j)
      	     do 20 k=1,newc
	     	 if (newid.eq.necntr(k)) GO TO 10
20      continue

c        if (nlin1 .eq. 0 .or. nlin1 .eq. 1 ) then
c       write (outline,11120) nlin1,newid,cenx(newid),ceny(newid)
c11120 format('eol = ',i2,2x,i8,2x,f24.8,2x,f24.8)
c        call xvmessage(outline,' ')

c        endif

C*******************
C     STAR NOT CONTINUED
C*******************
	     call fin(newid,sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1 sumpj2,sumpjl,sumpl2,ibis,window,minpix,satcnt,rejcnt,rows,
     2 abuf,ibuf,rbuf,nlin1,iprnt,numobj,sloff,ssoff,
     3 maxpix,minsum,maxsum,minns,maxns,minnl,maxnl,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 fmt)
10      continue
        return
C
C*******************
C     FINISH OFF EVERYTHING - JUST FINISHED LINE
C*******************
51      if (nolc.eq.0) return
        do 50 j=1,nolc
            newid=olcntr(j)
	    call fin(newid,sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1 sumpj2,sumpjl,sumpl2,ibis,window,minpix,satcnt,rejcnt,rows,
     2 abuf,ibuf,rbuf,nlin1,iprnt,numobj,sloff,ssoff,
     3 maxpix,minsum,maxsum,minns,maxns,minnl,maxnl,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 fmt)
50          continue
	return
	end
c****************************************************************************
	subroutine fin(newid,sumdn,npixs,isls,isss,el,es,cenx,ceny,
     1 sumpj2,sumpjl,sumpl2,ibis,window,MINPIX,satcnt,rejcnt,rows,
     2 abuf,ibuf,rbuf,nlin1,iprnt,numobj,sloff,ssoff,
     3 maxpix,minsum,maxsum,minns,maxns,minnl,maxnl,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 fmt)
C*******************
C     ROUTINE TO TERMINATE AN OBJECT AND WRITE TO CATALOG
C     ibuf=catalog I*4 data
c	rbuf=catalog R*4 data
c	abuf=catalog ASCII data
C*******************
	implicit none
	integer*4 isls(*),isss(*),el(*),es(*)
	integer*4 npixs(*),satcnt(*),rejcnt(*)
	integer*4 stat,sloff,ssoff
	integer*4 i,il,ind,is,ibis,abendc,rows
	integer*4 maxnl,minnl,maxns,minns,maxpix,minpix
	integer*4 newid,nlin1,nrot,numobj,nfull,nreal
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord
	integer*4 ibuf(nfull)
	logical*4 asteroid,iprnt,window
	real*4 rbuf(nreal),sumdn(*)
	real*4 mu20,mu02,mu11,rad,ratio,minsum,maxsum
	real*4 r,astratio,saoratio,theta,astmaxw,astmaxl
	real*4 amatrx(2,2),dmatrx(2),vmatrx(2,2)
	real*8 cenx(*),ceny(*),sumpj2(*),sumpjl(*),sumpl2(*)
	real*8 sump,dbuf(20)
	character*8 fmt
	character*16 abuf1
	character*20 abuf2,abuf(3)
c	character*100 outline
c
	data rad/57.29577/
	data abuf1/'                '/
	data abuf2/'                    '/
c
c  first, check out object rejection criteria
      	if (npixs(newid).eq.0) return		!no object if no area
      	if (npixs(newid).lt.minpix) go to 1000	!no object if lt min # pixels
      	if (npixs(newid).gt.maxpix) go to 1000	!no object if gt max pixels
      	il=el(newid)-isls(newid)+1
      	is=es(newid)-isss(newid)+1
      	if (il.lt.minnl) go to 1000		!no object if lt min lines
      	if (il.gt.maxnl) go to 1000		!no object if gt max line
      	if (is.lt.minns) go to 1000		!no onject if lt min samples
      	if (is.gt.maxns) go to 1000		!no object if gt max samples
	if (fmt.eq.'BYTE') then
	    if (sumdn(newid).le.0.0) go to 1000		!may have to worry about
	endif
c**      IF (ISUM(NEWID).LT.MININT) GO TO 100		!dn sums lt 0.0
c**      IF (ISUM(NEWID).GT.MAXINT) GO TO 100
	if (sumdn(newid).lt.minsum) go to 1000	!no obj if lt min sumdn
	if (sumdn(newid).gt.maxsum) go to 1000	!no obj if gt max sumdn
	if (rejcnt(newid).gt.1) go to 1000		!no object if 1 dn gt maxint
c
c	OBJECT IS REAL!!
c
c	ZERO out arrays
	do i=1,nfull
	   ibuf(i)=0
        enddo
	do i=1,nreal
	   rbuf(i)=0.0
	   dbuf(i)=0.0d0
        enddo
c        if (nlin1 .eq.1 ) then
c        write (outline,11105) dbuf(2),dbuf(3)
c        call xvmessage(outline,' ')
c
c        endif

	abuf(1)=abuf1
	abuf(2)=abuf2
	abuf(3)='       '
	numobj=numobj+1
	sump=dble(sumdn(newid))
C					!Catalog entries
	ibuf(1)=nlin1+1			!1 - entry number
	rbuf(1)=sumdn(newid)		!2 - sum of DNs of object (intensity)
      	ibuf(2)=npixs(newid)		!3 - number of pixels in area
	if (window) then			!64-bit
	    ibuf(3)=isls(newid) + sloff - 1     !4 - SL of object from size field - coor 10-17-2009
      	    ibuf(4)=isss(newid) + ssoff - 1     !5 - SS of object from size field - coor 10-17-2009
c	print *, "true: ibuf(3), ibuf(4) = ",ibuf(3),ibuf(4)
	
	else
	    ibuf(3)=isls(newid)         !4 - SL of object in original image
            ibuf(4)=isss(newid)         !5 - SS of object in original image
c	print *, "false: ibuf(3), ibuf(4) = ",ibuf(3),ibuf(4)
	endif
      	ibuf(5)=il			!6 - NL of object from size field
      	ibuf(6)=is			!7 - NS of object from size field
c        if (nlin1 .eq.0 .or. nlin1 .eq. 1 ) then
c        write (outline,11105) dbuf(2),dbuf(3)
c        call xvmessage(outline,' ')
c	write (outline,11120) nlin1,newid,cenx(newid),ceny(newid),sump
c11120 format('fin = ',i2,2x,i8,2x,f24.8,2x,f24.8,2x,f24.8)
c	call xvmessage(outline,' ')

c        do i=1,newid
c	    write (outline,11125) i,cenx(i),ceny(i),sump
c11125 format ('loop = ',i8,2x,f24.8,2x,f24.8,2x,f24.8)
c	    call xvmessage(outline,' ')	
c	enddo
c        endif

	if (window) then				!64-bia
	    dbuf(2) = (cenx(newid)/sump) + ssoff - 1
	    dbuf(3) = (ceny(newid)/sump) + sloff - 1
	    rbuf(2)=sngl((cenx(newid)/sump) + ssoff - 1)	!coor 10-17-2009
	    rbuf(3)=sngl((ceny(newid)/sump) + sloff - 1)	!coor 10-17-2009
	else
	    dbuf(2) = cenx(newid)/sump
	    dbuf(3) = ceny(newid)/sump
      	    rbuf(2)=sngl(cenx(newid)/sump)	!8 - centroid sample position
      	    rbuf(3)=sngl(ceny(newid)/sump)	!9 - centroid line position
	endif
	ibuf(7)=satcnt(newid)		!21 - # of saturated pixels

c        if (nlin1 .eq.1 ) then
c        write (outline,11105) dbuf(2),dbuf(3)
c11105 format(f24.8,2x,f24.8)
c        call xvmessage(outline,' ')
c
c        endif


c
      	mu20=sngl(sumpj2(newid)/sump-(cenx(newid)/(sump))**2.)
      	mu02=sngl(sumpl2(newid)/sump-(ceny(newid)/(sump))**2.)
      	mu11=sngl(sumpjl(newid)/sump-ceny(newid)*cenx(newid)/(sump)**2.)
c
      	amatrx(1,1)=mu20
      	amatrx(2,2)=mu02
      	amatrx(1,2)=mu11
      	amatrx(2,1)=mu11
      	call jacobi(amatrx,2,2,dmatrx,vmatrx,nrot,ind)
      	if (ind.eq.0) then
            call eigsrt(dmatrx,vmatrx,2,2)
            theta=-rad*atan2(vmatrx(2,1),vmatrx(1,1))
c						!finish catalog entries
            rbuf(4) = sqrt(abs(dmatrx(1)))	!10 - major axis of object
            rbuf(5) = sqrt(abs(dmatrx(2)))	!11 - minor axis of object
	    ratio=rbuf(5)/rbuf(4)	   	! minor/major axes
            rbuf(6) = ratio			!12 - minor/major ratio
            rbuf(7) = theta			!13 - rotation of major axis
        else
            numobj=numobj-1
            goto 1000
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
      	    if((int(rbuf(1)).ge.saominp).and.(ratio.gt.saoratio)) then !if intensity & area > then
		goto 180    			! SAO star candidate
            else if((int(rbuf(1)).ge.astminp).and.
     1          (ratio.lt.astratio).and.   	! asteroid candidates
     2          (rbuf(5).le.astmaxw)) then
		     r=sqrt(float(ibuf(5)**2+ibuf(6)**2)) 	        	! length
		if (r.lt.astmaxl) goto 190  		! accept object
            endif
       	    numobj=numobj-1   				! reject object
       	    goto 190
180	continue
c**	RBUF(ISTART+11)=1.0			!11- #1=SAO candidate
	    abuf(1)='starcat3'
	    abuf(3)='star'
	    saocnt=saocnt+1
	    go to 195
190	    continue
c**	RBUF(ISTART+11)=2.0			!11- #2=asteroid candidate
	    abuf(1)='starcat3'
	    abuf(3)='asteroid'
	    astcnt=astcnt+1
195	    continue
        endif
c200   continue
c	if (nlin1 .eq.1 ) then
c        write (outline,11100) (rbuf(i),i=1,6)
c11100 format(6f9.2)
c        call xvmessage(outline,' ')

c	endif
c 	skip printing if 'LIST' not given
	if (iprnt) call prinz(ibuf,rbuf,nfull,nreal)
C*******************
C     LOGIC FOR STATISTICS DATA SET
C*******************
	nlin1=nlin1+1		!bump catalog line counter
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
1000	CONTINUE
	sumdn(newid)=0.0
	npixs(newid)=0
	isls(newid)=32767
	isss(newid)=32767
	el(newid)=0
	es(newid)=0
	cenx(newid)=0.0
	ceny(newid)=0.0
	sumpj2(newid)=0.0
	sumpl2(newid)=0.0
	sumpjl(newid)=0.0
	satcnt(newid)=0
	rejcnt(newid)=0
	return
	end
c****************************************************************************
	subroutine prinz(ibuf,rbuf,nfull,nreal)
c
c	prints out catalog to log and terminal if 'LIST' keyword given
c
	implicit none
	integer*4 nfull,nreal
	integer*4 ibuf(nfull),i,j
	real*4 rbuf(nreal)
	character*150 outline
c
	write (outline,10100) ibuf(1),rbuf(1),(ibuf(i),i=2,6),
     1	(rbuf(j),j=2,6)
10100 format(i8,f9.0,5i9,2f9.2,13x,3f9.2)
	call xvmessage(outline,' ')
      return
      end
c****************************************************************************
	subroutine jacobi(a,n,np,d,v,nrot,ind)
c Computes eigenvalues & eigenvectors.
c A=n by n matrix of real symmetric values in an np by np matrix.
c D=returns n eigenvalues of A.
c V=returns normalized eigenvectors of A in NP by NP matrix.
c NROT=# jacobi rotations required.
c ind= 0(normal) 1(abnormal)
	implicit none
	integer*4 i,ind,ip,iq,j,n,np,nrot
	real*4 a(np,np),d(np),v(np,np),b(100),z(100)
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
	subroutine eigsrt(d,v,n,np)
c sorts the eigenvalues into ASCENDING order & moves the
c eigenvector columns in the same order.
	implicit none
	integer*4 i,j,k,n,np 
	real*4 d(np),v(np,np),p
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
	integer*4 inpbuf(32768)
	integer*4 iunit,sl,ss,nl,ns,maxfreq,mindn,maxdn,npts,i
	integer*4 sumcts,iback,npixels,jmaxdn,jmean
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65536:65535)
	real*4 mean,sigma,median,mode,xsigma
	logical*4 medianfound
	character*8 fmt
	character*100 outline
c
	medianfound=.false.
	npts=nl*ns	
	if (fmt.eq.'BYTE') then
		call comphist(iunit,sl,ss,nl,ns,ohist,
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
10100 format ('??E AUTOT:  All values zero in minimum search')
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
		call comphist2(iunit,sl,ss,nl,ns,hist,
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
		call comphist4(iunit,sl,ss,nl,ns,fhist,
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
	subroutine comphist(iunit,SL,SS,NL,NS,ohist,inpbuf)
C Compute a histogram of a byte image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c	and subtracts background if required
c
	implicit none
      integer*4 ohist(256)
      integer*4 inpbuf(32768)		!only difference from library routine
      integer*4 sl,ss,el,nl,ns,iunit
	integer*4 line,ind

      call zia(ohist,256)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         call xvread(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         call hsub(1,NS,INPBUF,OHIST)
      ENDDO

      return
      end
c============================================================================
	subroutine comphist2(iunit,sl,ss,nl,ns,hist,inpbuf)
C Compute a histogram of a halfword image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c
	implicit none
      integer*4 hist(-32768:32767)
      integer*4 inpbuf(32768)		!only difference from library routine
      integer*4 sl,ss,el,nl,ns,iunit,line,ind,j,idn

      call zia(hist,65536)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         call xvread(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS

            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      return
      end
c=============================================================================
	subroutine comphist4(iunit,sl,ss,nl,ns,hist,inpbuf)
C
C Compute a histogram of a halfword image.
c	modeled after comphist2 in standard vicar subroutine library
c	limited to +/- 65535 for unsigned 16-bit CCD work
C
	implicit none
      integer*4 hist(-65536:65535)
      integer*4 inpbuf(32768)
      integer*4 sl,ss,nl,ns,el,line,j,iunit,ind,idn
c
      call zia(hist,131072)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         call xvread(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS
            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      return
      end
c============================================================================
      subroutine histat4(fhist,npts,mean,sigma,mindn,maxdn,maxfreq)
C
C   Compute mean and standard deviation (for halfword inputs only).
C
c	modified standard routine histat2
c
	implicit none
	integer*4 mindn,maxdn,maxfreq,npts,j,npixels
      integer*4 fhist(-65536:65535)
      real*4 mean,sigma,dn
      real*8 dmean,dsigma
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
   15 return
      end
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
	call chkstat (stat,'??E XVQout error',1,0,0)
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
c	for 'READ', 'WRITE', or 'UPDATE'
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
     1    		1,' ')
			    if (count.lt.0) call ibis_signal(ibis,count,
     1	  		abendcode)
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
PARM INP  TYPE=STRING  COUNT=(0:1) DEFAULT=--
PARM OUT  TYPE=STRING  COUNT=(0:3) DEFAULT=--
PARM SL   TYPE=INTEGER DEFAULT=1
PARM SS   TYPE=INTEGER DEFAULT=1
PARM NL   TYPE=INTEGER DEFAULT=0
PARM NS   TYPE=INTEGER DEFAULT=0
PARM SIZE TYPE=INTEGER COUNT=4     DEFAULT=(1,1,0,0)
PARM LIST TYPE=KEYWORD COUNT=(0:1) VALID=(LIST,LIST1,NOLIST) DEFAULT=--
PARM BACK TYPE=INTEGER COUNT=(0:1) DEFAULT=0
PARM THRE TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM MAXD TYPE=INTEGER COUNT=(0:1) DEFAULT=255
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
PARM AUTO TYPE=KEYWORD COUNT=(0:1) VALID=(AUTO,NOAUTO) DEFAULT=--
PARM SENS TYPE=REAL COUNT=(0:1) DEFAULT=1.0
PARM ASTEROID TYPE=KEYWORD VALID=(ASTEROID,NOAST) DEFAULT=NOAST 
PARM SAOMINP TYPE=INTEGER DEFAULT=400
PARM ASTMINP TYPE=INTEGER DEFAULT=35
PARM SAORATIO TYPE=REAL   DEFAULT=0.7
PARM ASTRATIO TYPE=REAL   DEFAULT=0.27
PARM ASTMAXW TYPE=REAL   DEFAULT=1.6
PARM ASTMAXL TYPE=REAL   DEFAULT=35.
END-PROC
.TITLE
VICAR*2 PROGRAM STARCAT3
.HELP
PURPOSE
  STARCAT3 is a VICAR*2 applications program which locates and integrates
  features consisting of contiguous pixels above a given threshold in 
  an image and places the results into a catalog file.


OPERATION

  STARCAT3 locates and associates all contiguous pixels above a user
  specified threshold into single objects.
  
  STARCAT3 uses the first moment of the marginal distribution with a static
  threshold to detect objects. The marginal distribution is the algorthim
  described in Auer and Altena (Reference 1) with the modification described
  by Stone (Reference 2).


DATA FILES

  The input image is a starfield. It may be from any type of sensor such
  as a CCD, vidicon, or digitized photogrpahic plate or film..
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
  You do not want detect objects which are really only do to subtle variations
  in the background noise.

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
  orginal data (either with F2 or by inlcuding it as the 2nd input data set).
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
                   ij    INij

  Default; BACK=0.

  PARAMETERS THRE, AUTO, SENS

  The selection of a good threshold is the key to successful STARCAT3
  execution. If you only wish to see the brightest objects then the
  threshold can be somewhat arbitrary, but if you wish to detect fainter
  objects then care must be taken and it becomes more and more important
  to have a flattened background.

  Internally the program sets this value to be the lowest value allowed
  by the data set format, 0 for BYTE, -32768 for HALF, and -2,147,483,547
  for FULL data sets. Thus, it will find 1 large object (the entire data
  set) if no value is entered.

  A good place to start is some multiple of the standard deviation from
  the histogram of the image. A reasonable value to start with is about
  3 sigma above the mean.

  Alternatively, the user can display the image and select a DN value
  that lies somewhat above background and start from there.

  If KEYWORD "AUTO" is specified then the program computes a local
  threshold based upon the average DN in the image plus the standard 
  deviation (sigma).  The sensitivity factor, SENS, is the number of
  sigma to multiply by according to the equation 

	autothreshold = mean + sensitivity*std_deviation

  If you start with the AUTO option then choose SENS=3.0. Default is 1.0
  though.


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
  image format is 'BYTE' and the saturation value. MAXD, is 255 then any
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

  SPIKE AND LOVAL PARAMETERS (OBJECT REJECTION CRITERIA)

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
  LIST1 prints other messages to screen. NOLIST prints no messages.

  OBJECTS PARAMETER
 
  The program will return the number of objects found in this parameter


  ASTEROID PARAMETERS

  The KEYWORD "ASTEROID" causes the following 6 keywords to be activated:
  SAOMINP,ASTMINP,SAORATIO,ASTRATIO,ASTMAXW,ASTMAXL

  The ASTEROID parameter was designed for streak detection on old Palomar
  48-inch plates. Those plates were exposed long enough such that asteroids
  would show up as long streaks and these parameters allow automated tentative
  identification.  When the ASTEROID option is activated the catalog will
  add "asteroid" or "star" in column 20 (classification) and "starcat3"
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


	***************************************************************
    	descriptor	description		        format		    units
	    ----------	-----------		        ------		    -----
     1	entry		catalog entry number	 full(1)		<none>		
     2	DN-bkg  	sum of DNs - background	 real(1)		counts
     3	sumpix  	number of pixels 	     full(2)		<none>
     4	sl      	starting line		     full(3)		<none>
     5	ss		    starting sample          full(4)		<none>
     6	nl		    number of lines		     full(5)		<none>
     7	ns		    number of samples        full(6)		<none>
     8	centsamp	centroid sample          real(2)		<none>
     9	centline	centroid line            real(3)		<none>
    10	maj axis	major axis length        real(4)		<none>
    11	min axis	minor axis length        real(5)		<none>
    12	ratio     	ratio minor/major axis	 real(6)		<none>
    13	rotangle  	rotation angle of ellip	 real(7)		degrees
    14	catalog   	source catalog           ascii(1)-'A16'	<none>
    15	ID		    catalog ID               ascii(2)-'A20'	<none>
    16	RA		    right ascension          real(8)		hours
    17	dec		    declination              real(9)		degrees
    18	mag calc	calculated magnitude     real(10)      	<none>
    19	mag ref   	catalog magnitude        real(11)      	<none>
    20	class     	classification           ascii(3)-'A8'	<none>
    21  satpix    	number of saturated pix  full(7)       	<none>
	***************************************************************

  Prior to Jan 21, 1995, STARCAT3 produced STARCAT catalogs of TYPE 1
  which had only the first 20 columns. However, the addition of column
  21 allows better analysis of starfield for use in other VICAR Astometry
  and photometry programs.
  

  STARCAT3 fills in columns 1 to 13 and 21 for each object it detects.
  If the ASTEROID parameter is active it will fill in columns 14 and 20.
  Column 14 (catalog) will contain "starcat3" and column 20 (class)
  will contain either "asteroid" or "star".
 
    Prior to Oct 6, 2009 the catalog stored sl, ss, centline, centsamp
    with respect to the window if the size or sl,ss,nl,ns parmeters were
    used. This can cause problems with further uses of the starcat catalog
    that is produced since most users want the objects in the original\
    image.  Now if you want the catalog to refer to a window, then
    you should extract the smaller image out of the larger and then
    run starcat3.

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
   11     Used for ASTEROID     R*4     1.0= SAO star, 2.0= asteroid
   12     rotation angle	R*4	about major axis, degrees

There is no program to interconvert the catalogs at this time.

LIMITATIONS:

  1. Maximum of 2000 objects for the flag buffer.
  2. Input data files are limited to BYTE, HALF and FULL


TIMIMG
   Exact timings are not yet available for the VAX: IBM time
   for a 1300 x 1300 image with 400 objects was 40 seconds.


REFERENCES

  1. L.H. Auer and W.F. van Altena, The Astronomical Journal, Vol83, 531 (1978)
  2. Ronald C. Stone, The Astronomical Journal, Vol 97, 1227 (1989)


ERROR MESSAGES

??E XVunit error on 1st input file
??E XVopen error on 1st input file
??E XVopen error on output catalog file
??E XVunit error on 2nd output image
??E XVopen error on 2nd output image
??E XVunit error on 3rd output image
??E XVopen error on 3rd output image

1) ??E Format XXXXXX not supported by STARCAT3
	- program does not support REAL, DOUB or COMP images

2) ??E **SUBROUTINE FIND - All 2000 bins filled
	- exceeded maximum number of objects per line (2000) - the only
	  way to change this limit is to change parameter maxobjs in the
	  main routine, recompile and relink the program..

3) ??E **SUBROUTINE AUTOT -  All values zero in minimum search

4) Catalog of type ',a16,' not starcat catalog
	- Cannot be fixed

PROGRAM HISTORY:

  1977-09-12 J.J.LORRE   Initial release
  1978-01-17 J.J.LORRE   Revisions and documentation
  1978-11-10 J.J.LORRE   Accomodate VICAR-77 label
  1982-08-23 R.D.TOAZ    Increased efficiency of dupe rejection
  1982-10-13 J.J.LORRE   New keywords: MAXP,MINI,MAXI,MINL,MAXL,MAXS,LINE,OVER,UNDER
  1984-08-23 M.E.MORRILL Conversion to VAX VICAR*1
  1984-09-25 M.E.MORRILL Conversion to VAX VICAR*2
  1985-02-06 M.E.MORRILL Now extracts VICAR1 and VICAR2 labels
  1988-07-01 J.J.LORRE   Added 4 fields into catalog: major,minor,angle values.
                         Background file.
  1990-08-15 R.J.BAMBERY Add AUTO and SENS parameters	
  1992-12-28 R.J.BAMBERY Modified to run under UNIX
  1993-02-09 R.J.Bambery Fixed bug that didn't give correct NL to catalog
  1994-01-31 R.J.Bambery Made portable using new IBIS-2 format for catalog
  1994-03-28 R.J.Bambery Completed port
  1994-07-05 R.J.Bambery Fixed bug in output byte file from half files for rmindn
  1994-08-26 R.J.Bambery Added ability to process FULL data sets
  1994-11-02 R.J.Bambery No longer outlines objects in output image. Makes detected
                         objects white on black bkg
  1994-12-19 R.J.Bambery remove error messages on UNIX systems
  1995-01-05 R.J.Bambery parameterize maxobjs and set nbin to 2000
  1995-01-21 R.J.Bambery Changed catalogs to TYPE 2 by adding column 21 (# of 
                         saturated pixels). Improved flow by better defining MAXD, 
                         MINI & MAXI
  1995-02-06 R.J.Bambery Added SPIKE and LOVAL parameters - changed PRINT to LIST to 
                         conform to other programs in this series
  1995-02-22 R.J.Bambery Removed optional background data set as second input -- it 
                         caused too many coding problems
  1996-02-20 R.J.Bambery Added OBJECTS return value
  2004-09-05 R.J.Bambery Added LIST1 parameter to LIST`
  2009-10-06 R.J.Bambery got rid of old xvparm calls (changed to xvp) added extra 
                         parameters to chkstat calls, redefined output sl and ss to 
                         be with respect to full image when only windowing with size 
                         into the image
  2009-10-17 R.J.Bambery Fix catalog windowing offset error (offset - 1)
  2010-01-30 R.J.Bambery Made compatible with 64-bit afids Build 793 Linux, MacOSX 
                         (both Intel/PowerPC)
  2011-06-20 R.J.Bambery Removed all warning messages generated from gcc 4.4.4 Build 
                         1069. Removed variables from several misaligned subroutince 
                         calls
  2012-06-06 R.J.Bambery Removed unnecessary parameters in calls, fixed
                         misaligned arrays, changed all dimension (1)
                         values to (*) in subroutines to avoid
                         "Fortran runtime error: Index '2' of dimension 1 of array 'id' 
                         above upper bound of 1" in gfortran 4.6.3
  2012-06-25 R.J.Bambery Change include 'errdefs.fin' to include 'errdefs'
  2012-07-03 R.J.Bambery Removed <tab> in front of continuation
                         lines to make backward compatible with
                         32-bit Linux gfortran 4.2.1, otherwise
                         compatible 64-bit Linux gfortran 4.6.3
  2013-09-13 R.J.Bambery Removed last vestiges of debug statements
  2013-09-15 R.J.Bambery Initialized arrays for ibis output. Found call mve
                         is a problem for double arrays (call mve(8,nbin,0,cenx,0,1)) fails
  2016-03-07 W.L.Bunch   Added ithr rounding; changed MAXI default from 4.0E9 to 1.0E38 to 
                         better allow for saturation in fullword images (from SP circa 1997).
                         Replaced xqout with xvqout for shell vicar's benefit.
                         Migrated to mipl.

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
 log/terminal. LIST1 prints
 minimal other messages

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
 MAXD=32767 for BYTE images
 MAXD=2147483647 for FULL images

.VARIABLE SPIKE
 INTEGER-OPTIONAL
 Reject object if DN value
 greater or equal to
 SPIKE found in object

.VARIABLE LOVAL
 INTEGER-OPTIONAL
 If this DN value found in
 object replace it with 
 THRE value.

.VARIABLE AUTO
 KEYWORD-OPTIONAL
 Compute a global threshold for
 the objects
 If AUTO=AUTO then THRE is
 ignored.

.VARIABLE SENS
 REAL-OPTIONAL
 Number of sigma to add to the
 autothreshold if AUTO=AUTO
 Ignored if AUTO=NOAUTO
 Default; SENS=1.0

.VARIABLE THRE
 INTEGER-OPTIONAL
 Threshold DN for real objects.
 Lowest value in image format
 After background subtraction
 Default: THRE=--

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
 Default; MAXI=MAXD+1

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

.VARIABLE LOVAL
 INTEGER-OPTIONAL

.VARIABLE AUTO
 KEYWORD-OPTIONAL
 Computes a global threshold value. If set to AUTO=AUTO, ignores THRE and
 uses SENS parameter. 

.VARIABLE SENS
 REAL-OPTIONAL
 Number of sigma to add to the mean to get the autothreshold
 Ignored if AUTO=NOAUTO.
 Default; SENS=1.0 

.VARIABLE THRE
 INTEGER-OPTIONAL
 Specifies the DN in input image at and above which pixels are
 considered real events. All DNs below THRE are ignored.
 By default this will be  the lowest integer value for the data format
 type, 0 for BYTE image, -32768 for HALF image, and -2,147,483,648 for
 FULL image. Note: THRE is compared to data after either the BACK
 Ignored if AUTO=AUTO.
 Default: THRE=--

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
 Default; MAXI=Infinity (2,147,483,647).

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
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

local objs type=integer count=1

local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

refgbl $echo

! Sep 13, 2013 - RJB
! TEST SCRIPT FOR STARCAT3
! tests BYTE, HALF images
!
! Vicar Programs:
!       translog ibis-list xvd  
! 
! External Programs"
!   <none>
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 
!
!
! this test script exercises the following parameters
!  INP, OUT, SIZE, SL, SS, NL, NS, LIST, BACK, THRE, MINI, MAXI,
!  MINP, MAXP, MAXS, MINS SPIKE, LOVAL
!
! Does not test ASTEROID parameters or AUTO or SENSE parameters
 
body
let _onfail="stop"
let $echo="no"

enable-log

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata

if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/carto nt
    ush ln -s /project/test_work/testdata/gll ft
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/carto nt
        ush ln -s /raid1/vicar_test_images/testdata/gll ft
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto nt
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/gll ft
    end-if
end-if
let _onfail="goto rm"

let $echo="yes"
!
! TEST 1 - Default parametersi - no objects - list catalog headers

starcat3 ft/s233m.vic out=s233m.cat
ibis-li s233m.cat units=units groups=groups formats=formats
!
! TEST 2 - set threshold, bkg=0, output to tcl - 417 objects
!
starcat3 ft/s233m.vic out=s233m1.cat thre=5940 back=0 objects=objs +
	maxd=32767 list=list
ibis-li s233m1.cat sr=1 nr=412 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
let $echo="no"
write "TEST 2 - number of stars = &objs"
let $echo="yes"
! test second output
!
! TEST 3 - check second output
!
starcat3 ft/s233m.vic out=(s233m2.cat,s233m2.byte) thre=5940 back=0 objects=objs +
      maxd=32767
ibis-li s233m2.cat sr=1 nr=20 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
let $echo="no"
write "TEST 3 - number of stars = &objs"

if (mode = "nobatch" or mode = "inter")
   xvd s233m2.byte
end-if
let $echo="yes"

!cform s233m.vic s233m.raw irange=(5850,6210) orange=(0,255) oform=byte

!starcat3 s233m.vic out=(s233m.cat,s233m.byte) thre=5940 back=0 objects=objs +
!      maxd=32767 mins=6 minl=6 maxl=50  maxs=50
!write "number of stars = &objs"

!test third output
!
! TEST 4 - check 3rd output
!
starcat3 ft/s233m.vic out=(s233m3.cat,s233m3.byte,s233m3.flag) +
thre=5940 back=0 objects=objs +
      maxd=32767 mins=6 minl=6 maxl=50  maxs=50
ibis-li s233m3.cat sr=1 nr=20 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
let $echo="no"
write "TEST 4 - number of stars = &objs"
if (mode = "nobatch" or mode = "inter")
   xvd s233m3.byte
   xvd s233m3.flag
end-if

let $echo="yes"

!
! TEST 5  - test small image area
!
starcat3 ft/s233m.vic out=s233m4.cat size=(1,1,512,512) objects=objs +
    thre=6000 back=0 mins=6 minl=6 maxl=50  maxs=50 maxd=32767
ibis-li s233m4.cat  nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
let $echo="no"
write "TEST 5 - number of stars = &objs"
let $echo="yes"
!
! TEST 6 - same for sl, ss, nl, ns
!
starcat3 ft/s233m.vic out=s233m5.cat sl=1 ss=1 nl=256 ns=256 objects=objs +
    thre=6000 maxd=32767
ibis-li s233m4.cat  nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
let $echo="no"
write "TEST 6 - number of stars = &objs"
let $echo="yes"
!
! TEST 7 - list stars
!
starcat3 ft/s233m.vic out=s233m6.cat maxd=32767 list=list
!
! TEST 8 -  list with 2nd image
!
starcat3 nt/1994se_1026b.corr out=(1994se_1026b1.cat,1994se_1026b1.out) +
    thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2 +
    maxs=100 maxl=100 objects=objs
let $echo="no"
write "TEST 8 - number of stars = &objs"
if (mode = "nobatch" or mode = "inter")
   xvd 1994se_1026b1.out
end-if

let $echo="yes"

!
! TEST 9 - list1 with 2nd image
!
starcat3 nt/1994se_1026b.corr out=(1994se_1026b2.cat,1994se_1026b2.out) +
	thre=221 back=176 list=list1 maxd=32767 mini=221 mins=2 minl=2 +
	maxs=100 maxl=100 objects=objs
let $echo="no"
write "TEST 9 - number of stars = &objs"
if (mode = "nobatch" or mode = "inter")
   xvd 1994se_1026b2.out
end-if

let $echo="yes"
!
! TEST 10 -
!
starcat3 nt/1994se_1026b.corr out=(1994se_1026b3.cat,1994se_1026b3.out) +
	thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2 +
	maxs=100 maxl=100 size=(1,1,150,150)

let $echo="no"
write "TEST 10 - number of stars = &objs"
if (mode = "nobatch" or mode = "inter")
   xvd 1994se_1026b3.out
end-if

let $echo="yes"
!
! TEST 11 -
!
starcat3 nt/1994se_1026b.corr out=(1994se_1026b4.cat,1994se_1026b4.out) +
	thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2 +
	maxs=100 maxl=100 size=(1,1,150,700) objects=objs

let $echo="no"
write "TEST 11 - number of stars = &objs"
if (mode = "nobatch" or mode = "inter")
   xvd 1994se_1026b4.out
end-if

rm>
let $echo="no"
ush rm -f ft
ush rm -f nt

disable-log

end-proc
$!-----------------------------------------------------------------------------
$ create tststarcat3.log
starcat3 ft/s233m.vic out=s233m.cat
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
*** Total of          0 stars found ***
ibis-li s233m.cat units=units groups=groups formats=formats
Beginning VICAR task ibis
 
Number of Rows:0  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
Group 'Entry': 1
Group 'DN-bkg': 2
Group 'tot pix': 3
Group 'sl': 4
Group 'ss': 5
Group 'nl': 6
Group 'ns': 7
Group 'centsamp': 8
Group 'centline': 9
Group 'maj axis': 10
Group 'min axis': 11
Group 'ratio': 12
Group 'rotangle': 13
Group 'Catalog': 14
Group 'ID': 15
Group 'RA': 16
Group 'dec': 17
Group 'mag calc': 18
Group 'mag ref': 19
Group 'class': 20
Group 'sat pix': 21
Unit '<none>': 1 3 4 5 6 7 8 9 10 11 12 14 15 18 19 20 21
Unit 'counts': 2
Unit 'degrees': 13 17
Unit 'hours': 16
starcat3 ft/s233m.vic out=s233m1.cat thre=5940 back=0 objects=objs  +
	maxd=32767 list=list
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--ENTSAMP CENT
              MAJ AXIS   MIN AXIS   RATIO  ROTANGLE
       1   41779.        7        1      198        2        5   199.71     1.43                  1.41     0.44     0.31
       2   47683.        8        1      211        2        6   213.25     1.37                  1.59     0.37     0.23
       3   47701.        8        1      919        2        7   922.37     1.12                  2.12     0.29     0.14
       4   71533.       12        1      190        3        6   192.83     1.75                  1.59     0.80     0.50
       5   65620.       11        1      204        3        7   206.46     1.55                  1.91     0.56     0.29
       6 7989653.     1320        1      215        3      676   546.66     1.49                191.20     0.51     0.00
       7   59644.       10        1      942        4        7   945.30     2.20                  1.92     0.95     0.49
       8   47613.        8        6      880        3        6   881.88     7.13                  1.83     0.60     0.33
       9   41658.        7        7      868        4        4   869.57     8.29                  1.17     0.72     0.61
      10   29754.        5       11      847        2        5   849.00    11.40                  1.48     0.23     0.16
      11   41684.        7       10      260        4        3   260.86    11.29                  1.09     0.52     0.48
      12   23856.        4       11      546        3        2   546.25    12.00                  0.71     0.43     0.61
      13   35753.        6       11      549        3        4   550.33    11.67                  0.95     0.74     0.78
      14   23773.        4       13      315        2        4   316.50    13.75                  1.12     0.42     0.37
      15   23804.        4       13      489        2        3   490.25    13.50                  0.85     0.47     0.55
      16   23859.        4       14      323        2        3   324.25    14.25                  0.87     0.35     0.41
      17   23804.        4       14      568        2        2   568.50    14.50                  0.50     0.50     1.00
      18   41612.        7       12      828        4        4   829.29    13.29                  1.36     0.53     0.39
      19   23829.        4       15      571        2        2   571.50    15.50                  0.50     0.50     1.00
      20   41705.        7       13      803        4        3   803.86    14.29                  1.03     0.83     0.80
      21   47702.        8       14      281        4        6   283.25    15.00                  1.81     0.41     0.23
      22   29768.        5       15      329        3        3   329.80    16.20                  0.78     0.72     0.93
      23   23815.        4       15      456        3        4   457.50    16.00                  1.22     0.50     0.41
      24   35717.        6       16      469        2        4   470.50    16.50                  1.00     0.41     0.41
      25   47632.        8       14      499        4        7   501.87    15.38                  1.98     0.67     0.34
      26   23836.        4       16      547        2        3   548.00    16.75                  0.71     0.43     0.61
      27   47592.        8       12      552        6        5   554.50    14.87                  2.13     0.57     0.27
      28   41693.        7       15      790        3        4   791.29    15.86                  1.04     0.82     0.78
      29   53603.        9       15      528        4        7   530.45    16.22                  2.18     0.56     0.26
      30   23795.        4       17      591        2        3   592.25    17.75                  0.87     0.35     0.41
      31  107156.       18       13      507        7        7   510.06    16.33                  1.73     1.61     0.93
      32   23837.        4       18      598        2        3   599.00    18.50                  0.81     0.31     0.38
      33   29862.        5       19      325        2        4   326.20    19.60                  1.19     0.42     0.35
      34   29786.        5       18      611        3        4   612.60    19.20                  1.03     0.73     0.71
      35   47672.        8       19      514        3        5   515.62    20.25                  1.39     0.50     0.36
      36   35756.        6       18      606        4        4   607.83    19.67                  1.32     0.53     0.40
      37   29831.        5       20      638        2        4   639.40    20.60                  1.08     0.32     0.29
      38   29784.        5       21      390        2        5   392.00    21.40                  1.48     0.23     0.16
      39   23811.        4       20      757        3        3   758.00    20.75                  0.93     0.57     0.62
      40   35715.        6       22      685        2        5   686.83    22.67                  1.39     0.33     0.23
      41   59517.       10       21      423        4        6   425.50    22.40                  1.43     0.80     0.56
      42   23840.        4       23      430        2        3   431.00    23.50                  0.81     0.31     0.38
      43   23799.        4       23      454        2        3   454.75    23.50                  0.85     0.47     0.55
      44   71517.       12       22      597        3       10   601.33    22.83                  3.05     0.45     0.15
      45  113075.       19       20      674        6       10   678.79    22.53                  2.34     1.37     0.59
      46   26974.        4       25       10        2        2    10.49    25.54                  0.50     0.49     0.98
      47   35755.        6       23      607        4        4   608.50    24.67                  1.26     0.74     0.59
      48   89322.       15       21      618        6        6   620.60    23.87                  2.05     1.11     0.54
      4940344424.     6738        1      223       27      688   562.91     8.85                172.16     4.77     0.03
      50   29758.        5       25      716        3        3   716.80    25.80                  0.77     0.72     0.93
      51   23812.        4       27      494        2        4   495.50    27.25                  1.17     0.26     0.22
      52   23844.        4       26      602        3        2   602.50    27.25                  0.85     0.47     0.55
      53   65528.       11       26      659        3        6   660.73    26.91                  1.60     0.79     0.49
      54  184589.       31       20      663        9       10   668.52    22.55                  2.85     2.09     0.73
      55  107185.       18       25      698        5        8   702.00    27.05                  1.97     1.13     0.57
      56   23799.        4       28      579        3        2   579.50    29.25                  0.85     0.47     0.55
      57   29739.        5       32      712        2        5   714.00    32.40                  1.42     0.47     0.33
      58   23772.        4       32      608        3        2   608.50    33.25                  0.85     0.47     0.55
      59   29829.        5       32      623        3        4   624.80    32.80                  1.35     0.32     0.24
      60   71602.       12       30      649        5        6   651.92    31.67                  1.75     1.32     0.76
      61   29755.        5       31      604        5        2   604.20    33.00                  1.44     0.28     0.19
      62  857125.       84       27       38       10       12    43.11    31.44                  2.37     1.94     0.82
      63   23800.        4       38      667        2        4   668.50    38.75                  1.17     0.26     0.22
      64   23909.        4       40      450        2        3   450.75    40.25                  0.87     0.35     0.41
      65   29746.        5       44      665        2        4   666.40    44.80                  1.02     0.39     0.38
      66   23881.        4       45      948        2        2   948.50    45.50                  0.50     0.50     1.00
      67  108733.       18       45      909        4        6   911.45    46.56                  1.43     0.99     0.69
      68   29746.        5       47      656        3        4   657.60    48.00                  1.02     0.63     0.62
      69   41709.        7       56      656        2        5   658.43    56.29                  1.42     0.39     0.27
      70   53649.        9       63      621        4        4   622.11    64.11                  1.00     0.99     0.99
      71   55048.        9       65      316        3        4   317.67    65.89                  0.97     0.69     0.72
      72   23770.        4       67      624        3        3   625.00    68.00                  0.87     0.50     0.58
      73 5455699.      808       21      619       51       39   639.14    47.24                  9.07     7.60     0.84
      74  168996.       28       67      582        6        8   585.35    69.14                  1.94     1.39     0.71
      75   59802.       10       84      406        3        5   408.20    85.10                  1.26     0.67     0.53
      76   30334.        5       86       53        2        3    54.20    86.60                  0.77     0.45     0.58
      77   24986.        4       86      402        2        3   403.00    86.24                  0.69     0.43     0.62
      78   29928.        5       91      798        2        4   799.40    91.20                  1.05     0.39     0.37
      79 1552143.      187       74      877       19       20   886.86    82.70                  3.57     3.34     0.93
      80   30142.        5       94      681        2        3   681.80    94.60                  0.77     0.45     0.58
      81   85511.       14      100      430        4        6   433.01   101.13                  1.42     0.89     0.63
      82   24028.        4      104       92        2        3    93.00   104.25                  0.71     0.43     0.61
      83   60133.       10      102      397        4        4   398.30   103.70                  1.00     0.79     0.79
      84   87238.       14      113      877        3        5   878.86   113.92                  1.36     0.78     0.57
      85   41613.        7      114      804        3        4   805.43   115.29                  1.25     0.55     0.44
      86   83983.       14      118      514        4        6   516.07   119.57                  1.43     0.83     0.58
      87  231375.       38      115     1008        8        6  1010.18   118.32                  2.05     1.47     0.72
      88 1052163.      116      121      265       13       15   271.22   126.79                  2.81     2.44     0.87
      89   48136.        8      144      546        3        4   547.50   145.00                  0.87     0.70     0.81
      90   35882.        6      160      482        3        3   483.33   160.66                  0.91     0.53     0.58
      91  149670.       25      157      657        7        6   659.76   160.00                  1.76     1.31     0.75
      92 1542822.      224      155      420       17       22   430.41   161.87                  5.17     3.42     0.66
      93  331973.       46      165      815        8        9   818.38   168.52                  1.97     1.67     0.85
      94   78649.       13      174      834        4        5   835.85   175.31                  1.16     0.98     0.85
      95 1085094.      142      170      525       15       18   532.14   176.52                  3.33     2.99     0.90
      96   29728.        5      184      537        4        2   537.40   185.40                  1.05     0.42     0.40
      97  111870.       18      190      436        5        7   438.75   191.89                  1.58     0.95     0.60
      98   35690.        6      200      590        3        3   591.17   200.83                  0.78     0.58     0.74
      99  379724.       50      204      919        7       10   923.54   206.95                  2.13     1.59     0.74
     100   53807.        9      209       38        4        4    39.11   210.44                  1.07     0.73     0.68
     101  136201.       22      209      250        4        7   252.77   210.63                  1.80     0.95     0.53
     102  133294.       22      208      308        6        6   310.73   210.45                  1.42     1.25     0.88
     103   49040.        8      213      352        2        4   353.50   213.50                  1.11     0.50     0.45
     104   29793.        5      214      788        2        4   789.60   214.60                  1.09     0.32     0.29
     105   66233.       11      214     1012        4        6  1014.36   215.73                  1.36     0.84     0.62
     106   35802.        6      221      655        2        4   656.50   221.50                  1.00     0.41     0.41
     107   23839.        4      226      776        2        2   776.50   226.50                  0.50     0.50     1.00
     108   65872.       11      224      490        5        4   491.64   226.00                  1.24     0.71     0.57
     109   47828.        8      229      807        4        4   808.50   230.87                  1.23     0.58     0.47
     110   78007.       13      233      359        3        6   361.16   234.15                  1.57     0.64     0.41
     111   29767.        5      235      559        3        3   559.80   235.80                  1.00     0.35     0.35
     112   95922.       16      242      447        7        5   449.38   245.44                  1.94     0.69     0.35
     113   29853.        5      250      580        2        4   581.60   250.80                  1.02     0.39     0.38
     114   47790.        8      250      783        3        4   784.50   251.12                  0.94     0.69     0.74
     115   41841.        7      262      765        3        4   766.43   263.00                  1.00     0.62     0.62
     116   29855.        5      275      482        2        4   483.40   275.40                  1.05     0.42     0.40
     117  372054.       46      272      329        7       10   333.47   274.97                  1.91     1.54     0.80
     118   48515.        8      283     1009        3        4  1010.25   283.75                  0.97     0.65     0.67
     119  161895.       25      284      821        5        7   823.83   285.87                  1.71     1.10     0.65
     120   23818.        4      291      756        2        3   757.00   291.50                  0.81     0.31     0.38
     121   29783.        5      291      574        3        3   574.80   291.80                  0.85     0.63     0.75
     122   23984.        4      296      182        3        2   182.75   297.25                  0.86     0.35     0.41
     123   23780.        4      300      666        2        3   666.75   300.75                  0.87     0.35     0.41
     124  104806.       16      302      143        4        5   144.99   303.51                  1.20     0.97     0.81
     125   72207.       12      307      771        4        5   773.25   308.42                  1.19     0.83     0.70
     126  113924.       19      307      573        5        8   577.11   309.37                  1.81     1.12     0.62
     127   23796.        4      314      534        2        4   535.50   314.25                  1.17     0.26     0.22
     128  135275.       22      313      470        7        6   472.46   316.69                  1.61     1.26     0.78
     129   29753.        5      317      646        4        3   647.00   318.40                  1.05     0.59     0.56
     130   97989.       15      320      121        4        5   123.09   321.40                  1.22     0.92     0.76
     131   23783.        4      322      643        2        4   644.50   322.75                  1.12     0.42     0.37
     132   29759.        5      323      504        3        3   504.80   324.20                  0.96     0.45     0.47
     133   35815.        6      327      647        2        4   648.67   327.67                  1.11     0.46     0.41
     134   36694.        6      334      327        2        4   328.50   334.33                  0.95     0.47     0.50
     135  229786.       38      332      894        9        6   896.61   335.52                  2.23     1.42     0.63
     136  624117.       78      334      778       11       12   783.70   339.22                  2.34     2.18     0.93
     137  572320.       77      342      454       11       12   459.82   346.66                  2.43     2.29     0.94
     138   41672.        7      353      684        3        4   685.43   354.14                  0.91     0.63     0.69
     139   23837.        4      355      650        2        3   651.00   355.50                  0.81     0.31     0.38
     140   29864.        5      360      595        2        3   595.80   360.40                  0.77     0.45     0.58
     141  708817.      118      350     1006       12       16  1012.50   355.11                  3.49     3.03     0.87
     142   23827.        4      360     1018        2        4  1019.50   360.25                  1.14     0.23     0.20
     143   89702.       14      361      105        4        5   107.22   362.36                  1.16     0.90     0.78
     144   41687.        7      369      487        3        3   488.00   369.86                  0.89     0.69     0.78
     145   98500.       16      368      500        4        6   502.56   369.37                  1.41     0.89     0.63
     146  572851.       94      364      996       12       16  1002.45   369.68                  4.08     1.96     0.48
     147   60295.       10      376      756        3        4   757.70   376.80                  1.11     0.69     0.63
     148   24269.        4      379      654        3        2   654.25   380.00                  0.70     0.43     0.62
     149   35712.        6      383      598        4        3   598.83   384.17                  1.14     0.56     0.49
     150   23799.        4      386      539        2        3   540.00   386.50                  0.81     0.31     0.38
     151   30281.        5      386      570        3        2   570.60   387.21                  0.78     0.45     0.58
     152  225599.       37      384      462        9       11   466.16   387.53                  2.75     1.57     0.57
     153   23815.        4      391      523        2        4   524.50   391.75                  1.17     0.26     0.22
     154   83683.       14      389      596        4        7   599.00   390.21                  1.70     0.84     0.49
     155   35678.        6      391      487        3        4   488.33   392.33                  1.07     0.55     0.51
     156   72269.       12      390      836        4        4   837.34   391.50                  1.11     0.84     0.76
     157   23858.        4      393      739        2        3   740.00   393.25                  0.71     0.43     0.61
     158   35682.        6      392      557        4        4   558.17   393.83                  1.22     0.88     0.72
     159   41668.        7      396      579        4        3   579.71   397.00                  1.12     0.62     0.55
     160   23836.        4      400      576        2        4   577.50   400.75                  1.13     0.42     0.37
     161   35686.        6      401      637        2        5   639.17   401.33                  1.36     0.44     0.32
     162   29751.        5      401      599        3        4   600.60   401.80                  1.08     0.66     0.61
     163   48517.        8      404       33        2        4    34.50   404.50                  1.11     0.50     0.45
     164  206795.       34      401      937        9        6   939.62   405.16                  1.92     1.41     0.73
     165   71849.       12      406      558        5        5   560.08   407.41                  1.67     1.00     0.60
     166   23798.        4      409      607        2        3   608.00   409.50                  0.81     0.31     0.38
     167   23778.        4      410      631        2        4   632.50   410.50                  1.14     0.44     0.38
     168   23805.        4      410      670        2        3   671.25   410.50                  0.85     0.47     0.55
     169   23844.        4      411      611        2        4   612.50   411.50                  1.21     0.21     0.17
     170   23823.        4      413      632        2        3   633.25   413.25                  0.87     0.35     0.41
     171   29765.        5      411      636        4        2   636.40   412.60                  1.02     0.49     0.48
     172   35835.        6      413      822        2        3   823.00   413.50                  0.82     0.50     0.61
     173   35687.        6      413      550        3        4   551.50   413.83                  1.20     0.52     0.44
     174   29772.        5      414      613        2        5   615.00   414.80                  1.42     0.37     0.26
     175   53513.        9      412      502        6        3   503.44   414.56                  1.83     0.68     0.37
     176   53551.        9      415      671        3        5   673.22   416.33                  1.51     0.75     0.50
     177  112333.       17      418       64        4        5    65.92   419.76                  1.32     0.97     0.73
     178   41636.        7      419      662        3        5   663.86   420.00                  1.46     0.76     0.52
     179   23841.        4      422      600        3        4   601.50   423.00                  1.31     0.19     0.15
     180  110535.       18      421      753        4        7   756.41   422.72                  1.66     0.91     0.55
     181   23802.        4      424      654        2        4   655.50   424.50                  1.21     0.21     0.17
     182   29735.        5      425      615        3        3   616.20   426.20                  0.96     0.45     0.47
     183   35991.        6      425      762        3        3   762.83   425.66                  0.76     0.67     0.87
     184   36069.        6      428      313        2        3   314.00   428.50                  0.81     0.50     0.61
     185  103863.       17      427      507        4        6   508.99   428.70                  1.44     0.95     0.66
     186   78600.       13      432      482        4        5   483.30   433.77                  1.32     0.88     0.67
     187   35728.        6      432      663        5        3   663.83   434.17                  1.40     0.57     0.41
     188   29768.        5      436      552        2        3   552.80   436.40                  0.77     0.45     0.58
     189   96016.       16      438      536        4        6   537.93   439.63                  1.59     0.94     0.59
     190   29760.        5      442      665        2        4   666.40   442.20                  1.02     0.39     0.38
     191   41616.        7      448      658        4        5   660.00   449.57                  1.73     0.34     0.19
     192   29814.        5      450      262        3        3   263.00   450.80                  0.83     0.51     0.62
     193   23790.        4      456      439        2        3   440.00   456.50                  0.81     0.31     0.38
     194   23830.        4      457      549        2        3   550.25   457.75                  0.87     0.35     0.41
     195   71792.       12      457      450        4        6   452.41   458.59                  1.32     0.86     0.65
     196   35719.        6      465      652        3        4   653.50   466.33                  0.99     0.70     0.71
     197   29932.        5      476      196        3        3   197.00   477.00                  0.63     0.63     1.00
     198   23788.        4      475      651        4        3   651.75   476.50                  1.37     0.22     0.16
     199   73492.       12      476      860        4        5   861.99   477.75                  1.10     0.88     0.79
     200  228820.       33      479      909        6        7   911.62   481.42                  1.66     1.41     0.85
     201   35862.        6      485      441        2        3   442.00   485.50                  0.82     0.50     0.61
     202  240949.       40      480        4        9       10     8.92   483.77                  2.23     1.67     0.75
     203   23816.        4      493       55        4        3    56.00   494.50                  1.22     0.50     0.41
     204   23807.        4      495       78        2        3    79.00   495.50                  0.81     0.31     0.38
     205   23794.        4      494      622        4        3   623.25   495.50                  1.37     0.22     0.16
     206   23788.        4      495      758        3        3   758.75   496.00                  0.83     0.71     0.85
     207   23793.        4      497       59        2        4    60.50   497.25                  1.12     0.42     0.37
     208   59614.       10      496       64        3        5    65.90   496.90                  1.26     0.63     0.50
     209   35747.        6      496      467        3        3   468.00   497.17                  0.96     0.47     0.49
     210   42221.        7      499      491        2        4   492.29   499.57                  1.04     0.48     0.46
     211   29770.        5      499      629        2        4   630.40   499.60                  1.09     0.32     0.29
     212   47577.        8      500       65        3        6    67.50   501.00                  1.63     0.59     0.36
     213   87558.       14      502      987        4        5   988.99   503.71                  1.22     0.87     0.71
     214   54144.        9      503      735        4        4   736.11   505.00                  1.06     0.84     0.79
     215   42097.        7      505      246        3        4   247.43   505.86                  0.91     0.62     0.69
     216   98156.       16      505      461        5        6   463.76   507.13                  1.38     0.97     0.70
     217   23855.        4      511      614        2        4   615.50   511.25                  1.17     0.26     0.22
     218   23834.        4      511      635        2        3   635.75   511.50                  0.85     0.47     0.55
     219   23833.        4      511      647        2        4   648.50   511.50                  1.21     0.21     0.17
     220  114971.       19      509      753        4        7   756.16   510.47                  1.62     0.93     0.57
     221   35708.        6      511      605        4        5   606.83   512.33                  1.58     0.44     0.28
     222   41707.        7      512      907        3        4   908.00   512.71                  1.12     0.62     0.55
     223   41666.        7      513      623        3        4   624.72   514.29                  1.08     0.82     0.76
     224   23786.        4      513      683        3        4   684.50   514.00                  1.31     0.19     0.15
     225   41627.        7      514      556        4        5   558.29   515.57                  1.29     0.89     0.69
     226   29744.        5      517      763        3        3   764.20   518.00                  0.89     0.75     0.84
     227   36509.        6      518      927        2        3   928.00   518.50                  0.81     0.50     0.61
     228   29763.        5      517      603        4        4   604.40   518.40                  1.34     0.53     0.39
     229   23824.        4      519      624        2        3   625.00   519.75                  0.71     0.43     0.61
     230   23787.        4      518      655        3        4   656.50   518.75                  1.37     0.22     0.16
     231   59569.       10      518      615        4        7   618.70   520.10                  2.07     0.78     0.38
     232   35709.        6      519      687        3        5   688.83   520.17                  1.57     0.40     0.26
     233   29748.        5      520      605        3        4   606.60   521.40                  1.15     0.60     0.52
     234   23809.        4      520      709        3        2   709.50   521.25                  0.85     0.47     0.55
     235   23805.        4      522      609        3        2   609.75   523.25                  0.87     0.35     0.41
     236   23842.        4      525      175        2        2   175.50   525.50                  0.50     0.50     1.00
     237   29834.        5      526      610        2        5   612.00   526.20                  1.42     0.37     0.26
     238  103338.       17      522      773        6        5   774.36   524.89                  1.49     1.01     0.68
     239  103180.       17      524      603        5        6   605.60   525.59                  1.54     0.95     0.61
     240   59498.       10      526      683        3        6   685.50   526.80                  1.87     0.57     0.31
     241   84197.       13      528       85        3        5    86.66   529.01                  1.24     0.77     0.62
     242   23818.        4      530      707        2        3   708.00   530.25                  0.71     0.43     0.61
     243   23820.        4      530      536        3        3   537.25   531.25                  1.12     0.35     0.32
     244   29815.        5      531      623        2        4   624.40   531.40                  1.09     0.32     0.29
     245   35705.        6      532      524        3        5   526.17   533.17                  1.40     0.57     0.41
     246   47580.        8      532      588        4        5   590.00   533.75                  1.50     0.66     0.44
     247   59461.       10      531      629        5        8   632.20   532.70                  2.55     0.48     0.19
     248   23801.        4      534      710        2        3   711.00   534.50                  0.81     0.31     0.38
     249   35712.        6      535      544        2        5   546.33   535.83                  1.50     0.32     0.21
     250   65554.       11      534      598        4        6   600.18   535.18                  1.41     0.93     0.66
     251   41631.        7      534      696        4        5   698.14   535.57                  1.27     0.87     0.69
     252  105182.       17      534      487        5        5   489.06   536.23                  1.34     1.04     0.78
     253   23810.        4      537      527        2        3   528.25   537.50                  0.85     0.47     0.55
     254   29770.        5      536      621        3        3   622.20   536.80                  0.77     0.72     0.93
     255   47612.        8      534      692        5        5   694.00   536.38                  1.62     0.78     0.48
     256   23781.        4      538      373        2        4   374.50   538.50                  1.21     0.21     0.17
     257   23796.        4      538      532        2        3   533.00   538.75                  0.71     0.43     0.61
     258   65653.       11      535      626        5        4   627.09   536.64                  1.18     0.86     0.72
     259   29774.        5      536      703        4        2   703.60   537.40                  1.02     0.49     0.48
     260   41672.        7      539      579        2        4   580.28   539.57                  1.04     0.48     0.46
     261   53626.        9      538      613        3        7   616.00   538.89                  2.29     0.40     0.17
     262   41691.        7      542      534        2        6   536.14   542.71                  1.83     0.37     0.20
     263   29731.        5      541      592        3        5   594.00   541.60                  1.59     0.33     0.21
     264   53561.        9      540      635        4        5   636.78   541.44                  1.53     0.99     0.65
     265   29727.        5      542      598        4        2   598.20   543.20                  1.19     0.34     0.28
     266   29917.        5      546      828        3        3   829.00   547.00                  0.63     0.63     1.00
     267   53646.        9      547      594        3        5   596.11   547.78                  1.24     0.72     0.58
     268   29752.        5      547      616        3        3   617.20   547.80                  0.78     0.72     0.93
     269   29742.        5      549      610        4        4   611.80   550.40                  1.51     0.33     0.22
     270   47582.        8      553      620        5        5   622.25   554.75                  1.23     1.17     0.96
     271   35687.        6      556      615        5        2   615.50   558.17                  1.34     0.50     0.37
     272   47698.        8      556      624        5        4   625.38   558.25                  1.73     0.65     0.37
     273   29726.        5      558      595        4        2   595.80   559.60                  1.02     0.39     0.38
     274   29748.        5      558      617        4        3   618.40   559.80                  1.28     0.61     0.47
     275   23802.        4      562      635        2        3   636.00   562.25                  0.71     0.43     0.61
     276   23794.        4      564      573        2        4   574.50   564.75                  1.17     0.26     0.22
     277   23766.        4      562      608        4        2   608.50   563.50                  1.14     0.44     0.38
     278   23801.        4      564      594        3        3   594.75   565.00                  1.05     0.29     0.28
     279  160739.       27      562      615        8       13   620.74   565.81                  3.46     1.39     0.40
     280  121566.       20      566      920        4        6   922.75   567.55                  1.50     1.04     0.69
     281   66123.       11      572      703        3        5   704.54   573.00                  1.30     0.73     0.56
     282   29922.        5      577      642        2        3   642.80   577.40                  0.77     0.45     0.58
     283   29720.        5      577      632        3        4   633.80   577.80                  1.35     0.32     0.24
     284   45011.        7      587      322        2        5   324.45   587.70                  1.41     0.39     0.28
     285   29785.        5      587      523        3        4   524.60   588.00                  1.11     0.46     0.41
     286   65974.       11      588      203        4        4   204.64   589.64                  1.15     0.77     0.67
     287  188606.       31      593      952        5        9   956.27   594.80                  2.10     1.20     0.57
     288  340483.       57      588      631       11        9   634.44   593.35                  2.36     2.15     0.91
     289  472147.       78      588      934       11       11   939.31   593.17                  2.58     2.49     0.96
     290   61516.       10      611       47        3        4    48.50   611.80                  1.02     0.74     0.73
     291   71843.       12      609      575        5        5   577.00   611.50                  1.37     0.83     0.61
     292   53920.        9      613      254        3        4   255.33   614.00                  1.06     0.65     0.62
     293   23835.        4      613      873        3        2   873.25   614.00                  0.71     0.43     0.61
     294   30539.        5      616     1012        2        3  1012.80   616.40                  0.80     0.46     0.58
     295   35695.        6      615      748        4        2   748.50   616.17                  1.07     0.49     0.46
     296  152887.       24      614      803        5        7   806.26   616.41                  1.61     1.17     0.73
     297   89972.       15      619      281        4        5   282.67   620.66                  1.20     0.99     0.83
     298   47920.        8      623      615        3        3   615.88   624.13                  0.86     0.68     0.79
     299  313500.       52      619      604        8       11   608.58   623.18                  2.66     1.91     0.72
     300   89733.       15      623      902        4        7   904.80   624.53                  1.67     0.81     0.48
     301   65568.       11      628      912        5        3   912.82   629.73                  1.26     0.76     0.60
     302   23813.        4      635      659        2        4   660.50   635.75                  1.17     0.26     0.22
     303   54978.        9      635      879        3        4   880.44   635.67                  1.08     0.66     0.61
     304   29960.        5      641      905        2        3   906.20   641.40                  0.77     0.45     0.58
     305   23785.        4      642     1008        2        2  1008.50   642.50                  0.50     0.50     1.00
     306   47579.        8      641      781        4        4   782.00   642.50                  1.06     0.94     0.88
     307   29818.        5      645      778        3        3   779.20   646.00                  1.05     0.51     0.49
     308  120489.       20      647       10        4        8    13.85   648.35                  2.08     0.90     0.43
     309   23845.        4      651      461        2        3   462.25   651.50                  0.85     0.47     0.55
     310   53999.        9      651      615        4        4   616.67   652.44                  0.95     0.81     0.85
     311   59692.       10      656      608        3        5   610.40   657.00                  1.20     0.77     0.65
     312   41937.        7      659      765        3        4   766.14   659.86                  1.01     0.60     0.60
     313   23860.        4      664      617        2        2   617.50   664.50                  0.50     0.50     1.00
     314   31144.        5      665      865        2        3   866.19   665.40                  0.77     0.45     0.58
     315   29799.        5      669      780        3        3   781.20   670.00                  0.83     0.51     0.62
     316   23791.        4      671     1015        2        4  1016.50   671.75                  1.17     0.26     0.22
     317  160353.       25      672      805        7        7   807.70   674.55                  1.56     1.33     0.85
     318  249200.       37      678      847        6        9   850.64   680.34                  1.94     1.41     0.73
     319  537918.       86      682      930       12       11   935.16   687.28                  2.95     2.32     0.79
     320   29748.        5      699      797        2        4   798.20   699.20                  1.17     0.32     0.27
     321   30094.        5      700      435        2        3   435.80   700.40                  0.77     0.45     0.58
     322 1066406.      135      694      778       16       17   785.39   702.76                  3.16     2.90     0.92
     323   42293.        7      708      914        3        4   915.43   709.14                  0.91     0.62     0.69
     324 1345534.      218      693      950       19       19   958.91   702.69                  4.47     3.97     0.89
     325   48036.        8      721      115        4        4   116.25   722.88                  1.14     0.70     0.61
     326  118574.       19      728      757        5        6   759.32   729.84                  1.36     1.12     0.82
     327   59799.       10      731      342        4        5   343.80   732.40                  1.28     0.75     0.59
     328  235822.       39      734      469        7        9   472.82   737.29                  2.11     1.63     0.77
     329   60842.       10      755      798        3        4   799.79   756.00                  0.96     0.77     0.80
     330   99347.       16      755      289        4        5   290.57   756.38                  1.25     1.00     0.80
     331   23852.        4      758      595        3        2   595.50   759.25                  0.85     0.47     0.55
     332   80480.       13      758       79        4        5    80.85   759.23                  1.17     0.87     0.74
     333   29756.        5      772      870        2        3   871.20   772.40                  0.77     0.45     0.58
     334   59873.       10      771      527        4        4   528.70   772.30                  1.00     0.79     0.79
     335   99356.       16      774      253        4        5   254.83   775.31                  1.27     0.97     0.76
     336   35807.        6      780      162        2        4   163.50   780.33                  0.96     0.47     0.49
     337   42052.        7      791      434        2        4   435.71   791.43                  1.04     0.48     0.46
     338   23864.        4      796      658        2        3   659.00   796.50                  0.81     0.31     0.38
     339   53824.        9      803       51        3        5    53.33   803.89                  1.33     0.57     0.43
     340   47674.        8      821      472        3        4   473.38   822.00                  1.01     0.68     0.68
     341   59952.       10      832      561        3        4   562.20   832.90                  1.02     0.77     0.76
     342   23916.        4      837      681        2        2   681.50   837.50                  0.50     0.50     1.00
     343  351268.       49      833      573        9        8   576.42   837.13                  1.94     1.77     0.91
     344  217305.       36      836      903        8        8   906.12   839.75                  2.06     1.54     0.75
     345   84314.       14      847     1008        4        7  1010.92   848.50                  1.76     0.66     0.37
     346  125855.       21      852      307        4        7   309.62   853.48                  1.79     0.95     0.53
     347 2571484.      332      836      139       25       24   150.27   847.23                  4.76     4.61     0.97
     348  206465.       29      855      732        6        7   734.46   857.04                  1.58     1.31     0.83
     349   24479.        4      866      801        2        2   801.50   866.49                  0.50     0.50     1.00
     350   36117.        6      873      465        3        3   465.66   873.67                  0.91     0.53     0.58
     351   24201.        4      886       98        2        3    99.00   886.25                  0.71     0.43     0.61
     352  100451.       16      898      698        4        6   700.50   899.25                  1.30     0.95     0.73
     353  100507.       16      907      428        5        5   430.25   909.38                  1.30     1.01     0.78
     354   54819.        9      915       38        3        4    39.33   916.11                  0.97     0.69     0.72
     355   97388.       16      920      141        4        5   143.25   921.63                  1.20     1.04     0.86
     356   54091.        9      922      530        3        4   531.34   923.11                  0.97     0.70     0.72
     357  726330.       85      915      754       11       12   759.31   920.11                  2.36     2.19     0.93
     358   48468.        8      924      421        3        4   422.38   925.25                  1.03     0.60     0.58
     359   54035.        9      921      137        9        2   137.11   925.00                  2.58     0.26     0.10
     360  130675.       21      926      190        5        6   192.08   927.96                  1.40     1.20     0.86
     361  132674.       22      926      969        5        6   971.23   927.86                  1.59     1.16     0.73
     362   60038.       10      932      117        4        4   118.70   933.40                  1.12     0.76     0.68
     363  180766.       26      931      905        5        7   907.96   933.05                  1.58     1.21     0.77
     364   81577.       13      936      547        3        5   549.00   936.99                  1.31     0.72     0.55
     365   24721.        4      954      498        2        3   498.99   954.50                  0.80     0.31     0.39
     366   23934.        4      955      505        2        3   506.00   955.50                  0.81     0.31     0.38
     367   53574.        9      974      185        4        5   186.56   975.89                  1.29     0.82     0.64
     368   47600.        8      977      205        3        4   206.37   978.00                  0.99     0.87     0.87
     369   23797.        4      978      220        2        4   221.50   978.75                  1.17     0.26     0.22
     370   23811.        4      988      232        3        3   232.75   989.25                  1.12     0.35     0.32
     371   23855.        4      989      698        2        2   698.50   989.50                  0.50     0.50     1.00
     372   36694.        6      990     1000        2        4  1001.51   990.32                  0.93     0.47     0.50
     373   36035.        6      996       99        2        3   100.00   996.50                  0.82     0.50     0.61
     374  459385.       57      991      552        8       10   556.48   994.32                  2.07     1.77     0.86
     375   47617.        8      998      122        3        6   124.50   999.25                  1.50     0.82     0.55
     376   35753.        6      998      250        3        5   251.83   999.00                  1.35     0.56     0.42
     377  114191.       19      996      976        5        5   978.00   997.63                  1.37     1.10     0.80
     378   35696.        6      999      257        3        4   258.83   999.83                  1.15     0.79     0.69
     379   36301.        6     1000      803        3        3   803.84  1001.16                  0.78     0.58     0.74
     380   41673.        7     1002      269        4        5   271.00  1003.86                  1.49     0.70     0.47
     381   41657.        7     1003      275        3        6   277.43  1003.57                  1.63     0.63     0.39
     382   23835.        4     1005      259        3        2   259.75  1006.00                  0.71     0.43     0.61
     383   29782.        5     1005      266        4        2   266.40  1006.80                  1.17     0.48     0.41
     384   47659.        8     1005      358        4        5   360.00  1006.37                  1.51     0.46     0.30
     385   23814.        4     1008      296        2        2   296.50  1008.50                  0.50     0.50     1.00
     386   29762.        5     1010      305        2        4   306.40  1010.40                  1.05     0.42     0.40
     387  131019.       22     1004      618        8        7   621.59  1007.64                  1.90     1.55     0.81
     388   47627.        8     1009      292        4        5   294.00  1010.75                  1.50     0.66     0.44
     389   41652.        7     1009      358        4        6   360.57  1010.29                  1.87     0.28     0.15
     390   29849.        5     1011      372        2        4   373.40  1011.60                  1.05     0.42     0.40
     391   23784.        4     1010      385        3        3   386.00  1010.75                  1.05     0.29     0.28
     392   23779.        4     1010      611        3        3   612.00  1011.00                  0.87     0.50     0.58
     393   23809.        4     1011      696        2        3   697.00  1011.75                  0.71     0.43     0.61
     394  156424.       22     1009      982        4        6   984.50  1010.66                  1.53     1.02     0.67
     395   53550.        9     1010      327        4        5   328.78  1011.45                  1.28     0.88     0.68
     396   29748.        5     1011      402        3        4   403.80  1012.20                  1.22     0.65     0.53
     397   35713.        6     1010      615        4        4   616.83  1011.16                  1.35     0.67     0.49
     398   41670.        7     1012      333        3        7   336.00  1012.57                  2.08     0.43     0.21
     399   29727.        5     1011      369        4        2   369.20  1012.40                  1.02     0.39     0.38
     400  113132.       19     1008      317        8        8   321.05  1011.84                  2.03     1.82     0.90
     401   29876.        5     1013      350        3        3   351.20  1013.80                  0.96     0.45     0.47
     402   29787.        5     1013      429        3        4   430.60  1014.20                  1.23     0.28     0.23
     403   77404.       13     1013      379        4       10   383.54  1014.69                  2.92     0.64     0.22
     404   71442.       12     1014      691        3        7   694.42  1014.75                  2.11     0.69     0.33
     405   83257.       14     1011      725        6        7   728.07  1013.86                  1.80     1.36     0.75
     406   23827.        4     1017      667        3        3   668.25  1017.75                  1.12     0.35     0.32
     407   23832.        4     1018      691        2        3   691.75  1018.75                  0.87     0.35     0.41
     40839502976.     6368      974       90       51      371   219.15  1009.19                 79.03    11.52     0.15
     409   47743.        8     1023      465        2        7   468.00  1023.88                  1.87     0.33     0.18
     410   23841.        4     1023      504        2        3   505.25  1023.75                  0.87     0.35     0.41
     411   89432.       15     1021      525        4       11   530.93  1023.53                  3.19     0.83     0.26
     412   24004.        4     1023      540        2        3   541.25  1023.75                  0.87     0.35     0.41
     413   53774.        9     1023      544        2        7   546.45  1023.78                  2.08     0.35     0.17
     414 4527637.      755     1011      550       14      127   616.18  1020.42                 28.68     3.01     0.10
     415 2600288.      435     1015      670       10       91   713.22  1021.35                 24.03     2.08     0.09
     416   83707.       14     1020      761        5        9   764.86  1023.22                  2.26     1.25     0.55
     417   77540.       13     1023      771        2       10   775.85  1023.77                  2.63     0.41     0.16
*** Total of        417 stars found ***
ibis-li s233m1.cat sr=1 nr=412 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
Beginning VICAR task ibis
 
Number of Rows:417  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
 
Rows: 1:30
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1    41779.00           7           1         198           2           5      199.71        1.43        1.41
           2    47683.00           8           1         211           2           6      213.25        1.37        1.59
           3    47701.00           8           1         919           2           7      922.37        1.12        2.12
           4    71533.00          12           1         190           3           6      192.83        1.75        1.59
           5    65620.00          11           1         204           3           7      206.46        1.55        1.91
           6  7989653.00        1320           1         215           3         676      546.66        1.49      191.20
           7    59644.00          10           1         942           4           7      945.30        2.20        1.92
           8    47613.00           8           6         880           3           6      881.88        7.13        1.83
           9    41658.00           7           7         868           4           4      869.57        8.29        1.17
          10    29754.00           5          11         847           2           5      849.00       11.40        1.48
          11    41684.00           7          10         260           4           3      260.86       11.29        1.09
          12    23856.00           4          11         546           3           2      546.25       12.00        0.71
          13    35753.00           6          11         549           3           4      550.33       11.67        0.95
          14    23773.00           4          13         315           2           4      316.50       13.75        1.12
          15    23804.00           4          13         489           2           3      490.25       13.50        0.85
          16    23859.00           4          14         323           2           3      324.25       14.25        0.87
          17    23804.00           4          14         568           2           2      568.50       14.50        0.50
          18    41612.00           7          12         828           4           4      829.29       13.29        1.36
          19    23829.00           4          15         571           2           2      571.50       15.50        0.50
          20    41705.00           7          13         803           4           3      803.86       14.29        1.03
          21    47702.00           8          14         281           4           6      283.25       15.00        1.81
          22    29768.00           5          15         329           3           3      329.80       16.20        0.78
          23    23815.00           4          15         456           3           4      457.50       16.00        1.22
          24    35717.00           6          16         469           2           4      470.50       16.50        1.00
          25    47632.00           8          14         499           4           7      501.87       15.38        1.98
          26    23836.00           4          16         547           2           3      548.00       16.75        0.71
          27    47592.00           8          12         552           6           5      554.50       14.87        2.13
          28    41693.00           7          15         790           3           4      791.29       15.86        1.04
          29    53603.00           9          15         528           4           7      530.45       16.22        2.18
          30    23795.00           4          17         591           2           3      592.25       17.75        0.87
 
Rows: 31:60
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
          31   107156.00          18          13         507           7           7      510.06       16.33        1.73
          32    23837.00           4          18         598           2           3      599.00       18.50        0.81
          33    29862.00           5          19         325           2           4      326.20       19.60        1.19
          34    29786.00           5          18         611           3           4      612.60       19.20        1.03
          35    47672.00           8          19         514           3           5      515.62       20.25        1.39
          36    35756.00           6          18         606           4           4      607.83       19.67        1.32
          37    29831.00           5          20         638           2           4      639.40       20.60        1.08
          38    29784.00           5          21         390           2           5      392.00       21.40        1.48
          39    23811.00           4          20         757           3           3      758.00       20.75        0.93
          40    35715.00           6          22         685           2           5      686.83       22.67        1.39
          41    59517.00          10          21         423           4           6      425.50       22.40        1.43
          42    23840.00           4          23         430           2           3      431.00       23.50        0.81
          43    23799.00           4          23         454           2           3      454.75       23.50        0.85
          44    71517.00          12          22         597           3          10      601.33       22.83        3.05
          45   113075.00          19          20         674           6          10      678.79       22.53        2.34
          46    26974.00           4          25          10           2           2       10.49       25.54        0.50
          47    35755.00           6          23         607           4           4      608.50       24.67        1.26
          48    89322.00          15          21         618           6           6      620.60       23.87        2.05
          49 40344424.00        6738           1         223          27         688      562.91        8.85      172.16
          50    29758.00           5          25         716           3           3      716.80       25.80        0.77
          51    23812.00           4          27         494           2           4      495.50       27.25        1.17
          52    23844.00           4          26         602           3           2      602.50       27.25        0.85
          53    65528.00          11          26         659           3           6      660.73       26.91        1.60
          54   184589.00          31          20         663           9          10      668.52       22.55        2.85
          55   107185.00          18          25         698           5           8      702.00       27.05        1.97
          56    23799.00           4          28         579           3           2      579.50       29.25        0.85
          57    29739.00           5          32         712           2           5      714.00       32.40        1.42
          58    23772.00           4          32         608           3           2      608.50       33.25        0.85
          59    29829.00           5          32         623           3           4      624.80       32.80        1.35
          60    71602.00          12          30         649           5           6      651.92       31.67        1.75
 
Rows: 61:90
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
          61    29755.00           5          31         604           5           2      604.20       33.00        1.44
          62   857125.00          84          27          38          10          12       43.11       31.44        2.37
          63    23800.00           4          38         667           2           4      668.50       38.75        1.17
          64    23909.00           4          40         450           2           3      450.75       40.25        0.87
          65    29746.00           5          44         665           2           4      666.40       44.80        1.02
          66    23881.00           4          45         948           2           2      948.50       45.50        0.50
          67   108733.00          18          45         909           4           6      911.45       46.56        1.43
          68    29746.00           5          47         656           3           4      657.60       48.00        1.02
          69    41709.00           7          56         656           2           5      658.43       56.29        1.42
          70    53649.00           9          63         621           4           4      622.11       64.11        1.00
          71    55048.00           9          65         316           3           4      317.67       65.89        0.97
          72    23770.00           4          67         624           3           3      625.00       68.00        0.87
          73  5455699.00         808          21         619          51          39      639.14       47.24        9.07
          74   168996.00          28          67         582           6           8      585.35       69.14        1.94
          75    59802.00          10          84         406           3           5      408.20       85.10        1.26
          76    30334.00           5          86          53           2           3       54.20       86.60        0.77
          77    24986.00           4          86         402           2           3      403.00       86.24        0.69
          78    29928.00           5          91         798           2           4      799.40       91.20        1.05
          79  1552143.00         187          74         877          19          20      886.86       82.70        3.57
          80    30142.00           5          94         681           2           3      681.80       94.60        0.77
          81    85511.00          14         100         430           4           6      433.01      101.13        1.42
          82    24028.00           4         104          92           2           3       93.00      104.25        0.71
          83    60133.00          10         102         397           4           4      398.30      103.70        1.00
          84    87238.00          14         113         877           3           5      878.86      113.92        1.36
          85    41613.00           7         114         804           3           4      805.43      115.29        1.25
          86    83983.00          14         118         514           4           6      516.07      119.57        1.43
          87   231375.00          38         115        1008           8           6     1010.18      118.32        2.05
          88  1052163.00         116         121         265          13          15      271.22      126.79        2.81
          89    48136.00           8         144         546           3           4      547.50      145.00        0.87
          90    35882.00           6         160         482           3           3      483.33      160.66        0.91
 
Rows: 91:120
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
          91   149670.00          25         157         657           7           6      659.76      160.00        1.76
          92  1542822.00         224         155         420          17          22      430.41      161.87        5.17
          93   331973.00          46         165         815           8           9      818.38      168.52        1.97
          94    78649.00          13         174         834           4           5      835.85      175.31        1.16
          95  1085094.00         142         170         525          15          18      532.14      176.52        3.33
          96    29728.00           5         184         537           4           2      537.40      185.40        1.05
          97   111870.00          18         190         436           5           7      438.75      191.89        1.58
          98    35690.00           6         200         590           3           3      591.17      200.83        0.78
          99   379724.00          50         204         919           7          10      923.54      206.95        2.13
         100    53807.00           9         209          38           4           4       39.11      210.44        1.07
         101   136201.00          22         209         250           4           7      252.77      210.63        1.80
         102   133294.00          22         208         308           6           6      310.73      210.45        1.42
         103    49040.00           8         213         352           2           4      353.50      213.50        1.11
         104    29793.00           5         214         788           2           4      789.60      214.60        1.09
         105    66233.00          11         214        1012           4           6     1014.36      215.73        1.36
         106    35802.00           6         221         655           2           4      656.50      221.50        1.00
         107    23839.00           4         226         776           2           2      776.50      226.50        0.50
         108    65872.00          11         224         490           5           4      491.64      226.00        1.24
         109    47828.00           8         229         807           4           4      808.50      230.87        1.23
         110    78007.00          13         233         359           3           6      361.16      234.15        1.57
         111    29767.00           5         235         559           3           3      559.80      235.80        1.00
         112    95922.00          16         242         447           7           5      449.38      245.44        1.94
         113    29853.00           5         250         580           2           4      581.60      250.80        1.02
         114    47790.00           8         250         783           3           4      784.50      251.12        0.94
         115    41841.00           7         262         765           3           4      766.43      263.00        1.00
         116    29855.00           5         275         482           2           4      483.40      275.40        1.05
         117   372054.00          46         272         329           7          10      333.47      274.97        1.91
         118    48515.00           8         283        1009           3           4     1010.25      283.75        0.97
         119   161895.00          25         284         821           5           7      823.83      285.87        1.71
         120    23818.00           4         291         756           2           3      757.00      291.50        0.81
 
Rows: 121:150
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         121    29783.00           5         291         574           3           3      574.80      291.80        0.85
         122    23984.00           4         296         182           3           2      182.75      297.25        0.86
         123    23780.00           4         300         666           2           3      666.75      300.75        0.87
         124   104806.00          16         302         143           4           5      144.99      303.51        1.20
         125    72207.00          12         307         771           4           5      773.25      308.42        1.19
         126   113924.00          19         307         573           5           8      577.11      309.37        1.81
         127    23796.00           4         314         534           2           4      535.50      314.25        1.17
         128   135275.00          22         313         470           7           6      472.46      316.69        1.61
         129    29753.00           5         317         646           4           3      647.00      318.40        1.05
         130    97989.00          15         320         121           4           5      123.09      321.40        1.22
         131    23783.00           4         322         643           2           4      644.50      322.75        1.12
         132    29759.00           5         323         504           3           3      504.80      324.20        0.96
         133    35815.00           6         327         647           2           4      648.67      327.67        1.11
         134    36694.00           6         334         327           2           4      328.50      334.33        0.95
         135   229786.00          38         332         894           9           6      896.61      335.52        2.23
         136   624117.00          78         334         778          11          12      783.70      339.22        2.34
         137   572320.00          77         342         454          11          12      459.82      346.66        2.43
         138    41672.00           7         353         684           3           4      685.43      354.14        0.91
         139    23837.00           4         355         650           2           3      651.00      355.50        0.81
         140    29864.00           5         360         595           2           3      595.80      360.40        0.77
         141   708817.00         118         350        1006          12          16     1012.50      355.11        3.49
         142    23827.00           4         360        1018           2           4     1019.50      360.25        1.14
         143    89702.00          14         361         105           4           5      107.22      362.36        1.16
         144    41687.00           7         369         487           3           3      488.00      369.86        0.89
         145    98500.00          16         368         500           4           6      502.56      369.37        1.41
         146   572851.00          94         364         996          12          16     1002.45      369.68        4.08
         147    60295.00          10         376         756           3           4      757.70      376.80        1.11
         148    24269.00           4         379         654           3           2      654.25      380.00        0.70
         149    35712.00           6         383         598           4           3      598.83      384.17        1.14
         150    23799.00           4         386         539           2           3      540.00      386.50        0.81
 
Rows: 151:180
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         151    30281.00           5         386         570           3           2      570.60      387.21        0.78
         152   225599.00          37         384         462           9          11      466.16      387.53        2.75
         153    23815.00           4         391         523           2           4      524.50      391.75        1.17
         154    83683.00          14         389         596           4           7      599.00      390.21        1.70
         155    35678.00           6         391         487           3           4      488.33      392.33        1.07
         156    72269.00          12         390         836           4           4      837.34      391.50        1.11
         157    23858.00           4         393         739           2           3      740.00      393.25        0.71
         158    35682.00           6         392         557           4           4      558.17      393.83        1.22
         159    41668.00           7         396         579           4           3      579.71      397.00        1.12
         160    23836.00           4         400         576           2           4      577.50      400.75        1.13
         161    35686.00           6         401         637           2           5      639.17      401.33        1.36
         162    29751.00           5         401         599           3           4      600.60      401.80        1.08
         163    48517.00           8         404          33           2           4       34.50      404.50        1.11
         164   206795.00          34         401         937           9           6      939.62      405.16        1.92
         165    71849.00          12         406         558           5           5      560.08      407.41        1.67
         166    23798.00           4         409         607           2           3      608.00      409.50        0.81
         167    23778.00           4         410         631           2           4      632.50      410.50        1.14
         168    23805.00           4         410         670           2           3      671.25      410.50        0.85
         169    23844.00           4         411         611           2           4      612.50      411.50        1.21
         170    23823.00           4         413         632           2           3      633.25      413.25        0.87
         171    29765.00           5         411         636           4           2      636.40      412.60        1.02
         172    35835.00           6         413         822           2           3      823.00      413.50        0.82
         173    35687.00           6         413         550           3           4      551.50      413.83        1.20
         174    29772.00           5         414         613           2           5      615.00      414.80        1.42
         175    53513.00           9         412         502           6           3      503.44      414.56        1.83
         176    53551.00           9         415         671           3           5      673.22      416.33        1.51
         177   112333.00          17         418          64           4           5       65.92      419.76        1.32
         178    41636.00           7         419         662           3           5      663.86      420.00        1.46
         179    23841.00           4         422         600           3           4      601.50      423.00        1.31
         180   110535.00          18         421         753           4           7      756.41      422.72        1.66
 
Rows: 181:210
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         181    23802.00           4         424         654           2           4      655.50      424.50        1.21
         182    29735.00           5         425         615           3           3      616.20      426.20        0.96
         183    35991.00           6         425         762           3           3      762.83      425.66        0.76
         184    36069.00           6         428         313           2           3      314.00      428.50        0.81
         185   103863.00          17         427         507           4           6      508.99      428.70        1.44
         186    78600.00          13         432         482           4           5      483.30      433.77        1.32
         187    35728.00           6         432         663           5           3      663.83      434.17        1.40
         188    29768.00           5         436         552           2           3      552.80      436.40        0.77
         189    96016.00          16         438         536           4           6      537.93      439.63        1.59
         190    29760.00           5         442         665           2           4      666.40      442.20        1.02
         191    41616.00           7         448         658           4           5      660.00      449.57        1.73
         192    29814.00           5         450         262           3           3      263.00      450.80        0.83
         193    23790.00           4         456         439           2           3      440.00      456.50        0.81
         194    23830.00           4         457         549           2           3      550.25      457.75        0.87
         195    71792.00          12         457         450           4           6      452.41      458.59        1.32
         196    35719.00           6         465         652           3           4      653.50      466.33        0.99
         197    29932.00           5         476         196           3           3      197.00      477.00        0.63
         198    23788.00           4         475         651           4           3      651.75      476.50        1.37
         199    73492.00          12         476         860           4           5      861.99      477.75        1.10
         200   228820.00          33         479         909           6           7      911.62      481.42        1.66
         201    35862.00           6         485         441           2           3      442.00      485.50        0.82
         202   240949.00          40         480           4           9          10        8.92      483.77        2.23
         203    23816.00           4         493          55           4           3       56.00      494.50        1.22
         204    23807.00           4         495          78           2           3       79.00      495.50        0.81
         205    23794.00           4         494         622           4           3      623.25      495.50        1.37
         206    23788.00           4         495         758           3           3      758.75      496.00        0.83
         207    23793.00           4         497          59           2           4       60.50      497.25        1.12
         208    59614.00          10         496          64           3           5       65.90      496.90        1.26
         209    35747.00           6         496         467           3           3      468.00      497.17        0.96
         210    42221.00           7         499         491           2           4      492.29      499.57        1.04
 
Rows: 211:240
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         211    29770.00           5         499         629           2           4      630.40      499.60        1.09
         212    47577.00           8         500          65           3           6       67.50      501.00        1.63
         213    87558.00          14         502         987           4           5      988.99      503.71        1.22
         214    54144.00           9         503         735           4           4      736.11      505.00        1.06
         215    42097.00           7         505         246           3           4      247.43      505.86        0.91
         216    98156.00          16         505         461           5           6      463.76      507.13        1.38
         217    23855.00           4         511         614           2           4      615.50      511.25        1.17
         218    23834.00           4         511         635           2           3      635.75      511.50        0.85
         219    23833.00           4         511         647           2           4      648.50      511.50        1.21
         220   114971.00          19         509         753           4           7      756.16      510.47        1.62
         221    35708.00           6         511         605           4           5      606.83      512.33        1.58
         222    41707.00           7         512         907           3           4      908.00      512.71        1.12
         223    41666.00           7         513         623           3           4      624.72      514.29        1.08
         224    23786.00           4         513         683           3           4      684.50      514.00        1.31
         225    41627.00           7         514         556           4           5      558.29      515.57        1.29
         226    29744.00           5         517         763           3           3      764.20      518.00        0.89
         227    36509.00           6         518         927           2           3      928.00      518.50        0.81
         228    29763.00           5         517         603           4           4      604.40      518.40        1.34
         229    23824.00           4         519         624           2           3      625.00      519.75        0.71
         230    23787.00           4         518         655           3           4      656.50      518.75        1.37
         231    59569.00          10         518         615           4           7      618.70      520.10        2.07
         232    35709.00           6         519         687           3           5      688.83      520.17        1.57
         233    29748.00           5         520         605           3           4      606.60      521.40        1.15
         234    23809.00           4         520         709           3           2      709.50      521.25        0.85
         235    23805.00           4         522         609           3           2      609.75      523.25        0.87
         236    23842.00           4         525         175           2           2      175.50      525.50        0.50
         237    29834.00           5         526         610           2           5      612.00      526.20        1.42
         238   103338.00          17         522         773           6           5      774.36      524.89        1.49
         239   103180.00          17         524         603           5           6      605.60      525.59        1.54
         240    59498.00          10         526         683           3           6      685.50      526.80        1.87
 
Rows: 241:270
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         241    84197.00          13         528          85           3           5       86.66      529.01        1.24
         242    23818.00           4         530         707           2           3      708.00      530.25        0.71
         243    23820.00           4         530         536           3           3      537.25      531.25        1.12
         244    29815.00           5         531         623           2           4      624.40      531.40        1.09
         245    35705.00           6         532         524           3           5      526.17      533.17        1.40
         246    47580.00           8         532         588           4           5      590.00      533.75        1.50
         247    59461.00          10         531         629           5           8      632.20      532.70        2.55
         248    23801.00           4         534         710           2           3      711.00      534.50        0.81
         249    35712.00           6         535         544           2           5      546.33      535.83        1.50
         250    65554.00          11         534         598           4           6      600.18      535.18        1.41
         251    41631.00           7         534         696           4           5      698.14      535.57        1.27
         252   105182.00          17         534         487           5           5      489.06      536.23        1.34
         253    23810.00           4         537         527           2           3      528.25      537.50        0.85
         254    29770.00           5         536         621           3           3      622.20      536.80        0.77
         255    47612.00           8         534         692           5           5      694.00      536.38        1.62
         256    23781.00           4         538         373           2           4      374.50      538.50        1.21
         257    23796.00           4         538         532           2           3      533.00      538.75        0.71
         258    65653.00          11         535         626           5           4      627.09      536.64        1.18
         259    29774.00           5         536         703           4           2      703.60      537.40        1.02
         260    41672.00           7         539         579           2           4      580.28      539.57        1.04
         261    53626.00           9         538         613           3           7      616.00      538.89        2.29
         262    41691.00           7         542         534           2           6      536.14      542.71        1.83
         263    29731.00           5         541         592           3           5      594.00      541.60        1.59
         264    53561.00           9         540         635           4           5      636.78      541.44        1.53
         265    29727.00           5         542         598           4           2      598.20      543.20        1.19
         266    29917.00           5         546         828           3           3      829.00      547.00        0.63
         267    53646.00           9         547         594           3           5      596.11      547.78        1.24
         268    29752.00           5         547         616           3           3      617.20      547.80        0.78
         269    29742.00           5         549         610           4           4      611.80      550.40        1.51
         270    47582.00           8         553         620           5           5      622.25      554.75        1.23
 
Rows: 271:300
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         271    35687.00           6         556         615           5           2      615.50      558.17        1.34
         272    47698.00           8         556         624           5           4      625.38      558.25        1.73
         273    29726.00           5         558         595           4           2      595.80      559.60        1.02
         274    29748.00           5         558         617           4           3      618.40      559.80        1.28
         275    23802.00           4         562         635           2           3      636.00      562.25        0.71
         276    23794.00           4         564         573           2           4      574.50      564.75        1.17
         277    23766.00           4         562         608           4           2      608.50      563.50        1.14
         278    23801.00           4         564         594           3           3      594.75      565.00        1.05
         279   160739.00          27         562         615           8          13      620.74      565.81        3.46
         280   121566.00          20         566         920           4           6      922.75      567.55        1.50
         281    66123.00          11         572         703           3           5      704.54      573.00        1.30
         282    29922.00           5         577         642           2           3      642.80      577.40        0.77
         283    29720.00           5         577         632           3           4      633.80      577.80        1.35
         284    45011.00           7         587         322           2           5      324.45      587.70        1.41
         285    29785.00           5         587         523           3           4      524.60      588.00        1.11
         286    65974.00          11         588         203           4           4      204.64      589.64        1.15
         287   188606.00          31         593         952           5           9      956.27      594.80        2.10
         288   340483.00          57         588         631          11           9      634.44      593.35        2.36
         289   472147.00          78         588         934          11          11      939.31      593.17        2.58
         290    61516.00          10         611          47           3           4       48.50      611.80        1.02
         291    71843.00          12         609         575           5           5      577.00      611.50        1.37
         292    53920.00           9         613         254           3           4      255.33      614.00        1.06
         293    23835.00           4         613         873           3           2      873.25      614.00        0.71
         294    30539.00           5         616        1012           2           3     1012.80      616.40        0.80
         295    35695.00           6         615         748           4           2      748.50      616.17        1.07
         296   152887.00          24         614         803           5           7      806.26      616.41        1.61
         297    89972.00          15         619         281           4           5      282.67      620.66        1.20
         298    47920.00           8         623         615           3           3      615.88      624.13        0.86
         299   313500.00          52         619         604           8          11      608.58      623.18        2.66
         300    89733.00          15         623         902           4           7      904.80      624.53        1.67
 
Rows: 301:330
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         301    65568.00          11         628         912           5           3      912.82      629.73        1.26
         302    23813.00           4         635         659           2           4      660.50      635.75        1.17
         303    54978.00           9         635         879           3           4      880.44      635.67        1.08
         304    29960.00           5         641         905           2           3      906.20      641.40        0.77
         305    23785.00           4         642        1008           2           2     1008.50      642.50        0.50
         306    47579.00           8         641         781           4           4      782.00      642.50        1.06
         307    29818.00           5         645         778           3           3      779.20      646.00        1.05
         308   120489.00          20         647          10           4           8       13.85      648.35        2.08
         309    23845.00           4         651         461           2           3      462.25      651.50        0.85
         310    53999.00           9         651         615           4           4      616.67      652.44        0.95
         311    59692.00          10         656         608           3           5      610.40      657.00        1.20
         312    41937.00           7         659         765           3           4      766.14      659.86        1.01
         313    23860.00           4         664         617           2           2      617.50      664.50        0.50
         314    31144.00           5         665         865           2           3      866.19      665.40        0.77
         315    29799.00           5         669         780           3           3      781.20      670.00        0.83
         316    23791.00           4         671        1015           2           4     1016.50      671.75        1.17
         317   160353.00          25         672         805           7           7      807.70      674.55        1.56
         318   249200.00          37         678         847           6           9      850.64      680.34        1.94
         319   537918.00          86         682         930          12          11      935.16      687.28        2.95
         320    29748.00           5         699         797           2           4      798.20      699.20        1.17
         321    30094.00           5         700         435           2           3      435.80      700.40        0.77
         322  1066406.00         135         694         778          16          17      785.39      702.76        3.16
         323    42293.00           7         708         914           3           4      915.43      709.14        0.91
         324  1345534.00         218         693         950          19          19      958.91      702.69        4.47
         325    48036.00           8         721         115           4           4      116.25      722.88        1.14
         326   118574.00          19         728         757           5           6      759.32      729.84        1.36
         327    59799.00          10         731         342           4           5      343.80      732.40        1.28
         328   235822.00          39         734         469           7           9      472.82      737.29        2.11
         329    60842.00          10         755         798           3           4      799.79      756.00        0.96
         330    99347.00          16         755         289           4           5      290.57      756.38        1.25
 
Rows: 331:360
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         331    23852.00           4         758         595           3           2      595.50      759.25        0.85
         332    80480.00          13         758          79           4           5       80.85      759.23        1.17
         333    29756.00           5         772         870           2           3      871.20      772.40        0.77
         334    59873.00          10         771         527           4           4      528.70      772.30        1.00
         335    99356.00          16         774         253           4           5      254.83      775.31        1.27
         336    35807.00           6         780         162           2           4      163.50      780.33        0.96
         337    42052.00           7         791         434           2           4      435.71      791.43        1.04
         338    23864.00           4         796         658           2           3      659.00      796.50        0.81
         339    53824.00           9         803          51           3           5       53.33      803.89        1.33
         340    47674.00           8         821         472           3           4      473.38      822.00        1.01
         341    59952.00          10         832         561           3           4      562.20      832.90        1.02
         342    23916.00           4         837         681           2           2      681.50      837.50        0.50
         343   351268.00          49         833         573           9           8      576.42      837.13        1.94
         344   217305.00          36         836         903           8           8      906.12      839.75        2.06
         345    84314.00          14         847        1008           4           7     1010.92      848.50        1.76
         346   125855.00          21         852         307           4           7      309.62      853.48        1.79
         347  2571484.00         332         836         139          25          24      150.27      847.23        4.76
         348   206465.00          29         855         732           6           7      734.46      857.04        1.58
         349    24479.00           4         866         801           2           2      801.50      866.49        0.50
         350    36117.00           6         873         465           3           3      465.66      873.67        0.91
         351    24201.00           4         886          98           2           3       99.00      886.25        0.71
         352   100451.00          16         898         698           4           6      700.50      899.25        1.30
         353   100507.00          16         907         428           5           5      430.25      909.38        1.30
         354    54819.00           9         915          38           3           4       39.33      916.11        0.97
         355    97388.00          16         920         141           4           5      143.25      921.63        1.20
         356    54091.00           9         922         530           3           4      531.34      923.11        0.97
         357   726330.00          85         915         754          11          12      759.31      920.11        2.36
         358    48468.00           8         924         421           3           4      422.38      925.25        1.03
         359    54035.00           9         921         137           9           2      137.11      925.00        2.58
         360   130675.00          21         926         190           5           6      192.08      927.96        1.40
 
Rows: 361:390
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         361   132674.00          22         926         969           5           6      971.23      927.86        1.59
         362    60038.00          10         932         117           4           4      118.70      933.40        1.12
         363   180766.00          26         931         905           5           7      907.96      933.05        1.58
         364    81577.00          13         936         547           3           5      549.00      936.99        1.31
         365    24721.00           4         954         498           2           3      498.99      954.50        0.80
         366    23934.00           4         955         505           2           3      506.00      955.50        0.81
         367    53574.00           9         974         185           4           5      186.56      975.89        1.29
         368    47600.00           8         977         205           3           4      206.37      978.00        0.99
         369    23797.00           4         978         220           2           4      221.50      978.75        1.17
         370    23811.00           4         988         232           3           3      232.75      989.25        1.12
         371    23855.00           4         989         698           2           2      698.50      989.50        0.50
         372    36694.00           6         990        1000           2           4     1001.51      990.32        0.93
         373    36035.00           6         996          99           2           3      100.00      996.50        0.82
         374   459385.00          57         991         552           8          10      556.48      994.32        2.07
         375    47617.00           8         998         122           3           6      124.50      999.25        1.50
         376    35753.00           6         998         250           3           5      251.83      999.00        1.35
         377   114191.00          19         996         976           5           5      978.00      997.63        1.37
         378    35696.00           6         999         257           3           4      258.83      999.83        1.15
         379    36301.00           6        1000         803           3           3      803.84     1001.16        0.78
         380    41673.00           7        1002         269           4           5      271.00     1003.86        1.49
         381    41657.00           7        1003         275           3           6      277.43     1003.57        1.63
         382    23835.00           4        1005         259           3           2      259.75     1006.00        0.71
         383    29782.00           5        1005         266           4           2      266.40     1006.80        1.17
         384    47659.00           8        1005         358           4           5      360.00     1006.37        1.51
         385    23814.00           4        1008         296           2           2      296.50     1008.50        0.50
         386    29762.00           5        1010         305           2           4      306.40     1010.40        1.05
         387   131019.00          22        1004         618           8           7      621.59     1007.64        1.90
         388    47627.00           8        1009         292           4           5      294.00     1010.75        1.50
         389    41652.00           7        1009         358           4           6      360.57     1010.29        1.87
         390    29849.00           5        1011         372           2           4      373.40     1011.60        1.05
 
Rows: 391:412
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         391    23784.00           4        1010         385           3           3      386.00     1010.75        1.05
         392    23779.00           4        1010         611           3           3      612.00     1011.00        0.87
         393    23809.00           4        1011         696           2           3      697.00     1011.75        0.71
         394   156424.00          22        1009         982           4           6      984.50     1010.66        1.53
         395    53550.00           9        1010         327           4           5      328.78     1011.45        1.28
         396    29748.00           5        1011         402           3           4      403.80     1012.20        1.22
         397    35713.00           6        1010         615           4           4      616.83     1011.16        1.35
         398    41670.00           7        1012         333           3           7      336.00     1012.57        2.08
         399    29727.00           5        1011         369           4           2      369.20     1012.40        1.02
         400   113132.00          19        1008         317           8           8      321.05     1011.84        2.03
         401    29876.00           5        1013         350           3           3      351.20     1013.80        0.96
         402    29787.00           5        1013         429           3           4      430.60     1014.20        1.23
         403    77404.00          13        1013         379           4          10      383.54     1014.69        2.92
         404    71442.00          12        1014         691           3           7      694.42     1014.75        2.11
         405    83257.00          14        1011         725           6           7      728.07     1013.86        1.80
         406    23827.00           4        1017         667           3           3      668.25     1017.75        1.12
         407    23832.00           4        1018         691           2           3      691.75     1018.75        0.87
         408 39502976.00        6368         974          90          51         371      219.15     1009.19       79.03
         409    47743.00           8        1023         465           2           7      468.00     1023.88        1.87
         410    23841.00           4        1023         504           2           3      505.25     1023.75        0.87
         411    89432.00          15        1021         525           4          11      530.93     1023.53        3.19
         412    24004.00           4        1023         540           2           3      541.25     1023.75        0.87
 
Rows: 1:30
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.44        0.31       10.06           0
        0.37        0.23       11.53           0
        0.29        0.14       -4.30           0
        0.80        0.50       -9.04           0
        0.56        0.29       10.80           0
        0.51        0.00        0.01           0
        0.95        0.49      -24.11           0
        0.60        0.33       -0.29           0
        0.72        0.61      -53.35           0
        0.23        0.16       17.14           0
        0.52        0.48      -67.49           0
        0.43        0.61      -90.02           0
        0.74        0.78        9.07           0
        0.42        0.37       -6.62           0
        0.47        0.55       14.87           0
        0.35        0.41      -18.39           0
        0.50        1.00       45.00           0
        0.53        0.39      -45.00           0
        0.50        1.00       44.99           0
        0.83        0.80      -83.60           0
        0.41        0.23      -31.21           0
        0.72        0.93      -44.84           0
        0.50        0.41       26.58           0
        0.41        0.41      -18.45           0
        0.67        0.34       16.78           0
        0.43        0.61       -0.02           0
        0.57        0.27     -129.12           0
        0.82        0.78       14.50           0
        0.56        0.26       20.01           0
        0.35        0.41       18.41           0
 
Rows: 31:60
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.61        0.93     -122.86           0
        0.31        0.38      -31.72           0
        0.42        0.35      -13.30           0
        0.73        0.71       13.34           0
        0.50        0.36       19.69           0
        0.53        0.40       40.05           0
        0.32        0.29      -20.98           0
        0.23        0.16      -17.14           0
        0.57        0.62      -55.23           0
        0.33        0.23      -14.65           0
        0.80        0.56      -22.34           0
        0.31        0.38      -31.73           0
        0.47        0.55       14.87           0
        0.45        0.15       -9.97           0
        1.37        0.59       11.98           0
        0.49        0.98      -39.97           0
        0.74        0.59     -126.51           0
        1.11        0.54      -59.97           0
        4.77        0.03       -0.13           0
        0.72        0.93     -134.94           0
        0.26        0.22      -17.62           0
        0.47        0.55     -104.93           0
        0.79        0.49        3.41           0
        2.09        0.73      -42.09           0
        1.13        0.57        2.46           0
        0.47        0.55     -104.90           0
        0.47        0.33       -6.40           0
        0.47        0.55     -104.87           0
        0.32        0.24      -31.10           0
        1.32        0.76      -29.12           0
 
Rows: 61:90
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.28        0.19     -101.74           0
        1.94        0.82       -4.48           6
        0.26        0.22      -17.61           0
        0.35        0.41       18.43           0
        0.39        0.38       -5.15           0
        0.50        1.00       44.98           0
        0.99        0.69       10.46           0
        0.63        0.62       -0.02           0
        0.39        0.27       -9.61           0
        0.99        0.99     -134.16           0
        0.69        0.72       19.89           0
        0.50        0.58      -45.02           0
        7.60        0.84      -88.07          12
        1.39        0.71       -7.17           0
        0.67        0.53       11.18           0
        0.45        0.58       18.18           0
        0.43        0.62        0.17           0
        0.39        0.37        4.67           0
        3.34        0.93       29.97           8
        0.45        0.58      -18.50           0
        0.89        0.63       10.15           0
        0.43        0.61        0.15           0
        0.79        0.79      -44.87           0
        0.78        0.57        6.91           0
        0.55        0.44      -22.50           0
        0.83        0.58      -17.53           0
        1.47        0.72      -96.80           0
        2.44        0.87        4.17           6
        0.70        0.81       -0.17           0
        0.53        0.58      -44.99           0
 
Rows: 91:120
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.31        0.75       40.75           0
        3.42        0.66      -12.96           3
        1.67        0.85        7.27           0
        0.98        0.85        4.53           0
        2.99        0.90       -2.03           4
        0.42        0.40      -74.52           0
        0.95        0.60       10.75           0
        0.58        0.74     -134.99           0
        1.59        0.74       -1.92           0
        0.73        0.68       30.68           0
        0.95        0.53        7.42           0
        1.25        0.88       13.66           0
        0.50        0.45       -0.05           0
        0.32        0.29       20.99           0
        0.84        0.62       -9.37           0
        0.41        0.41      -18.43           0
        0.50        1.00      -44.99           0
        0.71        0.57      -59.42           0
        0.58        0.47     -126.14           0
        0.64        0.41      -17.48           0
        0.35        0.35     -134.98           0
        0.69        0.35      -58.66           0
        0.39        0.38        5.16           0
        0.69        0.74      -34.66           0
        0.62        0.62       33.35           0
        0.42        0.40      -15.49           0
        1.54        0.80       -3.31           0
        0.65        0.67       -6.99           0
        1.10        0.65        7.03           0
        0.31        0.38       31.72           0
 
Rows: 121:150
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.63        0.75      -45.07           0
        0.35        0.41     -108.36           0
        0.35        0.41      -18.45           0
        0.97        0.81        0.02           0
        0.83        0.70       14.96           0
        1.12        0.62        5.43           0
        0.26        0.22      -17.60           0
        1.26        0.78      -48.88           0
        0.59        0.56      -73.99           0
        0.92        0.76       -9.05           0
        0.42        0.37       -6.61           0
        0.45        0.47     -135.00           0
        0.46        0.41        6.27           0
        0.47        0.50        0.14           0
        1.42        0.63     -100.41           0
        2.18        0.93       -8.02           1
        2.29        0.94       16.52           0
        0.63        0.69      -10.94           0
        0.31        0.38       31.73           0
        0.45        0.58       18.43           0
        3.03        0.87      -14.86           0
        0.23        0.20       19.33           0
        0.90        0.78      -23.25           0
        0.69        0.78     -123.41           0
        0.89        0.63      -10.84           0
        1.96        0.48       36.06           0
        0.69        0.63      -18.65           0
        0.43        0.62      -89.94           0
        0.56        0.49      -66.33           0
        0.31        0.38       31.72           0
 
Rows: 151:180
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.45        0.58     -108.53           0
        1.57        0.57       30.05           0
        0.26        0.22       17.60           0
        0.84        0.49        7.57           0
        0.55        0.51      -33.41           0
        0.84        0.76       38.41           0
        0.43        0.61        0.09           0
        0.88        0.72      -44.95           0
        0.62        0.55     -110.62           0
        0.42        0.37       -6.60           0
        0.44        0.32        7.84           0
        0.66        0.61       24.72           0
        0.50        0.45       -0.00           0
        1.41        0.73      -77.15           0
        1.00        0.60      -50.33           0
        0.31        0.38      -31.71           0
        0.44        0.38       13.27           0
        0.47        0.55      -14.88           0
        0.21        0.17      -22.49           0
        0.35        0.41      -18.42           0
        0.49        0.48      -92.83           0
        0.50        0.61       -0.03           0
        0.52        0.44      -42.27           0
        0.37        0.26        6.13           0
        0.68        0.37      -92.69           0
        0.75        0.50       13.78           0
        0.97        0.73       -6.75           0
        0.76        0.52       -0.02           0
        0.19        0.15       31.71           0
        0.91        0.55        6.56           0
 
Rows: 181:210
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.21        0.17      -22.50           0
        0.45        0.47      -45.01           0
        0.67        0.87     -116.35           0
        0.50        0.61        0.01           0
        0.95        0.66       -0.00           0
        0.88        0.67      -34.56           0
        0.57        0.41     -107.67           0
        0.45        0.58       18.46           0
        0.94        0.59      -27.17           0
        0.39        0.38        5.15           0
        0.34        0.19      -41.74           0
        0.51        0.62     -124.19           0
        0.31        0.38      -31.72           0
        0.35        0.41       18.46           0
        0.86        0.65       -0.41           0
        0.70        0.71      -21.36           0
        0.63        1.00        0.12           0
        0.22        0.16     -126.10           0
        0.88        0.79       23.16           0
        1.41        0.85        7.82           0
        0.50        0.61        0.05           0
        1.67        0.75      -25.96           0
        0.50        0.41     -116.59           0
        0.31        0.38       31.73           0
        0.22        0.16     -126.09           0
        0.71        0.85        0.06           0
        0.42        0.37        6.63           0
        0.63        0.50       29.78           0
        0.47        0.49       36.89           0
        0.48        0.46       -8.40           0
 
Rows: 211:240
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.32        0.29      -20.99           0
        0.59        0.36       24.70           0
        0.87        0.71        0.82           0
        0.84        0.79      -39.98           0
        0.62        0.69       10.99           0
        0.97        0.70       -8.87           0
        0.26        0.22       17.58           0
        0.47        0.55       14.88           0
        0.21        0.17      -22.51           0
        0.93        0.57       -2.69           0
        0.44        0.28      -33.25           0
        0.62        0.55       20.60           0
        0.82        0.76       27.43           0
        0.19        0.15       31.71           0
        0.89        0.69       10.91           0
        0.75        0.84      -90.03           0
        0.50        0.61       -0.11           0
        0.53        0.39       44.97           0
        0.43        0.61       -0.07           0
        0.22        0.16       36.09           0
        0.78        0.38       15.96           0
        0.40        0.26      -32.02           0
        0.60        0.52       32.80           0
        0.47        0.55     -104.87           0
        0.35        0.41     -108.42           0
        0.50        1.00      -44.98           0
        0.37        0.26       -6.13           0
        1.01        0.68      -55.41           0
        0.95        0.61      -35.77           0
        0.57        0.31       -5.60           0
 
Rows: 241:270
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.77        0.62        0.22           0
        0.43        0.61        0.01           0
        0.35        0.32       44.99           0
        0.32        0.29       21.01           0
        0.57        0.41      -17.66           0
        0.66        0.44       21.79           0
        0.48        0.19      -25.65           0
        0.31        0.38      -31.71           0
        0.32        0.21        7.47           0
        0.93        0.66        6.42           0
        0.87        0.69      -14.53           0
        1.04        0.78      -35.40           0
        0.47        0.55      -14.87           0
        0.72        0.93      -45.29           0
        0.78        0.48      -48.35           0
        0.21        0.17       22.50           0
        0.43        0.61       -0.00           0
        0.86        0.72      -70.01           0
        0.49        0.48      -92.78           0
        0.48        0.46       -8.39           0
        0.40        0.17       10.18           0
        0.37        0.20       -8.53           0
        0.33        0.21       27.89           0
        0.99        0.65       41.88           0
        0.34        0.28     -100.90           0
        0.63        1.00        0.00           0
        0.72        0.58       18.61           0
        0.72        0.93      -45.30           0
        0.33        0.22      -40.80           0
        1.17        0.96     -134.95           0
 
Rows: 271:300
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.50        0.37      -93.07           0
        0.65        0.37     -110.41           0
        0.39        0.38      -95.15           0
        0.61        0.47     -117.66           0
        0.43        0.61        0.01           0
        0.26        0.22      -17.62           0
        0.44        0.38      -76.72           0
        0.29        0.28       39.69           0
        1.39        0.40       24.86           0
        1.04        0.69      -12.48           0
        0.73        0.56        4.95           0
        0.45        0.58       18.40           0
        0.32        0.24      -31.12           0
        0.39        0.28        9.93           0
        0.46        0.41      -25.67           0
        0.77        0.67      -30.33           0
        1.20        0.57      -11.45           0
        2.15        0.91      -94.83           0
        2.49        0.96      -54.97           0
        0.74        0.73        0.01           0
        0.83        0.61     -129.04           0
        0.65        0.62       35.81           0
        0.43        0.61      -89.99           0
        0.46        0.58       13.41           0
        0.49        0.46      -95.30           0
        1.17        0.73       19.13           0
        0.99        0.83       14.53           0
        0.68        0.79      -44.95           0
        1.91        0.72       10.49           0
        0.81        0.48      -25.36           0
 
Rows: 301:330
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.76        0.60     -109.85           0
        0.26        0.22       17.61           0
        0.66        0.61        4.84           0
        0.45        0.58      -18.29           0
        0.50        1.00      -45.01           0
        0.94        0.88     -134.93           0
        0.51        0.49      -53.38           0
        0.90        0.43        4.04           0
        0.47        0.55      -14.90           0
        0.81        0.85       18.48           0
        0.77        0.65        0.01           0
        0.60        0.60      -14.92           0
        0.50        1.00     -134.97           0
        0.45        0.58      -18.98           0
        0.51        0.62       34.19           0
        0.26        0.22      -17.61           0
        1.33        0.85      -32.54           0
        1.41        0.73        5.47           0
        2.32        0.79      -66.70           0
        0.32        0.27       12.34           0
        0.45        0.58       18.34           0
        2.90        0.92       -0.05           4
        0.62        0.69      -10.68           0
        3.97        0.89      -35.44           0
        0.70        0.61      -42.17           0
        1.12        0.82       -5.09           0
        0.75        0.59      -15.67           0
        1.63        0.77      -15.57           0
        0.77        0.80        2.18           0
        1.00        0.80       23.33           0
 
Rows: 331:360
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.47        0.55     -104.86           0
        0.87        0.74       11.09           0
        0.45        0.58      -18.42           0
        0.79        0.79      -45.10           0
        0.97        0.76       -4.75           0
        0.47        0.49        0.02           0
        0.48        0.46       -8.36           0
        0.31        0.38       31.75           0
        0.57        0.43       22.84           0
        0.68        0.68       13.67           0
        0.77        0.76       26.54           0
        0.50        1.00      -45.04           0
        1.77        0.91       25.74           0
        1.54        0.75       25.22           0
        0.66        0.37      -11.25           0
        0.95        0.53       21.08           0
        4.61        0.97      -64.49          12
        1.31        0.83       -6.57           0
        0.50        1.00       43.76           0
        0.53        0.58     -134.95           0
        0.43        0.61        0.16           0
        0.95        0.73       -0.09           0
        1.01        0.78       41.16           0
        0.69        0.72       20.29           0
        1.04        0.86      -14.38           0
        0.70        0.72       20.23           0
        2.19        0.93       -6.28           3
        0.60        0.58       19.28           0
        0.26        0.10      -86.17           0
        1.20        0.86       -4.56           0
 
Rows: 361:390
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.16        0.73       10.71           0
        0.76        0.68      -54.95           0
        1.21        0.77       -7.69           0
        0.72        0.55       15.38           0
        0.31        0.39      -31.93           0
        0.31        0.38      -31.77           0
        0.82        0.64      -17.47           0
        0.87        0.87        0.11           0
        0.26        0.22      -17.62           0
        0.35        0.32      -45.00           0
        0.50        1.00      -44.99           0
        0.47        0.50        2.56           0
        0.50        0.61        0.09           0
        1.77        0.86       -1.23           0
        0.82        0.55        4.53           0
        0.56        0.42       -6.35           0
        1.10        0.80       29.60           0
        0.79        0.69      -30.69           0
        0.58        0.74       44.40           0
        0.70        0.47      -42.25           0
        0.63        0.39       13.98           0
        0.43        0.61      -89.96           0
        0.48        0.41      -85.94           0
        0.46        0.30      -37.75           0
        0.50        1.00        4.10           0
        0.42        0.40      -15.48           0
        1.55        0.81      -82.26           0
        0.66        0.44      -21.78           0
        0.28        0.15      -32.33           0
        0.42        0.40       15.49           0
 
Rows: 391:412
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.29        0.28      -50.31           0
        0.50        0.58     -135.00           0
        0.43        0.61        0.02           0
        1.02        0.67        0.59           0
        0.88        0.68      -40.09           0
        0.65        0.53       20.99           0
        0.67        0.49      -45.04           0
        0.43        0.21      -16.69           0
        0.39        0.38      -95.14           0
        1.82        0.90     -109.88           0
        0.45        0.47     -134.99           0
        0.28        0.23      -35.29           0
        0.64        0.22      -15.49           0
        0.69        0.33       -6.33           0
        1.36        0.75        1.99           0
        0.35        0.32      -45.01           0
        0.35        0.41      -18.44           0
       11.52        0.15       -3.88           5
        0.33        0.18        0.00           0
        0.35        0.41       18.42           0
        0.83        0.26        5.80           0
        0.35        0.41       18.35           0
let $echo="no"
TEST 2 - number of stars = 417
starcat3 ft/s233m.vic out=(s233m2.cat,s233m2.byte) thre=5940 back=0 objects=objs  +
      maxd=32767
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
*** Total of        417 stars found ***
ibis-li s233m2.cat sr=1 nr=20 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
Beginning VICAR task ibis
 
Number of Rows:417  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
 
Rows: 1:20
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1    41779.00           7           1         198           2           5      199.71        1.43        1.41
           2    47683.00           8           1         211           2           6      213.25        1.37        1.59
           3    47701.00           8           1         919           2           7      922.37        1.12        2.12
           4    71533.00          12           1         190           3           6      192.83        1.75        1.59
           5    65620.00          11           1         204           3           7      206.46        1.55        1.91
           6  7989653.00        1320           1         215           3         676      546.66        1.49      191.20
           7    59644.00          10           1         942           4           7      945.30        2.20        1.92
           8    47613.00           8           6         880           3           6      881.88        7.13        1.83
           9    41658.00           7           7         868           4           4      869.57        8.29        1.17
          10    29754.00           5          11         847           2           5      849.00       11.40        1.48
          11    41684.00           7          10         260           4           3      260.86       11.29        1.09
          12    23856.00           4          11         546           3           2      546.25       12.00        0.71
          13    35753.00           6          11         549           3           4      550.33       11.67        0.95
          14    23773.00           4          13         315           2           4      316.50       13.75        1.12
          15    23804.00           4          13         489           2           3      490.25       13.50        0.85
          16    23859.00           4          14         323           2           3      324.25       14.25        0.87
          17    23804.00           4          14         568           2           2      568.50       14.50        0.50
          18    41612.00           7          12         828           4           4      829.29       13.29        1.36
          19    23829.00           4          15         571           2           2      571.50       15.50        0.50
          20    41705.00           7          13         803           4           3      803.86       14.29        1.03
 
Rows: 1:20
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        0.44        0.31       10.06           0
        0.37        0.23       11.53           0
        0.29        0.14       -4.30           0
        0.80        0.50       -9.04           0
        0.56        0.29       10.80           0
        0.51        0.00        0.01           0
        0.95        0.49      -24.11           0
        0.60        0.33       -0.29           0
        0.72        0.61      -53.35           0
        0.23        0.16       17.14           0
        0.52        0.48      -67.49           0
        0.43        0.61      -90.02           0
        0.74        0.78        9.07           0
        0.42        0.37       -6.62           0
        0.47        0.55       14.87           0
        0.35        0.41      -18.39           0
        0.50        1.00       45.00           0
        0.53        0.39      -45.00           0
        0.50        1.00       44.99           0
        0.83        0.80      -83.60           0
let $echo="no"
TEST 3 - number of stars = 417
starcat3 ft/s233m.vic out=(s233m3.cat,s233m3.byte,s233m3.flag)  +
thre=5940 back=0 objects=objs  +
      maxd=32767 mins=6 minl=6 maxl=50  maxs=50
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
*** Total of         45 stars found ***
ibis-li s233m3.cat sr=1 nr=20 nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
Beginning VICAR task ibis
 
Number of Rows:45  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
 
Rows: 1:20
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1   107156.00          18          13         507           7           7      510.06       16.33        1.73
           2   113075.00          19          20         674           6          10      678.79       22.53        2.34
           3    89322.00          15          21         618           6           6      620.60       23.87        2.05
           4   184589.00          31          20         663           9          10      668.52       22.55        2.85
           5   857125.00          84          27          38          10          12       43.11       31.44        2.37
           6   168996.00          28          67         582           6           8      585.35       69.14        1.94
           7  1552143.00         187          74         877          19          20      886.86       82.70        3.57
           8   231375.00          38         115        1008           8           6     1010.18      118.32        2.05
           9  1052163.00         116         121         265          13          15      271.22      126.79        2.81
          10   149670.00          25         157         657           7           6      659.76      160.00        1.76
          11  1542822.00         224         155         420          17          22      430.41      161.87        5.17
          12   331973.00          46         165         815           8           9      818.38      168.52        1.97
          13  1085094.00         142         170         525          15          18      532.14      176.52        3.33
          14   379724.00          50         204         919           7          10      923.54      206.95        2.13
          15   133294.00          22         208         308           6           6      310.73      210.45        1.42
          16   372054.00          46         272         329           7          10      333.47      274.97        1.91
          17   135275.00          22         313         470           7           6      472.46      316.69        1.61
          18   229786.00          38         332         894           9           6      896.61      335.52        2.23
          19   624117.00          78         334         778          11          12      783.70      339.22        2.34
          20   572320.00          77         342         454          11          12      459.82      346.66        2.43
 
Rows: 1:20
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.61        0.93     -122.86           0
        1.37        0.59       11.98           0
        1.11        0.54      -59.97           0
        2.09        0.73      -42.09           0
        1.94        0.82       -4.48           6
        1.39        0.71       -7.17           0
        3.34        0.93       29.97           8
        1.47        0.72      -96.80           0
        2.44        0.87        4.17           6
        1.31        0.75       40.75           0
        3.42        0.66      -12.96           3
        1.67        0.85        7.27           0
        2.99        0.90       -2.03           4
        1.59        0.74       -1.92           0
        1.25        0.88       13.66           0
        1.54        0.80       -3.31           0
        1.26        0.78      -48.88           0
        1.42        0.63     -100.41           0
        2.18        0.93       -8.02           1
        2.29        0.94       16.52           0
let $echo="no"
TEST 4 - number of stars = 45
starcat3 ft/s233m.vic out=s233m4.cat size=(1,1,512,512) objects=objs  +
    thre=6000 back=0 mins=6 minl=6 maxl=50  maxs=50 maxd=32767
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
*** Total of          7 stars found ***
ibis-li s233m4.cat  nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
Beginning VICAR task ibis
 
Number of Rows:7  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
 
Rows: 1:7
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1   379801.00          63           1         468           7          21      477.55        2.87        5.13
           2   138645.00          22          14         395           6           6      397.17       17.30        1.40
           3   785571.00          72          27          38           9          11       43.16       31.42        2.23
           4   932933.00          96         122         265          11          14      271.17      126.83        2.52
           5  1047462.00         141         156         422          12          18      429.77      161.22        4.52
           6   330232.00          39         272         330           7           8      333.53      274.96        1.73
           7   393363.00          47         344         456           7           9      459.95      346.80        1.88
 
Rows: 1:7
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.66        0.32        4.16           0
        1.35        0.97      -94.98           0
        1.73        0.78       -3.34           6
        2.16        0.86       -1.89           6
        2.36        0.52      -14.74           3
        1.41        0.81       -3.83           0
        1.56        0.83       20.72           0
let $echo="no"
TEST 5 - number of stars = 7
starcat3 ft/s233m.vic out=s233m5.cat sl=1 ss=1 nl=256 ns=256 objects=objs  +
    thre=6000 maxd=32767
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
*** Total of          5 stars found ***
ibis-li s233m4.cat  nc=14 cols=(1,2,3,4,5,6,7,8,9,10,11,12,13,21) screen=132
Beginning VICAR task ibis
 
Number of Rows:7  Number of Columns: 21      
File Version:IBIS-2  Organization:ROW  SubType:STARCAT TYPE   2
 
Rows: 1:7
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9        C:10
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1   379801.00          63           1         468           7          21      477.55        2.87        5.13
           2   138645.00          22          14         395           6           6      397.17       17.30        1.40
           3   785571.00          72          27          38           9          11       43.16       31.42        2.23
           4   932933.00          96         122         265          11          14      271.17      126.83        2.52
           5  1047462.00         141         156         422          12          18      429.77      161.22        4.52
           6   330232.00          39         272         330           7           8      333.53      274.96        1.73
           7   393363.00          47         344         456           7           9      459.95      346.80        1.88
 
Rows: 1:7
+-----------+-----------+-----------+-----------
        C:11        C:12        C:13        C:21
+-----------+-----------+-----------+-----------
        1.66        0.32        4.16           0
        1.35        0.97      -94.98           0
        1.73        0.78       -3.34           6
        2.16        0.86       -1.89           6
        2.36        0.52      -14.74           3
        1.41        0.81       -3.83           0
        1.56        0.83       20.72           0
let $echo="no"
TEST 6 - number of stars = 5
starcat3 ft/s233m.vic out=s233m6.cat maxd=32767 list=list
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--ENTSAMP CENT
              MAJ AXIS   MIN AXIS   RATIO  ROTANGLE
       1*********  1050624        1        1     1024     1026   514.23   512.43                295.98   295.00     1.00
*** Total of          1 stars found ***
starcat3 nt/1994se_1026b.corr out=(1994se_1026b1.cat,1994se_1026b1.out)  +
    thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2  +
    maxs=100 maxl=100 objects=objs
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--ENTSAMP CENT
              MAJ AXIS   MIN AXIS   RATIO  ROTANGLE
       1   53590.      308       12      313       22       21   322.44    22.06                  4.20     3.50     0.83
       2     303.        5       34      318        3        4   319.43    34.80                  1.16     0.63     0.54
       3    4041.       42       38      384        8       10   387.40    41.35                  2.01     1.42     0.71
       4  119890.      176       37      307       15       17   314.84    44.88                  2.66     2.12     0.80
       5    2334.        4       51      263        2        2   263.61    51.94                  0.49     0.24     0.49
       6   32796.       97       41      649       12       12   654.19    46.77                  2.05     1.99     0.97
       7     848.       13       81      712        6        6   713.97    83.40                  1.39     1.27     0.91
       8    3419.        4       88       96        2        2    96.91    88.33                  0.47     0.29     0.61
       9    3224.       39       90      598        7        8   601.44    92.84                  1.73     1.54     0.89
      10    7067.       68      127      370       10       12   375.18   131.59                  2.37     1.87     0.79
      11     620.       10      145      741        4        4   742.26   146.17                  0.99     0.93     0.94
      12     304.        5      171      600        5        3   601.22   172.85                  1.50     0.47     0.31
      13    1040.       17      244      164        6        6   165.51   245.82                  1.94     1.18     0.61
      14     268.        5      248      710        3        3   711.01   248.91                  0.90     0.89     0.99
      15    5593.       66      260      418       10       13   423.77   264.77                  2.58     2.03     0.79
      16    4841.       70      298      236       11       14   242.45   303.70                  2.98     2.19     0.73
      17    7808.       83      317      518       12       10   522.02   322.20                  2.50     2.26     0.90
      18    4940.       65      322      337       10       11   341.19   326.51                  2.78     1.86     0.67
      19  274388.      293      332      406       22       21   415.61   342.01                  3.36     2.98     0.89
      20    5803.       82      353       58       12       14    64.17   358.77                  2.95     2.31     0.78
      21  147396.      248      371      101       21       19   110.00   380.22                  3.12     2.55     0.82
      22   62513.      204      381      383       17       17   390.98   389.27                  3.33     2.87     0.86
      23     435.        8      395      110        4        5   112.03   396.49                  1.69     0.39     0.23
      24     321.        5      398      132        3        4   133.05   399.14                  1.15     0.74     0.64
      25     374.        7      398       75        5        4    76.50   400.14                  1.21     0.91     0.75
      26     342.        6      402      131        3        5   133.17   402.70                  1.55     0.41     0.26
      27     282.        5      406      120        2        4   121.83   406.23                  1.15     0.34     0.29
      28     261.        4      410       67        2        3    68.09   410.18                  0.89     0.32     0.36
      29     240.        5      411       61        3        5    63.00   411.59                  1.58     0.34     0.22
      30     612.       10      435      108        6        3   109.28   437.00                  1.83     0.64     0.35
      31   91212.      194      426      652       17       16   658.99   434.14                  2.98     2.35     0.79
      32   44132.      179      434      254       16       17   262.85   441.16                  3.29     2.74     0.83
      33     235.        4      453      667        2        3   667.94   453.52                  0.76     0.32     0.42
      34     255.        5      453      658        3        4   659.23   453.59                  1.33     0.43     0.32
      35    1569.       27      451       98       11       12   103.83   456.76                  4.45     1.24     0.28
      36     261.        5      462      103        4        3   103.78   463.44                  1.11     0.63     0.57
      37     383.        7      464       98        4        4    99.60   465.27                  1.19     0.69     0.57
      38     470.        8      467      539        4        5   541.09   468.55                  1.54     0.97     0.63
      39 5734792.     2223      397       59       77       60    88.42   426.24                 10.20     3.86     0.38
      40     417.        8      471      538        5        4   539.84   473.09                  1.38     0.95     0.69
      41     254.        5      474       80        4        4    81.59   475.35                  1.28     0.69     0.54
      42     273.        5      475       89        4        5    90.96   476.15                  1.76     0.22     0.12
      43    2363.       36      481      201        8        8   203.81   484.38                  2.00     1.76     0.88
      44    2229.       34      519      620        9        7   623.17   523.15                  2.09     1.36     0.65
      45     614.       11      523      638        6        4   639.64   525.55                  1.88     0.73     0.39
      46     518.       10      544       88        4        5    90.03   545.57                  1.51     0.74     0.49
      47     304.        6      549       86        2        4    87.59   549.34                  1.23     0.47     0.38
      48     286.        5      556      173        3        5   174.89   556.99                  1.48     0.43     0.29
      49     335.        6      559      180        5        3   180.64   561.01                  1.36     0.49     0.36
      50    1407.        4      594      141        2        2   141.19   594.24                  0.50     0.30     0.59
      51    5763.       60      589       74       11       11    79.26   593.20                  2.05     2.03     0.99
      52    2321.        4      608      371        3        2   371.53   609.32                  0.70     0.29     0.41
      53    1191.       18      610      695        4        7   697.71   611.36                  1.47     1.09     0.74
      54   40137.      157      617      415       17       15   421.99   626.00                  3.19     2.37     0.74
      55   24025.      120      632      523       14       16   529.66   639.34                  2.91     2.13     0.73
      56    2218.       33      682      488        7        9   491.34   684.64                  2.30     1.34     0.58
      57    6986.       72      697      488       11       11   492.99   702.60                  2.64     1.83     0.69
      58  268862.      266      700      388       21       22   398.32   709.62                  3.05     2.41     0.79
      59   49331.      150      708      461       14       16   468.14   714.60                  2.88     2.16     0.75
      60     909.       15      721      572        5        8   575.14   723.66                  1.82     1.10     0.61
      61    1755.       27      731      405        9        9   408.97   734.83                  2.47     1.44     0.59
      62     555.       10      740      491        5        5   492.90   742.03                  1.43     1.04     0.72
      63    5733.       82      731      393       18       11   398.19   740.72                  4.14     2.38     0.57
      64     601.       11      785      646        4        5   648.24   786.48                  1.46     0.72     0.50
      65    9277.       32      794       13        4       10    17.52   796.26                  1.89     0.90     0.48
      66   27024.       73      790      676        8       18   684.60   796.24                  2.85     1.10     0.39
*** Total of         66 stars found ***
let $echo="no"
TEST 8 - number of stars = 66
starcat3 nt/1994se_1026b.corr out=(1994se_1026b2.cat,1994se_1026b2.out)  +
	thre=221 back=176 list=list1 maxd=32767 mini=221 mins=2 minl=2  +
	maxs=100 maxl=100 objects=objs
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
THRESH =    221 as given
Line# =     500        #Objects =      43
*** Total of         66 stars found ***
let $echo="no"
TEST 9 - number of stars = 66
starcat3 nt/1994se_1026b.corr out=(1994se_1026b3.cat,1994se_1026b3.out)  +
	thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2  +
	maxs=100 maxl=100 size=(1,1,150,150)
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--ENTSAMP CENT
              MAJ AXIS   MIN AXIS   RATIO  ROTANGLE
       1    3419.        4       88       96        2        2    96.91    88.33                  0.47     0.29     0.61
*** Total of          1 stars found ***
let $echo="no"
TEST 10 - number of stars = 66
starcat3 nt/1994se_1026b.corr out=(1994se_1026b4.cat,1994se_1026b4.out)  +
	thre=221 back=176 list=list maxd=32767 mini=221 mins=2 minl=2  +
	maxs=100 maxl=100 size=(1,1,150,700) objects=objs
Beginning VICAR task starcat3
*** STARCAT3 version 2016-03-07
  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--ENTSAMP CENT
              MAJ AXIS   MIN AXIS   RATIO  ROTANGLE
       1   53590.      308       12      313       22       21   322.44    22.06                  4.20     3.50     0.83
       2     303.        5       34      318        3        4   319.43    34.80                  1.16     0.63     0.54
       3    4041.       42       38      384        8       10   387.40    41.35                  2.01     1.42     0.71
       4  119890.      176       37      307       15       17   314.84    44.88                  2.66     2.12     0.80
       5    2334.        4       51      263        2        2   263.61    51.94                  0.49     0.24     0.49
       6   32796.       97       41      649       12       12   654.19    46.77                  2.05     1.99     0.97
       7    3419.        4       88       96        2        2    96.91    88.33                  0.47     0.29     0.61
       8    3224.       39       90      598        7        8   601.44    92.84                  1.73     1.54     0.89
       9    7067.       68      127      370       10       12   375.18   131.59                  2.37     1.87     0.79
*** Total of          9 stars found ***
let $echo="no"
TEST 11 - number of stars = 9
$ Return
$!#############################################################################
