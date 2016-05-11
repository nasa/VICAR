$!****************************************************************************
$!
$! Build proc for MIPL module fitsin
$! VPACK Version 2.1, Friday, January 08, 2016, 12:45:40
$!
$! Execute by entering:		$ @fitsin
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
$ write sys$output "*** module fitsin ***"
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
$ write sys$output "Invalid argument given to fitsin.com file -- ", primary
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
$   if F$SEARCH("fitsin.imake") .nes. ""
$   then
$      vimake fitsin
$      purge fitsin.bld
$   else
$      if F$SEARCH("fitsin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fitsin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fitsin.bld "STD"
$   else
$      @fitsin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fitsin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fitsin.com -mixed -
	-s fitsin.f -
	-i fitsin.imake -
	-p fitsin.pdf -
	-t tstfitsin.pdf tstfitsin.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fitsin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44
c
c	program to transfer FITS files to vicar files
c	Made compatible with Linux and MacOSX 64-bit compilers
C	01-30-2010 - R. J. Bambery
c
c	Flags used within this program:
c	iexcise,  TRUE=EXCISE parameter passed, up to 10 FITS labels are not to be passed 
c	xtens,	  TRUE=Begin processing EXTENSION  
c	iscl,	  TRUE=BSCALE value given 
c	izero,	  TRUE=BZERO value given     
c	tlist	  TRUE=List table to screen
c	table,	  TRUE="TABLE" extension - transfer FITS table to IBIS, (tbxfer or btxfer will be TRUE) 
c	xtend,	  TRUE=Valid EXTEND FITS label found (TABLE or BINTABLE) - not 3DTABLE or A3DTABLE
c       xfer,	  TRUE=FITS will be converted to VICAR or IBIS, (imgxfer,tbxfer or btxfer will be TRUE) 
c	prfits	  TRUE=Create output ASCII FITS headers
c	ihist,	  TRUE=Do not pass HISTORY FITS labels
c	icomment, TRUE=Do not pass COMMENT FITS labels
c	inull,	  TRUE=Do not pass NULL ("blank") FITS labels	
c	idat,	  TRUE=Vicar output data will be in same format as input FITS, check u16
c       u16,      TRUE=Unsigned 16-bit integer data in 32-bit "FULL" output image
c	ipass
c	jpass
c       imgxfer,  TRUE=Image to be output
c	tbxfer,	  TRUE=Table to be output
c	btxfer,	  TRUE=Binary table to be output
c	ineof,	  TRUE=Reached end-of-file
c
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	integer*4 stat,tabbits,tabax,tabcols,tabrows,tabdepth
	integer*4 pcount,gcount,tfields,extver,extlevel,catrec
	integer*4 ouni1(10),ouni2(10),outunit,excnt,ibis,nods,nhdrs
	integer*4 iuni,npix,nlin,nband,btpix,bfctr,i,n,hdrunit
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 tbcol(999),fitsrec,tabrec,fitstart
	logical*4 iexcise,xtens,iscl,izero,tlist,table,xtend
	logical*4 xfer,prfits,ihist,icomment,inull,idat,ipass,jpass
	logical*4 imgxfer,tbxfer,btxfer,ineof,u16
	real*4 bzero,bscale
	character*4 ibfmt(999)
	character*8 passtart,unlikely,excludst(10),ttype(999),passover
	character*10 tform(999)
	character*16 extname
	character*60 version
	character*80 outline
	character*132 hdrfilename(10)
c
	data version/'*** FITSIN 08/03/2014 (64-bit) rjb '/
	data excludst/10*'        '/
	data unlikely/'}#%@^!~('/	!an unlikely combination of characters
c					!in a FITS header
	call xvmessage(version,' ')
	xtend = .false.
	jpass = .false.
	ihist = .true.
	tabdepth = 0
	passover=unlikely	
	stat=0				!return status for xv routines
	do i=1,10
	   ouni1(i)=0
	   ouni2(i)=0
	enddo
c					!process FITS parameters
	call parmproc (iuni,bfctr,excnt,xfer,imgxfer,tbxfer,btxfer,prfits,
     1 	ihist,icomment,inull,idat,ipass,jpass,iexcise,tlist,nods,nhdrs,
     2 	u16,passtart,excludst,unlikely,hdrfilename,passover)
c
	fitsrec=0
	fitstart=1
	if (nods.gt.0) then
	   do n=1,nods
		outunit=ouni1(n)
		call xvunit(outunit,'OUT',n,STAT,' ')
		call chkstat (stat,'??E - XVunit err on output file',1,0,0)
		if (stat.eq.1) xfer=.true.	!got output #1!
c					!process FITS header
		if (nhdrs.gt.0) then
	   	   hdrunit=ouni2(n)
	   	   call xvunit(hdrunit,'HEADER',n,stat,'U_NAME', hdrfilename(n),' ')
		   call chkstat(stat,'??E - XVunit err HDR file ',1,0,0)
		   if (stat.eq.1) prfits=.true.	!output FITS header file
		endif
		call hdrproc (bfctr,hdrunit,catrec,tabrec,
     1		fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
     2		xfer,prfits,iscl,izero,
     3		xtens,imgxfer,tbxfer,btxfer,table,ineof,
     4		tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
     5		tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)
	    	if (ineof) then
		    write (outline,10100) n-1,nods
10100 format ('**FITSIN - Only ',i3,' data sets found out of ',i3,
     1' requested')
		    call xvmessage (outline,' ')
		    call xvclose (iuni,stat,' ')
		    return
	        endif
c!    FITS to VICAR images......
	   if (imgxfer) then
		call imgopen (bfctr,iuni,outunit,npix,nlin,nband,idat,
     1		izero,iscl,inull,xtens,btpix,bzero,bscale,fitsrec,
     2		fitstart,passtart,iexcise,excnt,excludst,u16,icomment)
 		if (btpix.eq.-64) call dfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE double
		if (btpix.eq.-32) call rfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE real
		if (btpix.eq.32) call ifitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!input fullword
 		if (btpix.eq.16) then
		    if (u16) then
			call ufitsi (iuni,outunit,npix,nlin,
     1			nband,bfctr,bzero,bscale,fitsrec)
		    else			!input/output halfword
			call hfitsi (iuni,outunit,npix,nlin,
     1			nband,bfctr,bzero,bscale,fitsrec,idat)
		    endif
		endif
		if (btpix.eq.8) call bfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!input byte

		call xvclose(outunit,stat,' ')
		fitstart=fitsrec+1
		call xvmessage (' ',' ')
		if (btpix.eq.16.and..not.idat.and..not.u16) then 
		call xvmessage ('*** 16-bit FITS to VICAR HALF (no conversion) ***',' ')
		endif
		if (btpix.eq.16.and.idat) then
		call xvmessage ('*** 16-bit FITS to VICAR REAL(0.0 to 65535.0) ***',' ')
		endif
		if (btpix.eq.16.and.u16) then
		call xvmessage ('*** 16-bit FITS to VICAR FULL(0 to 65535) ***',' ')
		endif

	   endif
c!	FITS tables to VICAR IBIS files....
	   if (tbxfer) then
	        call tablopen  (bfctr,iuni,outunit,tabcols,tabrows,tabdepth,
     1	 	tfields,ttype,
     2	 	catrec,fitsrec,ibis,ibfmt,passtart,
     3	 	excludst,fullrecord,realrecord,asciirecord,doubrecord,
     4	 	xtend,icomment)
	        call fitsibis  (iuni,tabcols,tabrows,tabdepth,tfields,
     1		bfctr,tabrec,ibfmt,tform,tbcol,fullrecord,realrecord,
     2		asciirecord,doubrecord,ibis)
	        call ibis_file_close(ibis,' ',stat)
		if (stat.ne.1) call ibis_signal_u(ouni1,stat,1)
		fitstart=fitsrec+1
	    endif
c
	    if (table.and.tlist) then
		call tablelist (ttype,tfields,tabrows,
     1	 	extname,extver,iuni,bfctr,tabcols,tabrec)
	    endif
c
	    if (btxfer) then
		write (outline,10120) 
10120 format (' ??E - FITS BINARY TABLES to IBIS2 files not yet supported')
 		call xvmessage (outline,' ') 
c		call bintablopen     		!future expansion of function
c		call bintablproc		!future expansion of function
	    endif
	  enddo
	else	!if (nods.gt.0) then
	    fitsrec=0
	    fitstart=1
	    outunit=0
	    if (nhdrs.gt.0) then
	        hdrunit=ouni2(1)
	   	call xvunit(hdrunit,'HEADER',1,stat,'U_NAME',hdrfilename(1),' ')
		call chkstat(stat,'??E - XVunit err HDR file ',1,0,0)
		if (stat.eq.1) prfits=.true.	!output FITS header ASCII file
	    endif
c					!process FITS header
	    call hdrproc (bfctr,hdrunit,catrec,tabrec,
     1	    fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
     2	    xfer,prfits,iscl,izero,
     3	    xtens,imgxfer,tbxfer,btxfer,table,ineof,
     4	    tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
     5	    tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)
c
	    if (imgxfer) then
		call imgopen (bfctr,iuni,outunit,npix,nlin,nband,idat,
     1		izero,iscl,inull,xtens,btpix,bzero,bscale,fitsrec,
     2		fitstart,passtart,iexcise,excnt,excludst,u16,icomment)
		fitsrec=fitsrec+1
		if (btpix.eq.-64) call dfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE double
		if (btpix.eq.-32) call rfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!input IEEE real format
		if (btpix.eq.32) call ifitsi (iuni,outunit,npix,nlin,
     1 		nband,bfctr,bzero,bscale,fitsrec,idat)	!input fullword
		if (btpix.eq.16) then
		    if (u16) then		!output unsigned
			call ufitsi (iuni,outunit,npix,nlin,
     1			nband,bfctr,bzero,bscale,fitsrec)
		    else			!input/output halfword
			call hfitsi (iuni,outunit,npix,nlin,
     1			nband,bfctr,bzero,bscale,fitsrec,idat)
		    endif
		endif
		if (btpix.eq.8) call bfitsi (iuni,outunit,npix,nlin,
     1		nband,bfctr,bzero,bscale,fitsrec,idat)	!input byte
	   endif
c
	   if (tbxfer) then
	        call tablopen  (bfctr,iuni,outunit,tabcols,tabrows,tabdepth,
     1	 	tfields,ttype,
     2	 	catrec,fitsrec,ibis,ibfmt,passtart,
     3	 	excludst,fullrecord,realrecord,asciirecord,doubrecord,
     4		xtend,icomment)
	     	call fitsibis  (iuni,tabcols,tabrows,tabdepth,tfields,
     1		bfctr,tabrec,ibfmt,tform,tbcol,fullrecord,realrecord,
     2		asciirecord,doubrecord,ibis)
	   endif
c
	   if (table.and.tlist) then
		call tablelist (ttype,tfields,tabrows,
     1	 	extname,extver,iuni,bfctr,tabcols,tabrec)
	   endif
c
	   if (btxfer) then
		write (outline,10120) 
		call xvmessage (outline,' ') 
c		call bintablopen	!future expansion of function
c		call bintablproc	!future expansion of function
	   endif
c	close all files and exit
c
	endif
	call xvclose(IUNI,STAT,' ')
	return
	end
C=========================================================================
       subroutine parmproc (iuni,bfctr,excnt,xfer,imgxfer,tbxfer,btxfer,prfits,
     1 ihist,icomment,inull,idat,ipass,jpass,iexcise,tlist,nods,nhdrs,
     2 u16,passtart,excludst,unlikely,filename,passover)
C
C	ROUTINE TO PROCESS fitsin PARAMETERS
C
	implicit none
c
	common /fitskeys/ keyword
c
	integer*4 iuni,excnt,bfctr
	character*8 keyword(35),passtart,passover,unlikely,excludst(10)
	character*8 exclud(10)
c
c	byte exclud(120)
	integer*4 stat,nods,cnt,def,lookcnt,nhdrs,fitsns,kk		!jj,kk
c	integer*4 len(10),iptr(10)
	logical*4 xfer,prfits,ihist,icomment,inull,idat,ipass,jpass,iexcise
	logical*4 tlist,u16,imgxfer,tbxfer,btxfer
	character*8 curr_host,intfmt,realfmt
	character*10 hist,comt,nul,dtype,blktype,tablist
	character*80 outline
	character*132 filename(10)
c
c	data exclud/120*32/			!120 spaces or ' ' = 32
	data curr_host/'HOSTNAME'/
c
	xfer=.false.			!No output to be done
	imgxfer=.false.
	tbxfer=.false.
	btxfer=.false.
	idat=.false.			!Default is "FITS" data type out
	ipass=.false.			!no limit on hdr records to pass
	iexcise=.false.
	call xvpcnt('OUT',nods)		!get number of output data sets
	prfits=.false.			!no FITS header output file
	
	call xvpcnt('HEADER',nhdrs)	!get number of HEADER data sets
	call xvp ('HEADER',filename,nhdrs)
	if (nhdrs.gt.0) prfits=.true.

	bfctr=1				!blocking factor = 1
	call xvp('BLOCK',blktype,cnt)
	if (blktype.eq.'BLOCK') then
		call xvp('FACTOR',bfctr,cnt)
		if (bfctr.gt.10) then
			write (outline,10100) bfctr
10100	format ('??E - Blocking factor = ',i4,' - greater than 10')
			call xvmessage(outline,' ')
			call abend
		endif
	endif
	call xvparm('PASS',passover,lookcnt,def,1)   !WORD IN LABEL TO BEGIN EXTRACTING
	jpass=.true.
	if (passover.eq.' ') jpass=.false. !indicate that PASS= was not given
	if (passover.eq.unlikely) jpass=.false.

	if (lookcnt.ne.0) then
          call mvlc(passover,passtart,8)
          call uprcase(passtart)
        endif
c
c!	need to read in label item in 'PASS' and replace each null
c!	with a space since user probably will not fill 'PASS' parameter
c!	with blanks if keyword less than 8 characters
c
	if (jpass) then
		write (outline,10110) passtart
10110 format ('passtart = ',a8)
		call xvmessage (outline,' ')
	endif
c
c! note: exclud is passed over as a byte string since excluded items are all
c!  passed over as lower case
c
	call xvparm('EXCISE',exclud,excnt,def,10)	!WORDS TO DROP FROM LABELS
        do kk=1,excnt
            call mvlc(exclud(kk),excludst(kk),10)
            call uprcase(excludst(kk))
        enddo

	if (excnt.ge.1) iexcise=.true.
cc	if (.not.iexcise) go to 5
c
c!	same for 'EXCLUD' 
c
cc	call xvsptr(exclud,10,iptr,len)		!unpack the parameter string
cc	do jj=1,120
cc	if (exclud(jj).ge.97) exclud(jj)=exclud(jj)-32
c			!if small letters - convert to  caps
cc	enddo
cc	do kk=1,excnt
cc		call mvlc(exclud(iptr(kk)),excludst(kk),len(kk))
cc	enddo	
c5	continue
C!	IHST=.TRUE. FOR "HISTORY", .FALSE. FOR "NOHISTORY"
	ihist=.true.
	call xvp('HISTORY',hist,cnt)
	if (hist.eq.'NOHISTORY') ihist=.false.
C!	ICMT=.TRUE. FOR "COMMENT", .FALSE. FOR "NOCOMMENT"
	icomment=.true.
	call xvp('COMMENT',comt,cnt)
	if (comt.eq.'NOCOMMENT') icomment=.false.
c	
	inull=.true.			!pass null lines
	call xvp('NULL',nul,cnt)
	if (nul.eq.'NONULL') inull=.false.
C!	IDAT=.TRUE. FOR "TRUE" DATA, .FALSE. FOR "FITS" TAPE DATA
	idat=.false.
	u16=.false.
	call xvp('DATA',dtype,cnt)
	if (dtype.eq.'true') idat=.true.
	if (dtype.eq.'U16') u16=.true.
c!	tlist=.true. for list tabular file
	tlist=.false.
	call xvp ('TABLIST',tablist,cnt)
	if (tablist.eq.'LIST') tlist=.true.
c
	fitsns=2880*bfctr
C
C!	OPEN FITS (INPUT) DATA SET
C
	call xvunit(iuni,'INP',1,stat,' ')
	call xvopen(iuni,stat,'OPEN_ACT','SA','I_FORMAT',
     1 'BYTE','OP','READ','COND','NOLABELS,NOBLOCKS','U_FORMAT','BYTE',
     2 'U_NS',fitsns,'CONVERT','OFF',' ')
	call chkstat (stat,'??E - XVopen error on FITS file',1,0,0)
	call xvhost (curr_host,intfmt,realfmt,stat)
	call chkstat (stat,'??E - XVhost error',1,0,0)
c**	write (outline,10050) intfmt
c**10050 format (' XVhost Integer format = ',a8)
c**	call xvmessage (outline,' ')

	return
	end
C=========================================================================
	subroutine hdrproc (bfctr,ouni2,catrec,tabrec,
     1 fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
     2 xfer,prfits,iscl,izero,
     3 xtens,imgxfer,tbxfer,btxfer,table,ineof,
     4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
     5 tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)
c
c!	routine to process FITS headers into VICAR labels
c
	implicit none
c
	include 'errdefs'
c
	common /fitskeys/ keyword
c
	byte data(28800)
	integer*4 ouni2,totlab,catrec,tabrec
	integer*4 iuni,npix,nlin,nband,btpix,bfctr
	integer*4 fitsns,fitsrec,stat,nax,fitstart
	integer*4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount
	integer*4 gcount,tfields,extver,extlevel,tbcol(999)
	logical*4 xfer,prfits,iscl,izero
	logical*4 xend,imgxfer,tbxfer,btxfer,valid
	logical*4 iend,xtens,table,oneblock,xtend,group
	logical*4 hierarch,bintable,ineof
	real*4 bzero,bscale
	character*4 ibfmt(999)
	character*8 ttype(999)
	character*8 keyword(35)
	character*8 xtype
	character*10 tform(999)
	character*16 extname
	character*80 outline
c
	data xtype/'        '/
	xtend = .false.
c
C**************************
C!     EXTRACT EACH HEADER RECORD INTO ARRAY "DATA"(28800) AND EXAMINE.
C!	Header is 2880 bytes (36 80-byte card images) times the tape
C!	blocking factor (bfctr)
C**************************
	stat = 0
	fitsns=2880*bfctr
	fitsrec=fitstart
	ineof = .false.
 	call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
C					!CHECK FOR VALID HEADER KEYWORD
	if (stat.eq.END_OF_FILE) then
		ineof=.true.
		return
	endif
	call chkstat (stat,'??E - XVRead error - hdrproc',1,0,0)
	iend=.false.
	xtens=.false.
	table=.false.
	oneblock=.false.
	hierarch=.false.
	group=.false.
	bintable=.false.
	valid=.false.
	call valhdr  (data,xfer,oneblock,xtens,table,xtype,
     1 	bintable,imgxfer,tbxfer,btxfer,valid)
	if (.not.valid) then
		write (outline,10060)
10060 format ('??E - Invalid header keyword')
		call xvmessage (outline,' ')
		write (outline,10065)
10065 format ('??W - One header block listed - but no processing')
		call xvmessage (outline,' ')
		xfer=.false.
		iend=.true.
		oneblock=.true.		!list only 1st block of header
	endif
C
C!	GET OUTPUT DATA SET CHARACTERISTICS
C
	if (xfer) then
		imgxfer=.true.
		tbxfer=.true.
		btxfer=.true.
	endif
	bzero=0.0
	bscale=1.0
	totlab=1
11	continue
c					!compare FITS with desired output 
	call fitschk (data,bfctr,xfer,iscl,izero,xtend,group,totlab,
     1 	hierarch,iend,bscale,bzero,btpix,nlin,npix,nband,nax,
     2 	imgxfer,tbxfer,btxfer)
	if (oneblock) then
		call fitslist (bfctr,iend,data)
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		go to 150
	endif					!c***
	if (.not.xfer) call fitslist (bfctr,iend,data)
	if (iend) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif
	call chkstat (stat,'??E - XVRead error - hdrproc',1,0,0)
	go to 11
c
20	continue
	if (.not. xtend) then
		tbxfer=.false.
		btxfer=.false.
	endif
c						!see if XTENSIONS on end
	if (xtend) then
		fitsrec=fitsrec+1
		catrec=fitsrec
 		call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
     1		fitsns,' ')
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif

		call chkstat (stat,'??E - XVRead error - hdrproc',0,0,0)
		valid=.false.
C					!CHECK FOR VALID HEADER KEYWORD
		call valhdr  (data,xfer,oneblock,xtens,table,xtype,
     1 		bintable,imgxfer,tbxfer,btxfer,valid) 
	endif
	continue
	if (xtens) then
		xend=.false.
25		continue
c
		if (table) then
			call tablchk (data,bfctr,totlab,xend,tabbits,
     1			tabax,tabcols,tabrows,tabdepth,pcount,gcount,
     2			tfields,extname,extver,extlevel,ttype,tbcol,
     3			tform,ibfmt)
		endif
		if (.not.xfer) call fitslist (bfctr,iend,data)
		if (xend) go to 31
		fitsrec=fitsrec+1
		call xvread(iuni,data,stat,'LINE',fitsrec,'NSAMPS',
     1		fitsns,' ')
		if (stat.eq.END_OF_FILE) go to 140
		call chkstat (stat,'??E - XVRead error - hdrproc',0,0,0)
		call chkstat (stat,'??E - XVRead err in XTENSION checks',1,0,0)
		go to 25
c
c! **** what about multiple extension sets in same file??
c!	may have to have another test for "EXTENSION ="??
c
	endif
c
c! *** if writing header file do the following
c
31	continue
	tabrec=fitsrec
	if (xfer.and..not.table) tbxfer=.false.
	if (xfer.and..not.bintable) btxfer=.false.
	if (prfits) then
c
		call xvopen(ouni2,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1		'U_FORMAT','BYTE','O_FORMAT','BYTE','OP','WRITE','COND',
     2		'NOLABELS','U_ORG','BSQ','U_NL',totlab,'U_NS',81,
     3		'U_NB',1,' ')
		call chkstat(stat,'??E - xvopen err HDR File ',1,0,0)
		fitsrec=fitstart
 		call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
     1		fitsns,' ')
		call chkstat (stat,'XVRead error - hdrproc',1,0,0)
35		continue
		iend=.false.
c					!will write to output file
		call fitsprt (data,bfctr,ouni2,iend,totlab)
		if (iend) go to 38
		if (oneblock) go to 40
		fitsrec=fitsrec+1
		call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
     1		fitsns,' ')
		call chkstat (stat,'??E - XVRead error - hdrproc',1,0,0)
		GO TO 35
38		continue
		if (xtens) then
			xend=.false.
			call fitsprt  (data,bfctr,ouni2,xend,totlab)
			if (xend) go to 40
			fitsrec=fitsrec+1
			call xvread(iuni,DATA,stat,'LINE',fitsrec,
     1		'NSAMPS',fitsns,' ')
			call chkstat (stat,'??E - XVRead error - hdrproc',1,0,0)
		endif
		call xvclose (ouni2,stat,' ')
	endif
40	CONTINUE
	go to 150
c	
140	continue
	call abend
c
150	continue
	return
	end
C=========================================================================
	subroutine imgopen (bfctr,iuni,ouni1,npix,nlin,nband,idat,izero,
     1 iscl,inull,xtens,btpix,bzero,bscale,fitsrec,fitstart,
     2 passtart,iexcise,excnt,excludst,u16,icomment)
c
	implicit none
	include 'errdefs'
c
C!	OPEN VICAR (OUTPUT) IMAGE DATA SET
c
	byte data(28800),lab(80,144)
	integer*4 btpix,nlin,npix,nband,nlab,totlab,trlab,fitsrec
	integer*4 i,iuni,ouni1,stat,fitsns,bfctr,excnt,ilab,fitstart
	logical*4 idat,iscl,izero,iend,ipass,jpass,xtend,icomment
	logical*4 ihist,inull,iexcise,xtens,u16,ineof
	real*4 bscale,bzero
	character*4 xmat
	character*5 key
	character*8 passtart,excludst(10)
	character*80 outline,flabel
C 
	data KEY/'VF000'/
c
	xtend = .false.
	jpass = .false.
	ihist = .true.
	fitsns=2880*bfctr
	xmat='BYTE'
	if (btpix.eq.16) xmat='HALF'
	if (btpix.eq.16 .and. u16) xmat='FULL'
	if (btpix.eq.32) xmat='FULL'
	if (btpix.eq.-32) xmat='REAL'
	if (btpix.eq.-64) xmat='DOUB'
	if (.not. idat) go to 80		!BRANCH IF FITS
	xmat='REAL'	
80	continue
	call xvopen(ouni1,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1  'U_FORMAT',xmat,'O_FORMAT',xmat,'OP','WRITE','U_NL',
     2  nlin,'U_NS',npix,'U_NB',nband,'U_ORG','BSQ',' ')
	call chkstat (stat,'??E - XVopen error on VICAR file',1,0,0)
	if (.not.idat) go to 90		!BRANCH IF FITS DATA
C! MAY HAVE TO SOME DAY TREAT CASE WHERE "XMAT" HAS TO BE COMPLEX OR DOUBLE
	if (iscl .and. izero) go to 90	!BRANCH IF BSCALE AND BZERO FOUND
	if (.not.izero)	then
		write (outline,10100)
10100	format('??W - - KEYWORD BZERO = not found for DATA=TRUE')
		call xvmessage(outline,' ')
	endif
	if (.not. iscl) then
		write (outline,10200)
10200	format ('??W - KEYWORD BSCALE = not found for DATA=TRUE')
		call xvmessage (outline,' ')
	endif
	if (.not.iscl .or. .not.izero) then
		write (outline,10300)
10300	format ('??W - FITS data transferred as vicar real data')
		call xvmessage (outline,' ')
	endif
90	continue
	nlab=1			!INITIALIZE OUTPUT LABEL PTR
	totlab=0		!INITIALIZE TOTAL FITS KEYWORDS
	trlab=0			!initialize FITS keywords to be transferred
C
C!	REINITIALIZE POINTER TO START OF FITS FILE
C
	iend=.false.
	fitsrec=fitstart
	call xvread (iuni,data,stat,'LINE',fitsrec,'NSAMPS',fitsns,' ')
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif

	call chkstat (stat,'??E - XVRead error - imgopen',1,0,0)
c

100	continue
	if (.not. jpass) ipass=.true.
c				!EXAMINE/LIST REMAINING CARD IMAGES
	call fitskey (bfctr,npix,nlin,nband,jpass,
     1 ipass,bscale,bzero,iscl,izero,xtend,iend,icomment,ihist,
     2 inull,iexcise,excnt,xtens,nlab,trlab,totlab,
     3 excludst,passtart,lab,data)
C
C!	WRITE OUT FITS LABELS TO VICAR FILE
C
	do i=1,nlab
		call keyinc2(key)	!increment "VFxxx" VICAR Label prefix
 		ilab=trlab-nlab+i
		call mvlc (lab(1,i),flabel,80,' ')
		call xladd (ouni1,'HISTORY',key,flabel,STAT,'FORMAT',
     1	'STRING','ULEN',80,'ERR_ACT','S',' ')
		call chkstat(stat,'??E - xladd err LABEL # = ',0,ilab,1)
	ENDDO
	continue
	if (iend.and..not.xtens) go to 150	!Branch if done with headers
c	if (xend) go to 150
C***************************
C!     READ REST OF ANNOTATION RECORDS.
C***************************
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,
     1 ' ')					!READ NEXT HEADER
	call chkstat (stat,'??E - XVRead error - imgopen',0,0,0)
	nlab=1					!INITIALIZE OUTPUT LABEL PTR
	go to 100
c
150	continue
	return
	end
C=========================================================================
	subroutine tablopen (bfctr,iuni,ouni1,tabcols,tabrows,tabdepth,
     1 tfields,ttype,
     2 catrec,fitsrec,ibis,ibfmt,passtart,excludst,fullrecord,
     3 realrecord,asciirecord,doubrecord,xtend,icomment)
c
	implicit none
	include 'errdefs'
c
C!	OPEN IBIS TABLE (OUTPUT) DATA SET
c
	byte data(28800),lab(80,144)
	integer*4 i,j,nlab,totlab,trlab,fitsrec,catrec
	integer*4 tabcols,tabrows,tabdepth
	integer*4 tfields,ibis,count,abend
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 nfull,nreal,nascii,ndoub
	integer*4 iuni,ouni1,stat,fitsns,bfctr,excnt,ilab
	integer*4 ibis_column_find,ibis_group_new
	logical*4 iend,ipass,jpass,xtend,icomment,idat
	logical*4 xtens,xend,iscl,izero,ihist,inull
	logical*4 iexcise,ineof
	real*4 bscale,bzero
	character*4 ibfmt(999)
	character*5 key
	character*8 passtart,excludst(10)
	character*8 ttype(999)
	character*16 iborg
	character*30 fitstabfile
	character*80 outline,flabel
C 
	data KEY/'VF000'/,iborg/'ROW'/
	data fitstabfile/'FITS TABLE CONVERSION'/
c
	fitsns=2880*bfctr
	idat=.false.			!Default "FITS"
	ipass=.true.
	jpass=.false.
	iexcise=.false.
	ihist=.true.			!pass HISTORY= lines
	icomment=.true.			!pass COMMENT= lines
	inull=.true.			!pass null lines (all blanks)
	bscale=1.0
	bzero=0.0
	xend=.false.

	write (outline,10100) tfields,tabcols,tabrows
10100 format ('tfields = ',i5,' tabcols = ',i5,' tabrows = ',i5)
	call xvmessage (outline,' ')
	write (outline,10105) tfields
10105 format ('Column format: 1 - ',i4) 
	call xvmessage (outline,' ')
	write (outline,10110) (ibfmt(i), i=1,tfields)
10110 format (12(1x,a4))
	call xvmessage (outline,' ') 
c
	call ibis_file_open (ouni1,ibis,'WRITE',tfields,tabrows,ibfmt,
     1 iborg,stat)
	if (stat.ne.1) call ibis_signal_u (ouni1,stat,1)
c
	call ibis_file_set(ibis,'type',fitstabfile,stat)
	if (stat.lt.0) call ibis_signal(ibis,stat,abend)
	nfull=0
	nreal=0
	nascii=0
	ndoub=0
c
	nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
	if (nfull.lt.0) call ibis_signal (ibis,nfull,abend)
	nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
	if (nreal.lt.0) call ibis_signal (ibis,nreal,abend)
	nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
	if (nascii.lt.0) call ibis_signal (ibis,nascii,abend)
	ndoub=ibis_column_find(ibis,'format','DOUB',0,0,0)
	if (ndoub.lt.0) call ibis_signal (ibis,ndoub,abend)

	if (nfull.gt.0) then
		call ibis_record_open (ibis,fullrecord,'format:FULL',
     1		0,0,'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
		call chkstat (stat,'??E - IBIS_record_open error on FULLgroup',1,0,0)
	endif
	if (nreal.gt.0) then
		call ibis_record_open (ibis,realrecord,'format:REAL',
     1		0,0,'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	if (nascii.gt.0) then
		call ibis_record_open (ibis,asciirecord,'format:ASCII',
     1		0,0,'a40',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	if (ndoub.gt.0) then
		call ibis_record_open (ibis,doubrecord,'format:DOUB',
     1		0,0,'doub',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	do j=1,tfields
		count=ibis_group_new (ibis,'group',ttype(j),j,1,' ')
		if (count.lt.0) call ibis_signal (ibis,stat,abend)
	enddo
c
	continue
	nlab=1			!INITIALIZE OUTPUT LABEL PTR
	totlab=0		!INITIALIZE TOTAL FITS KEYWORDS
	trlab=0			!initialize FITS keywords to be transferred
C
C!	REINITIALIZE POINTER TO START OF FITS FILE
C
	iend=.false.
	fitsrec=catrec
 	call xvread(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'??E - XVRead error - tablopen',0,0,0)
100	continue
c				!EXAMINE/LIST REMAINING CARD IMAGES
	call fitskey (bfctr,tabcols,tabrows,tabdepth,jpass,
     1 ipass,bscale,bzero,iscl,izero,xtend,xend,icomment,ihist,
     2 inull,iexcise,excnt,xtens,nlab,trlab,totlab,
     3 excludst,passtart,lab,data)
C
C!	WRITE OUT FITS LABELS TO IBIS FILE
C
	do i=1,nlab
		call keyinc2(key)	!increment "VFxxx" VICAR Label prefix
 		ilab=trlab-nlab+i
		call mvlc (lab(1,i),flabel,80,' ')
		call xladd (ouni1,'HISTORY',key,flabel,STAT,'FORMAT',
     1		'STRING','ULEN',80,'ERR_ACT','S',' ')
		call chkstat(stat,'??E - xladd err LABEL # = ',0,ilab,1)
	ENDDO
	continue
	if (iend.and..not.xtens) go to 150	!Branch if done with headers
	if (xend) go to 150
C***************************
C!     READ REST OF ANNOTATION RECORDS.
C***************************
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',
     1 	fitsrec,' ')				!READ NEXT HEADER
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif

	call chkstat (stat,'??E - XVRead error - tablopen',1,0,0)
	nlab=1					!INITIALIZE OUTPUT LABEL PTR
	go to 100
c
150	continue
	return
	end
C=========================================================================
	subroutine bfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
     1 bscale,fitsrec,idat)
c
c!	subroutine to write out VICAR byte data from from FITS byte data
c
	implicit none
	include 'errdefs'
c
	byte bdata(28800),data(28800)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband
	integer*4 bfctr,fitsrec
	integer*4 vicptr,vicrow,vicband,fitsptr
	logical*4 idat,ineof
	real*4 bscale,bzero
	real*4 rdata(28800)
c
	fitsns=bfctr*2880		!samples(bytes)/record
	il=0
	fitsptr=fitsns+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c        if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
	    call chkstat (stat,'??E - XVRead error - bfitsi',1,0,0)
	endif
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns-fitsptr)
	do i=1,il
		bdata(vicptr+i)=data(fitsptr+i)
	enddo
C
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C!	NEED TO CONVERT HERE USING FORMULA
C!	ODATA = VAX*BSCALE+BZERO
C!	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=BDATA(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - bfitsi',0,0,0)
	else
		call xvwrit(ouni,bdata,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - bfitsi',0,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
c=========================================================================
	subroutine hfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
     1 bscale,fitsrec,idat)
c
c!	Routine to transfer FITS integer*2 data to VICAR image file
c
	implicit none
	include 'errdefs'
c
	byte data(28800)
	integer*2 hdata(14400),odata(14400)
	integer*4 il,i,j,stat,fitsns,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns2,fitsrec
	integer*4 transhalf(12)
	logical*4 idat,ineof
	real*4 bzero,bscale
	real*4 rdata(7200)
c
c!	modified algorithm from reference 1
c
	call xvtrans_in (transhalf,'HALF','HALF','HIGH','IEEE',stat)
	call chkstat (stat,'??E - XVtrans_in error for transhalf',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns2=fitsns/2		!halfs/record
	il=0
	fitsptr=fitsns2+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row 
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns2) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c        if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
            call chkstat (stat,'??E - XVRead error - hfitsi',1,0,0)
        endif

	call xvtrans (transhalf,data,hdata,fitsns2)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns2-fitsptr)
	do i=1,il
		odata(vicptr+i)=hdata(fitsptr+i)
	enddo
	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
	if (idat) then
C
C!	NEED TO CONVERT HERE USING FORMULA
C!	RDATA = ODATA*BSCALE+BZERO
C!	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=ODATA(J)*BSCALE+BZERO
		enddo
		call xvwrit(OUNI,RDATA,STAT,'NSAMPS',npix,'LINE',vicrow,
     1		'BAND',vicband,' ')		!Output data to VICAR
		call chkstat (stat,'??E - XVwrit error - hfitsi',1,0,0)
	else
		call xvwrit(OUNI,ODATA,STAT,'NSAMPS',npix,'LINE',vicrow,
     1		'BAND',vicband,' ')		!Output data to VICAR
		call chkstat (stat,'??E - XVwrit error - hfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10		!loop if not finished band
	vicband=vicband+1			!bump band count
	vicrow=1				!reinit row ctr
	if (vicband.le.nband) go to 10		!loop if want to start new band
	return
	end
c=========================================================================
	subroutine ufitsi (iuni,ouni,npix,nlin,nband,bfctr,
     1 bzero,bscale,fitsrec)
c
c!	Routine to transfer unsigned FITS integer*2 data to a
c!	fullword VICAR image file since VICAR 16-bit data is signed
c
	implicit none
	include 'errdefs'
c
	byte data(28800)
	integer*2 hdata(14400)
	integer*4 fdata(14400)
	integer*4 il,i,stat,fitsns,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns2,fitsrec,temp
	integer*4 transhalf(12)
	logical*4 ineof
	real*4 bzero,bscale
c
c!	modified algorithm from reference 1
c
	call xvtrans_in (transhalf,'HALF','HALF','HIGH','IEEE',stat)
	call chkstat (stat,'??E - XVtrans_in error for transhalf',0,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns2=fitsns/2		!halfs/record
	il=0
	fitsptr=fitsns2+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row 
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns2) go to 20
	fitsrec=fitsrec+1
	call xvread(iuni,data,stat,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c	if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
            call chkstat (stat,'??E - XVRead error - ufitsi',1,0,0)
        endif

	call xvtrans (transhalf,data,hdata,fitsns2)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns2-fitsptr)
	do i=1,il
		fdata(vicptr+i)=hdata(fitsptr+i)*bscale+bzero
		if (fdata(vicptr+i).lt.0) then
			temp=hdata(fitsptr+i)
			fdata(vicptr+i)=65536+temp
		endif
	enddo
	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
	call xvwrit(ouni,fdata,stat,'NSAMPS',npix,'LINE',vicrow,
     1	'BAND',vicband,' ')		!Output data to VICAR
	call chkstat (stat,'??E - XVwrit error - ufitsi',1,0,0)
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10		!loop if not finished band
	vicband=vicband+1			!bump band count
	vicrow=1				!reinit row ctr
	if (vicband.le.nband) go to 10		!loop if want to start new band
	return
	end
C=========================================================================
	subroutine ifitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
     1 bscale,fitsrec,idat)
c
c!	subroutine to write out VAX integer*4 data from FITS integer*4  data
c
	implicit none
	include 'errdefs'
c
	byte data(28800)
	integer*4 full(7200),vax(7200)
	integer*4 transfull(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns4,fitsrec
	logical*4 idat,ineof
	real*4 bzero,bscale
	real*4 rdata(28800)
c
	call xvtrans_in (transfull,'FULL','FULL','HIGH','IEEE',stat)
	call chkstat (stat,'??E - XVtrans_in error for transfull',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns4=fitsns/4		!fulls/record
	il=0
	fitsptr=fitsns4+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns4) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c        if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
            call chkstat (stat,'??E - XVRead error - ifitsi',1,0,0)
        endif

	call xvtrans (transfull,data,full,fitsns4)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns4-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=full(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C!	NEED TO CONVERT HERE USING FORMULA
C!	ODATA = VAX*BSCALE+BZERO
C!	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do J=1,npix
			RDATA(J)=VAX(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - ifitsi',1,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - ifitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine rfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
     1 bscale,fitsrec,idat)
c
c!	subroutine to write out VAX real*4 data from ieee real*4 from FITS
c
	implicit none
	include 'errdefs'
c
	byte data(28800)
	integer*4 transreal(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns4,fitsrec
	logical*4 idat,ineof
	real*4 vax(7200),rdata(28800)
	real*4 ieee(7200)
	real*4 bzero,bscale
c
	call xvtrans_in (transreal,'REAL','REAL','HIGH','IEEE',stat)
	call chkstat (stat,'??E - XVtrans_in error for transreal',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns4=fitsns/4		!reals/record
	il=0
	fitsptr=fitsns4+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns4) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c        if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
            call chkstat (stat,'XVRead error - rfitsi',1,0,0)
        endif

	call xvtrans (transreal,data,ieee,fitsns4)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns4-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=ieee(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C!	NEED TO CONVERT HERE USING FORMULA
C!	RDATA = ODATA*BSCALE+BZERO
C!	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do J=1,npix
			RDATA(J)=VAX(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - rfitsi',0,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - rfitsi',0,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine dfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
     1 bscale,fitsrec,idat)
c
c!	subroutine to write out VAX real*8 data from ieee real*8 from FITS
c
	implicit none
	include 'errdefs'
c
	byte data(28800)
	integer*4 transdble(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns8,fitsrec
	logical*4 idat,ineof
	real*8 vax(3600),rdata(28800)
	real*8 ieee8(3600)
	real*4 bzero,bscale
c
	call xvtrans_in (transdble,'DOUB','DOUB','HIGH','IEEE',stat)
	call chkstat (stat,'??E - XVtrans_in error for transdble',0,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns8=fitsns/8		!doubles/record
	il=0
	fitsptr=fitsns8+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns8) go to 20
	fitsrec=fitsrec+1
	call xvread(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
c        if (stat.eq.END_OF_FILE) then
c                ineof=.true.
c                return
c        endif
        if (stat.eq.END_OF_FILE .and. vicrow.ge.(nlin-1)) then
	    ineof=.true.
            call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
        else
            call chkstat (stat,'??E - XVRead error - dfitsi',1,0,0)
        endif

	call chkstat (stat,'??E - XVRead error - dfitsi',0,0,0)
	call xvtrans (transdble,data,ieee8,fitsns8)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns8-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=ieee8(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C!	NEED TO CONVERT HERE USING FORMULA
C!	RDATA = ODATA*BSCALE+BZERO
C!	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=VAX(J)*dble(BSCALE)+dble(BZERO)
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - dfitsi',1,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
     1		vicrow,'BAND',vicband,' ')
		call chkstat (stat,'??E - XVwrit error - dfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine fitsibis (iuni,tabcols,tabrows,tabdepth,tfields,
     1 bfctr,tabrec,ibfmt,tform,tbcol,fullrecord,realrecord,
     2 asciirecord,doubrecord,ibis)
c
c!	subroutine to write out IBIS data from from FITS Table data
c
c! npix=tabcols  nlin=tabrows   nband=tabdepth
c
	implicit none
	include 'errdefs'
c
	byte bdata(28800),data(28800)
	integer*4 il,stat,fitsns,i,field,iuni,tabcols,tabrows
	integer*4 tabdepth,bfctr,tabrec,fitsrec,tfields,ibis
	integer*4 vicptr,vicrow,vicband,fitsptr,abend
	integer*4 fullindex,realindex,asciiindex,doubindex
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 tbcol(999)
	integer*4 fullval(999)
	real*4 realval(999)
	real*8 doubval(999)
	logical*4 ineof
	character*4 ibfmt(999)
	character*10 tform(999)
	character*20 asciival(999)
	character*2880 fitstable
c
	realindex=0
	fullindex=0
	asciiindex=0
	doubindex=0
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsrec=tabrec
	il=0
	fitsptr=fitsns+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns) go to 20
	fitsrec=fitsrec+1
	call xvread(iuni,data,stat,'LINE',fitsrec,'NSAMPS',fitsns,' ')
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif

	call chkstat (stat,'??E - XVRead error - fitsibis',1,0,0)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(tabcols-vicptr,fitsns-fitsptr)
	do i=1,il
		bdata(vicptr+i)=data(fitsptr+i)
	enddo
C
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (fitsrec .ge. vicptr) then
		ineof=.true.
		return
	endif
	if (vicptr.lt.tabcols) go to 10
	call mvlc (bdata,fitstable,tabcols,' ')
	
	do field=1,tfields
		call fitsconv (field,fitstable,tfields,tform,tbcol,ibfmt,
     1		realindex,fullindex,asciiindex,doubindex,realval,
     2		fullval,asciival,doubval,tabcols)
	enddo
	if (fullindex.gt.0) then
		call ibis_record_write (fullrecord,fullval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (realindex.gt.0) then
		call ibis_record_write (realrecord,realval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (asciiindex.gt.0) then
		call ibis_record_write (asciirecord,asciival,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (doubindex.gt.0) then
		call ibis_record_write (doubrecord,doubval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	realindex=0
	fullindex=0
	asciiindex=0
	doubindex=0
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.tabrows) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.tabdepth) go to 10
	return
	end
C=========================================================================
	subroutine fitsconv (field,fitstable,tfields,tform,tbcol,ibfmt,
     1 realindex,fullindex,asciiindex,doubindex,realval,
     2 fullval,asciival,doubval,tabcols)
c
c!	routine to pack data into ibis buffers
c
	implicit none
c
	integer*4 field,tfields,realindex,fullindex,asciiindex,doubindex
	integer*4 colbeg,colend,tabcols
	integer*4 fullval(999),tbcol(999)
	real*4 realval(999)
	real*8 doubval(999)
	character*4 ibfmt(999)
	character*10 tform(999)
	character*20 asciival(999)
	character*2880 fitstable
c
	if (ibfmt(field).eq.'FULL') then
		fullindex=fullindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
     1		fullval(fullindex)
	else if (ibfmt(field).eq.'REAL') then
		realindex=realindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
     1		realval(realindex)
	else if (ibfmt(field).eq.'DOUB') then
		doubindex=doubindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
     1		doubval(doubindex)
	else if (ibfmt(field)(1:1).eq.'A') then
		asciiindex=asciiindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
     1		asciival(asciiindex)
	endif
c
	return
	end
C=========================================================================
	subroutine valhdr (data,xfer,oneblock,xtens,table,xtype,
     1 bintable,imgxfer,tbxfer,btxfer,valid) 
C
C!	Routine to examine FIRST CARD IMAGE OF HEADER and FIRST CARD
C!	IMAGE OF XTENSION HEADER
C
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800)
	integer*4 j
	logical*4 xfer,simple,xtens,table,bintable,oneblock
	logical*4 imgxfer,tbxfer,btxfer,valid
	character*1 ch1,fitstrue,fitsfalse
	character*8 keyword(35)
	character*8 hdrword,xtype
	character*80 outline
c  for some reason 'T' was changed to 124 (ASCII T) and 'F' to 70 (ASCII F)
C	that fix wouldnt work under Linux c compiler
c	Corrected by changing to character, not byte, data - May 4, 2011
	data fitstrue /'T'/, fitsfalse /'F'/		!true='T' (124)  false='F' (70)
C
C! Check first keyword
C
	call mvlc (data(1),hdrword,8,' ')
	if (keyword(1) .ne. HDRWORD) GO TO 10020		!SIMPLE =
	simple=.true.
	valid=.true.
	if (xfer) imgxfer=.true.
	call mvlc (data(30),ch1,1,' ')
	if (ch1.eq.fitstrue) GO TO 10100
	if (ch1.ne.fitsfalse) GO TO 10005
	simple=.false.
	write (outline,100)
100	format ('??W - SIMPLE = F, Data does not conform to FITS
     1 STANDARD')
	call xvmessage (outline,' ')
	write (outline,105)
105	format ('Processing will be attempted anyway...')
	call xvmessage (outline,' ')
	GO TO 10100
C
C!	LOOP THROUGH FIELDS 11-30 TO SEE IF 'T' OR 'F' IN NONSTANDARD POSITION
C
10005 CONTINUE
	do 10010 J=11,30
	call mvlc (data(j),ch1,1,' ')
	if (ch1 .ne. fitstrue) GO TO 10008
	write (outline,110)
110	format ('??W - SIMPLE = T, T found in nonstandard position')
	call xvmessage (outline,' ')
	GO TO 10100
c
10008 CONTINUE
	if (ch1 .ne. fitsfalse) GO TO 10010
	write (outline,115)
115	format ('??W -  SIMPLE = F, F found in nonstandard position')
	call xvmessage (outline,' ')
	write (outline,100)
	call xvmessage (outline,' ')
	write (outline,105)
	call xvmessage (outline,' ')
	GO TO 10100
c
10010 CONTINUE
	write (outline,120)
120	format ('??E - No valid T or F found for SIMPLE =')
	call xvmessage (outline,' ')
	write (outline,125)
125	format ('One header block listed - but no processing')
	call xvmessage (outline,' ')
	xfer=.false.			!LIST BUT NO PROCESSING
	oneblock=.true.			!list only 1st block of header
	GO TO 10100
C
C!	CHECK FOR HDRWORD = XTENSION
C!	common xtensions are '3DTABLE ', 'A3DTABLE', 'TABLE   ', 'BINTABLE'
C
10020 CONTINUE						!EXTENSION =
	if (keyword(15).NE.HDRWORD) GO TO 10030
	xtens=.true.
	valid=.true.
	call mvlc (data(12),xtype,8,' ')
	if (xtype.eq.'TABLE   ') then
		table=.true.
		if (xfer) tbxfer=.true.
	endif
	if (xtype.eq.'BINTABLE') then
		bintable=.true.
		if (xfer) btxfer=.true.
	endif
c**	write (outline,127) xtype
c**127	format ('**************     XTENSION = ',a8,' KEYWORD FOUND')
c**	call xvmessage (outline,' ')
	if (xtype.eq.'3DTABLE ') then
		write (outline,128)
128	format ('??W - 3DTABLE - That FITS data type not supported')
		call xvmessage (outline,' ')
		xfer=.false.
	endif
	if (xtype.eq.'A3DTABLE') then
		write (outline,128)
		call xvmessage (outline,' ')
		xfer=.false.
	endif
	GO TO 10100
C
C!	CHECK FOR HDRWORD = TABNAME - OLDER AIPS FILES
C
10030 CONTINUE
	IF (keyword(27).NE.HDRWORD) GO TO 10050			!TABNAME =
	table=.true.
	valid=.true.
	write (outline,130)
130	format ('??E - TABNAME = older AIPS keyword found')
	call xvmessage (outline,' ')
	write (outline,125)
	call xvmessage (outline,' ')
	write (outline,135)
135	format ('*** SEE RESPONSIBLE PROGRAMMER ***')
	call xvmessage (outline,' ')
	xfer=.false.			!LIST BUT NO PROCESSING
	go to 10100
C
C!	INVALID HEADER KEYWORD
C
10050 CONTINUE
	valid=.false.
10100 CONTINUE
	return
	end
C=========================================================================
	subroutine fitschk  (data,bfctr,xfer,iscl,izero,xtend,group,totlab,
     1 hierarch,iend,bscale,bzero,btpix,nlin,npix,nband,nax,
     2 imgxfer,tbxfer,btxfer)
C
C!	ROUTINE TO EXAMINE EACH HEADER RECORD OF 2880 BYTES,
C!	TO COMPARE FITS DATA WITH DESIRED VICAR OUTPUT IMAGE OR TABLE,
C!	AND PASS OUT APPROPRIATE WARNING MESSAGES
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),CARD(80,360)
	integer*4 i,k,m,one,two,three,nax,btpix,bfctr
	integer*4 npix,nlin,nband,totlab
	logical*4 iend,xtend,group,imgxfer,tbxfer,btxfer
	logical*4 xfer,iscl,izero,hierarch
	real*4 bzero,bscale
	character*1 ch1,fitstrue
	character*8 fkey(360)
	character*80 outline,cardimg
c
	data one,two,three/1,2,3/
c        data true /124/, false /70/             !true='T' (124)  false='F' (70)
c  for some reason 'T' was changed to 124 (ASCII T) and 'F' to 70 (ASCII F)
C       that fix wouldnt work under Linux c compiler
        data fitstrue /'T'/			!fitsfalse /70/   !true='T' (124)  false='F' (70)
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	do 10510 M=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')
	totlab=totlab+1
	if (fkey(m).eq.keyword(1)) go to 10510		!SIMPLE
	if (fkey(m).eq.keyword(18)) go to 10510		!'        '
	if (fkey(m).eq.keyword(11)) go to 10510		!COMMENT
	if (fkey(m).eq.keyword(12)) go to 10510		!HISTORY
	if (FKEY(M).NE.KEYWORD(2)) GO TO 10010		!BITPIX 
	read (cardimg(27:30),20110) btpix
20110 format (i4)
	if (btpix.eq.8) go to 10510
	if (btpix.eq.16) go to 10510
	if (btpix.eq.32) go to 10510
	if (btpix.eq.-32) go to 10510
	if (btpix.eq.-64) go to 10510
	write (outline,105) btpix
105	format ('??E - BITPIX =',i6,' a non-standard value')
	call xvmessage (outline,' ')
	write (outline,110)
110	format ('Header block listed - but no processing')
	call xvmessage (outline,' ')
	xfer=.false.
	imgxfer=.false.
	tbxfer=.false.
	btxfer=.false.
	GO TO 10510
c
10010 CONTINUE
	if (FKEY(M).NE.KEYWORD(3)) GO TO 10020		!NAXIS - NUMBER OF AXES
	read (cardimg(27:30),20110) nax
c**	if (nax.eq.0) then
c**		write (outline,115)
c**115	format ('** NAXIS = 0, NO DATA ARRAY PRESENT')
c**		call xvmessage (outline,' ')
c**		if (xfer) imgxfer=.false.
c**		go to 10510
c**	endif

	if (nax.gt.3) then
		write (outline,120) nax
120	format ('??E - NAXIS = ',i6,' NUMBER OF AXES GREATER THAN 3')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		write (outline,125)
125	format ('*** SEE RESPONSIBLE PROGRAMMER ***')
		call xvmessage (outline,' ')
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		xfer=.false.	!LIST BUT NO PROCESSING whether image or table
		GO TO 10510
	endif
	go to 10510
c
10020 CONTINUE
	IF (FKEY(M).NE.KEYWORD(4)) GO TO 10030	!NAXIS1 - Number of Pixels
	read (cardimg(27:30),20110) npix
	if (npix.eq.0) then
		write (outline,127) one
127	format ('??E - NAXIS',i1,' = 0 - No data array present')
		call xvmessage (outline,' ')
		if (xfer) imgxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10030 CONTINUE
	IF (FKEY(M).NE.KEYWORD(5)) GO TO 10040	!NAXIS2 - Number of Lines
	read (cardimg(27:30),20110) nlin
	if (nlin.eq.0) then
		write (outline,130) two,two
130   format ('??E - NAXIS',i1,' = 0, ,data in AXIS ',i1,
     1 ' not an array, or')
		call xvmessage (outline,' ')
		write (outline,117)
117	format ('data is possibly an irregularly-spaced array')
		call xvmessage(outline,' ')
		if (xfer) imgxfer=.false.
		GO TO 10510
	endif
	nband = 1			!correction 12-14-2012
	go to 10510
c
10040 CONTINUE
	IF (FKEY(M).NE.KEYWORD(6)) GO TO 10050	!NAXIS3 - Number of Bands
	read (cardimg(27:30),20110) nband
	if (nband.eq.0) then
		write (outline,130) three,three
		call xvmessage (outline,' ')
		write (outline,117)
		call xvmessage(outline,' ')
		GO TO 10510
	endif
	go to 10510
c
10050 CONTINUE
	IF (FKEY(M).NE.KEYWORD(7)) GO TO 10060		!BSCALE - 
c!	call incon (nword,card(11,m),bscale,20)
	read (cardimg(11:30),20120) bscale
20120 format (f20.0)
	if (bscale.gt.1.0) then
		write (outline,150)
150	format ('??W - Data cannot be restored with original accuracy')
		call xvmessage (outline,' ')
		write (outline,155) bscale
155	format ('**Note that BSCALE = ',f9.2)
		call xvmessage (outline,' ')
	endif
	iscl=.true.
	GO TO 10510
c
10060 CONTINUE
	IF (FKEY(M).NE.KEYWORD(8)) GO TO 10070		!BZERO - 
	read (cardimg(11:30),20120) bzero
	izero=.true.
	GO TO 10510
c
10070 CONTINUE
	IF (FKEY(M).NE.KEYWORD(13)) GO TO 10080		!BLOCKED - 
	call mvlc (card(30,m),ch1,1,' ' )
	IF (ch1 .eq. fitstrue) then
C
C!	DONT KNOW ABOUT FOLLOWING YET ....
C
		write (outline,170)
170	format ('??W - BLOCKED = T, TAPE records may be blocked')
		call xvmessage (outline,' ')
		write (outline,175)
175	format ('Processing will be attempted anyway...')
		call xvmessage (outline,' ')
		GO TO 10510
	endif
	go to 10510
c
10080 CONTINUE
	IF (FKEY(M).NE.KEYWORD(14)) GO TO 10100		!EXTEND -
	call mvlc (card(30,m),ch1,1,' ' )
	if (ch1 .eq. fitstrue) then
		write (outline,180)
180	format ('??W - EXTEND = T, Data may contain FITS extensions')
		call xvmessage (outline,' ')
		xtend=.true.
		GO TO 10510
	endif
	go to 10510
c
10100 CONTINUE
	IF (FKEY(M) .NE. KEYWORD(20)) GO TO 10110		!GROUPS -
	call mvlc (card(30,m),ch1,1,' ' )
	IF (ch1 .eq. fitstrue) then
		write (outline,190)
190	format ('??W - GROUPS = T, Data contains a GROUP FORMAT file')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		group=.true.
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10110 continue
	IF (FKEY(M).NE.KEYWORD(30)) GO TO 10120		!HIERARCH -
	call mvlc (card(30,m),ch1,1,' ' )
	if (ch1 .eq. fitstrue) then
		write (outline,200)
200	format ('??E - HIERARCH = T, No ability to process hierarch
     1 data type')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		hierarch=.true.
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10120	continue
	IF (FKEY(M).NE.KEYWORD(19)) GO TO 10510		!END - 
	iend=.true.
	GO TO 10515
c
10510	CONTINUE
10515	CONTINUE
	if (nax.eq.0.and.xfer.and.xtend) imgxfer=.false.  !Probably table
	return
	end
C=========================================================================
	subroutine fitsprt (data,bfctr,ouni,end,totlab)
C
C!	ROUTINE TO PRINT OUT FITS HEADER TO FILE
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),card(81,360)
	integer*4 bfctr
	integer*4 stat,i,k,m,ouni,totlab
	logical*4 end
	character*8 fkey(360)
C
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	card(81,k)=10
	enddo
c
	do m=1,36*bfctr
		call mvlc (card(1,m),fkey(m),8,' ')
		call xvwrit(ouni,card(1,m),stat,' ')  !Output data to HDR file
		call chkstat(stat,'??E - XVwrit err HDR File record = ',0,totlab,1)
	if (FKEY(m).eq.KEYWORD(19)) then		!END - 
		end=.true.
		return
	endif
	enddo
c
	return
	end
C=========================================================================
	subroutine fitslist (bfctr,iend,data)
C
C!	ROUTINE TO PRINT OUT FITS HEADER TO FILE
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),card(80,360)
	integer*4 i,k,m,bfctr
	logical*4 iend
	character*80 outline,cardimg
C
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	do m=1,36*bfctr
		call mvlc (card(1,m),cardimg,80,' ')
		write (outline,10000) cardimg
10000 format (a80)
		call xvmessage (outline,' ')
		if (cardimg(1:8).eq.KEYWORD(19)) then		!END - 
			iend=.true.
			return
		endif
	enddo
c
	return
	end
C=========================================================================
	subroutine fitskey (bfctr,npix,nlin,nband,jpass,
     1 ipass,bscale,bzero,iscl,izero,xtend,xend,icomment,ihist,
     2 inull,iexcise,excnt,xtens,nlab,trlab,totlab,
     3 excludst,passtart,lab,data)
C
C!	ROUTINE TO EXAMINE EACH HEADER OF 2880 BYTES
C!	AND TO INCLUDE OR EXCLUDE FITS KEYWORDS FROM TRANSFER
C!	TO VICAR LABEL
C
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800),lab(80,144)
	logical*4 ihist,icomment,inull,iscl,izero,ipass,jpass
	logical*4 iexcise,xend,xtens,xtend
	integer*4 excnt,trlab,nlab,totlab
	integer*4 npix,nlin,nband,bfctr
	real*4 bzero,bscale
	character*1 ch1,fitstrue,doubquo,singquo
	character*8 keyword(35),passtart,excludst(10)
c
	byte card(80,360)
	integer*4 i,ii,j,k,m,nbt,nax
	character*8 FKEY(360)
	character*80 outline,cardimg
c
        data fitstrue /'T'/,  doubquo /'"'/ 	!fitsfalse /70/, true='T' (124), false='F' (70), " (34)
	data singquo /"'"/
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo

	DO 10510 M=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')
	write (outline,10000) (card(ii,m), ii=1,80)
10000 format (80a1)
	call xvmessage (outline,' ')
	IF (FKEY(M).EQ.KEYWORD(1)) GO TO 10400	!SIMPLE 
	IF (FKEY(M).EQ.KEYWORD(15)) GO TO 10160	!XTENSION - Always store
	IF (FKEY(M).EQ.KEYWORD(9)) GO TO 10400	!BUNIT - Always store 
	IF (FKEY(M).EQ.KEYWORD(10)) GO TO 10400	!BLANK - Always store
	IF (FKEY(M).EQ.KEYWORD(16)) GO TO 10400	!DATAMAX - Always store
	IF (FKEY(M).EQ.KEYWORD(17)) GO TO 10400	!DATAMIN - Always store
	IF (FKEY(M).EQ.KEYWORD(20)) GO TO 10400	!GROUPS - Always store 
	IF (FKEY(M).EQ.KEYWORD(21)) GO TO 10400	!GCOUNT - Always store 
	IF (FKEY(M).EQ.KEYWORD(22)) GO TO 10400	!PCOUNT - Always store 
	IF (FKEY(M).EQ.KEYWORD(23)) GO TO 10400	!EXTNAME - Always store
	IF (FKEY(M).EQ.KEYWORD(24)) GO TO 10400	!EXTVER - Always store
	IF (FKEY(M).EQ.KEYWORD(25)) GO TO 10400	!EXTLEVEL - Always store
	IF (FKEY(M).EQ.KEYWORD(26)) GO TO 10400	!TFIELDS - Always store
	IF (FKEY(M).EQ.KEYWORD(27)) GO TO 10400	!TABNAME - Always store
	IF (FKEY(M).EQ.KEYWORD(28)) GO TO 10400	!OBJECT - Always store
	IF (FKEY(M).EQ.KEYWORD(29)) GO TO 10400	!DATE - Always store 
	IF (FKEY(M).EQ.KEYWORD(34)) GO TO 10400	!ORIGIN - Always store 
	IF (FKEY(M).NE.KEYWORD(2)) GO TO 10010	!BITPIX
	read (cardimg(27:30),20110) nbt
20110 format (i4)
	GO TO 10400				!Always store in VICAR label
10010 CONTINUE
	IF (FKEY(M).NE.KEYWORD(3)) GO TO 10020	!NAXIS - NUMBER OF AXES
	read (cardimg(27:30),20110) nax
	if (nax.eq.0) then
		call mvlc (card(1,m+1),passtart,8,' ')
		ipass=.true.
	endif
	GO TO 10400				!Always store in VICAR label
10020 CONTINUE
	IF (FKEY(M).NE.KEYWORD(4)) GO TO 10030	!NAXIS1 - Number of Pixels
	read (cardimg(27:30),20110) npix
	GO TO 10400				!Always store in VICAR label
10030 CONTINUE
	IF (FKEY(M).NE.KEYWORD(5)) GO TO 10040	!NAXIS2 - Number of Lines
	read (cardimg(27:30),20110) nlin
c!**	if (nax.gt.2) go to 10505		!Do following only if naxis<3
	if (jpass) go to 10505			!Skip following if PASS= given
	call mvlc (card(1,m+1),passtart,8,' ')
	ipass=.true.
	GO TO 10400				!Always store in VICAR label
10040 CONTINUE
	IF (FKEY(M).NE.KEYWORD(6)) GO TO 10050	!NAXIS3 - Number of Bands
	read (cardimg(27:30),20110) nband
	if (jpass) go to 10505			!Skip following if PASS= given
	call mvlc (card(1,m+1),passtart,8,' ')
	ipass=.true.
	GO TO 10400				!Always store in VICAR label
10050 CONTINUE
	IF (FKEY(M).NE.KEYWORD(7)) GO TO 10060	!BSCALE - 
	read (cardimg(11:30),20120) bscale
20120 format (f20.0)
	ISCL=.TRUE.
	GO TO 10400				!Always store in VICAR label
10060 CONTINUE
	IF (FKEY(M).NE.KEYWORD(8)) GO TO 10070	!BZERO - 
	read (cardimg(11:30),20120) bzero
	IZERO=.TRUE.
	GO TO 10400				!Always store in VICAR label
10070 CONTINUE
	IF (FKEY(M).NE.KEYWORD(13)) GO TO 10080	!BLOCKED -
	call mvlc (card(30,m),ch1,1,' ' ) 
	IF (ch1 .ne. fitstrue) GO TO 10300
	GO TO 10400				!Always store in VICAR label
10080 CONTINUE
	IF (FKEY(M).NE.KEYWORD(14)) GO TO 10090	!EXTEND - 
	call mvlc (card(30,m),ch1,1,' ' )
	if (ch1 .ne. fitstrue) GO TO 10300
	xtend=.true.
	GO TO 10400				!Always store in VICAR label
10090 CONTINUE
c!	IF (DATA(30).NE.false) GO TO 10500
	IF (FKEY(M).NE.KEYWORD(19)) GO TO 10110	!END - 
	xend=.true.
	GO TO 10400				!Always store in VICAR label
10110 CONTINUE
	IF (FKEY(M).NE.KEYWORD(11)) GO TO 10120	!COMMENT =
	if (.not. icomment) go to 10505		!Don't store
	GO TO 10400
10120 CONTINUE
	IF (FKEY(M).NE.KEYWORD(12)) GO TO 10130	!HISTORY =
	if (.not. ihist) go to 10505		!Don't store
	GO TO 10400
10130 CONTINUE
	IF (FKEY(M).NE.KEYWORD(18)) GO TO 10140	!'        ' =
	if (.not. inull) go to 10505		!Don't store
	GO TO 10400
10140 CONTINUE
	if (ipass) go to 10150			!Started "PASS"?
	IF (FKEY(M).NE.passtart) GO TO 10505	!If not - skip
	ipass=.true.				!Indicate we've found "PASS="
	GO TO 10400
10150 CONTINUE
	if (.not.iexcise) go to 10400
	do i=1,excnt
	IF (FKEY(M).EQ.EXCLUDST(i)) GO TO 10505	!Check to see if "EXCISE="
	enddo
	go to 10400
10160 continue
	xtens=.true.
	go to 10400
c
c	check for PASS= for things always entered but but not specifically
c	checked in PASS= loop
c
10300	continue
	IF (IPASS) GO TO 10400			!Started "PASS"?
	IF (FKEY(M).NE.passtart) GO TO 10505	!If not - skip
	ipass=.true.				!Indicate we've found "PASS="
C***************************
C!     MOVE OBJECT INFO TO LABEL.
C***************************
10400 CONTINUE

	do i=1,80
		lab(i,nlab)=card(i,m)
	enddo
	do j=1,80
		call mvlc (LAB(J,NLAB),ch1,1,' ' ) 
		if (ch1 .eq. singquo) then
			call mvcl (doubquo,LAB(J,NLAB),1,' ')
c			LAB(J,NLAB)=doubquo   	!replace ' with "
		endif
	enddo
	nlab=nlab+1
C
	trlab=trlab+1
	if (trlab.eq.1000) then
		write (outline,500)
500	format ('??W - Total FITS keywords to be passed exceed 999')
		call xvmessage (outline,' ')
	endif
10505	continue
	totlab=totlab+1				!INCREMENT TOTAL LABELS COUNTER
	if (xend) go to 10600
10510	continue
	nlab=nlab-1				!keep index counter correct
	return
c
10600 CONTINUE
	nlab=nlab-1
	write (outline,600) totlab
600	format (' Total FITS keywords found       = ',i6)
	call xvmessage (outline,' ')
	write (outline,605) trlab
605	format (' Total FITS keywords to transfer = ',i6)
	call xvmessage (outline,' ')
	if (.not.ipass) then
		write (outline,610) passtart
610	format ('??W - Did not find indicated FITS keyword for PASS = ',a8)
		call xvmessage (outline,' ')
	endif
	return
	end
C============================================================================
	subroutine tablchk (data,bfctr,totlab,yend,tabbits,tabax,tabcols,
     1 tabrows,tabdepth,pcount,gcount,tfields,extname,extver,extlevel,
     2 ttype,tbcol,tform,ibfmt)
c
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800),card(80,360)
	integer*4 i,k,m,totlab
	integer*4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount
	integer*4 tfields,extver,extlevel,index,bfctr
	integer*4 tbcol(999)
	logical*4 yend
	character*1 blank1,lparen,rparen
	character*2 blank2
	character*3 blank3
	character*4 ibfmt(999)
	character*8 keyword(35)
	character*8 fkey(360),ttype(999)
	character*10 tform(999)
	character*16 extname
	character*80 cardimg,outline
c
	data blank1/' '/,blank2/'  '/,blank3/'   '/
	data lparen/'('/,rparen/')'/
c
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	DO 10510 m=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')
	totlab=totlab+1
	if (fkey(m).NE.KEYWORD(2)) GO TO 10110		!BITPIX 
	read (cardimg(27:30),20100) tabbits
20100 format (i4)
	go to 10510
10110 continue
	if (fkey(m).ne.KEYWORD(3)) go to 10120		!NAXIS -
	read (cardimg(27:30),20100) tabax
	go to 10510
10120 continue
	if (fkey(m).ne.KEYWORD(4)) go to 10130		!NAXIS1 -
	read (cardimg(27:30),20100) tabcols
	go to 10510
10130 continue
	if (fkey(m).ne.KEYWORD(5)) go to 10140		!NAXIS2 -
	read (cardimg(27:30),20100) tabrows
	go to 10510
10140 continue
	if (fkey(m).ne.KEYWORD(6)) go to 10150		!NAXIS3 -
	read (cardimg(27:30),20100) tabdepth
	go to 10510
10150 continue
	if (fkey(m).ne.KEYWORD(22)) go to 10160		!PCOUNT -
	read (cardimg(27:30),20100) pcount
	go to 10510
10160 continue
	if (fkey(m).ne.KEYWORD(21)) go to 10170		!GCOUNT - 
	read (cardimg(27:30),20100) gcount
	go to 10510
10170 continue
	if (fkey(m).ne.KEYWORD(26)) go to 10180		!TFIELDS -
	read (cardimg(27:30),20100) tfields
	go to 10510
10180 continue
	if (fkey(m).ne.KEYWORD(23)) go to 10190		!EXTNAME -
	read (cardimg(12:27),20180) extname
20180 format (a16)
	go to 10510
10190 continue
	if (fkey(m).ne.KEYWORD(24)) go to 10200		!EXTVER -
	read (cardimg(27:30),20100) extver
	go to 10510
10200 continue
	if (fkey(m).ne.KEYWORD(25)) go to 10210		!EXTLEVEL -
	read (cardimg(27:30),20100) extlevel
	go to 10510
10210 continue
	if (tfields.lt.0) go to 10510
	if (fkey(m)(1:5).ne.KEYWORD(31)(1:5)) go to 10300	!TTYPExxx -
	if (fkey(m)(6:8).ne.blank3) go to 10220
	read (cardimg(12:19),20210) ttype(1)
20210 format (a8)
	go to 10510
10220 continue
	if (fkey(m)(7:8).ne.blank2) go to 10230
	read (cardimg(6:6),20220) index
20220 format (i1)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
10230 continue
	if (fkey(m)(8:8).ne.blank1) go to 10240
	read (cardimg(6:7),20230) index
20230 format (i2)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
10240 continue
	read (cardimg(6:8),20240) index
20240 format (i3)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
c
10300 continue
	if (fkey(m)(1:5).ne.KEYWORD(32)(1:5)) go to 10400	!TBCOLxxx -
	if (fkey(m)(6:8).ne.blank3) go to 10320
	read (cardimg(27:30),20100) tbcol(1)
	go to 10510
10320 continue
	if (fkey(m)(7:8).ne.blank2) go to 10330
	read (cardimg(6:6),20220) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
10330 continue
	if (fkey(m)(8:8).ne.blank1) go to 10340
	read (cardimg(6:7),20230) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
10340 continue
	read (cardimg(6:8),20240) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
c
10400 continue
	if (fkey(m)(1:5).ne.KEYWORD(33)(1:5)) go to 10500	!TFORMxxx -
	if (fkey(m)(6:8).ne.blank3) go to 10420
	index=1
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10420 continue
	if (fkey(m)(7:8).ne.blank2) go to 10430
	read (cardimg(6:6),20220) index
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10430 continue
	if (fkey(m)(8:8).ne.blank1) go to 10440
	read (cardimg(6:7),20230) index
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10440 continue
	read (cardimg(6:8),20240) index
	read (cardimg(12:19),20210) tform(index)(2:9)
c
10445 continue
	tform(index)(1:1)=lparen
	tform(index)(10:10)=rparen
	do i=9,30
		if (cardimg(i:i).eq.'A') then
			ibfmt(index)=cardimg(i:i+3)
			go to 10510
		endif
		if (cardimg(i:i).eq.'D') then
			ibfmt(index)='DOUB'
			go to 10510
		endif
		if (cardimg(i:i).eq.'E') then
			ibfmt(index)='REAL'
			go to 10510
		endif
		if (cardimg(i:i).eq.'F') then
			ibfmt(index)='REAL'
			go to 10510
		endif
		if (cardimg(i:i).eq.'I') then
			ibfmt(index)='FULL'
			go to 10510
		endif
	enddo
	write (outline,21000) cardimg(1:30)
21000 format ('??E - Did not find proper format for ',a30)
	call xvmessage(outline,' ')
	call abend 
c
10500 continue
	if (fkey(m).ne.keyword(19)) go to 10510
	yend=.true.
	go to 10515
c
10510 continue	
c
10515 continue
	return
	end
c===========================================================================
	subroutine tablelist (ttype,tfields,tabrows,extname,
     1 extver,unit,bfctr,tabcols,catrec)
c
	implicit none
	include 'errdefs'
c
	byte data(28800),tabdata(28800)
	integer*4 tfields,tabrows,extver,i,ii,unit,stat,bfctr,fitsns
	integer*4 fitsptr,tabptr,tabcols,il,row,catrec,temprec
	logical*4 ineof
	character*8 ttype(999)
	character*16 extname
	character*100 outline
c
	write (outline,10100) extname,extver
10100 format ('Listing of FITS data set ',a16,' Version ',i4)
	call xvmessage (outline,' ')
c
	write (outline,10110) (ttype(ii), ii=1,tfields)
10110 format (10(a8,2x))
	call xvmessage (outline,' ')
c
	temprec=catrec
	fitsns=2880*bfctr
	fitsptr=fitsns+1
	tabptr=0
	row=0
10	continue
	if (fitsptr.lt.fitsns) go to 20
	catrec=catrec+1
	call xvread (unit,data,stat,'LINE',catrec,'NSAMPS',fitsns,' ')
        if (stat.eq.END_OF_FILE) then
                ineof=.true.
                return
        endif

	call chkstat (stat,'??E - XVRead error - tablelist',1,0,0)
	fitsptr=0
20	continue
	il=min(tabcols-tabptr,fitsns-fitsptr)
	do i=1,il
		tabdata(tabptr+i)=data(fitsptr+i)
	enddo
c
	tabptr=tabptr+il
	fitsptr=fitsptr+il
	if (tabptr.lt.tabcols) go to 10
	write (outline,10200) (tabdata(ii), ii=1,il)
10200 format (80a1)
c
	call xvmessage (outline,' ')
	tabptr=0
	row=row+1
	if (row.le.tabrows) go to 10
c
	write (outline,10210) row-1
10210 format (i5,' Rows written')
	call xvmessage (outline,' ')
	catrec=temprec
	return
	end
c==========================================================================
      subroutine keyinc2(key)
C
C!	KEYINC2 -- SUBROUTINE TO INCREMENT CHARACTER*5 STRING KEY
C!		ALLOWS UP TO 999 INCREMENTS
C
C!	SEE KEYINC FOR CHARACTER*5 STRING TO ALLOW UP TO 99 INCREMENTS
C 
C!	PASSED VARIABLES:
C!
C! KEY   -- CHARACTER*5 STRING CONTAINING VICAR LABEL KEY
C!		SHOULD BE OF FORMAT AAXXX WHERE "A" IS ASCII ALPHABETIC
C!		AND "X" IS ASCII NUMERIC.
C!		FOR COMPATIBILITY WITH OLDER VICAR SUBROUTINE KEYINC 
C!		KEYINC2 WILL ACCEPT FORMAT AABXX WHERE BOTH "A" AND "B"
C!		ARE ALPHABETIC. WHEN IT DETECTS "B" ON AN INCREMENT INTO
C!		THE HUNDREDS DIGIT IT WILL CHANGE "B" TO A "1"  
C!
C!	LOCAL VARIABLES:
C
C! J,K,L -- ONE'S, TEN'S AND HUNDREDS DIGITS OF ASCII KEY
c
	implicit none
c
	byte j,k,l,zero,nine
	character*5 KEY
	character*80 outline

        data zero /48/,nine /57/		!zero ='0' (48)  nine='9' (57)
C
C
C!-- FOLLOWING PUT IN TO KEEP COMPATIBLE WITH OLDER ROUTINE
C!-- RESETS HUNDREDS DIGIT TO A NUMBER
C
	L=ichar(KEY(3:3))
	IF (L.LT.zero) L=zero
	IF (L.GT.nine) L=zero

	J = ichar(KEY(5:5))
	IF (J.LT.zero) then
		write (outline,10100) 
10100 format (' ??E - KEYINC2 - Final character of key is not a digit')
		call xvmessage (outline,' ')
		RETURN
	endif
	if (j.gt.nine) then
		write (outline,10100) 
		call xvmessage (outline,' ')
		RETURN
	endif
	j=j+1
C
C!-- UPDATE KEY NAME... J INDICATES ONE'S DIGIT, K THE TEN'S DIGIT
C!-- L THE HUNDREDS DIGIT
C
      IF (J .GT. nine) then
	J = zero
	K = ichar(KEY(4:4)) + 1
		IF (K.GT.nine) then
		K=zero
		L=L+1
			IF (L.GT.nine) then
			write (outline,10200) 
10200 format (' ??E - KEYINC2 - Key exceeds xx999')
			call xvmessage (outline,' ')
			return
			endif
		KEY(3:3) = char(L)
		endif
	KEY(4:4) = char(K)
      endif
      KEY(5:5) = char(J)
      return
      end
C============================================================================
	BLOCK DATA
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	DATA KEYWORD(1)  /'SIMPLE  '/		!LOGICAL
	DATA KEYWORD(2)  /'BITPIX  '/		!INTEGER
	DATA KEYWORD(3)  /'NAXIS   '/		!INTEGER
	DATA KEYWORD(4)  /'NAXIS1  '/		!INTEGER
	DATA KEYWORD(5)  /'NAXIS2  '/		!INTEGER
	DATA KEYWORD(6)  /'NAXIS3  '/		!INTEGER
	DATA KEYWORD(7)  /'BSCALE  '/		!FLOATING
	DATA KEYWORD(8)  /'BZERO   '/		!FLOATING
	DATA KEYWORD(9)  /'BUNIT   '/		!CHARACTER
	DATA KEYWORD(10) /'BLANK   '/		!INTEGER
	DATA KEYWORD(11) /'COMMENT '/		!CHARACTER
	DATA KEYWORD(12) /'HISTORY '/		!CHARACTER
	DATA KEYWORD(13) /'BLOCKED '/		!LOGICAL
	DATA KEYWORD(14) /'EXTEND  '/		!LOGICAL
	DATA KEYWORD(15) /'XTENSION'/		!CHARACTER
	DATA KEYWORD(16) /'DATAMAX '/		!FLOATING
	DATA KEYWORD(17) /'DATAMIN '/		!FLOATING
	DATA KEYWORD(18) /'        '/		!BLANKS ARE COMMENTS
	DATA KEYWORD(19) /'END     '/
	DATA KEYWORD(20) /'GROUPS  '/		!LOGICAL
	DATA KEYWORD(21) /'GCOUNT  '/		!INTEGER
	DATA KEYWORD(22) /'PCOUNT  '/		!INTEGER
	DATA KEYWORD(23) /'EXTNAME '/		!CHARACTER
	DATA KEYWORD(24) /'EXTVER  '/		!INTEGER
	DATA KEYWORD(25) /'EXTLEVEL'/		!INTEGER
	DATA KEYWORD(26) /'TFIELDS '/		!INTEGER
	DATA KEYWORD(27) /'TABNAME '/		!CHARACTER
	DATA KEYWORD(28) /'OBJECT  '/		!CHARACTER
	DATA KEYWORD(29) /'DATE    '/		!CHARACTER
	DATA KEYWORD(30) /'HIERARCH'/		!LOGICAL
	DATA KEYWORD(31) /'TTYPE   '/		!CHARACTER
	DATA KEYWORD(32) /'TBCOL   '/		!INTEGER
	DATA KEYWORD(33) /'TFORM   '/		!CHARACTER
	DATA KEYWORD(34) /'ORIGIN  '/		!CHARACTER
	DATA KEYWORD(35) /'        '/
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fitsin.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fitsin

   To Create the build file give the command:

		$ vimake fitsin			(VMS)
   or
		% vimake fitsin			(Unix)


************************************************************************/


#define PROGRAM	fitsin
#define R2LIB

#define MODULE_LIST fitsin.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport errdefs

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fitsin.pdf
process help=*
PARM INP    TYPE=STRING
PARM OUT    TYPE=STRING     COUNT=(0:10) DEFAULT=--
PARM HEADER TYPE=STRING     COUNT=(0:10) DEFAULT=--
PARM DATA   TYPE=STRING     COUNT=(0:1) VALID=(TRUE,FITS,U16) DEFAULT=FITS
PARM BLOCK  TYPE=STRING     COUNT=(0:1) VALID=(BLOCK,NOBLOCK) DEFAULT=NOBLOCK
PARM FACTOR TYPE=INTEGER    COUNT=(0:1) DEFAULT=1
PARM PASS   TYPE=STRING     COUNT=(0:1) DEFAULT=--
PARM EXCISE TYPE=STRING     COUNT=(0:10) DEFAULT=--
PARM COMMENT TYPE=STRING    COUNT=(0:1) VALID=(COMMENT,NOCOMMENT) 	DEFAULT=COMMENT
PARM HISTORY TYPE=STRING    COUNT=(0:1) VALID=(HISTORY,NOHISTORY) DEFAULT=HISTORY
PARM NULL   TYPE=STRING     COUNT=(0:1) VALID=(NULL,NONULL) DEFAULT=NULL
PARM TABLIST TYPE=STRING    COUNT=(0:1) VALID=(LIST,NOLIST) DEFAULT=NOLIST
END-PROC
.TITLE
FITSIN  --  log FITS format data.
.HELP
PURPOSE:
       
   FITSIN is a VICAR*2 Applications program to convert FITS formatted
Astronomical data into VICAR*2 formatted image files. It is based on 
documentation by Don Wells of NRAO.

OPERATION:

	Flexible Image Transport System (FITS) tapes are a general interchange
data format used by radio and optical astronomy observatories for passing
multidimensional data.  In the original definition the tape format consisted
of fixed length records of 2880 bytes with at least one header block of ASCII
information that preceeds the binary (data) information. The fixed length
record of 2880 bytes was chosen because it was divisible by the word lengths
of the most common computer systems of the late 1970's. However, since the
packing density of 1/2-inch tape was pretty poor when newer tape drives
became available in the 1980's the standard was revised to allow up to 10
logical 2880 byte records to be packed into up to 28800 bytes in a physical
block. The FITS keyword BLOCKED = T was added to denote that perhaps the tape
was blocked at greater than 2880 bytes. Note that it does not demand that
the tape be blocked only that it might be. 

	The data records from the instrument are packed into these
fixed length records after the FITS header.

	FITSIN reads the FITS header block of 2880 bytes and extracts
information from it. The entire header block is printed out on the log. All
or portions of the FITS ASCII header can be passed to the VICAR history
label. They will be stored as comments inside the VICAR label using the
keyword "VFnnn=" where nnn are three digits starting with 001. A maximum
of 999 label entries are allowed.

	FITS tapes may or may not contain ANSI standard labels.  If you 
process an ANSI FITS tape as an unlabeled tape then the real FITS images
files numbered 1, 2, 3, 4,... will be file numbers 2, 5, 8, 11,... on the
tape. File numbers 1, 3, 4, 6, 7, etc will be ANSI header records.


FITS HEADER 

	Each header block consists of 36 80-byte records (card images).
The first 10 columns of each card image contains an 8-character keyword
followed by a blank and an equals sign. Columns 11 through 30 contain a
value or string in ASCII characters which is right-justified. Columns 31
through 80 are a comment field. The first character of a comment field
(column 31) is a slash, /.

	The first FITS header record at a minimum consists of the following
information:

        '         111111111122222222223'  <--card column number
        '123456789012345678901234567890'
        'SIMPLE   =               T'            !LOGICAL
        'BITPIX   =              16'            !INTEGER
        'NAXIS    =               2'            !INTEGER
        'NAXIS1   =             512'            !INTEGER
        'NAXIS2   =             512'            !INTEGER
        'END                       '

	The first card contains "SIMPLE    =" in columns 1 through 10  
followed by a logical "T" or "F" in column 30. "T" indicates that the
tape format follows the basic rules of FITS format. "F" implies that
it does not.
	The second card contains "BITPIX    =" which gives the number of bits
per pixel in each data record.  The valid values are 8, 16, 32, -32 and -64.
All axes of data must have the same number of bits per pixel. Note that
integer data types 8, 16 and 32 are byte-reversed from the VAX and that 
BITPIX=-32 is IEEE floating point format, and -64 is IEEE double precision
floating point formate, not VAX floating and double, respectively.
	The third card contains "NAXIS     =" which gives the number of
dimensions (axes) of the data.  FITSIN currently only processes three
dimensions or less.  When a sample tape of four or more dimensions is
received then it should be possible to convert the data to IBIS tabular
files although the current program does not do this.
	The fourth and subsequent cards (up to the number of dimensions)
contain "NAXIS1    =", "NAXIS2     =", "NAXIS3    =",...,"NAXISn    ="
which define the number of pixels along each dimension, n.
	Any number of optional header cards may follow.
	The last card is always the "END" card.

	FITS allows many optional header cards.  These optional header cards
vary from one sensor to another and one observatory to another. Some of 
the more important ones, as far as FITSIN is concerned, are BZERO, BSCALE,
BUNIT, OBJECT, DATE, DATAMAX, DATAMIN, COMMENT, HISTORY, and BLANK. For
example, the following FITS keywords are never passed to the VICAR label:
	SIMPLE
	BITPIX
	NAXIS
	NAXISm	(where m is an integer)
	END
	
The following FITS keywords are always passed to the VICAR label if found:
	BZERO
	BSCALE
	BUNIT
	OBJECT
	DATE
	EXTEND
	XTENSION 
	TABNAME 
	BLANK
	DATAMAX 
	DATAMIN 
	EXTNAME 
	EXTVER 
	EXTLEVEL 
	TFIELDS 

An example of a more complete FITS header is one from the 200 inch
telescope at Palomar.

SIMPLE  =                    T /                                                
BITPIX  =                   16 /                                                
NAXIS   =                    2 /                                                
NAXIS1  =                  800 /                                                
NAXIS2  =                   60 /                                                
BSCALE  =         8.175754E-01 /                                                
BZERO   =         2.538707E+04 /                                                
OBJECT  = 'L637              ' /                                                
FRAME   =                  233 /                                                
NIGHT   =                    2 /                                                
DATE    = '27-APR-1989       ' /                                                
TELESCOP=                  200 /                                                
PORT    =                    1 /                                                
DEWAR   =                    8 /                                                
CHIP    = 'TI432             ' /                                                
ETIME   =                  300 /                                                
TIME    =                  300 /                                                
DECS    = '+                 ' /                                                
EPOCH   =                 1950 /                                                
HAS     = '-                 ' /                                                
SECZ    =         1.109078E+00 /                                                
TEMP    =        -1.147000E+02 /                                                
ERASE   =         1.228969E+03 /                                                
FILTER  = 'NONE              ' /                                                
SATURATE=                    0 /                                                
GRATING =                  600 /                                                
SLIT    = '2.0S              ' /                                                
CRVAL1  =  5.9551000976563E+03 / Angstroms                                      
CDELT1  =  1.1090113863032E+00 /                                                
CRPIX1  =         1.000000E+00 /                                                
CTYPE1  = 'WAVELENGTH        ' /                                                
CRVAL2  =  1.0000000000000E+00 /                                                
CDELT2  =  1.0000000000000E+00 /                                                
CRPIX2  =         1.000000E+00 /                                                
CTYPE2  = 'COLUMN #          ' /                                                
END                                                                             

CONVERSION OF FITS HEADERS INTO VICAR LABELS

	FITS header records are converted into VICAR label items using
the VFxxx keyword. Up to 999 labels can be transferred.  For example,
FITS keyword   

CRVAL1  =  5.9551000976563E+03 / Angstroms                                      

will transfer as

VF023='CRVAL1  =  5.9551000976563E+03 / Angstroms                   '


Many of the FITS labels have strings in them. By convention a string
begins with a single quote (') and ends with another quote.  Transfer
of single quotes to VICAR labels cause problems so single quotes are
converted into double quotes  into VICAR labels.  For example,
FITS keyword

OBJECT  = 'L637              ' /                                                

This would be converted into the VICAR label item

VF003='OBJECT  = "L637              " /                                '       



FITS DATA

	Data on fits tape are packed into 2880 byte fixed length records.
The label items BITPIX, NAXIS, NAXIS1, NAXIS2, ..., NAXISn describe how
the data is packed. BITPIX which can only take on the values 8, 16, 32,
-32 and -64 tell how many pits there are per pixel.  NAXIS tells how many
dimensions the data has. FITSIN in its current implementation can
process only values of 1, 2 and 3. NAXIS1, NAXIS2 and NAXIS3 give
the number of pixels per axis (sample, line, band) in VICAR nomenclature.
It is common to use BITPIX of 8 and NAXIS=0 for FITS tabular data.

PARAMETERS

	The OUT=filename parameter is not required.  If not given then
only the header of the INP= file is scanned and printed out to the terminal
and/or session log.  The scan will also state the number of FITS keywords
found and the number. Up to 10 output files can be given for multiple
FITS data sets per FITS file.

	The HEADER= parameter is not required. It saves each FITS label 
in a file which has 80 byte fixed length records. No FITS labels are 
created or omitted in this file and no other parameter controls the
contents. The HEADER file does not have VICAR labels.  Up to 10 HEADER
files are allowed for multiple data sets per FITS file.

	The parameter DATA="FITS"/"TRUE"/"U16" refers to the data format to
pass to the VICAR image. The default is FITS format which means that 
it will pass to the output image the same data format that is stored in the FITS
image. If the FITS data is 8 bit then the output will be VICAR format
'BYTE', if 16 bit then the output will be VICAR format 'HALF', etc.
In this mode it will thus ignore any BSCALE and BZERO FITS labels if
found in the FITS label. Consequently, if the original experimenter
normalized the data using BSCALE and BZERO to pack the data in 8 or 16 bit
format the VICAR image will retain this packing.  For some applications
this is entirely acceptable, for others it is not.

	If DATA="TRUE" then FITSIN will transform the FITS data into a VICAR
'REAL'format data set regardless of the FITS data values. It will then use the
FITS BSCALE and BZERO values stored in the FITS label to convert the data
(whatever the format, 8 bit, 16 bit or 32 bit integers or 32-bit IEEE
FITS floating point formats) into VAX 'REAL' format by the formula

			REAL = (FITS*BSCALE)+BZERO
where,

REAL is the R*4 output value in the VICAR image,
FITS is the FITS stored data value in 8 bit, 16 bit, 32 bit or -32 bit format,
BSCALE is the value stored in the FITS BSCALE header record,
BZERO is the value stored in the FITS BZERO header record.
					
If no BSCALE or BZERO values are found in the label when you give this option
then FITSIN will warn you and then use default values of BSCALE=1.0 and
BZERO=0.0 to do the conversion. Thus, use of the "DATA=TRUE" option will
force the output to be VICAR 'REAL' no matter what format the FITS data is in.

	DATA="U16"
	FITS data assumes that 8-bit is unsigned and 16-bit is signed. VICAR 
uses the same convention. Consequently, there can be problems for displaying 
in VICAR 16-bit FITS data converted to VICAR HALF format by FITSIN.
It will show up as -32768 to +32767 in VICAR. To get around this you can
force 16-bit data into a 32-bit integer (VICAR FULL format). Use DATA="U16" 
to give properly scaled data from 0 to 65535. The U16  parameter
will use BSCALE and BZERO if given in the FITS header.

	The BLOCK= parameter is only for reading of blocked FITS tapes. Valid entries
are "BLOCK" or "NOBLOCK".  If "BLOCK" is entered then enter a value of from one
to 10 for "FACTOR".  

Other parameters are geared toward limiting the FITS header keywords from
being passed to the VICAR label since only 999 are allowed. The options for
selecting which FITS labels to pass to the VICAR label are the following:

	PASS="FITS KEYWORD" - Indicates at which FITS keyword to begin passing
	the FITS label records to VICAR label records.  By default, FITS 
	keywords starting after FITS keyword "NAXIS3 =" are passed or
	"NAXIS =" if NAXIS = 0 (in the case of tables).
	
	EXCISE="FITS KEYWORD" - Indicates which FITS keywords not to pass to
	the VICAR label records.  Up to 10 keywords may be EXCISEd. By default
	no FITS keywords are excised.
	
	COMMENT="COMMENT"/"NOCOMMENT" - Pass/Don't Pass FITS "COMMENT =" records
	to the VICAR label.  By default, FITS "COMMENT =" keywords are passed.
	
	HISTORY="HISTORY"/"NOHISTORY" - Pass/Don't Pass FITS "HISTORY =" records
	to the VICAR label.  By default, FITS "HISTORY =" keywords are passed.
	
	NULL="NULL"/"NONULL" - Pass/Don't Pass FITS blank (null) records to
	the VICAR label.  By default, null keyword records are passed.

Note that by specifying PASS and EXCISE keywords along with COMMENT="NOCOMMENT",
HISTORY="NOHISTORY", and/or NULL="NONULL" that FITS "COMMENT=", "HISTORY=" and
null records will not be passed to the VICAR label. This technique is normally
used after a tape scan which detects pages of FITS label information which is
of little use to the VICAR user.

	TABLIST="LIST"/"NOLIST" - List FITS tables to screen


FITS files can transfer text data. This is usually done by the following:
SIMPLE  =                    T / Standard FITS format
BITPIX  =                    8 / Character Information
NAXIS   =                    0 / No image data array present
EXTEND  =                    T / Extension exists
 .
 .
 .
END
XTENSION= 'TABLE   '           / Table Extension
BITPIX  =                    8 / Character Information
NAXIS   =                    2 / Two-dimensional table
NAXIS1  =                   80 / Number of characters per line
NAXIS2  =                  560 / Number of rows
PCOUNT  =                    0 / No Random Parameters
GCOUNT  =                    1 / Only one group
TFIELDS =                    1 / One field per row

EXTNAME = 'REV_1_1 '           / Generic Comments
EXTVER  =                    1 / Integer Version Number

TTYPE1  = 'TEXT    '           / Free text
TBCOL1  =                    1 / Start in column 1
TFORM1  = 'A80     '           / 80 Character Field

END

You can list such text files to screen by using the TABLIST=LIST option

Suggestions for future:
    1 remove PASS,  EXCISE, HISTORY and COMMENT parameters. 
      They were only used on old VAX code that had limitations
      on the number of labels.
    2 Use fitsio for data interpretation. Leave ibis tables though

LIMITATIONS

	1. The program has not been implemented for multi-dimensional files
	greater than 3 dimensions. (NAXIS < 4)
	2. Up to 999 FITS keywords can be passed to the VICAR label.
	3. FITS "Blocked" tape formats are not supported although it will copy
	data as best it can.
	4. Changes (single quote) in FITS labels to (double quotes)
	in VICAR labels
	5. Cannot process FITS 'BINTABLE' or '3DTABLE' files
	6. Allows up to 10 FITS data sets to be embedded in one FITS file
	7. Program has been run on a variety of FITS tapes as well as disk
	data sets but none were in BLOCKED format.
	8. Need yet to create a TEXT output file

PROGRAM HISTORY:
    03 Aug 2014...R.J.Bambery...Reconciled differences between mipl version
                                and Cartographic Lab. This in response to
                                email by Walt Bunch to reconcile histories
                                of the 2 versions. i.e., add the following changes
                                12 MAY 2003   LWKamp   ..Added check for incomplete last block in FITS file;
                                   fixed chkstat calls (now require all arguments)
                                14 NOV 2002...A.C.Chen   ...Converted FITS keywords from "PASS" and 
                                   "EXCISE" parameters to upper case so FITS
                                   label keywords can be passed or excluded
                                   correctly. Modified test pdf.   
    14 Dec 2012...R.J.Bambery...Fixed bug which gave 62 bands for 1 band
                                in BITPIX=-32 Matlab created file
    28 Jun 2012...R.J.Bambery...Removed <tab> in front of continuation
                                lines to make backward compatible with
                                32-bit Linux gfortran 4.2.1, otherwise
                                compatible 64-bit Linux gfortran 4.6.3 
    07 Jun 2012...R.J.Bambery...Initialize variables in hdrproc that caused premature
                                END_OF_FILE with gfortran 4.6.3
    04 May 2011...R.J.Bambery...Fix abort on test script due to character*1 vs byte
                                conflicts -- worked on gcc 3.4 compilers
                                Fixed some logic on TABLE files to IBIS
    02 May 2011...R.J.Bambery...fix warning messages from gcc 4.4 compiler
    12 Sep 2010...R.J.Bambery...Fixed numerous errors due to HOST, reworked parameter
                                passings, and other internals
    12 Aug 2010...R.J.Bambery...Fixed errors (warning messages on Mac/Intel and Linux)
                                By changeing a number of variable names
                                No longer have access to Mac/PowerPC
    25 Mar 2010...R.J.Bambery...Somebody had changed the definitions of true = 'T'
                                and false = 'F' to 124 and 70 (ascii equivalents)
                                for some compiler, but it doesnt work under Linux
                                or MacOSX compilers. See subroutines valhdr,
                                fitschk and fitskey 
    30 Jan 2010...R.J.Bambery...Made compatible with 64-bit afids Build 793
                                Linux, MacOSX (both Intel/PowerPC)
    28 Aug 2008...R.J.Bambery...add proper number of arguments (5) to chkstat
                                adjust arguments for xvpcnt to 2
    24 Feb 2008...R.J.Bambery...fixes for linux g77
                                g77 doesnt like if (k.eq.'0') or k='0' statements
                                define   byte zero /'0'/
	23 Jun 2004...R.J.Bambery...put END_OF_FILE checks on xvreads
     5 Feb 2004...R.J.Bambery...Update comments, 16-bit messages to screen.
	22 Jul 2003...R.J.Bambery... Fixed BZERO,BSCALE in DATA=U16.
     2 MAR 1995...R.J.Bambery...Fixed text file printing to screen
							(was truncated to 45 characters)
	 9 NOV 1994...R.J.Bambery...Delivered to Gloria Conner
	14 AUG 1994...R.J.Bambery...Added U16 parameter to pass unsigned 16-bit
				    data to VICAR fullword images
	11 AUG 1994...R.J.Bambery...Added ability to process FITS -64 data
	23 MAR 1994...R.J.Bambery...Added ability to process up to 10
				    FITS data sets per FITS file
	18 MAR 1994...R.J.Bambery...Fixed a "PASS=" parameter bug
				    Changed default NULL=NULL from
				    NULL=NONULL
	 4 DEC 1993...R.J.Bambery...Incorporation of IBIS-2 tabular
				    file routines
	26 JUN 1993...R.J.Bambery...Allow for listing of FITS table files
	23 JUN 1993...R.J.Bambery...Fixed bugs in FITSCHK (nproc to noproc)
	10 MAY 1993...R.J.Bambery...Made portable to UNIX
				    Removed IEEE specific floating pt code
				    because of new VICAR executive
	10 APR 1991...R.J.BAMBERY...Added blocking factor and bands
	18 MAR 1991...R.J.BAMBERY...Fixed bugs and prevent ASCII dump of
					entire file when invalid header found
	20 FEB 1991...R.J.BAMBERY...ADDED IEEE FLOATING POINT CODE
	28 SEP 1990...R.J.BAMBERY...ADDED HEADER OPTION
	17 SEP 1990...R.J.BAMBERY...EXPANDED FITS LABELS TO 999 FROM 99
    25 AUG 1990...R.J.BAMBERY...EXPANDED OPTIONS,
				    MULTIPLE FITS HEADER RECORDS
	30 JUN 1987...G.W.GARNEAU...Updated and expanded
	12 DEC 1984...M.E.MORRILL...VAX/VICAR-2 Conversion
	17 APR 1982...M.E.MORRILL...IBM Version, initial release.

EXAMPLES:

       FITSIN INP=FITS.DAT  PARMS   (for scanning headers)
                   --or--
       FITSIN INP=FITS.DAT  OUT=IMAGE.DAT PARAMS (for converting data)

       FITSIN INP=FITS.DAT  HEADER=FITS.HDR (for saving FITS labels)
		  --or--
       FITSIN INP=FITS.DAT  OUT=IMAGE.DAT HEADER=FITS.HDR PARAMS (for
				 saving FITS labels and converting data)
      
Parameters are defined above and in the TUTOR mode. SIZE field is computed
from FITS information.

REFERENCE

http://fits.gfsc.nasa.gov/fits_intro.html

Donald C. Wells, "FITS: A Flexible Image Transport System", National Optical
Astronomy Observatories (NOAO) publication, 1979

Flexible Image Transport System (FITS) Draft Standard, NSDSSO 100-2.0,
Mar 29, 1999. Obtainable from web page above.

A User's Guide for the Flexible Image Transport System (FITS), Version 4.0,
Apr 14, 1997. Obtainable from web page above.

Help on FITS is available through NASA/Office of Standards and Technology,
Greenbelt MD.
anonymous ftp: nssdca.gsfc.nasa.gov
WWW: http://fits.cv.nrao.edu

Usenet newsgroup: sci.astro.fits

.LEVEL1
.VARIABLE INP
 STRING
 A FITS tape file number or
 disk file.
.VARIABLE OUT
 STRING--OPTIONAL
 A Vicar formated output
 image filename.
 (Up to 10 embedded data
 sets can be converted 
 into separate VICAR files)
.VARIABLE HEADER
 STRING-OPTIONAL
 A file name for outputting
 FITS labels
 (Up to 10 embedded data
 sets FITS labels can be
 listed into separate
 VICAR files)
.VARIABLE DATA
 STRING-OPTIONAL
 Data format to pass to VICAR
 image
 FITS/TRUE/U16
 DEFAULT="FITS"
.VARIABLE BLOCK
 STRING-OPTIONAL
 FITS image is blocked/not
 blocked,
 (NOT supported yet)
 BLOCK/NOBLOCK
 DEFAULT=BLOCK
.VARIABLE FACTOR
 INTEGER-OPTIONAL
 Blocking factor when 
 BLOCK=BLOCK is selected.
 Must be integer.
 DEFAULT=1
.VARIABLE PASS
 STRING-OPTIONAL
 Pass to VICAR history label,
 FITS label records beginning
 with PASS="FITS KEYWORD".
 DEFAULT=-- (All FITS labels)
.VARIABLE EXCISE
 STRING-OPTIONAL
 Do not pass to VICAR history
 label FITS label records
 beginning with
 EXCISE="FITS KEYWORD".
 DEFAULT="        " (null)
.VARIABLE COMMENT
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "COMMENT =" labels
 COMMENT/NOCOMMENT
 DEFAULT=COMMENT
.VARIABLE HISTORY
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "HISTORY =" labels
 HISTORY/NOHISTORY
 DEFAULT=HISTORY
.VARIABLE NULL
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "           " (null) labels
 NULL/NONULL
 DEFAULT=NONULL
.VARIABLE TABLIST
 STRING-OPTIONAL
 List/Nolist of FITS tables
 to screen
 DEFAULT=NOLIST
.LEVEL2
.VARIABLE INP
 A FITS formatted data file on tape or disk.
.VARIABLE OUT
 A Vicar formatted output image filename. Output can be 'BYTE', 'HALF', 'FULL'
 or 'REAL' depending on FITS format or the FITSIN parameter "DATA="
.VARIABLE HEADER
 An output file containing FITS label information in ASCII 80 byte fixed
 length records.
.VARIABLE DATA
 STRING-OPTIONAL
 Data format to pass to VICAR image. Default is "FITS" tape format which
 can be 'BYTE', 'HALF', 'FULL'  or 'REAL'.
 
 If "TRUE" then will pass R*4 format data to VICAR using FITS parameters
 BSCALE and BZERO keywords in FITS labels based on the formula 

	"TRUE" = (FITS*BSCALE)+BZERO.
 
 If DATA is set to "TRUE" and no BSCALE or BZERO values are not found then
 the program will warn you, but will create an output data set of type
 real*4 by using the default settings of BSCALE=1.0 and BZERO=0.0.
 
 When DATA=TRUE output file will always be 'REAL'.
 Use U16 to transform unsigned FITS data into unsigned VICAR FULL data since
 VICAR uses 16-bit signed data.
 FITS/TRUE/U16
 DEFAULT="FITS"
.VARIABLE BLOCK
 STRING-OPTIONAL
 FITS image tape is blocked/not blocked,
 BLOCK/NOBLOCK
 DEFAULT=BLOCK
.VARIABLE FACTOR
 INTEGER-OPTIONAL
 Blocking factor when BLOCK=BLOCK is selected. Maximum permitted value is 10.
 Must be integer. 
 Find Blocking factor from MIPL TAPES utility.
 DEFAULT=1
.VARIABLE PASS
 STRING-OPTIONAL
 Pass to VICAR history label the FITS label records beginning
 with PASS="FITS KEYWORD". Normally this is done after a tape scan.
 
 "BSCALE =", "BZERO =", "BUNIT =", "OBJECT =", "COMMENT =" and "DATE ="
 keywords will always be passed to the VICAR label even if found before the
 FITS keyword you choose for PASS=.
 
 DEFAULT=-- (All FITS labels)
.VARIABLE EXCISE
 STRING-OPTIONAL
 Don't pass to VICAR history label the FITS label records beginning
 with EXCISE="FITS KEYWORD". This gives you the option of eliminating other
 less helpful FITS  keywords. Normally this is done after a tape scan.
 Up to 10 keywords may be excised in addition to those specified with the
 COMMENT, HISTORY, and NULL keywords.
.VARIABLE COMMENT
 STRING-OPTIONAL
 Pass/Don't Pass FITS "COMMENT =" labels
 COMMENT/NOCOMMENT
 DEFAULT=COMMENT
.VARIABLE HISTORY
 STRING-OPTIONAL
 Pass/Don't Pass FITS "HISTORY =" labels
 HISTORY/NOHISTORY
 DEFAULT=HISTORY
.VARIABLE NULL
 STRING-OPTIONAL
 Pass/Don't Pass FITS "           "(null) labels
 NULL/NONULL
 DEFAULT=NONULL
.VARIABLE TABLIST
 STRING-OPTIONAL
 List/Nolist of FITS tables to screen
 DEFAULT=NOLIST
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfitsin.pdf
procedure

local   afidsroot   type=string count=1

refgbl $echo
refgbl $syschar
! Dec 14, 2012 - RJB
! TEST SCRIPT FOR FITSIN 
! tests BYTE, HALF, FULL, REAL images
!   and ibis TABULAR files
!
! Vicar Programs:
!       label-list list ibis-list
! 
! parameters:
!   <none>
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
body
enable-log
let _onfail="goto rm"
let $echo=("yes")
! New exhaustive test of FITSIN - Nov 8, 1994
!
! Uses files l637b.fits,p1.fits,pcf230w_a.fits
! 		1980ly1.fits,iras_12b1.fits
!		af784.fits,neat668.fits,0001.gsc
!       psf2x_l10_filter5.fits
!
! does not test parameters "BLOCK" or "FACTOR" since data not on tape
! (They have no meaning for disk files)
!

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot

if (afidsroot = "")
!MIPL        
    ush ln -s /project/test_work/testdata/gll tt
    ush ln -s /project/test_work/testdata/neat nn
    ush ln -s /project/test_work/testdata/carto ct
else         
!CARTLAB     
    ush ln -s /raid1/vicar_test_images/testdata/gll tt
    ush ln -s /raid1/vicar_test_images/testdata/neat nn
    ush ln -s /raid1/vicar_test_images/testdata/carto ct
end-if       
    
! --> test listing of fits header only
fitsin tt/1980ly1.fits

! --> translate 16-bit data to HALF vicar
fitsin tt/1980ly1.fits 1980ly1.vic
label-list 1980ly1.vic

list 1980ly1.vic sl=1 ss=1 nl=10 ns=10
! --> translate 16-bit data to HALF vicar
fitsin tt/1980ly1.fits 1980ly1.vic
label-list 1980ly1.vic
list 1980ly1.vic sl=1 ss=1 nl=10 ns=10

! --> translate 16-bit data to HALF vicar with embedded BSCALE
fitsin tt/l637.fits l637.vic
label-list l637.vic
list l637.vic sl=1 ss=1 nl=10 ns=10

! --> pass label information from telescope onward
fitsin tt/l637.fits l637.vic pass="telescop"
label-list l637.vic

! --> do not pass following label entries
fitsin tt/l637.fits l637.vic excise=("telescop","chip","has","secz","port",+
	"dewar","bscale","bzero")
label-list l637.vic

! --> translate BITPIX=16 with BSCALE parameter to REAL vicar
fitsin tt/l637.fits l637.vic data="true"
label-list l637.vic
list l637.vic sl=1 ss=1 nl=10 ns=10

! --> create header file only
fitsin tt/l637.fits header=l637.hdr
ush cat l637.hdr

! --> translate file and create header file
fitsin tt/l637.fits l637.vic header=l637.hdr
ush cat l637.hdr

list l637.vic sl=1 ss=1 nl=10 ns=10

! --> do not pass history= and comment= labels
fitsin tt/l637.fits l637.vic history=nohistory comment=nocomment 
label-list l637.vic

! --> list header of a file with a very long fits header
fitsin tt/pcf230w_a.fits

! --> create fits header file of a FITS image with a long fits header
fitsin tt/pcf230w_a.fits header=pcf230w_a.hdr
ush cat pcf230w_a.hdr

! --> translate BITPIX=16 fits file with a long header -- gets slower
fitsin tt/pcf230w_a.fits pcf230w_a.vic
label-list pcf230w_a.vic 
list pcf230w_a.vic sl=1 ss=1 nl=10 ns=10

! --> list FITS header for BITPIX=-32
fitsin tt/p1.fits

! --> create FITS header file for BITPIX=-32 data
fitsin tt/p1.fits header=p1.hdr
ush cat p1.hdr

! --> translate BITPIX=-32 (IEEE real) to REAL vicar
fitsin tt/p1.fits p1.vic 
label-list p1.vic
list p1.vic sl=1 ss=1 nl=6 ns=6
! test one which formerly gave 62 bands
fitsin ct/psf2x_l10_filter5.fits psf.vic
label-li psf.vic
list psf.vic sl=1 ss=1 nl=6 ns=6
! --> translate BITPIX=-64 (IEEE double) to DOUB vicar
fitsin tt/af784.fits af784.vic 
label-li af784.vic
list af784.vic sl=1 ss=1 nl=5 ns=5

! --> translate BITPIX=16 to HALF vicar
fitsin tt/iras_12b1.fits iras_12b1.vic
label-li iras_12b1.vic
list iras_12b1.vic sl=293 ss=480 nl=6 ns=6

! --> translate BITPIX=16 to HALF vicar
fitsin tt/iras_12b1.fits iras_12b1.vic data=true
label-li iras_12b1.vic
list iras_12b1.vic sl=293 ss=480 nl=6 ns=6

! --> translate BITPIX=16 to FULL vicar since data is unsigned 16-bit data
fitsin tt/neat668.fits neat668.vic data=u16
label-li neat668.vic 
list neat668.vic sl=100 ss=100 nl=6 ns=6

! --> create ibis-2 tabular file from FITS table 
fitsin nn/0001.gsc 0001.ibis
label-li 0001.ibis
ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats +
	screen=120

! --> create ibis-2 tabular file from FITS table with listing 
fitsin nn/0001.gsc 0001.ibis tablist=list 
label-li 0001.ibis
ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats +
	screen=120


rm>
ush rm -f tt 
ush rm -f nn
ush rm -f ct
let $echo="no"
disable-log
end-proc
$!-----------------------------------------------------------------------------
$ create tstfitsin.log
translog INP=AFIDS_ROOT TRANS=afidsroot
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/gll tt
    ush ln -s /project/test_work/testdata/neat nn
    ush ln -s /project/test_work/testdata/carto ct
else
end-if
fitsin tt/1980ly1.fits
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    2
NAXIS1  =                  516
NAXIS2  =                  516
OBJECT  = '1980ly-1' / TMO FILE NAME
DATE-OBS= '23-08-93'
TIME    = '08:10'              / UT TIME OF FILE CREATION
EXPTIME =                120.0 / SECONDS
PMCGAIN =                   90 / CGAIN OF PM CCD
GAIN    =                  4.2 / ELEC PER ADU
READOUT =                  7.5 / READ NOISE IN ELEC
FILTERS = 'NONE    '
IMAGETYP= 'OBJECT  '
END
fitsin tt/1980ly1.fits 1980ly1.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    2
NAXIS1  =                  516
NAXIS2  =                  516
OBJECT  = '1980ly-1' / TMO FILE NAME
DATE-OBS= '23-08-93'
TIME    = '08:10'              / UT TIME OF FILE CREATION
EXPTIME =                120.0 / SECONDS
PMCGAIN =                   90 / CGAIN OF PM CCD
GAIN    =                  4.2 / ELEC PER ADU
READOUT =                  7.5 / READ NOISE IN ELEC
FILTERS = 'NONE    '
IMAGETYP= 'OBJECT  '
END
 Total FITS keywords found       =     15
 Total FITS keywords to transfer =     15

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list 1980ly1.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File 1980ly1.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                516 lines per band
                516 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                   16'
VF003='NAXIS   =                    2'
VF004='NAXIS1  =                  516'
VF005='NAXIS2  =                  516'
VF006='OBJECT  = "1980ly-1" / TMO FILE NAME'
VF007='DATE-OBS= "23-08-93"'
VF008='TIME    = "08:10"              / UT TIME OF FILE CREATION'
VF009='EXPTIME =                120.0 / SECONDS'
VF010='PMCGAIN =                   90 / CGAIN OF PM CCD'
VF011='GAIN    =                  4.2 / ELEC PER ADU'
VF012='READOUT =                  7.5 / READ NOISE IN ELEC'
VF013='FILTERS = "NONE    "'
VF014='IMAGETYP= "OBJECT  "'
VF015='END'
 
************************************************************
list 1980ly1.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:22 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0   130   404   519   553   550   560   559   564   550
      2         0   218   451   565   584   584   608   593   591   610
      3         0   227   457   564   599   598   599   605   608   601
      4         0   249   475   572   606   607   614   637   622   619
      5         0   248   476   579   608   621   609   601   608   608
      6         0   283   484   574   589   608   623   602   616   631
      7         0   275   470   579   608   612   604   605   621   608
      8         0   260   473   568   603   606   618   601   635   617
      9         0   273   474   571   603   606   621   600   602   613
     10         0   276   485   574   626   602   616   619   622   616
fitsin tt/1980ly1.fits 1980ly1.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    2
NAXIS1  =                  516
NAXIS2  =                  516
OBJECT  = '1980ly-1' / TMO FILE NAME
DATE-OBS= '23-08-93'
TIME    = '08:10'              / UT TIME OF FILE CREATION
EXPTIME =                120.0 / SECONDS
PMCGAIN =                   90 / CGAIN OF PM CCD
GAIN    =                  4.2 / ELEC PER ADU
READOUT =                  7.5 / READ NOISE IN ELEC
FILTERS = 'NONE    '
IMAGETYP= 'OBJECT  '
END
 Total FITS keywords found       =     15
 Total FITS keywords to transfer =     15

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list 1980ly1.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File 1980ly1.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                516 lines per band
                516 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                   16'
VF003='NAXIS   =                    2'
VF004='NAXIS1  =                  516'
VF005='NAXIS2  =                  516'
VF006='OBJECT  = "1980ly-1" / TMO FILE NAME'
VF007='DATE-OBS= "23-08-93"'
VF008='TIME    = "08:10"              / UT TIME OF FILE CREATION'
VF009='EXPTIME =                120.0 / SECONDS'
VF010='PMCGAIN =                   90 / CGAIN OF PM CCD'
VF011='GAIN    =                  4.2 / ELEC PER ADU'
VF012='READOUT =                  7.5 / READ NOISE IN ELEC'
VF013='FILTERS = "NONE    "'
VF014='IMAGETYP= "OBJECT  "'
VF015='END'
 
************************************************************
list 1980ly1.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:22 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0   130   404   519   553   550   560   559   564   550
      2         0   218   451   565   584   584   608   593   591   610
      3         0   227   457   564   599   598   599   605   608   601
      4         0   249   475   572   606   607   614   637   622   619
      5         0   248   476   579   608   621   609   601   608   608
      6         0   283   484   574   589   608   623   602   616   631
      7         0   275   470   579   608   612   604   605   621   608
      8         0   260   473   568   603   606   618   601   635   617
      9         0   273   474   571   603   606   621   600   602   613
     10         0   276   485   574   626   602   616   619   622   616
fitsin tt/l637.fits l637.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     36

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list l637.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File l637.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                60 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T /'
VF002='BITPIX  =                   16 /'
VF003='NAXIS   =                    2 /'
VF004='NAXIS1  =                  800 /'
VF005='NAXIS2  =                   60 /'
VF006='BSCALE  =         8.175754E-01 /'
VF007='BZERO   =         2.538707E+04 /'
VF008='OBJECT  = "L637              " /'
VF009='FRAME   =                  233 /'
VF010='NIGHT   =                    2 /'
VF011='DATE    = "27-APR-1989       " /'
VF012='TELESCOP=                  200 /'
VF013='PORT    =                    1 /'
VF014='DEWAR   =                    8 /'
VF015='CHIP    = "TI432             " /'
VF016='ETIME   =                  300 /'
VF017='TIME    =                  300 /'
VF018='DECS    = "+                 " /'
VF019='EPOCH   =                 1950 /'
VF020='HAS     = "-                 " /'
VF021='SECZ    =         1.109078E+00 /'
VF022='TEMP    =        -1.147000E+02 /'
VF023='ERASE   =         1.228969E+03 /'
VF024='FILTER  = "NONE              " /'
VF025='SATURATE=                    0 /'
VF026='GRATING =                  600 /'
VF027='SLIT    = "2.0S              " /'
VF028='CRVAL1  =  5.9551000976563E+03 / Angstroms'
VF029='CDELT1  =  1.1090113863032E+00 /'
VF030='CRPIX1  =         1.000000E+00 /'
VF031='CTYPE1  = "WAVELENGTH        " /'
VF032='CRVAL2  =  1.0000000000000E+00 /'
VF033='CDELT2  =  1.0000000000000E+00 /'
VF034='CRPIX2  =         1.000000E+00 /'
VF035='CTYPE2  = "COLUMN #          " /'
VF036='END'
 
************************************************************
list l637.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:22 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -30462-30371-30479-30464-30495-30491-30504-30474-30439-30450
      2    -30534-30471-30357-30460-30607-30464-30523-30394-30497-30497
      3    -30444-30340-30504-30486-30403-30586-30422-30511-30406-30450
      4    -30521-30419-30409-30392-30301-30416-30552-30315-30585-30328
      5    -30458-30446-30447-30618-30488-30433-30328-30611-30674-30528
      6    -30495-30353-30413-30487-30473-30438-30427-30575-30472-30481
      7    -30557-30269-30421-30520-30592-30457-30275-30465-30483-30483
      8    -30322-30402-30550-30333-30349-30489-30346-30378-30482-30457
      9    -30515-30347-30242-30229-30179-30327-30179-30333-30206-30296
     10    -29766-29650-29734-29694-29853-29729-29671-29699-29843-29758
fitsin tt/l637.fits l637.vic pass="telescop"
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
passtart = TELESCOP
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     36

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list l637.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File l637.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                60 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T /'
VF002='BITPIX  =                   16 /'
VF003='NAXIS   =                    2 /'
VF004='NAXIS1  =                  800 /'
VF005='NAXIS2  =                   60 /'
VF006='BSCALE  =         8.175754E-01 /'
VF007='BZERO   =         2.538707E+04 /'
VF008='OBJECT  = "L637              " /'
VF009='FRAME   =                  233 /'
VF010='NIGHT   =                    2 /'
VF011='DATE    = "27-APR-1989       " /'
VF012='TELESCOP=                  200 /'
VF013='PORT    =                    1 /'
VF014='DEWAR   =                    8 /'
VF015='CHIP    = "TI432             " /'
VF016='ETIME   =                  300 /'
VF017='TIME    =                  300 /'
VF018='DECS    = "+                 " /'
VF019='EPOCH   =                 1950 /'
VF020='HAS     = "-                 " /'
VF021='SECZ    =         1.109078E+00 /'
VF022='TEMP    =        -1.147000E+02 /'
VF023='ERASE   =         1.228969E+03 /'
VF024='FILTER  = "NONE              " /'
VF025='SATURATE=                    0 /'
VF026='GRATING =                  600 /'
VF027='SLIT    = "2.0S              " /'
VF028='CRVAL1  =  5.9551000976563E+03 / Angstroms'
VF029='CDELT1  =  1.1090113863032E+00 /'
VF030='CRPIX1  =         1.000000E+00 /'
VF031='CTYPE1  = "WAVELENGTH        " /'
VF032='CRVAL2  =  1.0000000000000E+00 /'
VF033='CDELT2  =  1.0000000000000E+00 /'
VF034='CRPIX2  =         1.000000E+00 /'
VF035='CTYPE2  = "COLUMN #          " /'
VF036='END'
 
************************************************************
fitsin tt/l637.fits l637.vic excise=("telescop","chip","has","secz","port", +
	"dewar","bscale","bzero")
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     30

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list l637.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File l637.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                60 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T /'
VF002='BITPIX  =                   16 /'
VF003='NAXIS   =                    2 /'
VF004='NAXIS1  =                  800 /'
VF005='NAXIS2  =                   60 /'
VF006='BSCALE  =         8.175754E-01 /'
VF007='BZERO   =         2.538707E+04 /'
VF008='OBJECT  = "L637              " /'
VF009='FRAME   =                  233 /'
VF010='NIGHT   =                    2 /'
VF011='DATE    = "27-APR-1989       " /'
VF012='ETIME   =                  300 /'
VF013='TIME    =                  300 /'
VF014='DECS    = "+                 " /'
VF015='EPOCH   =                 1950 /'
VF016='TEMP    =        -1.147000E+02 /'
VF017='ERASE   =         1.228969E+03 /'
VF018='FILTER  = "NONE              " /'
VF019='SATURATE=                    0 /'
VF020='GRATING =                  600 /'
VF021='SLIT    = "2.0S              " /'
VF022='CRVAL1  =  5.9551000976563E+03 / Angstroms'
VF023='CDELT1  =  1.1090113863032E+00 /'
VF024='CRPIX1  =         1.000000E+00 /'
VF025='CTYPE1  = "WAVELENGTH        " /'
VF026='CRVAL2  =  1.0000000000000E+00 /'
VF027='CDELT2  =  1.0000000000000E+00 /'
VF028='CRPIX2  =         1.000000E+00 /'
VF029='CTYPE2  = "COLUMN #          " /'
VF030='END'
 
************************************************************
fitsin tt/l637.fits l637.vic data="true"
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     36

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list l637.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File l637.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                60 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:22 2016 ----
VF001='SIMPLE  =                    T /'
VF002='BITPIX  =                   16 /'
VF003='NAXIS   =                    2 /'
VF004='NAXIS1  =                  800 /'
VF005='NAXIS2  =                   60 /'
VF006='BSCALE  =         8.175754E-01 /'
VF007='BZERO   =         2.538707E+04 /'
VF008='OBJECT  = "L637              " /'
VF009='FRAME   =                  233 /'
VF010='NIGHT   =                    2 /'
VF011='DATE    = "27-APR-1989       " /'
VF012='TELESCOP=                  200 /'
VF013='PORT    =                    1 /'
VF014='DEWAR   =                    8 /'
VF015='CHIP    = "TI432             " /'
VF016='ETIME   =                  300 /'
VF017='TIME    =                  300 /'
VF018='DECS    = "+                 " /'
VF019='EPOCH   =                 1950 /'
VF020='HAS     = "-                 " /'
VF021='SECZ    =         1.109078E+00 /'
VF022='TEMP    =        -1.147000E+02 /'
VF023='ERASE   =         1.228969E+03 /'
VF024='FILTER  = "NONE              " /'
VF025='SATURATE=                    0 /'
VF026='GRATING =                  600 /'
VF027='SLIT    = "2.0S              " /'
VF028='CRVAL1  =  5.9551000976563E+03 / Angstroms'
VF029='CDELT1  =  1.1090113863032E+00 /'
VF030='CRPIX1  =         1.000000E+00 /'
VF031='CTYPE1  = "WAVELENGTH        " /'
VF032='CRVAL2  =  1.0000000000000E+00 /'
VF033='CDELT2  =  1.0000000000000E+00 /'
VF034='CRPIX2  =         1.000000E+00 /'
VF035='CTYPE2  = "COLUMN #          " /'
VF036='END'
 
************************************************************
list l637.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:22 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -30462-30371-30479-30464-30495-30491-30504-30474-30439-30450
      2    -30534-30471-30357-30460-30607-30464-30523-30394-30497-30497
      3    -30444-30340-30504-30486-30403-30586-30422-30511-30406-30450
      4    -30521-30419-30409-30392-30301-30416-30552-30315-30585-30328
      5    -30458-30446-30447-30618-30488-30433-30328-30611-30674-30528
      6    -30495-30353-30413-30487-30473-30438-30427-30575-30472-30481
      7    -30557-30269-30421-30520-30592-30457-30275-30465-30483-30483
      8    -30322-30402-30550-30333-30349-30489-30346-30378-30482-30457
      9    -30515-30347-30242-30229-30179-30327-30179-30333-30206-30296
     10    -29766-29650-29734-29694-29853-29729-29671-29699-29843-29758
fitsin tt/l637.fits header=l637.hdr
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
ush cat l637.hdr
fitsin tt/l637.fits l637.vic header=l637.hdr
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     36

*** 16-bit FITS to VICAR HALF (no conversion) ***
ush cat l637.hdr
list l637.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:23 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -30462-30371-30479-30464-30495-30491-30504-30474-30439-30450
      2    -30534-30471-30357-30460-30607-30464-30523-30394-30497-30497
      3    -30444-30340-30504-30486-30403-30586-30422-30511-30406-30450
      4    -30521-30419-30409-30392-30301-30416-30552-30315-30585-30328
      5    -30458-30446-30447-30618-30488-30433-30328-30611-30674-30528
      6    -30495-30353-30413-30487-30473-30438-30427-30575-30472-30481
      7    -30557-30269-30421-30520-30592-30457-30275-30465-30483-30483
      8    -30322-30402-30550-30333-30349-30489-30346-30378-30482-30457
      9    -30515-30347-30242-30229-30179-30327-30179-30333-30206-30296
     10    -29766-29650-29734-29694-29853-29729-29671-29699-29843-29758
fitsin tt/l637.fits l637.vic history=nohistory comment=nocomment
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T /
BITPIX  =                   16 /
NAXIS   =                    2 /
NAXIS1  =                  800 /
NAXIS2  =                   60 /
BSCALE  =         8.175754E-01 /
BZERO   =         2.538707E+04 /
OBJECT  = 'L637              ' /
FRAME   =                  233 /
NIGHT   =                    2 /
DATE    = '27-APR-1989       ' /
TELESCOP=                  200 /
PORT    =                    1 /
DEWAR   =                    8 /
CHIP    = 'TI432             ' /
ETIME   =                  300 /
TIME    =                  300 /
DECS    = '+                 ' /
EPOCH   =                 1950 /
HAS     = '-                 ' /
SECZ    =         1.109078E+00 /
TEMP    =        -1.147000E+02 /
ERASE   =         1.228969E+03 /
FILTER  = 'NONE              ' /
SATURATE=                    0 /
GRATING =                  600 /
SLIT    = '2.0S              ' /
CRVAL1  =  5.9551000976563E+03 / Angstroms
CDELT1  =  1.1090113863032E+00 /
CRPIX1  =         1.000000E+00 /
CTYPE1  = 'WAVELENGTH        ' /
CRVAL2  =  1.0000000000000E+00 /
CDELT2  =  1.0000000000000E+00 /
CRPIX2  =         1.000000E+00 /
CTYPE2  = 'COLUMN #          ' /
END
 Total FITS keywords found       =     36
 Total FITS keywords to transfer =     36

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list l637.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File l637.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                60 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:23 2016 ----
VF001='SIMPLE  =                    T /'
VF002='BITPIX  =                   16 /'
VF003='NAXIS   =                    2 /'
VF004='NAXIS1  =                  800 /'
VF005='NAXIS2  =                   60 /'
VF006='BSCALE  =         8.175754E-01 /'
VF007='BZERO   =         2.538707E+04 /'
VF008='OBJECT  = "L637              " /'
VF009='FRAME   =                  233 /'
VF010='NIGHT   =                    2 /'
VF011='DATE    = "27-APR-1989       " /'
VF012='TELESCOP=                  200 /'
VF013='PORT    =                    1 /'
VF014='DEWAR   =                    8 /'
VF015='CHIP    = "TI432             " /'
VF016='ETIME   =                  300 /'
VF017='TIME    =                  300 /'
VF018='DECS    = "+                 " /'
VF019='EPOCH   =                 1950 /'
VF020='HAS     = "-                 " /'
VF021='SECZ    =         1.109078E+00 /'
VF022='TEMP    =        -1.147000E+02 /'
VF023='ERASE   =         1.228969E+03 /'
VF024='FILTER  = "NONE              " /'
VF025='SATURATE=                    0 /'
VF026='GRATING =                  600 /'
VF027='SLIT    = "2.0S              " /'
VF028='CRVAL1  =  5.9551000976563E+03 / Angstroms'
VF029='CDELT1  =  1.1090113863032E+00 /'
VF030='CRPIX1  =         1.000000E+00 /'
VF031='CTYPE1  = "WAVELENGTH        " /'
VF032='CRVAL2  =  1.0000000000000E+00 /'
VF033='CDELT2  =  1.0000000000000E+00 /'
VF034='CRPIX2  =         1.000000E+00 /'
VF035='CTYPE2  = "COLUMN #          " /'
VF036='END'
 
************************************************************
fitsin tt/pcf230w_a.fits
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    3
NAXIS1  =                  256
NAXIS2  =                  256
NAXIS3  =                    1
FILENUM =                22510         / FILE NUMBER ASSGND BY READGEISS
OBJECT  = '3029_1          '           / OBJECT INIT'D WITH TRGTNAME FROM SHP
ITIME   =              300.000         / ITIME FROM S_ITIME OR E_ITIME
CAMERA  = 'PC      '                   / CAMERA ID FROM ENG DATA - E_CAMERA
CHIP    =                    6         / CHIP ID FROM ENG DATA - E_CHIP
FILTER  = 'F230W+OPEN'                 / ENG DATA - E_FNAME1 + E_FNAME2
DATE-OBS= '13/08/90'                   / STD HDR PACKET DATE - PKTTIME
TIME    = '04:46:52.42'                / STD HDR PACKET TIME - PKTTIME
EXPTYPE = 'EXPOSURE W/ PREFLASH  '     / ATTEMPT TO DETERMINE EXP TYPE
DATATYPE= 'INTEGER*2                                                       '
INSTRUME= 'WFP     ' /
ROOTNAME= 'W0BM0103B' /
CAXIS   =                    3 /
CNPIX1  =                  305
COMMENT ########### CARDS READ FROM GROUP DATA IN GEISS FILE ###########
CRVAL1  =  2.1173422216610E+02
CRVAL2  =  7.4593559597797E+01
CRVAL3  =  7.6190032000000E+07
CRPIX1  =  4.1200000000000E+02
CRPIX2  =  4.1200000000000E+02
CD1_1   =  3.3714873097779E-06
CD1_2   = -1.1447220458649E-05
CD2_1   = -1.1447220458649E-05
CD2_2   = -3.3714873097779E-06
DATAMIN =  0.0000000000000E+00
DATAMAX = -1.0000000000000E+00
MIR REVR=                    T
ORIENTAT= -1.0641099548340E+02
FILLCNT =                    0
ERRCNT  =                    0
FPKTTIME= '13-AUG-1990 04:47:12.54'
LPKTTIME= '13-AUG-1990 04:47:27.04'
CTYPE1  = 'RA___TAN'
CTYPE2  = 'DEC__TAN'
CTYPE3  = 'TIME    '
CNPIX2  =                  277
COMMENT ## IDT GEISS READER DECODES INFORMATION FROM THE EXTENDED REGISTER #
COMMENT ## AND STANDARD HEADER PACKET, AND CALCULATES STATISTICS. OTHER    #
COMMENT ## CARDS FROM DATA OR STANDARD HEADER PACKET HEADERS AS SPECIFIED  #
COMMENT ## BY COMMENTS.                                                    #
COMMENT ## CARD CONVENTIONS FOR WF/PC DATA READ FROM GEISS TAPES:          #
COMMENT ##   E_  -- DATA DECODED FROM EXTENDED REGISTER ENGINEERING DATA   #
COMMENT ##   S_  -- DATA DECODED FROM STANDARD HEADER PACKET DATA          #
COMMENT ##   D_  -- STATISTICAL DATA DETERMINED FROM THE FRAME ITSELF	#
HISTORY = 'A-D correction done with file:                                  '
HISTORY = 'Subtracted bias level of:   382.66                              '
COMMENT ########### DATA OBTAINED FROM ENGINEERING IN SCIENCE ##########
HISTORY = 'Subtracted preflash frome:                                      '
E_DROP  =                    0         / ROWS LOST TO DATA DROPOUTS
E_CAMERA= 'PC      '                   / CAMERA (WFC OR PC)
E_CHIP  =                    6         / Chip number
E_BLADEA= 'OPEN    '                   / Position of Shutter Blade A
E_BLADEB= 'CLOSED  '                   / Position of Shutter Blade B
E_CLRLST= '1 2 3 5 6 7 8 9 10 11 12 '  / List of clear filters
E_FILT1 = '4-C     '                   / Filter 1 Position
E_FNAME1= 'F230W   '                   / Filter 1 Name
E_FILT2 = 'OPEN    '                   / Filter 2 Position
E_FNAME2= 'OPEN    '                   / Filter 2 Name
COMMENT ########### CAMERA HEAD ELECTRONICS TEMPERATURES ###############
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.  ALL TEMPERATURES ARE DEGREES C.
E_CHET1 =               10.020         / CAMERA HEAD 1 ELEC BOARD (FIT)
E_CHET1D=                 2628         / 12-BIT TEMPERATURE
E_CHET2 =               10.020         / CAMERA HEAD 2 ELEC BOARD (FIT)
E_CHET2D=                 2628         / 12-BIT TEMPERATURE
E_CHET3 =                9.650         / CAMERA HEAD 3 ELEC BOARD (FIT)
E_CHET3D=                 2611         / 12-BIT TEMPERATURE
E_CHET4 =                9.650         / CAMERA HEAD 4 ELEC BOARD (FIT)
E_CHET4D=                 2611         / 12-BIT TEMPERATURE
E_CHET5 =               10.020         / CAMERA HEAD 5 ELEC BOARD (FIT)
E_CHET5D=                 2628         / 12-BIT TEMPERATURE
E_CHET6 =               10.770         / CAMERA HEAD 6 ELEC BOARD (FIT)
E_CHET6D=                 2662         / 12-BIT TEMPERATURE
E_CHET7 =               10.400         / CAMERA HEAD 7 ELEC BOARD (FIT)
E_CHET7D=                 2645         / 12-BIT TEMPERATURE
E_CHET8 =               10.020         / CAMERA HEAD 8 ELEC BOARD (FIT)
E_CHET8D=                 2628         / 12-BIT TEMPERATURE
COMMENT ########### HOT JUNCTION TEMPERATURES #########################
COMMENT NOTE: TEMPERATURES FOR CHIPS 1 AND 7 NOT AVAILABLE
E_CHHJ2 =              -20.470         / CAMERA HEAD 2 HOT JUNCTION (FIT)
E_CHHJ2D=                 1075         / 12-BIT TEMPERATURE
E_CHHJ3 =              -28.220         / CAMERA HEAD 3 HOT JUNCTION (FIT)
E_CHHJ3D=                  750         / 12-BIT TEMPERATURE
E_CHHJ4 =              -28.680         / CAMERA HEAD 4 HOT JUNCTION (FIT)
E_CHHJ4D=                  733         / 12-BIT TEMPERATURE
E_CHHJ5 =              -16.260         / CAMERA HEAD 5 HOT JUNCTION (FIT)
E_CHHJ5D=                 1280         / 12-BIT TEMPERATURE
E_CHHJ6 =              -13.610         / CAMERA HEAD 6 HOT JUNCTION (FIT)
E_CHHJ6D=                 1416         / 12-BIT TEMPERATURE
E_CHHJ8 =              -31.110         / CAMERA HEAD 8 HOT JUNCTION (FIT)
E_CHHJ8D=                  648         / 12-BIT TEMPERATURE
COMMENT ######### OPTICAL BENCH TEMPERATURES ##########################
E_OPBCH =                9.290         / OPTICAL BENCH CAMERA HEAD T (FIT)
E_OPBCHD=                 2594         / 12-BIT TEMPERATURE
E_OPBFM =               10.020         / OPTICAL BENCH FOLD MIRROR T (FIT)
E_OPBFMD=                 2628         / 12-BIT TEMPERATURE
COMMENT ########## COLD JUNCTION TEMPERATURES #########################
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.
E_CHCJ1 =              -97.260         / CAMERA HEAD 1 COLD JUNCTION (FIT)
E_CHCJ1D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ2 =              -97.640         / CAMERA HEAD 2 COLD JUNCTION (FIT)
E_CHCJ2D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ3 =              -97.860         / CAMERA HEAD 3 COLD JUNCTION (FIT)
E_CHCJ3D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ4 =              -97.610         / CAMERA HEAD 4 COLD JUNCTION (FIT)
E_CHCJ4D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ5 =              -97.960         / CAMERA HEAD 5 COLD JUNCTION (FIT)
E_CHCJ5D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ6 =              -98.200         / CAMERA HEAD 6 COLD JUNCTION (FIT)
E_CHCJ6D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ7 =              -97.550         / CAMERA HEAD 7 COLD JUNCTION (FIT)
E_CHCJ7D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ8 =              -98.330         / CAMERA HEAD 8 COLD JUNCTION (FIT)
E_CHCJ8D=                 3362         / 12-BIT TEMPERATURE
COMMENT ########## DATA FROM EXPORT PORT 1 COMMAND ####################
COMMENT NOTE:  THESE DATA ARE CORRUPTED ON ORBIT DUE TO HOW EXPOSURES
COMMENT        ARE COMMANDED.
E_ITIME =                0.110         / Exposure in Seconds
E_ERASE =                    F         / T if Chip Erased
E_EXPOSE=                    F         / T if Exposure
E_AUTOER=                    F         / T if Auto Erase
E_CALIBR=                    F         / T if Calibration Exposure
E_KSPOT =                    F         / T if K Spot
HISTORY = 'Divided by flatfield frame:                                     '
COMMENT ###### PARAMETERS EXTRACTED FROM THE STANDARD HEADER PACKET ####
COMMENT ## NOTE: MUCH MORE DATA EXISTS IN THE STANDARD HEADER PACKET ###
COMMENT ## THAN WHAT IS DECODED HERE.  THE PODPS .SHH FILE CONTAINS  ###
COMMENT ## DECODED VALUES OF ALL STANDARD HEADER PACKET DATA.        ###
CCOL    =                  433
S_ITIME =              300.000         / COMMANDED INTEGRATION TIME
S_ERASE =                    F         / T IF ERASE IN WEXPOCMD
S_SHUTTR=                    T         / T IF SHUTTER IN WEXPOCMD
S_CLEXEC=                    F         / T IF CAL EXEC IN WEXPOCMD
S_KSPOT =                    F         / T IF KSPOTS IN WEXPOCMD
S_AUTOER=                    F         / T IF AUTO ERASE IN WEXPOCMD
S_LMPCAL=                    F         / T IF LAMP/DARK CAL IN WEXPOCMD
S_PROGID= '0BM-01-03'                  / PROGRAM ID - OBS SET - OBS ID
S_APEXEC=                    T         / T IF APPL PROCESSOR CONTROL
S_CMDCAN=                    T         / T IF CANCEL CMD SENT
S_SHOVFL=                    F         / T IF SHUTTER LOG OVERFLOW
S_MODE  =                    F         / T IF MANUAL (SERIALS ON)
S_SHAMAN=                    F         / T IF BLADE A OPEN (MANUAL)
S_SHBMAN=                    F         / T IF BLADE B OPEN (MANUAL)
S_SH_AP =                    T         / T IF SHUTTER BLADE USED BY APPL PROCESS
S_NSHUT =                    1         / NUMBER OF ADDITIONAL SHUTTER CLOSURES
CROW    =                  405
COMMENT ### SELECTED CARDS FROM ASCII HEADER OF STANDARD HEADER PACKET ##
PKTTIME = '13-AUG-1990 04:46:52.42'
TRGTNAME= '3029_1          ' /
TARAQMOD= '03      ' /
OBSERVTN= '03      ' /
OBSET_ID= '01      ' /
DECLMOON=              21.0832 /
RTASMOON=              40.0589 /
DECLNSUN=              14.7115 /
RTASCSUN=              142.731 /
DECLNTRG=              74.5936 /
RTASNTRG=              211.734 /
DECLNV1 =              74.5993 /
RTASCNV1=              211.746 /
REFOBDEC=              74.5936 /
REFOBJPA=         0.000000E+00 /
REFOBJRA=              211.734 /
VELABBRA=              23.5877 /
VELOCSTX=            -0.494504 /
VELOCSTY=              7.31953 /
VELOCSTZ=             -1.80318 /
COMMENT ############ STATISTICS DETERMINED FROM DATA FRAME #############
D_NSAT  =                    3         / NUMBER OF SATURATED PIXELS IN ENTIRE FR
COMMENT STATISTICS OF SAMPLED 8-BIT BIAS VALUES IN ENGINEERING COLUMN
D_8BAVG =              191.553         / AVERAGE VALUE
D_8BRMS =                0.881         / RMS
D_8BLOW =                  189         / LOW DATA VALUE
D_8BHIGH=                  195         / HIGH DATA VALUE
D_BSREGN= 'X0 = 5  Y0 = 3  NX = 9  NY = 797'/ EXTENDED REGISTER BIAS REGION
D_BSAVG =              381.594         / AVERAGE VALUE (SAT, DROP IGNORED)
D_BSRMS =                1.625         / RMS OF REGION
D_BSLOW =                  375         / LOW DATA VALUE
D_BSHIGH=                  389         / HIGH DATA VALUE
D_BSAVG3=              381.585         / AVERAGE AFTER 3 SIGMA REJECTION
D_BSRMS3=                1.596         / RMS AFTER 3 SIGMA REJECTION
D_BSNREJ=                   19         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_BSGOOD=                 6368         / NUMBER OF PIXELS WITH GOOD DATA
D_BSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_BSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_WFREGN= 'X0 = 30  Y0 = 30  NX = 769  NY = 769'/ WHOLE FRAME, EXCLUDING PYR SHA
D_WFAVG =              388.967         / AVERAGE VALUE (SAT, DROP IGNORED)
D_WFRMS =               18.384         / RMS OF REGION
D_WFLOW =                  377         / LOW DATA VALUE
D_WFHIGH=                 3261         / HIGH DATA VALUE
D_WFAVG3=              388.255         / AVERAGE AFTER 3 SIGMA REJECTION
D_WFRMS3=                4.108         / RMS AFTER 3 SIGMA REJECTION
D_WFNREJ=                 2951         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_WFGOOD=               589821         / NUMBER OF PIXELS WITH GOOD DATA
D_WFNSAT=                    3         / NUMBER OF SATURATED PIXELS
D_WFNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LLREGN= 'X0 = 50  Y0 = 50  NX = 101  NY = 101'/ LOWER LEFT CORNER REGION
D_LLAVG =              387.584         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LLRMS =                6.333         / RMS OF REGION
D_LLLOW =                  381         / LOW DATA VALUE
D_LLHIGH=                  901         / HIGH DATA VALUE
D_LLAVG3=              387.442         / AVERAGE AFTER 3 SIGMA REJECTION
D_LLRMS3=                2.152         / RMS AFTER 3 SIGMA REJECTION
D_LLNREJ=                   14         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LLGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LLNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LLNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_ULREGN= 'X0 = 50  Y0 = 650  NX = 101  NY = 101'/ UPPER LEFT CORNER REGION
D_ULAVG =              386.836         / AVERAGE VALUE (SAT, DROP IGNORED)
D_ULRMS =                4.212         / RMS OF REGION
D_ULLOW =                  380         / LOW DATA VALUE
D_ULHIGH=                  692         / HIGH DATA VALUE
D_ULAVG3=              386.738         / AVERAGE AFTER 3 SIGMA REJECTION
D_ULRMS3=                2.123         / RMS AFTER 3 SIGMA REJECTION
D_ULNREJ=                   16         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_ULGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_ULNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_ULNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LRREGN= 'X0 = 650  Y0 = 50  NX = 101  NY = 101'/ LOWER RIGHT CORNER REGION
D_LRAVG =              386.603         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LRRMS =                4.152         / RMS OF REGION
D_LRLOW =                  377         / LOW DATA VALUE
D_LRHIGH=                  547         / HIGH DATA VALUE
D_LRAVG3=              386.445         / AVERAGE AFTER 3 SIGMA REJECTION
D_LRRMS3=                2.115         / RMS AFTER 3 SIGMA REJECTION
D_LRNREJ=                   30         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LRGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LRNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LRNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_URREGN= 'X0 = 650  Y0 = 650  NX = 101  NY = 101'/ UPPER RIGHT CORNER REGION
D_URAVG =              385.993         / AVERAGE VALUE (SAT, DROP IGNORED)
D_URRMS =                6.171         / RMS OF REGION
D_URLOW =                  379         / LOW DATA VALUE
D_URHIGH=                  855         / HIGH DATA VALUE
D_URAVG3=              385.837         / AVERAGE AFTER 3 SIGMA REJECTION
D_URRMS3=                2.063         / RMS AFTER 3 SIGMA REJECTION
D_URNREJ=                   17         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_URGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_URNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_URNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_CCREGN= 'X0 = 350  Y0 = 350  NX = 101  NY = 101'/ CENTRAL REGION
D_CCAVG =              425.882         / AVERAGE VALUE (SAT, DROP IGNORED)
D_CCRMS =              110.111         / RMS OF REGION
D_CCLOW =                  379         / LOW DATA VALUE
D_CCHIGH=                 3117         / HIGH DATA VALUE
D_CCAVG3=              416.486         / AVERAGE AFTER 3 SIGMA REJECTION
D_CCRMS3=               39.636         / RMS AFTER 3 SIGMA REJECTION
D_CCNREJ=                  110         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_CCGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_CCNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_CCNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_PSREGN= 'X0 = 3  Y0 = 3  NX = 15  NY = 791'/ PYRAMID SHADOW
D_PSAVG =              384.141         / AVERAGE VALUE (SAT, DROP IGNORED)
D_PSRMS =                8.785         / RMS OF REGION
D_PSLOW =                  377         / LOW DATA VALUE
D_PSHIGH=                 1105         / HIGH DATA VALUE
D_PSAVG3=              383.915         / AVERAGE AFTER 3 SIGMA REJECTION
D_PSRMS3=                1.997         / RMS AFTER 3 SIGMA REJECTION
D_PSNREJ=                   20         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_PSGOOD=                11060         / NUMBER OF PIXELS WITH GOOD DATA
D_PSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_PSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
DISK    = 'FITS                                                            '
BZERO   =  1.3271640625000E+03
BSCALE  =  4.0751308202744E-02
END
fitsin tt/pcf230w_a.fits header=pcf230w_a.hdr
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    3
NAXIS1  =                  256
NAXIS2  =                  256
NAXIS3  =                    1
FILENUM =                22510         / FILE NUMBER ASSGND BY READGEISS
OBJECT  = '3029_1          '           / OBJECT INIT'D WITH TRGTNAME FROM SHP
ITIME   =              300.000         / ITIME FROM S_ITIME OR E_ITIME
CAMERA  = 'PC      '                   / CAMERA ID FROM ENG DATA - E_CAMERA
CHIP    =                    6         / CHIP ID FROM ENG DATA - E_CHIP
FILTER  = 'F230W+OPEN'                 / ENG DATA - E_FNAME1 + E_FNAME2
DATE-OBS= '13/08/90'                   / STD HDR PACKET DATE - PKTTIME
TIME    = '04:46:52.42'                / STD HDR PACKET TIME - PKTTIME
EXPTYPE = 'EXPOSURE W/ PREFLASH  '     / ATTEMPT TO DETERMINE EXP TYPE
DATATYPE= 'INTEGER*2                                                       '
INSTRUME= 'WFP     ' /
ROOTNAME= 'W0BM0103B' /
CAXIS   =                    3 /
CNPIX1  =                  305
COMMENT ########### CARDS READ FROM GROUP DATA IN GEISS FILE ###########
CRVAL1  =  2.1173422216610E+02
CRVAL2  =  7.4593559597797E+01
CRVAL3  =  7.6190032000000E+07
CRPIX1  =  4.1200000000000E+02
CRPIX2  =  4.1200000000000E+02
CD1_1   =  3.3714873097779E-06
CD1_2   = -1.1447220458649E-05
CD2_1   = -1.1447220458649E-05
CD2_2   = -3.3714873097779E-06
DATAMIN =  0.0000000000000E+00
DATAMAX = -1.0000000000000E+00
MIR REVR=                    T
ORIENTAT= -1.0641099548340E+02
FILLCNT =                    0
ERRCNT  =                    0
FPKTTIME= '13-AUG-1990 04:47:12.54'
LPKTTIME= '13-AUG-1990 04:47:27.04'
CTYPE1  = 'RA___TAN'
CTYPE2  = 'DEC__TAN'
CTYPE3  = 'TIME    '
CNPIX2  =                  277
COMMENT ## IDT GEISS READER DECODES INFORMATION FROM THE EXTENDED REGISTER #
COMMENT ## AND STANDARD HEADER PACKET, AND CALCULATES STATISTICS. OTHER    #
COMMENT ## CARDS FROM DATA OR STANDARD HEADER PACKET HEADERS AS SPECIFIED  #
COMMENT ## BY COMMENTS.                                                    #
COMMENT ## CARD CONVENTIONS FOR WF/PC DATA READ FROM GEISS TAPES:          #
COMMENT ##   E_  -- DATA DECODED FROM EXTENDED REGISTER ENGINEERING DATA   #
COMMENT ##   S_  -- DATA DECODED FROM STANDARD HEADER PACKET DATA          #
COMMENT ##   D_  -- STATISTICAL DATA DETERMINED FROM THE FRAME ITSELF	#
HISTORY = 'A-D correction done with file:                                  '
HISTORY = 'Subtracted bias level of:   382.66                              '
COMMENT ########### DATA OBTAINED FROM ENGINEERING IN SCIENCE ##########
HISTORY = 'Subtracted preflash frome:                                      '
E_DROP  =                    0         / ROWS LOST TO DATA DROPOUTS
E_CAMERA= 'PC      '                   / CAMERA (WFC OR PC)
E_CHIP  =                    6         / Chip number
E_BLADEA= 'OPEN    '                   / Position of Shutter Blade A
E_BLADEB= 'CLOSED  '                   / Position of Shutter Blade B
E_CLRLST= '1 2 3 5 6 7 8 9 10 11 12 '  / List of clear filters
E_FILT1 = '4-C     '                   / Filter 1 Position
E_FNAME1= 'F230W   '                   / Filter 1 Name
E_FILT2 = 'OPEN    '                   / Filter 2 Position
E_FNAME2= 'OPEN    '                   / Filter 2 Name
COMMENT ########### CAMERA HEAD ELECTRONICS TEMPERATURES ###############
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.  ALL TEMPERATURES ARE DEGREES C.
E_CHET1 =               10.020         / CAMERA HEAD 1 ELEC BOARD (FIT)
E_CHET1D=                 2628         / 12-BIT TEMPERATURE
E_CHET2 =               10.020         / CAMERA HEAD 2 ELEC BOARD (FIT)
E_CHET2D=                 2628         / 12-BIT TEMPERATURE
E_CHET3 =                9.650         / CAMERA HEAD 3 ELEC BOARD (FIT)
E_CHET3D=                 2611         / 12-BIT TEMPERATURE
E_CHET4 =                9.650         / CAMERA HEAD 4 ELEC BOARD (FIT)
E_CHET4D=                 2611         / 12-BIT TEMPERATURE
E_CHET5 =               10.020         / CAMERA HEAD 5 ELEC BOARD (FIT)
E_CHET5D=                 2628         / 12-BIT TEMPERATURE
E_CHET6 =               10.770         / CAMERA HEAD 6 ELEC BOARD (FIT)
E_CHET6D=                 2662         / 12-BIT TEMPERATURE
E_CHET7 =               10.400         / CAMERA HEAD 7 ELEC BOARD (FIT)
E_CHET7D=                 2645         / 12-BIT TEMPERATURE
E_CHET8 =               10.020         / CAMERA HEAD 8 ELEC BOARD (FIT)
E_CHET8D=                 2628         / 12-BIT TEMPERATURE
COMMENT ########### HOT JUNCTION TEMPERATURES #########################
COMMENT NOTE: TEMPERATURES FOR CHIPS 1 AND 7 NOT AVAILABLE
E_CHHJ2 =              -20.470         / CAMERA HEAD 2 HOT JUNCTION (FIT)
E_CHHJ2D=                 1075         / 12-BIT TEMPERATURE
E_CHHJ3 =              -28.220         / CAMERA HEAD 3 HOT JUNCTION (FIT)
E_CHHJ3D=                  750         / 12-BIT TEMPERATURE
E_CHHJ4 =              -28.680         / CAMERA HEAD 4 HOT JUNCTION (FIT)
E_CHHJ4D=                  733         / 12-BIT TEMPERATURE
E_CHHJ5 =              -16.260         / CAMERA HEAD 5 HOT JUNCTION (FIT)
E_CHHJ5D=                 1280         / 12-BIT TEMPERATURE
E_CHHJ6 =              -13.610         / CAMERA HEAD 6 HOT JUNCTION (FIT)
E_CHHJ6D=                 1416         / 12-BIT TEMPERATURE
E_CHHJ8 =              -31.110         / CAMERA HEAD 8 HOT JUNCTION (FIT)
E_CHHJ8D=                  648         / 12-BIT TEMPERATURE
COMMENT ######### OPTICAL BENCH TEMPERATURES ##########################
E_OPBCH =                9.290         / OPTICAL BENCH CAMERA HEAD T (FIT)
E_OPBCHD=                 2594         / 12-BIT TEMPERATURE
E_OPBFM =               10.020         / OPTICAL BENCH FOLD MIRROR T (FIT)
E_OPBFMD=                 2628         / 12-BIT TEMPERATURE
COMMENT ########## COLD JUNCTION TEMPERATURES #########################
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.
E_CHCJ1 =              -97.260         / CAMERA HEAD 1 COLD JUNCTION (FIT)
E_CHCJ1D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ2 =              -97.640         / CAMERA HEAD 2 COLD JUNCTION (FIT)
E_CHCJ2D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ3 =              -97.860         / CAMERA HEAD 3 COLD JUNCTION (FIT)
E_CHCJ3D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ4 =              -97.610         / CAMERA HEAD 4 COLD JUNCTION (FIT)
E_CHCJ4D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ5 =              -97.960         / CAMERA HEAD 5 COLD JUNCTION (FIT)
E_CHCJ5D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ6 =              -98.200         / CAMERA HEAD 6 COLD JUNCTION (FIT)
E_CHCJ6D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ7 =              -97.550         / CAMERA HEAD 7 COLD JUNCTION (FIT)
E_CHCJ7D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ8 =              -98.330         / CAMERA HEAD 8 COLD JUNCTION (FIT)
E_CHCJ8D=                 3362         / 12-BIT TEMPERATURE
COMMENT ########## DATA FROM EXPORT PORT 1 COMMAND ####################
COMMENT NOTE:  THESE DATA ARE CORRUPTED ON ORBIT DUE TO HOW EXPOSURES
COMMENT        ARE COMMANDED.
E_ITIME =                0.110         / Exposure in Seconds
E_ERASE =                    F         / T if Chip Erased
E_EXPOSE=                    F         / T if Exposure
E_AUTOER=                    F         / T if Auto Erase
E_CALIBR=                    F         / T if Calibration Exposure
E_KSPOT =                    F         / T if K Spot
HISTORY = 'Divided by flatfield frame:                                     '
COMMENT ###### PARAMETERS EXTRACTED FROM THE STANDARD HEADER PACKET ####
COMMENT ## NOTE: MUCH MORE DATA EXISTS IN THE STANDARD HEADER PACKET ###
COMMENT ## THAN WHAT IS DECODED HERE.  THE PODPS .SHH FILE CONTAINS  ###
COMMENT ## DECODED VALUES OF ALL STANDARD HEADER PACKET DATA.        ###
CCOL    =                  433
S_ITIME =              300.000         / COMMANDED INTEGRATION TIME
S_ERASE =                    F         / T IF ERASE IN WEXPOCMD
S_SHUTTR=                    T         / T IF SHUTTER IN WEXPOCMD
S_CLEXEC=                    F         / T IF CAL EXEC IN WEXPOCMD
S_KSPOT =                    F         / T IF KSPOTS IN WEXPOCMD
S_AUTOER=                    F         / T IF AUTO ERASE IN WEXPOCMD
S_LMPCAL=                    F         / T IF LAMP/DARK CAL IN WEXPOCMD
S_PROGID= '0BM-01-03'                  / PROGRAM ID - OBS SET - OBS ID
S_APEXEC=                    T         / T IF APPL PROCESSOR CONTROL
S_CMDCAN=                    T         / T IF CANCEL CMD SENT
S_SHOVFL=                    F         / T IF SHUTTER LOG OVERFLOW
S_MODE  =                    F         / T IF MANUAL (SERIALS ON)
S_SHAMAN=                    F         / T IF BLADE A OPEN (MANUAL)
S_SHBMAN=                    F         / T IF BLADE B OPEN (MANUAL)
S_SH_AP =                    T         / T IF SHUTTER BLADE USED BY APPL PROCESS
S_NSHUT =                    1         / NUMBER OF ADDITIONAL SHUTTER CLOSURES
CROW    =                  405
COMMENT ### SELECTED CARDS FROM ASCII HEADER OF STANDARD HEADER PACKET ##
PKTTIME = '13-AUG-1990 04:46:52.42'
TRGTNAME= '3029_1          ' /
TARAQMOD= '03      ' /
OBSERVTN= '03      ' /
OBSET_ID= '01      ' /
DECLMOON=              21.0832 /
RTASMOON=              40.0589 /
DECLNSUN=              14.7115 /
RTASCSUN=              142.731 /
DECLNTRG=              74.5936 /
RTASNTRG=              211.734 /
DECLNV1 =              74.5993 /
RTASCNV1=              211.746 /
REFOBDEC=              74.5936 /
REFOBJPA=         0.000000E+00 /
REFOBJRA=              211.734 /
VELABBRA=              23.5877 /
VELOCSTX=            -0.494504 /
VELOCSTY=              7.31953 /
VELOCSTZ=             -1.80318 /
COMMENT ############ STATISTICS DETERMINED FROM DATA FRAME #############
D_NSAT  =                    3         / NUMBER OF SATURATED PIXELS IN ENTIRE FR
COMMENT STATISTICS OF SAMPLED 8-BIT BIAS VALUES IN ENGINEERING COLUMN
D_8BAVG =              191.553         / AVERAGE VALUE
D_8BRMS =                0.881         / RMS
D_8BLOW =                  189         / LOW DATA VALUE
D_8BHIGH=                  195         / HIGH DATA VALUE
D_BSREGN= 'X0 = 5  Y0 = 3  NX = 9  NY = 797'/ EXTENDED REGISTER BIAS REGION
D_BSAVG =              381.594         / AVERAGE VALUE (SAT, DROP IGNORED)
D_BSRMS =                1.625         / RMS OF REGION
D_BSLOW =                  375         / LOW DATA VALUE
D_BSHIGH=                  389         / HIGH DATA VALUE
D_BSAVG3=              381.585         / AVERAGE AFTER 3 SIGMA REJECTION
D_BSRMS3=                1.596         / RMS AFTER 3 SIGMA REJECTION
D_BSNREJ=                   19         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_BSGOOD=                 6368         / NUMBER OF PIXELS WITH GOOD DATA
D_BSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_BSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_WFREGN= 'X0 = 30  Y0 = 30  NX = 769  NY = 769'/ WHOLE FRAME, EXCLUDING PYR SHA
D_WFAVG =              388.967         / AVERAGE VALUE (SAT, DROP IGNORED)
D_WFRMS =               18.384         / RMS OF REGION
D_WFLOW =                  377         / LOW DATA VALUE
D_WFHIGH=                 3261         / HIGH DATA VALUE
D_WFAVG3=              388.255         / AVERAGE AFTER 3 SIGMA REJECTION
D_WFRMS3=                4.108         / RMS AFTER 3 SIGMA REJECTION
D_WFNREJ=                 2951         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_WFGOOD=               589821         / NUMBER OF PIXELS WITH GOOD DATA
D_WFNSAT=                    3         / NUMBER OF SATURATED PIXELS
D_WFNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LLREGN= 'X0 = 50  Y0 = 50  NX = 101  NY = 101'/ LOWER LEFT CORNER REGION
D_LLAVG =              387.584         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LLRMS =                6.333         / RMS OF REGION
D_LLLOW =                  381         / LOW DATA VALUE
D_LLHIGH=                  901         / HIGH DATA VALUE
D_LLAVG3=              387.442         / AVERAGE AFTER 3 SIGMA REJECTION
D_LLRMS3=                2.152         / RMS AFTER 3 SIGMA REJECTION
D_LLNREJ=                   14         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LLGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LLNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LLNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_ULREGN= 'X0 = 50  Y0 = 650  NX = 101  NY = 101'/ UPPER LEFT CORNER REGION
D_ULAVG =              386.836         / AVERAGE VALUE (SAT, DROP IGNORED)
D_ULRMS =                4.212         / RMS OF REGION
D_ULLOW =                  380         / LOW DATA VALUE
D_ULHIGH=                  692         / HIGH DATA VALUE
D_ULAVG3=              386.738         / AVERAGE AFTER 3 SIGMA REJECTION
D_ULRMS3=                2.123         / RMS AFTER 3 SIGMA REJECTION
D_ULNREJ=                   16         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_ULGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_ULNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_ULNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LRREGN= 'X0 = 650  Y0 = 50  NX = 101  NY = 101'/ LOWER RIGHT CORNER REGION
D_LRAVG =              386.603         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LRRMS =                4.152         / RMS OF REGION
D_LRLOW =                  377         / LOW DATA VALUE
D_LRHIGH=                  547         / HIGH DATA VALUE
D_LRAVG3=              386.445         / AVERAGE AFTER 3 SIGMA REJECTION
D_LRRMS3=                2.115         / RMS AFTER 3 SIGMA REJECTION
D_LRNREJ=                   30         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LRGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LRNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LRNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_URREGN= 'X0 = 650  Y0 = 650  NX = 101  NY = 101'/ UPPER RIGHT CORNER REGION
D_URAVG =              385.993         / AVERAGE VALUE (SAT, DROP IGNORED)
D_URRMS =                6.171         / RMS OF REGION
D_URLOW =                  379         / LOW DATA VALUE
D_URHIGH=                  855         / HIGH DATA VALUE
D_URAVG3=              385.837         / AVERAGE AFTER 3 SIGMA REJECTION
D_URRMS3=                2.063         / RMS AFTER 3 SIGMA REJECTION
D_URNREJ=                   17         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_URGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_URNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_URNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_CCREGN= 'X0 = 350  Y0 = 350  NX = 101  NY = 101'/ CENTRAL REGION
D_CCAVG =              425.882         / AVERAGE VALUE (SAT, DROP IGNORED)
D_CCRMS =              110.111         / RMS OF REGION
D_CCLOW =                  379         / LOW DATA VALUE
D_CCHIGH=                 3117         / HIGH DATA VALUE
D_CCAVG3=              416.486         / AVERAGE AFTER 3 SIGMA REJECTION
D_CCRMS3=               39.636         / RMS AFTER 3 SIGMA REJECTION
D_CCNREJ=                  110         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_CCGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_CCNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_CCNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_PSREGN= 'X0 = 3  Y0 = 3  NX = 15  NY = 791'/ PYRAMID SHADOW
D_PSAVG =              384.141         / AVERAGE VALUE (SAT, DROP IGNORED)
D_PSRMS =                8.785         / RMS OF REGION
D_PSLOW =                  377         / LOW DATA VALUE
D_PSHIGH=                 1105         / HIGH DATA VALUE
D_PSAVG3=              383.915         / AVERAGE AFTER 3 SIGMA REJECTION
D_PSRMS3=                1.997         / RMS AFTER 3 SIGMA REJECTION
D_PSNREJ=                   20         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_PSGOOD=                11060         / NUMBER OF PIXELS WITH GOOD DATA
D_PSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_PSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
DISK    = 'FITS                                                            '
BZERO   =  1.3271640625000E+03
BSCALE  =  4.0751308202744E-02
END
ush cat pcf230w_a.hdr
fitsin tt/pcf230w_a.fits pcf230w_a.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    3
NAXIS1  =                  256
NAXIS2  =                  256
NAXIS3  =                    1
FILENUM =                22510         / FILE NUMBER ASSGND BY READGEISS
OBJECT  = '3029_1          '           / OBJECT INIT'D WITH TRGTNAME FROM SHP
ITIME   =              300.000         / ITIME FROM S_ITIME OR E_ITIME
CAMERA  = 'PC      '                   / CAMERA ID FROM ENG DATA - E_CAMERA
CHIP    =                    6         / CHIP ID FROM ENG DATA - E_CHIP
FILTER  = 'F230W+OPEN'                 / ENG DATA - E_FNAME1 + E_FNAME2
DATE-OBS= '13/08/90'                   / STD HDR PACKET DATE - PKTTIME
TIME    = '04:46:52.42'                / STD HDR PACKET TIME - PKTTIME
EXPTYPE = 'EXPOSURE W/ PREFLASH  '     / ATTEMPT TO DETERMINE EXP TYPE
DATATYPE= 'INTEGER*2                                                       '
INSTRUME= 'WFP     ' /
ROOTNAME= 'W0BM0103B' /
CAXIS   =                    3 /
CNPIX1  =                  305
COMMENT ########### CARDS READ FROM GROUP DATA IN GEISS FILE ###########
CRVAL1  =  2.1173422216610E+02
CRVAL2  =  7.4593559597797E+01
CRVAL3  =  7.6190032000000E+07
CRPIX1  =  4.1200000000000E+02
CRPIX2  =  4.1200000000000E+02
CD1_1   =  3.3714873097779E-06
CD1_2   = -1.1447220458649E-05
CD2_1   = -1.1447220458649E-05
CD2_2   = -3.3714873097779E-06
DATAMIN =  0.0000000000000E+00
DATAMAX = -1.0000000000000E+00
MIR REVR=                    T
ORIENTAT= -1.0641099548340E+02
FILLCNT =                    0
ERRCNT  =                    0
FPKTTIME= '13-AUG-1990 04:47:12.54'
LPKTTIME= '13-AUG-1990 04:47:27.04'
CTYPE1  = 'RA___TAN'
CTYPE2  = 'DEC__TAN'
CTYPE3  = 'TIME    '
CNPIX2  =                  277
COMMENT ## IDT GEISS READER DECODES INFORMATION FROM THE EXTENDED REGISTER #
COMMENT ## AND STANDARD HEADER PACKET, AND CALCULATES STATISTICS. OTHER    #
COMMENT ## CARDS FROM DATA OR STANDARD HEADER PACKET HEADERS AS SPECIFIED  #
COMMENT ## BY COMMENTS.                                                    #
COMMENT ## CARD CONVENTIONS FOR WF/PC DATA READ FROM GEISS TAPES:          #
COMMENT ##   E_  -- DATA DECODED FROM EXTENDED REGISTER ENGINEERING DATA   #
COMMENT ##   S_  -- DATA DECODED FROM STANDARD HEADER PACKET DATA          #
COMMENT ##   D_  -- STATISTICAL DATA DETERMINED FROM THE FRAME ITSELF	#
HISTORY = 'A-D correction done with file:                                  '
HISTORY = 'Subtracted bias level of:   382.66                              '
COMMENT ########### DATA OBTAINED FROM ENGINEERING IN SCIENCE ##########
HISTORY = 'Subtracted preflash frome:                                      '
E_DROP  =                    0         / ROWS LOST TO DATA DROPOUTS
E_CAMERA= 'PC      '                   / CAMERA (WFC OR PC)
E_CHIP  =                    6         / Chip number
E_BLADEA= 'OPEN    '                   / Position of Shutter Blade A
E_BLADEB= 'CLOSED  '                   / Position of Shutter Blade B
E_CLRLST= '1 2 3 5 6 7 8 9 10 11 12 '  / List of clear filters
E_FILT1 = '4-C     '                   / Filter 1 Position
E_FNAME1= 'F230W   '                   / Filter 1 Name
E_FILT2 = 'OPEN    '                   / Filter 2 Position
E_FNAME2= 'OPEN    '                   / Filter 2 Name
COMMENT ########### CAMERA HEAD ELECTRONICS TEMPERATURES ###############
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.  ALL TEMPERATURES ARE DEGREES C.
E_CHET1 =               10.020         / CAMERA HEAD 1 ELEC BOARD (FIT)
E_CHET1D=                 2628         / 12-BIT TEMPERATURE
E_CHET2 =               10.020         / CAMERA HEAD 2 ELEC BOARD (FIT)
E_CHET2D=                 2628         / 12-BIT TEMPERATURE
E_CHET3 =                9.650         / CAMERA HEAD 3 ELEC BOARD (FIT)
E_CHET3D=                 2611         / 12-BIT TEMPERATURE
E_CHET4 =                9.650         / CAMERA HEAD 4 ELEC BOARD (FIT)
E_CHET4D=                 2611         / 12-BIT TEMPERATURE
E_CHET5 =               10.020         / CAMERA HEAD 5 ELEC BOARD (FIT)
E_CHET5D=                 2628         / 12-BIT TEMPERATURE
E_CHET6 =               10.770         / CAMERA HEAD 6 ELEC BOARD (FIT)
E_CHET6D=                 2662         / 12-BIT TEMPERATURE
E_CHET7 =               10.400         / CAMERA HEAD 7 ELEC BOARD (FIT)
E_CHET7D=                 2645         / 12-BIT TEMPERATURE
E_CHET8 =               10.020         / CAMERA HEAD 8 ELEC BOARD (FIT)
E_CHET8D=                 2628         / 12-BIT TEMPERATURE
COMMENT ########### HOT JUNCTION TEMPERATURES #########################
COMMENT NOTE: TEMPERATURES FOR CHIPS 1 AND 7 NOT AVAILABLE
E_CHHJ2 =              -20.470         / CAMERA HEAD 2 HOT JUNCTION (FIT)
E_CHHJ2D=                 1075         / 12-BIT TEMPERATURE
E_CHHJ3 =              -28.220         / CAMERA HEAD 3 HOT JUNCTION (FIT)
E_CHHJ3D=                  750         / 12-BIT TEMPERATURE
E_CHHJ4 =              -28.680         / CAMERA HEAD 4 HOT JUNCTION (FIT)
E_CHHJ4D=                  733         / 12-BIT TEMPERATURE
E_CHHJ5 =              -16.260         / CAMERA HEAD 5 HOT JUNCTION (FIT)
E_CHHJ5D=                 1280         / 12-BIT TEMPERATURE
E_CHHJ6 =              -13.610         / CAMERA HEAD 6 HOT JUNCTION (FIT)
E_CHHJ6D=                 1416         / 12-BIT TEMPERATURE
E_CHHJ8 =              -31.110         / CAMERA HEAD 8 HOT JUNCTION (FIT)
E_CHHJ8D=                  648         / 12-BIT TEMPERATURE
COMMENT ######### OPTICAL BENCH TEMPERATURES ##########################
E_OPBCH =                9.290         / OPTICAL BENCH CAMERA HEAD T (FIT)
E_OPBCHD=                 2594         / 12-BIT TEMPERATURE
E_OPBFM =               10.020         / OPTICAL BENCH FOLD MIRROR T (FIT)
E_OPBFMD=                 2628         / 12-BIT TEMPERATURE
COMMENT ########## COLD JUNCTION TEMPERATURES #########################
COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE
COMMENT CALIBRATION DATA ARE GIVEN.
E_CHCJ1 =              -97.260         / CAMERA HEAD 1 COLD JUNCTION (FIT)
E_CHCJ1D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ2 =              -97.640         / CAMERA HEAD 2 COLD JUNCTION (FIT)
E_CHCJ2D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ3 =              -97.860         / CAMERA HEAD 3 COLD JUNCTION (FIT)
E_CHCJ3D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ4 =              -97.610         / CAMERA HEAD 4 COLD JUNCTION (FIT)
E_CHCJ4D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ5 =              -97.960         / CAMERA HEAD 5 COLD JUNCTION (FIT)
E_CHCJ5D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ6 =              -98.200         / CAMERA HEAD 6 COLD JUNCTION (FIT)
E_CHCJ6D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ7 =              -97.550         / CAMERA HEAD 7 COLD JUNCTION (FIT)
E_CHCJ7D=                 3362         / 12-BIT TEMPERATURE
E_CHCJ8 =              -98.330         / CAMERA HEAD 8 COLD JUNCTION (FIT)
E_CHCJ8D=                 3362         / 12-BIT TEMPERATURE
COMMENT ########## DATA FROM EXPORT PORT 1 COMMAND ####################
COMMENT NOTE:  THESE DATA ARE CORRUPTED ON ORBIT DUE TO HOW EXPOSURES
COMMENT        ARE COMMANDED.
E_ITIME =                0.110         / Exposure in Seconds
E_ERASE =                    F         / T if Chip Erased
E_EXPOSE=                    F         / T if Exposure
E_AUTOER=                    F         / T if Auto Erase
E_CALIBR=                    F         / T if Calibration Exposure
E_KSPOT =                    F         / T if K Spot
HISTORY = 'Divided by flatfield frame:                                     '
COMMENT ###### PARAMETERS EXTRACTED FROM THE STANDARD HEADER PACKET ####
COMMENT ## NOTE: MUCH MORE DATA EXISTS IN THE STANDARD HEADER PACKET ###
COMMENT ## THAN WHAT IS DECODED HERE.  THE PODPS .SHH FILE CONTAINS  ###
COMMENT ## DECODED VALUES OF ALL STANDARD HEADER PACKET DATA.        ###
CCOL    =                  433
S_ITIME =              300.000         / COMMANDED INTEGRATION TIME
S_ERASE =                    F         / T IF ERASE IN WEXPOCMD
S_SHUTTR=                    T         / T IF SHUTTER IN WEXPOCMD
S_CLEXEC=                    F         / T IF CAL EXEC IN WEXPOCMD
S_KSPOT =                    F         / T IF KSPOTS IN WEXPOCMD
S_AUTOER=                    F         / T IF AUTO ERASE IN WEXPOCMD
S_LMPCAL=                    F         / T IF LAMP/DARK CAL IN WEXPOCMD
S_PROGID= '0BM-01-03'                  / PROGRAM ID - OBS SET - OBS ID
S_APEXEC=                    T         / T IF APPL PROCESSOR CONTROL
S_CMDCAN=                    T         / T IF CANCEL CMD SENT
S_SHOVFL=                    F         / T IF SHUTTER LOG OVERFLOW
S_MODE  =                    F         / T IF MANUAL (SERIALS ON)
S_SHAMAN=                    F         / T IF BLADE A OPEN (MANUAL)
S_SHBMAN=                    F         / T IF BLADE B OPEN (MANUAL)
S_SH_AP =                    T         / T IF SHUTTER BLADE USED BY APPL PROCESS
S_NSHUT =                    1         / NUMBER OF ADDITIONAL SHUTTER CLOSURES
CROW    =                  405
COMMENT ### SELECTED CARDS FROM ASCII HEADER OF STANDARD HEADER PACKET ##
PKTTIME = '13-AUG-1990 04:46:52.42'
TRGTNAME= '3029_1          ' /
TARAQMOD= '03      ' /
OBSERVTN= '03      ' /
OBSET_ID= '01      ' /
DECLMOON=              21.0832 /
RTASMOON=              40.0589 /
DECLNSUN=              14.7115 /
RTASCSUN=              142.731 /
DECLNTRG=              74.5936 /
RTASNTRG=              211.734 /
DECLNV1 =              74.5993 /
RTASCNV1=              211.746 /
REFOBDEC=              74.5936 /
REFOBJPA=         0.000000E+00 /
REFOBJRA=              211.734 /
VELABBRA=              23.5877 /
VELOCSTX=            -0.494504 /
VELOCSTY=              7.31953 /
VELOCSTZ=             -1.80318 /
COMMENT ############ STATISTICS DETERMINED FROM DATA FRAME #############
D_NSAT  =                    3         / NUMBER OF SATURATED PIXELS IN ENTIRE FR
COMMENT STATISTICS OF SAMPLED 8-BIT BIAS VALUES IN ENGINEERING COLUMN
D_8BAVG =              191.553         / AVERAGE VALUE
D_8BRMS =                0.881         / RMS
D_8BLOW =                  189         / LOW DATA VALUE
D_8BHIGH=                  195         / HIGH DATA VALUE
D_BSREGN= 'X0 = 5  Y0 = 3  NX = 9  NY = 797'/ EXTENDED REGISTER BIAS REGION
D_BSAVG =              381.594         / AVERAGE VALUE (SAT, DROP IGNORED)
D_BSRMS =                1.625         / RMS OF REGION
D_BSLOW =                  375         / LOW DATA VALUE
D_BSHIGH=                  389         / HIGH DATA VALUE
D_BSAVG3=              381.585         / AVERAGE AFTER 3 SIGMA REJECTION
D_BSRMS3=                1.596         / RMS AFTER 3 SIGMA REJECTION
D_BSNREJ=                   19         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_BSGOOD=                 6368         / NUMBER OF PIXELS WITH GOOD DATA
D_BSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_BSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_WFREGN= 'X0 = 30  Y0 = 30  NX = 769  NY = 769'/ WHOLE FRAME, EXCLUDING PYR SHA
D_WFAVG =              388.967         / AVERAGE VALUE (SAT, DROP IGNORED)
D_WFRMS =               18.384         / RMS OF REGION
D_WFLOW =                  377         / LOW DATA VALUE
D_WFHIGH=                 3261         / HIGH DATA VALUE
D_WFAVG3=              388.255         / AVERAGE AFTER 3 SIGMA REJECTION
D_WFRMS3=                4.108         / RMS AFTER 3 SIGMA REJECTION
D_WFNREJ=                 2951         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_WFGOOD=               589821         / NUMBER OF PIXELS WITH GOOD DATA
D_WFNSAT=                    3         / NUMBER OF SATURATED PIXELS
D_WFNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LLREGN= 'X0 = 50  Y0 = 50  NX = 101  NY = 101'/ LOWER LEFT CORNER REGION
D_LLAVG =              387.584         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LLRMS =                6.333         / RMS OF REGION
D_LLLOW =                  381         / LOW DATA VALUE
D_LLHIGH=                  901         / HIGH DATA VALUE
D_LLAVG3=              387.442         / AVERAGE AFTER 3 SIGMA REJECTION
D_LLRMS3=                2.152         / RMS AFTER 3 SIGMA REJECTION
D_LLNREJ=                   14         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LLGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LLNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LLNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_ULREGN= 'X0 = 50  Y0 = 650  NX = 101  NY = 101'/ UPPER LEFT CORNER REGION
D_ULAVG =              386.836         / AVERAGE VALUE (SAT, DROP IGNORED)
D_ULRMS =                4.212         / RMS OF REGION
D_ULLOW =                  380         / LOW DATA VALUE
D_ULHIGH=                  692         / HIGH DATA VALUE
D_ULAVG3=              386.738         / AVERAGE AFTER 3 SIGMA REJECTION
D_ULRMS3=                2.123         / RMS AFTER 3 SIGMA REJECTION
D_ULNREJ=                   16         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_ULGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_ULNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_ULNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_LRREGN= 'X0 = 650  Y0 = 50  NX = 101  NY = 101'/ LOWER RIGHT CORNER REGION
D_LRAVG =              386.603         / AVERAGE VALUE (SAT, DROP IGNORED)
D_LRRMS =                4.152         / RMS OF REGION
D_LRLOW =                  377         / LOW DATA VALUE
D_LRHIGH=                  547         / HIGH DATA VALUE
D_LRAVG3=              386.445         / AVERAGE AFTER 3 SIGMA REJECTION
D_LRRMS3=                2.115         / RMS AFTER 3 SIGMA REJECTION
D_LRNREJ=                   30         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_LRGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_LRNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_LRNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_URREGN= 'X0 = 650  Y0 = 650  NX = 101  NY = 101'/ UPPER RIGHT CORNER REGION
D_URAVG =              385.993         / AVERAGE VALUE (SAT, DROP IGNORED)
D_URRMS =                6.171         / RMS OF REGION
D_URLOW =                  379         / LOW DATA VALUE
D_URHIGH=                  855         / HIGH DATA VALUE
D_URAVG3=              385.837         / AVERAGE AFTER 3 SIGMA REJECTION
D_URRMS3=                2.063         / RMS AFTER 3 SIGMA REJECTION
D_URNREJ=                   17         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_URGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_URNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_URNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_CCREGN= 'X0 = 350  Y0 = 350  NX = 101  NY = 101'/ CENTRAL REGION
D_CCAVG =              425.882         / AVERAGE VALUE (SAT, DROP IGNORED)
D_CCRMS =              110.111         / RMS OF REGION
D_CCLOW =                  379         / LOW DATA VALUE
D_CCHIGH=                 3117         / HIGH DATA VALUE
D_CCAVG3=              416.486         / AVERAGE AFTER 3 SIGMA REJECTION
D_CCRMS3=               39.636         / RMS AFTER 3 SIGMA REJECTION
D_CCNREJ=                  110         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_CCGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA
D_CCNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_CCNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
D_PSREGN= 'X0 = 3  Y0 = 3  NX = 15  NY = 791'/ PYRAMID SHADOW
D_PSAVG =              384.141         / AVERAGE VALUE (SAT, DROP IGNORED)
D_PSRMS =                8.785         / RMS OF REGION
D_PSLOW =                  377         / LOW DATA VALUE
D_PSHIGH=                 1105         / HIGH DATA VALUE
D_PSAVG3=              383.915         / AVERAGE AFTER 3 SIGMA REJECTION
D_PSRMS3=                1.997         / RMS AFTER 3 SIGMA REJECTION
D_PSNREJ=                   20         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)
D_PSGOOD=                11060         / NUMBER OF PIXELS WITH GOOD DATA
D_PSNSAT=                    0         / NUMBER OF SATURATED PIXELS
D_PSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS
DISK    = 'FITS                                                            '
BZERO   =  1.3271640625000E+03
BSCALE  =  4.0751308202744E-02
END
 Total FITS keywords found       =    273
 Total FITS keywords to transfer =    273

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-list pcf230w_a.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File pcf230w_a.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                256 lines per band
                256 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:23 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                   16'
VF003='NAXIS   =                    3'
VF004='NAXIS1  =                  256'
VF005='NAXIS2  =                  256'
VF006='NAXIS3  =                    1'
VF007=
'FILENUM =                22510         / FILE NUMBER ASSGND BY READGEISS'
VF008=
'OBJECT  = "3029_1          "           / OBJECT INIT"D WITH TRGTNAME FROM SHP'
VF009=
'ITIME   =              300.000         / ITIME FROM S_ITIME OR E_ITIME'
VF010=
'CAMERA  = "PC      "                   / CAMERA ID FROM ENG DATA - E_CAMERA'
VF011=
'CHIP    =                    6         / CHIP ID FROM ENG DATA - E_CHIP'
VF012=
'FILTER  = "F230W+OPEN"                 / ENG DATA - E_FNAME1 + E_FNAME2'
VF013=
'DATE-OBS= "13/08/90"                   / STD HDR PACKET DATE - PKTTIME'
VF014=
'TIME    = "04:46:52.42"                / STD HDR PACKET TIME - PKTTIME'
VF015=
'EXPTYPE = "EXPOSURE W/ PREFLASH  "     / ATTEMPT TO DETERMINE EXP TYPE'
VF016=
'DATATYPE= "INTEGER*2                                                       "'
VF017='INSTRUME= "WFP     " /'
VF018='ROOTNAME= "W0BM0103B" /'
VF019='CAXIS   =                    3 /'
VF020='CNPIX1  =                  305'
VF021=
'COMMENT ########### CARDS READ FROM GROUP DATA IN GEISS FILE ###########'
VF022='CRVAL1  =  2.1173422216610E+02'
VF023='CRVAL2  =  7.4593559597797E+01'
VF024='CRVAL3  =  7.6190032000000E+07'
VF025='CRPIX1  =  4.1200000000000E+02'
VF026='CRPIX2  =  4.1200000000000E+02'
VF027='CD1_1   =  3.3714873097779E-06'
VF028='CD1_2   = -1.1447220458649E-05'
VF029='CD2_1   = -1.1447220458649E-05'
VF030='CD2_2   = -3.3714873097779E-06'
VF031='DATAMIN =  0.0000000000000E+00'
VF032='DATAMAX = -1.0000000000000E+00'
VF033='MIR REVR=                    T'
VF034='ORIENTAT= -1.0641099548340E+02'
VF035='FILLCNT =                    0'
VF036='ERRCNT  =                    0'
VF037='FPKTTIME= "13-AUG-1990 04:47:12.54"'
VF038='LPKTTIME= "13-AUG-1990 04:47:27.04"'
VF039='CTYPE1  = "RA___TAN"'
VF040='CTYPE2  = "DEC__TAN"'
VF041='CTYPE3  = "TIME    "'
VF042='CNPIX2  =                  277'
VF043=
'COMMENT ## IDT GEISS READER DECODES INFORMATION FROM THE EXTENDED REGISTER #'
VF044=
'COMMENT ## AND STANDARD HEADER PACKET, AND CALCULATES STATISTICS. OTHER    #'
VF045=
'COMMENT ## CARDS FROM DATA OR STANDARD HEADER PACKET HEADERS AS SPECIFIED  #'
VF046=
'COMMENT ## BY COMMENTS.                                                    #'
VF047=
'COMMENT ## CARD CONVENTIONS FOR WF/PC DATA READ FROM GEISS TAPES:          #'
VF048=
'COMMENT ##   E_  -- DATA DECODED FROM EXTENDED REGISTER ENGINEERING DATA   #'
VF049=
'COMMENT ##   S_  -- DATA DECODED FROM STANDARD HEADER PACKET DATA          #'
VF050=
'COMMENT ##   D_  -- STATISTICAL DATA DETERMINED FROM THE FRAME ITSELF	#'
VF051=
'HISTORY = "A-D correction done with file:                                  "'
VF052=
'HISTORY = "Subtracted bias level of:   382.66                              "'
VF053=
'COMMENT ########### DATA OBTAINED FROM ENGINEERING IN SCIENCE ##########'
VF054=
'HISTORY = "Subtracted preflash frome:                                      "'
VF055='E_DROP  =                    0         / ROWS LOST TO DATA DROPOUTS'
VF056='E_CAMERA= "PC      "                   / CAMERA (WFC OR PC)'
VF057='E_CHIP  =                    6         / Chip number'
VF058='E_BLADEA= "OPEN    "                   / Position of Shutter Blade A'
VF059='E_BLADEB= "CLOSED  "                   / Position of Shutter Blade B'
VF060='E_CLRLST= "1 2 3 5 6 7 8 9 10 11 12 "  / List of clear filters'
VF061='E_FILT1 = "4-C     "                   / Filter 1 Position'
VF062='E_FNAME1= "F230W   "                   / Filter 1 Name'
VF063='E_FILT2 = "OPEN    "                   / Filter 2 Position'
VF064='E_FNAME2= "OPEN    "                   / Filter 2 Name'
VF065=
'COMMENT ########### CAMERA HEAD ELECTRONICS TEMPERATURES ###############'
VF066=
'COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE'
VF067='COMMENT CALIBRATION DATA ARE GIVEN.  ALL TEMPERATURES ARE DEGREES C.'
VF068=
'E_CHET1 =               10.020         / CAMERA HEAD 1 ELEC BOARD (FIT)'
VF069='E_CHET1D=                 2628         / 12-BIT TEMPERATURE'
VF070=
'E_CHET2 =               10.020         / CAMERA HEAD 2 ELEC BOARD (FIT)'
VF071='E_CHET2D=                 2628         / 12-BIT TEMPERATURE'
VF072=
'E_CHET3 =                9.650         / CAMERA HEAD 3 ELEC BOARD (FIT)'
VF073='E_CHET3D=                 2611         / 12-BIT TEMPERATURE'
VF074=
'E_CHET4 =                9.650         / CAMERA HEAD 4 ELEC BOARD (FIT)'
VF075='E_CHET4D=                 2611         / 12-BIT TEMPERATURE'
VF076=
'E_CHET5 =               10.020         / CAMERA HEAD 5 ELEC BOARD (FIT)'
VF077='E_CHET5D=                 2628         / 12-BIT TEMPERATURE'
VF078=
'E_CHET6 =               10.770         / CAMERA HEAD 6 ELEC BOARD (FIT)'
VF079='E_CHET6D=                 2662         / 12-BIT TEMPERATURE'
VF080=
'E_CHET7 =               10.400         / CAMERA HEAD 7 ELEC BOARD (FIT)'
VF081='E_CHET7D=                 2645         / 12-BIT TEMPERATURE'
VF082=
'E_CHET8 =               10.020         / CAMERA HEAD 8 ELEC BOARD (FIT)'
VF083='E_CHET8D=                 2628         / 12-BIT TEMPERATURE'
VF084=
'COMMENT ########### HOT JUNCTION TEMPERATURES #########################'
VF085='COMMENT NOTE: TEMPERATURES FOR CHIPS 1 AND 7 NOT AVAILABLE'
VF086=
'E_CHHJ2 =              -20.470         / CAMERA HEAD 2 HOT JUNCTION (FIT)'
VF087='E_CHHJ2D=                 1075         / 12-BIT TEMPERATURE'
VF088=
'E_CHHJ3 =              -28.220         / CAMERA HEAD 3 HOT JUNCTION (FIT)'
VF089='E_CHHJ3D=                  750         / 12-BIT TEMPERATURE'
VF090=
'E_CHHJ4 =              -28.680         / CAMERA HEAD 4 HOT JUNCTION (FIT)'
VF091='E_CHHJ4D=                  733         / 12-BIT TEMPERATURE'
VF092=
'E_CHHJ5 =              -16.260         / CAMERA HEAD 5 HOT JUNCTION (FIT)'
VF093='E_CHHJ5D=                 1280         / 12-BIT TEMPERATURE'
VF094=
'E_CHHJ6 =              -13.610         / CAMERA HEAD 6 HOT JUNCTION (FIT)'
VF095='E_CHHJ6D=                 1416         / 12-BIT TEMPERATURE'
VF096=
'E_CHHJ8 =              -31.110         / CAMERA HEAD 8 HOT JUNCTION (FIT)'
VF097='E_CHHJ8D=                  648         / 12-BIT TEMPERATURE'
VF098=
'COMMENT ######### OPTICAL BENCH TEMPERATURES ##########################'
VF099=
'E_OPBCH =                9.290         / OPTICAL BENCH CAMERA HEAD T (FIT)'
VF100='E_OPBCHD=                 2594         / 12-BIT TEMPERATURE'
VF101=
'E_OPBFM =               10.020         / OPTICAL BENCH FOLD MIRROR T (FIT)'
VF102='E_OPBFMD=                 2628         / 12-BIT TEMPERATURE'
VF103=
'COMMENT ########## COLD JUNCTION TEMPERATURES #########################'
VF104=
'COMMENT BOTH DATA NUMBERS AND TEMPERATURES DETERMINED FROM A FIT TO THE'
VF105='COMMENT CALIBRATION DATA ARE GIVEN.'
VF106=
'E_CHCJ1 =              -97.260         / CAMERA HEAD 1 COLD JUNCTION (FIT)'
VF107='E_CHCJ1D=                 3362         / 12-BIT TEMPERATURE'
VF108=
'E_CHCJ2 =              -97.640         / CAMERA HEAD 2 COLD JUNCTION (FIT)'
VF109='E_CHCJ2D=                 3362         / 12-BIT TEMPERATURE'
VF110=
'E_CHCJ3 =              -97.860         / CAMERA HEAD 3 COLD JUNCTION (FIT)'
VF111='E_CHCJ3D=                 3362         / 12-BIT TEMPERATURE'
VF112=
'E_CHCJ4 =              -97.610         / CAMERA HEAD 4 COLD JUNCTION (FIT)'
VF113='E_CHCJ4D=                 3362         / 12-BIT TEMPERATURE'
VF114=
'E_CHCJ5 =              -97.960         / CAMERA HEAD 5 COLD JUNCTION (FIT)'
VF115='E_CHCJ5D=                 3362         / 12-BIT TEMPERATURE'
VF116=
'E_CHCJ6 =              -98.200         / CAMERA HEAD 6 COLD JUNCTION (FIT)'
VF117='E_CHCJ6D=                 3362         / 12-BIT TEMPERATURE'
VF118=
'E_CHCJ7 =              -97.550         / CAMERA HEAD 7 COLD JUNCTION (FIT)'
VF119='E_CHCJ7D=                 3362         / 12-BIT TEMPERATURE'
VF120=
'E_CHCJ8 =              -98.330         / CAMERA HEAD 8 COLD JUNCTION (FIT)'
VF121='E_CHCJ8D=                 3362         / 12-BIT TEMPERATURE'
VF122=
'COMMENT ########## DATA FROM EXPORT PORT 1 COMMAND ####################'
VF123='COMMENT NOTE:  THESE DATA ARE CORRUPTED ON ORBIT DUE TO HOW EXPOSURES'
VF124='COMMENT        ARE COMMANDED.'
VF125='E_ITIME =                0.110         / Exposure in Seconds'
VF126='E_ERASE =                    F         / T if Chip Erased'
VF127='E_EXPOSE=                    F         / T if Exposure'
VF128='E_AUTOER=                    F         / T if Auto Erase'
VF129='E_CALIBR=                    F         / T if Calibration Exposure'
VF130='E_KSPOT =                    F         / T if K Spot'
VF131=
'HISTORY = "Divided by flatfield frame:                                     "'
VF132=
'COMMENT ###### PARAMETERS EXTRACTED FROM THE STANDARD HEADER PACKET ####'
VF133=
'COMMENT ## NOTE: MUCH MORE DATA EXISTS IN THE STANDARD HEADER PACKET ###'
VF134=
'COMMENT ## THAN WHAT IS DECODED HERE.  THE PODPS .SHH FILE CONTAINS  ###'
VF135=
'COMMENT ## DECODED VALUES OF ALL STANDARD HEADER PACKET DATA.        ###'
VF136='CCOL    =                  433'
VF137='S_ITIME =              300.000         / COMMANDED INTEGRATION TIME'
VF138='S_ERASE =                    F         / T IF ERASE IN WEXPOCMD'
VF139='S_SHUTTR=                    T         / T IF SHUTTER IN WEXPOCMD'
VF140='S_CLEXEC=                    F         / T IF CAL EXEC IN WEXPOCMD'
VF141='S_KSPOT =                    F         / T IF KSPOTS IN WEXPOCMD'
VF142='S_AUTOER=                    F         / T IF AUTO ERASE IN WEXPOCMD'
VF143=
'S_LMPCAL=                    F         / T IF LAMP/DARK CAL IN WEXPOCMD'
VF144=
'S_PROGID= "0BM-01-03"                  / PROGRAM ID - OBS SET - OBS ID'
VF145='S_APEXEC=                    T         / T IF APPL PROCESSOR CONTROL'
VF146='S_CMDCAN=                    T         / T IF CANCEL CMD SENT'
VF147='S_SHOVFL=                    F         / T IF SHUTTER LOG OVERFLOW'
VF148='S_MODE  =                    F         / T IF MANUAL (SERIALS ON)'
VF149='S_SHAMAN=                    F         / T IF BLADE A OPEN (MANUAL)'
VF150='S_SHBMAN=                    F         / T IF BLADE B OPEN (MANUAL)'
VF151=
'S_SH_AP =                    T         / T IF SHUTTER BLADE USED BY APPL PROCESS'
VF152=
'S_NSHUT =                    1         / NUMBER OF ADDITIONAL SHUTTER CLOSURES'
VF153='CROW    =                  405'
VF154=
'COMMENT ### SELECTED CARDS FROM ASCII HEADER OF STANDARD HEADER PACKET ##'
VF155='PKTTIME = "13-AUG-1990 04:46:52.42"'
VF156='TRGTNAME= "3029_1          " /'
VF157='TARAQMOD= "03      " /'
VF158='OBSERVTN= "03      " /'
VF159='OBSET_ID= "01      " /'
VF160='DECLMOON=              21.0832 /'
VF161='RTASMOON=              40.0589 /'
VF162='DECLNSUN=              14.7115 /'
VF163='RTASCSUN=              142.731 /'
VF164='DECLNTRG=              74.5936 /'
VF165='RTASNTRG=              211.734 /'
VF166='DECLNV1 =              74.5993 /'
VF167='RTASCNV1=              211.746 /'
VF168='REFOBDEC=              74.5936 /'
VF169='REFOBJPA=         0.000000E+00 /'
VF170='REFOBJRA=              211.734 /'
VF171='VELABBRA=              23.5877 /'
VF172='VELOCSTX=            -0.494504 /'
VF173='VELOCSTY=              7.31953 /'
VF174='VELOCSTZ=             -1.80318 /'
VF175=
'COMMENT ############ STATISTICS DETERMINED FROM DATA FRAME #############'
VF176=
'D_NSAT  =                    3         / NUMBER OF SATURATED PIXELS IN ENTIRE FR'
VF177='COMMENT STATISTICS OF SAMPLED 8-BIT BIAS VALUES IN ENGINEERING COLUMN'
VF178='D_8BAVG =              191.553         / AVERAGE VALUE'
VF179='D_8BRMS =                0.881         / RMS'
VF180='D_8BLOW =                  189         / LOW DATA VALUE'
VF181='D_8BHIGH=                  195         / HIGH DATA VALUE'
VF182=
'D_BSREGN= "X0 = 5  Y0 = 3  NX = 9  NY = 797"/ EXTENDED REGISTER BIAS REGION'
VF183=
'D_BSAVG =              381.594         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF184='D_BSRMS =                1.625         / RMS OF REGION'
VF185='D_BSLOW =                  375         / LOW DATA VALUE'
VF186='D_BSHIGH=                  389         / HIGH DATA VALUE'
VF187=
'D_BSAVG3=              381.585         / AVERAGE AFTER 3 SIGMA REJECTION'
VF188='D_BSRMS3=                1.596         / RMS AFTER 3 SIGMA REJECTION'
VF189=
'D_BSNREJ=                   19         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF190=
'D_BSGOOD=                 6368         / NUMBER OF PIXELS WITH GOOD DATA'
VF191='D_BSNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF192=
'D_BSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF193=
'D_WFREGN= "X0 = 30  Y0 = 30  NX = 769  NY = 769"/ WHOLE FRAME, EXCLUDING PYR SHA'
VF194=
'D_WFAVG =              388.967         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF195='D_WFRMS =               18.384         / RMS OF REGION'
VF196='D_WFLOW =                  377         / LOW DATA VALUE'
VF197='D_WFHIGH=                 3261         / HIGH DATA VALUE'
VF198=
'D_WFAVG3=              388.255         / AVERAGE AFTER 3 SIGMA REJECTION'
VF199='D_WFRMS3=                4.108         / RMS AFTER 3 SIGMA REJECTION'
VF200=
'D_WFNREJ=                 2951         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF201=
'D_WFGOOD=               589821         / NUMBER OF PIXELS WITH GOOD DATA'
VF202='D_WFNSAT=                    3         / NUMBER OF SATURATED PIXELS'
VF203=
'D_WFNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF204=
'D_LLREGN= "X0 = 50  Y0 = 50  NX = 101  NY = 101"/ LOWER LEFT CORNER REGION'
VF205=
'D_LLAVG =              387.584         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF206='D_LLRMS =                6.333         / RMS OF REGION'
VF207='D_LLLOW =                  381         / LOW DATA VALUE'
VF208='D_LLHIGH=                  901         / HIGH DATA VALUE'
VF209=
'D_LLAVG3=              387.442         / AVERAGE AFTER 3 SIGMA REJECTION'
VF210='D_LLRMS3=                2.152         / RMS AFTER 3 SIGMA REJECTION'
VF211=
'D_LLNREJ=                   14         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF212=
'D_LLGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA'
VF213='D_LLNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF214=
'D_LLNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF215=
'D_ULREGN= "X0 = 50  Y0 = 650  NX = 101  NY = 101"/ UPPER LEFT CORNER REGION'
VF216=
'D_ULAVG =              386.836         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF217='D_ULRMS =                4.212         / RMS OF REGION'
VF218='D_ULLOW =                  380         / LOW DATA VALUE'
VF219='D_ULHIGH=                  692         / HIGH DATA VALUE'
VF220=
'D_ULAVG3=              386.738         / AVERAGE AFTER 3 SIGMA REJECTION'
VF221='D_ULRMS3=                2.123         / RMS AFTER 3 SIGMA REJECTION'
VF222=
'D_ULNREJ=                   16         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF223=
'D_ULGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA'
VF224='D_ULNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF225=
'D_ULNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF226=
'D_LRREGN= "X0 = 650  Y0 = 50  NX = 101  NY = 101"/ LOWER RIGHT CORNER REGION'
VF227=
'D_LRAVG =              386.603         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF228='D_LRRMS =                4.152         / RMS OF REGION'
VF229='D_LRLOW =                  377         / LOW DATA VALUE'
VF230='D_LRHIGH=                  547         / HIGH DATA VALUE'
VF231=
'D_LRAVG3=              386.445         / AVERAGE AFTER 3 SIGMA REJECTION'
VF232='D_LRRMS3=                2.115         / RMS AFTER 3 SIGMA REJECTION'
VF233=
'D_LRNREJ=                   30         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF234=
'D_LRGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA'
VF235='D_LRNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF236=
'D_LRNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF237=
'D_URREGN= "X0 = 650  Y0 = 650  NX = 101  NY = 101"/ UPPER RIGHT CORNER REGION'
VF238=
'D_URAVG =              385.993         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF239='D_URRMS =                6.171         / RMS OF REGION'
VF240='D_URLOW =                  379         / LOW DATA VALUE'
VF241='D_URHIGH=                  855         / HIGH DATA VALUE'
VF242=
'D_URAVG3=              385.837         / AVERAGE AFTER 3 SIGMA REJECTION'
VF243='D_URRMS3=                2.063         / RMS AFTER 3 SIGMA REJECTION'
VF244=
'D_URNREJ=                   17         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF245=
'D_URGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA'
VF246='D_URNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF247=
'D_URNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF248='D_CCREGN= "X0 = 350  Y0 = 350  NX = 101  NY = 101"/ CENTRAL REGION'
VF249=
'D_CCAVG =              425.882         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF250='D_CCRMS =              110.111         / RMS OF REGION'
VF251='D_CCLOW =                  379         / LOW DATA VALUE'
VF252='D_CCHIGH=                 3117         / HIGH DATA VALUE'
VF253=
'D_CCAVG3=              416.486         / AVERAGE AFTER 3 SIGMA REJECTION'
VF254='D_CCRMS3=               39.636         / RMS AFTER 3 SIGMA REJECTION'
VF255=
'D_CCNREJ=                  110         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF256=
'D_CCGOOD=                10000         / NUMBER OF PIXELS WITH GOOD DATA'
VF257='D_CCNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF258=
'D_CCNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF259='D_PSREGN= "X0 = 3  Y0 = 3  NX = 15  NY = 791"/ PYRAMID SHADOW'
VF260=
'D_PSAVG =              384.141         / AVERAGE VALUE (SAT, DROP IGNORED)'
VF261='D_PSRMS =                8.785         / RMS OF REGION'
VF262='D_PSLOW =                  377         / LOW DATA VALUE'
VF263='D_PSHIGH=                 1105         / HIGH DATA VALUE'
VF264=
'D_PSAVG3=              383.915         / AVERAGE AFTER 3 SIGMA REJECTION'
VF265='D_PSRMS3=                1.997         / RMS AFTER 3 SIGMA REJECTION'
VF266=
'D_PSNREJ=                   20         / NUMBER OF PIXELS REJECTED (> 3*SIGMA)'
VF267=
'D_PSGOOD=                11060         / NUMBER OF PIXELS WITH GOOD DATA'
VF268='D_PSNSAT=                    0         / NUMBER OF SATURATED PIXELS'
VF269=
'D_PSNDRP=                    0         / NUMBER OF PIXELS LOST TO DROPOUTS'
VF270=
'DISK    = "FITS                                                            "'
VF271='BZERO   =  1.3271640625000E+03'
VF272='BSCALE  =  4.0751308202744E-02'
VF273='END'
 
************************************************************
list pcf230w_a.vic sl=1 ss=1 nl=10 ns=10
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:23 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32599-32572-32583-32648-32619-32495-32563-32596-32617-32570
      2    -32572-32491-32548-32508-32671-32598-32606-32454-32626-32578
      3    -32576-32600-32548-32481-32597-32516-32604-32550-32557-32506
      4    -32510-32528-32582-32636-32579-32447-32675-32582-32459-32646
      5    -32643-32568-32543-32651-32626-32539-32548-32546-32496-32536
      6    -32572-32468-32668-32544-32679-32461-32607-32532-32561-32477
      7    -32497-32518-32572-32548-32604-32654-32528-32567-32615-32557
      8    -32580-32462-32479-32636-32567-32512-32515-32487-32568-32571
      9    -32558-32537-32582-32560-32487-32597-32551-32522-32629-32637
     10    -32629-32542-32528-32533-32572-32633-32575-32676-32650-32553
fitsin tt/p1.fits
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - BLOCKED = T, TAPE records may be blocked
Processing will be attempted anyway...
??W - EXTEND = T, Data may contain FITS extensions
SIMPLE  =                    T
BITPIX  =                  -32
NAXIS   =                    3        / # OF AXES
NAXIS1  =                  500
NAXIS2  =                  500
NAXIS3  =                    1
BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880
EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS
BUNIT   = 'MJY/SR  '                  / INTENSITY
CRVAL1  =      0.000000000E+00        / RA AT ORIGIN (DEGREES)
CRPIX1  =                 250.
CTYPE1  = 'RA---TAN'                  /  DECREASES IN VALUE AS SAMPLE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT1  =     -2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON SAMPLE AXIS
CRVAL2  =      7.000000000E+01        /  DEC AT ORIGIN (DEGREES)
CRPIX2  =                 250.
CTYPE2  = 'DEC--TAN'                  /  DECREASES IN VALUE AS LINE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT2  =      2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON LINE AXIS
CRVAL3  =      1.200000000E-05        /  WAVELENGTH IN METERS
CRPIX3  =                   1.
CTYPE3  = 'LAMBDA  '                  /
CDELT3  =      0.000000000E+00        /
EPOCH   =      1.950000000E+03        /  EME50
DATE-MAP= '90/08/26'                  /  MAP RELEASE DATE (YY/MM/DD)
DATE    = '90/08/26'                  /  DATE TAPE WRITTEN(YY/MM/DD)
ORIGIN  = 'JPL-IPAC'                  /  INSTITUTION
TELESCOP= 'IRAS    '                  /
INSTRUME= 'SSFPLATE'                  /  IRAS SUPER SKYFLUX PLATE
OBJECT  = 'p408h000'                  /  PLATE NUMBER-HCON
PROJTYPE= 'GNOMONIC'                  /  PROJECTION TYPE
EDITTED =                    T        /  SCANS EDITTED
APPCAL  =                    T        /  CALIBRATION CORRECTION 25 MICRONS
DE-ZODY =                    T        /  DE-ZODIED IMAGE
GLBL D  =                    T        /  APPLIED GLOBAL PARAMETERS
LOCAL D =                    T        /  LOCAL DESTRIPER
CABLANK =                    T        /  COMET/ASTEROID BLANKING
COMMENT                               /
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
HISTORY *0 (imported)
HISTORY *1  901129.1625 (PAR vers: 90921.1025 ) IPAC
HISTORY Flattened, planar subtraction (IPAC flatten V1.1)
HISTORY *1 imarith1 901130.0957 (imarith1.c 1.1 90/07/18) IPAC
HISTORY * IN=flat7822.1.fits OUT=p1 OP=pow C=.2
END
fitsin tt/p1.fits header=p1.hdr
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - BLOCKED = T, TAPE records may be blocked
Processing will be attempted anyway...
??W - EXTEND = T, Data may contain FITS extensions
SIMPLE  =                    T
BITPIX  =                  -32
NAXIS   =                    3        / # OF AXES
NAXIS1  =                  500
NAXIS2  =                  500
NAXIS3  =                    1
BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880
EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS
BUNIT   = 'MJY/SR  '                  / INTENSITY
CRVAL1  =      0.000000000E+00        / RA AT ORIGIN (DEGREES)
CRPIX1  =                 250.
CTYPE1  = 'RA---TAN'                  /  DECREASES IN VALUE AS SAMPLE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT1  =     -2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON SAMPLE AXIS
CRVAL2  =      7.000000000E+01        /  DEC AT ORIGIN (DEGREES)
CRPIX2  =                 250.
CTYPE2  = 'DEC--TAN'                  /  DECREASES IN VALUE AS LINE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT2  =      2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON LINE AXIS
CRVAL3  =      1.200000000E-05        /  WAVELENGTH IN METERS
CRPIX3  =                   1.
CTYPE3  = 'LAMBDA  '                  /
CDELT3  =      0.000000000E+00        /
EPOCH   =      1.950000000E+03        /  EME50
DATE-MAP= '90/08/26'                  /  MAP RELEASE DATE (YY/MM/DD)
DATE    = '90/08/26'                  /  DATE TAPE WRITTEN(YY/MM/DD)
ORIGIN  = 'JPL-IPAC'                  /  INSTITUTION
TELESCOP= 'IRAS    '                  /
INSTRUME= 'SSFPLATE'                  /  IRAS SUPER SKYFLUX PLATE
OBJECT  = 'p408h000'                  /  PLATE NUMBER-HCON
PROJTYPE= 'GNOMONIC'                  /  PROJECTION TYPE
EDITTED =                    T        /  SCANS EDITTED
APPCAL  =                    T        /  CALIBRATION CORRECTION 25 MICRONS
DE-ZODY =                    T        /  DE-ZODIED IMAGE
GLBL D  =                    T        /  APPLIED GLOBAL PARAMETERS
LOCAL D =                    T        /  LOCAL DESTRIPER
CABLANK =                    T        /  COMET/ASTEROID BLANKING
COMMENT                               /
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
HISTORY *0 (imported)
HISTORY *1  901129.1625 (PAR vers: 90921.1025 ) IPAC
HISTORY Flattened, planar subtraction (IPAC flatten V1.1)
HISTORY *1 imarith1 901130.0957 (imarith1.c 1.1 90/07/18) IPAC
HISTORY * IN=flat7822.1.fits OUT=p1 OP=pow C=.2
END
ush cat p1.hdr
fitsin tt/p1.fits p1.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - BLOCKED = T, TAPE records may be blocked
Processing will be attempted anyway...
??W - EXTEND = T, Data may contain FITS extensions
SIMPLE  =                    T
BITPIX  =                  -32
NAXIS   =                    3        / # OF AXES
NAXIS1  =                  500
NAXIS2  =                  500
NAXIS3  =                    1
BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880
EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS
BUNIT   = 'MJY/SR  '                  / INTENSITY
CRVAL1  =      0.000000000E+00        / RA AT ORIGIN (DEGREES)
CRPIX1  =                 250.
CTYPE1  = 'RA---TAN'                  /  DECREASES IN VALUE AS SAMPLE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT1  =     -2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON SAMPLE AXIS
CRVAL2  =      7.000000000E+01        /  DEC AT ORIGIN (DEGREES)
CRPIX2  =                 250.
CTYPE2  = 'DEC--TAN'                  /  DECREASES IN VALUE AS LINE
COMMENT                               /  INDEX INCREASES (GNOMONIC)
CDELT2  =      2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL
COMMENT                               /  AT ORIGIN ON LINE AXIS
CRVAL3  =      1.200000000E-05        /  WAVELENGTH IN METERS
CRPIX3  =                   1.
CTYPE3  = 'LAMBDA  '                  /
CDELT3  =      0.000000000E+00        /
EPOCH   =      1.950000000E+03        /  EME50
DATE-MAP= '90/08/26'                  /  MAP RELEASE DATE (YY/MM/DD)
DATE    = '90/08/26'                  /  DATE TAPE WRITTEN(YY/MM/DD)
ORIGIN  = 'JPL-IPAC'                  /  INSTITUTION
TELESCOP= 'IRAS    '                  /
INSTRUME= 'SSFPLATE'                  /  IRAS SUPER SKYFLUX PLATE
OBJECT  = 'p408h000'                  /  PLATE NUMBER-HCON
PROJTYPE= 'GNOMONIC'                  /  PROJECTION TYPE
EDITTED =                    T        /  SCANS EDITTED
APPCAL  =                    T        /  CALIBRATION CORRECTION 25 MICRONS
DE-ZODY =                    T        /  DE-ZODIED IMAGE
GLBL D  =                    T        /  APPLIED GLOBAL PARAMETERS
LOCAL D =                    T        /  LOCAL DESTRIPER
CABLANK =                    T        /  COMET/ASTEROID BLANKING
COMMENT                               /
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
COMMENT
HISTORY *0 (imported)
HISTORY *1  901129.1625 (PAR vers: 90921.1025 ) IPAC
HISTORY Flattened, planar subtraction (IPAC flatten V1.1)
HISTORY *1 imarith1 901130.0957 (imarith1.c 1.1 90/07/18) IPAC
HISTORY * IN=flat7822.1.fits OUT=p1 OP=pow C=.2
END
 Total FITS keywords found       =     52
 Total FITS keywords to transfer =     52

label-list p1.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File p1.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                500 lines per band
                500 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:24 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                  -32'
VF003='NAXIS   =                    3        / # OF AXES'
VF004='NAXIS1  =                  500'
VF005='NAXIS2  =                  500'
VF006='NAXIS3  =                    1'
VF007=
'BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880'
VF008=
'EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS'
VF009='BUNIT   = "MJY/SR  "                  / INTENSITY'
VF010='CRVAL1  =      0.000000000E+00        / RA AT ORIGIN (DEGREES)'
VF011='CRPIX1  =                 250.'
VF012='CTYPE1  = "RA---TAN"                  /  DECREASES IN VALUE AS SAMPLE'
VF013='COMMENT                               /  INDEX INCREASES (GNOMONIC)'
VF014='CDELT1  =     -2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL'
VF015='COMMENT                               /  AT ORIGIN ON SAMPLE AXIS'
VF016='CRVAL2  =      7.000000000E+01        /  DEC AT ORIGIN (DEGREES)'
VF017='CRPIX2  =                 250.'
VF018='CTYPE2  = "DEC--TAN"                  /  DECREASES IN VALUE AS LINE'
VF019='COMMENT                               /  INDEX INCREASES (GNOMONIC)'
VF020='CDELT2  =      2.500000000E-02        /  COORD VALUE INCR DEG/PIXEL'
VF021='COMMENT                               /  AT ORIGIN ON LINE AXIS'
VF022='CRVAL3  =      1.200000000E-05        /  WAVELENGTH IN METERS'
VF023='CRPIX3  =                   1.'
VF024='CTYPE3  = "LAMBDA  "                  /'
VF025='CDELT3  =      0.000000000E+00        /'
VF026='EPOCH   =      1.950000000E+03        /  EME50'
VF027='DATE-MAP= "90/08/26"                  /  MAP RELEASE DATE (YY/MM/DD)'
VF028='DATE    = "90/08/26"                  /  DATE TAPE WRITTEN(YY/MM/DD)'
VF029='ORIGIN  = "JPL-IPAC"                  /  INSTITUTION'
VF030='TELESCOP= "IRAS    "                  /'
VF031='INSTRUME= "SSFPLATE"                  /  IRAS SUPER SKYFLUX PLATE'
VF032='OBJECT  = "p408h000"                  /  PLATE NUMBER-HCON'
VF033='PROJTYPE= "GNOMONIC"                  /  PROJECTION TYPE'
VF034='EDITTED =                    T        /  SCANS EDITTED'
VF035=
'APPCAL  =                    T        /  CALIBRATION CORRECTION 25 MICRONS'
VF036='DE-ZODY =                    T        /  DE-ZODIED IMAGE'
VF037='GLBL D  =                    T        /  APPLIED GLOBAL PARAMETERS'
VF038='LOCAL D =                    T        /  LOCAL DESTRIPER'
VF039='CABLANK =                    T        /  COMET/ASTEROID BLANKING'
VF040='COMMENT                               /'
VF041='COMMENT'
VF042='COMMENT'
VF043='COMMENT'
VF044='COMMENT'
VF045='COMMENT'
VF046='COMMENT'
VF047='HISTORY *0 (imported)'
VF048='HISTORY *1  901129.1625 (PAR vers: 90921.1025 ) IPAC'
VF049='HISTORY Flattened, planar subtraction (IPAC flatten V1.1)'
VF050='HISTORY *1 imarith1 901130.0957 (imarith1.c 1.1 90/07/18) IPAC'
VF051='HISTORY * IN=flat7822.1.fits OUT=p1 OP=pow C=.2'
VF052='END'
 
************************************************************
list p1.vic sl=1 ss=1 nl=6 ns=6
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:24 2016
     Samp             1           2           3           4           5           6
   Line
      1      -8.508E-01  -8.649E-01  -8.500E-01  -8.596E-01  -8.581E-01  -8.688E-01
      2      -8.176E-01  -8.451E-01  -8.401E-01  -8.444E-01  -8.510E-01  -8.403E-01
      3      -8.039E-01  -7.780E-01  -8.315E-01  -8.428E-01  -8.462E-01  -8.511E-01
      4      -7.440E-01  -7.561E-01  -7.850E-01  -8.369E-01  -8.454E-01  -8.317E-01
      5      -7.770E-01  -7.385E-01  -7.980E-01  -8.361E-01  -8.335E-01  -8.162E-01
      6      -7.735E-01  -7.429E-01  -7.564E-01  -7.992E-01  -8.264E-01  -8.283E-01
fitsin ct/psf2x_l10_filter5.fits psf.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T
BITPIX  =                  -32
NAXIS   =                    2
NAXIS1  =                 1024
NAXIS2  =                 1024
BZERO   =                    0
CREATOR = 'Matlab  '
END
 Total FITS keywords found       =      8
 Total FITS keywords to transfer =      8

label-li psf.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File psf.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                1024 lines per band
                1024 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:24 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                  -32'
VF003='NAXIS   =                    2'
VF004='NAXIS1  =                 1024'
VF005='NAXIS2  =                 1024'
VF006='BZERO   =                    0'
VF007='CREATOR = "Matlab  "'
VF008='END'
 
************************************************************
list psf.vic sl=1 ss=1 nl=6 ns=6
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:24 2016
     Samp             1           2           3           4           5           6
   Line
      1       9.586E-09   1.028E-08   1.116E-08   1.216E-08   1.329E-08   1.453E-08
      2       9.314E-09   9.597E-09   1.035E-08   1.127E-08   1.225E-08   1.334E-08
      3       9.658E-09   9.362E-09   9.822E-09   1.062E-08   1.144E-08   1.228E-08
      4       1.059E-08   9.565E-09   9.436E-09   9.955E-09   1.067E-08   1.137E-08
      5       1.180E-08   1.012E-08   9.192E-09   9.181E-09   9.745E-09   1.045E-08
      6       1.287E-08   1.099E-08   9.398E-09   8.695E-09   8.871E-09   9.511E-09
fitsin tt/af784.fits af784.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T / Written by IDL:   8-Aug-1994 14:31:31.00
BITPIX  =                  -64 / IEEE double precision floating point
NAXIS   =                    2 / NUMBER OF AXES
NAXIS1  =                  666 /Number of positions along axis 1
NAXIS2  =                  606 /Number of positions along axis 2
BSCALE  =   1.000000000000e+00 / VALUE= DATA*BSCALE+BZERO
BZERO   =   0.000000000000e+00 /
IRAFNAME= 'i940721a.784'
INSTRUME=  'MIRAC2: Mid-IR Array Camera' / INSTRUMENT
TIME-OBS=  '05:56:53.05'       / TIME OF OBSERVATION (UT)
TIME-END=  '05:56:55.09'       / END TIME OF OBSERVATION (UT)
OBJECT  = 'JUPITER'
DATE-OBS=  '07/21/94'          / DATE OF OBSERVATION (UT)
TELESCOP= 'IRTF'
RA      =   2.133947083333e+02 / RIGHT ASCENSION (DEGREES)
DEC     =  -1.224700000000e+01 / DECLINATION (DEGREES)
EPOCH   =   1.950000000000e+03 / EPOCH FOR RA AND DEC
LAMBDA  =   1.220257399837e-05 / WAVELENGTH (METERS)
FILTER1 =   0.000000000000e+00 / POSITION OF FW 1
FILTER2 =   1.000000000000e+01 / POSITION OF FW 2
OBSERVAT=          'Mauna Kea' / LOCATION OF OBSERVATION
SOFTVER =   4.119999885559e+00 / VERSION NUMBER FOR MIRAC SOFTWARE
CHOPFREQ=   2.543131589890e+00 / CHOP FREQUENCY (HZ)
FRAMERAT=   3.276799991727e-02 / FRAME RATE (HZ)
ITIME   =   1.966079950333e+00 / ON-SOURCE INT. TIME
NODWAITT=   2.000000000000e+00 / ON-SRC NOD WAIT TIME (SEC)
NODOFFWT=   1.000000000000e+00 / NOD WAIT TIME FOR OFF SRC
MAGNIFIC=   4.050000011921e-01 / MAGNIFICATION SETTING
DETBIAS =   3.000000000000e+00 / DETECTOR BIAS VOLTS
HEATERV =   1.805720448494e+00 / HEATER VOLTAGE
DETTEMP =   6.983307838440e+00 / DETECTOR TEMP. (K)
TOTCOADD=                   60 / TOTAL COADDS (ON-SRC)
TELESCLE=   2.000000000000e+00 / PIXEL SCALE (ARCSEC/MM)
OFFSET1 =   0.000000000000e+00 / X OFFSET (PIXELS)
OFFSET2 =   0.000000000000e+00 / Y OFFSET (PIXELS)
ARRAYXSZ=                  128 / X ARRAY SIZE
ARRAYYSZ=                  128 / Y ARRAY SIZE
MASKFILE= 'o:ibad78v.msk'
FILENAME= 'c940721a.784'
GAINFILE= 'o:785e.gan'
SAMPMODE=                    0 / 0 = SINGLE SAMPLE MODE
DELAYCYC=                    0 / DELAY CYCLES
CLOCKRAT=                    5 / CLOCK FREQUENCY (15^6/2^N HZ)
COPERCHP=                    1 / COIMAGES PER CHOP CYCLE
FRPERCOI=                    5 / FRAMES PER COIMAGE
CHOPWTFR=                    0 / CHOPPER WAIT FRAMES
CHOPWAIT=   3.276800155640e+01 / CHOP WAIT TIME (MSEC)
NUMCHOPS=                    0 / NUMBER OF CHOP CYCLES
SKYFLUX =   9.308472290039e+02 / SKY FLUX (AVG CNTS OFF-SRC)
SKYRMS  =   0.000000000000e+00 / RMS OF LAST 5 SKY FLUXES
SKYDIF  =   0.000000000000e+00 / DIFFERENCE FROM LAST SKY FLUX
ERTEMP1 =   3.924659013748e+00 / AMBIENT TEMP (C)
ERTEMP2 =   2.064120101929e+01 / TEMP INSIDE CAMERA ELEC. (C)
ERTEMP3 =   4.128957748413e+01 / TEMP OF A/D COMPONENT (C)
AIRMASS =   1.253398060799e+00 / AIRMASS OF OBSERVATION
RAOFFSET=   0.000000000000e+00 / RA OFFSET (ARCSEC)
DEOFFSET=   0.000000000000e+00 / DEC OFFSET (ARCSEC)
FILEDIR = 'd:\d940721a'
BAKUPDIR= 'j:\mirac\data\d940721a'
FW1STPOS=                 3416 / FILTER W1 STEP POS
FW2STPOS=                 3847 / FILTER W2 STEP POS
PUPILPOS=                    2 / PUPIL POSITION = 2: f/35
COMMENT   THE ORIENTST SHOWS THE ORIGINAL ORIENTATION OF THE DATA
ORIENTST= 'ULYY'
DBLREAD =                    0 / DOUBLE READ FLAG, 0=FALSE
WAITCYCL=                    1 / CHOPPER WAIT CYCLES
BURSTCYC=                    0 / BURST DELAY CYCLES
SUBROWST=                    0 / SUBARRAY ROW START
SUBROWND=                    3 / SUBARRAY ROW END
SUBCOLST=                    0 / SUBARRAY COL START
SUBCOLND=                    3 / SUBARRAY COL END
ARYCYCTM=   3.276799991727e-02 / ARRAY CYCLE TIME (SEC)
BURSTMOD=                    0 / BURST MODE 1=ON,0=OFF
COMMENT   DATA HAS BEEN REARRANGED INTO ROWS AND COLUMNS
FLUXOFST=   1.792000000000e+03 / FLUX OFFSET VALUE (COUNTS)
RAWMIN  =  -1.535766723633e+03 / RAW ON-SOURCE DATA MIN
RAWMAX  =  -4.878166809082e+02 / RAW ON-SOURCE DATA MAX
CDELT1  =  -1.028806581334e-04 / DEGREES/PIXEL IN R.A.
CDELT2  =   1.028806581334e-04 / DEGREES/PIXEL IN DEC.
CRVAL1  =   2.133947083333e+02 / RA OF REFERENCE LOCATION
CRVAL2  =  -1.224700000000e+01 / DEC OF REFERENCE LOCATION
CROTA1  =   0.000000000000e+00 / ROTATION OF AXIS 1
CROTA2  =   0.000000000000e+00 / ROTATION OF AXIS 2
CRPIX1  =   6.400000000000e+01 / ARRAY LOCATION OF REF. PIXEL
CRPIX2  =   6.400000000000e+01 / ARRAY LOCATION OF REF. PIXEL
CTYPE1  =           'RA---SIN' / TYPE OF COORD. ON AXIS 1
CTYPE2  =           'DEC--SIN' / TYPE OF COORD. ON AXIS 2
HISTORY   THIS FILE WAS CONVERTED FROM MIRAC-PC FORMAT BY MRC2FTS
HISTORY   TIME OF CONVERSION: Fri Jul 22 01:44:46 1994
HISTORY   ALL OFF-SOURCE FRAMES IN THE ORIGINAL IMAGE HAVE BEEN SUBTRACTED
HISTORY   MASK FILE APPLIED: new_mrc2fts_mask.fts
HISTORY   GAIN MAP APPLIED: gan785e.fts
SCALED  = '[1,1]'              /
END
 Total FITS keywords found       =     94
 Total FITS keywords to transfer =     94

label-li af784.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File af784.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in DOUB format from a X86-LINUX host
                1 bands
                606 lines per band
                666 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:24 2016 ----
VF001=
'SIMPLE  =                    T / Written by IDL:   8-Aug-1994 14:31:31.00'
VF002='BITPIX  =                  -64 / IEEE double precision floating point'
VF003='NAXIS   =                    2 / NUMBER OF AXES'
VF004='NAXIS1  =                  666 /Number of positions along axis 1'
VF005='NAXIS2  =                  606 /Number of positions along axis 2'
VF006='BSCALE  =   1.000000000000e+00 / VALUE= DATA*BSCALE+BZERO'
VF007='BZERO   =   0.000000000000e+00 /'
VF008='IRAFNAME= "i940721a.784"'
VF009='INSTRUME=  "MIRAC2: Mid-IR Array Camera" / INSTRUMENT'
VF010='TIME-OBS=  "05:56:53.05"       / TIME OF OBSERVATION (UT)'
VF011='TIME-END=  "05:56:55.09"       / END TIME OF OBSERVATION (UT)'
VF012='OBJECT  = "JUPITER"'
VF013='DATE-OBS=  "07/21/94"          / DATE OF OBSERVATION (UT)'
VF014='TELESCOP= "IRTF"'
VF015='RA      =   2.133947083333e+02 / RIGHT ASCENSION (DEGREES)'
VF016='DEC     =  -1.224700000000e+01 / DECLINATION (DEGREES)'
VF017='EPOCH   =   1.950000000000e+03 / EPOCH FOR RA AND DEC'
VF018='LAMBDA  =   1.220257399837e-05 / WAVELENGTH (METERS)'
VF019='FILTER1 =   0.000000000000e+00 / POSITION OF FW 1'
VF020='FILTER2 =   1.000000000000e+01 / POSITION OF FW 2'
VF021='OBSERVAT=          "Mauna Kea" / LOCATION OF OBSERVATION'
VF022='SOFTVER =   4.119999885559e+00 / VERSION NUMBER FOR MIRAC SOFTWARE'
VF023='CHOPFREQ=   2.543131589890e+00 / CHOP FREQUENCY (HZ)'
VF024='FRAMERAT=   3.276799991727e-02 / FRAME RATE (HZ)'
VF025='ITIME   =   1.966079950333e+00 / ON-SOURCE INT. TIME'
VF026='NODWAITT=   2.000000000000e+00 / ON-SRC NOD WAIT TIME (SEC)'
VF027='NODOFFWT=   1.000000000000e+00 / NOD WAIT TIME FOR OFF SRC'
VF028='MAGNIFIC=   4.050000011921e-01 / MAGNIFICATION SETTING'
VF029='DETBIAS =   3.000000000000e+00 / DETECTOR BIAS VOLTS'
VF030='HEATERV =   1.805720448494e+00 / HEATER VOLTAGE'
VF031='DETTEMP =   6.983307838440e+00 / DETECTOR TEMP. (K)'
VF032='TOTCOADD=                   60 / TOTAL COADDS (ON-SRC)'
VF033='TELESCLE=   2.000000000000e+00 / PIXEL SCALE (ARCSEC/MM)'
VF034='OFFSET1 =   0.000000000000e+00 / X OFFSET (PIXELS)'
VF035='OFFSET2 =   0.000000000000e+00 / Y OFFSET (PIXELS)'
VF036='ARRAYXSZ=                  128 / X ARRAY SIZE'
VF037='ARRAYYSZ=                  128 / Y ARRAY SIZE'
VF038='MASKFILE= "o:ibad78v.msk"'
VF039='FILENAME= "c940721a.784"'
VF040='GAINFILE= "o:785e.gan"'
VF041='SAMPMODE=                    0 / 0 = SINGLE SAMPLE MODE'
VF042='DELAYCYC=                    0 / DELAY CYCLES'
VF043='CLOCKRAT=                    5 / CLOCK FREQUENCY (15^6/2^N HZ)'
VF044='COPERCHP=                    1 / COIMAGES PER CHOP CYCLE'
VF045='FRPERCOI=                    5 / FRAMES PER COIMAGE'
VF046='CHOPWTFR=                    0 / CHOPPER WAIT FRAMES'
VF047='CHOPWAIT=   3.276800155640e+01 / CHOP WAIT TIME (MSEC)'
VF048='NUMCHOPS=                    0 / NUMBER OF CHOP CYCLES'
VF049='SKYFLUX =   9.308472290039e+02 / SKY FLUX (AVG CNTS OFF-SRC)'
VF050='SKYRMS  =   0.000000000000e+00 / RMS OF LAST 5 SKY FLUXES'
VF051='SKYDIF  =   0.000000000000e+00 / DIFFERENCE FROM LAST SKY FLUX'
VF052='ERTEMP1 =   3.924659013748e+00 / AMBIENT TEMP (C)'
VF053='ERTEMP2 =   2.064120101929e+01 / TEMP INSIDE CAMERA ELEC. (C)'
VF054='ERTEMP3 =   4.128957748413e+01 / TEMP OF A/D COMPONENT (C)'
VF055='AIRMASS =   1.253398060799e+00 / AIRMASS OF OBSERVATION'
VF056='RAOFFSET=   0.000000000000e+00 / RA OFFSET (ARCSEC)'
VF057='DEOFFSET=   0.000000000000e+00 / DEC OFFSET (ARCSEC)'
VF058='FILEDIR = "d:\d940721a"'
VF059='BAKUPDIR= "j:\mirac\data\d940721a"'
VF060='FW1STPOS=                 3416 / FILTER W1 STEP POS'
VF061='FW2STPOS=                 3847 / FILTER W2 STEP POS'
VF062='PUPILPOS=                    2 / PUPIL POSITION = 2: f/35'
VF063='COMMENT   THE ORIENTST SHOWS THE ORIGINAL ORIENTATION OF THE DATA'
VF064='ORIENTST= "ULYY"'
VF065='DBLREAD =                    0 / DOUBLE READ FLAG, 0=FALSE'
VF066='WAITCYCL=                    1 / CHOPPER WAIT CYCLES'
VF067='BURSTCYC=                    0 / BURST DELAY CYCLES'
VF068='SUBROWST=                    0 / SUBARRAY ROW START'
VF069='SUBROWND=                    3 / SUBARRAY ROW END'
VF070='SUBCOLST=                    0 / SUBARRAY COL START'
VF071='SUBCOLND=                    3 / SUBARRAY COL END'
VF072='ARYCYCTM=   3.276799991727e-02 / ARRAY CYCLE TIME (SEC)'
VF073='BURSTMOD=                    0 / BURST MODE 1=ON,0=OFF'
VF074='COMMENT   DATA HAS BEEN REARRANGED INTO ROWS AND COLUMNS'
VF075='FLUXOFST=   1.792000000000e+03 / FLUX OFFSET VALUE (COUNTS)'
VF076='RAWMIN  =  -1.535766723633e+03 / RAW ON-SOURCE DATA MIN'
VF077='RAWMAX  =  -4.878166809082e+02 / RAW ON-SOURCE DATA MAX'
VF078='CDELT1  =  -1.028806581334e-04 / DEGREES/PIXEL IN R.A.'
VF079='CDELT2  =   1.028806581334e-04 / DEGREES/PIXEL IN DEC.'
VF080='CRVAL1  =   2.133947083333e+02 / RA OF REFERENCE LOCATION'
VF081='CRVAL2  =  -1.224700000000e+01 / DEC OF REFERENCE LOCATION'
VF082='CROTA1  =   0.000000000000e+00 / ROTATION OF AXIS 1'
VF083='CROTA2  =   0.000000000000e+00 / ROTATION OF AXIS 2'
VF084='CRPIX1  =   6.400000000000e+01 / ARRAY LOCATION OF REF. PIXEL'
VF085='CRPIX2  =   6.400000000000e+01 / ARRAY LOCATION OF REF. PIXEL'
VF086='CTYPE1  =           "RA---SIN" / TYPE OF COORD. ON AXIS 1'
VF087='CTYPE2  =           "DEC--SIN" / TYPE OF COORD. ON AXIS 2'
VF088='HISTORY   THIS FILE WAS CONVERTED FROM MIRAC-PC FORMAT BY MRC2FTS'
VF089='HISTORY   TIME OF CONVERSION: Fri Jul 22 01:44:46 1994'
VF090=
'HISTORY   ALL OFF-SOURCE FRAMES IN THE ORIGINAL IMAGE HAVE BEEN SUBTRACTED'
VF091='HISTORY   MASK FILE APPLIED: new_mrc2fts_mask.fts'
VF092='HISTORY   GAIN MAP APPLIED: gan785e.fts'
VF093='SCALED  = "[1,1]"              /'
VF094='END'
 
************************************************************
list af784.vic sl=1 ss=1 nl=5 ns=5
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:24 2016
     Samp             1           2           3           4           5
   Line
      1       1.370E+02   1.500E+02   1.620E+02   1.670E+02   1.660E+02
      2       1.270E+02   1.410E+02   1.550E+02   1.600E+02   1.590E+02
      3       1.280E+02   1.410E+02   1.530E+02   1.610E+02   1.580E+02
      4       1.440E+02   1.500E+02   1.560E+02   1.590E+02   1.560E+02
      5       1.410E+02   1.420E+02   1.430E+02   1.440E+02   1.430E+02
fitsin tt/iras_12b1.fits iras_12b1.vic
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - BLOCKED = T, TAPE records may be blocked
Processing will be attempted anyway...
??W - EXTEND = T, Data may contain FITS extensions
??W - Data cannot be restored with original accuracy
**Note that BSCALE =   6364.99
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    3
NAXIS1  =                  649
NAXIS2  =                  325
NAXIS3  =                    1
BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880
EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS
BSCALE  =          6364.985163
BZERO   =                   0.
BLANK   =               -32768
BUNIT   = 'JY/SR   '
CRPIX1  =                 325.
CDELT1  =                 -0.5
CRVAL1  =                   0.
CTYPE1  = 'ELON-AIT'
CRPIX2  =                 163.
CDELT2  =                 -0.5
CRVAL2  =                   0.
CTYPE2  = 'ELAT-AIT'
CRVAL3  =              1.2E-05 / WAVELENGTH IN METERS
CTYPE3  = 'LAMBDA  '
DATE    = '03/03/93'           / 14:48:44 PST CREATION DATE/TIME
ORIGIN  = 'IPAC    '           / INFRARED PROCESSING AND ANALYSIS CENTER
TELESCOP= 'IRAS    '           / INFRARED ASTRONOMICAL SATELLITE
COMMENT  ACKNOWLEDGEMENT OF THE SOURCE OF THIS DATA SHOULD READ:
COMMENT   "THE IRAS DATA WERE OBTAINED USING THE FACILITIES OF THE IPAC.
COMMENT    IPAC IS FUNDED BY NASA AS PART OF THE IRAS EXTENDED MISSION
COMMENT    PROGRAM UNDER CONTRACT TO JPL."
COMMENT
COMMENT  BSCALE AND BUNIT DERIVED USING
COMMENT  BANDWIDTH 1.348E+13 HZ FOR IRAS BAND 1 (1.2E-05 M)
COMMENT
COMMENT VICAR HEADER FROM SOURCE IMAGE FOLLOWS
COMMENT 77                   325    1298 3251298 I 2                          SC
COMMENT IRAS  SKYFLUX ECLH12B1     RAW     BAND= 1 MINSOP=    29 MAXSOP= 425  EC
COMMENT ZPLOT( 3.6) I 1DN=8.58E-10 WATT/M2/SR    1PIXEL= 0.50 DEGREE          EC
COMMENT       AITOFF   MAP CENTERED AT LAT=  0.00 LON=    0.00( ECLIPTIC)     EC
COMMENT              SL=  -162 SS=  -324 EL=   162 ES=   324                  EL
COMMENT  END OF SOURCE VICAR HEADER
END
 Total FITS keywords found       =     41
 Total FITS keywords to transfer =     41

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-li iras_12b1.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File iras_12b1.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                325 lines per band
                649 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:25 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                   16'
VF003='NAXIS   =                    3'
VF004='NAXIS1  =                  649'
VF005='NAXIS2  =                  325'
VF006='NAXIS3  =                    1'
VF007=
'BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880'
VF008=
'EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS'
VF009='BSCALE  =          6364.985163'
VF010='BZERO   =                   0.'
VF011='BLANK   =               -32768'
VF012='BUNIT   = "JY/SR   "'
VF013='CRPIX1  =                 325.'
VF014='CDELT1  =                 -0.5'
VF015='CRVAL1  =                   0.'
VF016='CTYPE1  = "ELON-AIT"'
VF017='CRPIX2  =                 163.'
VF018='CDELT2  =                 -0.5'
VF019='CRVAL2  =                   0.'
VF020='CTYPE2  = "ELAT-AIT"'
VF021='CRVAL3  =              1.2E-05 / WAVELENGTH IN METERS'
VF022='CTYPE3  = "LAMBDA  "'
VF023='DATE    = "03/03/93"           / 14:48:44 PST CREATION DATE/TIME'
VF024=
'ORIGIN  = "IPAC    "           / INFRARED PROCESSING AND ANALYSIS CENTER'
VF025='TELESCOP= "IRAS    "           / INFRARED ASTRONOMICAL SATELLITE'
VF026='COMMENT  ACKNOWLEDGEMENT OF THE SOURCE OF THIS DATA SHOULD READ:'
VF027=
'COMMENT   "THE IRAS DATA WERE OBTAINED USING THE FACILITIES OF THE IPAC.'
VF028=
'COMMENT    IPAC IS FUNDED BY NASA AS PART OF THE IRAS EXTENDED MISSION'
VF029='COMMENT    PROGRAM UNDER CONTRACT TO JPL."'
VF030='COMMENT'
VF031='COMMENT  BSCALE AND BUNIT DERIVED USING'
VF032='COMMENT  BANDWIDTH 1.348E+13 HZ FOR IRAS BAND 1 (1.2E-05 M)'
VF033='COMMENT'
VF034='COMMENT VICAR HEADER FROM SOURCE IMAGE FOLLOWS'
VF035=
'COMMENT 77                   325    1298 3251298 I 2                          SC'
VF036=
'COMMENT IRAS  SKYFLUX ECLH12B1     RAW     BAND= 1 MINSOP=    29 MAXSOP= 425  EC'
VF037=
'COMMENT ZPLOT( 3.6) I 1DN=8.58E-10 WATT/M2/SR    1PIXEL= 0.50 DEGREE          EC'
VF038=
'COMMENT       AITOFF   MAP CENTERED AT LAT=  0.00 LON=    0.00( ECLIPTIC)     EC'
VF039=
'COMMENT              SL=  -162 SS=  -324 EL=   162 ES=   324                  EL'
VF040='COMMENT  END OF SOURCE VICAR HEADER'
VF041='END'
 
************************************************************
list iras_12b1.vic sl=293 ss=480 nl=6 ns=6
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:25 2016
     Samp     480   481   482   483   484   485
   Line
    293      1389  1865  2555  2494  1594  1405
    294      2297  2010  2250  1974  1701  1339
    295      2583  3406 19315 10068  2783  2315
    296      4404 13419 15511  8930  3662  2418
    297      4037  5454  4620  3184  3146  1874
    298      1408  2047  3458  4841  2551  1331
fitsin tt/iras_12b1.fits iras_12b1.vic data=true
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - BLOCKED = T, TAPE records may be blocked
Processing will be attempted anyway...
??W - EXTEND = T, Data may contain FITS extensions
??W - Data cannot be restored with original accuracy
**Note that BSCALE =   6364.99
SIMPLE  =                    T
BITPIX  =                   16
NAXIS   =                    3
NAXIS1  =                  649
NAXIS2  =                  325
NAXIS3  =                    1
BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880
EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS
BSCALE  =          6364.985163
BZERO   =                   0.
BLANK   =               -32768
BUNIT   = 'JY/SR   '
CRPIX1  =                 325.
CDELT1  =                 -0.5
CRVAL1  =                   0.
CTYPE1  = 'ELON-AIT'
CRPIX2  =                 163.
CDELT2  =                 -0.5
CRVAL2  =                   0.
CTYPE2  = 'ELAT-AIT'
CRVAL3  =              1.2E-05 / WAVELENGTH IN METERS
CTYPE3  = 'LAMBDA  '
DATE    = '03/03/93'           / 14:48:44 PST CREATION DATE/TIME
ORIGIN  = 'IPAC    '           / INFRARED PROCESSING AND ANALYSIS CENTER
TELESCOP= 'IRAS    '           / INFRARED ASTRONOMICAL SATELLITE
COMMENT  ACKNOWLEDGEMENT OF THE SOURCE OF THIS DATA SHOULD READ:
COMMENT   "THE IRAS DATA WERE OBTAINED USING THE FACILITIES OF THE IPAC.
COMMENT    IPAC IS FUNDED BY NASA AS PART OF THE IRAS EXTENDED MISSION
COMMENT    PROGRAM UNDER CONTRACT TO JPL."
COMMENT
COMMENT  BSCALE AND BUNIT DERIVED USING
COMMENT  BANDWIDTH 1.348E+13 HZ FOR IRAS BAND 1 (1.2E-05 M)
COMMENT
COMMENT VICAR HEADER FROM SOURCE IMAGE FOLLOWS
COMMENT 77                   325    1298 3251298 I 2                          SC
COMMENT IRAS  SKYFLUX ECLH12B1     RAW     BAND= 1 MINSOP=    29 MAXSOP= 425  EC
COMMENT ZPLOT( 3.6) I 1DN=8.58E-10 WATT/M2/SR    1PIXEL= 0.50 DEGREE          EC
COMMENT       AITOFF   MAP CENTERED AT LAT=  0.00 LON=    0.00( ECLIPTIC)     EC
COMMENT              SL=  -162 SS=  -324 EL=   162 ES=   324                  EL
COMMENT  END OF SOURCE VICAR HEADER
END
 Total FITS keywords found       =     41
 Total FITS keywords to transfer =     41

*** 16-bit FITS to VICAR HALF (no conversion) ***
label-li iras_12b1.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File iras_12b1.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                325 lines per band
                649 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:25 2016 ----
VF001='SIMPLE  =                    T'
VF002='BITPIX  =                   16'
VF003='NAXIS   =                    3'
VF004='NAXIS1  =                  649'
VF005='NAXIS2  =                  325'
VF006='NAXIS3  =                    1'
VF007=
'BLOCKED =                    T / TAPE MAY BE BLOCKED IN MULTIPLES OF 2880'
VF008=
'EXTEND  =                    T / TAPE MAY HAVE STANDARD FITS EXTENSIONS'
VF009='BSCALE  =          6364.985163'
VF010='BZERO   =                   0.'
VF011='BLANK   =               -32768'
VF012='BUNIT   = "JY/SR   "'
VF013='CRPIX1  =                 325.'
VF014='CDELT1  =                 -0.5'
VF015='CRVAL1  =                   0.'
VF016='CTYPE1  = "ELON-AIT"'
VF017='CRPIX2  =                 163.'
VF018='CDELT2  =                 -0.5'
VF019='CRVAL2  =                   0.'
VF020='CTYPE2  = "ELAT-AIT"'
VF021='CRVAL3  =              1.2E-05 / WAVELENGTH IN METERS'
VF022='CTYPE3  = "LAMBDA  "'
VF023='DATE    = "03/03/93"           / 14:48:44 PST CREATION DATE/TIME'
VF024=
'ORIGIN  = "IPAC    "           / INFRARED PROCESSING AND ANALYSIS CENTER'
VF025='TELESCOP= "IRAS    "           / INFRARED ASTRONOMICAL SATELLITE'
VF026='COMMENT  ACKNOWLEDGEMENT OF THE SOURCE OF THIS DATA SHOULD READ:'
VF027=
'COMMENT   "THE IRAS DATA WERE OBTAINED USING THE FACILITIES OF THE IPAC.'
VF028=
'COMMENT    IPAC IS FUNDED BY NASA AS PART OF THE IRAS EXTENDED MISSION'
VF029='COMMENT    PROGRAM UNDER CONTRACT TO JPL."'
VF030='COMMENT'
VF031='COMMENT  BSCALE AND BUNIT DERIVED USING'
VF032='COMMENT  BANDWIDTH 1.348E+13 HZ FOR IRAS BAND 1 (1.2E-05 M)'
VF033='COMMENT'
VF034='COMMENT VICAR HEADER FROM SOURCE IMAGE FOLLOWS'
VF035=
'COMMENT 77                   325    1298 3251298 I 2                          SC'
VF036=
'COMMENT IRAS  SKYFLUX ECLH12B1     RAW     BAND= 1 MINSOP=    29 MAXSOP= 425  EC'
VF037=
'COMMENT ZPLOT( 3.6) I 1DN=8.58E-10 WATT/M2/SR    1PIXEL= 0.50 DEGREE          EC'
VF038=
'COMMENT       AITOFF   MAP CENTERED AT LAT=  0.00 LON=    0.00( ECLIPTIC)     EC'
VF039=
'COMMENT              SL=  -162 SS=  -324 EL=   162 ES=   324                  EL'
VF040='COMMENT  END OF SOURCE VICAR HEADER'
VF041='END'
 
************************************************************
list iras_12b1.vic sl=293 ss=480 nl=6 ns=6
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:25 2016
     Samp     480   481   482   483   484   485
   Line
    293      1389  1865  2555  2494  1594  1405
    294      2297  2010  2250  1974  1701  1339
    295      2583  3406 19315 10068  2783  2315
    296      4404 13419 15511  8930  3662  2418
    297      4037  5454  4620  3184  3146  1874
    298      1408  2047  3458  4841  2551  1331
fitsin tt/neat668.fits neat668.vic data=u16
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
SIMPLE  =                    T          ; FITS type
BITPIX  =                   16          ; Bits per pixel
NAXIS   =                    2          ; Number of axis
NAXIS1  =                 1100          ; Number of columns
NAXIS2  =                 1024          ; Number of rows
OBJECT  =    neatseqn6a0.668.fits        ; Object name
HST     =           NOT_AVAILABLE       ; Time stamp not available        
RA      =                99:99:99        ; RA position
DEC     =                99:99:99        ; Dec position
ELEV    =                99:99:99        ; Telescope elevation
TEMP    =                  999                ; Temperature
FOCUS   =                  310                ; Focus encoder
RENC    =                  170                ; Rotation stage encoder
TENC    =                 3289                ; Translation stage encoder
FRAME   =                  668                ; Frame number
INTEG   =                30000                ; Integration time (ms)
END
 Total FITS keywords found       =     17
 Total FITS keywords to transfer =     17

*** 16-bit FITS to VICAR FULL(0 to 65535) ***
label-li neat668.vic
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File neat668.vic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                1024 lines per band
                1100 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:25 2016 ----
VF001='SIMPLE  =                    T          ; FITS type'
VF002='BITPIX  =                   16          ; Bits per pixel'
VF003='NAXIS   =                    2          ; Number of axis'
VF004='NAXIS1  =                 1100          ; Number of columns'
VF005='NAXIS2  =                 1024          ; Number of rows'
VF006='OBJECT  =    neatseqn6a0.668.fits        ; Object name'
VF007=
'HST     =           NOT_AVAILABLE       ; Time stamp not available        '
VF008='RA      =                99:99:99        ; RA position'
VF009='DEC     =                99:99:99        ; Dec position'
VF010='ELEV    =                99:99:99        ; Telescope elevation'
VF011='TEMP    =                  999                ; Temperature'
VF012='FOCUS   =                  310                ; Focus encoder'
VF013=
'RENC    =                  170                ; Rotation stage encoder'
VF014=
'TENC    =                 3289                ; Translation stage encoder'
VF015='FRAME   =                  668                ; Frame number'
VF016='INTEG   =                30000                ; Integration time (ms)'
VF017='END'
 
************************************************************
list neat668.vic sl=100 ss=100 nl=6 ns=6
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:FITSIN    User:wlb       Date_Time:Fri Jan  8 12:43:25 2016
     Samp          100        101        102        103        104        105
   Line
    100          29867      32337      34686      32136      42566      28005
    101          29422      28278      32127      34447      33925      33516
    102          32395      30323      27808      31846      36095      34095
    103          34789      34859      33300      32123      42016      36511
    104          29202      28600      29891      32418      26748      27499
    105          32814      36470      34608      33266      25417      33087
fitsin nn/0001.gsc 0001.ibis
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - EXTEND = T, Data may contain FITS extensions
tfields =    10 tabcols =    45 tabrows =  2605
Column format: 1 -   10
 FULL REAL REAL REAL REAL REAL FULL FULL A4   A1
XTENSION= 'TABLE   '           / Table Extension
BITPIX  =                    8 / Character Information
NAXIS   =                    2 / Two-dimensional table
NAXIS1  =                   45 / Number of characters per line
NAXIS2  =                 2605 / Number of rows
PCOUNT  =                    0 / No random parameters
GCOUNT  =                    1 / Only one group
TFIELDS =                   10 / Ten fields per row

EXTNAME = 'GSC_REGION_00001'   / GSC Region No. 00001
EXTVER  =                    1 / Integer Version Number

TTYPE1  = 'GSC_ID  '           / ID within Region
TBCOL1  =                    1 / Start in column 1
TFORM1  = 'I5      '           / Integer, 5 character field (I5.5 Style)

TTYPE2  = 'RA_DEG  '           / Right Ascension - Decimal Degrees (0 to 360)
TBCOL2  =                    6 / Start in column 6
TFORM2  = 'F9.5    '           / Floating, 9 character field

TTYPE3  = 'DEC_DEG '           / Declination - Decimal Degrees (-90 to +90)
TBCOL3  =                   15 / Start in column 15
TFORM3  = 'F9.5    '           / Floating, 9 character field

TTYPE4  = 'POS_ERR '           / Position Error in Arc Seconds
TBCOL4  =                   24 / Start in column 24
TFORM4  = 'F5.1    '           / Floating, 5 character field

TTYPE5  = 'MAG     '           / Magnitude
TBCOL5  =                   29 / Start in column 29
TFORM5  = 'F5.2    '           / Floating, 5 character field

TTYPE6  = 'MAG_ERR '           / Magnitude error
TBCOL6  =                   34 / Start in column 34
TFORM6  = 'F4.2    '           / Floating, 4 character field

TTYPE7  = 'MAG_BAND'           / Magnitude Band
TBCOL7  =                   38 / Start in column 38
TFORM7  = 'I2      '           / Integer, 2 character field (I2.2 Style)

TTYPE8  = 'CLASS   '           / Classification
TBCOL8  =                   40 / Start in column 40
TFORM8  = 'I1      '           / Integer, 1 character field

TTYPE9  = 'PLATE_ID'           / GSSS Internal Plate Number
TBCOL9  =                   41 / Start in column 41
TFORM9  = 'A4      '           / 4 character field

TTYPE10 = 'MULTIPLE'           / (T/F) Flag for additional entries
TBCOL10 =                   45 / Start in column 45
TFORM10 = 'A1      '           / Logical flag, 1 character field

END
 Total FITS keywords found       =     53
 Total FITS keywords to transfer =     53
label-li 0001.ibis
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File 0001.ibis ************
                3 dimensional TABULAR file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                0 lines per band
                512 samples per line
                326 lines of binary header of type IBIS
                0 bytes of binary prefix per line
---- Property: IBIS ----
TYPE='FITS TABLE CONVERSION'
NR=2605
NC=10
ORG='ROW'
FMT_DEFAULT='REAL'
FMT_FULL=(1, 7, 8)
FMT_ASCII=(9, 10)
ASCII_LEN=(4, 1)
GROUPS=('GSC_ID', 'RA_DEG', 'DEC_DEG', 'POS_ERR', 'MAG', 'MAG_ERR', 
'MAG_BAND', 'CLASS', 'PLATE_ID', 'MULTIPLE')
GROUP_1=1
GROUP_2=2
GROUP_3=3
GROUP_4=4
GROUP_5=5
GROUP_6=6
GROUP_7=7
GROUP_8=8
GROUP_9=9
GROUP_10=10
SEGMENT=64
BLOCKSIZE=512
COFFSET=(0, 4, 8, 12, 16, 20, 24, 28, 32, 37)
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:25 2016 ----
VF001='XTENSION= "TABLE   "           / Table Extension'
VF002='BITPIX  =                    8 / Character Information'
VF003='NAXIS   =                    2 / Two-dimensional table'
VF004='NAXIS1  =                   45 / Number of characters per line'
VF005='NAXIS2  =                 2605 / Number of rows'
VF006='PCOUNT  =                    0 / No random parameters'
VF007='GCOUNT  =                    1 / Only one group'
VF008='TFIELDS =                   10 / Ten fields per row'
VF009=''
VF010='EXTNAME = "GSC_REGION_00001"   / GSC Region No. 00001'
VF011='EXTVER  =                    1 / Integer Version Number'
VF012=''
VF013='TTYPE1  = "GSC_ID  "           / ID within Region'
VF014='TBCOL1  =                    1 / Start in column 1'
VF015=
'TFORM1  = "I5      "           / Integer, 5 character field (I5.5 Style)'
VF016=''
VF017=
'TTYPE2  = "RA_DEG  "           / Right Ascension - Decimal Degrees (0 to 360)'
VF018='TBCOL2  =                    6 / Start in column 6'
VF019='TFORM2  = "F9.5    "           / Floating, 9 character field'
VF020=''
VF021=
'TTYPE3  = "DEC_DEG "           / Declination - Decimal Degrees (-90 to +90)'
VF022='TBCOL3  =                   15 / Start in column 15'
VF023='TFORM3  = "F9.5    "           / Floating, 9 character field'
VF024=''
VF025='TTYPE4  = "POS_ERR "           / Position Error in Arc Seconds'
VF026='TBCOL4  =                   24 / Start in column 24'
VF027='TFORM4  = "F5.1    "           / Floating, 5 character field'
VF028=''
VF029='TTYPE5  = "MAG     "           / Magnitude'
VF030='TBCOL5  =                   29 / Start in column 29'
VF031='TFORM5  = "F5.2    "           / Floating, 5 character field'
VF032=''
VF033='TTYPE6  = "MAG_ERR "           / Magnitude error'
VF034='TBCOL6  =                   34 / Start in column 34'
VF035='TFORM6  = "F4.2    "           / Floating, 4 character field'
VF036=''
VF037='TTYPE7  = "MAG_BAND"           / Magnitude Band'
VF038='TBCOL7  =                   38 / Start in column 38'
VF039=
'TFORM7  = "I2      "           / Integer, 2 character field (I2.2 Style)'
VF040=''
VF041='TTYPE8  = "CLASS   "           / Classification'
VF042='TBCOL8  =                   40 / Start in column 40'
VF043='TFORM8  = "I1      "           / Integer, 1 character field'
VF044=''
VF045='TTYPE9  = "PLATE_ID"           / GSSS Internal Plate Number'
VF046='TBCOL9  =                   41 / Start in column 41'
VF047='TFORM9  = "A4      "           / 4 character field'
VF048=''
VF049='TTYPE10 = "MULTIPLE"           / (T/F) Flag for additional entries'
VF050='TBCOL10 =                   45 / Start in column 45'
VF051='TFORM10 = "A1      "           / Logical flag, 1 character field'
VF052=''
VF053='END'
 
************************************************************
ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats  +
	screen=120
Beginning VICAR task ibis
 
Number of Rows:2605  Number of Columns: 10      
File Version:IBIS-2  Organization:ROW  SubType:FITS TABLE CONVERSION
Group 'GSC_ID': 1
Group 'RA_DEG': 2
Group 'DEC_DEG': 3
Group 'POS_ERR': 4
Group 'MAG': 5
Group 'MAG_ERR': 6
Group 'MAG_BAND': 7
Group 'CLASS': 8
Group 'PLATE_ID': 9
Group 'MULTIPLE': 10
 
Rows: 1:15
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9
        FULL        REAL        REAL        REAL        REAL        REAL        FULL        FULL          A4
GSC_ID      RA_DEG      DEC_DEG     POS_ERR     MAG         MAG_ERR     MAG_BAND    CLASS       PLATE_ID    
 --          --          --          --          --          --          --          --          --         
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1        0.99        2.33        0.30       14.76        0.41           0           0        012J
           1        0.99        2.33        0.20       14.34        0.42           1           0        0516
           2        0.78        2.41        0.30       15.55        0.41           0           3        012J
           3        0.35        1.95        0.20       13.85        0.42           1           0        0516
           3        0.35        1.95        0.30       14.47        0.38           0           0        012J
           4        1.65        1.62        0.30       14.01        0.38           0           0        012J
           4        1.65        1.62        0.30       13.76        0.42           1           0        0516
           5        1.80        2.22        0.30       14.91        0.42           1           0        0516
           5        1.80        2.22        0.30       13.86        0.53           1           0        04YB
           5        1.80        2.22        0.30       15.01        0.41           0           3        012J
           6        0.33        1.61        0.20       14.85        0.42           1           0        0516
           6        0.33        1.61        0.30       15.19        0.34           0           0        012J
           7        1.57        1.94        0.30       14.23        0.42           1           0        0516
           7        1.57        1.94        0.30       14.76        0.40           0           0        012J
           8        2.32        2.23        0.20       11.56        0.53           1           0        04YB
 
Rows: 1:15
+-----------
        C:10
          A1
MULTIPLE    
 --         
+-----------
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
fitsin nn/0001.gsc 0001.ibis tablist=list
Beginning VICAR task fitsin
*** FITSIN 08/03/2014 (64-bit) rjb
??W - EXTEND = T, Data may contain FITS extensions
tfields =    10 tabcols =    45 tabrows =  2605
Column format: 1 -   10
 FULL REAL REAL REAL REAL REAL FULL FULL A4   A1
XTENSION= 'TABLE   '           / Table Extension
BITPIX  =                    8 / Character Information
NAXIS   =                    2 / Two-dimensional table
NAXIS1  =                   45 / Number of characters per line
NAXIS2  =                 2605 / Number of rows
PCOUNT  =                    0 / No random parameters
GCOUNT  =                    1 / Only one group
TFIELDS =                   10 / Ten fields per row

EXTNAME = 'GSC_REGION_00001'   / GSC Region No. 00001
EXTVER  =                    1 / Integer Version Number

TTYPE1  = 'GSC_ID  '           / ID within Region
TBCOL1  =                    1 / Start in column 1
TFORM1  = 'I5      '           / Integer, 5 character field (I5.5 Style)

TTYPE2  = 'RA_DEG  '           / Right Ascension - Decimal Degrees (0 to 360)
TBCOL2  =                    6 / Start in column 6
TFORM2  = 'F9.5    '           / Floating, 9 character field

TTYPE3  = 'DEC_DEG '           / Declination - Decimal Degrees (-90 to +90)
TBCOL3  =                   15 / Start in column 15
TFORM3  = 'F9.5    '           / Floating, 9 character field

TTYPE4  = 'POS_ERR '           / Position Error in Arc Seconds
TBCOL4  =                   24 / Start in column 24
TFORM4  = 'F5.1    '           / Floating, 5 character field

TTYPE5  = 'MAG     '           / Magnitude
TBCOL5  =                   29 / Start in column 29
TFORM5  = 'F5.2    '           / Floating, 5 character field

TTYPE6  = 'MAG_ERR '           / Magnitude error
TBCOL6  =                   34 / Start in column 34
TFORM6  = 'F4.2    '           / Floating, 4 character field

TTYPE7  = 'MAG_BAND'           / Magnitude Band
TBCOL7  =                   38 / Start in column 38
TFORM7  = 'I2      '           / Integer, 2 character field (I2.2 Style)

TTYPE8  = 'CLASS   '           / Classification
TBCOL8  =                   40 / Start in column 40
TFORM8  = 'I1      '           / Integer, 1 character field

TTYPE9  = 'PLATE_ID'           / GSSS Internal Plate Number
TBCOL9  =                   41 / Start in column 41
TFORM9  = 'A4      '           / 4 character field

TTYPE10 = 'MULTIPLE'           / (T/F) Flag for additional entries
TBCOL10 =                   45 / Start in column 45
TFORM10 = 'A1      '           / Logical flag, 1 character field

END
 Total FITS keywords found       =     53
 Total FITS keywords to transfer =     53
Listing of FITS data set GSC_REGION_00001 Version    1
GSC_ID    RA_DEG    DEC_DEG   POS_ERR   MAG       MAG_ERR   MAG_BAND  CLASS     PLATE_ID  MULTIPLE
00001  0.98809  2.33445  0.314.760.41 00012JT
00001  0.98809  2.33455  0.214.340.42 100516T
00002  0.78175  2.40578  0.315.550.41 03012JF
00003  0.34892  1.95044  0.213.850.42 100516T
00003  0.34910  1.95037  0.314.470.38 00012JT
00004  1.64942  1.62345  0.314.010.38 00012JT
00004  1.64926  1.62352  0.313.760.42 100516T
00005  1.79843  2.22069  0.314.910.42 100516T
00005  1.79855  2.22080  0.313.860.53 1004YBT
00005  1.79830  2.22076  0.315.010.41 03012JT
00006  0.33287  1.60750  0.214.850.42 100516T
00006  0.33315  1.60732  0.315.190.34 00012JT
00007  1.56837  1.93673  0.314.230.42 100516T
00007  1.56841  1.93660  0.314.760.40 00012JT
00008  2.31770  2.23193  0.211.560.53 1004YBT
00008  2.31752  2.23173  0.412.280.41 00012JT
00009  1.91358  2.23084  0.313.290.53 1004YBT
00009  1.91324  2.23078  0.314.530.41 00012JT
00009  1.91348  2.23077  0.313.870.42 100516T
00010  0.92832  1.74069  0.214.360.42 100516T
00010  0.92855  1.74057  0.315.410.36 00012JT
00011  1.52889  0.59498  0.315.220.29 00012JT
00011  1.52876  0.59539  0.314.740.42 100516T
00012  1.03299  1.89095  0.313.730.38 00012JT
00012  1.03286  1.89115  0.213.040.42 100516T
00013  1.12544  2.26744  0.3 8.650.42 100516T
00013  1.12551  2.26743  0.4 9.770.41 00012JT
00014  0.83853  1.48351  0.314.810.33 00012JT
00014  0.83824  1.48368  0.214.340.42 100516T
00015  1.62125  1.15550  0.314.860.34 00012JT
00015  1.62113  1.15577  0.314.710.42 100516T
00016  1.05699  1.89805  0.212.190.42 100516T
00016  1.05714  1.89797  0.312.640.38 00012JT
00017  0.05067  1.77152  0.410.140.37 03012JT
00017  0.05037  1.77156  0.3 9.520.42 100516T
00018  1.99463  2.25606  0.314.300.53 1004YBF
00019  0.80380  1.00966  0.314.280.26 00012JT
00019  0.80354  1.01000  0.314.040.42 100516T
00020  0.37204  2.48036  0.211.070.41 100516T
00020  0.37217  2.48037  1.211.780.41 00012JT
00021  1.93412  2.26628  0.313.470.53 1004YBT
00021  1.93388  2.26619  0.314.840.41 00012JT
00021  1.93410  2.26624  0.314.200.42 100516T
00022  1.02737  1.45862  0.212.220.42 100516T
00022  1.02770  1.45845  0.312.550.33 00012JT
00023  1.03540  1.88776  0.213.870.42 100516T
00023  1.03551  1.88762  0.314.200.38 00012JT
00024  0.77397  1.71059  0.212.520.42 100516T
00024  0.77425  1.71045  0.312.860.35 00012JT
00025  1.17002  2.17213  0.315.290.41 00012JT
00025  1.16999  2.17222  0.215.000.42 100516T
00026  0.56261  2.30504  0.313.150.41 00012JT
00026  0.56242  2.30526  0.212.670.42 100516T
00027  0.95326  2.16071  0.212.660.42 100516T
00027  0.95340  2.16049  0.313.190.40 00012JT
00028  1.75357  2.27852  0.314.550.53 1004YBF
00029  1.96712  2.28456  0.313.010.42 100516T
00029  1.96710  2.28468  0.312.630.53 1004YBT
00029  1.96689  2.28461  0.313.680.41 00012JT
00030  0.24466  2.25167  0.315.380.41 00012JT
00030  0.24447  2.25182  0.214.220.41 100516T
00031  1.84285  2.28599  0.314.250.42 100516T
00031  1.84293  2.28611  0.313.390.53 1004YBT
00031  1.84266  2.28609  0.314.660.41 00012JT
00032  1.26962  0.73381  0.314.940.27 00012JT
00032  1.26951  0.73424  0.314.630.42 100516T
00033  1.10978  1.14337  0.313.550.42 100516T
00033  1.10999  1.14317  0.314.480.30 00012JT
00034  2.03570  2.29861  0.314.460.41 00012JT
00034  2.03593  2.29867  0.313.510.53 1004YBT
00035  0.63728  1.98122  0.212.650.42 100516T
00035  0.63745  1.98103  0.413.030.38 00012JT
00036  0.29738  1.92469  0.211.670.42 100516T
00036  0.29757  1.92455  0.411.870.38 00012JT
00037  1.45829  0.85513  0.315.480.30 00012JF
00038  2.13855  2.30447  0.313.210.41 00012JT
00038  2.13877  2.30458  0.312.200.53 1004YBT
00039  2.37129  2.30397  0.5 9.690.41 03012JT
00039  2.37136  2.30411  0.3 9.500.53 1004YBT
00040  1.44457  1.35375  0.313.330.42 100516T
00040  1.44482  1.35361  0.313.620.34 00012JT
00041  1.80888  2.31567  0.312.090.42 100516T
00041  1.80869  2.31577  0.412.860.41 00012JT
00041  1.80889  2.31570  0.311.740.53 1004YBT
00042  2.10047  2.31498  0.313.890.53 1004YBT
00042  2.10024  2.31490  0.315.180.41 00012JT
00043  0.31165  1.70734  0.314.340.36 03012JT
00043  0.31140  1.70749  0.213.630.42 100516T
00044  1.16798  1.09084  0.313.830.30 03012JT
00044  1.16779  1.09113  0.313.400.42 100516T
00045  1.99642  2.31137  0.314.830.42 100516T
00045  1.99635  2.31146  0.314.060.53 1004YBT
00046  0.69649  2.42429  0.315.500.41 00012JT
00046  0.69646  2.42434  0.214.740.42 100516T
00047  1.97257  2.30843  0.315.140.41 00012JT
00047  1.97280  2.30848  0.314.000.53 1004YBT
00048  0.63086  1.62710  0.315.350.42 130516T
00048  0.63112  1.62686  0.315.420.34 03012JT
00049  1.08227  2.12257  0.313.040.40 00012JT
00049  1.08220  2.12265  0.212.450.42 100516T
00050  0.57992  1.70289  0.412.510.35 00012JT
00050  0.57968  1.70315  0.211.840.42 100516T
00051  1.30809  1.78572  0.315.300.38 00012JT
00051  1.30788  1.78598  0.214.930.42 100516T
00052  1.19666  1.87529  0.315.500.38 00012JF
00053  0.71299  1.22755  0.314.270.29 00012JT
00053  0.71270  1.22780  0.213.740.42 100516T
00054  0.87815  2.31610  0.211.610.42 100516T
00054  0.87823  2.31602  0.411.970.41 00012JT
00055  2.46582  2.04848  1.211.770.41 00012JT
00055  2.46598  2.04864  0.211.150.53 1004YBT
00056  0.20606  1.53485  0.313.420.34 03012JT
00056  0.20574  1.53503  0.214.880.42 130516T
00057  2.25050  2.04620  0.314.220.41 00012JT
00057  2.25070  2.04633  0.313.280.53 1004YBT
00058  1.26428  1.03789  0.5 9.460.30 03012JT
00058  1.26423  1.03799  0.3 8.910.42 100516T
00059  1.17010  2.25342  0.313.850.41 00012JT
00059  1.17009  2.25352  0.213.560.42 100516T
00060  1.25837  2.03882  0.213.190.42 100516T
00060  1.25849  2.03869  0.313.520.40 00012JT
00061  1.35019  0.99760  0.313.820.30 00012JT
00061  1.34992  0.99794  0.313.190.42 100516T
00062  1.95613  2.06023  0.311.950.53 1004YBT
00062  1.95606  2.06003  0.312.200.42 100516T
00062  1.95590  2.06017  0.312.730.41 00012JT
00063  2.10449  2.06022  0.314.370.53 1004YBT
00063  2.10422  2.06002  0.315.400.41 00012JT
00064  0.60043  2.34951  0.214.310.42 100516T
00064  0.60054  2.34943  0.314.530.41 00012JT
00065  0.28644  1.20922  0.213.020.42 100516T
00065  0.28673  1.20900  0.313.380.30 00012JT
00066  0.52487  1.87087  0.212.740.42 100516T
00066  0.52514  1.87070  0.412.980.37 00012JT
00067  0.25194  2.44363  0.314.910.41 00012JT
00067  0.25179  2.44376  0.214.610.40 100516T
00068  0.27579  2.40373  0.214.310.40 100516T
00068  0.27594  2.40363  0.314.650.41 00012JT
00069  1.48799  2.17655  0.314.960.42 100516T
00069  1.48799  2.17663  0.315.300.41 00012JT
00070  1.56142  1.81979  0.415.060.39 03012JF
00071  0.22256  0.68333  0.312.080.42 100516T
00071  0.22273  0.68304  0.312.650.24 00012JT
00072  0.77165  2.37716  0.212.170.42 100516T
00072  0.77174  2.37708  0.312.550.41 00012JT
00073  0.87793  2.34009  0.315.060.41 03012JF
00074  2.16079  2.08643  0.314.970.41 00012JT
00074  2.16102  2.08658  0.313.720.53 1004YBT
00075  1.86451  2.08433  0.311.590.53 1004YBT
00075  1.86434  2.08420  0.311.980.42 100516T
00075  1.86422  2.08427  0.412.380.41 00012JT
00076  0.54996  0.93671  0.315.240.25 00012JT
00076  0.54971  0.93700  0.314.650.42 100516T
00077  0.45656  0.56716  0.315.350.42 100516T
00077  0.45667  0.56684  0.314.350.20 03012JT
00078  2.37907  2.10845  0.315.440.41 00012JT
00078  2.37928  2.10864  0.213.920.53 1004YBT
00079  1.88282  2.11222  0.311.860.53 1004YBT
00079  1.88267  2.11202  0.312.070.42 100516T
00079  1.88251  2.11211  0.412.600.41 00012JT
00080  0.60134  2.17853  0.314.770.40 00012JT
00080  0.60123  2.17866  0.214.500.42 100516T
00081  1.32762  2.34392  0.314.800.41 00012JT
00081  1.32760  2.34412  0.214.660.42 100516T
00082  1.48871  2.17985  0.214.370.42 100516T
00082  1.48866  2.17978  0.314.620.41 00012JT
00083  1.66812  2.10520  0.310.480.53 1004YBT
00083  1.66769  2.10532  0.411.240.41 00012JT
00083  1.66780  2.10535  0.310.330.42 100516T
00084  2.03208  2.10036  0.313.170.53 1304YBT
00084  2.03175  2.10030  0.314.120.41 03012JT
00084  2.03194  2.10014  0.313.290.42 130516T
00085  0.11682  2.12028  0.215.010.41 100516T
00085  0.11703  2.12014  0.315.440.40 00012JT
00086  0.81515  0.57697  0.314.320.20 00012JT
00086  0.81502  0.57732  0.314.050.42 100516T
00087  1.61107  1.83079  0.314.730.39 00012JT
00087  1.61095  1.83091  0.314.510.42 100516T
00088  0.22167  2.11908  0.314.510.40 00012JT
00088  0.22150  2.11925  0.214.080.42 100516T
00089  2.06268  2.09918  0.313.700.42 100516T
00089  2.06248  2.09935  0.314.140.41 00012JT
00089  2.06272  2.09944  0.313.480.53 1004YBT
00090  2.16866  2.09980  0.312.220.53 1004YBT
00090  2.16842  2.09967  0.313.500.41 00012JT
00091  2.13685  2.11466  0.415.090.53 1004YBF
00092  2.46134  0.27925  0.412.390.39 00012JF
00093  0.80104  2.19996  0.315.090.40 00012JT
00093  0.80087  2.20010  0.214.760.42 100516T
00094  1.01055  2.10475  0.314.160.40 00012JT
00094  1.01044  2.10488  0.213.880.42 100516T
00095  0.06878  1.27039  0.315.400.32 00012JT
00095  0.06846  1.27051  0.214.890.42 100516T
00096  1.17076  1.24055  0.314.130.42 100516T
00096  1.17102  1.24032  0.314.800.31 00012JT
00097  0.56830  2.48765  0.313.880.41 00012JT
00097  0.56821  2.48770  0.213.380.42 100516T
00098  0.87462  1.74924  0.211.300.42 100516T
00098  0.87489  1.74905  0.411.900.36 00012JT
00099  0.20666  1.23995  0.214.190.42 100516T
00099  0.20701  1.23968  0.315.110.31 00012JT
00100  2.37061  0.33073  0.412.430.38 00012JF
00101  2.01549  2.13621  0.313.550.42 100516T
00101  2.01520  2.13633  0.314.120.41 00012JT
00101  2.01544  2.13642  0.313.230.53 1004YBT
00102  1.32438  1.87386  0.215.190.42 100516T
00102  1.32451  1.87369  0.315.490.38 00012JT
00103  1.68782  2.12519  0.313.990.41 00012JT
00103  1.68811  2.12511  0.312.810.53 1004YBT
00103  1.68787  2.12529  0.313.870.42 100516T
00104  2.46675  2.12318  0.315.140.41 00012JT
00104  2.46699  2.12341  0.214.120.53 1004YBT
00105  0.27563  1.94287  0.214.260.42 100516T
00105  0.27589  1.94269  0.314.530.38 00012JT
00106  1.04817  1.86112  0.215.090.42 100516T
00106  1.04838  1.86091  0.315.450.37 00012JT
00107  0.29727  1.70899  0.315.200.36 00012JT
00107  0.29695  1.70914  0.214.490.42 100516T
00108  0.35080  2.16728  0.212.370.42 100516T
00108  0.35099  2.16721  0.312.720.40 00012JT
00109  0.10334  2.15710  0.212.870.41 100516T
00109  0.10354  2.15699  0.313.480.41 00012JT
00110  0.26641  2.18475  0.314.080.41 00012JT
00110  0.26618  2.18488  0.213.720.42 100516T
00111  1.88647  2.14245  0.412.100.41 00012JT
00111  1.88673  2.14255  0.311.280.53 1004YBT
00111  1.88664  2.14236  0.311.560.42 100516T
00112  0.94310  1.53171  0.214.220.42 100516T
00112  0.94333  1.53140  0.314.600.34 00012JT
00113  1.79317  2.15224  0.314.420.53 1004YBF
00114  1.64180  2.02319  0.313.100.42 100516T
00114  1.64178  2.02311  0.313.540.41 00012JT
00115  0.25791  1.99926  0.214.440.42 100516T
00115  0.25813  1.99907  0.315.520.39 00012JT
00116  0.23238  2.04408  0.214.300.42 100516T
00116  0.23266  2.04394  0.315.230.39 00012JT
00117  1.10717  1.68215  0.214.890.42 100516T
00117  1.10736  1.68205  0.315.260.36 00012JT
00118  1.60300  2.17417  0.314.490.42 100516T
00118  1.60296  2.17412  0.314.550.41 00012JT
00119  0.18713  1.25235  0.213.920.42 100516T
00119  0.18749  1.25211  0.314.310.31 00012JT
00120  1.02305  1.75369  0.212.520.42 100516T
00120  1.02329  1.75359  0.313.060.36 00012JT
00121  1.66112  1.12519  0.314.760.42 100516T
00121  1.66131  1.12490  0.315.440.34 00012JT
00122  0.73207  0.98586  0.315.450.26 00012JF
00123  1.92859  2.16555  0.314.460.53 1004YBF
00124  2.01771  2.17077  0.312.960.53 1004YBT
00124  2.01749  2.17071  0.313.890.41 00012JT
00124  2.01778  2.17067  0.313.120.42 100516T
00125  0.11238  2.34160  0.315.240.41 00012JT
00125  0.11219  2.34173  0.214.420.40 100516T
00126  1.63714  0.45480  0.314.810.30 00012JT
00126  1.63704  0.45532  0.414.480.42 100516T
00127  0.62554  0.90044  0.314.530.25 00012JT
00127  0.62531  0.90075  0.314.190.42 100516T
00128  0.82987  0.56970  0.314.000.20 03012JT
00128  0.82972  0.57008  0.314.050.42 100516T
00129  1.21942  0.73523  0.314.870.26 00012JT
00129  1.21926  0.73565  0.314.540.42 100516T
00130  2.48599  2.17728  0.315.170.41 00012JT
00130  2.48618  2.17748  0.214.160.53 1004YBT
00131  1.01052  1.44330  0.312.520.33 00012JT
00131  1.01025  1.44356  0.212.110.42 100516T
00132  0.45714  0.50748  0.314.280.42 100516T
00132  0.45717  0.50716  0.315.460.19 00012JT
00133  0.57073  2.03819  0.412.600.39 03012JT
00133  0.57046  2.03840  0.212.190.42 130516T
00134  2.30429  2.18410  0.314.810.41 00012JT
00134  2.30452  2.18424  0.314.000.53 1004YBT
00135  0.33190  2.08869  0.215.000.42 100516T
00135  0.33217  2.08853  0.315.470.39 00012JT
00136  0.43293  1.82384  0.212.180.42 100516T
00136  0.43328  1.82368  0.412.910.37 00012JT
00137  2.15627  0.93388  0.315.440.38 03012JF
00138  2.43546  2.18991  0.315.120.41 00012JT
00138  2.43569  2.19005  0.214.050.53 1004YBT
00139  2.21799  2.19111  0.314.700.41 00012JT
00139  2.21821  2.19131  0.313.490.53 1004YBT
00140  2.42646  2.19118  0.212.940.53 1004YBT
00140  2.42622  2.19101  0.313.710.41 00012JT
00141  1.34414  2.16193  0.315.140.41 00012JF
00142  2.01784  2.19419  0.312.110.42 100516T
00142  2.01754  2.19435  0.412.910.41 00012JT
00142  2.01772  2.19438  0.311.880.53 1004YBT
00143  1.33248  2.30537  0.214.700.42 100516T
00143  1.33245  2.30521  0.314.930.41 00012JT
00144  2.26780  2.48539  0.415.480.41 00012JF
00145  0.25854  0.81786  0.311.240.42 100516T
00145  0.25874  0.81757  0.411.800.25 03012JT
00146  0.46894  1.38631  0.214.540.42 100516T
00146  0.46926  1.38601  0.314.920.32 00012JT
00147  0.12072  1.34880  0.213.040.42 100516T
00147  0.12107  1.34861  0.313.400.32 00012JT
00148  1.22045  0.68527  0.315.110.26 00012JT
00148  1.22029  0.68567  0.314.530.42 100516T
00149  2.46885  2.20914  0.214.240.53 1004YBT
00149  2.46860  2.20897  0.315.400.41 00012JT
00150  0.13484  1.95563  0.411.420.39 00012JT
00150  0.13461  1.95571  0.210.490.42 100516T
00151  2.25632  2.20938  0.314.010.53 1004YBT
00151  2.25608  2.20917  0.315.010.41 00012JT
00152  0.31734  2.24709  0.313.680.41 00012JT
00152  0.31719  2.24716  0.213.070.42 100516T
00153  1.46087  2.27584  0.313.580.41 00012JT
00153  1.46095  2.27580  0.213.070.42 100516T
00154  0.86941  0.74952  0.312.950.23 00012JT
00154  0.86927  0.74982  0.312.240.42 100516T
00155  0.98695  1.63782  0.211.880.42 100516T
00155  0.98716  1.63763  0.312.560.35 00012JT
00156  2.08020  2.48807  0.312.220.53 1004YBT
00156  2.08003  2.48806  0.412.940.41 00012JT
00156  2.08027  2.48791  0.312.440.42 100516T
00157  1.95578  2.48748  0.315.080.41 03012JT
00157  1.95602  2.48757  0.313.530.53 1004YBT
00157  1.95609  2.48749  0.314.190.42 130516T
00158  1.57352  1.24588  0.314.120.34 02012JF
00159  1.18678  1.51043  0.214.340.42 100516T
00159  1.18704  1.51024  0.314.630.34 00012JT
00160  0.50976  1.92706  0.214.620.42 100516T
00160  0.50998  1.92687  0.315.120.38 00012JT
00161  0.44788  0.82180  0.313.620.42 100516T
00161  0.44808  0.82153  0.314.310.24 00012JT
00162  0.19674  2.38503  0.3 9.690.40 100516T
00162  0.19693  2.38490  0.410.130.41 00012JT
00163  0.61504  2.25412  0.312.620.41 00012JT
00163  0.61482  2.25422  0.211.850.42 100516T
00164  1.75645  2.47778  0.314.210.53 1004YBT
00164  1.75647  2.47784  0.315.390.42 100516T
00164  1.75624  2.47788  0.315.410.41 00012JT
00165  0.35864  1.24826  0.314.470.30 00012JT
00165  0.35837  1.24850  0.214.470.42 100516T
00166  1.47350  0.54297  0.412.100.28 00012JT
00166  1.47340  0.54331  0.311.520.42 100516T
00167  1.54676  1.93296  0.312.850.42 100516T
00167  1.54683  1.93286  0.313.370.40 00012JT
00168  0.42346  1.80691  0.412.320.36 00012JT
00168  0.42318  1.80710  0.211.930.42 100516T
00169  2.13083  2.48127  0.314.840.41 00012JT
00169  2.13109  2.48140  0.313.790.53 1004YBT
00170  1.47802  2.33023  0.315.330.41 00012JT
00170  1.47810  2.33017  0.215.290.42 100516T
00171  1.94848  2.41595  0.312.990.42 100516T
00171  1.94845  2.41606  0.312.660.53 1004YBT
00171  1.94829  2.41602  0.313.560.41 00012JT
00172  0.48152  0.80668  0.315.250.23 00012JT
00172  0.48134  0.80701  0.314.660.42 100516T
00173  1.86705  2.42938  0.314.250.42 100516T
00173  1.86689  2.42948  0.314.690.41 00012JT
00173  1.86708  2.42948  0.313.390.53 1004YBT
00174  1.17611  0.37284  0.412.370.23 00012JF
00175  1.56462  0.37447  0.314.830.29 00012JF
00176  0.44353  0.75032  0.315.300.23 03012JF
00177  1.87550  2.41841  0.314.220.53 1004YBT
00177  1.87526  2.41832  0.315.530.41 00012JT
00177  1.87549  2.41826  0.315.430.42 100516T
00178  1.61296  0.27816  0.314.150.30 00012JF
00179  2.25709  2.43292  0.314.220.41 00012JT
00179  2.25735  2.43308  0.213.290.53 1004YBT
00180  1.38757  2.04378  0.214.280.42 100516T
00180  1.38762  2.04367  0.314.600.40 00012JT
00181  2.11899  2.43764  0.313.930.53 1004YBT
00181  2.11878  2.43756  0.314.990.41 00012JT
00182  2.26225  1.95710  0.315.370.41 00012JF
00183  1.85027  2.44914  0.313.270.41 00012JT
00183  1.85049  2.44914  0.312.390.42 100516T
00183  1.85048  2.44908  0.311.910.53 1004YBT
00184  1.28484  2.17550  0.314.070.41 00012JT
00184  1.28479  2.17558  0.213.970.42 100516T
00185  1.92600  2.44499  0.313.560.42 100516T
00185  1.92603  2.44502  0.313.270.53 1004YBT
00185  1.92573  2.44493  0.314.110.41 00012JT
00186  2.01165  2.44441  0.310.570.42 100516T
00186  2.01131  2.44446  0.411.380.41 00012JT
00186  2.01146  2.44450  0.310.590.53 1004YBT
00187  2.26047  2.45124  0.314.630.41 00012JT
00187  2.26069  2.45138  0.213.400.53 1004YBT
00188  0.07669  1.59878  0.315.300.35 00012JT
00188  0.07641  1.59894  0.214.760.42 100516T
00189  0.21247  1.68553  0.214.010.42 100516T
00189  0.21274  1.68536  0.314.650.36 00012JT
00190  1.71664  2.46733  0.315.120.42 100516T
00190  1.71646  2.46737  0.315.520.41 00012JT
00190  1.71669  2.46722  0.313.980.53 1004YBT
00191  0.80208  2.48424  0.412.990.41 00012JT
00191  0.80196  2.48431  0.212.420.42 100516T
00192  1.23914  1.46513  0.213.720.42 100516T
00192  1.23941  1.46482  0.314.090.34 00012JT
00193  2.32819  2.46245  0.411.470.41 00012JT
00193  2.32833  2.46258  0.210.930.53 1004YBT
00194  0.02650  1.33196  0.3 9.460.42 100516T
00194  0.02688  1.33178  0.410.130.33 00012JT
00195  1.74691  2.46045  0.315.060.42 100516T
00195  1.74699  2.46033  0.313.850.53 1004YBT
00195  1.74671  2.46046  0.315.360.41 00012JT
00196  0.26421  1.17697  0.313.740.30 00012JT
00196  0.26385  1.17723  0.213.150.42 100516T
00197  1.23267  0.21954  0.412.150.24 00012JF
00198  2.39725  2.45922  0.213.980.53 1004YBT
00198  2.39709  2.45906  0.415.300.41 00012JT
00199  0.27737  1.18625  0.315.280.30 00012JT
00199  0.27705  1.18647  0.214.460.42 100516T
00200  0.83047  2.12625  0.315.050.40 00012JT
00200  0.83035  2.12632  0.214.700.42 100516T
00201  0.60471  1.52142  0.211.710.42 100516T
00201  0.60500  1.52116  0.412.300.33 00012JT
00202  2.31190  2.38540  0.213.250.53 1004YBT
00202  2.31170  2.38530  0.314.180.41 00012JT
00203  1.32429  1.57132  0.213.940.42 100516T
00203  1.32448  1.57115  0.314.540.36 00012JT
00204  2.15872  2.39215  0.314.440.41 00012JT
00204  2.15897  2.39228  0.313.260.53 1004YBT
00205  1.65743  1.39549  0.313.280.42 100516T
00205  1.65764  1.39537  0.313.590.36 00012JT
00206  1.34146  2.47143  0.411.790.41 00012JT
00206  1.34146  2.47153  0.211.280.42 100516T
00207  2.43153  2.40682  0.413.800.41 00012JT
00207  2.43167  2.40700  0.212.770.53 1004YBT
00208  1.29234  2.39743  0.3 8.950.42 100516T
00208  1.29228  2.39739  0.5 9.260.41 03012JT
00209  0.73954  1.84913  0.315.040.37 00012JF
00210  1.84922  2.41201  0.315.050.41 00012JT
00210  1.84948  2.41204  0.313.810.53 1004YBT
00211  0.29855  2.30851  0.315.370.41 00012JT
00211  0.29835  2.30864  0.214.940.41 100516T
00212  2.30124  2.38237  0.214.170.53 1004YBT
00212  2.30105  2.38217  0.315.430.41 00012JT
00213  0.26023  1.44098  0.314.900.33 00012JT
00213  0.25993  1.44125  0.214.460.42 100516T
00214  1.19639  2.22148  0.315.300.41 00012JT
00214  1.19636  2.22151  0.214.920.42 100516T
00215  2.08802  0.20995  0.314.400.35 00012JF
00216  2.31317  2.36684  0.314.620.41 00012JT
00216  2.31339  2.36699  0.213.650.53 1004YBT
00217  2.15441  2.37694  0.314.550.53 1004YBF
00218  1.51568  1.66125  0.314.720.42 100516T
00218  1.51584  1.66121  0.314.990.37 00012JT
00219  2.47775  2.35706  0.415.210.41 00012JT
00219  2.47799  2.35721  0.314.210.53 1004YBT
00220  1.27512  0.72992  0.411.950.27 00012JT
00220  1.27502  0.73029  0.310.470.42 100516T
00221  0.55983  1.68184  0.315.460.35 00012JF
00222  1.23168  1.08051  0.313.990.30 00012JT
00222  1.23148  1.08090  0.313.530.42 100516T
00223  2.40150  2.35797  0.213.360.53 1004YBT
00223  2.40131  2.35773  0.314.370.41 00012JT
00224  0.39372  1.85046  0.211.760.42 100516T
00224  0.39400  1.85030  0.412.410.37 00012JT
00225  2.43265  2.36173  0.415.220.41 00012JT
00225  2.43287  2.36189  0.214.020.53 1004YBT
00226  0.60803  0.67104  0.314.520.21 00012JT
00226  0.60786  0.67142  0.313.980.42 100516T
00227  1.69441  2.35771  0.311.540.53 1004YBT
00227  1.69418  2.35783  0.413.050.41 00012JT
00227  1.69426  2.35781  0.312.300.42 100516T
00228  2.00114  1.06616  0.314.010.37 03012JF
00229  1.18974  1.02137  0.314.070.29 00012JT
00229  1.18947  1.02165  0.313.580.42 100516T
00230  1.63662  1.40178  0.314.360.36 00012JT
00230  1.63632  1.40204  0.314.280.42 100516T
00231  0.48076  0.35480  0.315.040.16 00012JF
00232  2.24584  2.31857  0.313.140.53 1004YBT
00232  2.24559  2.31839  0.313.930.41 00012JT
00233  1.37568  0.95790  0.315.130.30 00012JT
00233  1.37539  0.95824  0.314.520.42 100516T
00234  0.68486  2.35517  0.212.300.42 100516T
00234  0.68490  2.35515  0.313.030.41 00012JT
00235  0.41417  1.43276  0.312.740.32 00012JT
00235  0.41392  1.43302  0.212.160.42 100516T
00236  1.92828  0.31547  0.314.200.34 00012JF
00237  1.65559  1.96430  0.313.520.42 100516T
00237  1.65562  1.96422  0.313.770.41 00012JT
00238  1.30237  1.68539  0.315.270.37 00012JT
00238  1.30213  1.68555  0.214.840.42 100516T
00239  0.23664  1.75959  0.315.410.36 00012JT
00239  0.23631  1.75981  0.214.830.42 100516T
00240  0.00809  0.42388  0.313.560.42 100516T
00240  0.00822  0.42357  0.314.360.25 00012JT
00241  2.41729  2.33902  0.314.790.41 00012JT
00241  2.41742  2.33911  0.213.810.53 1004YBT
00242  0.50318  1.56369  0.215.000.42 100516T
00242  0.50349  1.56342  0.315.550.34 00012JT
00243  2.27141  2.33705  0.314.600.41 00012JT
00243  2.27161  2.33716  0.213.540.53 1004YBT
00244  1.92233  2.35331  0.314.830.42 100516T
00244  1.92235  2.35347  0.313.930.53 1004YBT
00244  1.92210  2.35338  0.315.190.41 00012JT
00245  0.73468  2.35533  0.312.960.41 00012JT
00245  0.73459  2.35538  0.212.560.42 100516T
00246  2.44013  2.35057  0.214.330.53 1004YBF
00247  1.84448  2.35002  0.313.700.41 00012JT
00247  1.84465  2.35002  0.312.480.53 1004YBT
00247  1.84467  2.34999  0.313.070.42 100516T
00248  0.99310  2.38539  0.410.940.41 00012JT
00248  0.99310  2.38543  0.3 9.850.42 100516T
00249  1.08684  0.56487  0.314.590.23 00012JT
00249  1.08668  0.56521  0.314.290.42 100516T
00250  2.32185  2.34830  0.214.520.53 1004YBF
00251  0.00306  1.24733  1.211.780.32 00012JT
00251  0.00270  1.24749  0.211.410.42 100516T
00252  2.27522  2.34410  0.214.310.53 1004YBF
00253  1.39999  0.93349  0.314.670.30 00012JT
00253  1.39974  0.93386  0.314.280.42 100516T
00254  0.67038  2.00038  0.211.740.42 100516T
00254  0.67058  2.00022  0.412.520.38 00012JT
00255  1.80981  2.34045  0.313.250.53 1004YBT
00255  1.80976  2.34050  0.314.130.42 100516T
00255  1.80959  2.34056  0.314.340.41 00012JT
00256  0.86557  2.11696  0.314.610.40 03012JF
00257  1.99768  2.05502  0.314.990.41 00012JT
00257  1.99797  2.05497  0.315.170.42 100516T
00258  1.62586  2.04574  0.315.050.42 100516T
00258  1.62586  2.04570  0.315.240.41 00012JT
00259  1.62599  1.69665  0.313.750.42 100516T
00259  1.62609  1.69644  0.314.000.38 00012JT
00260  1.04228  1.27429  0.214.270.42 100516T
00260  1.04249  1.27397  0.315.050.31 00012JT
00261  1.35826  1.68632  0.215.160.42 100516T
00261  1.35844  1.68618  0.315.480.37 00012JT
00262  0.38819  2.22368  0.313.600.41 00012JT
00262  0.38803  2.22379  0.212.940.42 100516T
00263  0.14321  1.06613  0.3 9.030.42 100516T
00263  0.14349  1.06604  0.4 9.810.29 00012JT
00264  0.05317  1.12007  0.410.660.30 03012JF
00265  0.24859  1.93557  0.313.510.38 00012JT
00265  0.24831  1.93572  0.213.060.42 100516T
00266  1.73964  1.68157  0.315.170.42 100516T
00266  1.74006  1.68158  0.313.990.53 1004YBT
00267  1.54414  1.72871  0.315.040.38 00012JT
00267  1.54406  1.72867  0.314.830.42 100516T
00268  0.62459  1.58123  0.214.470.42 100516T
00268  0.62487  1.58101  0.315.350.34 00012JT
00269  2.20570  1.25450  0.3 9.310.53 1004YBT
00269  2.20552  1.25428  0.410.780.40 00012JT
00270  2.33612  1.67788  0.314.640.53 1004YBF
00271  0.11537  1.38334  0.315.500.33 00012JT
00271  0.11507  1.38349  0.214.870.42 100516T
00272  0.53099  0.73006  0.313.090.42 100516T
00272  0.53110  0.72970  0.313.750.22 00012JT
00273  1.87645  1.67288  0.314.850.42 100516T
00273  1.87653  1.67291  0.315.090.40 00012JT
00273  1.87689  1.67304  0.313.760.53 1004YBT
00274  0.65038  2.26769  0.214.090.42 100516T
00274  0.65048  2.26764  0.314.540.41 00012JT
00275  0.04758  0.52216  0.314.110.42 100516T
00275  0.04770  0.52191  0.314.770.25 00012JT
00276  0.81419  0.51417  0.314.390.19 00012JT
00276  0.81410  0.51452  0.314.020.42 100516T
00277  0.05532  0.81005  0.314.310.27 00012JT
00277  0.05509  0.81036  0.313.370.42 100516T
00278  0.75570  2.11751  0.213.830.42 100516T
00278  0.75579  2.11747  0.314.880.40 00012JT
00279  0.89055  2.13760  0.315.470.40 00012JF
00280  0.81624  0.67152  0.314.980.21 00012JT
00280  0.81609  0.67191  0.314.590.42 100516T
00281  0.73347  2.26121  0.315.130.41 00012JT
00281  0.73332  2.26123  0.214.780.42 100516T
00282  1.12052  2.22591  0.214.590.42 120516T
00282  1.12074  2.22579  0.315.460.41 02012JT
00283  2.11873  2.04427  0.312.840.53 1004YBT
00283  2.11851  2.04420  0.313.550.41 00012JT
00284  1.16619  1.54950  0.213.750.42 100516T
00284  1.16636  1.54930  0.314.110.35 00012JT
00285  2.40575  1.24702  0.314.360.53 1004YBF
00286  0.52693  0.41513  0.5 9.630.16 00012JT
00286  0.52695  0.41541  0.3 9.140.42 100516T
00287  0.95732  2.31935  0.314.200.41 00012JT
00287  0.95730  2.31949  0.214.010.42 100516T
00288  1.01143  0.16326  0.314.010.20 00012JF
00289  0.03547  0.72195  0.315.530.26 00012JT
00289  0.03529  0.72227  0.314.630.42 100516T
00290  1.24234  0.83250  0.314.610.42 100516T
00290  1.24250  0.83206  0.314.760.27 00012JT
00291  0.19285  2.30686  0.315.180.41 00012JT
00291  0.19264  2.30695  0.214.720.41 100516T
00292  1.57613  1.87341  0.312.460.40 00012JT
00292  1.57607  1.87350  0.311.910.42 100516T
00293  2.36692  1.24400  0.1 7.300.00 40+056T
00293  2.36650  1.24384  0.3 7.270.53 1304YBT
00293  2.36682  1.24401  0.5 7.920.41 00012JT
00294  2.10554  0.96328  0.313.430.53 1004YBT
00294  2.10536  0.96293  0.314.430.37 00012JT
00295  2.26219  1.66825  0.413.080.41 00012JT
00295  2.26241  1.66853  0.312.160.53 1004YBT
00296  1.55892  1.56217  0.312.910.42 100516T
00296  1.55907  1.56206  0.313.520.37 00012JT
00297  0.30277  2.43182  0.412.130.41 00012JT
00297  0.30263  2.43188  0.211.560.41 100516T
00298  0.06415  1.20809  0.211.890.42 100516T
00298  0.06453  1.20790  0.412.310.31 00012JT
00299  2.09864  1.26568  0.313.890.39 00012JT
00299  2.09893  1.26599  0.312.410.53 1004YBT
00300  1.87585  1.26432  0.313.590.53 1004YBT
00300  1.87551  1.26412  0.314.610.37 00012JT
00300  1.87536  1.26424  0.314.550.42 100516T
00301  0.91265  2.38810  0.314.770.41 00012JT
00301  0.91258  2.38816  0.214.560.42 100516T
00302  1.43894  2.02157  0.313.620.40 00012JT
00302  1.43888  2.02166  0.213.200.42 100516T
00303  2.19495  1.26663  0.314.170.53 1004YBF
00304  1.99025  1.65574  0.314.400.40 00012JT
00304  1.99019  1.65573  0.314.010.42 100516T
00304  1.99060  1.65597  0.313.570.53 1004YBT
00305  0.95468  0.96048  0.312.750.42 100516T
00305  0.95494  0.96010  0.313.390.26 00012JT
00306  0.20357  2.09619  0.414.940.40 03012JF
00307  1.39791  0.15610  0.312.200.27 03012JF
00308  1.25396  2.22866  0.213.520.42 100516T
00308  1.25399  2.22856  0.313.980.41 00012JT
00309  1.43294  0.30745  0.314.690.27 03012JF
00310  0.30799  2.40856  0.315.100.41 00012JT
00310  0.30785  2.40861  0.214.560.41 100516T
00311  2.04491  2.04067  0.314.360.42 100516T
00311  2.04503  2.04095  0.313.960.53 1004YBT
00311  2.04473  2.04085  0.315.160.41 00012JT
00312  0.37906  1.48492  0.213.630.42 100516T
00312  0.37942  1.48465  0.313.980.33 00012JT
00313  0.75390  0.53203  0.313.340.42 100516T
00313  0.75399  0.53167  0.314.050.18 00012JT
00314  1.80381  0.20708  0.313.410.32 00012JF
00315  0.01111  0.48529  0.312.300.42 100516T
00315  0.01129  0.48496  0.312.990.25 00012JT
00316  2.28248  1.24053  0.315.530.40 00012JT
00316  2.28280  1.24093  0.314.170.53 1004YBT
00317  0.73672  1.50426  0.314.620.33 00012JT
00317  0.73648  1.50448  0.214.060.42 100516T
00318  0.72464  1.77306  0.212.970.42 100516T
00318  0.72492  1.77299  0.313.270.36 00012JT
00319  0.13643  0.35200  0.314.990.23 00012JF
00320  1.74928  1.71149  0.312.830.53 1004YBT
00320  1.74897  1.71149  0.314.060.39 00012JT
00320  1.74890  1.71143  0.313.730.42 100516T
00321  2.13242  2.04063  0.313.260.53 1004YBT
00321  2.13215  2.04050  0.313.890.41 00012JT
00322  1.59074  2.32632  0.411.220.41 00012JT
00322  1.59087  2.32636  0.310.660.42 100516T
00323  0.75912  1.11406  0.213.580.42 100516T
00323  0.75936  1.11376  0.313.900.28 00012JT
00324  0.53298  1.70632  0.211.830.42 100516T
00324  0.53329  1.70603  0.412.300.35 00012JT
00325  0.44852  0.19636  0.412.130.17 00012JF
00326  2.44593  1.72695  0.213.600.53 1004YBT
00326  2.44573  1.72677  0.314.360.41 00012JT
00327  2.01707  2.03732  0.314.280.41 00012JT
00327  2.01732  2.03742  0.313.270.53 1004YBT
00327  2.01729  2.03733  0.313.490.42 100516T
00328  1.48473  1.48434  0.315.030.36 00012JT
00328  1.48453  1.48443  0.314.620.42 100516T
00329  1.42806  0.83947  0.311.460.42 100516T
00329  1.42828  0.83915  0.412.080.30 00012JT
00330  1.93569  2.03697  0.314.550.41 00012JT
00330  1.93585  2.03692  0.314.210.42 100516T
00330  1.93598  2.03711  0.313.560.53 1004YBT
00331  1.14700  0.89401  0.314.880.42 100516T
00331  1.14726  0.89372  0.315.500.27 00012JT
00332  2.43924  1.73107  0.213.950.53 1004YBT
00332  2.43897  1.73084  0.315.060.41 00012JT
00333  0.07721  0.92232  0.314.340.42 100516T
00333  0.07753  0.92204  0.314.620.28 00012JT
00334  2.01693  1.73148  0.312.740.53 1004YBT
00334  2.01668  1.73131  0.312.800.42 100516T
00334  2.01664  1.73133  0.313.400.41 00012JT
00335  2.21992  1.73139  0.315.260.41 00012JT
00335  2.22015  1.73163  0.314.200.53 1004YBT
00336  1.00854  2.45502  0.213.500.42 100516T
00336  1.00858  2.45487  0.313.910.41 00012JT
00337  1.19289  1.53639 10.7 8.360.35 03012JT
00337  1.19297  1.53688  0.3 8.420.42 100516T
00338  1.45900  2.28795  0.213.870.42 100516T
00338  1.45895  2.28800  0.314.140.41 00012JT
00339  0.16084  0.60244  0.314.940.24 00012JT
00339  0.16074  0.60273  0.314.330.42 100516T
00340  1.51286  1.97239  0.313.530.42 100516T
00340  1.51289  1.97234  0.314.010.40 00012JT
00341  1.61649  1.92256  0.3 9.090.42 100516T
00341  1.61646  1.92261  0.410.040.40 03012JT
00342  0.52123  2.02924  0.412.510.39 00012JT
00342  0.52103  2.02941  0.211.820.42 100516T
00343  1.28973  2.17622  0.314.040.41 00012JT
00343  1.28975  2.17635  0.213.860.42 100516T
00344  2.22615  1.65209  0.314.540.53 1004YBF
00345  2.31967  1.64838  0.313.960.41 00012JT
00345  2.31991  1.64862  0.313.320.53 1004YBT
00346  1.63281  0.58614  0.313.020.30 00012JT
00346  1.63270  0.58657  0.312.460.42 100516T
00347  0.99748  0.51346  0.315.020.21 00012JF
00348  1.58229  0.55604  0.313.700.30 00012JT
00348  1.58214  0.55644  0.313.340.42 100516T
00349  0.69321  0.88885  0.313.890.42 100516T
00349  0.69344  0.88856  0.315.010.24 00012JT
00350  2.09871  1.05615  0.315.470.38 03012JF
00351  1.16295  2.35675  1.212.880.41 00012JT
00351  1.16298  2.35679  0.212.400.42 100516T
00352  1.61507  0.55242  0.313.250.30 00012JT
00352  1.61489  0.55286  0.312.860.42 100516T
00353  0.54374  0.57229  0.313.150.42 100516T
00353  0.54387  0.57189  0.313.610.19 00012JT
00354  2.02827  0.95089  0.411.410.36 00012JT
00354  2.02845  0.95113  0.3 9.990.53 1004YBT
00354  2.02806  0.95097  0.4 9.940.42 100516T
00355  0.35251  1.49911  0.314.290.33 03012JF
00356  2.23427  0.94999  0.313.980.38 00012JT
00356  2.23447  0.95044  0.312.920.53 1004YBT
00357  2.29689  0.94982  0.313.660.39 00012JT
00357  2.29709  0.95024  0.312.650.53 1004YBT
00358  2.00668  1.63835  0.313.130.42 100516T
00358  2.00673  1.63831  0.313.520.40 00012JT
00358  2.00709  1.63853  0.312.790.53 1004YBT
00359  0.58310  1.84120  0.314.450.37 03012JT
00359  0.58295  1.84140  0.215.440.42 130516T
00360  0.26306  2.42351  0.310.100.40 100516T
00360  0.26320  2.42340  0.510.890.41 00012JT
00361  0.86067  0.34963  0.313.340.17 00012JF
00362  0.18467  0.73131  0.314.900.42 100516T
00362  0.18482  0.73094  0.315.270.25 00012JT
00363  1.67505  0.17399  0.313.800.30 00012JF
00364  0.96117  0.27848  0.312.630.19 00012JF
00365  1.72952  0.29987  0.315.500.31 00012JF
00366  1.89104  0.27283  0.315.330.33 00012JF
00367  1.59377  2.37146  0.314.380.41 00012JT
00367  1.59396  2.37151  0.314.040.42 100516T
00368  0.89599  2.47031  0.315.240.41 00012JT
00368  0.89591  2.47036  0.214.630.42 100516T
00369  0.46870  2.43460  0.313.870.41 00012JT
00369  0.46861  2.43474  0.213.520.42 100516T
00370  2.20774  1.27633  0.314.520.40 00012JT
00370  2.20805  1.27672  0.313.420.53 1004YBT
00371  0.57471  2.04257  0.315.490.39 00012JF
00372  1.35407  1.16789  0.313.520.42 100516T
00372  1.35426  1.16762  0.314.580.32 03012JT
00373  0.76527  1.42693  0.214.740.42 100516T
00373  0.76554  1.42676  0.315.160.32 00012JT
00374  0.18392  0.67317  0.315.200.42 130516T
00374  0.18403  0.67285  0.314.010.24 03012JT
00375  1.24427  0.96199  0.315.030.29 03012JF
00376  2.29058  1.62084  0.313.800.41 00012JT
00376  2.29079  1.62110  0.312.770.53 1004YBT
00377  1.44452  0.10952  0.314.960.28 00012JF
00378  2.31750  1.62033  0.315.530.41 00012JT
00378  2.31775  1.62058  0.314.550.53 1004YBT
00379  0.27133  1.75295  0.214.380.42 100516T
00379  0.27162  1.75281  0.315.370.36 00012JT
00380  1.81799  1.61519  0.314.350.39 00012JT
00380  1.81833  1.61527  0.313.380.53 1004YBT
00381  0.00109  1.08886  0.4 9.670.30 03012JT
00381  0.00071  1.08902  0.3 9.290.42 100516T
00382  0.03521  1.95608  0.213.920.42 100516T
00382  0.03545  1.95591  0.314.280.39 00012JT
00383  1.38664  0.16124  0.315.290.27 00012JF
00384  0.22253  0.52314  0.314.660.42 100516T
00384  0.22258  0.52276  0.315.080.22 00012JT
00385  2.10284  0.18593  0.313.160.36 00012JF
00386  0.82662  1.79159  0.313.510.36 00012JT
00386  0.82634  1.79182  0.213.070.42 100516T
00387  1.60616  1.28363  0.313.210.35 00012JT
00387  1.60592  1.28383  0.312.740.42 100516T
00388  1.40650  2.20618  0.213.810.42 100516T
00388  1.40647  2.20617  0.314.250.41 00012JT
00389  1.51114  1.03449  0.314.640.32 03012JF
00390  0.99441  2.31837  0.314.060.41 00012JT
00390  0.99433  2.31840  0.213.770.42 100516T
00391  0.53442  2.21992  0.312.480.41 00012JT
00391  0.53423  2.22009  0.212.150.42 100516T
00392  2.43441  1.28509  0.412.290.41 00012JT
00392  2.43462  1.28542  0.311.620.53 1004YBT
00393  1.48632  2.24644  0.214.630.42 100516T
00393  1.48634  2.24641  0.314.820.41 00012JT
00394  2.47876  0.92926  0.314.190.53 1004YBF
00395  0.42338  0.34120  0.315.250.17 00012JF
00396  0.62751  1.02271  0.314.030.42 100516T
00396  0.62774  1.02243  0.314.470.26 00012JT
00397  0.19268  2.34964  0.213.600.40 100516T
00397  0.19289  2.34956  0.314.530.41 00012JT
00398  1.26512  2.01031  0.212.780.42 100516T
00398  1.26516  2.01017  0.313.300.40 00012JT
00399  0.55033  0.32443  0.315.500.14 00012JF
00400  0.60359  0.72800  0.313.340.42 100516T
00400  0.60370  0.72768  0.313.880.22 00012JT
00401  0.34416  1.13319  0.212.960.42 100516T
00401  0.34438  1.13299  0.313.590.29 00012JT
00402  0.31522  1.80889  0.213.390.42 100516T
00402  0.31551  1.80877  0.313.760.37 00012JT
00403  2.43887  1.58793  0.214.210.53 1004YBT
00403  2.43859  1.58763  0.315.420.41 00012JT
00404  2.01491  1.58685  0.314.140.53 1004YBT
00404  2.01455  1.58666  0.314.440.42 100516T
00404  2.01449  1.58664  0.315.200.40 00012JT
00405  2.49207  0.92363  0.314.460.41 00012JT
00405  2.49200  0.92411  0.515.080.40 0000WST
00405  2.49219  0.92404  0.313.080.53 1004YBT
00406  0.80984  0.14521  0.313.680.17 00012JF
00407  0.05106  2.09715  0.412.500.40 03012JF
00408  0.64199  2.09087  0.315.170.39 00012JT
00408  0.64185  2.09098  0.214.440.42 100516T
00409  0.09388  0.93928  0.314.420.28 00012JT
00409  0.09361  0.93956  0.313.270.42 100516T
00410  1.42711  0.14004  0.315.130.27 00012JF
00411  1.86236  0.92031  1.215.660.42 100516T
00411  1.86282  0.92036  0.314.940.53 1304YBT
00411  1.86256  0.92005  0.314.720.35 03012JT
00412  1.85229  1.29162  0.315.350.37 00012JT
00412  1.85213  1.29170  0.314.170.42 100516T
00412  1.85256  1.29171  0.313.330.53 1004YBT
00413  0.27554  0.79206  0.313.880.42 100516T
00413  0.27572  0.79171  0.314.730.25 00012JT
00414  1.29251  1.95760  0.213.800.42 100516T
00414  1.29262  1.95747  0.314.270.39 00012JT
00415  1.63524  1.75019  0.311.470.42 100516T
00415  1.63528  1.75012  0.412.110.39 00012JT
00416  0.70508  0.75134  0.313.710.22 03012JT
00416  0.70500  0.75164  0.314.310.42 130516T
00417  1.47434  0.45352  0.314.890.28 03012JF
00418  1.59273  2.18841  0.313.970.41 00012JT
00418  1.59282  2.18847  0.313.390.42 100516T
00419  2.42944  0.91050  0.314.370.40 00012JT
00419  2.42962  0.91091  0.313.290.53 1004YBT
00420  0.85970  1.01036  0.312.640.42 100516T
00420  0.85991  1.01002  0.313.180.27 00012JT
00421  1.90686  1.09412  1.214.940.36 05012JF
00422  1.78173  0.91259  0.413.990.53 1004YBT
00422  1.78139  0.91226  0.315.490.34 00012JT
00423  1.94586  1.57135  0.314.000.53 1004YBT
00423  1.94549  1.57117  0.314.750.42 100516T
00423  1.94552  1.57109  0.315.230.39 00012JT
00424  1.96419  1.65507  0.315.510.40 00012JF
00425  1.92851  0.90603  0.313.750.53 1004YBT
00425  1.92823  0.90561  0.315.070.35 00012JT
00425  1.92802  0.90591  0.314.560.42 100516T
00426  0.77510  1.94881  0.314.850.38 00012JT
00426  0.77491  1.94899  0.214.380.42 100516T
00427  0.94296  0.76809  0.313.450.24 00012JT
00427  0.94280  0.76859  0.313.150.42 100516T
00428  1.00214  2.24257  0.210.620.42 100516T
00428  1.00227  2.24246  0.411.820.41 00012JT
00429  2.44818  1.29751  0.313.890.53 1004YBT
00429  2.44789  1.29719  0.315.550.41 00012JT
00430  1.95103  0.23478  0.412.270.34 00012JF
00431  1.75239  0.90292  0.314.950.33 03012JT
00431  1.75278  0.90313  1.214.870.53 1304YBT
00432  1.48459  2.24250  0.315.050.41 00012JF
00433  0.37653  2.04040  0.412.350.39 00012JT
00433  0.37626  2.04052  0.211.990.42 100516T
00434  1.57410  1.21836  0.313.730.34 00012JT
00434  1.57388  1.21859  0.313.420.42 100516T
00435  0.42323  1.73267  0.213.640.42 100516T
00435  0.42350  1.73249  0.314.130.36 00012JT
00436  1.00478  2.49253  0.314.100.41 00012JT
00436  1.00473  2.49256  0.213.710.42 100516T
00437  1.70626  1.29858  0.314.540.42 100516T
00437  1.70673  1.29845  0.313.630.53 1004YBT
00437  1.70643  1.29833  0.315.020.36 00012JT
00438  1.02767  2.10039  0.412.950.40 00012JT
00438  1.02755  2.10048  0.212.510.42 100516T
00439  0.08605  0.42623  0.315.230.24 00012JT
00439  0.08603  0.42652  0.314.480.42 100516T
00440  0.65677  2.05196  0.214.550.42 100516T
00440  0.65694  2.05182  0.315.160.39 00012JT
00441  0.29372  1.32004  0.214.860.42 100516T
00441  0.29401  1.31980  0.315.260.31 00012JT
00442  0.81809  2.06537  0.212.320.42 100516T
00442  0.81822  2.06529  0.412.980.39 00012JT
00443  0.96490  1.29354  0.315.520.31 00012JT
00443  0.96460  1.29376  0.215.270.42 100516T
00444  2.40681  0.89604  0.314.190.53 1004YBT
00444  2.40664  0.89553  0.315.520.40 00012JT
00445  2.08962  1.30291  0.313.230.53 1004YBT
00445  2.08930  1.30265  0.314.100.39 00012JT
00446  0.38252  0.31598  0.313.220.18 00012JF
00447  2.30537  1.30293  0.314.970.41 03012JT
00447  2.30568  1.30332  0.313.990.53 1004YBT
00448  0.09484  1.50761  0.314.320.34 00012JT
00448  0.09452  1.50776  0.213.850.42 100516T
00449  1.31625  0.83429  0.313.460.42 100516T
00449  1.31642  0.83393  0.314.230.28 00012JT
00450  2.00469  1.53519  0.315.470.40 00012JT
00450  2.00471  1.53521  0.314.570.42 100516T
00450  2.00506  1.53538  0.313.930.53 1004YBT
00451  0.36526  0.30145  0.410.120.18 00012JF
00452  1.66621  0.74917  0.315.200.32 00012JT
00452  1.66607  0.74970  0.314.810.42 100516T
00453  2.42812  1.30845  0.314.540.53 1004YBF
00454  0.44539  2.02969  0.212.650.42 100516T
00454  0.44562  2.02951  0.313.110.39 00012JT
00455  1.58489  0.22532  0.315.070.29 00012JF
00456  0.41619  1.44355  0.213.770.42 100516T
00456  0.41645  1.44333  0.314.140.32 00012JT
00457  2.17368  0.88603  0.310.820.53 1004YBT
00457  2.17284  0.88583  1.211.440.38 03012JT
00458  0.71199  1.85568  0.215.030.42 100516T
00458  0.71225  1.85553  0.315.430.37 00012JT
00459  2.40179  0.88601  0.314.340.40 03012JT
00459  2.40198  0.88641  0.314.880.53 1304YBT
00460  0.97130  1.09443  0.312.950.28 00012JT
00460  0.97102  1.09475  0.312.750.42 100516T
00461  2.04021  1.31102  0.315.290.38 00012JT
00461  2.04057  1.31133  0.314.240.53 1004YBT
00462  1.95329  1.31100  0.310.390.42 100516T
00462  1.95361  1.31114  0.310.450.53 1004YBT
00462  1.95336  1.31096  0.411.230.38 00012JT
00463  0.75467  0.86298  0.312.810.42 100516T
00463  0.75487  0.86270  0.313.280.24 00012JT
00464  0.98620  0.48577  0.314.010.42 100516T
00464  0.98622  0.48530  0.314.480.21 00012JT
00465  0.91175  1.40915  0.311.050.42 100516T
00465  0.91203  1.40889  0.411.500.32 00012JT
00466  2.42521  0.88376  0.315.150.40 00012JT
00466  2.42539  0.88419  0.313.980.53 1004YBT
00467  2.42481  1.51912  0.314.850.41 00012JT
00467  2.42506  1.51938  0.213.970.53 1004YBT
00468  0.24030  0.05351  0.314.030.22 00012JF
00469  1.01146  1.70133  0.313.860.36 00012JT
00469  1.01121  1.70159  0.213.610.42 100516T
00470  0.10241  0.63113  0.314.820.25 00012JT
00470  0.10227  0.63144  0.314.170.42 100516T
00471  2.10918  1.32132  0.313.690.53 1004YBT
00471  2.10889  1.32104  0.314.200.39 00012JT
00472  1.87545  0.88000  0.411.180.35 00012JT
00472  1.87531  0.88015  0.310.170.42 100516T
00472  1.87566  0.88037  0.4 9.880.53 1004YBT
00473  2.16487  0.88040  0.314.520.53 1004YBF
00474  1.02506  0.19017  0.315.020.21 00012JF
00475  1.68356  0.87799  0.313.930.33 00012JT
00475  1.68337  0.87825  0.313.400.42 100516T
00475  1.68410  0.87823  0.412.880.53 1004YBT
00476  0.78233  2.30838  0.313.160.41 00012JT
00476  0.78217  2.30836  0.212.600.42 100516T
00477  1.55893  1.45555  0.311.800.42 100516T
00477  1.55911  1.45539  0.412.240.36 00012JT
00478  2.10830  1.32410  0.314.050.53 1304YBF
00479  2.03086  1.32468  0.314.460.38 00012JT
00479  2.03070  1.32476  0.313.950.42 100516T
00479  2.03124  1.32498  0.313.500.53 1004YBT
00480  1.12501  1.63452  0.412.490.35 00012JT
00480  1.12480  1.63470  0.211.930.42 100516T
00481  1.70868  0.87809  0.313.960.42 100516T
00481  1.70934  0.87808  0.413.160.53 1004YBT
00481  1.70887  0.87782  0.314.700.33 00012JT
00482  0.96965  1.59100  0.213.830.42 100516T
00482  0.96992  1.59081  0.314.440.34 00012JT
00483  1.38712  1.36070  0.314.440.42 100516T
00483  1.38741  1.36049  0.314.800.34 00012JT
00484  0.22141  0.46674  0.315.050.22 00012JT
00484  0.22136  0.46710  0.314.460.42 100516T
00485  2.14767  1.50037  0.314.630.53 1004YBF
00486  1.77546  1.32492  0.312.310.42 100516T
00486  1.77586  1.32495  0.311.730.53 1004YBT
00486  1.77566  1.32484  0.312.790.36 00012JT
00487  1.71741  1.49728  0.310.810.53 1004YBT
00487  1.71706  1.49736  0.311.220.42 100516T
00487  1.71719  1.49730  0.411.880.37 00012JT
00488  1.85490  1.49610  0.314.940.38 00012JT
00488  1.85523  1.49622  0.313.800.53 1004YBT
00488  1.85482  1.49609  0.314.470.42 100516T
00489  0.33278  0.89345  0.312.170.42 100516T
00489  0.33298  0.89318  0.312.870.26 00012JT
00490  1.57260  1.24557  0.512.830.42 120516T
00490  1.57283  1.24536  0.413.590.34 02012JT
00491  1.55889  2.10031  0.314.520.42 100516T
00491  1.55887  2.10027  0.314.720.41 00012JT
00492  1.12304  2.22540  0.511.700.41 02012JT
00492  1.12319  2.22546  0.311.240.42 120516T
00493  2.36961  1.32953  0.314.140.53 1004YBT
00493  2.36934  1.32919  0.315.190.41 00012JT
00494  1.16856  0.58393  0.315.400.24 00012JF
00495  1.33068  0.60268  0.315.190.27 00012JT
00495  1.33055  0.60305  0.314.390.42 100516T
00496  2.47841  0.86947  0.314.390.53 1004YBF
00497  1.80771  1.49258  0.314.420.42 100516T
00497  1.80813  1.49260  0.313.760.53 1004YBT
00497  1.80786  1.49260  0.315.480.38 00012JT
00498  2.19398  1.49147  0.313.900.53 1004YBT
00498  2.19366  1.49106  0.314.660.41 00012JT
00499  1.84063  1.48956  0.314.280.38 00012JT
00499  1.84047  1.48963  0.313.930.42 100516T
00499  1.84090  1.48965  0.312.980.53 1004YBT
00500  1.78008  0.86684  0.315.010.42 100516T
00500  1.78031  0.86648  0.315.360.33 00012JT
00500  1.78059  0.86681  0.413.930.53 1004YBT
00501  2.41727  0.15397  0.315.350.39 00012JF
00502  1.66204  0.70437  0.313.800.42 100516T
00502  1.66220  0.70396  0.314.370.31 00012JT
00503  1.87105  0.08907  0.314.420.33 00012JF
00504  0.02045  0.84031  0.314.370.28 00012JT
00504  0.02016  0.84056  0.313.820.42 100516T
00505  0.77754  1.53008  0.211.540.42 100516T
00505  0.77781  1.52998  0.412.350.33 00012JT
00506  0.95560  2.29710  0.314.710.41 00012JT
00506  0.95562  2.29722  0.214.180.42 100516T
00507  2.06371  0.86565  0.312.630.36 00012JT
00507  2.06393  0.86603  0.311.880.53 1004YBT
00507  2.06340  0.86575  0.412.260.42 100516T
00508  0.31238  0.08555  0.314.350.20 00012JF
00509  1.47930  1.48533  0.314.460.36 00012JT
00509  1.47906  1.48541  0.313.960.42 100516T
00510  2.08070  1.33506  0.412.000.39 00012JT
00510  2.08103  1.33527  0.311.110.53 1004YBT
00510  2.08059  1.33504  0.311.590.42 100516T
00511  1.83540  1.48395  0.312.140.53 1004YBT
00511  1.83499  1.48383  0.312.680.42 100516T
00511  1.83512  1.48385  0.412.910.38 00012JT
00512  2.00513  1.48281  0.312.460.42 100516T
00512  2.00514  1.48278  0.313.390.39 00012JT
00512  2.00546  1.48302  0.312.370.53 1004YBT
00513  0.15398  0.20022  0.314.940.22 00012JF
00514  2.15820  1.33929  0.313.760.53 1004YBT
00514  2.15786  1.33894  0.314.680.40 00012JT
00515  1.87483  0.86283  0.413.260.53 1004YBT
00515  1.87460  0.86245  0.314.650.34 00012JT
00515  1.87441  0.86271  0.314.270.42 100516T
00516  0.18551  2.22330  0.315.070.41 00012JT
00516  0.18535  2.22341  0.214.490.41 100516T
00517  2.19206  1.47909  0.314.510.53 1004YBF
00518  2.18799  0.85834  0.315.400.38 00012JT
00518  2.18818  0.85886  0.314.180.53 1004YBT
00519  1.83733  1.47673  0.315.420.38 00012JT
00519  1.83768  1.47683  0.314.070.53 1004YBT
00520  1.64255  0.08254  0.314.400.30 00012JF
00521  1.91377  1.34677  0.3 9.200.53 1304YBT
00521  1.91355  1.34658  0.410.420.38 00012JT
00521  1.91361  1.34649  0.3 9.140.42 100516T
00522  0.51676  1.89296  0.315.360.37 00012JT
00522  0.51650  1.89305  0.214.980.42 100516T
00523  0.69543  1.90414  0.215.100.42 100516T
00523  0.69561  1.90396  0.315.440.37 00012JT
00524  0.17280  1.16510  0.315.060.30 00012JT
00524  0.17252  1.16534  0.214.860.42 100516T
00525  1.80708  1.34840  0.314.920.42 100516T
00525  1.80728  1.34835  0.315.540.37 00012JT
00525  1.80756  1.34841  0.313.790.53 1004YBT
00526  0.53993  0.92724  0.314.040.42 100516T
00526  0.54014  0.92695  0.314.670.25 00012JT
00527  0.38871  0.90434  0.314.310.42 100516T
00527  0.38896  0.90406  0.314.690.25 00012JT
00528  1.66674  1.46648  0.314.410.53 1004YBT
00528  1.66620  1.46664  0.314.350.42 100516T
00528  1.66637  1.46649  0.314.830.37 00012JT
00529  2.26485  1.34974  0.314.440.41 00012JT
00529  2.26513  1.35013  0.313.540.53 1004YBT
00530  0.62727  0.19123  0.314.540.14 00012JF
00531  0.77173  1.10806  0.314.250.28 00012JT
00531  0.77150  1.10825  0.213.750.42 100516T
00532  1.69587  0.30513  0.313.190.31 00012JF
00533  0.59414  1.97991  0.214.640.42 100516T
00533  0.59435  1.97970  0.315.130.38 00012JT
00534  0.56473  2.05063  0.213.930.42 100516T
00534  0.56491  2.05049  0.314.330.39 00012JT
00535  1.51394  0.82111  1.211.570.30 00012JT
00535  1.51367  0.82128  0.311.100.42 130516T
00536  1.80302  1.46401  0.312.410.53 1004YBT
00536  1.80262  1.46400  0.312.890.42 100516T
00536  1.80267  1.46402  0.313.580.38 00012JT
00537  1.90335  1.46054  0.315.000.38 00012JT
00537  1.90361  1.46071  0.313.930.53 1004YBT
00537  1.90318  1.46065  0.314.930.42 100516T
00538  1.43067  1.94793  0.313.140.40 00012JT
00538  1.43058  1.94803  0.212.830.42 100516T
00539  2.05007  1.46001  0.314.690.42 100516T
00539  2.05041  1.46026  0.314.330.53 1004YBT
00540  1.19870  1.33861  0.212.730.42 100516T
00540  1.19899  1.33844  0.313.090.32 00012JT
00541  1.02512  1.51918  0.212.190.42 100516T
00541  1.02537  1.51896  0.412.870.34 00012JT
00542  2.21615  1.36178  0.314.470.40 00012JT
00542  2.21646  1.36216  0.313.410.53 1004YBT
00543  0.70499  0.82591  0.314.440.42 100516T
00543  0.70512  0.82565  0.314.810.23 00012JT
00544  2.28220  1.45682  0.312.350.53 1004YBT
00544  2.28192  1.45655  0.313.730.41 00012JT
00545  1.98483  0.18271  0.312.480.34 00012JF
00546  1.57363  1.90403  0.313.530.40 00012JT
00546  1.57360  1.90408  0.313.080.42 130516T
00547  0.41210  1.44183  0.315.050.32 03012JF
00548  2.09641  0.06671  0.314.000.36 00012JF
00549  2.18740  0.06484  0.314.420.37 00012JF
00550  0.97874  0.69972  0.314.390.42 100516T
00550  0.97890  0.69934  0.314.610.23 00012JT
00551  0.27757  1.64465  0.211.510.42 100516T
00551  0.27791  1.64446  0.312.480.35 00012JT
00552  2.09511  1.45222  0.313.200.53 1004YBT
00552  2.09476  1.45200  0.314.170.40 00012JT
00553  0.00373  1.78724  0.314.280.37 03012JF
00554  2.26191  0.82683  0.314.500.38 00012JT
00554  2.26216  0.82736  0.313.210.53 1004YBT
00555  2.49771  1.45194  0.214.640.53 1004YBF
00556  1.71983  1.38993  0.315.510.36 00012JT
00556  1.71999  1.39002  0.314.160.53 1004YBT
00557  2.07121  1.39171  0.314.330.39 00012JT
00557  2.07153  1.39204  0.313.370.53 1004YBT
00557  2.07109  1.39172  0.313.900.42 100516T
00558  0.95986  0.61789  0.410.750.22 00012JT
00558  0.95974  0.61808  0.3 9.550.42 100516T
00559  0.06599  0.51139  0.315.350.24 00012JF
00560  1.53361  0.12134  0.4 8.850.29 03012JF
00561  0.64948  1.29908  0.213.810.42 100516T
00561  0.64977  1.29883  0.313.990.30 00012JT
00562  1.98370  1.39410  0.315.110.39 00012JT
00562  1.98401  1.39437  0.313.970.53 1004YBT
00562  1.98358  1.39422  0.314.500.42 100516T
00563  0.03874  1.28952  0.410.790.32 00012JT
00563  0.03840  1.28962  0.310.310.42 100516T
00564  2.05972  1.39666  0.314.280.53 1004YBT
00564  2.05930  1.39647  0.314.900.42 100516T
00565  0.98531  1.86064  0.213.740.42 100516T
00565  0.98552  1.86052  0.314.180.37 00012JT
00566  1.03206  1.34342  0.214.630.42 100516T
00566  1.03226  1.34313  0.314.950.32 00012JT
00567  2.01145  0.05720  0.313.830.35 00012JF
00568  1.54070  1.23615  0.310.670.42 100516T
00568  1.54085  1.23595  0.411.540.34 00012JT
00569  2.43163  1.44164  0.313.610.53 1004YBT
00569  2.43142  1.44136  0.314.890.41 00012JT
00570  1.11692  2.12928  0.213.610.42 100516T
00570  1.11695  2.12926  0.313.910.40 00012JT
00571  0.41281  1.88168  0.313.250.37 00012JT
00571  0.41259  1.88190  0.212.850.42 100516T
00572  1.44413  0.58783  0.314.790.28 00012JT
00572  1.44398  0.58824  0.314.660.42 100516T
00573  1.68555  0.80959  0.414.180.53 1004YBT
00573  1.68486  0.80967  0.315.130.42 100516T
00574  1.00036  0.60900  0.313.960.42 100516T
00574  1.00052  0.60865  0.314.950.22 00012JT
00575  0.61665  0.71093  0.315.430.21 00012JT
00575  0.61647  0.71141  0.314.890.42 100516T
00576  0.27649  2.47311  0.213.450.40 100516T
00576  0.27665  2.47305  0.313.770.41 00012JT
00577  2.48485  1.37136  0.713.840.40 0300WST
00577  2.48510  1.37146  0.312.730.53 1304YBT
00577  2.48479  1.37120  0.413.150.41 03012JT
00578  1.18851  2.02647  0.313.470.39 00012JT
00578  1.18838  2.02657  0.212.820.42 100516T
00579  1.59671  1.83028  0.312.990.42 100516T
00579  1.59682  1.83020  0.313.330.39 00012JT
00580  2.37297  0.83815  0.313.370.53 1004YBT
00580  2.37281  0.83768  0.314.550.39 00012JT
00581  1.21509  0.73278  0.313.770.26 00012JT
00581  1.21492  0.73316  0.313.350.42 100516T
00582  0.43233  1.58291  0.313.660.34 00012JT
00582  0.43202  1.58308  0.213.250.42 100516T
00583  1.32397  0.69400  0.312.700.42 100516T
00583  1.32411  0.69359  0.313.040.27 00012JT
00584  1.55197  0.51705  0.412.170.29 00012JT
00584  1.55186  0.51745  0.410.830.42 100516T
00585  1.57149  2.06215  0.312.770.41 00012JT
00585  1.57149  2.06219  0.312.360.42 100516T
00586  2.03942  0.83299  0.315.260.36 00012JT
00586  2.03974  0.83339  0.314.020.53 1004YBT
00586  2.03924  0.83314  0.414.600.42 100516T
00587  0.47894  2.00906  0.214.700.42 100516T
00587  0.47910  2.00891  0.315.290.38 00012JT
00588  0.09329  0.74539  0.314.250.42 100516T
00588  0.09355  0.74506  0.314.910.26 00012JT
00589  2.20762  1.37672  0.314.480.53 1004YBF
00590  2.18475  1.41839  0.314.440.40 00012JT
00590  2.18506  1.41876  0.313.220.53 1004YBT
00591  2.01531  1.37663  0.313.980.42 100516T
00591  2.01539  1.37649  0.314.580.39 00012JT
00591  2.01569  1.37674  0.313.650.53 1004YBT
00592  0.50230  1.36447  0.214.650.42 100516T
00592  0.50263  1.36423  0.315.280.31 00012JT
00593  0.74621  0.25267  0.315.390.14 00012JF
00594  1.64664  0.23181  0.313.850.30 00012JF
00595  1.75993  1.41320  0.313.730.42 100516T
00595  1.76034  1.41313  0.312.880.53 1004YBT
00595  1.76012  1.41311  0.314.040.37 00012JT
00596  0.00756  0.75593  0.313.310.42 100516T
00596  0.00781  0.75565  0.313.850.27 03012JT
00597  0.26638  1.52596  0.313.270.34 00012JT
00597  0.26605  1.52617  0.212.920.42 100516T
00598  1.05792  1.07604  0.313.540.42 100516T
00598  1.05813  1.07577  0.313.830.29 00012JT
00599  1.64117  2.48250  0.315.390.41 00012JF
00600  1.23461  0.52378  0.310.680.42 100516T
00600  1.23471  0.52339  0.411.370.25 00012JT
00601  1.03160  0.15164  0.314.530.21 00012JF
00602  1.96114  1.40925  0.311.680.42 100516T
00602  1.96152  1.40938  0.311.470.53 1004YBT
00602  1.96122  1.40916  0.412.870.38 00012JT
00603  0.49596  0.87011  0.311.350.42 100516T
00603  0.49620  0.86992  0.312.230.24 00012JT
00604  0.52102  1.35826  0.214.380.42 100516T
00604  0.52129  1.35807  0.314.970.31 00012JT
00605  0.51513  0.63176  0.315.520.20 03012JF
00606  1.26066  0.84214  0.313.500.42 100516T
00606  1.26082  0.84172  0.314.080.28 00012JT
00607  1.80439  1.40511  0.313.940.53 1004YBT
00607  1.80395  1.40507  0.315.120.42 100516T
00608  1.28181  1.41285  0.314.950.34 00012JT
00608  1.28161  1.41314  0.214.570.42 100516T
00609  1.99360  0.03066  0.315.350.35 00012JF
00610  1.82712  1.40414  0.314.590.37 00012JT
00610  1.82695  1.40419  0.314.400.42 100516T
00610  1.82739  1.40430  0.313.550.53 1004YBT
00611  1.97238  0.84708  0.413.000.42 100516T
00611  1.97280  0.84716  0.312.570.53 1004YBT
00611  1.97250  0.84674  0.313.510.35 00012JT
00612  1.20067  2.41526  0.314.750.41 00012JT
00612  1.20068  2.41524  0.214.110.42 100516T
00613  2.23120  1.40220  0.412.860.41 00012JT
00613  2.23144  1.40260  0.312.150.53 1004YBT
00614  2.27745  0.84804  0.315.070.38 00012JT
00614  2.27769  0.84849  0.313.810.53 1004YBT
00615  2.27542  1.40141  0.412.050.41 00012JT
00615  2.27559  1.40168  0.310.930.53 1004YBT
00616  0.38368  0.47085  0.313.080.42 100516T
00616  0.38378  0.47051  0.313.470.19 00012JT
00617  2.20120  0.99490  0.314.360.38 00012JT
00617  2.20144  0.99534  0.313.240.53 1004YBT
00618  1.54455  1.13296  0.314.220.42 100516T
00618  1.54473  1.13274  0.314.640.33 00012JT
00619  1.89387  1.23271  0.311.810.53 1004YBT
00619  1.89352  1.23267  0.312.100.42 100516T
00619  1.89369  1.23252  0.312.530.37 00012JT
00620  2.46416  0.99688  0.312.240.53 1004YBT
00620  2.46401  0.99648  0.313.460.41 00012JT
00621  2.48991  1.74456  0.211.600.53 1004YBT
00621  2.48985  1.74463  0.512.640.40 0000WST
00621  2.48973  1.74440  0.412.270.41 00012JT
00622  1.26227  0.24002  0.313.950.25 00012JF
00623  0.50227  0.19337  0.315.450.16 00012JF
00624  1.40960  0.31760  0.314.120.27 00012JF
00625  2.23439  1.74199  0.312.550.53 1004YBT
00625  2.23417  1.74177  0.313.250.41 00012JT
00626  1.16936  1.58557  0.315.310.35 00012JT
00626  1.16925  1.58571  0.314.930.42 100516T
00627  1.86592  1.74449  0.413.120.40 00012JT
00627  1.86584  1.74438  0.312.510.42 100516T
00627  1.86619  1.74459  0.311.930.53 1004YBT
00628  2.11599  1.74966  0.315.400.41 00012JT
00628  2.11631  1.74974  0.313.920.53 1004YBT
00629  2.03576  1.74758  0.3 9.790.53 1004YBT
00629  2.03557  1.74752  0.410.470.41 00012JT
00629  2.03570  1.74745  0.3 9.510.42 100516T
00630  1.96785  0.07704  0.314.320.34 00012JF
00631  0.14598  0.68603  0.314.210.25 00012JT
00631  0.14576  0.68630  0.313.760.42 100516T
00632  0.90743  0.76678  0.313.410.23 00012JT
00632  0.90728  0.76716  0.313.050.42 100516T
00633  0.80133  1.61938  0.315.360.34 00012JF
00634  0.70675  1.93228  0.213.350.42 100516T
00634  0.70699  1.93209  0.313.990.38 00012JT
00635  1.09543  2.04371  0.315.430.39 00012JT
00635  1.09530  2.04385  0.214.940.42 100516T
00636  1.04577  0.30467  0.314.120.21 00012JF
00637  1.30088  1.91413  0.315.550.39 03012JF
00638  2.36642  1.76178  0.214.060.53 1004YBT
00638  2.36620  1.76159  0.315.270.41 00012JT
00639  1.13850  1.94841  0.315.330.39 00012JT
00639  1.13837  1.94852  0.215.110.42 100516T
00640  0.71364  1.87489  0.211.920.42 100516T
00640  0.71389  1.87473  0.412.300.37 00012JT
00641  1.48989  0.70178  0.313.650.42 100516T
00641  1.49000  0.70147  0.314.400.29 00012JT
00642  2.46512  1.76503  0.214.650.53 1004YBF
00643  2.20324  1.76271  0.314.460.41 00012JT
00643  2.20358  1.76291  0.313.670.53 1004YBT
00644  1.21280  0.98193  0.313.740.42 100516T
00644  1.21302  0.98157  0.314.010.29 00012JT
00645  0.65675  1.92594  0.313.680.38 00012JT
00645  0.65651  1.92611  0.213.260.42 100516T
00646  0.51893  1.13110  0.315.370.28 00012JT
00646  0.51875  1.13138  0.214.610.42 100516T
00647  2.10503  2.02404  0.314.240.41 00012JT
00647  2.10529  2.02420  0.313.040.53 1004YBT
00648  0.26321  1.34810  0.315.450.32 03012JF
00649  0.30236  1.20358  0.214.600.42 100516T
00649  0.30265  1.20338  0.315.250.30 00012JT
00650  1.81816  0.12565  0.412.380.32 00012JF
00651  0.34019  0.93157  0.314.440.26 00012JT
00651  0.33994  0.93183  0.314.090.42 100516T
00652  1.28587  1.10995  0.313.670.31 00012JT
00652  1.28561  1.11029  0.313.060.42 100516T
00653  2.00516  2.02332  0.313.420.53 1004YBT
00653  2.00508  2.02323  0.313.590.42 100516T
00653  2.00489  2.02328  0.314.300.41 00012JT
00654  2.24143  0.36765  0.314.590.37 00012JF
00655  0.33919  1.11603  0.412.280.28 00012JT
00655  0.33888  1.11621  0.311.450.42 100516T
00656  2.12043  1.77271  0.312.730.41 00012JT
00656  2.12065  1.77287  0.311.850.53 1004YBT
00657  0.52792  1.29633  0.313.770.30 00012JT
00657  0.52763  1.29662  0.213.280.42 100516T
00658  1.48811  0.34036  0.314.850.28 00012JF
00659  1.30764  2.05590  0.412.110.40 00012JT
00659  1.30754  2.05600  0.211.630.42 100516T
00660  1.93140  1.77511  0.314.160.41 00012JT
00660  1.93172  1.77536  0.313.090.53 1004YBT
00660  1.93138  1.77522  0.313.410.42 100516T
00661  1.85455  1.77474  0.313.720.53 1304YBT
00661  1.85428  1.77462  0.314.500.40 03012JT
00661  1.85418  1.77468  0.314.550.42 130516T
00662  0.05855  1.84054  0.212.860.42 100516T
00662  0.05881  1.84034  0.313.620.38 00012JT
00663  2.13126  0.11621  0.314.990.36 00012JF
00664  1.74436  1.77743  0.310.790.42 100516T
00664  1.74461  1.77743  0.310.420.53 1004YBT
00664  1.74439  1.77744  1.211.700.40 00012JT
00665  1.18905  0.05636  0.4 9.190.24 03012JF
00666  0.36519  0.73480  0.315.390.23 00012JT
00666  0.36507  0.73508  0.314.930.42 100516T
00667  0.15926  1.65846  0.314.360.36 00012JT
00667  0.15898  1.65868  0.214.060.42 100516T
00668  1.07041  1.58879  0.315.480.35 00012JT
00668  1.07024  1.58891  0.215.180.42 100516T
00669  2.26435  1.77995  0.315.440.41 00012JT
00669  2.26463  1.78022  0.314.340.53 1004YBT
00670  1.32753  0.46578  0.315.400.42 130516T
00670  1.32761  0.46535  0.313.980.26 03012JT
00671  0.49097  2.46266  0.214.710.42 100516T
00671  0.49107  2.46266  0.314.910.41 00012JT
00672  0.26840  1.92516  0.312.810.38 00012JT
00672  0.26814  1.92533  0.212.260.42 100516T
00673  1.09853  2.03371  0.314.110.39 00012JT
00673  1.09846  2.03388  0.213.760.42 100516T
00674  2.48873  1.02105  0.315.440.41 00012JT
00674  2.48893  1.02141  0.314.220.53 1004YBT
00675  1.85019  1.78476  0.315.220.40 00012JT
00675  1.85050  1.78492  0.313.890.53 1004YBT
00675  1.85014  1.78469  0.314.670.42 100516T
00676  2.35884  2.01587  0.213.150.53 1004YBT
00676  2.35870  2.01565  0.313.920.41 00012JT
00677  1.38531  0.93418  0.313.740.30 00012JT
00677  1.38508  0.93444  0.312.870.42 100516T
00678  0.53154  2.00754  0.313.270.38 00012JT
00678  0.53133  2.00774  0.213.000.42 100516T
00679  1.54079  1.08416  0.314.280.33 00012JT
00679  1.54054  1.08440  0.314.020.42 100516T
00680  2.21782  0.69243  0.5 8.170.37 00012JF
00681  0.73802  0.55400  0.314.190.19 00012JT
00681  0.73788  0.55436  0.313.850.42 100516T
00682  0.04432  1.32036  0.212.440.42 100516T
00682  0.04467  1.32026  0.312.700.32 00012JT
00683  1.95207  1.78914  0.314.340.41 00012JT
00683  1.95235  1.78928  0.313.400.53 1004YBT
00683  1.95205  1.78908  0.313.810.42 100516T
00684  0.54924  2.28501  0.213.490.42 100516T
00684  0.54943  2.28498  0.314.050.41 00012JT
00685  1.62365  1.06253  0.313.580.42 100516T
00685  1.62380  1.06226  0.314.020.33 00012JT
00686  0.93331  0.66593  0.315.530.22 00012JF
00687  1.84400  1.03055  0.314.140.53 1004YBT
00687  1.84349  1.03037  0.315.240.42 100516T
00687  1.84373  1.03020  0.315.290.35 00012JT
00688  1.66043  0.67670  0.314.090.42 100516T
00688  1.66056  0.67622  0.314.530.31 00012JT
00689  1.00920  0.12706  0.313.670.21 00012JF
00690  1.87630  1.16769  0.315.500.36 03012JF
00691  2.26296  1.80015  0.315.410.41 00012JT
00691  2.26322  1.80035  0.314.330.53 1004YBT
00692  0.59911  1.09768  0.212.460.42 100516T
00692  0.59937  1.09736  0.313.150.27 00012JT
00693  1.59285  0.80374  0.315.100.31 00012JT
00693  1.59269  0.80410  0.314.920.42 100516T
00694  1.43539  0.03996  0.313.730.28 00012JF
00695  2.48087  1.80094  0.314.350.41 00012JT
00695  2.48107  1.80115  0.515.030.40 0000WST
00695  2.48103  1.80111  0.213.340.53 1004YBT
00696  2.12294  1.21349  0.313.990.39 00012JT
00696  2.12326  1.21380  0.312.920.53 1004YBT
00697  0.40067  0.62760  0.314.030.42 100516T
00697  0.40076  0.62731  0.314.420.21 03012JT
00698  0.39441  0.27426  0.314.750.18 00012JF
00699  1.99583  1.80412  0.314.990.42 100516T
00699  1.99575  1.80414  0.315.500.41 00012JT
00699  1.99607  1.80432  0.314.520.53 1004YBT
00700  1.52440  0.33044  0.313.760.28 00012JF
00701  1.06332  1.16957  0.313.510.30 00012JT
00701  1.06302  1.16981  0.312.780.42 100516T
00702  2.06010  1.21223  0.314.150.42 100516T
00702  2.06050  1.21241  0.313.850.53 1004YBT
00702  2.06021  1.21216  0.314.830.38 00012JT
00703  2.20816  2.00143  0.311.490.53 1004YBT
00703  2.20790  2.00129  0.412.240.41 00012JT
00704  0.28064  0.79880  0.312.210.42 100516T
00704  0.28083  0.79847  0.312.540.25 00012JT
00705  2.38007  2.00036  0.214.600.53 1004YBF
00706  2.32728  0.12677  0.313.900.38 00012JF
00707  0.31174  0.79785  0.313.080.42 100516T
00707  0.31186  0.79758  0.313.610.24 00012JT
00708  0.48758  2.33029  0.315.460.41 00012JF
00709  1.87777  1.81092  0.314.030.53 1004YBT
00709  1.87734  1.81077  0.315.270.41 00012JT
00709  1.87740  1.81076  0.314.880.42 100516T
00710  0.41768  1.09172  0.315.540.28 03012JF
00711  1.08525  1.41737  0.313.600.33 00012JT
00711  1.08499  1.41747  0.213.340.42 100516T
00712  1.41935  1.36794  0.315.340.34 00012JT
00712  1.41913  1.36817  0.314.870.42 100516T
00713  0.02093  2.14258  0.214.180.41 100516T
00713  0.02117  2.14255  0.315.080.41 00012JT
00714  2.44329  1.99036  0.313.280.41 03012JT
00714  2.44346  1.99051  0.312.160.53 1304YBT
00715  0.51613  0.83661  0.312.220.42 100516T
00715  0.51633  0.83637  0.312.760.24 00012JT
00716  2.12628  1.04161  0.314.110.53 1004YBF
00717  1.73527  1.03958  0.413.970.53 1004YBF
00718  1.15344  0.32480  0.313.940.23 00012JF
00719  1.44973  0.79183  0.315.090.29 00012JT
00719  1.44955  0.79218  0.314.200.42 100516T
00720  2.08522  1.81815  0.314.110.53 1004YBT
00720  2.08489  1.81803  0.315.340.41 00012JT
00721  1.48867  2.09307  0.314.300.41 00012JT
00721  1.48867  2.09305  0.214.150.42 100516T
00722  1.85837  1.81999  0.314.470.42 100516T
00722  1.85869  1.82008  0.313.720.53 1004YBT
00722  1.85838  1.81995  0.314.950.41 00012JT
00723  1.28548  0.43012  0.311.900.42 100516T
00723  1.28553  0.42959  0.312.470.25 00012JT
00724  1.67563  1.98736  0.315.370.41 00012JT
00724  1.67561  1.98743  0.315.160.42 100516T
00724  1.67601  1.98736  0.314.210.53 1004YBT
00725  0.51858  0.45755  0.314.660.42 100516T
00725  0.51861  0.45715  0.315.010.17 00012JT
00726  0.80641  0.01774  0.313.780.19 00012JF
00727  1.73968  1.20381  0.314.520.35 00012JT
00727  1.74001  1.20396  0.313.380.53 1004YBT
00727  1.73949  1.20403  0.314.110.42 100516T
00728  2.28849  1.82337  0.314.180.41 00012JT
00728  2.28874  1.82358  0.313.470.53 1004YBT
00729  1.77777  1.04429  0.314.380.42 100516T
00729  1.77832  1.04438  0.413.890.53 1004YBT
00729  1.77794  1.04409  0.314.950.34 00012JT
00730  1.92515  1.04773  0.313.190.42 100516T
00730  1.92554  1.04793  0.312.770.53 1004YBT
00730  1.92532  1.04763  0.313.900.36 00012JT
00731  1.77673  1.04739  0.413.690.53 1304YBT
00731  1.77644  1.04708  0.314.760.34 00012JT
00732  2.07958  1.82364  0.314.300.53 1004YBT
00732  2.07928  1.82344  0.315.290.41 00012JT
00732  2.07925  1.82336  0.315.050.42 100516T
00733  1.78317  1.04640  0.4 9.670.53 1304YBT
00733  1.78291  1.04619  0.410.320.35 03012JT
00733  1.78279  1.04633  0.3 9.980.42 100516T
00734  1.44083  1.28399  0.314.580.42 100516T
00734  1.44103  1.28377  0.315.520.33 00012JT
00735  2.16845  1.98455  0.314.280.41 00012JT
00735  2.16873  1.98473  0.313.380.53 1004YBT
00736  1.43284  0.30041  0.311.810.27 00012JF
00737  1.50302  0.55807  0.312.990.42 100516T
00737  1.50313  0.55762  0.313.830.29 00012JT
00738  1.36401  0.43100  0.312.800.26 00012JT
00738  1.36394  0.43137  0.312.040.42 100516T
00739  1.96696  1.05103  0.314.330.42 100516T
00739  1.96737  1.05104  0.313.720.53 1004YBT
00739  1.96705  1.05073  0.315.010.36 00012JT
00740  0.61593  0.07025  0.314.690.17 00012JF
00741  1.80057  1.82864  0.412.820.40 00012JT
00741  1.80081  1.82871  0.311.830.53 1004YBT
00741  1.80052  1.82863  0.312.270.42 100516T
00742  1.03604  0.99736  0.314.960.42 100516T
00742  1.03627  0.99705  0.315.420.27 00012JT
00743  1.62816  0.23268  0.314.730.30 00012JF
00744  1.89206  1.05520  0.312.570.36 00012JT
00744  1.89231  1.05548  0.311.710.53 1004YBT
00744  1.89190  1.05543  0.312.190.42 100516T
00745  0.33353  1.98587  0.211.670.42 100516T
00745  0.33378  1.98568  0.412.200.38 00012JT
00746  1.94518  1.83007  0.314.460.53 1004YBF
00747  1.13164  1.45432  0.213.360.42 100516T
00747  1.13191  1.45419  0.313.790.33 00012JT
00748  0.31041  0.66634  0.314.130.42 100516T
00748  0.31054  0.66604  0.315.260.23 00012JT
00749  2.31102  0.19162  0.315.140.38 00012JF
00750  1.37690  1.97768  0.212.890.42 100516T
00750  1.37697  1.97765  0.313.170.40 00012JT
00751  2.15835  0.37805  0.315.510.36 00012JF
00752  1.71378  1.97407  0.313.990.42 130516T
00752  1.71369  1.97395  0.414.220.41 03012JT
00752  1.71403  1.97390  0.313.040.53 1304YBT
00753  1.03313  0.28300  0.314.510.21 00012JF
00754  0.45663  0.00740  0.314.730.19 00012JF
00755  0.11805  0.73610  0.411.470.26 00012JT
00755  0.11787  0.73635  0.310.460.42 100516T
00756  1.77263  1.84239  0.315.040.40 00012JT
00756  1.77287  1.84246  0.313.480.53 1004YBT
00756  1.77257  1.84248  0.314.590.42 100516T
00757  1.63847  0.50678  0.412.610.42 100516T
00757  1.63861  0.50621  0.313.260.30 00012JT
00758  0.63954  0.75789  0.314.500.42 100516T
00758  0.63962  0.75759  0.315.190.22 00012JT
00759  2.46958  0.36986  0.314.750.39 00012JF
00760  0.75939  2.35763  0.3 9.680.42 100516T
00760  0.75954  2.35766  0.410.730.41 00012JT
00761  1.63235  1.64302  0.311.820.42 100516T
00761  1.63247  1.64286  0.312.610.38 00012JT
00762  1.85908  0.19773  0.314.640.33 00012JF
00763  2.12915  1.06818  0.314.670.38 00012JT
00763  2.12940  1.06857  0.313.680.53 1004YBT
00764  2.39350  0.04553  0.314.950.39 00012JF
00765  2.47375  1.18427  0.313.460.53 1004YBT
00765  2.47349  1.18386  0.314.340.41 00012JT
00766  1.89993  1.18186  0.313.590.36 00012JT
00766  1.90014  1.18206  0.312.300.53 1004YBT
00766  1.89974  1.18203  0.312.990.42 100516T
00767  2.39864  1.96639  0.213.770.53 1004YBT
00767  2.39839  1.96625  0.315.320.41 00012JT
00768  1.92094  1.96540  0.313.710.53 1004YBT
00768  1.92071  1.96524  0.314.280.42 100516T
00768  1.92059  1.96529  0.314.860.41 00012JT
00769  1.05782  1.00112  0.314.960.28 03012JF
00770  1.83812  0.26673  0.313.070.32 00012JF
00771  2.11657  0.26526  0.411.840.36 00012JF
00772  1.04141  1.26631  0.315.100.31 00012JT
00772  1.04115  1.26663  0.214.780.42 100516T
00773  2.31698  1.85385  0.314.750.41 00012JT
00773  2.31724  1.85411  0.314.110.53 1004YBT
00774  2.45212  2.04208  1.215.230.41 03012JF
00775  1.62661  2.16827  0.314.740.42 100516T
00775  1.62649  2.16825  0.315.230.41 00012JT
00776  1.46615  0.77248  0.313.380.29 00012JT
00776  1.46596  0.77279  0.312.510.42 100516T
00777  2.16257  1.85865  0.313.810.53 1004YBT
00777  2.16232  1.85841  0.315.290.41 00012JT
00778  0.85535  1.61811  0.215.090.42 100516T
00778  0.85565  1.61786  0.315.550.34 00012JT
00779  2.35273  0.07218  0.5 8.920.38 03012JF
00780  0.51778  2.46034  0.212.930.42 100516T
00780  0.51782  2.46031  0.313.600.41 00012JT
00781  0.37855  1.98953  0.213.590.42 100516T
00781  0.37877  1.98935  0.313.960.38 00012JT
00782  2.43011  0.09309  0.315.240.39 00012JF
00783  1.71809  1.86084  0.3 9.050.42 100516T
00783  1.71793  1.86096  0.5 9.570.40 00012JT
00783  1.71801  1.86080  0.3 8.740.53 1304YBT
00784  1.74114  1.07405  0.413.080.53 1004YBT
00784  1.74079  1.07384  0.314.140.34 00012JT
00784  1.74059  1.07404  0.313.990.42 100516T
00785  2.21855  1.95881  0.314.470.53 1004YBF
00786  0.66562  2.16289  0.315.420.40 00012JT
00786  0.66541  2.16297  0.215.060.42 100516T
00787  0.40691  1.77974  0.314.590.36 00012JT
00787  0.40664  1.77993  0.214.310.42 100516T
00788  1.04172  1.55905  0.411.950.34 00012JT
00788  1.04149  1.55922  0.211.580.42 100516T
00789  1.78969  0.12762  0.315.370.32 00012JF
00790  1.14252  1.75477  0.212.760.42 100516T
00790  1.14275  1.75465  0.313.860.37 00012JT
00791  0.69058  2.14060  0.313.060.40 00012JT
00791  0.69042  2.14066  0.212.180.42 100516T
00792  1.75863  0.04418  0.315.100.32 00012JF
00793  1.11189  0.02137  0.314.780.23 00012JF
00794  1.49560  0.78301  0.313.190.30 00012JT
00794  1.49547  0.78335  0.312.610.42 100516T
00795  1.97346  1.95795  0.314.690.41 00012JT
00795  1.97355  1.95792  0.314.070.42 100516T
00795  1.97382  1.95809  0.313.690.53 1004YBT
00796  2.37912  1.08320  0.314.400.53 1004YBF
00797  0.97416  1.88116  0.411.820.37 00012JT
00797  0.97395  1.88131  0.211.550.42 100516T
00798  0.86655  1.23468  0.311.210.42 100516T
00798  0.86678  1.23448  0.412.440.30 00012JT
00799  2.05813  1.87235  0.310.730.42 100516T
00799  2.05803  1.87245  1.211.930.41 00012JT
00799  2.05824  1.87259  0.310.910.53 1004YBT
00800  0.14419  0.17475  0.411.730.23 00012JF
00801  1.76582  1.08261  0.313.190.35 00012JT
00801  1.76610  1.08280  0.312.080.53 1004YBT
00801  1.76559  1.08279  0.312.670.42 100516T
00802  2.12045  1.08324  0.313.360.53 1004YBT
00802  2.12022  1.08294  0.314.570.38 00012JT
00803  1.31731  1.13137  0.314.000.31 03012JT
00803  1.31720  1.13161  0.313.550.42 100516T
00804  0.21107  1.29290  0.315.250.31 00012JT
00804  0.21075  1.29312  0.214.750.42 100516T
00805  1.00289  1.22475  0.314.140.30 00012JT
00805  1.00262  1.22503  0.213.840.42 100516T
00806  1.71401  1.87821  0.312.030.42 100516T
00806  1.71399  1.87823  0.312.770.40 00012JT
00806  1.71422  1.87822  0.311.320.53 1004YBT
00807  0.62310  0.39141  0.411.910.14 00012JT
00807  0.62311  0.39178  0.310.510.42 100516T
00808  0.66530  0.94253  0.314.240.25 03012JF
00809  2.41590  1.16446  0.314.660.41 00012JT
00809  2.41618  1.16491  0.313.640.53 1004YBT
00810  1.45229  0.27121  0.314.910.27 00012JF
00811  0.41394  2.19576  0.213.890.42 100516T
00811  0.41410  2.19571  0.314.510.40 00012JT
00812  1.16647  0.21124  0.315.320.23 00012JF
00813  1.12074  0.13999  0.315.330.22 00012JF
00814  1.00044  0.93651  0.314.420.26 00012JT
00814  1.00020  0.93686  0.313.880.42 100516T
00815  2.30407  0.02188  0.315.180.38 00012JF
00816  0.39470  0.11050  0.312.320.19 00012JF
00817  0.57093  0.23844  0.314.880.13 00012JF
00818  1.91092  0.17902  0.410.560.33 00012JF
00819  0.91253  1.58595  0.510.000.34 00012JT
00819  0.91234  1.58614  0.3 9.230.42 100516T
00820  0.46202  1.52054  0.213.930.42 100516T
00820  0.46226  1.52037  0.314.530.33 00012JT
00821  2.41640  1.15258  0.314.270.53 1004YBT
00821  2.41614  1.15214  0.315.350.41 00012JT
00822  0.52661  1.54136  0.212.830.42 100516T
00822  0.52691  1.54115  0.313.720.33 00012JT
00823  0.40156  2.30055  0.212.870.42 100516T
00823  0.40168  2.30053  0.313.650.41 00012JT
00824  0.69499  1.46503  0.313.510.32 00012JT
00824  0.69473  1.46519  0.212.850.42 100516T
00825  0.62001  0.56048  0.312.450.42 100516T
00825  0.62010  0.56005  0.312.940.18 00012JT
00826  1.98214  1.88847  0.313.940.53 1004YBT
00826  1.98193  1.88840  0.314.410.42 100516T
00826  1.98183  1.88841  0.314.900.41 00012JT
00827  1.43923  0.01570  0.314.600.28 00012JF
00828  0.77755  2.33756  0.212.080.42 100516T
00828  0.77770  2.33755  0.312.490.41 00012JT
00829  1.23451  1.58962  0.314.640.35 00012JT
00829  1.23429  1.58990  0.214.030.42 100516T
00830  0.87289  1.27797  0.313.630.30 00012JT
00830  0.87264  1.27826  0.213.310.42 100516T
00831  0.22748  1.71192  0.314.840.36 00012JT
00831  0.22720  1.71213  0.214.350.42 100516T
00832  2.05681  1.10061  0.313.870.53 1004YBT
00832  2.05652  1.10028  0.314.820.37 00012JT
00832  2.05635  1.10046  0.314.250.42 100516T
00833  0.05197  2.09435  0.212.720.41 100516T
00833  0.05223  2.09430  0.313.370.40 03012JT
00834  0.13910  0.12698  0.314.730.23 00012JF
00835  0.29076  0.80151  0.314.580.42 100516T
00835  0.29089  0.80115  0.315.090.25 00012JT
00836  2.26318  1.89054  0.314.540.53 1004YBF
00837  0.48063  1.41179  0.215.190.42 100516T
00837  0.48092  1.41158  0.315.520.32 00012JT
00838  1.69626  1.88915  0.313.920.53 1004YBF
00839  1.80691  1.14585  0.314.210.42 100516T
00839  1.80710  1.14573  0.314.490.35 00012JT
00839  1.80739  1.14587  0.313.390.53 1004YBT
00840  2.09392  1.10265  0.314.120.53 1004YBF
00841  0.58311  1.62660  0.315.290.34 00012JT
00841  0.58290  1.62687  0.214.940.42 100516T
00842  2.13899  1.89802  0.312.120.53 1004YBT
00842  2.13874  1.89788  0.312.760.41 00012JT
00843  1.59505  1.47206  0.313.770.42 100516T
00843  1.59528  1.47182  0.314.150.36 00012JT
00844  1.84790  1.13748  0.314.790.36 00012JT
00844  1.84768  1.13770  0.314.290.42 100516T
00844  1.84818  1.13772  0.313.560.53 1004YBT
00845  1.35005  0.34131  0.313.230.26 00012JF
00846  2.14392  1.90011  0.313.050.53 1004YBT
00846  2.14366  1.89994  0.313.650.41 00012JT
00847  0.70113  0.30584  0.313.470.13 00012JF
00848  1.99338  1.10796  0.315.250.37 00012JT
00848  1.99322  1.10810  0.314.680.42 100516T
00848  1.99363  1.10829  0.314.050.53 1004YBT
00849  1.11848  0.75380  0.314.230.42 100516T
00849  1.11865  0.75346  0.314.960.25 00012JT
00850  2.38039  0.33629  0.314.600.38 00012JF
00851  0.68780  0.38430  0.315.430.14 00012JF
00852  1.03564  2.15154  0.214.920.42 100516T
00852  1.03567  2.15140  0.315.210.40 00012JT
00853  2.31620  1.94397  0.315.340.41 00012JT
00853  2.31641  1.94415  0.314.180.53 1004YBT
00854  2.21791  1.11021  0.313.860.53 1304YBT
00854  2.21743  1.10986  0.314.700.39 03012JT
00855  2.37573  1.83316  0.315.240.41 00012JF
00856  2.00229  1.94372  0.314.500.53 1004YBF
00857  2.27669  1.47087  0.315.250.41 00012JF
00858  0.91947  0.09735  0.312.730.19 00012JF
00859  1.49339  1.13588  0.314.880.42 100516T
00859  1.49363  1.13563  0.315.200.33 00012JT
00860  2.27858  1.91392  0.312.830.53 1004YBT
00860  2.27839  1.91376  0.313.530.41 00012JT
00861  1.06761  0.22337  0.315.420.21 00012JF
00862  2.28955  1.91216  0.313.570.53 1004YBT
00862  2.28929  1.91192  0.314.530.41 00012JT
00863  1.19436  2.42920  0.315.380.41 00012JT
00863  1.19435  2.42929  0.214.800.42 100516T
00864  2.25424  1.11841  0.314.410.53 1004YBT
00864  2.25398  1.11798  0.315.460.39 00012JT
00865  2.06815  1.91999  0.312.470.53 1004YBT
00865  2.06794  1.91966  0.312.420.42 100516T
00865  2.06784  1.91979  0.313.180.41 00012JT
00866  2.38189  1.92113  0.214.870.53 1004YBF
00867  0.54650  0.47338  0.310.090.42 100516T
00867  0.54652  0.47309  0.410.700.17 00012JT
00868  2.09236  1.91817  0.315.250.42 100516T
00868  2.09223  1.91828  0.314.630.41 00012JT
00868  2.09250  1.91841  0.313.680.53 1004YBT
00869  2.12211  1.11942  0.313.890.53 1004YBT
00869  2.12183  1.11904  0.314.910.38 00012JT
00870  0.05746  0.21994  0.411.740.24 00012JF
00871  1.92458  1.08891  0.414.200.36 03012JF
00872  2.11089  1.94036  0.313.740.41 00012JT
00872  2.11117  1.94043  0.312.910.53 1004YBT
00873  2.11807  0.21943  0.314.750.36 00012JF
00874  0.56204  0.26165  0.313.960.13 00012JF
00875  0.95362  0.50298  0.315.130.20 00012JT
00875  0.95351  0.50343  0.314.490.42 100516T
00876  0.90724  0.21879  0.314.770.18 00012JF
00877  0.81480  1.86097  0.213.300.42 100516T
00877  0.81499  1.86087  0.314.190.37 00012JT
00878  2.33588  1.12980  0.314.000.53 1004YBT
00878  2.33558  1.12938  0.315.220.40 00012JT
00879  0.97706  1.31373  0.411.720.31 00012JT
00879  0.97680  1.31395  0.210.810.42 100516T
00880  1.94067  1.92652  0.312.420.53 1004YBT
00880  1.94040  1.92635  0.412.900.41 00012JT
00880  1.94044  1.92640  0.312.760.42 100516T
00881  1.53206  1.73301  0.311.970.42 100516T
00881  1.53221  1.73286  0.312.720.38 00012JT
00882  1.99595  0.10011  1.213.100.34 00012JF
00883  1.88657  0.15715  0.313.940.33 00012JF
00884  2.30199  0.07491  0.410.240.38 00012JF
00885  0.80867  1.24046  0.313.150.30 00012JT
00885  0.80839  1.24069  0.212.410.42 100516T
00886  0.76946  1.60788  0.314.340.34 00012JT
00886  0.76921  1.60810  0.214.090.42 100516T
00887  1.83787  1.93650  0.313.600.41 00012JT
00887  1.83791  1.93653  0.313.330.42 100516T
00887  1.83819  1.93660  0.312.610.53 1004YBT
00888  0.42797  1.68551  0.213.590.42 100516T
00888  0.42825  1.68534  0.314.220.35 00012JT
00889  1.39734  1.94635  0.215.170.42 100516T
00889  1.39748  1.94626  0.315.270.39 00012JT
00890  1.08802  1.15417  0.310.970.42 100516T
00890  1.08823  1.15396  0.411.890.30 00012JT
00891  0.44588  1.80282  0.315.410.36 03012JF
00892  0.65202  0.41312  0.314.540.42 100516T
00892  0.65198  0.41274  0.314.750.15 00012JT
00893  1.16821  0.48570  0.314.910.24 00012JT
00893  1.16810  0.48606  0.314.340.42 100516T
00894  1.88777  0.12170  0.412.210.33 00012JF
00895  0.82118  0.60478  0.315.160.42 100516T
00895  0.82126  0.60440  0.315.480.20 00012JT
00896  0.04938  0.64518  0.314.140.42 100516T
00896  0.04954  0.64486  0.315.350.26 00012JT
00897  0.79142  1.06629  0.312.610.27 00012JT
00897  0.79117  1.06656  0.311.830.42 100516T
00898  1.03445  2.13570  0.214.450.42 100516T
00898  1.03451  2.13558  0.315.120.40 00012JT
00899  1.29226  1.49290  0.214.270.42 100516T
00899  1.29251  1.49267  0.314.460.35 00012JT
00900  1.28057  0.36946  0.411.350.25 00012JF
00901  0.04473  0.86424  0.312.510.42 100516T
00901  0.04502  0.86399  0.313.080.28 00012JT
00902  1.27338  2.06060  0.211.490.42 130516T
00902  1.27346  2.06053  0.312.440.40 03012JT
00903  0.69638  2.13014  0.5 7.200.40 03012JT
00903  0.69646  2.12994  0.1 7.400.10 40+056T
00903  0.69617  2.13054  0.3 7.640.42 100516T
00904  0.67740  1.06552  1.211.690.27 00012JT
00904  0.67715  1.06573  0.310.870.42 100516T
00905  1.12700  0.63635  0.314.080.24 00012JT
00905  1.12683  0.63673  0.313.570.42 100516T
00906  0.74399  1.64065  0.314.860.34 00012JT
00906  0.74373  1.64079  0.214.340.42 100516T
00907  0.02181  2.05041  0.314.900.40 00012JF
00908  0.59415  1.22661  0.412.160.29 00012JT
00908  0.59395  1.22681  0.211.000.42 100516T
00909  2.16198  0.79512  0.314.270.37 00012JT
00909  2.16226  0.79566  0.313.120.53 1004YBT
00910  2.24372  0.01785  0.313.490.37 00012JF
00911  1.20356  0.99130  0.312.610.29 00012JT
00911  1.20336  0.99159  0.311.870.42 100516T
00912  1.70922  0.18896  0.315.350.31 00012JF
00913  1.45000  0.52867  0.315.120.28 00012JT
00913  1.44982  0.52904  0.314.600.42 100516T
00914  1.17590  0.80181  0.314.810.26 00012JT
00914  1.17573  0.80217  0.314.350.42 100516T
00915  0.32810  0.18533  0.315.550.19 00012JF
00916  0.33510  0.16457  0.314.690.19 00012JF
00917  0.84381  1.05779  0.313.880.27 03012JT
00917  0.84357  1.05812  0.315.060.42 130516T
00918  2.16973  0.23420  0.412.310.36 00012JF
00919  1.02687  0.18187  0.313.680.21 00012JF
00920  1.26958  1.11984  0.313.110.42 100516T
00920  1.26981  1.11958  0.313.740.31 00012JT
00921  0.13038  1.67247  0.213.060.42 100516T
00921  0.13070  1.67234  0.313.460.36 00012JT
00922  1.32065  0.12804  0.314.530.26 00012JF
00923  1.92187  0.17948  0.315.020.33 00012JF
00924  0.18141  1.53739  0.213.420.42 100516T
00924  0.18179  1.53718  0.313.660.34 00012JT
00925  1.98177  0.79092  0.4 9.770.35 03012JT
00925  1.98172  0.79106  0.4 8.950.42 130516T
00925  1.98185  0.79128  0.4 8.970.53 1004YBT
00926  1.30495  2.35500  0.213.030.42 100516T
00926  1.30491  2.35490  0.313.390.41 00012JT
00927  0.68487  2.02065  0.214.660.42 100516T
00927  0.68499  2.02050  0.315.200.39 00012JT
00928  0.18137  0.49462  0.313.870.42 100516T
00928  0.18150  0.49429  0.314.590.23 00012JT
00929  2.46159  0.79018  0.314.780.40 00012JT
00929  2.46170  0.79058  0.313.180.53 1004YBT
00930  1.53848  1.80016  0.315.530.39 03012JF
00931  0.47237  1.24685  0.211.180.42 100516T
00931  0.47263  1.24665  0.411.700.30 00012JT
00932  0.44863  0.48006  0.313.200.18 00012JT
00932  0.44852  0.48040  0.312.590.42 100516T
00933  1.42430  0.69223  0.314.050.42 130516T
00933  1.42450  0.69193  0.314.960.28 03012JT
00934  1.59046  2.48891  0.315.510.41 00012JT
00934  1.59059  2.48896  0.314.750.42 100516T
00935  1.22909  0.36486  0.315.530.24 00012JF
00936  0.89143  0.92748  0.313.810.42 100516T
00936  0.89171  0.92708  0.314.220.26 00012JT
00937  0.18406  0.16560  0.315.470.22 00012JF
00938  1.04042  0.83574  0.315.070.26 00012JT
00938  1.04026  0.83613  0.314.790.42 100516T
00939  0.96101  0.77564  0.410.610.24 00012JT
00939  0.96085  0.77586  0.310.150.42 100516T
00940  1.61913  0.48470  0.315.300.30 03012JF
00941  1.62157  2.38050  0.314.780.41 00012JF
00942  2.16856  0.02209  0.315.520.36 00012JF
00943  1.69970  0.26310  0.312.670.31 00012JF
00944  1.58614  1.49337  0.315.210.42 100516T
00944  1.58629  1.49327  0.315.520.36 00012JT
00945  1.78621  0.33983  0.410.670.32 00012JF
00946  1.13062  1.57689  0.214.960.42 100516T
00946  1.13083  1.57674  0.315.300.35 00012JT
00947  0.91962  0.82984  0.315.290.24 00012JT
00947  0.91945  0.83022  0.314.670.42 100516T
00948  0.59129  0.24843  0.313.480.13 00012JF
00949  1.53150  1.86154  0.313.420.42 100516T
00949  1.53166  1.86143  0.313.700.39 00012JT
00950  1.05682  1.38396  0.212.600.42 100516T
00950  1.05708  1.38379  0.313.210.32 00012JT
00951  0.07304  0.21324  0.314.870.24 00012JF
00952  1.08057  0.93430  0.313.790.27 00012JT
00952  1.08032  0.93451  0.313.350.42 100516T
00953  2.18356  0.04548  0.313.580.37 00012JF
00954  0.73815  0.13983  0.315.520.16 00012JF
00955  1.31009  1.39166  0.313.320.42 100516T
00955  1.31034  1.39145  0.313.760.34 00012JT
00956  1.40510  1.25323  0.314.670.33 00012JT
00956  1.40490  1.25344  0.314.230.42 100516T
00957  1.04444  1.26857  0.314.890.31 03012JF
00958  1.07043  0.70498  0.312.940.24 00012JT
00958  1.07025  0.70528  0.312.390.42 100516T
00959  0.12194  0.71924  0.314.450.42 100516T
00959  0.12211  0.71897  0.315.010.25 00012JT
00960  0.97281  0.86461  0.313.060.42 100516T
00960  0.97304  0.86429  0.313.770.25 00012JT
00961  1.37752  0.36950  0.314.060.26 00012JF
00962  1.10917  1.03599  0.313.580.28 00012JT
00962  1.10890  1.03627  0.313.250.42 100516T
00963  1.14115  0.67705  0.313.950.42 100516T
00963  1.14128  0.67671  0.314.140.25 00012JT
00964  0.69979  0.14885  0.313.040.15 00012JF
00965  1.09090  1.08941  0.315.620.42 100516T
00965  1.09109  1.08912  0.314.640.29 03012JT
00966  0.07627  0.78499  0.314.700.27 00012JT
00966  0.07606  0.78525  0.314.180.42 100516T
00967  0.21632  0.37103  0.312.890.21 00012JF
00968  0.27317  1.45075  0.314.020.33 00012JT
00968  0.27287  1.45098  0.213.590.42 100516T
00969  1.52402  1.01532  0.313.900.32 00012JT
00969  1.52374  1.01559  0.313.580.42 100516T
00970  0.78144  0.08882  0.411.720.17 00012JF
00971  0.16197  2.19441  0.211.920.41 100516T
00971  0.16220  2.19431  0.412.360.41 00012JT
00972  2.05463  0.79177  0.315.020.36 00012JF
00973  0.37854  1.88428  0.213.190.42 100516T
00973  0.37882  1.88411  0.313.580.37 00012JT
00974  0.45975  1.03223  0.315.310.42 130516T
00974  0.45998  1.03201  0.314.060.27 03012JT
00975  0.70882  1.57244  0.314.730.34 00012JT
00975  0.70853  1.57267  0.214.240.42 100516T
00976  0.40159  0.23575  0.411.300.17 00012JF
00977  0.69268  1.01665  0.313.930.26 00012JT
00977  0.69244  1.01692  0.313.070.42 100516T
00978  0.65589  1.01608  0.313.010.26 00012JT
00978  0.65562  1.01639  0.312.510.42 100516T
00979  1.95876  0.13003  0.4 9.710.34 00012JF
00980  1.06301  0.07189  0.314.930.22 00012JF
00981  0.49175  0.12870  0.410.060.17 03012JF
00982  0.69116  1.63501  0.211.480.42 100516T
00982  0.69142  1.63488  0.412.340.34 00012JT
00983  0.92430  1.89093  0.315.540.37 03012JF
00984  0.93061  0.38582  0.314.610.42 100516T
00984  0.93056  0.38540  0.314.930.19 00012JT
00985  0.39749  0.12744  0.313.860.18 00012JF
00986  1.02593  0.90961  0.313.710.26 00012JT
00986  1.02573  0.90998  0.313.140.42 100516T
00987  0.85461  1.34646  0.213.350.42 100516T
00987  0.85485  1.34623  0.313.820.31 00012JT
00988  2.48682  0.13235  0.412.300.40 00012JT
00988  2.48670  0.13227  0.512.580.40 0000WST
00989  2.49259  0.13103  0.313.600.40 00012JT
00989  2.49243  0.13099  0.514.210.40 0000WST
00990  1.09998  1.41465  0.412.960.33 03012JT
00990  1.09976  1.41494  0.312.770.42 130516T
00991  0.19291  2.05228  0.210.200.42 100516T
00991  0.19317  2.05214  0.410.720.39 00012JT
00992  0.54229  0.44937  0.313.540.42 100516T
00992  0.54233  0.44895  0.314.200.16 00012JT
00993  0.31928  0.67214  0.315.080.23 00012JT
00993  0.31913  0.67246  0.314.390.42 100516T
00994  0.55250  1.47245  0.214.330.42 100516T
00994  0.55282  1.47225  0.314.710.32 00012JT
00995  0.87374  2.15913  0.312.640.40 00012JT
00995  0.87359  2.15922  0.212.110.42 130516T
00996  0.35684  0.11346  0.315.450.19 00012JF
00997  1.63222  2.41998  0.311.330.42 100516T
00997  1.63202  2.41993  0.411.940.41 00012JT
00998  2.08786  0.75913  0.314.070.53 1004YBT
00998  2.08756  0.75866  0.315.210.36 00012JT
00999  0.67871  2.49869  0.313.430.41 00012JT
00999  0.67862  2.49873  0.212.880.42 100516T
01000  2.23936  0.75871  0.313.960.53 1004YBT
01000  2.23914  0.75821  0.315.010.38 00012JT
01001  1.41477  2.44886  0.411.160.41 00012JT
01001  1.41490  2.44889  0.310.770.42 100516T
01002  1.48892  1.17041  0.314.760.42 100516T
01002  1.48918  1.17016  0.315.090.33 00012JT
01003  0.01993  0.78156  0.314.580.27 03012JF
01004  1.26200  1.72969  0.314.570.37 00012JT
01004  1.26189  1.72982  0.214.300.42 100516T
01005  0.22566  0.09905  0.314.580.22 00012JF
01006  2.32025  0.06747  0.315.360.38 00012JF
01007  0.34450  1.69354  0.312.620.35 00012JT
01007  0.34421  1.69365  0.212.390.42 100516T
01008  1.25274  0.61601  0.313.630.42 100516T
01008  1.25295  0.61564  0.314.220.26 00012JT
01009  0.13703  0.72080  0.312.640.25 00012JT
01009  0.13683  0.72107  0.311.950.42 100516T
01010  0.73241  0.76535  0.313.900.22 00012JT
01010  0.73222  0.76571  0.313.490.42 100516T
01011  0.65065  2.06898  0.215.350.42 100516T
01011  0.65087  2.06889  0.315.510.39 00012JT
01012  0.30216  1.59364  0.213.640.42 100516T
01012  0.30244  1.59344  0.314.110.34 00012JT
01013  1.21663  0.65761  0.314.040.42 100516T
01013  1.21678  0.65729  0.314.650.26 00012JT
01014  0.37760  2.21494  0.214.640.42 130516T
01014  0.37783  2.21484  0.313.310.41 03012JT
01015  0.16288  0.22285  0.5 8.980.22 00012JF
01016  0.42668  1.78708  0.211.250.42 100516T
01016  0.42697  1.78697  0.411.990.36 00012JT
01017  1.18979  1.76626  0.214.520.42 100516T
01017  1.19003  1.76615  0.314.710.37 00012JT
01018  0.02272  1.51921  0.315.230.35 00012JT
01018  0.02243  1.51933  0.214.780.42 100516T
01019  1.59428  1.77827  0.314.440.39 00012JT
01019  1.59418  1.77836  0.314.260.42 100516T
01020  1.29319  0.46796  0.314.690.25 03012JT
01020  1.29309  0.46839  0.315.600.42 100516T
01021  0.48449  0.43488  0.312.470.17 00012JT
01021  0.48447  0.43524  0.311.790.42 100516T
01022  0.56946  2.31904  0.314.570.41 00012JT
01022  0.56935  2.31911  0.214.160.42 100516T
01023  0.76518  0.20902  0.314.210.15 00012JF
01024  1.08062  0.87566  0.411.030.26 00012JT
01024  1.08042  0.87588  0.310.460.42 100516T
01025  0.16748  2.11538  0.212.220.42 100516T
01025  0.16768  2.11529  0.312.730.40 00012JT
01026  0.05730  0.72492  0.315.450.26 00012JF
01027  0.59091  0.42572  0.313.700.15 00012JT
01027  0.59085  0.42613  0.313.120.42 100516T
01028  2.17148  0.74892  0.313.880.53 1004YBT
01028  2.17129  0.74840  0.314.830.37 00012JT
01029  0.60588  1.08728  0.312.910.42 100516T
01029  0.60618  1.08696  0.313.300.27 00012JT
01030  0.03005  1.42159  0.212.390.42 100516T
01030  0.03042  1.42142  0.312.680.34 00012JT
01031  0.66159  0.84370  0.314.990.24 00012JT
01031  0.66143  0.84398  0.314.720.42 100516T
01032  0.73466  1.57467  0.315.020.34 00012JT
01032  0.73439  1.57490  0.214.610.42 100516T
01033  1.57928  0.26426  0.314.690.29 03012JF
01034  1.05665  1.01162  0.314.720.42 100516T
01034  1.05690  1.01135  0.315.250.28 00012JT
01035  1.08384  0.45323  0.313.250.22 00012JT
01035  1.08376  0.45357  0.312.220.42 100516T
01036  0.42838  0.15988  0.314.140.17 00012JF
01037  1.88837  0.25732  0.313.610.33 00012JF
01038  2.21068  0.03908  0.314.180.37 00012JF
01039  1.86629  0.78513  0.314.960.34 03012JF
01040  0.39855  0.44475  0.312.360.19 00012JT
01040  0.39851  0.44504  0.311.760.42 100516T
01041  2.45922  0.74480  0.312.870.53 1004YBT
01041  2.45914  0.74442  0.314.040.40 00012JT
01042  1.20408  0.95107  0.314.840.28 03012JF
01043  1.48821  0.51994  0.315.190.28 00012JT
01043  1.48806  0.52036  0.314.760.42 100516T
01044  0.45320  0.60516  0.312.480.42 100516T
01044  0.45330  0.60486  0.312.960.20 00012JT
01045  2.02837  0.23575  0.315.150.35 00012JF
01046  1.29035  1.63192  0.412.960.36 00012JT
01046  1.29016  1.63217  0.212.420.42 100516T
01047  1.26534  1.80198  0.315.550.38 00012JF
01048  0.49626  1.69721  0.214.690.42 100516T
01048  0.49657  1.69707  0.315.250.35 00012JT
01049  0.48377  0.83159  0.313.250.24 03012JT
01049  0.48361  0.83189  0.313.050.42 100516T
01050  0.25025  0.02014  0.315.090.22 00012JF
01051  0.31215  0.98079  0.314.580.27 00012JT
01051  0.31187  0.98108  0.314.190.42 100516T
01052  0.08775  2.08640  0.314.020.40 00012JT
01052  0.08743  2.08645  0.213.610.41 100516T
01053  1.62852  2.03274  0.315.040.41 00012JT
01053  1.62857  2.03283  0.314.840.42 100516T
01054  2.08249  0.35253  0.314.630.35 00012JF
01055  1.72329  0.08954  0.314.280.31 00012JF
01056  0.34273  0.56789  0.313.150.42 100516T
01056  0.34283  0.56765  0.313.890.21 00012JT
01057  1.82417  0.61163  0.315.470.33 03012JF
01058  0.16667  0.31504  1.211.720.22 00012JF
01059  0.98200  0.97226  0.313.580.27 00012JT
01059  0.98178  0.97261  0.313.230.42 100516T
01060  2.19625  0.73308  0.313.710.37 00012JT
01060  2.19642  0.73361  0.312.780.53 1004YBT
01061  0.22842  0.77317  0.314.620.42 100516T
01061  0.22862  0.77291  0.315.320.25 00012JT
01062  2.14892  0.73296  0.315.460.37 00012JT
01062  2.14918  0.73344  0.314.030.53 1004YBT
01063  1.43868  0.58252  0.412.040.28 00012JT
01063  1.43861  0.58290  0.310.920.42 100516T
01064  1.02909  1.84173  0.314.520.37 00012JT
01064  1.02885  1.84180  0.214.080.42 100516T
01065  0.98877  1.11798  0.313.210.42 100516T
01065  0.98898  1.11761  0.313.630.29 00012JT
01066  1.73109  0.73213  0.315.460.32 00012JT
01066  1.73134  0.73244  0.414.070.53 1004YBT
01067  0.07568  2.15676  0.314.880.41 00012JT
01067  0.07552  2.15687  0.214.360.41 100516T
01068  0.17524  0.52876  0.315.530.23 00012JT
01068  0.17513  0.52911  0.314.720.42 100516T
01069  0.88632  0.92314  0.315.120.26 00012JT
01069  0.88607  0.92345  0.314.540.42 100516T
01070  1.39223  0.86168  0.314.740.29 03012JF
01071  1.45099  0.38312  0.314.080.27 00012JT
01071  1.45097  0.38351  0.413.520.42 100516T
01072  1.25076  2.05672  0.211.160.42 100516T
01072  1.25083  2.05665  1.211.670.40 00012JT
01073  1.49309  1.22208  0.312.640.42 100516T
01073  1.49334  1.22191  0.313.090.33 00012JT
01074  1.45512  0.45416  0.314.130.42 100516T
01074  1.45514  0.45370  0.314.050.28 00012JT
01075  0.88299  1.03711  0.314.420.27 00012JT
01075  0.88270  1.03749  0.314.200.42 100516T
01076  0.08405  1.83458  0.314.930.38 00012JT
01076  0.08375  1.83470  0.214.450.42 100516T
01077  0.35641  0.34123  0.315.110.18 00012JF
01078  1.03603  0.69819  0.315.100.24 00012JF
01079  0.95691  2.06646  1.213.280.39 03012JT
01079  0.95671  2.06661  1.214.670.42 130516T
01080  1.78717  1.27002  1.215.410.36 03012JF
01081  1.25406  1.63579  0.314.190.36 00012JT
01081  1.25382  1.63600  0.213.900.42 100516T
01082  1.00047  0.50617  0.315.120.21 03012JF
01083  2.44670  0.72630  0.314.500.53 1004YBF
01084  0.19827  0.51600  0.313.310.42 100516T
01084  0.19839  0.51571  0.314.270.23 00012JT
01085  1.62787  0.00117  0.312.650.30 00012JF
01086  0.98847  0.06428  0.314.060.21 00012JF
01087  0.33998  0.49839  0.315.490.20 00012JT
01087  0.33992  0.49867  0.314.760.42 100516T
01088  1.00149  0.67410  0.315.550.23 00012JF
01089  1.01053  1.69551  0.315.460.36 03012JF
01090  0.79206  0.43570  0.315.440.17 00012JT
01090  0.79201  0.43608  0.315.030.42 100516T
01091  0.12074  0.45955  0.313.310.42 100516T
01091  0.12073  0.45928  0.314.200.23 00012JT
01092  1.91686  1.08444  1.214.660.36 05012JF
01093  0.68648  2.47802  0.411.780.41 00012JT
01093  0.68644  2.47802  0.211.200.42 100516T
01094  0.93957  1.04460  0.315.380.28 00012JF
01095  0.05054  2.14642  0.314.830.41 00012JT
01095  0.05034  2.14650  0.214.300.41 100516T
01096  0.38378  1.76276  0.214.790.42 100516T
01096  0.38407  1.76260  0.315.040.36 00012JT
01097  0.98441  0.54519  0.315.180.21 00012JF
01098  0.01064  1.46671  0.212.720.42 100516T
01098  0.01101  1.46654  0.313.160.34 00012JT
01099  0.75140  2.21173  0.315.260.40 00012JT
01099  0.75130  2.21182  0.214.870.42 100516T
01100  1.87560  0.71937  0.413.880.42 100516T
01100  1.87572  0.71914  0.313.920.34 00012JT
01100  1.87600  0.71962  0.412.930.53 1004YBT
01101  2.48192  0.71753  0.310.270.53 1004YBT
01101  2.48178  0.71713  0.611.550.40 0000WST
01101  2.48189  0.71711  0.411.370.40 00012JT
01102  1.90496  0.71533  0.413.400.42 100516T
01102  1.90513  0.71498  0.313.550.34 00012JT
01102  1.90532  0.71550  0.412.630.53 1004YBT
01103  0.04690  1.70561  0.313.200.36 00012JT
01103  0.04658  1.70578  0.212.430.42 100516T
01104  1.12879  0.59115  0.314.390.42 100516T
01104  1.12894  0.59076  0.314.900.24 00012JT
01105  0.28174  1.72371  0.315.530.36 00012JT
01105  0.28150  1.72386  0.214.270.42 100516T
01106  1.87532  0.05328  0.314.990.33 00012JF
01107  1.25108  1.00601  0.314.220.42 100516T
01107  1.25138  1.00568  0.314.400.29 00012JT
01108  2.27059  0.71304  0.313.620.53 1004YBT
01108  2.27042  0.71251  0.314.690.38 00012JT
01109  0.89622  1.42102  0.213.980.42 100516T
01109  0.89644  1.42084  0.314.320.32 00012JT
01110  1.83362  0.10973  0.315.460.33 03012JF
01111  1.52362  0.42070  0.412.790.42 100516T
01111  1.52366  0.42022  0.313.790.29 00012JT
01112  2.12706  0.71019  0.311.490.53 1004YBT
01112  2.12687  0.70968  0.412.290.36 00012JT
01113  1.58877  2.45426  0.412.120.41 00012JT
01113  1.58892  2.45423  0.311.810.42 100516T
01114  0.71025  0.70095  0.314.100.42 100516T
01114  0.71035  0.70064  0.314.670.21 00012JT
01115  0.35108  0.25140  0.411.130.18 00012JF
01116  1.46316  1.55050  0.313.550.36 00012JT
01116  1.46297  1.55057  0.313.170.42 100516T
01117  1.12456  0.92929  0.313.360.27 00012JT
01117  1.12431  0.92965  0.313.030.42 100516T
01118  1.23796  0.77640  0.313.680.42 100516T
01118  1.23816  0.77599  0.314.110.27 00012JT
01119  2.48485  0.34105  0.514.930.40 0000WST
01119  2.48493  0.34109  0.314.230.40 00012JT
01120  1.28794  0.38918  0.313.890.25 00012JT
01120  1.28788  0.38974  0.413.540.42 100516T
01121  0.91314  2.06969  0.211.190.42 100516T
01121  0.91334  2.06951  0.411.670.39 00012JT
01122  0.46225  1.56980  0.210.650.42 100516T
01122  0.46258  1.56962  0.411.160.34 00012JT
01123  1.14430  1.24036  0.213.880.42 100516T
01123  1.14457  1.24014  0.314.170.31 00012JT
01124  0.15302  2.23209  0.415.480.41 03012JF
01125  0.01850  0.97496  0.314.530.29 00012JT
01125  0.01817  0.97519  0.314.100.42 100516T
01126  0.62339  0.24561  0.315.240.12 00012JF
01127  0.70813  1.43650  0.315.520.32 00012JF
01128  1.70730  0.69656  0.314.400.32 00012JT
01128  1.70718  0.69692  0.314.060.42 100516T
01128  1.70767  0.69691  0.413.070.53 1004YBT
01129  0.04858  0.42966  0.313.850.42 100516T
01129  0.04866  0.42937  0.314.440.24 00012JT
01130  1.47222  1.17071  0.312.560.33 00012JT
01130  1.47195  1.17095  0.312.230.42 100516T
01131  1.26200  2.02259  0.214.690.42 100516T
01131  1.26202  2.02248  0.315.140.40 00012JT
01132  2.41296  0.69700  0.312.760.53 1004YBT
01132  2.41291  0.69651  0.313.670.39 00012JT
01133  0.06987  1.36410  0.314.700.33 00012JT
01133  0.06953  1.36426  0.213.880.42 100516T
01134  0.06348  0.42425  0.314.140.24 00012JT
01134  0.06340  0.42455  0.313.540.42 100516T
01135  1.82714  0.69302  0.410.050.53 1004YBT
01135  1.82698  0.69269  0.411.010.33 00012JT
01135  1.82687  0.69286  0.410.320.42 100516T
01136  1.05623  0.64693  0.312.740.42 100516T
01136  1.05635  0.64664  0.313.400.23 00012JT
01137  2.29266  0.69283  0.313.860.38 00012JT
01137  2.29280  0.69344  0.312.680.53 1004YBT
01138  0.35525  0.20097  0.313.980.19 00012JF
01139  1.73837  0.69154  0.414.800.53 1304YBT
01139  1.73801  0.69115  0.314.200.32 03012JT
01140  2.19971  0.18597  0.313.330.37 00012JF
01141  0.55625  0.72066  0.311.880.42 100516T
01141  0.55639  0.72036  0.312.640.22 00012JT
01142  1.53871  0.00754  0.312.910.29 00012JF
01143  0.09327  0.75148  0.315.550.26 00012JF
01144  1.36545  0.56547  0.313.480.27 00012JT
01144  1.36527  0.56591  0.312.980.42 100516T
01145  2.28130  0.45880  0.314.340.38 00012JT
01145  2.28131  0.45930  0.413.200.53 1004YBT
01146  2.48008  0.68948  0.314.100.53 1004YBT
01146  2.48003  0.68900  0.315.270.40 00012JT
01147  0.52467  1.40489  0.315.220.32 00012JT
01147  0.52439  1.40515  0.215.060.42 100516T
01148  1.14253  0.97420  0.315.020.28 00012JT
01148  1.14226  0.97455  0.314.600.42 100516T
01149  1.55784  0.08553  0.411.830.29 00012JF
01150  2.00010  0.67999  0.412.400.42 100516T
01150  2.00042  0.68029  0.412.100.53 1004YBT
01150  2.00019  0.67975  0.312.970.35 00012JT
01151  2.17771  0.68049  0.313.060.53 1004YBT
01151  2.17753  0.67993  0.314.270.37 00012JT
01152  1.63046  1.18853  0.313.930.42 100516T
01152  1.63059  1.18824  0.314.480.34 00012JT
01153  0.08054  1.93324  0.314.000.39 00012JT
01153  0.08029  1.93335  0.213.100.42 100516T
01154  1.20561  0.82711  0.313.050.27 00012JT
01154  1.20546  0.82745  0.312.410.42 100516T
01155  0.73925  2.25471  0.212.940.42 100516T
01155  0.73941  2.25467  0.314.050.41 00012JT
01156  2.07181  0.67661  0.411.120.42 100516T
01156  2.07194  0.67635  0.411.780.36 00012JT
01156  2.07206  0.67683  0.410.570.53 1004YBT
01157  1.57177  0.24691  0.313.390.29 00012JF
01158  0.38611  0.67034  0.313.960.42 100516T
01158  0.38625  0.67000  0.315.090.22 00012JT
01159  1.04352  0.23757  0.315.350.21 00012JF
01160  2.06500  0.67470  0.410.010.53 1004YBT
01160  2.06488  0.67428  0.411.400.36 00012JT
01160  2.06478  0.67449  0.410.260.42 100516T
01161  2.20891  0.67335  0.314.910.37 00012JT
01161  2.20912  0.67394  0.313.790.53 1004YBT
01162  2.14994  0.07734  0.314.880.36 00012JF
01163  0.90853  1.55368  0.315.330.34 00012JF
01164  1.94670  0.04002  0.314.630.34 00012JF
01165  2.18863  0.22772  0.314.610.36 00012JF
01166  1.12830  0.78573  0.314.070.42 100516T
01166  1.12851  0.78536  0.314.390.26 00012JT
01167  0.11561  0.20469  0.4 9.820.23 00012JF
01168  0.81811  0.70645  0.314.650.22 00012JT
01168  0.81796  0.70679  0.314.080.42 100516T
01169  1.15442  0.84795  0.315.510.27 00012JF
01170  2.29299  0.47430  0.413.810.53 1004YBT
01170  2.29299  0.47364  0.315.480.38 00012JT
01171  2.44589  0.66429  0.312.190.53 1004YBT
01171  2.44577  0.66381  0.313.120.40 00012JT
01172  0.25558  0.53132  0.313.820.22 00012JT
01172  0.25549  0.53168  0.312.950.42 100516T
01173  2.37421  0.48190  0.312.860.53 1004YBT
01173  2.37424  0.48132  0.314.080.39 00012JT
01174  2.41749  0.48001  0.313.820.53 1004YBT
01174  2.41752  0.47948  0.315.050.39 00012JT
01175  0.32743  0.21774  0.315.390.19 00012JF
01176  2.47779  0.48170  0.513.570.40 0000WST
01176  2.47780  0.48232  0.312.040.53 1004YBT
01176  2.47787  0.48178  0.313.080.40 00012JT
01177  0.57641  0.62767  0.313.480.42 100516T
01177  0.57654  0.62727  0.313.860.20 03012JT
01178  0.85608  1.81972  0.214.670.42 100516F
01179  2.29881  0.65668  0.315.200.53 1004YBT
01179  2.29865  0.65613  0.315.210.38 00012JT
01180  0.63354  0.41253  0.413.620.42 130516F
01181  0.07248  1.45223  0.215.200.42 100516F
01182  1.11443  0.49861  0.314.720.42 100516F
01183  1.33414  1.89512  0.215.440.42 100516F
01184  0.12885  0.48069  0.314.990.42 100516F
01185  0.05317  1.12034  0.313.450.42 130516F
01186  0.21699  0.79099  0.315.300.42 100516F
01187  2.34039  0.63565  0.310.830.53 1004YBT
01187  2.34028  0.63523  0.411.990.38 00012JT
01188  1.83451  0.49297  0.414.660.53 1004YBT
01188  1.83428  0.49281  0.415.300.42 130516T
01188  1.83438  0.49249  0.314.290.33 03012JT
01189  0.39601  1.34201  0.214.470.42 100516F
01190  0.00499  0.63643  0.315.450.42 100516F
01191  1.89919  0.63180  0.413.400.53 1004YBT
01191  1.89889  0.63166  0.414.250.42 100516T
01191  1.89900  0.63133  0.315.270.34 03012JT
01192  0.18029  1.57435  0.214.990.42 100516F
01193  0.23593  0.40761  0.314.500.42 100516F
01194  0.32855  1.59858  0.215.060.42 100516F
01195  0.37583  0.40600  0.315.370.42 100516F
01196  2.01171  1.28669  0.315.320.42 100516F
01197  1.64326  0.55369  0.315.180.42 100516F
01198  0.89641  2.21873  0.215.180.42 100516F
01199  1.10769  0.69559  0.315.600.42 100516F
01200  1.28082  2.32780  0.215.510.42 100516F
01201  0.46101  0.38810  0.314.800.42 100516F
01202  0.23743  1.72039  0.215.230.42 100516F
01203  0.00108  0.66533  0.314.970.42 100516F
01204  0.41791  2.25107  0.215.580.42 100516F
01205  2.00437  0.50702  0.414.820.42 100516T
01205  2.00454  0.50726  0.413.940.53 1004YBT
01205  2.00443  0.50670  0.315.490.35 00012JT
01206  0.89761  0.69712  0.315.530.42 100516F
01207  0.40354  1.59457  0.215.350.42 100516F
01208  0.76844  0.67165  0.315.280.42 100516F
01209  1.73107  0.60139  0.414.000.53 1004YBT
01209  1.73065  0.60137  0.415.370.42 100516T
01209  1.73088  0.60094  0.315.480.32 00012JT
01210  2.19512  0.59911  0.313.190.37 00012JT
01210  2.19523  0.59972  0.312.020.53 1004YBT
01211  2.33161  0.59973  1.215.260.53 1304YBT
01211  2.33147  0.59910  1.214.690.38 03012JT
01212  0.06956  1.87816  0.214.940.42 100516F
01213  0.00237  0.42514  0.315.560.42 100516F
01214  0.60486  1.28370  0.215.110.42 100516F
01215  1.86170  0.58841  0.414.850.42 100516T
01215  1.86186  0.58811  0.315.160.33 00012JT
01215  1.86199  0.58864  0.413.690.53 1004YBT
01216  0.45142  0.77925  0.314.940.42 100516F
01217  1.98638  2.40478  0.315.250.42 100516F
01218  1.75067  0.51414  0.314.290.32 00012JT
01218  1.75055  0.51452  0.413.860.42 100516T
01218  1.75090  0.51464  0.412.880.53 1004YBT
01219  1.57059  0.58900  0.315.520.42 100516F
01220  0.39515  2.18927  0.215.040.42 100516F
01221  0.03927  0.49663  1.213.610.42 130516F
01222  0.66002  2.33992  0.214.730.42 100516F
01223  0.74608  0.45743  0.315.280.42 100516F
01224  0.58050  1.33396  0.215.250.42 100516F
01225  1.09574  0.74510  0.315.160.42 100516F
01226  2.05025  0.58436  0.412.620.53 1004YBT
01226  2.05007  0.58416  0.413.090.42 100516T
01226  2.05010  0.58385  0.314.120.35 00012JT
01227  2.01352  0.58326  0.314.570.35 00012JT
01227  2.01347  0.58363  0.413.930.42 100516T
01227  2.01371  0.58381  0.413.280.53 1004YBT
01228  0.13516  2.34479  0.215.560.40 100516F
01229  0.74126  0.42270  0.315.370.42 100516F
01230  1.83977  0.58036  0.414.320.42 100516T
01230  1.84010  0.58049  0.413.290.53 1004YBT
01230  1.83995  0.58002  0.314.700.33 00012JT
01231  0.19089  1.94595  0.215.420.42 100516F
01232  2.14892  0.57569  0.412.430.36 00012JT
01232  2.14899  0.57623  0.411.480.53 1004YBT
01233  1.04267  0.55030  0.315.420.42 100516F
01234  1.43463  1.41172  0.315.130.42 100516F
01235  2.38462  0.57622  0.313.190.53 1004YBT
01235  2.38454  0.57566  0.314.300.39 00012JT
01236  0.32259  0.45511  0.315.360.42 100516F
01237  1.29366  1.02684  0.315.620.42 100516F
01238  2.33517  0.57602  0.313.170.53 1004YBT
01238  2.33509  0.57547  0.314.120.38 00012JT
01239  0.28182  0.46508  0.314.910.42 100516F
01240  1.64588  1.54159  0.315.560.42 100516F
01241  1.94594  0.39208  0.314.990.34 00012JT
01241  1.94601  0.39285  0.413.630.53 1004YBT
01241  1.94592  0.39240  0.414.340.42 100516T
01242  0.44263  0.57906  0.315.610.42 100516F
01243  1.45831  1.70121  0.315.160.42 100516F
01244  0.79283  0.44560  0.314.910.42 100516F
01245  0.93768  1.43208  0.315.590.42 130516F
01246  1.66006  0.61827  0.314.930.42 100516F
01247  0.54470  2.36562  0.215.440.42 100516F
01248  2.25527  0.40524  0.314.910.37 00012JT
01248  2.25528  0.40595  0.413.650.53 1004YBT
01249  2.23333  0.56196  0.313.240.53 1004YBT
01249  2.23326  0.56137  0.314.470.37 00012JT
01250  2.26847  0.56040  0.313.810.38 00012JT
01250  2.26860  0.56095  0.312.780.53 1004YBT
01251  1.08507  0.55055  0.314.580.42 100516F
01252  2.17661  0.40903  0.411.320.53 1004YBT
01252  2.17659  0.40843  0.412.330.36 00012JT
01253  2.13022  0.55904  0.313.500.36 00012JT
01253  2.13031  0.55962  0.412.040.53 1004YBT
01254  0.16061  1.63506  0.215.060.42 100516F
01255  0.87054  0.54075  0.315.650.42 100516F
01256  1.80631  2.44593  0.315.330.42 100516F
01257  0.40487  0.57222  1.213.410.42 130516F
01258  1.57001  2.48594  0.315.560.42 100516F
01259  0.69575  0.58242  0.315.460.42 100516F
01260  2.35876  0.55338  0.412.370.38 00012JT
01260  2.35878  0.55393  0.311.150.53 1004YBT
01261  0.69357  1.53530  0.215.300.42 100516F
01262  0.20654  0.46578  0.315.160.42 100516F
01263  1.07836  2.16682  0.215.600.42 100516F
01264  0.50502  0.80236  0.315.300.42 100516F
01265  1.38962  0.96383  0.314.930.42 100516F
01266  1.93676  0.54731  0.315.290.34 00012JT
01266  1.93685  0.54789  0.413.850.53 1004YBT
01266  1.93657  0.54772  0.414.790.42 100516T
01267  0.36522  0.48062  0.315.070.42 100516F
01268  0.68378  2.40465  0.215.000.42 100516F
01269  1.91236  0.42640  0.314.820.33 00012JT
01269  1.91238  0.42706  0.413.240.53 1004YBT
01269  1.91227  0.42682  0.414.190.42 100516T
01270  0.82245  0.93236  0.315.510.42 100516F
01271  0.55341  1.04508  0.315.210.42 100516F
01272  1.87591  0.42691  0.313.710.33 00012JT
01272  1.87602  0.42759  0.412.290.53 1004YBT
01272  1.87584  0.42731  0.412.990.42 100516T
01273  1.07481  2.07814  0.215.560.42 100516F
01274  2.15535  0.42955  0.412.480.53 1004YBT
01274  2.15521  0.42893  0.313.840.36 00012JT
01275  0.55398  2.11330  0.215.550.42 100516F
01276  1.88825  0.53593  0.412.760.42 100516T
01276  1.88852  0.53612  0.412.090.53 1004YBT
01276  1.88840  0.53552  0.313.230.33 00012JT
01277  1.61681  2.03483  0.315.020.42 100516F
01278  1.52638  1.58583  0.314.890.42 100516F
01279  0.91179  2.34697  0.215.430.42 100516F
01280  0.56226  2.07808  0.215.520.42 100516F
01281  1.57017  0.87826  0.315.480.42 100516F
01282  1.68703  0.43300  0.511.310.53 1004YBT
01282  1.68673  0.43248  0.412.440.31 00012JT
01282  1.68664  0.43300  0.411.920.42 100516T
01283  1.28997  1.38578  0.215.590.42 100516F
01284  1.04410  2.46876  0.311.040.42 130516F
01285  1.36832  1.77770  0.215.430.42 100516F
01286  0.06018  1.72793  0.215.290.42 100516F
01287  0.32859  1.60042  0.315.030.42 100516F
01288  2.47805  0.45100  0.514.200.40 0000WST
01288  2.47806  0.45158  0.312.380.53 1004YBT
01288  2.47816  0.45105  0.313.570.40 00012JT
01289  0.61611  1.04274  0.315.200.42 100516F
01290  0.57033  1.44750  0.215.530.42 100516F
01291  2.21871  0.52521  0.313.500.37 00012JT
01291  2.21873  0.52581  0.412.360.53 1004YBT
01292  0.13000  1.81719  0.215.500.42 100516F
01293  0.28657  2.13873  0.215.010.42 100516F
01294  0.24333  0.91810  0.315.380.42 100516F
01295  0.56352  2.20482  0.215.440.42 100516F
01296  0.22524  1.03738  0.315.240.42 100516F
01297  0.23976  2.12519  0.215.560.42 100516F
01298  2.36877  0.45910  0.414.320.53 1004YBF
01299  0.18524  2.10449  0.215.370.42 100516F
01300  1.78416  0.52000  0.413.940.53 1004YBT
01300  1.78401  0.51947  0.315.440.32 00012JT
01301  0.05076  2.09733  0.214.970.41 130516F
01302  1.24863  2.36003  0.215.550.42 130516F
01303  0.93350  1.44169  0.215.130.42 100516F
01304  0.82139  2.28280  0.215.100.42 100516F
01305  1.29566  1.26945  0.315.540.42 100516F
01306  2.09377  0.51502  0.314.490.36 00012JT
01306  2.09387  0.51560  0.413.010.53 1004YBT
01307  2.48219  1.44347  0.214.790.53 1004YBF
01308  1.94367  2.17377  0.314.930.53 1004YBF
01309  2.10663  0.53053  0.414.080.53 1004YBF
01310  1.80747  2.32297  0.315.230.53 1004YBF
01311  1.82136  0.43721  0.414.440.53 1004YBF
01312  1.85061  2.16336  0.314.340.53 1004YBF
01313  2.43312  1.10872  0.314.600.53 1004YBF
01314  2.41107  1.80681  0.314.400.53 1004YBF
01315  2.21736  0.69238  0.4 7.150.53 1304YBF
01316  2.36774  1.85112  0.313.920.53 1304YBT
01316  2.36753  1.85092  0.315.520.41 03012JT
01317  2.15479  0.90928  0.314.540.53 1004YBF
01318  2.18916  2.43045  0.314.970.53 1004YBF
01319  2.41112  1.54029  0.315.500.53 1004YBF
01320  2.09923  2.08883  0.314.690.53 1004YBF
01321  1.86156  0.76850  0.414.520.53 1004YBF
01322  2.49318  2.21047  0.213.980.53 1004YBF
01323  1.72706  0.77927  0.414.320.53 1004YBF
01324  1.86526  2.06519  0.315.640.42 100516T
01324  1.86548  2.06533  0.314.730.53 1004YBT
01325  2.35229  0.77540  0.314.590.53 1004YBF
01326  1.72413  2.20391  0.315.130.53 1004YBF
01327  2.03998  0.91583  0.414.980.42 100516T
01327  2.04052  0.91597  0.314.030.53 1004YBT
01328  1.79043  2.46230  0.314.820.53 1004YBF
01329  1.74834  0.94263  0.414.910.53 1004YBF
01330  2.13467  2.21705  0.314.780.53 1004YBF
01331  1.90562  0.66225  0.414.720.53 1004YBF
01332  1.79499  2.11010  0.314.900.53 1004YBF
01333  2.35761  0.60692  0.314.170.53 1004YBF
01334  1.78692  2.04705  0.314.090.53 1004YBT
01334  1.78667  2.04712  0.315.010.42 100516T
01335  1.98548  0.91746  0.314.700.42 100516T
01335  1.98596  0.91759  0.313.950.53 1004YBT
01336  1.73274  2.26659  0.314.570.53 1004YBF
01337  2.12601  0.72191  0.314.980.53 1004YBF
01338  1.87717  1.78000  0.315.380.42 100516T
01338  1.87756  1.78020  0.314.950.53 1004YBT
01339  1.83153  1.66484  0.3 9.060.42 100516T
01339  1.83146  1.66502  0.5 9.600.39 00012JT
01339  1.83154  1.66503  0.3 8.820.53 1004YBT
01340  1.68213  2.00280  0.315.190.53 1004YBF
01341  1.93715  0.59800  0.414.580.53 1004YBF
01342  2.14636  1.89925  0.314.990.41 03012JT
01342  2.14672  1.89934  0.314.940.53 1004YBT
01343  1.77988  0.49374  0.414.750.53 1004YBF
01344  2.13576  2.48010  0.314.850.53 1004YBF
01345  1.67178  0.41849  0.415.400.42 100516T
01345  1.67223  0.41859  0.514.470.53 1004YBT
01346  1.93906  2.05074  0.314.940.53 1004YBF
01347  1.94051  0.52515  0.414.410.53 1304YBF
01348  1.69772  1.91507  0.315.310.53 1304YBF
01349  1.82709  0.60561  0.414.550.53 1004YBF
01350  2.36596  1.85005  0.313.890.53 1304YBT
01350  2.36576  1.84985  0.314.550.41 03012JT
01351  1.82762  0.58222  0.415.200.53 1004YBF
01352  1.99266  1.73980  0.313.660.53 1004YBF
01353  2.37342  1.10318  0.314.850.53 1004YBF
01354  1.79656  1.80373  0.315.270.53 1004YBF
01355  2.44491  1.56917  0.215.120.53 1004YBF
01356  2.32646  2.14299  0.214.210.53 1004YBF
01357  2.14918  1.19612  0.314.320.53 1004YBF
01358  1.73601  2.36663  0.314.690.53 1004YBF
01359  2.36884  0.43413  0.414.610.53 1004YBF
01360  1.78084  2.33968  0.314.590.53 1004YBF
01361  2.03492  1.50649  0.315.290.53 1004YBF
01362  2.36944  2.41390  0.214.340.53 1004YBF
01363  1.70026  0.58333  0.413.960.53 1004YBF
01364  1.72814  2.00541  0.314.260.53 1004YBT
01364  1.72786  2.00548  0.315.460.42 100516T
01365  1.95870  1.36787  0.315.420.53 1004YBF
01366  1.82494  1.76084  0.315.280.53 1004YBF
01367  1.78854  1.29337  0.315.100.53 1004YBF
01368  1.91464  2.14899  0.314.590.53 1004YBF
01369  2.04667  0.40936  0.414.590.53 1004YBF
01370  2.42754  2.43343  0.214.370.53 1004YBF
01371  2.35655  0.51139  0.513.580.53 1304YBF
01372  2.06876  1.83283  0.314.760.53 1004YBF
01373  2.33338  0.47345  0.414.850.53 1304YBF
01374  1.98696  1.75026  0.314.530.53 1304YBF
01375  2.41428  1.25928  0.314.730.53 1004YBF
01376  1.72951  1.90067  0.314.900.42 100516T
01376  1.72979  1.90067  0.313.870.53 1004YBT
01377  2.35214  0.96109  0.314.930.53 1004YBF
01378  1.70270  2.05826  0.314.540.53 1004YBF
01379  2.34682  0.84308  0.314.580.53 1004YBF
01380  2.19374  2.38951  0.314.790.53 1004YBF
01381  2.09452  1.43193  0.314.780.53 1004YBF
01382  1.95250  1.84587  0.314.660.53 1004YBT
01382  1.95222  1.84568  0.315.320.42 100516T
01383  2.41326  1.02821  0.314.880.53 1004YBF
01384  1.67920  2.11183  0.314.580.53 1004YBT
01384  1.67886  2.11204  0.315.630.42 100516T
01385  1.78045  0.70882  0.414.470.53 1004YBF
01386  2.33996  0.71374  0.315.010.53 1004YBF
01387  1.81167  1.31176  0.314.570.53 1004YBF
01388  1.82008  1.39223  0.314.600.53 1004YBF
01389  2.04268  0.80564  0.414.670.42 100516T
01389  2.04306  0.80591  0.314.240.53 1004YBT
01390  1.77282  1.70042  0.314.870.53 1004YBF
01391  1.76633  0.45126  0.414.550.53 1004YBF
01392  2.07229  0.81919  0.314.250.53 1004YBF
01393  1.90577  1.05603  0.314.270.53 1004YBF
01394  1.91677  1.09437  0.4 8.260.53 1004YBT
01394  1.91689  1.09415  0.4 8.380.42 100516T
01394  1.91691  1.09424  0.6 8.240.36 00012JT
01395  1.87588  0.94381  0.314.160.53 1004YBT
01395  1.87539  0.94365  0.314.840.42 100516T
01396  2.02527  0.92959  0.314.290.53 1004YBT
01396  2.02472  0.92948  0.414.900.42 100516T
01397  2.08102  1.47451  0.314.940.53 1004YBF
01398  1.70463  1.70361  0.315.410.53 1004YBF
01399  2.03316  1.67259  0.315.130.53 1004YBF
01400  2.17010  0.88704  0.313.880.53 1004YBF
01401  1.90835  1.48073  0.314.720.53 1004YBF
01402  2.48003  0.55106  0.314.710.53 1004YBF
01403  2.14265  1.12492  0.314.350.53 1004YBF
01404  2.33665  1.27670  1.215.090.53 1004YBF
01405  2.14344  1.02303  0.314.590.53 1004YBF
01406  2.29704  1.12149  0.315.010.53 1004YBF
01407  2.09064  0.83585  0.314.950.53 1004YBF
01408  2.13846  0.51042  0.414.390.53 1004YBF
01409  1.74789  1.03607  0.414.370.53 1304YBF
01410  1.87243  1.07398  0.314.720.53 1004YBT
01410  1.87190  1.07385  0.315.590.42 100516T
01411  2.47455  0.40334  0.414.670.53 1004YBF
01412  1.82012  1.37670  0.314.400.53 1004YBF
01413  1.67425  0.69955  0.414.680.53 1004YBF
01414  2.43703  1.49718  0.214.810.53 1004YBF
01415  1.78078  0.86045  0.315.240.42 100516T
01415  1.78130  0.86046  0.414.190.53 1004YBT
01416  2.31358  1.01551  0.314.920.53 1004YBF
01417  1.99712  1.56258  0.314.940.53 1004YBF
01418  2.07739  0.89490  1.215.140.53 1004YBF
01419  2.44488  0.69626  0.313.950.53 1004YBF
01420  2.34221  0.77759  0.314.630.53 1004YBF
01421  1.71880  0.55343  0.414.350.53 1004YBF
01422  1.75656  0.51706  0.415.110.53 1004YBF
01423  2.31262  1.10907  0.314.600.53 1004YBF
01424  2.35735  0.50782  0.513.480.53 1004YBF
01425  2.49137  1.65645  0.214.750.53 1004YBF
01426  2.44655  0.85852  0.315.370.53 1004YBF
01427  2.30373  0.59147  0.314.640.53 1004YBF
01428  2.30157  0.72999  0.314.990.53 1004YBF
01429  2.02204  1.51422  0.315.190.53 1304YBF
01430  2.00380  0.45183  0.415.050.53 1004YBF
01431  1.73623  0.63419  0.414.360.53 1304YBF
01432  2.30370  1.30515  0.314.320.53 1304YBF
01433  2.40522  0.76673  0.314.330.53 1004YBF
01434  2.20832  0.62152  0.314.510.53 1004YBF
01435  2.21913  0.96739  0.314.860.53 1004YBF
01436  2.11762  1.05396  0.315.230.53 1004YBF
01437  1.81607  1.08632  0.314.800.53 1004YBF
01438  1.76841  1.57601  0.314.460.53 1004YBF
01439  2.20654  1.18370  0.314.810.53 1004YBF
01440  1.86341  0.86667  0.414.310.53 1004YBF
01441  2.48153  0.39507  0.414.810.53 1004YBF
01442  1.80424  0.74627  1.214.040.53 1004YBF
01443  2.14388  0.63121  0.314.650.53 1004YBF
01444  1.99538  0.84596  0.415.070.42 100516T
01444  1.99583  0.84606  0.314.250.53 1004YBT
01445  1.70355  0.96167  0.414.560.53 1004YBF
01446  2.19014  1.62745  0.315.000.53 1004YBF
01447  1.13203  1.35646  0.214.890.42 100516F
01448  0.17134  2.47298  0.215.120.39 100516F
01449  1.29973  1.32621  0.315.360.42 100516F
01450  0.90375  1.20399  0.215.270.42 100516F
01451  1.13147  1.57223  1.215.640.42 100516F
01452  0.16758  1.07936  0.214.480.42 100516F
01453  0.77762  2.46172  0.214.980.42 100516F
01454  0.66635  1.31978  0.215.320.42 100516F
01455  1.56077  2.23256  0.315.280.42 100516F
01456  0.09044  0.96447  0.314.560.42 100516F
01457  0.81784  2.21532  0.215.610.42 100516F
01458  0.99035  2.22633  0.215.630.42 100516F
01459  1.30597  1.51035  0.215.350.42 100516F
01460  1.63298  1.16277  0.315.630.42 100516F
01461  0.80707  2.18318  0.215.340.42 100516F
01462  0.34735  1.56165  0.215.240.42 100516F
01463  0.36298  1.47223  0.214.960.42 100516F
01464  0.81083  1.43489  0.215.430.42 100516F
01465  1.74124  1.64240  0.315.440.42 100516F
01466  0.38209  1.89454  0.215.640.42 100516F
01467  0.32696  2.13955  1.215.490.42 100516F
01468  1.10181  1.69648  0.215.060.42 100516F
01469  0.73189  1.47854  0.215.370.42 100516F
01470  0.88336  1.27656  0.214.800.42 100516F
01471  1.17217  1.15036  0.315.270.42 100516F
01472  0.06075  1.76451  0.215.450.42 100516F
01473  0.76043  1.24264  0.215.650.42 100516F
01474  1.43772  1.87820  0.215.470.42 100516F
01475  0.95803  0.90744  0.314.970.42 100516F
01476  1.68344  0.92941  0.315.660.42 100516F
01477  0.04680  0.83064  0.314.990.42 100516F
01478  0.92576  0.97936  0.315.230.42 100516F
01479  0.17860  0.95968  0.315.210.42 100516F
01480  0.82387  1.05253  0.315.220.42 100516F
01481  0.23898  2.03775  0.215.590.42 100516F
01482  1.51504  1.07981  0.315.280.42 100516F
01483  0.94562  1.27577  0.215.520.42 100516F
01484  0.73296  1.93595  0.214.720.42 100516F
01485  0.46185  0.85774  0.315.170.42 100516F
01486  0.46507  2.21277  0.215.200.42 100516F
01487  1.26115  1.27353  0.314.970.42 100516F
01488  0.23968  1.14924  0.214.840.42 100516F
01489  0.35657  0.85425  0.315.070.42 100516F
01490  0.41992  1.10108  0.214.960.42 100516F
01491  0.86810  1.30616  0.315.110.42 130516F
01492  1.33572  1.29318  0.315.560.42 100516F
01493  0.03283  0.89953  0.315.160.42 100516F
01494  0.35185  2.04760  0.215.370.42 100516F
01495  0.26148  1.32856  0.215.140.42 100516F
01496  0.25814  1.02097  0.315.140.42 100516F
01497  0.29901  0.82772  0.315.620.42 100516F
01498  0.74146  1.97671  0.215.200.42 100516F
01499  1.45189  1.19921  1.215.370.42 130516F

 2605 Rows written
label-li 0001.ibis
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File 0001.ibis ************
                3 dimensional TABULAR file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                0 lines per band
                512 samples per line
                326 lines of binary header of type IBIS
                0 bytes of binary prefix per line
---- Property: IBIS ----
TYPE='FITS TABLE CONVERSION'
NR=2605
NC=10
ORG='ROW'
FMT_DEFAULT='REAL'
FMT_FULL=(1, 7, 8)
FMT_ASCII=(9, 10)
ASCII_LEN=(4, 1)
GROUPS=('GSC_ID', 'RA_DEG', 'DEC_DEG', 'POS_ERR', 'MAG', 'MAG_ERR', 
'MAG_BAND', 'CLASS', 'PLATE_ID', 'MULTIPLE')
GROUP_1=1
GROUP_2=2
GROUP_3=3
GROUP_4=4
GROUP_5=5
GROUP_6=6
GROUP_7=7
GROUP_8=8
GROUP_9=9
GROUP_10=10
SEGMENT=64
BLOCKSIZE=512
COFFSET=(0, 4, 8, 12, 16, 20, 24, 28, 32, 37)
---- Task: FITSIN -- User: wlb -- Fri Jan  8 12:43:25 2016 ----
VF001='XTENSION= "TABLE   "           / Table Extension'
VF002='BITPIX  =                    8 / Character Information'
VF003='NAXIS   =                    2 / Two-dimensional table'
VF004='NAXIS1  =                   45 / Number of characters per line'
VF005='NAXIS2  =                 2605 / Number of rows'
VF006='PCOUNT  =                    0 / No random parameters'
VF007='GCOUNT  =                    1 / Only one group'
VF008='TFIELDS =                   10 / Ten fields per row'
VF009=''
VF010='EXTNAME = "GSC_REGION_00001"   / GSC Region No. 00001'
VF011='EXTVER  =                    1 / Integer Version Number'
VF012=''
VF013='TTYPE1  = "GSC_ID  "           / ID within Region'
VF014='TBCOL1  =                    1 / Start in column 1'
VF015=
'TFORM1  = "I5      "           / Integer, 5 character field (I5.5 Style)'
VF016=''
VF017=
'TTYPE2  = "RA_DEG  "           / Right Ascension - Decimal Degrees (0 to 360)'
VF018='TBCOL2  =                    6 / Start in column 6'
VF019='TFORM2  = "F9.5    "           / Floating, 9 character field'
VF020=''
VF021=
'TTYPE3  = "DEC_DEG "           / Declination - Decimal Degrees (-90 to +90)'
VF022='TBCOL3  =                   15 / Start in column 15'
VF023='TFORM3  = "F9.5    "           / Floating, 9 character field'
VF024=''
VF025='TTYPE4  = "POS_ERR "           / Position Error in Arc Seconds'
VF026='TBCOL4  =                   24 / Start in column 24'
VF027='TFORM4  = "F5.1    "           / Floating, 5 character field'
VF028=''
VF029='TTYPE5  = "MAG     "           / Magnitude'
VF030='TBCOL5  =                   29 / Start in column 29'
VF031='TFORM5  = "F5.2    "           / Floating, 5 character field'
VF032=''
VF033='TTYPE6  = "MAG_ERR "           / Magnitude error'
VF034='TBCOL6  =                   34 / Start in column 34'
VF035='TFORM6  = "F4.2    "           / Floating, 4 character field'
VF036=''
VF037='TTYPE7  = "MAG_BAND"           / Magnitude Band'
VF038='TBCOL7  =                   38 / Start in column 38'
VF039=
'TFORM7  = "I2      "           / Integer, 2 character field (I2.2 Style)'
VF040=''
VF041='TTYPE8  = "CLASS   "           / Classification'
VF042='TBCOL8  =                   40 / Start in column 40'
VF043='TFORM8  = "I1      "           / Integer, 1 character field'
VF044=''
VF045='TTYPE9  = "PLATE_ID"           / GSSS Internal Plate Number'
VF046='TBCOL9  =                   41 / Start in column 41'
VF047='TFORM9  = "A4      "           / 4 character field'
VF048=''
VF049='TTYPE10 = "MULTIPLE"           / (T/F) Flag for additional entries'
VF050='TBCOL10 =                   45 / Start in column 45'
VF051='TFORM10 = "A1      "           / Logical flag, 1 character field'
VF052=''
VF053='END'
 
************************************************************
ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats  +
	screen=120
Beginning VICAR task ibis
 
Number of Rows:2605  Number of Columns: 10      
File Version:IBIS-2  Organization:ROW  SubType:FITS TABLE CONVERSION
Group 'GSC_ID': 1
Group 'RA_DEG': 2
Group 'DEC_DEG': 3
Group 'POS_ERR': 4
Group 'MAG': 5
Group 'MAG_ERR': 6
Group 'MAG_BAND': 7
Group 'CLASS': 8
Group 'PLATE_ID': 9
Group 'MULTIPLE': 10
 
Rows: 1:15
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6         C:7         C:8         C:9
        FULL        REAL        REAL        REAL        REAL        REAL        FULL        FULL          A4
GSC_ID      RA_DEG      DEC_DEG     POS_ERR     MAG         MAG_ERR     MAG_BAND    CLASS       PLATE_ID    
 --          --          --          --          --          --          --          --          --         
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------
           1        0.99        2.33        0.30       14.76        0.41           0           0        012J
           1        0.99        2.33        0.20       14.34        0.42           1           0        0516
           2        0.78        2.41        0.30       15.55        0.41           0           3        012J
           3        0.35        1.95        0.20       13.85        0.42           1           0        0516
           3        0.35        1.95        0.30       14.47        0.38           0           0        012J
           4        1.65        1.62        0.30       14.01        0.38           0           0        012J
           4        1.65        1.62        0.30       13.76        0.42           1           0        0516
           5        1.80        2.22        0.30       14.91        0.42           1           0        0516
           5        1.80        2.22        0.30       13.86        0.53           1           0        04YB
           5        1.80        2.22        0.30       15.01        0.41           0           3        012J
           6        0.33        1.61        0.20       14.85        0.42           1           0        0516
           6        0.33        1.61        0.30       15.19        0.34           0           0        012J
           7        1.57        1.94        0.30       14.23        0.42           1           0        0516
           7        1.57        1.94        0.30       14.76        0.40           0           0        012J
           8        2.32        2.23        0.20       11.56        0.53           1           0        04YB
 
Rows: 1:15
+-----------
        C:10
          A1
MULTIPLE    
 --         
+-----------
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
ush rm -f tt
ush rm -f nn
ush rm -f ct
let $echo="no"
$ Return
$!#############################################################################
