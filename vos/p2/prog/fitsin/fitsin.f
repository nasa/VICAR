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
