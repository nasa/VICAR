$!****************************************************************************
$!
$! Build proc for MIPL module manmatch
$! VPACK Version 1.9, Friday, August 05, 2005, 11:55:03
$!
$! Execute by entering:		$ @manmatch
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
$ write sys$output "*** module manmatch ***"
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
$ write sys$output "Invalid argument given to manmatch.com file -- ", primary
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
$   if F$SEARCH("manmatch.imake") .nes. ""
$   then
$      vimake manmatch
$      purge manmatch.bld
$   else
$      if F$SEARCH("manmatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake manmatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @manmatch.bld "STD"
$   else
$      @manmatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create manmatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack manmatch.com -mixed -
	-s manmatch.f tc4sedr.f tc4nosedr.f -
	-i manmatch.imake -
	-p manmatch.pdf -
	-t tstmanmatch.pdf unix_moon.list unix_red.list unix_summ.list -
	   casimglist.unix vms_moon.list vms_red.list vms_summ.list -
	   casimglist.vms tstmanmatch.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create manmatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
        include 'VICMAIN_FOR'
        subroutine main44
	implicit none
	include 'ibisfile'			
 	include 'ibiserrs'        
	integer	ncol,maxpix,maxpts,inst1,inst2
	data inst1/1/,inst2/2/
	parameter (ncol = 12,maxpix=200,maxpts=1000)
        integer   ibisUnit, ibis, status, count, def
	integer   instance,side1,side2
 	integer   winsl1, winss1, winsl2, winss2
 	integer   mean1,mean2,left_image,right_image,pair
        integer   npict,noverlap,ncol1,ncol2,ncol3,dcode7
        integer   fds(maxpix), icam
        integer   window(2,maxpts)
        integer   overlap(maxpix,2)
        integer  corrsize(2)
 	integer*4  dell, dels, dell2, dels2
 	integer  l, s, i, j, k, n, m
 	integer  clen, tiep, col
 	integer  ierr, ncorr
 	integer   bottom,right,sl1,ss1,sl2,ss2,side
 	integer   iseed,number
        integer   lbox,nextpair
 	integer   four,zero,one
 	integer    vrdi_unit
        integer*4 idatal(40),idatar(40)
        integer idgeom,idibis
        integer ioerr,nfds,switch,zoom1,zoom2
        integer i256,i190,i0,i1,i2,i3,i4,i5,i6,i7,i8,i9
        integer  i21,i22,i23,i27,i26,i28,i29,i30,i31
        integer x1,x2,x3,x4,x5,x6
	integer igoto32, igoto34, igoto33

	data igoto32, igoto34, igoto33/0, 0, 0/
        data   i256/256/,dcode7/7/,i190/190/,i1/1/,i2/2/,i3/3/
        data   i4/4/,i5/5/,i6/6/,i7/7/,i8/8/,i9/9/,i0/0/
        data  i21/21/,i22/22/,i23/23/,i26/26/,i27/27/,i28/28/
        data  i29/29/,i30/30/,i31/31/
         
	byte       	brite
        character*1     iname(255,maxpix)
        character*255   iiname
	character*255   name(maxpix)
	
        real*8  data8l(20),data8r(20),coefx(4),coefy(4)

	real	buf1(256,256), buf2(256,256)
	real	bufout (256*258),  corrpatch(3,3)
	real	ibisbuf (maxpts,ncol),over(maxpix,2)
	real	phase, pfilter, offmax
	real	delline, delsamp, corrval,percent
	real    radpol(maxpix),eqpol(maxpix),focal(maxpix),r_icam(maxpix)
        real    optaxl(maxpix),optaxs(maxpix),scale(maxpix)
        real    rsvec(maxpix,3),omangl(maxpix,3)
        real*4    datal(40),datar(40)
C       real    lout_cen,sout_cen
	real*4 cen_line, cen_samp
	real    conv(3600)
	
	logical	hpf, subpix, xvptst, redocorr, noprint
        logical xviptst,redoimg,automatic,sedr,SEDRead,nocor
        
        integer nofile,xnl1, xns1,xnl2,xns2
        

        character*5    project
	character*3    funcstr
        character*255  dirname
	character*255  string,filename
	character*256  typestr
	character*512  msgbuf 
	character*256  msgbuf1 
	character*256  msgbuf2 
	character*512  msgbuf3
	
C	GLOBAL VARIABLES:

	integer	winnl, winns, unit1, unit2, func
	integer	nl1, ns1, nl2, ns2, bord, gridsize(2)
	real	xg(3,2)
	common /global/  winnl, winns, unit1, unit2, func,
     +		nl1, ns1, nl2, ns2, bord, gridsize, xg
      
   	equivalence (data8l,datal,idatal)			
    	equivalence (data8r,datar,idatar)			
 	
        data bottom/512/,right/512/,four/4/,zero/0/
        data one/1/,iseed/98737461/,side1/1/,side2/2/
        
        integer graphicsplane, imageplane
        data imageplane/1/
        
	call ifmessage(' MANMATCH Version Nov 10 2002')

C	GET PARAMETERS FROM PDF
	call xvp ('CORR', corrsize, count)
	winnl = corrsize(1)
	winns = corrsize(2)
	call xvp ('PFILTER', pfilter, count)
        call xvp ('PHASE', phase, count)
        call xvp ('OFFMAX', offmax, count)
        call xvp ('FUNC', funcstr, count)
        if (funcstr .eq. 'LOG')  func = 1
	if (funcstr .eq. 'EXP')  func = 2
        call xvparm('PROJECT',project,count,def,' ')
        call xvparm('DIR',dirname,count,def,' ')
        subpix = xvptst ('SUBPIX')
        redocorr = xvptst ('REDO')
        noprint = xvptst('NOPRINT')
        call xvparm('FILENAME',filename,n,i,' ')
        if(xvptst('SEDR')) sedr=.true.
        if(xvptst('NOSEDR')) sedr=.false.             

c 	GET FILENAME 
        if(filename.ne.'NOFILE')then
          open(unit=3,file=filename,status='OLD',err=102,
     +         access='SEQUENTIAL',form='FORMATTED')
          goto 101
          
c	  ERROR READING FILENAME
102       msgbuf='Error opening FILENAME file: '
	  write(msgbuf(30:),'(A)') filename
	  call xvmessage(msgbuf,' ')
          call abend
          
101       i=0
          do while(.true.)
            i=i+1
            do k=1,255
               iname(k,i)=' '
            enddo
 	    read(3,103,iostat=ioerr,err=104,end=105)
     +        (iname(j,i),j=1,80)
     
103         format(80a1)
            do m=1,255
            	  write(iiname(m:),'(A)') iname(m,i)
            enddo
            write(name(i),'(A)') iiname
          enddo
          
104       msgbuf='Error opening FILENAME file: '
	  write(msgbuf(30:),'(A)') filename
	  call xvmessage(msgbuf,' ')
          call prnt(4,1,ioerr,'fortran iostat error #=.')
          call abend
          
105       close(unit=3)
          nfds=i-1
          call prnt(4,1,nfds,'# images in FILENAME file=.')
        endif

c 	set word 39 of convev buffer
        if(xvptst('OBJECT'))then
           call xvmessage('Treating images as OBJECT space images',' ')
           idatal(39)=8
           idatar(39)=8
        else
           call xvmessage('Treating images as IMAGE space images',' ')
           idatar(39)=7
           idatal(39)=7
        endif         

c 	SET DEFAULTS
	hpf = .true.
	ncorr = 0
	winsl1 = 10
        winsl2=  10
        winss2=  10
        winss1=  10
	tiep = 1
        pair=1
        sl1=1
        ss1=1
        sl2=1
        ss2=1
        redoimg=.false.
        percent=2.
        nocor=.false.
        
c 	READ IN SEDR.INT FILE
	typestr = 'SEDR.INT file'
	instance=1
        call openIFile4Rd(ibisUnit,ibis,instance,npict,ncol1,
     +   			nofile,typestr,maxpix)		
        if(nofile .eq. 1) then
            call xvmessage('First input file not a SEDR.INT file',' ')
            call prnt(4,1,ncol1,'Number of columns=.')
            call abend
        endif
        call prnt(4,1,npict,'# images in SEDR file=.')    

c 	....For GLL or Cassini, all frames must be in same imaging mode
        if (project(1:3).eq.'GLL' .or. project(1:5).eq.'CASSI') then
          call mvcl(project,conv,5)
          call ibis_column_read(ibis,r_icam,4,1,npict,status)
          DO i=1,npict
            IF (r_icam(i).NE.r_icam(1)) THEN
              write(msgbuf3,*) 'Imaging mode is different ',
     +             'between images!!!'
              call mabend (msgbuf3,' ')
            END IF
          END DO
          icam = r_icam(1)
          call mve(4,1,icam,conv(3),1,1)
        endif

c read first column (fds times).
c replace with 1,2,3,4.. if FILENAME file is used.
        if(filename.eq.'NOFILE')then
     		call ibis_column_read(ibis,fds,i1,i1,npict,status)
     	   	
        else
           if (npict.ne.nfds) then
              	msgbuf1='The # pictures in FILENAME and the SEDR '
              	msgbuf2='ibis file disagree'
              	msgbuf3=msgbuf1
              	write(msgbuf3(41:),'(A)') msgbuf2
		call xvmessage(msgbuf3,' ')
              call abend
           endif
           do i=1,npict
              fds(i)=i
           enddo
        endif
        
c 	read the xtra columns needed if SEDR requested
        SEDRead = .FALSE.      ! signal that SEDR not read
        if(sedr)then
           if(ncol1.lt.31) then
            call xvmessage('SEDR.INT file not created by IBISNAV',' ')
            call prnt(4,1,ncol1,'Number of columns=.')
            call abend
           endif
	   call ibis_column_read(ibis,rsvec(1,1),i5,i1,npict,status) 
	   call ibis_column_read(ibis,rsvec(1,2),i6,i1,npict,status)  
	   call ibis_column_read(ibis,rsvec(1,3),i7,i1,npict,status) 
	   call ibis_column_read(ibis,omangl(1,1),i21,i1,npict,status)
	   call ibis_column_read(ibis,omangl(1,2),i22,i1,npict,status)
	   call ibis_column_read(ibis,omangl(1,3),i23,i1,npict,status) 
	   call ibis_column_read(ibis,radpol,i26,i1,npict,status)
	   call ibis_column_read(ibis,eqpol,i27,i1,npict,status)
	   call ibis_column_read(ibis,focal,i28,i1,npict,status)
	   call ibis_column_read(ibis,optaxl,i29,i1,npict,status)
	   call ibis_column_read(ibis,optaxs,i30,i1,npict,status)
	   call ibis_column_read(ibis,scale,i31,i1,npict,status)

           SEDRead = .TRUE.      ! Signal that SEDR is read
	endif
        call closeIbisFile(ibisUnit,ibis, status)
               
               
c       READ OVER.INT FILE
99	typestr = 'OVER.INT file'
	instance = 2
        call openIFile4Rd(ibisUnit,ibis,instance,noverlap,ncol2,
     +   				nofile,typestr,maxpix)	
        
        if(ncol2.ne.2) then
           call xvmessage('Second input file not an OVER file',' ')
           call prnt(4,1,ncol2,'Number of columns=.')
           call abend
        endif
        col = 1     		
        call ibis_column_read(ibis,over(1,1),i1,i1,noverlap,status)
        call ibis_column_read(ibis,over(1,2),i2,i1,noverlap,status)
        
        call closeIbisFile(ibisUnit,ibis, status)
        call prnt(4,1,noverlap,'# image pairs in over file=.')
                
        do i=1,noverlap
          overlap(i,1)=nint(over(i,1))
          overlap(i,2)=nint(over(i,2))
        enddo

c Determine what the ID of files 3 and 4 are (if present).
c idgeom=INP# of geom file, 0 means not there.
c idibis=INP# of ibis continuation file, 0 means not there.
        call getinput_id(idgeom,idibis)

c 	check consistency.
        if((idatal(39).eq.7).and.(idgeom.eq.0).and.sedr)then
          if(project.ne.'GLL  ' .and. project.ne.'CASSI')then
            call xvmessage('SEDR & IMAGE space requires GEOMA file',' ')
            call abend
          endif
        endif

c 	READ TIEPOINTS FILE  (if present as input file 3 or 4.)
        if(idibis.gt.0)then
           automatic=.false.
           redoimg=.true.
           side=0
           typestr = 'Tiepoints file'
           call openIFile4Rd(ibisUnit,ibis,idibis,tiep,
     +				      ncol3,nofile,typestr,maxpix) 		                     
           if(ncol3.ne.ncol)then
              call xvmessage(' Incorrect # cols in tiepoints file ',' ')              
              call abend
           endif
           do col=1,ncol3
     	      call ibis_column_read(ibis,ibisbuf(1,col),col,i1,tiep,
     +     	      					status)	
           enddo
           call closeIbisFile(ibisUnit,ibis, status)
           call prnt(4,1,tiep,' Points located in tiep file=.')
           
c          Determine the last pair used.
           left_image=nint(ibisbuf(tiep,1))
           right_image=nint(ibisbuf(tiep,2))
           do i=1,noverlap
              if((left_image.eq.overlap(i,1)).and.
     +           (right_image.eq.overlap(i,2)))then
                pair=i
                goto 37
              endif
           enddo         
     	   msgbuf='WARNING: unknown image pair ID in IBIS file '
           call xvmessage(msgbuf,' ')
37         continue
           do i=1,tiep
              window(1,i)=winnl
              window(2,i)=winns
           enddo
           tiep=tiep+1
        endif

c 	READ GEOMA correction file.
        if(idgeom.gt.0)then
              call readGeoma( conv )
        endif

c 	SETUP SEDR for the latest image pair
        if(sedr) call setnav(pair,overlap,maxpix,omangl,rsvec,
     +      radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +      datal,datar)
     
c 	INIT GRAPHICS DISPLAY
        call init_display(vrdi_unit,bottom,right,graphicsplane)

c 	OPEN FIRST PAIR OF IMAGES
        call prnt(4,1,pair,'displaying image pair #.')
        left_image=fds(overlap(pair,1))
        right_image=fds(overlap(pair,2))
        call open_image_file(unit1,left_image,dirname,inst1,nl1,
     +              ns1,filename,name,maxpix)
        call open_image_file(unit2,right_image,dirname,inst2,nl2,
     +              ns2,filename,name,maxpix)

c jump to redisplay images AND graphics in case a tiepoints
c file has been input for continuation.
c       call xvpcnt('INP',count,' ')
        if(tiep.gt.1) then
		igoto33 = 1
		goto 333
	endif

c       Display first pair of images
        call clear_display(vrdi_unit,graphicsplane)
        call draw_image(unit1,vrdi_unit,sl1,ss1,bottom,right,
     +                 side1,buf1,buf2,nl1,ns1,zoom1,percent)
        call draw_image(unit2,vrdi_unit,sl2,ss2,bottom,right,
     +                 side2,buf1,buf2,nl2,ns2,zoom2,percent)
        call draw_controls(nl1,zoom1,bottom,right,vrdi_unit,
     +				lbox,imageplane)
       
c 	MAIN LOOP FOR ALL TIEPOINTS
333	do while (.true.)
	    if (igoto33.ne.0) then
		igoto33 = 0
		goto 33
	    endif

C ** <cr>  **  BEGIN INTERACTIVE PARAMETER PROCESSOR
30          call xvintract('CURSOR','MANMATCH')
            
c ** EXIT **          
            if(xviptst('EXIT')) goto 200 ! to WRITE OUT TIEPOINTS
            
c ** HELP **
            if(xviptst('HELP')) then
               
               call xvmessage( 
     +                  ' Note:  Keyword type parms require leading ',
     +                  ' ')
     	       call xvmessage( '        single quote mark',' ')
               call xvmessage(
     +               	'OPERATION: ',' ')
               call xvmessage(
     +			'<carriage-return>  manual tiepoint entry',' ')
               call xvmessage(
     +			'<1st button>    accept point, or, return',' ')
               call xvmessage(
     +               	'<2nd button>    switch to left image',' ')
               call xvmessage(
     +		        '<3rd button>    switch to right image',' ')
               call xvmessage(
     +		        'KEYWORDS: (numbers are program defaults)',' ')
               call xvmessage(
     +		        '  CORR=(64,64) nl,ns of correlation size',' ')
               call xvmessage(
     +		        '  NOCORR        deactivate correlation',' ')
               call xvmessage(
     +		        '  NEXT          display next image pair',' ')
               call xvmessage(
     +		        '  LAST          display last image pair',' ')
               call xvmessage(
     +			'  DELETE=n      n is a tiepoint to delete',' ')
c              call xvmessage('D1=n or D2=n move L or R pix down by n',' ') 
c              call xvmessage('U1=n or U2=n move L or R pix up by n',' ') 
               call xvmessage(
     +               	'  REDRAW        redraw tiepoints',' ')
               call xvmessage(
     +			'  PERCENT=2     auto stretch hist satur.',' ')
               call xvmessage(
     +               	'  PAIR=n        display image pair # n',' ')
               call xvmessage(
     +		        '  SEDR          activate navigation assist',
     +			' ')
               call xvmessage(
     +			'  NOSEDR        de-activate nav. assist',' ')
               call xvmessage(
     +          	'  EXIT          exit program',' ')
               goto 30
            endif

c ** PAIR **
            call  xviparm('PAIR',n,count,def,' ')
            if(count.gt.0)then
               
               if((n.le.noverlap).and.(n.ge.1))then
                  pair=n-1           ! We'll trick NEXT to do the job for us
                  igoto32 = 1        ! enable NEXT execution segment
                  goto 32
               else
                  call xvmessage(
     +			'PAIR out of range, request ignored',' ')
                  call prnt(4,1,noverlap,'max number of pairs=.')
                 
                  goto 30
               endif
            endif

c ** SEDR **
            if(xviptst('SEDR'))then
              if (SEDRead) then
                call setnav(pair,overlap,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
                sedr=.true.
                call xvmessage('SEDR navigation turned ON',' ')
              else
                call xvmessage (
     +               'Program was initiated in NOSEDR mode',' ')
                call xvmessage ('This option is disabled.',' ')
              end if
              goto 30
            endif               

c ** NOSEDR **
            if(xviptst('NOSEDR'))then
              sedr=.false.
              call  xvmessage('SEDR navigation turned OFF',' ')
              goto 30
            endif               

c ** NEXT **
32          if(xviptst('NEXT') .or. (igoto32.eq.1))then
	      if (igoto32.ne.0) igoto32 = 0
              if(pair+1.le.noverlap)then
               pair=pair+1
               call  prnt(4,1,pair,'New pair number=.')
               call  xvclose(unit1,status,' ')
               call  xvclose(unit2,status,' ')
               left_image=fds(overlap(pair,1))
               right_image=fds(overlap(pair,2))
               
               call open_image_file(unit1,left_image,dirname,inst1,
     +              nl1,ns1,filename,name,maxpix)
               call open_image_file(unit2,right_image,dirname,inst2,
     +              nl2,ns2,filename,name,maxpix)
               redoimg=.true.
c              setup SEDR for the next image pair
               if(sedr) call setnav(pair,overlap,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
               goto 33
              else
               call xvmessage('No more image pairs !, EXIT to get out'
     +              				,' ')
               goto 30
              endif
            endif

c ** LAST **
34          if(xviptst('LAST') .or. (igoto34.eq.1))then
  	      if (igoto34.ne.0) igoto34 = 0            
              if(pair-1.ge.1)then
               pair=pair-1
               call prnt(4,1,pair,'New pair number=.')
               call  xvclose(unit1,status,' ')
               call  xvclose(unit2,status,' ')
               left_image=fds(overlap(pair,1))
               right_image=fds(overlap(pair,2))
               call open_image_file(unit1,left_image,dirname,inst1,
     +              nl1,ns1,filename,name,maxpix)
               call open_image_file(unit2,right_image,dirname,inst2,
     +              nl2,ns2,filename,name,maxpix)
               redoimg=.true.
c              setup SEDR for the next image pair
               if(sedr) call setnav(pair,overlap,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
               goto 33
              else
               call xvmessage(
     +               'Are at first image pair, request ignored!'
     +               ,' ')
               goto 30
              endif
            endif

c ** PERCENT ** 
            call xviparm('PERCENT',percent,count,def,' ')
            if(count.eq.1) then                
            	redoimg=.true.
            endif

c ** NOCORR **
            if(xviptst('NOCORR')) then
               nocor=.true.
               ierr=0
               delline=0
               delsamp=0
            endif

c ** CORR **
            call  xviparm('CORR',corrsize,count,def,' ')
            if(count.gt.0) nocor=.false.
            if(count.gt.0) winnl=corrsize(1)
            if(count.gt.1) winns=corrsize(2)
            if(count.gt.0) goto 30
            
c ** DELETE ** 
            call  xviparm('DELETE',n,count,def,' ')
            if(count.eq.1)then
              
              if(n.gt.0.and.n.lt.tiep)then
                tiep=tiep-1
                if(n.lt.tiep)then
                   do j=n+1,tiep
                      do k=1,ncol
                         ibisbuf(j-1,k)=ibisbuf(j,k)
                      enddo       
                      window(1,j-1)=window(1,j)
                      window(2,j-1)=window(2,j)
                   enddo
                endif
              else
                call xvmessage(
     +             'Tiepoint out of range, request ignored'
     +		   ,' ')
              endif
cc              goto 36
	       redoimg=.true.
            endif

CC D1, D2, U1, U2 no long are working
cc ** D1 **
c            call  xviparm('D1',n,count,def,' ')
c            if(count.eq.1)then
c               sl1=sl1+n
c               if(sl1.gt.nl1-bottom+1) sl1=nl1-bottom+1
c               redoimg=.true.
c            endif
c
cc ** D2 ** 
c            call  xviparm('D2',n,count,def,' ')
c            if(count.eq.1)then
c               sl2=sl2+n
c               if(sl2.gt.nl2-bottom+1) sl2=nl2-bottom+1
c               redoimg=.true.
c            endif
c
cc ** U1 **
c            call  xviparm('U1',n,count,def,' ')
c            if(count.eq.1)then
c               sl1=sl1-n
c               if(sl1.lt.1) sl1=1
c               redoimg=.true.
c            endif
c
cc ** U2 **
c            call  xviparm('U2',n,count,def,' ')
c            if(count.eq.1)then
c               sl2=sl2-n
c               if(sl2.lt.1) sl2=1
c               redoimg=.true.
c            endif
            
c ** REDRAW **
            if(xviptst('REDRAW'))then 
            	redoimg=.true.    
            endif
                        
                     
c END INTERACTIVE PARAMETER PROCESSOR


c           redisplay image & graphics if requested.
33          continue
            if(redoimg)then
               call clear_display(vrdi_unit,graphicsplane)
               call xvmessage('redisplaying images',' ')
               call draw_image(unit1,vrdi_unit,sl1,ss1,bottom,right,
     +                        side1,buf1,buf2,nl1,ns1,zoom1,percent)
               call draw_image(unit2,vrdi_unit,sl2,ss2,bottom,right,
     +                        side2,buf1,buf2,nl2,ns2,zoom2,percent)
               call draw_controls(nl1,zoom1,bottom,right,         
     +                        vrdi_unit,lbox,imageplane)
               redoimg=.false.
               do n=1,tiep-1
                  if((nint(ibisbuf(n,1)).eq.overlap(pair,1)).and.
     +               (nint(ibisbuf(n,2)).eq.overlap(pair,2))) then
                     x1=ibisbuf(n,3)-window(1,n)/2+.5
                     x2=ibisbuf(n,4)-window(2,n)/2+.5
                     x3=ibisbuf(n,5)-window(1,n)/2+.5
                     x4=ibisbuf(n,6)-window(2,n)/2+.5
                     x5=window(1,n)
                     x6=window(2,n)
                     brite=255
                     call draw_tiep_squares(x1,x2,x3,x4,x5,x6,
     +                  sl1,nl1,sl2,nl2,vrdi_unit,brite,n,ss1,
     +                  ss2,zoom1,zoom2,right,imageplane)
                  endif
               enddo
               goto 31
            endif

c  BEGIN MANUAL TIEPOINT ACQUISITION MODE.

c           Enter interactive tiepoint selection process. 
31          continue
            if(sedr)then    ! use the sedr
               call tc4sedr(winsl1,winss1,winsl2,winss2,
     +         side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +         vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +         lbox,nextpair,datal,datar,conv,graphicsplane)
            else            ! no sedr used
               call tc4nosedr(winsl1,winss1,winsl2,winss2,
     +         side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +         vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +         lbox,nextpair,graphicsplane)
            endif
            if(nextpair.gt.0) then 	!cursor in control box "next pair"
		igoto32 = 1
		goto 32
            endif
            if(nextpair.lt.0) then   	!cursor in control box "last pair"
		igoto34 = 1
		goto 34
            endif
            if(side.eq.0) goto 30        !cursor in control box "parameter..."

c           Exit with a tiepoint candidate position.

c  END MANUAL TIEPOINT ACQUISITION MODE.
            if(nocor) goto 110

c           Compute polynomial mapping from left to right images.
c           for 4 corners of left correlation area.
            if(sedr)then
               call getmapping(datal,datar,conv,winsl1,winss1,winsl2,
     +                      winss2,winnl,winns,coefx,coefy,ierr)
               if(ierr.gt.0) goto 190
            endif

c           Read image areas around tiepoint for correlation purposes.
	    call extrct_tiep_imagechip (winsl1,winss1,buf1,winsl2,
     +                winss2,buf2,ierr,mean1,mean2,coefx,coefy,sedr)
            if(ierr.ne.0)then
               call xvmessage(' Pass#1 FFT area not within picture',' ')        
               goto 190
            endif

c           display projected portion of right image
c            call display_mapping(buf2,vrdi_unit,winnl,winns,bottom)

C           Correlate the two image chips
	    ncorr = ncorr + 1
	    call crosscorr_tiep (buf1,256,buf2,256,winnl,winns,7,
     +			.true.,hpf, phase, pfilter, offmax,
     +			bufout, dell, dels, corrval, ierr)
	    if (ierr .eq. 2) then
              call  xvmessage(' OFFMAX distance exceeded in pass#1',' ')
                goto 190                
            endif
            if(ierr.ne.0)then
                call  xvmessage(' error returned from crosscor, pass#1',
     +                        ' ')
                goto 190
            endif

            if(sedr)then
c             update mapping coefficients
              call mapping_shift(coefx,coefy,dell,dels,winsl1,winss1,
     +                           winnl,winns,cen_line,cen_samp)
            endif

	    if (redocorr.and.((dell.ne.0).or.(dels.ne.0)) ) then

		call extrct_tiep_imagechip (winsl1,winss1, buf1, 
     +		   winsl2+dell,winss2+dels, buf2,ierr,mean1,mean2,
     +             coefx,coefy,sedr)
                if(ierr.ne.0)then
                   call  xvmessage(' Pass#2 FFT area not within picture'
     +                           ,' ')
                   goto 190
                endif

c               display projected portion of right image
c               call display_mapping(buf2,vrdi_unit,winnl,winns,bottom)

		ncorr = ncorr + 1
		call crosscorr_tiep (buf1, i256, buf2,i256,winnl,
     +			winns,dcode7,.true.,
     +			hpf, phase, pfilter, offmax,
     +			bufout, dell2, dels2, corrval, ierr)
		if (ierr .eq. 0) then
		    if (abs(dell2) .gt. 2 .or. abs(dels2) .gt. 2) then
                      call xvmessage(' Pass#2 shift > 2 pixels',' ')
                      ierr=4
                      goto 190
   		    else
			dell = dell + dell2
			dels = dels + dels2
		    endif
		else if (ierr .eq. 2) then
                    call xvmessage(' OFFMAX distance exceeded in pass#2'
     +                            ,' ')
                    goto 190                
		else
                   call  xvmessage(
     +                   ' error returned from crosscor, pass#2',' ')
                   goto 190
		endif

                if(sedr)then
c                  update mapping coefficients
                   call mapping_shift(coefx,coefy,dell2,dels2,winsl1,
     +                           winss1,winnl,winns,cen_line,cen_samp)
                endif

	    endif


C	    Do subpixel interpolation if desired
	    delline = dell
	    delsamp = dels
	    if (subpix) then
		do i = 1, 3
		    do j = 1, 3
			l = winnl/2 + 1 + dell+i-2
			s = winns/2 + 1 + dels+j-2
			corrpatch(i,j) = bufout(s + winns*(l-1))
		    enddo
		enddo
		call refine (corrpatch, delline, delsamp, i190)
	    endif

C	    Stuff tiepoint in IBIS file array
110         window(1,tiep)=winnl
            window(2,tiep)=winns
	    ibisbuf(tiep,i1) = overlap(pair,i1)
	    ibisbuf(tiep,i2) = overlap(pair,2)
	    ibisbuf(tiep,i3) = float(winsl1+winnl/2)
	    ibisbuf(tiep,i4) = float(winss1+winns/2)
            if(sedr.and.(.not.nocor))then
              ibisbuf(tiep,i5) = cen_line
              ibisbuf(tiep,i6) = cen_samp
            else
              ibisbuf(tiep,i5) = float(winsl2+winnl/2) + delline
              ibisbuf(tiep,i6) = float(winss2+winns/2) + delsamp
            endif
	    ibisbuf(tiep,i7) = mean1
	    ibisbuf(tiep,i8) = mean2
	    ibisbuf(tiep,i9) = 1.0
            if(tiep.gt.maxpts-50)then
              call prnt(4,1,maxpts-tiep,'room for only #points=.')
            endif

C	    Print out info
	    if (.not. noprint) then
               call  xvmessage(
     +' left  right  left(line,samp) right(line,samp) left_dn  right_dn'
     +,' ')
		write (string, 11) (ibisbuf(tiep,k), k = 1, 8)
11		format (1x,2f5.0,1x,2f8.1,2x,2f8.1,2x,
     +                  2f8.2)
		call xvmessage (string,' ')
	    endif

	    if (ierr .eq. 0) then 
               tiep = tiep + 1

               brite=0
c              erase estimated tiepoint position from graphics                
               call draw_tiep_squares(winsl1,winss1,winsl2,winss2,winnl,
     +             winns,sl1,nl1,sl2,nl2,vrdi_unit,brite,i0,ss1,ss2,
     +             zoom1,zoom2,right,graphicsplane)

               if(sedr.and.(.not.nocor))then
                 winsl2=cen_line-winnl/2
                 winss2=cen_samp-winns/2
               else
                 winsl2=winsl2+delline
                 winss2=winss2+delsamp
               endif
               brite=255
c              redraw correlated tiepoint position
	       number=tiep-1
               call draw_tiep_squares(winsl1,winss1,winsl2,winss2,winnl,
     +             winns,sl1,nl1,sl2,nl2,vrdi_unit,brite,number,
     +             ss1,ss2,zoom1,zoom2,right,imageplane)

            else
               call xvmessage(' ERROR   not suppose to be here',' ')
            endif

c  	HANDLE FAILURE IN CORRELATION PROCESS

190	    continue
            if(ierr.ne.0)then
               ierr=0
               brite=0
c              Erase aborted attempt.
               call draw_tiep_squares(winsl1,winss1,winsl2,winss2,
     +             winnl,winns,sl1,nl1,sl2,nl2,vrdi_unit,brite,i0,
     +             ss1,ss2,zoom1,zoom2,right,graphicsplane)
             else
c              display final centered areas in input images
	       switch = 0
	       xnl1=nl1
	       xns1=ns1
	       xnl2=nl2
	       xns2=ns2
               call draw_tiep_image(unit1,unit2,vrdi_unit,winsl1,
     +             winss1,winsl2,winss2,xnl1,xns1,xnl2,xns2,bottom,
     +             right,winnl,winns,switch)
            endif


            delline=0.
            delsamp=0.

            if(nextpair.eq.0) goto 31   ! return for another point
                                        ! in same pair.

	enddo    ! return to parameter processor.
c  	END OF MAIN TIEPOINT GENERATION LOOP



c  	WRITE OUT TIEPOINTS INTO IBIS TABULAR FILE

200	continue
	clen = tiep - 1

	call xvclose (unit1, status,' ')
	call xvclose (unit2, status,' ')

	write (string, '(1x,a,i4)')  'number of correlations: ', ncorr
	call xvmessage (string,' ')

	if (clen .eq. 0) then
	    call xvmessage (' No tiepoints output',' ')
	else
		
	  write (string, '(1x,a,i4)')  'number of tiepoints: ', clen
	  call xvmessage (STRING,' ')
	
c	  Output array to IBIS file in col_ordr
	  typestr='IBIS file'
	  
	  call xvunit(ibisUnit,'OUT',1,status,' ')
	  call ibis_file_open(ibisUnit,ibis,'WRITE',ncol,clen,
     +                     ' ',' ',status)     
	  do col = 1, ncol
     		call ibis_column_write(ibis,ibisbuf(1,col),
     +     				col,1,clen,status)
	     	if (status  .ne.  1) 
     +	     		call ibis_signal_u(ibisUnit,status,0)    
	  enddo
	  call closeIbisFile(ibisUnit,ibis, status)
	endif
		
C	CLOSE GRAPHICS
	call close_display(vrdi_unit)

C	EXIT
	return
	end

c**************************************************************
c	NOT USED !?!
c**************************************************************
      subroutine display_mapping(buf2,vrdi_unit,winnl,winns,bottom)
      include 'fortport'
c displays projected right image correl patch
      byte bb(256)
      integer i2,lk
      real*4 buf2(256,256),mindn,maxdn
       integer winnl,winns,bottom,gstatus,imageplane
       integer   vrdi_unit
      integer xdilinewrite,xcoord
      data imageplane/1/
      

      mindn=1.e+20
      maxdn=-1.e+20
      do i=1,winnl
        do j=1,winns
           mindn=min(buf2(j,i),mindn)
           maxdn=max(buf2(j,i),maxdn)
        enddo
      enddo
      if(maxdn.eq.mindn) maxdn=maxdn+1
      slope=255./(maxdn-mindn)      
      offset=255.-slope*maxdn
      lk=bottom-winnl
      do i=1,winnl
        lk=lk+1
        do j=1,winns
           k=buf2(j,i)*slope+offset
           if(k.lt.0) k=0
           if(k.gt.255) k=255
           i2=k
           bb(j)=int2byte(i2)

        enddo
        xcoord = 2*(winns+2)
        gstatus=xdilinewrite(vrdi_unit,imageplane,xcoord,lk,winns,bb)
        if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdilinewrite error #.')
        endif
      enddo
      return
      end

c*****************************************************************
      subroutine mapping_shift(coefx,coefy,dell,dels,winsl1,winss1,
     +   winnl,winns,yright1,xright1)
c correct polynomial mapping from input to output and
c compute new output center from input center
      real*8 coefx(4),coefy(4),rl,rs
      integer winsl1,winss1,winnl,winns,dell,dels
      real*4 xright1,xright2, yright1, yright2

c update coefficients
      rl=winsl1+winnl/2
      rs=winss1+winns/2
      yright1=rs*coefy(1)+rl*coefy(2)+rl*rs*coefy(3)+coefy(4)
      xright1=rs*coefx(1)+rl*coefx(2)+rl*rs*coefx(3)+coefx(4)
      rl=rl+dell
      rs=rs+dels
      yright2=rs*coefy(1)+rl*coefy(2)+rl*rs*coefy(3)+coefy(4)
      xright2=rs*coefx(1)+rl*coefx(2)+rl*rs*coefx(3)+coefx(4)
      coefx(4)=coefx(4)+xright2-xright1
      coefy(4)=coefy(4)+yright2-yright1

c update output center
      rl=winsl1+winnl/2
      rs=winss1+winns/2
      yright1=rs*coefy(1)+rl*coefy(2)+rl*rs*coefy(3)+coefy(4)
      xright1=rs*coefx(1)+rl*coefx(2)+rl*rs*coefx(3)+coefx(4)
      return
      end

c ****************************************************************
      subroutine draw_tiep_image(unit1,unit,vrdi_unit,sl1,ss1,sl2,ss2,
     +                     nl1,ns1,nl2,ns2,bottom,right,
     +                     winnl,winns,switch)
      implicit integer(a-z)
      include 'fortport'
c display 2 correlated areas for verification.
      real*4 slope,offset
      
      integer*2 i2,maxdn,mindn, k,buf(256)
      integer  xcoord,imageplane
      integer switch,sl1,ss1,sl2,ss2,nl1,ns1,nl2,ns2,one,lk
      integer bottom,right,winnl,winns,unit1,unit,vrdi_unit
      integer j1,j2,j3,j4,j,knt,i
      byte bb(256),b
      data imageplane/1/, one/1/
      equivalence(i2,b)
      

c left image chip
      mindn=32767
      maxdn=-32767
      j1=max(one,sl1)
      j2=max(one,ss1)
      k=sl1+winnl-1
      j3=min(k,nl1)
      k=ns1-ss1
      j4=min(k,winns)

C     GET MAX AND MIN DN VALUES 
      do i=j1,j3
        call  xvread(unit1,buf,status,'LINE',i,'SAMP',j2,'NSAMPS',j4,
     +			' ')              
        do j=1,j4
           mindn=min(buf(j),mindn)
           maxdn=max(buf(j),maxdn)
        enddo
      enddo      
      if(maxdn.eq.mindn) then
          slope=1.0
          offset=0.0
      else
          slope=255./(real(maxdn)-real(mindn))      
          offset=255.-slope*maxdn
      endif
      lk=bottom-winnl
      
C     READ & DRAW TIEP IMAGE (1 LINE AT A TIME) 
      knt=0    
      do i=j1,j3
        knt=knt+1
        lk=lk+1
        call xvread(unit1,buf,status,'LINE',i,'SAMP',j2,'NSAMPS',j4,
     +        		' ')
        do j=1,j4
           k=buf(j)*slope+offset
           if(k.lt.0) k=0
           if(k.gt.255) k=255
           bb(j)=int2byte(k)
        enddo
        if(knt.eq.winnl/2)then
          b=bb(knt)  
           if(b.lt.128)then
              b=255
           else
              b=0
           endif
           bb(knt)=b
        endif
        xcoord = 1
        gstatus=xdilinewrite(vrdi_unit,imageplane,xcoord,
     +        				lk,winns,bb)
        if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdilinewrite error #.')
        endif
      enddo

      if(switch.eq.1)return

c right image chip
      mindn=32767
      maxdn=-32767
      j1=max(1,sl2)
      j2=max(1,ss2)
      k=sl2+winnl-1
      j3=min(k,nl2)
      k=ns2-ss2
      j4=min(k,winns)
      
      do i=j1,j3
        call  xvread(unit,buf,status,'LINE',i,'SAMP',j2,'NSAMPS',
     +        				j4,' ')
        do j=1,j4
           mindn=min(buf(j),mindn)
           maxdn=max(buf(j),maxdn)
        enddo
      enddo
      if(maxdn.eq.mindn) then
          slope=1.0
          offset=0.0
      else
          slope=255./(real(maxdn)-real(mindn))      
          offset=255.-slope*maxdn
      endif
      lk=bottom-winnl
      knt=0
      do i=j1,j3
        knt=knt+1
        lk=lk+1
        call  xvread(unit,buf,status,'LINE',i,'SAMP',j2,'NSAMPS',j4,
     +        					' ')
        do j=1,j4
           k=buf(j)*slope+offset
           if(k.lt.0) k=0
           if(k.gt.255) k=255
	   bb(j)=int2byte(k)
        enddo
        
         if(knt.eq.winnl/2)then
          b=bb(knt)  
           if(b.lt.128)then
              b=255
           else
              b=0
           endif
	   bb(knt)=b
        endif
        xcoord = 1
        gstatus=xdilinewrite(vrdi_unit,imageplane,
     +        			winns+2,lk,winns,bb)
        
        if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdilinewrite error #.')
        endif
      enddo
      return
      end
      
c**********************************************************************/
c*	close VRDI 								*/
c**********************************************************************/
	subroutine close_display(vrdi_unit)
	integer vrdi_unit
	integer gstatus, xddclose, xdglinit
	integer one
	data one/1/
	integer xcurs,ycurs

        xcurs=25
        ycurs=25 
        gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
        
c	INIT DEFAULT GRAPHICS FOR EXITING PROGRAM
	gstatus = xdglinit( vrdi_unit, one)	
	
c	CLOSE DISPLAY (VRDI) 
	gstatus = xddclose( vrdi_unit )
	
	return
	end
c**********************************************************************/
c*
c**********************************************************************/
        subroutine setnav(pair,overlap,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
c Fills the DATAL & DATAR for convev subroutine.
        real*8    r81,r82,r83,data8l(20),data8r(20)
        integer   pair,i1
        integer   maxpix
        integer   overlap(maxpix,2)
        real      omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real      focal(1),optaxl(1),optaxs(1)
        real 	  scale(1)
        real*4	  datal(40),datar(40)
        data	  i1/1/
        
c left image
        i=overlap(pair,1)
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8l)
        data8l(10)=rsvec(i,1)
        data8l(11)=rsvec(i,2)
        data8l(12)=rsvec(i,3)
        datal(25)=radpol(i)
        if(datal(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
c right image
        i=overlap(pair,2)
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8r)
        data8r(10)=rsvec(i,1)
        data8r(11)=rsvec(i,2)
        data8r(12)=rsvec(i,3)
        datar(25)=radpol(i)
        if(datar(25).eq.0.)call  xvmessage('WARNING: SEDR is blank',' ')
        datar(26)=eqpol(i)
        datar(27)=focal(i)
        datar(28)=optaxl(i)
        datar(29)=optaxs(i)
        datar(30)=scale(i)         
        return
        end

c *************************************************************
        subroutine getinput_id(idgeom,idibis)
c returns the INP # of the geom & tiepoints files if present
c in positions 3 & 4
        integer count,unit,status,idgeom,idibis
        character*4 fmt
        call  xvpcnt('INP',count,' ')
        idgeom=0
        idibis=0
        if(count.lt.3)return
        call  xvunit(unit,'INP',3,status,' ')
        call  xvopen(unit,status,'OPEN_ACT','AS','IO_ACT','AS',' ')
        call  xvget(unit,status,'NL',nl,'NS',ns,'FORMAT',fmt,' ')
        if((fmt.eq.'REAL').and.(ns.eq.900))then
           idgeom=3
        else if((fmt.eq.'BYTE').and.(ns.eq.512))then
           idibis=3
        else
           call  xvmessage('Input #3 is neither an IBIS or GEOMA file',
     +                   ' ')
           call abend
        endif
        if(count.eq.4)then
           if(idgeom.eq.3) idibis=4
           if(idibis.eq.3) idgeom=4
        endif
        call  xvclose(unit,status,' ')
        return
        end


c **************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(20)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c
	real*8	cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8	cos_kappa, sin_kappa,dtr

        dtr = 3.141592653589793D0/180.d0
	sin_alpha = sin(alpha*dtr)
	cos_alpha = cos(alpha*dtr)
	sin_delta = sin(delta*dtr)
	cos_delta = cos(delta*dtr)
	sin_kappa = sin(kappa*dtr)
	cos_kappa = cos(kappa*dtr)
	
c	c(1,1)= -sin_alpha * cos_kappa - cos_alpha * sin_delta  
c     +				* sin_kappa
c	c(1,2)= cos_alpha * cos_kappa - sin_alpha * sin_delta 
c     +				* sin_kappa
c	c(1,3) =  cos_delta * sin_kappa
c		c(2,1)= sin_alpha * sin_kappa- cos_alpha * sin_delta 
c     +				* cos_kappa
c	c(2,2)= -cos_alpha * sin_kappa- sin_alpha * sin_delta 
c     +				* cos_kappa
c	c(2,3) =  cos_delta * cos_kappa
c	c(3,1) =  cos_alpha * cos_delta
c	c(3,2) =  sin_alpha * cos_delta
c	c(3,3) =  sin_delta
	
	
	c(1)= -sin_alpha * cos_kappa - cos_alpha * sin_delta  
     +				* sin_kappa
	c(4)= cos_alpha * cos_kappa - sin_alpha * sin_delta 
     +				* sin_kappa
	c(7) =  cos_delta * sin_kappa
	c(2)= sin_alpha * sin_kappa- cos_alpha * sin_delta 
     +				* cos_kappa
	c(5)= -cos_alpha * sin_kappa- sin_alpha * sin_delta 
     +				* cos_kappa
	c(8) =  cos_delta * cos_kappa
	c(3) =  cos_alpha * cos_delta
	c(6) =  sin_alpha * cos_delta
	c(9) =  sin_delta
	
	
	return
	end


c *****************************************************************
c *
c *****************************************************************
      subroutine init_display(vrdi_unit,nl,ns,graphicsplane)

      integer xddopen,xddactivate,xddconfigure
      integer xdgconnect,xdcon,xdcautotrack
      integer xdlramp,xdlconnect,xdtsize,xdtfont
      integer xdtrotate,xddunit,xdeaction,xdglwrite,xdgon
      integer xdsgraph
      integer gconfig(4),four,one,zero,gstatus,six
      integer vrdi_unit,start,numbwords,textheight
      integer two,three,xddinfo
      integer gblue(256),ggreen(256),gred(256),info(80)
      integer graphicsplane,imageplane,ns,nl
      data imageplane/1/
      
      real*4 scale,rotate
      data gconfig/0,0,0,0/,one/1/,zero/0/
c      data gconfig/3,0,0,0/,one/1/,zero/0/
      data six/6/,rotate/0.0/
      data two/2/,three/3/,four/4/
      

cstruct color_struct {
c  char color[8];
c  int red;
c  int green;
c  int blue;
c} color_chart[18] = {
c  	"red",		255,0,0,   
c  	"green",	0,255,0,   
c  	"blue",		0,0,255,   
c  	"orange",	255,128,0,
c  	"yellow",	255,255,130,   
c  	"magenta",	255,0,255,  
c  	"cyan",		0,255,255,   
c  	"purple",	100,0,150,
c  	"brown",	150,100,50,   
c  	"pink",		255,80,120,   
c  	"ltgreen",	120,255,120,   
c  	"ltblue",	100,140,255,
c  	"violet",	160,80,255,   
c  	"grey",		128,128,128,   
c  	"black",	0,0,0,      
c  	"white",	255,255,255,
c  	"aqua",		0,255,200,   
c  	"melon",	255,150,120
c};

      gblue(1)=0
      ggreen(1)=0
      gred(1)=0
      gblue(256)=0
      ggreen(256)=0
      gred(256)=255

c set up display device
      gstatus=xdeaction(two,two,two)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdeaction error #.')
         call abend
      endif
      gstatus=xddunit(vrdi_unit)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xddunit error #.')
         call abend
      endif
      gstatus=xddopen(vrdi_unit)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xddopen error #.')
         call abend
      endif
      gstatus=xddactivate(vrdi_unit,.true.)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xddactivate error #.')
         call abend
      endif
      gstatus=xddconfigure(vrdi_unit,gconfig)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xddconfigure error #.')
         call abend
      endif
      start = 1
      numbwords = 80
      gstatus=xddinfo(vrdi_unit,start,numbwords,info)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xddinfo error #.')
         call abend
      endif
      nl=info(14)
      ns=info(15)
      graphicsplane = xdsgraph( vrdi_unit )
      if(graphicsplane.le.1)then
         call prnt(4,1,gstatus,'xdsgraph error #.')
         call abend
      endif
      gstatus=xdgconnect(vrdi_unit,graphicsplane,one,.false.)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdgconnect error #.')
         call abend
      endif
      gstatus=xdlconnect(vrdi_unit,imageplane,one,one,.false.)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlconnect error #.')
         call abend
      endif
      gstatus=xdlconnect(vrdi_unit,imageplane,two,one,.false.)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlconnect error #.')
         call abend
      endif
      gstatus=xdlconnect(vrdi_unit,imageplane,three,one,.false.)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlconnect error #.')
         call abend
      endif
      gstatus=xdlramp(vrdi_unit,one,one)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlramp error #.')
         call abend
      endif
      gstatus=xdlramp(vrdi_unit,two,one)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlramp error #.')
         call abend
      endif
      gstatus=xdlramp(vrdi_unit,three,one)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlramp error #.')
         call abend
      endif
      gstatus=xdglwrite(vrdi_unit,one,gred,ggreen,gblue)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdglwrite error #.')
         call abend
      endif
      gstatus=xdcon(vrdi_unit,one,one,zero)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdcon error #.')
         call abend
      endif
      gstatus=xdgon(vrdi_unit)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdgon error #.')
         call abend
      endif
      gstatus=xdcautotrack(vrdi_unit,one,one,.true.)  
	if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdcautotrack error #.')
         call abend
      endif
      gstatus=xdtfont(zero)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdtfont error #.')
         call abend
      endif
      gstatus=xdtrotate(rotate)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdtrotate error #.')
         call abend
      endif
      call xvparm('FONTHT',textheight,count,def,' ')
      call xvparm('FONTSC',scale,count,def,' ')
      gstatus=xdtsize(textheight,scale)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdtsize error #.')
         call abend
      endif      
      return
      end


c *******************************************************************
      subroutine clear_display(vrdi_unit,graphicsplane)
      integer zero,one,four
      integer xdifill,gstatus
      data zero/0/,one/1/,four/4/
      integer graphicsplane,imageplane,vrdi_unit
      data imageplane/1/
      
      call xddbatch(vrdi_unit,1)
      
      gstatus=xdifill(vrdi_unit,imageplane,zero)      
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdifill error #.')
c         call abend
      endif
      gstatus=xdifill(vrdi_unit,graphicsplane,zero)      
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdifill error #.')
c         call abend
      endif
      
      call xddbatch(vrdi_unit,0)
      
      return
      end

c *****************************************************************
      subroutine draw_controls(nl,zoom,bottom,right,vrdi_unit,
     +      		line,graphicsplane)
c draw control grid below left image
      integer zoom,bottom,right,xdipolyline
      integer   xbox(5),ybox(5)
      integer gstatus,xdttext,nl,xdtcolor
      integer  vrdi_unit
      integer graphicsplane, n, nlbox
      integer   i0,strlen,xcoord,ycoord1,ycoord2,loc,npts
      integer   one, line, imageplane, zero
      byte      i255 
      data i255/255/, loc/1/, npts/5/, i0/0/, one/1/, imageplane/1/
      data zero/0/

      nlbox=100
      line=nl/zoom+20
      if(line+nlbox.gt.bottom) nlbox=bottom-line
      m=line+nlbox/2
      n=right/6
      
      gstatus=xdtcolor(i255,zero)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdttext error #.')
c         call abend
      endif      
      call xddbatch(vrdi_unit,1)
      
c left area
      call get_square_coord(xbox,ybox,one,line,n,nlbox)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,
     +      				npts,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdipolyline error #.')
c         call abend
      endif
      strlen = 4
      xcoord = 10
      ycoord1 = m
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord1,
     +      				loc,strlen,'LAST')
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdttext error #.')
c         call abend
      endif
      ycoord2 = m+12
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord2,
     +      				loc,strlen,'PAIR')

c middle area
      call get_square_coord(xbox,ybox,n,line,n,nlbox)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,npts,
     +      				xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdipolyline error #.')
c         call abend
      endif
      xcoord = 10+n
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord1,
     +      				loc,strlen,'NEXT')
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdttext error #.')
c         call abend
      endif
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord2,
     +      				loc,strlen,'PAIR')

c right area
      xcoord = 2*n
      call get_square_coord(xbox,ybox,xcoord,line,n,nlbox)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,
     +      				npts,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdipolyline error #.')
c         call abend
      endif
      strlen = 9
      xcoord = 2*n + 5
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord1,
     +      				loc,strlen,'PARAMETER')
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,' xdttext error #.')
c         call abend
      endif
      gstatus=xdttext(vrdi_unit,graphicsplane,xcoord,ycoord2,
     +      			loc,strlen,'PROCESSOR')
     
     
      call xddbatch(vrdi_unit,0)
     
      return
      end

c ******************************************************************
      subroutine draw_tiep_squares(winsl1,winss1,winsl2,winss2,winnl,
     +             winns,sl1,nl1,sl2,nl2,vrdi_unit,
     +             brite,number,ss1,ss2,zoom1,zoom2,right,
     +		   graphicsplane)
      implicit integer (a-z)
      character*4 str
      character*1 bstr(4)
      integer xdipolyline,xdttext,xdtcolor
      integer   xcurs,ycurs,zero,gstatus,four
      integer   xbox(5),ybox(5),vrdi_unit
      integer  five,three,one,right
      byte      i255,brite
      data zero/0/,four/4/,five/5/,three/3/,one/1/
      data      i255/255/
      integer graphicsplane


      call xddbatch(vrdi_unit,1)

      gstatus=xdtcolor(brite,zero)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdtcolor error #.')
c         call abend
      endif
      write(str,12)number
12    format(i3)
      do i=1,4
      	  write(bstr(i),'(A)') str(i:i)
      enddo


c     DRAW/ERASE SIDE 1
      xcurs=(winss1-ss1+1)/zoom1
      ycurs=(winsl1-sl1+1)/zoom1
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +      				winns/zoom1,winnl/zoom1)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,brite,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif
      if (number.gt.0) then  !don't draw if being erased
         gstatus=xdttext(vrdi_unit,graphicsplane,xcurs,ycurs,one,
     +    			four,str)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdttext error #.')
c            call abend
         endif
      endif  
      

c     DRAW/ERASE SIDE 2
      xcurs=(winss2-ss2+1)/zoom2 + right/2
      ycurs=(winsl2-sl2+1)/zoom2
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +      				winns/zoom2,winnl/zoom2)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,brite,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif
      if (number.gt.0) then !don't draw if being erased
            gstatus=xdttext(vrdi_unit,graphicsplane,xcurs,ycurs,
     +				one,three,str)
            if(gstatus.ne.1)then
          	call prnt(4,1,gstatus,'xdttext error #.')
c         	call abend
      	    endif
      endif
      
      
      call xddbatch(vrdi_unit,0)
      
      return
      end


c ******************************************************************
      subroutine get_square_coord(xbox,ybox,xcurs,ycurs,winns,winnl)
      integer  xcurs,ycurs
      integer     xbox(5),ybox(5)
      integer winns,winnl
      xbox(1)=xcurs
      ybox(1)=ycurs
      xbox(2)=xcurs+winns-1
      ybox(2)=ycurs
      xbox(3)=xbox(2)
      ybox(3)=ycurs+winnl-1
      xbox(4)=xcurs
      ybox(4)=ybox(3)
      xbox(5)=xcurs
      ybox(5)=ycurs
      return
      end


c **********************************************************************
        subroutine draw_image(unit,vrdi_unit,sl,ss,bottom,right,side,
     +      buf,hist,nl,ns,izoom,percent)
        include 'fortport'
	
        integer gstatus,one,gleft,side
        integer unit,sl,ss,bottom,right,status,nl
        integer j,jj
        

        integer ns,xdilinewrite,hist(1),vrdi_unit
        integer i2
        integer*2 buf(1)
        byte bb(2048)
       data one/1/

        if(side.eq.1)then
           gleft=1
        else
           gleft=right/2+1
        endif

c set zoom factor
        izoom=max(nl/bottom+1,ns/(right/2)+1)

c compute histogram
        do i=1,32770
           hist(i)=0
        enddo
        do i=15,nl,izoom
           call  xvread(unit,buf,status,'LINE',i,' ')
           do j=15,ns,izoom
              if(buf(j).ge.0)then
                 hist(buf(j)+1)=hist(buf(j)+1) + 1
              endif
           enddo
        enddo

c compute percent count limit
        n=0
        do i=2,32768
           n=n+hist(i)
        enddo
        j=n*percent/100.

c compute the saturation points
        k=0
        do i=2,32768
           k=k+hist(i)
           if(k.gt.j)then
              imin=i-1
              goto 10
           endif
        enddo
10      k=0
        do i=32768,2,-1
           k=k+hist(i)
           if(k.gt.j)then
              imax=i-1
              goto 20
           endif
        enddo

c compute scales
20      if(imax.eq.imin)imax=imin+1
        slope=255./(imax-imin)
        offset=255.-slope*imax

c write display
        line=0
        do i=1,nl,izoom
           line=line+1
           call  xvread(unit,buf,status,'LINE',i,' ')
           jj=0
           do j=1,ns,izoom
             jj=jj+1
             k=buf(j)*slope+offset
             if(k.lt.0) k=0
             if(k.gt.255) k=255
             i2=k
             bb(jj)=int2byte(i2)
           enddo
           gstatus=xdilinewrite(vrdi_unit,one,gleft,line,jj,bb)
           if(gstatus.ne.1)then
              call prnt(4,1,gstatus,'xdilinewrite error #.')
c              call abend
           endif
        enddo
        return
        end

c ******************************************************************
        subroutine open_image_file(unit,fds,dirname,n,nl,ns,
     +              filenames,name,maxpix)
c to open frames whose filenames are FDS.img in directory DIRNAME.
c or whose names are in NAME(fds).
        integer unit,fds,status,nl,ns,n
        character*512  msgbuf
        character*255 dirname,filename,string,filenames,name(maxpix)
        character*1 st1(255),st2(255)

      if(filenames.eq.'NOFILE')then ! no file with names is present
        write(string,11) fds
11      format (i8)

c        write(st2,'(A)') string
        do i=1,255
	   write(st2(i),'(A)') string(i:i)
	enddo
	
c find start of ascii fds time
        do i=1,8
          if(st2(i).ne.' ')then
            k1=i
            goto 10
          endif
        enddo
10      filename(1:255)=dirname(1:255)

c find end of directory name
	do i=1,255
		write(st1(i),'(A)') filename(i:i)
	enddo
c	write(st1,'(A)') filename(1:255)
        k2=0
        do i=255,1,-1
          if(st1(i).ne.' ') then
            k2=i
            goto 20
          endif
        enddo

c move fds time to end of directoryname & add .IMG
20      do i=k1,8
          k2=k2+1
          st1(k2)=st2(i)
        enddo
        st1(k2+1)='.'
        st1(k2+2)='i'
        st1(k2+3)='m'
        st1(k2+4)='g'
C        write(filename,'(A)') st1(1:255)
	do i=1,255
	   write(filename(i:),'(A)') st1(i)
	enddo
      else
        filename=name(fds)
      endif
      
      msgbuf='Opening filename:  '
      write(msgbuf(19:),'(A)') filename
      call xvmessage(msgbuf,' ')

c open the file & return UNIT number & nl & ns   
        call  xvunit(unit,'OLD',n,status,'U_NAME',filename,' ')
        call  xvsignal(unit,status,.true.)
        call  xvopen(unit,status,'OPEN_ACT','SA','IO_ACT','SA',
     +              'OP','READ','U_FORMAT','HALF',' ')
        call xvsignal(unit,status,.true.)
        call xvget(unit,status,'NL',nl,'NS',ns,' ')
        call xvsignal(unit,status,.true.)
        return
        end


c *****************************************************************
C		Extract the image chips from each image
c *****************************************************************
	subroutine extrct_tiep_imagechip (sl1,ss1, buf1, sl2,ss2, 
     +                    buf2,ierr,mean1,mean2,coefx,coefy,sedr)
	implicit none
	integer	sl1, ss1, sl2, ss2, j
	real	buf1(256,256), buf2(256,256)
	real   xr(4),yr(4)
	integer	l, s, status,ierr,mean1,mean2,kl,ks
        integer k,itop,ibot,ilft,irt
        real*8 coefx(4),coefy(4),rl,rj
        real*4 xright,yright,r,wtop,wbot
C These will hold the total dn values of the patch before taking the
C average. Previously mean1 and mean2 were used but were not large enough -dpp
	real*4 tmp_mean1, tmp_mean2
        logical sedr
        integer*2 b(512)

        
C	Global variables
	integer	winnl, winns, unit1, unit2, func
	integer	nl1, ns1, nl2, ns2, bord, gridsize(2)
	real	xg(3,2)
        common /global/  winnl, winns, unit1, unit2, func,
     +		nl1, ns1, nl2, ns2, bord, gridsize, xg


c       check to assure the areas are within the data
        ierr=0
        if(ss1.lt.1)ierr=3
        if(sl1.lt.1)ierr=3
        if(ss2.lt.1)ierr=3
        if(sl2.lt.1)ierr=3
        if(ss1+winns-1.gt.ns1)ierr=3
        if(ss2+winns-1.gt.ns2)ierr=3
        if(sl1+winnl-1.gt.nl1)ierr=3
        if(sl2+winnl-1.gt.nl2)ierr=3
        if(ierr.ne.0)return
        tmp_mean1=0.0
        tmp_mean2=0.0

        if(sedr)then        ! perform local map2

c         compute the four corner points in the right image
          k=0
          do l=1,winnl,winnl-1
             rl=l+sl1-1
             do j=1,winns,winns-1
                k=k+1
                rj=j+ss1-1
                yr(k)=rj*coefy(1)+rl*coefy(2)+rl*rj*coefy(3)+coefy(4)
                xr(k)=rj*coefx(1)+rl*coefx(2)+rl*rj*coefx(3)+coefx(4)
                if((yr(k).gt.1.0).and.(yr(k).lt.nl2).and.
     +             (xr(k).gt.1.0).and.(xr(k).lt.ns2))then
                   ! ok
                else
                   ierr=3
                   return
                endif
             enddo
          enddo

c         Isolate the area to geom
          ilft=min(xr(1),xr(2),xr(3),xr(4))
          irt=max(xr(1),xr(2),xr(3),xr(4))+1
          itop=min(yr(1),yr(2),yr(3),yr(4))
          ibot =max(yr(1),yr(2),yr(3),yr(4))+1
          if((ibot-itop+1.gt.256).or.(irt-ilft+1.gt.256))then
             ierr=3
             return
          endif

c         read in the right block containing the data to geom
          k=0
          do l=itop,ibot
             call  xvread(unit2,b,status,'LINE',l,'SAMP',ilft,
     +                   'NSAMPS',irt-ilft+1,' ')
             k=k+1
             do j=1,irt-ilft+1
                buf1(j,k)=b(j)
             enddo
          enddo      

c         Geom the data from buf1 to buf2
          do l=1,winnl
            rl=l+sl1-1
            do j=1,winns
               rj=j+ss1-1
               yright=rj*coefy(1)+rl*coefy(2)+rl*rj*coefy(3)+coefy(4)
               xright=rj*coefx(1)+rl*coefx(2)+rl*rj*coefx(3)+coefx(4)
               kl=yright
               ks=xright
               r=xright-ks
               wtop=buf1(ks-ilft+2,kl-itop+1)*r +
     +              buf1(ks-ilft+1,kl-itop+1)*(1.-r)
               wbot=buf1(ks-ilft+2,kl-itop+2)*r +
     +              buf1(ks-ilft+1,kl-itop+2)*(1.-r)
               r=(yright-kl)*wbot+(kl+1-yright)*wtop
               buf2(j,l)=r
               tmp_mean2=tmp_mean2+r
            enddo
          enddo

        else                ! no projection
          do l=1,winnl
	    call  xvread (unit2, b, status, 'LINE',sl2+l-1,
     +			'SAMP',ss2, 'NSAMPS',winns,' ')
            do j=1,winns
               buf2(j,l)=b(j)
               tmp_mean2=tmp_mean2+b(j)
            enddo
          enddo
        endif

c read the left image area into buf1
	do l = 1, winnl
	    call  xvread (unit1, b, status, 'LINE',sl1+l-1,
     +			'SAMP',ss1, 'NSAMPS',winns,' ')
            do j=1,winns
               buf1(j,l)=b(j)
               tmp_mean1=tmp_mean1+b(j)
            enddo
        enddo


        mean1=tmp_mean1/(winnl*winns)
        mean2=tmp_mean2/(winnl*winns)

	if (func .eq. 1) then
	    do l = 1, winnl
		do s = 1, winns
		    buf1(s,l) = 46.*log(buf1(s,l)+1)
		    buf2(s,l) = 46.*log(buf2(s,l)+1)
		enddo
	    enddo
	else if (func .eq. 2) then
	    do l = 1, winnl
		do s = 1, winns
		    buf1(s,l) = exp(buf1(s,l)/46.)
		    buf2(s,l) = exp(buf2(s,l)/46.)
		enddo
	    enddo
	endif


	return
	end



C********************************************************************
      subroutine crosscorr_tiep( a,ia, b,ib, m,n, dcode, first, hpf,
     .                       phase, pfilter, offmax, c, il,is, qual, 
     .			     istatus )
C  NAME OF ROUTINE
C      CROSSCORR (CROSS-CORRelation)
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        6-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      FLOATA, FTCORR, MVE, QPRINT

      implicit none

C...ARGUMENT DECLARATIONS

      byte        a(*),b(*)
      integer*4   ia, ib, m, n, dcode, il,is, istatus
      logical*4   first, hpf
      real*4      phase, pfilter, offmax, c(n,m+2), qual

C...LOCAL DECLARATIONS

      integer*4   i,  iptra,   iptrb,   jptr,   lsizea,  lsizeb
      integer*4   mcode,       maxm_par,        mm2_par, pixsize(8)

      parameter   (maxm_par = 256)       ! max for m & n.  could be increased.
      parameter   (mm2_par  = maxm_par*(maxm_par+2))

      real*4      fa( mm2_par ), fb(mm2_par)
      save        fa                     ! fft of a slept here.

      data        pixsize  /  1, 2, 0, 4, 0, 0, 4, 8  /  ! bytes per pixel

C
C======================START OF EXECUTABLE CODE======================

      istatus = 0

C...CONVERT IMAGE DATA TO REAL*4 AND MOVE INTO FA & FB.

      lsizea = ia * pixsize(dcode)
      lsizeb = ib * pixsize(dcode)
      iptra  = 1
      iptrb  = 1
      jptr   = 1

      if ( n .ne. min(n,ia,ib))  then
        istatus = -1
        call  xvmessage(' ERROR IN CROSSCORR: INVALID N VALUE',' ')
        
      else if ( n .gt. maxm_par .or. m .gt. maxm_par)  then
        istatus = -1
        call  xvmessage(' ERROR IN CROSSCORR: INVALID M OR N VALUE',' ')
        
      else 
        if (dcode .eq. 1 .or. dcode .eq. 2 .or. dcode .eq. 4)  then
          do i = 1, m
            if (first)  then            ! move a line and convert to real*4.
               call floata( dcode, n, a(iptra), fa(jptr) )
               iptra = iptra + lsizea
            end if
            call floata( dcode, n, b(iptrb), fb(jptr) )
            iptrb = iptrb + lsizeb
            jptr = jptr + n
          end do
  
        else if (dcode .eq. 7 .or. dcode .eq. 8)  then
          if (dcode .eq. 7)  mcode = 7
          if (dcode .eq. 8)  mcode =-9
          do i = 1, m
            if (first)  then            ! move a line.
               call mve( mcode, n, a(iptra), fa(jptr), 1, 1 )
               iptra = iptra + lsizea
            end if
            call mve( mcode, n, b(iptrb), fb(jptr), 1, 1 )
            iptrb = iptrb + lsizeb
            jptr = jptr + n
          end do
  
        else
          istatus = -1
          call  xvmessage(' ERROR IN CROSSCORR: INVALID DCODE VALUE',
     +    			' ')                 
        end if
      end if

      if (istatus .eq. 0)
     .  call ftcorr( fa,fb, m,n, first, hpf, phase, pfilter, offmax,
     .               c, il,is, qual, istatus)

      return
      end


c **********************************************************************
      subroutine ftcorr( fa,fb, m,n, first, hpf, phase, pfilter, offmax,
     .                   c, il,is, qual, istatus)

C  NAME OF ROUTINE
C      FTCORR (Fourier Transform cross-CORRelation)
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        6-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  FTCORR descended from routine RFIT by K.F.Evans & routine FTCORR by L.W.Kamp.
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  SUBROUTINES CALLED
C      RFT2, QPRINT

      implicit none

C...ARGUMENT DECLARATIONS

      integer*4   m, n, il,is, istatus
      logical*4   first, hpf
      real*4      phase, pfilter, offmax, c(n,m+2), qual,
     .            fa(n,m+2), fb(n,m+2)

C...LOCAL DECLARATIONS

      integer*4   i,  j,  ixmax, jxmax,  mhalf, nhalf, rftstat
     

      real*4      dnorm,  filpow, power, powera, power1a,power1b,
     .            powerb, power2a, power2b,  filt, r2, y2, amp, ampp,
     .            cijr, ciji, vmax, t, rv
      save        powera
C
C======================START OF EXECUTABLE CODE======================

      istatus = 0
      dnorm   = max( m,n )
      dnorm   = 1./dnorm
C...CHECK THAT PARAMETERS ARE VALID.

      if ( mod(m,2) .ne. 0 .or. mod(n,2) .ne. 0 )     goto 8100
      if ( phase .lt. 0 .or. phase .gt. 1.0)          goto 8200
      if ( pfilter .lt. 0)                            goto 8300
      if ( offmax .le. 0 .or. offmax .gt. 1.0)        goto 8400

C...FFT the image areas

      if (first)  then
          call rft2(fa, m,n, 1, rftstat)
          if (rftstat.ne.1) goto 8100

C...Do radial filter for A area

	  if (pfilter .ne. 0.0) then

	    if (phase .eq. 1.0) then 
               filpow = pfilter             ! all for a and none for b.
            else
               filpow = pfilter/2.          ! half for a and half for b.
            end if

	    do i = 1, n
	      y2 = ( min(i-1, n-i+1)*dnorm )**2 
	      do j = 1, m+2, 2
	        r2 = y2 + ( ((j-1)/2)*dnorm )**2
                if (filpow .eq. 1.0)  then
                   filt = r2
                else if (filpow .eq. .5)  then
                   filt = sqrt(r2)
                else
                   filt = r2**filpow
                end if
	        fa(i,j) = filt*fa(i,j)
	        fa(i,j+1) = filt*fa(i,j+1)
	      enddo
	    enddo
	  endif

C...HANDLE FRACTIONAL PHASE VALUES.

          if (phase .gt. 0.0 .and. phase .lt. 1.0)  then
	    do j = 1, m+2, 2
	      do i = 1, n
	        amp = amax1( sqrt( fa(i,j)**2 + fa(i,j+1)**2), 1.e-12)
                if (phase .eq. .5)  then
                    ampp= 1./ sqrt(amp)
                else
                    ampp= 1./( amp**phase )
                end if
	        fa(i,j)   = ampp*fa(i,j)
	        fa(i,j+1) = ampp*fa(i,j+1)
	      enddo
	    enddo
	  endif

C		Zero first row and column if HPF

          if (hpf) then
            call zia( fa, 2*n)
            do j = 3,m+2
              fa(1,j) = 0.
            enddo
          endif

C		Calculate power for normalization of nophase

	  if (phase .ne. 1.0) then
	    fa(1,1) = 0.0               ! zero direct current (dc) term.
	    fa(1,2) = 0.0    ! in case not 0 because of rounding.
            power1a = 0.0
            power2a = 0.0
            do i = 1, n
      	      power1a = power1a + ( fa(i,1)**2 + fa(i,2)**2 )
      	      power1a = power1a + ( fa(i,m+1)**2 + fa(i,m+2)**2 )
              do j = 3, m, 2
	        power2a = power2a + ( fa(i,j)**2 + fa(i,j+1)**2 )
              enddo
	    enddo
	    powera = power1a + 2*power2a
	  endif
      endif

C...COMPUTE FFT OF B IMAGE AREA.

      call rft2(fb, m,n, 1, rftstat)
      if (rftstat.ne.1) goto 8100
		
C...If phase corr. multiply FFT's  & divide by the amplitudes.
 
      if (phase .eq. 1.0)  then
        do j = 1, m+1, 2
          do i = 1, n
            cijr = fa(i,j)*fb(i,j) + fa(i,j+1)*fb(i,j+1)
            ciji = fa(i,j)*fb(i,j+1) - fa(i,j+1)*fb(i,j)
            amp  = amax1( sqrt(cijr*cijr+ciji*ciji), 1.e-12)
            c(i,j)   = cijr/amp
            c(i,j+1) = ciji/amp
          enddo
        enddo

      else   ! otherwise we filter and compute normalization factor powerb
             ! and then multiply ffts.

C...Do radial filter for B area

	  if (pfilter .ne. 0.0) then
            filpow = pfilter/2.          ! half for a and half for b.

	    do i = 1, n
	      y2 = ( min(i-1, n-i+1)*dnorm )**2 
	      do j = 1, m+2, 2
	        r2 = y2 + ( ((j-1)/2)*dnorm )**2
                if (filpow .eq. 1.0)  then
                   filt = r2
                else if (filpow .eq. .5)  then
                   filt = sqrt(r2)
                else
                   filt = r2**filpow
                end if
	        fb(i,j) = filt*fb(i,j)
	        fb(i,j+1) = filt*fb(i,j+1)
	      enddo
	    enddo
	  endif

C...HANDLE FRACTIONAL PHASE VALUES.

          if (phase .gt. 0.0 .and. phase .lt. 1.0)  then
	    do j = 1, m+2, 2
	      do i = 1, n
	        amp = amax1( sqrt( fb(i,j)**2 + fb(i,j+1)**2), 1.e-12)
                if (phase .eq. .5)  then
                    ampp= 1./ sqrt(amp)
                else
                    ampp= 1./( amp**phase )
                end if
	        fb(i,j)   = ampp*fb(i,j)
	        fb(i,j+1) = ampp*fb(i,j+1)
	      enddo
	    enddo
	  endif

C		Zero first row and column if HPF

          if (hpf) then
            call zia( fb, 2*n)
            do j = 3,m+2
              fb(1,j) = 0.
            enddo
          endif

C		Calculate power for normalization.

	    fb(1,1) = 0.0               ! zero direct current (dc) term.
	    fb(1,2) = 0.0    ! in case not 0 because of rounding.
            power1b = 0.0
            power2b = 0.0
            do i = 1, n
      	      power1b = power1b + ( fb(i,1)**2 + fb(i,2)**2 )
      	      power1b = power1b + ( fb(i,m+1)**2 + fb(i,m+2)**2 )
              do j = 3, m, 2
	        power2b = power2b + ( fb(i,j)**2 + fb(i,j+1)**2 )
              enddo
	    enddo
	    powerb = power1b + 2*power2b

        do j = 1, m+1, 2
          do i = 1, n
            c(i,j)   = fa(i,j)*fb(i,j) + fa(i,j+1)*fb(i,j+1)
            c(i,j+1) = fa(i,j)*fb(i,j+1) - fa(i,j+1)*fb(i,j)
          enddo
        enddo

      endif
C		FFT back to the image domain and rearrange 
C			matrix to put the DC in center
      call rft2(c, m,n, -1, rftstat)
      if (rftstat.ne.1) goto 8100

      mhalf = m/2
      nhalf = n/2

      do j = 1, mhalf
        do i = 1, nhalf
          t = c(i,j)
          c(i,j) = c(i+nhalf,j+mhalf)
          c(i+nhalf,j+mhalf) = t
          t = c(i+nhalf,j)
          c(i+nhalf,j) = c(i,j+mhalf)
          c(i,j+mhalf) = t
        enddo
      enddo
C		Search for the correlation peak
      vmax = c(1+nhalf, 1+mhalf)            ! initialize.
      ixmax = 1 + nhalf
      jxmax = 1 + mhalf
      do j = 1, m
            do i = 1, n
               rv = c(i,j)
               if (vmax .lt. rv) then
                  vmax = rv
                  ixmax = i
                  jxmax = j
               endif
            enddo
      enddo
C		Normalize the correlation value

      if ( phase .eq. 1.0 )  then
         if (hpf)  then
            qual = vmax/ ( (m-1)*(n-1) )
         else
            qual = vmax/ (m*n)
         end if

      else
         power = sqrt(powera*powerb) 
         if (power .gt. 0.0)  then
            qual = vmax/power
         else
            qual = vmax
         end if
      endif
C		Calculate the offsets.

      il = jxmax - mhalf - 1
      is = ixmax - nhalf - 1

C...Test for correlation peak too near edge of matrix.

      if ( iabs(il) .gt. offmax*mhalf .or. iabs(is) .gt. offmax*nhalf )
     .     istatus = 2

7000  return

C...ERROR HANDLING

8100  continue
      istatus = -1
      call  xvmessage(' ERROR IN FTCORR: INVALID M OR N VALUE',' ')
      goto 7000

8200  continue
      istatus = -1
      call  xvmessage(' ERROR IN FTCORR: INVALID PHASE VALUE',' ')
      goto 7000

8300  continue
      istatus = -1
      call  xvmessage(' ERROR IN FTCORR: INVALID PFILTER VALUE',' ')
      goto 7000

8400  continue
      istatus = -1
      call  xvmessage(' ERROR IN FTCORR: INVALID OFFMAX VALUE',' ')
      goto 7000

      end


c **************************************************************
      subroutine refine(corr,vloff,vsoff,*)
      integer ipiv(6)
      real*4 corr(3,3),a(9,6),b(9),s(6),aux(12), eps
C
      do 1 i=1,3
      y = float(i)
      do 1 j=1,3
      x = float(j)
      iq = (i-1)*3+j
      a(iq,1) = x*x
      a(iq,2) = x*y
      a(iq,3) = y*y
      a(iq,4) = x
      a(iq,5) = y
      a(iq,6) = 1.
 1    b(iq) = corr(j,i)
      eps = 1.e-7
      call llsq(a,b,9,6,1,s,ipiv,eps,ier,aux)
      if (ier.ne.0) return 1
      if (s(1).eq.0.) return 1
      b2a = s(2)/(s(1)*2.)
      y0 = (b2a*s(4)-s(5))/(2.*s(3)-b2a*s(2))-2.
      x0 = -b2a*(y0+2.)-s(4)/(s(1)*2.)-2.
      if (x0*x0+y0*y0.ge.4.) return
      vloff = vloff+y0
      vsoff = vsoff+x0
      return
      end

c ********************************************************************
      subroutine getmapping(datal,datar,conv,winsl1,winss1,winsl2,
     +   winss2,winnl,winns,coefx,coefy,ind)

c returns the polynomial coefficients A-H which map the four corners
c of the left picture correlation area to the right image assuming
c the SEDR mapping AND corrects the mapping so that the initial
c tiepoint location indicated by the user is honored .
c the mapping is of the form:
c       yright=Ax+By+Cxy+D
c       xright=Ex+Fy+Gxy+H

      integer winsl1,winss1,winsl2,winss2,winnl,winns,i1,i2,i8,i16
      real*4 datal(40),datar(40)
      real*8 coefx(4),coefy(4),a(4,4),b(4,4)
      real*4 lat,lon,xcorner(4),ycorner(4), x, y
      integer   conv(3600), i4
      data   i1/1/,i2/2/,i4/4/,i8/8/,i16/16/

      ind=0

c locate central pixel
      xleft=winss1+winns/2
      yleft=winsl1+winnl/2
      xright=winss2+winns/2
      yright=winsl2+winnl/2

c specify corner pixels
      xcorner(1)=xleft-winns/2
      ycorner(1)=yleft-winnl/2
      xcorner(2)=xleft+winns/2
      ycorner(2)=yleft-winnl/2
      xcorner(3)=xleft-winns/2
      ycorner(3)=yleft+winnl/2
      xcorner(4)=xleft+winns/2
      ycorner(4)=yleft+winnl/2

c compute corner points in right image
      do i=1,4
         x=xcorner(i)
         y=ycorner(i)
         a(i,1)=x
         a(i,2)=y
         a(i,3)=dble(x)*dble(y)
         a(i,4)=1.d0
c        x,y -> lat,lon left image
         call convev(ind,datal,datal,y,x,lat,lon,i2,conv) 
         if(ind.ne.0)then
            call  xvmessage('CONVEV: corner point off planet, abort',
     +			' ')    
            return
         endif
c        lat,lon -> x,y right image
         call convev(ind,datar,datar,y,x,lat,lon,i1,conv) 
         if(ind.ne.0)then
            call  xvmessage('CONVEV: corner point off planet, abort',
     +       		' ')                   
            return
         endif
         coefy(i)=y
         coefx(i)=x
      enddo
      call mve(8,16,a,b,1,1)

c fit polynomial to corner mappings.
      call dsimq(a,coefx,4,ind)
      if(ind.ne.0)then
         call  xvmessage('DSIMQ: singular polynomial solution, abort',
     +                 ' ')
         return
      endif
      call dsimq(b,coefy,4,ind)
      if(ind.ne.0)then
         call  xvmessage('DSIMQ: singular polynomial solution, abort',
     +                 ' ')
         return
      endif
      
c compute right position of left center         
      x=xleft*coefx(1)+yleft*coefx(2)+dble(xleft)*dble(yleft)*
     +  coefx(3)+coefx(4)
      y=xleft*coefy(1)+yleft*coefy(2)+dble(xleft)*dble(yleft)*
     +  coefy(3)+coefy(4)

c correct offset so polynomial predicts the user specified mapping
c of the centers .
      coefx(4)=coefx(4)+xright-x
      coefy(4)=coefy(4)+yright-y
      return
      end

C*********************************************************************
C REPLACED previous version of dsimq with one that works properly. The old had 
C errors, and did not return the coefficients in B.   DPP
C*****************************************************************
      SUBROUTINE DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)

      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

c **************************************************************
c *
c **************************************************************
	subroutine  openIFile4Rd(unit,ibis, instance,nr,nc,failure,
     +					typestr, maxpix)		
	integer   	unit
	integer   	instance
	integer   	nr
	integer   	nc
	integer   	failure
	integer		maxpix
	character*20 	typestr

	integer status
	parameter (status=1)
	integer ibis
	character*4  name
	parameter(name='INP')
	
C	GLOBAL VARIABLES:
	integer	winnl, winns, unit1, unit2, func
	integer	nl1, ns1, nl2, ns2, bord, gridsize(2)
	real	xg(3,2)
	common /global/  winnl, winns, unit1, unit2, func,
     +		nl1, ns1, nl2, ns2, bord, gridsize, xg

C	INIT FILE
ccc	IMODE_READ='READ'
	call initIbisFile( unit, ibis, name,instance,'READ',nc,nr,
     +						failure,typestr,maxpix) 
	
		
	return
	end

c **************************************************************
c *
c **************************************************************
	subroutine initIbisFile(unit,ibis,name,instance,mode,
     +					nc,nr,failed,typestr,maxpix)
	integer 	unit
	integer		ibis
	integer 	failed
	integer		instance
	integer 	nc
	integer 	nr
	integer		i0,i1
	data		i0/0/,i1/1/
	integer		maxpix
	character*5     mode
	character*20 	typestr
	character*256	msgbuf
	character*4   	name
	integer 	status
	

C	GLOBAL VARIABLES:
	integer	winnl, winns, unit1, unit2, func
	integer	nl1, ns1, nl2, ns2, bord, gridsize(2)
	real	xg(3,2)
	common /global/  winnl, winns, unit1, unit2, func,
     +		nl1, ns1, nl2, ns2, bord, gridsize, xg

     
C	INIT VARIABLES
	status = 0
	unit = -1
	ibis = -1
				
C 	OPEN FILE
	call xvunit( unit, name, instance, status,' ')
	if ((status.ne.1).or.(unit .lt. 0).or.(unit.gt.maxpix)) then
	     msgbuf = 'Unable to open (xvunit)  '
	     write(msgbuf(24:),'(A)') typestr
	     call xvmessage(msgbuf,' ')
	     failed = 1
	else	
	
c	     GET SIZE INFO 
	     call ibis_file_open(unit,ibis,'READ',i0,i0,' ',
     +					     ' ',status)
     	     
	     if (status .ne. 1) then 
	     	msgbuf = 'Unable to open   '
	  	write(msgbuf(16:),'(A)') typestr
	  	call xvmessage(msgbuf,' ')
             	failed = 1
		call ibis_signal_u(unit, status, i0)
	     else
	       	count = ibis_file_get(ibis,'NC', nc, i1,i1)
 	        count = ibis_file_get(ibis,'NR', nr, i1,i1)
	     endif
	endif                      
		
	return	
	end

c **************************************************************
c *
c **************************************************************
	subroutine closeIbisFile(unit,ibis,status)
	character*256 msgbuf
	integer unit
	integer status
	integer ibis
	integer i0, i1
	data    i0/0/, i1/1/

	
C	GLOBAL VARIABLES:
	integer	winnl, winns, unit1, unit2, func
	integer	nl1, ns1, nl2, ns2, bord, gridsize(2)
	real	xg(3,2)
	common /global/  winnl, winns, unit1, unit2, func,
     +		nl1, ns1, nl2, ns2, bord, gridsize, xg
	

	call ibis_file_close( ibis, ' ', status)
	if (status .ne. 1) then
		msgbuf = 'Unable to close ibis file  '
	        call xvmessage(msgbuf,' ')
	        call ibis_signal(ibis,status,i1)
		
	endif

cc	call xvclose(unit, status)

	return 
	end

c **************************************************************
c *
c **************************************************************
	subroutine readGeoma( conv )
	   real    conv(3600)
	   logical xvptst
	   integer unit_geom, nlgeom, line
	
	       
           call xvunit(unit_geom,'INP',idgeom,status,' ')
           call xvopen(unit_geom,status,
     + 			'OPEN_ACT','AS','IO_ACT','AS',' ')
           call xvget(unit_geom,status,'NL',nlgeom,' ')         
           line=1
           call xvread(unit_geom,conv(1),status,'LINE',line,
     +                 'SAMP',2,'NSAMPS',899,' ')  
	   i=0
	  
           do line=2,nlgeom
              i=i+900
              call xvread(unit_geom,conv(i),status,'LINE',line,' ')
           enddo

            call xvclose(unit_geom,status,' ')
            call xvmessage(
     +		'GEOMA image space correction file read ok',' ')
            if(xvptst('OBJECT'))then
              call xvmessage('WARNING: geoma file will be ignored',' ')
                                         
           endif
	return 
	end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tc4sedr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c *******************************************************************
      subroutine tc4sedr(winsl1,winss1,winsl2,winss2,
     +   side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +   vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +   lbox,pair,datal,datar,conv,graphicsplane)
c SEDR assisted tiepoint acquisition.
c move correlation window & get cursor location.
c side=0 means first pass for new tiepoint
c side=1 left side
c side=2 right side

      integer zoom1,zoom2,vrdi_unit
      integer oldxcurs,oldycurs,xcurs,ycurs
      integer gstatus,pair,value,side
      integer xdclocation,xdipolyline,xdxswitch,xdcset
      integer winnl  			! winnl = x = 64,  from CORR(x,y)
      integer winns        		! winns = y = 64,  from CORR(x,y)	
      integer winsl1,winss1		! line/samp of 1st image
      integer winsl2,winss2		! line/samp of 2nd image
      integer sl1			! sl1 from U1 or D1
      integer sl2			! sl2  
      integer ss1,ss2			! ss1 = ss2 = 1
      integer bottom
      integer right			! 512=right side of window
      integer xcursr,ycursr,xboxr(5),yboxr(5)
      integer xbox(5),ybox(5)
      integer nr			! size of each control box
      real*4    lat,lon
      real*4    winsl, winss
      real*4   datal(40),datar(40)
      real   conv(1)
      
      integer two,three,four,five,zero,one
      byte     i255, i0
      data one/1/,four/4/,i255/255/,five/5/,zero/0/,two/2/
      data three/3/, i0/0/
      
      integer graphicsplane
      
      
c     SET CURSOR     
      pair=0
      side=0
      xcurs=right/4
      ycurs=(nl1/zoom1)/2
      oldxcurs=xcurs
      oldycurs=ycurs
      xcursr=xcurs+right/2
      ycursr=ycurs
      nr=right/6
      gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdcset error #.')
         call prnt(4,1,xcurs,'xcurs value=.')
         call prnt(4,1,ycurs,'ycurs value=.')
      endif
      
c     DRAW BOX IN LEFT IMAGE (ONLY)
      winsl1=ycurs*zoom1+sl1-1
      winss1=xcurs*zoom1+ss1-1
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +      			winns/zoom1,winnl/zoom2)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
      endif


c main loop
ccc30    call vwait(50)
30    continue
      value=0          

c     CHECK FOR QUIT CONDITION (left trackball button)
      gstatus=xdxswitch(vrdi_unit,one,one,value)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         if(side.ne.2)then
                  
c            gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,
c     +            			five,xbox,ybox)
            if(gstatus.ne.1)then
               call prnt(4,1,gstatus,'xdipolyline error #.')
            endif
         endif
         return
      endif


c     CHECK FOR LEFT SIDE CONDITION (left button)
      gstatus=xdxswitch(vrdi_unit,one,two,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         value=0
         side=1
         xcurs=(winss1-ss1+1)/zoom1			! new cursor location 
         ycurs=(winsl1-sl1+1)/zoom1			
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
        goto 30
      endif

c     CHECK FOR RIGHT SIDE CONDITION (right button)
      gstatus=xdxswitch(vrdi_unit,one,three,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
      endif
      if(value.eq.1)then
         value=0
         side=2
         xcurs=(winss2-ss2+1)/zoom2 + right/2
         ycurs=(winsl2-sl2+1)/zoom2
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         			winns/zoom2,winnl/zoom2)
         goto 30
      endif

c     GET NEW CURSOR LOCATION
      gstatus=xdclocation(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdlocation error #.')
      endif


c     RESTRICT CURSOR TO BE WITHIN CORRELATION/DISPLAY AREA
      if(side.eq.2)then   					! right side
         if(xcurs.lt.right/2) xcurs=right/2
         if(xcurs+(winns/zoom2)-1.gt.right)
     +        xcurs=right-(winns/zoom2)+1
      else							! left side
         if(xcurs.lt.1) xcurs=1
         if(xcurs+(winns/zoom1)-1.gt.right/2-1)
     +        xcurs=(right/2)-1-(winns/zoom1)+1
      endif

      if(xcurs.eq.oldxcurs.and.ycurs.eq.oldycurs) goto 30
      oldxcurs=xcurs
      oldycurs=ycurs

c     ERASE THE OLD BOX(s)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,
     +         			  xbox,ybox)
      if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
      endif
      if(side.ne.2) then    ! erase right box too
         call get_square_coord(xboxr,yboxr,xcursr,ycursr,
     +         			  winns/zoom2,winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,
     +         			  xboxr,yboxr)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
c            call abend
         endif
      endif

c     DRAW RIGHT BOX AT LATEST CURSOR LOCATION (if cursor in right)
      if(side.eq.2)then
         winsl2=ycurs*zoom2+sl2-1
         winss2=(xcurs-right/2)*zoom2+ss2-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,winns/zoom2,
     +         			winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,
     +         			xbox,ybox)
         if(gstatus.ne.1)then
            call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      else
         winsl1=ycurs*zoom1+sl1-1
         winss1=xcurs*zoom1+ss1-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         			winns/zoom1,winnl/zoom1)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,
     +         			xbox,ybox)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      endif


c     DRAW SEDR BOX ON RIGHT SIDE (IF CURSOR IN LEFT)         
      if(side.ne.2)then   
c        convert from line,samp to lat,lon in left image
         winsl=winsl1	!+(winnl/(2*zoom1))  
         winss=winss1	!+(winns/(2*zoom1))  

C        print *, 'RIGHT WINSL1 ', winsl1, ' WINSS1 ', winss1
         
         call convev(ind,datal,datal,winsl,winss,lat,lon,two,conv)
         if(ind.ne.0)then
            call xvmessage ('Point off planet', ' ')
            goto 31
         endif

c        convert lat,lon to line,samp in right image

         call convev(ind,datar,datar,winsl,winss,lat,lon,one,conv)
         if(ind.ne.0)then
            call xvmessage('                     Point off planet',' ')
            goto 31
         endif
         winsl2=nint(winsl)	!-(winnl/(2*zoom2))
         winss2=nint(winss)	!-(winns/(2*zoom2))
         
         xcursr=(winss2-ss2+1)/zoom2+(right/2)
         ycursr=(winsl2-sl2+1)/zoom2
         
         if(xcursr.lt.right/2) xcursr=right/2
         if(xcursr+winns/zoom2-1.gt.right)
     +        xcursr=right-winns/zoom2+1
         if(ycursr+winnl/zoom2-1.gt.bottom) 
     +        ycursr=bottom-winnl/zoom2+1
         if(ycursr.lt.1)ycursr=1
         winsl2=ycursr*zoom2+sl2-1
         winss2=(xcursr-right/2)*zoom2+ss2-1
C        print *, 'LEFT WINSL2 ', winsl2, ' WINSS2 ', winss2
         call get_square_coord(xboxr,yboxr,xcursr,ycursr,
     +         				winns/zoom2,winnl/zoom2)
         gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xboxr,
     +         				yboxr)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdipolyline error #.')
         endif
      endif


c     CHECK IF CURSOR IS IN ONE OF CONTROL AREAS BELOW IMAGES
31    continue
      if((ycurs.le.lbox).or.(xcurs.ge.3*nr)
     +    .or.(ycurs.gt.lbox+100) ) goto 30
      if(xcurs.lt.nr)then
         pair=-1
         return
      else if(xcurs.lt.2*nr)then
         pair=1
         return
      else
         side=0
         return
      endif

      goto 30
      end



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tc4nosedr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c *******************************************************************
      subroutine tc4nosedr(winsl1,winss1,winsl2,winss2,
     +   side,sl1,ss1,nl1,ns1,sl2,ss2,nl2,ns2,
     +   vrdi_unit,winnl,winns,bottom,right,zoom1,zoom2,
     +   lbox,pair,graphicsplane)
c move correlation window & get cursor location.
c side=0 means first pass for new tiepoint
c side=1 left side
c side=2 right side

      integer zoom1,zoom2
      integer four,five,zero,one,xcurs,ycurs
      integer oldxcurs,oldycurs,value
      integer two,three,gstatus,pair
      integer xdclocation,xdipolyline,xdxswitch,xdcset
      integer winnl,winns,winsl1,winss1,winsl2,winss2,side
      integer sl1,ss1,sl2,ss2,bottom,right
      integer   xbox(5),ybox(5),vrdi_unit
      byte      i255, i0
      data one/1/,four/4/,i255/255/,five/5/,zero/0/,two/2/
      data three/3/, i0/0/
      integer graphicsplane
    
     
      pair=0
      side=0
      nr=right/6
      xcurs=right/4
      ycurs=(nl1/zoom1)/2
      oldxcurs=xcurs
      oldycurs=ycurs
      gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdcset error #.')
          call prnt(4,1,xcurs,'xcurs value=.')
          call prnt(4,1,ycurs,'ycurs value=.')
c         call abend
      endif
      winsl1=ycurs*zoom1+sl1-1
      winss1=xcurs*zoom1+ss1-1
      call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif

      winsl2=(ycurs*zoom2)+sl2-1
      winss2=(xcurs*zoom2)+ss2-1
      winsl1=(ycurs*zoom1)+sl1-1
      winss1=(xcurs*zoom1)+ss1-1

c main loop
ccc 30    call vwait(100)

30	continue
      value=0          

c     Check for QUIT condition (left button)
      gstatus=xdxswitch(vrdi_unit,one,one,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         if(side.eq.0)then
            gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,
     +        				five,xbox,ybox)
            if(gstatus.ne.1)then
                call prnt(4,1,gstatus,'xdipolyline error #.')
c               call abend
            endif
         endif
         return
      endif


c     Check for LEFT side condition (left button)
      gstatus=xdxswitch(vrdi_unit,one,two,value)
      if(gstatus.ne.1)then
         call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         value=0
         side=1
         xcurs=(winss1-ss1+1)/zoom1
         ycurs=(winsl1-sl1+1)/zoom1
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
c            call abend
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
        goto 30
      endif

c     Check for RIGHT side condition (third button)
      gstatus=xdxswitch(vrdi_unit,one,three,value)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdxswitch error #.')
c         call abend
      endif
      if(value.eq.1)then
         value=0
         side=2
         xcurs=(winss2-ss2+1)/zoom2 + (right/2)
         ycurs=(winsl2-sl2+1)/zoom2
         gstatus=xdcset(vrdi_unit,one,xcurs,ycurs)
         if(gstatus.ne.1)then
             call prnt(4,1,gstatus,'xdcset error #.')
c            call abend
         endif
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom2,winnl/zoom2)
         goto 30
      endif

c read new cursor position.
      gstatus=xdclocation(vrdi_unit,one,xcurs,ycurs)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdlocation error #.')
c         call abend
      endif

c restrict correlation area to be within the display area.
      if(side.eq.2)then
         if(xcurs.lt.right/2) xcurs=right/2
         if(xcurs+(winns/zoom2)-1.gt.right)
     +        xcurs=right-(winns/zoom2)+1
      else
         if(xcurs.lt.1) xcurs=1
         if(xcurs+(winns/zoom1)-1.gt.right/2-1)
     +        xcurs=right/2-1-winns/zoom1+1
      endif
      if(ycurs+(winnl/zoom1)-1.gt.bottom) 
     +        ycurs=bottom-winnl/zoom1+1
      if(ycurs.lt.1)ycurs=1

      if(xcurs.eq.oldxcurs.and.ycurs.eq.oldycurs) goto 30
      oldxcurs=xcurs
      oldycurs=ycurs
c erase the old box location
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i0,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif
c update picture coordinates
      if(side.eq.2)then
         winsl2=ycurs*zoom2+sl2-1
         winss2=(xcurs-right/2)*zoom2+ss2-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +        				winns/zoom2,winnl/zoom2)
      else
         winsl1=ycurs*zoom1+sl1-1
         winss1=xcurs*zoom1+ss1-1
         call get_square_coord(xbox,ybox,xcurs,ycurs,
     +         				winns/zoom1,winnl/zoom1)
      endif
c draw in box
      gstatus=xdipolyline(vrdi_unit,graphicsplane,i255,five,xbox,ybox)
      if(gstatus.ne.1)then
          call prnt(4,1,gstatus,'xdipolyline error #.')
c         call abend
      endif

c Check if cursor is in one of the control areas below left picture.
      if((ycurs.le.lbox).or.(xcurs.ge.3*nr)
     +    .or.(ycurs.gt.lbox+100) ) goto 30
      if(xcurs.lt.nr)then
         pair=-1
         return
      else if(xcurs.lt.2*nr)then
         pair=1
         return
      else
         side=0
         return
      endif

      goto 30
      end


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create manmatch.imake
#define PROGRAM	manmatch
#define R2LIB

#define MODULE_LIST manmatch.f tc4sedr.f tc4nosedr.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport ibisfile ibiserrs
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_VRDI

/* #define LIB_LOCAL        /* Remove upon delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create manmatch.pdf
PROCESS HELP=*
SUBCMD-DEFAULT DUMMY 
 PARM INP      TYPE=STRING  COUNT=(2:4)
 PARM OUT      TYPE=STRING  COUNT=1
 PARM PROJECT  TYPE=(STRING,5) COUNT=(0:1) DEFAULT=-- +
    VALID=("VGR-1","VGR-2","MAR10","MAR-9","VIKOR","GLL  ","CASSI")
 PARM CORR     TYPE=INTEGER VALID=(16,32,64,128,256) COUNT=2 DEFAULT=(64,64)
 PARM PFILTER  TYPE=REAL    DEFAULT=0
 PARM PHASE    TYPE=REAL    DEFAULT=0
 PARM OFFMAX   TYPE=REAL    DEFAULT=0.8
 PARM FUNC     TYPE=(STRING,3) DEFAULT=""
 PARM REDOCORR TYPE=KEYWORD VALID=(REDO,NOREDO) DEFAULT=REDO
 PARM SUBPIX   TYPE=KEYWORD VALID=(SUBPIX,NOSUBPIX) DEFAULT=NOSUBPIX
 PARM PRINT    TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT
 PARM DIR      TYPE=(STRING,80) COUNT=1 DEFAULT=" "
 PARM OBJECT   TYPE=KEYWORD VALID=(IMAGE,OBJECT) DEFAULT=IMAGE
 PARM SEDR     TYPE=KEYWORD VALID=(SEDR,NOSEDR) DEF=SEDR
 PARM FILENAME TYPE=(STRING,80) COUNT=(0:1) DEFAULT="NOFILE"
 PARM FONTHT   TYPE=INTEGER COUNT=(0:1) DEFAULT=10
 PARM FONTSC   TYPE=REAL COUNT=(0:1) DEFAULT=1.0
END-SUBCMD
SUBCMD CURSOR
 PARM CORR TYPE=INTEGER COUNT=(0,2) VALID=(16,32,64,128,256) DEFAULT=--
 PARM DELETE TYPE=INTEGER COUNT=(0:1) VALID=(1:500) DEFAULT=--
 PARM EXIT TYPE=KEYWORD COUNT=(0:1) VALID=EXIT DEFAULT=--
 PARM REDRAW TYPE=KEYWORD COUNT=(0:1) VALID=REDRAW DEFAULT=--
 PARM HELP TYPE=KEYWORD COUNT=(0:1) VALID=(HELP) DEFAULT=--
 PARM NEXT TYPE=KEYWORD COUNT=(0:1) VALID=(NEXT) DEFAULT=--
 PARM LAST TYPE=KEYWORD COUNT=(0:1) VALID=(LAST) DEFAULT=--
 PARM PERCENT TYPE=REAL COUNT=(0:1) VALID=(0.:49.) DEFAULT=--
 PARM SEDR TYPE=KEYWORD COUNT=(0:1) VALID=(SEDR) DEFAULT=--
 PARM NOSEDR TYPE=KEYWORD COUNT=(0:1) VALID=(NOSEDR) DEFAULT=--
 PARM PAIR TYPE=INTEGER COUNT=(0:1) VALID=(1:200) DEFAULT=--
 PARM NOCORR TYPE=KEYWORD COUNT=(0:1) VALID=(NOCORR) DEFAULT=--
END-SUBCMD
END-PROC

.TITLE
VICAR/IBIS Program MANMATCH

.HELP
PURPOSE

MANMATCH or Manual Match, is a program which produces an IBIS format file
containing tiepoint information for the purpose of producing mosaics.  The 
program displays the a paired images with overlapping parts allowing the user 
to manually match the tiepoints (or similar pixels).

A more global view of the entire mosaicking process was documented in some
procedures in r3lib called MANUAL2.COM for Voyager, and MANUAL3.COM for Galileo. 
As of Aug.2005, these are no longer on the system.  However, extracts from them
have been included in this Help file.

.page
EXECUTION

To run without a SEDR:
   MANMATCH  inp=(sedr.int,over.int)  out=match.int 'nosedr

To run with a sedr on image space images:
   MANMATCH  inp=(sedr.int,over.int,geoma.cor)  out=match.int 
  (note that Galileo does not have a geoma correction file,
   you must instead specify project=gll).

or to continue from a partially complete tiepoint set:
   MANMATCH  inp=(sedr.int,over.int,match.int,geoma.cor) +  
             out=newmatch.int

EXAMPLE: manmatch inp=(jupsedr.int,jupover.int) out=jupmatch.int +
         dir=ud4:[jjl059.mosaic]
         Note: the images are in the directory ud4:[jjl059.mosaic]

.PAGE
The following procedure is a copy of MANUAL4.COM which shows how to
build a mosaic using manmatch. Refer directly to manual3 or manual4
rather than to this procedure because they may change.

procedure help=*
 REFGBL $ECHO
BODY
WRITE "This is a guide for masaicking GALILEO images "
WRITE "using OMCOR2 rather than OMCOR."
WRITE "To mosaic Voyager images refer to MANUAL2.COM."
WRITE "MANUAL4 MOSAICKING PROCEDURE (Things to do first)"
WRITE "(1)You must create a file with the editor containing"
WRITE "the full names of all the frames to be contained in"
WRITE "the mosaic, one filename per record."
WRITE "Ground control frame(s) must be at the END."
WRITE "Here we call this file LISTOFFILES."
WRITE "(2)YOU MUST ALLOCATE THE DISPLAY WITH  use  BEFORE STARTING"
WRITE "(3)To use this proc you must edit the following:"
WRITE "   -> The values assigned in the following LET commands."
WRITE "All plots will go to the printronix printer."

local (numpix,nlpix,nspix,camera,idfts) integer
local (latcen,loncen) real
local (planet,project) string
local return string initial=""

WRITE "nlpix=the number of lines in each picture."
WRITE "nspix=the number of samples in each picture."
WRITE "numpix=the number of pictures in the mosaic."
WRITE "planet=the target body."
let nlpix=800
let nspix=800
let numpix=2
let planet="venus"
let project="GLL"

plotting 'print

WRITE " "
WRITE "Create the ibis sedr file"
WRITE "Place the ground control image LAST"
WRITE "Replace the fds counts by those you are to use."
WRITE "List them in the order of priority, highest first."
ibisnav out=gasedr.int sedr=(fare,fare) +
         planet=&planet project=&project +
         filename=listoffiles.
WRITE " "
WRITE "Make first footprint plot to get center of projection"
WRITE "From the map select an approximate center of projection"
WRITE "and enter it in the next two prompts."
mosplot inp=gasedr.int nl=&nlpix ns=&nspix project=&project
dcl print/nofeed printronx.plt

WRITE " "
WRITE "Provide the latitude at the center of projection:"
getpar latcen
WRITE "Provide the longitude at the center of projection:"
getpar loncen

WRITE " "
WRITE "Make stereographic plot and create overlap file"
mosplot inp=gasedr.int out=gaover.int 'plot project=&project +
         nl=&nlpix ns=&nspix latlon=(&latcen,&loncen)
dcl print/nofeed printronx.plt

WRITE " "
WRITE "Delete unnecessary cross terms from the overlap file."
WRITE "Consult the footprint plot. In general each picture"
WRITE "should see the ones to left,right,top,& bottom but"
WRITE "needn't see more than those."
WRITE "You will be placed into program EDIBIS."
WRITE "To continue hit RETURN"
getpar return
edibis inp=gaover.int

WRITE " "
WRITE "Collect tiepoints MANUALLY using program MANMATCH."
WRITE "This is a time consuming interactive step. Try to read"
WRITE "the help file for MANMATCH before proceeding"
WRITE "To continue hit RETURN"
getpar return
manmatch inp=(gasedr.int,gaover.int) out=gamatch.int +
         filename=listoffiles. project=&project 'sedr 

WRITE "Plot tiepoints using old sedr."
WRITE "This is to see what residuals look like before"
WRITE "correction of the sedr."
mosplot inp=(gasedr.int,gamatch.int) project=&project +
         nl=&nlpix ns=&nspix exag=10. latlon=(&latcen,&loncen)
dcl print/nofeed printronx.plt

WRITE "Iteratively correct the ibis sedr file."
omcor2 inp=(gasedr.int,gamatch.int) project=GLL
omcor2 inp=(gasedr.int,gamatch.int) project=GLL

WRITE "Plot the tiepoints using the corrected sedr."
mosplot inp=(gasedr.int,gamatch.int) 'new +
         project=&project +
         nl=&nlpix ns=&nspix exag=100. latlon=(&latcen,&loncen)
dcl print/nofeed printronx.plt

WRITE "Correct the archival sedr from the ibis sedr."
ibisupdate inp=gasedr.int 'update planet=&planet +
         project=&project +

dcl del printronx.plt;*
end-proc


.PAGE
USAGE:
MANMATCH is an interactive program which has in the SEDR.INT file
the FDS or SCET times of all of the frames to inspect along
with all the navigation for these images.. 
You must create this file using the program IBISNAV
At the moment you must remember to use the single quote ' before all
single keywords. Thus 'HELP and 'EXIT is correct and HELP is incorrect.

Manmatch can obtain the names (and thus the locations) of all the
input files in one of two ways:

1. By the user specifying the parameter FILENAME which points to
   a file created by a text editor and which contains the full
   file names of each input picture in the order of priority, one
   name per record. This is the same file used in IBISNAV.
   So, if the first 2 pictures are called scx1:[jjl059]1234567.img
   and mipldisk:[mipl.gll]venus.img the first two records of
   FILENAME would contain:

   scx1:[jjl059]1234567.img
   mipldisk:[mipl.gll]venus.img

   This is the preferred usage.

2. By building each file name by appending to the DIR keyword
   the integer contained in column 1 of the SEDR.INT file followed
   by .IMG. You must in this mode name the input pictures
   accordingly. 
   For Voyager, column 1 is the FDS times thus the frames must be
   named the same as their FDS times; ie: 7654321.img is the file
   name of the image with fds time 7654321. 
   For Galileo the FDS times are just 1,2,3,4.. so the file names are
   1.img   2.img etc.
   So, if DIR=scx1:[jjl059] and SEDR.INT has fds time 1234567 in column
   one then the built-up name would be:  scx1:[jjl059]1234567.img

   This method is relatively obsolete.

The second input file 
contains the overlap frame pairs to present to the user.
This file is created by program MOSPLOT.

First input= SEDR.INT produced by program IBISNAV
Second input= OVER.INT produced by program MOSPLOT

When project is of Galileo, the camera s/n used to distinguish the mode of the
SSI camera is obtained from the first entry of the SEDR.INT file.  Thus, it is 
expected that the image files being processes are all taken by the same camera.

The program will attempt to present the user with each frame
overlap pair, one pair at a time. The user has control from 
the keyboard and from the trackball using 3 buttons. 
The object of the game is for the user to cursor two tiepoints
connecting each frame pair for all the frame pairs presented.
You can collect from none to 1000 points but two is sufficient.
There can be up to 200 frames and 1000 points total.

There are two modes:

MODE1 is the parameter processor. When in this mode you will 
have a prompt of the form: MANMATCH> . The program is waiting
for keyword entry. A carriage return or most keywords will
put you into MODE2. To get into MODE1 you can:
1. Depress the leftmost trackball button before depressing
   any of the others.  -or-
2  Move the cursor into the box labelled: PARAMETER PROCESSOR.

MODE2 is the tiepoint acquisition mode. To get into this mode depress the 
carriage return. When in MODE2 the cursor is active. You can move a rectangular
box the size of the correlation area over the image, called the correlation 
box. At first the box will remain in the left image area. After you have placed
the cursor over a likely region in the left image you can move to the right 
image by depressing the right most track ball button. Toggling the middle or 
right button switches attention between left & right image areas.  To enter a 
tiepoint THEN depress the left button. If you depress the left button BEFORE 
the middle or right button you are returned to the parameter processor. You can
advance one frame pair or revisit one frame pair by moving the cursor to within
the boxes indicating: NEXT or LAST PAIR.

When the last pair has been reached you will return to
the parameter processor. To exit type: 'EXIT.
Interactive parameters are listed with: 'HELP.

The program performs correlations to assist in precision. The
default size is: 64 by 64.

.page
SEDR PARAMETER

If the SEDR=SEDR is specified, then the program will read in the SEDR 
information from the first file to help the user visually locate the
overlapping coordinates of the paired image when in tiepoint acquisition mode.
This assistance is done by synchronizing the movement of two tiepoint boxes
on the left and right side of the image.  

Occasionally the SEDR is bad and the synchronization would result in nothing 
but annoyance.  In such case, the user may initiate the program with 
SEDR=NOSEDR to disable the SEDR control for the whole interactive session.
Another way is to temporarily disable SEDR control with the 'NOSEDR in the
parameter processor mode.

.PAGE
FILE STRUCTURE:
  The tiepoints file is an IBIS format tabular file. It consists
of 512 byte records where each column of data is written as
sequential records until exhausted. The next column begins at the
start of the next record etc. Record #1 contains the number of points
per column. All data is real*4 binary.

.page
Revision History:

Who When       What
--- ---------- ----------------------------------------------------------------
SMC 09/27/1996 Updated CONVEV call to reflect (FR89818)
SMC 09/27/1996 Make 'SEDR command reload the SEDR information.  (FR89849)
SMC 08/05/1996 FR 89352
               Disable 'SEDR in parameter processor initiated with 'NOSEDR
               Took out DEVICE parameter
SMC 10/22/1996 * Updated VALID input list for 2 parameter CORRs to match
               * Took out parameters D1, D2, U1, and U2              (FR89888)
SMC 10/22/1996 Make SEDR information be reloaded when 'SEDR command  (FR89849)
SMC 10/22/1996 Updated CONVEV calling sequence so that Summation Mode is
                 supported.                           (FR89818)
SMC 10/22/1996 Use MVCL to write 'GLL  ' to CONV instead of MVE    (FR89353)
LWK 05aug2005  Fixed some errors in the Help text.  Closed ARs 9037 and 9038
               because they were not reproducible and predated above changes
               by SMC.

.LEVEL1
.VARIABLE INP
Batch parameter !
All inputs are IBIS tabular files.

First input= SEDR.INT produced
 by program IBISNAV
Second input= OVER.INT produced
 by program MOSPLOT
Third or fourth input=
An old tiepoints file 
Third or fourth input=
A GEOMA distortion file.

.VARIABLE OUT
Batch parameter !
The output IBIS tabular file.
nine columns output:
col1=first frame number of a pair
col2=second frame number of a pair
col3=first frame tiepoint line
col4=first frame tiepoint sample
col5=second frame tiepoint line
col6=second frame tiepoint sample
col7=first tiepoint DN value
col8=second tiepoint DN value
col9=correlation value (1.0)

.VARIABLE HELP
List the interactive parameters 
available in a session.

.VARIABLE PROJECT
Batch parameter !
Specifies  the 
project. 

.VARIABLE SEDR
Batch parameter !
Causes navigation assisted
tiepoint acquisition.
Requires IBISNAV
sedr ibis file.

.VARIABLE OBJECT
Batch parameter !
Causes images to be 
considered geometrically
corrected.

.VARIABLE PFILTER
Batch parameter !
The power of the FFT high pass
power law filter. Default 
is no power filter.
default is: pfilter=0.0 real

.VARIABLE PHASE
Batch parameter !
Degree of phase correlation:
0 for normal correlation. ( Default ).
1 for phase correlation

.VARIABLE OFFMAX
Batch parameter !
How far off center, correlation
peak is allowed (0 to 1). 
If the peak is more than
OFFMAX*window/2 pixels from 
the center it is rejected.
Default is 0.8

.VARIABLE FUNC
Batch parameter !
Pixel function before 
correlation is performed..
The options are: LOG or EXP.
DEfault is no funtion performed.

.VARIABLE REDO
Batch parameter !
Keyword for redo mode. 
Valid responses are:
REDO or NOREDO. 
The default is REDO.
Redo specifies that if 
the first correlation for
an area has an offset of 
less than the OFFMAX limit
but greater than zero 
the correlation is rerun
beginning at the new location.

.VARIABLE PRINT
Batch parameter !
'NOPRINT to turn off 
terminal output

.VARIABLE DIR
Batch parameter.
Specifies the directory 
name where all the 
input files for the
mosaic reside.

.VARIABLE FILENAME
Batch parameter.
Specifies a file name 
which contains the 
names of all the
images to be included 
in the mosaic.

.VARIABLE FONTHT
Batch parameter.
Specifies Font height
of text displayed in 
image window. 

.VARIABLE FONTSC
Batch parameter.
Specifies Font scale
of text displayed in 
image window. This
is the horizonal
scaling factor (width/height)
to be used when drawing text.
 
.VARIABLE EXIT
Interactive parameter !
To exit from the program 
in interactive mode.

.VARIABLE CORR
Interactive & batch parameter !
The number of lines and samples
in the correlation window. 
Defaults to 
64 lines and 64 samples.
Valid values are: 16 32 64 128 256.
Default is: corr=(64,64)

.VARIABLE DELETE
Interactive parameter !
To delete tiepoint #n specify:
delete=n

.VARIABLE REDRAW
Interactive parameter !
To redraw all the acquired 
tiepoints.

.VARIABLE NEXT
Interactive parameter
To select the next frame pair.

.VARIABLE LAST
Interactive parameter
To select the last frame pair.

.VARIABLE PERCENT
Interactive parameter
Specifies the percentage of the
histogram to saturate at each
end for autostretching of the
displayed images.
Default=2.0

.VARIABLE PAIR
Interactive parameter !
Forces the image pair 
specified to
be displayed and to become
 the current
pair.

.VARIABLE NOSEDR
Interactive parameter !
nosedr causes the 
SEDR navigation to
be turned off 
during interactive
session. 

.VARIABLE SEDR
Interactive parameter !
sedr causes the SEDR 
navigation to
be turned back on 
during interactive
session. See 'NOSEDR.

.VARIABLE NOCORR
Interactive parameter
Turns off correlation.
To reactivate use CORR.

.LEVEL2
.VARIABLE INP
Batch parameter !
All inputs are IBIS tabular files.

First input= SEDR.INT produced by program IBISNAV
  The first column contains the FDS times of the frames to mosaic.
  It is assumed that the file names of the pictures to mosaic are
  named by their FDS times with a .IMG trailer.
  For example: 564329.img

Second input= OVER.INT produced by program MOSPLOT.
  Columns 1 and 2 contain the frame overlap numbers.
  Frame pairs are selected from the overlap numbers only.

An old tiepoints file can be entered as a third or fourth input if you
wish to continue where you left off the last time. (optional)

A geom correction file can be entered as the third or fourth input
if the images are in image space (not geometrically corrected).

.VARIABLE OUT
Batch parameter !
The output IBIS tabular file.
nine columns output:
col1=first frame number of a pair
col2=second frame number of a pair
col3=first frame tiepoint line
col4=first frame tiepoint sample
col5=second frame tiepoint line
col6=second frame tiepoint sample
col7=first tiepoint DN value
col8=second tiepoint DN value
col9=correlation value (1.0)

(The file actually contains 12 columns, but the last three are
always zero.)

The contents of this file can be listed with program IBIS-LIST.

.VARIABLE HELP
List the interactive parameters available in a session.

.VARIABLE PROJECT
Batch parameter !
Specifies the project . This is required if
the images are un-geometrically corrected
and the project is GLL.

.VARIABLE SEDR
Batch parameter !
Causes navigation assisted tiepoint acquisition.
Requires VGRIBIS sedr ibis file rather than the
file generated by automosaicking procedure.
If the images are NOT geometrically corrected
then you MUST include a GEOMA correction file
as the third or fourth input.

.VARIABLE OBJECT
Batch parameter !
Causes images to be considered geometrically corrected.
The geoma file will be ignored if present.

.VARIABLE PFILTER
Batch parameter !
The power of the FFT high pass
power law filter. Default is no power filter.
default is: pfilter=0.0 real

.VARIABLE PHASE
Batch parameter !
Degree of phase correlation:
0 for normal correlation. ( Default ).
1 for phase correlation

.VARIABLE OFFMAX
Batch parameter !
How far off center, correlation
peak is allowed (0 to 1). If the peak is more than
OFFMAX*window/2 pixels from the center it is rejected.
Default is 0.8

.VARIABLE FUNC
Batch parameter !
Pixel function before correlation is performed..
The options are: LOG or EXP.
DEfault is no funtion performed.

.VARIABLE REDO
Batch parameter !
Keyword for redo mode. Valid responses are:
REDO or NOREDO. The default is REDO.
Redo specifies that if the first correlation for
an area has an offset of less than the OFFMAX limit
but greater than zero the correlation is rerun
beginning at the new location.

.VARIABLE PRINT
Batch parameter !
'NOPRINT to turn off terminal output

.VARIABLE DIR
Batch parameter.
Optional parameter required only if the FILENAME parameter
is NOT specified. 
Specifies the directory name where all the input files for the
mosaic reside.

.VARIABLE FILENAME
Batch parameter.
Optional parameter required only if the DIR keyword is not
specified.
Specifies a file name which contains the names of all the
images to be included in the mosaic, one name per record.
This file is created by the user with a text editor.
For example, if the first 2 pictures are called 
scx1:[jjl059]1234567.img and mipldisk:[mipl.gll]venus.img 
the first two records of FILENAME would contain:

scx1:[jjl059]1234567.img
mipldisk:[mipl.gll]venus.img 

.VARIABLE FONTHT
Batch parameter. Specifies Font height in 
pixels of text displayed in (VRDI) image window. 

.VARIABLE FONTSC
Batch parameter. Specifies Font scale
of text displayed in (VRDI) image window. This
is the horizonal scaling factor (width/height)
to be used when drawing text.
 
.VARIABLE EXIT
Interactive parameter !
To exit from the program in interactive mode.

.VARIABLE CORR
Interactive & batch parameter !
The number of lines and samples
in the correlation window. Defaults to 
64 lines and 64 samples.
Valid values are: 16 32 64 128 256.
Default is: corr=(64,64)

.VARIABLE PAIR
Interactive parameter !
Forces the image pair specified to
be displayed and to become the current
pair. Do not use routinely - keep
the images in order if possible.
More for diagnostic use.

.VARIABLE NOSEDR
Interactive parameter !
nosedr causes the SEDR navigation to
be turned off during interactive
session. Useful when some of the SEDR
is bogus. Can turn back on with 'SEDR.

.VARIABLE SEDR
Interactive parameter !
sedr causes the SEDR navigation to
be turned back on during interactive
session. See 'NOSEDR.
WARNING: To reactivate the sedr the
program must have been given a sedr
ibis file to begin with.

.VARIABLE DELETE
Interactive parameter !
To delete tiepoint #n specify:
delete=n

.VARIABLE REDRAW
Interactive parameter !
To redraw all the acquired tiepoints.

.VARIABLE NEXT
Interactive parameter
To select the next frame pair.

.VARIABLE LAST
Interactive parameter
To select the last frame pair.

.VARIABLE PERCENT
Interactive parameter
Specifies the percentage of the
histogram to saturate at each
end for autostretching of the
displayed images.
Default=2.0

.VARIABLE NOCORR
Interactive parameter
Turns off correlation. In this mode the position of the cursor is
accepted verbatim. To reactivate use CORR.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmanmatch.pdf
procedure
  refgbl $echo
  refgbl $SysChar
body

  Local CASDir Type=String
  Local GLLDir Type=String
  Local VGRDir Type=String
  Local SysPath Type=String

  let $echo="no"
  If ($SysChar(1)="VAX_VMS")
    Let CASDir="wms_test_work:[testdata.cassini.cas$i$ss]"
    Let GLLDir="wms_test_work:[testdata.gll]"
    Let VGRDir="wms_test_work:[testdata.vgr]"
    Let SysPath = "vms"
  Else
    Let CASDir="/project/test_work/testdata/cassini/casIss/"
    Let GLLDir="/project/test_work/testdata/gll/"
    Let VGRDir="/project/test_work/testdata/vgr/"
    Let SysPath = "unix"
  End-If

!  write "This test PDF file will produce 2 IBIS file on the current directory."
!  write "The files will be named moon.tiep and red.tiep"

!  write "===For Galileo, Summation Mode"
!  write "===Please interactively select tie points with the mouse"
!  let $echo="yes"
! MANMATCH inp=(&"GLLDir"summ.sedr, &"GLLDir"summ.over) +
!          out=summ.tiep project=GLL file=&"SysPath"_summ.list
! IBIS-LIST summ.tiep
  let $echo="no"

  write "===For Galileo, Full Frame"
  write "===Please interactively select tie points with the mouse"
  let $echo="yes"
 MANMATCH inp=(&"GLLDir"moon.sedr, &"GLLDir"moon.over) +
          out=moon.tiep 'OBJECT project=GLL file=&"SysPath"_moon.list
 IBIS-LIST moon.tiep 
  let $echo="no"

  write "===For Voyager 2"
  write "===Please interactively select tie pointes with the mouse"
  let $echo="yes"
 MANMATCH inp=(&"VGRDir"red.sedr, &"VGRDir"red.over) project=VGR-2 'object +
               out=red.tiep 'sedr file=&"SysPath"_red.list
 IBIS-LIST red.tiep  
  let $echo="no"

  write "===For Cassini"
  write "===Please interactively select tie pointes with the mouse"
  let $echo="yes"
 MANMATCH inp=(&"CASDir"cassedr.int, &"CASDir"casover.int) +
          out=casmatch.int 'OBJECT project=cassi file=casimglist.&"SysPath"
 IBIS-LIST casmatch.int 
end-proc
$!-----------------------------------------------------------------------------
$ create unix_moon.list
/project/test_work/testdata/gll/7500.spk
/project/test_work/testdata/gll/8000.spk
/project/test_work/testdata/gll/8500.spk
$!-----------------------------------------------------------------------------
$ create unix_red.list
/project/test_work/testdata/vgr/2063121.geom 
/project/test_work/testdata/vgr/2063131.geom
/project/test_work/testdata/vgr/2063105.geom 
/project/test_work/testdata/vgr/2063123.geom
/project/test_work/testdata/vgr/2063113.geom 
/project/test_work/testdata/vgr/2063103.geom
$!-----------------------------------------------------------------------------
$ create unix_summ.list
/project/gll/ssi/udr/s0349632200.1
/project/gll/ssi/udr/s0349632100.1
/project/gll/ssi/udr/s0349632122.1
/project/gll/ssi/udr/s0349632145.7
$!-----------------------------------------------------------------------------
$ create casimglist.unix
/project/test_work/testdata/cassini/casIss/n1355064875.1
/project/test_work/testdata/cassini/casIss/n1355065283.1
/project/test_work/testdata/cassini/casIss/n1355065703.1
/project/test_work/testdata/cassini/casIss/n1355066109.1
$!-----------------------------------------------------------------------------
$ create vms_moon.list
wms_test_work:[testdata.gll]7500.spk
wms_test_work:[testdata.gll]8000.spk
wms_test_work:[testdata.gll]8500.spk
$!-----------------------------------------------------------------------------
$ create vms_red.list
wms_test_work:[testdata.vgr]2063121.geom 
wms_test_work:[testdata.vgr]2063131.geom
wms_test_work:[testdata.vgr]2063105.geom 
wms_test_work:[testdata.vgr]2063123.geom
wms_test_work:[testdata.vgr]2063113.geom 
wms_test_work:[testdata.vgr]2063103.geom
$!-----------------------------------------------------------------------------
$ create vms_summ.list
WMS_GLL:[SSI.UDR]s0349632200.1
WMS_GLL:[SSI.UDR]s0349632100.1
WMS_GLL:[SSI.UDR]s0349632122.1
WMS_GLL:[SSI.UDR]s0349632145.7
$!-----------------------------------------------------------------------------
$ create casimglist.vms
wms_test_work:[testdata.cassini.cas$i$ss]n1355064875.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355065283.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355065703.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355066109.1
$!-----------------------------------------------------------------------------
$ create tstmanmatch.log
ush $VRDILIB/usedisp a xwc0

tstmanmatch
  Local CASDir Type=String
  Local GLLDir Type=String
  Local VGRDir Type=String
  Local SysPath Type=String
  let $echo="no"
===For Galileo, Full Frame
===Please interactively select tie points with the mouse
 MANMATCH inp=(/project/test_work/testdata/gll/moon.sedr+
, /project/test_work/testdata/gll/moon.over)            out=moon.tiep 'OBJECT project=GLL file=unix_moon.list
Beginning VICAR task MANMATCH
 MANMATCH Version Nov 10 2002
# images in FILENAME file=          3
Treating images as OBJECT space images
# images in SEDR file=          3
# image pairs in over file=          2
displaying image pair #          1
Opening filename: /project/test_work/testdata/gll/7500.spk
Opening filename: /project/test_work/testdata/gll/8000.spk

corr=32

New pair number=          2
Opening filename: /project/test_work/testdata/gll/8000.spk
Opening filename: /project/test_work/testdata/gll/8500.spk
redisplaying images
corr=(32,32)

corr=(128,128)

'next
No more image pairs !, EXIT to get out
corr=(64,64)

 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    2.   3.    204.0   512.0     185.2    68.8     90.00  128.00
 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    2.   3.    294.0   522.0     276.0    78.1    117.00  169.00

 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    2.   3.    376.0   570.0     358.9   126.0    117.00  172.00
 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    2.   3.    390.0   500.0     373.0    54.1    178.00  262.00
 Pass#2 FFT area not within picture

 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    2.   3.     74.0   500.0      54.0    56.0     84.00  117.00
No more image pairs !, EXIT to get out
'exit
 number of correlations:   11
 number of tiepoints:    5
 IBIS-LIST moon.tiep
Beginning VICAR task IBIS
 
Number of Rows:5  Number of Columns: 12      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        2.00        3.00      204.00      512.00      185.16       68.81
        2.00        3.00      294.00      522.00      276.01       78.13
        2.00        3.00      376.00      570.00      358.94      125.96
        2.00        3.00      390.00      500.00      372.99       54.12
        2.00        3.00       74.00      500.00       53.98       56.04
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
+-----------+-----------+-----------+-----------+-----------+-----------
       90.00      128.00        1.00        0.00        0.00        0.00
      117.00      169.00        1.00        0.00        0.00        0.00
      117.00      172.00        1.00        0.00        0.00        0.00
      178.00      262.00        1.00        0.00        0.00        0.00
       84.00      117.00        1.00        0.00        0.00        0.00
  let $echo="no"
===For Voyager 2
===Please interactively select tie pointes with the mouse
 MANMATCH inp=(/project/test_work/testdata/vgr/red.sedr, /project/test_work/+
testdata/vgr/red.over) project=VGR-2 'object                 out=red.tiep 'sedr file=unix_red.list
Beginning VICAR task MANMATCH
 MANMATCH Version Nov 10 2002
# images in FILENAME file=          6
Treating images as OBJECT space images
# images in SEDR file=          6
# image pairs in over file=          9
displaying image pair #          1
Opening filename: /project/test_work/testdata/vgr/2063121.geom
Opening filename: /project/test_work/testdata/vgr/2063105.geom

 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    1.   3.     94.0   642.0     838.1   150.1   3542.00 3545.00
 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    1.   3.    102.0   940.0     846.0   439.0   3197.00 3127.00
'exit
 number of correlations:    4
 number of tiepoints:    2
 IBIS-LIST red.tiep
Beginning VICAR task IBIS
 
Number of Rows:2  Number of Columns: 12      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        3.00       94.00      642.00      838.11      150.13
        1.00        3.00      102.00      940.00      846.00      439.03
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
+-----------+-----------+-----------+-----------+-----------+-----------
     3542.00     3545.00        1.00        0.00        0.00        0.00
     3197.00     3127.00        1.00        0.00        0.00        0.00
  let $echo="no"
===For Cassini
===Please interactively select tie pointes with the mouse
 MANMATCH inp=(/project/test_work/testdata/cassini/casIss/ca+
ssedr.int, /project/test_work/testdata/cassini/casIss/casover.int)            out=casmatch.int 'OBJECT project=cassi file=casimglis+
t.unix
Beginning VICAR task MANMATCH
 MANMATCH Version Nov 10 2002
# images in FILENAME file=          4
Treating images as OBJECT space images
# images in SEDR file=          4
# image pairs in over file=          6
displaying image pair #          1
Opening filename: /project/test_work/testdata/cassini/casIss/n1355064875.1
Opening filename: /project/test_work/testdata/cassini/casIss/n1355065283.1

 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    1.   2.    785.0   737.0      60.0   770.9    137.00  141.00
 left  right  left(line,samp) right(line,samp) left_dn  right_dn
    1.   2.    773.0   935.0      48.0   968.9    140.00  143.00
'exit
 number of correlations:    4
 number of tiepoints:    2
 IBIS-LIST casmatch.int
Beginning VICAR task IBIS
 
Number of Rows:2  Number of Columns: 12      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        2.00      785.00      737.00       59.99      770.93
        1.00        2.00      773.00      935.00       48.00      968.95
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
+-----------+-----------+-----------+-----------+-----------+-----------
      137.00      141.00        1.00        0.00        0.00        0.00
      140.00      143.00        1.00        0.00        0.00        0.00
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
