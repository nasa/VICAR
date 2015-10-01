$!****************************************************************************
$!
$! Build proc for MIPL module automatch
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:29
$!
$! Execute by entering:		$ @automatch
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
$ write sys$output "*** module automatch ***"
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
$ write sys$output "Invalid argument given to automatch.com file -- ", primary
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
$   if F$SEARCH("automatch.imake") .nes. ""
$   then
$      vimake automatch
$      purge automatch.bld
$   else
$      if F$SEARCH("automatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake automatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @automatch.bld "STD"
$   else
$      @automatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create automatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack automatch.com -mixed -
	-s automatch.f -
	-i automatch.imake -
	-p automatch.pdf -
	-t tstautomatch.pdf listoffiles. vmsfiles. smifiles. casimglist.vms -
	   casimglist.unx
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create automatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
c VICAR program AUTOMATCH:
c	10-JUL-95  CRS (CRI) MSTP S/W CONVERSION (VICAR PORTING)
c	12-DEC-96  PXA Summation mode image changes - FR 088182
c	08-Nov-02  GMY Port to Linux.  Cassini update.
c
	SUBROUTINE MAIN44
	IMPLICIT NONE
	INTEGER	NCOL,maxpix,maxpts
	PARAMETER (NCOL = 12,maxpix=256,maxpts=1024)

	INTEGER	unitin1,unitin2,UNITO, STATUS, COUNT, DEF
	INTEGER	WINSL1, WINSS1, WINSL2, WINSS2
        integer mean1,mean2,left_image,right_image,pair
        integer npict,noverlap,ncol1,ncol2
        integer fds(maxpix),overlap(maxpix,2)
	INTEGER	CORRSIZE(2)
	INTEGER	DELL, DELS, DELL2, DELS2
	INTEGER	L, S, I, J, K,n
	INTEGER	CLEN, TIEP, COL
	INTEGER	IERR, NCORR
        integer bottom,right
        integer iseed
        integer unit4
        integer*4 four,zero,one
        integer*4 idatal(40),idatar(40)
        integer*4 ioerr,nfds
        integer*4 left,top,ncand,candidate,camera
        integer*4 inp_count,nah,nav,ind,iflag
	integer  ibis,ibis2,ibis3

        byte iname(80,maxpix)

        real*8 data8l(20),data8r(20),coefx(4),coefy(4)

	REAL	BUF1(256,256), BUF2(256,256)
	REAL	BUFOUT (256*258),  CORRPATCH(3,3)
	REAL	IBISBUF (maxpts,NCOL),over(maxpix,2)
	REAL	PHASE, PFILTER, OFFMAX
	REAL	DELLINE, DELSAMP, CORRVAL
        real    radpol(maxpix),eqpol(maxpix),focal(maxpix)
        real    optaxl(maxpix),optaxs(maxpix),scale(maxpix)
        real    rsvec(maxpix,3),omangl(maxpix,3),conv(3600)
        real    datal(40),datar(40),lout_cen,sout_cen
        real    candidate_pts(4,20),tolerance

	LOGICAL	HPF, SUBPIX, XVPTST, REDOCORR, NOPRINT
        logical sedr
        logical valid(20)

        character*5 project
	CHARACTER*3  FUNCSTR
        character*80 dirname
	CHARACTER*80  STRING,filename,name(maxpix),msgbuf

C		Global variables
	INTEGER	WINNL, WINNS, UNIT1, UNIT2, FUNC
	INTEGER	NL1, NS1, NL2, NS2, BORD, GRIDSIZE(2)
	REAL	XG(3,2)
	COMMON /GLOBAL/  WINNL, WINNS, UNIT1, UNIT2, FUNC,
     +		NL1, NS1, NL2, NS2, BORD, GRIDSIZE, XG

        equivalence (overlap,over),(data8l,datal,idatal)
        equivalence (data8r,datar,idatar),(iname,name)

        data bottom/512/,right/512/,four/4/,zero/0/
        data one/1/,iseed/98737461/


	call ifmessage('**** AUTOMATCH - version Nov 8 2002 ****')

C		Get the parameters

	CALL XVP ('CORR', CORRSIZE, COUNT)
	WINNL = CORRSIZE(1)
	WINNS = CORRSIZE(2)
	CALL XVP ('PFILTER', PFILTER, COUNT)
	CALL XVP ('PHASE', PHASE, COUNT)
	CALL XVP ('OFFMAX', OFFMAX, COUNT)
	CALL XVP ('FUNC', FUNCSTR, COUNT)
	IF (FUNCSTR .EQ. 'LOG')  FUNC = 1
	IF (FUNCSTR .EQ. 'EXP')  FUNC = 2
        call xvparm('PROJECT',project,count,def,1)
        call xvparm('DIR',dirname,count,def,1)
	SUBPIX = XVPTST ('SUBPIX')
	REDOCORR = XVPTST ('REDO')
	NOPRINT = XVPTST('NOPRINT')
        sedr=xvptst('SEDR')
        call xvparm('CAMERA',camera,count,def,1)
        call xvparm('NTPTS',ncand,count,def,1)
        call xvparm('TOLERANC',tolerance,count,def,1)
        call xvparm('FILENAME',filename,n,i,1)

c read the input file names from file FILENAME and store them in NAME.
        if(filename.ne.'NOFILE')then
          open(unit=3,file=filename,status='old',err=102,
     +         access='SEQUENTIAL', form='FORMATTED',
     +         iostat=i)
          if(i.eq.0) goto 101
102       call xvmessage('error opening listoffiles file',' ')
          msgbuf='error opening file  '
	  write(msgbuf(20:),'(A60)') filename
	  call xvmessage(msgbuf,' ')
          call abend
101       i=0
          do while(.true.)
            i=i+1
            if(i.gt.maxpix)then
               call ifmessage('Exceeded max number filename files')
	       msgbuf='Max # files='
	       write(msgbuf(14:17),'(I4)') maxpix
	       call xvmessage(msgbuf,' ')
               call abend
            endif

            read(3,103,iostat=ioerr,err=104,end=105)
     +		 (iname(j,i),j=1,80)
103         format(80A1)
          enddo
104       msgbuf='error reading file  '
	  write(msgbuf(20:), '(A60)') filename
	  call xvmessage(msgbuf,' ')
	  msgbuf='FORTRAN iostat error #='
	  write(msgbuf(25:28), '(I4)') ioerr
	  call xvmessage(msgbuf,' ')
          call abend
105       close(unit=3)
          nfds=i-1
	  msgbuf='# images in FILENAME file=.'
	  write(msgbuf(28:31),'(I4)') nfds
          call xvmessage(msgbuf,' ')
        endif

c set word 39 of convev buffer
        if(xvptst('OBJECT'))then
           call ifmessage('Treating images as OBJECT space images')
           idatal(39)=8
           idatar(39)=8
        else
           call ifmessage('Treating images as IMAGE space images')
           idatar(39)=7
           idatal(39)=7
        endif         

c set defaults
	HPF = .TRUE.
	NCORR = 0
	TIEP = 1
        pair=1

c Read in the SEDR.INT file
	call xvunit(unitin1,'INP',1,status,' ')
	call ibis_file_open(unitin1,ibis,'READ',0,0,' ',' ',status)
	if (status.ne.1)then
c          call ibis_signal(ibis,status,1)
           call xvmessage('cannot open sedr input file',' ')
           call abend
        endif
	call ibis_file_get(ibis,'NR',npict,1,1)
	call ibis_file_get(ibis,'NC',ncol1,1,1)
 	msgbuf='# images in SEDR file= '
	write(msgbuf(24:27), '(I4)') npict
	call xvmessage(msgbuf,' ')
        if(npict.gt.maxpix)then
           call mabend('Number input sedr entries too large')
        endif
        if(ncol1.lt.23) then
           call ifmessage('First input file not a SEDR.INT file')
 	   msgbuf='Number of columns='
	   write(msgbuf(20:23), '(I4)') ncol1
	   call xvmessage(msgbuf,' ')
           call abend
        endif
c read first column (fds times).
c replace with 1,2,3,4.. if FILENAME file is used.
        if(filename.eq.'NOFILE')then
	    call ibis_column_read(ibis,fds,1,1,npict,status)
	    if (status.ne.1) call ibis_signal(ibis,status,1)
        else
           if(npict.ne.nfds)then
              call ifmessage('The # pictures in FILENAME and ')
              call ifmessage('the SEDR ibis file disagree')
              call abend
           endif
           do i=1,npict
              fds(i)=i
           enddo
        endif
c read the xtra columns needed if SEDR requested
        if(sedr)then
           if(ncol1.lt.31) then
            call ifmessage('SEDR.INT file not created by IBISNAV')
 	    msgbuf='Number of columns='
	    write(msgbuf(20:23), '(I4)') ncol1
	    call xvmessage(msgbuf,' ')
            call abend
           endif
	   call ibis_column_read(ibis,rsvec(1,1),5,1,npict,status)
	   call ibis_column_read(ibis,rsvec(1,2),6,1,npict,status)
	   call ibis_column_read(ibis,rsvec(1,3),7,1,npict,status)
c
c           call getcol(unito,8,npict,omangl(1,1))
c           call getcol(unito,9,npict,omangl(1,2))
c           call getcol(unito,10,npict,omangl(1,3))
c
	   call ibis_column_read(ibis,omangl(1,1),21,1,npict,status)
	   call ibis_column_read(ibis,omangl(1,2),22,1,npict,status)
	   call ibis_column_read(ibis,omangl(1,3),23,1,npict,status)
	   call ibis_column_read(ibis,radpol,26,1,npict,status)
	   call ibis_column_read(ibis,eqpol,27,1,npict,status)
	   call ibis_column_read(ibis,focal,28,1,npict,status)
	   call ibis_column_read(ibis,optaxl,29,1,npict,status)
	   call ibis_column_read(ibis,optaxs,30,1,npict,status)
	   call ibis_column_read(ibis,scale,31,1,npict,status)
        endif       
	call ibis_file_close(ibis,' ',status)
	if (status.ne.1) call ibis_signal(ibis,status,0)
        call xvclose(unitin1,status,' ')

c Read in the OVER.INT file
	call xvunit(unitin2,'INP',2,status,' ')
	call ibis_file_open(unitin2,ibis2,'READ',0,0,' ',' ',status)
	if (status.ne.1)then
c          call ibis_signal(ibis2,status,1)
           call xvmessage('cannot open overlap input file',' ')
           call abend
        endif
	call ibis_file_get(ibis2,'NC',ncol2,1,1)
	call ibis_file_get(ibis2,'NR',noverlap,1,1)
        if(npict.gt.maxpix)then
           call mabend('Number input sedr entries too large')
        endif
	
        if(noverlap.gt.maxpix)then
           call mabend('Too many overlap pairs')
        endif
        if(ncol2.ne.2) then
           call ifmessage('Second input file not an OVER file')
 	   msgbuf='Number of columns='
	   write(msgbuf(20:23), '(I4)') ncol2
	   call xvmessage(msgbuf,' ')
           call abend
        endif
	call ibis_column_read(ibis2,overlap(1,1),1,1,noverlap,status)
	call ibis_column_read(ibis2,overlap(1,2),2,1,noverlap,status)
	call ibis_file_close(ibis2,' ',status)
	if (status.ne.1) call ibis_signal(ibis,status,0)
        call xvclose(unitin2,status,' ')
	msgbuf='# image pairs in OVER file='
	write(msgbuf(29:32), '(I4)') noverlap
	call xvmessage(msgbuf,' ')
        do i=1,noverlap
          overlap(i,1)=nint(over(i,1))
          overlap(i,2)=nint(over(i,2))
        enddo

c get geom parameters
        call xvpcnt('INP',inp_count)
        if(.not.xvptst('OBJECT'))then
           if(inp_count.lt.3)then
              iflag=0
              if(project.ne.'GLL  '.and.project.ne.'CASSI')
     +		 call ifmessage('Using nominals')
           else
              iflag=1
              call xvunit(unit4,'INP',inp_count,status,' ')
              call xvopen(unit4,status,'OPEN_ACT','SA',' ')
           endif
           call getgeom(unit4,project,camera,iflag,conv,conv,nah,
     +                  nav,ind)
	   if (ind.ne.0) call mabend('getgeom failure')
        endif

c LOOP ON IMAGE PAIRS
      msgbuf='Image pair # '
      do 31 pair=1,noverlap
        call xvmessage('   ',' ')
	write(msgbuf(14:17), '(I4)') pair
	call xvmessage(msgbuf,' ')

c setup SEDR for image pair
        if(sedr) call setnav(pair,overlap,maxpix,omangl,rsvec,
     +      radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +      datal,datar)
           
c Open pair of images
        left_image=fds(overlap(pair,1))
        right_image=fds(overlap(pair,2))
        call openit(unit1,left_image,dirname,1,nl1,ns1,
     +              filename,name,maxpix,optaxl,optaxs,pair,overlap)
        call openit(unit2,right_image,dirname,2,nl2,ns2,
     +              filename,name,maxpix,optaxl,optaxs,pair,overlap)
c Determine the overlap area limits in the right image
        call get_overlap_area(ind,conv,datal,datar,nl1,ns1,nl2,ns2,
     +                        left,right,top,bottom)
        if(ind.ne.0)then
            call ifmessage('Unable to process this pair.')
            goto 30
        endif

c determine correlation area size.
        call get_correl_size(ind,left,right,top,bottom,winnl,winns)
        if(ind.ne.0) then
           call ifmessage('Insufficient overlap for correlations.')
           goto 30
        endif

c Select a candidate set of tiepoints in the overlap area.
        call select_tiepoints(ind,conv,datal,datar,left,right,
     +                        top,bottom,candidate_pts,ncand,valid,
     +                        winnl,winns)
        if(ind.ne.0)then
            call ifmessage('Unable to process this pair.')
            call
     +       ifmessage('Too few candidate tiepoints in overlap area.')
            goto 30
        endif


c LOOP ON TIEPOINT CANDIDATES CONNECTING EACH PAIR
        do 40 candidate=1,ncand

            if(.not.valid(candidate)) goto 40
            
            winsl1=candidate_pts(1,candidate) - winnl/2
            winss1=candidate_pts(2,candidate) - winns/2
            winsl2=candidate_pts(3,candidate) - winnl/2
            winss2=candidate_pts(4,candidate) - winns/2


c           Compute polynomial mapping from left to right images.
c           for 4 corners of left correlation area.
            if(sedr)then
               call getmapping(datal,datar,conv,winsl1,winss1,winsl2,
     +                      winss2,winnl,winns,coefx,coefy,ierr)
               if(ierr.gt.0) goto 190
            endif

c           Read image areas around tiepoint for correlation purposes.
	    CALL EXTRACTCHIPS (WINSL1,WINSS1, BUF1, WINSL2,WINSS2, BUF2,
     +                         ierr,mean1,mean2,coefx,coefy,sedr)
            if(ierr.ne.0)then
               call ifmessage('Pass#1 FFT area not within picture')
               goto 190
            endif

c           display projected portion of right image
c            call display_mapping(buf2,gunit,winnl,winns,bottom)

C                  Correlate the two image chips
	    NCORR = NCORR + 1
	    CALL CROSSCORR (BUF1,256,BUF2,256,WINNL,WINNS, 7, .TRUE.,
     +			HPF, PHASE, PFILTER, OFFMAX,
     +			BUFOUT, DELL, DELS, CORRVAL, IERR)
	    IF (IERR .EQ. 2) THEN
              call ifmessage('OFFMAX distance exceeded in pass#1')
                goto 190                
            endif
            if(ierr.ne.0)THEN
                call ifmessage('error returned from crosscor, pass#1')
                GOTO 190
            endif

            if(sedr)then
c             update mapping coefficients
              call mapping_shift(coefx,coefy,dell,dels,winsl1,winss1,
     +                           winnl,winns,lout_cen,sout_cen)
            endif

	    IF (REDOCORR.AND.((dell.ne.0).or.(dels.ne.0)) ) THEN


		CALL EXTRACTCHIPS (WINSL1,WINSS1, BUF1, 
     +		   WINSL2+DELL,WINSS2+DELS, BUF2,ierr,mean1,mean2,
     +             coefx,coefy,sedr)
                if(ierr.ne.0)then
                   call ifmessage('Pass#2 FFT area not within picture')
                   goto 190
                endif

c               display projected portion of right image
c                call display_mapping(buf2,gunit,winnl,winns,bottom)

		NCORR = NCORR + 1
		CALL CROSSCORR (BUF1,256,BUF2,256,WINNL,WINNS, 7, .TRUE.,
     +			HPF, PHASE, PFILTER, OFFMAX,
     +			BUFOUT, DELL2, DELS2, CORRVAL, IERR)
		IF (IERR .EQ. 0) THEN
		    IF (ABS(DELL2) .GT. 2 .OR. ABS(DELS2) .GT. 2) THEN
                      call ifmessage('Pass#2 shift > 2 pixels')
                      ierr=4
                      goto 190
   		    ELSE
			DELL = DELL + DELL2
			DELS = DELS + DELS2
		    ENDIF
		ELSE IF (IERR .EQ. 2) THEN
                    call ifmessage('OFFMAX distance exceeded in pass#2')
                    goto 190                
		ELSE
                   call
     +                 ifmessage('error returned from crosscor, pass#2')
                   goto 190
		ENDIF

                if(sedr)then
c                  update mapping coefficients
                   call mapping_shift(coefx,coefy,dell2,dels2,winsl1,
     +                           winss1,winnl,winns,lout_cen,sout_cen)
                endif

	    ENDIF


C			Do subpixel interpolation if desired
	    DELLINE = DELL
	    DELSAMP = DELS
	    IF (SUBPIX) THEN
		DO I = 1, 3
		    DO J = 1, 3
			L = WINNL/2 + 1 + DELL+I-2
			S = WINNS/2 + 1 + DELS+J-2
			CORRPATCH(I,J) = BUFOUT(S + WINNS*(L-1))
		    ENDDO
		ENDDO
		CALL REFINE (CORRPATCH, DELLINE, DELSAMP, 190)
	    ENDIF

c           store the tiepoint
            valid(candidate)=.true.
            candidate_pts(1,candidate)=float(winsl1+winnl/2)
            candidate_pts(2,candidate)=float(winss1+winns/2)
            if(sedr)then
               candidate_pts(3,candidate)=lout_cen
               candidate_pts(4,candidate)=sout_cen
            else
               candidate_pts(3,candidate)=float(winsl2+winnl/2)+
     +                                    delline
               candidate_pts(4,candidate)=float(winss2+winns/2)+
     +                                    delsamp
            endif   


C           Print out info
	    IF (.NOT. NOPRINT) THEN
               if(candidate.eq.1) then
                 msgbuf=' left  right  left(line,samp) right(line,samp)'
	         call xvmessage(msgbuf,' ')
               endif
		WRITE (STRING, 11) (overlap(pair,k),k=1,2),
     +                             (candidate_pts(k,candidate),k=1,4)
11		FORMAT (1X,2I5,1X,2F8.1,2X,2F8.1,2X)
		CALL xvmessage(STRING,' ')
	    ENDIF

            goto 200

c If error in correlation go here.
190	    CONTINUE
            valid(candidate)=.false.

200         continue
            ierr=0
            delline=0.
            delsamp=0.

40        continue                  
c         END OF MAIN TIEPOINT GENERATION LOOP

c         Eliminate bad tiepoints, leaving 2 remaining as widely
c         spaced as possible.
          call edit_tiepoints(ind,candidate_pts,valid,ncand,tolerance)
          if(ind.ne.0)then
             call ifmessage('Unable to determine 2 good tiepoints.')
             goto 30
          endif

c         Load the IBIS buffers with the remaining points.
          do i=1,ncand
             if(valid(i))then
                ibisbuf(tiep,1)=overlap(pair,1)
                ibisbuf(tiep,2)=overlap(pair,2)
                ibisbuf(tiep,3)=candidate_pts(1,i)
                ibisbuf(tiep,4)=candidate_pts(2,i)
                ibisbuf(tiep,5)=candidate_pts(3,i)
                ibisbuf(tiep,6)=candidate_pts(4,i)
                tiep=tiep+1
             endif
          enddo

30        continue
          CALL XVCLOSE (UNIT1, STATUS,' ')
          CALL XVCLOSE (UNIT2, STATUS,' ')

31      continue        
c       END OF IMAGE PAIR LOOP

c  WRITE OUT TIEPOINTS INTO IBIS TABULAR FILE

	CLEN = TIEP - 1
        call mve(7,clen,1.0,ibisbuf(1,7),0,1)
        call mve(7,clen,1.0,ibisbuf(1,8),0,1)
        call mve(7,clen,1.0,ibisbuf(1,9),0,1)

	WRITE (STRING, '(1X,A,I4)')  'Number of correlations: ', NCORR
	CALL xvmessage(STRING,' ')

	IF (CLEN .EQ. 0) THEN
	    CALL ifmessage(' No tiepoints output')
	ELSE
	WRITE (STRING, '(1X,A,I4)')  'Number of tiepoints: ', clen
	CALL xvmessage(STRING,' ')
C		Output array to IBIS file in col_ordr
	call xvunit(unito,'OUT',1,status,' ')
	call ibis_file_open(unito,ibis3,'WRITE',NCOL,CLEN,
     +                     ' ',' ',status)
	if (status.ne.1) call ibis_signal(ibis3,status,1)
	    DO COL = 1, NCOL
		call ibis_column_write(ibis3,ibisbuf(1,col),col,
     +                                  1,clen,status)
	    ENDDO
	    call ibis_file_close(ibis3,' ',status)
	    if (status.ne.1) call ibis_signal(ibis,status,0)

	    CALL XVCLOSE (UNITO, STATUS,' ')
	ENDIF

	RETURN
	END

c****************************************************************
      subroutine edit_tiepoints(ind,candidate_pts,valid,ncand,
     +           tolerance)
c edit bad tiepoints, returns two furthest apart.
      integer*4 lookup(20)
      real*4 candidate_pts(4,20)
      logical valid(20)
      real*8 c(20,4),cl(20),ex(4),a(4,4),r(4,4),q(4,4),e
      real*8 line_sol(4),samp_sol(4),resid_l(20),resid_s(20)
      character*80  msgbuf

      if(ncand.lt.2)then
         ind=1
         return
      endif

      ind=0
      if(ncand.lt.4) goto 100

10    num=0
      do i=1,ncand                   ! line equation: y1=Ax+By+C
        if(valid(i))then
           num=num+1
           cl(num)=candidate_pts(1,i)
           c(num,1)=candidate_pts(4,i) ! sample
           c(num,2)=candidate_pts(3,i) ! line
           c(num,3)=1.d0
           lookup(num)=i
        endif
      enddo

      if(num.lt.4) goto 100
      
      call lsqp(ind,num,3,c,cl,line_sol,resid_l,e,ex,a,r,q)
      if(ind.ne.0)then
         call ifmessage('Singular LSQP solution for y equation')
         ind=0
         goto 100
      endif

      num=0
      do i=1,ncand                   ! samp equation: x1=Ax+By+C
        if(valid(i))then
           num=num+1
           cl(num)=candidate_pts(2,i)
        endif
      enddo
      call lsqp(ind,num,3,c,cl,samp_sol,resid_s,e,ex,a,r,q)
      if(ind.ne.0)then
         call ifmessage('Singular LSQP solution for x equation')
         ind=0
         goto 100
      endif

c locate largest residual
      errmax=-1.e+20
      do i=1,num
         error=dsqrt(resid_l(i)**2+resid_s(i)**2)
         if(error.gt.errmax)then
            errmax=error
            k=i
         endif
      enddo

c flag worst offender & return if necessary
      if(errmax.gt.tolerance)then
         valid(lookup(k))=.false.
	 msgbuf='Largest fit residual= '
	 write(msgbuf(23:),'(F10.4)' ) errmax
	 call xvmessage(msgbuf,' ')
	 msgbuf='Tiepoint # deleted= '
	 write(msgbuf(21:24), '(I4)') lookup(k)
	call xvmessage(msgbuf,' ')
         if(num.lt.4)then
            goto 100
         endif
         goto 10   ! re-do the fit
      endif
      msgbuf='Largest fit residual remaining= '
      write(msgbuf(33:),'(F10.4)' ) errmax
      call xvmessage(msgbuf,' ')

c locate first & last good points, delete the rest.
100   do i=1,ncand
         j=i
         if(valid(i))goto 20
      enddo
20    do i=ncand,1,-1
         k=i
         if(valid(i))goto 30
      enddo
30    if(k-j.gt.1)then
         do i=j+1,k-1
            valid(i)=.false.
         enddo
      endif
      return
      end

c****************************************************************
      subroutine get_correl_size(ind,left,right,top,bottom,winnl,
     +                           winns)
c input the overlap area,
c returns winnl,winns the correlation size.
      integer*4 left,right,top,bottom,winnl,winns
      character*80 msgbuf
c 20 is random number width, see SELECT_TIEPOINTS      
       
      ind=0

      if(right-left.gt.bottom-top)then
         winns=(right-left)/3
      else      
         winns=(right-left-20)*0.6
      endif

      if(winns.ge.128)then
         winns=128
      else if(winns.ge.64)then
         winns=64
      else if(winns.ge.32)then
         winns=32
      else if(winns.ge.16)then
         winns=16
      else
	 msgbuf='Horizontal overlap= '
	 write(msgbuf(21:24),'(I4)' ) right-left
	 call xvmessage(msgbuf,' ')
         ind=1
         return
      endif

      if(bottom-top.gt.right-left)then
         winnl=(bottom-top)/3
      else      
         winnl=(bottom-top-20)*0.6
      endif

      if(winnl.ge.128)then
         winnl=128
      else if(winnl.ge.64)then
         winnl=64
      else if(winnl.ge.32)then
         winnl=32
      else if(winnl.ge.16)then
         winnl=16
      else
	 msgbuf='Vertical overlap= '
	 write(msgbuf(19:22),'(I4)' ) bottom-top
	 call xvmessage(msgbuf,' ')
         ind=1
         return
      endif

	 msgbuf='vertical correlation size= '
	 write(msgbuf(28:31),'(I4)' ) winnl
	 call xvmessage(msgbuf,' ')
	 msgbuf='horizontal correlation size= '
	 write(msgbuf(30:33),'(I4)' ) winns
	 call xvmessage(msgbuf,' ')
      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,X1,V,E,EX,A,R,Q)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C          V(I) = RESIDUALS  (I.E. OBSERVED MINUS COMPUTED)
C          E = MEAN ERROR OF THE UNIT WEIGHT
C          EX(J) = MEAN ERRORS OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      REAL*8  A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SL,SQ,P,SUM
      REAL*8 C(20,4),CL(20),X1(4),V(20),EX(4)
C
      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      if(a(nu,nu).eq.0.d0)goto 999
      Q(NU,NU)=1./A(NU,NU)
      DO 150 I=1,NUM
      NP=NUP-1
      DO 135 J=I,NUM
      NM=NU-J
      JP=NM+1
      P=0.
      DO 135 K=JP,NU
      P=P-R(NM,K)*Q(NP,K)
      Q(NP,NM)=P
135   Q(NM,NP)=P
      NPM=NP-1
      SQ=0.
      DO 145 L=NP,NU
145   SQ=SQ-R(NPM,L)*Q(L,NPM)
      if(a(npm,npm).eq.0.d0)goto 999
150   Q(NPM,NPM)=1./A(NPM,NPM)+SQ
      DO 151 I=1,NE
      V(I)=0.
      DO 151 J=1,NU
151   V(I)=V(I)+C(I,J)* X(J)
      SL=0.
      DO 153 I=1,NE
      V(I)=CL(I)-V(I)
153   SL=SL+V(I)*V(I)
      FNE=NE
      FNU=NU
      E=DSQRT(SL/(FNE-FNU))
      DO 160 I=1,NU
        IF ( Q(I,I) .GE. 0.D0 ) THEN
          EX(I)=E*DSQRT(Q(I,I))
        ELSE
          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
        END IF
160   CONTINUE      
      RETURN
999   ind=1
      return
      END

c*****************************************************************
      subroutine get_overlap_area(ind,conv,datal,datar,nl1,ns1,nl2,
     +          ns2,left,right,top,bottom)
c returns left,right,top,bottom of the overlap area in pixels for the
c second input of the pair.

      integer*4 left,right,top,bottom
      real*4 conv(1),datal(1),datar(1),line(4),samp(4),lat,lon
      real*4 s_min,s_max,l_min,l_max
      logical valid(4)

      line(1)=1
      line(2)=1
      line(3)=nl1
      line(4)=nl1
      samp(1)=1
      samp(2)=ns1
      samp(3)=1
      samp(4)=ns1

      ind=0
      num=0
c compute left image corners in right image
      do 10 i=1,4
         call convev(ind,datal,datal,line(i),samp(i),lat,lon,2,conv)
         if(ind.ne.0)then
            call ifmessage('Point off planet in left image.')
            valid(i)=.false.
            goto 10
         endif
         call convev(ind,datar,datar,line(i),samp(i),lat,lon,1,conv)
         if(ind.ne.0)then
            call ifmessage('Point behind planet in right image.')
            valid(i)=.false.
            goto 10
         endif
         valid(i)=.true.
         num=num+1
10    continue

      if(num.lt.2)then
         call ifmessage('Less than 2 candidate points generated')
         ind=1
         return
      endif

c compute limits of overlap
      s_min=1.0e+20
      s_max=-1.0e+20
      l_min=1.0e+20
      l_max=-1.0e+20
      do i=1,4
         if(valid(i))then
            s_min=min(s_min,samp(i))
            s_max=max(s_max,samp(i))
            l_min=min(l_min,line(i))
            l_max=max(l_max,line(i))
         endif
      enddo

c check to see if images don't overlap.
      if((s_min.gt.float(ns2)).or.(l_min.gt.float(nl2)).or.
     +     (s_max.lt.1.).or.(l_max.lt.1.)) then
         call ifmessage('No overlap predicted from navigation.')
         ind=1
         return
      endif

      if(s_min.lt.1.0) then  !left side of right image.
         left=1
         if(s_max.gt.float(ns2))then
            right=ns2
         else
            right=s_max
         endif
      else if(s_max.gt.float(ns2))then  ! right side of right image.
         right=ns2
         left=s_min
      else 
         left=s_min
         right=s_max  
      endif

      if(l_min.lt.1.0) then     ! above right image
         top=1
         if(l_max.gt.float(nl2))then
            bottom=nl2
         else
            bottom=l_max
         endif
      else if(l_max.gt.float(nl2))then   ! below right image
         bottom=nl2
         top=l_min
      else
         top=l_min
         bottom=l_max
      endif

      if((right-left.lt.32).or.(bottom-top.lt.32))then
         ind=1
         call ifmessage('Insufficient overlap for correlation')
         return
      endif

      return
      end

c****************************************************************
      subroutine select_tiepoints(ind,conv,datal,datar,left,right,
     +     top,bottom,candidate_pts,ncand,valid,winnl,winns)
c compute a set of candidate tiepoints to correlate on.
      integer*4 left,right,top,bottom,begin,winnl,winns
      real*4 conv(1),datal(1),datar(1),candidate_pts(4,20),lat,lon
      real*4 line,samp,rannum
      logical valid(20)

      scale=20.
      iseed=987654321

c select points in right image.
      if(right-left.gt.bottom-top)then
         interval=(right-left-winns-40)/(ncand-1)
         begin=left+20+winns/2-interval
         if(interval.lt.1)then
            ind=1
            return
         endif
         do i=1,ncand
	    call rangen(iseed,rannum)
            candidate_pts(3,i)=(bottom-top)/2+top+            !line
     +                         scale*(rannum-0.5)   ! (ran(iseed)-0.5)
            candidate_pts(4,i)=begin+i*interval               !sample
         enddo
      else      
         interval=(bottom-top-winnl-40)/(ncand-1)
         begin=top+20+winnl/2-interval
         if(interval.lt.1)then
            ind=1
            return
         endif
         do i=1,ncand
	    call rangen(iseed,rannum)
            candidate_pts(3,i)=begin+i*interval                !line
            candidate_pts(4,i)=(right-left)/2+left+            !sample
     +                         scale*(rannum-.05) ! (ran(iseed)-0.5)
         enddo
      endif         

c compute the corresponding locations in the left image.
      num=0
      do 10 i=1,ncand
         line=candidate_pts(3,i)
         samp=candidate_pts(4,i)
         call convev(ind,datar,datar,line,samp,
     +               lat,lon,2,conv)
         if(ind.ne.0)then
            call ifmessage('Point off planet in right image.')
            valid(i)=.false.
            goto 10
         endif
         call convev(ind,datal,datal,candidate_pts(1,i),
     +               candidate_pts(2,i),lat,lon,1,conv)
         if(ind.ne.0)then
            call ifmessage('Point behind planet in left image.')
            valid(i)=.false.
            goto 10
         endif
         valid(i)=.true.
         num=num+1
10    continue

      if(num.lt.4)then
         ind=1
      else
         ind=0
      endif

      return
      end


c*****************************************************************
      subroutine mapping_shift(coefx,coefy,dell,dels,winsl1,winss1,
     +   winnl,winns,yright1,xright1)
c correct polynomial mapping from input to output and
c compute new output center from input center
      real*8 coefx(4),coefy(4),rl,rs
      integer*4 winsl1,winss1,winnl,winns,dell,dels

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


c **************************************************************
        subroutine setnav(pair,overlap,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
c Fills the DATAL & DATAR for convev subroutine.
        real*8 r81,r82,r83,data8l(20),data8r(20)
        integer*4 pair,overlap(maxpix,2)
        real*4 omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real*4 focal(1),optaxl(1),optaxs(1),scale(1)
        real*4 datal(40),datar(40)
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
        if(datal(25).eq.0.)call qprint('WARNING: SEDR is blank')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
c        write(*,*)pair,i
c        write(*,*)(data8l(j),j=1,9)
c        write(*,*)(data8l(j),j=10,12)
c        write(*,*)(datal(j),j=25,30)
c        call mve(4,1,datal(39),j,1,1)
c        write(*,*)j
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
        if(datar(25).eq.0.)call qprint('WARNING: SEDR is blank')
        datar(26)=eqpol(i)
        datar(27)=focal(i)
        datar(28)=optaxl(i)
        datar(29)=optaxs(i)
        datar(30)=scale(i)         
c        write(*,*)pair,i
c        write(*,*)(data8r(j),j=1,9)
c        write(*,*)(data8r(j),j=10,12)
c        write(*,*)(datar(j),j=25,30)
c        call mve(4,1,datar(39),j,1,1)
c        write(*,*)j
        return
        end

c **************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(3,3)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
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
	c(1,1) = -sin_alpha * cos_kappa - 
     +     cos_alpha * sin_delta * sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - 
     +     sin_alpha * sin_delta * sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - 
     +      cos_alpha * sin_delta * cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - 
     +      sin_alpha * sin_delta * cos_kappa
	c(2,3) =  cos_delta * cos_kappa
	c(3,1) =  cos_alpha * cos_delta
	c(3,2) =  sin_alpha * cos_delta
	c(3,3) =  sin_delta
	return
	end



c ******************************************************************
        subroutine openit(unit,fds,dirname,n,nl,ns,
     +              filenames,name,maxpix,optaxl,optaxs,pair,overlap)
c to open frames whose filenames are FDS.img in directory DIRNAME.
c or whose names are in NAME(fds).
        integer       unit,fds,status
        character*80  dirname,filename,string,filenames,name(maxpix)
        character*132  msgbuf
        character*1   st1(80),st2(80)
        real*4 optaxl(1),optaxs(1)
        integer*4 pair,overlap(maxpix,2)
        equivalence (filename,st1),(string,st2)
        
       
      if(filenames.eq.'NOFILE')then ! no file with names is present
        write(string,11) fds
11      format (i8)
     
c find start of ascii fds time
        do i=1,8
          if(st2(i).ne.' ')then
            k1=i
            goto 10
          endif
        enddo
10      filename(1:80)=dirname(1:80)

c find end of directory name
        k2=0
        do i=80,1,-1
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

      else
        filename=name(fds)
      endif

c        call qprint('Opening filename: '//filename)
	msgbuf='Opening filename: '
	write(msgbuf(19:),'(A)') filename
	call xvmessage(msgbuf,' ')

      if (n .eq. 1) then
         ii=overlap(pair,1)
         nl=2*int(optaxl(ii))
         ns=2*int(optaxs(ii))
      endif
      if (n .eq. 2) then
         jj=overlap(pair,2)
         nl=2*int(optaxl(jj))
         ns=2*int(optaxs(jj))
      endif 
c open the file & return UNIT number & nl & ns
        call xvunit(unit,'OLD',n,status,'U_NAME',filename,' ')
        call xvsignal(unit,status,.true.)
        call xvopen(unit,status,'OPEN_ACT','SA','IO_ACT','SA',
     +              'OP','READ','U_FORMAT','HALF',' ')
        call xvsignal(unit,status,.true.)
c        call xvget(unit,status,'NL',nl,'NS',ns,' ')
        call xvsignal(unit,status,.true.)

        return
        end


c *****************************************************************
C			Extract the image chips from each image
	SUBROUTINE EXTRACTCHIPS (SL1,SS1, BUF1, SL2,SS2, BUF2,ierr,
     +                           mean1,mean2,coefx,coefy,sedr)
	IMPLICIT NONE
	INTEGER	SL1, SS1, SL2, SS2
	REAL	BUF1(256,256), BUF2(256,256)
	INTEGER	L, S, STATUS,ierr,mean1,mean2,kl,ks
        integer k,itop,ibot,ilft,irt
        real*8 coefx(4),coefy(4),rl,rj
        real*4 xright,yright,r,wtop,wbot
        logical sedr
C		Global variables
	INTEGER	WINNL, WINNS, UNIT1, UNIT2, FUNC,j
	INTEGER	NL1, NS1, NL2, NS2, BORD, GRIDSIZE(2)
	REAL	XG(3,2),xr(4),yr(4)
        integer*2 b(512)
              
	COMMON /GLOBAL/  WINNL, WINNS, UNIT1, UNIT2, FUNC,
     +		NL1, NS1, NL2, NS2, BORD, GRIDSIZE, XG

c                  check to assure the areas are within the data
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
        mean1=0
        mean2=0

        if(sedr)then        ! perform local map2

c         compute the four corner points in the right image
          k=0
          do L=1,winnl,winnl-1
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
          do L=itop,ibot
             call xvread(unit2,b,status,'LINE',L,'SAMP',ilft,
     +                   'NSAMPS',irt-ilft+1,' ')
             k=k+1
             do j=1,irt-ilft+1
                buf1(j,k)=b(j)
             enddo
          enddo      

c         Geom the data from buf1 to buf2
          do L=1,winnl
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
               buf2(j,L)=r
               mean2=mean2+r
            enddo
          enddo

        else                ! no projection
          do L=1,winnl
	    CALL XVREAD (UNIT2, b, STATUS, 'LINE',SL2+L-1,
     +			'SAMP',SS2, 'NSAMPS',WINNS,' ')
            do j=1,winns
               buf2(j,L)=b(j)
               mean2=mean2+b(j)
            enddo
          ENDDO
        endif

c read the left image area into buf1
	DO L = 1, WINNL
	    CALL XVREAD (UNIT1, b, STATUS, 'LINE',SL1+L-1,
     +			'SAMP',SS1, 'NSAMPS',WINNS,' ')
            do j=1,winns
               buf1(j,L)=b(j)
               mean1=mean1+b(j)
            enddo
        enddo

        mean1=mean1/(winnl*winns)
        mean2=mean2/(winnl*winns)

	IF (FUNC .EQ. 1) THEN
	    DO L = 1, WINNL
		DO S = 1, WINNS
		    BUF1(S,L) = 46.*LOG(BUF1(S,L)+1)
		    BUF2(S,L) = 46.*LOG(BUF2(S,L)+1)
		ENDDO
	    ENDDO
	ELSE IF (FUNC .EQ. 2) THEN
	    DO L = 1, WINNL
		DO S = 1, WINNS
		    BUF1(S,L) = EXP(BUF1(S,L)/46.)
		    BUF2(S,L) = EXP(BUF2(S,L)/46.)
		ENDDO
	    ENDDO
	ENDIF


	RETURN
	END



C********************************************************************
      SUBROUTINE CROSSCORR( A,IA, B,IB, M,N, DCODE, FIRST, HPF, PHASE,
     .                      PFILTER, OFFMAX,
     .                      C, IL,IS, QUAL, ISTATUS )
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

      IMPLICIT NONE

C...ARGUMENT DECLARATIONS

      BYTE        A(*),B(*)
      INTEGER*4   IA, IB, M, N, DCODE, IL,IS, ISTATUS
      LOGICAL*4   FIRST, HPF
      REAL*4      PHASE, PFILTER, OFFMAX, C(N,M+2), QUAL

C...LOCAL DECLARATIONS
                 
      INTEGER*4   I,  IPTRA,   IPTRB,   JPTR,   LSIZEA,  LSIZEB,
     .            MCODE,       MAXM_PAR,        MM2_PAR, PIXSIZE(8)

      PARAMETER   (MAXM_PAR = 256)       ! MAX FOR M & N.  COULD BE INCREASED.
      PARAMETER   (MM2_PAR  = MAXM_PAR*(MAXM_PAR+2))

      REAL*4      FA( MM2_PAR ), FB(MM2_PAR)
      SAVE        FA                     ! FFT OF A SLEPT HERE.

      DATA        PIXSIZE  /  1, 2, 0, 4, 0, 0, 4, 8  /  ! BYTES PER PIXEL

C
C======================START OF EXECUTABLE CODE======================

      ISTATUS = 0

C...CONVERT IMAGE DATA TO REAL*4 AND MOVE INTO FA & FB.

      LSIZEA = IA * PIXSIZE(DCODE)
      LSIZEB = IB * PIXSIZE(DCODE)
      IPTRA  = 1
      IPTRB  = 1
      JPTR   = 1

      IF ( N .NE. MIN(N,IA,IB))  THEN
        ISTATUS = -1
        CALL ifmessage('ERROR IN CROSSCORR: INVALID N VALUE')
        
      ELSE IF ( N .GT. MAXM_PAR .OR. M .GT. MAXM_PAR)  THEN
        ISTATUS = -1
        CALL ifmessage('ERROR IN CROSSCORR: INVALID M OR N VALUE')
        
      ELSE       ! automatch always uses decode = 7 
        IF (DCODE .EQ. 1 .OR. DCODE .EQ. 2 .OR. DCODE .EQ. 4)  THEN
          DO I = 1, M
            IF (FIRST)  THEN            ! MOVE A LINE AND CONVERT TO REAL*4.
               CALL FLOATA( DCODE, N, A(IPTRA), FA(JPTR) )
               IPTRA = IPTRA + LSIZEA
            END IF
            CALL FLOATA( DCODE, N, B(IPTRB), FB(JPTR) )
            IPTRB = IPTRB + LSIZEB
            JPTR = JPTR + N
          END DO
  
        ELSE IF (DCODE .EQ. 7 .OR. DCODE .EQ. 8)  THEN
          IF (DCODE .EQ. 7)  MCODE = 7
          IF (DCODE .EQ. 8)  MCODE =-9   ! automatch always uses decode = 7 
	  DO I=1, M
	      IF (FIRST) THEN                 ! MOVE A LINE
	        CALL MVE( MCODE, N, A(IPTRA), FA(JPTR),1,1 )
          	IPTRA = IPTRA + LSIZEA
              END IF
              CALL MVE( MCODE, N, B(IPTRB), FB(JPTR),1,1 )
              IPTRB = IPTRB + LSIZEB
              JPTR = JPTR + N
          END DO
  
        ELSE
          ISTATUS = -1
          CALL ifmessage('ERROR IN CROSSCORR: INVALID DCODE VALUE')
        END IF
      END IF

      IF (ISTATUS .EQ. 0)
     .  CALL FTCORR( FA,FB, M,N, FIRST, HPF, PHASE, PFILTER, OFFMAX,
     .               C, IL,IS, QUAL, ISTATUS)

      RETURN
      END


c **********************************************************************
      SUBROUTINE FTCORR( FA,FB, M,N, FIRST, HPF, PHASE, PFILTER, OFFMAX,
     .                   C, IL,IS, QUAL, ISTATUS)

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

      IMPLICIT NONE

C...ARGUMENT DECLARATIONS

      INTEGER*4   M, N, IL,IS, ISTATUS, rftstat
      LOGICAL*4   FIRST, HPF
      REAL*4      PHASE, PFILTER, OFFMAX, C(N,M+2), QUAL,
     .            FA(N,M+2), FB(N,M+2)

C...LOCAL DECLARATIONS

      INTEGER*4   I,  J,  IXMAX, JXMAX,  MHALF, NHALF

      REAL*4      DNORM,  FILPOW, POWER, POWERA, POWER1A,POWER1B,
     .            POWERB, POWER2A, POWER2B,  FILT, R2, Y2, AMP, AMPP,
     .            CIJR, CIJI, VMAX, T, RV
      SAVE        POWERA
C
C======================START OF EXECUTABLE CODE======================

      ISTATUS = 0
      DNORM   = MAX( M,N )
      DNORM   = 1./DNORM
C...CHECK THAT PARAMETERS ARE VALID.

      IF ( MOD(M,2) .NE. 0 .OR. MOD(N,2) .NE. 0 )     GOTO 8100
      IF ( PHASE .LT. 0 .OR. PHASE .GT. 1.0)          GOTO 8200
      IF ( PFILTER .LT. 0)                            GOTO 8300
      IF ( OFFMAX .LE. 0 .OR. OFFMAX .GT. 1.0)        GOTO 8400

C...FFT the image areas

      IF (FIRST)  THEN
          CALL RFT2(FA, M,N, 1, rftstat)
          if (rftstat.ne.1) goto 8100

C...Do radial filter for A area

	  IF (PFILTER .NE. 0.0) THEN

	    IF (PHASE .EQ. 1.0) THEN 
               FILPOW = PFILTER             ! ALL FOR A AND NONE FOR B.
            ELSE
               FILPOW = PFILTER/2.          ! HALF FOR A AND HALF FOR B.
            END IF

	    DO I = 1, N
	      Y2 = ( MIN(I-1, N-I+1)*DNORM )**2 
	      DO J = 1, M+2, 2
	        R2 = Y2 + ( ((J-1)/2)*DNORM )**2
                IF (FILPOW .EQ. 1.0)  THEN
                   FILT = R2
                ELSE IF (FILPOW .EQ. .5)  THEN
                   FILT = SQRT(R2)
                ELSE
                   FILT = R2**FILPOW
                END IF
	        FA(I,J) = FILT*FA(I,J)
	        FA(I,J+1) = FILT*FA(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C...HANDLE FRACTIONAL PHASE VALUES.

          IF (PHASE .GT. 0.0 .AND. PHASE .LT. 1.0)  THEN
	    DO J = 1, M+2, 2
	      DO I = 1, N
	        AMP = AMAX1( SQRT( FA(I,J)**2 + FA(I,J+1)**2), 1.E-12)
                IF (PHASE .EQ. .5)  THEN
                    AMPP= 1./ SQRT(AMP)
                ELSE
                    AMPP= 1./( AMP**PHASE )
                END IF
	        FA(I,J)   = AMPP*FA(I,J)
	        FA(I,J+1) = AMPP*FA(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C		Zero first row and column if HPF

          IF (HPF) THEN
            CALL ZIA( FA, 2*N)
            DO J = 3,M+2
              FA(1,J) = 0.
            ENDDO
          ENDIF

C		Calculate power for normalization of nophase

	  IF (PHASE .NE. 1.0) THEN
	    FA(1,1) = 0.0               ! ZERO DIRECT CURRENT (DC) TERM.
	    FA(1,2) = 0.0    ! IN CASE NOT 0 BECAUSE OF ROUNDING.
            POWER1A = 0.0
            POWER2A = 0.0
            DO I = 1, N
      	      POWER1A = POWER1A + ( FA(I,1)**2 + FA(I,2)**2 )
      	      POWER1A = POWER1A + ( FA(I,M+1)**2 + FA(I,M+2)**2 )
              DO J = 3, M, 2
	        POWER2A = POWER2A + ( FA(I,J)**2 + FA(I,J+1)**2 )
              ENDDO
	    ENDDO
	    POWERA = POWER1A + 2*POWER2A
	  ENDIF
      ENDIF

C...COMPUTE FFT OF B IMAGE AREA.

      CALL RFT2(FB, M,N, 1, rftstat)
      if (rftstat.ne.1) goto 8100
		
C...If phase corr. multiply FFT's  & divide by the amplitudes.
 
      IF (PHASE .EQ. 1.0)  THEN
        DO J = 1, M+1, 2
          DO I = 1, N
            CIJR = FA(I,J)*FB(I,J) + FA(I,J+1)*FB(I,J+1)
            CIJI = FA(I,J)*FB(I,J+1) - FA(I,J+1)*FB(I,J)
            AMP  = AMAX1( SQRT(CIJR*CIJR+CIJI*CIJI), 1.E-12)
            C(I,J)   = CIJR/AMP
            C(I,J+1) = CIJI/AMP
          ENDDO
        ENDDO

      ELSE   ! OTHERWISE WE FILTER AND COMPUTE NORMALIZATION FACTOR POWERB
             ! AND THEN MULTIPLY FFTS.

C...Do radial filter for B area

	  IF (PFILTER .NE. 0.0) THEN
            FILPOW = PFILTER/2.          ! HALF FOR A AND HALF FOR B.

	    DO I = 1, N
	      Y2 = ( MIN(I-1, N-I+1)*DNORM )**2 
	      DO J = 1, M+2, 2
	        R2 = Y2 + ( ((J-1)/2)*DNORM )**2
                IF (FILPOW .EQ. 1.0)  THEN
                   FILT = R2
                ELSE IF (FILPOW .EQ. .5)  THEN
                   FILT = SQRT(R2)
                ELSE
                   FILT = R2**FILPOW
                END IF
	        FB(I,J) = FILT*FB(I,J)
	        FB(I,J+1) = FILT*FB(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C...HANDLE FRACTIONAL PHASE VALUES.

          IF (PHASE .GT. 0.0 .AND. PHASE .LT. 1.0)  THEN
	    DO J = 1, M+2, 2
	      DO I = 1, N
	        AMP = AMAX1( SQRT( FB(I,J)**2 + FB(I,J+1)**2), 1.E-12)
                IF (PHASE .EQ. .5)  THEN
                    AMPP= 1./ SQRT(AMP)
                ELSE
                    AMPP= 1./( AMP**PHASE )
                END IF
	        FB(I,J)   = AMPP*FB(I,J)
	        FB(I,J+1) = AMPP*FB(I,J+1)
	      ENDDO
	    ENDDO
	  ENDIF

C		Zero first row and column if HPF

          IF (HPF) THEN
            CALL ZIA( FB, 2*N)
            DO J = 3,M+2
              FB(1,J) = 0.
            ENDDO
          ENDIF

C		Calculate power for normalization.

	    FB(1,1) = 0.0               ! ZERO DIRECT CURRENT (DC) TERM.
	    FB(1,2) = 0.0    ! IN CASE NOT 0 BECAUSE OF ROUNDING.
            POWER1B = 0.0
            POWER2B = 0.0
            DO I = 1, N
      	      POWER1B = POWER1B + ( FB(I,1)**2 + FB(I,2)**2 )
      	      POWER1B = POWER1B + ( FB(I,M+1)**2 + FB(I,M+2)**2 )
              DO J = 3, M, 2
	        POWER2B = POWER2B + ( FB(I,J)**2 + FB(I,J+1)**2 )
              ENDDO
	    ENDDO
	    POWERB = POWER1B + 2*POWER2B

        DO J = 1, M+1, 2
          DO I = 1, N
            C(I,J)   = FA(I,J)*FB(I,J) + FA(I,J+1)*FB(I,J+1)
            C(I,J+1) = FA(I,J)*FB(I,J+1) - FA(I,J+1)*FB(I,J)
          ENDDO
        ENDDO

      ENDIF
C		FFT back to the image domain and rearrange 
C			matrix to put the DC in center
      CALL RFT2(C, M,N, -1, rftstat)
      if (rftstat.ne.1) goto 8100

      MHALF = M/2
      NHALF = N/2

      DO J = 1, MHALF
        DO I = 1, NHALF
          T = C(I,J)
          C(I,J) = C(I+NHALF,J+MHALF)
          C(I+NHALF,J+MHALF) = T
          T = C(I+NHALF,J)
          C(I+NHALF,J) = C(I,J+MHALF)
          C(I,J+MHALF) = T
        ENDDO
      ENDDO
C		Search for the correlation peak
      VMAX = C(1+NHALF, 1+MHALF)            ! INITIALIZE.
      IXMAX = 1 + NHALF
      JXMAX = 1 + MHALF
      DO J = 1, M
            DO I = 1, N
               RV = C(I,J)
               IF (VMAX .LT. RV) THEN
                  VMAX = RV
                  IXMAX = I
                  JXMAX = J
               ENDIF
            ENDDO
      ENDDO
C		Normalize the correlation value

      IF ( PHASE .EQ. 1.0 )  THEN
         IF (HPF)  THEN
            QUAL = VMAX/ ( (M-1)*(N-1) )
         ELSE
            QUAL = VMAX/ (M*N)
         END IF

      ELSE
         POWER = SQRT(POWERA*POWERB) 
         IF (POWER .GT. 0.0)  THEN
            QUAL = VMAX/POWER
         ELSE
            QUAL = VMAX
         END IF
      ENDIF
C		Calculate the offsets.

      IL = JXMAX - MHALF - 1
      IS = IXMAX - NHALF - 1

C...Test for correlation peak too near edge of matrix.

      IF ( IABS(IL) .GT. OFFMAX*MHALF .OR. IABS(IS) .GT. OFFMAX*NHALF )
     .     ISTATUS = 2

7000  RETURN

C...ERROR HANDLING

8100  CONTINUE
      ISTATUS = -1
      CALL ifmessage('ERROR IN FTCORR: INVALID M OR N VALUE')
      GOTO 7000

8200  CONTINUE
      ISTATUS = -1
      CALL ifmessage('ERROR IN FTCORR: INVALID PHASE VALUE')
      GOTO 7000

8300  CONTINUE
      ISTATUS = -1
      CALL ifmessage('ERROR IN FTCORR: INVALID PFILTER VALUE')
      GOTO 7000

8400  CONTINUE
      ISTATUS = -1
      CALL ifmessage('ERROR IN FTCORR: INVALID OFFMAX VALUE')
      GOTO 7000

      END


c **************************************************************
      SUBROUTINE REFINE(CORR,VLOFF,VSOFF,*)
      INTEGER*4 IPIV(6)
      REAL*4 CORR(3,3),A(9,6),B(9),S(6),AUX(12)
C
      DO 1 I=1,3
      Y = FLOAT(I)
      DO 1 J=1,3
      X = FLOAT(J)
      IQ = (I-1)*3+J
      A(IQ,1) = X*X
      A(IQ,2) = X*Y
      A(IQ,3) = Y*Y
      A(IQ,4) = X
      A(IQ,5) = Y
      A(IQ,6) = 1.
 1    B(IQ) = CORR(J,I)
      EPS = 1.E-7
      CALL LLSQ(A,B,9,6,1,S,IPIV,EPS,IER,AUX)
      IF (IER.NE.0) RETURN 1
      IF (S(1).EQ.0.) RETURN 1
      B2A = S(2)/(S(1)*2.)
      Y0 = (B2A*S(4)-S(5))/(2.*S(3)-B2A*S(2))-2.
      X0 = -B2A*(Y0+2.)-S(4)/(S(1)*2.)-2.
      IF (X0*X0+Y0*Y0.GE.4.) RETURN
      VLOFF = VLOFF+Y0
      VSOFF = VSOFF+X0
      RETURN
      END

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

      integer*4 winsl1,winss1,winsl2,winss2,winnl,winns
      real*8 datal(20),datar(20),coefx(4),coefy(4),a(4,4),b(4,4)
      real*4 lat,lon,xcorner(4),ycorner(4),conv(3600)

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
         call convev(ind,datal,datal,y,x,lat,lon,2,conv) 
         if(ind.ne.0)then
            call ifmessage('CONVEV: corner point off planet, abort')
            return
         endif
c        lat,lon -> x,y right image
         call convev(ind,datar,datar,y,x,lat,lon,1,conv) 
         if(ind.ne.0)then
            call ifmessage('CONVEV: corner point off planet, abort')
            return
         endif
         coefy(i)=y
         coefx(i)=x
      enddo
      call mve(8,16,a,b,1,1)

c fit polynomial to corner mappings.
      call dsimq(a,coefx,4,ind)
      if(ind.ne.0)then
         call ifmessage('DSIMQ: singular polynomial solution, abort')
         return
      endif
      call dsimq(b,coefy,4,ind)
      if(ind.ne.0)then
         call ifmessage('DSIMQ: singular polynomial solution, abort')
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
C
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



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create automatch.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM automatch

   To Create the build file give the command:

		$ vimake automatch			(VMS)
   or
		% vimake automatch			(Unix)


************************************************************************/


#define PROGRAM	automatch
#define R2LIB

#define MODULE_LIST automatch.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create automatch.pdf
PROCESS HELP=*
 PARM INP      TYPE=STRING  COUNT=(2:3)
 PARM OUT      TYPE=STRING  COUNT=1
 PARM PROJECT  TYPE=(STRING,5) COUNT=(0:1) DEFAULT=-- +
    VALID=("VGR-1","VGR-2","MAR10","MAR-9","VIKOR","GLL  ","CASSI")
 PARM CAMERA TYPE=INTEGER COUNT=(0:1) DEFAULT=--
 PARM CORR     TYPE=INTEGER COUNT=2 DEFAULT=(64,64)
 PARM PFILTER  TYPE=REAL    DEFAULT=0
 PARM PHASE    TYPE=REAL    DEFAULT=0
 PARM OFFMAX   TYPE=REAL    DEFAULT=0.8
 PARM FUNC     TYPE=(STRING,3) DEFAULT=""
 PARM REDOCORR TYPE=KEYWORD VALID=(REDO,NOREDO) DEFAULT=REDO
 PARM SUBPIX   TYPE=KEYWORD VALID=(SUBPIX,NOSUBPIX) DEFAULT=NOSUBPIX
 PARM PRINT    TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=PRINT
 PARM DIR      TYPE=(STRING,80) COUNT=1 DEFAULT=" "
 PARM OBJECT   TYPE=KEYWORD VALID=(IMAGE,OBJECT) DEFAULT=IMAGE
 PARM SEDR     TYPE=KEYWORD VALID=(SEDR) DEF=SEDR
 PARM NTPTS    TYPE=INTEGER COUNT=1 DEFAULT=8 VALID=(4:20)
 PARM TOLERANC TYPE=REAL COUNT=1 DEFAULT=2.0
 PARM FILENAME TYPE=(STRING,80) COUNT=(0:1) DEFAULT="NOFILE"
END-PROC

.TITLE
VICAR/IBIS Program AUTOMATCH

.HELP
PURPOSE
  AUTOMATCH is a program which automatically acquires tiepoints from
sets of images for the purpose of producing mosaics.
To obtain a more global view of the entire mosaicking process
you should look at the procedures in r3lib called:
MANUAL2.COM for Voyager, and MANUAL4.COM for Galileo. 
AUTOMATCH replaces MANMATCH in these procedures.

  Automatch is designed to generate tiepoints from sets of images which 
have been generated in a "burst" of frames where the scale & rotation
do not differ much between images. Provision is built into 
the program to compensate for differences between images by projecting
the correlation areas before correlation. This is limited however
to scale ranges of at most 2:1 and modest rotations which will
not confuse the overlap area algorithm. Automatch will NOT
work well on cases containing the limb.

EXECUTION

For VGR:
   AUTOMATCH  inp=(sedr.int,over.int,geoma.cor)  out=match.int + 
                   project=VGR-1 filename=LISTOFFILES

For GLL:
   automatch inp=(jupsedr.int,jupover.int) out=jupmatch.int +
         filename=LISTOFFILES project=GLL

.PAGE
USAGE:
AUTOMATCH is an batch program which has in the SEDR.INT file
the FDS or SCET times of all of the frames to inspect along
with all the navigation for these images.. 
You must create this file using the program IBISNAV

AUTOMATCH can obtain the names (and thus the locations) of all the
input files in one of two ways:

(recommended)
1. By the user specifying the parameter FILENAME which points to
   a file created by a text editor and which contains the full
   file names of each input picture in the order of priority, one
   name per record. This is the same file used in IBISNAV.
   So, if the first 2 pictures are called scx1:[jjl059]1234567.img
   and mipldisk:[mipl.gll]venus.img the first two records of
   FILENAME would contain:

   scx1:[jjl059]1234567.img
   mipldisk:[mipl.gll]venus.img

(obsolete)
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

The second input file 
contains the overlap frame pairs to present to the user.
This file is created by program MOSPLOT.

First input= SEDR.INT produced by program IBISNAV
Second input= OVER.INT produced by program MOSPLOT

Automatch will perform the following steps:

For each overlap pair:
1. Compute the overlap area in the right image by projecting the
   4 corners of the left image into the right image using the SEDR.
2. Select the correlation size to fit in the overlap area.
3. Select a row or column (whichever is the longest) through the
   overlap area and select a number of candidate tiepoint locations
   in the right image.
4. Compute the corresponding tiepoint positions in the left image
   using the SEDR.
5. Perform correlations.
6. Fit a 1st order polynomial to the tiepoints.
7. Iteratively reject all points (one at a time) exceeding a threshold
   pixel tolerance.
8. Select the 2 remaining points the furthest apart.
9. Place these in the output.

NOTE: 

The output file is the same as the file written by MANMATCH. So
if AUTOMATCH cannot process a certain number of overlap pairs you
can input the AUTOMATCH output into MANMATCH as the last input file
and go to the pairs in question to acquire points manually.

Be sure to inspect footprint plots of your mosaic generated by
MOSPLOT and to edit the overlap file with EDIBIS to delete 
extraneous & redundant overlaps to save time and later confusion.

At the moment automatch can accept up to 256 image pairs.

.PAGE
FILE STRUCTURE:
  The tiepoints file is an IBIS format tabular file. It consists
of 512 byte records where each column of data is written as
sequential records until exhausted. The next column begins at the
start of the next record etc. Record #1 contains the number of points
per column. All data is real*4 binary.

.LEVEL1
.VARIABLE INP
All inputs are IBIS tabular files.

First input= SEDR.INT produced
 by program IBISNAV
Second input= OVER.INT produced
 by program MOSPLOT
Third input=geoma parameters
(optional).

.VARIABLE OUT
The output IBIS tabular file.
nine columns output:
col1=first frame number of a pair
col2=second frame number of a pair
col3=first frame tiepoint line
col4=first frame tiepoint sample
col5=second frame tiepoint line
col6=second frame tiepoint sample
col7=1
col8=1
col9=1

.VARIABLE PROJECT
Batch parameter !
Specifies  the 
project. 

.VARIABLE CAMERA
Camera serial no.

.VARIABLE OBJECT
Causes images to be 
considered geometrically
corrected.

.VARIABLE PFILTER
The power of the FFT high pass
power law filter. Default 
is no power filter.
default is: pfilter=0.0 real

.VARIABLE PHASE
Degree of phase correlation:
0 for normal correlation. ( Default ).
1 for phase correlation

.VARIABLE OFFMAX
How far off center, correlation
peak is allowed (0 to 1). 
If the peak is more than
OFFMAX*window/2 pixels from 
the center it is rejected.
Default is 0.8

.VARIABLE FUNC
Pixel function before 
correlation is performed..
The options are: LOG or EXP.
DEfault is no funtion performed.

.VARIABLE REDO
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

.VARIABLE NTPTS
The number of 
tiepoints to collect
per pair.

.VARIABLE TOLERANC
The largest
permitted tiepoint
residual.

.VARIABLE PRINT
'NOPRINT to turn off 
terminal output

.VARIABLE DIR
Specifies the directory 
name where all the 
input files for the
mosaic reside.
(obsolete)

.VARIABLE FILENAME
Specifies a file name 
which contains the 
names of all the
images to be included 
in the mosaic

.VARIABLE NOPRINT
To suppress printout 
about each tiepoint 
displayed at the terminal

.VARIABLE CORR
Specifies the correlation size.
Is not supported now.

.VARIABLE SUBPIX
Specifies sub-pixel correlation.
Not tested or supported.

!NOSEDR option currently not supported for testing.
!.VARIABLE NOSEDR
!Limits reading of the 
!navigation file.
!Is not supported now.


.LEVEL2
.VARIABLE INP
All inputs are IBIS tabular files.

First input= SEDR.INT produced by program IBISNAV
  The first column contains the FDS times if the frames to mosaic.

Second input= OVER.INT produced by program MOSPLOT.
  Columns 1 and 2 contain the frame overlap numbers.
  Frame pairs are selected from the overlap numbers only.

A geom correction file can be entered as the third input
if the images are in image space (un geometrically corrected).

.VARIABLE OUT
The output IBIS tabular file.
nine columns output:
col1=first frame number of a pair
col2=second frame number of a pair
col3=first frame tiepoint line
col4=first frame tiepoint sample
col5=second frame tiepoint line
col6=second frame tiepoint sample
col7=1
col8=1
col9=1

.VARIABLE PROJECT
Specifies the project . This is required if
the images are un-geometrically corrected
and the project is GLL.

.VARIABLE CAMERA
Camera serial number.
Used in reading geoma correction parameters if 
nominals are to be used.
Ignored for GLL.

.VARIABLE OBJECT
Causes images to be considered geometrically corrected.
The geoma file will be ignored if present.

.VARIABLE PFILTER
The power of the FFT high pass
power law filter. Default is no power filter.
default is: pfilter=0.0 real

.VARIABLE PHASE
Degree of phase correlation:
0 for normal correlation. ( Default ).
1 for phase correlation

.VARIABLE OFFMAX
How far off center, correlation
peak is allowed (0 to 1). If the peak is more than
OFFMAX*window/2 pixels from the center it is rejected.
Default is 0.8

.VARIABLE FUNC
Pixel function before correlation is performed..
The options are: LOG or EXP.
DEfault is no funtion performed.

.VARIABLE REDO
Keyword for redo mode. Valid responses are:
REDO or NOREDO. The default is REDO.
Redo specifies that if the first correlation for
an area has an offset of less than the OFFMAX limit
but greater than zero the correlation is rerun
beginning at the new location.

.VARIABLE NTPTS
The number of tiepoints to collect per pair.
This initial set will be edited (see TOLERANCE) until only
good points remain. Fewer than 4 good points invalidates
this image pair.

.VARIABLE TOLERANC
The largest permitted tiepoint residual.
All the tiepoints are fitted to a polynomial model of the
form:
y1=Ax+By+C   x1=Dx+Ey+F
Points which disagree with the model by over TOLERANC
pixels will be rejected (one point rejected per fitting pass)
until all are within TOLERANC. If fewer than 4 points
remain the pair is rejected.

.VARIABLE PRINT
'NOPRINT to turn off terminal output

.VARIABLE DIR
Optional parameter required only if the FILENAME parameter
is NOT specified. 
Specifies the directory name where all the input files for the
mosaic reside. (obsolete)

.VARIABLE FILENAME
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

.VARIABLE NOPRINT
To suppress printout 
about each tiepoint 
displayed at the terminal

.VARIABLE CORR
Specifies the correlation size.
Is not supported now.

.VARIABLE SUBPIX
Specifies sub-pixel correlation.
Not tested or supported.

!NOSEDR option currently not supported for testing
!.VARIABLE NOSEDR
!Limits reading of the 
!navigation file.
!Is not supported now.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstautomatch.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let _onfail="continue"
let $autousage="none"
let $echo="no"
let $echo="yes"
!
if ($syschar(1) = "UNIX")
  ush cp /project/it/testdata/sitod1/test_data/gll/autosedr.int . 
  ush cp /project/it/testdata/sitod1/test_data/gll/autoover.int . 
  ush cp /project/test_work/testdata/gll/smisedr.int .
  ush cp /project/test_work/testdata/gll/smiover.int .
  ush cp /project/it/testdata/sitod1/test_data/gll/60.img .
  ush cp /project/it/testdata/sitod1/test_data/gll/57.img .
  ush cp /project/it/testdata/sitod1/test_data/gll/48.img .
  ush cp /project/it/testdata/sitod1/test_data/gll/42.img .
  ush cp /project/it/testdata/sitod1/test_data/gll/45.img .
  ush cp /project/test_work/testdata/gll/s0349666200.u .
  ush cp /project/test_work/testdata/gll/s0349666345.u .
else
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]autosedr.int * 
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]autoover.int * 
  dcl copy WMS_TEST_WORK:[TESTDATA.GLL]smisedr.int *
  dcl copy WMS_TEST_WORK:[TESTDATA.GLL]smiover.int *
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]60.img *
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]57.img *
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]48.img *
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]42.img *
  dcl copy WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]45.img *
  dcl copy WMS_TEST_WORK:[TESTDATA.GLL]s0349666200.u *
  dcl copy WMS_TEST_WORK:[TESTDATA.GLL]s0349666345.u *
end-if

automatch inp=(autosedr.int,autoover.int) +
          out=automatch.int project=GLL +
          filename=listoffiles.
ibis-list inp=automatch.int 

!  ******************************************
!  Summation Mode Images Test Portion
!  ****************************************** 
automatch inp=(smisedr.int,smiover.int) +
	out=smimatch.int project=GLL +
	filename=smifiles.
ibis-list inp=smimatch.int

!*******************Cassini Test****************************
!****will not work until xrt plot packager has been fixed****
!ibisnav out=cassedr.ibis filename=casimglist.vms target=jupiter project=cassi
!mosplot cassedr.ibis casover.int project=cassi
!automatch inp=(cassedr.int,casover.int) out=casmatch.int project=CASSI +
!          filename=casimglist.vms
!ibis-list inp=casmatch.int 
 
if ($syschar(1) = "UNIX")
   ush rm *.img 
   ush rm *.u
else
   dcl del *.img;*
   dcl del *.u;*
end-if

end-proc
$!-----------------------------------------------------------------------------
$ create listoffiles.
60.img
57.img
48.img
42.img
45.img
$!-----------------------------------------------------------------------------
$ create vmsfiles.
WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]60.img
WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]57.img
WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]48.img
WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]42.img
WMS_TEST_WORK:[TESTDATA.SITOD1.TEST_DATA.GLL]45.img
$!-----------------------------------------------------------------------------
$ create smifiles.
s0349666200.u
s0349666345.u
$!-----------------------------------------------------------------------------
$ create casimglist.vms
wms_test_work:[testdata.cassini.cas$i$ss]n1355064875.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355065283.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355065703.1
wms_test_work:[testdata.cassini.cas$i$ss]n1355066109.1
$!-----------------------------------------------------------------------------
$ create casimglist.unx
/project/test_work/testdata/cassini/casIss/n1355064875.1
/project/test_work/testdata/cassini/casIss/n1355065283.1
/project/test_work/testdata/cassini/casIss/n1355065703.1
/project/test_work/testdata/cassini/casIss/n1355066109.1
$ Return
$!#############################################################################
