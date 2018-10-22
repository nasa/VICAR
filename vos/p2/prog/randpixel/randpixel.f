	include 'VICMAIN_FOR'
	SUBROUTINE MAIN44
c
	implicit none
c
        integer*4 iunit,ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 nids,slshape,ssshape,stat
	integer*4 randseed
	logical*4 symmetry,list
	character*8 shape
	character*50 version
c
	data version/'*** RANDPIXEL version 2016-06-08 ***'/
c
        call xvmessage (version,' ')
        call parmproc  (iunit,ounit,nlo,nso,points,slshape,nlshape,
     1 ssshape,nsshape,shape,randfill,randseed,symmetry,nids,list)

	if (shape.eq.'square') Then
	    if (nids.eq.0) then
		if (symmetry) then
		    call subsqgen_symm (ounit,nlo,nso,points,
     1 	    slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
		else
		    call subsqgen (ounit,nlo,nso,points,
     1	    slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
		endif
	    else
		if (symmetry) then
		    call subsqgen_img_symm (iunit,ounit,nlo,nso,
     1	    points,slshape,nlshape,ssshape,nsshape,randfill,randseed,
     2	    list)
		else
		    call subsqgen_img (iunit,ounit,nlo,nso,points,
     1	    slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
		endif
	    endif
	endif
	if (shape.eq.'circle') Then
	    if (nids.eq.0) then
		if (symmetry) then
		    call subcircgen_symm (ounit,nlo,nso,points,
     1	    slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
		else
		    call subcircgen (ounit,nlo,nso,points,
     1	    slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
 		endif
	    else
		if (symmetry) then
		    call subcircgen_img_symm (ounit,nlo,
     1	    nso,points,slshape,nlshape,ssshape,nsshape,randfill,randseed,
     2	    list)
		else
		    call subcircgen_img (ounit,nlo,
     1	    nso,points,slshape,nlshape,ssshape,nsshape,randfill,randseed,
     2	    list)
		endif
	    endif
	endif
	if (nids.eq.1) then
		call xvclose (iunit,stat,' ')
		call chkstat (stat,'??E - XVclose error on input',0,0,0)
	endif
	call xvclose (ounit,stat,' ' )
	call chkstat (stat,'??E - XVclose error on output',0,0,0)

	return
	end
C=========================================================================
	subroutine parmproc  (iunit,ounit,nlo,nso,points,slshape,nlshape,
     1 ssshape,nsshape,shape,randfill,randseed,symmetry,nids,list)
C
C       ROUTINE TO PROCESS RANDPIXEL PARAMETERS
C
	implicit none
	integer*4 iunit,ounit,nli,nsi,nlo,nso,nlshape,nsshape,stat,cnt
	integer*4 nids,points,randfill,slshape,ssshape
	integer*4 randseed,totpix,icode
	logical*4 symmetry,list
	character*4 outfmt,infmt
	character*8 shape,symm,prtbl
	character*80 outline
c
	character*4 fmt(2)/'BYTE','HALF'/
	totpix=2048 * 2048
	call xvpcnt('INP',nids)     !get number of input data sets - no trailing ' '
	if (nids.gt.0) then
		call xvunit(iunit,'INP',1,stat,' ')
		call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT',
     1       'SA',' ')
		call chkstat (stat,'??E - XVopen error on input file',0,0,0)
		call xvget (iunit,stat,'NL',nli,'NS',nsi,'FORMAT',infmt,' ')
                if (infmt.eq.'HALF'.or.infmt.eq.'BYTE') go to 100
                write (outline,10100) infmt
10100 format ('??E - Input format ',a4,' illegal - only BYTE or HALF')
                call xvmessage (outline,' ')
                call abend
100	continue
		if (infmt.eq.'BYTE') icode=1
		if (infmt.eq.'HALF'.or.infmt.eq.'WORD') icode=2

		call xvclose(iunit,stat,' ')
		call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1	'I_FORMAT',fmt(icode),'U_FORMAT',fmt(2),' ')		!FMT(INCODE),' ')
		outfmt = infmt
		nlo = nli
		nso = nsi
		if (nlo*nso .gt. totpix) then
		    call xvmessage('??E - Total pixels gt 2048x2048',' ')
		    call abend
		endif
	endif

	if (nids .eq. 0) then
	    call xvp ('ONL',nlo,cnt)
	    if (nlo.eq.0) nlo=nli
	    if (nlo.eq.0) then
		write (outline,10200) nlo
10200 format ('??E - Output has no lines',' ')
                call xvmessage (outline,' ')
               call abend
	    endif
	    call xvp ('ONS',nso,cnt)
	    if (nso.eq.0) nso=nsi
	    if (nso.eq.0) then
		write (outline,10210) nso
10210 format ('??E - Output has no samples',' ')
               call xvmessage (outline,' ')
               call abend
	    endif
            if (nlo*nso .gt. totpix) then
                call xvmessage('??E - Total pixels gt 2048x2048',' ')
                call abend
            endif

	    call xvp ('OUTFMT',outfmt,cnt)
	    if (outfmt.eq.'HALF'.or.outfmt.eq.'BYTE') go to 220
	    if (outfmt.eq.'half'.or.outfmt.eq.'byte') go to 220
            write (outline,10220) outfmt
10220 format ('??E - Output format ',a4,' illegal - only BYTE or HALF')
            call xvmessage (outline,' ')
            call abend
	endif
220 	continue
c
	call xvunit(ounit,'OUT',1,stat,' ')
	call xvopen (ounit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     1       'U_FORMAT',fmt(2),'O_FORMAT',outfmt,'OP','WRITE',
     2       'U_NL',nlo,'U_NS',nso,'U_ORG','BSQ',' ')
	call chkstat (stat,'??E - XVopen error on output image',0,0,0)


	call xvp ('POINTS',points,cnt)
	symmetry=.false.
	call xvp ('SYMMETRY',symm,cnt)
	if (symm.eq.'SYMMETRY') symmetry=.true.
	call xvp ('SHAPE',shape,cnt)

	call xvp ('SLSHAPE',slshape,cnt)
	if (slshape.eq.0) slshape=1
	call xvp ('NLSHAPE',nlshape,cnt)
	if (nlshape.eq.0) nlshape=nlo
	call xvp ('SSSHAPE',ssshape,cnt)
	if (ssshape.eq.0) ssshape=1
	call xvp ('NSSHAPE',nsshape,cnt)
	if (nsshape.eq.0) nsshape=nso

	call xvp('RANDFILL',randfill,cnt)
	call xvp ('RANDSEED',randseed,cnt)

	list=.false.
	call xvp ('LIST',prtbl,cnt)
	if (prtbl.eq.'LIST') list=.true.
	return
	end
c==========================================================================
	subroutine subsqgen (ounit,nlo,nso,points,slshape,nlshape,
     1	ssshape,nsshape,randfill,randseed,list)
c
c
c	random selection of random points around square image
c
	implicit none
c	2048x2048=4194304
	integer*2 imgdata(4194304),shapedata(4194304)
	integer*4 randlist(2000000)
	integer*2 buffer(4194304)
	integer*4 ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 stat,totalpts,shapepts,slshape,ssshape,line
	integer*4 i,j,k,numsamp,chkval
	integer*4 randseed
	integer*8 dseed
	logical*4 list
	real*4 rand_num
	character*80 outline

c
	dseed=randseed
	shapepts=nlshape*nsshape
	totalpts=nlo*nso
c
c	put subimage array in its own array
c
c	call xvmessage ('Put subimage in its own array',' ')
	k=0
	do i=1,nsshape
	    do j=1,nlshape
		numsamp=nso*(slshape-1)+i + (ssshape-1)+j
		k=k+1
		shapedata(k)=numsamp
	    enddo
	enddo
c
c	Now pick random pixels
c
c	call xvmessage ('Picking random pixels',' ')
	if (list) 
     1 call xvmessage('point       seed  rand_num     chkval randlist',' ')

c	rangen parms are (long,float)

	do i=1,points
	    call rangen(dseed,rand_num)
	    chkval=int(rand_num*shapepts)
	    randlist(i)=chkval
	    if (list) then
	        write (outline,10210) i,dseed,rand_num,chkval,randlist(i)
10210 format (i4,1x,i12,1x,f11.8,1x,i10,i10)
		call xvmessage (outline,' ')
	    endif
	    dseed=chkval
	enddo
c
c	prepare output
c
c	call xvmessage ('Preparing output array',' ')
	do i=1,points
		imgdata(randlist(i))=randfill
	enddo
c	call xvmessage ('Writing output file',' ')
	line=0
	do i=1,nlo
	    line=i
	    do j=1,nso
	   	buffer(j)=imgdata(nso*(line-1)+j)
	    enddo
	    call xvwrit (ounit,buffer,stat,'LINE',line,'NSAMPS',nso,' ')
	    call chkstat (stat,'XVwrit error on output',0,0,0)
c		write (outline,10300) line
c10300 format ('Line = ',i6)
c		call xvmessage (outline,' ')
	enddo
c	call xvmessage ('DONE!',' ')
	return
	end
c========================================================================
	subroutine subsqgen_symm (ounit,nlo,nso,points,slshape,
     1 nlshape,ssshape,nsshape,randfill,randseed,list)
c
c	symmetric selection of random points around center of square
c	image
c
	implicit none
	integer*2 imgdata(4194304),shapedata(4194304)
	integer*4 randlist(2000000)
	integer*2 buffer(4194304)
	integer*4 ounit,nlo,nso,points,points2,nlshape,nsshape,randfill
	integer*4 stat,totalpts,shapepts,slshape,ssshape,line
	integer*4 i,j,k,numsamp,chkval
	integer*4 randseed
	integer*8 dseed
	logical*4 list
	real*4 rand_num
	character*80 outline

c
	dseed=randseed
	shapepts=nlshape*nsshape
	totalpts=nlo*nso
c
c	put subimage array in its own array
c
c	call xvmessage ('Put subimage in its own array',' ')
	k=0
	do i=1,nsshape
	    do j=1,nlshape
		numsamp=nso*(slshape-1)+i + (ssshape-1)+j
		k=k+1
		shapedata(k)=numsamp
	    enddo
	enddo
c
c	Now pick random pixels that are symmetric through center
c
c	call xvmessage ('Picking symmetric random pixels',' ')
	if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',' ')

c	rangen parms are (long,float)

	points2=points/2
	do i=1,points2
	    call rangen(dseed,rand_num)
	    chkval=int(rand_num*shapepts)
	    randlist(i)=chkval
	    randlist(i+points2-1)=nlo*nso-chkval+1
	    if (list) then
		  write (outline,10210) i,dseed,rand_num,chkval,randlist(i)
10210 format (i4,1x,i12,1x,f11.8,1x,i10,i10)
		  call xvmessage (outline,' ')
	    endif
	    dseed=chkval
	enddo
	if (list) then
	    do i=points2+1,points
	        write (outline,10210) i,dseed,rand_num,chkval,randlist(i)
		call xvmessage (outline,' ')
	    enddo
	endif
c
c	prepare output
c
cx	call xvmessage ('Preparing output array',' ')
	do i=1,points
	     imgdata(randlist(i))=randfill
	enddo
c	call xvmessage ('Writing output file',' ')
	line=0
	do i=1,nlo
		line=i
		do j=1,nso
			buffer(j)=imgdata(nso*(line-1)+j)
		enddo
		call xvwrit (ounit,buffer,stat,'LINE',line,'NSAMPS',nso,' ')
		call chkstat (stat,'XVwrit error on output',0,0,0)
c		write (outline,10300) line
c10300 format ('Line = ',i6)
c		call xvmessage (outline,' ')
	enddo
c	call xvmessage ('DONE!',' ')
	return
	end
c==========================================================================
	subroutine subsqgen_img (iunit,ounit,nlo,nso,points,
     1 slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
        integer*2 imgdata(4194304),shapedata(4194304)
        integer*4 randlist(4194304)
        integer*2 buffer(4194304)
	integer*2 iimage(4096)
	integer*4 iunit,ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape
	integer*4 i,j,k,numsamp,chkval,stat,line
	integer*4 randseed
	integer*8 dseed
	logical*4 list
	real*4 rand_num
c	character*8 shape
        character*80 outline
c
	dseed=randseed
	shapepts=nlshape*nsshape
	totalpts=nlo*nso

C   READ INPUT IMAGE
        do i = 1,nlo
	    call xvread (iunit,iimage,stat,'LINE',i,'NSAMPS',nso,' ')
	    do j=1,nso
		imgdata(nso*(i-1)+j) = iimage(j)
	    enddo
	enddo
c
c       put subimage array in its own array
c
c        call xvmessage ('Put subimage in its own array',' ')
        k=0
        do i=1,nsshape
            do j=1,nlshape
                 numsamp=nso*(slshape-1)+i + (ssshape-1)+j
                 k=k+1
                 shapedata(k)=numsamp
            enddo
        enddo
c
c       Now pick random pixels
c
c        call xvmessage ('Picking random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',' ')

c	rangen parms are (long,float)

        do i=1,points
            call rangen(dseed,rand_num)
            chkval=int(rand_num*shapepts)
            randlist(i)=chkval
            if (list) then
                   write (outline,10210) i,dseed,rand_num,chkval,randlist(i)
10210 format (i4,1x,i12,1x,f11.8,1x,i10,i10)
                   call xvmessage (outline,' ')
            endif
            dseed=chkval
        enddo
c
c       prepare output
c
c       call xvmessage ('Preparing output array',' ')
        do i=1,points
            imgdata(randlist(i))=randfill
        enddo
c        call xvmessage ('Writing output file',' ')
        line=0
        do i=1,nlo
            line=i
            do j=1,nso
                buffer(j)=imgdata(nso*(line-1)+j)
            enddo
            call xvwrit (ounit,buffer,stat,'LINE',line,'NSAMPS',nso,' ')
            call chkstat (stat,'XVwrit error on output',0,0,0)
c               write (outline,10300) line
c10300 format ('Line = ',i6)
c               call xvmessage (outline,' ')
        enddo


c	call xvmessage ('??E - Random square with input data not active',' ')
	return
	end
c==========================================================================
	subroutine subsqgen_img_symm (iunit,ounit,nlo,nso,points,
     1 slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
        integer*2 imgdata(4194304),shapedata(4194304)
        integer*4 randlist(4194304)
        integer*2 buffer(4194304)
        integer*2 iimage(4096)
	integer*4 iunit,ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape,points2
	integer*4 i,j,k,numsamp,chkval,stat
	integer*4 randseed,line
	integer*8 dseed
	logical*4 list
	real*4 rand_num
c	character*8 shape
	character*80 outline
c
	dseed=randseed
	shapepts=nlshape*nsshape
	totalpts=nlo*nso
C   READ INPUT IMAGE
        do i = 1,nlo
            call xvread (iunit,iimage,stat,'LINE',i,'NSAMPS',nso,' ')
            do j=1,nso
                imgdata(nso*(i-1)+j) = iimage(j)
            enddo
        enddo
c
c       put subimage array in its own array
c
c        call xvmessage ('Put subimage in its own array',' ')
        k=0
        do i=1,nsshape
            do j=1,nlshape
                numsamp=nso*(slshape-1)+i + (ssshape-1)+j
                k=k+1
                shapedata(k)=numsamp
            enddo
        enddo
c
c       Now pick random pixels that are symmetric through center
c
c        call xvmessage ('Picking symmetric random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',
     2' ')

c	rangen parms are (long,float)

        points2=points/2
        do i=1,points2
            call rangen(dseed,rand_num)
            chkval=int(rand_num*shapepts)
            randlist(i)=chkval
            randlist(i+points2-1)=nlo*nso-chkval+1
            if (list) then
                  write (outline,10210) i,dseed,rand_num,chkval,randlist(i)
10210 format (i4,1x,i12,1x,f11.8,1x,i10,i10)
                  call xvmessage (outline,' ')
            endif
            dseed=chkval
        enddo
c
c       prepare output
c
c        call xvmessage ('Preparing output array',' ')
        do i=1,points
            imgdata(randlist(i))=randfill
        enddo
c        call xvmessage ('Writing output file',' ')
        line=0
        do i=1,nlo
            line=i
            do j=1,nso
                buffer(j)=imgdata(nso*(line-1)+j)
            enddo
            call xvwrit (ounit,buffer,stat,'LINE',line,'NSAMPS',nso,' ')
            call chkstat (stat,'XVwrit error on output',0,0,0)
c              write (outline,10300) line
c10300 forma ('Line = ',i6)
c            call xvmessage (outline,' ')
        enddo

	return
	end
c==========================================================================
	subroutine subcircgen (ounit,nlo,nso,points,slshape,
     1	nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
        integer*4 ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape
	integer*4 randseed
	integer*8 dseed
	logical*4 list
c	character*8 shape
c
        dseed=randseed
        shapepts=nlshape*nsshape
        totalpts=nlo*nso

c        call xvmessage ('Picking random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',
     2' ')

	call xvmessage ('??E - Random circle not active',' ')

	return
	end
c==========================================================================
	subroutine subcircgen_symm (ounit,nlo,nso,points,slshape,
     1	nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
      integer*4 ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape
	integer*4 randseed
	integer*8 dseed
	logical*4 list
c	character*8 shape
c
        dseed=randseed
        shapepts=nlshape*nsshape
        totalpts=nlo*nso

c        call xvmessage ('Picking random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',
     2' ')

	call xvmessage ('??E - Random symmetric circle not active',' ')

	return
	end
c==========================================================================
	subroutine subcircgen_img (ounit,nlo,nso,points,slshape,
     1	nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
      integer*4 ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape
	integer*4 randseed
	integer*8 dseed
	logical*4 list
c	character*8 shape
c
        dseed=randseed
        shapepts=nlshape*nsshape
        totalpts=nlo*nso

c        call xvmessage ('Picking random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',
     2' ')

	call xvmessage ('??E - Random circle with input data not active',' ')

	return
	end
c==========================================================================
	subroutine subcircgen_img_symm (ounit,nlo,nso,points,
     1 slshape,nlshape,ssshape,nsshape,randfill,randseed,list)
c
	implicit none
      integer*4 ounit,nlo,nso,points,nlshape,nsshape,randfill
	integer*4 totalpts,shapepts,slshape,ssshape
	integer*4 randseed
	integer*8 dseed
	logical*4 list
c	character*8 shape
c
        dseed=randseed
        shapepts=nlshape*nsshape
        totalpts=nlo*nso

c        call xvmessage ('Picking random pixels',' ')
        if (list)
     1 call xvmessage('point       seed  rand_num     chkval randlist',
     2' ')

	call xvmessage ('??E - Random symm circle with input data not active',
     1 ' ')

	return
	end
