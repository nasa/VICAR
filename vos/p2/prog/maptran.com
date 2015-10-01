$!****************************************************************************
$!
$! Build proc for MIPL module maptran
$! VPACK Version 1.9, Monday, April 05, 2010, 11:45:07
$!
$! Execute by entering:		$ @maptran
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
$ write sys$output "*** module maptran ***"
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
$ write sys$output "Invalid argument given to maptran.com file -- ", primary
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
$   if F$SEARCH("maptran.imake") .nes. ""
$   then
$      vimake maptran
$      purge maptran.bld
$   else
$      if F$SEARCH("maptran.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maptran
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maptran.bld "STD"
$   else
$      @maptran.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maptran.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maptran.com -mixed -
	-s maptran.f -
	-i maptran.imake -
	-p maptran.pdf -
	-t tstmaptran.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maptran.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program maptran
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (ngrids=70000,npixels=1000000,npixout=1000000)
c ngrids=   number of possible output grid intersections stored.
c npixels=  number of input pixels that can be stored at one time.
c npixout=  number of output pixels stored at one time.
      byte area_status(ngrids)
      real*4 image(npixels) ! row order
      real*4 out(npixout)   ! row order
      integer*4 status,outunit,count,def,tempunit,sb,eb
      integer*4 idata1(40),idata2(40)
      real*4 data1(40),data2(40),conv(1),code,dnthresh,dninter
      real*4 grid(2,ngrids)   ! line,sample,index
      real*4 latlon(2,ngrids) ! lat,lon    ,index
      real*8 coefx(4),coefy(4),a(4,4),aa(4,4)
      real*8 data81,data82
      real*8 rline_top_out,rline_bot_out
      real*8 risamp_left_out,risamp_right_out
      character*80 msg
      character*8 org
      character*7 fmt
      logical xvptst,interpolate,mosaic,check_wraparound
      equivalence (data81,data1,idata1),(data82,data2,idata2)
      equivalence (latlon,image)

      real*8 mp

      call xvmessage(' **** MAPTRAN version 26-Mar-10 ****',' ')

c parameters
      call xvparm('NL',nlout,count,def,1)
      call xvparm('NS',nsout,count,def,1)
      call xvparm('INC',inc,count,def,1)
      call xvparm('RANGE',added_range,count,def,1)
      call xvparm('THRESH',dnthresh,count,def,1)
      call xvparm('DNINTER',dninter,count,def,1)
      call xvparm('CODE',code,count,def,1)
      interpolate=xvptst('INTERPOL')
      mosaic=xvptst('MOSAIC')
      check_wraparound=xvptst('CHECK')


c initialize mp routines - bam 1/96

      call mp_init(mp,status)
      if ( status .ne. 0 ) then
          call xvmessage ('MP error on init',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if

c read the input labels on the map projection.
      call xvunit(input1,'INP',1,status,' ')
      call xvopen(input1,status,'IO_ACT','AS','OPEN_ACT','AS',
     +            'U_FORMAT','REAL',' ')
      call xvget(input1,status,'NL',nl,'NS',ns,'ORG',org,
     1 'FORMAT',fmt,' ')
      rnl=nl
      rns=ns
      call xvbands( sb, nbout, nb)
      ! nb is from input label
      ! nbout is from param NB or BANDS, whichever is non-zero;  else zero
      if (nb.gt.1 .and. org.eq.'BIP') 
     1  call mabend(' BIP not supported, use pgm TRAN to convert')
      if ((sb+nbout-1).gt.nb) then
        call xvmessage(' NB too large, reduced to fit input',' ')
        nbout = nb-sb+1
      endif
      if (nbout.le.0) nbout = nb-sb+1
      eb = sb+nbout-1

c     for unix port - bam 1/96
c
c     call searcv2(input1,x,x,data1,data1)

      call mp_label_read(mp,input1,status)
      if ( status .eq. 0 ) then

          call mp_mpo2buf(mp,data1,status)
          if ( status .ne. 0 ) then 
                  call xvmessage (' Error in mp label read',' ')
                  call xvmessage (' Program terminated.',' ')
              call abend
          end if

      end if
!         	end searcv2 code


      if(idata1(39).eq.7.or.idata1(39).eq.8)then
         call xvmessage('Input image is not a map projection',' ')
         call abend
      endif      

c Should we check for wraparound ?
      if(check_wraparound)then
        if(idata1(39).eq.6.or.idata1(39).eq.9.or.idata1(39).eq.10)then
          check_wraparound=.true.
          circumference_pixels=2.0*3.1415926*data1(26)/data1(7)
        else
          check_wraparound=.false.
        endif
      endif

c read the output labels on the map projection.
      call xvunit(input2,'INP',2,status,' ')
      call xvopen(input2,status,'IO_ACT','AS','OPEN_ACT','AS',
     +            'U_FORMAT','REAL',' ')
      call xvget(input2,status,'NL',nlref,'NS',nsref,' ')
      if(nlout.eq.0) nlout=nlref
      if(nsout.eq.0) nsout=nsref

c     for unix port - bam 1/96
c
c     call searcv2(input2,x,x,data2,data2)

      call mp_label_read(mp,input2,status)
      if ( status .eq. 0 ) then

          call mp_mpo2buf(mp,data2,status)
          if ( status .ne. 0 ) then 
                  call xvmessage (' Error in mp label read',' ')
                  call xvmessage (' Program terminated.',' ')
              call abend
          end if

      end if
!         	end searcv2 code


      if(idata2(39).eq.7.or.idata2(39).eq.8)then
         call xvmessage('Output image is not a map projection',' ')
         call abend
      endif      

c compute the INC value (grid spacing in pixels on the output)
      do i=inc,10000
        if((nlout/i+2)*(nsout/i+2).lt.ngrids) goto 10
      enddo
10    inc=i
      nlgrid=nlout/inc+1
      if((nlgrid-1)*inc+1.le.nlout) nlgrid=nlgrid+1
      nsgrid=nsout/inc+1
      if((nsgrid-1)*inc+1.le.nsout) nsgrid=nsgrid+1
      write(msg,20) inc
20    format(' grid spacing=',i3,' pixels')
      call xvmessage(msg,' ')

c check that enough room resides in output buffer OUT to accomodate
c INC output lines.
      if(nbout*nsout*inc.gt.npixout)then
         call xvmessage('Output image buffer too small to ',' ')
         call xvmessage('accomodate INC output lines. ',' ')
         call abend
      endif      


c compute the input line,sample for each output grid point.
      k=0
      do l=1,nlgrid
        rline=(l-1)*inc+1
        do i=1,nsgrid
          rsamp=(i-1)*inc+1            
          k=k+1
c         compute grid intersection lat,lon
          call convev(ind,data2,data2,rline,rsamp,rlat,rlon,2,conv)
          if(ind.eq.0)then
            latlon(1,k)=rlat
            latlon(2,k)=rlon
c           compute input image grid intersection line,sample
            call convev(ind,data1,data1,y,x,rlat,rlon,1,conv)
            if(ind.eq.0)then  ! point is visible
              if(check_wraparound)then
c               Reset pixels outside input to input for 3 projections.
                call wraparound(x,rns,circumference_pixels)
              endif
              grid(1,k)=y
              grid(2,k)=x
            else
              grid(1,k)=-9999.    ! error code
            endif
          else
            grid(1,k)=-9999.      ! error code
          endif
        enddo
      enddo


c set status of each grid area
      r1=nl/2
      r2=ns/2
      k=0
      do l=1,nlgrid-1
        do i=1,nsgrid-1
          k=k+1

c         compute number of vertices on the visible planet for area k
          n=0
          m1=(l-1)*nsgrid+i
          if(grid(1,m1).ne.-9999.) n=n+1
          if(grid(1,m1+1).ne.-9999.) n=n+1
          m2=l*nsgrid+i
          if(grid(1,m2).ne.-9999.) n=n+1
          if(grid(1,m2+1).ne.-9999.) n=n+1
          area_status(k)=n             ! # visible corners on planet

c         compute number of vertices on the input image area
          if(area_status(k).eq.4)then
            n=0
            if((grid(1,m1).ge.1.0).and.(grid(1,m1).le.rnl).and.
     +         (grid(2,m1).ge.1.0).and.(grid(2,m1).le.rns)) n=n+1
            if((grid(1,m1+1).ge.1.0).and.(grid(1,m1+1).le.rnl).and.
     +         (grid(2,m1+1).ge.1.0).and.(grid(2,m1+1).le.rns)) n=n+1
            if((grid(1,m2).ge.1.0).and.(grid(1,m2).le.rnl).and.
     +         (grid(2,m2).ge.1.0).and.(grid(2,m2).le.rns)) n=n+1
            if((grid(1,m2+1).ge.1.0).and.(grid(1,m2+1).le.rnl).and.
     +         (grid(2,m2+1).ge.1.0).and.(grid(2,m2+1).le.rns)) n=n+1
            if(n.eq.0) area_status(k)=0  ! set area to zero
          endif

c         if the corners appear to wrap around the input image then
c         set the status for individual pixel computation.
          if(area_status(k).eq.4)then
            n=0
            if(abs(grid(1,m1)-grid(1,m1+1)).gt.r1) n=1
            if(abs(grid(1,m2)-grid(1,m2+1)).gt.r1) n=1
            if(abs(grid(2,m1)-grid(2,m1+1)).gt.r2) n=1
            if(abs(grid(2,m2)-grid(2,m2+1)).gt.r2) n=1
            if(abs(grid(1,m1)-grid(1,m2)).gt.r1) n=1
            if(abs(grid(2,m1)-grid(2,m2)).gt.r2) n=1
            if(abs(grid(1,m1+1)-grid(1,m2+1)).gt.r1) n=1
            if(abs(grid(2,m1+1)-grid(2,m2+1)).gt.r2) n=1
            if(n.eq.1) area_status(k)=1  ! exact solution for each point
          endif
        enddo
      enddo

c review of area_status impact:
c If status=0    set output dn's to zero in the area.
c If status<4    compute each pixel exactly in output.
c If status=4    compute polynomial mapping and use it to get pixel location.

c open output
      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'U_NL',nlout,'U_NS',nsout,'U_NB',nbout,
     +            'OP','WRITE','OPEN_ACT','AS',' ')

c Add new projection to output label.
c Notes: 1. Array latlon is lost from this point on. It was kept in case
c           you needed to know lat & lon.


!         Ported to UNIX - eliminated maplabv2
!        	inserted mp routines    bam 10/95

!      call maplabv2(data2,data2,outunit,i,image)


      call mp_buf2mpo( data2, mp, status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP error on output data transfer',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if

      call mp_label_write( mp, outunit, 'HISTORY', status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP error on label write',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if

      call mp_label_write( mp, outunit, 'PROPERTY', status )
      if ( status .ne. 0 ) then 
          call xvmessage ('MP error on label write',' ')
          call xvmessage ('Program terminated',' ')
          return
      end if


c Open intermediate file used to store image as it builds up. 
c This file is deleted on close.
      call xvunit(tempunit,'NEW',1,status,'U_NAME','maptran.temp',' ')
      call xvopen(tempunit,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'U_NL',nlout,'U_NS',nsout,'U_NB',nbout,
     +            'O_FORMAT','REAL','COND','NOLABELS',
     +            'OP','WRITE','OPEN_ACT','AS',' ')
      do l=1,nsout
        out(l)=code
      enddo
c format it 
      if (org.eq.'BSQ') then
        do ib=1,nbout
          do il=1,nlout
            call xvwrit(tempunit,out,status,'LINE',il,'BAND',ib,' ')
          enddo
        enddo
      else        ! BIL
        do il=1,nlout
          do ib=1,nbout
            call xvwrit(tempunit,out,status,'LINE',il,'BAND',ib,' ')
          enddo
        enddo
      endif
      call xvclose(tempunit,status,' ')
c reopen for update & deletion when completed.
      call xvopen(tempunit,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'CLOS_ACT','DELETE',
     +            'U_NL',nlout,'U_NS',nsout,'U_NB',nbout,
     +            'I_FORMAT','REAL','COND','NOLABELS',
     +            'OP','UPDATE','OPEN_ACT','AS',' ')


c Compute number of lines from input storable in array IMAGE at one time.
      nlblock=npixels/(ns*nbout)
      if (nlblock.lt.1) call mabend(' input image buffer too small')
      if(nlblock.gt.nl) nlblock=nl


C From this point on we process input lines by blocks, processing the
c whole output for each input block.

      npasses=0                ! number of passes through output (1 to n)
      npasses_input=0          ! number of passes through input (1 or 2)
310   line_top=-nlblock+2      ! redo input image
300   number_unset_pixels=0    ! redo output image


      line_top=line_top + nlblock-1    ! top line of input block
      line_bot=line_top + nlblock-1    ! bottom line of input block
      if(line_bot.gt.nl) line_bot=nl
      nl_block_in=line_bot - line_top +1
      rline_bot=line_bot
      rline_top=line_top
      
c     load a block of input lines into array IMAGE.
      i=1
      do ib=sb,eb
        do il=line_top,line_bot
          call xvread(input1,image(i),status,'LINE',il,'BAND',ib,' ')
          i=i+ns
        enddo
      enddo
      do i=1,nl_block_in * ns * nbout
        if(image(i).eq.code) image(i)=code+1
      enddo

c     Process entire output image.

c     Row loop on the output grid
      k=0
      do 100 lgrid=1,nlgrid-1
        line_top_out=(lgrid-1)*inc+1               ! top line in output
        line_bot_out=line_top_out + inc -1         ! bot line in output
        rline_bot_out=line_bot_out+1               ! leave here
        if(line_bot_out.gt.nlout) line_bot_out=nlout
        nl_block_out=line_bot_out - line_top_out +1
        rline_top_out=line_top_out

c       read in INC lines from the temporary file maptran.temp  .
        j=1
        do ib=1,nbout
          do il=line_top_out,line_bot_out
            call xvread(tempunit,out(j),status,'LINE',il,'BAND',ib,' ')
            j=j+nsout
          enddo
        enddo        

c       Column loop on output grid
        do 200 igrid=1,nsgrid-1
          k=k+1

c         skip area if already completed.
          if(area_status(k).eq.-1) goto 200

          isamp_left_out=(igrid-1)*inc+1            ! left output samp
          isamp_right_out=isamp_left_out + inc -1   ! right output samp
          risamp_right_out=isamp_right_out+1        ! leave here
          if(isamp_right_out.gt.nsout) isamp_right_out=nsout
          m1=(lgrid-1)*nsgrid+igrid             ! top left grid pt    
          m2=lgrid*nsgrid+igrid                 ! bot left grid pt    
          risamp_left_out=isamp_left_out


c         fill area with zeroes if:
c         all 4 vertices are off planet or if
c         all the vertices are not visible on the planet or if
c         all vertices fall off input picture.
          if(area_status(k).eq.0) then
            do ib=1,nbout
              do il=1,nl_block_out
                n=(ib-1)*nsout*nl_block_out+(il-1)*nsout
                do i=isamp_left_out,isamp_right_out
                  out(n+i)=0
                enddo
              enddo
            enddo
            area_status(k)=-1       ! mark done
            goto 200
          endif


c         fit a polynomial mapping the output to the input and use the
c         coefficients to get the input dn location for each output pixel.
          if(area_status(k).eq.4)then

c           compute number of vertices on the input block.
            if(npasses_input.eq.0)then  ! invoke on first pass only
              n=0
              if((grid(1,m1).gt.rline_top)
     +        .and.(grid(1,m1).lt.rline_bot)
     +        .and.(grid(2,m1).gt.1.)
     +        .and.(grid(2,m1).lt.rns)) n=n+1
              if((grid(1,m2).gt.rline_top)
     +        .and.(grid(1,m2).lt.rline_bot)
     +        .and.(grid(2,m2).gt.1.)
     +        .and.(grid(2,m2).lt.rns)) n=n+1
              if((grid(1,m1+1).gt.rline_top)
     +        .and.(grid(1,m1+1).lt.rline_bot)
     +        .and.(grid(2,m1+1).gt.1.)
     +        .and.(grid(2,m1+1).lt.rns)) n=n+1
              if((grid(1,m2+1).gt.rline_top)
     +        .and.(grid(1,m2+1).lt.rline_bot)
     +        .and.(grid(2,m2+1).gt.1.)
     +        .and.(grid(2,m2+1).lt.rns)) n=n+1
c             skip area if all 4 vertices are off the input block
              if(n.eq.0) goto 200          
            endif


c           load matrices to compute polynomial fits
            a(1,1)=rline_top_out*risamp_left_out
            a(1,2)=rline_top_out
            a(1,3)=risamp_left_out
            a(1,4)=1.d0
            a(2,1)=rline_top_out*risamp_right_out
            a(2,2)=rline_top_out
            a(2,3)=risamp_right_out
            a(2,4)=1.d0
            a(3,1)=rline_bot_out*risamp_left_out
            a(3,2)=rline_bot_out
            a(3,3)=risamp_left_out
            a(3,4)=1.d0
            a(4,1)=rline_bot_out*risamp_right_out
            a(4,2)=rline_bot_out
            a(4,3)=risamp_right_out
            a(4,4)=1.d0
            call mve(8,16,a,aa,1,1) ! save a

c           to left image line
            coefy(1)=grid(1,m1)
            coefy(2)=grid(1,m1+1)
            coefy(3)=grid(1,m2)
            coefy(4)=grid(1,m2+1)
            call dsimq(a,coefy,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               area_status(k)=1
               goto 400
            endif

c           to left image sample
            call mve(8,16,aa,a,1,1) ! restore a
            coefx(1)=grid(2,m1)
            coefx(2)=grid(2,m1+1)
            coefx(3)=grid(2,m2)
            coefx(4)=grid(2,m2+1)
            call dsimq(a,coefx,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               area_status(k)=1
               goto 400
            endif

c           compute pixel values in left image
            do ib=1,nbout
              do il=1,nl_block_out
                rline=line_top_out+il-1		! right image line
                m=(ib-1)*nsout*nl_block_out+1
                n=m+(il-1)*nsout-1
                m1=(ib-1)*ns*nl_block_in+1
                do i=isamp_left_out,isamp_right_out
                  if(out(n+i).eq.code)then
                    rsamp=i			! right image sample

c                   compute the x,y position in the left image.
                    y=coefy(1)*dble(rline)*rsamp+coefy(2)*rline+
     +                coefy(3)*rsamp+coefy(4)
                    x=coefx(1)*dble(rline)*rsamp+coefx(2)*rline+
     +                coefx(3)*rsamp+coefx(4)

c                   Reset pixels outside input to input for 3 projections.
                    call wraparound(x,rns,circumference_pixels)

c                   compute output dn value
                    call set_dn_value(y,x,line_top,nl_block_in,
     +               ns,nl,nsout,il,i,interpolate,out(m),image(m1),
     +               dninter)
                  endif
                enddo
              enddo
            enddo

c           check if area is all filled by good pixels.
            ncount=0
            do ib=1,nbout
              do il=1,nl_block_out
                m=(ib-1)*nsout*nl_block_out+(il-1)*nsout
                do i=isamp_left_out,isamp_right_out
                  if(out(m+i).eq.code) ncount=ncount+1
                enddo
              enddo
            enddo
            if(ncount.eq.0) area_status(k)=-1  ! mark done
            goto 200
          endif

c         process each pixel independently.
400       if(area_status(k).lt.4)then

c           check if a good point is within the input line block +-
c           a border added_range. Note this doesen't guarantee
c           the input pixel is in memory but it excludes a lot of
c           excess computation. On the next input pass this check
c           is dropped.
            if(npasses_input.eq.0)then      ! only on first pass
              if(grid(1,m1).ne.-9999.)then
                if((grid(1,m1).gt.rline_top - added_range)
     +          .and.(grid(1,m1).lt.rline_bot + added_range)
     +          .and.(grid(2,m1).gt. -added_range)
     +          .and.(grid(2,m1).lt. rns + added_range)) goto 410
              endif
              if(grid(1,m1+1).ne.-9999.)then
                if((grid(1,m1+1).gt.rline_top - added_range)
     +          .and.(grid(1,m1+1).lt.rline_bot + added_range)
     +          .and.(grid(2,m1+1).gt. -added_range)
     +          .and.(grid(2,m1+1).lt. rns + added_range)) goto 410
              endif
              if(grid(1,m2).ne.-9999.)then
                if((grid(1,m2).gt.rline_top - added_range)
     +          .and.(grid(1,m2).lt.rline_bot + added_range)
     +          .and.(grid(2,m2).gt. -added_range)
     +          .and.(grid(2,m2).lt. rns + added_range)) goto 410
              endif
              if(grid(1,m2+1).ne.-9999.)then
                if((grid(1,m2+1).gt.rline_top - added_range)
     +          .and.(grid(1,m2+1).lt.rline_bot + added_range)
     +          .and.(grid(2,m2+1).gt. -added_range)
     +          .and.(grid(2,m2+1).lt. rns + added_range)) goto 410
              endif
              goto 200  ! no points close to input data block
            endif

410         do ib=1,nbout
              do il=1,nl_block_out
                rline=line_top_out+il-1
                m=(ib-1)*nsout*nl_block_out+1
                n=m+(il-1)*nsout-1
                m1=(ib-1)*ns*nl_block_in+1
                do i=isamp_left_out,isamp_right_out
                  if(out(n+i).eq.code)then
                    rsamp=i
c                   compute lat,lon
                    call convev(ind,data2,data2,rline,rsamp,rlat,rlon,
     +                      2,conv)
                    if(ind.eq.0)then
c                     compute input image line,sample
                      call convev(ind,data1,data1,y,x,rlat,rlon,
     +                        1,conv)
                      if(ind.eq.0)then  ! point is visible

c                       Reset pixels outside input to input for 3 project
                        call wraparound(x,rns,circumference_pixels)

c                       compute output dn value
                        call set_dn_value(y,x,line_top,nl_block_in,
     +                   ns,nl,nsout,il,i,interpolate,out(m),image(m1),
     +                   dninter)
                      else
                        out(n+i)=0  ! not on visbl part of planet input
                      endif
                    else
                      out(n+i)=0    ! not on planet in output image
                    endif
                  endif
                enddo
              enddo
            enddo

c           check if area is all filled by good pixels.
            ncount=0
            do ib=1,nbout
              do il=1,nl_block_out
                m=(ib-1)*nsout*nl_block_out+(il-1)*nsout
                do i=isamp_left_out,isamp_right_out
                  if(out(m+i).eq.code) ncount=ncount+1
                enddo
              enddo
            enddo
            if(ncount.eq.0) area_status(k)=-1  ! mark done
          endif

200     continue         ! end of grid column loop

c       Count number of unset pixels in output block.
        do i=1,nl_block_out*nsout*nbout
          if(out(i).eq.code) number_unset_pixels=number_unset_pixels+1
        enddo

c       write a block of output lines
        j=1
        do ib=1,nbout
          do il=line_top_out,line_bot_out
            call xvwrit(tempunit,out(j),status,'LINE',il,'BAND',ib,' ')
            j=j+nsout
          enddo
        enddo

100   continue           ! end of grid row loop
      npasses=npasses+1

c     return for another block of input lines
      if(line_bot.lt.nl) goto 300        ! not done with input

c     return for a complete rerun of the input in case there are still
c     pixels in the output not set.
      if((number_unset_pixels.gt.0).and.(npasses_input.eq.0)) then
        npasses_input = npasses_input +1
        call xvmessage('Missed some output values, redo input',' ')
        goto 310
      endif

c     Write output file in one of two ways:
      if(mosaic)then
c       Mosaic the second input and the intermediate file.
        do ib=1,nbout
          do il=1,nlout
            call xvread(input2,image,status,'LINE',il,'BAND',ib,' ')
            call xvread(tempunit,out,status,'LINE',il,'BAND',ib,' ')
	    ! if actual data are byte, then check to ensure that output is
	    ! in byte range
            if (fmt.eq.'BYTE') then
              do i=1,nsout
                if(out(i).lt.0.0) out(i)=0.0
                if(out(i).gt.255.0) out(i)=255.0
              enddo
            endif
            do i=1,nsout
              if(image(i).gt.dnthresh) out(i)=image(i)
            enddo
            call xvwrit(outunit,out,status,'LINE',il,'BAND',ib,' ')
          enddo
        enddo
      else
c       Copy intermediate file into the output and repack it.
        do ib=1,nbout
          do il=1,nlout
            call xvread(tempunit,out,status,'LINE',il,'BAND',ib,' ')
            if (fmt.eq.'BYTE') then
              do i=1,nsout
                if(out(i).lt.0.0) out(i)=0.0
                if(out(i).gt.255.0) out(i)=255.0
              enddo
            endif
            call xvwrit(outunit,out,status,'LINE',il,'BAND',ib,' ')
          enddo
        enddo
      endif

      write(msg,30) npasses
30    format(' ',i4,' passes through output required')
      call xvmessage(msg,' ')
      write(msg,31) npasses_input + 1
31    format(' ',i4,' passes through input required')
      call xvmessage(msg,' ')
      if(npasses.gt.2)then
        call xvmessage('You can decrease the number of passes by',' ')
        call xvmessage('recompiling with NPIXELS set higher.',' ')
      endif

      return
      end

c********************************************************************
c routine lifted from old maptrans to reset a sample value which wraps
c out of the input image back into the input image.
c sample is input sample value, reset on output.
c rns is the input line length in pixels (input)
c circumference_pixels is the planet circumference in pixels (input)
      subroutine wraparound(sample,rns,circumference_pixels)
      if(sample.gt.rns.and.sample.gt.circumference_pixels)then
        sample=sample-circumference_pixels
      endif
      if(sample.lt.1.0)then
        sample=sample+circumference_pixels
      endif
      return
      end

c********************************************************************
c routine to determine output dn value.
c line_top= top line of input line block
c nl_block_in= number of lines in input block
c nsout=number samples/output line
c ns=number samples/input line
c nl=number lines input image
c i=output sample
c l=output line in output block
c rline=input line
c rsamp=input sample
c dninter=input dn's  <= dninter are not used in interpolation.

      subroutine set_dn_value(rline,rsamp,line_top,
     +                  nl_block_in,ns,nl,nsout,l,i,
     +                  interpolate,out,image,dninter)
      logical interpolate
      real*4 out(nsout,1),image(ns,1)
      real*4 line(3),samp(3),dnval(3),dninter

      if(interpolate)then

c       check if is in input image block
        rl=rline - line_top +1
        if((rsamp.gt.1.).and.(rsamp.lt.ns))then
         if((rl.gt.1.).and.(rl.lt.nl_block_in))then

c         point is inside input block of nl_block_in lines
c         count number of the 4 adjacent pixels which are > dninter
          il=rline - line_top +1
          is=rsamp
          n=0
          if(image(is,il).gt.dninter) n=n+1
          if(image(is+1,il).gt.dninter) n=n+1          
          if(image(is,il+1).gt.dninter) n=n+1          
          if(image(is+1,il+1).gt.dninter) n=n+1          

c         bilinear interpolation
          if(n.eq.4)then
            wl=rline-int(rline)
            ws=rsamp-is
            top=image(is+1,il)*ws+image(is,il)*(1.0-ws)
            bot=image(is+1,il+1)*ws+image(is,il+1)*(1.0-ws)
            out(i,l)=bot*wl+top*(1.0-wl)
            return
          endif

c         fill in missing point and do bilinear interpolation.
          if(n.eq.3)then
            if(image(is,il).le.dninter)then
              save=image(is,il)
              image(is,il)=image(is+1,il)+image(is,il+1)
     +                    -image(is+1,il+1)
              wl=rline-int(rline)
              ws=rsamp-is
              top=image(is+1,il)*ws+image(is,il)*(1.0-ws)
              bot=image(is+1,il+1)*ws+image(is,il+1)*(1.0-ws)
              out(i,l)=bot*wl+top*(1.0-wl)
              image(is,il)=save
              return
            endif
            if(image(is+1,il).le.dninter)then
              save=image(is+1,il)
              image(is+1,il)=image(is,il)+image(is+1,il+1)
     +                    -image(is,il+1)
              wl=rline-int(rline)
              ws=rsamp-is
              top=image(is+1,il)*ws+image(is,il)*(1.0-ws)
              bot=image(is+1,il+1)*ws+image(is,il+1)*(1.0-ws)
              out(i,l)=bot*wl+top*(1.0-wl)
              image(is+1,il)=save
              return
            endif
            if(image(is,il+1).le.dninter)then
              save=image(is,il+1)
              image(is,il+1)=image(is,il)+image(is+1,il+1)
     +                    -image(is+1,il)
              wl=rline-int(rline)
              ws=rsamp-is
              top=image(is+1,il)*ws+image(is,il)*(1.0-ws)
              bot=image(is+1,il+1)*ws+image(is,il+1)*(1.0-ws)
              out(i,l)=bot*wl+top*(1.0-wl)
              image(is,il+1)=save
              return
            endif
            if(image(is+1,il+1).le.dninter)then
              save=image(is+1,il+1)
              image(is+1,il+1)=image(is+1,il)+image(is,il+1)
     +                    -image(is,il)
              wl=rline-int(rline)
              ws=rsamp-is
              top=image(is+1,il)*ws+image(is,il)*(1.0-ws)
              bot=image(is+1,il+1)*ws+image(is,il+1)*(1.0-ws)
              out(i,l)=bot*wl+top*(1.0-wl)
              image(is+1,il+1)=save
              return
            endif
          endif

c         Only one or two points remain > dninter
c         find them
          n=0
          if(image(is,il).gt.dninter) then
            n=n+1          
            line(n)=il
            samp(n)=is
            dnval(n)=image(is,il)
          endif
          if(image(is+1,il).gt.dninter) then
            n=n+1          
            line(n)=il
            samp(n)=is+1
            dnval(n)=image(is+1,il)
          endif
          if(image(is,il+1).gt.dninter) then          
            n=n+1          
            line(n)=il+1
            samp(n)=is
            dnval(n)=image(is,il+1)
          endif
          if(image(is+1,il+1).gt.dninter) then
            n=n+1          
            line(n)=il+1
            samp(n)=is+1
            dnval(n)=image(is+1,il+1)
          endif

          if(n.eq.2)then            ! two neighbors > dninter
            r1=abs(rl-line(1))+abs(rsamp-samp(1))
            r2=abs(rl-line(2))+abs(rsamp-samp(2))
            out(i,l)=(dnval(1)*r2+dnval(2)*r1)/(r1+r2)
          else if(n.eq.1)then            ! one neighbor > dninter
            out(i,l)=dnval(1)
          else                           ! no neighbors > dninter
c           nearest neighbor
            il=nint(rline) - line_top + 1
            is=nint(rsamp)
            out(i,l)=image(is,il)
          endif
     
         else  ! point was not in the input block of lines

          if((rline.gt.1.0).and.(rline.lt.nl))then
            ! is inside input image though, do nothing.
          else
            out(i,l)=0  ! is outside whole input image.
          endif

         endif

        else
         out(i,l)=0  ! is outside whole input image (sample test).

        endif

c     nearest neighbor option
      else                               ! nearest neighbor only

        il=nint(rline) - line_top +1
        is=nint(rsamp)
c       check if is in input image block
        if((is.ge.1).and.(is.le.ns))then
          if((il.ge.1).and.(il.le.nl_block_in))then
            out(i,l)=image(is,il) ! is in input image block
          else
            il=il + line_top -1
            if((il.ge.1).and.(il.le.nl))then
              ! is inside input image, do nothing
            else
              out(i,l)=0   ! is outside input image
            endif
          endif
        else
          out(i,l)=0       ! is outside input image
        endif

      endif

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
$ create maptran.imake
#define PROGRAM maptran

#define MODULE_LIST maptran.f 

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77

/*#define DEBUG		/* comment out on delivery */
/*#define LIB_LOCAL 	/* comment out on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create maptran.pdf
process help=*
PARM INP	TYPE = STRING   COUNT=2
PARM OUT	TYPE = STRING   COUNT=1
PARM NL         TYPE = INTEGER  COUNT=1                  DEFAULT=0
PARM NS         TYPE = INTEGER  COUNT=1                  DEFAULT=0
PARM NB         TYPE = INTEGER  COUNT=1                  DEFAULT=0
PARM BANDS      TYPE = INTEGER  COUNT=2                  DEFAULT=(1,0)
PARM INC        TYPE = INTEGER  COUNT=1   VALID=(1:1000) DEFAULT=10
PARM RANGE      TYPE = REAL     COUNT=1                  DEFAULT=10
PARM THRESH     TYPE = REAL     COUNT=1                  DEFAULT=0
PARM DNINTER    TYPE = REAL     COUNT=1                  DEFAULT=0
PARM CODE       TYPE = REAL     COUNT=1                  DEFAULT=-99999
PARM NOINTERP   TYPE = KEYWORD    VALID=(INTERPOL,NOINTERP) +
                DEFAULT=INTERPOL
PARM MOSAIC     TYPE = KEYWORD    VALID=(MOSAIC,NOMOSAIC) +
                DEFAULT=NOMOSAIC
PARM NOCHECK    TYPE = KEYWORD    VALID=(CHECK,NOCHECK) +
                DEFAULT=CHECK
END-PROC
.TITLE
VICAR program MAPTRAN.

.HELP
PURPOSE:
Maptran converts map projected images from one projection to another.
All images must have an image label compatible with one written by Map3.

Maptran performs the transformation using a grid of tiepoints computed
using the exact solution in subroutine convev. The tiepoints form
polygon areas with 4 vertices each. If all 4 vertices lie on the planet
then the polygon is mapped using a polynomial, otherwise each pixel
in the polygon is mapped exactly. 

It differs from Maptrans by performing the geom itself.
It differs from Maptran3 by interpolating with a polynomial.

.PAGE
USAGE & EXECUTION:
Maptran has no projection parameters. It reads the label from the
second input file and uses this label for the output file.
There are two ways to execute Maptran:

Method1. 
In this mode (the default) the output file contains the projection of the
first input file only. In this case the second input file is only used for
its label and sizefield, the rest is ignored. Here you can use the
'NOGEOM option in Map3 to save actually creating the second input file
since we only want its label.

Method2:
In this mode (see the 'MOSAIC keyword) the output file will contain the 
second input mosaicked with the projected first input file. Here the
second input must be a complete projected image.

Example for Method1:

If for example you had created a stereographic projection called: ster.img
MAP3 overlay.img ster.img NL=500 NS=500 'STER SCAL=10. +
  LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. 

 ...and you wanted to convert it to an orthographic image
you would then create using map3 a label with the desired target
projection , and say 'NOGEOM  as in
MAP3 overlay.img orth.img NL=500 NS=500 'ORTH SCAL=10. +
  LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. +
  'NOGEOM

 ...then you would do:
maptran inp=(ster.img,orth.img) out=new_orth.img

 ...if you didn't like the size field of orth.img you could override
it by specifying it as:
maptran inp=(ster.img,orth.img) out=new_orth.img nl=600 ns=700

Example for Method2:

	! oblique stereographic
MAP3 overlay.img ster.img NL=500 NS=500 'STER SCAL=10. +
  LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. 

	! oblique orthographic
MAP3 overlay.img orth.img NL=600 NS=700 'ORTH SCAL=10. +
  LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30.

maptran inp=(ster.img,orth.img) out=new_orth.img


.PAGE
EFFICIENT USAGE:
Maptran tries to stuff as much of the input into memory as possible.
If you have a dedicated machine then recompile the program increasing
the array dimension parameter NPIXELS to something realistic like
10,000,000 (twenty megabytes). If your buffer is small the program
will make many passes through the output file trying to anticipate
where the pixels will map in the input.

.PAGE
OPERATION:
Maptran is a rather involved program. This outline describes the logic
structure:

Read parameters.

Open input files.

Read Map3 label information from both input files.

Compute the smallest grid (larger than the default value) that will fit
in the grid buffer memory. See INC keyword.

Set the output NL and NS values to those of the second input if NL and NS
are not specified via parameters.

Compute the input line & sample value for each output grid image
coordinate using the mapping transformations.

Compute for each grid area the number of vertices which lie on the 
visible planet (0 to 4). Set this count to K.

If all four vertices lie off the input image set K to zero.

If K is 4 but any two points are separated by over 1/2 the picture
size set K to 1. We will use K later on.

Open the output file.

Copy the Map3 label from the second input to the output.

Open the temporary file, format it with CODE dn values (see CODE keyword),
close it, re-open it for update.

LOOP:

Load a block of input lines into memory.
If a dn is equal to CODE set it to CODE+1.

Loop on grid area rows.

Load a block of output lines from the temporary file.

Loop on grid area columns.

For each area:

    If the area has been completed skip it.

    If K (# vertices on planet) is 0 then write zeroes to output.

    If K is 4 then fit a polynomial to the vertices of the form:
      line_in=A*line_out*samp_out+B*line_out+C*samp_out+D
      samp_in=E*line_out*samp*out+F*line_out+G*samp_out+H
    and use it to map the output to the input.

    If K is 1,2,or 3, Compute the mapping from output to input exactly
    using the mapping transformations for each pixel.

    If the projection of the first input is of types 6,9, or 10 
    (convev buffer item #39) then check for wraparound condition.

    Each output pixel, once mapped to the input can be evaluated for 
    a DN value. If nointerpolation is selected the nearest input neighbor
    is selected.  If interpolation is selected then:

    Cases:
    If 4 input neighbors are > DNINTER then bilinear interpolation is done.
    If 3 input neighbors are > dninter the fourth is extrapolated and then
       interpolation is done.
    If 2 input neighbors are > dninter the output pixel is determined from
       a linear weighting of the 2 good pixels.
    If 1 input neighbor is > dninter it is copied to the output.
    If all the input neighbors are <= dninter the output is determined from
       the nearest neighbor.

    Notes:
    1. All output pixels are written to an intermediate file that is
       stored in half format. Later this file is copied to the output.
    2. Only on the first pass through the input the valid vertices on 
       the input are checked to see if they lie on or near the input
       block of lines. If they do not the area is temporarily skipped.
       See RANGE keyword.

End of loop on columns.

Write a block of output lines.

End of loop on rows.

If the last input block read does not contain the last input line 
then go to LOOP and redo the entire output file.

If any output pixel has a DN of code (see code parameter) and only one
pass has been made through the input picture then go to LOOP and redo 
the entire input picture.

If MOSAIC is specified then:
  Combine the intermediate halfword file with the second input file.
  Pixel loop
     If the projected pixel dn value is greater than THRESH place it
     in the output otherwise put the second input file pixel in the
     output.
  End pixel loop.
otherwise
  Copy the intermediate halfword file to the output.

.PAGE
HISTORY:
5-15-93  J Lorre. At delivery time the subroutine SEARCV2 had not been
         ported. This means that the line:
           if(primary.eqs." ")then primary="SYS"
         must be inserted into the .com file after the line:
           if(primary.eqs."")then primary=" "
         before delivery.
6-05-98  R Patel AR-9644. Fixed tst pdf and pdf to prevent illegal values.
                          Subroutine tranv was updated to make maptran work
                          under sgi.
19oct04  L.Kamp:  converted I*2 variables to R*4 in order to support REAL
         format;  added support for 3-D files.
26mar10  L.Kamp:  added check for byte data that the output pix are in byte
         range, to prevent spikes caused by wraparound of negative interpolated
         values.
ORIGINALLY WRITTEN BY: Jean Lorre 5/30/93
COGNIZANT PROGRAMMER:  Lucas Kamp

.LEVEL1
.VARI INP
STRING-input datasets.
.VARI OUT
STRING-output dataset.
.VARI NL
Output picture
number lines
.VARI NS
Output picture
number samples
.VARI NB
Output picture
number bands
.VARI BANDS
Output picture
starting band &
number bands
.VARI INC
Grid spacing in
pixels.
.VARI RANGE
pixel search range.
.VARI THRESH
Input threshold
.VARI DNINTER
Interpolation DN
threshold
.VARI NOINTERP
No interpolation.
.VARI CODE
Unset pixel code.
.VARI MOSAIC
Specify to mosaic
.VARI NOCHECK
No checking for
wraparound

.LEVEL2
.VARI INP
STRING-input datasets.
First input:
  The picture to be projected and placed into the output.
  This picture must have map projection labels compatible with those
  created by MAP3.
Second input:
  If the MOSAIC keyword is used then:
  This picture is used for its label,sizefield, and contents.
  The label defines the output projection type.
  The output will be a mosaic of this file AND the projected first input.
  otherwise:
  This picture is used merely for its label and for the NL and NS values.
  The image itself is ignored. The label defines the output projection type.
  This picture must have map projection labels compatible with those
  created by MAP3.

.VARI OUT
STRING-output dataset.
This is the projected version of the first input file. Its projection 
label will be copied from the second input file.

.VARI NL
Output picture number of lines. If NL is not specified it defaults to
the size of the second input file.

.VARI NS
Output picture number of samples. If NS is not specified it defaults to
the size of the second input file.

.VARI NB
Output picture number of bands. If NB is not specified it defaults to
the size of the first input file.  (Note that this differs from the
defaults for NL and NS!)

.VARI BANDS
Output picture starting band & number of bands. The default is to use
NB of the first input file.  

.VARI INC
Grid spacing in pixels for the output file. 
A fine grid is overlayed on the output file and the exact mapping is 
computed at each grid intersection. Pixels within the grid are 
interpolated. INC specifies the smallest grid desired. If INC is too
small MAPTRAN will increase it until the grid can reside in memory.

.VARI RANGE
In the case where individual pixels are being projected using the 
precise mapping projection equations (versus polynomial mapping)
the on-planet corners of the INC by INC area will fall either on or
off the input block of data lines. In case the points fall more than
RANGE pixels from the data block then this INC by INC area is bypassed
on the first pass through the input image. We hope to save time by
avoiding needless computations for pixels mapping outside the
input data block. If a second pass is needed this restriction is
lifted.

.VARI THRESH
Used only in the MOSAIC mode.
Thresh specifies the dn value above which the projected image has
precedence over the second input file during mosaicking of the two files.
Default is zero, ie: any projected dn which is zero or less will be
ignored in favor of the dn in input number 2.

.VARI DNINTER
Specifies a DN threshold used only in interpolation mode. If, when
interpolating a dn value in the input file, one or more of the four
neighbors is less than or equal to DNINTER it will not be used
in the interpolation.
Several combinations of input dn values can occur.
Cases:
If 4 input neighbors are > dninter then bilinear interpolation is done.
If 3 input neighbors are > dninter the fourth is extrapolated and then
   interpolation is done.
If 2 input neighbors are > dninter the output pixel is determined from
   a linear weighting of the 2 good pixels.
If 1 input neighbor is > dninter it is copied to the output.
If all the input neighbors are <= dninter the output is determined from
   the nearest neighbor.

.VARI NOINTERP
Specifies that no interpolation between pixels is desired.  
The output pixel will come from the nearest neighbor in the input.
Default is to interpolate.
See DNINTER keyword for interpolation modes.

.VARI CODE
This is a number that is used by Maptran to mark the location of output 
pixels whose DN values have not yet been determined. The default is -99999. 
If you have input DN's that happen to have this value (in the first input 
file) they will be reset to one dn higher. Thus -99999 would become -99998.
(Obviously, this could only happen if the input file were in REAL format.)
It is assumed that this change is insignificant, but if it should be, then
CODE should be set to some number outside of the range of the input data.

.VARI NOCHECK
Normally Maptran checks for wraparound in the sample direction for three
projections. These are: Mercator, Normal Cylindrical, and Simple Cylindrical.
Nocheck deactivates this checking.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmaptran.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
refgbl $syschar

local dir string
local dirg string

if ($syschar(1) = "UNIX")
   let dir = "/project/test_work/testdata/mipl/vgr/"
   let dirg = "/project/test_work/testdata/mipl/gll/"
else                            ! on Alpha:
   let dir = "wms_test_work:[testdata.mipl.vgr]"
end-if

write " "
write "This test produces 15 images, called mapt1.img to mapt16.img"
write "( mapt8.img is not generated)"
write "mapt7.img is used to store a mosaicking test and is comparable"
write "to mapt12.img."
write "Each image is a mosaic of 4 images which test one projection."
write "The upper left is a perspective projection."
write "The upper right is a map3 projection to be tested."
write "The lower left is maptrans try at converting the upper right to"
write "the upper left image."
write "The lower right is maptrans try at converting the upper left to"
write "the upper right image."
write "Check each mosaic visually and remember that maptrans can only"
write "project what it can see of the input."

write "*** Perspective projection"
cform &"dir"f1636832.geo a.img irange=(0,6000) orange=(0,255) oform=BYTE
map3 a.img b.img nl=500 ns=500 scale=10. 'perspect 'remote target=io
write "*** place overlay grid on the image of IO"
overlay b.img perspect.img 

write "*** Polar Orthographic projection"
map3 a.img b.img NL=500 NS=500 'ORTH SCALE=10. 'remote +
  LINE=250.  SAMP=250. LATI=90. LONG=330. 'POLE 'NOINTERP target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt1.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt1.img

write "*** Oblique orthographic"
map3 a.img b.img NL=500 NS=500 'ORTH SCAL=10. 'remote +
    LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt2.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt2.img

write "*** Polar Stereographic projection"
map3 a.img b.img NL=500 NS=500 'STER SCALE=10. 'remote +
    LINE=250. SAMP=250. LATI=-90. LONG=180. 'POLE 'SOUTH target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt3.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt3.img

write "*** Oblique stereographic"
map3 a.img b.img NL=500 NS=500 'STER SCAL=10. 'remote +
   LINE=250. SAMP=250. LATI=-10. LONG=150. NORTH=30. target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt4.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt4.img

write "*** Lambert projection"
map3 a.img b.img NL=500 NS=500 'LAMB SCALE=10. 'remote +
    LINE=250. SAMP=250. LATI=-80. LONG=150. PAR1=-30. +
    PAR2=-60. 'SOUTH target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt5.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt5.img

write "*** Mercator projection"
map3 a.img b.img NL=500 NS=500 'MERC SCALE=10. 'remote +
     LATI=70. LONG=240. target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt6.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt6.img

write "*** Cylindrical projection"
map3 a.img b.img NL=500 NS=500 'CYLI SCALE=10. 'remote +
    LINE=0. SAMP=1. LATI=90. LONG=240. target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt9.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt9.img

write "*** Rectangular projection"
map3 a.img b.img NL=500 NS=500 'RECT SCALE=10. 'remote +
     SAMP=1.  LATI=90. LONG=225. target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500 'nocheck
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt10.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt10.img

write "*** Oblique simple cylindrical projection"
write "*** no planet rotation, sees the center of the input."
map3 a.img b.img NL=500 NS=500 'OBCY SCALE=10. 'remote +
     'RECENT target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt11.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt11.img

write "*** Sinusoidal projection"
write "*** test nointerp"
map3 a.img b.img 'remote +
  nl=500 ns=500 scale=10. 'sinusoid latitude=20 longitud=150 +
  line=250 samp=250 target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500 +
  'nointerp
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500 +
  'nointerp
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt12.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt12.img

write "*** Oblique Sinusoidal projection"
write "*** no rotation , observe center of input"
map3 a.img b.img 'remote +
  nl=500 ns=500 scale=10. 'obsinuso 'recenter target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt13.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt13.img

write "*** Mollweide projection"
write "*** center input in output"
map3 a.img b.img 'remote +
  nl=500 ns=500 scale=10. 'mollweid 'recenter  target=io  
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt14.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt14.img

write "*** Transverse Mercator projection"
write "*** central meridian defaults to p5 point"
map3 a.img b.img 'remote +
  nl=500 ns=500 scale=10. 'tmercato target=io
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt15.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt15.img

write "*** Perspective projection"
map3 a.img b.img 'remote +
  nl=500 ns=500 scale=10. 'perspect target=io +
  north=45. latitude=80. longitud=150. line=200 samp=200
overlay b.img map3.img
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500
label-list c.img
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500
label-list d.img
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt16.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000
label-list mapt16.img

write "*** Test of REAL data"
cform perspect.img perspectR.img 'real
maptran inp=(perspectR.img,map3.img) out=cR.img nl=500 ns=500
label-list cR.img
cform cR.img c1.img 'byte
write " SMALL DIFFERENCES ARE DUE TO EDGE EFFECTS & ARE UNIMPORTANT:"
difpic (c.img c1.img) d.img

write "*** Test of cube format"
insert3d (perspect.img perspect.img) test.cub band=2
insert3d (test.cub perspect.img) test.cub band=3
label-l test.cub
maptran inp=(test.cub,map3.img) out=test1.cub nl=500 ns=500
label-l test1.cub
copy test1.cub c1.img nb=1
difpic (c.img c1.img)
copy test1.cub c3.img nb=1 sb=3
difpic (c.img c3.img)
maptran inp=(test.cub,map3.img) out=test1.cub nl=500 ns=500 nb=2
label-l test1.cub
copy test1.cub c2.img nb=1 sb=2
difpic (c.img c2.img)

write "*** Test of MOSAIC and other keywords"
maptran inp=(perspect.img,map3.img) out=c.img nl=500 ns=500 +
  'mosaic inc=11 range=11 thresh=1 dninter=1 code=-32741 'nointerp
maptran inp=(map3.img,perspect.img) out=d.img nl=500 ns=500 +
  'mosaic inc=11 range=11 thresh=1 dninter=1 code=-32741 'nointerp
fastmos inp=(perspect.img,map3.img,d.img,c.img) out=mapt7.img +
 off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) nl=1000 ns=1000

write "*** Test of DAR 26-Mar-2010 fix"
maptran (&"dirg"maptran_test_1.img &"dirg"maptran_test_2.img) maptran_test_out.img
write "this difpic should give zero (or few) differences:"
difpic (maptran_test_out.img &"dirg"maptran_test_comp_new.img) 'mod
write "this difpic should give many differences:"
difpic (maptran_test_out.img &"dirg"maptran_test_comp_old.img) 'mod

if ($syschar(1) = "UNIX")
  ush rm perspect.img
  ush rm a.img
  ush rm b.img
  ush rm c.img
  ush rm c1.img
  ush rm c2.img
  ush rm c3.img
  ush rm d.img
  ush rm map3.img
  ush rm cR.img
  ush rm test.cub
  ush rm test1.cub
  ush rm maptran_test_*.img
else
  dcl del perspect.img;
  dcl del a.img;
  dcl del b.img;
  dcl del c.img;
  dcl del c1.img;
  dcl del c2.img;
  dcl del c3.img;
  dcl del d.img;
  dcl del map3.img;
  dcl del test.cub
  dcl del test1.cub
end-if
end-proc
$ Return
$!#############################################################################
