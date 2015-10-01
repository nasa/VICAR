$!****************************************************************************
$!
$! Build proc for MIPL module morph
$! VPACK Version 1.8, Wednesday, February 07, 1996, 16:24:23
$!
$! Execute by entering:		$ @morph
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
$ write sys$output "*** module morph ***"
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
$ write sys$output "Invalid argument given to morph.com file -- ", primary
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
$   if F$SEARCH("morph.imake") .nes. ""
$   then
$      vimake morph
$      purge morph.bld
$   else
$      if F$SEARCH("morph.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake morph
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @morph.bld "STD"
$   else
$      @morph.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create morph.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack morph.com -
	-s morph.f -
	-i morph.imake -
	-p morph.pdf -
	-t tstmorph.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create morph.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      include 'VICMAIN_FOR'

      subroutine main44


      parameter (nlbuf=1200,nsbuf=1000,ntable=10000,numbox=150)

c nlbuf = max # lines in two first input files
c nsbuf = max # samples in two first input files
c ntable = max # tiepoints
c numbox = max # horizontal boxes (nsbuf/parameter INC)

      integer*2 dnleft(nsbuf,nlbuf),dnright(nsbuf,nlbuf)
      integer*2 dnout(nsbuf)
      integer*4 status,outunit,count,def,located(20)
      integer   maxcnt, nods, inc, nptsfit, nstart
      real*4 left_line(ntable),left_samp(ntable)
      real*4 right_line(ntable),right_samp(ntable)
      real*4 morph_line(ntable),morph_samp(ntable)
      real*4 buf(4,32)
      real*4 left,right,top,bottom

      real*8 l_coefx(4,numbox),l_coefy(4,numbox)
      real*8 r_coefx(4,numbox),r_coefy(4,numbox)
      real*8 a(4,4),aa(4,4),rl,rk
      real*8 c(20,4),cl(20),weights(20)

      byte    flag(numbox)
      integer name_length, i, inunit1, inunit2, nl, np, nl2
      integer np2, nboxes, npts, inmark, nl3, ns3, j, n_output
      integer num_start, name_len, lbox, jcol, jbox, i_topleft
      integer i_topright, i_botleft, i_botright, kstat, n, k
      integer jj, l, m  
      real    wt_right, wt_left, gridl, grids, d_topleft, d_topright
      real    d_botleft, d_botright, d, dist, x, y, yy, xx, d2
      real    d1, dx, dy, dntop, dnbot, dnl, dnr

      character*80 filename,fname
      character*1 name1(80),name2(80)

      logical leftonly,xvptst

      equivalence (filename,name1),(fname,name2)

! Begin data initialization

      data status/0/,outunit/0/,count/0/,def/0/,located/20*0/
      data maxcnt/0/, nods/0/, inc/0/, nptsfit/0/, nstart/0/
      data left/0.0/,right/0.0/,top/0.0/,bottom/0.0/
      data rl/0.d0/, rk/0.d0/

      data name_length/0/, i/0/, inunit1/0/, inunit2/0/
      data nl/0/, np/0/, nl2/0/, np2/0/, nboxes/0/, npts/0/
      data inmark/0/, nl3/0/, ns3/0/, j/0/, n_output/0/
      data num_start/0/, name_len/0/, lbox/0/, jcol/0/, m/0/
      data jbox/0/, i_topleft/0/, i_topright/0/, i_botleft/0/
      data i_botright/0/, kstat/0/, n/0/, k/0/, jj/0/, l/0/

      call zia (dnleft, nsbuf*nlbuf/2)
      call zia (dnright, nsbuf*nlbuf/2)
      call zia (dnout, nsbuf/2)
      
      call zia (left_line, ntable)
      call zia (left_samp, ntable)
      call zia (right_line, ntable)
      call zia (right_samp, ntable)
      call zia (morph_line, ntable)
      call zia (morph_samp, ntable)
      call zia (buf, 4*32)
      call zia (l_coefx, 4*numbox*2)
      call zia (l_coefy, 4*numbox*2)
      call zia (r_coefx, 4*numbox*2)
      call zia (r_coefy, 4*numbox*2)
      call zia (r_coefy, 4*numbox*2)
      call zia (a,  4*4*2)
      call zia (aa, 4*4*2)
      call zia (c,  20*4*2)
      call zia (cl, 20*2)
      call zia (weights, 20*2)

      call zia (flag, numbox/4)
      filename = ' '
      fname    = ' '
      do 40000 maxcnt = 1, 80
         name1(maxcnt) = ' '
         name2(maxcnt) = ' '
40000 continue

! End data initialization

      call ifmessage ('MORPH version 31-OCT-95')

      maxcnt = 1
      call xvparm('FRAMES',nods,count,def,maxcnt)
      maxcnt = 80
      call xvparm('NAME',filename,count,def,maxcnt)
      maxcnt = 1
      call xvparm('INC',inc,count,def,maxcnt)
      call xvparm('NPTS',nptsfit,count,def,maxcnt)
      call xvparm('START',nstart,count,def,maxcnt)
      leftonly=xvptst('LEFTONLY')
      if(leftonly) call xvmessage('Only left input image is used',' ')

c find length of filename prefix
      do i=80,1,-1
        if(name1(i).ne.' ') then
          name_length=i
          goto 5
        endif
      enddo
5     continue

c open input images
      call xvunit(inunit1,'INP',1,status,' ')
      call xvopen(inunit1,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit1,status,'NL',nl,'NS',np,' ')

      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',np2,' ')
      nl=min(nl,nl2)
      np=min(np,np2)
      nboxes=np/inc

c checks
      if((nl*np.gt.nlbuf*nsbuf).or.(nl.gt.nlbuf).or.(np.gt.nsbuf))then
         call xvmessage ('Input images too large',' ')
         call prnt (4,1,nlbuf,'NL limit=.')
         call prnt (4,1,nsbuf,'NS limit=.')
         call abend
      endif
      if(nboxes.gt.numbox)then
         call xvmessage ('ns/inc <= 100',' ')
         call abend
      endif

c load left image into memory as integer*2 
      do i=1,nl
         call xvread(inunit1,dnleft(1,i),status,'LINE',i,' ')
      enddo
      call xvclose(inunit1,status,'CLOS_ACT','FREE',' ')

c load right image into memory as integer*2 
      do i=1,nl
         call xvread(inunit2,dnright(1,i),status,'LINE',i,' ')
      enddo
      call xvclose(inunit2,status,'CLOS_ACT','FREE', ' ')
      
c load the tiepoint table from the mark file.
      npts=0
      call xvunit(inmark,'INP',3,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl3,'NS',ns3,' ')
      do i=1,nl3
         call xvread(inmark,buf,status,'LINE',i,' ')
         do 10 j=1,32
            if(buf(1,j).eq.0.0) goto 10
            if(buf(2,j).eq.0.0) goto 10
            if(buf(3,j).eq.0.0) goto 10
            if(buf(4,j).eq.0.0) goto 10
            npts=npts+1
            if(npts.gt.ntable)then
               call prnt(4,1,ntable,'Too many input points, limit= .')
               call abend
            endif
            left_line(npts)=buf(1,j)            
            left_samp(npts)=buf(2,j)            
            right_line(npts)=buf(3,j)            
            right_samp(npts)=buf(4,j)            
10       continue
      enddo
      call xvclose(inmark,status,'CLOS_ACT','FREE',' ')

c loop on the number of output files.
      n_output=nstart-1   ! the starting output file - 1
100   n_output=n_output+1

c compute the MORPH tiepoint locations by weighting the points
c linearly with the distance of the morph from left & right.
      wt_right=float(n_output)/float(nods+1)   ! weight of right image
      wt_left=1.0 - wt_right                   ! weight of left  image
      do i=1,npts
         morph_line(i)=wt_left * left_line(i) + wt_right * right_line(i)
         morph_samp(i)=wt_left * left_samp(i) + wt_right * right_samp(i)
      enddo

c determine the limits on the range of the morph points.
      left=morph_samp(1)
      right=left
      top=morph_line(1)
      bottom=top
      do i=2,npts
         if(left.gt.morph_samp(i)) left=morph_samp(i)
         if(right.lt.morph_samp(i)) right=morph_samp(i)
         if(top.gt.morph_line(i)) top=morph_line(i)
         if(bottom.lt.morph_line(i)) bottom=morph_line(i)
      enddo

c build output filename
      write(fname,20) n_output
20    format(i8)
      do i=1,8
        if(name2(i).ne.' ') then
          num_start=i
          goto 30
        endif
      enddo
30    name_len=name_length
      do i=num_start,8
        name_len=name_len + 1
        name1(name_len)=name2(i)
      enddo
      name1(name_len+1)='.'
      name1(name_len+2)='i'
      name1(name_len+3)='m'
      name1(name_len+4)='g'

c open the next output
      call xvunit(outunit,'NEW',n_output,status,
     &            'U_NAME',filename,' ')
      call xvopen(outunit,status,'U_FORMAT','HALF',
     +            'OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')

c     loop on the rows of output boxes inc apart
      do lbox=1,nl+inc,inc
         if(lbox.gt.nl) goto 1000
         gridl=lbox + inc/2.0 -0.5    ! line box center

c        loop on the columns of output boxes inc apart
         do jcol=1,nboxes
            jbox=(jcol-1)*inc+1
            grids=jbox + inc/2.0 -0.5    ! sample box center

c           determine the 4 nearest points, one in each quadrant.
            d_topleft=1.0e+10
            d_topright=1.0e+10
            d_botleft=1.0e+10
            d_botright=1.0e+10
            i_topleft=0
            i_topright=0
            i_botleft=0
            i_botright=0
            do i=1,npts
               if(morph_line(i).lt.gridl)then
                  if(morph_samp(i).lt.grids)then  ! upper left
                     d=gridl-morph_line(i) + grids-morph_samp(i)
                     if(d.lt.d_topleft)then
                        d_topleft=d
                        i_topleft=i
                     endif
                  else                            ! upper right
                     d=gridl-morph_line(i) + morph_samp(i)-grids
                     if(d.lt.d_topright)then
                        d_topright=d
                        i_topright=i
                     endif
                  endif
               else                               
                  if(morph_samp(i).lt.grids)then  ! lower left
                     d=morph_line(i)-gridl + grids-morph_samp(i)
                     if(d.lt.d_botleft)then
                        d_botleft=d
                        i_botleft=i
                     endif
                  else                            ! lower right
                     d=morph_line(i)-gridl + morph_samp(i)-grids
                     if(d.lt.d_botright)then
                        d_botright=d
                        i_botright=i
                     endif
                  endif
               endif
            enddo

c           PERFORM FITTING STEP. 

            if(nptsfit.gt.4) goto 200

c           perform exact fit using 4 points only, no weighting.

c           skip column if all 4 quadrants dont have a point
            if(i_topleft.eq.0.or.i_topright.eq.0.or.
     +         i_botleft.eq.0.or.i_botright.eq.0)then
               flag(jcol)=0
               goto 80
            endif

            a(1,1)=dble(morph_line(i_topleft))*
     +             dble(morph_samp(i_topleft))
            a(1,2)=dble(morph_line(i_topleft))
            a(1,3)=dble(morph_samp(i_topleft))
            a(1,4)=1.d0
            a(2,1)=dble(morph_line(i_topright))*
     +             dble(morph_samp(i_topright))
            a(2,2)=dble(morph_line(i_topright))
            a(2,3)=dble(morph_samp(i_topright))
            a(2,4)=1.d0
            a(3,1)=dble(morph_line(i_botleft))*
     +             dble(morph_samp(i_botleft))
            a(3,2)=dble(morph_line(i_botleft))
            a(3,3)=dble(morph_samp(i_botleft))
            a(3,4)=1.d0
            a(4,1)=dble(morph_line(i_botright))*
     +             dble(morph_samp(i_botright))
            a(4,2)=dble(morph_line(i_botright))
            a(4,3)=dble(morph_samp(i_botright))
            a(4,4)=1.d0
            call mve(8,16,a,aa,1,1) ! save a

c           to left image line
            l_coefy(1,jcol)=left_line(i_topleft)
            l_coefy(2,jcol)=left_line(i_topright)
            l_coefy(3,jcol)=left_line(i_botleft)
            l_coefy(4,jcol)=left_line(i_botright)
            call dsimq(a,l_coefy(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif

c           to left image sample
            call mve(8,16,aa,a,1,1) ! restore a
            l_coefx(1,jcol)=left_samp(i_topleft)
            l_coefx(2,jcol)=left_samp(i_topright)
            l_coefx(3,jcol)=left_samp(i_botleft)
            l_coefx(4,jcol)=left_samp(i_botright)
            call dsimq(a,l_coefx(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif
            
c           to right image line
            call mve(8,16,aa,a,1,1) ! restore a
            r_coefy(1,jcol)=right_line(i_topleft)
            r_coefy(2,jcol)=right_line(i_topright)
            r_coefy(3,jcol)=right_line(i_botleft)
            r_coefy(4,jcol)=right_line(i_botright)
            call dsimq(a,r_coefy(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif

c           to right image sample
            call mve(8,16,aa,a,1,1) ! restore a
            r_coefx(1,jcol)=right_samp(i_topleft)
            r_coefx(2,jcol)=right_samp(i_topright)
            r_coefx(3,jcol)=right_samp(i_botleft)
            r_coefx(4,jcol)=right_samp(i_botright)
            call dsimq(a,r_coefx(1,jcol),4,kstat)
            if(kstat.ne.0)then
               call xvmessage ('DSIMQ singular solution',' ')
               flag(jcol)=0
               goto 80
            else
               flag(jcol)=1
            endif
80          continue            ! abort fit for a column
            goto 300

c           Least squares fit using from 5 to 20 nearest points.
200         continue

c           move 4 quadrant points into located() buffer
            n=0
            if(i_topleft.gt.0)then
              n=n+1
              located(n)=i_topleft
            endif
            if(i_topright.gt.0)then
              n=n+1
              located(n)=i_topright
            endif
            if(i_botleft.gt.0)then
              n=n+1
              located(n)=i_botleft
            endif
            if(i_botright.gt.0)then
              n=n+1
              located(n)=i_botright
            endif

c           locate additional points as the nearest ones remaining
            do i=n+1,nptsfit
              d=1.0e+20
              do j=1,npts
                do k=1,i-1   ! avoid points already located
                  if(located(k).eq.j) goto 210
                enddo
                dist=(gridl-morph_line(j))**2+(grids-morph_samp(j))**2
                if(dist.lt.d) then
                   d=dist
                   jj=j
                endif
210             continue
              enddo
              located(i)=jj
            enddo

c           compute weights for points inversely with distance
            do i=1,nptsfit
              dist=sqrt((gridl-morph_line(located(i)))**2 +
     +                  (grids-morph_samp(located(i)))**2 )
              weights(i)=inc/(dist+1.0)
            enddo

c           fit for left image line direction.
            do i=1,nptsfit
              c(i,1)=dble(morph_line(located(i))) *
     +               dble(morph_samp(located(i)))
              c(i,2)=dble(morph_line(located(i)))
              c(i,3)=dble(morph_samp(located(i)))
              c(i,4)=1.d0
              cl(i)=left_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,l_coefy(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ') 
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for left image sample direction.
            do i=1,nptsfit
              cl(i)=left_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,l_coefx(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for right image line direction.
            do i=1,nptsfit
              cl(i)=right_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,r_coefy(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif

c           fit for right image sample direction.
            do i=1,nptsfit
              cl(i)=right_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,r_coefx(1,jcol))
            if(kstat.ne.0)then
               call xvmessage ('LSQP singular solution',' ')
               flag(jcol)=0
               goto 220
            else
               flag(jcol)=1
            endif
220         continue

300         continue

c END OF FITTING

         enddo    ! end of column loop

c        write out a block of INC lines
         do l=lbox,lbox+inc-1
            if(l.gt.nl) goto 1000
            rl=l
            k=0
            do jcol=1,nboxes
               if(flag(jcol).gt.0)then ! have good polynomial fit
                 grids=(jcol-1)*inc +inc/2.0 +0.5
                 do i=1,inc
900                 k=k+1
                    rk=k

c                   compute the x,y position in the left image.
                    y=l_coefy(1,jcol)*rl*rk+l_coefy(2,jcol)*rl+
     +                l_coefy(3,jcol)*rk+l_coefy(4,jcol)
                    x=l_coefx(1,jcol)*rl*rk+l_coefx(2,jcol)*rl+
     +                l_coefx(3,jcol)*rk+l_coefx(4,jcol)

c                   compute same transform on adjacent fit and weight
c                   the results for smoothness.
                    if(rk.lt.grids.and.jcol.gt.1)then
                      yy=l_coefy(1,jcol-1)*rl*rk+l_coefy(2,jcol-1)*rl+
     +                   l_coefy(3,jcol-1)*rk+l_coefy(4,jcol-1)
                      xx=l_coefx(1,jcol-1)*rl*rk+l_coefx(2,jcol-1)*rl+
     +                   l_coefx(3,jcol-1)*rk+l_coefx(4,jcol-1)
                      d2=(grids-rk)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif
                    if(rk.ge.grids.and.jcol.lt.nboxes)then
                      yy=l_coefy(1,jcol+1)*rl*rk+l_coefy(2,jcol+1)*rl+
     +                   l_coefy(3,jcol+1)*rk+l_coefy(4,jcol+1)
                      xx=l_coefx(1,jcol+1)*rl*rk+l_coefx(2,jcol+1)*rl+
     +                   l_coefx(3,jcol+1)*rk+l_coefx(4,jcol+1)
                      d2=(rk-grids)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif

c                   interpolate the DN value in the left image.
                    n=x
                    m=y
                    if(n.lt.1.or.n.ge.np.or.m.lt.1.or.m.ge.nl)then
                       dnout(k)=0
                       goto 90
                    endif
                    dx=x-n
                    dy=y-m
                    dntop=dnleft(n,m)*(1.-dx)+dnleft(n+1,m)*dx 
                    dnbot=dnleft(n,m+1)*(1.-dx)+dnleft(n+1,m+1)*dx 
                    dnl=dntop*(1.-dy)+dnbot*dy

c                   compute the x,y position in the right image.
                    y=r_coefy(1,jcol)*rl*rk+r_coefy(2,jcol)*rl+
     +                r_coefy(3,jcol)*rk+r_coefy(4,jcol)
                    x=r_coefx(1,jcol)*rl*rk+r_coefx(2,jcol)*rl+
     +                r_coefx(3,jcol)*rk+r_coefx(4,jcol)

c                   compute same transform on adjacent fit and weight
c                   the results for smoothness.
                    if(rk.lt.grids.and.jcol.gt.1)then
                      yy=r_coefy(1,jcol-1)*rl*rk+r_coefy(2,jcol-1)*rl+
     +                   r_coefy(3,jcol-1)*rk+r_coefy(4,jcol-1)
                      xx=r_coefx(1,jcol-1)*rl*rk+r_coefx(2,jcol-1)*rl+
     +                   r_coefx(3,jcol-1)*rk+r_coefx(4,jcol-1)
                      d2=(grids-rk)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif
                    if(rk.ge.grids.and.jcol.lt.nboxes)then
                      yy=r_coefy(1,jcol+1)*rl*rk+r_coefy(2,jcol+1)*rl+
     +                   r_coefy(3,jcol+1)*rk+r_coefy(4,jcol+1)
                      xx=r_coefx(1,jcol+1)*rl*rk+r_coefx(2,jcol+1)*rl+
     +                   r_coefx(3,jcol+1)*rk+r_coefx(4,jcol+1)
                      d2=(rk-grids)/real(inc)
                      d1=1.0-d2
                      y=y*d1+yy*d2
                      x=x*d1+xx*d2
                    endif

c                   interpolate the DN value in the right image.
                    n=x
                    m=y
                    if(n.lt.1.or.n.ge.np.or.m.lt.1.or.m.ge.nl)then
                       dnout(k)=0
                       goto 90
                    endif
                    dx=x-n
                    dy=y-m
                    dntop=dnright(n,m)*(1.-dx)+dnright(n+1,m)*dx    
                    dnbot=dnright(n,m+1)*(1.-dx)+dnright(n+1,m+1)*dx
                    dnr=dntop*(1.-dy)+dnbot*dy

                    if(leftonly)then
                      dnout(k)=nint(dnl)
                    else
                      dnout(k)=nint(dnl*wt_left+dnr*wt_right)
                    endif

90                  continue        ! abort pixel, set dn to zero
                    if(jcol.eq.nboxes.and.k.lt.np) goto 900  ! endofline
                 enddo
              else                  ! abort column of pixels
                 do i=1,inc
                    k=k+1
                    dnout(k)=0
                 enddo
              endif
            enddo

c           pad end of line if columns stop short of right edge
            if(k.lt.np)then
               do i=k+1,np
                  dnout(i)=0
               enddo
            endif

            call xvwrit(outunit,dnout,status,'LINE',l,' ')
         enddo

      enddo       ! end of row loop

c write blank lines if last row doesen't extend to end of image
c      if(lbox+inc-1.lt.nl)then
c         call mve(2,np,0,dnout,0,1)
c         do i=lbox+inc,nl
c            call xvwrit(outunit,dnout,status,'LINE',l,' ')
c         enddo
c      endif

1000  call xvclose(outunit,status,'CLOS_ACT','FREE',' ')  ! close output

      if(n_output.lt.nods) goto 100   ! go make another picture
      return
      end

c**************************************************************

      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
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
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      implicit none

! Passed Parameters
      REAL*8  C(20,4), CL(20), wts(20), X1(4)
      integer ind, NE, NU

! Local Parameters
      REAL*8  A(4,4), AL(4), R(4,4), RL(4), Q(4,4),
     &        X(4), SUM
      integer J, I, K, NUM, NUP, l, ix, ixi

      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
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
      RETURN
999   ind=1
      return
      END


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

      implicit none

! Passed parameters
      real*8  A(1),B(1)
      integer N, KS

! Local Parameters
      real*8  biga,save,tol
      integer JJ, IMAX, I, J, IT, IJ, I1, I2, K, IX, JX, JY, 
     &        IXJ, IQS, IXJX, JJX, NY, IA, IB, IC

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
$ create morph.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM morph

   To Create the build file give the command:

		$ vimake morph			(VMS)
   or
		% vimake morph			(Unix)
************************************************************************/
#define PROGRAM	morph
#define R2LIB
#define MODULE_LIST morph.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create morph.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=3
PARM NAME         TYPE=STRING       COUNT=1
PARM FRAMES       TYPE=INTEGER      COUNT=1
PARM INC          TYPE=INTEGER      COUNT=(0:1)   DEFAULT=25
PARM NPTS   TYPE=INTEGER      COUNT=(0:1)   DEFAULT=8 VALID=(4:20)
PARM START  TYPE=INTEGER      COUNT=1       DEFAULT=1
PARM LEFTONLY TYPE=KEYWORD COUNT=(0:1) VALID=("LEFTONLY","BOTH") +
     DEFAULT="BOTH"
END-PROC

.TITLE
VICAR2 program MORPH

.HELP
MORPH is used to produce intermediate images between two input images.
It will use the geometric distortion between the input images
and will introduce a percentage of this distortion into the outputs.
It will also weight the input images proportionately with their 
distance from the inputs in intensity. 
  The geometric distortion is determined by programs TRACKER or 
TRACKER2 which create a tiepoint grid using hundreds or thousands of
points not necessarily in a regular pattern. Each output is
the sum of both inputs weighted in intensity and interpolated in position.

Method:
If there are N output images and we are interested in output image
number n where n <= N. And if we have two input images called left
and right, Then the weighting would be:
right_weight = n/(N+1)
left_weight=1-right_weight
Each tiepoint is interpolated to give it's equivalent position for image n.
tiepoint_line_image_n=tiepoint_line_left_input*left_weight +
                      tiepoint_line_right_input*right_weight
tiepoint_samp_image_n=tiepoint_samp_left_input*left_weight +
                      tiepoint_samp_right_input*right_weight
We then create a regular grid on image n and for each intersection locate
the nearest tiepoint on n in each of the four quadrants. If more points
are desired the rest are found from the nearest remaining points 
(see NPTS keyword). 
We then fit
x and y polynomials going from n to the left and right images of the form:
Y=Axy+By+Cx+D  and X=Exy+Fy+Gx+H for each grid intersection ( INC keyword).
The box INC by INC centered on the grid intersection is then mapped via the
polynomials onto the left and right images. This determines where a pixel 
on n comes from in both input images. Each point is averaged by linearly
weighting each polynomial based upon how far the point is from the center
of that polynomial. Weighting is done horizontally only between adjacent
polynomials.
   The DN value in each input image is interpolated bilinearly and then
combined to give the DN value for that pixel in n, thus:
DN_n(X,Y)=DN_left*left_weight+DN_right*right_weight.

Restrictions:
Morph must place all the inputs into memory as 16 bit images. Restrictions
can easily be aleviated by changing the parameter statement in the main
program and recompiling it. 
At the moment:
  the two input images cannot exceed 1000 lines by 1000 pixels.
  the number of tiepoints cannot exceed 10000.
  the ratio of NS/INC cannot exceed 150.
In the NPTS=4 mode if a grid point has less than 4 points (one in each 
quadrant) the grid point area is abandoned. This leads to blank space
around the output images. Higher values of NPTS permits extrapolation.

.page
Example:

tracker2 inp=(left.img,right.img) out=mark.img +
  grid=100 nlw=11 nsw=11 edge=10 'print window=(2,1) look=(5,5) +
  limit=(8,8) see=1
morph inp=(left.img,right.img,mark.img) frames=2 name=morph

(tracker2 generates a grid of tiepoints in the mark file. Morph uses
the tiepoints field to determine the amount of distortion  between the
images. In this case it generates two outputs 1/3 and 2/3 of the way
between left and right images. Each output is weighted in intensity
linearly between the inputs. Thus morph1.img has 2/3 the weight of
left.img and 1/3 the weight of right.img . 
The output images are named: morph1.img and morph2.img)

HISTORY

Written By: J Lorre			Nov 15 1992
Cognizant Programmer: J Lorre
.LEVEL1
.VARI INP
Three inputs.
#1 is the left image
#2 is the right image.
#3 is the mark file

.VARI FRAMES
Number of output
images generated.

.VARI START
Starting output
frame number.

.VARI NAME
The name prefix
of outputs.

.VARI INC
Grid interval

.VARI NPTS
# points used
in local fit.

.VARI LEFTONLY
Only use left image.

.LEVEL2

.VARI INP
There are three input files.
File#1: This is the left or first input image given to
        program TRACKER or TRACKER2. 
File#2: This is the right or second input image given to
        program TRACKER. 
File#3: This is the MARK file written by program TRACKER.
        It is 'REAL' format with 512 byte records containing
        pairs of tiepoints in the order:
        left_line,left_samp,right_line,right_samp.

.VARI FRAMES
The number of output images produced. Outputs are the same size
as inputs. output morphs are spaced equally between the two input
images. Thus if there were 3 output images then they would be spaced
at 1/4, 1/2, and 3/4 of the way between the two input images.
The format of the output is the same as the input.

.VARI START
The starting number of the first output frame. Normally you would
not specify START and it would default to 1.  In the event that the
program were aborted in mid run you could restart it where you left off
by setting START=n where n was the output frame number which was the
first missing output frame (missing because the job was cancelled).
Thus if 5 outputs were desired and only two were created then you
could restart the job USING IDENTICAL PARAMETERS AS BEFORE but
adding START=3.

.VARI NAME
The prefix for the output file names.
If you specify name=abc and if frames=3 for example then the three
output files will be called (in order left to right):
abc1.img abc2.img and abc3.img  .

.VARI INC
The grid interval in pixels in the output image. At each grid intersection
a polynomial is fit to the nearest 4 tiepoints to perform a geom mapping
from the morph to the first and second input images. 
One tiepoint is selected from each of the 4 quadrants. INC should be
less than the tiepoint grid spacing but not much less than half of it
for efficiency .

.VARI NPTS
Number of  points used at each grid intersection to fit a surface
linking the morph to the left and right images geometrically.
The range is 4 to 20.  If npts=4 then an exact fit is made and no
extrapolation beyond the point array is permitted. Lerger values
permit a least squares fit with inverse distance weighting and
supports extrapolation.

.VARI LEFTONLY
Only use left image to compute the output images. This converts the program 
into a geom operation in which the left image is moved towards the right image.
The right input image is ignored.
$ Return
$!#############################################################################
$Test_File:
$ create tstmorph.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! TEST SCRIPT FOR THE PROGRAM MORPH
gausnois out=a.img nl=305 ns=305
copy a.img left.img nl=300 ns=300
copy a.img right.img sl=5 ss=3 nl=300 ns=300
tracker3 inp=(left.img,right.img) out=mark.img +
  grid=100 nlw=11 nsw=11 nlarea=21 nsarea=21 'print 
gen out=b.img nl=305 ns=305 ival=10 linc=0 sinc=0
qsar inp=b.img out=a.img area=(150,150,5,5,200)
copy a.img l.img nl=300 ns=300
copy a.img r.img sl=5 ss=3 nl=300 ns=300
morph inp=(l.img,r.img,mark.img) +
   inc=25 npts=5 frames=2 name=morph
list inp=l.img sl=143 ss=143 nl=15 ns=15
list inp=r.img sl=143 ss=143 nl=15 ns=15
list inp=morph1.img sl=143 ss=143 nl=15 ns=15
list inp=morph2.img sl=143 ss=143 nl=15 ns=15
morph inp=(l.img,r.img,mark.img) +
   inc=25 npts=5 frames=2 name=morph start=2
list inp=morph2.img sl=143 ss=143 nl=15 ns=15
gen out=r.img nl=300 ns=300
morph inp=(l.img,r.img,mark.img) +
   inc=25 npts=5 frames=2 name=morph start=2 'leftonly
list inp=morph2.img sl=143 ss=143 nl=15 ns=15
stretch inp=l.img out=ll.img linear=(5,260)
morph inp=(ll.img,r.img,mark.img) +
   inc=25 npts=5 frames=2 name=morph start=2 'leftonly
list inp=morph2.img sl=143 ss=143 nl=15 ns=15
end-proc
$ Return
$!#############################################################################
