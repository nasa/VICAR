C
C
C	FORTRAN  Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:
C
C		BODFND
C		BODVAR
C		CKLPF
C		CKGP
C		CKGPAV
C		ERRACT
C		ERRPRT
C		ET2UTC
C		DAFBNA
C		DAFFNA
C		DAFOPR
C		DAFOPW
C		LDPOOL
C		RTPOOL
C		SCENCD
C		SCS2E
C		SPKAPP
C		SPKLEF
C		SPKSSB
C		UTC2ET
C		=============================================
C		EUL2M
C		SCTIKS		written by: Sam Le 04/27/1995
C		CKBSS
C		CKSNS
C		CKPFS
C		DAFA2B
C		DAFB2A
C		SPCA2B
C		SPCB2A
C		TXTOPR
C		SPCT2B
C		TXTCLS
C		=============================================
C               INVERT
C               TKFRAM   Written by: Michael Brady 10/29/2001
C		=============================================
C

c***************************************** 
c xms_bodfnd:
c 2nd-stage bridge to BODFND, in Fortran
c****************************************** 

      INTEGER FUNCTION xms_bodfnd(body, item, i)
     
      integer body
      byte item(1)
      integer i
      logical found
      logical bodfnd
      character*20 text

      text=' '

      if (i.gt.80) print*, 'xms_bodfnd: string is too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      found = bodfnd(body,text)
      if ( found ) then
C txh::removed because of invalid variable name.
C       xbodfnd = 1
	xms_bodfnd = 1
      else
C txh::removed because of invalid variable name.
C       xbodfnd = 0
	xms_bodfnd = 0
      endif

      return
      end


c***************************************** 
c  xms_bodvar:
C  2nd-stage bridge to BODVAR, in Fortran
c****************************************** 

      subroutine xms_bodvar(body, item, i, dim, values)
      
      integer body
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      character*80 text

      text=' '

      if (i.gt.80) print*,'xms_bodvar, string too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      call bodvar(body, text, dim, values)

      return
      end


c***************************************** 
c  xms_spklef:
C  2nd-stage bridge to SPKLEF, in Fortran
c****************************************** 

      subroutine xms_spklef(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) print*,'xspklef, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call spklef(text, handl)

      return
      end


c***************************************** 
c  xms_utc2et:
C  2nd-stage bridge to UTC2ET, in Fortran
c****************************************** 

      subroutine xms_utc2et(utc, i, et)
      
      integer i
      byte utc(1)
      double precision et
      character*80 text

      if (i.gt.80) print*,'xutc2et, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(utc, text, i)

      call utc2et(text, et)

      return
      end


c***************************************** 
c xms_cklpf:
C 2nd-stage bridge to CKLPF, in Fortran
c****************************************** 

      subroutine xms_cklpf(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) print*, 'xcklpf, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call cklpf(text, handl)

      return
      end


c***************************************** 
c xms_ckgp:
C  2nd-stage bridge to CKGP, in Fortran
c****************************************** 

      subroutine xms_ckgp(inst,sclkdp,tol,ref,i,cmat,clkout,status)

      
      double precision sclkdp
      double precision tol
      double precision cmat(3,3)
      double precision clkout
      integer inst
      integer status
      integer i
      byte ref(1)
      character*80 text
      logical found


      if (i.gt.80) print*, 'xckgp, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)

      call ckgp(inst, sclkdp, tol, text, cmat, clkout, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 

      return
      end


c***************************************** 
c xms_ckgpav:
C  2nd-stage bridge to CKGPAV, in Fortran
c****************************************** 

      subroutine xms_ckgpav(inst,sclkdp,tol,ref,i,cmat,av,clkout,status)

      
      double precision sclkdp
      double precision tol
      double precision cmat(3,3)
      double precision av(3)
      double precision clkout
      integer inst
      integer status
      integer i
      byte ref(1)
      character*80 text
      logical found


      if (i.gt.80) print*, 'xckgpav, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)

      call ckgpav(inst, sclkdp, tol, text, cmat, av, clkout, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 

      return
      end


c***************************************** 
c xms_errprt:
C 2nd-stage bridge to ERRPRT in Fortran
c****************************************** 

      subroutine xms_errprt(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xerrprt, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xerrprt, second string is too long'
      endif

C     Initialize arrays

      cmd=' '
      option=' '

C     Transformations to Fortran-string

      call mvlc(x,cmd,i)
      call mvlc(y,option,j)

      call errprt(cmd,option)

      return
      end


c***************************************** 
c xms_dafopr:
C 2nd-stage Bridge to DAFOPR, in Fortran
c****************************************** 

      subroutine xms_dafopr(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) print*, 'xdafopr, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopr(text,handle)

      return
      end

c***************************************** 
c xms_spkapp:
C 2nd-stage bridge to SPKAPP in Fortran
c****************************************** 

      subroutine xms_spkapp(targ,et,ref,i,sobs,abcorr,j,starg,lt)
      
      double precision et
      double precision sobs(6)
      double precision starg(6)
      double precision lt
      integer targ
      integer i, j
      byte ref(1)
      byte abcorr(1)
      character*80 texta
      character*80 textb

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xspkapp, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xspkapp, second string is too long'
      endif

C     Initialize arrays

      texta = ' '
      textb = ' '

C     Transformations to Fortran-string

      call mvlc(ref,texta,i)
      call mvlc(abcorr,textb,j)

      call spkapp(targ,et,texta,sobs,textb,starg,lt)

      return
      end



c***************************************** 
c   xms_spkssb:
C   2nd-stage bridge to SPKSSB, in Fortran
c*****************************************

      subroutine xms_spkssb(targ,et,ref,i,starg)
      
      double precision et
      double precision starg(6)
      integer targ
      integer i
      byte ref(1)
      character*80 text

      if (i.gt.80) print*, 'xspkssb, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)
      call spkssb(targ,et,text,starg)

      return
      end


c***************************************** 
c  xms_erract:
C  2nd-stage bridge to ERRACT in Fortran
c****************************************** 

      subroutine xms_erract(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xerract, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xerract, second string is too long'
      endif

C     Initialize arrays

      cmd    = ' '
      option = ' '

C     Transformations to Fortran-string

      call mvlc(x,cmd,i)
      call mvlc(y,option,j)

      call erract(cmd,option)

      return
      end


c***************************************** 
c   xms_daffna:
C   2nd-stage bridge to DAFFNA, in Fortran
c*****************************************
      subroutine xms_daffna(status)

      integer status
      logical found

      call daffna(found)

      if (found) then
	status = 1         !TRUE
      else
	status = 0         !FALSE
      endif 

      return
      end


c***************************************** 
c   xms_dafopw:
C   2nd-stage Bridge to DAFOPW, in Fortran
c*****************************************
      subroutine xms_dafopw(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) print*, 'xdafopw, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopw(text,handle)

      return
      end


c*****************************************
c   xms_dafbna:
C   2nd-stage Bridge to DAFBNA, in Fortran
c***************************************** 

      subroutine xms_dafbna(handle, sum, name, i)
      
      double precision sum(*)
      integer handle
      integer i
      byte name(1)
      character*80 text

      if (i.gt.80) print*, 'xdafbna, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(name,text,i)

      call dafbna(handle,sum,text)

      return
      end


c***************************************** 
c  xms_ldpool:
C  2nd-stage bridge to LDPOOL, in Fortran
c*****************************************

      subroutine xms_ldpool(x,i)
      
      integer i
      byte x(1)
      character*80 text

      if (i.gt.80) print*, 'xldpool, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call ldpool(text)

      return
      end


C***************************************** 
C xms_scencd:
C 2nd-stage bridge to SCENCD, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
      subroutine xms_scencd(sc, btext, i, sclkdp)      
      integer sc
      byte btext(1)
      integer i
      double precision sclkdp

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.80) then
	print*, 'xscencd, string too long'
      else
        text = ' '
        call mvlc( btext, text, i)
        call scencd(sc, text, sclkdp)      
      endif

      return
      end


C***************************************** 
C xms_scs2e:
C 2nd-stage bridge to SCS2E, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
      subroutine xms_scs2e(sc, SCLK, i, et)
      
      integer sc
      byte SCLK(1)
      integer i
      double precision et

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.32) then
	print*, 'xscs2e, string too long'
      else
        text = ' '
        call mvlc(SCLK, text, i)
        call scs2e(sc, text, et)
      endif

      return
      end


C***************************************** 
C xms_et2utc:
C 2nd-stage bridge to ET2UTC, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
      subroutine xms_et2utc(et,format,prec,UTCtmp) 

      double precision et
      integer prec
      byte format(1)
      byte UTCtmp(80)
      character*80 text
      character*1  ftext

      call mvlc(format,ftext,1)

      call et2utc(et,ftext,prec,text)

      call mvcl(text, UTCtmp, 80)

      return
      end

c***************************************** 
c  xms_rtpool:
C  2nd-stage bridge to RTPOOL, in Fortran
c  23nov94 -lwk- fixed 'flag' to be -1 on success
c****************************************** 

      subroutine xms_rtpool(item, i, dim, values, flag)
      
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      integer flag
      logical status

      character*80 text

      text=' '

      if (i.gt.80) print*, 'xrtpool, string too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      call rtpool(text, dim, values, status)

      if ( status ) then
	flag = -1
      else
	flag = 0
      endif

      return
      end
C*************************************************
C xms_sctiks:
C 2nd-stage bridge to SCTIKS, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by: 		Sam Le
C Date:			April 27, 1995
C*************************************************
	subroutine xms_sctiks(sc, clkstr, i,  ticks)
	integer			sc, i
	byte			clkstr(1)
	character*12 		text
	double precision	ticks

C	Transformation to Fortran-string

	if (i .gt. 12) then
	   print*, 'xsctiks, clkstr is too long'
        else
	   text = ' '
	   call mvlc(CLKSTR, text, i)
	   call sctiks(sc, text, ticks)
        end if

	return
	end
C*************************************************
C xms_ckbss:
C 2nd-stage bridge to xckbss, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_ckbss(inst, sclk, tol, needav)
	integer			inst
	double precision	sclk
	double precision	tol
	integer			needav
	logical			flag

	if (needav .eq. 1) then
	   flag = .true.
	else
	   flag = .false.
	endif

	call ckbss(inst, sclk, tol, flag)

	return
	end
C*************************************************
C xms_cksns:
C 2nd-stage bridge to xcksns, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_cksns(handle, descr, segid, found)
	integer			handle
	double precision	descr(5)
	byte			segid(40)
	integer			found
	logical			status
	character*40		tempsegid

	call cksns(handle, descr, tempsegid, status)
	call mve(1, 40, tempsegid, segid, 1, 1)
 
	if (status) then
	   found = 1
	else
	   found = 0
	endif

	return
	end
C*************************************************
C xms_ckpfs:
C 2nd-stage bridge to xckpfs, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_ckpfs(handle, descr, sclkin, tol, 
     1			needav, cmat, av, clkout, found)
	integer			handle
	double precision	descr (*)
	double precision	sclkin
	double precision	tol
	integer			needav
	double precision	cmat (3,3)
	double precision	av(*)
	double precision	clkout
	integer			found
	logical			in, out

	if (needav .eq. 1) then
	   in = .true.
	else 
	   in = .false.
	endif
	
	call ckpfs(handle, descr, sclkin, tol, in, 
     1			cmat, av, clkout, out)
	if (out) then
	   found = 1
	else 
	   found = 0
	endif

	return
	end
C*************************************************
C xms_surfpt:
C 2nd-stage bridge to xsurfpt, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_surfpt(pos, u, a, b, c, pts, fnd)
	double precision	pos(3), u(3)
	double precision	a, b, c, pts(3)
	integer			fnd
	logical			flag

	call surfpt(pos, u, a, b, c, pts, flag)
	if (flag) then
	   fnd = 1
	else
	   fnd = 0
	endif

	return
	end
C*************************************************
C xms_dafa2b:
C 2nd-stage bridge to dafa2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_dafa2b(as, alen, da, dlen,resv)
	byte as(1)
	integer alen
	byte da(1)
	integer dlen
	integer resv
	character*80 aname, dname

	aname = ' '
	dname = ' '
	call mvlc(as, aname, alen)
	call mvlc(da, dname, dlen)
	call dafa2b(aname, dname, resv)

	return
	end
C*************************************************
C xms_dafb2a:
C 2nd-stage bridge to dafb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_dafb2a(da, dlen, as, alen)
	byte 		da(1)
	integer 	dlen
	byte 		as(1)
	integer 	alen
	character*80 	dname, aname

	dname = ' '
	aname = ' '
	call mvlc(da, dname, dlen)
	call mvlc(as, aname, alen)

	call dafb2a(dname, aname)

	return
	end
C*************************************************
C xms_spca2b:
C 2nd-stage bridge to spca2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_spca2b(ascfile, asclen, binfile, binlen)
        integer asclen, binlen
        byte	 ascfile(1)
	byte	 binfile(1)
        character*80 fascfile, fbinfile

        fascfile = ' '
        fbinfile = ' '

        call mvlc(ascfile, fascfile, asclen)
        call mvlc(binfile, fbinfile, binlen)

        call spca2b(fascfile, fbinfile)
        return
        end
C*************************************************
C xms_spcb2a:
C 2nd-stage bridge to spcb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_spcb2a(binfile, binlen, ascfile, asclen)
        integer binlen, asclen
        byte	binfile(1)
	byte	ascfile(1)
        character*80 fbinfile, fascfile

        fbinfile = ' '
        fascfile = ' '

        call mvlc(binfile, fbinfile, binlen)
        call mvlc(ascfile, fascfile, asclen)

        call spcb2a(fbinfile, fascfile)

        return
        end
C*************************************************
C xms_txtopr:
C 2nd-stage bridge to txtopr, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_txtopr(fname, flen, unit)
        integer flen, unit
        character*80 fname, ffname

        ffname = ' '

        call mvlc(fname, ffname, flen)
        call txtopr(ffname, unit)
        return
        end
C*************************************************
C xms_spct2b:
C 2nd-stage bridge to spct2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_spct2b(unit, fname, len)
	integer 	unit
	character*80	fname
	integer		len
	character*80	ffname

	ffname = ' '

	call mvlc(fname, ffname, len)
	call spct2b(unit, ffname)

	return
	end
C***************************************************
C xms_txtcls:
C 2nd-stage bridge to close, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_txtcls(txtunit)
        integer txtunit
        close (txtunit)
        return
        end
C***************************************************
C xms_sce2s:
C 2nd-stage bridge to convert SCET to SCLK string
C
C Part of bridge to be called from C applications
C Written by:   	Sam Le
C Date      :		June 6, 1995
C***************************************************
	subroutine xms_sce2s(sc, etc, sclk)
	integer			sc
	double precision	etc
	character*(*)		sclk
	character*80		text

	call sce2s(sc, etc, text)
	call mvcl(text, sclk, 80)

	return
	end
C*******************************************************************
C xms_dafhfn:
C 2nd-stage bridge to xdafhfn, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:		Sam Le
C Date	    : 		September 14, 1995
C*******************************************************************
	subroutine xms_dafhfn (handle, fname)
	integer		handle
	byte		fname(256)
	character*256	temp

	call dafhfn(handle, temp)
	call mve(1, 256, temp, fname, 1, 1)

	return
	end 
C*******************************************************************
C xms_bodn2c:
C 2nd-stage bridge to bodn2c, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date      :           2/20/1997
C*******************************************************************
        subroutine xms_bodn2c(name, len, id, found)
        integer len, id, found, code
	logical lfound
	byte name(1)
        character*80 tname

        tname = ' '

        call mvlc(name, tname, len)
        call bodn2c(tname, code, lfound)

	if (lfound) then
	   found = 1
	   id    = code
	else
	   found = 0
	endif

        return
        end
C*******************************************************************
C xms_ckw01:
C 2nd-stage bridge to ckw01, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date      :           3/5/1997
C*******************************************************************
        subroutine xms_ckw01(handle, begtime, endtime, inst, ref,
     1		len, avflag, segid, nrec, sclkdp, quat, av)

        integer handle
        double precision begtime
	double precision endtime
	integer inst
	byte ref(1)
	integer len
	integer avflag
	byte segid
	integer nrec
	double precision sclkdp
	double precision quat(4)
	double precision av(3)
	logical flag

	character*8 tref
	character*40 tsegid

        tref = ' '
	tsegid = ' '

	call mvlc (ref, tref, len)
	call mvlc (segid, tsegid, 40)

	if (avflag .eq. 1) then
	   flag = .true.
	else
	   flag = .false.
	endif

	call ckw01 (handle, begtime, endtime, inst, tref,
     1		flag, tsegid, nrec, sclkdp, quat, av)

	return
	end
C*******************************************************************
C xms_tkfram:
C 2nd-stage bridge to tkfram, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Michael Brady
C Date      :           10/29/2001
C*******************************************************************
        subroutine xms_tkfram(id, rot, frame, fnd)
        integer                 id
        double precision        rot   ( 3, 3 )
        integer                 frame
	integer			fnd
	logical			flag

	call tkfram(id, rot, frame, flag)
	if (flag) then
	   fnd = 1
	else
	   fnd = 0
	endif

	return
	end
C*******************************************************************
C xms_invert:
C 2nd-stage bridge to invert, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Michael Brady
C Date      :           10/29/2001
C*******************************************************************
        subroutine xms_invert(m1, m2)
        double precision        m1   ( 3, 3 )
        double precision        m2   ( 3, 3 )

	call invert(m1, m2)

	return
	end
