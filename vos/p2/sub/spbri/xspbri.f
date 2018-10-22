C
C
C	FORTRAN  Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:
C
C		BODFND
C		BODVAR
C		BODN2C
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
C		STR2ET		by Payam Zamani on 30-Sep-1999
C		SPKEZ

c***************************************** 
c xbodfnd:
c 2nd-stage bridge to BODFND, in Fortran
c****************************************** 

      INTEGER FUNCTION xbodfnd(body, item, i)
     
      integer body
      byte item(1)
      integer i
      logical found
      logical bodfnd
      character*20 text

      text=' '

      if (i.gt.80) call xvmessage('xbodfnd, string is too long',' ')

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      found = bodfnd(body,text)
      if ( found ) then
	xbodfnd = 1
      else
	xbodfnd = 0
      endif

      return
      end


c***************************************** 
c  xbodvar:
C  2nd-stage bridge to BODVAR, in Fortran
c****************************************** 

      subroutine xbodvar(body, item, i, dim, values)
      
      integer body
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      character*80 text

      text=' '

      if (i.gt.80) call xvmessage('xbodvar, string too long',' ')

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      call bodvar(body, text, dim, values)

      return
      end


c***************************************** 
c  xbodn2c:
C  2nd-stage bridge to BODN2C, in Fortran
c*****************************************  
      subroutine xbodn2c(body_name, i, body_id, status)
      
      byte body_name(1)
      integer body_id
      integer i
      integer status
      character*80 text
      logical found

      text=' '

      if (i.gt.80) call xvmessage('xbodn2c, string too long',' ')

C     Transformation to Fortran-string
      call mvlc(body_name, text, i)

      call bodn2c(text, body_id, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 

      return
      end


c***************************************** 
c  xspklef:
C  2nd-stage bridge to SPKLEF, in Fortran
c****************************************** 

      subroutine xspklef(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) call xvmessage('xspklef, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call spklef(text, handl)

      return
      end


c***************************************** 
c  xutc2et:
C  2nd-stage bridge to UTC2ET, in Fortran
c****************************************** 

      subroutine xutc2et(utc, i, et)
      
      integer i
      byte utc(1)
      double precision et
      character*80 text

      if (i.gt.80) call xvmessage('xutc2et, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(utc, text, i)

      call utc2et(text, et)

      return
      end


c***************************************** 
c xcklpf:
C 2nd-stage bridge to CKLPF, in Fortran
c****************************************** 

      subroutine xcklpf(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) call xvmessage('xcklpf, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call cklpf(text, handl)

      return
      end


c***************************************** 
c xckgp:
C  2nd-stage bridge to CKGP, in Fortran
c****************************************** 

      subroutine xckgp(inst,sclkdp,tol,ref,i,cmat,clkout,status)

      
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


      if (i.gt.80) call xvmessage('xckgp, string too long',' ')

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
c xckgpav:
C  2nd-stage bridge to CKGPAV, in Fortran
c****************************************** 

      subroutine xckgpav(inst,sclkdp,tol,ref,i,cmat,av,clkout,status)

      
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


      if (i.gt.80) call xvmessage('xckgpav, string too long',' ')

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
c xerrprt:
C 2nd-stage bridge to ERRPRT in Fortran
c****************************************** 

      subroutine xerrprt(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	call xvmessage('xerrprt, first string is too long',' ')
      endif

      if (j.gt.80) then
	call xvmessage('xerrprt, second string is too long',' ')
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
c xdafopr:
C 2nd-stage Bridge to DAFOPR, in Fortran
c****************************************** 

      subroutine xdafopr(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) call xvmessage('xdafopr, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopr(text,handle)

      return
      end

c***************************************** 
c xspkapp:
C 2nd-stage bridge to SPKAPP in Fortran
c****************************************** 

      subroutine xspkapp(targ,et,ref,i,sobs,abcorr,j,starg,lt)
      
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
	call xvmessage('xspkapp, first string is too long',' ')
      endif

      if (j.gt.80) then
	call xvmessage('xspkapp, second string is too long',' ')
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
c xspkez:
C 2nd-stage bridge to SPKEZ in Fortran
c****************************************** 

      subroutine xspkez(targ,et,ref,i,abcorr,j,sobs,starg,lt)
      
      double precision et
      integer          sobs
      double precision starg(6)
      double precision lt
      integer          targ
      integer          i, j
      byte             ref(1)
      byte             abcorr(1)
      character*80     texta
      character*80     textb

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	call xvmessage( 'xspkez, first string is too long',' ')
      endif

      if (j.gt.80) then
	call xvmessage( 'xspkez, second string is too long',' ')
      endif

C     Initialize arrays

      texta = ' '
      textb = ' '

C     Transformations to Fortran-string

      call mvlc( ref, texta, i)
      call mvlc( abcorr, textb, j)

      call spkez( targ, et, texta, textb, sobs, starg, lt)

      return
      end


c***************************************** 
c   xspkssb:
C   2nd-stage bridge to SPKSSB, in Fortran
c*****************************************

      subroutine xspkssb(targ,et,ref,i,starg)
      
      double precision et
      double precision starg(6)
      integer targ
      integer i
      byte ref(1)
      character*80 text

      if (i.gt.80) call xvmessage('xspkssb, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)
      call spkssb(targ,et,text,starg)

      return
      end


C***************************************** 
C   xstr2et:
C   2nd-stage bridge to STR2ET, in Fortran
c*****************************************

      subroutine xstr2et( str, i, et)
      
      integer i
      byte str(1)
      double precision et
      character*80 text

      if (i.gt.80) call xvmessage('xstr2et, string too long',' ')

      text=' '

C     Transformation to Fortran-string
C
      call mvlc( str, text, i)
      call str2et( text, et)

      return
      end
c***************************************** 
c  xerract:
C  2nd-stage bridge to ERRACT in Fortran
c****************************************** 

      subroutine xerract(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	call xvmessage('xerract, first string is too long',' ')
      endif

      if (j.gt.80) then
	call xvmessage('xerract, second string is too long',' ')
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
c   xdaffna:
C   2nd-stage bridge to DAFFNA, in Fortran
c*****************************************
      subroutine xdaffna(status)

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
c   xdafopw:
C   2nd-stage Bridge to DAFOPW, in Fortran
c*****************************************
      subroutine xdafopw(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) call xvmessage('xdafopw, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopw(text,handle)

      return
      end


c*****************************************
c   xdafbna:
C   2nd-stage Bridge to DAFBNA, in Fortran
c***************************************** 

      subroutine xdafbna(handle, sum, name, i)
      
      double precision sum(*)
      integer handle
      integer i
      byte name(1)
      character*80 text

      if (i.gt.80) call xvmessage('xdafbna, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(name,text,i)

      call dafbna(handle,sum,text)

      return
      end


c***************************************** 
c  xldpool:
C  2nd-stage bridge to LDPOOL, in Fortran
c*****************************************

      subroutine xldpool(x,i)
      
      integer i
      byte x(1)
      character*80 text

      if (i.gt.80) call xvmessage('xldpool, string too long',' ')

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call ldpool(text)

      return
      end


C***************************************** 
C xscencd:
C 2nd-stage bridge to SCENCD, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
      subroutine xscencd(sc, btext, i, sclkdp)      
      integer sc
      byte btext(1)
      integer i
      double precision sclkdp

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.80) then
	call xvmessage('xscencd, string too long',' ')
      else
        text = ' '
        call mvlc( btext, text, i)
        call scencd(sc, text, sclkdp)      
      endif

      return
      end


C***************************************** 
C xscs2e:
C 2nd-stage bridge to SCS2E, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
      subroutine xscs2e(sc, SCLK, i, et)
      
      integer sc
      byte SCLK(1)
      integer i
      double precision et

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.32) then
	call xvmessage('xscs2e, string too long',' ')
      else
        text = ' '
        call mvlc(SCLK, text, i)
        call scs2e(sc, text, et)
      endif

      return
      end


C***************************************** 
C xet2utc:
C 2nd-stage bridge to ET2UTC, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
c  15feb00 -lwk- specified length of character string for format specifier
c               in order to accomodate recent change to zet2utc()
c


      subroutine xet2utc(et,format,fl,prec,UTCtmp) 

      double precision et
      integer prec
      byte format(1)
      integer	fl
      byte UTCtmp(80)
      character*80 text
      character*80  ftext

      call mvlc(format,ftext,fl)

      call et2utc(et,ftext(fl:fl),prec,text)

      call mvcl(text, UTCtmp, 80)

      return
      end

c***************************************** 
c  xrtpool:
C  2nd-stage bridge to RTPOOL, in Fortran
c  23nov94 -lwk- fixed 'flag' to be -1 on success
c****************************************** 

      subroutine xrtpool(item, i, dim, values, flag)
      
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      integer flag
      logical status

      character*80 text

      text=' '

      if (i.gt.80) call xvmessage('xrtpool, string too long',' ')

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
C xsctiks:
C 2nd-stage bridge to SCTIKS, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by: 		Sam Le
C Date:			April 27, 1995
C*************************************************
	subroutine xsctiks(sc, clkstr, i,  ticks)
	integer			sc, i
	byte			clkstr(1)
	character*12 		text
	double precision	ticks

C	Transformation to Fortran-string

	if (i .gt. 12) then
	   call xvmessage('xsctiks, clkstr is too long', ' ')
        else
	   text = ' '
	   call mvlc(CLKSTR, text, i)
	   call sctiks(sc, text, ticks)
        end if

	return
	end
C*************************************************
C xckbss:
C 2nd-stage bridge to xckbss, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xckbss(inst, sclk, tol, needav)
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
C xcksns:
C 2nd-stage bridge to xcksns, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xcksns(handle, descr, segid, found)
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
C xckpfs:
C 2nd-stage bridge to xckpfs, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xckpfs(handle, descr, sclkin, tol, 
     1		needav, cmat, av, clkout, found)
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
C xsurfpt:
C 2nd-stage bridge to xsurfpt, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xsurfpt(pos, u, a, b, c, pts, fnd)
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
C xdafa2b:
C 2nd-stage bridge to dafa2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xdafa2b(as, alen, da, dlen,resv)
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
C xdafb2a:
C 2nd-stage bridge to dafb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xdafb2a(da, dlen, as, alen)
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
C xspca2b:
C 2nd-stage bridge to spca2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xspca2b(ascfile, asclen, binfile, binlen)
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
C xspcb2a:
C 2nd-stage bridge to spcb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xspcb2a(binfile, binlen, ascfile, asclen)
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
C xtxtopr:
C 2nd-stage bridge to txtopr, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xtxtopr(fname, flen, unit)
        integer flen, unit
        character*80 fname, ffname

        ffname = ' '

        call mvlc(fname, ffname, flen)
        call txtopr(ffname, unit)
        return
        end
C*************************************************
C xspct2b:
C 2nd-stage bridge to spct2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xspct2b(unit, fname, len)
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
C xtxtcls:
C 2nd-stage bridge to close, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xtxtcls(txtunit)
        integer txtunit
        close (txtunit)
        return
        end
C***************************************************
C xsce2s:
C 2nd-stage bridge to convert SCET to SCLK string
C
C Part of bridge to be called from C applications
C Written by:   	Sam Le
C Date      :		June 6, 1995
C***************************************************
	subroutine xsce2s(sc, etc, sclk)
	integer			sc
	double precision	etc
	character*(*)		sclk
	character*80		text

	call sce2s(sc, etc, text)
	call mvcl(text, sclk, 80)

	return
	end
C*******************************************************************
C xdafhfn:
C 2nd-stage bridge to xdafhfn, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:		Sam Le
C Date	    : 		September 14, 1995
C*******************************************************************
	subroutine xdafhfn (handle, fname)
	integer		handle
	byte		fname(256)
	character*256	temp

	call dafhfn(handle, temp)
	call mve(1, 256, temp, fname, 1, 1)

	return
	end 
