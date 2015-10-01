$!****************************************************************************
$!
$! Build proc for MIPL module spbri
$! VPACK Version 1.9, Thursday, July 13, 2000, 11:31:54
$!
$! Execute by entering:		$ @spbri
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module spbri ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to spbri.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("spbri.imake") .nes. ""
$   then
$      vimake spbri
$      purge spbri.bld
$   else
$      if F$SEARCH("spbri.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spbri
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spbri.bld "STD"
$   else
$      @spbri.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spbri.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spbri.com -
	-s xspbri.f xspbri_gll_ext.f zspbri.c zspbri_gll_ext.c -
	-i spbri.imake -
	-t tspbri1.c tspbri1.pdf tspbri1.imake tspbri2.c tspbri2.pdf -
	   tspbri2.imake tsubs.c tsubs.pdf tsubs.imake tstspbri.pdf -
	-o spbri.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xspbri.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xspbri_gll_ext.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C
C	FORTRAN Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:
C
C		BODN2C_G
C
C

c***************************************** 
c  xbodn2c_g:
C  2nd-stage bridge to BODN2C_G, in Fortran
c*****************************************  
      subroutine xbodn2c_g(body_name, i, body_id, status)
      
      byte body_name(1)
      integer body_id
      integer i
      integer status
      character*80 text
      logical found

      text=' '

      if (i.gt.80) call xvmessage('xbodn2c_g, string too long',' ')

C     Transformation to Fortran-string
      call mvlc(body_name, text, i)

      call bodn2c_g(text, body_id, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 
  
      return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zspbri.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*

C Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:

        BODEUL  DAFCLS  DPR     M2Q     SCENCD
        BODFND  DAFENA  ERRACT  MXM     SCE2T
        BODVAR  DAFFNA  ERRPRT  MXMT    SCS2E
	BODN2C  		MXV	SCT2E	
	CKGP			MTXV	SPKAPP
        CKGPAV  DAFGS   ET2UTC  PI	SPKLEF
        CKLPF   DAFHSF  FAILED  RECLAT  SPKSSB
        CLPOOL  DAFOPR  HALFPI          TWOPI
        DAFADA  DAFOPW  IRFROT  ROTATE  UTC2ET
        DAFBFS  DAFPS   LDPOOL  ROTMAT  VMINUS
        DAFBNA  DAFUS   M2EUL   RTPOOL  XPOSE
	======================================
	CKBSS	CKSNS 	CKPFS	CKUPF
	DAFA2B	DAFB2A	EUL2M	SCTIKS
	SPCA2B	SPCB2A	SPKUEF	SURFPT
	VADD	VNORM	VSEP	VSUB
	TXTOPR  SPCT2B	TXTCLS	SCE2S
	Written By: Sam Le, 4/27/95
	======================================
        LATREC
        Done by: Helen Mortensen
*
* -----------   --------------- ------------------------------------------
*  7-Oct-1999	Payam Zamani	Fixed bug in ET2UTC call interface.  Format
*				parameter was passed as char rather than
*				pointer to array of charcters.
* 30-Sep-1999	Payam Zamani	Added C bridges for:
*					STR2ET
*					SPKEZ
*					VPACK
*					RESET
*/

/*
1st bridge for BODEUL, called from C 
*/
void zbodeul( body, et, ra, dec, w, lambda )
int    body;         	/* input */
double et;          	/* input */
double *ra;         	/* output */
double *dec;         	/* output */
double *w;         	/* output */
double *lambda;         /* output */

{
   FTN_NAME(bodeul) ( &body, &et, ra, dec, w, lambda );
}

/*
 1st bridge for BODFND, called from C
*/
int zbodfnd(body, item)

int body;
char *item;
{
   int i, status;
   i=strlen(item);

   status = FTN_NAME(xbodfnd) (&body, item, &i );
   return status;
}

/*
 1st bridge for BODVAR, called from C 
*/
void zbodvar(body, item, dim, values)

int body;
char *item;
int *dim;
double *values;
{
   int i;
   i=strlen(item);

   FTN_NAME(xbodvar) (&body, item, &i, dim, values);
}


/*
 1st bridge for BODN2C, called from C 
*/
void zbodn2c(body_name, body_id, status)

char *body_name;
int *body_id;
int *status;
{
   int i;

   i = strlen(body_name);

   FTN_NAME(xbodn2c) (body_name, &i, body_id, status);
}


/*
 1st bridge for CKGP, called from C 
*/
void zckgp(inst, sclkdp, tol, ref, cmat, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xckgp) (&inst,&sclkdp,&tol,ref,&i,cmat,clkout,status);
}

/*
 1st bridge for CKGPAV, called from C 
*/
void zckgpav(inst, sclkdp, tol, ref, cmat, av, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
void *av;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xckgpav) (&inst,&sclkdp,&tol,ref,&i,cmat,av,clkout,status);
 }

/*
 1st bridge for CKLPF, called from C 
*/
void zcklpf(x, handl)

char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME(xcklpf) (x, &i, handl);
}

/*
1st bridge for CLPOOL, called from C 
*/
void zclpool()
{
   FTN_NAME(clpool) ();
}

/*
Bridge for DAFADA, called from C 
*/
void zdafada( buf, n)
int n;       /*input*/
void *buf;      /*input*/

{
   FTN_NAME(dafada) ( buf, &n);
}

/*
1st bridge for DAFBFS, called from C 
*/
void zdafbfs( handle )
int    handle;      /*input*/

{
   FTN_NAME(dafbfs) ( &handle );
}

/*
 1st-stage bridge for DAFBNA, called from C 
*/
void zdafbna( handle, sum, name)

char *name; /*input*/
int handle; /*input*/
void *sum; /*input*/
{
   int i;
   i=strlen(name);

   FTN_NAME(xdafbna) (&handle, sum, name, &i);
}

/*
 1st bridge for DAFCLS, called from C 
*/
void zdafcls(handle)

int handle;      /*input*/
{
   FTN_NAME(dafcls) (&handle);
}

/*
Bridge for DAFENA, called from C 
*/
void zdafena()

{
  FTN_NAME(dafena) ();
  return;
}


/*
 1st bridge for DAFFNA, called from C 
*/
void zdaffna(status)
int *status;     /*outpu*/
{
   FTN_NAME(xdaffna) (status);
}

/*
1st bridge for DAFGS, called from C 
*/
void zdafgs( sum )
void *sum;      /*output*/

{
   FTN_NAME(dafgs) ( sum );
}

/*
Bridge for DAFHSF, called from C 
*/
void zdafhsf( handle , nd, ni )
int    handle;      /*input*/
int    *nd;         /*output*/
int    *ni;         /*output*/

{
   FTN_NAME(dafhsf) ( &handle, nd, ni );
}

/*
 1st bridge for DAFOPR, called from C 
*/
void zdafopr(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME(xdafopr) (fname, &i, handle);
}


/*
 1st-stage bridge for DAFOPW, called from C 
*/
void zdafopw(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME(xdafopw) (fname, &i, handle);
}

/*
Bridge for DAFPS, called from C 
*/
void zdafps( nd, ni, dc, ic, sum )
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*input*/
void  *ic;        /*input*/
void *sum;      /*output*/

{
   FTN_NAME(dafps) ( &nd, &ni, dc, ic, sum );
}

/*
1st bridge for DAFUS, called from C 
*/
void zdafus( sum, nd, ni, dc, ic )
void *sum;      /*input*/
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*output*/
void  *ic;        /*output*/

{
   FTN_NAME(dafus) ( sum, &nd, &ni, dc, ic );
}

/*
1st bridge for DPR, called from C 
*/
double zdpr()
{
  double dpr();
  double FTN_NAME(dpr) ();

  return FTN_NAME(dpr) ();
}

/*
1st bridge for RPD, called from C 
*/
double zrpd()
{
  double rpd();
  double FTN_NAME(rpd) ();

  return FTN_NAME(rpd) ();
}

/*
 1st bridge for ERRACT, called from C 
*/
void zerract(x,y)
char *x,*y;
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xerract) (x,&i,y,&j);
}

/*
 1st bridge for ERRPRT, called from C 
*/
void zerrprt(x,y)
char *x;            /*input*/
char *y;          /*i/o*/
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xerrprt) (x,&i,y,&j);
}

/*

NAIF SPICE C-language Bridge for ET2UTC

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

 History:

 February 25, 1994   	DEVELOPER NOTE

 The UTC string variable UTC_temp is of fixed length 80 to provide an
 appropriate interface with FORTRAN SPICE routine ET2UTC. UTC_temp is pruned 
 to remove superfluous blanks and returned to the user in the string UTC.

 02mar95 -lwk- prune the UTC string *before* moving it to the user buffer,
		to reduce chance of overwriting memory (can't be totally
		avoided since there is no way to emulate the character*(*)
		argument to ET2UTC from C)
*/

void zet2utc(ephemeris_time,format,precision,UTC)
double ephemeris_time;
char *format;
int precision;
char *UTC;
{
int  i;
char UTC_temp[80];

    i = strlen( format);
    FTN_NAME(xet2utc) (&ephemeris_time, format, &i, &precision, UTC_temp);

/* Process FORTRAN character string */

/* Remove blanks from the end of this string */
i = 79;
while(UTC_temp[i] == ' ' && i>0)
	i--;

strncpy(UTC, UTC_temp, ++i);

/* Add NULL terminator */
UTC[i] = '\0';
}

/*
 1st bridge for FAILED, called from C 
*/
int zfailed()

{
 int status;

   status = FTN_NAME(failed) ();
   return status;
}

/*
1st bridge for PI, called from C 
*/
double zpi()
{
  double pi();
  double FTN_NAME(pi) ();

  return FTN_NAME(pi) ();
}

/*
1st bridge for HALFPI, called from C 
*/
double zhalfpi()
{
  double halfpi();
  double FTN_NAME(halfpi) ();

  return FTN_NAME(halfpi) ();
}

/*
1st bridge for TWOPI, called from C 
*/
double ztwopi()
{
  double twopi();
  double FTN_NAME(twopi) ();

  return FTN_NAME(twopi) ();
}

/*
1st bridge for IRFROT, called from C 
*/
void zirfrot( refa, refb, rotab )
int    refa;  /*input*/
int    refb;  /*input*/
void *rotab;  /*output*/

{
 FTN_NAME(irfrot) ( &refa, &refb, rotab );
}

/*
 1st bridge for LDPOOL, called from C 
*/
void zldpool(x)
char *x;
{
   int i;
   i=strlen(x);
   FTN_NAME(xldpool) (x,&i);
}

/*
1st bridge for M2EUL, called from C 
*/
void zm2eul( r, axis3, axis2, axis1, angle3, angle2, angle1 )
void *r;        /* input */
int    axis3;        /* input */
int    axis2;        /* input */
int    axis1;        /* input */
double *angle3;        /* output */
double *angle2;        /* output */
double *angle1;        /* output */

{
   FTN_NAME(m2eul) ( r, &axis3, &axis2, &axis1, angle3, angle2, angle1 );
}

/*
Bridge for M2Q, called from C 
*/
void zm2q( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/


{
   FTN_NAME(m2q) ( m1, mout );
}

/*
1st bridge for MXM, called from C 
*/
void zmxm( m1, m2, mout )
void *m1;   /*input*/
void *m2;   /*input*/
void *mout;   /*output*/


{
 FTN_NAME(mxm) ( m1, m2, mout );
}

/*
1st bridge for MXMT, called from C 
*/
void zmxmt( m1, m2, mout )
void *m1;		/*input*/
void *m2;		/*input*/
void *mout;		/*output*/


{
 FTN_NAME(mxmt) ( m1, m2, mout );
}

/*
1st bridge for MXV, called from C 
*/
void zmxv( matrix, vin, vout )
void  *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 FTN_NAME(mxv) ( matrix, vin, vout);
}

/*
1st bridge for MTXV, called from C 
*/
void zmtxv( matrix, vin, vout )
void *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 FTN_NAME(mtxv) ( matrix, vin, vout);
}

/*
1st bridge for RECLAT, called from C 
*/
void zreclat(rectan, radius, longi, lat)

void *rectan;  /*input*/
double *radius;
double *longi;
double *lat;
{
   FTN_NAME(reclat) (rectan, radius, longi, lat);
}


/*
1st bridge for ROTATE, called from C 
*/
void zrotate( angle, iaxis, mout )
double angle;        	/* input */
int    iaxis;         	/* input */
void *mout;   		/* output */

{
   FTN_NAME(rotate) ( &angle, &iaxis, mout );
}

/*
1st bridge for ROTMAT, called from C 
*/
void zrotmat( m1, angle, iaxis, mout )
void  *m1;      /*input*/
double angle;      /*input*/
int    iaxis;      /*input*/
void  *mout;      /*output*/

{
   FTN_NAME(rotmat) ( m1, &angle, &iaxis, mout );
}

/*
 1st bridge for RTPOOL, called from C 
*/
void zrtpool(x,dim,values,flag)
char 	*x;
int 	*dim;
double 	*values;
int 	*flag;
{
   int i;
   i=strlen(x);

   FTN_NAME(xrtpool) (x,&i,dim,values,flag);
}

/*
1st bridge for SCENCD, called from C 
*/
void zscencd( sc, text, sclkdp)
int    sc;           /*input*/
char *text;         /*input*/
double *sclkdp;       /*output*/

{
  int i;
  i=strlen(text);
  FTN_NAME(xscencd) (  &sc, text, &i, sclkdp);
}

/*
1st bridge for SCE2T, called from C 
*/
void zsce2t( sc, et, sclkdp)
int    sc;           /*input*/
double et;         /*input*/
double *sclkdp;       /*output*/

{
   FTN_NAME(sce2t) (  &sc, &et, sclkdp);
}

/*

NAIF SPICE C-language Bridge for SCS2E

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

*/
void zscs2e(spacecraft_code,SCLK,ephemeris_time)
int spacecraft_code;
char *SCLK;
double *ephemeris_time;
{
int i;

i=strlen(SCLK);		/* Get string length of SCLK */
FTN_NAME(xscs2e) (&spacecraft_code, SCLK, &i, ephemeris_time);
}

/*
1st bridge for SCT2E, called from C 
*/
void zsct2e( sc, sclkdp, et )
int    sc;           /*input*/
double sclkdp;      /*input*/
double *et;          /*output*/

{
   FTN_NAME(sct2e) ( &sc, &sclkdp, et );
}

/*
 1st bridge for SPKAPP, called from C 
*/
void zspkapp( targ, et, ref, sobs, abcorr, starg, lt )

int targ;          /*input*/
double et;        /*input*/
char *ref;        /*input*/
void *sobs;        /*input*/
char *abcorr;        /*input*/
void *starg;        /*output*/
double *lt;       /*output*/
{
   int i,j;
   i=strlen(ref);
   j=strlen(abcorr);

   FTN_NAME(xspkapp) (&targ,&et,ref,&i,sobs,abcorr,&j,starg,lt);
}

/*
 *===========================================================================
 * 1st bridge for FAILED, called from C 
 *===========================================================================
 */
int zreset()

{
	int		status;

    status = FTN_NAME(reset)();
    return status;
}
/*
 *===========================================================================
 * 1st bridge for SPKEZ, called from C
 *===========================================================================
 */
void	zspkez( 
int		target,			/* target id			*/
double		et,			/* Ephemeris time		*/
char		*ref,
char		*abcorr,
int		obs,
void		*strag,
double		*lt)
{
	int		i;
	int		j;

    i = strlen( ref);
    j = strlen( abcorr);
    FTN_NAME( xspkez)( &target,	&et, ref, &i, abcorr, &j, &obs, strag, lt);
}


/*
 1st bridge for SPKLEF, called from C 
*/
void zspklef(x, handl)
char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME(xspklef) (x, &i, handl);
}

/*
 1st bridge for SPKSSB, called from C 
*/
void zspkssb(targ, et, ref, starg)

int targ;           /*input*/
double et;           /*input*/
char *ref;           /*input*/
void *starg;      /*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xspkssb) (&targ, &et, ref, &i, starg);
}

/*
 *===========================================================================
 * 1st bridge for STR2ET, called from C
 *===========================================================================
 */
void	zstr2et(
char		*str,
double		*et)
{
	int		i;

    i = strlen( str);
    FTN_NAME( xstr2et) ( str, &i, et);
}

/*
 1st bridge for UTC2ET, called from C 
*/
void zutc2et(utc, et)

char *utc;
double *et;

{
   int i;
   i=strlen(utc);

   FTN_NAME(xutc2et) (utc, &i, et);

}

/*
1st bridge for VMINUS, called from C 
*/
void zvminus( v1, vout )
void *v1;       /*input*/
void *vout;      /*output*/


{
   FTN_NAME(vminus) ( v1, vout );
}

/*
 *============================================================================
 * 1st bridge for VPACK, called from C
 *============================================================================
 */
void	zvpack(
double		x,
double		y,
double		z,
void		*v)
{
    FTN_NAME( vpack) ( &x, &y, &z, v);
}

/*
Bridge for XPOSE, called from C 
*/
void zxpose( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/
{
 FTN_NAME(xpose) ( m1, mout );
}
/*=====================================================
1st bridge for CKBSS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Search through loaded file to find data segment with the
same s/c instrument and sclk value (or within sclk_tol)
=======================================================*/
void zckbss(inst, sclk, tol, needav)
 int	inst;				/* input: instrument id	*/
 double	sclk;				/* input: sclk value	*/
 double	tol;				/* input: error_tolrnce */
 int	needav;				/* input: need av ?	*/
{
 FTN_NAME(ckbss) (&inst, &sclk, &tol, &needav);
}
/*=====================================================
1st bridge for CKSNS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
CKBSS specifies the search criteria and CKSNS searches
through the loaded files for data segment(s) that match
the criteria; segment id and descriptor are also returned
=======================================================*/
void zcksns(int *handle, double *descr, char *segid, int *found, ZFORSTR_PARAM)
#if 0
 int		*handle;		/* output	*/
 double		*descr;			/* output	*/
 char 		*segid;			/* output	*/
 int		*found;			/* output	*/
#endif
{
 ZFORSTR_BLOCK
 char	temp[41];

 FTN_NAME(xcksns) (handle, descr, temp, found);
 zsfor2c(segid, 40, temp, &handle, 4, 3, 1, found);
}
/*==============================================
1st bridge for CKPFS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Evaluate pointing data from a given segment for
a given time.
================================================*/
void zckpfs(handle, descr, sclkin, tol, needav, 
			cmat, av, clkout, found)
 int		handle;			/*   input	*/
 double		*descr;			/*   input	*/
 double		sclkin;			/*   input	*/
 double		tol;			/*   input	*/
 int		needav;			/*   input	*/
 double		*cmat;			/*   output	*/
 double		*av;			/*   output	*/
 double		*clkout;		/*   output	*/
 int		*found;			/*   output	*/
{
 int		i, j;
 double		*dptr, tcmat[3][3];

 FTN_NAME(xckpfs) (&handle, descr, &sclkin, &tol,
		&needav, tcmat, av, clkout, found);
 dptr = cmat;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = tcmat[j][i];
       dptr++;
       }
}
/*=====================================================
1st bridge for CKUPF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload a CK pointing file so that it will no longer be
searched by the readers.
=======================================================*/
void zckupf(handle)
 int handle;
{
 FTN_NAME(ckupf) (&handle);
}
/*=====================================================
1st bridge for DAFA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFA2B
converts the ascii (text) file into its daf file (binary).
=======================================================*/
void zdafa2b(ascii_name, daf_name, resv)
 char	*ascii_name;
 char	*daf_name;
 int	resv;
{
 int	asc_len,
	daf_len;

 asc_len = strlen(ascii_name);
 daf_len = strlen(daf_name);

 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFA2B::File Name Is Too Long\n");
 else
    FTN_NAME(xdafa2b) (ascii_name, &asc_len,
		daf_name, &daf_len, &resv);
}
/*=====================================================
1st bridge for DAFB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFB2A
converts the daf (binary) file into its ascii file (text).
=======================================================*/
void zdafb2a(dname, aname)
 char   *dname;
 char   *aname;
{
 int    daf_len,
        asc_len;

 asc_len = strlen(aname);
 daf_len = strlen(dname);
 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFB2A::File Name Is Too Long\n");
 else
    FTN_NAME(xdafb2a) (dname, &daf_len, aname, &asc_len);
}
/*=====================================================
1st bridge for EUL2M, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
EUL2M constructs a rotation matrix from a set of Euler
angles and their rotation axes

last modified: Tue Oct  8 18:17:04 PDT 1996
	remove matrix inversion requested by LWK.
=======================================================*/
void zeul2m(angle3, angle2, angle1, axis3, axis2, axis1, matrix)
 double                 angle3;                 /* input        */
 double                 angle2;                 /* input        */
 double                 angle1;                 /* input        */
 int                    axis3;                  /* input        */
 int                    axis2;                  /* input        */
 int                    axis1;                  /* input        */
 double                 *matrix;                /* output       */
{
 FTN_NAME(eul2m) (&angle3, &angle2, &angle1,
                &axis3, &axis2, &axis1, matrix);
}
/*=====================================================
1st bridge for SPCA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCA2B convert the given ascii CK/SPK file into its
binary format, including the comment area.
=======================================================*/
void zspca2b(ascfile, binfile)
 char *ascfile; char *binfile;
{
 int    asclen, binlen;

 asclen = strlen(ascfile);
 binlen = strlen(binfile);
 if ((asclen > 80) || (binlen > 80))
    printf("ZSPCA2B: File name is too long\n");
 else
    FTN_NAME(xspca2b) (ascfile, &asclen, binfile, &binlen);
}
/*=====================================================
1st bridge for SPCB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCB2A convert the given binary CK/SPK file into its
ascii format, including the comment area.
=======================================================*/
void zspcb2a(binfile, ascfile)
 char *binfile;
 char *ascfile;
{
 int binlen, asclen;

 binlen = strlen(binfile);
 asclen = strlen(ascfile);
 if ((binlen > 80) || (asclen > 80))
    printf("ZSPCB2A: File name is too long\n");
 else
    FTN_NAME(xspcb2a) (binfile, &binlen, ascfile, &asclen);
}
/*=====================================================
1st bridge for SCTIKS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSCTIKS receives a s/c_id and a clock string then
convert the clock string into ticks for that specific
spacecraft
=======================================================*/
void zsctiks(sc, clkstr, ticks)
 int            sc;
 char           *clkstr;
 double         *ticks;
{
 int    i;
 char	for_str[15];

 i = strlen(clkstr);
 FTN_NAME(xsctiks) (&sc,clkstr, &i, ticks);
 }
/*=====================================================
1st bridge for SPKUEF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload an ephemeris file so that it will no longer be
searched by the readers
=======================================================*/
void zspkuef(handle)
 int handle;
{
 FTN_NAME(spkuef) (&handle);
}
/*=====================================================
1st bridge for VADD, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
add two vector v1[3] and v2[3] and return the results
back via vout[3]
=======================================================*/
void zvadd_mat(v1, v2, vout)
 double	*v1;
 double *v2;
 double *vout;
{
 FTN_NAME(vadd) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFPT, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Determine the intersection of a line-of-sight vector
with the surface of an ellipsoid.
=======================================================*/
void zsurfpt(pos, u, a, b, c, pts, fnd)
 double *pos;			/* input	*/
 double *u;			/* input	*/
 double a;			/* input	*/
 double b;			/* input	*/
 double c;			/* input	*/
 double *pts;			/* output	*/
 int	*fnd;			/* output	*/
{
 FTN_NAME(xsurfpt) (pos, u, &a, &b, &c, pts, fnd);
}
/*=====================================================
1st bridge for VNORM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the magnitude of a double precision,
3-dimensional vector.
=======================================================*/
double zvnorm(v)
 double *v;
{
 double value;
 double FTN_NAME(vnorm) (double *);
 return (FTN_NAME(vnorm) (v));
}
/*=====================================================
1st bridge for VSUB, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the different between two 3-dimensional, double
precision vectors
=======================================================*/
void zvsub(v1, v2, vout)
 double	*v1;			/* input	*/
 double *v2;			/* input	*/
 double *vout;			/* output	*/
{
 FTN_NAME(vsub) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFNM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Computes the outward-pointing, unit normal vector
from a point on the surface of an ellipsoid.
=======================================================*/
void zsurfnm(a, b, c, pt, out)
 double	a;
 double b;
 double c;
 double *pt;
 double *out;
{
 FTN_NAME(surfnm) (&a, &b, &c, pt, out);
}
/*=====================================================
1st bridge for VSEP, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Find the separation angle in radians between two double
precision, 3-dimensional vectors. This angle is defined as
zero if either vector is zero.
=======================================================*/
double zvsep(v1, v2)
 double	*v1;
 double *v2;
{
 double FTN_NAME(vsep) (double *, double *);
 return (FTN_NAME(vsep) (v1, v2));
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Open a text (ASCII) file for reading, return the unit number
========================================================*/
void ztxtopr(fname, unit)
 char *fname; int *unit;
{
 int flen;

 flen = strlen(fname);
 if ((flen > 80) || (flen <= 0)) {
    printf("ZTXTOPR: File name is too long/short\n");
    exit (0);
    }
 FTN_NAME(xtxtopr) (fname, &flen, unit);
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Convert a given text file into its corresponding binary
format.
========================================================*/
void zspct2b(unit, fname)
 int	unit;
 char	*fname;
{
 int	len;
 len = strlen(fname);
 if (len >= 80) 
    printf("Error File Name Is Too Long\n");
 else
    FTN_NAME(xspct2b) (&unit, fname, len);
}
/*======================================================
1st bridge for TXTCLS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Close the text file associated with the given unit
========================================================*/
void ztxtcls(unit)
 int	unit;
{
 FTN_NAME(xtxtcls) (&unit);
}
/*=======================================================
1st bridge for SCE2S, called from C
Written By: Sam Le
Date	  : 06/6/1995

Development Note:
Convert a given ETC (double precision) to SCLK string
=========================================================*/
void zsce2s(sc, etc, sclk)
 int	sc;
 double	etc;
 char	*sclk;
{
 int	i;
 char	SCLK_temp[80];

 FTN_NAME(xsce2s) (&sc, &etc, SCLK_temp);

 i = 79;
 while (SCLK_temp[i] == ' ') i--;
 i++; SCLK_temp[i] = '\0';
 strcpy(sclk, SCLK_temp);
}
/*=======================================================
1st bridge for DAFGH, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFGH returns the handle of the file being searched
currenty. MIPS SPICE routines used this with GAFHFN to
retrieve the name and id of the file being searched.
=========================================================*/
void zdafgh(handle)
 int *handle;
{
 FTN_NAME(dafgh) (handle);
}
/*=========================================================
1st bridge for DAFHFN, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFHFN return the name of the file currently being searched
given its file handle. This subroutine does not check for
the string length of the fname. It assumes that the calling
program allocate enough memory space to store the file name.
===========================================================*/
void zdafhfn(int *handle, char *fname, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 char	temp_fname[256];

 FTN_NAME(xdafhfn) (handle, temp_fname);
 zsfor2c(fname, 256, temp_fname, &handle, 2, 2, 1, fname);
}
/*=========================================================
1st bridge for LATREC, called from C
Written by: Helen Mortensen
Date	  : 01/04/1998

===========================================================*/
void zlatrec(range, lon, lat, rec_coordinates)
 double range, lon, lat, *rec_coordinates;
{
 FTN_NAME(latrec) (&range, &lon, &lat, rec_coordinates);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zspbri_gll_ext.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/*

C Language Bridges for GALILEO NAIF TOOLKIT ROUTINES:

	BODN2C_G

*/

/*
 1st bridge for BODN2C_G, called from C 
*/
void zbodn2c_g(body_name, body_id, status)

char *body_name;
int *body_id;
int *status;
{
   int i;

   i = strlen(body_name);

   FTN_NAME(xbodn2c_g) (body_name, &i, body_id, status);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create spbri.imake

#define SUBROUTINE   	spbri

#define MODULE_LIST  	zspbri.c zspbri_gll_ext.c \
			xspbri.f xspbri_gll_ext.f
#define USES_ANSI_C
#define USES_FORTRAN
#define P2_SUBLIB
#define FTN_STRING

/*#define DEBUG		/* for development only */
/*#define LIB_LOCAL	/* for development only */
$ Return
$!#############################################################################
$Test_File:
$ create tspbri1.c
#include <stdio.h>
#include "vicmain_c"
#include "mp_routines.h"


/**********************************************************************
 
Test Program for C Bridges (TSPBRI1.C)

Program calls mpGetPar which calls mpPConstants and a low
level which in turn uses a subset of the C bridges for NAIF
SPICE toolkit.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		October 1993
Revision history:
                        February 1, 94 	(JFM)
				
			Success status flags for MP routines
                        revised in unit test source to be 
                        consistent with MP.H (FR 82914 - JFM059)
***********************************************************************/
int mp_debug;

void main44()
{
extern int mp_debug;
int	count;
int 	i,j,k;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];
char    ntest[3];

double	double_value;

MP mp_obj;
mp_debug = FALSE;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of SPBRI bridges for SPICE routines:"," ");
zvmessage("\t\tERRACT, LDPOOL, BODN2C_G, BODVAR\n"," ");
zvmessage("***************************************************\n"," ");

/* make this call right away in order to link in the local version of SPBRI */
zerract("SET","IGNORE");

/*

Define user parameters to be retrieved from PDF file.

*/

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_BODY);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);
	
strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

pdf_parms[17][0] = '\0';
pds_keys[17][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvp("PCK_PATH",PCKpath,&count);
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}

zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]] = '\0';

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKfile );
if ( status < 0 )
	{
	zvmessage("Error inzgetpar call"," ");
	zvmessage("Test failed."," ");
	return;
	}

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	return;
	}

for ( i=0; i<number_keywords; i++ )
	switch ( types[i] )	{

	case mpCHAR:

		status = mpGetValues( mp_obj,keys[i],string_value,"" );
		if (status < mpSUCCESS ) return;
		
		sprintf(string,"KEYWORD %s equals %s\n",keys[i],string_value);
		zvmessage(string," ");
		
		break;

	case mpDBLE:

		status = mpGetValues( mp_obj,keys[i],&double_value,"" );
		if (status < mpSUCCESS ) return;
		
		sprintf(string,"KEYWORD %s equals %4.3e\n",keys[i],double_value);
		zvmessage(string," ");

		break;

	default:

		zvmessage("PDS KEY of unacceptable data type"," ");
		break;	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tspbri1.pdf
process help=*
PARM TARGET		STRING	COUNT=1
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=1
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1
PARM CARTESIAN_AZ	REAL	COUNT=0:1
PARM LINE_OFFSET	REAL	COUNT=0:1
PARM SAMPLE_OFFSET	REAL	COUNT=0:1
PARM PARALLEL_ONE	REAL	COUNT=0:1
PARM PARALLEL_TWO	REAL	COUNT=0:1
PARM PCK_PATH		STRING 	COUNT=1

end-proc
.TITLE
VICAR program TSPBRI1
.HELP
PURPOSE:
This program is a simple test program for the C bridges for
the a subset of NAIF SPICE toolkit functions. This program
calls MPGETPAR and prints the contents of the map projection 
object.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.END
$!-----------------------------------------------------------------------------
$ create tspbri1.imake
#define PROGRAM   tspbri1

#define MODULE_LIST tspbri1.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_SPICE
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB

#define DEBUG		/* for local testing */
#define LIB_LOCAL	/* for local testing */
$!-----------------------------------------------------------------------------
$ create tspbri2.c
#include <stdio.h>
#include "vicmain_c"
#include <math.h>

#define MAX_INPUTS	20
#define MAX_LENGTH	80

#define CHECKif(x) if(x){ zvmessage("ERROR IN TEST ROUTINE"," "); zabend();}

/**********************************************************************
 
Test Program for C Bridges

This program tests ERRACT, ET2UTC, LDPOOL, UTC2ET and SCS2E bridge routines.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		February 1994
Revision history:	June 24, 1994	JFM
			
			Null terminator added to SCLK_time string to
			avoid failure on the DEC ALPHA. (FR 85089)

*/

void main44()
{
int	count;
int	minimum_count;
int 	i,j,k;
int	time_indices[MAX_LENGTH];
int	time_lengths[MAX_LENGTH];
int	status;
int	precision[MAX_INPUTS];
int	spacecraft_codes[MAX_INPUTS];
char	string[300];
char 	leapseconds_kernel[100],sclk_kernel[100],pc_kernel[100];
char	kernel_path[100];
char    format[MAX_INPUTS*2], cformat[2];
char 	UTC_times[MAX_INPUTS*MAX_LENGTH],SCLK_times[MAX_INPUTS*MAX_LENGTH];
char	UTC_time[MAX_LENGTH],SCLK_time[MAX_LENGTH];
double  m_values[3];
double  ephemeris_time;
float	ephemeris_times[MAX_INPUTS];
double	double_value;

zvmessage("************************************************************"," ");
zvmessage("\n\tTest of Bridges for SPICE Routines: \n"," ");
zvmessage("\n\tERRACT, ET2UTC, LDPOOL, RTPOOL, UTC2ET, SCS2E\n"," ");
zvmessage("************************************************************"," ");

/* Set NAIF SPICE Toolkit Error Handling values */
zerract("SET","IGNORE");

/*

Get user parameters from PDF file.

*/
status = zvp("SCLK_KERNEL",sclk_kernel,&count);
CHECKif( status < 0 );

status = zvp("LEAP_KERNEL",leapseconds_kernel,&count);
CHECKif( status < 0 );

status = zvp("PC_KERNEL",pc_kernel,&count);
CHECKif( status < 0 );

status = zvp("ET_INPUTS",ephemeris_times,&minimum_count);
CHECKif( status < 0 );

status = zvp("SCLK_INPUTS",SCLK_times,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

zvsptr( SCLK_times,count,time_indices,time_lengths);

status = zvp("SPACECRAFT_CODE",spacecraft_codes,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

status = zvp("PRECISION",precision,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

status = zvp("FORMAT",format,&count);
CHECKif( status < 0 );

if( count < minimum_count )
	minimum_count = count;

/* Load Kernel pools */
zldpool(sclk_kernel);

zldpool(leapseconds_kernel);

zldpool(pc_kernel);

for( i=0; i<minimum_count; i++)
	{
	j = i+1;
	k = i*2;
        cformat[0] = format[k];
        cformat[1] = '\0';

	/* print output message for each set of user input */
	sprintf(string,"\n\n*** User input %d ***\n",j);
	zvmessage(string," ");

	/* get SCLK time from input parameter array of strings */
	time_indices[i]--;
	strncpy(SCLK_time,&SCLK_times[time_indices[i]],time_lengths[i]);
	SCLK_time[time_lengths[i]] = '\0';

	/* complete translations for various times */
	zscs2e(spacecraft_codes[i],SCLK_time,&ephemeris_time);
        zet2utc(ephemeris_time,cformat,precision[i],UTC_time);
        zutc2et(UTC_time,&double_value);

	/* print results of conversion */
	sprintf(string,"Ephemeris time for S/C code %d and SCLK %s : %f",
		spacecraft_codes[i],SCLK_time,ephemeris_time);
	zvmessage(string," ");

	sprintf(string,"Corresponding UTC time (precision %d, format %c) : %s",
               precision[i],cformat,UTC_time);
	zvmessage(string," ");
	sprintf(string,"Retranslated ET from UTC above : %f",double_value);
	zvmessage(string," ");
	}

/* Return a value from the leapseconds kernel */
zrtpool( "BODY401_RADII", &i, m_values, &status );
if ( status>=0 && i==3 )
	{
	zvmessage("\n\n*** RTPOOL Test ***\n"," ");
	sprintf(string,"Body 401 radii measures from kernel %s:",pc_kernel);
	zvmessage(string," ");
	sprintf(string,"\t( %f, %f, %f )\n",
		m_values[0],m_values[1],m_values[2] );
	zvmessage(string," ");
	}

zvmessage(" "," ");
zvmessage("************************************************************"," ");
zvmessage("\n\tEnd of test\n"," ");
zvmessage("************************************************************"," ");
zvmessage(" "," ");
}
$!-----------------------------------------------------------------------------
$ create tspbri2.pdf
process help=*
PARM LEAP_KERNEL 	STRING	COUNT=1		DEFAULT="LEAPSECONDS.KER"
PARM SCLK_KERNEL 	STRING	COUNT=1		DEFAULT="SCLK.KER"
PARM PC_KERNEL 		STRING	COUNT=1		DEFAULT="P_CONSTANTS.KER"
PARM ET_INPUTS		REAL	COUNT=1:20
PARM SCLK_INPUTS	STRING	COUNT=1:20
PARM SPACECRAFT_CODE	INTEGER COUNT=1:20
PARM FORMAT		STRING  COUNT=1:20
PARM PRECISION		INTEGER COUNT=1:20
end-proc
.TITLE
VICAR program TSPBRI2
.HELP
PURPOSE:
This program is a simple test program for the C bridges for
a subset of NAIF SPICE toolkit functions. The C bridges
ZLDPOOL, ZET2UTC, XET2UTC, ZSCS2E, and XSCS2E are tested by this
program.
.LEVEL1
.VARI LEAP_KERNEL
Leapseconds kernel file name.
.VARI PC_KERNEL
Planetary constants kernel file name.
.VARI SCLK_KERNEL
Spacecraft clock  kernel file name.
.VARI ET_INPUTS
Ephemeris time inputs.
.VARI SCLK_INPUTS
Spacecraft clock inputs.
.VARI UTC_INPUTS
UTC time inputs.
.VARI SPACECRAFT_CODES
NAIF standard spacecraft ID codes
.VARI FORMAT
Formats for printing of UTCs
.VARI PRECISION
Precision specifications for UTCs
.END
$!-----------------------------------------------------------------------------
$ create tspbri2.imake
#define PROGRAM   tspbri2

#define MODULE_LIST tspbri2.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_SPICE
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB

#define DEBUG		/* for local testing */
#define LIB_LOCAL	/* for local testing */
$!-----------------------------------------------------------------------------
$ create tsubs.c
#include "vicmain_c"
#include <string.h>
#include <stdlib.h>

#define NUM_SPK_SUM     5
/**********************************************************************
 
TSUBS: Test Program for additional C Bridges

Revision history:  

	28may00		lwk	replaced dafopr of SPK with spklef, and
				spklef of CK with cklpf (!)

        24may00         lwk     changed ET for SPKSSB lookup, as the old
				date was not in GLL_LONG_2.BSP (!)

	06mar96		lwk	replaced zreset() with zreset1()

	14dec94		lwk	added ckgp,mtxv,pi,scencd,twopi
		  
	06-24-94	JFM	Print statement (ZPRNT) after DAFGS removed.
				SUMMARY array must be unpacked by DAFUS
				prior to printing. (FR 85089)

	03-15-94 	TLT	Original test for bridges:dmpool_1,halfpi,
                                rotate,rotmat,mxv,mxm,mxmt,sct2e,bodeul,
                                m2eul,failed,irfrot,rspool_1,dafgs,dpr,sce2t,
                                dafus,dafbfs,dafhsf,daffna,errprt,spkssb,
                                spkapp,ckgpav,dafopr,dafcls,reset,vminus,
                                xpose,m2q,dafopw,dafps,dafbna,dafada,dafena.

***********************************************************************/


void main44()

{
  double        timout;
  double        spk_dbls[2];
  double        spk_ints[6];
  double	summary[NUM_SPK_SUM];
  double        x_pi;
  double        zpi();
  double        half_pi;
  double        zhalfpi();
  double        two_pi;
  double        ztwopi();
  double        zdpr();
  double	zrpd();
  double	sclk_tol;
  double	sclkdp;
  double        val;
  double        val4;
  double        val5;
  double        val6;
  double        val7;
  double        mout[3][3];
  double        mout2[3][3];
  double        matrix[3][3];
  double        matrix2[3][3];
  double        vin[3];
  double        vout[3];
  double        c_ra,c_dec,c_twist;
  double	etime;
  double	et;
  double	et_lt;
  double	craft_target_lt;
  double        c_matrix[3][3];
  double        target_craft[3];
  double        c_av[3];
  double	ssb_craft[6];
  double	ssb_target[6];
  double	craft_target[6];
  double        craft_target_pos[3]; 
  int           handle;
  int           found;
  int           count;
  int		num_doubles;
  int	        ck_id;
  int		num_ints;	
  int		daf_found;
  int           zfailed();
  char          sclkc[18];
  char          CKfile[200];
  char          LONGfile[200];
  char          SCLKfile[200];
  char          CONSTfile[200];
  char          LEAPSfile[200];
  char		*system = "J2000";
  char		*lighttime = "LT";
  char          *env;
  char          binpool[1024];

  zvmessage("/*************begin tsubs********************/","");
  zreset1();
  initspice();
  zvp("LONG_PATH",LONGfile,&count);
  zdafopr(LONGfile, &handle);
  if (zfailed()) {
    zvmessage(" DAFOPR failed","");
    zreset1();
  }
  else zprnt(4,1,&handle,"after dafopr ...handle = ");

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zspklef(LONGfile, &handle);
  zvmessage("after spklef ...","");
  if (zfailed()) zreset1();

  zdafbfs(handle);
  zvmessage("after dafbfs ...","");
  if (zfailed()) zreset1();

  zdafhsf(handle,&num_doubles,&num_ints);
  if (zfailed()) {
    zvmessage(" DAFHSF failed","");
    zreset1();
  }
  else {
    zprnt(4,1,&num_doubles,"after dafhsf ...num_doubles = ");
    zprnt(4,1,&num_ints,"after dafhsf ...num_ints = ");
  }

  zdaffna(&daf_found);
  if (zfailed()) {
    zvmessage(" DAFFNA failed","");
    zreset1();
  }
  else zprnt(4,1,&daf_found,"after daffna ...daf_found = ");

  memset(summary,0,8*NUM_SPK_SUM);
  zdafgs(summary);
  if (zfailed()) zreset1();

  zdafus(summary, num_doubles, num_ints, spk_dbls, spk_ints);
  if (zfailed()) {
    zvmessage(" DAFUS failed","");
    zreset1();
  }
  else {
    zprnt(8,2,spk_dbls,"after dafus ...spk_dbls = ");
    zprnt(4,2,spk_ints,"after dafus ...spk_ints = ");
  }

  et = -3.139E+08;
  zspkssb(299, et, system, ssb_craft);
  if (zfailed()) {
    zvmessage(" SPKSSB failed","");
    zreset1();
  }
  else {
    zprnt(8,6,ssb_craft,"after spkssb ...ssb_craft = ");
    /*  this requests the state of 299 relative to the observer at
     *  ssb_craft, which is also 299, so result must be 0 ... */
    zspkapp(299, et, system, ssb_craft,
          lighttime, craft_target_pos,&craft_target_lt);
    if (zfailed()) {
      zvmessage(" SPKAPP failed","");
      zreset1();
    }
    else {
      zprnt(8,3,craft_target_pos,"after spkapp ...target_pos = ");
      zprnt(8,1,&craft_target_lt,"after spkapp ...craft_target_lt = ");
    }
  }

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zvp("CK_PATH",CKfile,&count);
  zdafopr(CKfile,&handle);
  if (zfailed()) {
    zvmessage(" DAFOPR failed","");
    zreset1();
  }
  else zprnt(4,1,&handle,"after dafopr ...handle = ");

  zdafcls(&handle);
  zvmessage("after dafcls ...","");
  if (zfailed()) zreset1();

  zcklpf(CKfile,&handle);
  zvmessage("after cklpf ...","");
  if (zfailed()) zreset1();

  et_lt = et - craft_target_lt;
  zbodeul(299, et_lt, &val4,&val5,&val6,&val7);
  if (zfailed()) {
    zvmessage(" BODEUL failed","");
    zreset1();
  }
  else {
    zprnt(8,1,&val4,"after bodeul ...val4 = ");
    zprnt(8,1,&val5,"after bodeul ...val5 = ");
    zprnt(8,1,&val6,"after bodeul ...val6 = ");
    zprnt(8,1,&val7,"after bodeul ...val7 = ");
  }

  memset(c_matrix,0,8*9);
  memset(c_av,0,8*3);
  timout = 0.0E0;
  sclkdp = 1.383E+10;
  zckgp(-77001,sclkdp,3000.0E0,system,c_matrix,&timout,&found);
  if (zfailed()) {
    zvmessage(" CKGP failed","");
    zreset1();
  }
  else {
    zprnt(8,9,c_matrix,"after ckgp ...cmatrix = ");
    zprnt(8,1,&timout,"after ckgp ...timout = ");
    zprnt(4,1,&found,"after ckgp ...found = ");
  }

  memset(c_matrix,0,8*9);
  memset(c_av,0,8*3);
  timout = 0.0E0;
  sclkdp = 1.383E+10;
  zckgpav(-77001,sclkdp,3000.0E0,system,c_matrix,c_av,&timout,&found);
  if (zfailed()) {
    zvmessage(" CKGPAV failed","");
    zreset1();
  }
  else {
    zprnt(8,9,c_matrix,"after ckgpav ...cmatrix = ");
    zprnt(8,3,c_av,"after ckgpav ...c_av = ");
    zprnt(8,1,&timout,"after ckgpav ...timout = ");
    zprnt(4,1,&found,"after ckgpav ...found = ");
  }

  matrix[0][0]= 0.0E0;
  matrix[1][0]= 1.0E0;
  matrix[2][0]= 0.0E0;
  matrix[0][1]= -1.0E0;
  matrix[1][1]= 0.0E0;
  matrix[2][1]= 0.0E0;
  matrix[0][2]= 0.0E0;
  matrix[1][2]= 0.0E0;
  matrix[2][2]= 1.0E0;
  zm2eul(matrix,3,2,3,&c_twist,&c_dec,&c_ra);
  if (zfailed()) {
    zvmessage(" M2EUL failed","");
    zreset1();
  }
  else {
    zprnt(8,1,&c_twist,"after m2eul ...c_twist = ");
    zprnt(8,1,&c_dec,"after m2eul ...c_dec = ");
    zprnt(8,1,&c_ra,"after m2eul ...c_ra = ");
  }

  vin[0] = 1.0E0;
  vin[1] = -2.0E0;
  vin[2] = 3.0E0;
  zvminus(vin,target_craft);
  if (zfailed()) {
    zvmessage(" VMINUS failed","");
    zreset1();
  }
  else zprnt(8,3,target_craft,"after vminus ...target_craft = ");

  zvp("SCLK_PATH",SCLKfile,&count);
  zldpool(SCLKfile);
  
  zsct2e(-77,sclkdp,&etime);
  if (zfailed()) {
    zvmessage(" SCT2E failed","");
    zreset1();
  }
  else zprnt(8,1,&etime,"after sct2e...etime = -2.078E+08 =? ");

  strcpy(sclkc,"1651033.47.4.1");
  zvmessage(" SCLK (RIM.MOD91.MOD10.MOD8) = 1651033.47.4.0","");
  zscencd(-77,sclkc,&sclkdp);
  if (zfailed()) {
    zvmessage(" SCNCD failed","");
    zreset1();
  }
  else zprnt(8,1,&sclkdp,"after scencd, sclkdp = 12019524032.0 =? ");

  sclkdp = 0.0E0;
  zsce2t(-77,etime,&sclkdp);
  if (zfailed()) {
    zvmessage(" SCE2T failed","");
    zreset1();
  }
  else zprnt(8,1,&sclkdp,"after sce2t...sclkdp = 1.383E+10 =? ");

  val = zdpr();
  if (zfailed()) {
    zvmessage(" DPR failed","");
    zreset1();
  }
  else zprnt(8,1,&val,"degrees per radian is....");

  val = zrpd();
  if (zfailed()) {
    zvmessage(" RPD failed","");
    zreset1();
  }
  else zprnt(8,1,&val,"radians per degree is....");

  x_pi = zpi();
  if (zfailed()) {
    zvmessage(" PI failed","");
    zreset1();
  }
  else zprnt(8,1,&x_pi,"pi is....");

  half_pi = zhalfpi();
  if (zfailed()) {
    zvmessage(" HALFPI failed","");
    zreset1();
  }
  else zprnt(8,1,&half_pi,"halfpi is....");

  two_pi = ztwopi();
  if (zfailed()) {
    zvmessage(" TWOPI failed","");
    zreset1();
  }
  else zprnt(8,1,&two_pi,"twopi is....");

  val = half_pi/2;
  zrotate(val,3,mout);
  if (zfailed()) {
    zvmessage(" ROTATE failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after rotate  ...mout = ");

  memset(mout2,0,8*9);
  zrotmat(mout,half_pi,1,mout2);
  if (zfailed()) {
    zvmessage(" ROTMAT failed","");
    zreset1();
  }
  else zprnt(8,9,mout2,"after rotmat  ...mout2 = ");

  zmxv(matrix,vin,vout);
  if (zfailed()) {
    zvmessage(" MXV failed","");
    zreset1();
  }
  else zprnt(8,3,vout,"after mxv   ... vout = ");

  zmtxv(matrix,vin,vout);
  if (zfailed()) {
    zvmessage(" MTXV failed","");
    zreset1();
  }
  else zprnt(8,3,vout,"after mtxv   ... vout = ");

  zmxmt(matrix,matrix,mout);
  if (zfailed()) {
    zvmessage(" MXMT failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after mxmt  ...mout = ");

  matrix2[0][0]= 1.0E0;
  matrix2[1][0]= 0.0E0;
  matrix2[2][0]= 0.0E0;
  matrix2[0][1]= 0.0E0;
  matrix2[1][1]= 1.0E0;
  matrix2[2][1]= 1.0E0;
  matrix2[0][2]= 0.0E0;
  matrix2[1][2]= -1.0E0;
  matrix2[2][2]= 1.0E0;
  zmxm(matrix,matrix2,mout);
  if (zfailed()) {
    zvmessage(" MXM failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after mxm  ...mout = ");


  memset(mout,0,8*9);
  zirfrot(4,1,mout);
  if (zfailed()) {
    zvmessage(" IRFROT failed","");
    zreset1();
  }
  else zprnt(8,9,mout,"after irfrot  ...mout = ");


/* Thu Oct 31 13:24:50 PST 1996				*/
/* need to take out this test because the new version	*/
/* spice toolkit does not support dmpool anymore	*/
/*  zdmpool_1("dump.out");				*/
  zvmessage("No need to test for this anymore with ","");
  zvmessage("The new SPICE toolkit !!!!","");

  zvmessage("/*************end tsubs********************/","");
}
void zvwait()
{
}
void vwait()
{
}
$!-----------------------------------------------------------------------------
$ create tsubs.pdf
process 
PARM CK_PATH		STRING 	COUNT=1
PARM SCLK_PATH		STRING 	COUNT=1
PARM LONG_PATH		STRING 	COUNT=1
PARM CONST		STRING 	COUNT=1
PARM LEAPS		STRING 	COUNT=1
end-proc
$!-----------------------------------------------------------------------------
$ create tsubs.imake
#define PROGRAM   tsubs

#define MODULE_LIST tsubs.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_SPICE
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_NETWORK

#define DEBUG		/* for local testing */
#define LIB_LOCAL	/* for local testing */
$!-----------------------------------------------------------------------------
$ create tstspbri.pdf
!*****************************************************************************
! tstspbri.pdf - unit test for spbri
!
!   Testers: please read the unit test for information!
!
!   PORTED TO UNIX by T. Truong  March 21, 1994
!   This pdf assumes that if you are not on a unix host, then you are on
!   a  vax-vms host.
!*****************************************************************************

procedure help=*
refgbl $echo
body
let $echo = "no"
let _onfail="continue"
write "**************************************"
write "		NOTE TO TESTER:"
write "PRECISION DIFFERENCES MAY EXIST BETWEEN PLATFORMS"
write "**************************************"
Write  " "
Write  " The Following Test Data are handled separately for VMS and UNIX: "
write  "  /project/spice/ops/$VICCPU/pred_ev6.ssi_ck "
write  "  /project/spice/ops/$VICCPU/gll_long_2.bsp "
Write  " Currently under UNIX, in order to run the program, these files"
Write  " must  be copied to the LOCAL directory where the program resides."
write " "
write " NOTE: FOR TEST TSPBRI1, values are set"
write " only when they are valid for a particular"
write " map projection.  First and second standard"
write " parallels are not set for the sinusoidal"
write " projection but are set for Albers."
write " ALSO NOTE THAT TEST TSPBRI1 requires the GLL"
write " SPICELIB during its linking. TSPBRI1's imake"
write " file contains #define LIB_SPICE which links"
write " to this Galileo library automatically."
write " "
refgbl $syschar
local tls   type=string             !...leapseconds.ker
local tpc   type=string             !...gll00006.tpc
local tsc   type=string             !...gll00012.tsc_1
local ssi_ck   type=string          !...pred_ev6.ssi_ck
local bsp   type=string             !...gll_long_2.bsp
local ker   type=string             !...p_constants.ker

if ($syschar(1) = "UNIX")
  let tls = "/project/spice/ops/leapseconds.ker"
  let tpc = "/project/spice/ops/p_constants.ker"   
  let tsc = "/project/spice/ops/sclk.ker"
  let ssi_ck = "pred_ev6.ssi_ck"
  let bsp = "gll_long_2.bsp"
  let ker = "/project/spice/ops/p_constants.ker"
else ! VAX format
  let tls = "SPICEKER:leapseconds.ker"
  let tpc = "SPICEKER:p_constants.ker"   
  let tsc = "SPICEKER:sclk.ker"
  let ssi_ck = "SPICEKER:pred_ev6.ssi_ck"
  let bsp = "SPICEKER:gll_long_2.bsp"
  let ker = "SPICEKER:p_constants.ker"
end-if
let  $echo="yes"

!
! BEGIN testing
!
tsubs +
	CK_PATH=@ssi_ck     +
	SCLK_PATH=@tsc      +
	LONG_PATH=@bsp      +
        CONST=@tpc          +
        LEAPS=@tls      

tspbri1 +
	TARGET="EARTH" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=2.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=10.8	+
	SAMPLE_OFFSET=5.9	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@ker

tspbri1 +
	TARGET="MARS" 		+
	PROJ="ALBERS"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=2.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=10.8	+
	SAMPLE_OFFSET=5.9	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@ker

tspbri2 +
	LEAP_KERNEL=@tls	+
	SCLK_KERNEL=@tsc	+
	PC_KERNEL=@ker		+
      	ET_INPUTS=(	-527644192.5403653,     +
                        -527644192.5403653,     +
                        -527644192.5403653,     +
                        -527644192.5403653 )    +
        SCLK_INPUTS=(   "2 / 3110578:89:09",	+
			"2 / 3110578:89:09",    +
                        "2 / 3110578:89:09",	+
                        "2 / 3110578:89:09" )	+
        SPACECRAFT_CODE=( -77,-77,-77,-77 )     +
        FORMAT= ( "C","C","D","J" )             +
        PRECISION= ( 0,3,5,7 )

let  $echo="no"
end-proc
.title
tstspbri.pdf - unit test for spbri
.help

If you are running on unix, the following required  resources must be 
present in the working directory from which tstspbri.pdf is run:

pred_ev6.ssi_ck
gll_long_2.bsp

If you are on vax/vms, you have no prerequisites.
$ Return
$!#############################################################################
$Other_File:
$ create spbri.hlp
1 VICAR SUBROUTINE spbri

Purpose				

SPBRI.com provides a C programming language interface to 
a subset of functions from the FORTRAN-based NAIF SPICE 
toolkits: general, Galileo, and MIPS extended library.
We currently support the following routines with C bridges:

     o  GENERAL NAIF TOOLKIT ROUTINES

        BODEUL  DAFCLS  DPR     M2Q     SCENCD
        BODFND  DAFENA  ERRACT  MXM     SCE2T
        BODVAR  DAFFNA  ERRPRT  MXMT    SCS2E
	CKGP			MXV	SCT2E
				MTXV	SPKAPP
        CKGPAV  DAFGS   ET2UTC  PI	SPKLEF
        CKLPF   DAFHSF  FAILED  RECLAT  SPKSSB
        CLPOOL  DAFOPR  HALFPI          TWOPI
        DAFADA  DAFOPW  IRFROT  ROTATE  UTC2ET
        DAFBFS  DAFPS   LDPOOL  ROTMAT  VMINUS
        DAFBNA  DAFUS   M2EUL   RPD	RTPOOL
	XPOSE

     o  NAIF-MIPS TOOLKIT EXTENSIONS

	DMPOOL_1
	RSPOOL_1

     o  NAIF-GALILEO TOOLKIT EXTENSIONS

	BODN2C_G

2 OPERATION

Following some of the guidelines of the VICAR Porting
Guide, D-9395, functions in C and FORTRAN have been
written using the C STRLEN operator and the VICAR MVLC
function to handle the strings that are passed between
C and FORTRAN. 

All C bridges to the NAIF SPICE functions begin with
the letter 'z' followed by the SPICE function name.
Calling sequences vary as shown below. See NAIF SPICE
toolkit help for details on descriptions of arguments.

CALLING SEQUENCES

        zbodeul(int body,double et,double *ra,double *dec,
                double *w,double *lambda);

	int status =  zbodfnd( int body, char *item );

	zbodn2c_g( char *body, int *body_id, int *status );

	zbodvar( int body, char *item, int *dim, double *values );

        zckgpav(int inst,double sclkdp,double tol,char *ref,
                double cmat[3][3],double av[3],double *clkout,int *status);

	zcklpf( char *x, int *handle );

	zclpool();

	zdafbfs( int handle );

	zdafcls( int handle );

	zdaffna( int *status );

	zdafgs( double *sum );

	zdafhsf( int handle, int *nd, int *ni );

	zdafopr(char *fname, int *handle );

	zdafus( double *sum, int nd, int ni, double *dc, int *ic );

	zdmpool_1( char *dmpfil );

        double deg_rad = zdpr();

        double rad_deg = zrpd();

	zerract( char *x, char *y );	

	zerrprt(char *x, char *y );

        zet2utc( double ephemeris_time, char format, int precision,
                 char *UTC);

	int status = zfailed();

	double half_pi = zhalfpi();

	zirfrot( int refa, int refb, double rotab[3][3] );

	zldpool( char *x );

	zm2eul(double r[3][3], int axis3, int axis2, int axis1,
	       double *angle3, double *angle2, double *angle1 );

	zmxm( double m1[3][3], double m2[3][3], double mout[3][3] );

	zmxmt( double m1[3][3], double m2[3][3], double mout[3][3] );

	zmxt( double matrix[3][3], double vin[3], double vout[3] );

	zreclat( double rectan[3], double *radius, double *lon, double *lat );

	zrotate(double angle, int iaxis, double mout[3][3] );

	zrotmat(double m1[3][3], double angle, int iaxis,
	        double mout[3][3] );

	zrspool_1(char *dmpfil);

	zrtpool( char *variable, int *dim, double *values, int *status );

	zsct2e(int sc, double sclkdp, double *et );

	zsce2t(int sc, double et, double *sclkdp);

        zscs2e( int sc_code, char *SCLK, double *ephemeric_time )

	zspkapp(int targ, double et, char *ref, double sobs[6],
                char *abcorr, double starg[6], double *lt );

	zspklef( char *x, int *handle );

	zspkssb( int targ, double et, char *ref, double starg[6] );

	zutc2et( char *utc, double *et );   

	zvminus( double v1[3], double vout[3] );

*** The following bridges were added by Sam Le to support new spice
	routines. For more details on what these routines do, see
	their implementation file 'zspbri.c'

	zckbss(int, double, double, int)
	zcksns(int*, double*, char*, int*)
	zckpfs(int, double*, double, double, int, double, double, double, int)
	zckupf(int)
	zdafa2b(char*, char*, int)
	zdafb2a(char*, char*)
	zeul2m(double, double, double, int, int, int, double)
	zspca2b(char*, char*)
	zspcb2a(char*, char*)
	zsctiks(int, char*, double*)
	zspkuef(int)
	zvadd_mat(double*, double*, double*)	
	zsurfpt(double*, double*, double, double, double, double*, int*)
	zvnorm(double*)
	zvsub(double*, double*, double*)
	zsurfnm(double, double, double, double*, double*)
	zvsep(double*, double*)
	ztxtopr(char*, int*)
	zspct2b(int, char*)
	ztxtcls(int)
	zsce2s(int, double, char*)
	zdafgh(int*)
	zdafhfn(int*, char*)

Libraries and subroutines required to run this
routine: mp_routines, NAIF SPICELIB library

3 ENVIRONMENT and LANGUAGE

Software Platform:		VICAR 12.0 (VMS/UNIX)
Hardware Platforms:		No particular hardware required;
				tested on VAX 8650 and Sun Sparc.
Programming Language:		ANSI C, Fortran

3 HISTORY

Author:			Thomas Roatsch, DLR
			Justin McNeill, JPL
			Thuy Troung, 	JPL
Date of Original:	November 1993
Cognizant Engineer:	Justin McNeill, JPL
Traceability:		MSTP SRD D-10637 Section 3 <3>, Section 7 <3>

Revision history:
			June 24, 1994		(JFM)

			Print statement (ZPRNT) after DAFGS removed.
			SUMMARY array must be unpacked first by DAFUS.
                        Null terminator added to SCLK_time string in
			TSPBRI2.C to avoid failure on the DEC ALPHA. 
			(FR 85089)

			May 17, 1994		(JFM)

			Removed incorrect inclusion of _G source files
			in xspbri.f.  Removed BODTRN_G from source as it
			already resides in the NAIF GLL SPICELIB.
			(FR 82918, 82935)
	
			April 22, 1994		(JFM)

			Revised HELP file to categorize NAIF TOOLKIT
			routines into general, Galileo, and MIPS specific.
			Grouped source code files into general bridges
			(z/xspbri), Galileo (z/xspbri_gll_ext), and
			MIPS specific (z/xspbri_vicar_ext). (FR 82921) 

			March 30, 1994		(JFM)
			
			Added bridges for et2utc, rtpool, and scs2e.
			Traceability for this revision is the
			Mars 94 Post-telemetry Processing SRD 4.1.1, D-11125.
		
                   	March 21, 1994 		(TLT)
			
			For FR 83082, SPBRI was produced from the combination 
			of zspbri.com and xspbri.com and with the following 
			changes:

			1. added bridges for dpr,dmpool_1,halfpi,
                        rotate,rotmat,mxv,mxm,mxmt,sct2e,sce2t,
                        bodeul,m2eul,failed,irfrot,rspool_1,
			dafgs,dpr,dafus,dafbfs,dafhsf,daffna,
			errprt,spkssb,spkapp,ckgpav,dafopr,
			dafcls,reset,vminus,xpose,m2q,dafopw,
                        dafps,dafbna,dafada,dafena.

			2. Fixed incorrect function definition in
			C bridge for UTC2ET (FR 76818).

			20dec1994 (LWK) - added bridges for ckgp,
			pi, twopi, mtxv, scencd;  changed xrtpool
			to return -1 on success, consistent with
			Fortran ".TRUE.".

			
			JUN-19-1995	(SVL)
			Added: CKBSS   CKSNS   CKPFS   CKUPF
			       DAFA2B  DAFB2A  EUL2M   SCTIKS
			       SPCA2B  SPCB2A  SPKUEF  SURFPT
			       VADD    VNORM   VSEP    VSUB
			       TXTOPR  SPCT2B  TXTCLS  SCE2S
			these are NAIF toolkit bridges used by
			spice Cmatrix95, spice95, etc.

			MARCH-05-1996	(SVL)
			added prototypes for new bridges writen by
			Sam Le.

			06-MAR-1996 (LWK) - removed reset() because
			this routine is replaced by reset1() (in c89.com)
			due to name conflict; fixed problem with utc string
			in zet2utc().

			29 Mar 96 (GMY) Added rpd bridge (zrpd)

			OCT-30-1996	(SVL)
			Remove: zspbri_vicar_ext.c & xspbri_vicar_ext.f
				from spbri.repack & spbri.imake file because
				the new SPICE Toolkit no longer support
				binary pool. Therefore, DMPOOL_1 & ZRSPOOL_1
				is not in spicelib.a anymore.
				Also commented out test case for DMPOOL_1
				in tsubs.c
$ Return
$!#############################################################################
