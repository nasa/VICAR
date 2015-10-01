$!****************************************************************************
$!
$! Build proc for MIPL module fpse
$! VPACK Version 1.9, Monday, December 07, 2009, 16:18:45
$!
$! Execute by entering:		$ @fpse
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
$ write sys$output "*** module fpse ***"
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
$ write sys$output "Invalid argument given to fpse.com file -- ", primary
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
$   if F$SEARCH("fpse.imake") .nes. ""
$   then
$      vimake fpse
$      purge fpse.bld
$   else
$      if F$SEARCH("fpse.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fpse
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fpse.bld "STD"
$   else
$      @fpse.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fpse.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fpse.com -mixed -
	-s fpse.f zfpse.c -
	-i fpse.imake -
	-t tfpse.f tzfpse.c tfpse.imake tzfpse.imake tfpse.pdf tzfpse.pdf -
	   tstfpse.pdf -
	-o fpse.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fpse.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 2 January 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
C27 January 1995 ... SP      Changed the name VADD to VADDEM because of
C                            name conflict with SPICE.  

c dummy routines for FPS emulation package

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apinit
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apwd
      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apwr
      return
      end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine apget( buf, offs, n, type)

c FPS emulation routine.
c Move N elements from APMEM starting at specified offset OFFS to array BUF,
c reformatting as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
	implicit integer(a-y)

        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf(1)

        integer   F2HBUF(12)            ! Full to half translation
        integer   status

        call xvtrans_set (F2HBUF,'FULL','HALF',status)
        if (status .ne. 1) then
           call mabend ('APGET: Full to HALF translation error',' ')
        endif

	if (type .eq. 0) then               ! type 0: in I*4 /out = I*4
            call MVE ( 4, N, zbuf(offs+1), buf, 1, 1)
	ELSE if (type.eq.1) then          ! type 1: in I*4 /out = I*2
	    call XVTRANS (F2HBUF,zbuf(offs+1), buf, n)
	ELSE if (type.eq.2 .or. type .eq. 3) then ! type 2or3: in R*4 /out = R*4
	    call MVE ( 7, n, zbuf(offs+1), buf, 1, 1)
        ENDIF

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine apput( buf, offs, n, type)

c FPS emulation routine.
c Move N elements from array BUF to APMEM starting at specified offset OFFS,
c reformatting as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VUP8, VFLT32)
c	       1: in = I*2, out = I*4 ( then use VFLT)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c (This implies that only for TYPE=1 do we need to do any work)

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf(1)

	if (type.eq.0) then
            call mve ( 4,n,buf,zbuf(offs+1),1,1)
	else if (type.eq.1) then
 	    call mve( 6, n, buf, zbuf(offs+1),1,1)
	elseif (type.eq.2 .or. type .eq. 3) then
 	    call mve( 7, n, buf, zbuf(offs+1),1,1)
	endif

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine conv( a, i, b, j, c, k, n1, n2)

c FPS emulation routine.
c Correlate or convolve arrays A and B to obtain C:
c	C(mK) = SUM (A((m+q)I) * B(qJ),	from q = 1 to N2, for m = 1,...,N1
c Note that A, B, and C are offsets in APMEM.
c If I & J have the same sign, the operation is correlation, else it
c is convolution.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m = 0,n1-1
	  zsum = 0.
	  do q = 0, n2-1
	    zsum = zsum + zbuf(a+1+(m+q)*i) * zbuf(b+1+q*j)
	  enddo
	  zbuf(c+1+m*k) = zsum
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine hist( a, i, c, n, nb, amax, amin)

c FPS emulation routine.
c Generates the histogram of an array starting at AP menory offset A, 
c increment = I, with limits AMAX, AMIN, and puts the results in the 
c AP memory array at offset C.

c 12-sep-85 ...REA... made binwidth,diff real*4

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	real binwidth,diff

c (don't inititalize C because FPS routine doesn't)

    	diff = zbuf(amax+1)-zbuf(amin+1)
	if (diff.eq.0.) then
	  call xvmessage ('** HIST: BAD LIMITS **',' ')
	  return
	endif
	binwidth = nb/diff
	do m = 0,n-1
	  if (zbuf(a+1+m*i).lt.zbuf(amin+1)) then
	    j = 1
	  elseif (zbuf(a+1+m*i).ge.zbuf(amax+1)) then
	    j = nb
	  else
	    j = binwidth*(zbuf(a+1+m*i)-zbuf(amin+1)) + 1.0
	  endif
	  zbuf(c+j) = zbuf(c+j)+1.0
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine mmul32( a, i, b, j, c, k, mc, nc, na)

c FPS emulation routine.
c Matrix multiply arrays A and B to obtain C

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do p = 0,mc-1
	  do q = 0,nc-1
	    zsum = 0.
	    do r = 0, na-1
	      zsum = zsum + zbuf(a+1+(p+r*mc)*i) * zbuf(b+1+(r+q*na)*j) 
	    enddo
	    zbuf(c+1+(p+q*mc)*k) = zsum
	  enddo
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vaddem ( a, i, b, j, c, k, n)

c FPS emulation routine.
c Add arrays A and B to obtain C:
c	C(mK) = A(mI) + B(mJ),	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) + zbuf(b+1+(m-1)*j)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vclip (a, i, b, c, d, k, n)

c FPS emulation routine.
c Move array A to D, clipping it to the range (B - C):
c	D(mK) = B	if A(mI) < B		m = 0,...,N-1
c		A(mI) 	if B <= A(mI) < C
c		C	if C <= A(mI)
c Note that A, B, C, and D are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	zlo = zbuf(b+1)
	zhi = zbuf(c+1)
	do m =1,n
	  z1 = zbuf(a+1+(m-1)*i) 
	  if (z1.lt.zlo) then
	    z1 = zlo
	  elseif (z1.gt.zhi) then
	    z1 = zhi
	  endif
	  zbuf(d+1+(m-1)*k) = z1
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vclr (c, k, n)

c FPS emulation routine.
c  Clears an array starting at C, increment = K.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m = 0,n-1
	  zbuf(c+1+m*k) = 0.
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vfix (a, i, c, k, n)

c FPS emulation routine.
c Convert elements from Floating-point to Integer:
c	C(mK) = FIX( A(mI)),	m = 0,...,N-1
c Note that C and A are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	integer*4 buf
        integer   R2FBUF(12)            ! Real to Full translation

	entry vfix32( a, i, c, k, n)

        call xvtrans_set (R2FBUF,'REAL','FULL',STATUS)
        if (status .ne. 1) then
           call mabend ('VFIX: Real to Full translation error',' ')
        endif

	do m =1,n
	  call XVTRANS (R2FBUF,zbuf(a+1+(m-1)*i), buf, 1)
	  call mve(1,4, buf, zbuf(c+1+(m-1)*k), 1,1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vflt( a, i, c, k, n)

c FPS emulation routine.
c Convert elements from Integer to Floating-point:
c	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
c Note that C and A are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

        integer   F2RBUF(12)            ! Full to Real translation

	entry vflt32( a, i, c, k, n)

        call xvtrans_set (F2RBUF,'FULL','REAL',STATUS)
        if (status .ne. 1) then
           call mabend ('VFLT: Full to Real translation error',' ')
        endif

        do m = 1,n
	   call XVTRANS (F2RBUF,zbuf(a+1+(m-1)*i),zbuf(c+1+(m-1)*k),1)
        end do

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vmov( a, i, c, k, n)

c FPS emulation routine.
c Move array A to C:
c	C(mK) = A(mI) 
c Note that A and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) 
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vpk8( a, i, c, k, n)

c FPS emulation routine.
c  Packs lo-order byte (unsigned) from 4 words of A into each word of C.

	implicit integer(a-y)
        include 'fortport'

        real zbuf (1000000)
	common/apmem/ zbuf	! mock AP memory
	byte      bbuf(4)
        integer   IBUF
        integer   R2BBUF(12)           
        integer   R2FBUF(12)           
        integer   F2BBUF(12)           

        call xvtrans_set (R2BBUF,'REAL','BYTE',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Real to Byte translation error',' ')
        endif

        call xvtrans_set (R2FBUF,'REAL','FULL',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Real to Full translation error',' ')
        endif

        call xvtrans_set (F2BBUF,'FULL','BYTE',STATUS)
        if (status .ne. 1) then
           call mabend ('VPK8: Full to Byte translation error',' ')
        endif

	do m = 0,n-1
	  do j=0,3

            ! Move one word of zbuf 
	    call XVTRANS (R2BBUF,zbuf(a+1+(4*m+j)*i), bbuf(j+1), 1)

	    call XVTRANS (R2FBUF,zbuf(a+1+(4*m+j)*i), IBUF, 1)

	    call XVTRANS (F2BBUF,IBUF,BBUF(J+1), 1)

	  enddo
	  call mve( 1,4,bbuf, zbuf(c+1+m*k), 1,1)
	enddo
	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vsadd( a, i, b, c, k, n)

c FPS emulation routine.
c Add array A and scalar B to obtain C:
c	C(mK) = A(mI) + B,	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.

	implicit integer(a-y)
        real zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) + zbuf(b+1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vsmul( a, i, b, c, k, n)

c FPS emulation routine.
c Multiply array A and scalar B to obtain C:
c	C(mK) = A(mI) * B,	m = 0,...,N-1
c Note that A, B, and C are offsets in APMEM.
       
	implicit integer(a-y)
        real zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

	do m =1,n
	  zbuf(c+1+(m-1)*k) = zbuf(a+1+(m-1)*i) * zbuf(b+1)
	enddo

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
	subroutine vup8( a, i, c, k, n)

c FPS emulation routine.
c  Unpacks 4 bytes (unsigned) from each byte of single word A into 4 words of C.

	implicit integer(a-y)
        include 'fortport'
        real      zbuf (1000000)
        common/apmem/ zbuf	        ! mock AP memory

        ! Call C function to perform unpack; Located in C bridge package
        call zfvup8( zbuf, a, i, c, k, n)

	return
	end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zfpse.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h" 
/* #include "vicmain_c" */
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for FPSE - FPS emulation package
/************************************************************************/

void zapinit (void)
{

   FTN_NAME2(apinit, APINIT) ( );
   return;
}


/*
calling statement:  void zapwd ();
Where:

*/

void zapwd (void)
{

   FTN_NAME2(apwd, APWD) ( );

}

/*
calling statement:  void zapwr ();
Where:

*/

void zapwr (void) 
{

   FTN_NAME2(apwr, APWR) ( );
   return;
}

/*
Move N elements from APMEM starting at specified offset OFFS to array BUF,
reformatting as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
	       1: in = I*4, out = I*2 ( then use VFIX)
	       2: in/out = R*4
	       3: in/out = R*4 (ignore "IBM format")

calling statement:  void zapget (&buf, offs, n, type);
Where:
	buf   : void pointer to local buffer to receive data
		move to AP memory
        offs  : offset into AP memory
        n     : number of elements to move
        type  : type of data transfer:
		0: in/out I*4
		1: in = I*4, out = I*2
		2: in/out = R*4
		3: in/out = R*4 

*/

void zapget (buf, offs, n, type) 
void *buf;
int  offs, n, type;
{

   FTN_NAME2(apget, APGET) (buf, &offs, &n, &type);
   return;

}


/*
Move N elements from array BUF to APMEM starting at specified offset OFFS,
reformatting as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VUP8, VFLT32)
	       1: in = I*2, out = I*4 ( then use VFLT)
	       2: in/out = R*4
	       3: in/out = R*4 (ignore "IBM format")

calling statement:  void zapput (&buf, offs, n, type);
Where:
	buf   : void pointer to local buffer containing data to be
		moved to AP memory
        offs  : offset into AP memory
        n     : number of elements to move
        type  : type of data transfer:
		0: in/out I*4
		1: in = I*4, out = I*2
		2: in/out = R*4
		3: in/out = R*4 

*/
void zapput (buf, offs, n, type) 
void *buf;
int  offs, n, type;
{

   FTN_NAME2(apput, APPUT) (buf, &offs, &n, &type);
   return;

}


/*
Correlate or convolve arrays A and B to obtain C:
	C(mK) = SUM (A((m+q)I) * B(qJ),	from q = 1 to N2, for m = 1,...,N1
Note that A, B, and C are offsets in APMEM.
If I & J have the same sign, thje operation is correlation, else it
is convolution.

calling statement:  void zconv (a, i, b, j, c, k, n1, n2);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n1    : 
        n2    : 

*/
void zconv (a, i, b, j, c, k, n1, n2) 
int  a,b,c;
int  i,j,k,n1,n2;
{

   FTN_NAME2(conv, CONV) (&a, &i, &b, &j, &c, &k, &n1, &n2);
   return;

}


/*
Generates the histogram of an array starting at AP menory offset A, 
increment = I, with limits AMAX, AMIN, and puts the results in the 
AP memory array at offset C.

calling statement:  void zhist (a, i, c, n, nb, amax, amin);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	c     : Offset into AP memory for array 'c'
        n     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n1    : 
        n2    : 

*/
void zhist (a, i, b, j, c, k, n1, n2) 
int  a,b,c;
int  i,j,k,n1,n2;
{

   FTN_NAME2(hist, HIST) (&a, &i, &b, &j, &c, &k, &n1, &n2);
   return;

}



/*
Matrix multiply arrays A and B to obtain C

calling statement:  void zmmul32 (a, i, b, j, c, k, mc, nc, na);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'c'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        mc    : 
        nc    : 
        na    : 

*/
void zmmul32 (a, i, b, j, c, k, mc, nc, na)
int  a,b,c;
int  i,j,k,mc,nc,na;
{

   FTN_NAME2(mmul32, MMUL32) (&a, &i, &b, &j, &c, &k, &mc, &nc, &na);
   return;

}




/*
Add arrays A and B to obtain C:
	C(mK) = A(mI) + B(mJ),	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.


calling statement:  void zvaddem (a, i, b, j, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
        j     : Increment j from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'a'
        n     : number of elements in each array.
*/
void zvaddem (a, i, b, j, c, k, n) 
int  a,b,c;
int  i,j,k,n;
{

   FTN_NAME2(vaddem, VADDEM) (&a, &i, &b, &j, &c, &k, &n);
   return;

}

/*
Move array A to D, clipping it to the range (B - C):
	D(mK) = B	if A(mI) < B		m = 0,...,N-1
		A(mI) 	if B <= A(mI) < C
		C	if C <= A(mI)
Note that A, B, C, and D are offsets in APMEM.

calling statement:  void zvclip( a, i, b, c, d, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'b'
	c     : Offset into AP memory for array 'c'
	d     : Offset into AP memory for array 'd'
        k     : Increment k from 'd'
        n     : 
*/
void zvclip( a, i, b, c, d, k, n)
int  a,b,c,d;
int  i,k,n;
{

   FTN_NAME2(vclip, VCLIP) ( &a, &i, &b, &c, &d, &k, &n);
   return;

}

/*
Clears an array starting at C, increment = K.


calling statement:  void zvclr( c, k, n);
Where:
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'd'
        n     : Number of elements
*/
void zvclr( c, k, n)
int  c;
int  k,n;
{

   FTN_NAME2(vclr, VCLR) ( &c, &k, &n);
   return;

}


/*
Convert elements from Floating-point to Integer:
	C(mK) = FIX( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvfix( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvfix( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vfix, VFIX) ( &a, &i, &c, &k, &n);
   return;

}

/*
Convert elements from Floating-point to Integer:
	C(mK) = FIX( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvfix32( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvfix32( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vfix32, VFIX32) ( &a, &i, &c, &k, &n);
   return;

}

/*
Convert elements from Integer to Floating-point
	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvflt ( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvflt( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vflt, VFLT) ( &a, &i, &c, &k, &n);
   return;

}


/*
Convert elements from Integer to Floating-point
	C(mK) = FLOAT( A(mI)),	m = 0,...,N-1
Note that C and A are offsets in APMEM.

calling statement:  void zvflt32 ( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvflt32( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vflt32, VFLT32) ( &a, &i, &c, &k, &n);
   return;

}




/*
Move array A to C:
	C(mK) = A(mI) 
Note that A and C are offsets in APMEM.

calling statement:  void zvmov( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvmov( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vmov, VMOV) ( &a, &i, &c, &k, &n);
   return;

}


/*
Packs lo-order byte (unsigned) from 4 words of A into one word of C,
for n elements.
Note that A and C are offsets in APMEM.

calling statement:  void zvpk8( a, i, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment k from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvpk8( a, i, c, k, n)
int  a,c;
int  i,k,n;
{

   FTN_NAME2(vpk8, VPK8) ( &a, &i, &c, &k, &n);
   return;

}


/*
Add array A and scalar B to obtain C:
	C(mK) = A(mI) + B,	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.

calling statement:  void zvsadd( a, i, b, c, k, n);
Where:
	a     : Offset into AP memory for array A
        i     : Increment i from 'a'
	b     : Offset address for scaler B
	c     : Offset into AP memory for array C
        k     : Increment k from 'c'
        n     : Number of elements
*/

void zvsadd( a, i, b, c, k, n)
int  a,b,c;
int  i,k,n;
{

   FTN_NAME2(vsadd, VSADD) ( &a, &i, &b, &c, &k, &n);
   return;

}

/*
Multiply array A and scalar B to obtain C:
	C(mK) = A(mI) * B,	m = 0,...,N-1
Note that A, B, and C are offsets in APMEM.

calling statement:  void zvsmul( a, i, b, c, k, n);
Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	b     : Offset into AP memory for array 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements
*/
void zvsmul( a, i, b, c, k, n)
int  a,b,c;
int  i,k,n;
{

   FTN_NAME2(vsmul, VSMUL) ( &a, &i, &b, &c, &k, &n);
   return;

}


/* Unpack 4 bytes (unsigned) from each word of A into 4 words of C.

   calling statement:  void zvup8( a, i, c, k, n);
   Where:
	a     : Offset into AP memory for array 'a'
        i     : Increment i from 'a'
	c     : Offset into AP memory for array 'c'
        k     : Increment k from 'c'
        n     : Number of elements

   Call fortran function zup8 to get address of AP memory block. ZUP8
   then calls C function zfvup8 below to perform remainder of unpack function.
*/
                   


void zvup8( a, i, c, k, n)
int  *a,*c;
int  *i,*k,*n;
{

   FTN_NAME2(vup8, VUP8) ( &a, &i, &c, &k, &n);
   return;

}


void FTN_NAME2 (zfvup8, ZFVUP8)( zbuf, a, i, c, k, n)
int *zbuf;
int *a,*c;
int *i,*k,*n;
{
int j, m, ibuf, II, ibuf2;
int A,I,C,K,N;
float *fptr;


   I = *i;
   A = *a;
   C = *c;
   K = *k;
   N = *n;

   for (m = 0; m < N; m++) {
      II = A + m * I;
      ibuf = *(zbuf + II);
      for (j = 0; j < 4; j++) {
         ibuf2 = (ibuf & 0x00ff);
         II = C + ((4 * m + j) * K);
         fptr= (float *) (zbuf + II);
         *fptr = (float)ibuf2;
         ibuf = (ibuf >> 8);
      }
   }
   return;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fpse.imake
/* Imake file for VICAR subroutine fpse */

#define SUBROUTINE fpse

#define MODULE_LIST fpse.f zfpse.c

#define P2_SUBLIB

#define USES_ANSI_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tfpse.f
C 2 January 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting)
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      integer F2RBUF(12)            ! Full to Real translation buffer 
      integer OFFS,N,TYPE

      integer   i2offs,i4offs, R4OFFS1, R4OFFS2, workoffs1, workoffs2

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      data    I2BUF /100*0/,I4BUF /100*0/, OFFS/0/, N/0/, TYPE/0/
      data    R4BUF1 /100*0.0/, R4BUF2 /100*0.0/
      
!     Initialize offsets into AP memory array      
      i2offs = 1000               ! I*2 Working area
      i4offs = 2000               ! I*4 Working area
      R4OFFS1 = 3000              ! R*4 Working area #1
      R4OFFS2 = 4000              ! R*4 Working area #2
      workoffs1 = 5000            ! Out put work offset
      workoffs2 = 6000            ! Out put work offset


      call xvtrans_set (F2RBUF,'FULL','REAL',ISTAT)
      if (ISTAT .ne. 1) then
         call mabend ('TFPSE: Full to Real translation error',' ')
      endif

      call IFMESSAGE ('TFPSE version 2-Jan-95')

! Call FPSE subroutine APINIT
      CALL XVMESSAGE ('Call APINIT',' ')
      CALL  APINIT()

! Call FPSE subroutine APWD
      CALL XVMESSAGE ('Call  APWD',' ')
      CALL  APWD()


! Call FPSE subroutine APWR
      CALL XVMESSAGE ('Call  APWR',' ')
      CALL  APWR()

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Begin test of FPSE

!    
!     Clear segmentS of memory to be used for test
      CALL XVMESSAGE ('VCLR  - Clear segments of AP memory',' ')
      CALL VCLR (I2OFFS,1,100)    !! Clear each word of INTEGER*2 AREA
      CALL VCLR (I4OFFS,1,100)    !! Clear each word of INTEGER*4 AREA
      CALL VCLR (R4OFFS1,1,100)   !! Clear each word of REAL*4 AREA
      CALL VCLR (R4OFFS2,1,100)   !! Clear each word of REAL*4 AREA
      CALL VCLR (WORKOFFS1,1,100) !! Clear each word of WORK AREA #1
      CALL VCLR (WORKOFFS2,1,100) !! Clear each word of WORK AREA #2

! Get and display cleared AP memory segments of
      CALL XVMESSAGE 
     &('APGET - Get and display cleared AP memory segments',' ')

! Get 100 entries from AP memory in I*2 format into I2BUF 
      TYPE = 1                     !! Return I*2 format
      CALL APGET (I2BUF, I2OFFS, 100, TYPE)
      CALL DISPLAY_I2BUF()

! Get 100 entries from AP memory in I*4 format into I4BUF 
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, I4OFFS, 100, TYPE)
      call display_i4buf()

! Get 100 entries from AP memory in R*4 format into R4BUF1 
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, R4OFFS1, 100, TYPE)
      call display_R4BUF1()

! initialize SEGMENTS of local memory
      DO I = 1,100
        I2BUF(I) = I
        I4BUF(I) = I
        CALL XVTRANS (F2RBUF,I,R4BUF1(I),1)
        R4BUF1(I) = R4BUF1(I) * 0.1
      END DO

! Put local memory to AP memory
c Reformatting will be performed by FPSE as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
      CALL XVMESSAGE 
     & ('APPUT - Put initialized memory segments into AP memory',' ')
      CALL APPUT (I2BUF, I2OFFS,100,1)
      CALL APPUT (I4BUF, I4OFFS,100,0)
      CALL APPUT (R4BUF1,R4OFFS1,100,2)

! GET segments of AP memory to check for initialized values

! But first clear local memory segments to zero
      DO I = 1,100
        I2BUF(I) = 0
        I4BUF(I) = 0
        R4BUF1(I) = 0.0
      END DO

! Get 100 entries from AP memory in I*2 format into I2BUF
      CALL XVMESSAGE 
     &('APGET - Get and display initialized AP memory segments',' ')

      TYPE = 1                     !! Return I*2 format
      CALL APGET (I2BUF, I2OFFS, 100, TYPE)
      call display_i2buf()

! Get 100 entries from AP memory in I*4 format into I4BUF 
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, I4OFFS, 100, TYPE)
      call display_I4buf()

! Get 100 entries from AP memory in R*4 format into R4BUF1 
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, R4OFFS1, 100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Correlate or convolute arrays A and B to obtain C
!
! initialize SEGMENTS of local memory
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.25 + 50.0
        R4BUF2(I) = 50.0 - RTEMP * 0.25
      END DO

! Put local memory to AP memory
c Reformatting will be performed by FPSE as specified by TYPE:
c	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
c	       1: in = I*4, out = I*2 ( then use VFIX)
c	       2: in/out = R*4
c	       3: in/out = R*4 (ignore "IBM format")
c
      CALL XVMESSAGE 
     & ('APPUT - Put initialized memory segments into AP memory',' ')
      CALL APPUT (R4BUF1,R4OFFS1,100,2)
      CALL APPUT (R4BUF2,R4OFFS2,100,2)

      call xvmessage('CONV  - Correlate arrays',' ')
      CALL CONV (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,90,10)

! Get100 entries from AP memory in R*4 format into R4BUF1 
      call xvmessage('APGET - Display Correlated array',' ')
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1, 100, TYPE)
      call display_R4BUF1()

! Clear AP memory working segment 
      CALL VCLR (WORKOFFS1,1,100)    !! Clear each word of WORK AREA

! Convulate arrays
      call xvmessage('CONV  - Convolute arrays',' ')
      CALL CONV (R4OFFS1,1,R4OFFS2,-1,WORKOFFS1,1,90,10)

! Get100 entries from AP memory in R*4 format into R4BUF1 
      call xvmessage('APGET - Display convoluted array',' ')
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1, 100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Generate histogram of an array starting at A, increment I
! with limits AMAX, AMIN, and stored in array C

      call xvmessage('HIST  - Create histogram',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 2.5 + 100.0
        R4BUF2(I) = RTEMP * 2.5 + 100.0
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Create histogram
      CALL HIST (R4OFFS1,1,WORKOFFS1,100,70,R4OFFS2+70,R4OFFS2+20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display histogram array',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! MMUL32 - Matrix multiply arrays A and B to obtain C
! 
      call xvmessage('MMUL32- Matrix multiply arrays A and B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.1 + 100.0
        R4BUF2(I) = 100.0 - RTEMP * 0.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Perform Matrix Multioplication
      CALL MMUL32 (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,100,10,10)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display MMUL32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Add arrays A and B to obtain C
! 

      call xvmessage('VADDEM  - Add arrays A and B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = (100.0 + RTEMP * 0.1) * 100.0
        R4BUF2(I) = 100.0 + RTEMP * 0.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Add arrays A and B to obtain C
      CALL VADDEM (R4OFFS1,1,R4OFFS2,1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VADDEM results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VCLIP - Move Array A to D, clipping it to range (B to C)
! 
      call xvmessage('VCLIP - Move array A to D & clip',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area
      CALL VCLR (R4OFFS2  ,1,10)     !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RTEMP = REAL(I)
        R4BUF1(I) = RTEMP * 0.25 + 100.0
      END DO
      R4BUF2(1) =  105.5
      R4BUF2(2) =  120.3

! Put segments into AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,2,2)

! Move array and clip
      CALL VCLIP (R4OFFS1,1,R4OFFS2,R4OFFS2+1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VCLIP results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFIX - Convert Elements from floating-point to Integer
! 

      call xvmessage
     & ('VFIX  - Convert Elements from floating-point to Integer',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known real values
      DO I = 1,100
        R4BUF1(I) = real(I) * 1.25
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Convert float to integer
      CALL VFIX (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory into I4BUF
      call xvmessage('APGET - Get and display VFIX results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFIX32 - Convert elements from floating point to integer
! 

      call xvmessage
     & ('VFIX32  - Convert from floating point to integer',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known real values
      DO I = 1,100
        R4BUF1(I) = real(I) * 1.25
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Convert float to integer
      CALL VFIX32 (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory into I4BUF
      call xvmessage('APGET - Get and display VFIX32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF()

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFLT - Convert elements from integer to floating point
! 

      call xvmessage
     & ('VFLT  - Convert elements from integer to floating pointr',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        I4BUF(I) = I * 100 
      END DO

! Put segments in AP memory
      CALL APPUT (I4BUF, I4OFFS,100,0)

! Perform Integer to Floating point conversion
      CALL VFLT (I4OFFS,1,WORKOFFS1+20,1,20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VFLT results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VFLT32 - Convert elements from integer to floating point
! 

      call xvmessage
     & ('VFLT32  - Convert from integer to floating point',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        I4BUF(I) = I * 100
      END DO

! Put segments into AP memory
      CALL APPUT (I4BUF, I4OFFS,100,0)

! Perform Integer to Floating point conversion
      CALL VFLT32 (I4OFFS,1,WORKOFFS1+20,1,20)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VFLT32 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VMOV - Move array A to Array C
! 

      call xvmessage
     & ('VMOV  - Move array A to C',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        R4BUF1(I) = real(I) * 123.5
      END DO

! Put real segments into AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)

! Move data
      CALL VMOV (R4OFFS1,1,WORKOFFS1,1,100)

! Get100 entries from AP memory (in R*4 format) into R4BUF1 
      call xvmessage('APGET - Get and display VMOV results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return I*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VPK8 - Packs LSB from 4 words of A into a single word C.
! 

      call xvmessage
     & ('VPK8  - Pack low bytes of 4 words of A into 1 word of C',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        R4BUF1(I) = 16909060.0   !! '01020304'X
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,0)

! Pack data
      CALL VPK8 (R4OFFS1,1,WORKOFFS1,1,100)

! Get packed entries from AP memory
      call xvmessage('APGET - Get and display VPK8 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 0                     !! Return I*4 format
      CALL APGET (I4BUF, WORKOFFS1,100, TYPE)
      call display_I4BUF5()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VSADD - Add array A to Scaler B to obtain C
! 
      call xvmessage
     & ('VSADD - Add array A to Scaler B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
        RealTemp = real(i)
        R4BUF1(I) = RealTemp * 1.1 + 100.0
        R4BUF2(I) = 100.0 - RealTemp * 1.1
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1, R4OFFS1,100,2)
      CALL APPUT (R4BUF2, R4OFFS2,100,2)

! Move data
      CALL VSADD (R4OFFS1,1,R4OFFS2+50,WORKOFFS1,1,100)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VSADD results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VSMUL - Multiply array A and Scaler B to obtain C
! 
      call xvmessage
     & ('VSMUL - Multiply array A by Scaler B',' ')
! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area
      DO I = 1,100
         R4BUF2(I) = 0.0
      END DO
      CALL APPUT (R4BUF2,R4OFFS1-50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS1+50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS2-50, 100,2)
      CALL APPUT (R4BUF2,R4OFFS2+50, 100,2)
      CALL APPUT (R4BUF2,WORKOFFS1-50, 100,2)
      CALL APPUT (R4BUF2,WORKOFFS2+50, 100,2)

! initialize SEGMENTS of local memory to known values
      DO I = 1,100
         RealTemp = real(I+100) * 1.1       
         R4BUF1(I) = RealTemp
      END DO

! Put segments in AP memory
      CALL APPUT (R4BUF1,R4OFFS1, 100,2)
      CALL APPUT (50.2,  R4OFFS2, 100,2)

! Multiply data 
      CALL VSMUL (R4OFFS1,1,R4OFFS2,WORKOFFS1,1,100)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VSMUL results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return R*4 format
      CALL APGET (R4BUF2, WORKOFFS1,100, TYPE)
      call display_R4BUF2()

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! VUP8 - Unpacks 4 bytes, from four bytes of word A, into 4 words of C
! 
      call xvmessage
     & ('VUP8 - Unpack four bytes of A into four words of C',' ')

! Clear AP memory working area
      CALL VCLR (WORKOFFS1,1,100)    !! Clear work area

! initialize SEGMENTS of local memory to known values
      
      DO I = 1,100
         I4BUF(I) = '01020304'X     !! Assign with => (01020304) hex 
      END DO

! Put segments in AP memory
      CALL APPUT (I4BUF,I4OFFS,100,0)

! Unpack data
      CALL VUP8 (I4OFFS,1,WORKOFFS1,1,25)

! Get 100 entries from AP memory
      call xvmessage('APGET - Get and display VUP8 results',' ')

! Call APGET to retrieve working array from AP memory
      TYPE = 2                     !! Return I*4 format
      CALL APGET (R4BUF1, WORKOFFS1,100, TYPE)
      call display_R4BUF1()

! Convert floating point to fixed point format
      call VFIX (WORKOFFS1,1,WORKOFFS2,1,100)
      call APGET (I4BUF,WORKOFFS2,100,0)
      CALL DISPLAY_I4BUF()

      return
      end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_I2BUF()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('I2BUF',' ')
      DO J = 1,100,10
      WRITE (STRING, 90010) 
     &  I2BUF(J+00), I2BUF(J+01), I2BUF(J+02), I2BUF(J+03), I2BUF(J+04),
     &  I2BUF(J+05), I2BUF(J+06), I2BUF(J+07), I2BUF(J+08), I2BUF(J+09)
90010 FORMAT (10I8)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      
      SUBROUTINE DISPLAY_I4BUF()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer I, J

      CALL XVMESSAGE ('I4BUF',' ')
      DO I = 1,100,10
      J = I
      WRITE (STRING, 90010) 
     &  I4BUF(J+00), I4BUF(J+01), I4BUF(J+02), I4BUF(J+03), I4BUF(J+04),
     &  I4BUF(J+05), I4BUF(J+06), I4BUF(J+07), I4BUF(J+08), I4BUF(J+09)
90010 FORMAT (10I8)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END
                           
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      
      SUBROUTINE DISPLAY_I4BUF5()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('I4BUF',' ')
      DO J = 1,25,5
      WRITE (STRING, 90010) 
     &  I4BUF(J+00), I4BUF(J+01), I4BUF(J+02), I4BUF(J+03), I4BUF(J+04)
90010 FORMAT (5I16)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END
                           
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_R4BUF1()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('R4BUF1',' ')
      DO J = 1,100,5
      WRITE (STRING, 90010) 
     &  R4BUF1(J+00), R4BUF1(J+01), R4BUF1(J+02), R4BUF1(J+03), 
     &  R4BUF1(J+04)
90010 FORMAT (5F16.4)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE DISPLAY_R4BUF2()

      integer*2 I2BUF (100)
      integer   I4BUF (100)
      real      R4BUF1 (100), R4BUF2 (100)
      common/buffers/I2BUF,I4BUF,R4BUF1,R4BUF2

      character*132 string
      integer J

      CALL XVMESSAGE ('R4BUF2',' ')
      DO J = 1,100,5       !corrected step to 5; was skipping parts of R4BUF2
      WRITE (STRING, 90010) 
     &  R4BUF2(J+00), R4BUF2(J+01), R4BUF2(J+02), R4BUF2(J+03), 
     &  R4BUF2(J+04)
90010 FORMAT (5F16.4)
      CALL XVMESSAGE (STRING,' ')
      END DO

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzfpse.c
#include "vicmain_c"
#include "ftnbridge.h"
#include <string.h>
/************************************************************************/
/*                                                               	*/
/************************************************************************/


char  string[132];

int   f2rbuf[12];            /* Full to Real translation buffer  */
int   i2offs,i4offs, r4offs1, r4offs2, workoffs1, workoffs2;

short i2buf [100];
int   i4buf [100];
float rtemp, r4buf1[100], r4buf2 [100];
void  zdisplay_i2buf(void);
void  zdisplay_i4buf (void);
void  zdisplay_i4buf5 (void);
void  zdisplay_r4buf1 (void);
void  zdisplay_r4buf2 (void);


void main44()
{
int i, type;
    memset (i2buf,0,sizeof(i2buf));
    memset (i4buf,0,sizeof(i4buf));
    memset (i2buf,0,sizeof(i2buf));
    memset (r4buf1,0,sizeof(r4buf1));
    memset (r4buf2,0,sizeof(r4buf2));
      
    /* Initialize offsets into AP memory array */
    i2offs = 1000;              /* I*2 Working area */
    i4offs = 2000;              /* I*4 Working area */
    r4offs1 = 3000;             /* R*4 Working area #1 */
    r4offs2 = 4000;             /* R*4 Working area #2 */
    workoffs1 = 5000;           /* Out put work offset #1 */
    workoffs2 = 6000;           /* Out put work offset #2 */

    zifmessage ("TZFPSE version 2-Jan-95");

    /* Call FPSE subroutine APINIT */
    zvmessage ("Call APINIT","");
    zapinit ();

    /* Call FPSE subroutine APWD */
    zvmessage ("Call APWD","");
    zapwd ();

    /* Call FPSE subroutine APWR */
    zvmessage ("Call APWR",""); 
    zapwr ();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Clear segmentS of memory to be used for test */
   zvmessage ("VCLR  - Clear segments of AP memory","");
   zvclr (i2offs,1,100);        /* Clear each word of INTEGER*2 AREA */
   zvclr (i4offs,1,100);        /* Clear each word of INTEGER*4 AREA */
   zvclr (r4offs1,1,100);       /* Clear each word of REAL*4 AREA */
   zvclr (r4offs2,1,100);       /* Clear each word of REAL*4 AREA */
   zvclr (workoffs1,1,100);     /* Clear each word of WORK AREA */
   zvclr (workoffs2,1,100);     /* Clear each word of WORK AREA */


   /* Get and display cleared AP memory segments */
   zvmessage ("APGET - Get and display cleared AP memory segments","");
     

   /* Get 100 entries from AP memory in I*2 format into I2BUF  */
   type = 1;                         /* Return I*2 format */
   zapget (i2buf, i2offs, 100, type);
   zdisplay_i2buf();

   /* Get 100 entries from AP memory in I*4 format into I4BUF  */
   type = 0;                       /* Return I*4 format */
   zapget (i4buf, i4offs, 100, type);
   zdisplay_i4buf();

   /* Get 100 entries from AP memory in R*4 format into R4BUF1 */
   type = 2;                       /* Return R*4 format */
   zapget (r4buf1, r4offs1, 100, type);
   zdisplay_r4buf1();                   

   /* Initialize SEGMENTS of local memory */
   for (i = 0; i < 100; i++) {
      i2buf[i] = i+1;  
      i4buf[i] = i+1;  
      r4buf1[i] = (float)(i+1) * 0.1;
   }

   /* Put local memory to AP memory
      Reformatting will be performed by FPSE as specified by TYPE:
 	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
 	       1: in = I*4, out = I*2 ( then use VFIX)
 	       2: in/out = R*4
 	       3: in/out = R*4 (ignore "IBM format")
   */

   zvmessage ("APPUT - Put initialized memory segments into AP memory","");
   zapput (i2buf, i2offs,100,1);
   zapput (i4buf, i4offs,100,0);
   zapput (r4buf1,r4offs1,100,2);

   /* GET segments of AP memory to check for initialized values */

   /* But first clear local memory segments to zero */
   for (i = 0; i < 100; i++) {
     i2buf[i] = 0;
     i4buf[i] = 0;
     r4buf1[i] = 0.0;
   }

   /* Get 100 entries from AP memory in I*2 format into I2BUF */
   zvmessage ("APGET - Get and display initialized AP memory segments","");
   type = 1;                        /* Return I*2 format */
   zapget (i2buf, i2offs, 100, type);
   zdisplay_i2buf();

   /* Get 100 entries from AP memory in I*4 format into I4BUF */
   type = 0;                        /* Return I*4 format */
   zapget (i4buf, i4offs, 100, type);
   zdisplay_i4buf();

   /* Get 100 entries from AP memory in R*4 format into R4BUF1  */
   type = 2;                        /* Return R*4 format */
   zapget (r4buf1, r4offs1, 100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Correlate or convolute arrays A and B to obtain C */

   /* Initialize SEGMENTS of local memory */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.25 + 50.0;
     r4buf2[i] = 50.0 - (float)(i+1) * 0.25;
   }

   /* Put local memory to AP memory
   Reformatting will be performed by FPSE as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
	       1: in = I*4, out = I*2 ( then use VFIX)
  	       2: in/out = R*4
 	       3: in/out = R*4 (ignore "IBM format")
   */
   zvmessage ("APPUT - Put initialized memory segments into AP memory","");
   zapput (r4buf1,r4offs1,100,2);
   zapput (r4buf2,r4offs2,100,2);

   zvmessage ("CONV  - Correlate arrays","");
   zconv (r4offs1,1,r4offs2,1,workoffs1,1,90,10);

   /* Get100 entries from AP memory in R*4 format into R4BUF1 */
   zvmessage ("APGET - Display Correlated array","");
   type = 2;                          /* Return R*4 format */
   zapget (r4buf1, workoffs1, 100, type);
   zdisplay_r4buf1();

   /* Clear AP memory working segment */
   zvclr (workoffs1,1,100);           /* Clear each word of WORK AREA */

   /* Convulate arrays */
   zvmessage ("CONV  - Convolute arrays","");
   zconv (r4offs1,1,r4offs2,-1,workoffs1,1,90,10);

   /* Get100 entries from AP memory in R*4 format into R4BUF1  */
   zvmessage ("APGET - Display convoluted array","");
   type = 2;                         /* Return R*4 format */
   zapget (r4buf1, workoffs1, 100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Generate histogram of an array starting at A, increment I
   with limits AMAX, AMIN, and stored in array C */

   zvmessage ("HIST  - Create histogram","");
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);           /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 2.5 + 100.0;
     r4buf2[i] = (float)(i+1) * 2.5 + 100.0;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Create histogram */
   zhist (r4offs1,1,workoffs1,100,70,r4offs2+70,r4offs2+20);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display histogram array","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                       /* Return R*4 format */
   zapget (r4buf1, workoffs1,100,type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* MMUL32 - Matrix multiply arrays A and B to obtain C */
 
   zvmessage ("MMUL32- Matrix multiply arrays A and B","");
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);          /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.1 + 100.0;
     r4buf2[i] = 100.0 - (float)(i+1) * 0.1;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Perform Matrix Multioplication */
   zmmul32 (r4offs1,1,r4offs2,1,workoffs1,1,100,10,10);

   /* Get 100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display MMUL32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                          /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Add arrays A and B to obtain C */
   zvmessage ("VADDEM  - Add arrays A and B","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);          /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (100.0 + (float)(i+1) * 0.1) * 100.0;
     r4buf2[i] = 100.0 + (float)(i+1) * 0.1;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Add arrays A and B to obtain C */
   zvaddem (r4offs1,1,r4offs2,1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1  */
   zvmessage ("APGET - Get and display VADDEM results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                            /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VCLIP - Move Array A to D, clipping it to range (B to C) */
   zvmessage ("VCLIP - Move array A to D & clip","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */
   zvclr (r4offs2  ,1,10);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.25 + 100.0;
   }
   r4buf2[0] = 105.5;
   r4buf2[1] = 120.3;
   
   /* Put segments into AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,2,2);

   /* Move array and clip */
   zvclip (r4offs1,1,r4offs2,r4offs2+1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display VCLIP results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                             /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFIX - Convert Elements from floating-point to Integer */
   zvmessage ("VFIX  - Convert Elements from floating-point to Integer","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */

   /* Initialize SEGMENTS of local memory to known real values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.25;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Convert float to integer */
   zvfix (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory into I4BUF */
   zvmessage ("APGET - Get and display VFIX results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                            /* Return R*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf();
 
 
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
 
   /* VFIX32 - Convert elements from floating point to integer */

   zvmessage ("VFIX32  - Convert from floating point to integer","");
     

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);            /* Clear work area */

   /* Initialize SEGMENTS of local memory to known real values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.25;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Convert float to integer */
   zvfix32 (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory into I4BUF */
   zvmessage ("APGET - Get and display VFIX32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                              /* Return I*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFLT - Convert elements from integer to floating point */
   zvmessage ("VFLT  - Convert elements from integer to floating point","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = (i+1) * 100;
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Perform Integer to Floating point conversion */
   zvflt (i4offs,1,workoffs1+20,1,20);

   /* Get 100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display VFLT results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();
  
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFLT32 - Convert elements from integer to floating point */
   zvmessage ("VFLT32  - Convert from integer to floating point","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = (i+1) * 100;
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Perform Integer to Floating point conversion */
   zvflt32 (i4offs,1,workoffs1+20,1,20);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */ 
   zvmessage ("APGET - Get and display VFLT32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();
 
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
 
   /* VMOV - Move array A to Array C */
   zvmessage ("VMOV  - Move array A to C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 123.5;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Move data */
   zvmov (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1  */
   zvmessage ("APGET - Get and display VMOV results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VPK8 - Packs LSB from 4 words of A into a single word C */
   zvmessage ("VPK8  - Pack low bytes of 4 words of A into 1 word of C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = 16909060.0;             /* '01020304'X */
   }

   /* Put segments in AP memory in I*4 format */
   zapput (r4buf1, r4offs1,100,0);

   /* Pack data */
   zvpk8 (r4offs1,1,workoffs1,1,100);

   /* Get packed entries from AP memory */
   zvmessage ("APGET - Get and display VPK8 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                            /* Return I*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf5();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VSADD - Add array A to Scaler B to obtain C */
 
   zvmessage ("VSADD - Add array A to Scaler B","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.1 + 100.0;
     r4buf2[i] = 100.0 - (float)(i+1) * 1.1;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Move data */
   zvsadd (r4offs1,1,r4offs2+50,workoffs1,1,100);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VSADD results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                           /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VSMUL - Multiply array A and Scaler B to obtain C */

   zvmessage ("VSMUL - Multiply array A by Scaler B","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   for (i = 0; i < 100; i++) {
     r4buf2[i] = 0.0;
   }
   zapput (r4buf2, r4offs1-50,100,2);
   zapput (r4buf2, r4offs1+50,100,2);
   zapput (r4buf2, r4offs2-50,100,2);
   zapput (r4buf2, r4offs2+50,100,2);
   zapput (r4buf2, workoffs1-50,100,2);
   zapput (r4buf2, workoffs1+50,100,2);

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     rtemp = (float) i;
     r4buf1[i] = (rtemp + 1.0 + 100.0) * 1.1;
     r4buf2[i] = 50.2;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Multiply data  */
   zvsmul (r4offs1,1,r4offs2,workoffs1,1,100);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VSMUL results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf2, workoffs1,100, type);
   zdisplay_r4buf2();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VUP8 - Unpacks 4 bytes, from four bytes of word A, into 4 words of C */
 
    zvmessage ("VUP8 - Unpack four bytes of A into four words of C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = 0X01020304;  
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Unpack data */
   zvup8 (i4offs,1,workoffs1,1,25);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VUP8 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

   /* Convert floating point to fixed point format */
   zvfix (workoffs1,1,workoffs2,1,100);
   zapget (i4buf,workoffs2,100,0);
   zdisplay_i4buf();

   return;
}
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
                                                                              
void zdisplay_i2buf (void)
{
int j;

   zvmessage ("I2BUF","");
   for (j = 0; j < 100; j+=10) {
      sprintf (string,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d",
       i2buf[j+0], i2buf[j+1], i2buf[j+2], i2buf[j+3], i2buf[j+4],
       i2buf[j+5], i2buf[j+6], i2buf[j+7], i2buf[j+8], i2buf[j+9]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

void zdisplay_i4buf (void)
{
int j;

   zvmessage ("I4BUF","");
   for (j = 0; j < 100; j+=10) {
      sprintf (string,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d",
           i4buf[j+0], i4buf[j+1], i4buf[j+2], i4buf[j+3], i4buf[j+4],
           i4buf[j+5], i4buf[j+6], i4buf[j+7], i4buf[j+8], i4buf[j+9]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

void zdisplay_i4buf5 (void)
{
int j;

   zvmessage ("I4BUF5","");
   for (j = 0; j < 25; j+=5) {
      sprintf (string,"%16d%16d%16d%16d%16d",
          i4buf[j+0], i4buf[j+1], i4buf[j+2], i4buf[j+3], i4buf[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

void zdisplay_r4buf1 (void)
{
int j;

   zvmessage ("R4BUF1","");
   for (j = 0; j < 100; j+=5) {
      sprintf (string,"%16.4f%16.4f%16.4f%16.4f%16.4f",
           r4buf1[j+0], r4buf1[j+1], r4buf1[j+2], r4buf1[j+3], r4buf1[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

void zdisplay_r4buf2 (void)
{
int j;

   zvmessage ("R4BUF2","");
   for (j = 0; j < 100; j+=5) {
      sprintf (string,"%16.4f%16.4f%16.4f%16.4f%16.4f",
           r4buf2[j+0], r4buf2[j+1], r4buf2[j+2], r4buf2[j+3], r4buf2[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
$!-----------------------------------------------------------------------------
$ create tfpse.imake
/* Imake file for Test of VICAR subroutine fpse */

#define PROGRAM tfpse

/*#define MODULE_LIST tzfpse.c tfpse.f*/
#define MODULE_LIST tfpse.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

#define LIB_RTL
/*#define LIB_LOCAL*/ 
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tzfpse.imake
/* Imake file for Test of VICAR subroutine fpse */

#define PROGRAM tzfpse

#define MODULE_LIST tzfpse.c

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define USES_ANSI_C
#define R2LIB
#define LIB_RTL
#define LIB_FORTRAN
/*#define LIB_LOCAL*/

#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tfpse.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tzfpse.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstfpse.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let _onfail="stop"
write " "
WRITE "This is a test of subroutine FPSE and the C bridge for FPSE"
write "BEFORE RUNNING, build BOTH  tfpse AND tzpse "
tfpse
write " "
tzfpse
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create fpse.hlp
1 FPSE

  FPSE is a Fortran and C subroutine library package that emulates the 
  Floating Point Systems (FPS) subroutine library for the AP-120B Array 
  Processor. It is provided for programs calling the FPS library when the 
  Array Processor is not available.

  
2 Members

  The Fortran subroutines and their respective C-Bridge interfaces are 
  described as follows: 
 
  1.  Subroutine APINIT

      Fortran interface:

        call apinit()

      C interface:  

        void zapinit();
  

  2.  Subroutine APWD

      Fortran interface:

        call apwd()

      C interface:  

        void zapwd();
  

  3.  Subroutine APGET

      Move N elements from APMEM starting at specified offset OFFS to array 
      BUF, reformatting as specified by type.
 
      Fortran interface:

        call apget (buf, offs, n, type)

      C interface:  

        void zapget (buf, offs, n, type);
  
      Where:
        buf     = Local buffer to receive data 
        offs    = Offset into memory array APMEM
        n       = Number of elements
	TYPE    = 0: in/out = I*4 (then use VPK8, VFIX32)
	          1: in = I*4, out = I*2 ( then use VFIX)
	          2: in/out = R*4
	          3: in/out = R*4 (ignore "IBM format")


  4.  Subroutine APPUT

      Move N elements from array BUF to APMEM starting at specified offset 
      OFFS into array APMEM, reformatting as specified by type.
      
      Fortran interface:

        call apput (buf, offs, n, type)

      C interface:  

        void zapput (buf, offs, n, type);
  
      Where:
        buf     = Local buffer to be moved to array APMEM
        offs    = Offset into memory array APMEM
        n       = Number of elements
	TYPE    = 0: in/out = I*4 (then use VPK8, VFIX32)
	          1: in = I*4, out = I*2 ( then use VFIX)
	          2: in/out = R*4
	          3: in/out = R*4 (ignore "IBM format")

	
  5.  Subroutine CONV

      Correlate or convolve arrays A and B to obtain C:
           C(mK) = SUM (A((m+q)I) * B(qJ), from q = 1 to N2, for m = 1,...,N1
      Note that A, B, and C are offsets in APMEM.
      If I & J have the same sign, the operation is correlation, else it
      is convolution.
      
      Fortran interface:

        call conv (a, i, b, j, c, k, n1, n2)

      C interface:  

        void zconv (a, i, b, j, c, k, n1, n2);
  
      Where:
        a       = Offset array A in APMEM
        b       = Offset array B in APMEM
        c       = Offset array c in APMEM
        i       = Increment for array A
        j       = Increment for arrau B
        k       = Increment for array C
        n1      = Iteration loop count for array A
        n2      = Iteration loop count for array B


  5.  Subroutine HIST


      Generate the histogram of an array starting in APMEM at offset A, 
      increment = I, with limits AMAX, AMIN, and put the results in  
      APMEM at offset C.
 
      Fortran interface:

	call hist (a, i, c, n, nb, amax, amin)

      C interface:  

	void zhist (a, i, c, n, nb, amax, amin);
  
      Where:
        a       = Offset array A for which the histogram is being created
        c       = Offset array C to receive results of histogram
        i       = Increment for array A
        n       = Increment for array C
       nb       = Binwidth
     amax       = Limit amax
     amin       = Limit amin


  6.  Subroutine MMUL32


      Matrix multiply arrays A and B to obtain C
 
      Fortran interface:

	call mmul32 ( a, i, b, j, c, k, mc, nc, na)

      C interface:  

        void zmmul32 ( a, i, b, j, c, k, mc, nc, na);
  
      Where:
        a       = Offset array A 
        b       = Offset array B
        c       = Offset array C 
        i       = Offset A ... increment for array A
        j       = Offset A ... increment for array B
        k       = Offset A ... increment for array C

  7.  Subroutine VADDEM

      Add arrays A and B to obtain C:
           C(mK) = A(mI) + B(mJ), m = 0,...,N-1
      Note that A, B, and C are offsets in APMEM.
 
      Fortran interface:

	call vaddem  (a, i, b, j, c, k, n)

      C interface:  

        void zvaddem  (a, i, b, j, c, k, n);
  
      Where:
        a       = Offset array A 
        b       = Offset array B
        c       = Offset array C 
        i       = Increment for array A
        j       = Increment for array B
        k       = Increment for array C
        n       = Number of elements

  8.  Subroutine VCLIP

      Move array A to D, clipping it to the range (B - C):
           D(mK) = B	if A(mI) < B		m = 0,...,N-1
           A(mI) 	if B <= A(mI) < C
           C	        if C <= A(mI)
      Note that A, B, C, and D are offsets in APMEM.
 
      Fortran interface:

	call vclip (a, i, b, c, d, k, n)

      C interface:  

        void zclip (a, i, b, c, d, k, n);

      Where:
        a       = Offset array A 
        b       = Offset array B
        c       = Offset array C 
        d       = Offset array D 
        i       = Increment for array A
        k       = Increment for array D
        n       = Number of elements


  9.  Subroutine VCLR

      Clears an array starting at C, increment = K.
 

      Fortran interface:

	call vclr (c, k, n)

      C interface:  

        void zvclr (c, k, n);
  

 10.  Subroutine VFIX

      Convert elements from Floating-point to Integer:
            C(mK) = FIX( A(mI)), m = 0,...,N-1
      Note that C and A are offsets in APMEM.
 
      Fortran interface:

	call vfix (a, i, c, k, n)

      C interface:  

        void zvfix (a, i, c, k, n);

      Where:
        a       = Offset array A 
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements


 11.  Subroutine VFLT

      Convert elements from Integer to Floating-point:
              C(mK) = FLOAT( A(mI)), m = 0,...,N-1
      Note that C and A are offsets in APMEM.

 
      Fortran interface:

	call vflt (a, i, c, k, n)

      C interface:  

        void zvflt (a, i, c, k, n);
  

      Where:
        a       = Offset array A 
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements


 12.  Subroutine VMOV

      Move array A to C:
      C(mK) = A(mI) 
      Note that A and C are offsets in APMEM.

 
      Fortran interface:

	call vmov (a, i, c, k, n)

      C interface:  

        void zvmov (a, i, c, k, n);
  
      Where:
        a       = Offset array A 
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements


 13.  Subroutine VPK8

      Packs lo-order byte (unsigned) from 4 words of A into each word of C.

 
      Fortran interface:

	call vpk8 (a, i, c, k, n)

      C interface:  

        void zvvpk8 (a, i, c, k, n);
  

 14.  Subroutine VSADD

      Add array A and scalar B to obtain C:
         C(mK) = A(mI) + B,	m = 0,...,N-1
      Note that A, B, and C are offsets in APMEM.

      Fortran interface:

	call vsadd (a, i, b, c, k, n)

      C interface:  

        void zvsadd (a, i, b, c, k, n);
  
      Where:
        a       = Offset array A 
        b       = Offset address for scaler B
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements

 15.  Subroutine VSMUL

      Multiply array A and scalar B to obtain C:
          C(mK) = A(mI) * B, m = 0,...,N-1
      Note that A, B, and C are offsets in APMEM.
 
      Fortran interface:

	call vsmul( a, i, b, c, k, n)

      C interface:  

        void zvsmul( a, i, b, c, k, n);
  
      Where:
        a       = Offset array A 
        b       = Offset address for scaler B
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements


 16.  Subroutine VUP8

      Unpack 4 bytes (unsigned) from each word of A into 4 words of C.
      Note that A, B, and C are offsets in APMEM.

 
      Fortran interface:

	call vup8( a, i, c, k, n)

      C interface:  

        void zvup8( a, i, c, k, n);
  
      Where:
        a       = Offset array A 
        c       = Offset array C 
        i       = Increment for array A
        k       = Increment for array C
        n       = Number of elements

2 Documentation

  For complete documentation, see the FPS manuals.

2 History

  Original Programmer: L. W. Kamp, 9 August 1984
  Current Cognizant Programmer: L. W. Kamp
  FPSE Source Language: Fortran

 2 Jan. 1995                 Made portable for UNIX (CRI)    
 			     ZFPSE Bridge Source Language: C

 27 January 1995 ... SP      Changed the name VADD to VADDEM because of
                             name conflict with SPICE.  

 28 Feb. 1996 ... FFM        Delete the unused variable string to avoid
                             conflict with rtl routine string. (FR 89182). 

 11 Oct 1996  ... BAM        Increased buffer size to 1000000 to accommodate
                             larger arrays and to obsolete array size as a 
                             function of the array processor memory

$ Return
$!#############################################################################
