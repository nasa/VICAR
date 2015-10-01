$!****************************************************************************
$!
$! Build proc for MIPL module minfilt
$! VPACK Version 1.9, Wednesday, January 09, 2013, 13:53:07
$!
$! Execute by entering:		$ @minfilt
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
$ write sys$output "*** module minfilt ***"
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
$ write sys$output "Invalid argument given to minfilt.com file -- ", primary
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
$   if F$SEARCH("minfilt.imake") .nes. ""
$   then
$      vimake minfilt
$      purge minfilt.bld
$   else
$      if F$SEARCH("minfilt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake minfilt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @minfilt.bld "STD"
$   else
$      @minfilt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create minfilt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack minfilt.com -mixed -
	-s minfilt.f -
	-i minfilt.imake -
	-p minfilt.pdf -
	-t tstminfilt.pdf tstminfilt.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create minfilt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C     PROGRAM minfilt
c        jan 93       jjl      
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
	implicit none
      integer*4 nsmax
      parameter (nsmax=10000)
c      implicit integer(a-z)
      integer*4 sl,ss,nl,ns,nlin,nsin,nlw,nsw,icode
      integer*4 curr,i,j,l,l1,l2,l3,l4,l5,m,iline,line
c      integer*2 outbuf(nsmax), inbuf(51*nsmax)
	real*4	outbuf(nsmax), inbuf(51*nsmax)
      integer*4 outunit,inunit,cnt,nlwm1,imax,ipos,inc
      integer*4 stat
	character*3 orgin 
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      	character*8 format
	
	call xvmessage ('** MINFILT - 18-May-2011',' ')
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      call xvunit(inunit,'INP',1,stat,' ')
	call xvopen(inunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(inunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
        if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

	call xvclose(inunit,stat,' ')

      call xvopen(inunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     +            'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

      call xvsize(sl,ss,nl,ns,nlin,nsin)
c	the following calls were never implemented
c	call xvp('ONL',onl,cnt)
c	call xvp('ONS',ons,cnt)
      call xvunit(outunit,'OUT',1,STAT,' ')
      call xvopen(outunit,stat,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
      SL = SL - 1

C
C  GET NLW AND NSW VALUES
      call xvp('NLW',nlw,cnt)
      call xvp('NSW',nsw,cnt)
      NLW = NLW / 2 * 2 + 1
      NSW = NSW / 2 * 2 + 1

      if(nsmax.lt.ns+nsw-1)then
         call xvmessage('??E - input line too long',' ')
         call abend
      endif
      if(51*nsmax.lt.(ns+nsw-1)*nlw)then
         call xvmessage('??E - insufficient memory available',' ')
         call abend
      endif
C
C  DETERMINE STARTING LINE AND INCREMENT VALUES
      LINE = 1 - NLW / 2
      L = NSW / 2
      L1 = (NSW + 1) / 2
      L2 = L1 + 1
      L3 = L1 - 1
      L4 = L1 + NS - 2
      L5 = L1 + NS
      M = NL + NL
      CURR = (NLW + 1) / 2
      IPOS = 0
      INC = NS + NSW - 1
      IMAX = (NLW) * INC
      NLWM1 = NLW - 1
      IF (NLWM1 .NE. 0) THEN
C
C  READ INITIAL NLW LINES INTO CORE, REFLECTING AT BOUNDARIES
C
         DO 200 I=1,NLWM1
            ILINE=LINE
            LINE=LINE+1
  201       IF (ILINE .LT. 1) ILINE = 2 - ILINE
            IF (ILINE .GT. NL) ILINE = M - ILINE
            IF (ILINE .LT. 1) GO TO 201
            call xvread(inunit,inbuf(ipos+l1),stat,'NSAMPS',ns,'SAMP',ss,
     &                'LINE',sl+iline,' ')
            IF (NSW .NE. 1) THEN
                do j=1,L
                   inbuf(ipos+L3+1-j)=inbuf(ipos+L2-1+j)
                   inbuf(ipos+L5-1+j)=inbuf(ipos+L4+1-j)
                enddo
            ENDIF
            IPOS=IPOS+INC
200      CONTINUE
      ENDIF


      DO 300 I = 1, NL
         ILINE = LINE
         LINE = LINE + 1
301      IF (ILINE .LT. 1) ILINE = 2 - ILINE
         IF(ILINE .GT. NL) ILINE = M - ILINE
         IF (ILINE .LT. 1) GOTO 301
         call xvread(inunit,inbuf(ipos+l1),stat,'NSAMPS',ns,'SAMP',ss,
     &               'LINE',sl+iline,' ')
         IF(NSW .NE. 1) THEN
             do j=1,L
                inbuf(ipos+L3+1-j)=inbuf(ipos+L2-1+j)
                inbuf(ipos+L5-1+j)=inbuf(ipos+L4+1-j)
             enddo
         ENDIF

         call min2d(inbuf,outbuf,nlw,nsw,ns)

         call xvwrit(outunit,outbuf,stat,' ')
         IPOS=IPOS+INC
         IF(IPOS .GE. IMAX) IPOS=0
         CURR=MOD(CURR,NLW)+1
300   CONTINUE
C
C *****         CLOSE DATA SETS
C
      call xvclose(inunit,stat,' ')
      call xvclose(outunit,stat,' ')
      RETURN
      END
c==============================================================
      subroutine min2d(in,out,nlw,nsw,ns)
	implicit none
c      integer*2 in(1),out(1),mindn
	integer*4 nlw,nsw,ns
	real*4 in(ns),out(ns),mindn 
      integer*4 i,j,k,len,m,mincol
      len=ns+nsw-1

c find min dn for left most block
      k=0
      mindn=in(1)
      mincol=1
      do j=1,nlw
        do i=1,nsw
          k=k+1
          if(in(k).lt.mindn)then
             mincol=i
             mindn=in(k)      
          endif
        enddo
        k=j*len
      enddo
      out(1)=mindn

c do the rest
      do i=2,ns
        if(mincol.ge.i)then                ! check only right column
           k=i+nsw-1
           do j=1,nlw
             if(in(k).lt.mindn)then
                mincol=i+nsw-1
                mindn=in(k)      
             endif
             k=k+len
           enddo
           out(i)=mindn
        else                               ! check all columns
           mindn=in(i)
           mincol=i
           k=i
           do j=1,nlw
             do m=i,i+nsw-1
               if(in(k).lt.mindn)then
                  mincol=m
                  mindn=in(k)      
               endif
               k=k+1
             enddo
             k=j*len+i
           enddo
           out(i)=mindn
        endif
      enddo
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create minfilt.imake
#define  PROGRAM   minfilt

#define MODULE_LIST minfilt.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create minfilt.pdf
process help=*
PARM INP	TYPE = STRING
PARM OUT	TYPE = STRING 	          DEFAULT = MINFILT
PARM SIZE	TYPE = INTEGER 	COUNT=0:4 DEFAULT=--
#PARM ONL	TYPE = INTEGER  COUNT=0:1 DEFAULT=--
#PARM ONS    TYPE = INTEGER  COUNT=0:1 DEFAULT=--
PARM NLW    TYPE = INTEGER  COUNT=1	  DEFAULT = 3
PARM NSW    TYPE = INTEGER	COUNT=1	  DEFAULT = 3
END-PROC
.TITLE
VICAR program MINFILT.

.HELP
PURPOSE:
Selects the minimum DN within a rectangular convolution window for
images in BYTE, HALF, FULL or REAL formats.

.PAGE
EXECUTION:

MINFILT may be executed in the following manner:

		MINFILT INP=A OUT=B SIZE=(SL,SS,NL,NS) PARAMS

where INP, OUT, SIZE, AND PARAMS are parameters and are explained in their
respective parameter section.

.PAGE
OPERATION:

MINFILT finds the local minimum value of a rectangular window centered at
each pixel in an image. 
The program takes advantage of the fact that if the minimum of an area
was not in the left column then the next area to the right (one 
pixel over) need only be checked in the right column.

EXAMPLE:

	 MINFILT INP=A OUT=B NLW=5 NSW=7
  The window is 5 lines by 7 samples.

.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: J Lorre 1/30/93
COGNIZANT PROGRAMMER:  Ray Bambery

    May 18, 2011 - Ray Bambery - Mods for gcc4.4.4 64-bit and
                    add FULL and REAL data types, changed 
                    confusing NL & NS parms to ONL and ONS since
                    they refer to output file

    09-Jan-2013 -lwk- ONL and ONS commented out, since the code does
                      not use them.  (NL,NS are supposed to be alternatives
                      to the SIZE parameter.)

.LEVEL1
.VARI INP
STRING-input dataset.
.VARI OUT
STRING-output dataset.
.VARI SIZE
4 INTEGERS-VICAR size field,
(SL,SS,NL,NS).
.VARI ONL
INTEGER-number of lines in
output file.
.VARI ONS
INTEGER-Number of samples in
output file.
.VARI NLW
INTEGER-Size of filter kernel
in lines.
.VARI NSW
INTEGER-Size of filter kernel
in samples.
.LEVEL2
.VARI INP
STRING - INP=A where A is the input dataset name.
.VARI OUT
STRING - OUT=B where B is the output dataset name.
.VARI SIZE
4 INTEGERS - SIZE=(SL,SS,NL,NS) where SL is the starting line, SS is the
 starting sample, NL is the number of lines in the input dataset and NS
 is the number of samples in the input dataset. (SIZE is usually defined
 as SIZE=(1,1,NL,NS)). Default is taken from the VICAR label within the
 program.
.VARI ONL
INTEGER - NL=N1 where is N1 is the number of lines in the output dataset.
.VARI ONS
INTEGER - NS=N1 where is N1 is the number of samples in the output dataset.
.VARI NLW
INTEGER - NLW=I1 where I1 is an integer and specifies the size of the filter
 kernel in lines. Default is NLW=3.
.VARI NSW
INTEGER - NSW=I2 where I2 is an integer and specifies the size of the filter
 kernel in samples. Default is NSW=3.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstminfilt.pdf
procedure
refgbl $echo
refgbl $autousage
! Jun 24, 2012 - RJB
! TEST SCRIPT FOR MINFILT
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list gausnois  
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!
body
let $autousage = "none"
let _onfail="stop"
let $echo="yes"
!This is a test file for minfilt
!BYTE
gen out=a.img nl=10 ns=10
list a.img
minfilt inp=a.img out=b1.img nlw=5 nsw=1
list b1.img
minfilt inp=a.img out=b2.img nlw=1 nsw=5
list b2.img
minfilt inp=a.img out=b3.img nlw=5 nsw=5
list b3.img
gausnois out=g.img nl=10 ns=10 seed=10595362
list g.img
minfilt inp=g.img out=b4.img nlw=5 nsw=5
list b4.img

!HALF
gen out=h.img nl=10 ns=10 format=half ival=1000 linc=100 sinc=100
list h.img
minfilt inp=h.img out=c1.img nlw=5 nsw=1
list c1.img
minfilt inp=h.img out=c2.img nlw=1 nsw=5
list c2.img
minfilt inp=h.img out=c3.img nlw=5 nsw=5
list c3.img
gausnois out=n.img nl=10 ns=10 format=HALF mean=1280 seed=10595365
list n.img
minfilt inp=n.img out=c4.img nlw=5 nsw=5
list c4.img

!FULL
gen out=f.img nl=10 ns=10 format=full ival=10000 linc=100 sinc=100
list f.img
minfilt inp=f.img out=d1.img nlw=5 nsw=1
list d1.img
minfilt inp=f.img out=d2.img nlw=1 nsw=5
list d2.img
minfilt inp=f.img out=d3.img nlw=5 nsw=5
list d3.img
gausnois out=p.img nl=10 ns=10 format=FULL mean=12800 sigma=140 seed=10595368
list p.img
minfilt inp=p.img out=d4.img nlw=5 nsw=5
list d4.img


!REAL
gen out=r.img nl=10 ns=10 format=real ival=1504.4 linc=156.6 sinc=156.6
list r.img
minfilt inp=r.img out=e1.img nlw=5 nsw=1
list e1.img
minfilt inp=r.img out=e2.img nlw=1 nsw=5
list e2.img
minfilt inp=r.img out=e3.img nlw=5 nsw=5
list e3.img
gausnois out=q.img nl=10 ns=10 format=REAL mean=134.5 sigma=19.0 seed=10595372
list q.img
minfilt inp=q.img out=e4.img nlw=5 nsw=5
list e4.img

! clean up:
ush rm -f ?.img
ush rm -f ??.img

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstminfilt.log_solos
tstminfilt
gen out=a.img nl=10 ns=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:01 2013
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
minfilt inp=a.img out=b1.img nlw=5 nsw=1
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list b1.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:01 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:02 2013
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       0   1   2   3   4   5   6   7   8   9
      3       0   1   2   3   4   5   6   7   8   9
      4       1   2   3   4   5   6   7   8   9  10
      5       2   3   4   5   6   7   8   9  10  11
      6       3   4   5   6   7   8   9  10  11  12
      7       4   5   6   7   8   9  10  11  12  13
      8       5   6   7   8   9  10  11  12  13  14
      9       6   7   8   9  10  11  12  13  14  15
     10       7   8   9  10  11  12  13  14  15  16
minfilt inp=a.img out=b2.img nlw=1 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list b2.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:01 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:03 2013
     Samp     1       3       5       7       9
   Line
      1       0   0   0   1   2   3   4   5   6   7
      2       1   1   1   2   3   4   5   6   7   8
      3       2   2   2   3   4   5   6   7   8   9
      4       3   3   3   4   5   6   7   8   9  10
      5       4   4   4   5   6   7   8   9  10  11
      6       5   5   5   6   7   8   9  10  11  12
      7       6   6   6   7   8   9  10  11  12  13
      8       7   7   7   8   9  10  11  12  13  14
      9       8   8   8   9  10  11  12  13  14  15
     10       9   9   9  10  11  12  13  14  15  16
minfilt inp=a.img out=b3.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list b3.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:01 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:03 2013
     Samp     1       3       5       7       9
   Line
      1       0   0   0   1   2   3   4   5   6   7
      2       0   0   0   1   2   3   4   5   6   7
      3       0   0   0   1   2   3   4   5   6   7
      4       1   1   1   2   3   4   5   6   7   8
      5       2   2   2   3   4   5   6   7   8   9
      6       3   3   3   4   5   6   7   8   9  10
      7       4   4   4   5   6   7   8   9  10  11
      8       5   5   5   6   7   8   9  10  11  12
      9       6   6   6   7   8   9  10  11  12  13
     10       7   7   7   8   9  10  11  12  13  14
gausnois out=g.img nl=10 ns=10 seed=10595362
Beginning VICAR task gausnois
Gausnois - 18-Jun-2012
list g.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:04 2013
     Samp     1       3       5       7       9
   Line
      1     130 159 113 146 157 133 119 135 106 138
      2     105 140 114 152 123 100 124 126 126 126
      3     109 114 141 145 125 141 105 137 118 144
      4     122 131 137 137 127 169 130 109 114 144
      5     119 111 102 142 125 135 138 139 141 111
      6     117 141 122 127 102 132 106 130 125 102
      7     142 127 143 138 114 146 142 119 132 153
      8     115 119 152 134 111 121 150 140 119 101
      9     112 100  97 139 122 116 125 126 127 145
     10     117 129 152  96 138 163 122 127 124  89
minfilt inp=g.img out=b4.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list b4.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:04 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
     Samp     1       3       5       7       9
   Line
      1     105 105 105 100 100 100 100 100 105 106
      2     105 105 105 100 100 100 100 100 105 106
      3     102 102 102 100 100 100 100 100 105 106
      4     102 102 102 100 100 100 100 100 102 102
      5     102 102 102 102 102 102 102 102 102 102
      6     102 102 102 102 102 102 102 101 101 101
      7      97  97  97  97  97 102 102 101 101 101
      8      97  96  96  96  96  96 102  89  89  89
      9      97  96  96  96  96  96 111  89  89  89
     10      97  96  96  96  96  96 111  89  89  89
gen out=h.img nl=10 ns=10 format=half ival=1000 linc=100 sinc=100
Beginning VICAR task gen
GEN Version 6
GEN task completed
list h.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1000  1100  1200  1300  1400  1500  1600  1700  1800  1900
      2      1100  1200  1300  1400  1500  1600  1700  1800  1900  2000
      3      1200  1300  1400  1500  1600  1700  1800  1900  2000  2100
      4      1300  1400  1500  1600  1700  1800  1900  2000  2100  2200
      5      1400  1500  1600  1700  1800  1900  2000  2100  2200  2300
      6      1500  1600  1700  1800  1900  2000  2100  2200  2300  2400
      7      1600  1700  1800  1900  2000  2100  2200  2300  2400  2500
      8      1700  1800  1900  2000  2100  2200  2300  2400  2500  2600
      9      1800  1900  2000  2100  2200  2300  2400  2500  2600  2700
     10      1900  2000  2100  2200  2300  2400  2500  2600  2700  2800
minfilt inp=h.img out=c1.img nlw=5 nsw=1
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list c1.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1000  1100  1200  1300  1400  1500  1600  1700  1800  1900
      2      1000  1100  1200  1300  1400  1500  1600  1700  1800  1900
      3      1000  1100  1200  1300  1400  1500  1600  1700  1800  1900
      4      1100  1200  1300  1400  1500  1600  1700  1800  1900  2000
      5      1200  1300  1400  1500  1600  1700  1800  1900  2000  2100
      6      1300  1400  1500  1600  1700  1800  1900  2000  2100  2200
      7      1400  1500  1600  1700  1800  1900  2000  2100  2200  2300
      8      1500  1600  1700  1800  1900  2000  2100  2200  2300  2400
      9      1600  1700  1800  1900  2000  2100  2200  2300  2400  2500
     10      1700  1800  1900  2000  2100  2200  2300  2400  2500  2600
minfilt inp=h.img out=c2.img nlw=1 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list c2.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:06 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1000  1000  1000  1100  1200  1300  1400  1500  1600  1700
      2      1100  1100  1100  1200  1300  1400  1500  1600  1700  1800
      3      1200  1200  1200  1300  1400  1500  1600  1700  1800  1900
      4      1300  1300  1300  1400  1500  1600  1700  1800  1900  2000
      5      1400  1400  1400  1500  1600  1700  1800  1900  2000  2100
      6      1500  1500  1500  1600  1700  1800  1900  2000  2100  2200
      7      1600  1600  1600  1700  1800  1900  2000  2100  2200  2300
      8      1700  1700  1700  1800  1900  2000  2100  2200  2300  2400
      9      1800  1800  1800  1900  2000  2100  2200  2300  2400  2500
     10      1900  1900  1900  2000  2100  2200  2300  2400  2500  2600
minfilt inp=h.img out=c3.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list c3.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:05 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:07 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1000  1000  1000  1100  1200  1300  1400  1500  1600  1700
      2      1000  1000  1000  1100  1200  1300  1400  1500  1600  1700
      3      1000  1000  1000  1100  1200  1300  1400  1500  1600  1700
      4      1100  1100  1100  1200  1300  1400  1500  1600  1700  1800
      5      1200  1200  1200  1300  1400  1500  1600  1700  1800  1900
      6      1300  1300  1300  1400  1500  1600  1700  1800  1900  2000
      7      1400  1400  1400  1500  1600  1700  1800  1900  2000  2100
      8      1500  1500  1500  1600  1700  1800  1900  2000  2100  2200
      9      1600  1600  1600  1700  1800  1900  2000  2100  2200  2300
     10      1700  1700  1700  1800  1900  2000  2100  2200  2300  2400
gausnois out=n.img nl=10 ns=10 format=HALF mean=1280 seed=10595365
Beginning VICAR task gausnois
Gausnois - 18-Jun-2012
list n.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:07 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1282  1311  1265  1298  1309  1285  1271  1287  1258  1290
      2      1257  1292  1266  1304  1275  1252  1276  1278  1278  1278
      3      1261  1266  1293  1297  1277  1293  1257  1289  1270  1296
      4      1274  1283  1289  1289  1279  1321  1282  1261  1266  1296
      5      1271  1263  1254  1294  1277  1287  1290  1291  1293  1263
      6      1269  1293  1274  1279  1254  1284  1258  1282  1277  1254
      7      1294  1279  1295  1290  1266  1298  1294  1271  1284  1305
      8      1267  1271  1304  1286  1263  1273  1302  1292  1271  1253
      9      1264  1252  1249  1291  1274  1268  1277  1278  1279  1297
     10      1269  1281  1304  1248  1290  1315  1274  1279  1276  1241
minfilt inp=n.img out=c4.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list c4.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:07 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:08 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      1257  1257  1257  1252  1252  1252  1252  1252  1257  1258
      2      1257  1257  1257  1252  1252  1252  1252  1252  1257  1258
      3      1254  1254  1254  1252  1252  1252  1252  1252  1257  1258
      4      1254  1254  1254  1252  1252  1252  1252  1252  1254  1254
      5      1254  1254  1254  1254  1254  1254  1254  1254  1254  1254
      6      1254  1254  1254  1254  1254  1254  1254  1253  1253  1253
      7      1249  1249  1249  1249  1249  1254  1254  1253  1253  1253
      8      1249  1248  1248  1248  1248  1248  1254  1241  1241  1241
      9      1249  1248  1248  1248  1248  1248  1263  1241  1241  1241
     10      1249  1248  1248  1248  1248  1248  1263  1241  1241  1241
gen out=f.img nl=10 ns=10 format=full ival=10000 linc=100 sinc=100
Beginning VICAR task gen
GEN Version 6
GEN task completed
list f.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:08 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          10000      10100      10200      10300      10400      10500      10600      10700      10800      10900
      2          10100      10200      10300      10400      10500      10600      10700      10800      10900      11000
      3          10200      10300      10400      10500      10600      10700      10800      10900      11000      11100
      4          10300      10400      10500      10600      10700      10800      10900      11000      11100      11200
      5          10400      10500      10600      10700      10800      10900      11000      11100      11200      11300
      6          10500      10600      10700      10800      10900      11000      11100      11200      11300      11400
      7          10600      10700      10800      10900      11000      11100      11200      11300      11400      11500
      8          10700      10800      10900      11000      11100      11200      11300      11400      11500      11600
      9          10800      10900      11000      11100      11200      11300      11400      11500      11600      11700
     10          10900      11000      11100      11200      11300      11400      11500      11600      11700      11800
minfilt inp=f.img out=d1.img nlw=5 nsw=1
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list d1.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:08 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:09 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          10000      10100      10200      10300      10400      10500      10600      10700      10800      10900
      2          10000      10100      10200      10300      10400      10500      10600      10700      10800      10900
      3          10000      10100      10200      10300      10400      10500      10600      10700      10800      10900
      4          10100      10200      10300      10400      10500      10600      10700      10800      10900      11000
      5          10200      10300      10400      10500      10600      10700      10800      10900      11000      11100
      6          10300      10400      10500      10600      10700      10800      10900      11000      11100      11200
      7          10400      10500      10600      10700      10800      10900      11000      11100      11200      11300
      8          10500      10600      10700      10800      10900      11000      11100      11200      11300      11400
      9          10600      10700      10800      10900      11000      11100      11200      11300      11400      11500
     10          10700      10800      10900      11000      11100      11200      11300      11400      11500      11600
minfilt inp=f.img out=d2.img nlw=1 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list d2.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:08 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:09 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          10000      10000      10000      10100      10200      10300      10400      10500      10600      10700
      2          10100      10100      10100      10200      10300      10400      10500      10600      10700      10800
      3          10200      10200      10200      10300      10400      10500      10600      10700      10800      10900
      4          10300      10300      10300      10400      10500      10600      10700      10800      10900      11000
      5          10400      10400      10400      10500      10600      10700      10800      10900      11000      11100
      6          10500      10500      10500      10600      10700      10800      10900      11000      11100      11200
      7          10600      10600      10600      10700      10800      10900      11000      11100      11200      11300
      8          10700      10700      10700      10800      10900      11000      11100      11200      11300      11400
      9          10800      10800      10800      10900      11000      11100      11200      11300      11400      11500
     10          10900      10900      10900      11000      11100      11200      11300      11400      11500      11600
minfilt inp=f.img out=d3.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list d3.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:08 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:10 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          10000      10000      10000      10100      10200      10300      10400      10500      10600      10700
      2          10000      10000      10000      10100      10200      10300      10400      10500      10600      10700
      3          10000      10000      10000      10100      10200      10300      10400      10500      10600      10700
      4          10100      10100      10100      10200      10300      10400      10500      10600      10700      10800
      5          10200      10200      10200      10300      10400      10500      10600      10700      10800      10900
      6          10300      10300      10300      10400      10500      10600      10700      10800      10900      11000
      7          10400      10400      10400      10500      10600      10700      10800      10900      11000      11100
      8          10500      10500      10500      10600      10700      10800      10900      11000      11100      11200
      9          10600      10600      10600      10700      10800      10900      11000      11100      11200      11300
     10          10700      10700      10700      10800      10900      11000      11100      11200      11300      11400
gausnois out=p.img nl=10 ns=10 format=FULL mean=12800 sigma=140 seed=10595368
Beginning VICAR task gausnois
Gausnois - 18-Jun-2012
list p.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          12820      13075      12676      12958      13061      12849      12721      12867      12615      12892
      2          12602      12908      12678      13011      12757      12556      12768      12788      12790      12786
      3          12633      12683      12914      12956      12774      12921      12603      12883      12718      12947
      4          12748      12832      12881      12885      12794      13160      12823      12640      12679      12941
      5          12728      12657      12579      12925      12777      12865      12895      12896      12917      12657
      6          12706      12921      12748      12798      12572      12839      12614      12817      12777      12581
      7          12923      12798      12934      12891      12678      12963      12924      12725      12837      13027
      8          12692      12727      13011      12855      12656      12741      13000      12913      12725      12566
      9          12668      12560      12537      12901      12750      12697      12775      12791      12792      12951
     10          12708      12815      13011      12525      12891      13111      12755      12795      12765      12466
minfilt inp=p.img out=d4.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list d4.img
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1          12602      12602      12602      12556      12556      12556      12556      12556      12603      12615
      2          12602      12602      12602      12556      12556      12556      12556      12556      12603      12615
      3          12579      12579      12579      12556      12556      12556      12556      12556      12603      12615
      4          12579      12579      12572      12556      12556      12556      12556      12556      12581      12581
      5          12579      12579      12572      12572      12572      12572      12572      12581      12581      12581
      6          12579      12579      12572      12572      12572      12572      12572      12566      12566      12566
      7          12537      12537      12537      12537      12537      12572      12572      12566      12566      12566
      8          12537      12525      12525      12525      12525      12525      12572      12466      12466      12466
      9          12537      12525      12525      12525      12525      12525      12656      12466      12466      12466
     10          12537      12525      12525      12525      12525      12525      12656      12466      12466      12466
gen out=r.img nl=10 ns=10 format=real ival=1504.4 linc=156.6 sinc=156.6
Beginning VICAR task gen
GEN Version 6
GEN task completed
list r.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      2       1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03
      3       1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03
      4       1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03
      5       2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03
      6       2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03
      7       2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03
      8       2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03   4.010E+03
      9       2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03   4.010E+03   4.167E+03
     10       2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03   4.010E+03   4.167E+03   4.323E+03
minfilt inp=r.img out=e1.img nlw=5 nsw=1
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list e1.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:12 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      2       1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      3       1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      4       1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03
      5       1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03
      6       1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03
      7       2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03
      8       2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03
      9       2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03
     10       2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03   4.010E+03
minfilt inp=r.img out=e2.img nlw=1 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list e2.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:13 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.504E+03   1.504E+03   1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03
      2       1.661E+03   1.661E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03
      3       1.818E+03   1.818E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      4       1.974E+03   1.974E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03
      5       2.131E+03   2.131E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03
      6       2.287E+03   2.287E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03
      7       2.444E+03   2.444E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03
      8       2.601E+03   2.601E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03
      9       2.757E+03   2.757E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03
     10       2.914E+03   2.914E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03   3.853E+03   4.010E+03
minfilt inp=r.img out=e3.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list e3.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Jan  9 13:19:11 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:14 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.504E+03   1.504E+03   1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03
      2       1.504E+03   1.504E+03   1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03
      3       1.504E+03   1.504E+03   1.504E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03
      4       1.661E+03   1.661E+03   1.661E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03
      5       1.818E+03   1.818E+03   1.818E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03
      6       1.974E+03   1.974E+03   1.974E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03
      7       2.131E+03   2.131E+03   2.131E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03
      8       2.287E+03   2.287E+03   2.287E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03
      9       2.444E+03   2.444E+03   2.444E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03
     10       2.601E+03   2.601E+03   2.601E+03   2.757E+03   2.914E+03   3.070E+03   3.227E+03   3.384E+03   3.540E+03   3.697E+03
gausnois out=q.img nl=10 ns=10 format=REAL mean=134.5 sigma=19.0 seed=10595372
Beginning VICAR task gausnois
Gausnois - 18-Jun-2012
list q.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:14 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.373E+02   1.719E+02   1.177E+02   1.560E+02   1.700E+02   1.412E+02   1.238E+02   1.437E+02   1.094E+02   1.470E+02
      2       1.077E+02   1.493E+02   1.181E+02   1.633E+02   1.287E+02   1.014E+02   1.303E+02   1.330E+02   1.332E+02   1.326E+02
      3       1.119E+02   1.187E+02   1.501E+02   1.558E+02   1.311E+02   1.509E+02   1.078E+02   1.459E+02   1.234E+02   1.546E+02
      4       1.275E+02   1.389E+02   1.456E+02   1.460E+02   1.337E+02   1.834E+02   1.377E+02   1.128E+02   1.181E+02   1.537E+02
      5       1.248E+02   1.151E+02   1.046E+02   1.515E+02   1.315E+02   1.434E+02   1.474E+02   1.476E+02   1.504E+02   1.152E+02
      6       1.218E+02   1.510E+02   1.275E+02   1.344E+02   1.037E+02   1.399E+02   1.094E+02   1.369E+02   1.314E+02   1.048E+02
      7       1.513E+02   1.343E+02   1.527E+02   1.470E+02   1.181E+02   1.566E+02   1.514E+02   1.244E+02   1.395E+02   1.654E+02
      8       1.199E+02   1.247E+02   1.632E+02   1.421E+02   1.150E+02   1.265E+02   1.617E+02   1.499E+02   1.244E+02   1.029E+02
      9       1.167E+02   1.019E+02   9.887E+01   1.483E+02   1.278E+02   1.206E+02   1.312E+02   1.333E+02   1.335E+02   1.550E+02
     10       1.221E+02   1.367E+02   1.632E+02   9.721E+01   1.469E+02   1.767E+02   1.285E+02   1.338E+02   1.298E+02   8.927E+01
minfilt inp=q.img out=e4.img nlw=5 nsw=5
Beginning VICAR task minfilt
** MINFILT - 18-May-2011
list e4.img
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GAUSNOIS  User:lwk       Date_Time:Wed Jan  9 13:19:14 2013
 Task:MINFILT   User:lwk       Date_Time:Wed Jan  9 13:19:15 2013
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.077E+02   1.077E+02   1.077E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.078E+02   1.094E+02
      2       1.077E+02   1.077E+02   1.077E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.078E+02   1.094E+02
      3       1.046E+02   1.046E+02   1.046E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.078E+02   1.094E+02
      4       1.046E+02   1.046E+02   1.037E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.014E+02   1.048E+02   1.048E+02
      5       1.046E+02   1.046E+02   1.037E+02   1.037E+02   1.037E+02   1.037E+02   1.037E+02   1.048E+02   1.048E+02   1.048E+02
      6       1.046E+02   1.046E+02   1.037E+02   1.037E+02   1.037E+02   1.037E+02   1.037E+02   1.029E+02   1.029E+02   1.029E+02
      7       9.887E+01   9.887E+01   9.887E+01   9.887E+01   9.887E+01   1.037E+02   1.037E+02   1.029E+02   1.029E+02   1.029E+02
      8       9.887E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   1.037E+02   8.927E+01   8.927E+01   8.927E+01
      9       9.887E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   1.150E+02   8.927E+01   8.927E+01   8.927E+01
     10       9.887E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   9.721E+01   1.150E+02   8.927E+01   8.927E+01   8.927E+01
ush rm -f ?.img
ush rm -f ??.img
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
