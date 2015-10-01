$!****************************************************************************
$!
$! Build proc for MIPL module dvector
$! VPACK Version 1.9, Wednesday, August 29, 2001, 10:01:10
$!
$! Execute by entering:		$ @dvector
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
$ write sys$output "*** module dvector ***"
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
$ write sys$output "Invalid argument given to dvector.com file -- ", primary
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
$   if F$SEARCH("dvector.imake") .nes. ""
$   then
$      vimake dvector
$      purge dvector.bld
$   else
$      if F$SEARCH("dvector.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dvector
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dvector.bld "STD"
$   else
$      @dvector.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dvector.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dvector.com -mixed -
	-s dvector.f -
	-i dvector.imake -
	-p dvector.pdf -
	-t tstdvector.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dvector.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'

      subroutine main44

      implicit none

      character*32 oformat
      character*4 ifmt,ofmt,ifmt3
      real*4      data(4,32),exag
      integer*2   buf(500000),zero
      integer*4   inmark(101),file
      integer*4   maxcnt, count, def, dn, nids,inunit
      integer*4   status, nl, np, i, nl3, ns3, outunit
      integer*4   sl, nlstor, pass, npass, n, top, bot, line
      integer*4   ldat, tpt

! Begin data initialization

      data exag /0.0/
      data file/0/, maxcnt/0/, count/0/, def/0/, dn/0/, zero/0/
      data nids/0/,inunit/0/, status/0/, nl/0/, np/0/, i/0/
      data nl3/0/, ns3/0/, outunit/0/,sl/0/, nlstor/0/, pass/0/
      data npass/0/, n/0/, top/0/, bot/0/, line/0/,ldat/0/, tpt/0/

      call zia (inmark,101)
      call zia (data, 4*32)
      call zia (buf, 500000/2)
      oformat = ' '
      ifmt = ' '
      ofmt = ' '
      ifmt3= ' '
     
! End data initialization

      call ifmessage ('DVECTOR version 31-OCT-94')
      maxcnt = 1
      call xvparm('EXAG',exag,count,def,maxcnt)
      call xvparm('DN',dn,count,def,maxcnt)
      call xvpcnt('INP',nids)

c open files
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','HALF','IO_ACT','AS'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inunit,status,'NL',nl,'NS',np,'FORMAT',oformat,' ')

      do i=2,nids
        call xvunit(inmark(i),'INP',i,status,' ')
        call xvopen(inmark(i),status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      enddo

      call xvunit(outunit,'OUT',1,status,' ')
      call xvopen(outunit,status,'U_FORMAT','HALF','O_FORMAT',
     +            oformat,'OP','WRITE','OPEN_ACT','AS',
     +            'IO_ACT','AS',' ')
      sl=1

c compute constants
      nlstor=500000/np
      if(nlstor.gt.nl) nlstor=nl
      npass=nl/nlstor
      if(npass*nlstor.lt.nl) npass=npass+1

c main line loop for image blocks
      do 120 pass=1,npass

c       Read a block of lines into buf
        n=1
        top=(pass-1)*nlstor+1
        bot=pass*nlstor
        do 130 line=top,bot
          if(line.gt.nl) goto 130
          if(dn.ge.0)then
            call xvread(inunit,buf(n),status,'LINE',line,' ')
          else
            call mve(2,np,zero,buf(n),0,1)
          endif
          n=n+np
130     continue

c       Loop on input mark files
        do file=2,nids
          call xvget(inmark(file),status,'NL',nl3,'NS',ns3,' ')

c         Loop on tiepoint records
          do 140 ldat=1,nl3
            call xvread(inmark(file),data,status,'LINE',ldat,' ')

c           Loop on tiepoints
            do 150 tpt=1,32
              if((data(1,tpt).eq.0.0).and.(data(2,tpt).eq.0.0)) goto 150

c             Draw vectors on image segment stored in buf.
              call draw(buf,np,nlstor,top,bot,data(1,tpt),data(2,tpt),
     +                data(3,tpt),data(4,tpt),exag,dn,tpt,pass,file)
       
150         continue
140       continue
        enddo
c       Write the buffer back to the output
        n=1
        do 160 line=top,bot
          if(line.gt.nl) goto 160
          call xvwrit(outunit,buf(n),status,'LINE',line,' ')
          n=n+np
160     continue

120   continue

      return
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine draw(buf,np,nlstor,top,bot,y1,x1,y2,x2,exag,
     +                dn,tpt,pass,file)

c Draw dvectors on image segment

      integer*2 buf(np,nlstor)
      integer*4 top,bot,dn,tpt,pass,file,dnout

      rtop=top
      rbot=bot
      rleft=1.
      rright=np
      dy=(y2-y1)*exag
      dx=(x2-x1)*exag
      y3=y1+dy
      x3=x1+dx
      if((y1.lt.rtop).and.(y3.lt.rtop)) return
      if((y1.gt.rbot).and.(y3.gt.rbot)) return
      dnout=dn

      if(dn.lt.0)then
        angle=57.2958*atan2(-dy,dx)
        if(angle.lt.0.0)angle=angle+360.
        angle=angle*255./360.
        dnout=nint(angle)
        if(dnout.eq.0)dnout=1
      endif

c draw the base X
      n=0
      if((file.eq.2).and.(dn.ge.0))n=1   ! only on first mark file
      l=nint(y1)
      j=nint(x1)
      do 5 i=j-n,j+n
          if((i.ge.1).and.(i.le.np).and.(l.ge.top).
     +      and.(l.le.bot)) buf(i,l-top+1)=dnout
5     continue
      do 6 i=l-n,l+n
        if((j.ge.1).and.(j.le.np).and.(i.ge.top).
     +      and.(i.le.bot)) buf(j,i-top+1)=dnout
6     continue

c draw long vertical vectors
      if(abs(dy).ge.abs(dx))then
        if(abs(dy).lt.1.0) return
        if(y1.gt.y3)then
          k=-1
        else
          k=1
        endif
        ratio=k*dx/dy
        x=x1-ratio
        do 10 l=nint(y1),nint(y3),k
          x=x+ratio
          j=nint(x)
          if((l.lt.top).or.(l.gt.bot)) goto 10
          if((j.lt.1).or.(j.gt.np)) goto 10
          buf(j,l-top+1)=dnout
10      continue
      else

c draw long horizontal vectors
        if(abs(dx).lt.1.0) return
        if(x1.gt.x3)then
          k=-1
        else
          k=1
        endif
        ratio=k*dy/dx
        y=y1-ratio
        do 20 j=nint(x1),nint(x3),k
          y=y+ratio
          l=nint(y)
          if((j.lt.1).or.(j.gt.np)) goto 20
          if((l.lt.top).or.(l.gt.bot)) goto 20
          buf(j,l-top+1)=dnout
20      continue
      endif
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dvector.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM dvector

   To Create the build file give the command:

		$ vimake dvector			(VMS)
   or
		% vimake dvector			(Unix)
************************************************************************/
#define PROGRAM	dvector
#define R2LIB
#define MODULE_LIST dvector.f
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
$ create dvector.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=(2:101)
PARM OUT          TYPE=STRING       COUNT=1
PARM EXAG TYPE=REAL DEFAULT=1.0
PARM DN   TYPE=INTEGER DEFAULT=255
END-PROC

.TITLE
VICAR2 program DVECTOR

.HELP
    dvector can draw any number of vectors on a picture of any size.
Vectors are drawn from the left tiepoint location towards the right
tiepoint location. The left coordinate will have a 3 by 3 pixel cross
at that location. The right coordinate will be at the vector tip IF
EXAG is defaulted to 1.0. Otherwise the vector will fall short or
cross over the right point.
    Vectors come from the second (and beyond) input files in mark
format. They are essentially tiepoints in the order:
left_line, left_sample, right_line, right_sample.
    Internal image storage is as integer*2 so real images will
be rounded.
    Dvector uses a storage buffer of 500000 integers. It will process
all the vectors for each image buffer, making as many passes as
required to process the entire input taken 500000 pixels at a time.
    Up to 100 mark files can be input to DVECTOR. An X will be drawn
at the base of the vector for only the first mark files vectors.
All mark files must be identical in size and number of tiepoints.

If DN is negative:
1. Vectors are drawn on an image of zero dn.
2. Vectors are written from 1 to 255 dn representing angles from 0
   to 360 degrees.
3. No X is drawn at the vector base.


.page
Example:
       dvector in,points out parameters
  where: IN is an input image to superimpose vectors upon.
         POINTS is a MARK file containing the vectors to draw.
          Mark files contain 512 byte records of real*4 data.
          Coordinates are stored in groups of 4 words in the
          order left_line,left_sample,right_line,right_sample...
         OUT is a copy of IN with superimposed vectors.

EXAMPLE:
  (one mark file)
      tracker a,c t
      dvector a,t b
  (two mark files)
      tracker2 c,d,t tt
      dvector a,t,tt b

HISTORY

Written By: J Lorre			    1 AUGUST 1978
Cognizant Programmer: J Lorre
REVISIONS:
03 Oct 94 CRI Made portable for UNIX ... Jim Turner (CRI)
29 Aug 01 GMY Fixed bug to allow use of all 500,000 bytes of buffer allocated.

.LEVEL1
.VARI INP
Two inputs minimum.
#1 is the left image
#2 is the MARK file.
#3-#101 optional

.VARI OUT
Image with superimposed
vectors.

.VARIABLE EXAG
Exag is a real number 
which exaggerates the 
vector length.
Default is exag=1.0

.VARIABLE DN
DN is an integer which 
sets the DN intensity 
value of the
vectors. 
Default is n=255
(see special case if
dn is negative)

.LEVEL2

.VARI INP
There are two input files minimum.
File#1: This is the left or first input image given to
        program TRACKER. Vectors will be grawn upon this
        image with the base at the correct location.
File#2: This is the MARK file written by program TRACKER.
        It is 'REAL' format with 512 byte records containing
        pairs of tiepoints in the order:
        left_line,left_samp,right_line,right_samp.
Files 3 to 101 are optional mark files forming a chain vector end to
        end.  
NOTE: all mark files must be the same size and length.

.VARI OUT
This is the output image. It is a copy of input #1 (in that 
format) with superimposed vectors contained in the MARK file.
Vectors are drawn from left_line,left_samp  to
right_line,right_samp.

.VARIABLE EXAG
Exag is a real number which exaggerates the vector length.
Default is exag=1.0

.VARIABLE DN
DN is an integer which sets the DN intensity value of the
vectors. Default is n=255
If DN is negative then:
1. Vectors are drawn on an image of zero dn.
2. Vectors are written from 1 to 255 dn representing angles from 0
   to 360 degrees.
3. No X is drawn at the vector base.

$ Return
$!#############################################################################
$Test_File:
$ create tstdvector.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! TEST SCRIPT FOR THE PROGRAM DVECTOR
gausnois out=a.img nl=305 ns=305
copy a.img left.img nl=300 ns=300
copy a.img right.img sl=5 ss=3 nl=300 ns=300
copy a.img mid.img sl=3 ss=5 nl=300 ns=300
tracker3 inp=(left.img,right.img) out=mark.img +
  grid=100 nlw=11 nsw=11 'print limit=8
tracker3 inp=(right.img,mid.img,mark.img) out=mark2.img +
  grid=100 nlw=11 nsw=11 'print limit=8
list left.img (13,13,9,9)
list right.img (13,13,9,9)
dvector inp=(left.img,mark.img,mark2.img) out=vector.img
list vector.img (13,13,9,9)
end-proc
$ Return
$!#############################################################################
