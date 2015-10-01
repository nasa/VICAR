$!****************************************************************************
$!
$! Build proc for MIPL module markibis
$! VPACK Version 1.8, Tuesday, November 07, 1995, 14:18:30
$!
$! Execute by entering:		$ @markibis
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
$ write sys$output "*** module markibis ***"
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
$ write sys$output "Invalid argument given to markibis.com file -- ", primary
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
$   if F$SEARCH("markibis.imake") .nes. ""
$   then
$      vimake markibis
$      purge markibis.bld
$   else
$      if F$SEARCH("markibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake markibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @markibis.bld "STD"
$   else
$      @markibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create markibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack markibis.com -
	-s markibis.f -
	-i markibis.imake -
	-p markibis.pdf -
	-t tstmarkibis.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create markibis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program markibis
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (ntable=200000)
c ntable = max # tiepoints
      integer*4 status,inmark,outmark,inibis,outibis
      real*4 buf(4,ntable)
      character*80 msg
      logical flag

c determine the type of the input file
      call xvunit(i,'INP',1,status,' ')
      call ibis_file_open(i,j,'READ',0,0,' ',' ',status)
      if(status.eq.1)then
        mode=1 ! ibis input
      else
        mode=2 ! mark input
      endif
      call ibis_file_close(j,' ',status)
      if(mode.eq.1) goto 200

c*********************************************************************
c load the tiepoint table from the mark file.
      call xvunit(inmark,'INP',1,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl,'NS',ns,' ')
      npts=nl*ns/4
      if(npts.gt.ntable)then
        call xvmessage('Too many input points',' ')
        call abend
      endif
      k=1
      do i=1,nl
         call xvread(inmark,buf(1,k),status,'LINE',i,' ')
         k=k+ns/4
      enddo
      write(msg,*)nl,' mark tiepoint records read'
      call xvmessage(msg,' ')

c remove zeroes 
      j=0
10    j=j+1
11      flag=.false.
        if(buf(1,j).eq.0.0) flag=.true.  
        if(buf(2,j).eq.0.0) flag=.true.  
        if(buf(3,j).eq.0.0) flag=.true.  
        if(buf(4,j).eq.0.0) flag=.true.  
        if(flag)then
          if(j.eq.npts)then ! last point is bad
            npts=npts-1
            goto 12
          endif
          do i=1,npts-j     ! move everything up one
            buf(1,j+i-1)=buf(1,j+i)
            buf(2,j+i-1)=buf(2,j+i)
            buf(3,j+i-1)=buf(3,j+i)
            buf(4,j+i-1)=buf(4,j+i)
          enddo
          npts=npts-1
          goto 11
        endif
      if(j.lt.npts) goto 10
12    continue
      write(msg,*)npts,' good tiepoints located'
      call xvmessage(msg,' ')

c switch the tiepoint locations so IN is columns 3,4 and OUT is 1,2
c which is the ibis tiepoint file convention.
      do i=1,npts
        tl=buf(1,i)
        ts=buf(2,i)
        buf(1,i)=buf(3,i)
        buf(2,i)=buf(4,i)
        buf(3,i)=tl
        buf(4,i)=ts
      enddo

c write the ibis file
      call xvunit(outibis,'OUT',1,status,' ')
      call iwrite_tiepoints(outibis,0,0,npts,buf,4)
      call xvmessage('Ibis file written',' ')
      return

c**********************************************************************
c read the ibis file
200   continue
      call xvunit(inibis,'INP',1,status,' ')
      i=0
      call iread_tiepoints(inibis,i,npts,ntable,buf,4)
      write(msg,*)npts,' tiepoints located'
      call xvmessage(msg,' ')

c fill the end of the buffer with zeroes
      nl=npts/32
      if(nl*32.lt.npts) nl=nl+1
      if(nl*32.gt.npts)then
        do i=npts+1,nl*32
          buf(1,i)=0.0      
          buf(2,i)=0.0      
          buf(3,i)=0.0      
          buf(4,i)=0.0      
        enddo
      endif

c switch the tiepoint locations so IN is columns 1,2 and OUT is 3,4
c which is the mark convention.
      do i=1,npts
        tl=buf(1,i)
        ts=buf(2,i)
        buf(1,i)=buf(3,i)
        buf(2,i)=buf(4,i)
        buf(3,i)=tl
        buf(4,i)=ts
      enddo

c write the mark file.
      call xvunit(outmark,'OUT',1,status,' ')
      call xvopen(outmark,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'U_NL',nl,'U_NS',128,'U_NB',1,
     +            'OP','WRITE','O_FORMAT','REAL','OPEN_ACT','AS',' ')
      do i=1,nl
         call xvwrit(outmark,buf(1,(i-1)*32+1),status,'LINE',i,' ')
      enddo
      call xvclose(outmark,status,'CLOS_ACT','FREE',' ')
      call xvmessage('Mark file written',' ')

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create markibis.imake
#define  PROGRAM   markibis

#define MODULE_LIST markibis.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create markibis.pdf
process help=*
PARM INP	TYPE = STRING   COUNT=1
PARM OUT	TYPE = STRING   COUNT=1

!# annot function="Vicar Data Conversion"
!# annot keywords=(Mark,IBIS,tiepoint,file,format)
END-PROC

.TITLE
Converts tiepoints from Mark to IBIS format or vice-versa

.HELP
PURPOSE:
Converts between tiepoint file formats.
If the input is an ibis file it converts it to a mark file.
If the input is a mark file it converts it to an ibis file.

EXECUTION:

markibis may be executed in the following manner:

                TRACKER3 INP=(LEFT,RIGHT) OUT=A
		MARKIBIS INP=A OUT=B

.PAGE
OPERATION:
If the mark file is ordered as:
left_line,left_samp,right_line_right_samp  ( as produced by tracker3 )
then the ibis file is ordered as:
column 1= right_line                       ( as required for geoming from
column 2= right_samp                         left image to right image )
column 3= left_line
column 4= left_samp

.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: J Lorre 11/1/95
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
STRING-input dataset.
.VARI OUT
STRING-output dataset.

.LEVEL2
.VARI INP
STRING-input dataset.
.VARI OUT
STRING-output dataset.
$ Return
$!#############################################################################
$Test_File:
$ create tstmarkibis.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gausnois out=a.img nl=220 ns=220
copy inp=a.img out=b.img nl=200 ns=200
copy inp=a.img out=c.img sl=5 ss=5 nl=200 ns=200
tracker3 inp=(b.img,c.img) +
 out=mark.img grid=25 nlw=21 nsw=21
markibis inp=mark.img out=ibis.img
markibis inp=ibis.img out=mark2.img
difpic (mark.img,mark2.img)
ibis-list ibis.img
end-proc
$ Return
$!#############################################################################
