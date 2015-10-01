$!****************************************************************************
$!
$! Build proc for MIPL module csys
$! VPACK Version 1.9, Wednesday, January 10, 2001, 11:27:00
$!
$! Execute by entering:		$ @csys
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
$ write sys$output "*** module csys ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to csys.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("csys.imake") .nes. ""
$   then
$      vimake csys
$      purge csys.bld
$   else
$      if F$SEARCH("csys.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake csys
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @csys.bld "STD"
$   else
$      @csys.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create csys.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack csys.com -mixed -
	-s csys.f -
	-p csys.pdf -
	-i csys.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create csys.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c   program CSYS  --  Change SYStems 

c  convert RA,Dec,Twist from B1950 to J2000 or vice versa

	implicit real*8 (a-h,o-z)
	real*8 cmat(3,3), cmat1(3,3)
	real*8 to1950(3,3)/
     1    0.9999256794956877, -0.0111814832204662, -0.0048590038153592,
     2    0.0111814832391717,  0.9999374848933135, -0.0000271625947142,
     3    0.0048590037723143, -0.0000271702937440,  0.9999881946023742/

	degrad = 180.d0/pi()	! degrees per radian

	print*,' Enter 1 to go to J2000, 2 to go to B1950:'
	read(5,*) mode
	if (mode.lt.1 .or. mode.gt.2) call exit

c  get the pointing:
1	print*,' enter RA (degrees) ... or -999 to quit:'
	read(5,*) ra
	if (ra.lt.-900.) call exit
	print*,' enter Dec (degrees):'
	read(5,*) dec
	print*,' enter Twist (degrees):'
	read(5,*) twt

	ra = ra/degrad		! convert to radians
	dec = dec/degrad
	twt = twt/degrad

	cdec = halfpi()-dec
	call eul2m( twt, cdec, ra, 3, 2, 3, cmat)	! make C-matrix

c  convert C-matrix by column vectors:
	do i=1,3
	  do j=1,3
	    cmat1(i,j) = 0.0
	    do k=1,3
	      if (mode.eq.1) then
		cmat1(i,j) = cmat1(i,j) + cmat(i,k)*to1950(k,j)
	      else
		cmat1(i,j) = cmat1(i,j) + cmat(i,k)*to1950(j,k)
	      endif
	    enddo
	  enddo
	enddo

		! convert back to angles
	call m2eul( cmat1, 3, 2, 3, twt, cdec, ra)
	dec = halfpi()-cdec
	if (ra.lt.0.) ra = ra+2.0*pi()
	if (twt.lt.0.) twt = twt+2.0*pi()

		! and list:
	write(6,1001) ra*degrad, dec*degrad, twt*degrad

	go to 1

1001	format(' RA =', f8.3,'  Dec =', f8.3,'  Twt =', f8.3)

	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create csys.pdf
process help=*
end-proc
.TITLE
VICAR program CTIM
.HELP
Program to convert Euler angles (Right Ascension, Declination, and
Twist) from B1950 to J2000, or vice versa.

NOTE: this is not a VICAR program and must be run from the system level.
It is interactive and should be self-explanatory.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create csys.imake
#define PROGRAM csys
#define MODULE_LIST csys.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
/*#define LIB_LOCAL		/* remove on delivery */
#define LIB_P2SUB
#define LIB_RTL
#define LIB_SPICE
#define LIB_FORTRAN
$ Return
$!#############################################################################
