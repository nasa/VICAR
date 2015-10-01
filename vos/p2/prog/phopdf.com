$!****************************************************************************
$!
$! Build proc for MIPL module phopdf
$! VPACK Version 1.7, Thursday, March 02, 1995, 10:43:01
$!
$! Execute by entering:		$ @phopdf
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module phopdf ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to phopdf.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("phopdf.imake") .nes. ""
$   then
$      vimake phopdf
$      purge phopdf.bld
$   else
$      if F$SEARCH("phopdf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake phopdf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @phopdf.bld "STD"
$   else
$      @phopdf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create phopdf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack phopdf.com -
	-p pho_global.pdf pho_none.pdf pho_par_file.pdf pho_lambert.pdf -
	   pho_minnaert.pdf pho_irvine.pdf pho_veverka.pdf pho_buratti1.pdf -
	   pho_buratti2.pdf pho_buratti3.pdf pho_mosher.pdf -
	   pho_lumme_bowel_hg1.pdf pho_hapke_81_le2.pdf pho_hapke_81_cook.pdf -
	   pho_hapke_86_hg1.pdf pho_hapke_86_hg2.pdf pho_hapke_86_le2.pdf -
	   pho_hapke_hg1_dom.pdf pho_regner_hapke_hg1.pdf -
	   pho_atmo_corr_regner.pdf phopdf.pdf -
	-i phopdf.imake -
	-t tphopdf.c tphopdf.imake tphopdf.pdf tphopdf.mdf tphopdf_general.pdf -
	   tphopdf_spice.pdf tstphopdf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create pho_global.pdf
globals

!-----------------------------------------------------------------------------
! PHO_GLOBAL.PDF
!
! This is a Globals PDF which defines global variables 
! for photometric application programs.
!-----------------------------------------------------------------------------

	parm PHO_FUNC_type type=string count=1 valid = ( +
			NONE,				 +
			PAR_FILE,			 +
			LAMBERT,			 +
			MINNAERT, 			 +
			IRVINE,				 +
			VEVERKA, 			 +
			BURATTI1,			 +
			BURATTI2,			 +
			BURATTI3,			 +
			MOSHER,				 +
			LUMME_BOWEL_HG1,		 +
			HAPKE_81_LE2,			 +
			HAPKE_81_COOK,			 +
			HAPKE_86_HG1,			 +
			HAPKE_86_HG2,			 +
			HAPKE_86_LE2,			 +
			HAPKE_HG1_DOM,			 +
			REGNER_HAPKE_HG1,		 +
			ATMO_CORR_REGNER		 +
						)	 +
						 default=Lambert

	parm pho_PROC_NAME type=string count=0:1 default=--

end-proc
$!-----------------------------------------------------------------------------
$ create pho_none.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! PHO_NONE.PDF
!
! This is the PDF for the menu point of the MDF which defines the photometric 
! function, if "PHO_FUNC" has been set to 'NONE' in the first point of the 
! menu (*_GENERAL.PDF):
! In this PDF, the option to specify no photometric function is covered,
! i.e. no one is asked for nothing.
!
!-----------------------------------------------------------------------------

	procedure name=none_sub help=*
	body
	end-proc

body

	if (_tutor=1)
	   tutor none_sub 
	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for no function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.Title
Sub-menu PHO_NONE (for VICAR photometry programs )

.help
If you don't like a photometric function --> you don't have to specify 
					     function parameters

Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.end
$!-----------------------------------------------------------------------------
$ create pho_par_file.pdf
procedure option=selftutor help=*

  refgbl pho_PROC_NAME
!-----------------------------------------------------------------------------
! PHO_PAR_FILE.PDF
!
! This is the PDF for the point of the MDF which defines the photometric 
! function, if "PHO_FUNC" has been set to 'PAR_FILE' in the first point of the 
! menu "pho_PROC_NAME"_GENERAL.PDF):
! In this PDF, the user is asked for the name of a file containing photo-
! metric parameters.
!
!-----------------------------------------------------------------------------

        parm PHO_PAR_FILE	string count=0:1 def=--

	PARMSET name=pho_par_file_sub help=*

	  parm PHO_PAR_FILE	string count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	   tutor pho_par_file_sub 				+
		|restore=pho_par_file.par, save=pho_par_file.par|

!	   repeat the tutur one time if the filename was not given :

	   restore-parm pho_par_file.par

	   if ( $count( PHO_PAR_FILE ) = 0 )				+
	   	tutor pho_par_file_sub 					+
			|restore=pho_par_file.par, save=pho_par_file.par|

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " a photometric parameter IBIS2-file ."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
PHO_PAR_FILE sub menu (for input of photometric parameter file name as input)

.help

 PHO_PAR_FILE.PDF

 This is the PDF for the point of the MDF which defines the photometric 
 function, if "PHO_FUNC" has been set to 'PAR_FILE' in the first point of the 
 menu ("pho_PROC_NAME"_GENERAL.PDF):
 In this PDF, the user is asked for the name of a IBIS2-file containing photo-
 metric parameters (see pho_par_file-package).

Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.vari PHO_PAR_FILE
photometric parameter file

.level2

.vari PHO_PAR_FILE
Name of the photometric parameter file containing parameters for some 
photometric functions. This IBIS-file is of the type "phopar" (see the 
pho_par_file-subroutine-package).

.end

$!-----------------------------------------------------------------------------
$ create pho_lambert.pdf
procedure option=selftutor help=*

        parm ALBEDO_temp	real count=0:1 			def=1.0
        parm ALBEDO		real count=0:1 valid=(0:1)     	def=--

	PARMSET name=pho_lambert_sub help=*

             parm ALBEDO	real count=0:1 valid=(0:1)     	def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_lambert.par

	  if ($count(ALBEDO)=0)
	 						let ALBEDO_temp=1.0
	  else
		let ALBEDO_temp=&ALBEDO
	  end-if

	  tutor pho_lambert_sub 				+
		|restore=pho_lambert.par, save=pho_lambert.par| +
			ALBEDO=&ALBEDO_temp

	  return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the Lambert function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_LAMBERT' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

This is the sub-menu associated with the Lambert photometric
function. This function needs just one input parameter (ALBEDO).

.page

MATHEMATICAL BACKGROUND :

Lambert's law does provide a reasonably good descrition of the reflectance of 
high-albedo surfaces, like snow, but the approximation is poor for dark 
surfaces.

The Lambert's law is based on the empirical observation that the brightnessses 
of many surfaces are nearly independent of the emmission angle and azimuthal 
angle and on the fact that the brightness of any surface must be proportional 
to cos(incidence angle).

bidirectional reflectance [1/str] :

r(i,e,g)=A*cos(i)


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.var ALBEDO
Surface albedo

.level2

.var ALBEDO
This parameter gives the albedo of the surface. 

.end

$!-----------------------------------------------------------------------------
$ create pho_minnaert.pdf
procedure option=selftutor help=*

    	parm ALBEDO_temp	 real count=0:1 		def=1.0 
 	parm EXPONENT_temp	 real count=0:1 		def=0.6

    	parm ALBEDO	 	 real count=0:1 def=-- 
 	parm EXPONENT	 	 real count=0:1 def=--

	PARMSET name=pho_minnaert_sub help=*

    	   parm ALBEDO		 real count=0:1 def=-- 
 	   parm EXPONENT	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	   restore-parm pho_minnaert.par


	   if ($count(ALBEDO)=0)
	 						let ALBEDO_temp=1.0
	   else
		let ALBEDO_temp=&ALBEDO
	   end-if


	   if ($count(EXPONENT)=0)
	 						let EXPONENT_temp=0.6
	   else
		let EXPONENT_temp=&EXPONENT
	   end-if


	   tutor pho_minnaert_sub 					+
		|restore=pho_minnaert.par, save=pho_minnaert.par|	+
			ALBEDO=&ALBEDO_temp				+
			EXPONENT=&EXPONENT_temp
	
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the MINNAERT function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc
.title
'PHO_MINNAERT' sub-menu (for photometry VICAR application programs)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Minnaerts's photometric function. This function needs just two input parameters 
(Albedo, geometric exponent k).

.page
MATHEMATICAL BACKGROUND :

It is found empirically that Minnaert's law approximately describes the 
variation of brightness of many surfaces over a limited range of angles.
But the general law breaks down completely at the limb of a planet.

Minnaert (1941) suggested generalizing Lambert's law so that the power emitted 
per unit solid angle per unit area of the surface be proportional 
(cos(i)*cos(e))**k [ Lambert's power is proportional to (cos(i)*cos(e))**1 ].

bidirectional reflectance [1/str] :

r(i,e,g)=ALBEDO*(cos(i)*cos(e))**EXPONENT/cos(e)

REFERENCE :
M. Minnaert, The reciprocity principle in Lunar photometry,
Astrophysical Journal, Vol. 93, No. 2, p. 403-410, 1941
.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.var ALBEDO
Surface albedo

.var EXPONENT
Minnaert exponent


.level2

.var ALBEDO
This parameter gives the albedo of the surface. 

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.end
$!-----------------------------------------------------------------------------
$ create pho_irvine.pdf
procedure option=selftutor help=*

        parm EXPONENT_temp	 real count=0:1 		def=0.9
	parm IRV_EXP1_temp	 real count=0:1 		def=0.118
	parm IRV_EXP2_temp	 real count=0:1 		def=0.0039

        parm EXPONENT		 real count=0:1 def=--
	parm IRV_EXP1		 real count=0:1 def=--
	parm IRV_EXP2		 real count=0:1 def=--

	PARMSET name=pho_irvine_sub help=*

             parm EXPONENT	real count=0:1	def=--
	     parm IRV_EXP1	real count=0:1	def=--
	     parm IRV_EXP2	real count=0:1 def=--

	end-proc

body

	if (_tutor=1)

	  restore-parm pho_irvine.par

	  if ($count(EXPONENT)=0)
	 					let EXPONENT_temp=0.9
	  else
	 	let EXPONENT_temp=&EXPONENT
	  end-if

	  if ($count(IRV_EXP1)=0)
	 					let IRV_EXP1_temp=0.118
	  else
	 	let IRV_EXP1_temp=&IRV_EXP1
	  end-if


	  if ($count(IRV_EXP2)=0)
	 					let IRV_EXP2_temp=0.0039
	  else
	 	let IRV_EXP2_temp=&IRV_EXP2
	  end-if

	  tutor pho_irvine_sub 					+
		|restore=pho_irvine.par, save=pho_irvine.par|	+
			EXPONENT=&EXPONENT_temp			+
			IRV_EXP1=&IRV_EXP1_temp			+
			IRV_EXP2=&IRV_EXP2_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the Irvine function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_IRVINE' sub-menu (for a photometry VICAR application program)

.help
PURPOSE:

This is the sub-menu associated with Irvine's photometric
function. This function needs three input parameters:
 EXPONENT, IRV_EXP1, IRV_EXP2.

.page



MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g)=( cos(i)*cos(e) )**EXPONENT) / cos(e) * (1-exp(-cos(i)*IRV_EXP1))/(1-exp(-cos(e)*IRV_EXP2))


REFERENCES :
old VICAR Photometry programs

.page
PROGRAMMER:

.page
Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.vari EXPONENT
Minnaert exponent

.vari IRV_EXP1
Irvine's first exponent

.vari IRV_EXP2
Irvine's second exponent


.level2

.vari EXPONENT
Exponent - the geometrical constant k of the Minnaert part of the photometric 
function.

.vari IRV_EXP1
Irvine's first exponent - parameter of the Irvine photometric function.

.vari IRV_EXP2
Irvine's second exponent - parameter of the Irvine photometric function.

.end

$!-----------------------------------------------------------------------------
$ create pho_veverka.pdf
procedure option=selftutor help=*

	parm A_VEVERKA_temp	 		real count=0:1 def=0.997
	parm B_VEVERKA_temp	 		real count=0:1 def=0.6 
	parm C_VEVERKA_temp			real count=0:1 def=0.003 
	parm D_VEVERKA_temp	 		real count=0:1 def=0.14 

	parm A_VEVERKA	 real count=0:1
	parm B_VEVERKA	 real count=0:1
	parm C_VEVERKA	 real count=0:1 
	parm D_VEVERKA	 real count=0:1 

	procedure name=pho_veverka_sub help=*

	    parm A_VEVERKA	 real count=0:1 def=--
	    parm B_VEVERKA	 real count=0:1 def=--
	    parm C_VEVERKA	 real count=0:1 def=--
	    parm D_VEVERKA	 real count=0:1 def=-- 

	body
	end-proc

body

	if (_tutor=1)

	  restore-parm pho_veverka.par

	  if ($count(A_VEVERKA)=0)
	 					let A_VEVERKA_temp=0.997
	  else 
		let A_VEVERKA_temp=&A_VEVERKA
	  end-if


	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKA_temp=0.6
	  else
		let B_VEVERKA_temp=&B_VEVERKA
	  end-if


	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKA_temp=0.003
	  else
		let C_VEVERKA_temp=&C_VEVERKA
	  end-if


	  if ($count(D_VEVERKA)=0)
						let D_VEVERKA_temp=0.14
	  else
		let D_VEVERKA_temp=&D_VEVERKA
	  end-if



	   tutor pho_veverka_sub 					+
		|restore=pho_veverka.par, save=pho_veverka.par|		+
			A_VEVERKA=&A_VEVERKA_temp			+
			B_VEVERKA=&B_VEVERKA_temp			+
			C_VEVERKA=&C_VEVERKA_temp			+
			D_VEVERKA=&D_VEVERKA_temp


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the VEVERKA function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_VEVERKA' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the Veverka's
photometric function.

.page
MATHEMATICAL BACKGROUND :

Exept close to zero phase, this expression is a good description of light 
scattered by low-albedo bodies of the solar system, such as the Moon and 
Mercury, for which only light that has been scattered once contributes 
significantly to the brightnes.

bidirectional reflectance [1/str] :

r(i,e,g) = ( cos(i) / (cos(e)+cos(e)) ) 
	 * exp(-DVEVERKA * phase) 
	 * (A_VEVERKA + B_VEVERKA * phase + C_VEVERKA)

usually :
C_VEVERKA=1-A_VEVERKA


.page
REFERENCE :
Joseph Veverka, J. Goguen, S. Young, J. Elliont, Scattering of light from 
particulate surfaces. 
I. A laboratory assessment of multiple-scattering effects.
Icarus, Vol. 34, p. 406-414


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.level2

.VARI A_VEVERKA 
Parameter of the Veverka photometric function.

.VARI B_VEVERKA
Parameter of the Veverka photometric function.

.VARI C_VEVERKA
Parameter of the Veverka photometric function.

.VARI D_VEVERKA
Parameter of the Veverka photometric function.

.end
$!-----------------------------------------------------------------------------
$ create pho_buratti1.pdf
procedure option=selftutor help=*

	parm ALBEDO_temp	 real count=0:1 			def=0.5 
	parm B_VEVERKA_temp	 real count=0:1 			def=0.6
	parm E_BURATTI_temp	 real count=0:1 			def=0.14

	parm ALBEDO	 real count=0:1 	def=-- 
	parm B_VEVERKA	 real count=0:1 	def=--
	parm E_BURATTI	 real count=0:1 	def=--

	PARMSET name=pho_buratti1_sub help=*

	    parm ALBEDO		 real count=0:1 def=-- 
	    parm B_VEVERKA	 real count=0:1 def=--
	    parm E_BURATTI	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti1.par


	  if ($count(ALBEDO)=0)
	 						let ALBEDO_temp=0.5
	  else
	 	let ALBEDO_temp=&ALBEDO
	  end-if


	  if ($count(B_VEVERKA)=0)
	 						let B_VEVERKA_temp=0.6
	  else
	 	let B_VEVERKA=&B_VEVERKA
	  end-if


	  if ($count(E_BURATTI)=0)
	 						let E_BURATTI_temp=0.14
	  else
	 	let ALBEDO_temp=&E_BURATTI
	  end-if




	   tutor pho_buratti1_sub      					+
		|restore=pho_buratti1.par, save=pho_buratti1.par|	+
		ALBEDO=&ALBEDO_temp	       		       		+
		B_VEVERKA=&B_VEVERKA_temp      				+
		E_BURATTI=&E_BURATTI_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_BURATTI1' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for Buratti's
photometric function.

.page
MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) * cos(phase))/PI)

BURATTI1 approximation :
phi = 1 + B * phase

REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA 
Veverka parameter

.VARI E_BURATTI
Buratti's parameter


.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI B_VEVERKA 
Parameter of the Veverka and Mosher photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.end
$!-----------------------------------------------------------------------------
$ create pho_buratti2.pdf
procedure option=selftutor help=*

	 parm ALBEDO_temp	 real count=0:1 		def=0.5 
	 parm B_VEVERKA_temp	 real count=0:1 		def=0.6
	 parm C_VEVERKA_temp	 real count=0:1 		def=-0.003
	 parm E_BURATTI_temp	 real count=0:1			def=0.14

	 parm ALBEDO	 real count=0:1 	def=-- 
	 parm B_VEVERKA	 real count=0:1 	def=--
	 parm C_VEVERKA	 real count=0:1 	def=--
	 parm E_BURATTI	 real count=0:1 	def=--

	PARMSET name=pho_buratti2_sub help=*

	    parm ALBEDO		 real count=0:1 def=-- 
	    parm B_VEVERKA	 real count=0:1 def=--
	    parm C_VEVERKA	 real count=0:1 def=--
	    parm E_BURATTI	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti2.par


	  if ($count(ALBEDO)=0)
	 					let ALBEDO_temp=0.5
	  else
	 	let ALBEDO_temp=&ALBEDO
	  end-if

	  if ($count(B_VEVERKA)=0)
	 					let B_VEVERKA_temp=0.6
	  else
	 	let B_VEVERKA_temp=&B_VEVERKA
	  end-if

	  if ($count(C_VEVERKA)=0)
	 					let C_VEVERKA_temp=-0.003
	  else
	 	let C_VEVERKA_temp=&C_VEVERKA
	  end-if

	  if ($count(E_BURATTI)=0)
	 					let E_BURATTI_temp=0.14
	  else
	 	let E_BURATTI_temp=&E_BURATTI
	  end-if


	 	
	   tutor pho_buratti2_sub      					+
		|restore=pho_buratti2.par, save=pho_buratti2.par|	+
			ALBEDO=&ALBEDO_temp				+
			B_VEVERKA=&B_VEVERKA_temp			+
			C_VEVERKA=&C_VEVERKA_temp			+
			E_BURATTI=&E_BURATTI_temp

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_BURATTI2' sub-menu' (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the Buratti's
photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) 
* cos(phase))/PI)

BURATTI2 approximation : 
phi = 1 + phase * (B + phase * C)

REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983


.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI E_BURATTI
Buratti's parameter


.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.end
$!-----------------------------------------------------------------------------
$ create pho_buratti3.pdf
procedure option=selftutor help=*

	parm ALBEDO_temp	 real count=0:1 		def=0.5 
	parm B_VEVERKA_temp	 real count=0:1 		def=0.6
	parm C_VEVERKA_temp	 real count=0:1 		def=-0.003
	parm D_VEVERKA_temp	 real count=0:1 		def=0.14
	parm E_BURATTI_temp	 real count=0:1 		def=0.14

	parm ALBEDO	 real count=0:1 	def=-- 
	parm B_VEVERKA	 real count=0:1 	def=--
	parm C_VEVERKA	 real count=0:1 	def=--
	parm D_VEVERKA	 real count=0:1 	def=--
	parm E_BURATTI	 real count=0:1 	def=--

	PARMSET name=pho_buratti3_sub help=*

	    parm ALBEDO		 real count=0:1 		def=-- 
	    parm B_VEVERKA	 real count=0:1 		def=--
	    parm C_VEVERKA	 real count=0:1 		def=--
	    parm D_VEVERKA	 real count=0:1 		def=--
	    parm E_BURATTI	 real count=0:1 		def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_buratti3.par


	  if ($count(ALBEDO)=0)
	 						let ALBEDO_temp=0.5
	  else
	 	let ALBEDO_temp=&ALBEDO
	  end-if

	  if ($count(B_VEVERKA)=0)
	 						let B_VEVERKA_temp=0.6
	  else
	 	let B_VEVERKA_temp=&B_VEVERKA
	  end-if

	  if ($count(C_VEVERKA)=0)
	 					     let C_VEVERKA_temp=-0.003
	  else
	 	let C_VEVERKA_temp=&C_VEVERKA
	  end-if

	  if ($count(D_VEVERKA)=0)
	 						let D_VEVERKA_temp=0.14
	  else
	 	let D_VEVERKA_temp=&D_VEVERKA
	  end-if

	  if ($count(E_BURATTI)=0)
	 						let E_BURATTI_temp=0.14
	  else
	 	let E_BURATTI_temp=&E_BURATTI
	  end-if



	   tutor pho_buratti3_sub 					+
		|restore=pho_buratti3.par, save=pho_buratti3.par|	+
			ALBEDO=&ALBEDO_temp				+
			B_VEVERKA=&B_VEVERKA_temp			+
			C_VEVERKA=&C_VEVERKA_temp			+
			D_VEVERKA=&D_VEVERKA_temp			+
			E_BURATTI=&E_BURATTI_temp


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the BURATTI3 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_BURATTI3' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the Buratti's
photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = A * cos(i)/(cos(i)+cos(e)) * burf + (1 - A) * cos(i)

burf = ((1 - A) * 2/3 + A*E)*phi - (1 - A) * 2/3 * ((sin(phase) + (PI - phase) 
* cos(phase))/PI)

BURATTI3 approximation : 
phi=1.0 + params[1] * phase + params[2] * (exp(-params[3] * phase) - 1.0)


REFERENCE :
Bonnie Buratti, Voyager Disk Resolved Photometry of the Saturnian Satellites,
	Icarus, Vol. 59, p. 392-405, 
Bonnie Buratti, Joseph Veverka, Voyager Photometry of Europa,
	Icarus, Vol. 55, p. 93-110, 1983

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.VARI ALBEDO
isotropic/unisotropic 
reflectance weight 

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI E_BURATTI
Buratti's parameter


.level2

.VARI ALBEDO
Weight of unisotropic and isotropic reflectance.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.end
$!-----------------------------------------------------------------------------
$ create pho_mosher.pdf
procedure option=selftutor help=*

	parm A_VEVERKA_temp	 real count=0:1 		def=0.5
	parm B_VEVERKA_temp	 real count=0:1 		def=0.6
	parm C_VEVERKA_temp	 real count=0:1 		def=0.003
	parm D_VEVERKA_temp	 real count=0:1 		def=0.14
	parm MO_EXP1_temp	 real count=0:1 		def=0.5
	parm MO_EXP2_temp	 real count=0:1 		def=0.1

	parm A_VEVERKA	 real count=0:1 	def=--
	parm B_VEVERKA	 real count=0:1 	def=--
	parm C_VEVERKA	 real count=0:1 	def=--
	parm D_VEVERKA	 real count=0:1 	def=--
	parm MO_EXP1	 real count=0:1 	def=--
	parm MO_EXP2	 real count=0:1 	def=--

	PARMSET name=pho_mosher_sub help=*

	    parm A_VEVERKA	 real count=0:1 def=--
	    parm B_VEVERKA	 real count=0:1 def=--
	    parm C_VEVERKA	 real count=0:1 def=--
	    parm D_VEVERKA	 real count=0:1 def=--
	    parm MO_EXP1	 real count=0:1 def=--
	    parm MO_EXP2	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_mosher.par


	  if ($count(A_VEVERKA)=0)
	 						let A_VEVERKA_temp=0.5
	  else
	 	let A_VEVERKA_temp=&A_VEVERKA
	  end-if

	  if ($count(B_VEVERKA)=0)
	 						let B_VEVERKA_temp=0.6
	  else
	 	let B_VEVERKA_temp=&B_VEVERKA
	  end-if

	  if ($count(C_VEVERKA)=0)
	 						let C_VEVERKA_temp=0.003
	  else
	 	let C_VEVERKA_temp=&C_VEVERKA
	  end-if

	  if ($count(D_VEVERKA)=0)
	 						let D_VEVERKA_temp=0.14
	  else
	 	let D_VEVERKA_temp=&D_VEVERKA
	  end-if

	  if ($count(MO_EXP1)=0)
	 						let MO_EXP1_temp=0.5
	  else
	 	let MO_EXP1_temp=&MO_EXP1
	  end-if

	  if ($count(MO_EXP2)=0)
	 						let MO_EXP2_temp=0.1
	  else
	 	let MO_EXP2_temp=&MO_EXP2
	  end-if




	   tutor pho_mosher_sub 				+
		|restore=pho_mosher.par, save=pho_mosher.par|	+
			A_VEVERKA=&A_VEVERKA_temp		+
			B_VEVERKA=&B_VEVERKA_temp		+
			C_VEVERKA=&C_VEVERKA_temp		+
			D_VEVERKA=&D_VEVERKA_temp		+
			MO_EXP1=&MO_EXP1_temp                   +
			MO_EXP2=&MO_EXP2_temp     



	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the MOSHER function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_MOSHER' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the Mosher's
photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = (A + B * g + 
C * exp(- D * g)) * pow(cos(i) * cos(e), MO_EXP1 + MO_EXP2 * g) / cos(e)

with:
i 	incidense angle
e 	emission angle
g 	phase angle 
A 	A_VEVERKA
B 	B_VEVERKA
C 	C_VEVERKA
D 	D_VEVERKA	

REFERENCE :
old VICAR Photometry programs
!
.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.level2

.VARI A_VEVERKA 
Parameter of the Veverka photometric function.

.VARI B_VEVERKA
Parameter of the Veverka photometric function.

.VARI C_VEVERKA
Parameter of the Veverka photometric function.

.VARI D_VEVERKA
Parameter of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.end
$!-----------------------------------------------------------------------------
$ create pho_lumme_bowel_hg1.pdf
procedure option=selftutor help=*

	parm W_SOIL_temp 	 real count=0:1 		def=0.3
	parm H_SHOE_temp	 real count=0:1 		def=0.06
	parm DEN_SOIL_temp	 real count=0:1 		def=0.8
	parm THETA_temp	 	 real count=0:1 		def=20
	parm HG1_SOIL_temp	 real count=0:1 		def=-0.26

	parm W_SOIL 	 real count=0:1 def=--
	parm H_SHOE	 real count=0:1 def=--
	parm DEN_SOIL	 real count=0:1 def=--
	parm THETA	 real count=0:1 def=--
	parm HG1_SOIL	 real count=0:1 def=--

	PARMSET name=pho_lumme_bowel_hg1_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm DEN_SOIL	 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)


	  restore-parm pho_lumme_bowel_hg1.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if


	  if ($count(DEN_SOIL)=0)
	 						let DEN_SOIL_temp=0,8
	  else
		let DEN_SOIL_temp=&DEN_SOIL
	  end-if


	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.26
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if



	   tutor pho_lumme_bowel_hg1_sub 			+
			|restore=pho_lumme_bowel_hg1.par,	+
			    save=pho_lumme_bowel_hg1.par|	+
	    		W_SOIL=&W_SOIL_temp			+
	   		H_SHOE=&H_SHOE_temp			+ 
			DEN_SOIL=&DEN_SOIL_temp			+
	    		THETA=&THETA_temp			+ 
	    		HG1_SOIL=&HG1_SOIL_temp 
			

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the LUMME_BOWEL_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'LUMME_BOWEL_HG1' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Lumme-Bowell photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

 r(i,e,g)=...


REFERENCE :
Kari Lume, Edward Bowell, Radiative Transfer in Surfaces of 
	Atmosphereless Bodies
1. Theory,
	The Astronomical Jounal, Vol. 86, No. 11, p. 1694-1704, 1981
2. Interpretation of Phase curves,
	The Astronomical Jounal, Vol. 86, No. 11, p. 1705-1721, 1981
3. Interpretation of Lunar Photometry,
	The Astronomical Jounal, Vol. 87, No. 7, p. 1076-1082, 1982

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.level1

.var W_SOIL 
Single-scattering albedo

.var H_SHOE
width of opposition surge

.var DEN_SOIL
density of the soil

.var THETA
topographic slope angle

.var HG1_SOIL
Henyey-Greenstein term


.level2

.var W_SOIL 
Single-scattering albedo of the soil particles. It characterizes the efficiency 
of an average particle to scatter and absorb light. 

.var H_SHOE
Parameter which characterizes the soil structure (angular width of the 
opposition surge due to shadowing). 

.var DEN_SOIL
Specific volume density of the soil.

.var THETA
Average topographic slope angle of surface roughness at subresolution scale.

.var HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.


$!-----------------------------------------------------------------------------
$ create pho_hapke_81_le2.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	real count=0:1 			def=0.3
	    parm H_SHOE_temp	real count=0:1 			def=0.06
	    parm LE1_SOIL_temp	real count=0:1 			def=0.3
	    parm LE2_SOIL_temp	real count=0:1 			def=0.3

	    parm W_SOIL 	real count=0:1 def=--
	    parm H_SHOE		real count=0:1 def=--
	    parm LE1_SOIL	real count=0:1 def=--
	    parm LE2_SOIL	real count=0:1 def=--

	PARMSET name=pho_hapke_81_le2_sub help=*

	    parm W_SOIL 	real count=0:1 def=--
	    parm H_SHOE		real count=0:1 def=--
	    parm LE1_SOIL	real count=0:1 def=--
	    parm LE2_SOIL	real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_81_le2.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(LE1_SOIL)=0)
	 						let LE1_SOIL_temp=0.3
	  else
		let LE1_SOIL_temp=&LE1_SOIL
	  end-if

	  if ($count(LE2_SOIL)=0)
	 						let LE2_SOIL_temp=0.3
	  else
		let LE2_SOIL_temp=&LE2_SOIL
	  end-if

	   tutor pho_hapke_81_le2_sub  					  +
		|restore=pho_hapke_81_le2.par, save=pho_hapke_81_le2.par| +
			W_SOIL=&W_SOIL_temp				  +
			H_SHOE=&H_SHOE_temp				  +
			LE1_SOIL=&LE1_SOIL_temp				  +
			LE2_SOIL=&LE2_SOIL_temp	

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_81_LE2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPHE_81_LE2' sub-menu (for a photometry VICAR application program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1981-two-term-Legendre-Polynomial photometric function.

.page
MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...


REFERENCE :
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
1. Theory
   Journal of Geophysical Research Vol. 86, No.. B4, p. 3039-3054

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.end
$!-----------------------------------------------------------------------------
$ create pho_hapke_81_cook.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	 real count=0:1 		def=0.3
	    parm H_SHOE_temp	 real count=0:1 		def=0.06
	    parm LE1_SOIL_temp	 real count=0:1 		def=0.3
	    parm LE2_SOIL_temp	 real count=0:1 		def=0.3
	    parm COOK_temp	 real count=0:1 		def=0.9

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm LE1_SOIL	 real count=0:1 def=--
	    parm LE2_SOIL	 real count=0:1 def=--
	    parm COOK		 real count=0:1 def=--

	PARMSET name=pho_hapke_81_cook_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm LE1_SOIL	 real count=0:1 def=--
	    parm LE2_SOIL	 real count=0:1 def=--
	    parm COOK		 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)


	  restore-parm pho_hapke_81_cook.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(LE1_SOIL)=0)
	 						let LE1_SOIL_temp=0.3
	  else
		let LE1_SOIL_temp=&LE1_SOIL
	  end-if

	  if ($count(LE2_SOIL)=0)
	 						let LE2_SOIL_temp=0.3
	  else
		let LE2_SOIL_temp=&LE2_SOIL
	  end-if

	  if ($count(COOK)=0)
	 						let COOK_temp=0.9
	  else
		let COOK_temp=&COOK
	  end-if



	   tutor pho_hapke_81_cook_sub 					    +
		|restore=pho_hapke_81_cook.par, save=pho_hapke_81_cook.par| +
			W_SOIL=&W_SOIL_temp				    +
			H_SHOE=&H_SHOE_temp				    +
			LE1_SOIL=&LE1_SOIL_temp				    +
			LE2_SOIL=&LE2_SOIL_temp				    +
			COOK=&COOK_temp



	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_81_COOK function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE_81_COOK' sub-menu (for photometry VICAR application programs)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1981-two-term-Legendre-Polynomial bidirectional reflectance in the 
modification of Cook.

.page
MATHEMATICAL BACKGROUND :


The Cook bidirectional reflectance is a modification to the 1981 Hapke one. 
The modification is in the redefining incidence and emission angles to their 
new values :
	cos(i) <-- sqrt(1-COOK*COOK*(1-cos(i)*cos(i)))
	cos(e) <-- sqrt(1-COOK*COOK*(1-cos(e)*cos(e)))



REFERENCE :
Old VICAR Photometry programs

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.end
$!-----------------------------------------------------------------------------
$ create pho_hapke_86_hg1.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	real count=0:1 			def=0.3
	    parm H_SHOE_temp	real count=0:1 			def=0.06
	    parm B_SHOE_temp	real count=0:1 			def=2.0
	    parm THETA_temp	real count=0:1 			def=15.0
	    parm HG1_SOIL_temp	real count=0:1 			def=-0.26

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--

	PARMSET name=pho_hapke_86_hg1_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_hg1.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
		let B_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.26
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if

	   tutor pho_hapke_86_hg1_sub  					  +
		|restore=pho_hapke_86_hg1.par, save=pho_hapke_86_hg1.par| +
	    		W_SOIL=&W_SOIL_temp				  +
	   		H_SHOE=&H_SHOE_temp				  + 
	   		B_SHOE=&B_SHOE_temp				  + 
	    		THETA=&THETA_temp				  + 
	    		HG1_SOIL=&HG1_SOIL_temp 


	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write "  This program is only intended to be run "
	   write "  as tutor from other programs needs "
	   write "  photometric function parameters "
	   write "  for the HAPKE_86_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE-86_HG1' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1986-one-term-Henyey-Greenstein photometric function.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, particle-size distribution, and rate of compaction with depth (angular width of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge due to shadowing. It is the ratio of the light scattered from near the illuminated surface of the particle to the total amount of light scattered at zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 


.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.end
$!-----------------------------------------------------------------------------
$ create pho_hapke_86_hg2.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	  real count=0:1 		def=0.21
	    parm H_SHOE_temp	  real count=0:1 		def=0.07
	    parm B_SHOE_temp	  real count=0:1		def=2.0
	    parm THETA_temp	  real count=0:1 		def=20
	    parm HG1_SOIL_temp	  real count=0:1 		def=-0.29
	    parm HG2_SOIL_temp	  real count=0:1 		def=0.39
	    parm HG_ASY_SOIL_tem real count=0:1 		def=1

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm HG2_SOIL	 real count=0:1 def=--
	    parm HG_ASY_SOIL	 real count=0:1 def=--

	PARMSET name=pho_hapke_86_hg2_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm HG2_SOIL	 real count=0:1 def=--
	    parm HG_ASY_SOIL	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_hg2.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.21
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.07
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
		let B_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.29
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if


	  if ($count(HG2_SOIL)=0)
	 						let HG2_SOIL_temp=0.39
	  else
		let HG2_SOIL_temp=&HG1_SOIL
	  end-if

	  if ($count(HG_ASY_SOIL)=0)
	 					      let HG_ASY_SOIL_tem=1
	  else
		let HG_ASY_SOIL_tem=&HG_ASY_SOIL
	  end-if


	   tutor pho_hapke_86_hg2_sub  					  +
		|restore=pho_hapke_86_hg2.par, save=pho_hapke_86_hg2.par| +
	    		W_SOIL=&W_SOIL_temp				  +
	   		H_SHOE=&H_SHOE_temp				  + 
	   		B_SHOE=&B_SHOE_temp				  + 
	    		THETA=&THETA_temp				  + 
	    		HG1_SOIL=&HG1_SOIL_temp				  +
	    		HG2_SOIL=&HG2_SOIL_temp				  +
	    		HG_ASY_SOIL=&HG_ASY_SOIL_tem	
 

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_86_HG2 function"
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE_86_HG2' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1986-two-term-Henyey-Greenstein photometric function.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI B_SHOE
Opposition magnitude coefficient (height of opposition surge due to shadowing);
one of the classical Hapke parameter. In some papers give the parameter S0.
The relation between both is:
B_SHOE==S0/(W_SOIL * soilPhaseFunction(0) )

.VARI THETA
Average macroscopic slope angle;
one of the classical Hapke parameter. 

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.end
$!-----------------------------------------------------------------------------
$ create pho_hapke_86_le2.pdf
procedure option=selftutor help=*

	parm W_SOIL_temp 	 real count=0:1 		def=0.3
	parm H_SHOE_temp	 real count=0:1 		def=0.06
	parm B_SHOE_temp	 real count=0:1 		def=2.0
	parm THETA_temp	 	 real count=0:1 		def=20.0
	parm LE1_SOIL_temp	 real count=0:1 		def=0.3
	parm LE2_SOIL_temp	 real count=0:1 		def=0.3

	parm W_SOIL 	 real count=0:1 def=--
	parm H_SHOE	 real count=0:1 def=--
	parm B_SHOE	 real count=0:1 def=--
	parm THETA	 real count=0:1 def=--
	parm LE1_SOIL	 real count=0:1 def=--
	parm LE2_SOIL	 real count=0:1 def=--

	PARMSET name=pho_hapke_86_le2_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm LE1_SOIL	 real count=0:1 def=--
	    parm LE2_SOIL	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_86_le2.par


	  if ($count(W_SOIL)=0) 
							let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0) 
							let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
	 	let B_SHOE_temp=&B_SHOE
	  end-if

	  if ($count(THETA)=0) 	
							let THETA_temp=20.0
	  else
		let THETA_temp=&THETA
	  end-if

	  if ($count(LE1_SOIL)=0)
	 						let LE1_SOIL_temp=0.3
	  else
	 	let LE1_SOIL_temp=&LE1_SOIL
	  end-if

	  if ($count(LE2_SOIL)=0)
	 						let LE2_SOIL_temp=0.3
	  else
	 	let LE2_SOIL_temp=&LE2_SOIL
	  end-if


	   tutor pho_hapke_86_le2_sub 			+
		|restore=pho_hapke_86_le2.par,		+
		 save=pho_hapke_86_le2.par|		+
			W_SOIL=&W_SOIL_temp		+
			H_SHOE=&H_SHOE_temp		+
			B_SHOE=&B_SHOE_temp		+
			THETA=&THETA_temp		+
			LE1_SOIL=&LE1_SOIL_temp		+
			LE2_SOIL=&LE2_SOIL_temp	
	
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_86_LE2 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE_86_LE2' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1986-two-term-Legendre-Polynomial photometric function.

.page
MATHEMATICAL BACKGROUND :

bidirectional reflectance [1/str] :

r(i,e,g) = ...


REFERENCE :
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
3. Correction for Macroscopic Roughness
   Icarus, Vol. 59, p.41-59, 1984
4. The Extinction Coefficient and the Opposition Effect
   Icarus, Vol. 67, p. 264-280, 1986

.page
PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI B_SHOE
Opposition magnitude coefficient (height of opposition surge due to shadowing);
one of the classical Hapke parameter. In some papers give the parameter S0.
The relation between both is:
B_SHOE==S0/(W_SOIL * soilPhaseFunction(0) )


.VARI THETA
Average macroscopic slope angle;
one of the classical Hapke parameter. 

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.end
$!-----------------------------------------------------------------------------
$ create pho_hapke_hg1_dom.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	real count=0:1 			def=0.3
	    parm H_SHOE_temp	real count=0:1 			def=0.06
	    parm B_SHOE_temp	real count=0:1 			def=2.0
	    parm THETA_temp	real count=0:1 			def=15.0
	    parm HG1_SOIL_temp	real count=0:1 			def=-0.26
    	    parm H_CBOE_temp 	real count=0:1 			def=0.06
    	    parm B_CBOE_temp 	real count=0:1 			def=1.0

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
    	    parm H_CBOE 	 real count=0:1 def=--
    	    parm B_CBOE 	 real count=0:1 def=--

	PARMSET name=pho_hapke_hg1_dom_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
    	    parm H_CBOE 	 real count=0:1 def=--
    	    parm B_CBOE 	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_hapke_hg1_dom.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
		let B_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.26
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if

	  if ($count(H_CBOE)=0)
	 						let H_CBOE_temp=0.06
	  else
		let H_SHOE_temp=&H_CBOE
	  end-if

	  if ($count(B_CBOE)=0)
	 						let B_CBOE_temp=1.0
	  else
		let B_CBOE_temp=&B_CBOE
	  end-if




	   tutor pho_hapke_hg1_dom_sub 					    +
		|restore=pho_hapke_hg1_dom.par, save=pho_hapke_hg1_dom.par| +
	    		W_SOIL=&W_SOIL_temp				    +
	   		H_SHOE=&H_SHOE_temp				    + 
	   		B_SHOE=&B_SHOE_temp				    + 
	    		THETA=&THETA_temp				    + 
	    		HG1_SOIL=&HG1_SOIL_temp 			    +
	   		H_CBOE=&H_CBOE_temp				    + 
	   		B_CBOE=&B_CBOE_temp 




	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the HAPKE_HG1_DOM function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_HAPKE_HG1_DOM' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Hapke-1986-one-term-Henyey-Greenstein photometric function containing the
coherent backscattering term in the form of Dominique 1992.

.page

MATHEMATICAL BACKGROUND :


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

Deborah Dominique, A Simple Method for Comparing Shadow-Hiding and Coherent 
  Backscatter Models of the Opposition Effect
  The 24th Annual Meeting of the Division for Planetary Sciences, 
  12-16 October 1992, Munich, Germany, 
  Bulletin of the American Astronomical Society, 
  Annual Report of the AAS, DPS Abstracts, 
  DDA/HAD Abstracts, Vol. 24, No. 3, 1992, p.958
Bruce Hapke, Coherent Backscatter and the Radar Characteristics of Outer Planet 
  Satellites,
  Icarus, Vol. 88, 407-417, 1990
Bruce Hapke, Bidirectional Reflectance Spectroscopy.
  3. Correction for Macroscopic Roughness
  Icarus, Vol. 59, p.41-59, 1984


PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI H_CBOE
Hapke parameter

.VARI B_CBOE
Hapke parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI B_SHOE
Opposition magnitude coefficient (height of opposition surge due to shadowing);
one of the classical Hapke parameter. 

.VARI THETA
Average macroscopic slope angle;
one of the classical Hapke parameter. 

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI H_CBOE
Parameter of the coherent backscattering ( width of theopposition surge due 
to the backscatter ).

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to backscatter). 

.end
$!-----------------------------------------------------------------------------
$ create pho_regner_hapke_hg1.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	real count=0:1 			def=0.3
	    parm H_SHOE_temp	real count=0:1 			def=0.06
	    parm B_SHOE_temp	real count=0:1 			def=2.0
	    parm THETA_temp	real count=0:1 			def=15.0
	    parm HG1_SOIL_temp	real count=0:1 			def=-0.26
	    parm W_ATM_temp	real count=0:1 			def=0.78
	    parm TAU_ATM_temp	real count=0:1 			def=0.05
	    parm HG1_ATM_temp	real count=0:1 			def=0.35

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm W_ATM		 real count=0:1 def=--
	    parm TAU_ATM	 real count=0:1 def=--
	    parm HG1_ATM	 real count=0:1 def=--

	PARMSET name=pho_regner_hapke_hg1_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm W_ATM		 real count=0:1 def=--
	    parm TAU_ATM	 real count=0:1 def=--
	    parm HG1_ATM	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_regner_hapke_hg1.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
		let B_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.26
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if

	  if ($count(W_ATM)=0)
	 						let W_ATM_temp=0.78
	  else
		let W_ATM_temp=&W_ATM
	  end-if

	  if ($count(TAU_ATM)=0)
	 						let TAU_ATM_temp=0.05
	  else
		let TAU_ATM_temp=&TAU_ATM
	  end-if

	  if ($count(HG1_ATM)=0)
	 						let HG1_ATM_temp=0.35
	  else
		let HG1_ATM_temp=&HG1_ATM
	  end-if

	   tutor pho_regner_hapke_hg1_sub 	   		+
		|restore=pho_regner_hapke_hg1.par, 		+
		    save=pho_regner_hapke_hg1.par| 		+
	    		W_SOIL=&W_SOIL_temp			+
	   		H_SHOE=&H_SHOE_temp			+ 
	   		B_SHOE=&B_SHOE_temp			+ 
	    		THETA=&THETA_temp			+ 
	    		HG1_SOIL=&HG1_SOIL_temp 		+
	    		W_ATM=&W_ATM_temp 			+
	    		TAU_ATM=&TAU_ATM_temp 			+
	    		HG1_ATM=&HG1_ATM_temp 			

	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the REGNER_HAPKE_HG1 function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'PHO_REGNER_HAPKE_HG1' sub-menu (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the
combined photometric function of the system atmosphere/surface as proposed by 
P.Regner. 

.page

MATHEMATICAL BACKGROUND :

The approximation from Van Blerkom for the atmosphere is combined with the 
Hapke-1986-one-term-Henyey-Greenstein photometric function for the surface.


bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

  Peter Regner, Photometric Analysis for the Determination of Physical and 
  Structural Properties of the Martian Surface in the Oxia Palus Region, 
  Thesis University Munich, DLR-FB 90-29, 1990

 Van Blerkom, D. J., The effect of haze on the visibility of Martian surface 
 features, ICARUS,vol.14, pp. 235-244, 1971

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI W_ATM
Regner parameter

.VARI TAU_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI B_SHOE
Opposition magnitude coefficient (height of opposition surge due to shadowing);
one of the classical Hapke parameter. 

.VARI THETA
Average macroscopic slope angle;
one of the classical Hapke parameter. 

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.end
$!-----------------------------------------------------------------------------
$ create pho_atmo_corr_regner.pdf
procedure option=selftutor help=*

	    parm W_SOIL_temp 	real count=0:1 			def=0.3
	    parm H_SHOE_temp	real count=0:1 			def=0.06
	    parm B_SHOE_temp	real count=0:1 			def=2.0
	    parm THETA_temp	real count=0:1 			def=15.0
	    parm HG1_SOIL_temp	real count=0:1 			def=-0.26
	    parm W_ATM_temp	real count=0:1 			def=0.78
	    parm TAU_ATM_temp	real count=0:1 			def=0.05
	    parm HG1_ATM_temp	real count=0:1 			def=0.35

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm W_ATM		 real count=0:1 def=--
	    parm TAU_ATM	 real count=0:1 def=--
	    parm HG1_ATM	 real count=0:1 def=--

	PARMSET name=pho_atmo_corr_regner_sub help=*

	    parm W_SOIL 	 real count=0:1 def=--
	    parm H_SHOE		 real count=0:1 def=--
	    parm B_SHOE		 real count=0:1 def=--
	    parm THETA		 real count=0:1 def=--
	    parm HG1_SOIL	 real count=0:1 def=--
	    parm W_ATM		 real count=0:1 def=--
	    parm TAU_ATM	 real count=0:1 def=--
	    parm HG1_ATM	 real count=0:1 def=--

	END-PROC

body

	if (_tutor=1)

	  restore-parm pho_regner_hapke_hg1.par


	  if ($count(W_SOIL)=0)
	 						let W_SOIL_temp=0.3
	  else
		let W_SOIL_temp=&W_SOIL
	  end-if

	  if ($count(H_SHOE)=0)
	 						let H_SHOE_temp=0.06
	  else
		let H_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(B_SHOE)=0)
	 						let B_SHOE_temp=2.0
	  else
		let B_SHOE_temp=&H_SHOE
	  end-if

	  if ($count(THETA)=0)
	 						let THETA_temp=20
	  else
		let THETA_temp=&THETA
	  end-if


	  if ($count(HG1_SOIL)=0)
	 						let HG1_SOIL_temp=-0.26
	  else
		let HG1_SOIL_temp=&HG1_SOIL
	  end-if

	  if ($count(W_ATM)=0)
	 						let W_ATM_temp=0.78
	  else
		let W_ATM_temp=&W_ATM
	  end-if

	  if ($count(TAU_ATM)=0)
	 						let TAU_ATM_temp=0.05
	  else
		let TAU_ATM_temp=&TAU_ATM
	  end-if

	  if ($count(HG1_ATM)=0)
	 						let HG1_ATM_temp=0.35
	  else
		let HG1_ATM_temp=&HG1_ATM
	  end-if


	   tutor pho_atmo_corr_regner_sub 	   		+
		|restore=pho_atmo_corr_regner.par, 		+
		    save=pho_atmo_corr_regner.par|		+
	    		W_SOIL=&W_SOIL_temp			+
	   		H_SHOE=&H_SHOE_temp			+ 
	   		B_SHOE=&B_SHOE_temp			+ 
	    		THETA=&THETA_temp			+ 
	    		HG1_SOIL=&HG1_SOIL_temp 		+
	    		W_ATM=&W_ATM_temp 			+
	    		TAU_ATM=&TAU_ATM_temp 			+
	    		HG1_ATM=&HG1_ATM_temp 			
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs needs "
	   write " photometric function parameters "
	   write " for the ATMO_CORR_REGNER function."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.title
'pho_atmo_corr_regner' (for photometry VICAR program)

.help

PURPOSE:

In this PDF, the user is asked for the only parameters needed for the 
Regner-Hapke-1986-one-term-Henyey-Greenstein photometric function.
This is only (!) usefully to do a kind of atmospheric correction together 
with the photometric correction !!!

.page

MATHEMATICAL BACKGROUND :

This mode is only(!) usfully to compute the photometric correction factor
including the atmospheric correction.
For the originaly meassured illumination condition it is used the combined 
photometric function of the system atmosphere/surface as proposed by P.Regner
(using the Hapke-1986-one-term-Henyey-Greenstein function).
For the artifitialy desired illumination condition it is used the 
Hapke-1986-one-term-Henyey-Greenstein function only.

bidirectional reflectance [1/str] :

r(i,e,g) = ...

REFERENCE :

  Peter Regner, Photometric Analysis for the Determination of Physical and 
  Structural Properties of the Martian Surface in the Oxia Palus Region, 
  Thesis University Munich, DLR-FB 90-29, 1990

PROGRAMMER:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)

.level1

.VARI W_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI THETA
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI W_ATM
Regner parameter

.VARI TAU_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.level2

.VARI W_SOIL
Single-scattering albedo of the soil particles; 
one of the classical Hapke parameter.

.VARI H_SHOE
Parameter which characterizes the soil structure in the terms of porosity 
or compaction (width of opposition surge due to shadowing); 
one of the classical Hapke parameter.

.VARI B_SHOE
Opposition magnitude coefficient (height of opposition surge due to shadowing);
one of the classical Hapke parameter. 

.VARI THETA
Average macroscopic slope angle;
one of the classical Hapke parameter. 

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.end
$!-----------------------------------------------------------------------------
$ create phopdf.pdf
procedure
body
write " "
write "**********************************************************************"
write " "
write " This procedure is only a dummy and does nothing. "
write " It helps to install the phopdf's into the VICAR. "
write " "
write "**********************************************************************"
end-proc  
$ Return
$!#############################################################################
$Imake_File:
$ create phopdf.imake
#define PROCEDURE 	phopdf
$ Return
$!#############################################################################
$Test_File:
$ create tphopdf.c
#include <math.h>
#include "vicmain_c"
#include "pho.h"

/* Program TPHOPDF  */

void main44()
{
  int cnt, def, i, ival, ival1, num, illMode, status;
  float temp;
  double dval, dval1, IncAng, EmAng, PhasAng, phoFuncVal;
  char subcmd[9], cval[133], cval1[133], msg[133],
  keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char *pkeylist;
  PHO pho_obj;
  PHO_ILLUM Millum, Tillum;

  zvmessage(" program TPHOPDF", "");
  zvmessage( " ", "");

  zveaction("","");

  status = phoInit( &pho_obj);

/* get the photometric function and there input parameters from the PDF     */
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj);
  if(status != phoSUCCESS) return;	
  
/* get the number of parameters of the current photometric function : 	    */

  status = phoGetKeys( pho_obj, 0, &num); 
  strcpy( msg, " parameter number = " );
  sprintf( cval1, " %i", num);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the list of parameter keywords for the current photometric function : */

  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
  pkeylist = (char *)keylist;

  status = phoGetKeys( pho_obj, pkeylist, &num);

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  strcpy( msg, " Function =" );
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

  for (i=0; i<num; i++) {

    status = phoGetVal( pho_obj, keylist[i], &dval1);
    strcpy( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", dval1);
    strcat( msg, cval1);
    zvmessage( msg, "");
  }

/* reads in the function arguments from PDF and fill the illumination union: */

/* fill the illumination union for the meassured illumination conditions :   */

  zvmessage( " ", "");

  zvp("INC_ANG", &temp, &cnt);
  strcpy( msg, "Incidence Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.inc = cos(RETURN_RADIANS((double )temp));

  zvp("EM_ANG", &temp, &cnt);
  strcpy( msg, "Emission Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.em = cos(RETURN_RADIANS((double )temp));

  zvp("PHAS_ANG", &temp, &cnt);
  strcpy( msg, "Phase Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.phas = cos(RETURN_RADIANS((double )temp));

  zvmessage( " ", "");

  Millum.mode = illEllCos;
  Millum.type.sunshadow = illNoShadow;
  Millum.type.viewshadow = illNoShadow;

/* fill the illumination union for the target illumination conditions : */

  Tillum.mode = illEllCos;
  Tillum.type.sunshadow = illNoShadow;
  Tillum.type.viewshadow = illNoShadow;

  Tillum.cos.inc  = 1.0;
  Tillum.cos.em = 1.0;
  Tillum.cos.phas = 1.0;



/* get the Bidirectional Reflectance Value : */

  status = phoBidiRef( pho_obj, &Millum, &phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (meassured illumin.) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoBidiRef( pho_obj, &Tillum, &phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the photometric function value : */

  status = phoFunc( pho_obj, &Millum, &phoFuncVal );

  strcpy( msg, "Photometric Function Value (meassuered illumin.) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoFunc( pho_obj, &Tillum, &phoFuncVal );

  strcpy( msg, "Photometric Function Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the correction value to nadir viewing and illumination conditions : */

  status = phoCorrect( pho_obj, &Millum, &Tillum, &phoFuncVal );

  strcpy( msg, " to-nadir Correction Value =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  zvmessage( " ", "");


  status = phoFree( pho_obj);
  return;
}
$!-----------------------------------------------------------------------------
$ create tphopdf.imake
#define PROGRAM  	tphopdf

#define MODULE_LIST  	tphopdf.c

#define MAIN_LANG_C

#define USES_ANSI_C

#define TEST

#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB

/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define LIB_LOCAL
#define DEBUG

*******************************************/

$!-----------------------------------------------------------------------------
$ create tphopdf.pdf
  procedure option=selftutor help=*

  !*******************************************************************
  ! If OPTIONS=SELFTUTOR (see the first line of this procedure), the
  ! TAE Terminal Monitor (TM) does not do the tutoring, i.e. when a
  ! user requests t(utor) for a SELFTUTOR procedure, the procedure
  ! is executed immediately to perform its own version of tutor.
  !*******************************************************************


	!*************************************************************
	! The process of the main program part (written in C) is named
	! TPHOPDF and has to be declared here.
	!*************************************************************

	process name=tphopdf
	end-proc

	!*************************************************************
	! The following definitions/defaults will be used in 
	! the batch modus :
	!*************************************************************
	


  ! general input parameters :


	! dummy inputs :

	parm inp	type=(string,32) count=0:1	default=inp.img
	parm out	type=(string,32) count=0:1	default=out.img


	! photometric functions :

	parm PHO_FUNC type=(string,32) count=1 		+
			valid = (			+
				NONE,			+
				PAR_FILE,		+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default=MINNAERT


	! illumination conditions :

        parm INC_ANG	real count=0:1 			default=30
        parm EM_ANG 	real count=0:1 			default=40
        parm PHAS_ANG 	real count=0:1 			default=50




  ! SPICE parameters (HRSC/WAOSS parameters) :

	parm GECALDIR	type=(string,80) count = 0:2	+
			default = (HRSC_GEOCAL_DIR, WAOSS_GEOCAL_DIR)
	parm GECALDAT	type=(string,32) count = 0:1	+
			default = HRSC_GEOCAL_DATE
	parm BSPFILE	type=(string,32) count = 0:3	+
			default = HWSPICE_BSP
	parm SUNFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_SUN
	parm BCFILE	type=(string,32) count = 0:6	+
			default = HWSPICE_BC
	parm TSCFILE	type=(string,32) count = 0:6	+
			default = HWSPICE_TSC
	parm TIFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TI
	parm TPCFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TPC
	parm TLSFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TLS
	


  ! photometric parameters:

       parm PHO_PAR_FILE	string  count=0:1 	default=--

       parm ALBEDO 	real count=0:1 			default=--
       parm EXPONENT 	real count=0:1 valid=(0:1)	default=--
       parm A_VEVERKA 	real count=0:1 			default=--
       parm B_VEVERKA 	real count=0:1 			default=--
       parm C_VEVERKA 	real count=0:1 			default=--
       parm D_VEVERKA 	real count=0:1 			default=--
       parm MO_EXP1 	real count=0:1 			default=--
       parm MO_EXP2 	real count=0:1 			default=--
       parm E_BURATTI 	real count=0:1 			default=--
       parm DEN_SOIL 	real count=0:1 			default=--
       parm W_SOIL 	real count=0:1 valid=(0:1)	default=--
       parm HG1_SOIL 	real count=0:1 			default=--
       parm HG2_SOIL 	real count=0:1 			default=--
       parm HG_ASY_SOIL real count=0:1 			default=--
       parm LE1_SOIL 	real count=0:1 			default=--
       parm LE2_SOIL 	real count=0:1 			default=--
       parm H_SHOE 	real count=0:1 			default=--
       parm B_SHOE 	real count=0:1 			default=--
       parm H_CBOE 	real count=0:1 			default=--
       parm B_CBOE 	real count=0:1 			default=--
       parm THETA 	real count=0:1 			default=--
       parm COOK 	real count=0:1 			default=--
       parm TAU_ATM 	real count=0:1 			default=--
       parm W_ATM 	real count=0:1 valid=(0:1)	default=--
       parm HG1_ATM 	real count=0:1 			default=--
       parm IRV_EXP1 	real count=0:1 			default=--
       parm IRV_EXP2 	real count=0:1 			default=--


      !*******************************************************************
      ! local variable for the photometric parameters to enable different
      !	default values of the same parameter in different photometric 
      ! functions :
      !*******************************************************************

       local ALBEDO_count 	int 
       local EXPONENT_count 	int 
       local A_VEVERKA_count 	int
       local B_VEVERKA_count 	int
       local C_VEVERKA_count	int
       local D_VEVERKA_count 	int 
       local MO_EXP1_count 	int 
       local MO_EXP2_count 	int
       local E_BURATTI_count 	int
       local DEN_SOIL_count 	int
       local W_SOIL_count 	int
       local HG1_SOIL_count 	int
       local HG2_SOIL_count 	int
       local HG_ASY_SOIL_cou 	int
       local LE1_SOIL_count 	int
       local LE2_SOIL_count 	int
       local H_SHOE_count 	int
       local B_SHOE_count 	int
       local H_CBOE_count 	int
       local B_CBOE_count 	int
       local THETA_count 	int
       local COOK_count 	int
       local TAU_ATM_count 	int
       local W_ATM_count 	int
       local HG1_ATM_count 	int
       local IRV_EXP1_count	int
       local IRV_EXP2_count  	int




  ! the name of the parameter file :

       parm SAVE_PAR	type=(string,40) count=1 default=last.par
       parm MAIN_PROC_NAME 	string	 count=1 default=tphopdf



  ! for running the main program: 

       parm readparam	keyword		 count=0:1	valid=read def=--

	!*******************************************************************
	! The globals "$MENUOPT" and "SYSCHAR" are used in this proc,
	! so we have to declare they here with the command REFGBL
	!*******************************************************************

	refgbl $menuopt
	refgbl $syschar

  body


  !*************************************************************************
  ! "pho_global.pdf" is being executed in the following line. 
  ! In this PDF the global PHO_FUNC_type is defined. 
  !*************************************************************************

  pho_global PHO_FUNC_type = "&PHO_FUNC" pho_PROC_NAME="&_PROC"



  ! for different photometric funtions :

  !*******************************************************************
  ! set the value oft local variable parameter-count to the beginning 
  ! parameter_count:
  !*******************************************************************

  let ALBEDO_count 	=$count(ALBEDO)
  let EXPONENT_count 	=$count(EXPONENT)
  let A_VEVERKA_count 	=$count(A_VEVERKA)
  let B_VEVERKA_count 	=$count(B_VEVERKA)
  let C_VEVERKA_count	=$count(C_VEVERKA)
  let D_VEVERKA_count 	=$count(D_VEVERKA)
  let MO_EXP1_count 	=$count(MO_EXP1)
  let MO_EXP2_count 	=$count(MO_EXP2)
  let E_BURATTI_count 	=$count(E_BURATTI)
  let DEN_SOIL_count 	=$count(DEN_SOIL)
  let W_SOIL_count 	=$count(W_SOIL)
  let HG1_SOIL_count 	=$count(HG1_SOIL)
  let HG2_SOIL_count 	=$count(HG2_SOIL)
  let HG_ASY_SOIL_cou 	=$count(HG_ASY_SOIL)
  let LE1_SOIL_count 	=$count(LE1_SOIL)
  let LE2_SOIL_count 	=$count(LE2_SOIL)
  let H_SHOE_count 	=$count(H_SHOE)
  let B_SHOE_count 	=$count(B_SHOE)
  let H_CBOE_count 	=$count(H_CBOE)
  let B_CBOE_count 	=$count(B_CBOE)
  let THETA_count 	=$count(THETA)
  let COOK_count 	=$count(COOK)
  let TAU_ATM_count 	=$count(TAU_ATM)
  let W_ATM_count 	=$count(W_ATM)
  let HG1_ATM_count 	=$count(HG1_ATM)
  let IRV_EXP1_count	=$count(IRV_EXP1)
  let IRV_EXP2_count  	=$count(IRV_EXP2)




    !*************************************************************************
    ! _TUTOR (type=integer) is an implicit local variable in a procedure
    ! with OPTIONS=SELFTUTOR (see first line of this proc). When TAE
    ! receives an initial tutor request for a procedure declared as selftutor,
    ! _TUTOR is set to one, otherwise it is set to zero.
    !*************************************************************************

    if (_tutor=1)

  	!*********************************************************************
  	! dummy files in case the procs aren't called :
  	! The save-variable (sub-)commmand is used to save the specified
  	! variables into the save file.
  	!
  	! Command:
  	! SAVE-VARIABLE FILE = save_file_name, VARIABLE = variable_list
  	!*********************************************************************

  	!*********************************************************************
  	! Saving the parameter-file with the photometric function parameters
  	! either the inputed parameters or if did no parameter input - the  
  	! default parameter for the actual photometric function :
  	!*********************************************************************	
	!*********************************************************************
	! At this position you can input function-specific default values 
	! for the function parameter for the tutor modus : 
	!*********************************************************************




  ! LAMBERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0

          save-var pho_lambert.par, ALBEDO

	if (ALBEDO_count=0)	let ALBEDO=--


  ! MINNAERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0
	if (EXPONENT_count=0)				let EXPONENT=0.6

          save-var pho_minnaert.par, (		+
				ALBEDO,		+
				EXPONENT	+
				)


	if (ALBEDO_count=0)	let ALBEDO=--
	if (EXPONENT_count=0)	let EXPONENT=--

  ! IRVINE :

	if (EXPONENT_count=0)				let EXPONENT=0.9
	if (IRV_EXP1_count=0)				let IRV_EXP1=0.118
	if (IRV_EXP2_count=0)				let IRV_EXP2=0.0039

          save-var pho_irvine.par,  (		+
				EXPONENT,	+
				IRV_EXP1,	+
				IRV_EXP2	+
				)

	if (EXPONENT_count=0)	let EXPONENT=--
	if (IRV_EXP1_count=0)	let IRV_EXP1=--
	if (IRV_EXP2_count=0)	let IRV_EXP2=--

  ! VEVERKA :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.997
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14

          save-var pho_veverka.par, (		+
				A_VEVERKA,	+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA	+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--

  ! BURATTI1 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14

          save-var pho_buratti1.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! BURATTI2 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (E_BURATTI_count=0) 			let E_BURATTI=0.14

          save-var pho_buratti2.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				C_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! BURATTI3 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14

          save-var pho_buratti3.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! MOSHER :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (MO_EXP1_count=0) 				let MO_EXP1=0.5
	if (MO_EXP2_count=0) 				let MO_EXP2=0.1

          save-var pho_mosher.par, (		+
				A_VEVERKA,	+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA,	+
				MO_EXP1,	+
				MO_EXP2		+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (MO_EXP1_count=0) 	let MO_EXP1=--
	if (MO_EXP2_count=0) 	let MO_EXP2=--

  ! LUMME_BOWEL_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (DEN_SOIL_count=0) 				let DEN_SOIL=0.8
	if (THETA_count=0) 				let THETA=20
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26

          save-var pho_lumme_bowel_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				DEN_SOIL,	+
				THETA,		+
				HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (DEN_SOIL_count=0) 	let DEN_SOIL=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--

  ! HAPKE_81_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3

          save-var pho_hapke_81_le2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				LE1_SOIL,	+
				LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--

  ! HAPKE_81_COOK :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3
	if (COOK_count=0) 				let COOK=0.9

          save-var pho_hapke_81_cook.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				LE1_SOIL,	+
				LE2_SOIL,	+
				COOK		+
				)


	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--
	if (COOK_count=0) 	let COOK=--

  ! HAPKE_86_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=15.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26

          save-var pho_hapke_86_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--

  ! HAPKE_86_HG2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.29
	if (HG2_SOIL_count=0) 				let HG2_SOIL=0.39
	if (HG_ASY_SOIL_cou=0) 				let HG_ASY_SOIL=1

          save-var pho_hapke_86_hg2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				HG2_SOIL,	+
				HG_ASY_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--
	if (HG2_SOIL_count=0) 	let HG2_SOIL=--
	if (HG_ASY_SOIL_cou=0) let HG_ASY_SOIL=--

  ! HAPKE_86_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.29
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.39

          save-var pho_hapke_86_le2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				LE1_SOIL,	+
				LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--

  ! HAPKE_HG1_DOM :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (H_CBOE_count=0) 				let H_CBOE=0.06
	if (B_CBOE_count=0) 				let B_CBOE=1.0

          save-var pho_hapke_hg1_dom.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				H_CBOE,		+
				B_CBOE		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (H_CBOE_count=0) 	let H_CBOE=--
	if (B_CBOE_count=0) 	let B_CBOE=--

  ! REGNER_HAPKE_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (W_ATM_count=0)				let W_ATM=0.78
	if (TAU_ATM_count=0)				let TAU_ATM=0.05
	if (HG1_ATM_count=0)				let HG1_ATM=0.35

          save-var pho_regner_hapke_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				W_ATM,		+
				TAU_ATM,	+
				HG1_ATM		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (W_ATM_count=0)	let W_ATM=--
	if (TAU_ATM_count=0)	let TAU_ATM=--
	if (HG1_ATM_count=0)	let HG1_ATM=--

  ! ATMO_CORR_REGNER :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (W_ATM_count=0)				let W_ATM=0.78
	if (TAU_ATM_count=0)				let TAU_ATM=0.05
	if (HG1_ATM_count=0)				let HG1_ATM=0.35

          save-var pho_atmo_corr_regner.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				W_ATM,		+
				TAU_ATM,	+
				HG1_ATM		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (W_ATM_count=0)	let W_ATM=--
	if (TAU_ATM_count=0)	let TAU_ATM=--
	if (HG1_ATM_count=0)	let HG1_ATM=--







	!*******************************************************************
	! other dummy files in case the procs aren't called :
	!*******************************************************************

          save-var &"_PROC"_general.par, (	+
				inp,		+
				out, 		+
				PHO_FUNC,	+
				INC_ANG,	+
				EM_ANG,		+
				PHAS_ANG	+
				)

	  save-var &"_PROC"_spice.par, (	+
				GECALDIR,	+
				GECALDAT,	+
				BSPFILE,	+
				SUNFILE,	+
				BCFILE,		+
				TSCFILE,	+
				TIFILE,		+
				TPCFILE,	+
				TLSFILE		+
				)

	  save-var common_save_par.par, SAVE_PAR

	  save-var pho_par_file.par, PHO_PAR_FILE

	  save-var common_proc_name.par, MAIN_PROC_NAME





	  !****************************************************************
	  ! The string global variable $MENUOPT allows suppression of the
	  ! "Press RETURN key for menu" message. Hence, when typing
	  ! "VICAR> tutor TPHOPDF" the menu can be entered directly 
	  ! without the need to press the RETURN key first.
	  !****************************************************************

	    let $menuopt="NO_PRESS_FOR_MENU"

	  !****************************************************************
	  ! "menu" puts VICAR into the Menu Mode, i.e. here we enter the
	  !  menu of 'TPHOPDF.MDF'
	  !****************************************************************

	    menu &"_PROC".mdf

   end-if

    write "returned into main-program &_PROC"

! passing the parameters from tutor mode and then running the process :

	!***************************************************************
	! $COUNT returns the current number of elements for "readparam":
	! If the keyword parameter "readparam" has been set to 'read',
	! $count(readparam)=1, else $count(readparam)<>1 (see below).
	!***************************************************************

	if ($count(readparam)=1) 


		!****************************************************
		! The restore-parms (sub-)command is used to restore
		! only the parameters in the save file.
		!
		! Command:
		! RESTORE-PARMS FILE = save_file_name
		!****************************************************

		restore-parm &"_PROC"_general.par
		restore-parm &"_PROC"_spice.par


		restore-parm common_save_par.par


		if ("&PHO_FUNC" = "PAR_FILE" ) 			+
					restore-parm pho_par_file.par
		if ("&PHO_FUNC" = "LAMBERT" )  			+
					restore-parm pho_lambert.par
		if ("&PHO_FUNC" = "MINNAERT")  			+
					restore-parm pho_minnaert.par
		if ("&PHO_FUNC" = "IRVINE")    			+
					restore-parm pho_irvine.par
		if ("&PHO_FUNC" = "VEVERKA")   			+
					restore-parm pho_veverka.par
		if ("&PHO_FUNC" = "BURATTI1")  			+
					restore-parm pho_buratti1.par
		if ("&PHO_FUNC" = "BURATTI2")  			+
					restore-parm pho_buratti2.par
		if ("&PHO_FUNC" = "BURATTI3")  			+
					restore-parm pho_buratti3.par
		if ("&PHO_FUNC" = "MOSHER")    			+
					restore-parm pho_mosher.par
		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1") 		+
					restore-parm pho_lumme_bowel_hg1.par
		if ("&PHO_FUNC" = "HAPKE_81_LE2")		+
  					restore-parm pho_hapke_81_le2.par
		if ("&PHO_FUNC" = "HAPKE_81_COOK")  		+
 					restore-parm pho_hapke_81_cook.par
		if ("&PHO_FUNC" = "HAPKE_86_HG1")		+
 					restore-parm pho_hapke_86_hg1.par
		if ("&PHO_FUNC" = "HAPKE_86_HG2")		+
 					restore-parm pho_hapke_86_hg2.par
		if ("&PHO_FUNC" = "HAPKE_86_LE2") 		+
 					restore-parm pho_hapke_86_le2.par
		if ("&PHO_FUNC" = "HAPKE_HG1_DOM") 		+
					restore-parm pho_hapke_hg1_dom.par
		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")  		+
					restore-parm pho_regner_hapke_hg1.par
		if ("&PHO_FUNC" = "ATMO_CORR_REGNER")		+
 					restore-parm pho_atmo_corr_regner.par




		save-var "&SAVE_PAR", (		+
			inp,			+
			out, 			+
			PHO_FUNC,		+
			INC_ANG,		+
			EM_ANG,			+
			PHAS_ANG,		+
			GECALDIR,		+
			GECALDAT,		+
			BSPFILE,		+
			SUNFILE,		+
			BCFILE,			+
			TSCFILE,		+
			TIFILE,			+
			TPCFILE,		+
			TLSFILE,		+
			PHO_PAR_FILE,		+
			ALBEDO,			+ 
			EXPONENT,		+
      			A_VEVERKA, 		+
       			B_VEVERKA, 		+
       			C_VEVERKA, 		+
       			D_VEVERKA,		+ 
       			MO_EXP1, 		+
       			MO_EXP2, 		+
       			E_BURATTI, 		+
       			DEN_SOIL, 		+
       			W_SOIL, 		+
       			HG1_SOIL, 		+
       			HG2_SOIL, 		+
       			HG_ASY_SOIL,		+
       			LE1_SOIL, 		+
       			LE2_SOIL, 		+
       			H_SHOE, 		+
       			B_SHOE, 		+
       			H_CBOE, 		+
       			B_CBOE, 		+
       			THETA ,			+
       			COOK,			+
       			TAU_ATM,		+
       			W_ATM, 			+
       			HG1_ATM,		+
       			IRV_EXP1, 		+
       			IRV_EXP2 		+
			SAVE_PAR                +
				)



		!*********************************************
		! The parameter and their values can be 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*************************************************
		! ... and the main program TPHOPDF is run
		!*************************************************

		&_PROC

	end-if



! running the process in the batch modus :

	if ($count(readparam) <> 1) 

		!***********************************************************
		! At this position you can input function-specific default 
		! values for the function parameter for the batch modus : 
		!***********************************************************


		if ( "&PHO_FUNC" = "LAMBERT" )
			if (ALBEDO_count=0)		let ALBEDO=1.0
		end-if

		if ("&PHO_FUNC" = "MINNAERT")
			if (ALBEDO_count=0)		let ALBEDO=1.0
			if (EXPONENT_count=0)		let EXPONENT=0.6
		end-if

		if ("&PHO_FUNC" = "IRVINE")
			if (EXPONENT_count=0)		let EXPONENT=0.9
			if (IRV_EXP1_count=0)		let IRV_EXP1=0.118
			if (IRV_EXP2_count=0)		let IRV_EXP2=0.0039
		end-if

		if ("&PHO_FUNC" = "VEVERKA")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.997
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI1")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI2")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI3")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "MOSHER")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (MO_EXP1_count=0) 		let MO_EXP1=0.5
			if (MO_EXP2_count=0) 		let MO_EXP2=0.1
		end-if

		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (DEN_SOIL_count=0) 		let DEN_SOIL=0.8
			if (THETA_count=0) 		let THETA=20
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_COOK")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
			if (COOK_count=0) 		let COOK=0.9
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=15.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.29
			if (HG2_SOIL_count=0) 		let HG2_SOIL=0.39
			if (HG_ASY_SOIL_cou=0) 		let HG_ASY_SOIL=1
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.29
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.39
		end-if

		if ("&PHO_FUNC" = "HAPKE_HG1_DOM")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (H_CBOE_count=0) 		let H_CBOE=0.06
			if (B_CBOE_count=0) 		let B_CBOE=1.0
		end-if

		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (W_ATM_count=0)		let W_ATM=0.78
			if (TAU_ATM_count=0)		let TAU_ATM=0.05
			if (HG1_ATM_count=0)		let HG1_ATM=0.35
		end-if

		if ("&PHO_FUNC" = "ATMO_CORR_REGNER")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (W_ATM_count=0)		let W_ATM=0.78
			if (TAU_ATM_count=0)		let TAU_ATM=0.05
			if (HG1_ATM_count=0)		let HG1_ATM=0.35
		end-if





		!*********************************************
		! The parameter and their values can be 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*********************************************
		! ... and the main program TPHOPDF is run
		!*********************************************

		&_PROC

	end-if



! delete the temporary .par files only for the tutor modus :

	if ($count(readparam) = 1) 

	    if ($syschar(1) = "UNIX")

               ush /bin/rm -f tphopdf_general.par;+
		   /bin/rm -f tphopdf_spice.par;

	       ush /bin/rm -f pho_par_file.par;+
		   /bin/rm -f pho_lambert.par;+
		   /bin/rm -f pho_minnaert.par;+
		   /bin/rm -f pho_irvine.par;+
		   /bin/rm -f pho_veverka.par;+
		   /bin/rm -f pho_buratti1.par;+
		   /bin/rm -f pho_buratti2.par;+
		   /bin/rm -f pho_buratti3.par;+
		   /bin/rm -f pho_mosher.par;+
		   /bin/rm -f pho_lumme_bowel_hg1.par;+
		   /bin/rm -f pho_hapke_81_le2.par;+
		   /bin/rm -f pho_hapke_81_cook.par;+
		   /bin/rm -f pho_hapke_86_hg1.par;+
		   /bin/rm -f pho_hapke_86_hg2.par;+
		   /bin/rm -f pho_hapke_86_le2.par;+
		   /bin/rm -f pho_hapke_hg1_dom.par;+
		   /bin/rm -f pho_regner_hapke_hg1.par;+
		   /bin/rm -f pho_atmo_corr_regner.par;

	       ush /bin/rm -f common_proc_name.par;+
		   /bin/rm -f common_save_par.par

		   
       
	    else

	      dcl if f$search ("tphopdf_general.par;*") .nes. "" 	+
			then delete tphopdf_general.par;* 
	      dcl if f$search ("tphopdf_spice.par;*") .nes. "" 		+
			then delete tphopdf_spice.par;*

 	      dcl if f$search ("pho_par_file.par;*") .nes. "" 		+
		 	then delete pho_par_file.par;*
	      dcl if f$search ("pho_lambert.par;*") .nes. "" 		+
		 	then delete pho_lambert.par;*
	      dcl if f$search ("pho_minnaert.par;*") .nes. "" 		+
		 	then delete pho_minnaert.par;*
	      dcl if f$search ("pho_irvine.par;*") .nes. "" 		+
		 	then delete pho_irvine.par;*
	      dcl if f$search ("pho_veverka.par;*") .nes. "" 		+
		 	then delete pho_veverka.par;*
	      dcl if f$search ("pho_buratti1.par;*") .nes. "" 		+
		 	then delete pho_buratti1.par;*
	      dcl if f$search ("pho_buratti2.par;*") .nes. "" 		+
		 	then delete pho_buratti2.par;*
	      dcl if f$search ("pho_buratti3.par;*") .nes. "" 		+
		 	then delete pho_buratti3.par;*
	      dcl if f$search ("pho_mosher.par;*") .nes. "" 		+
		 	then delete pho_mosher.par;*
	      dcl if f$search ("pho_lumme_bowel_hg1.par;*") .nes. "" 	+
		 	then delete pho_lumme_bowel_hg1.par;*
	      dcl if f$search ("pho_hapke_81_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_le2.par;*
	      dcl if f$search ("pho_hapke_81_cook.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_cook.par;*
	      dcl if f$search ("pho_hapke_86_hg1.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg1.par;*
	      dcl if f$search ("pho_hapke_86_hg2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg2.par;*
	      dcl if f$search ("pho_hapke_86_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_le2.par;*
	      dcl if f$search ("pho_hapke_hg1_dom.par;*") .nes. "" 	+
		 	then delete pho_hapke_hg1_dom.par;*
	      dcl if f$search ("pho_regner_hapke_hg1.par;*") .nes. "" 	+
		 	then delete pho_regner_hapke_hg1.par;*
	      dcl if f$search ("pho_atmo_corr_regner.par;*") .nes. "" 	+
		 	then delete pho_atmo_corr_regner.par;*

              dcl if f$search ("common_proc_name.par;*") .nes. "" 	+
		 	then delete common_proc_name.par;*
	      dcl if f$search ("common_save_par.par;*") .nes. "" 	+
		 	then delete common_save_par.par;*


	    end-if
	end-if

	! delete all photometrical globals:

!	delete-global pho_global 

  end-proc

.title
VICAR program TPHOPDF to test and to demonstrate the usage 
of menu-driven input of photometric and other grouped parameters

.help
FUNCTION:

PHOPDF is a collection of photometry sub-pdf's and 
TPHOPDF is a program to test and demonstrate the usage of menu-driven input of 
the large number of different PDF parameters related to photometric calculus 
into the main program (including their various permitted ranges and default 
settings).  

The MDF is used to organize PDF parameters into different "menus".  The goal of 
such menus is to organize input parameters into parameter groups related to 
specific topics (e.g. I/O-parameters, photometric parameters, SPICE parameters)
in order to facilitate the preparation and the execution of a program to the 
user.

This programs uses most of the sub PDFs delivered by the routine PHOPDF.  

This demo program shows to the software user how to
	-navigate through a menu;
	-switch between the interactive operation modes of TAE (i.e. Command  
	  Line Mode [TCL], Tutor Mode, and Menu Mode);
	-save the parameter set for later re-processing;
	-delete temporary parameter files.

This program demonstrates to the software developer how to
	-integrate the sub PDFs of PHOPDF into their main program
	-set the different defaults for parameters (with different or the same 
	  name) related to different photometric functions for a tutor-session 
	  or the batch mode;

The programmers are welcome to cut out and vary the tphopdf.pdf, 
tphopdf_general.pdf, tphopdf_spice.pdf and the tphopdf.mdf for use in their own 
programs using other defaults, additional parameters, and different or 
additional submenues (see also the demonstration program PHODEM). 
But be carefully for creating names for global variables, because these global
defintions are for the hole VICAR-session. Note that the default values of the 
sub PDFs (e.g. of the photometry menus from PHOPDF) will be overwritten by the 
default values of the main-pdf (in this case by TPHOPDF.PDF).

TPHOPDF simulates a program to read the parameters from an IBIS2 parameter file 
(see the pho_par_file package) and for reading MARS94/96-SPICE informations. 
However, this is just a simulation.  There is no real IBIS2-processing nor real 
SPICE reading done in this demo!


EXECUTION:

There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  In this demo program, the menu points have the following 
(dummy/simulated!) meanings:

1. Select the first menu point to input the general parameters for a program
   such as the names of input/output images, the illumination conditions, and 
   so on.
2. In the second menu point you can input (dummy) parameters
   related to SPICE (navigation data, pointing data, instrument data, etc.)
3. In this case, the PDF pertaining to this menu point and the name of this 
   menu point are changing depending on your input of the parameter PHO_FUNC
   in the first menu point.
   There are a few special entries of this parameter:
   - to run the program without using a photometric function you have
     to select PHO_FUNC = "NONE",
   - to read in the photometric function and its associated parameters from a 
     photometric parameter IBIS2-file you have to select PHO_FUNC = "PAR_FILE", 
     and
   - to enter photometry parameters by yourself using the tutor mode
     you have to select the desired photometric function.
4. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'


5. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


HELPS :

- You will get the common help contained in the ".mdf" file (tphopdf.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (tphopdf.pdf)  
  by processing of "help-help" applied to the program (should be verry 
  similary).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.



OPERATION:

Although the main purpose of this program is to test and demonstrate the use of 
menus in VICAR programs, the main part of the test program TPHOPDF actually
- reads in the photometric parameters and the illumination conditions,
- computes the bidirectional reflectance, the photometric function and the 
  photometric correction factor to change radiance values from measured viewing 
  and illumination conditions to new artificial target viewing and illumination 
  conditions.  

No image processing is being done.  Results are written to the screen.



SUBROUTINES REQUIRED TO RUN PROGRAM:	pho_routines package


INCLUDE FILES REQUIRED TO RUN PROGRAM:	pho.h, pho.fin
					pho_global.pdf


VICAR PARAMETERS:

NOTE: All parameters (including those from the photometry subPDFs) have to be 
defined in the main-PDF (TPHOPDF.pdf).

PARAMETERS SORTED BY SUB-PDFS :

Name of PDF file	PDF parameters	default	   comments
(will appear as a	within this	setting	
separate sub-menu)	particular 
			sub-menu			


1) GENERAL INPUT / OUTPUT :
	
tphopdf_general	 	INP

			OUT

			PHO_FUNC	MINNAERT The photometric function
						(see PHOPDF)
									
						Permitted values :

						NONE, 
						PAR_FILE, 
						LAMBERT, 
						MINNAERT, 
						IRVINE, 
						VEVERKA, 
						BURATTI1, 
						BURATTI2, 
						BURATTI3, 
						MOSHER, 
						LUMME_BOWEL_HG1, 
						HAPKE_81_LE2, 
						HAPKE_81_COOK, 
						HAPKE_86_HG1, 
						HAPKE_86_HG2, 
						HAPKE_86_LE2, 
						HAPKE_HG1_DOM, 
						REGNER_HAPKE_HG1, 
						ATMO_CORR_REGNER
				INC_ANG
				EM_ANG
				PHAS_ANG


2) SPICE PARAMETERS :
dummy parameters and logical/environments as proposed for the MARS94/96 mission)
						
tphopdf_spice			GECALDIR	M94GEOCAL
				GECALDAT	HRSC_GEOCAL_DATE
				BSPFILE		HWSPICE_BSP
				SUNFILE		HWSPICE_SUN
				BCFILE		HWSPICE_BC
				TSCFILE		HWSPICE_TSC
				TIFILE		HWSPICE_TI
				TPCFILE		HWSPICE_TPC
				TLSFILE		HWSPICE_TLS


3) PHOTOMETRY MENU :

Note: the subPDFs pertaining to photometry are deliverd to the VICAR system by 
PHOPDF.  They may not be changed, however, the default values may be set, reset 
or overwritten by the mainPDF (PHOPDFin this case).  All PDF parameters must 
be defined in the main program's PDF in oder to access the different submenues 
correctly.  

actuell third menu	Name		default		Comment

PHO_PAR_FILE		PHO_PAR_FILE		parameters from IBIS 
						pho_par_file

NONE			none			do not apply any 
						photometric processing

LAMBERT			ALBEDO		1.0

MINNAERT		ALBEDO		1.0
			EXPONENT	0.6

IRVINE			EXPONENT	0.9
			IRV_EXP1	0.118
			IRV_EXP2	0.0039

VEVERKA			A_VEVERKA	0.997
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14

BURATTI1		ALBEDO		0.5
			B_VEVERKA	0.6
			E_BURATTI	0.14

BURATTI2		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
 			E_BURATTI	0.14

BURATTI3		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			E_BURATTI	0.14

MOSHER			A_VEVERKA	0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			MO_EXP1		0.5
			MO_EXP2		0.1

LUMME_BOWEL_HG1	W_SOIL	0.3
			H_SHOE		0.06
			DEN_SOIL	0.8
			THETA		20
			HG1_SOIL	-0.26

HAPKE_81_LE2		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_81_COOK		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3
			COOK		0.9

HAPKE_86_HG1		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		15.0
			HG1_SOIL	-0.26

HAPKE_86_HG2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.29
			HG2_SOIL	0.39
			HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_HG1_DOM		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		1.0
			THETA		20.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			B_CBOE		1.0

REGNER_HAPKE_HG1	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35

ATMO_CORR_REGNER	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35


4) PAR-FILE NAME

common_save_par		SAVE_PAR		Name of the TAE-parameter file 
						containing all parameters 
						needed for running the program. 
                                                default:
						name of the main program 
						with the extention ".par"

5) RUN  MAIN PROGRAM

common_proc_name		-			runs the program


GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

	Name		Type			Description

	PHO_FUNC_type 	string			It containes the names of the 
						valid photometric functions (to 
						pass into the menu).

	pho_PROC_NAME 	string			Name of the main procedure 



	

BACKGROUND AND REFERENCES :	TAE Command Language (TCL)
				Programmers's Manual
				Version 5.1
				NASA Goddard Space Flight Centre, 1991


SOFTWARE PLATFORM :		VICAR 13, TAE 5.2
				(VMS/AXP/SUNOS/SOLARIS)

HARDWARE PLATFORM :		No particular hardware required;
				tested on VMS/AXP/SUNOS/SOLARIS

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Friedel Oschuetz, July '94, original

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)

.LEVEL1

.VARI inp
File name of the input image (dummy)

.VARI out
File name for the output image (dummy)

.VARI PHO_FUNC
photometric function

.VARI  GECALDIR
Directories with geom. Calib. files	

.VARI  GECALDAT	
Creation date of the geom. Calib. files

.VARI  BSPFILE
Trajectories, SP-kernels

.VARI  SUNFILE
Ephemeris of Sun and planets

.VARI  BCFILE
Attitude, C-kernels

.VARI  TSCFILE
Clock, SCLK-kernels

.VARI  TIFILE
Instrument data, I-kernel

.VARI  TPCFILE
Planetary constants, PC-kernels

.VARI  TLSFILE
Leapseconds, LS-kernel

.VARI PHO_PAR_FILE
photometric parameter file

.VARI ALBEDO
albedo

VARI EXPONENT
Minnaert's konstant

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI DEN_SOIL
Hapke parameter

.VARI W_SOIL
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI H_CBOE
Hapke-Dominique parameter

.VARI B_CBOE
Hapke-Dominique parameter

.VARI THETA
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI TAU_ATM
Regner parameter

.VARI W_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.VARI IRV_EXP1
Irvine parameter

.VARI IRV_EXP2
Irvine parameter

.VARI INC_ANG
incidence angle

.VARI EM_ANG
emission angle

.VARI PHAS_ANG
phase angle

.VARI SAVE_PAR
file name for par-file


.LEVEL2

.VARI inp
File name of the input image (dummy)

.VARI out
File name for the output image(dummy)

.VARI PHO_FUNC
Photometric function :

This parameter selects the menu point for input the photometry task:
   1. to run the program without using a photometric function, you have 
      to select "NONE"'
   2. to read in the photometric function and its associated parameters
      from a photometric parameter file, you have to select "PAR_FILE" and
   3. to put in the parameter by yourself from the tutor mode, 
      you have to select the desired photometric function.

When returning to the highest level of the menu (i.e. the MDF-file) you will
see that the third selection point has been changed according to your input of
PHO_FUNC in this menu.

.VARI  GECALDIR
Directories with geom. Calib. files

Default Logicals/Environments :

HRSC_GEOCAL_DIR  -  Directory with two complete sets of 
	      	    geometric calibration files for HRSC
WAOSS_GEOCAL_DIR -  Directory with two complete sets of 
	      	    geometric calibration files for WAOSS
 

.VARI  GECALDAT	
Creation date of the geometric calibration files

Default Logicals/Environments :
	HRSC_GEOCAL_DATE
	WAOSS_GEOCAL_DATE

.VARI  BSPFILE
Trajectories data, SP-kernels

Default Logicals/Environments :
	HWSPICE_BSP

.VARI  SUNFILE
Ephemeris data of the Sun and planets

Default Logicals/Environments :
	HWSPICE_SUN

.VARI  BCFILE
Attitude data, C-kernels

Default Logicals/Environments :
	HWSPICE_BC

.VARI  TSCFILE
Clock, SCLK-kernels

Default Logicals/Environments :
	HWSPICE_TSC

.VARI  TIFILE
Instrument data, I-kernel

Default Logicals/Environments :
	HWSPICE_TI

.VARI  TPCFILE
Planetary constants, PC-kernels

Default Logicals/Environments :
	HWSPICE_TPC

.VARI  TLSFILE
Leapseconds, LS-kernel

.VARI PHO_PAR_FILE
This is a (dummy) name of a IBIS2-file containing parameters for 
some photometric functions.

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI DEN_SOIL
Specific volume density of the soil.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI IRV_EXP1
Parameter of the Irvine photometric function.

.VARI IRV_EXP2
Parameter of the Irvine photometric function.

.VARI INC_ANG
Incidence angle in degree.

.VARI EM_ANG
Emission angle in degree.

.VARI PHAS_ANG
Phase angle in degree.

.VARI SAVE_PAR
This is the name for the TAE-parameter file. The default name is TPHOPDF.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

.end
$!-----------------------------------------------------------------------------
$ create tphopdf.mdf
.TITLE
VICAR demonstrationprogram  '&"pho_PROC_NAME"'

.proc &"pho_PROC_NAME"_general  
Enter general input parameters and select function of the third menu-point
(type RUN when done)

.proc &"pho_PROC_NAME"_spice
Enter dummy SPICE parameters.
(type RUN when done)
This point has to be addapted to HRSC/WAOSS application programs

.proc pho_&PHO_FUNC_type
Enter parameters for the "&PHO_FUNC_type" photometric function 
(type RUN when done)

.proc common_save_par
Enter the name for the par-file where you want to save your parameters
(type RUN when done)

.proc common_proc_done
Run main application program '&"pho_PROC_NAME"'



.help
Name of Program:	TPHOPDF
	
PURPOSE:	TPHOPDF tests and demonstrates the usage of menu-driven 
		input of photometric and other grouped parameters.
	
FUNCTION:

TPHOPDF is a program to test and demonstrate the usage of menu-driven input of 
the large number of different PDF parameters related to photometric calculus 
into the main program (including their various permitted ranges and default 
settings).  

The MDF is used to organize PDF parameters into different "menus".  The goal of 
such menus is to organize input parameters into parameter groups related to 
specific topics (e.g. I/O-parameters, photometric parameters, SPICE parameters)
in order to facilitate the preparation and the execution of a program to the 
user.

This programs uses most of the sub PDFs delivered by the PHOPDF.COM.  

This testo program shows to the software user how to
	-navigate through a menu;
	-switch between the interactive operation modes of TAE (i.e. Command  
	  Line Mode [TCL], Tutor Mode, and Menu Mode);
	-save the parameter set for later re-processing;
	-delete temporary parameter files.

This program demonstrates to the software developer how to
	-integrate the sub PDFs of PHOPDF into their main program
	-set the different defaults for parameters (with different or the same 
	  name) related to different photometric functions for a tutor-session 
	  or the batch mode;

The programmers are welcome to cut out and vary the tphopdf.pdf 
tphopdf_general.pdf, tphopdf_spice.pdf and the tphopdf.pdf for use 
in their own programs using other defaults, additional parameters, and 
different or additional submenues (see also the demonstration program PHODEM). 
But be carefully for creating names for global variables, because these global
defintions are for the hole VICAR-session.  Note that the default values of the 
sub-PDF's (e.g. of the photometry menus from PHOPDF) will be overwritten by 
the default values of the main-pdf (in this case by TPHOPDF.PDF).

TPHOPDF simulates a program to read the parameters from an IBIS2 parameter file 
(see the pho_par_file package) and for reading MARS94/96-SPICE informations. 
However, this is just a simulation.  There is no real IBIS2-processing nor real 
SPICE reading done in this demo!


EXECUTION:

There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  In this demo program, the menu points have the following 
(dummy/simulated!) meanings:

1. Select the first menu point to input the general parameters for a program
   such as the names of input/output images, the illumination conditions, and 
   so on.
2. In the second menu point you can input (dummy) parameters
   related to SPICE (navigation data, pointing data, instrument data, etc.)
3. In this case, the PDF pertaining to this menu point and the name of this 
   menu point are changing depending on your input of the parameter PHO_FUNC
   in the first menu point.
   There are a few special entries of this parameter:
   - to run the program without using a photometric function you have
     to select PHO_FUNC = "NONE",
   - to read in the photometric function and its associated parameters from a 
     photometric parameter IBIS2-file you have to select PHO_FUNC = "PAR_FILE", 
     and
   - to enter photometry parameters by yourself using the tutor mode
     you have to select the desired photometric function.
4. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or the 'name 
					of the application procedure .par' as 
					it is given by the parameter 'save_par'
5. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.


HELPS :

- You will get the common help contained in the ".mdf" file (tphopdf.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (tphopdf.pdf)  
  by processing of "help-help" applied to the program (should be verry 
  similary).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.



OPERATION:

Although the main purpose of this program is to test and demonstrate the use of 
menus in VICAR programs, the main part of the test program TPDOPDF actually
- reads in the photometric parameters and the illumination conditions,
- computes the bidirectional reflectance, the photometric function and the 
  photometric correction factor to change radiance values from measured viewing 
  and illumination conditions to new artificial target viewing and illumination 
  conditions.  

No image processing is being done.  Results are written to the screen.



SUBROUTINES REQUIRED TO RUN PROGRAM:	pho_routines package


INCLUDE FILES REQUIRED TO RUN PROGRAM:	pho.h, pho.fin
					pho_global.pdf


VICAR PARAMETERS:

NOTE: All parameters (including those from the photometry subPDFs) have to be 
defined in the mainPDF (TPHOPDF.pdf).

Name		Type		Description

inp		string		File name of the input image (dummy)
 						
out		string		File name for the output image(dummy)
						
PHO_FUNC	string		Photometric function :
				This parameter selects the menu point for 
				input the photometry task:
				1. to run the program without using a 
				   photometric function, you 
				   have to select "NONE"'
				2. to read in the photometric function 
				   and its associated parameters from 
				   a photometric  parameter file, you have to  
				   select "PAR_FILE" and
				3. to put in the parameters by yourself from 
				   the tutor mode, you have to select the 
				   desired photometric function.

				When returning to the highest level of the 
				menu (i.e. the MDF-file) you will see that 
 				the third selection point has been changed 
				according to your input of PHO_FUNC .

GECALDIR	string		Directories with geom. Calib. files 	
				Default Logicals/Environments :
				HRSC_GEOCAL_DIR - Directory with two complete 
					          sets of geometric calibration 
					          files for HRSC 
				WAOSS_GEOCAL_DIR - Directory with two complete 
					          sets of geometric calibration 
					          files for WAOSS 
						
GECALDAT	string		Creation date of the geometric calibration files
				Default Logicals/Environments :
					HRSC_GEOCAL_DATE
					WAOSS_GEOCAL_DATE

BSPFILE		STRING		Trajectory data, SP-kernels
				Default Logicals/Environments :
					HWSPICE_BSP

SUNFILE		string		Ephemeris data of the Sun and planets 
				Default Logicals/Environments :
					HWPICE_SUN

BCFILE		string		Attitude data, C-kernels
				Default Logicals/Environments :
					HWSPICE_BC

TSCFILE		string		Clock, SCLK-kernels
				Default Logicals/Environments :
					HWSPICE_TSC

TIFFILE		string		Instrument data, I-kernel
				Default Logicals/Environments :
					HWSPICE_TI

TPCFILE		string		Planetary constants, PC-kernels
				Default Logicals/Environments :
					HWSPICE_TPC

TLSFILE		string		Leapseconds, LS-kernel
				Default Logicals/Environments :
					HWSPICE_TLS

PHO_PAR_FILE	string		Name of a IBIS2-file containing parameters for 
				some photometric functions.

ALBEDO		real		Albedo -  valid for the Lambert and Minnaert 
				photometric functions.

EXPONENT	real		Exponent - the geometrical constant k of the 
				Minnaert photometric function.

A_VEVERKA	real		Parameter of the Veverka, Squyres-Veverka 
				and Mosher photometric functions.

B_VEVERKA	real		Parameter of the Veverka, Mosher, 
				Squyres-Veverka and Buratti photometric 
				functions.

C_VEVERKA	real		Parameter of the Veverka, Mosher, 
				Squyres	Veverka and Buratti photometric 
				functions.

D_VEVERKA	real		Parameter of the Veverka, Mosher, 
				Squyres-Veverka and Buratti photometric 
				functions.

E_BURATTI	real		Buratti's parameter for modification of the 
				Veverka photometric function.

MO_EXP1		real		Modification of the coefficient k in the 	
				Minnaert part of Mosher's photometric function 
				(goes along with MO_EXP2).

MO_EXP2		real		Modification of the coefficient k in the 
				Minnaertpart of Mosher's photometric function 
				(goes along with MO_EXP1).

DEN_SOIL	real		Specific volume density of the soil.

W_SOIL		real		Single-scattering albedo of the soil particles. 
				It characterizes the efficiency of an average 
				particle to scatter and absorb ligth.
				One of the classical Hapke parameter.

HG1_SOIL	real		Parameter of the first term of the 
				Henyey-Greenstein soil particle phase function. 
						
HG2_SOIL	real		Parameter of the second term of the 
				Henyey-Greenstein soil particle phase function.

HG_ASY_SOIL	real		Asymmetry parameter (weight of the two terms 
				in the Henyey-Greenstein soil phase function).

LE1_SOIL	real		Parameter of the first term of the 
				Legendre-Polynomial soil particle phase 
				function.

LE2_SOIL	real		Parameter of the second term of the 
				Legendre-Polynomial soil particle phase 
				function.

H_SHOE		real		One of the classical Hapke parameter.
				Parameter which characterizes the soil 
				structure in the terms of porosity, 
				partparticle-size distribution, and rate of 
				compaction with depth (angular width  
				of opposition surge due to shadowing).

B_SHOE		real		One of the classical Hapke parameter.
				Opposition magnitude coefficient. The total 
				amplitude of the opposition surge due to 
				shadowing. It  is the ratio of the light 
				scattered from near the illuminated surface of 
				the particle to the total amount of light 
				scattered at zero phase :
				B_SHOE=S(0)/(W_SOIL*p(0))
					with p(0) - soil phase function
					S(0) - opposition surge amplitude 
					       term which characterizes the 
					       contribution of light scattered 
					       from near the front surface of 
					       individual particles at zero 
						phase.
				For a true, shadow-hiding opposition effect, 
				0<=B_SHOE<=1.
				However, there are several other phenomena that 
				may also cause a surge in brightness at small 
				phase angles. T
				These including the folllowing:
				1) The coherent backscatter or weak photon 
				   localisation due to multiply scattered light.
				2) An single-particle opposition effect caused 
				   by complex porous agglomerates (soil phase 
				   function )
				3) Glory caused by sperical particles ( soil 
				   phase function )
				4) Internal reflections of transparent particles
				   ( soil phase function )
   				These various phenomena may be large enough to 
				increase the opposition surge by more than a 
				factor of 2. This possibility may be taken into 
				account by allowing B_SHOE to be greater than 1.

H_CBOE		real		Parameter of the coherent backscattering 
				( angular width of the opposition surge due to 
				multiply scattered light):
				H_CBOE=lambda/(2*pi*L)
					lambda - wavelength
					L - the free path of the phonon in the 
					    medium

B_CBOE		real		Opposition magnitude coefficient of the coherent
				backscattering (height of opposition surge due 
				to multiply scattered light).

THETA		real		Average topographic slope angle of surface 
				roughness at subresolution scale.
				One of the classical Hapke parameter.

COOK		real		Parameter of the Cook's modification of the old 
				Hapke function.

TAU_ATM		real		Optical depth of the atmosphere.

W_ATM		real		Single scattering albedo of the atmospheric 
				aerosols.

HG1_ATM		real		Parameter of the first term of the 
				Henyey-Greenstein atmospheric phase function.

IRV_EXP1	real		Parameter of the Irvine photometric function.

IRV_EXP2	real		Parameter of the Irvine photometric function.

INC_ANG		real		Incidence angle in degree.

EM_ANG		real		Emission angle in degree.

PHAS_ANG	real		Phase angle in degree.

SAVE_PAR	string		This is the name for the TAE-parameter file. 
				The default name is TPHOPDF.PAR.  A 
				user-specified name can be given to that file. 
				This is similar to the SAVE command in the Tutor
				Mode.



PARAMETERS SORTED BY SUB-PDFS :

Name of PDF file	PDF parameters	default	comments
(will appear as a	within this		setting	
separate sub-menu)	particular sub-menu			


1) GENERAL INPUT / OUTPUT :
	
tphopdf_general	 	INP

			OUT

			PHO_FUNC	MINNAERT The photometric function
						(see PHOPDF)
									
						Permitted values :

						NONE, 
						PAR_FILE, 
						LAMBERT, 
						MINNAERT, 
						IRVINE, 
						VEVERKA, 
						BURATTI1, 
						BURATTI2, 
						BURATTI3, 
						MOSHER, 
						LUMME_BOWEL_HG1, 
						HAPKE_81_LE2, 
						HAPKE_81_COOK, 
						HAPKE_86_HG1, 
						HAPKE_86_HG2, 
						HAPKE_86_LE2, 
						HAPKE_HG1_DOM, 
						REGNER_HAPKE_HG1, 
						ATMO_CORR_REGNER
				INC_ANG
				EM_ANG
				PHAS_ANG


2) SPICE PARAMETERS :
dummy parameters and logical/environments as proposed for the MARS94/96 mission)
						
tphopdf_spice			GECALDIR	M94GEOCAL
				GECALDAT	HRSC_GEOCAL_DATE
				BSPFILE		HWSPICE_BSP
				SUNFILE		HWSPICE_SUN
				BCFILE		HWSPICE_BC
				TSCFILE		HWSPICE_TSC
				TIFFILE		HWSPICE_TI
				TPCFILE		HWSPICE_TPC
				TLSFILE		HWSPICE_TLS


3) PHOTOMETRY MENU :

Note: the subPDFs pertaining to photometry are deliverd to the VICAR system by 
PHOPDF.  They may not be changed, however, the default values may be set, reset 
or overwritten by the mainPDF (TPHOPDF in this case).  All PDF parameters must 
be defined in the main program's PDF in oder to access the different submenues 
correctly.  

actuell third menu	Name		default	Comment

PHO_PAR_FILE		PHO_PAR_FILE		parameters from IBIS 
						pho_par_file

NONE			none			do not apply any 
						photometric processing

LAMBERT			ALBEDO		1.0

MINNAERT		ALBEDO		1.0
			EXPONENT	0.6

IRVINE			EXPONENT	0.9
			IRV_EXP1	0.118
			IRV_EXP2	0.0039

VEVERKA			A_VEVERKA	0.997
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14

BURATTI1		ALBEDO		0.5
			B_VEVERKA	0.6
			E_BURATTI	0.14

BURATTI2		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
 			E_BURATTI	0.14

BURATTI3		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			E_BURATTI	0.14

MOSHER			A_VEVERKA	0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			MO_EXP1		0.5
			MO_EXP2		--

LUMME_BOWEL_HG1	W_SOIL	0.3
			H_SHOE		0.06
			DEN_SOIL	0.8
			THETA		20
			HG1_SOIL	-0.26

HAPKE_81_LE2		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_81_COOK		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3
			COOK		0.9

HAPKE_86_HG1		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		15.0
			HG1_SOIL	-0.26

HAPKE_86_HG2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.29
			HG2_SOIL	0.39
			HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_HG1_DOM		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		1.0
			THETA		20.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			B_CBOE		1.0

REGNER_HAPKE_HG1	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35

ATMO_CORR_REGNER	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35


4) PAR-FILE NAME

pho_save_par		SAVE_PAR		Name of the TAE-parameter file 
						containing all parameters 
						needed to running the program.
                                                default:
						name of the main program 
						with the extention ".par"

5) RUN  MAIN PROGRAM

pho_proc_name		-			runs the program


GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

	Name		Type			Description

	PHO_FUNC_type 	string			It containes the names of the 
						valid photometric functions (to 
						pass into the menu).

	pho_PROC_NAME 	string			Name of the main program 



	

BACKGROUND AND REFERENCES :	TAE Command Language (TCL)
				Programmers's Manual
				Version 5.1
				NASA Goddard Space Flight Centre, 1991


SOFTWARE PLATFORM :		VICAR 13, TAE 5.2
				(VMS/AXP/SUNOS/SOLARIS)

HARDWARE PLATFORM :		No particular hardware required;
				tested on VMS/AXP/SUNOS/SOLARIS

PROGRAMMING LANGUAGE :		TCL , C	

HISTORY:			Friedel Oschuetz, July '94, original

COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



.end
$!-----------------------------------------------------------------------------
$ create tphopdf_general.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! TPHOPDF_GENERAL.PDF
!
! This is the PDF for the first menu point of the MDF file.
! In this PDF file general parameters are defined like the names of 
! input/output files.
!
!-----------------------------------------------------------------------------

	!**********************************************************************
	! The global variables PHO_FUNC_type (and $MENUS) will be used in this
	! procedure, so they have to be declared here.
	! The global PHO_FUNC_type (the desired photometric function) will be
	! used to change the third menu point of the MDF file (see above)
	! according to the input to PHO_FUNC in this PDF.
	! The global $MENUS keeps the active stack of Menu Definition File
	! names. $MENUS(1) is the root menu name, $MENUS(2) the menu selected
	! from the root menu, and so on. The current menu stack can be
	! displayed with "DISPLAY $MENUS". Since there is only one MDF in
	! this demo, it doesn't make too much sense to have that global
	! included here. It is just referenced for completeness.
	!**********************************************************************

	refgbl PHO_FUNC_type 
!	refgbl pho_PROC_NAME
	refgbl $menus

	parm MAIN_PROC_NAME string

	procedure name=tphopdf_general_sub help=*

		! dummy inputs :

		parm inp	type=(string,32) count=0:1 default=--
		parm out	type=(string,32) count=0:1 default=--

		! photometric functions :

		parm PHO_FUNC type=(string,32) count=1 	+
			valid = (			+
				NONE,			+
				PAR_FILE,		+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default="&PHO_FUNC_type"

		! illumination conditions :

    		parm INC_ANG	real count=0:1 +
					default=--
    		parm EM_ANG 	real count=0:1 +
					default=--
    		parm PHAS_ANG 	real count=0:1 +
					default=--



	body

	!*******************************************************************
	! "pho_global.pdf" is being executed in the following line. 
	! In this PDF, two globals are defined (PHO_FUNC_type, pho_PROC_NAME)
	!*******************************************************************
 
	pho_global PHO_FUNC_type="&PHO_FUNC"

	end-proc
body

	if (_tutor=1)

	   restore-parm common_proc_name.par

 	   tutor tphopdf_general_sub 				+
			|restore=&"MAIN_PROC_NAME"_general.par 	+
		 	 save=&"MAIN_PROC_NAME"_general.par|
	else
	   write " ************************************************"
	   write " "
	   write " This program works only when run from tutor mode"
           write " of other programs using photometric calculus."
	   write " "
	   write " ************************************************"
	end-if

end-proc

.TITLE
VICAR sub-menu TPHOPDF_GENERAL

.HELP
PURPOSE:
This menu point is dedicated to input general parameters for the program such 
as the names of input/output images the illumination conditions, and so on.

NOTE : The parameter PHO_FUNC is in need of the photometric task :  

This parameter selects the menu point for input the photometry task:
   1. to run the program without using a photometric function, you have 
      to select "NONE"'
   2. to read in the photometric function and its associated parameters
      from a photometric parameter file, you have to select "PAR_FILE" and
   3. to put in the parameter by yourself from the tutor mode, 
      you have to select the desired photometric function.

When returning to the highest level of the menu 
(i.e. the &"MAIN_PROC_NAME".MDF-file) you will see that the third selection 
point has been changed according to your input of PHO_FUNC in this menu.


.PAGE
Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)



.LEVEL1

.VARI inp
Input image

.VARI out
Output image

.VARI PHO_FUNC
Photometric function type

.VARI INC_ANG
incidence angle

.VARI EM_ANG
emission angle

.VARI PHAS_ANG
phase angle


.LEVEL2

.VARI inp
File name of the input image

.VARI out
File name for the output image

.VARI PHO_FUNC
Photometric function :

This parameter selects the menu point for input the photometry task:
   1. to run the program without using a photometric function, you have 
      to select "NONE"'
   2. to read in the photometric function and its associated parameters
      from a photometric parameter file, you have to select "PAR_FILE" and
   3. to put in the parameter by yourself from the tutor mode, 
      you have to select the desired photometric function.

When returning to the highest level of the menu (i.e. the MDF-file) you will
see that the third selection point has been changed according to your input of
PHO_FUNC in this menu.

.VARI INC_ANG
Incidence angle in degree.

.VARI EM_ANG
Emission angle in degree.

.VARI PHAS_ANG
Phase angle in degree.



.end
$!-----------------------------------------------------------------------------
$ create tphopdf_spice.pdf
procedure option=selftutor help=*

parm MAIN_PROC_NAME string

!-----------------------------------------------------------------------------
!
! This is the PDF for the second menu point of the MDF.
! In this PDF, the user is asked for parameters belonging to a specific
! topic. In this pho_routines-test, the topic is SPICE, i.e. in this PDF we 
! could specify SPICE-kernels ad directories with the help of 
! LOGICALs/ENVIRONMENTs. In the test, however, it's all dummy variables
! similar to HRSC/WAOS.
!
!-----------------------------------------------------------------------------

	PARMSET name=tphopdf_spice_sub help=*

	parm GECALDIR	type=(string,80) count = 0:2	+
			default = (HRSC_GEOCAL_DIR, WAOSS_GEOCAL_DIR)
	parm GECALDAT	type=(string,32) count = 0:1	+
			default = HRSC_GEOCAL_DATE
	parm BSPFILE	type=(string,32) count = 0:3	+
			default = HWSPICE_BSP
	parm SUNFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_SUN
	parm BCFILE	type=(string,32) count = 0:6	+
			default = HWSPICE_BC
	parm TSCFILE	type=(string,32) count = 0:6	+
			default = HWSPICE_TSC
	parm TIFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TI
	parm TPCFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TPC
	parm TLSFILE	type=(string,32) count = 0:1	+
			default = HWSPICE_TLS

	END-PROC

body

	if (_tutor=1)

	   restore-parm common_proc_name.par


	   !***************************************************************   
	   ! There are always two ways to call a procedure:
	   ! 1) "tutor proc_name"
	   ! 2) "VICAR> proc_name" (i.e. from the command line)
	   !
	   ! As described above, an initial tutor request sets the variable
	   ! _TUTOR to 1 in an SELFTUTOR procedure. Hence, if this PDF gets
	   ! an initial tutor request the Tutor Mode of the procedure 
	   ! '&"MAIN_PROC_NAME"_spice_sub' is entered: "tutor  
	   ! '&"MAIN_PROC_NAME"_spice_sub"' (see below).
	   ! There, we are actually asked for the parameters. Since this
	   ! procedure '&"MAIN_PROC_NAME"_spice_sub' does not have anything to 
	   ! execute in it's body, we have to terminate the procedure execution 
	   ! of '&"MAIN_PROC_NAME_spice' by the command RETURN (see below) to  
	   ! return to the Menu Mode.
	   !***************************************************************

	   tutor tphopdf_spice_sub 	       		+
		|restore=&"MAIN_PROC_NAME"_spice.par,	+
		 save=&"MAIN_PROC_NAME"_spice.par|
	   return

	else
	   write " "
	   write "*********************************************************"
	   write " "
	   write " This program is only intended to be run "
	   write " as tutor from other programs."
	   write " "
	   write "*********************************************************"
	   write " "
	end-if

end-proc

.Title
Sub-menu TPHOPDF_SPICE 
(for VICAR mdf/pdf-test and demonstration-Programs &"MAIN_PROC_NAME")

.help
In this sub-menu it is possible to specify the names of additional parameters,
e.g. SPICE Directories and Kernels. In our example, dummy parameters similar to 
HRSC/WAOSS can be specified.

This program is only intended to be run as tutor from other programs.
When run from Command Mode, we get an error message:

*********************************************************"

 This program is only intended to be run 
 as tutor from other programs.

*********************************************************"

Type "run" when you have specified the dummy parameters.
.page
Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)



.level1

.var  GECALDIR
Directories with 
geom. Calib. files	

.var  GECALDAT	
Creation date of the 
geom. Calib. files

.var  BSPFILE
Trajectories, SP-kernels

.var  SUNFILE
Ephemeris of Sun and planets

.var  BCFILE
Attitude, C-kernels

.var  TSCFILE
Clock, SCLK-kernels

.var  TIFILE
Instrument data, I-kernel

.var  TPCFILE
Planetary constants, PC-kernels

.var  TLSFILE
Leapseconds, LS-kernel


.level2

.var  GECALDIR
Directories with geom. Calib. files

Default Logicals/Environments :

GEOCAL_DIR -   	Directory with twoa complete sets of 
		geometric calibration files

.var  GECALDAT	
Creation date of the geometric calibration files

.var  BSPFILE
Trajectories data, SP-kernels

.var  SUNFILE
Ephemeris data of the Sun and planets

.var  BCFILE
Attitude data, C-kernels

.var  TSCFILE
Clock, SCLK-kernels

.var  TIFILE
Instrument data, I-kernel

.var  TPCFILE
Planetary constants, PC-kernels

.var  TLSFILE
Leapseconds, LS-kernel


.end
$!-----------------------------------------------------------------------------
$ create tstphopdf.pdf
procedure
    PARMSET name= tstphopdf_parms
    	parm PHO_FUNC	string 	count=0:1 	  +
			valid=(	LAMBERT,	  +
				MINNAERT,	  +
				IRVINE,		  +
				VEVERKA,	  +
				BURATTI1,	  +
				BURATTI2,	  +
				BURATTI3,	  +
				MOSHER,		  +
				LUMME_BOWEL_HG1,  +
				HAPKE_81_LE2,	  +
				HAPKE_81_COOK,	  +
				HAPKE_86_HG1,	  +
				HAPKE_86_HG2,	  +
				HAPKE_86_LE2,	  +
				HAPKE_HG1_DOM,	  +
				REGNER_HAPKE_HG1, +
				ATMO_CORR_REGNER) +
			default=MINNAERT

    	parm ALBEDO 	real count=0:1 +
			valid=(0:1)	default=1.0
    	parm EXPONENT 	real count=0:1 +
			valid=(0:1)	default=0.5
    	parm A_VEVERKA 	real count=0:1 +
					default=1.0
    	parm B_VEVERKA 	real count=0:1 +
					default=0.005
    	parm C_VEVERKA 	real count=0:1 +
					default=0.5
    	parm D_VEVERKA 	real count=0:1 +
					default=0.05 
    	parm MO_EXP1 	real count=0:1 +
					default=0.05
    	parm MO_EXP2 	real count=0:1 +
					default=.005
    	parm E_BURATTI 	real count=0:1 +
					default=2.0
    	parm DEN_SOIL 	real count=0:1 +
					default=0.005
    	parm W_SOIL 	real count=0:1 +
			valid=(0:1)	default=0.7
    	parm HG1_SOIL 	real count=0:1 +
					default=5.0
    	parm HG2_SOIL 	real count=0:1 +
					default=3.0
    	parm HG_ASY_SOIL real count=0:1 +
					default=1.0
    	parm LE1_SOIL 	real count=0:1 +
					default=3.0
   	parm LE2_SOIL 	real count=0:1 +
					default=2.0
    	parm H_SHOE 	real count=0:1 +
					default=0.4
    	parm B_SHOE 	real count=0:1 +
					default=0.22
    	parm H_CBOE 	real count=0:1 +
					default=0.3
   	parm B_CBOE 	real count=0:1 +
					default=0.1
    	parm THETA 	real count=0:1 +
					default=10.0
   	parm COOK 	real count=0:1 +
					default=0.9
    	parm TAU_ATM 	real count=0:1 +
					default=0.4
    	parm W_ATM 	real count=0:1 +
			valid=(0:1)	default=0.9
    	parm HG1_ATM 	real count=0:1 +
					default=3.0
    	parm IRV_EXP1 	real count=0:1 +
					default=0.8
    	parm IRV_EXP2 	real count=0:1 +
					default=0.08
    	parm INC_ANG	real count=0:1 +
					default=10.0
    	parm EM_ANG 	real count=0:1 +
					default=50.0
    	parm PHAS_ANG 	real count=0:1 +
					default=55.0
   end-proc

body

tstphopdf_parms |save=tstphopdf_parms|
tphopdf |restore=tstphopdf_parms|	PHO_FUNC=LAMBERT


end-proc
$ Return
$!#############################################################################
