$!****************************************************************************
$!
$! Build proc for MIPL module zplanck
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:59
$!
$! Execute by entering:		$ @zplanck
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
$ write sys$output "*** module zplanck ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("zplanck.imake") .nes. ""
$   then
$      vimake zplanck
$      purge zplanck.bld
$   else
$      if F$SEARCH("zplanck.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zplanck
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zplanck.bld "STD"
$   else
$      @zplanck.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zplanck.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zplanck.com -
	-s zplanck.c -
	-i zplanck.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zplanck.c
$ DECK/DOLLARS="$ VOKAGLEVE"

/*---------------------------------------------------------------------------*/
/*   this is a c version of the fortran subroutine planck;                   */
/*   given wavelength and temperature, return radiance by Planck's Law       */


#include <math.h>

double zplanck (wavelength, temperature)
 double  wavelength,                    /* wavelength in microns             */
         temperature;                   /* temperature in degrees Kelvin     */
{

   double  c1   =  3.74151e-16,         /* 'constants'                       */
           c2   =  0.0143879,
           fact =  1.0e-6,
           pi   =  3.14159265;

   double  a, b, radiance;

   if (temperature > 0.0) {
      wavelength = wavelength * fact;       /* convert um to m               */
      a = pow(wavelength,5.0);
      b = exp(c2 / (wavelength*temperature));
      radiance = c1 / (a * (b-1.0));

      radiance = (radiance * fact) / pi;    /* W/(m*m*m) to W/(m*m*sr*um)    */
   }
   else
      radiance=0.0;

   return radiance;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zplanck.imake

#define SUBROUTINE zplanck

#define MODULE_LIST zplanck.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
