$!****************************************************************************
$!
$! Build proc for MIPL module refl
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:56
$!
$! Execute by entering:		$ @refl
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
$ write sys$output "*** module refl ***"
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
$   if F$SEARCH("refl.imake") .nes. ""
$   then
$      vimake refl
$      purge refl.bld
$   else
$      if F$SEARCH("refl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake refl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @refl.bld "STD"
$   else
$      @refl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create refl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack refl.com -
	-s refl.c -
	-i refl.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create refl.c
$ DECK/DOLLARS="$ VOKAGLEVE"

#include <stdio.h>
#include <math.h>
#include "const.h"


/*---------------------------------------------------------------------------*/
/* read in wavelength-reflectance value pairs from the user specified file   */
/* there should be one pair per line; tab or space delimited                 */

int zreadref (wave, refl)
     float  wave[1000], refl[1000];
{
  
  int  eof, npts, status, count;
  char reflfile[64], inbuf[LineLength];
  FILE *rfile;
  
  npts = 0;
  
  status = zvp ("REFLFILE", reflfile, &count);
  
  rfile = fopen(reflfile,"r");
  
  while (getline_vicar(inbuf, LineLength, rfile, &eof) >= 0 && !eof)
    if (sscanf(inbuf, "%f %f", &wave[npts], &refl[npts]) == 2)
      npts++;
  
  
  fclose(rfile);
  
  return npts;
}


/*---------------------------------------------------------------------------*/
/* use default values for reflectance if the user doesn"t specify anything   */

int zbldref(salb, wave, refl)
     float  salb;
     float  wave[1000], refl[1000];
{
  
  wave[0] = 0.1;
  wave[1] = 100.0;
  refl[0] = salb;
  refl[1] = salb;
  
  return 2;          /* npts == 2, is number of values in the arrays */
}

/*---------------------------------------------------------------------------*/
/* interpolate input or defaulted reflectance values to produce an array     */
/* with spacing and step size defined by user parms v1, v2 and dv            */

int zinterp_refl (start_wave, end_wave, wave_step, in_pts, wave, refl, specalb)
     float  start_wave, end_wave, wave_step;
     int in_pts;
     float  wave[1000], refl[1000], specalb[8000];
{
  int i, j, out_pts;
  float  ratio, wave_i;
  
  /*----- convert from wavelength to wavenumbers (micrometers to cm -1 -----*/
  for (j = 0; j < in_pts; j++)
    wave[j] = 10000.0 / wave[j];
  
  out_pts = (int ) ceil((end_wave - start_wave + wave_step) / wave_step);
  
  j = in_pts - 1;
  wave_i = start_wave;
  
  for (i = 0; i < out_pts; i++) {
    
    while (wave[j] < wave_i && j > 0)
      j -= 1;
    
    ratio = (wave[j] - wave_i) / (wave[j] - wave[j+1]);
    specalb[i] = ratio * refl[j+1] + (1 - ratio) * refl[j];
    
    wave_i += wave_step;
  }
  return out_pts;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create refl.imake

#define SUBROUTINE refl

#define MODULE_LIST refl.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
