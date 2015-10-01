$!****************************************************************************
$!
$! Build proc for MIPL module interpolate
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:46
$!
$! Execute by entering:		$ @interpolate
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
$ write sys$output "*** module interpolate ***"
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
$   if F$SEARCH("interpolate.imake") .nes. ""
$   then
$      vimake interpolate
$      purge interpolate.bld
$   else
$      if F$SEARCH("interpolate.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake interpolate
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @interpolate.bld "STD"
$   else
$      @interpolate.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create interpolate.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack interpolate.com -
	-s interpolate.c -
	-i interpolate.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create interpolate.c
$ DECK/DOLLARS="$ VOKAGLEVE"


/*
 * interpolate input y's to spacing of outx's
 */
  /*
     inx and iny contain original points" x"s and y"s. outx contains the
     x"s for which the interpolated y"s are to be found.  read the original
      y"s and compute the interpolations
  */

void interpolate (inx,iny,inpts,outx,outy,outpts)
 int inpts, outpts;
 float inx[], iny[], outx[], outy[];
{
   int  i;
   float  rmin = 0.001;
   float *offsetinx, *offsetiny, *offsetyderiv, *yderiv;
  
   yderiv = (float *)get_space(inpts * sizeof(*yderiv));
  
   /*
      spline needs unit offset vectors to work
   */
   offsetinx = inx - 1;
   offsetiny = iny - 1;
   offsetyderiv = yderiv - 1;
  
   /*
      get the y derivatives for the input array
   */
   nr_spline(offsetinx,offsetiny,inpts,0.0,0.0,offsetyderiv);

   for (i = 0; i < outpts; i++) {
     if (offsetinx[1]<=outx[i] && outx[i]<=offsetinx[inpts]) {
        nr_splint (offsetinx,offsetiny,offsetyderiv,inpts,outx[i],&outy[i]);
        outy[i] = (outy[i] < rmin) ? 0.0 : outy[i];
     }
     else
        outy[i] = 0.0;
   }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create interpolate.imake

#define SUBROUTINE interpolate

#define MODULE_LIST interpolate.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
