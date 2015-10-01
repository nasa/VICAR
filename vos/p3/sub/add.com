$!****************************************************************************
$!
$! Build proc for MIPL module add
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:43
$!
$! Execute by entering:		$ @add
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
$ write sys$output "*** module add ***"
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
$   if F$SEARCH("add.imake") .nes. ""
$   then
$      vimake add
$      purge add.bld
$   else
$      if F$SEARCH("add.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake add
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @add.bld "STD"
$   else
$      @add.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create add.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack add.com -
	-s add.c -
	-i add.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create add.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>

/* g: see mve.c */

add_(numargs, dcode, nelem, avec, bvec, ainc, binc)
     int *numargs, *dcode, *nelem, *avec, *bvec, *ainc, *binc;
{
  int i, j;
  
  /* vectors */
  unsigned char *bytein,   *byteout;
  short         *halfin,   *halfout;
  long          *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  /* increments */
  int in_inc, out_inc;
  
  switch (*numargs) {
  case 4:
    in_inc = 1;
    out_inc = 1;
    break;
  case 5:
    in_inc = *ainc;
    out_inc = 1;
    break;
  case 6:
    in_inc = *ainc;
    out_inc = *binc;
    break;
  default:
    printf("Wrong number of arguments to add\n");
    zabend();
  }
  
  switch (*dcode) {
  case -1:
  case 1:
    bytein = (unsigned char *) avec;
    byteout = (unsigned char *) bvec;
    for (i=0; i < *nelem; i++, bytein+=in_inc, byteout+=out_inc) {
      *byteout = *bytein + *byteout;
    }
    break;
  case -2:
  case 2:
    halfin = (short *) avec;
    halfout = (short *) bvec;
    for (i = 0; i <*nelem; i++, halfin+=in_inc, halfout+=out_inc){
      *halfout = *halfin + *halfout;
    }
    break;
  case -3:
    halfin = (short *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i<*nelem ;i++,halfin+=in_inc, byteout+=out_inc){
      *byteout = *halfin + *byteout ;
    }
    break;
  case 3:
    bytein = (unsigned char *) avec;
    halfout = (short *) bvec;
    for (i = 0; i< *nelem; i++, bytein+=in_inc,halfout+=out_inc){
      *halfout = *bytein + *halfout ;
    }
    break;
  case -4:
  case  4:
    fullin = (long *) avec;
    fullout = (long *) bvec;
    for (i = 0; i<*nelem ;i++,fullin+=in_inc,fullout+=out_inc){
      *fullout = *fullin + *fullout;
    }
    break;
  case -5:
    fullin = (long *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i< *nelem; i++,fullin+=in_inc,byteout+=out_inc){
      *byteout = *fullin + *byteout;
    }
    break;
  case 5:
    bytein = (unsigned char *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< *nelem; i++, bytein+=in_inc,fullout+=out_inc){
      *fullout = *bytein + *fullout;
    }
    break;
  case -6:
    fullin = (long *) avec;
    halfout = (short *) bvec;
    for (i = 0;i< *nelem; i++, fullin+=in_inc,halfout+=out_inc){
        *halfout = *fullin + *halfout;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< *nelem; i++, realin+=in_inc,realout+=out_inc){
      *realout = *realin + *realout;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< *nelem; i++, doublein+=in_inc,doubleout+=out_inc){
      *doubleout = *doublein + *doubleout;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< *nelem; i++, realin+=in_inc,realout+=out_inc){
      *realout = *realin + *realout;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< *nelem; i++,realin+=in_inc,doubleout+=out_inc){
      *doubleout = *realin + *doubleout;
    }
    break;
  default:
    printf("Invalid DCODE value: %d\n", *dcode);
    zabend();
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create add.imake

#define SUBROUTINE add

#define MODULE_LIST add.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
