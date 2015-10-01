$!****************************************************************************
$!
$! Build proc for MIPL module fminmax
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:45
$!
$! Execute by entering:		$ @fminmax
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
$ write sys$output "*** module fminmax ***"
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
$   if F$SEARCH("fminmax.imake") .nes. ""
$   then
$      vimake fminmax
$      purge fminmax.bld
$   else
$      if F$SEARCH("fminmax.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fminmax
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fminmax.bld "STD"
$   else
$      @fminmax.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fminmax.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fminmax.com -
	-s fminmax.c -
	-i fminmax.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fminmax.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdlib.h>
#include <string.h>


/*---------------------------------------------------------------------------*/
int fileminmax (unit, format, nl, ns, filemin, filemax,zeroes)
  char *format;    /* format of input file                              */
  int    unit,       /* vicar unit number of file, must be open for read  */
         nl,         /* number of lines of input file                     */
         ns,         /* number of samples of input file                   */
         zeroes;     /* use zeroes as data ?                              */
  double *filemin,   /* returned value - minimum value found              */
         *filemax;   /* returned value - maximum value found              */
{
   int stat;

   if (strcmp(format,"BYTE") == 0)
      stat = byteminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"HALF") == 0)
      stat = halfminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"FULL") == 0)
      stat = fullminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"REAL") == 0)
      stat = realminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"DOUB") == 0)
      stat = doubminmax (unit, nl, ns, filemin, filemax,zeroes);

   return stat;
}


/*---------------------------------------------------------------------------*/
int byteminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   char  *buf;
   int line, samp, status, first=0;
   double value;

   buf = (char *)malloc(sizeof(*buf)*ns);
   memset (buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(char *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free (buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int halfminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   short int  *buf;
   int line, samp, status, first=0;
   double value;

   buf = (short *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(short *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}


/*---------------------------------------------------------------------------*/
int fullminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   int  *buf;
   int line, samp, status, first=0;
   double value;
   
   buf = (int *)malloc(sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(int *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int realminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   float  *buf;
   int line, samp, status, first=0;
   double value;
   
   buf = (float *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(float *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int doubminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   double  *buf, value;
   int line, samp, status, first=0;
   
   buf = (double *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = *(double *)(buf + samp);
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fminmax.imake

#define SUBROUTINE fminmax

#define MODULE_LIST fminmax.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
