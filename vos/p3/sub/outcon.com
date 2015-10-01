$!****************************************************************************
$!
$! Build proc for MIPL module outcon
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:55
$!
$! Execute by entering:		$ @outcon
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
$ write sys$output "*** module outcon ***"
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
$   if F$SEARCH("outcon.imake") .nes. ""
$   then
$      vimake outcon
$      purge outcon.bld
$   else
$      if F$SEARCH("outcon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake outcon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @outcon.bld "STD"
$   else
$      @outcon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create outcon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack outcon.com -
	-s outcon.c -
	-i outcon.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create outcon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
static char sccsNewoutcon[] = "@(#) outcon.c 2.7 9/14/89 PDS Vicar2";

/* Outcon is a routine which puts a number into a character array
   in ASCII format.

   i.e.
     1  =     061
     20 = 062 060

   note: all constants are in octal format

   Parameters:

   numargs - the number of arguments being passed to outcon.
   input   - the value to put into the array.
   outloc  - a pointer to the right-most location in the array
             where the formatted number will be put.
   nchar   - the length allocated for the formatted number in the array
   nfract    - a nfract for printing real numbers:
               negative : format in E style .xxxxExx
	       zero     : format in I style  xxxx
	       positive : format in F style  xxxx.xx
 
*/
#include <math.h> /* included for log10(x) - log base 10 of x
		              and pow(x,y) - raises x to the y power */
#include <stdio.h>
#include <string.h>

outcon_(numargs,input,outloc,nchar,nfract)
     /* bridge to outcon from FORTRAN programs */
{
  outcon(numargs,input,outloc,nchar,nfract);
}

doutcon_(numargs,input,outloc,nchar,nfract)
     /* bridge to doutcon from FORTRAN programs */
{
  doutcon(numargs,input,outloc,nchar,nfract);
}


outcon(numargs,input,outloc,nchar,nfract)
int *numargs, *nchar, *nfract;
{
  if (*numargs == 3)
    print_int(input,input,outloc,*nchar);       /* print an integer    */
  else
    print_flt(input,outloc,*nchar,*nfract); /* print a real number */
}

doutcon(numargs,input,outloc,nchar,nfract)
     int *numargs, *nchar, *nfract;
{
  print_dflt(input,outloc,*nchar,*nfract);
}

print_int(sh_input,ln_input,outloc,nchar)

     /* this routine puts an integer into the character array */

int nchar;
short int *sh_input;
long  int *ln_input;
char *outloc;
{
  register i;
  char p_buf[80];
  char *ptr;
  int integer;
  char outbuf[80];

  if (nchar < 0) {
    integer = *sh_input;
    nchar = abs(nchar);
  }
  else
    integer = *ln_input;
  ptr = outloc - nchar + 1;
  
  sprintf(p_buf,"%%%d%s",nchar,"d");
  sprintf(outbuf,p_buf,integer);
  for (i=0;i<strlen(outbuf);i++) 
    ptr[i] = outbuf[i];
}


print_flt(input,outloc,nchar,nfract)

     /* this routine puts a  real number into the character array */
     
int nchar, nfract;
char *outloc;
float *input;
{
  register i;
  char outbuf[80];
  char p_buf[80];
  char format[2];
  char *ptr;
  int temp;

  ptr = outloc - nchar + 1;

  if (nfract < 0) {    /* E format */
    strcpy(format,"e");
    sprintf(p_buf,"%%%d.%d%s",nchar,abs(nfract),format);
    sprintf(outbuf,p_buf,*input);
  }
  else {
    strcpy(format,"f");
    sprintf(p_buf,"%%%d.%d%s",nchar,nfract,format);
    sprintf(outbuf,p_buf,*input);
  }
  for (i=0;i<strlen(outbuf);i++)
    ptr[i] = outbuf[i];
}

print_dflt(input,outloc,nchar,nfract)

     /* this routine puts a  real number into the character array */
     
int nchar, nfract;
char *outloc;
double *input;
{
  register i;
  char outbuf[80];
  char p_buf[80];
  char format[2];
  char *ptr;
  int temp;

  ptr = outloc - nchar + 1;

  if (nfract < 0) {    /* E format */
    strcpy(format,"e");
    sprintf(p_buf,"%%%d.%d%s",nchar,abs(nfract),format);
    sprintf(outbuf,p_buf,*input);
  }
  else {
    strcpy(format,"f");
    sprintf(p_buf,"%%%d.%d%s",nchar,nfract,format);
    sprintf(outbuf,p_buf,*input);
  }
  for (i=0;i<strlen(outbuf);i++)
    ptr[i] = outbuf[i];
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create outcon.imake

#define SUBROUTINE outcon

#define MODULE_LIST outcon.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
