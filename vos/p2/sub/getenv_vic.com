$!****************************************************************************
$!
$! Build proc for MIPL module getenv_vic
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:45
$!
$! Execute by entering:		$ @getenv_vic
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module getenv_vic ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to getenv_vic.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("getenv_vic.imake") .nes. ""
$   then
$      vimake getenv_vic
$      purge getenv_vic.bld
$   else
$      if F$SEARCH("getenv_vic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getenv_vic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getenv_vic.bld "STD"
$   else
$      @getenv_vic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getenv_vic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getenv_vic.com -mixed -
	-s getenv_vic.c -
	-i getenv_vic.imake -
	-t tstgetenv_vic.pdf tgetenv_vic_f.f tgetenv_vic_f.imake -
	   tgetenv_vic_f.pdf tgetenv_vic_c.c tgetenv_vic_c.imake -
	   tgetenv_vic_c.pdf -
	-o getenv_vic.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getenv_vic.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* getenv_vic - routine to return the value of an environment variable	*/
/* (Unix) or logical name (VMS).  On VMS, it will return multivalued	*/
/* logical names as a comma-separated list of translations.		*/
/*									*/
/* C call:								*/
/*   value = getenv_vic("NAME");					*/
/*									*/
/* Just like getenv(), the return value is a pointer to a static	*/
/* array.  So, the value must be used or copied before this routine	*/
/* (or getenv()) is called again.  NULL is returned if the name is	*/
/* not found.								*/
/*									*/
/* Fortran call:							*/
/*   call xgetenv_vic(name, value)					*/
/* Both arguments are character*n variables.  A blank string is		*/
/* returned if the name is not found.					*/
/************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"

#if VMS_OS

#include <descrip.h>
#include <lnmdef.h>
#include <ssdef.h>

char *getenv_vic(name)
char *name;
{
    int status, namelen, index;
    unsigned int buffer[32], max_index;
    static char output_buffer[512], temp[132];
    char *table = "LNM$DCL_LOGICAL";

    $DESCRIPTOR(name_desc,name);
    $DESCRIPTOR(table_desc,table);

    table_desc.dsc$w_length = strlen(table);

    name_desc.dsc$w_length = strlen(name);

    max_index = 0;
    buffer[0] = (LNM$_MAX_INDEX << 16) | sizeof(max_index);
    buffer[1] = (unsigned int)&max_index;
    buffer[2] = 0;
    buffer[3] = 0;
    status = sys$trnlnm(0,&table_desc,&name_desc,0,buffer);

    *output_buffer = '\0';

    for (index = 0;index <= max_index;index++) {
	buffer[0] = (LNM$_INDEX << 16) | sizeof(index);
	buffer[1] = (unsigned int)&index;
	buffer[2] = 0;
	buffer[3] = (LNM$_STRING << 16) | sizeof(temp);
	buffer[4] = (unsigned int)temp;
	buffer[5] = 0;
	buffer[6] = (LNM$_LENGTH << 16) | sizeof(namelen);
	buffer[7] = (unsigned int)&namelen;
	buffer[8] = 0;
	buffer[9] = 0;
	status = sys$trnlnm(0,&table_desc,&name_desc,0,buffer);

	if (status != SS$_NORMAL) return((char *)0);
	temp[namelen] = '\0';
	if (*output_buffer == '\0') strcpy(output_buffer,temp);
	else {
	    (void)strcat(output_buffer,",");
	    (void)strcat(output_buffer,temp);
	}
    }
    return(output_buffer);
}

#else

#include <stdlib.h>

char *getenv_vic(name)
char *name;
{
    return getenv(name);
}

#endif


/************************************************************************/
/* Fortran bridge							*/
/************************************************************************/

void FTN_NAME2_(xgetenv_vic,XGETENV_VIC)(char *name, char *value, ZFORSTR_PARAM)
{
    ZFORSTR_BLOCK
    char c_name[512], *c_value;

    zsfor2c(c_name, 511, name, &name, 2, 1, 1, value);

    c_value = getenv_vic(c_name);

    if (c_value == (char *)0)
        c_value = " ";		/* return blanks */

    zsc2for(c_value, 0, value, &name, 2, 2, 2, value);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getenv_vic.imake
#define SUBROUTINE getenv_vic

#define MODULE_LIST getenv_vic.c

#define FTN_STRING
#define USES_ANSI_C

#define P2_SUBLIB

/*#define LIB_LOCAL	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstgetenv_vic.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
refgbl $syschar

! basic test:
tgetenv_vic_c
tgetenv_vic_f

! check different logical name tables in VMS
if ( $syschar(1) = "UNIX" ) return

! set up a process logical name that duplicates an existing system one:
dcl assign random_string clue$history
tgetenv_vic_f

! assign multiple logical names:
dcl assign string1,string2 clue$history
tgetenv_vic_f

! clean up the logical name we borrowed ...
dcl deassign clue$history

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tgetenv_vic_f.f
	include 'VICMAIN_FOR'
	subroutine main44
	character*200 value

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage('this should be a non-existent name:', ' ')
	call xgetenv_vic( 'XYZZY', value)
	call xvmessage( 'XYZZY = '//value, ' ')

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage('this should always return a value:', ' ')
	call xgetenv_vic( 'SPICEKER', value)
	call xvmessage( 'SPICEKER = '//value, ' ')

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage(
	1 'this name has a system value in VMS but none in Unix:',' ')
	call xgetenv_vic( 'CLUE$HISTORY', value)
	call xvmessage( 'CLUE$HISTORY = '//value, ' ')

	return
	end
$!-----------------------------------------------------------------------------
$ create tgetenv_vic_f.imake
#define  PROGRAM   tgetenv_vic_f

#define MODULE_LIST tgetenv_vic_f.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN

/*#define LIB_LOCAL	/* remove on delivery */
$!-----------------------------------------------------------------------------
$ create tgetenv_vic_f.pdf
process help=*
end-proc
.TITLE
VICAR program TGETENV_VIC_F
.HELP
This program is a the Fortran test program for getenv_vic().
.END
$!-----------------------------------------------------------------------------
$ create tgetenv_vic_c.c
#include <stdio.h>
#include "vicmain_c"

void main44()
{
  char strng[201], *x, *getenv_vic(), *strcat();

  zvmessage("this should be a non-existent name:\n","");
  x = getenv_vic("XYZZY");
  if (x == NULL) {
    sprintf( strng, "%s\n", "XYZZY =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "XYZZY = ");
    strcat( x, getenv_vic("XYZZY"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");

  zvmessage("this should always return a value:\n","");
  x = getenv_vic("SPICEKER");
  if (x == NULL) {
    sprintf( strng, "%s\n", "SPICEKER =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "SPICEKER = ");
    strcat( x, getenv_vic("SPICEKER"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");

  zvmessage("'this name has a system value in VMS but none in Unix:\n","");
  x = getenv_vic("CLUE$HISTORY");
  if (x == NULL) {
    sprintf( strng, "%s\n", "CLUE$HISTORY =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "CLUE$HISTORY = ");
    strcat( x, getenv_vic("CLUE$HISTORY"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");
}

$!-----------------------------------------------------------------------------
$ create tgetenv_vic_c.imake
#define PROGRAM tgetenv_vic_c
#define MODULE_LIST tgetenv_vic_c.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN

/*#define LIB_LOCAL	/* remove on delivery */
$!-----------------------------------------------------------------------------
$ create tgetenv_vic_c.pdf
process help=*
end-proc
.TITLE
VICAR program TGETENV_VIC_C
.HELP
This program is a the "C" test program for getenv_vic().
.END
$ Return
$!#############################################################################
$Other_File:
$ create getenv_vic.hlp
1 getenv_vic 
 This routine returns the value of an environment variable (Unix) or
 logical name (VMS).  Under VMS, it will return multivalued logical
 names as a comma-separated list of translations.

 Calling sequence (Fortran):

     call xgetenv_vic(name, value)

 Both arguments are character*n variables.  A blank string is
 returned if the name is not found.

 Calling sequence (C):

     value = getenv_vic("NAME");

 Just like getenv(), the return value is a pointer to a static
 array.  So, the value must be used or copied before this routine
 (or getenv()) is called again.  NULL is returned if the name is
 not found.
$ Return
$!#############################################################################
