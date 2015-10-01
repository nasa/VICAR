$!****************************************************************************
$!
$! Build proc for MIPL module calltp
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:16
$!
$! Execute by entering:		$ @calltp
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
$ write sys$output "*** module calltp ***"
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
$ write sys$output "Invalid argument given to calltp.com file -- ", primary
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
$   if F$SEARCH("calltp.imake") .nes. ""
$   then
$      vimake calltp
$      purge calltp.bld
$   else
$      if F$SEARCH("calltp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake calltp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @calltp.bld "STD"
$   else
$      @calltp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create calltp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack calltp.com -mixed -
	-s calltp.c -
	-i calltp.imake -
	-o calltp.hlp -
	-t test_calltp.pdf test_calltp.c test_calltp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create calltp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/********************************************************************
 * calltp.cc: Contains subroutines that starts-up interactive
 * tiepoint collection program and gets tiepoints when that
 * program exits.  The tiepoints are transformed back and forth
 * through a temporary file.
 ********************************************************************
 * vxp	12/14/97 - original delivery
 * vxp  07/08/97 - replaced ascii file with ibis-2 file
 ********************************************************************/

#include "calltp.h"
#include "ibistiepnt.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zvproto.h>

#if !defined(__VMS)
  #include <unistd.h>
#endif

/********************************************************************
 * display_points: Starts-up interactive tiepoint collection program 
 * and gets tiepoints when that program exits.  It returns 2d array 
 * in form of { {S11 L11 S12 L12} ... {Sn1 Ln1 Sn2 Ln2} }.  Number of 
 * tiepoints n is returned in parameter npoints.
 ********************************************************************/
double **display_points(const char *image_1, const char *image_2, 
			double array[][4], int *npoints, char *pathname, 
			int *tp_status)
{
    char point_file_ibis[256];
    int i;
    int unit, ibis;
    int status;
    char cmd[1024];
    char x_resources[1024];
    char filenames[MAX_noimgs][FNAMLEN];
    int nrows;

    int num_gen_quals;
    char gen_qual_names[MAX_nogenqlf][STRING_32];
    char gen_qual_format[MAX_nogenqlf][IFMT_SIZE];
    char gen_qual_unit[MAX_nogenqlf][STRING_32];

    int num_pnt_quals;
    char pnt_qual_names[MAX_noimgqlf][STRING_32];
    char pnt_qual_format[MAX_noimgqlf][IFMT_SIZE];
    char pnt_qual_unit[MAX_noimgqlf][STRING_32];

    int num_images;

    int *gen_quals_full=NULL;
    float *gen_quals_real=NULL;
    char *gen_quals_text=NULL;
    int *pnt_quals_full=NULL;
    float *pnt_quals_real=NULL;
    char *pnt_quals_text=NULL;

    TPOINT *out_array;
    char rmcmd[256];

    float lines[2];
    float samps[2];


    strcpy(filenames[0], image_1);
    strcpy(filenames[1], image_2);

    if (pathname == NULL)
#if defined(__VMS)
    pathname = (char *)strdup("tp");
#else
    pathname = (char *)strdup("$R2LIB/tp");
#endif

#if defined(__VMS)
    sprintf(point_file_ibis, "tp%d", getpid() % 1000);
#else
    sprintf(point_file_ibis, "/tmp/tp.%d", getpid());
#endif

    nrows = *npoints;

    /* Create tiepoint file to be read by interactive program
     * Each tiepoint takes one line in the form 
     * line1 sample1 line2 sample2 separated by tab.
     */

    status = zvunit(&unit, "dp_1",  1, "u_name", point_file_ibis, NULL);
    if (status != OK) {
	fprintf(stderr, "Error occurred in zvunit, no changes were made\n");
	*npoints = 0;
	return NULL;
    }

    num_gen_quals = 0;
    num_pnt_quals = 0;
    
    status = zitiepnt_openw(unit, &ibis, 2, filenames,
               num_gen_quals, gen_qual_names, gen_qual_format, gen_qual_unit,
               num_pnt_quals, pnt_qual_names, pnt_qual_format, pnt_qual_unit,
               nrows);
    if (status != OK) {
	fprintf(stderr,
		"Error occurred in zitiepnt_openw\n");
	zitiepnt_close(unit);
	*npoints = 0;
	return NULL;
    }


    for (i = 0; i < nrows; i++) {
	samps[0] = (float)array[i][0];
	lines[0] = (float)array[i][1];
	samps[1] = (float)array[i][2];
	lines[1] = (float)array[i][3];
	status = zitiepnt_write(unit, i+1, lines, samps, 0, 0, 0, 0, 0, 0);
	if (status != OK) {
	    fprintf(stderr, 
		   "Error occurred in zitiepnt_write, no changes were made\n");
	    zitiepnt_close(unit);
	    *npoints = 0;
	    return NULL;
	}
    }

    status = zitiepnt_close(unit);
    if (status != OK) {
	fprintf(stderr, "Error occurred in zitiepnt_close\n");
    }
    
    /* Execute interactive tiepoint collection program */

    strcpy(x_resources, "-xrm \"*numImagesDisplayed: 2\"");
    strcat(x_resources, " -xrm \"*fullMatchIsEnforced: True\"");
    strcat(x_resources, " -xrm \"*enableSetSpecialStatus: True\"");
    sprintf(cmd, 
	    "%s -pfile %s %s %s %s", 
	    pathname, point_file_ibis, x_resources, image_1, image_2);
    printf("Executing %s\n", cmd);
    *tp_status = system(cmd);
    if (*tp_status == -1)
	fprintf(stderr, "Tiepoint program failed (errno=%d)\n", errno);

    /* Open tiepoint file to read the tiepoints generated by 
     * interactive program
     */

    *npoints = 0;
    status = zvunit(&unit, "dp_2",  1, "u_name", point_file_ibis, NULL);
    if (status != OK) {
        fprintf(stderr, "Error occurred in zvunit, no changes were made\n");
	*npoints = 0;
        return NULL;
    }

    /* Read the tiepoints into a list of arrays, each array 
     * consists of four elements.
     */

    status = zitiepnt_openr(unit, &ibis, &num_images, filenames,
			    &num_gen_quals, gen_qual_names, 
			    gen_qual_format, gen_qual_unit,
			    &num_pnt_quals, pnt_qual_names, 
			    pnt_qual_format, pnt_qual_unit,
			    npoints);
    if (status != OK) {
	fprintf(stderr, 
		"Error occurred in zitiepnt_openr, no changes were made\n");
	*npoints = 0;
        return NULL;
    }

    /* Allocate two dimensional array */

    out_array = (TPOINT *)malloc(*npoints * sizeof(TPOINT));
    if (out_array == NULL) {
	fprintf(stderr, "Not enough memory to allocate out_array!\n");
	zitiepnt_close(unit);
	*npoints = 0;
	return NULL;
    }

    for (i = 0; i < *npoints; i++) {

	/* Read one line of data */

	status = zitiepnt_read(unit, i+1,
                               lines, samps,
                               gen_quals_real, gen_quals_full, gen_quals_text,
                               pnt_quals_real, pnt_quals_full, pnt_quals_text);

	if (status != OK) {
	    fprintf(stderr,
                "Error occurred in zitiepnt_read, no changes were made\n");
	    zitiepnt_close(unit);
	    *npoints = 0;
	    return NULL;
	}

	out_array[i] = (TPOINT)malloc(4 * sizeof(double));

	if (out_array[i] == NULL) {
            fprintf(stderr, "Not enough memory to allocate out_array[%d]!\n",
                    i);
	    *npoints = 0;
            return NULL;
        }

	out_array[i][0] = (double)samps[0];
	out_array[i][1] = (double)lines[0];
	out_array[i][2] = (double)samps[1];
	out_array[i][3] = (double)lines[1];

    }

    status = zitiepnt_close(unit);
    if (status != OK) {
        fprintf(stderr, "Error occurred in zitiepnt_close\n");
    }

#if defined(__VMS)
    sprintf(rmcmd, "delete %s", point_file_ibis);
#else
    sprintf(rmcmd, "rm %s", point_file_ibis);
#endif

    system(rmcmd);

    return out_array;
}

#ifdef __VMS
/*******************************************************************
 * strdup: VMS (at least Alpha OpenVMS 6.1) does not have strdup,
 * so we create one here.  This is cribbed from Young's release notes.
 *******************************************************************/
char *strdup(const char *str)
{
    char *newStr;

    /* not robust, but neither is strdup */
    newStr = (char *)malloc((strlen(str) + 1)  * sizeof(char));
    strcpy(newStr, str);
    return (newStr);
}
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create calltp.imake
#define SUBROUTINE calltp
#define MODULE_LIST calltp.c

/* #define DEBUG */

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Other_File:
$ create calltp.hlp
1 CALLTP

  Module CALLTP contains routines that spawn off the tiepoint collection 
  program TP, given a set of initial points and filename.  These are mostly 
  convenience functions that could be called from batch subroutines for 
  purposes of result verification and corrections.  Currently, one function 
  display_points() is implemented, although more could be added in the 
  future if there is demand.

    display_points displays tiepoints for two images.

  Tiepoints are stored in the temporary directory in the IBIS-2 file and 
  the name of that file is passed to the TP program as an argument.

2  CALLING SEQUENCE
 
  C Calling Sequences:        
 
#include "calltp.h"

    out_array = display_points(file1, file2, array, &n, tp_location);

  FORTRAN Calling Sequences:  not implemented.

2  ARGUMENTS

  display_points

    file1		VICAR image filename
    file2		VICAR image filename
    array		Two-dimensional array of tiepoints in the form
			{ {S11 L11 S12 L12} ... {Sn1 Ln1 Sn2 Ln2} }
    n			Number of tiepoints
    tp_location		Location of TP program.  If NULL is passed, the 
			program will run out of $R2LIB.

2  HISTORY

	Original Programmer: Vadim Parizher     June 1997
	Current Cognizant Programmer: Vadim Parizher
	Source Language: C

 Revision History
  vxp  12/14/97 - original delivery
  vxp  07/08/97 - replaced ascii file with ibis-2 file

2  OPERATION

  display_points: Starts-up interactive tiepoint collection program 
  and gets tiepoints when that program exits.  It returns 2d array 
  in form of { {L11 S11 L12 S12} ... {Ln1 Sn1 Ln2 Sn2} }.  Number of 
  tiepoints n is returned in parameter npoints.

2 REFERENCES

  ibistiepnt.hlp: in P2$ Return
$ Return
$!#############################################################################
$Test_File:
$ create test_calltp.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create test_calltp.c
/*******************************************************************
 * Test main routine.  It shows how to use display_points() and how to
 * process data that came back from the subroutine.
 * vxp 4/97 - Initial delivery
 * vxp 5/97 - Fixed Line and Sample being printed in reverse
 * 	    - Added parameters to tp
 *******************************************************************/
#include "calltp.h"
#include <stdio.h>
#include <stdlib.h>

int main()
{
    int i, j;
    int n = 3;             /* number of points */
    char file1[256];	   /* VICAR file */
    char file2[256];	   /* VICAR file */
    int status;            /* Status returned from tiepoint program */
    double array[3][4] = { {25.50, 75.75, 75.20, 175}, 
			   {50.40, 100.1, 100.3, 200.938}, 
			   {75,    125,   125,   225} };
    double **out_array;

#ifdef __VMS
    sprintf(file1, "%s", "images:io.red");
    sprintf(file2, "%s", "images:io.blu");
#else
    sprintf(file1, "%s", "/usr/local/images/io.red");
    sprintf(file2, "%s", "/usr/local/images/io.blu");
#endif

    out_array = display_points(file1, file2, array, &n, NULL, &status);

    printf("Interactive program finished with exit status = %d\n", status);
    printf("%d tiepoints collected\n", n);
    printf("S1\t\tL1\t\t|\tS2\t\tL2\n");
    printf("-----------------------------------------------------------------\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j < 4; j++) {
            if (j == 2) 
		printf("|\t");
            printf("%f\t", out_array[i][j]);
        }
        printf("\n");
    }
    return 0;
}
$!-----------------------------------------------------------------------------
$ create test_calltp.imake
#define PROGRAM test_calltp
#define MODULE_LIST test_calltp.c

#define TEST
/* #define DEBUG */

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_P2SUB
#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE

/* #define LIB_LOCAL */

$ Return
$!#############################################################################
