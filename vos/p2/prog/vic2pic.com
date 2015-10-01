$!****************************************************************************
$!
$! Build proc for MIPL module vic2pic
$! VPACK Version 1.9, Monday, December 07, 2009, 17:08:08
$!
$! Execute by entering:		$ @vic2pic
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
$ write sys$output "*** module vic2pic ***"
$!
$ Create_Source = ""
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
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vic2pic.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("vic2pic.imake") .nes. ""
$   then
$      vimake vic2pic
$      purge vic2pic.bld
$   else
$      if F$SEARCH("vic2pic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vic2pic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vic2pic.bld "STD"
$   else
$      @vic2pic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vic2pic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vic2pic.com -mixed -
	-s vic2pic.c pic_write.c -
	-i vic2pic.imake -
	-p vic2pic.pdf -
	-t tstvic2pic.pdf new_3d_session.log old_session.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vic2pic.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************************************************

MIPS VICAR program VIC2PIC

Converts VICAR format files to PIC files.

Date: 			March 18,1994
Cognizant Engineer:	S.V.Hwan
References:		PIC software, Todd Litwin, February 1993,
			Caltech copyright (C) 1993
Modifications:
SVH	22 July 1994	Moved PICS related routines to seperate file

Notes:
	This routine is to be used for a single band image, and is
intended for use on BYTE data.

*/


/* INCLUDE FILES */
#include <stdio.h>
#include <string.h>
#if UNIX_OS		/* needed for memcpy */
#include <memory.h>
#endif
#include <stdlib.h>
#include "vicmain_c"
#include "zvproto.h"

/* DEFINES */
#define CHECKif(x) 		if (x) return picFAILURE
#define ABENDif(x)		if (x) zabend()

#define	TRUE			1
#define	FALSE		0

#define picSUCCESS       0
#define picFAILURE     (-1)

#define MAX_FILE_NAME  200

#ifndef PMODE
#define PMODE 0666
#endif

/* routines found in pic_routines */
/*extern int pic_write(*/
/*char *filename, /* input name of PIC file */
/*int rows,       /* input number of rows */
/*int cols,       /* input number of columns */
/*int bands,      /* input number of bands */
/*char *image);    /* input pointer to image buffer */


typedef struct	{
	char	input_file_name[MAX_FILE_NAME];
	char	output_file_name[MAX_FILE_NAME];
}	parameters;


void main44()
{
	int	i,j,k;
	int	nl,ns,nb, npixels, imagesize;
	int	status;
	int	unit_number;
	char	string[MAX_FILE_NAME];
	unsigned char *red   = NULL;
     unsigned char *green = NULL;
     unsigned char *blue  = NULL;
     
	
	parameters user_parameters;
	
	zvmessage(" "," ");
	zvmessage("MIPS VICAR program VIC2PIC version 09.15.03"," ");
	zvmessage(" "," ");


	status = retrieve_user_parameters(&user_parameters);
	ABENDif(status<picSUCCESS);
	/* Read input image */
     
	status = zvunit( &unit_number,"INP",1, NULL);
	ABENDif( status<picSUCCESS );
    
	status = zvopen( unit_number,"OP","READ",
			"OPEN_ACT","SA","U_FORMAT", "BYTE", NULL);
	ABENDif( status<picSUCCESS );
     
	zvget(unit_number, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

     if (nb < 1 || nb > 3)
     {
       fprintf(stderr, "Error: %d bands in input.  Must be either 1 or 3.", nb);
       zabend();
     }
     

	imagesize = nl * ns * nb;
     npixels = nl * ns;

	red   = (unsigned char *) malloc(imagesize * sizeof(unsigned char));
     green = red;
     blue  = red;
	for(j=0; j<nl; ++j)
	   zvread(unit_number, red+(j*ns),"LINE",j+1,
                 "BAND",1,"NSAMPS",ns,NULL);

     /* case of color PIC */
     if (nb == 3)
     {
       green = red + npixels;
       for(j=0; j<nl; ++j)
       {
         zvread(unit_number, green+(j*ns),"LINE",j+1,
                              "BAND",2,"NSAMPS",ns,NULL);
       }

	  blue  = green + npixels;
       for(j=0; j<nl; ++j)
	   zvread(unit_number, blue+(j*ns),"LINE",j+1,
                 "BAND",3,"NSAMPS",ns,NULL);
     }

	cpic_write(user_parameters.output_file_name,nl,ns,nb,red,green,blue);
	status = zvclose( unit_number, NULL);		/* Close input file */
	ABENDif(status<picSUCCESS);
	free(red);
	
	zvmessage(" "," ");
	zvmessage("MIPS VICAR program VIC2PIC completed"," ");
	zvmessage(" "," ");
}

/*

retrieve_user_parameters

Routine to retrieve the input parameters of the application specified
by the user on the command line via VICAR parameters defined in the
.pdf file.

*/
int retrieve_user_parameters( parameters *parms ) 
{
int 	i,j,k,l;
int	count;
int	status;

	status = zvp("INP", parms->input_file_name,&count);
	CHECKif(status<picSUCCESS);
	
	status = zvp("PIC_OUT", parms->output_file_name,&count);
	CHECKif(status<picSUCCESS);
	
	return picSUCCESS;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pic_write.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************
*                                                                             *
*                                     P I C                                   *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 17 Feb 1993                  *
*                                       Updated: 10 Dec 2002                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1997, 2001, *
*                                                     2002                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions to write color PIC files. PIC files
	were used by the JPL vision group on VAX/VMS machines, and is still
	used some of the time on Unix machines by descendent groups. The
	format of the files is simply 2 4-byte integers followed by image
	data. The first integer is the number of rows, the second is the
	number of columns, and the following image data is in scanline
	order.

	This file contains functions
	to support the color extension of PIC files: CPIC files. Their format
	is the same as the PIC files, except with the addition of two more
	block images following the first. The order of the image data is
	the green image, following by the red image, followed by the blue
	image. The reason from deviating from an RGB order is to allow
	older programs, which expect a monochrome image, to read the first
	block and get green, which is the best one for human viewing.
	*/


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#ifndef SUCCESS
#define SUCCESS 0
#endif

#ifndef FAILURE
#define FAILURE (-1)
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef PMODE
#define PMODE 0666
#endif
#ifdef __STDC__

int cpic_write(
    char *filename,	      /* input name of PIC file */
    int rows,			 /* input number of rows */
    int cols,			 /* input number of columns */
    int bands,         	 /* input number of bands */
    unsigned char *red,	 /* input pointer to red buffer */
    unsigned char *green, /* input pointer to green buffer */
    unsigned char *blue); /* input pointer to blue buffer */

#else
int cpid_write();
#endif

/******************************************************************************
********************************   CPIC_WRITE   *******************************
*******************************************************************************

    This function writes an image to a CPIC file. This function returns 0 for
    success and -1 for failure. */

int cpic_write(
    char *filename, 	/* input name of PIC file */
    int rows,			/* input number of rows */
    int cols,			/* input number of columns */
    int bands,           /* input number of bands */
    unsigned char *red,	/* input pointer to red buffer */
    unsigned char *green,/* input pointer to green buffer */
    unsigned char *blue)	/* input pointer to blue buffer */
{
    unsigned char rows4[4], cols4[4];
    int fd, npixels, n;

    if (bands < 1 || bands > 3)
    {
      fprintf(stderr, "Error: Number of bands must be 1, 2, or 3");
      return FAILURE;
    }

    /* Open the PIC file */
    if ((fd = creat(filename, PMODE)) < 0) 
    {
      fprintf(stderr, "Error creating CPIC file: %s\n", filename);
      return FAILURE;
    }

    /* Convert the rows and columns to byte arrays */
    rows4[0] = 0;
    rows4[1] = 0;
    rows4[2] = rows / 256;
    rows4[3] = rows % 256;
    cols4[0] = 0;
    cols4[1] = 0;
    cols4[2] = cols / 256;
    cols4[3] = cols % 256;

    /* Write out number of rows and columns as byte arrays */
    if ((write(fd, rows4, 4) != 4) || (write(fd, cols4, 4) != 4)) 
    {
	 fprintf(stderr, "Error writing CPIC-file header: %s\n", filename);
	 close(fd);
	 return FAILURE;
    }

    /* Write the image to the file */
    npixels = rows * cols;

    /* Green band - a must */
    if ((n = write(fd, green, npixels)) != npixels) 
    {
      fprintf(stderr,
	   "Error writing pixels to CPIC file: %s, %d bytes written\n",
	   filename, n);
      close(fd);
      return FAILURE;
    }

    /* Red band */
    if (bands > 1)
    {
      if ((n = write(fd, red, npixels)) != npixels) 
      {
        fprintf(stderr,
		"Error writing pixels to CPIC file: %s, %d bytes written\n",
		filename, npixels + n);
        close(fd);
        return FAILURE;
      }
    }

    /* Blue band */
    if (bands > 2)
    {
      if ((n = write(fd, blue, npixels)) != npixels) 
      {
        fprintf(stderr,
		"Error writing pixels to CPIC file: %s, %d bytes written\n",
		filename, 2*npixels + n);
        close(fd);
        return FAILURE;
      }
	}

    /* Close the CPIC file */
    if (close(fd) < 0) 
    {
      fprintf(stderr, "Error closing CPIC file: %s\n", filename);
      return FAILURE;
    }

    return SUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vic2pic.imake
#define PROGRAM   	vic2pic

#define MODULE_LIST 	vic2pic.c pic_write.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define R2LIB 

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create vic2pic.pdf
process help=*
PARM INP	STRING	COUNT=1
PARM PIC_OUT	STRING	COUNT=1

!# annot function="Importing and Exporting Data"
!# annot keywords=(PIC,compress,convert)

end-proc
.TITLE
Converts VICAR images to PIC format.
.LEVEL1
.VARI INP
VICAR file of packed compressed images
.VARI PIC_OUT
PIC format image file to be converted.
.LEVEL2
.VARI INP
This parameter specifies the complete path name
of the VICAR format image file of one band to
be converted to a PIC image.
.VARI PIC_OUT
This parameter specifies the complete path name
of the output PIC image.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstvic2pic.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
write " "
write "MIPS Test PDF for PIC2VIC and VIC2PIC"
write " "
write " "
write "Generate a 200x200 VICAR image... "
let $echo="yes"
gen testvic 200 200

let $echo="no"
write "*** List labels and contents in upper left corner"
write " "
let $echo="yes"
label-list testvic
list testvic nl=10 ns=10

let $echo="no"
write "*** Convert test file into PICS format (pic file)"
write " "
let $echo="yes"
vic2pic testvic testpic

let $echo="no"
write "*** And back to VICAR format"
write " "
let $echo="yes"
pic2vic testpic testvic2

let $echo="no"
write "*** List labels and contents in upper left corner of result"
write " "
let $echo="yes"
label-list testvic2
list testvic2 nl=10 ns=10

let $echo="no"
write "*** Compare the two pictures"
write " "
let $echo="yes"
difpic (testvic testvic2)

let $echo="no"

!
! Test 3D image
!
let $echo="no"
write "*** Convert 3D test file into PICS format (pic file)"
write " "
let $echo="yes"
gen testvic3d 200 200 3
label-list testvic3d
list testvic3d nl=10 ns=10 
vic2pic testvic3d testpic3d

let $echo="no"
write "*** And back to VICAR format"
write " "
let $echo="yes"
pic2vic testpic3d testvic3d2 nb=3

let $echo="no"
write "*** Compare the two pictures"
write " "
let $echo="yes"
label-list testvic3d2
list testvic3d2 nl=10 ns=10 
difpic (testvic3d testvic3d2)
!
end-proc
$!-----------------------------------------------------------------------------
$ create new_3d_session.log
tstvic2pic
 
MIPS Test PDF for PIC2VIC and VIC2PIC
 
 
Generate a 200x200 VICAR image... 
gen testvic 200 200
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
*** List labels and contents in upper left corner
 
label-list testvic
Beginning VICAR task label
************************************************************
 
        ************  File testvic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: ntt -- Mon Sep 15 18:31:49 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep 15 18:31:49 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
*** Convert test file into PICS format (pic file)
 
vic2pic testvic testpic
Beginning VICAR task vic2pic
 
MIPS VICAR program VIC2PIC version 09.15.03
 
 
MIPS VICAR program VIC2PIC completed
 
let $echo="no"
*** And back to VICAR format
 
pic2vic testpic testvic2
Beginning VICAR task pic2vic
 
PIC2VIC version 15 Sept 2003
 
let $echo="no"
*** List labels and contents in upper left corner of result
 
label-list testvic2
Beginning VICAR task label
************************************************************
 
        ************  File testvic2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: PIC2VIC -- User: ntt -- Mon Sep 15 18:31:51 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Mon Sep 15 18:31:51 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
*** Compare the two pictures
 
difpic (testvic testvic2)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
let $echo="no"
*** Convert 3D test file into PICS format (pic file)
 
gen testvic3d 200 200 3
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-list testvic3d
Beginning VICAR task label
************************************************************
 
        ************  File testvic3d ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                3 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: ntt -- Mon Sep 15 18:31:52 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic3d nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep 15 18:31:52 2003
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18


 Task:GEN       User:ntt       Date_Time:Mon Sep 15 18:31:52 2003
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14
      6       6   7   8   9  10  11  12  13  14  15
      7       7   8   9  10  11  12  13  14  15  16
      8       8   9  10  11  12  13  14  15  16  17
      9       9  10  11  12  13  14  15  16  17  18
     10      10  11  12  13  14  15  16  17  18  19


 Task:GEN       User:ntt       Date_Time:Mon Sep 15 18:31:52 2003
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15
      6       7   8   9  10  11  12  13  14  15  16
      7       8   9  10  11  12  13  14  15  16  17
      8       9  10  11  12  13  14  15  16  17  18
      9      10  11  12  13  14  15  16  17  18  19
     10      11  12  13  14  15  16  17  18  19  20
vic2pic testvic3d testpic3d
Beginning VICAR task vic2pic
 
MIPS VICAR program VIC2PIC version 09.15.03
 
 
MIPS VICAR program VIC2PIC completed
 
let $echo="no"
*** And back to VICAR format
 
pic2vic testpic3d testvic3d2 nb=3
Beginning VICAR task pic2vic
 
PIC2VIC version 15 Sept 2003
 
let $echo="no"
*** Compare the two pictures
 
label-list testvic3d2
Beginning VICAR task label
************************************************************
 
        ************  File testvic3d2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                3 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: PIC2VIC -- User: ntt -- Mon Sep 15 18:31:53 2003 ----
 
************************************************************
list testvic3d2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Mon Sep 15 18:31:53 2003
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18


 Task:PIC2VIC   User:ntt       Date_Time:Mon Sep 15 18:31:53 2003
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14
      6       6   7   8   9  10  11  12  13  14  15
      7       7   8   9  10  11  12  13  14  15  16
      8       8   9  10  11  12  13  14  15  16  17
      9       9  10  11  12  13  14  15  16  17  18
     10      10  11  12  13  14  15  16  17  18  19


 Task:PIC2VIC   User:ntt       Date_Time:Mon Sep 15 18:31:53 2003
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15
      6       7   8   9  10  11  12  13  14  15  16
      7       8   9  10  11  12  13  14  15  16  17
      8       9  10  11  12  13  14  15  16  17  18
      9      10  11  12  13  14  15  16  17  18  19
     10      11  12  13  14  15  16  17  18  19  20
difpic (testvic3d testvic3d2)
Beginning VICAR task difpic
DIFPIC version 10-11-95
  Number of bands to process =   3
 NUMBER OF DIFFERENCES =   0
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_session.log
tstvic2pic
 
MIPS Test PDF for PIC2VIC and VIC2PIC
 
 
Generate a 200x200 VICAR image... 
gen testvic 200 200
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
*** List labels and contents in upper left corner
 
label-list testvic
Beginning VICAR task label
************************************************************
 
        ************  File testvic ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: ntt -- Mon Sep  8 10:50:15 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 10:50:15 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
*** Convert test file into PICS format (pic file)
 
vic2pic testvic testpic
Beginning VICAR task vic2pic
 
MIPS VICAR program VIC2PIC version 07.22.94
 
 
MIPS VICAR program VIC2PIC completed
 
let $echo="no"
*** And back to VICAR format
 
pic2vic testpic testvic2
Beginning VICAR task pic2vic
 
PIC2VIC version 15 May 2001
 
let $echo="no"
*** List labels and contents in upper left corner of result
 
label-list testvic2
Beginning VICAR task label
************************************************************
 
        ************  File testvic2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                200 lines per band
                200 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: PIC2VIC -- User: ntt -- Mon Sep  8 10:50:16 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Mon Sep  8 10:50:16 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
let $echo="no"
*** Compare the two pictures
 
difpic (testvic testvic2)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
let $echo="no"
disable-log
$ Return
$!#############################################################################
