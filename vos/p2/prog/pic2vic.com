$!****************************************************************************
$!
$! Build proc for MIPL module pic2vic
$! VPACK Version 1.9, Monday, December 07, 2009, 16:51:59
$!
$! Execute by entering:		$ @pic2vic
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
$ write sys$output "*** module pic2vic ***"
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
$ write sys$output "Invalid argument given to pic2vic.com file -- ", primary
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
$   if F$SEARCH("pic2vic.imake") .nes. ""
$   then
$      vimake pic2vic
$      purge pic2vic.bld
$   else
$      if F$SEARCH("pic2vic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pic2vic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pic2vic.bld "STD"
$   else
$      @pic2vic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pic2vic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pic2vic.com -mixed -
	-s pic2vic.c pic_read.c -
	-i pic2vic.imake -
	-p pic2vic.pdf -
	-t tstpic2vic.pdf new_3d_session.log old_3d_session.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pic2vic.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*********************************************************************

MIPS VICAR program PIC2VIC

Converts PIC files to VICAR format files.

Date: 			March 18,1994
Cognizant Engineer:	S.V.Hwan
References:		PIC software, Todd Litwin, February 1993,
			Caltech copyright (C) 1993
Modifications:
SVH	22 July 1994	Moved PICS related routines to seperate file
CRI     05 Sept 1994    MSTP S/W Conversion (VICAR Porting)
RGD     28 Sept 2000    Added BAND parameter
AXC     15 May  2001    Modified pic_read.c routine to process multiband image
                        on VMS system (AR-105417)
NTT     04 Sept 2003    Modified to convert multiband images

Notes:
	This routine outputs one or more banded image, and is intended for
use on BYTE data.

*/


/* INCLUDE FILES */
#include <stdio.h>
#include <string.h>
#if UNIX_OS		/* needed for memcpy */
#include <memory.h>
#endif
#include "vicmain_c"
#include <stdlib.h>

/* DEFINES */

#define CHECKif(x) 		if (x) return picFAILURE
#define ABENDif(x)		if (x) zabend()

#define	TRUE			     1
#define	FALSE			0

#define SUCCESS               0
#define FAILURE             (-1)
#define picSUCCESS            0
#define picFAILURE          (-1)

#define MAX_FILE_NAME		200

#ifndef PMODE
#define PMODE 0666
#endif

typedef struct	{
	char	input_file_name[MAX_FILE_NAME];
	char	output_file_name[MAX_FILE_NAME];
        int     sl;   /* Starting line */
        int     ss;   /* Starting sample */
	   int	 sb;   /* Starting band */
        int     nl;   /* Number of lines */
        int     ns;   /* Number of samples */
        int     nb;   /* Number of bands */
}  	parameters;



/* routines found in pic_routines */
/* pic_read( */
/* char *, */	    /* input name of PIC file */
/* int  *, */	    /* output number of rows */
/* int  *, */	    /* output number of columns */
/* char **);   /* output pointer to image buffer */


void main44(void)
{
	int	i,j,k,l,g,start,lineStart;
	int	SL,SS,SB,NL,NS,NB,nli,nsi,nbi;
	int	nl,ns,nb; /*set by pic_read */
	int	status;
	int	unit_number;
	char	string[MAX_FILE_NAME];
	unsigned  char *red;
     unsigned char  *grn;
     unsigned char  *blu;
     unsigned char  *image_ptr;
	parameters user_parameters;
	
	zifmessage(" ");
	zifmessage("PIC2VIC version 15 Sept 2003");
	zifmessage(" ");

	status = retrieve_user_parameters(&user_parameters);
	ABENDif(status<picSUCCESS);

	/* Read input image */
	status = cpic_read(user_parameters.input_file_name,&nl,&ns,&nb,
                        &red, &grn, &blu);
     
     ABENDif(status<picSUCCESS);
     /* Validate user parameters from test pdf against pic file */
     status = validate_user_parameters
                (&user_parameters,&SL,&SS,&SB,&NL,&NS,&NB,&nl,&ns,&nb);
	ABENDif( status<picSUCCESS );
	
	/* Open output file */
	status = zvunit( &unit_number,"OUT",1, NULL);
	ABENDif( status<picSUCCESS );
	
	status = zvopen( unit_number,"OP","WRITE",
			"OPEN_ACT","SA",
   			"U_NL",NL,"U_NS",NS,"U_NB",NB, NULL);
	ABENDif( status<picSUCCESS );
        
     /* calculate starting pixel location. Starting line * number of 
        samples per line * Starting Sample */
     start = (SL-1)*ns+(SS-1);

     /* case of one band */
     if (NB == 1)
     {
       if (SB == 1)
         image_ptr = red;
       else if (SB == 2)
         image_ptr = grn;
       else if (SB == 3)
         image_ptr = blu;
     
       for (j = 0; j < NL; ++j)
       {
         lineStart = start+(j*ns); /*curLine start*/
	    zvwrit(unit_number, &image_ptr[lineStart],"LINE",j+1,
                 "SAMP",1,"NSAMPS",NS,"BAND",1, NULL);
       }
     }

     /* case of three bands */
     else if (NB == 3)
     {
       /* red band */
       for (j = SL-1; j < NL; ++j)
       {
         lineStart = start+(j*NS); /*curLine start*/
	    zvwrit(unit_number, &red[lineStart],"LINE",j+1,
                "SAMP",1,"NSAMPS",NS,"BAND",1, NULL);
       }

       /* green band */
       for (j = SL-1; j < NL; ++j)
       {
         lineStart = start+(j*NS); /*curLine start*/
	    zvwrit(unit_number, &grn[lineStart],"LINE",j+1,
                "SAMP",1,"NSAMPS",NS,"BAND",2, NULL);
       }

       /* blue band */
       for (j = SL-1; j < NL; ++j)
       {
         lineStart = start+(j*NS); /*curLine start*/
	    zvwrit(unit_number, &blu[lineStart],"LINE",j+1,
                "SAMP",1,"NSAMPS",NS,"BAND",3, NULL);
       }
     }

	status = zvclose( unit_number, NULL);		/* Close input file */
	ABENDif(status<picSUCCESS);
	free(red);
        return;
}

/*

retrieve_user_parameters

Routine to retrieve the input parameters of the application specified
by the user on the command line via VICAR parameters defined in the
.pdf file.

*/
int retrieve_user_parameters( parameters *user_parameters ) 
{
int 	i,j,k,l,nli,nsi,nbi;
int	count;
int	status;
 int  tempBand1, tempBand2;

	status = zvp("PIC_INP", user_parameters->input_file_name,&count);
	CHECKif(status<picSUCCESS);
	
	status = zvp("OUT", user_parameters->output_file_name,&count);
	CHECKif(status<picSUCCESS);

     zvsize (&user_parameters->sl,&user_parameters->ss,
                          &user_parameters->nl,&user_parameters->ns,
                          &nli,&nsi);

     status = zvp("SB",&user_parameters->sb, &count);
	CHECKif(status<picSUCCESS);

     /* for backward compatibility */
     if (user_parameters->sb == -1)
     {
       status = zvp("BAND", &user_parameters->sb,&count);
       CHECKif(status<picSUCCESS);
     }
     status = zvp("NB", &user_parameters->nb,&count);
	CHECKif(status<picSUCCESS);
	
	return picSUCCESS;
}

/* Validate input parameters against pic file input */
validate_user_parameters (
   parameters *parms, /* User parameters list */                                             
   int *SL,           /* Starting line */
   int *SS,           /* Starting sample */
   int *SB,           /* Starting band */
   int *NL,           /* Number of lines */
   int *NS,           /* Number of samples */
   int *NB,           /* Number of bands */
   int *nl,           /* Number of lines from PIC file */
   int *ns,           /* Number of samples from PIC file */
   int *nb)           /* Number of bands from PIC file */
{
   if (parms->sl == -1) {   /* If operator did not specify starting line */
      *SL = 1;
       parms->sl = 1;
   } else {
      if (parms->sl > *nl || parms->sl < 1) {
         zvmessage ("Starting line number 'SL' is invalid",0);
         return picFAILURE;
      } else {
         *SL = parms->sl;
      }
   }
   
   if (parms->ss == -1) {   /* If operator did not specify starting sample */
      *SS = 1;       
       parms->ss = 1;
   } else {
      if (parms->ss > *ns || parms->ns < 1) {
         zvmessage ("Starting sample number 'SS' is invalid",0);
         return picFAILURE;
      } else {
         *SS = parms->ss;
      }
   }
   
   if (parms->sb == -1) {   /* If operator did not specify starting band */
      *SB = 1;       
       parms->sb = 1;
   } else {
      if (parms->sb > *nb || parms->nb < 1) {
         zvmessage ("Starting band number 'SB' is invalid",0);
         return picFAILURE;
      } else {
         *SB = parms->sb;
      }
   }

   if (parms->ns == -1) {   /* If operator did not specify number of samples */
      *NS = *ns - *SS +1;  
       parms->ns = *NS;
   } else {
      if ((parms->ns+parms->ss-1) > *ns || parms->ns < 1) {
         zvmessage ("Number of samples 'NS' is invalid",0);
         return picFAILURE;
      } else {
         *NS = parms->ns;
      }
   }
   
   if (parms->nl == -1) {   /* If operator did not specify number of lines */
      *NL = *nl - *SL +1;           
       parms->nl = *NL;        
   } else {
      if ((parms->nl+parms->sl-1) > *nl || parms->nl < 1) {
         zvmessage ("Number of lines 'NL' is invalid",0);
         return picFAILURE;
      } else {
         *NL = parms->nl;
      }
   }
   
   if (parms->nb == -1) {   /* If operator did not specify number of bands */
      *NB = *nb - *SB + 1;           
       parms->nb = *NB;        
   } else {
      if ((parms->nb+parms->sb-1) > *nb || parms->nb < 1) 
      {
         zvmessage ("Number of bands 'NB' is invalid",0);
         return picFAILURE;
      } 
      else if ( (parms->nb != 1) && (parms->nb != 3) )
      {
         zvmessage ("Number of bands 'NB' must be 1 or 3",0);
         return picFAILURE;
      }
      else 
      {
         *NB = parms->nb;
      }
   }

   return picSUCCESS;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pic_read.c
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


	This file contains functions to read color PIC files. PIC files
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


     Modifications:

     09-15-2003     NTT     Extracted function from Tood Litwin's pic.c file
                            which contains PIC I/O utility functions.
                            Modified function to include bands as a parameter to
                            determine how many are included (1 or 3).  If the 
                            quantity is a number other than 1 or 3, and error 
                            is returned.
	*/


#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

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

int cpic_read(
    char *filename,	/* input name of PIC file */
    int *rows,			/* output number of rows */
    int *cols,			/* output number of columns */
    int *bands,         	/* output number of bands */
    unsigned char **red,	/* output pointer to red buffer */
    unsigned char **green,	/* output pointer to green buffer */
    unsigned char **blue);

#else
int cpid_read();
#endif

/******************************************************************************
********************************   CPIC_READ   ********************************
*******************************************************************************

    This function reads in a CPIC or PIC image from a file. It allocates the
    memory internally to hold the image. It is the caller's responsibility to
    free the memory when it is no longer needed. Note that all of the image
    bands will be placed into a single allocated block; free only the red
    pointer. Note that if the input image is monochrome, then all three
    pointers will point to the same address. This function returns 0 for
    success and -1 for failure. */

int cpic_read(
    char *filename,	/* input name of PIC file */
    int *rows,			/* output number of rows */
    int *cols,			/* output number of columns */
    int *bands,         	/* output number of bands */
    unsigned char **red,	/* output pointer to red buffer */
    unsigned char **green,	/* output pointer to green buffer */
    unsigned char **blue)	/* output pointer to blue buffer */
{
    unsigned char rows4[4], cols4[4];
    int i, fd, npixels, color;
    unsigned char *r, *g, *b;
    long l0, l, imagesize;

    /* Open the PIC/CPIC file */
    if ((fd = open(filename, 0)) < 0) {
	fprintf(stderr, "Error opening PIC file: %s\n", filename);
	return FAILURE;
	}

    /* Read in the number of rows and columns as byte arrays */
    if ((read(fd, rows4, 4) != 4) || (read(fd, cols4, 4) != 4)) {
	fprintf(stderr, "Error reading PIC-file header: %s\n", filename);
	close(fd);
	return FAILURE;
	}

    /* Convert the rows and columns to normal integers */
    if ((rows4[0] == 0) && (rows4[1] == 0) &&
	(cols4[0] == 0) && (cols4[1] == 0)) {
	*rows = (rows4[2] * 256) + rows4[3];
	*cols = (cols4[2] * 256) + cols4[3];
	}
    else if ((rows4[2] == 0) && (rows4[3] == 0) &&
	    (cols4[2] == 0) && (cols4[3] == 0)) {
	*rows = (rows4[1] * 256) + rows4[0];
	*cols = (cols4[1] * 256) + cols4[0];
	}
    else {
	fprintf(stderr, "Bad PIC-file header: %s\n", filename);
	close(fd);
	return FAILURE;
	}

    /* Find out if this is a monochrome or color file */
    if ((l0 = lseek(fd, 0L, SEEK_CUR)) < 0) {
	fprintf(stderr, "Error checkpointing position in PIC file: %s\n",
		filename);
	close(fd);
	return FAILURE;
	}
    if ((l = lseek(fd, 0L, SEEK_END)) < 0) {
	fprintf(stderr, "Error checking length of PIC file: %s\n",
		filename);
	close(fd);
	return FAILURE;
	}
    if ((l0 = lseek(fd, l0, SEEK_SET)) < 0) {
	fprintf(stderr, "Error resetting position in PIC file: %s\n",
		filename);
	close(fd);
	return FAILURE;
	}
    npixels = *rows * *cols;
    color = (l > (2 * npixels));


    if (get_file_size(filename, &imagesize) == FAILURE)
    {
      fprintf(stderr, "Error occurred while getting filesize for PIC file: %s\n",
		filename);
	close(fd);
	return FAILURE;
    }
    imagesize = imagesize - 8;
    /*
    fprintf(stderr, "ROW SIZE   = %d\n", *rows );
    fprintf(stderr, "COL SIZE   = %d\n", *cols );
    fprintf(stderr, "IMAGE SIZE = %d\n", imagesize );
    */ 
    
    /* get the band count */
    *bands = ((int) imagesize) / npixels;

    /* fprintf(stderr, "NUM BANDS = %d\n", *bands); */

    /* make sure there was no lost remainder */
    if (*bands * npixels != imagesize)
    {
      fprintf(stderr, "Error: Could not calculate band count of file: %s.\n",
			filename);
	 close(fd);
	 return FAILURE;
    }
    
    /* enforce band count of 1 or 3 */
    if (*bands != 1 && *bands != 3)
    {
      fprintf(stderr, "Error: %d bands found from file: %s. Count must be 1 or 3.\n",
			filename, *bands);
		close(fd);
		return FAILURE;
    }


    /* Handle a color image */
    if (color) 
    {

	/* Allocate enough memory to hold the image */
	if ((*red = (unsigned char *)malloc(3 * npixels)) == NULL)
	    color = FALSE;

	/* Set up pointers */
	*green = *red   + npixels;
	*blue  = *green + npixels;

	/* Read the image into local memory */
	if (color) {
	    if ((read(fd, *green, npixels) != npixels) ||
		(read(fd, *red,   npixels) != npixels) ||
		(read(fd, *blue,  npixels) != npixels)) {
		fprintf(stderr, "Error reading pixels from CPIC file: %s\n",
			filename);
		free(*red);
		close(fd);
		return FAILURE;
		}
	    }

	/* Check if it is really monochrome data masquarading as color */
	r = *red;
	g = *green;
	b = *blue;
	for (i=npixels; i>0; i--,r++,g++,b++) {
	    if ((*r != *g) || (*g != *b))
		break;
	    }
	if (i <= 0)
	    *green = *blue = *red;
	}

    /* Handle a monochrome image, or when we can't get memory for color */
    if (!color) {

	/* Allocate enough memory to hold the image */
	if ((*red = (unsigned char *)malloc(npixels)) == NULL) {
	    fprintf(stderr, "Error allocating %d bytes for PIC file: %s\n",
		npixels, filename);
	    close(fd);
	    return FAILURE;
	    }

	/* Set up pointers */
	*green = *red;
	*blue  = *red;

	/* Read the image into local memory */
	if (read(fd, *red, npixels) != npixels) {
	    fprintf(stderr, "Error reading pixels from PIC file: %s\n",
			filename);
	    free(*red);
	    close(fd);
	    return FAILURE;
	    }
	}

    /* Close the PIC file */
    close(fd);

    return SUCCESS;
    }


/* Gets the filesize of the filename, used to determine # bands*/
get_file_size(
char *filename,   /* input name of the PIC file */
long *filesize     /* output parameter to contain size of file */
)
{
  FILE* fp;
  
  if ((fp = fopen(filename, "rb")) == 0)
    return FAILURE;

  if (fseek (fp, 0L, SEEK_END) != 0)
    return FAILURE;

  if ((*filesize = ftell(fp)) == -1)
    return FAILURE;

  fclose(fp);

  return SUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pic2vic.imake
#define PROGRAM  pic2vic

#define MODULE_LIST pic2vic.c pic_read.c

#define MAIN_LANG_C
#define TEST
#define USES_FORTRAN 
#define USES_C
#define R2LIB 
#define LIB_RTL
#define LIB_FORTRAN
#define FTN_STRING 


#define LIB_TAE
#define LIB_P2SUB

$ Return
$!#############################################################################
$PDF_File:
$ create pic2vic.pdf
process help=*
PARM PIC_INP	TYPE=(STRING,40)	COUNT=1
PARM OUT	TYPE=(STRING,40)	COUNT=1
PARM SL         TYPE=INTEGER COUNT=0:1 DEFAULT=-1
PARM SS         TYPE=INTEGER COUNT=0:1 DEFAULT=-1
PARM SB         TYPE=INTEGER COUNT=0:1 DEFAULT=-1
PARM NL         TYPE=INTEGER COUNT=0:1 DEFAULT=-1
PARM NS         TYPE=INTEGER COUNT=0:1 DEFAULT=-1
PARM NB         TYPE=INTEGER COUNT=0:1 DEFAULT=1
PARM SIZE       TYPE=INTEGER COUNT=0:4 DEFAULT=(-1,-1,-1,-1)
PARM BAND       TYPE=INTEGER COUNT=1   DEFAULT=1

!# annot function="Importing and Exporting Data"
!# annot keywords=(parameter,PIC,path,compress)

end-proc
.TITLE
Converts PIC format images to VICAR
.HELP
Converts PIC formatted images to VICAR.  PIC is a trivially simple file format
used by the Robotics Group at JPL.  It consists of a 4-byte header with lines
and samples followed by image data.

PIC does not seem to support multi-band data.  However, the FIDO rover at
least uses it that way.  The BAND parameter can be used to select a band to
read.  No check is made that the band is present.  Data for each band simply
follows each other in the file with no intervening header.  There is also
no header to specify the number of bands (which is why it's not checked).

REVISION HISTORY
Date:                   March 18,1994
Cognizant Engineer:     S.V.Hwan
References:             PIC software, Todd Litwin, February 1993,
                        Caltech copyright (C) 1993
Modifications:
SVH     22 July 1994    Moved PICS related routines to seperate file
CRI     05 Sept 1994    MSTP S/W Conversion (VICAR Porting)
RGD     28 Sept 2000    Added BAND parameter
AXC     15 May  2001    Modified pic_read.c routine to process multiband image
                        on VMS system (AR-105427)

.LEVEL1
.VARI PIC_INP
PIC format image file
.VARI OUT
VICAR output file
.VARI BAND
Starting band number to read
.VARI NBANDS
Number of bands to read
.LEVEL2
.VARI PIC_INP
This parameter specifies the complete path name
of the PIC format image file of one or more bands
to be converted to a single-band VICAR image.
.VARI OUT
This parameter specifies the complete path name
of the output VICAR compressed image.
.VARI BAND
Band number to read.  Specifying a band # too large will result in
an end of file error.  Bands start counting at 1.
.VARI NBANDS
Number of bands to read.  If (BAND-1) + NBANDS > # Band in Image,
then an error will occur.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpic2vic.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
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
write "*** Again to VICAR format with cropped size=(SL,SS,NL,NS)"
write " "
let $echo="yes"
pic2vic testpic testvic2 size=(5,5,10,10)

let $echo="no"
write "*** List labels and contents in with cropped size"
write " "
let $echo="yes"
label-list testvic2
list testvic2 nl=10 ns=10


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

end-proc
$!-----------------------------------------------------------------------------
$ create new_3d_session.log
tstpic2vic
 
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
---- Task: GEN -- User: ntt -- Tue Sep 16 11:13:15 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:15 2003
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
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:15 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:15 2003
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
*** Again to VICAR format with cropped size=(SL,SS,NL,NS)
 
pic2vic testpic testvic2 size=(5,5,10,10)
Beginning VICAR task pic2vic
 
PIC2VIC version 15 Sept 2003
 
let $echo="no"
*** List labels and contents in with cropped size
 
label-list testvic2
Beginning VICAR task label
************************************************************
 
        ************  File testvic2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:16 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:16 2003
     Samp     1       3       5       7       9
   Line
      1       8   9  10  11  12  13  14  15  16  17
      2       9  10  11  12  13  14  15  16  17  18
      3      10  11  12  13  14  15  16  17  18  19
      4      11  12  13  14  15  16  17  18  19  20
      5      12  13  14  15  16  17  18  19  20  21
      6      13  14  15  16  17  18  19  20  21  22
      7      14  15  16  17  18  19  20  21  22  23
      8      15  16  17  18  19  20  21  22  23  24
      9      16  17  18  19  20  21  22  23  24  25
     10      17  18  19  20  21  22  23  24  25  26
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
---- Task: GEN -- User: ntt -- Tue Sep 16 11:13:16 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic3d nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:16 2003
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


 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:16 2003
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


 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:16 2003
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
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:17 2003 ----
 
************************************************************
list testvic3d2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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


 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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


 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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
$ create old_3d_session.log
tstpic2vic
 
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
---- Task: GEN -- User: ntt -- Tue Sep 16 11:13:55 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:55 2003
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
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:55 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:55 2003
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
*** Again to VICAR format with cropped size=(SL,SS,NL,NS)
 
pic2vic testpic testvic2 size=(5,5,10,10)
Beginning VICAR task pic2vic
 
PIC2VIC version 15 May 2001
 
let $echo="no"
*** List labels and contents in with cropped size
 
label-list testvic2
Beginning VICAR task label
************************************************************
 
        ************  File testvic2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:56 2003 ----
 
************************************************************
list testvic2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:56 2003
     Samp     1       3       5       7       9
   Line
      1       8   9  10  11  12  13  14  15  16  17
      2       9  10  11  12  13  14  15  16  17  18
      3      10  11  12  13  14  15  16  17  18  19
      4      11  12  13  14  15  16  17  18  19  20
      5      12  13  14  15  16  17  18  19  20  21
      6      13  14  15  16  17  18  19  20  21  22
      7      14  15  16  17  18  19  20  21  22  23
      8      15  16  17  18  19  20  21  22  23  24
      9      16  17  18  19  20  21  22  23  24  25
     10      17  18  19  20  21  22  23  24  25  26
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
---- Task: GEN -- User: ntt -- Tue Sep 16 11:13:56 2003 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
list testvic3d nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:56 2003
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


 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:56 2003
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


 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:13:56 2003
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
[TAE-BADPAR] 'nb' is an undefined parameter or unknown qualifier.;
 proc 'tstpic2vic', line 80
continue
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
---- Task: PIC2VIC -- User: ntt -- Tue Sep 16 11:13:17 2003 ----
 
************************************************************
list testvic3d2 nl=10 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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


 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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


 Task:PIC2VIC   User:ntt       Date_Time:Tue Sep 16 11:13:17 2003
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
$ Return
$!#############################################################################
