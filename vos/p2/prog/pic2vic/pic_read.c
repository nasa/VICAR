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
