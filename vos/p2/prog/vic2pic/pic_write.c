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
