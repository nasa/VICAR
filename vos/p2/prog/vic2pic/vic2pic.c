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
