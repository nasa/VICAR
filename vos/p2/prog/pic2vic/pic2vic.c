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

