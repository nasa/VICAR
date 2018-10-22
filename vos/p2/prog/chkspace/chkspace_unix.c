/*********************************  chkspace_unix  ****************************
 * CHKSPACE_UNIX  (Check for available space on a disk)
 *
 *  REVISION HISTORY
 *   26-FEB-90  TCG  First version
 *   02-JAN-95  CRI(RNR) MSTP S/W Conversion (VICAR Porting) 
 *   21-AUG-95  As per FR85841 local chkspace.ush used and -a removed from df
 ******************************************************************************/
#include <stdio.h>
#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include <math.h>
#include <string.h>

/*** Routine definitions      ***/

static int get_parameters();
static int write_to_taevbl();
static int get_file_data();
static int convert_ascii();

/*** Define constants        ***/ 

#define SUCCESS 1
#define FAILURE 0
#define PARSIZE 5000

void main44(void)
{
    char message[512];		/* User message buffer */
    int result_blocks = 0;	/* Number of blocks to tell user */
    char file_name[128];	/* Temporary file name input */
    char ascii_space[20];	/* available space in ascii */
    int space_length;		/* Length of available space string */
    int status;                 /* Return status */

    zifmessage ("CHKSPACE version 02-JAN-95"); 

    /* Get the temporary file name */
    get_parameters(file_name);

    /* Open the file and read in the available space from UNIX DF */
    status = get_file_data (file_name, ascii_space, &space_length) ;  
    if (status == SUCCESS) {

      if (space_length > 0)
          convert_ascii (ascii_space, space_length, &result_blocks) ;  
      else result_blocks = 0;

      zvmessage("CHKSPACE: Space constrained by disk space.","");
    }
    else
      zvmessage("CHKSPACE: DISK name not valid.","");

    sprintf(message, "CHKSPACE: %d blocks available.", result_blocks);
    zvmessage(message, "");

    /* return the results to a TAE variable */
    write_to_taevbl(&result_blocks);

    return;
}

static int get_file_data (char *filename, char *space, int *length)
{                                    
    FILE *fileptr;
    char errfile[7] = "errors";	/* error file name */

    /* see if error file was created */
    fileptr = fopen (errfile, "r");
    if (fileptr) {
       fclose (fileptr);
       return FAILURE;
    }
    /* open the temporary file */
    fileptr = fopen (filename, "r");
    if (!fileptr) {
       return FAILURE;
    }

    space[0] = 0;

    /* get the ascii number */
    fgets (space,20,fileptr);

    /* get the string length */
    *length = strlen (space);

    /* close the temporary file */
    fclose (fileptr);
                                                   
    return SUCCESS;                                                                 
}

static int get_parameters(char *fname)
/*
 *  Returns the value of TCL parameter file in 'fname'
 */
{
    int count;

    /* get the temporary file name */
    zvp("INP", fname, &count);

    return SUCCESS;
}

static int convert_ascii(char *aspace, int length, int *blocks)
/*
 *  Returns the converted value of available 512 byte blocks'
 */
{
    int i, tspace;

    *blocks = 0;
    for (i=0; i<length-1; i++)
      {
        tspace = (unsigned char) *(aspace + i) - 48;
        tspace *= (int)pow ((double)10,(double)((length - 2) - i));
        *blocks += tspace;
      }
      *blocks *= 2;  /* convert kilo-bytes to 512 byte blocks */

    return SUCCESS;
}

static int write_to_taevbl(int *size)
/*
 *  Writes the value size into the TAE varaiable VAR_NAME
 */
{
    struct PARBLK par_block;

    char name[9] = "LOCALVAR";	/* TCL name - where to put results */
    int tsize[2] =		/* Temp size, requires array (!?) */
    {0, 0};

/* Load a temp variable with the results */
    tsize[0] = *size;

/* Init parameter block */
    q_init(&par_block, P_BYTES, P_ABORT);

/* Write an integer */
    q_intg(&par_block, name, 1, tsize, P_ADD);


/* Return parameter block to TAE */
    zvq_out(&par_block);

    return SUCCESS;
}
/*****************************************************************************/
/*                              End of Module                                */
/*****************************************************************************/
