/*******************************************************************************

  Title:     io_view.c
  Author:    Mike Burl
  Date:      (1992/06/29 --- 2005/01/26)
  
  Substantially revised by Mike Burl; 
  Original program was by Jim Brase and included the following
  copyright and notices:

     (c) Copyright 1987 the Regents of the University
         of California. All rights reserved.

     This work was produced at the Lawrence Livermore
     National Laboratory. The United States Government
     retains certain rights therein.


HISTORY:
-------
20050218 (MCB) - Added code to strip off (.sdt) filename extension if 
              provided in filename.

20050127 (MCB) - Changed source file from vio.c to io_view.c. Added in all 
              the shorthand functions like write_bfile, write_dfile, etc.
              in this source. Changed names of vread.c and vwrite.c to
              read_view.c and write_view.c. Kept old functions vread.c and
              vwrite.c around for backward compatibility. Also, made some
              cosmetic changes to the formatting and added some comments
              to rd_view_par. Changed the symbolic names of some of
              the types to be more clear. (WORD -> SHORT, LWORD -> INT,
              REAL -> FLOAT). Types are now defined in io_types.h.

20020719 (MCB) - A problem similar to the 20020507 bug was observed
              when using vwrite so added the O_BINARY flag and WIN32 
              ifdefs to vwrite.

20020507 (MCB) - Observed a problem under Windows in which the binary
              .sdt file is interpretted as a "text file" and the number of
              bytes read is errorneously reported in rd_data. We found a
              fix and implemented it; I also added prototypes in here for the
              various "helper" functions. Removed errno.h. Removed printAlert
              function and upgraded error reporting to go to stderr.

20020211 - Didn't actually change anything (except reordering history).
              Need to verify that vwrite with all of dims = 0 creates
              a .sdt file and appropriate spr file.
 
20010605 - Realized an error in shuffle_bytes was causing data to get scrambled
              on little endian platforms. Need to shuffle_bytes into a new array.
              Cannot do it in place like original shuffle_bytes assumed.

20010327 - Upgraded function specifications to more modern style and included
              in vio.h. Added endianness check on read and write. Upgraded some 
              of the error reporting.

              *** Convention is that files will always be written so bytes are in 
              *** BIG_ENDIAN form on disk. Upon reading, bytes will be swapped to 
              *** native platform endianness in memory.

19961210 - It was necessary to add fcntl.h, which contains the
              definitions of O_RDONLY, etc. Apparently, these constants 
              are in <sys/file.h> on Berkeley systems and <fcntl.h> on System V UNIX.
              In the past vio could be compiled using just <sys/file.h> but probably
              this no longer works due to changes in the local system.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#ifdef WIN32
#include <io.h>
#else
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <fcntl.h>
#include <string.h>
#include "endianness.h"
#include "qmalloc.h"
#include "imageio_return_values.h"
#include "imageio_types.h"
#include "io_view.h"
#include "pfx.h"


/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/
#ifndef MAXSTRING
#define MAXSTRING 1024
#endif

#define MAXDIM 4

/* These functions are used internally, but do not need to be exposed outside */
int rd_data(unsigned char *ptr, int num_bytes, char *filename);
int wr_view_par(char *filename, int type, int ndim, int *dim);

/**************************************/
/* READ_VIEW                          */
/**************************************/
/* Read a VIEW format file. Returns either number of bytes read or ERR. */
/*   It is assumed that filename comes in without an extension. */

int read_view(char *filename, int *type_adr, int *ndim_adr, int *dim, unsigned char **B_adr)

{
  int           p, f, e, L;
  char          ext[MAXSTRING], tmpfile[MAXSTRING];
  float         ori[MAXDIM], inter[MAXDIM];
  float         dispmin, dispmax;
  int           size, i, n ;
  int           num_bytes ;
  char          data_filename[MAXSTRING];
  unsigned char *tmp;
  unsigned char *B;
  char          infunc[] = "read_view";

  /* Following code was added by MCB on 2005/02/18 */
  /* Check for a .sdt extension and strip it off if found */
  pfx(filename, &p, &f, &e);
  L = strlen(filename);
  if (e < L) {
    /* has_ext = TRUE; */
    sprintf(ext, "%s", filename+e); 
    if (strcasecmp(ext, ".sdt") == 0) {
      /* strip extension */
      for (i = 0; i < e; i++) {
        tmpfile[i] = filename[i];
      }
      tmpfile[e] = '\0';
    }
  }
  else {
    sprintf(tmpfile, "%s", filename);
  }
  /* end added code */

  /* read parameter file */
  read_view_header(tmpfile, type_adr, ndim_adr, dim, ori, inter, &dispmin, &dispmax) ;

  /* calculate number of bytes in signal */
  switch (*type_adr) {
   case BYTE:
    num_bytes = 1 ;
    break ;

   case SHORT:
    num_bytes = 2 ;
    break ;

   case INT:
    num_bytes = 4 ;
    break ;

   case FLOAT:
    num_bytes = 4 ;
    break ;

   case COMPLEX:
    num_bytes = 8 ;
    break ;

   case DOUBLE:
    num_bytes = 8;
    break;

   default:
    num_bytes = 1 ;
  }

  size = 1 ;
  for (i = 0; i < (*ndim_adr); i++) {
    size = size * dim[i] ;
  }
  size = size * num_bytes ;
  /* printf("rd size = %d\n", size); */
  B  = (unsigned char *) qmalloc(size, sizeof(unsigned char), 0, infunc, "B");

  /* read data file into signal buffer */
  strcpy(data_filename, tmpfile) ;
  strcat(data_filename, ".sdt") ;

  if ((n = rd_data(B,size,data_filename)) < 0) {
    fprintf(stderr, "ERROR (%s): rd_data failed on %s\n", infunc, data_filename);
    free((void *) B); /* MCB - Added this line 20050127 */
    *B_adr = NULL;
    return(ERR) ;
  }
  else {
    *B_adr = B;
    if (endianness() == ENDIAN_LITTLE) {
      /* Need to shuffle bytes */
      tmp = (unsigned char *) qmalloc(size, sizeof(unsigned char), 0, infunc, "tmp");
      /* Copy contents at B into tmp then shuffle tmp */
      for (i = 0; i < size; i++) {
	tmp[i] = B[i];
      }
      shuffle_bytes(size/num_bytes, num_bytes, tmp, B);
      free((void *) tmp); /* MCB - Added this line 20050127 */
    }
    return(n) ;
  }
}

/*******************************************/
/* WRITE_VIEW                              */
/*******************************************/

int write_view(char *filename, int type, int ndim, int *dim, unsigned char *B)

{
  int           fp, i, n ;
  int           num_bytes;
  int           size ;
  unsigned char *tmp;
  int           L, p, f, e;
  char          ext[MAXSTRING], tmpfile[MAXSTRING];
  char          data_filename[MAXSTRING] ;
  char          infunc[] = "write_view";


  /* Following code was added by MCB on 2005/03/08 */
  /* Check for a .sdt extension and strip it off if found */
  pfx(filename, &p, &f, &e);
  L = strlen(filename);
  if (e < L) {
    /* has_ext = TRUE; */
    sprintf(ext, "%s", filename+e); 
    if (strcasecmp(ext, ".sdt") == 0) {
      /* strip extension */
      for (i = 0; i < e; i++) {
        tmpfile[i] = filename[i];
      }
      tmpfile[e] = '\0';
    }
  }
  else {
    sprintf(tmpfile, "%s", filename);
  }
  /* end added code */

  /* write parameter file */
  if (wr_view_par(tmpfile, type, ndim, dim) != 0) {
    fprintf(stderr, "ERROR (%s): wr_view_par on %s failed\n", infunc, tmpfile);
    return(ERR);
  }

  /* calculate number of bytes in signal */
  size = 1 ;
  for (i = 0; i < ndim; i++) {
    size *= dim[i] ;
  }
  switch (type) {
    case BYTE:    num_bytes = 1 ; break ;
    case SHORT:   num_bytes = 2 ; break ;
    case INT:     num_bytes = 4 ; break ;
    case FLOAT:   num_bytes = 4 ; break ;
    case COMPLEX: num_bytes = 8 ; break ;
    case DOUBLE:  num_bytes = 8 ; break ;
    default:
      fprintf(stderr, "ERROR (%s): Illegal data type=%d in file %s\n", infunc, type, tmpfile) ;
      return(ERR) ;
      break ;
  }
  size = size * num_bytes;

  strcpy(data_filename,tmpfile) ;
  strcat(data_filename,".sdt") ;

#ifdef WIN32
  fp = open(tmpfile,O_WRONLY|O_BINARY|O_TRUNC,0777) ;
#else
  fp = open(data_filename,O_WRONLY|O_TRUNC,0777) ;
#endif
  if (fp == -1) {
#ifdef WIN32
    fp = open(tmpfile,O_WRONLY|O_BINARY|O_CREAT,0777) ;
#else
    fp = open(data_filename,O_WRONLY|O_CREAT,0777) ;
#endif
    if (fp == -1) {
      fprintf(stderr, "ERROR (%s): Cannot open signal file %s\n", infunc, data_filename) ;
      return(ERR) ;
    }
  }
  if ((endianness() == ENDIAN_LITTLE) && (type != BYTE)) {
    /* Need to shuffle bytes to ENDIAN_BIG form prior to writing */
    tmp = (unsigned char *) qmalloc(size, sizeof(unsigned char), 0, infunc, "tmp");
    shuffle_bytes(size/num_bytes, num_bytes, B, tmp);
    n = write(fp, tmp, size) ;
    free((void *) tmp);
  }
  else {
    n = write(fp, B, size) ;
  }
  /* printf("n = %d\n", n); */
  if (n != size) {
    fprintf(stderr, "ERROR (%s):  Error writing file %s\n", infunc, data_filename) ;
    close(fp) ;
    return(ERR) ;
  }

  close(fp) ;

  /* change protections of file written to world read-write */
  if (chmod(data_filename,0666) < 0) {
    fprintf(stderr, "WARNING (%s): Can't change protection of data file %s\n", infunc, data_filename) ;
  }

  return(OK) ;
}

/**************************************/
/* READ_VIEW_HEADER                   */
/**************************************/
/* It is assumed that filename comes in without an extension. */

int read_view_header(char *filename, int *type_adr, int *ndim_adr, int *dim, float *ori, 
            float *inter, float *dispmin, float *dispmax)
{
  FILE          *pfp;
  char          par_filename[MAXSTRING];
  int           i;
  char          type_string[MAXSTRING];
  char          infunc[] = "read_view_header";
    
  /* read parameter file */
  strcpy(par_filename, filename) ;
  strcat(par_filename, ".spr") ;
  pfp = fopen(par_filename,"r") ;
  /*-------------------------------------------------------------------*/
  /* This handles the abnormal case in which opening the parameter file fails */
  if (pfp == NULL) {
    /* If it is unable to open the parameter file, it prompts you to interactively enter the values */
    fprintf(stderr, "ERROR(%s): Could not open signal parameter file=%s\n", infunc, par_filename) ;
    /* Prompt to get data type */
    fprintf(stderr, "    Data type:  ") ;
    scanf("%s",type_string) ;
    switch (type_string[0]) {
      case 'B':
      case 'b':
	*type_adr = BYTE ;
	break ;
      case 'W':
      case 'w':
      case 'S':
      case 's':
	*type_adr = SHORT ;
	break ;
      case 'L':
      case 'l':
      case 'I':
      case 'i':
	*type_adr = INT ;
	break ;
      case 'R':
      case 'r':
      case 'F':
      case 'f':
	*type_adr = FLOAT ;
	break ;
      case 'C':
      case 'c':
	*type_adr = COMPLEX ;
	break ;
      case 'D':
      case 'd':
	*type_adr = DOUBLE ;
	break ;
      default:
	fprintf(stderr, "ERROR (%s): Illegal data type\n", infunc) ;
	return(ERR) ;
    }
    /* Prompt to get number of dimensions */
    printf("    Number of dimensions:  ") ;
    scanf("%d",ndim_adr) ;
    /* Prompt to get sizes along each dimension */
    for (i=0; i<*ndim_adr; i++) {
      printf("    Dimension %2d size:  ",i) ;
      scanf("%d",&dim[i]) ;
      printf("    Dimension %2d origin:  ",i) ;
      scanf("%f",&ori[i]) ;
      printf("    Dimension %2d sample interval:  ",i) ;
      scanf("%f",&inter[i]) ;
    }
    *dispmin = 0 ;
    *dispmax = 0 ;

    /* Try to write the entered parameters into a real parameter file */
    if ((pfp = fopen(par_filename,"w")) == NULL) {
      fprintf(stderr, "ERROR (%s): Could not write parameter file %s\n", infunc, par_filename) ;
      return(ERR);
    }
    else {
      fprintf(pfp,"%d\n",*ndim_adr) ;
      for (i=0; i<*ndim_adr; i++) {
	fprintf(pfp,"%d\n",dim[i]) ;
	fprintf(pfp,"%f\n",ori[i]) ;
	fprintf(pfp,"%f\n",inter[i]) ;
	}
      fprintf(pfp,"%d\n",*type_adr) ;
      fprintf(pfp,"%f\n",*dispmin) ;
      fprintf(pfp,"%f\n",*dispmax) ;
      fclose(pfp) ;
    }
  }
  /*-------------------------------------------------------------------*/
  /* This is the normal situation in which we are able to open the parameter file */
  else {
    if (fscanf(pfp,"%d",ndim_adr) != 1) {
      fprintf(stderr, "ERROR (%s):Can't read parameter file", infunc);
      return(ERR);
    }
    for (i=0; i<*ndim_adr; i++) {
      if (fscanf(pfp,"%d",&dim[i]) != 1) {
	fprintf(stderr, "ERROR (%s):Can't read parameter file", infunc);
	return(ERR);
      }
      if (fscanf(pfp,"%f",&ori[i]) != 1) {
	fprintf(stderr, "ERROR (%s):Can't read parameter file", infunc);
	return(ERR);
      }
      if (fscanf(pfp,"%f",&inter[i]) != 1) {
	fprintf(stderr, "ERROR (%s):Can't read parameter file", infunc);
	return(ERR);
      }
    }
    if (fscanf(pfp,"%d",type_adr) != 1) {
      fprintf(stderr, "ERROR (%s):Can't read parameter file", infunc);
      return(ERR);
    }
    if (fscanf(pfp,"%f",dispmin) != 1) {
      *dispmin = 0;
    }
    if (fscanf(pfp,"%f",dispmax) != 1) {
      *dispmax = 0 ;
    }
    fclose(pfp) ;
  }
  return(OK);
}

/**************************************/
/* RD_DATA                            */
/**************************************/

int rd_data(unsigned char *ptr, int num_bytes, char *filename)
{
  int  fp ;
  int  n ;
  char infunc[] = "rd_data";

    /* Windows bug fix by Geoff Menegay */
#ifdef WIN32
  fp = open(filename,O_RDONLY|O_BINARY,0) ;
#else
  fp = open(filename,O_RDONLY,0) ;
#endif

  if (fp == -1) {
    fprintf(stderr, "ERROR (%s): Cannot open signal data file %s", infunc, filename) ;
    return(ERR) ;
  }

  n = read(fp,ptr,num_bytes) ;
  if (n < num_bytes) {
    fprintf(stderr, "ERROR (%s): Unable to read data file", infunc) ;
    close(fp) ;
    return(ERR) ;
    }
  close(fp) ;


  return(n) ;
}

/*******************************************/
/* WR_VIEW_PAR                             */
/*******************************************/

int wr_view_par(char *filename, int type, int ndim, int *dim)

{
  int i ;
  FILE *pfp ;
  char  par_filename[MAXSTRING] ;
  char  infunc[] = "wr_view_par";

  /* write parameter file */
  strcpy(par_filename,filename) ;
  strcat(par_filename,".spr") ;

  if ((pfp = fopen(par_filename,"w")) == NULL) {
    printf("ERROR: Can't create parameter file %s\n",par_filename) ;
    return(ERR) ;
  }
  fprintf(pfp,"%d\n",ndim) ;
  for (i = 0; i < ndim; i++) {
    fprintf(pfp,"%d\n",dim[i]) ;
    fprintf(pfp,"%f\n", 0.0) ;    /* origin          */
    fprintf(pfp,"%f\n", 1.0) ;    /* sample interval */
    }
  fprintf(pfp,"%d\n", type) ;
  fclose(pfp) ;

  /* change protections of file written to world read-write */
  if (chmod(par_filename,0666) < 0) {
    printf("ERROR (%s):  Can't change protection of parameter file %s\n", infunc, par_filename) ;
  }

  return(OK) ;
}

/**********************/
/* WRITE_BFILE        */
/**********************/
/* Write unsigned character (byte) data to a VIEW format file. */
/* 20050127 (MCB) - Removed the redundant variable "data" */
/* 20030612 (MCB) - Original */


int write_bfile(char *outfile, int nr, int nc, unsigned char *B)

{
  int           ndim, type, dim[MAXDIM];

  type = BYTE;
  ndim = 2;
  dim[0] = nc;
  dim[1] = nr;
  write_view(outfile, type, ndim, dim, B);

  return(OK);
}


/**********************/
/* WRITE_SFILE        */
/**********************/
/* Write short data to a VIEW format file. */

int write_sfile(char *outfile, int nr, int nc, short *S)

{
  int           ndim, type, dim[MAXDIM];
  unsigned char *data;

  type = SHORT;
  ndim = 2;
  dim[0] = nc;
  dim[1] = nr;
  data = (unsigned char *) S;
  write_view(outfile, type, ndim, dim, data);

  return(OK);
}


/**********************/
/* WRITE_IFILE        */
/**********************/
/* Write integer data to a VIEW format file. */

int write_ifile(char *outfile, int nr, int nc, int *I)

{
  int           ndim, type, dim[MAXDIM];
  unsigned char *data;

  type = INT;
  ndim = 2;
  dim[0] = nc;
  dim[1] = nr;
  data = (unsigned char *) I;
  write_view(outfile, type, ndim, dim, data);

  return(OK);
}

/**********************/
/* WRITE_FFILE        */
/**********************/
/* Write floating point data to a VIEW format file. */
int write_ffile(char *outfile, int nr, int nc, float *F)

{
  int           ndim, type, dim[MAXDIM];
  unsigned char *data;

  type = FLOAT;
  ndim = 2;
  dim[0] = nc;
  dim[1] = nr;
  data = (unsigned char *) F;
  write_view(outfile, type, ndim, dim, data);

  return(OK);
}



/**********************/
/* WRITE_DFILE        */
/**********************/
/* Write double data to a VIEW format file. */
int write_dfile(char *outfile, int nr, int nc, double *D)

{
  int           ndim, type, dim[MAXDIM];
  unsigned char *data;

  type = DOUBLE;
  ndim = 2;
  dim[0] = nc;
  dim[1] = nr;
  data = (unsigned char *) D;
  write_view(outfile, type, ndim, dim, data);

  return(OK);
}
