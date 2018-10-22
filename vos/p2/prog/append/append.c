/* C     MODIFIED FOR VAX CONVERSION BY ASM, 26 AUG 1983		*/
/* C									*/
/* C     84-11-26   ...LWK... CONVERTED TO VICAR2			*/
/* C									*/
/* C     85-5-2     ...BAM... ADDED ABILITY TO INPUT 30 INPUT DATA SETS	*/
/* C                          ( MAXIMUM VICAR ALLOWS )			*/
/* C									*/
/* C     89-4-3     BAM - INCREASED BUFFER SIZE TO 300000		*/
/* C									*/
/* 14 July 1994	SVH	Completely trashed the old program since it	*/
/*			did a lot of comparisons in a silly way, and	*/
/*			had enough flaws and missing capabilities	*/
/*			that it required a scratch rewrite.  As long	*/
/*			as I had to rewrite, I rewrote in C.		*/
/*			And also it's ported to UNIX now.		*/
#include <stdio.h>
#include <string.h>
#if UNIX_OS
#include <memory.h>
#endif
#include "taeconf.inp"
#include "vicmain_c"
#include "parblk.inc"
#include "pgminc.inc"


void main44(void)
{
  int i,j;			/* counter variables */
  int def;			/* default flag for zv routines */
  char dataline[80];
  char *buf;
  char infiles[30][80];		/* input file names */
  int u[30];			/* unit number */
  int nds;			/* number of data files */
  int nli[30];			/* number of lines in data file */
  int nsi;			/* number of samples in current input file */
  int ibytes;			/* bytes used in current format for 1 pt. */
  char fcod[10],prev_fcod[10];  /* Current file formats */
  char ofmt[10];		/* output file format */
  char error_msg[80];
  
  int oun;
  int nline=0;			/* total number of lines for output file */
  int nsamp=0;			/* number of samples on a line -output file */
  int nbyt=0;			/* Number of bytes on a line */
  int fbyt=0;			/* max # bytes in input file */
  int flag=0;			/* 0=all same, 1=mixture */

  zvparm( "INP", infiles, &nds, &def,30,80);

  /* Read all the vicar labels to find the size of the output pic. */

  for(i=0; i<nds; i++) {
    /* get parameters from this file */
    zvunit(&u[i], "INP", i+1, NULL);
    zvopen( u[i], "OPEN_ACT", "SA", "IO_ACT", "SA",NULL);
    zvget( u[i], "NL", &nli[i], "NS", &nsi, "FORMAT", fcod,NULL);
    zvpixsize(&ibytes, fcod, "LOCAL", "LOCAL");

    /* issue warning if we are mixing file types */
    if (i==0) {
      strcpy(prev_fcod,fcod);
      strcpy(ofmt,fcod);
    } else if ( strcmp(fcod,prev_fcod)) {  /* if they are _NOT_ the same type */
      zvmessage("** WARNING: YOU ARE MIXING DATA TYPES **","");
      flag=1;
      if (strcmp(ofmt,"COMP")) 		/* if old isn't complex-the maximum */
        if (!strcmp(fcod,"COMP")) 	/* if new format complex */
          strcpy(ofmt,"COMP");
        else if (!strcmp(fcod,"DOUB")) 	/* if new format double */
            strcpy(ofmt,"DOUB");
          else if (strcmp(ofmt,"DOUB"))/* if old isn't double - the 2nd max */
            if (!strcmp(fcod,"REAL")) 	/* if new format real */
              strcpy(ofmt,"REAL");
            else if (strcmp(ofmt,"REAL")) 	/* if old isn't real-3rd max */
              if (!strcmp(fcod,"FULL")) 	/* if new format full */
                strcpy(ofmt,"FULL");
              else if (strcmp(ofmt,"FULL")) 	/* if old isn't full */
                if (!strcmp(fcod,"HALF")) 	/* if new format half */
                  strcpy(ofmt,"HALF");
                else if (strcmp(ofmt,"HALF")) 	/* if old isn't half */
                  if (!strcmp(fcod,"BYTE")) 	/* if new format byte */
                    strcpy(ofmt,"BYTE");
                  else {
                    sprintf(error_msg,
				"** ERROR: UNRECOGNIZED TYPE %s **\n",fcod);
                    zvmessage(error_msg,"");
                  }
    }

    /* update final output parameters */
    nline += nli[i];
    nbyt = max(nbyt, nsi* ibytes);
    fbyt = max(fbyt, nsi* nli[i]* ibytes);
    nsamp = max( nsamp, nsi);

    /* close the file - right now it is in its own format. */
    zvclose(u[i],NULL);
  }
  buf = (char *) malloc(fbyt);

  /* Clue in the user on what's gonna happen. */
  sprintf(dataline," OUTPUT: NL=%6d, NS=%6d, FORMAT=%s.\n",nline, nsamp, ofmt);
  zvmessage(dataline,"");

  /* open and write the final output file */
  zvunit( &oun, "OUT", 1, NULL);
  zvopen( oun, "OP", "WRITE", "U_NL", nline, "U_NS", nsamp, "O_FORMAT", ofmt,
	"U_FORMAT", ofmt, "OPEN_ACT", "SA", "IO_ACT", "SA",NULL);
  for(i=0; i<nds; i++) {		/* for every file */
    zvopen( u[i], "OPEN_ACT", "SA", "IO_ACT", "SA","U_FORMAT",ofmt,NULL);
    memset(buf,0,nbyt);
    for(j=0; j<nli[i]; j++) {		/* copy over every line in the file */
      zvread (u[i],buf,NULL);
      zvwrit (oun,buf,NULL);
    }
    zvclose(u[i],NULL);
  }
  zvclose(oun,NULL);

  return;
}
