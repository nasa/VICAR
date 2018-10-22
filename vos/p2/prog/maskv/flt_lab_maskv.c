#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

struct  node { char * str;  /* LINK list for output buffer */
	       struct node * next;
	     };
typedef struct node NODE;
NODE *flight_label();

void FTN_NAME(flt_lab_maskv)(unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf)
int *unit;	
int *lmarg, *lfact, *reduce, *iline, *nsw, *ntime;
char *outbuf; 
{ zflt_lab_maskv(unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf);
}

void  zwline (iline,nsw,buf,nline)  
int   *iline,*nsw,*nline;
char  *buf;
{
FTN_NAME(wline) (iline,nsw,buf,nline) ;
}

zflt_lab_maskv( unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf)
int *unit;		/* Unit number of file whose label is listed	      */
int *lmarg, *lfact, *reduce, *iline, *nsw, *ntime;
char * outbuf;
{NODE * list;
 int j,ntime2,length,expnd;
   expnd = *lfact - * reduce;
   ntime2 = *ntime * 2;
   list = flight_label(*unit);
if (list != NULL)
   while (list != NULL){
          for (j=0; j<7; j++) {
	    length = strlen(list->str);
            ztext(list->str,length,j,&outbuf[*lmarg],6,0);
	    if (*lfact - *reduce > 1) {
	             length = length * 6;
		     zexpand(&outbuf[*lmarg],length,expnd);
		                      }
	   zwline(iline,nsw,outbuf,ntime);
	                      }
	  memset(outbuf,0,*nsw);
	  zwline(iline,nsw,outbuf,&ntime2);
	  list = list->next;
	}
else return -1;
}


