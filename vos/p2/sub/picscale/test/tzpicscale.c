/* TZPICSCALE -- Test program for subroutine PICSCALE */
#include <stdio.h>
#include <string.h>
#include "vicmain_c"
#include "ftnbridge.h"
#include "mp_routines.h"

#define THIS_ROUTINE "tzpicscale"
#define DBUG 0
#define LOCAL  0
#define REMOTE 1
static  char project [6]  = "     ";
static  char source  [5]  = "    ";
static  char planet  [13] = "            ";
void t_mpGetValues();

void main44()
{
    float  hscale,vscale;           /* scale in meters/pixel */
    float  noraz,sunaz,scaz;	    /* north, solar, and s/c azimuth angles */
    double line,samp;               /* Coordinates at which scale applies */
    int    nl,ns;
    int    input,frame_id,mptype,status;
    double sbuf[200]; 	         /* buffer returned by GETSPICE */
    float  pbuf[30];	         /* buffer returned by PICSCALE */
    int camera;
    float  focal_length,oal,oas,pxlscale;
    double sclat,sclon;		/* spacecraft lat-lon */
    float srange;        	/* slant range */
    char msg[132];

    int index;

/*   VARIABLES USED FOR CALLING NEW MP_ROUTINES */
    MP     mp;
    char   MapType[mpMAX_KEYWD_LENGTH];
    char   keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1] ;
    float  data  [40];  
    int    type  [mpNUMBER_OF_KEYWORDS];
    int    class [mpNUMBER_OF_KEYWORDS];
    int    number_of_keywords;

    zifmessage ("TZPICSCALE version March 21, 1998"); 

    memset (project,'\0',sizeof(project));
    memset (source, '\0',sizeof(source));
    memset (planet, '\0',sizeof(planet));

    for (index=0; index<200; index++)
       sbuf[index] = 0.0;
    for (index=0; index<30; index++)
       pbuf[index] = 0.0;
    for (index=0; index<40; index++)
       data[index] = 0.0;

    status = zvunit(&input,"INP",1, 0);
    status = zvopen(input, "OPEN_ACT","SA","IO_ACT","SA", 0);
    status = zvget(input,"NL",&nl,"NS",&ns, 0); 

    zgetproj(input,project,&camera,&frame_id,&status); 
    zgetcamcon(project,camera,&focal_length,&oal,&oas,&pxlscale,&status);
    status = zgetspice2(input,1,sbuf);

    status = mpInit(&mp);
    if (status == -1) {
       sprintf(msg,"***mpInit error status: %d.\n",status);
       zvmessage(msg,"");
       zabend();
    };   
    status = mpLabelRead(mp,input );
    if (status != mpSUCCESS) {
       mptype = 7;           /* if not map projected, assume image-space */
       mp = NULL;
       zmve (8,9,&sbuf[58],&data[0],1,1); /* OM Matrix */
       zmve (8,3,&sbuf[21],&data[18],1,1);/* RS Vector */
       data[24] = (float)sbuf[14];     /* polar radius */
       data[25] = (float)sbuf[12];     /* equitorial radius */
       data[26] = focal_length;        /* camera focal length */
       data[27] = oal;                 /* optical axis line */
       data[28] = oas;                 /* optical axis sample */
       data[29] = pxlscale;            /* picture scale in pixels/mm */
       data[37] = (float)sbuf[26];     /* space craft range */
       sclat    = sbuf[29];  	       /* space craft latitude */
       sclon    = sbuf[30];	       /* space craft longitude */		
    } else {
       mptype = -7;
       status = mpGetValues(mp,mpMAP_PROJECTION_TYPE,MapType,0);
       if (status != mpSUCCESS) {
	 sprintf (msg,
         "***mp error, unrecognized projection, status: %d.",status);
 	 zvmessage(msg, "");
	 mpFree( mp );
	 zabend();
       }
       status=mpGetKeywords(mp,keywords,&number_of_keywords,&type[0],&class[0]);
       if (status != mpSUCCESS) {
	 sprintf (msg,"***mpGetKeywords error, status: %d.",status);
 	 zvmessage(msg, "");
	 mpFree( mp );
	 zabend();
       }
    }

    zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
    hscale = pbuf[0];
    vscale = pbuf[1];
    sunaz  = pbuf[3];
    scaz   = pbuf[4];
    srange = pbuf[5];
    noraz  = pbuf[6];

    sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
    zvmessage(msg,"");
    sprintf(msg,"noraz=%e  sunaz=%e  scaz=%e  ",noraz,sunaz,scaz);
    zvmessage(msg,"");
    sprintf(msg,"slant range=%e",srange);
    zvmessage(msg,"");
	   /* If map projection type other than 7, 8, 16 terminate test */
   if (mptype != 7 && mptype != 8 && mptype != 16) return;

/*  Move optical-axis around to test other parts of the code */
/*  then Call zpicscale again */

      oas = -300;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];

      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

      oal = 800.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

      oal = 0.0;
      oas = 400.0;
      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

      oal = 1100.0;
      oas =  400.0;
  
      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************* */

      oal =  100.0;
      oas = -300.0;

      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************* */

      oal = 100.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;
  
      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

/*    Quick Test  of Fortran to C bridge */
/*    zpicscale (sbuf,data,mp,mptype,sclat,sclon,nl,ns,pbuf,&line,&samp); */

      oal = 100.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;

      FTN_NAME(txpicscale) (&sbuf[0], &data[0], &mp, &mptype, &sclat, &sclon, 
                            &nl, &ns, &pbuf[0], &line, &samp);
      hscale = pbuf[0];
      vscale = pbuf[1];

      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

      status = mpFree( mp );
}
/************************************************************************************/
/***  GET (INT) VALUE FROM MP OBJECT                                 **/
/************************************************************************************/
void t_mpGetValues(mp,  keyword,  value)
MP mp;
char *keyword;
int  *value;
{
int status, results;
static char msg[132];

      status = mpGetValues( mp,  keyword,  &results, "" );

      *value = results;
      if (status != mpSUCCESS) { 
	sprintf
          (msg, "Call to mp_routines mpGetValues returned error status: %d.\n",
           status);
	zvmessage(msg, "");
	mpFree( mp );
	zabend();
      }
}
