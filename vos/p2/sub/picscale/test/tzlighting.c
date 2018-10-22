#include <math.h>
#include <stdio.h>
#include "vicmain_c"
#include "mp_routines.h"
#ifndef  TRUE
#define TRUE  1
#define FALSE 0
#endif
void main44(){
double	spicebuf[100];		
MP      mpo;
int	i,j,camera,fsd,ptype,inunit,
	nl,ns,istat,mode,ilat;	
float	data[40],pbuf[30],focal_length,	
	oal,oas,pxlscale,radius,srange,
        rline,rsamp,rlat,rlon;         	
double  dlat,dlon,sclat,sclon,line,samp;
char    msg[132],project[6];

   istat=zvunit(&inunit,"INP",1,0);   
   istat=zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA",0);
   zgetproj(inunit,project,&camera,&fsd,&istat);
   if(istat != 0) zprnt(4,1,&istat,"zgetproj fatal indicator=.");
   if(!zgetspice2(inunit,1, spicebuf)) exit(1);
   istat=zvget(inunit,"NL",&nl,"NS",&ns,0);	
   /* Get Camera constants			*/
   zgetcamcon(project,camera,&focal_length,&oal,&oas,&pxlscale,&istat);
   /* set up call to PICSCALE */
   ptype=8;
   zspice2convev(spicebuf,focal_length,oal,oas,pxlscale,ptype,data);
   sclat=spicebuf[29];
   sclon=spicebuf[30];
   zvmessage("Items calculated by PICSCALE using zspice2convev buffer"," ");
   zpicscale(spicebuf,data,mpo,ptype,sclat,sclon,nl,ns,pbuf,&line,&samp);
   sprintf(msg,"line=%f samp=%f",line,samp);
   zvmessage(msg," ");
   sprintf(msg,"HSCL = %-15.5f   VSCL   = %-15.5f",pbuf[0],pbuf[1]);  
   zvmessage(msg," ");
   sprintf(msg,"NORAZ= %-15.5f   SUNAZ  = %-15.5f",pbuf[2],pbuf[3]);	
   zvmessage(msg," ");       
   sprintf(msg,"SCAZ = %-15.5f   SLRANGE= %-15.5f",pbuf[4],pbuf[5]);	
   zvmessage(msg," ");  
   mode = 2;  /* 2=convert from line-samp to lat-lon*/
   ilat = 0;  /* 0=planetocentric latitudes */
   rline = line;
   rsamp = samp;
   zpproj(data,&rline,&rsamp,&rlat,&rlon,mode,ilat,&radius,&srange,&istat);
   dlat = (double) rlat;
   dlon = (double) rlon;
   zvmessage("Items calculated by LIGHTING ********************"," ");
   zlighting(spicebuf,dlat,dlon,&spicebuf[80],&spicebuf[78],&spicebuf[79]);
   sprintf(msg,"INA = %-15.5f   EMA = %-15.5f",spicebuf[78],spicebuf[79]);
   zvmessage(msg," ");
   sprintf(msg,"PHA = %-15.5f",spicebuf[80]);	
   zvmessage(msg," ");
   istat=zvclose(inunit,0);	     /* Close input file		*/
}
