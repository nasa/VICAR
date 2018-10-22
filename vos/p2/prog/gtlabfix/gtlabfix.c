#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

void main44(void)
{
  int i,i_unit,parmct,parmdf,nelement,maxlen;
  int status,len,len2,nlabfix;
  double rval;
  char property[100],key[33],valformat[9];
  char *p,value[5000],ovalue[50];
  char keyvec[1000][33],ovaluevec[1000][1000];
  
  zifmessage( "gtlabfix version 2017-05-31" );
   
  /* get parameters, open file */
   
  zvparm("property",property,&parmct,&parmdf,1,99);
  
  status = zvunit(&i_unit,"INP",1, NULL);
  status = zvopen(i_unit,"OP","UPDATE",
	"OPEN_ACT","SA","IO_ACT","SA","TYPE","IMAGE", NULL);
  
  nlabfix = 0;
  do {
     status=zlninfo(i_unit,key,valformat,&maxlen,
        &nelement,"ERR_ACT"," ", NULL);
     if (status!=1) break;
     if (strcmp(key,"PROPERTY")==0) continue;
     status=zlinfo(i_unit,"PROPERTY",key,valformat,
        &maxlen,&nelement,"PROPERTY",property,
        "ERR_ACT"," ", NULL);
     if (status!=1) continue;
     if (strcmp(key,"PROPERTY")==0) continue;
     /*if (strcmp(valformat,"STRING")!=0) continue;*/
     
     status=zlget(i_unit,"PROPERTY",key,value,"ERR_ACT","SA",
        "FORMAT","STRING","PROPERTY",property, NULL);
     strcpy(keyvec[nlabfix],key);
     strcpy(ovaluevec[nlabfix++],value);
     
     len = strspn(value,"0123456789Ee+-.");
     if (len!=strlen(value)) continue;
     p = strpbrk(value,"Ee");
     if (p==0) continue;
     len2 = strlen(p);
     if (len2>4) continue;
     
     sscanf(value,"%lf",&rval);
     sprintf(ovalue,"%18.15f",rval);
     
     strcpy(ovaluevec[nlabfix-1],ovalue);
     }
  while (1);
    
  for (i=0;i<nlabfix;i++)
     {
     status=zldel(i_unit,"PROPERTY",keyvec[i],
        "ERR_ACT","SA","PROPERTY",property, NULL);
     status=zladd(i_unit,"PROPERTY",keyvec[i],ovaluevec[i],
        "ERR_ACT","SA","FORMAT","STRING","PROPERTY",property, NULL);
     }
  
  return;
}
