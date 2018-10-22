/************************************************************************/
/*  C-Callable Version zlightning                                       */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zlighting (buf,lat,lon,phase,incidence,emission)  
double buf[];         /* spice buffer  */
double lat;           /* latitude  */
double lon;           /* longitude */
double *phase;        /* returned angle */
double *incidence;    /* returned angle */
double *emission;     /* returned angle */ 
{
FTN_NAME2(lighting, LIGHTING) (buf,&lat,&lon,phase,incidence,emission) ;
}
