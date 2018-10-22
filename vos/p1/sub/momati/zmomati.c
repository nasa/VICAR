#include "xvmaininc.h"
#include "momati.h"
void zmomati(double oal, double oas,double ssl,double sss,double scale,
	     double fl,double sclo,double scla,double angn,double range,
	     void *a,void *rs)
{
momati_c(oal,oas,ssl,sss,scale,fl,sclo,scla,angn,range,a,rs);
}
