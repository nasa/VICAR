/* Program TELLIPSE_PROJ.C */

#include <math.h>
#include "vicmain_c"

void main44()
{

  double ra,rb,rc;	/* camera radii */
  double lora;		/* longitude of semi-major axis */
  double fl;		/* Camera focal length in mm */
  double oal,oas;    	/* Optical axis intercept line-sample */
  double scale;          /* Picture scale in pixels/mm */

  double om[3][3]={0.22529468, 0.33034190, 0.91657871,
		  -0.51654703, -0.75716293, 0.39985430,
		   0.82608805, -0.56354106, 0.00005235};

  double vsc[3]={-739030.7,-321741.5,-456.1};
  double vsun[3];	/* Vector from target center to sun */

  double v[3];		/* Vector from targe center to surface point */
  double lat,lon;	/* geocentric latitude, west longitude of surface pt */
  double sunlat,sunlon,sunrange;

  double scline,scsamp,angln;
  double sclat,sclon,scrange;
  double rline,rsamp;	/* line-sample image coordinates */
  double radius;	/* planetocentric radius */
  double noraz;		/* north azimuth */
  double scaz;		/* scaz azimuth */
  double sunaz;		/* solar azimuth */
  double gcr,rad;
  int status,i;
  char msg[80];

  zvmessage(" program TELLIPSE_PROJ", "");

  rad = 180./3.141592653589793;		/* degrees per radian */
  fl = 1500.19;
  scale = 84.8214;
  oal = 500.0;
  oas = 500.0;

  ra = 1829.4;
  rb = 1819.3;
  rc = 1815.7;

  scrange = 806030.;
  sclat = -0.032;
  sclon = 156.474;
  lora = 0.;

  angln = 55.70;
  scline = 541.84;
  scsamp = 607.65;

	/* convert from rline,rsamp to v... */
  status = zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,scline,scsamp,v);
	/* convert from v to rline,rsamp */
  status = zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,&rline,&rsamp);

  sprintf(msg,"Before: (line,samp)=(%12.7f,%12.7f)",scline,scsamp);
  zvmessage(msg,0);
  sprintf(msg,"After: (line,samp)=(%12.7f,%12.7f)",rline,rsamp);
  zvmessage(msg,0);

  for (i=0; i<360; i++) {
    lat = sclat/rad;
    lon = (360.-i)/rad;
    zellipse_radius(sclat,sclon,ra,rb,rc,lora,&radius);
    v[0] = radius*cos(lon)*cos(lat);
    v[1] = radius*sin(lon)*cos(lat);
    v[2] = radius*sin(lat);
    status = zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,&rline,&rsamp);
    if (status == 1) {
       sprintf(msg,"lon=%d (line,samp)=(%9.4f,%9.4f)",i,rline,rsamp);
       zvmessage(msg,0);
    }
  }

  gcr = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
  zellipse_radius(sclat,sclon,ra,rb,rc,lora,&radius);
  sprintf(msg,"GCR=%7.1f radius=%7.1f",gcr,radius);
  zvmessage(msg,0);

  sunlat = 0.541/rad;
  sunlon = (360.-171.276)/rad;
  sunrange = 10000000.;

  vsun[0] = sunrange*cos(sunlon)*cos(sunlat);
  vsun[1] = sunrange*sin(sunlon)*cos(sunlat);
  vsun[2] = sunrange*sin(sunlat);
  zazimuth(om,v,vsc,vsun,ra,rb,rc,1,&noraz,&scaz,&sunaz);
  sprintf(msg,"noraz=%8.2f, scaz=%8.2f, sunaz=%8.2f",noraz,scaz,sunaz);
  zvmessage( msg,0);
}
