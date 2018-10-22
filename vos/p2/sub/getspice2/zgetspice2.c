/* C-bridge for FORTRAN routines GETSPICE2, GETSPICE3, GETSPICE4 */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spice89.h"
#include <string.h>


int zgetspice2(unit,provenance,buf)
int unit,provenance;
buf_union_typ *buf;
{
  int ind;

  FTN_NAME2(getspice2, GETSPICE2) (&unit,&provenance,buf->intbuf,&ind);
  return(ind);
}
int zgetspice3(project,target,camera_sn,sclk,scet,provenance,buf)
char project[6];	/* GLL, VGR-1, VGR-2 */
char target[13];	/* Target/planet for GLL/VGR respectively */
int camera_sn;		/* Camera serial number */
int sclk;		/* Frame number (FDS for VGR) */
int scet[6];		/* SCET (year,day,hour,minute,sec,msec) */
int provenance;
buf_union_typ *buf;	/* output SPICE/SEDR buffer */
{
  int ind,i,n;
  int data[80];	/* input label buffer (from GETLABCON) */
  char ztarget[13];

  for (i=0; i<80; i++)
     data[i] = -999;

  data[1] = sclk;
  data[5] = camera_sn;
  for (i=0; i<6; i++) data[7+i]=scet[i];
  strcpy(ztarget,target);
  for (i=strlen(target); i<12; i++) ztarget[i]=' ';
  strncpy((char *)&data[24],ztarget,12);
  n = strlen(project);  
  FTN_NAME2(xgetspice4,XGETSPICE4)(project,&n,&provenance,data,
						buf->intbuf,&ind);
  return(ind);
}
int zgetspice4(project,provenance,data,buf)
char *project;
int provenance;
int data[80];		/* input label buffer (from GETLABCON) */
buf_union_typ *buf;
{
  int ind,n;

  n = strlen(project);  
  FTN_NAME2(xgetspice4, XGETSPICE4) (project,&n,&provenance,data,
							buf->intbuf,&ind);
  return(ind);
}
