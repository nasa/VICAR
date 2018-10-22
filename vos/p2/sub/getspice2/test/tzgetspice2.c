/************************************************************************/
/* Test C-bridge zgetspice2 of FORTRAN subroutine GETSPICE2
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spiceinc.h"

void compare_jbuf (int [], int []);
void printBuffer (void *);

void FTN_NAME(tzgetspice2)(unit,buf,data)
int *unit,data[80];
int buf[200];
{
 int ind,i;
 char project[6];
 char target[13];
 int camera_sn,sclk,scet[6];
 int jbuf[200];

 for (i=0; i<200; i++)
 {
    buf[i] = 0;
    jbuf[i] = 0;
 }

 for (i=0; i<80; i++)
   data[i] = -999;

 ind = zgetspice2(*unit,1,buf);
 if (ind != SUCCESS) {
    zvmessage("***ZGETSPICE2 test failed",0); 
    zabend();
 }
 printBuffer((void *)buf);

 sclk = data[1];
 camera_sn = data[5];

 zgetproj (*unit, project, &camera_sn, &sclk, &ind);
 zgetlabcon (*unit, project, data, &ind);

 for (i=0; i<6; i++) scet[i]=data[7+i];
 strncpy(target,&data[24],12);
 ind = zgetspice3(project,target,camera_sn,sclk,scet,1,jbuf);
 compare_jbuf(buf,jbuf);
 ind = zgetspice4(project,1,data,jbuf);
 compare_jbuf(buf,jbuf);
 return;
}
/************************************************************************
 * Compare buf and jbuf.						*
 ************************************************************************/
void compare_jbuf(buf,jbuf)
int buf[200],jbuf[200];
{
  int i;
  char msg[80];

  for (i=0; i<200; i++) {
      if (buf[i] != jbuf[i]) {
         sprintf(msg," ***Buffers do not match, i=%d",i);
         zvmessage(msg,0);
      }
  }
  return;
}
/*========================================================*
 * void printBuffer(): Print the content of the MIPS	  *
 *			buffer				  *
 *========================================================*/
void printBuffer(buf) 
void *buf;
{
 char str[200];
 char msg[200];
 int i;
 buf_union_typ  cbuf;
 memcpy((char *) &cbuf, (char *) buf,
                sizeof(buf_union_typ));


 sprintf(msg,"SC_ID: %d", cbuf.intbuf[0]);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[1], 4);
 str[4] = '\0';
 sprintf(msg,"Inst : %s", str);
 zvmessage(msg,0);
 sprintf(msg,"measurement time year          : %d", cbuf.intbuf[2]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time day           : %d", cbuf.intbuf[3]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time hour          : %d", cbuf.intbuf[4]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time minute        : %d", cbuf.intbuf[5]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time second        : %d", cbuf.intbuf[6]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time millisec      : %d", cbuf.intbuf[7]);
 zvmessage(msg,0);
 sprintf(msg,"Target body code (target_id)   : %d", cbuf.intbuf[8]);
 zvmessage(msg,0);

 if (cbuf.intbuf[9] == J2000)
    strcpy(str, "J2000");
 else if (cbuf.intbuf[9] == B1950)
    strcpy(str, "B1950");
 sprintf(msg,"Coordinate System              : %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[10], 4);
 str[4] = '\0';
 sprintf(msg,"Source file to search          : %s", str);
 zvmessage(msg,0);

 sprintf(msg,"XYZ of SC relative central body: %g %g %g",
                        cbuf.doublebuf[15],
                        cbuf.doublebuf[16],
                        cbuf.doublebuf[17]);
 zvmessage(msg,0);
 sprintf(msg,"XYZ of picture body relative SC: %g %g %g",
                        cbuf.doublebuf[18],
                        cbuf.doublebuf[19],
                        cbuf.doublebuf[20]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from sun              : %g", cbuf.doublebuf[24]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from central body     : %g", cbuf.doublebuf[25]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from picture body     : %g", cbuf.doublebuf[26]);
 zvmessage(msg,0);

 sprintf(msg,"lat & lon of sun rel pic body : %g %g",
                cbuf.doublebuf[27], cbuf.doublebuf[28]);
 zvmessage(msg,0);
 sprintf(msg,"lat & lon of sc rel pic body  : %g %g",
                cbuf.doublebuf[29], cbuf.doublebuf[30]);
 zvmessage(msg,0);

 zvmessage("C_MATRIX:",0);
 for (i=40; i<49; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"lat & lon of P5 point: %g %g",
        cbuf.doublebuf[76], cbuf.doublebuf[77]);
 zvmessage(msg,0);
 sprintf(msg,"incidence angle at P5 point: %g",
                        cbuf.doublebuf[78]);
 zvmessage(msg,0);
 sprintf(msg,"emission  angle at P5 point: %g",
                        cbuf.doublebuf[79]);
 zvmessage(msg,0);
 sprintf(msg,"phase     angle at P5 point: %g",
                        cbuf.doublebuf[80]);
 zvmessage(msg,0);
 sprintf(msg,"Hor & Vert pix size at P5: %g %g",
        cbuf.doublebuf[81], cbuf.doublebuf[82]);
 zvmessage(msg,0);

 zvmessage("ME_MATRIX:",0);
 for (i=49; i<58; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"SC Range to P5 : %g", cbuf.doublebuf[83]);
 zvmessage(msg,0);
 sprintf(msg,"North Angle    : %g", cbuf.doublebuf[67]);
 zvmessage(msg,0);

 sprintf(msg,"Picture body equat radius, long : %g", cbuf.doublebuf[12]);
 zvmessage(msg,0);
 sprintf(msg,"Picture body equat radius, short: %g", cbuf.doublebuf[13]);
 zvmessage(msg,0);
 sprintf(msg,"Picture body polar radius       : %g", cbuf.doublebuf[14]);
 zvmessage(msg,0);

 zvmessage("OM_MATRIX:",0);
 for (i=58; i<66; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"RS-Vector: %g %g %g", cbuf.doublebuf[21],
                cbuf.doublebuf[22], cbuf.doublebuf[23]);
 zvmessage(msg,0);

 sprintf(msg,"line   sub-s/c-point: %g", cbuf.doublebuf[68]);
 zvmessage(msg,0);
 sprintf(msg,"sample sub-s/c-point: %g", cbuf.doublebuf[69]);
 zvmessage(msg,0);

 memcpy((char *) str, (char *)&cbuf.intbuf[168], 4);
 str[4] = '\0';
 sprintf(msg,"Year: %s", str);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[169], 4);
 str[4] = '\0';
 sprintf(msg,"Month_day: %s", str);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[170], 4);
 str[4] = '\0';
 sprintf(msg,"Hour_min: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[13], 4);
 str[4] = '\0';
 sprintf(msg,"SPK_ref: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[171], 4);
 str[4] = '\0';
 sprintf(msg,"CK_ref: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[172], 4);
 str[4] = '\0';
 sprintf(msg,"Purpose: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[173], 7);
 str[7] = '\0';
 sprintf(msg,"Prog_name: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[175], 4);
 str[4] = '\0';
 sprintf(msg,"Job_req_no: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[176], 6);
 str[6] = '\0';
 sprintf(msg,"Usr_grp_id: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[188], 4);
 str[4] = '\0';
 sprintf(msg,"Institution: %s", str);
 zvmessage(msg,0);
 return;
}
