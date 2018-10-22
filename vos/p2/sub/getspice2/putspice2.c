#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"
#include <string.h>

/*========================================================*
 * PUTSPICE2(): FORTRAN to C bridge routine
 *========================================================*/
void FTN_NAME2(putspice2, PUTSPICE2) (char *source, char *program, int buf[200],
			int *ind, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 int maxlen;
 char csource[15];
 char cprogram[7];

 maxlen = 5;
 zsfor2c(csource,maxlen,source,&source,5,2,1, ind);
 maxlen = 7;
 zsfor2c(cprogram,maxlen,program,&source,5,2,1, ind);

 *ind = zputspice2(csource,cprogram,buf);
}
/**********************************************************************
 * zputspice2: routine to copy provenance parameters and  PUTSPICE95
 **********************************************************************/

int zputspice2(source,program,buf)
char source[5];		/* FARE, NAV, NAV2, AMOS, DAVI, NEAR, NAIF, SEDR */
char program[20];	/* Program name */
int buf[200];
{
  int sc_id;		/* Spacecraft ID: GLL=-77 */
  int mode,count,def,ind,len,i;
  char institute[5],purpose[5],reqnum[5];
  char userid[7];
  char groupid[4];
  char defaultspice[12];
  char *getenv_vic(char *);
  char *cuserid_p2();
  char *ptr;

  sc_id = buf[0];
  memset(institute,'\0',5);
  memset(purpose,'\0',5);
  memset(reqnum,'\0',5);
  memset(userid,'\0',7);
  memset(groupid,'\0',4);

  zvparm("INSTITUTE",institute,&count,&def,0,0);
  if (count == 0) strcpy (institute, "NONE");

  spaceFilledStr(institute,4);
  zvparm("PURPOSE",purpose,&count,&def,0,0);
  zvparm("REQNUM",reqnum,&count,&def,0,0);
  zvparm("CDATE",&buf[168],&count,&def,0,0);

  strncpy(userid,cuserid_p2(),6);
  len = strlen(userid);
  if (len < 4) {		/* Unix userid is only 3 chars */
     zvparm("GROUPID",groupid,&count,&def,0,0);
     if (count==0) {
        ptr = getenv_vic("GROUPID");
        if (ptr != 0) strncpy(groupid,getenv_vic("GROUPID"),3);
     }
     strncpy(&userid[3],groupid,3); 
  }
  zccase(userid,1,6);
  strncpy((char *)&buf[10],source,4);
  strncpy((char *)&buf[171],"NONE",4);
  strcpy((char *)&buf[172],purpose);
  strncpy((char *)&buf[173],program,6);
  strcpy((char *)&buf[175],reqnum);
  strncpy((char *)&buf[176],userid,6);
  strncpy((char *)&buf[188],institute,4);

  if (zvptst("remote")) mode=1;
  else if (zvptst("local")) mode=0;
  else {
     strcpy(defaultspice,getenv_vic("DEFAULTSPICE"));
     mode = 0;
     if (!strncmp(defaultspice,"REMOTESPICE",11)) mode=1;
  }

  ind = zputspice95(sc_id,buf,mode);
  return(ind);
}
