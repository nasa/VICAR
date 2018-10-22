/* C-bridge for FORTRAN routines PBNAME, PBID */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spice89.h"
#include <string.h>

int zpbname(id, par)
int id;
char par[13];
{
  int ind;
  FTN_NAME2(xpbname,XPBNAME)(&id,par,&ind);
  return(ind);
}

int zpbid(par, id)
char par[13];
int *id;
{
  int ind;
  FTN_NAME2(xpbid,XPBID)(par,id,&ind);
  return(ind);
}
