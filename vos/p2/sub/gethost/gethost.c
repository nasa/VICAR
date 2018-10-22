/******************************************************************************/
/*                                                                            */
/*  FORTRAN:                                                                  */
/*  integer status, gethost                                                   */
/*  char*20 hostname                                                          */
/*  status = gethost(hostname)                                                */
/*                                                                            */
/*  C:                                                                        */
/*  char hostname[21];                                                        */
/*  int  status, length=20, zgethost();                                       */
/*  status = zgethost(hostname, length);                                      */
/*                                                                            */
/*  This function will return the name of the current host in the 'hostname'  */
/*  parameter.  For C calls, you must include the length of the string, for   */
/*  Fortran, this is unnecessary.  The returned name will always be lower-    */
/*  case.  If the function fails for any reason, it will return 0; otherwise, */
/*  it returns 1.  If the function fails, the hostname string will be set to  */
/*  null.  If the string is too small to contain the entire host name, this   */
/*  function will return as many characters as will fit in the string you     */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew                                   */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  05-25-93  81831  PDB - Added LIB_P2SUB to tgethost.imake (no code change).*/
/*  02-22-93   N/A   PDB - Initial release.                                   */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#if VMS_OS
#include syidef
#include ssdef
#endif

#ifndef SUCCESS
#define SUCCESS		1
#define FAILURE		0
#endif

FTN_NAME2(gethost, GETHOST) (char *hostname, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char *c_string;
   int  length, status;

   zsfor2len(length, hostname, &hostname, 1, 1, 1, hostname);
   c_string = (char *) malloc(sizeof(char) * (length+1));
   status = zgethost(c_string, length);
   zsc2for(c_string, length, hostname, &hostname, 1, 1, 1, hostname);

   free(c_string);
   return (status);
}

int zgethost(hostname, namelength)
char *hostname;
int  namelength;
{
   int  status, i;

#if VMS_OS
   char temp[16];
   long retlen;
   struct itmlst {
      short length;
      short code;
      long *bufadr;
      long *retadr;
   } itemlist[] = {15, SYI$_NODENAME, temp, &retlen, 0, 0, 0, 0};

   status = sys$getsyiw(0, 0, 0, itemlist, 0, 0, 0);
   if (status != SS$_NORMAL) {
      hostname[0] = '\0';
      status = FAILURE;
   }
   else {
      status = SUCCESS;
      if (strlen(itemlist[0].bufadr) > namelength) {
         strncpy(hostname, itemlist[0].bufadr, namelength);
         hostname[namelength] = '\0';
      }
      else
         strcpy(hostname, itemlist[0].bufadr);
      for (i = 0; i < strlen(hostname); i++)
         hostname[i] = (char) tolower((char) hostname[i]);
   }
#else
   status = gethostname(hostname, namelength);
   if (status != 0) {
      hostname[0] = '\0';
      status = FAILURE;
   }
   else {
      hostname[namelength] = '\0';
      status = SUCCESS;
      for (i = 0; i < strlen(hostname); i++)
         hostname[i] = (char) tolower((char) hostname[i]);
   }
#endif

   return status;
}
