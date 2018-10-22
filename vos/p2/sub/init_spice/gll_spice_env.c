#include "xvmaininc.h"
#include "string.h"
#include "ftnbridge.h"
#include "ms_defines.h"

#define MAX_GLL_STR 132

int zgll_getenv ();

/* FORTRAN bridge for getting GLL-specific SPICE environment variables. */ 
void FTN_NAME2_ (gll_spice_env,GLL_SPICE_ENV) (char *kdb, char *spiceker,
	char *mipsker,
	char *sclk, char *consts, char *bodyids, char *leapsec, int *ind,
	ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char tkdb[MAX_GLL_STR], tspiceker[MAX_GLL_STR], tmipsker[MAX_GLL_STR],
        tsclk[MAX_GLL_STR], tconsts[MAX_GLL_STR], tbodyids[MAX_GLL_STR],
        tleapsec[MAX_GLL_STR];

   *ind = zgll_spice_env (tkdb, tspiceker, tmipsker, tsclk, tconsts, tbodyids, 
                          tleapsec);
   /* failed when *ind < 0 */
   if (*ind >= 0)
   {
      zsc2for (tkdb, 0, kdb, &kdb, 8, 1, 1, ind);
      zsc2for (tspiceker, 0, spiceker, &kdb, 8, 2, 2, ind);
      zsc2for (tmipsker, 0, mipsker, &kdb, 8, 3, 3, ind);
      zsc2for (tsclk, 0, sclk, &kdb, 8, 4, 4, ind);
      zsc2for (tconsts, 0, consts, &kdb, 8, 5, 5, ind);
      zsc2for (tbodyids, 0, bodyids, &kdb, 8, 6, 6, ind);
      zsc2for (tleapsec, 0, leapsec, &kdb, 8, 7, 7, ind);
   }
} 


/* subroutine to retrieve GLL-spceific SPICE environment variables. */
int zgll_spice_env (kdb, spiceker, mipsker, sclk, consts, bodyids,
                    leapsec)
   char *kdb;
   char *spiceker;
   char *mipsker;
   char *sclk;
   char *consts;
   char *bodyids;
   char *leapsec;
{
   msEnvStruct env;   /* structure to hold returned environment values */
   int len, ind;

   /* invoke MSPICE subroutine to return environment variables. */
   ind = mslcl_getgllenv (&env);

   if (ind >= 0)
   {
      strcpy (kdb, env.kdb);
      strcpy (spiceker, env.spiceker);
      len = (int) strlen (spiceker);

      /* remove the '/' from the SPICEKER environment to be consistent with 
         old SPICE environment. */
      if (spiceker[len-1] == '/') spiceker[len-1] = spiceker[len];

      strcpy (mipsker, env.mipsker);
      len = (int) strlen (mipsker);
 
      /* remove the '/' from the MIPSKER environment to be consistent with
         old SPICE environment. */ 
      if (mipsker[len-1] == '/') mipsker[len-1] = mipsker[len];
      strcpy (sclk, env.sclk);
      strcpy (consts, env.consts);
      strcpy (bodyids, env.bodyids);
      strcpy (leapsec, env.leapsec);
   }

   return ind;
}



