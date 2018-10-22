#include "xvmaininc.h"
#include "ftnbridge.h"

#define TINIT_STR_LEN 132

void FTN_NAME (tzinit_spice) ()
{
   char kdb[TINIT_STR_LEN];
   char spiceker[TINIT_STR_LEN];
   char mipsker[TINIT_STR_LEN];
   char sclk[TINIT_STR_LEN];
   char consts[TINIT_STR_LEN];
   char bodyids[TINIT_STR_LEN];
   char leapsec[TINIT_STR_LEN];

   char msg[256];

   int ind;

   zvmessage ("**** Testing C Interface ****", "");

   ind = zgll_spice_env(kdb, spiceker, mipsker, sclk, consts, bodyids, leapsec);

   if (ind < 0)
      zmabend ("Undefined GLL SPICE environment variables.");

   sprintf (msg, "GLL_KDB ---------->%s", kdb);
   zvmessage (msg, "");

   sprintf (msg, "GLL_SPICEKER ----->%s", spiceker);
   zvmessage (msg, "");

   sprintf (msg, "GLL_MIPSKER ------>%s", mipsker);
   zvmessage (msg, "");

   sprintf (msg, "GLL_SCLK --------->%s", sclk);
   zvmessage (msg, "");

   sprintf (msg, "GLL_CONSTANTS ---->%s", consts);
   zvmessage (msg, "");

   sprintf (msg, "GLL_BODY_IDS ----->%s", bodyids);
   zvmessage (msg, "");

   sprintf (msg, "GLL_LEAPSECONDS -->%s", leapsec);
   zvmessage (msg, "");
   zvmessage ("", "");
   zvmessage ("Calling INITSPICE", "");
   initspice();
   zvmessage ("Return from INITSPICE", "");

   zvmessage ("", "");

   return;
}





