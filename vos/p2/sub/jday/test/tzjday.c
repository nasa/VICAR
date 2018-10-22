#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzjday) ()
{
  int  month, day, year, out;
  int  icnt, idef;

  zvmessage(" ", " ");
  zvmessage("******  Testing C-Bridge version  ****** ", " ");
  zvparm ("YEAR",  &year,  &icnt, &idef, 0, 0);
  zvparm ("MONTH", &month, &icnt, &idef, 0, 0);
  zvparm ("DAY",   &day,   &icnt, &idef, 0, 0);
  zjday (month, day, year, &out);
  zprnt (4, 1, &out, "Day-of-Year = .");
  zvmessage (" ", " ");

}

