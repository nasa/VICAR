#include "parm.h"
#include "const.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void l_message();

int l_get_value (par)
   struct parm *par;
{
   char msg[80];
   int status, i, defaulted, length, count;

   /*---- see how many of the parameter have been specified -----------------*/
   status = zvpcnt (par->name, &par->count);

   /*---- maxcnt == 0 means there is no limit on count ----------------------*/
   if (par->maxcnt && par->count > par->maxcnt) {
      sprintf (msg, " too many of par %s  specified", par->name);
      l_message (msg);
      exit(0);
   }

   /*---- length tells zvparm how long the strings being passed in are ------*/
   if (EQUAL(par->type,"STRING"))
      length = MAX_PAR_STRING;
   else
      length = 0;

   /*---- grab space for parameter values depending on type and count -------*/
   if (EQUAL(par->type,"STRING")) {
      par->val.c = (char *)get_space(par->count*length*sizeof(char));
      status = zvparm (par->name,par->val.c,&par->count,
                                         &defaulted,par->maxcnt,length);
      count = par->count;
      for (i=0; i<count; i++) {
         if (strlen(par->val.c + i*MAX_PAR_STRING) == 0)
             par->count -= 1;
      }
   }
   else if (EQUAL(par->type,"INT")) {
      par->val.i = (int *)get_space(par->count * sizeof(int));
      status = zvparm (par->name,par->val.i,&par->count,
                               &defaulted,par->maxcnt,length);
   }
   else if (EQUAL(par->type,"REAL")) {
      par->val.f = (float *)get_space(par->count * sizeof(float));
      status = zvparm (par->name,par->val.f,&par->count,
                                   &defaulted,par->maxcnt,length);
   }
   else if (EQUAL(par->type,"DOUB")) {
      par->val.d = (double *)get_space(par->count * sizeof(double));
      status = zvparmd (par->name,par->val.d,&par->count,
                                     &defaulted,par->maxcnt,length);
   }
}

int l_list_value (par)
   struct parm *par;
{
   char msg[256], value[200];
   int i;

   memset (msg,'\0',sizeof(msg));
   sprintf (msg," %10s = ",par->name);
   for (i=0; i<par->count; i++) {
      memset (value,'\0',sizeof(value));

      if (EQUAL(par->type,"STRING"))
         sprintf (value, "%s", (par->val.c + i*MAX_PAR_STRING));
      else if (EQUAL(par->type,"INT"))
         sprintf (value, "%d", par->val.i[i]);
      else if (EQUAL(par->type,"REAL"))
         sprintf (value, "%g", par->val.f[i]);
      else if (EQUAL(par->type,"DOUB"))
         sprintf (value, "%g", par->val.d[i]);

      sprintf (msg, "%s%s", msg, value);
      l_message (msg);

      memset (msg,'\0',sizeof(msg));
      sprintf (msg,"              ");
   }

}


/*
 * use vicar call to open image files
 */
void l_open_file (fil, action)
 struct gfile *fil;
 char *action;
{
  int stat;

  if (EQUAL(fil->pdf_name,"INP") || EQUAL(fil->pdf_name,"OUT"))
     stat = zvunit (&fil->unit,fil->pdf_name,fil->instance,"");
  else
     stat = zvunit (&fil->unit,fil->pdf_name,fil->instance,
                                        "U_NAME",fil->name,"");

  if (EQUAL(action,"READ")) {
     stat = zvopen(fil->unit, "IO_ACT","SA", "OPEN_ACT","SA", "");
     stat = zvget (fil->unit, "NL",&fil->nl, "NS",&fil->ns, "NB",&fil->nb,
                                             "FORMAT",fil->format, "");
  }
  else if (EQUAL(action,"WRITE")) {
     stat = zvopen (fil->unit, "U_NL",fil->nl, "U_NS",fil->ns,
               "U_NB",fil->nb, "IO_ACT","SA", "OPEN_ACT","SA", "OP","WRITE",
               "O_FORMAT",fil->format, "");
  }

  /*
    don't want to have to play around with bytes or halves in most programs
  */
  if (EQUAL(fil->format,"BYTE") || EQUAL(fil->format,"HALF")) {
     stat = zvclose (fil->unit, "");
     if (EQUAL(action,"READ")) {
        stat = zvopen(fil->unit, "IO_ACT","SA", "OPEN_ACT","SA",
                      "U_FORMAT","FULL","");
        stat = zvget (fil->unit, "NL",&fil->nl, "NS",&fil->ns, "NB",&fil->nb,
                      "FORMAT",fil->format, "");
     }
     else if (EQUAL(action,"WRITE")) {
        stat = zvopen (fil->unit, "U_NL",fil->nl, "U_NS",fil->ns,
               "U_NB",fil->nb, "IO_ACT","SA", "OPEN_ACT","SA", "OP","WRITE",
               "U_FORMAT","FULL","O_FORMAT",fil->format, "");
     }
  }

}


/*
 * use zvclose to close a file
 */
void l_close_file (unit)
 int unit;
{
   int stat;
   stat = zvclose (unit, "CLOS_ACT","FREE", "");
}

/*
 * use zvmessage to send a message to the user
 */
void l_message (msg)
 char *msg;
{
   zvmessage(msg, "");
}

/*
 * add a label item to a vicar file
 */
void l_add_label (unit, field, value)
 int unit;
 char *field;
 char *value;
{
   char msg[80];
   int s;

   s = zladd (unit,"HISTORY",field,value,"FORMAT","STRING","");
   if (s != 1){
      sprintf (msg, " Error adding %s label to unit %d", field, unit);
      l_message (msg, "");
   }
}


/*
 * delete a vicar label item
 */

void l_delete_label (unit, field, task)
 int unit;
 char *field;
 char *task;
{
   char msg[80];
   int s;

   s = zldel (unit,"HISTORY",field,"HIST",task,"");
   if (s != 1){
      sprintf (msg, " Error deleting %s label from unit %d", field, unit);
      l_message (msg, "");
   }
}



void l_read_line (unit, buffer, line, band)
   int unit;
   int line;
   int band;
   void  *buffer;
{
   int status;

   status = zvread (unit, buffer, "LINE",line,"BAND",band, "");
}

void l_write_line (unit, buffer, line, band)
   int unit;
   int line;
   int band;
   void *buffer;
{
   int status;

   status = zvwrit (unit, buffer, "LINE",line,"BAND",band, "");
}

