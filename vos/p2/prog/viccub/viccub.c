#include "vicmain_c"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void my_abort(char abort_message[80]);

void main44()
{
int lauf, lauf_nl;
int status;
int inunit, outunit;
int nl,ns, ni, nb, insampl, inlines;
char    format[10];
char    informat[10];
char    *buf;
char    outstring[80];

status=zvpcnt("INP", &ni);

status=zvunit(&inunit, "INP", 1, NULL);
status=zvopen(inunit, NULL);
if (status != 1) my_abort("can not open first input image");

status=zvget(inunit,"nl",&nl,"ns",&ns, "FORMAT", format, NULL);
status = zvclose(inunit,NULL);

if (!strcmp(format,"COMP")) 
   my_abort
      ("first input image has complex format, not supported");


status=zvunit(&outunit, "out", 1, NULL);
status=zvopen(outunit, 
       "OP", "WRITE",
       "O_FORMAT", format,
       "U_NS", ns, "U_NL", nl, "U_NB", ni,
       NULL);
if (status != 1) my_abort("can not open output image");

/* the largest image format is double */
buf=(char *)malloc(ns*sizeof(double));   
if(buf==NULL)
   my_abort("input images to large, memory problems !!");


lauf=0;

for (lauf=1; lauf <= ni; lauf++)
   {
   status=zvunit(&inunit, "INP", lauf, NULL);
   status=zvopen(inunit, NULL);
if (status != 1) 
   {
   sprintf(outstring,"can not open input image # %5d",lauf);
   my_abort(outstring);
   }

   status=zvget(inunit,"nl",&inlines,"ns",&insampl, "nb", &nb,
                "format", informat, NULL);

   if (nb != 1) 
      {
      sprintf(outstring,"input image # %5d has more than 1 band",lauf);
      my_abort(outstring);
      }
   if (insampl != ns)
      {
      sprintf(outstring,
         "input image # %5d has different number of samples",lauf);
      my_abort(outstring);
      }
   if (inlines != nl)
      {
      sprintf(outstring,
         "input image # %5d has different number of lines",lauf);
      my_abort(outstring);
      }
   if (strcmp(format,informat)) 
      {
      sprintf(outstring,
         "input image # %5d has different image format",lauf);
      my_abort(outstring);
      }

   for (lauf_nl=1; lauf_nl <= nl; lauf_nl++)         
      {
      status=zvread(inunit, buf,  NULL);
      status=zvwrit(outunit, buf, NULL);
      }
   zvclose(inunit, NULL);
   }

status=zvclose(outunit,NULL);
}

void my_abort(abort_message)

char abort_message[80];
{
   zvmessage("","");
   zvmessage("     ******* VICCUB error *******","");
   zvmessage(abort_message,"");
   zvmessage("","");
   zabend();
}

