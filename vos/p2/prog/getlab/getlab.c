#include "vicmain_c"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"
#include "defines.h"            /* for MAX_TASKS, MAX_LABEL_KEY_SIZE */
#include <string.h>
#include <ctype.h>
 
#define SUCCESS     1
#define PARSIZE     1000
#define MAX_OUT     1000
#define KEY_SIZE    MAX_LABEL_KEY_SIZE

void get_parameters (char *, char *, char *, char *, int *, int *);
void upper_case (char *);
void get_keyword_value (char *, char *, char *, char *, int, char *, int);
void write_parblock (char *, char *);
  
void main44(void) {
   char key[KEY_SIZE+1], task[KEY_SIZE+1];
   char location[15], type[15];
   char output[MAX_OUT];
   int instance;
   int element;
 
   zifmessage("getlab version 2018-01-03");

   /* retrive user supplied parameters */ 
   get_parameters(key,location,type,task,&instance,&element);

   /* process user input keyword value */
   get_keyword_value(key,location,type,task,instance,output, element);

   /* write retrived value to the output parameter based on the data type */
   write_parblock(type,output);
 
   return;
}


/* this subroutine will process user inputs, by retriving input values
   and convert string values into upper case characters.  */
void get_parameters (char *key, char *loc, char *type, char *task,
                     int *instance, int *element) {
 
   int status;
   int count;

   zvp("ELEMENT", element, &count);

   /* retrive label item, convert to upper case and store into 'key' */ 
   zvp("LAB_ITEM",key,&count);
   upper_case(key);

   /* retrive item type, conver to upper case and store into 'type' */
   zvp("ITM_TYPE",type,&count);
   upper_case(type);

   /* test for label type input, ans store the keyword into 'loc' */ 
   if (zvptst("SYSTEM")) {
      strcpy(loc,"SYSTEM");
      return;
   }
   else if (zvptst("PROPERTY")) {
      strcpy(loc,"PROPERTY");
      zvp("ITM_TASK",task,&count);
      upper_case(task);
      return;
   }
   else
      strcpy(loc,"HISTORY");

   /* test for which mode of retrival, (i.e. INSTANCE, LATEST, or EARLIEST), 
      and store the value into 'task' */ 
   if (zvptst("INSTANCE")) {
      strcpy(task,"INSTANCE");
      zvp("ITM_INST",instance,&count);
      zvp("ITM_TASK",task,&count);
      upper_case(task);
   }
   else if (zvptst("LATEST")) 
      strcpy(task,"LATEST");
   else
      strcpy(task,"EARLIEST");
 
   return;
}


/* this subroutine convers input string into upper case characters. */ 
void upper_case (char *s) {
   int i;
 
   for (i = 0; i <= strlen(s); i++)
      s[i] = islower(s[i]) ? toupper(s[i]) : s[i];
   return;
}


/* this subroutine process keyword input, by determining which keyword and 
   retrive the proper value from the label */ 
void get_keyword_value (char *key, char *location, char *type, char *task, 
                        int instance, char *out, int element) {

   int   instances[MAX_TASKS], count, j;
   int   unit, length, form;
   int   status;
   char  tasks[MAX_TASKS][KEY_SIZE+1];
 
   zvunit(&unit,"INP",1, NULL);
   zvopen(unit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
 
   if (!strcmp(location,"SYSTEM"))
   /* retrive value from system label */
      status = zlget(unit,"SYSTEM",key,out, "FORMAT",type, "LENGTH",&length,
		     "ELEMENT", element, "NELEMENT", 1, NULL);

   else if (!strcmp(location,"PROPERTY")) {      
   /* retrive value from property label */
      status = zlget(unit, "PROPERTY", key, out, "FORMAT",type,
                     "LENGTH",&length, "PROPERTY",task,
		     "ELEMENT", element, "NELEMENT", 1, NULL);

   }

   else {
      count = MAX_TASKS;
      zlhinfo(unit, (char *)tasks, instances, &count, "ULEN", KEY_SIZE+1, NULL);
      if (!strcmp(task,"LATEST")) {
      /* retrive value from the latest history label */
	 char tmpbuf[MAX_LABEL_VALUE_SIZE + 1];
         for (j = count-1;  j >= 0; j--) {
            status = zlget(unit, "HISTORY", key, tmpbuf, "ERR_ACT","",
                           "HIST",tasks[j], "INSTANCE",instances[j],
                           "FORMAT",type, "LENGTH",&length,
			   "ELEMENT", element, "NELEMENT", 1, "ULEN", MAX_LABEL_VALUE_SIZE, NULL);
            if (status == SUCCESS) break;
            else if (status == CANNOT_FIND_KEY) continue;
            else zvsignal(unit, status, 1);     /* abort */
         }
	 strcpy(out, tmpbuf);
      }
      else if (!strcmp(task,"EARLIEST")) {
      /* retrive value from the earliest history label */
         for (j = 0;  j <= count-1; j++) {
            status = zlget(unit, "HISTORY", key, out, "ERR_ACT","",
                           "HIST",tasks[j], "INSTANCE",instances[j],
                           "FORMAT",type, "LENGTH",&length,
			   "ELEMENT", element, "NELEMENT", 1, NULL);
            if (status == SUCCESS) break;
            else if (status == CANNOT_FIND_KEY) continue;
            else zvsignal(unit, status, 1);     /* abort */
         }
      }
      else    /* INSTANCE mode */ {
      /* retrive value from history label based on the user supplied instance */
         status = zlget(unit, "HISTORY", key, out, "FORMAT",type,
                        "INSTANCE",instance, "LENGTH",&length, "HIST",task,
			"ELEMENT", element, "NELEMENT", 1, NULL);
      }
   }
   if (status != SUCCESS) zvsignal(unit, status, 1);    /* abort */
   if (!strcmp(type,"STRING")) out[length]='\0';
 
   return;
}


/* This subroutine assigns the retrived value to the user supplied output 
   parameter based on the supplied data type. */ 
void write_parblock (char *type, char *output) {

   struct PARBLK par_block;
   static char name[9] = "ITM_NAME";
   double doubleout;
 
   if (!strcmp(type,"REAL"))
      doubleout = *(float *)output;
 
   q_init(&par_block, P_BYTES, P_ABORT);
 
   if (!strcmp(type,"STRING")) q_string(&par_block,name,1,&output,    P_ADD);
   if (!strcmp(type,"INT"))    q_intg  (&par_block,name,1, output,    P_ADD);
   if (!strcmp(type,"REAL"))   q_real  (&par_block,name,1,&doubleout, P_ADD);
 
   zvq_out(&par_block);
 
   return;
}         

