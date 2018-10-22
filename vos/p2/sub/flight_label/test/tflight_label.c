
#include   "vicmain_c"
/*
#include   <stdio.h>
#include   <stdlib.h>
*/
struct  node { char * str;  /* LINK list for output buffer */
               struct node * next;
             };
typedef struct node NODE;
void main44()
{NODE * list;
int   count, unit[10], sta, x, y;
char  inputs[800];
char  outbuf[100];
sta = zvp("INP",inputs, &count);
for( x=0,y=1; x<count; x++,y++ ) /* Open files before calling flight_label*/
 {
   sta = zvunit(&unit[x],"INP", y, 0);
   sta = zvopen(unit[x],"OP","READ","OPEN_ACT","SA", 0);
   list =(NODE*) flight_label(unit[x]);        /* Display flight label  */
   zvmessage("",""); 
   zvmessage("PRINTING FROM OUTPUT BUFFER"," ");
   zvmessage("","");
   if (list == NULL)  zvmessage("Nothing to print.  Buffer is empty.","");
   else  while (list != NULL){
             strcpy (outbuf, list->str);
             zvmessage(outbuf,"");
	     list = list->next;
   }
     
   sta = zvclose(unit[x], 0);
   zvmessage("FINISHED", "");
   zvmessage("", "");
  }
}

