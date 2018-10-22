#include "vicmain_c"
#include <string.h>

void main44() {
   int i=1,j,k,l;
   char s[20];
   strcpy(s,"ERR-CODE-F");
   zvmessage("The next line will be blank.",0);
   null_message();
   message("The above line is blank. This is a test of message.");
   errmess("This is a test of errmess.",s);  

   message1("This is a test of message1. %d.",i);
   errmess1("This is a test of errmess1. %d.",i,s);

   j=i;i++;
   message2("This is a test of message2. %d, %d.",i,j);
   errmess2("This is a test of errmess2. %d, %d.",i,j,s);

   k=j;j++;i++;
   message3("This is a test of message3. %d, %d, %d.",i,j,k);
   errmess3("This is a test of errmess3. %d, %d, %d.",i,j,k,s);

   l=k;k++;j++;i++;
   message4("This is a test of message4. %d, %d, %d, %d.",i,j,k,l);  
   errmess4("This is a test of errmess4. %d, %d, %d, %d.",i,j,k,l,s);

   zvmessage("Now test the error handling....try to print a string that is too long.",0);
   message("A very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              very very very very very very very very very very very very \
              long string!");
}
/* end module tusr_message.c */
