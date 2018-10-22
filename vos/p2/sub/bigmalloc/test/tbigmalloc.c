#include "vicmain_c"

unsigned char *BigMalloc(), *BigMallocForce();
void BigFree();

main44()
{
   unsigned char *ptr;
   int i;
   void try();

   try(1024, 0);

   try(1024 * 1024, 0);

   try(1024 * 4096, 0);

   try(1024, 1);
}

void try(size, flag)
int size, flag;
{
   unsigned char *ptr;
   unsigned int i;
   unsigned char tmp;
   char msg[256];

   if (flag)
      ptr = BigMallocForce(size);
   else
      ptr = BigMalloc(size);

   if (ptr == NULL) {
      sprintf(msg,
	   "BigMalloc failed for size %d.  This may or may not indicate test",
	   size);
      zvmessage(msg, "");
      zvmessage(
	   "failure.  If on VMS, check for space available on v2$scratch and try again","");
      return;
   }


   for (i=0; i<size; i++)
      *(ptr+i) = i % 256;
   for (i=0; i<size; i++) {
      tmp = i%256;
      if (*(ptr+i) != tmp) {
         sprintf(msg, "Compare error in size %d, location %d, value=%d, should be %d",
		size, i, *(ptr+i), tmp);
         zvmessage(msg, "");
         zvmessage("Test failed", "");
         return;
      }
   }

   for (i=0; i<size; i++)
      *(ptr+i) = ~ (i % 256);
   for (i=0; i<size; i++) {
      tmp = ~ (i % 256);
      if (*(ptr+i) != tmp) {
         sprintf(msg, "Compare inv error in size %d, location %d, value=%d, should be %d",
		size, i, *(ptr+i), tmp);
         zvmessage(msg, "");
         zvmessage("Test failed", "");
         return;
      }
   }

   BigFree(ptr);

   sprintf(msg, "Test of size %d succeeded", size);
   zvmessage(msg, "");

}

