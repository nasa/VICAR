#include <stdlib.h>
#include <string.h>

/*
  get space for something and zero it out
*/

void *get_space (size)
  int size;
{
   void *pointer;

   pointer = (void *)malloc(size);
   memset (pointer, 0, size);

   return pointer;
}

