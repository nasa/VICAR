/* Imitate the VMS and Sun C string functions that the Alliant doesn't have! */

#define FALSE 0
#define TRUE 1

strspn(str, charset)
char *str;
char *charset;
{
   int i, j, found;

   if (str == NULL)
      return 0;

   for (i=0; i<strlen(str); i++) {
      found = FALSE;
      for (j=0; j<strlen(charset); j++) {
         if (str[i] == charset[j]) {
            found = TRUE;
            break;
         }
      }
      if (!found)
         return i;
   }
   return strlen(str);
}

char *strchr(str, character)
char *str;
char character;
{
   int i;

   for (i=0; i<strlen(str); i++) {
      if (*(str+i) == character)
         return str+i;
   }
   return 0;
}

