#include <pwd.h>

/*
  char *cuserid(s)
  char *s;

  This routine returns a pointer to the current user's real name, based on
  the real user id not the effective user id.  If a pointer to a buffer is
  passed in, then that buffer should be at least nine characters long.
  Otherwise, if s is a null pointer, then space is allocated for the user
  name in the static area.

*/

char *cuserid(s)
     char *s;
{
   char *rtrn;
   struct passwd *pw;

   pw = getpwuid(getuid());

   if (s == (char *) 0) {
      rtrn = (char *) malloc(strlen(pw->pw_name));
   }
   else {
      rtrn = s;
   }

   strcpy(rtrn, pw->pw_name);

   return(rtrn);
}

#if 0

/****************
  TEST PROGRAM FOR cuserid
 ****************/

main()
{
   char name[80], *name1, name2[80];
   char *cuserid();

   cuserid(name);
   name1 = cuserid(0);
   strcpy(name2, cuserid(0));

   printf("name %s name1 %s name2 %s\n", name, name1, name2);
}

#endif
