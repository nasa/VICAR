#include "vicmain_c"

main44()
{
   int unit;
   char key[100];
   char task[100];
   int instance;
   int count;
   int status;
   char msg[256];

   zveaction("sa","");

   zvunit(&unit, "inp", 1, 0);
   zvopen(unit, "op", "read", 0);

   zvp("key", key, &count);

   /* Find first */

   status = find_hist_key(unit, key, 0, task, &instance);

   sprintf(msg, "First occurrence: status=%d, task='%s', instance=%d",
		status, task, instance);
   zvmessage(msg, "");

   /* Find last */

   status = find_hist_key(unit, key, 1, task, &instance);

   sprintf(msg, "Last occurrence: status=%d, task='%s', instance=%d",
		status, task, instance);
   zvmessage(msg, "");

   zvclose(unit, 0);

}

