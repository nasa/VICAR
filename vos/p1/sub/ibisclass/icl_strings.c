/*
 *  ICL keyword search utilities (case-insensitive)
 */

#include <string.h>
#include <ctype.h>


static int strcmp_nocase(str1,str2)
char *str1;
char *str2;
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}


int icl_keymatch(char *keystr, char **keys)
/* keystr: input key string; keys: array of pointers to keys */
{
	int keyno;
	
	if (!keystr) return 0;
	for (keyno=1; *keys && strcmp_nocase(*keys,keystr); keys++)
		keyno++;
	
	if (*keys) return (keyno);
	else return 0;
}

