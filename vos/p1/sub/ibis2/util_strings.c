#include "ibis.h"
#include <string.h>
#include "ibisdeclares.h"
#include <ctype.h>

int _i_strcmp_nocase(char* str1,char* str2)
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}

void _i_make_uppercase(char* str)
{
	if (!str) return;	
	for ( ; *str; str++) *str=toupper(*str);
}

int _i_strncmp_nocase(char* str1,char* str2, int n)
{
	char c1,c2;
	
	if (n<1) return 1;
	for((c1= *str1,c2= *str2); c1&&c2&&n; (c1= *++str1,c2= *++str2,n--) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return 0; /* they don't have to end at same time */
}

int _i_keymatch(char* keystr, char **keys)
{
	int keyno;
	
	if (!keystr) return 0;
	for (keyno=1; *keys && _i_strcmp_nocase(*keys,keystr); keys++)
		keyno++;
	
	if (*keys) return (keyno);
	else return 0;
}

void *_i_mem_dup(char* data, int size )
{
	char *datacopy=(char *)0;
	
	if (!data || !size ) return data;
	datacopy = (char *)calloc(1L, size);
	if (!datacopy) return datacopy;

	memcpy(datacopy, data, size);
	return datacopy;
}



