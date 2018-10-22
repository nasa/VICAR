/*
 *	ttimeout.c
 *
*/
#include "vicmain_c"

void main44()
{
	int count, def;
	int date[6];
	char string[25];


	zvparm("DATE", date, &count, &def, 0, 0);

	timeout(date,string);
	zvmessage(string,0);
}
