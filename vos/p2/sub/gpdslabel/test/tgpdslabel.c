#include <stdio.h>
#include "vicmain_c"
#include <stdlib.h>

main44()
{
int 	count, unit, stat, x, y;
char 	output[200];
char	expanded_output[200];
char 	string[500];
int	flags;   
int	status;
char	msg[200];

FILE	*ptr;

int i;

zvp("OUT",output,&count, 0);	/* Get file name of output		  */
zvselpi(1);
status = zvfilename(output, expanded_output, 199 );

ptr = fopen(expanded_output, "w+" );

/* TEST OF STRINGS OVER 80 CHARACTERS */
strcpy(string,"CCSD3ZF000100000001NJPL3IF0PDS200SDFKLJERJTSDJFKJKLJ");
strcat(string,"CCSD3ZF000100000001NJPL3IF0PDS200000001 = SFDU_LABEL");

zapdsline(ptr,'c',string,&x,1);

zapdsline(ptr,'c',"/* File format and length */",&x,1);

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",0);	/* WARNING Expected */

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",1);

zapdsline(ptr,'w',"RECORD_TYPE","FIXED_LENGTH",5);
i = 100;
zapdsline(ptr,'i',"RECORD_BYTES",&i,18);
i=1000;
zapdsline(ptr,'i',"RECORD_BYTES",&i,1);

zapdsline(ptr,'l',"DATA_SET_ID","GO-V/E-SSI-EDR-E1-V1.0",1);

zapdsline(ptr,'l',"DATA_SET_ID","GO-V/E-SSI-EDR-E1-V1.0",12);

zapdsline(ptr,'s',"CURRENT_TIME","91.203 09:54:02.201",1);

/* TEST OF STRING ARGUMENTS OVER RECORD BOUNDARY */
strcpy(string,"Nineteen hundred ninty-one, day two hundred three,");
strcat(string,"Time is nine fifty-four and two thousand two hundred");
strcat(string," and one milliseconds.");
zapdsline(ptr,'s',"CURRENT_TIME_IN_ENGLISH",string,1);

/* close(ptr);	*/			/* Close output file */
}
