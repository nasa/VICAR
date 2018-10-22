/*  tzhexcon is the program that test the C-Callable hexcon subroutine */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
	unsigned char ibuf[256];
	int i,j,n;
        char obuf[512], ms1[256];

        sprintf(ms1,"Test the C interface");
        zvmessage(ms1," ");
        zvmessage(" "," ");

	for (i=0; i<256; i++) ibuf[i]=i;
        n = 256;
	zhexcon(ibuf,obuf,&n);
	for (j=0; j<512; j+=32)
		{
		for (i=0; i<32; i++) ibuf[i]=obuf[i+j];
		ibuf[32] = 0;
		sprintf(ms1,"%s",ibuf);
                zvmessage(ms1," ");
		}

        zvmessage(" "," ");
        sprintf(ms1,"Test the FORTRAN interface");
        zvmessage(ms1," ");
        zvmessage(" "," ");

        FTN_NAME(thexcon)();
}
