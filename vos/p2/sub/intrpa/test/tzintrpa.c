#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* 	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void FTN_NAME(tzintrpa)() 
{
	unsigned char  	b[10];		/* Byte variables	*/
	short int	h[10];		/* Halfword variable	*/
	int		f[10];		/* Fullword variable	*/
	char		card[100];	/* Print buffer		*/
	char		string[30];	/* String for concaten.	*/

	zvmessage(" "," ");
	zvmessage("**BEGIN TEST OF MODULE INTRPA**"," ");
	zvmessage(" "," ");

 	zvmessage("**BYTE DATA**"," ");
	zintrpa(1, 10, b, 1, 10);
	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		b[0],b[1],b[2],b[3],b[4],b[5],b[6]);
	sprintf(string,"   %02X   %02X   %02X",b[7],b[8],b[9]);
	strcat(card,string);
	zvmessage(card," ");
	zvmessage(" "," ");

 	zvmessage("**HALFWORD DATA**"," ");
 	zintrpa(2, 10, h, 1, 10);
      	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		h[0],h[1],h[2],h[3],h[4],h[5],h[6]);
      	sprintf(string,"   %02X   %02X   %02X",h[7],h[8],h[9]);
	strcat(card,string);
	zvmessage(card," ");
	zvmessage(" "," ");

 	zvmessage("**FULLWORD DATA**"," ");
 	zintrpa(4, 10, f, 1, 10);
      	sprintf(card,"    %02X   %02X   %02X   %02X   %02X   %02X   %02X",
		f[0],f[1],f[2],f[3],f[4],f[5],f[6]);
      	sprintf(string,"   %02X   %02X   %02X",f[7],f[8],f[9]);
	strcat(card,string);
	zvmessage(card," ");

	zvmessage("**END TEST OF MODULE INTRPA**"," ");
      	zvmessage(" "," ");

}
