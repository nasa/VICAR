/*===========================================================================*
 *	tzvgrimfmt.c
 *
 *  Routine to test zvgrimfmt in C
 *============================================================================
 */
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzvgrimfmt) ()
{
	int	imc,imcode,count,def;
	int	scan_rate;
	char	format[6];
	int	i,status;
	short	ss[10],ns[10];
	char	message[128];

	imcode=31;
	for ( imc=0; imc<=imcode; imc++ ) {
		for (i=0; i<10; i++)
			ss[i]=ns[i]=0;

		status = zvgrimfmt( imc,format,&scan_rate,ss,ns);
		if (status == 1) {
			sprintf(message,"IMCODE=%d, FORMAT=%s, SCAN_RATE=%d",imc,format,scan_rate);
			zvmessage(message,"");
			sprintf(message,"	SS	NS");
			zvmessage(message,"");
			for (i=0; i<scan_rate; i++){ 
			   sprintf(message,"	%d	%d",ss[i],ns[i]);
			   zvmessage(message,"");
			}
		}
		zvmessage(" ","");
	}
}
