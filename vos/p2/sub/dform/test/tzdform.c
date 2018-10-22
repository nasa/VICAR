/*

	Test subroutine of DFORM from C programming language

	Author:	Justin McNeill
	Date:	June 1993

	Revisions:

	December 13, 1993 	Byte-to-byte data transfer test added.
				(FR ) (JFM059)
*/

#include "xvmaininc.h"
#include "ftnbridge.h"

FTN_NAME(tzdform)()
{
       	int hisbuf[256],coll,isw;
	int h,i,j,k;
       	char substring[10],string[200];
       	char try[40],try1[20];
	short int try2[10];
	int try3[10];
	float try4[10];
	float r1, r2;

	/* initialize values of 8-bit array TRY */
	for(i=25,j=0;i<35;i++,j++)
		try[j] = i;

	/*
	test 0 checks the collect flag - byte-to-byte data transfer	
	*/
        isw  = 1;			/* initialize and do not acquire */
	coll = 0;			/* histogram 			 */
     	zvmessage("\n Byte data input."," ");
	
	sprintf(string,"  %d",try[0]);
	for(k=1;k<j;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");
 
	zdform( try, try1, j, hisbuf, isw, 0, 2, 0, 7, 0., 100., coll );

	isw  = 0;
	coll = 0;
	zdform( try, try1, j, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );

	zvmessage(" "," ");
	zvmessage(" Entries scaled by a factor of 2.55"," ");
	zvmessage(" "," ");
	
	sprintf(string,"  %d",try1[0]);
	for(k=1;k<j;k++)
		{
		sprintf(substring,"  %d",try1[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/* initialize values of array TRY1 	*/
	for(i=1,j=0;i<11;i++,j++)
		try1[j] = i;
	for(i=(-127);i<-122;i++,j++)
		try1[j] = i;
	for(i=(-5);i<0;i++,j++)
		try1[j] = i;

	/* initialize values of array TRY2 	*/
	for(i=125,j=0;i<135;i++,j++)
		try2[j] = i;

	/* initialize values of array TRY3 	*/
	for(i=1001,j=0;i<1011;i++,j++)
		try3[j] = i;

	/* initialize values of array TRY4 	*/
	for(i=10001,j=0;i<10011;i++,j++)
		try4[j] = (float)i;

	
	/*
	test 1 checks the collect flag - use byte data for simplicity
	*/
        isw  = 1;			/* initialize and do not acquire */
	coll = 0;			/* histogram 			 */
     	zvmessage("\n Byte data input."," ");
	
	sprintf(string,"  %d",try1[0]);
	for(k=1;k<20;k++)
		{
		sprintf(substring,"  %d",try1[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");
 
	zdform( try1, try, 20, hisbuf, isw, 0, 0, 0, 7, 0., 255., coll );
 	zvmessage("\n No histogram produced."," ");

	for(h=0,k=0;k<32;k++)		/* print histogram		*/
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 0;			/* acquire histogram		*/
	coll = 1;
	zdform( try1, try, 20, hisbuf, isw, 0, 0, 0, 0,	0., 0., coll );
	zvmessage("\n Histogram produced."," ");

	for(h=0,k=0;k<32;k++)		/* print histogram		*/
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	/*
	test 2 - work with integer*2 data 
	first, extract the first 3 bits then collect histogram and
	finally scale.
	*/

	isw  = 1;
	zvmessage("\n Half word data input."," ");

	sprintf(string,"  %d",try2[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try2[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zdform ( try2, try, 10, hisbuf, isw, 1, 1, 0, 2, 0., 255., coll );
	
	isw = 0;			/* acquire histogram		*/
	coll = 1;	
	zdform ( try2, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );
	zvmessage("\n Bits 0 thru 2 extracted "," ");

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 1;			/* initialize with scale values */
	r1   = 120.0;
	r2   = 150.0;
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
 	zvmessage( string," " );
	zdform ( try2, try, 10, hisbuf, isw, 1, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* scale values			*/
	coll = 0;	
	zdform( try2, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll);
	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/*
	test 3 - integer data 
	*/

	isw  = 1;
	zvmessage("\n Integer data input."," ");
	
	sprintf(string,"  %d",try3[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try3[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zdform ( try3, try, 10, hisbuf, isw, 2, 1, 0, 7, 0., 255., coll );

	isw = 0;			/* acquire histogram and do	*/
	coll = 1;			/* bit extraction.		*/
	zdform( try3, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll);
	zvmessage("\n Bits 0 thru 7 extracted "," ");

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}

	isw  = 1;
	coll = 0;
      	r1   = 1000.0;
	r2   = 1010.0;
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
       	zvmessage( string," " );
       	zdform ( try3, try, 10, hisbuf, isw, 2, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* perform bit extraction	*/

       	zdform ( try3, try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	/*
	test 4 - real data 
	*/

 	zvmessage("\n Real data input."," ");

	sprintf(string,"  %7.2f",try4[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %7.2f",try4[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

	isw  = 1;
	r1   = 10000.0;
	r2   = 10010.0;

	zdform ( try4,try, 10, hisbuf, isw, 3, 2, 0, 7, r1, r2, coll );

	isw = 0;			/* Acquire histogram and perform */
	coll = 1;			/* scaling of data.		 */

	zdform ( try4,try, 10, hisbuf, isw, 0, 0, 0, 0, 0., 0., coll );
	sprintf( string,"\n Data scaled r1 = %f  r2 = %f", r1, r2 );
	zvmessage( string, " " );

	sprintf(string,"  %d",try[0]);
	for(k=1;k<10;k++)
		{
		sprintf(substring,"  %d",try[k]);
		strcat(string,substring);
		}
	zvmessage(string," ");

 	zvmessage("\n Histogram produced."," ");
	for(h=0,k=0;k<32;k++)
		{
		sprintf(string,"      %d",hisbuf[h++]);
		for(j=1;j<8;j++)
			{
			sprintf(substring,"      %d",hisbuf[h++]);
			strcat(string,substring);
			}
		zvmessage(string," ");
		}
	zvmessage("\n\n TEST PROGRAM COMPLETED"," ");
}
