/*
 * Test program for ISTAT library
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"

void FTN_NAME(c_test)()
{
	int unit, status;
	
	zvmessage("*** C - TEST ***", " ");
	status = zvunit(&unit,"OUT",1,0);
	if (status < 0) zvsignal(unit, status, 1);
	
	CreateStatsFile(unit);
	ReadStatsFile(unit);
}

CreateStatsFile(unit)
int unit;
{
	int status,def;
	int nclasses;
	int nbands;
	char *inst=(char *)0;
	
	zvp("NCLASS",&nclasses,&def);
	zvp("NBAND",&nbands,&def);
	if (zvptst("USE_INST")) inst = "Testing";
	
	status = ISTATFileOpen(unit,IMODE_WRITE,nclasses,nbands,inst);
	if (status<0) ISTATSignal(unit, status, 1);
	
	WriteStatsData(unit,nclasses,nbands);
	
	status = ISTATFileClose(unit);
	if (status<0) ISTATSignal(unit, status, 1);
	
}

WriteStatsData(unit,nclasses,nbands)
int unit;
int nclasses;
int nbands;
{
	int status;
	int row;
	char classname[9];
	float covariance[1000];
	float means[1000];
	int npix;
	
	memset(covariance,0,sizeof(covariance));
	memset(means,0,sizeof(means));
	for (row=1;row<=nclasses;row++)
	{
		/* just make some junk up */
		sprintf(classname,"CLASS%03d",row);
		npix = row*row;
		means[0] = (float)row/2;
		covariance[0] = (float)row*2;
		
		/* and write it out */
		status = ISTATRecordWrite(unit,row, classname,npix,
				nbands,means,covariance);
		if (status < 0) ISTATSignal(unit,status,1);
	}
}

ReadStatsFile(unit)
int unit;
{
	int status;
	int nbands,nclasses;
	int ncor;
	float *means,*covariance;
	char classname[9];
	char message[80];
	int npix,nb,row;

	status = ISTATFileOpen(unit,IMODE_READ,0,0,0);
	if (status<0) ISTATSignal(unit, status, 1);
	
	/* find out how big the file is and allocate memory */
	
	ISTATFileInfo(unit, &nclasses,&nbands,0);
	ncor = nbands*(nbands +1)/2;
	means = (float *)malloc(sizeof(float)*nbands);
	covariance = (float *)malloc(sizeof(float)*ncor);

	/* print out some records */

	sprintf(message,"%9s  %10s  %6s %6s %8s %8s",
	  "ROW","CLASSNAME","NPIX","NB","MEANS","COVAR");
	zvmessage(message," ");
	
	for (row=1;row<=nclasses;row++)
	{
		status = ISTATRecordRead(unit,row, classname,&npix,
				&nb,means,covariance);
		if (status < 0) ISTATSignal(unit,status,1);
		
		sprintf(message,"%9d \"%10s\" %6d %6d %8.3f %8.3f",
			row,classname,npix,nb,means[0],covariance[0]);
		zvmessage(message," ");
	}

	zvmessage("--- Searching for Class003 ---"," ");	
	row = ISTATRecordFind(unit,"class003");
	if (row > 0) 
	{
	  status = ISTATRecordRead(unit,row, classname,&npix,
			&nb,means,covariance);
	  if (status < 0) ISTATSignal(unit,status,1);
	
	  sprintf(message,"%9d \"%10s\" %6d %6d %8.3f %8.3f",
		row,classname,npix,nb,means[0],covariance[0]);
	  zvmessage(message," ");
	}
	else zvmessage("  Not Found. "," ");

	zvmessage("--- Searching for Class Wookie ---"," ");	
	row = ISTATRecordFind(unit,"class Wookie");
	if (row < 1 )  zvmessage ("  Not Found. "," ");

	status = ISTATFileClose(unit);
	if (status<0) ISTATSignal(unit, status, 1);
}

