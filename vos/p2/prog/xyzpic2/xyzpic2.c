#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "ibisfile.h"
#include "ibiserrs.h"

void swap(d1, d2)
	double *d1, *d2;
{
	double tmp;
	
	tmp = *d1;
	*d1 = *d2;
	*d2 = tmp;
}

void selectionSort(line, samp, dn, nr)
	double *line, *samp, *dn;
	int nr;
{
	int i;
	
	for(i = 0; i < nr; i++)
	{
		int j, min;
		
		min = i;
		for(j = i+1; j < nr; j++)
			if(line[min] > line[j]) min = j;
		
		if(min != i)
		{
			swap(&(line[min]), &(line[i]));
			swap(&(samp[min]), &(samp[i]));
			swap(&(dn[min]), &(dn[i]));
		}
	}
}

void main44(void)
{
	int iunit, ounit, status, nl, ns, ibis, nr, nc, count;
	int i, index;
	double *ibisLine;
	double *ibisSamp;
	double *ibisDN;
	double *imgSamp;
	char format[5], org[7], ver[7];
	
	zifmessage("XYZPIC2 version 2016-06-09");
	
	/*get the parameters*/
	zvp("nl", &nl, &count);
	zvp("ns", &ns, &count);
	zvp("FORMAT", format, &count);
	
	/*open the IBIS input file*/
	status = zvunit(&iunit, "inp", 1, NULL);
	if(status != 1) zmabend("Error while opening input file - 1");
	status = IBISFileOpen(iunit, &ibis, IMODE_READ, 0, 0, 0, 0);
	if(status != 1) zmabend("Error while opening input file - 2:\nPossible error: check to see if IBIS input file is IBIS-2 graphics(ROW) file.");
	status = IBISFileGet(ibis, "nr", &nr, 1, 0, 0);
	if(status != 1) zmabend("Error while getting nr info.");
	status = IBISFileGet(ibis, "nc", &nc, 1, 0, 0);
	if(status != 1) zmabend("Error while getting nc info.");
	status = IBISFileGet(ibis, "org", org, 1, 0, 7);
	if(status != 1) IBISSignal(ibis, status, 0);
	status = IBISFileGet(ibis, "version", ver, 1, 0, 7);
	if(status != 1) IBISSignal(ibis, status, 0);
	for(i = 0; i < nc; i++)
	{
		status = IBISColumnSet(ibis, "U_FORMAT", "DOUB", i+1);
		if(status != 1) zmabend("Error while setting IBIS column formats.");
	}
	
	/*precondition*/
	if(strcmp(org, "ROW") != 0) zmabend("Input ibis file must be a graphics file (row-ordered).");
	if(strcmp(ver, "IBIS-2") != 0) zmabend("Input ibis file must be IBIS-2 file.");
	
	/*open the image output file*/
	zvselpi(0);
	status = zvunit(&ounit, "out", 1, NULL);
	if(status != 1) zmabend("Error while opening output file - 1");
	status = zvopen(ounit, "OP", "WRITE", "U_NL", nl, "U_NS", ns, "U_FORMAT", "DOUB", "O_FORMAT", format, "IO_ACT", "SA", NULL);
	if(status != 1) zmabend("Error while opening output file - 2");
	
	/*allocate memory*/
	ibisLine = (double*)malloc(sizeof(double)*nr);
	ibisSamp = (double*)malloc(sizeof(double)*nr);
	ibisDN = (double*)malloc(sizeof(double)*nr);
	imgSamp = (double*)malloc(sizeof(double)*ns);
		
	/*read IBIS file*/
	status = IBISColumnRead(ibis, (char*) ibisLine, 1, 1, nr);
	if(status != 1) zmabend("Error while reading 1st column of IBIS file.");
	status = IBISColumnRead(ibis, (char*) ibisSamp, 2, 1, nr);
	if(status != 1) zmabend("Error while reading 2nd column of IBIS file.");
	status = IBISColumnRead(ibis, (char*) ibisDN, 3, 1, nr);
	if(status != 1) zmabend("Error while reading 3rd column of IBIS file.");
	
	/*sort by line*/
	selectionSort(ibisLine, ibisSamp, ibisDN, nr);

	/*for(i = 0; i < nr; i++) printf("%d: %f %f %f\n", i+1, ibisLine[i], ibisSamp[i], ibisDN[i]);*/

	/*process - put points in buffer then write to image*/
	index = 0; /*index for the sorted ibis data*/
	for(i = 1; i <= nl; i++)
	{
		int k;
		for(k = 0; k < ns; k++) imgSamp[k] = 0;
		
		while(index < nr && ibisLine[index] == i)
		{	
			int samp;
			
			samp = (int)(ibisSamp[index] - 0.5);
			if(samp >= 0 && samp < ns)
				imgSamp[samp] = ibisDN[index];
				
			++index;
		}

        status = zvwrit(ounit, imgSamp, "LINE", i, NULL);
        if(status != 1) zmabend("Error while writing to image file.");
	}
	
	/*free memory*/
	free(ibisLine);
	free(ibisSamp);
	free(ibisDN);
	free(imgSamp);
	
	/*close files*/
	status = IBISFileClose(ibis, 0);
	if(status != 1) zmabend("Error while closing IBIS file.");
	status = zvclose(ounit, NULL);
	if(status != 1) zmabend("Error while closing image file.");
}
