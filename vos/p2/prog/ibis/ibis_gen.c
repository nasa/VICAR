#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>
#include <stdlib.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-GEN                               ***
 ***                                                                  ***
 ************************************************************************/
 
int new_file(void)
{
	int outunit,status;
	int out;
	int nc, nr,i;
	int count, def;
	int fmt_cols[100];
	char org[10];
	char type[201];
	char def_fmt[20];
	char colformat[MAX_COLUMN][IFMT_SIZE];
	char format_buf[MAX_FMT][IFMT_SIZE];
	char *fmt_ptr=(char *)0;
	char *mode;
	
 	status=zvunit( &outunit, "out", 1, NULL);
	if (status!=1) zvsignal( outunit, status, 1);

	zvp("nr", &nr, &def ); if (!nr) nr=10;
	zvp("nc", &nc, &def ); if (!nc) nc=10;
	zvp("deffmt", def_fmt, &def );
	if (!def_fmt[0] || def_fmt[0]==' ') strcpy(def_fmt,IFMT_REAL);

	/* Create the Formatting string */
	format_buf[0][0]='\0';
	new_format_string(MAX_COLUMN,def_fmt,colformat);
	zvparm("format", colformat, &count, &def, MAX_COLUMN, IFMT_SIZE);
	if (!def && count && strlen((char *) colformat)) 
	{
		fmt_ptr=(char *)colformat;
		zvp("fmtcols", fmt_cols, &def );
		if (count > nc) nc = count;
		if (fmt_cols[0] || count < nc)
		{
			/* format specified columns; all others default */
			new_format_string(nc,def_fmt,format_buf);
			fmt_ptr=(char *)format_buf;
			if (fmt_cols[0])
			{
			  for (i=0;i<count;i++)
			   if (fmt_cols[i]<(nc+1))
				strcpy(format_buf[fmt_cols[i]-1], colformat[i]);
			}
			else
			   memcpy(format_buf,colformat,(long)IFMT_SIZE*count);
		}
	}
	
	if (zvptst("ibis-2")) mode=IMODE_WRITE;
	else mode=IMODE_OWRITE;
  
	/* Create the output file unit, or abort on error */
	zvpone( "org", org, 1, 0 );

	status = IBISFileUnit(outunit, &out, mode, nc, nr, fmt_ptr, org);
	if (status!=1) IBISSignalU( outunit, status, 1);

	/* Set up subfile type */
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);

	/* Install Default-Format, if any */
	if (def_fmt[0] && def_fmt[0]!=' ')
	{
	   status=IBISFileSet(out,IFILE_FMT_DEFAULT,def_fmt,0);
	   if (status!=1) IBISSignalU( outunit, status, 1);
	}

	/* Create and open the file */
	status = IBISFileUnitOpen(out);
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	/* Write out data ? */
	status = write_data(out,nr,nc);
	if (status!=1) IBISSignalU( outunit, status, 1);

	/* close up shop */
	
	status = IBISFileClose( out, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}

int write_data(ibis)
int ibis;
{
	int status=1;
	int ncols,def,indexcol;
	int record;
	int cols[MAX_DATA];
	int nvals,nrows,row;

	zvp("indexcol",&indexcol,&def);
	if (indexcol)
	{
		int data[512],nr,i,rowsnow,rowsleft;
		
		IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
		status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,
				IFMT_FULL,indexcol);
		if (status !=1) goto failure;
		
		/* write out the INDEX values */
		for (row=1,rowsleft=nr;rowsleft>0;rowsleft-=rowsnow)
		{
		   rowsnow = rowsleft < 512? rowsleft : 512;
		   for (i=0;i<rowsnow;i++)
		   	data[i] = row+i;
		   status = IBISColumnWrite(ibis,(char *)data,indexcol,
								row,rowsnow);
		   if (status !=1) goto failure;
		   row += rowsnow;
		}
		
	}
	
	zvparm("datacols",cols,&ncols,&def,0,0);
	if (ncols && cols[0])
	{
		float *data,*dptr;
		
		zvpcnt("data",&nvals);
		nrows = (nvals + ncols-1)/ncols;
		if (!nvals || !nrows) goto next;
		
		data=(float *)calloc(1L,sizeof(float)*nvals);
		zvp("data",data,&def);

		status=IBISRecordOpen(ibis,&record,0,cols,ncols,IFMT_REAL);
		if (status !=1) goto failure;
		
		for (row=1,dptr=data;row<=nrows;row++,dptr+=ncols)
		{
		   status = IBISRecordWrite(record,(char *)dptr,row);
		   if (status !=1) goto failure;
		}
		
		status=IBISRecordClose(record);
		if (status !=1) goto failure;

		free (data);
	}
next:	
	zvparm("strcols",cols,&ncols,&def,0,0);
	if (ncols && cols[0])
	{
		char *data,*dptr;
		int size=81;
		
		zvpcnt("string",&nvals);
		nrows = (nvals + ncols-1)/ncols;
		if (!nvals || !nrows) goto end;
		
		data=(char *)calloc(1L,size*nvals);
		zvparm("string",data,&nvals,&def,0,size);

		status=IBISRecordOpen(ibis,&record,0,cols,ncols,"A80");
		if (status !=1) goto failure;
		
		for (row=1,dptr=data;row<=nrows;row++,dptr+=ncols*size)
		{
		   status = IBISRecordWrite(record,dptr,row);
		   if (status !=1) goto failure;
		}
		
		status=IBISRecordClose(record);
		if (status !=1) goto failure;

		free (data);
	}
end:	
	return 1;
failure:
	return status;
}

