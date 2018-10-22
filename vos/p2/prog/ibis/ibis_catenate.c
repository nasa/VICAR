#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-CATENATE                          ***
 ***                                                                  ***
 ************************************************************************/
 
int catenate_files()
{
	int inunit,outunit,status;
	int in, out,def;
	int incol,outcol,numout;
	int nc=0, nr=0,nr_file,nc_file, col;
	int colsize=1;
	int row, row_inc, nrow,cur_col,i;
	int srow[MAX_FILE],scol[MAX_FILE];
	int by_row;
	char type[201];
	char *format, org[10];
	char cform[20];
	char *mode;
	double buffer[DBUFSIZE]; /* force alignment */

	zvpcnt("INP",&numout);
	by_row = zvptst("BYROW");

	/* open the input IBIS file, abort on error */
	
	srow[0]=1;
	scol[0]=1;	
	if (by_row)
	{
		zvunit( &inunit, "inp", 1, NULL);
		status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
		if (status!=1)  IBISSignalU( inunit, status, 1);
		
		/* get the input column counts */
		
		IBISFileGet( in, IFILE_NC, &nc, 1, 1, 0 );
		
		/* get the input format string */
		
		format = new_format_string(nc,IFMT_REAL,0);
		if (!format)
		{
			zvmessage( "unable to allocate format string", 0);
			zabend();
		}
		IBISFileGet( in,  IFILE_FORMATS, format, 1, nc, IFMT_SIZE );
		IBISFileClose(in,0);
	
		/* Determine total number of rows from all files */
		for (i=1,nr=0;i<=numout;i++)
		{
			zvunit( &inunit, "inp", i, NULL);
			status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
			if (status!=1)  IBISSignalU( inunit, status, 1);
			IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
			nr += nr_file;
			IBISFileClose(in,0);
			scol[i]=1;
			srow[i]=srow[i-1]+nr_file;
		}

	}
	else
	{
		/* allocate the input format string */		
		format = new_format_string(1024,IFMT_REAL,0);
		if (!format)
		{
			zvmessage( "unable to allocate format string", 0);
			zabend();
		}
	
		/* Determine file format string and number of Rows & Cols */
		for (i=1,cur_col=1;i<=numout;i++)
		{
			zvunit( &inunit, "inp", i, NULL);
			status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
			if (status!=1)  IBISSignalU( inunit, status, 1);
			IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
			IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
			IBISFileGet( in,  IFILE_FORMATS,
				format+IFMT_SIZE*(cur_col-1), 1, nc_file, IFMT_SIZE );
			cur_col += nc_file;
			nc += nc_file;
			nr = (nr > nr_file) ? nr : nr_file;
			srow[i]=1;
			scol[i]=scol[i-1]+nc_file;
			IBISFileClose(in,0);
		}

	}
	
	/* open the output file, or abort on error */
	
	zvpone( "org", org, 1, 0 );
	zvunit( &outunit, "out", 1, NULL);
	if (zvptst("ibis-2")) mode=IMODE_WRITE;
	else mode=IMODE_OWRITE;

	status = IBISFileOpen( outunit, &out, mode, 
		nc, nr, format, org );
	if (status!=1)  IBISSignalU( outunit, status, 1);
	
	/* Set the File Type */	
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);
	
	/* 
	 * copy the columns of all files
	 */

	for (i=1;i<=numout;i++)
	{
		zvunit( &inunit, "inp", i, NULL);
		status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
		if (status!=1)  IBISSignalU( inunit, status, 1);
		IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
		IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	
		for (col=0; col<nc_file; col++)
		{
			outcol = col+scol[i-1]; incol = col+1;
			IBISColumnGet( out, ICOLUMN_FORMAT, cform, col+scol[i-1] );		
			status = IBISColumnSet(in, ICOLUMN_U_FORMAT, cform, col+1 );		
			if (status !=1 ) IBISSignal(out,status,1);
			IBISColumnGet( in, ICOLUMN_U_SIZE, &colsize, col+1 );
			
			/* Compute # rows that will fit into buffer : */
			row_inc = (IBUFSIZE + colsize  - 1)/colsize ;
			
			/* Copy the data of a single column */
			for (row = 0; row<nr_file; row += row_inc)
			{
				nrow = MIN( nr_file-row , row_inc);
				
				status = IBISColumnRead( in, (char *)buffer,
					incol, row+1, nrow );
				if (status !=1 ) IBISSignal(in,status,1);
				status = IBISColumnWrite( out, (char *)buffer,
					outcol, row+srow[i-1], nrow );
				if (status !=1 ) IBISSignal(out,status,1);
				
			}

			/* transfer the group memberships */
			if (zvptst("gcopy") )
			{
				status=IBISGroupTransfer( in, out, ITYPE_GROUP, &incol, &outcol, 1);
				if (status < 0) IBISSignal(in,status,0);
			}
			if (zvptst("ucopy") )
			{
				status=IBISGroupTransfer( in, out, ITYPE_UNIT, &incol, &outcol, 1);
				if (status < 0) IBISSignal(in,status,0);
			}

		}
	
		IBISFileClose( in, 0 );
		
	} /* end of file loop */
	
	/* close up shop */
	status = IBISFileClose( out, 0 );
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	return 1;
}


