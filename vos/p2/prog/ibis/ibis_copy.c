#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>


/************************************************************************
 ***                                                                  ***
 ***                           IBIS-COPY                              ***
 ***                                                                  ***
 ************************************************************************/
 

int copy_file()
{
	int inunit,outunit,status;
	int cols[1024],outcols[1024];
	int in, out,def;
	int incol,outcol;
	int nc_file,nr_file,nc=1, nr=1, sr,sc,col,ncout;
	int colsize=1;
	int row, row_inc, nrow;
	int ccnt,occnt,old_in,old_out;
	char type[201];
	char *format,*outformat=(char *)0, org[10],*mode;
	char inversion[10],cform[20];
	double buffer[DBUFSIZE]; /* force alignment */

	/* open the input IBIS file, abort on error */
	
	zvunit( &inunit, "inp", 1, NULL);
	status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
	if (status!=1)  IBISSignalU( inunit, status, 1);
	
	/* get the input row & column counts */
	
	IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );

	/* pre-format 0ld IBIS */
	IBISFileGet( in, IFILE_VERSION, inversion,  1, 1, 9 );
	old_in = !strcmp(inversion,IVERSION_1);
	old_out = zvptst(IVERSION_1);
	if (old_in) pre_format(in);
	

	zvp( "sr", &sr, &def );
	zvp( "nr", &nr, &def );
	if (!nr) nr=nr_file;
	if (sr > nr_file) sr=nr_file;
	if (sr + nr - 1 > nr_file)
		nr = nr_file + 1 - sr;
		
	zvp( "sc", &sc, &def );
	zvp( "nc", &ncout, &def );
	if (!ncout) ncout=nc_file;
	nc = ncout;
	zvp( "incols", cols, &ccnt );
	if (cols[0]) nc=ccnt; 
	else ccnt=0;
	
	zvp( "outcols", outcols, &occnt );
	if (!outcols[0]) occnt = 0;

	if (ccnt && occnt && occnt!=ccnt)
	{
		zvmessage("#incols and #outcols are unequal"," ");
		zabend();
	}
	if (sc > nc_file) sc=nc_file;
	if (sc + nc - 1 > nc_file)
		nc = nc_file + 1 - sc;

	/* get the input format string */
	format = new_format_string(nc_file,IFMT_REAL,0);
	if (!format)
	{
		zvmessage( "unable to allocate format string", 0);
		zabend();
	}
	IBISFileGet( in,  IFILE_FORMATS, format, 1, nc_file, IFMT_SIZE );

	/*
	 *  The only case where we have to be careful about
	 *  formatting the output file is when the input is
	 *  IBIS-2 and the out is IBIS-1; in this case we
	 *  we the "intcol" and "a4col" parms to set up the file.
	 */
	if (!(!old_in && old_out))
	{
		outformat = new_format_string(ncout,IFMT_REAL,0);
		if (!outformat)
		{
			zvmessage( "unable to allocate outformat string", 0);
			zabend();
		}
		for (col=0;col<nc;col++)
		{
			incol = ccnt ? cols[col]-1: col;
			outcol = occnt ? outcols[col]-1: col;
			strcpy(outformat+(outcol*IFMT_SIZE),
				format + (incol*IFMT_SIZE) );
		}
	}
	
	/* open the output file, or abort on error */
	
	zvpone( "org", org, 1, 0 );
	mode = old_out ? IMODE_OWRITE : IMODE_WRITE;
	zvunit( &outunit, "out", 1, NULL);
	status = IBISFileOpen( outunit, &out, mode, ncout, nr, outformat, org );
	if (status!=1)  IBISSignalU( outunit, status, 1);
	
	if (!old_in && old_out) pre_format(out);

	/* Set the File Type */	
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);

	
	/* 
	 * copy the columns
	 */

	for (col=0; col<nc; col++)
	{
		incol = ccnt ? cols[col] : col+1;
		outcol = occnt ? outcols[col] : col+1;

		if (old_out) /* The output determines buffer */
		{
		IBISColumnGet( out, ICOLUMN_U_FORMAT, cform, outcol );		
		status = IBISColumnSet( in, ICOLUMN_U_FORMAT, cform, incol );		
		if (status !=1 ) IBISSignal(in,status,1);
		}
		else  /* The input determines buffer */
		{
		IBISColumnGet( in, ICOLUMN_U_FORMAT, cform, incol );		
		status = IBISColumnSet(out, ICOLUMN_U_FORMAT, cform, outcol );		
		if (status !=1 ) IBISSignal(out,status,1);
		}
		IBISColumnGet( in, ICOLUMN_U_SIZE, &colsize, incol ); /* size of element */
		
		/* Compute # rows that will fit into buffer : */
		row_inc = (IBUFSIZE + colsize  - 1)/colsize ;
		
		for (row = 0; row<nr; row += row_inc)
		{
			nrow = MIN( nr-row , row_inc);
			
			status = IBISColumnRead( in, (char *)buffer, incol, row+sr, nrow );
			if (status !=1 ) IBISSignal(in,status,1);
			status = IBISColumnWrite( out, (char *)buffer, outcol, row+1, nrow );
			if (status !=1 ) IBISSignal(out,status,1);
			
		}
	}

	if (zvptst("gcopy") && !old_out)
	{
		status=IBISGroupTransfer( in, out, ITYPE_GROUP, cols, outcols, nc);
		if (status < 0) 
		{
			IBISSignal(in,status,0);
			zvmessage("Non-fatal; continuing..."," ");
		}
	}
	if (zvptst("ucopy") && !old_out)
	{
		status=IBISGroupTransfer( in, out, ITYPE_UNIT, cols, outcols, nc);
		if (status < 0) 
		{
			IBISSignal(in,status,0);
			zvmessage("Non-fatal; continuing..."," ");
		}
	}

	/* close up shop */

	IBISFileClose( in, 0 );
	status = IBISFileClose( out, 0 );
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	return 1;
}


