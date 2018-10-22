/**
 **  tstibis2.c.c  Creates, modifes & Prints out values of columns of IBIS file
 **          Uses new IBIS-2 Subroutine Library.
 **/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif

int gr1_nc; /* NC for Graphics-1 files */

void FTN_NAME2_(main44_c,MAIN44_C)(void)
{
    int count,def;
    char command[20];
    
    /* get the global parm */
    zvp( "GR1_NC", &gr1_nc, &def);
    
    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (tolower(command[0]))
    {
	case 'c' : 
		   if (tolower(command[3]) == 'd')
		       delete_column();
		   else
		       insert_column();
		   break;
        case 'f' : 
			/* Call the Fortran routine */
        		FTN_NAME2_(fortran_test,FORTRAN_TEST)();
		   		break;
		   		
        case 'g' : 
        		group_define();
		   		break;
		   		
        case 'l' : 
        		list_file();
		   		break;

        case 'n' : 
        		new_file();
		   		break;

	case 'r' : 
		switch( tolower(command[3]) )
		{
			case 'd': delete_row(); break;
			case 'c': clear_row(); break;
			case 'r': record_read(); break;
			case 'w': record_write(); break;
			case 'z': record_zero(); break;
			default:  insert_row(); break;
		}
		break;
	case 's' : 
		test_signals();
		break;

        case 'z' : 
        	zap_label();
		break;
    }
    return;
}

#define NROWS 5
#define CUSIZE 8
#define CUFMT "A7"

new_file()
{
	int outunit,status;
	int out;
	int col,i;
	int nc, nr, nro;
	int count, def;
	char format[10];
	char org[10];
	char format_buf[20][IFMT_SIZE];
	char *fmt_ptr=(char *)0;
	char *mode;
	int ibuf[NROWS];
	char cbuf[NROWS][CUSIZE];
	
 	status=zvunit( &outunit, "out", 1, 0);
	if (status!=1) zvsignal( outunit, status, 1);

	zvp("nr", &nr, &def );
	zvp("nc", &nc, &def );

	zvparm("format", format_buf, &count, &def, 20, IFMT_SIZE);
	if (!def && count) 
	{
		fmt_ptr=(char *)format_buf;
		nc = count;
	}
	
  	nro =  (NROWS > nr) ? nr : NROWS;
    if (zvptst("ibis2")) mode=IMODE_WRITE;
    else mode=IMODE_OWRITE;
  
	/* open the output file, or abort on error */
	zvpone( "org", org, 1, 0 );

	status = IBISFileOpen(outunit, &out, mode, nc, nr, fmt_ptr, org);
	if (status!=1) IBISSignalU( outunit, status, 1);

	/* Set a Subtype */
	status = IBISFileSet(out, IFILE_TYPE, "TSTIBIS_TEST_FILE_TYPE",0);
	if (status!=1) IBISSignal( out, status, 1);

	/* write out something comprehensible to each column */
	
	for (col=1; col<=nc; col++)
	{
		status = IBISColumnGet(out, ICOLUMN_FORMAT, format, col); 
		if (status !=1 ) IBISSignalU( out, status, 1);
		if (tolower(format[0])=='a')
		{
			/* ASCII */
			for (i=0;i<NROWS;i++)
				sprintf(cbuf[i], "C%dR%d", col,i+1);
			status = IBISColumnSet(out, ICOLUMN_U_FORMAT, CUFMT, col);
			if (status !=1 ) IBISSignalU( out, status, 1);
			status = IBISColumnWrite(out, cbuf,col,1,nro);
			if (status !=1 ) IBISSignalU( out, status, 1);
		}
		else
		{
			/* NUMERIC */
			for (i=0;i<NROWS;i++)
				ibuf[i] = col+i;
			status = IBISColumnSet(out, ICOLUMN_U_FORMAT, IFMT_FULL, col);
			if (status !=1 ) IBISSignalU( out, status, 1);
			status = IBISColumnWrite(out, ibuf,col,1,nro);
			if (status !=1 ) IBISSignalU( out, status, 1);
		}
	}

	/* close up shop */
	
	status = IBISFileClose( out, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}



/* print an open ibis file */


group_define()
{
	int outunit;
	int status,def,count;
	int ibis;
	int cols[20],ncols;
	int *colptr=(int*)0;
	int nc, nr;
	char type[I2_MAX_TYPE_NAME];
	char name[I2_MAX_GRP_NAME];
	char expr[80];
	char *exprptr=(char *)0;
	
 	status=zvunit( &outunit, "inp", 1, 0);
	if (status!=1) zvsignal( outunit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(outunit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( outunit, status, 1);

	zvp("type", type, &def );
	zvp("grpname", name, &def );
    zvparm("cols",cols,&ncols,&def,0,0);
    if (!def) colptr=cols;
    zvparm("expr",expr,&count,&def,0,0);
    if (!def) exprptr = expr;

	count=IBISGroupNew( ibis, type, name, colptr, ncols , exprptr);
	if (count<0) IBISSignal( ibis, count, 0);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}

delete_column()
{
	int unit;
	int status,def;
	int ibis;
	int col,ncols;
	int nc, nr;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("col", &col, &def );
	zvp("ncol", &ncols, &def );

	status = IBISColumnDelete(ibis,col,ncols); 
	if (status!=1) IBISSignal( ibis, status, 1);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}


insert_column()
{
	int unit;
	int status,def;
	int ibis;
	int col,ncols;
	int nc, nr;
	int format[IFMT_SIZE];
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("col", &col, &def );
	zvp("ncol", &ncols, &def );
	zvp("format", format, &def );

	status = IBISColumnNew(ibis,col,ncols,format); 
	if (status!=1) IBISSignal( ibis, status, 1);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

test_signals()
{
	int status;
	zvmessage("*********** Test Signals ***************"," ");
	
	for (status=IBIS_BASE-1; status>IBIS_LAST; status--)
		IBISSignalU( 0, status, 0);

	zvmessage("*********** Test Signals End  ***************"," ");

	/* This should not return an error message */
	IBISSignalU( 0, 1, 0);

}

record_read()
{
	int unit;
	int status,def;
	int ibis;
	int row, srow,nrows,ncols,cols[20];
	int nc, nr;
	int col;
	int record;
	int fsize;
	int bufsize;
	char format[20];
	char valueelemt[80];
	char msgString[200];
	char *buffer,*ptr;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_READ, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("srow", &srow, &def );
	zvp("nrow", &nrows, &def );
	zvp("format", format, &def );
	zvparm("cols", cols, &ncols, &def, 0 );

	status = IBISRecordOpen( ibis, &record,  0, cols, ncols, format);
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordGet( record, IRECORD_REC_SIZE, &bufsize, 1, 1 );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordGet( record, IRECORD_U_SIZE, &fsize, 1, 1 );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	buffer = (char *)malloc( bufsize );
	if (!buffer) zabend();
	
	for (row = 0; row <nrows; row++)
	{
		status = IBISRecordRead( record, buffer, row+srow);
		if (status != 1) IBISSignal( ibis, status, 1);
		ptr = buffer;
		msgString[0] = '\0';
		for (col=0;col<ncols;col++)
		{
			switch (tolower(format[0])) { 
				case 'b': sprintf( valueelemt, "%-3d ",    *(char *)  (ptr));  break; 
				case 'h': sprintf( valueelemt, "%-6hd ",    *(short *) (ptr));  break; 
				case 'f': sprintf( valueelemt, "%-8ld ",    *(int *)  (ptr));  break; 
				case 'r': sprintf( valueelemt, "%-8g ",    *(float *)  (ptr));  break; 
				case 'd': sprintf( valueelemt, "%-8lg ",    *(double *)(ptr));  break; 
				case 'c': sprintf( valueelemt, "%-8g + i*%-8g",    ((float*)ptr)[0],((float*)ptr)[1] );break; 
				case 'a': sprintf( valueelemt, "%-8s ",    ptr );break; 
				default:  sprintf( valueelemt, "%s ",   "BAD FORMAT!" );break; 
			}
			strcat( msgString, valueelemt );
			ptr += fsize;
		}
		zvmessage( msgString, " ");
	} 
	
	/* close up shop */
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

record_write()
{
	int unit;
	int status,def;
	int ibis;
	int row, inrow,outrow,ncols,cols[20];
	int nc, nr;
	int nrows,recrows;
	int col;
	int inrecord,outrecord;
	int fsize;
	int bufsize;
	char format[20];
	char *buffer;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("inrow", &inrow, &def );
	zvp("nrows", &nrows, &def );
	zvp("recrows", &recrows, &def );
	zvp("outrow", &outrow, &def );
	zvp("format", format, &def );
	zvparm("cols", cols, &ncols, &def, 0 );

	/*
	 *  Create a record with the given rows, format translation
	 *  and also resize the buffer to "recrows" rows.
	 */

	status = IBISRecordOpen( ibis, &inrecord,  0, cols, ncols, format);
	if (status != 1) IBISSignal( ibis, status, 1 );

	status = IBISRecordSet( inrecord, IRECORD_NR, recrows );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordOpen( ibis, &outrecord,  0, cols, ncols, format);
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordGet( inrecord, IRECORD_REC_SIZE, &bufsize, 1, 1 );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordGet( inrecord, IRECORD_U_SIZE, &fsize, 1, 1 );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	buffer = (char *)malloc( bufsize );
	if (!buffer) zabend();

	for (row=0;row<nrows;row++)
	{
		status = IBISRecordRead( inrecord, buffer, inrow+row);
		if (status != 1) IBISSignal( ibis, status, 1);
		
		status = IBISRecordWrite( outrecord, buffer, outrow+row);
		if (status != 1) IBISSignal( ibis, status, 1);
	}
	
	/* close up shop */
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

record_zero()
{
	int unit;
	int status,def;
	int ibis;
	int row, srow,nrows,ncols,cols[20];
	int nc, nr;
	int col;
	int record;
	char format[20];
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("srow", &srow, &def );
	zvp("nrow", &nrows, &def );
	zvp("format", format, &def );
	zvparm("cols", cols, &ncols, &def, 0 );

	status = IBISRecordOpen( ibis, &record,  0, cols, ncols, format);
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	status = IBISRecordClear( record, srow, nrows );
	if (status != 1) IBISSignal( ibis, status, 1 );
	
	/* close up shop */
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

delete_row()
{
	int unit;
	int status,def;
	int ibis;
	int row,nrows;
	int nc, nr;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("row", &row, &def );
	zvp("nrow", &nrows, &def );

	status = IBISRowDelete(ibis,row,nrows); 
	if (status!=1) IBISSignal( ibis, status, 1);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

insert_row()
{
	int unit;
	int status,def;
	int ibis;
	int row,nrows;
	int nc, nr;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("row", &row, &def );
	zvp("nrow", &nrows, &def );

	status = IBISRowNew(ibis,row,nrows); 
	if (status!=1) IBISSignal( ibis, status, 1);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}

clear_row()
{
	int unit;
	int status,def;
	int ibis;
	int row,nrows;
	int nc, nr;
	
 	status=zvunit( &unit, "inp", 1, 0);
	if (status!=1) zvsignal( unit, status, 1);
 
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(unit, &ibis, IMODE_UPDATE, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( unit, status, 1);

	zvp("row", &row, &def );
	zvp("nrow", &nrows, &def );

	status = IBISRowClear(ibis,row,nrows); 
	if (status!=1) IBISSignal( ibis, status, 1);

	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( unit, status, 1);
}



#define MAXCOLSIZE 40
#define ROWSIZE 132
#define MAXCOLPERROW 20
#define IBUFSIZE 6000
#define MAXGRPS 20

list_file()
{
	int ibis; /* ibis descriptor */
	int srow,nrows,scol,ncols;
	int inunit,status,def;
	long in;
	int nc=1, nr=1, rinc, rows_left,  col,i;
	int row, row_inc=30;
	int cols[I2_MAX_COL], deflt, org[10];
	int colm;
	int colnum;
	int ngcols;
	int group;
	int ngroups;
	char grouplist[MAXGRPS][I2_MAX_GRP_NAME];
	int srow1;
	int csize=15,colperrow,rowsize=132;
	int colsize[MAXCOLPERROW];
	
	/* we need to do this to insure byte-alignment: */

	double buffer[MAXCOLPERROW][IBUFSIZE/sizeof(double)];
	char *format;
	char *dataPtr[MAXCOLPERROW];
	char headerStr[ROWSIZE+2], headelemt[MAXCOLSIZE+1];
	char formatStr[ROWSIZE+2], fmtelemt[MAXCOLSIZE+1];
	char groupStr[ROWSIZE+2], grpelemt[MAXCOLSIZE+1];
	char unitStr[ROWSIZE+2], unitelemt[MAXCOLSIZE+1];
	char lineStr[ROWSIZE+2],lineelemt[MAXCOLSIZE+1];
	char valueStr[ROWSIZE+2], valueelemt[MAXCOLSIZE+1];
	char headerfmt[10],formatfmt[14];
	char valuefmt[MAXCOLPERROW][14];
	char outStr[ROWSIZE+2];
	char version[10];
	char *ptr,*fmtstr,fmtchar;

	zvp("sr", &srow, &def );
	zvp("nr", &nrows, &def );
	zvp("sc", &scol, &def );
	zvp("nc", &ncols, &def );

	/* 
	 * We will add zveaction to make sure that everything
	 * still works with this turned on.
	 */
	
	zveaction("SA"," ");

	/* Open file. */
 	status=zvunit( &inunit, "inp", 1, 0);
	if (status!=1) zvsignal( inunit, status, 1);	
	status = IBISFileOpen(inunit, &ibis, IMODE_READ, gr1_nc, 0, 0, 0);
	if (status!=1) IBISSignalU( inunit, status, 1);
  
	
	/* get the file attributes */
	
	status=IBISFileGet( ibis, IFILE_NC, &nc, 1, 1, 0 );
	if (status<0) goto failure;

	IBISFileGet( ibis, IFILE_NR, &nr,  1, 1, 0 );
	if (status<0) goto failure;
	
	IBISFileGet( ibis, IFILE_ORG, org,  1, 1, 9 );
	if (status<0) goto failure;
	
	IBISFileGet( ibis, IFILE_VERSION, version,  1, 1, 9 );
	if (status<0) goto failure;

	
	zvmessage( " ", " ");
	sprintf( outStr, "Number of Rows:%-d  Number of Columns: %-8d", nr, nc );
	zvmessage( outStr, " ");
	sprintf( outStr, "File Version: %s  Organization:%s", version, org );
	zvmessage( outStr, " ");


	ngroups = IBISFileGet( ibis, IFILE_GROUPS, grouplist,  
					1, MAXGRPS, I2_MAX_GRP_NAME );
	if (ngroups<0) goto failure;
	for (group=0;group<ngroups;group++)
	{
		sprintf( outStr, "Group %s:", grouplist[group] );
		ngcols=IBISColumnFind( ibis, ITYPE_GROUP, grouplist[group],
				cols, 1, 10);
		if (ngcols<0) 
		{
			IBISSignal( ibis, ngcols, 0);
			continue;
		}
		for (i=0;i<ngcols;i++)
		{
			sprintf(valueelemt, " %d", cols[i]);
			strcat(outStr, valueelemt);
		}
		zvmessage( outStr, " ");
	}

	ngroups = IBISFileGet( ibis, IFILE_UNITS, grouplist, 
					1, MAXGRPS, I2_MAX_GRP_NAME );
	if (ngroups<0) goto failure;
	for (group=0;group<ngroups;group++)
	{
		sprintf( outStr, "Unit %s:", grouplist[group] );
		ngcols=IBISColumnFind( ibis, ITYPE_UNIT, grouplist[group], cols,
					1, 10);
		if (ngcols<0) 
		{
			IBISSignal( ibis, ngcols, 0);
			continue;
		}
		for (i=0;i<ngcols;i++)
		{
			sprintf(valueelemt, " %d", cols[i]);
			strcat(outStr, valueelemt);
		}
		zvmessage( outStr, " ");
	}

	
	/* get the input format string */
	
	format = (char *)malloc( sizeof(char)*IFMT_SIZE* (nc+1) );
	if (!format)
	{
		zvmessage( "unable to allocate format string", 0);
		zabend();
	}
	
	csize=14; /* 16-character columns */
	rowsize=76; /* 80-char screen */

	colperrow = rowsize/csize;
	
	/* set up rows */
	if (srow > nr) srow=nr;
	if (nrows > nr + 1 - srow ) nrows = nr + 1 - srow;
	if (nrows > row_inc) row_inc=nrows;
	
	/* set up columns */
	if (scol > nc) scol=nc;
	if (ncols > nc + 1 - scol ) ncols = nc + 1 - scol;
	
	/* Set up the column formatting strings */

	sprintf(headerfmt, " C:%%-%dd", csize-3);
	sprintf(formatfmt, " %%-%ds", csize-1);
	
	for (i=0;i<csize;i++) lineelemt[i]='-';
	lineelemt[csize]='\0';
	for (colnum = 0; colnum<colperrow && colnum < nc; colnum++)
		sprintf( lineStr+colnum*csize, formatfmt, lineelemt );

	/* The big loop */

	for (col=0; col<ncols; col+=colperrow)
	{
		
		/* set up the header */
		
		headerStr[0]=formatStr[0]='\0';
		
		for (colnum = 0; colnum<colperrow && col+colnum<ncols; colnum++)
		{
			colm = scol + col+colnum;
			status=IBISColumnGet( ibis, ICOLUMN_FORMAT, format+IFMT_SIZE*colm, colm );
			if (status<0) goto failure;
			status=IBISColumnGet( ibis, ICOLUMN_U_SIZE, colsize+colnum, colm, 0 );
			if (status<0) goto failure;
			
			strcpy(grpelemt," -- ");
			status=IBISGroupFind( ibis, ITYPE_GROUP, colm,
							 grpelemt, 1,1,sizeof(grpelemt));
			if (status < 0) goto failure;
			sprintf(groupStr+csize*colnum,formatfmt, grpelemt);

			strcpy(unitelemt," -- ");
			status=IBISGroupFind( ibis, ITYPE_UNIT, colm,
							 unitelemt, 1,1,sizeof(unitelemt));
			if (status<0) goto failure;
			sprintf(unitStr+csize*colnum,formatfmt, unitelemt);

			fmtchar = format[IFMT_SIZE*colm];
			fmtstr = valuefmt[colnum];
			switch (tolower(fmtchar)) { 
				case 'b': 
				case 'h': 
				case 'f': sprintf( fmtstr, "%%-%dd ",    (csize)-1 );  break; 
				case 'r': sprintf( fmtstr, "%%-%dg ",  (csize)-1);  break; 
				case 'd': sprintf( fmtstr, "%%-%dlg ", (csize)-1);  break; 
				case 'c': sprintf( fmtstr, "%%-%dg+i%%-%dg", 
							((csize)-1)/2, ((csize)-1)/2 );break; 
				case 'a': sprintf( fmtstr, "\'%%-%ds\' ", (csize)-3 );break; 
				default:  sprintf( fmtstr, "%%-%ds ", (csize)-1);break; 
			}
			sprintf(headerStr+csize*colnum,headerfmt, colm);

			sprintf(fmtelemt,formatfmt, format + IFMT_SIZE*colm);
			strcat(formatStr, fmtelemt );
		}

		srow1=srow;
		for (rows_left=nrows; rows_left>0; rows_left-=rinc)
		{

			rinc = row_inc;
			if (rows_left < rinc) rinc = rows_left;

			zvmessage( " ", " ");
			sprintf( outStr, "Rows: %-d:%-d", srow1, srow1+rinc-1 );
			zvmessage( outStr, " ");
			zvmessage( lineStr, " " );
			zvmessage( headerStr, " ");
			zvmessage( formatStr, " ");
			zvmessage( groupStr, " ");
			zvmessage( unitStr, " ");
			zvmessage( lineStr, " " );
		
			/* get the column values */
	
			for (colnum = 0; colnum<colperrow && col+colnum<ncols; colnum++)
			{
				colm = scol + col+colnum;
				status=IBISColumnRead( ibis, buffer[colnum], colm, srow1, rinc );
				if (status!=1) goto failure;
				dataPtr[colnum] = (char *)buffer[colnum];
			}
			
	
			/* print the column values */
	
			for (row=0; row<rinc; row++)
			{

				strcpy(valueStr," ");
				for (colnum = 0; colnum<colperrow && col+colnum<ncols; colnum++)
				{
					colm = scol + col+colnum;

					ptr=dataPtr[colnum];
					fmtchar=format[IFMT_SIZE*colm];
					fmtstr=valuefmt[colnum];
					switch (tolower(fmtchar)) { 
						case 'b': sprintf( valueelemt, fmtstr,    *(char *)  (ptr));  break; 
						case 'h': sprintf( valueelemt, fmtstr,    *(short *) (ptr));  break; 
						case 'f': sprintf( valueelemt, fmtstr,    *(int *)  (ptr));  break; 
						case 'r': sprintf( valueelemt, fmtstr,    *(float *)  (ptr));  break; 
						case 'd': sprintf( valueelemt, fmtstr,    *(double *)(ptr));  break; 
						case 'c': sprintf( valueelemt, fmtstr,    ((float*)ptr)[0],((float*)ptr)[1] );break; 
						case 'a': sprintf( valueelemt, fmtstr,    ptr );break; 
						default:  sprintf( valueelemt, fmtstr,   "BAD FORMAT!" );break; 
					}
					valueelemt[csize]='\0';
					strcat(valueStr, valueelemt);
					dataPtr[colnum] += colsize[colnum];
				}
				zvmessage( valueStr, " ");
				/*** if (!((srow1 + row)%10)) zvmessage(" "," "); ***/
			}
			srow1 += rinc;
		}
	}

	return status;
	
failure:

	return status;
}


/*
 * Routine purges IBIS-2 labels from files
 */
zap_label()
{
	int unit,status;

	status = zvunit(&unit,"inp",1,0);
	if (status != 1) zvsignal(unit,status,1);

	status = zvopen(unit,"op","update",0);
	if (status != 1) zvsignal(unit,status,1);

	status = IBISLabelRemove(unit);	
	if (status != 1) zvsignal(unit,status,1);

	status = zvclose(unit,0);
}




