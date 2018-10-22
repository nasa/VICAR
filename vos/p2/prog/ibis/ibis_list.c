#include "ibis_local.h"
#include <string.h>
#include <stdio.h>
#include <zvproto.h>
#include <stdlib.h>
#include <string.h>

void outmessage(char* msg,char* dummy);
void pre_format(int ibis);

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-LIST                              ***
 ***                                                                  ***
 ************************************************************************/

static FILE* fd=0;

#define IBISPRINT( str, ptr, fmtchar, fmtStr ) \
	switch (tolower(fmtchar)) { \
		case 'b': sprintf( str, fmtStr,    *(char *)  (ptr));  break; \
		case 'h': sprintf( str, fmtStr,    *(short *) (ptr));  break; \
		case 'f': sprintf( str, fmtStr,    *(long *)  (ptr));  break; \
		case 'r': sprintf( str, fmtStr,    *(float *)  (ptr));  break; \
		case 'd': sprintf( str, fmtStr,    *(double *)(ptr));  break; \
		case 'c': sprintf( str, fmtStr,    ((float*)ptr)[0],((float*)ptr)[1] );break; \
		case 'a': sprintf( str, fmtStr,    ptr );break; \
		default:  sprintf( str, fmtStr,   "BAD FORMAT!" );break; \
	}

#define IBISFORMAT( str, fmtchar, colsize ) \
	switch (tolower(fmtchar)) { \
		case 'b': \
		case 'h': \
		case 'f': sprintf( str, "%%%dd",  (colsize));  break; \
		case 'r': sprintf( str, "%%%d.2f",  (colsize));  break; \
		case 'd': sprintf( str, "%%%d.2lf", (colsize));  break; \
		case 'c': sprintf( str, " (%%-%d.2f,%%%d.2f)", \
			((colsize)-4)/2, colsize - (4 + ((colsize)-4)/2) );\
			 break; \
		case 'a': sprintf( str, "%%%ds", (colsize) );break; \
		default:  sprintf( str, "%%%ds", (colsize));break; \
	}


static void parse_user_format();
static void show_header();
static int show_groups();
static void show_preamble();

int list_file(void)
{
	int inunit,status;
	int in;
	int nc_file,nr_file,nr,nc, rinc, rows_left, scol, col,i;
	int row, row_inc;
	int cols[1024], def,ccnt;
	int colm;
	int space;
	int total;
	int colnum;
	int srow,srow1;
	int csize[MAXCOLPERROW],colperrow,rowsize=132;
	int colsize[MAXCOLPERROW],coffset[MAXCOLPERROW];
	int disp_header,disp_groups,disp_units,disp_formats;
	int user_format=0,col_header;
	double buffer[MAXCOLPERROW][DBUFSIZE];
	char *format;
	char *dataPtr[MAXCOLPERROW];
	char headerStr[ROWSIZE+2];
	char formatStr[ROWSIZE+2], fmtelemt[MAXCOLSIZE+1];
	char groupStr[ROWSIZE+2], grpelemt[MAXCOLSIZE+1];
	char unitStr[ROWSIZE+2], unitelemt[MAXCOLSIZE+1];
	char lineStr[ROWSIZE+2],lineelemt[MAXCOLSIZE+1];
	char valueStr[ROWSIZE+2], valueelemt[MAXCOLSIZE+1];
	char headerfmt[20],formatfmt[20];
	char valuefmt[MAXCOLPERROW][20];
	char userfmt[200],cfmt[MAXCOLPERROW][20];
	char outStr[ROWSIZE+2];
	char version[10];
	char colformat[10];
	char outfile[255];
	
	/* open the input IBIS file, abort on error */
	
	zvunit( &inunit, "inp", 1, NULL);
	status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
	if (status!=1)  IBISSignalU( inunit, status, 1);
	
	/* Find out if there is an ASCII text file */
	zvp("outfile", outfile, &def );
	if (outfile[0]!=' ')
	{
		fd = fopen(outfile,"w");
		if (!fd)
		{
			zvmessage("cant open output file"," ");
			zabend();
		}
	}

	/* get the input row & column counts */
	
	IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	IBISFileGet( in, IFILE_NR, &nr_file,  1, 1, 0 );
	
	disp_groups = zvptst("groups");
	disp_units = zvptst("units");
	disp_header = zvptst("header");
	col_header = zvptst("colhead");
	disp_formats = zvptst("formats");
	zvp("space", &space, &def );

	show_preamble(); /* user preamble */
	
	IBISFileGet( in, IFILE_VERSION, version,  1, 1, 9 );
	if (disp_header) 
	{
		show_header(in,nr_file,nc_file,version);
		if (disp_groups) show_groups(in,"Group \'%s\':",IFILE_GROUPS,ITYPE_GROUP);
		if (disp_units) show_groups(in,"Unit \'%s\':",IFILE_UNITS,ITYPE_UNIT);	
	}
	
	/* get the input format string */
	
	format = (char *)malloc( sizeof(char)*IFMT_SIZE* (nc_file+1) );
	if (!format)
	{
		outmessage( "unable to allocate format string", " ");
		zabend();
	}
	
	zvp( "cformat",userfmt,&def);
	memset(csize,0,sizeof(csize));
	if (strlen(userfmt))
	{
		user_format = 1;
		parse_user_format(userfmt,cfmt,csize);
	}
	else
	{
		zvp( "csize", csize, &def );
		if (!csize[0]) csize[0] = 15;
	}
	zvp( "screen", &rowsize, &def );
	if (!rowsize) rowsize=80;

	/* which & how many rows to read */
	
	zvp( "sr", &srow, &def );
	zvp( "nr", &nr, &def );
	if (!nr) nr=nr_file;
	if (srow > nr_file) srow=nr_file;
	if (srow + nr - 1 > nr_file)
		nr = nr_file + 1 - srow;
	row_inc = nr;
	if (row_inc > 30) row_inc=30;
	
	/* which & how many columns to read */	
	
	zvp("sc", &scol, &def );
	zvp("nc", &nc, &def );
	if (!nc) nc = nc_file;
	
	zvp( "cols", cols, &ccnt );
	if (cols[0]) nc=ccnt;
	else ccnt = 0;
	
	if (scol > nc_file) scol=nc_file;
	if (scol + nc - 1 > nc_file)
		nc = nc_file + 1 - scol;
	
	/* pre-format old IBIS files */
	if (!strcmp(version,IVERSION_1)) pre_format(in);
	
	/* Set up the column formatting strings */

	/* This used to be %%-%dd and %%-%ds but csize and csize-2	*/
	/* are pointers so carto changed them to %p.  The whole thing	*/
	/* makes no sense, but appears to be unused anyway. rgd 3/2010	*/
	sprintf(headerfmt, "C:%%-%pd", csize-2);
	sprintf(formatfmt, "%%-%ps", csize);
	
	if (!csize[1]) 
		for (colnum=1;colnum<nc;colnum++) 
			csize[colnum]=csize[0];
	csize[nc+1]=1;

	for (i=0;i<MAXCOLSIZE;i++) lineelemt[i]='-';
	lineelemt[MAXCOLSIZE]='\0';
	

	/* The big loop */

	for (col=0; col<nc; col+=colperrow)
	{
		/* figure out how many cols this time */
		coffset[0]=0;
		for (colperrow=0,total=csize[col];
			total<rowsize && col+colperrow<nc;colperrow++)
		{
			coffset[colperrow+1]=total;
			total+=csize[col+colperrow+1];
		}
		
		/* draw some dashed lines */
		for (colnum = 0; colnum<colperrow; colnum++)
		{
			strcpy(lineStr+coffset[colnum],"+");
			strncpy( lineStr+coffset[colnum]+1,
				lineelemt,csize[col+colnum]-1);
			lineStr[coffset[colnum+1]] = '\0';
		}
		/* set up the header */
		
		headerStr[0]=formatStr[0]='\0';
		
		for (colnum = 0; colnum<colperrow; colnum++)
		{
			colm = ccnt ?  cols[col+colnum] : col+scol+colnum;
			IBISColumnGet( in, ICOLUMN_FORMAT,colformat, colm );
			IBISColumnGet( in, ICOLUMN_U_FORMAT, format+IFMT_SIZE*colm, colm );
			IBISColumnGet( in, ICOLUMN_U_SIZE, colsize+colnum, colm);
			
			strcpy(grpelemt," -- ");
			IBISGroupFind( in, ITYPE_GROUP, colm, grpelemt, 1,1,sizeof(grpelemt));
			sprintf(groupStr+coffset[colnum],
				"%-*s",csize[col+colnum], grpelemt);

			strcpy(unitelemt," -- ");
			IBISGroupFind( in, ITYPE_UNIT, colm, unitelemt, 1,1,sizeof(unitelemt));
			sprintf(unitStr+coffset[colnum],
				"%-*s",csize[col+colnum], unitelemt);
			if (user_format)
				strcpy(valuefmt[colnum], cfmt[colnum+col]);
			else {
			  IBISFORMAT( valuefmt[colnum],format[IFMT_SIZE*colm], csize[col+colnum] )
			}
			sprintf(fmtelemt,"C:%-d", colm);
			sprintf(headerStr+coffset[colnum],"%*s",csize[col+colnum], fmtelemt);

			sprintf(fmtelemt,"%*s",csize[col+colnum], colformat);
			strcat(formatStr, fmtelemt );
		}

		srow1=srow;
		for (rows_left=nr; rows_left>0; rows_left-=rinc)
		{

			rinc = row_inc;
			if (rows_left < rinc) rinc = rows_left;

			if (col_header)
			{
				outmessage( " ", " ");
				sprintf( outStr, "Rows: %-d:%-d", srow1, srow1+rinc-1 );
				outmessage( outStr, " ");
				outmessage( lineStr, " " );
				outmessage( headerStr, " ");
				if (disp_formats) outmessage( formatStr, " ");
				if (disp_groups) outmessage( groupStr, " ");
				if (disp_units) outmessage( unitStr, " ");
				outmessage( lineStr, " " );
			}
			/* get the column values */
	
			for (colnum = 0; colnum<colperrow && col+colnum<nc; colnum++)
			{
				colm = ccnt ?   cols[col+colnum] : col+scol+colnum ;
				IBISColumnRead( in, (char*) buffer[colnum], colm, srow1, rinc );
				dataPtr[colnum] = (char *)buffer[colnum];
			}
			
	
			/* print the column values */
	
			for (row=0; row<rinc; row++)
			{

				/* strcpy(valueStr," "); */
				valueStr[0]='\0';
				for (colnum = 0; colnum<colperrow && col+colnum<nc; colnum++)
				{
					colm = ccnt ? cols[col+colnum] : col+scol+colnum ;
					IBISPRINT ( valueelemt, 
					   dataPtr[colnum], format[IFMT_SIZE*colm], valuefmt[colnum])
					valueelemt[csize[col+colnum]]='\0';
					strcat(valueStr, valueelemt);
					dataPtr[colnum] += colsize[colnum];
				}
				/* make sure no bad chars */
				for (i=0;i<rowsize && valueStr[i];i++)
				   if (!isprint(valueStr[i]))
					valueStr[i]='.';
				outmessage( valueStr, " ");
				if (space && !((srow1 + row)%space)) outmessage(" "," ");
			}
			srow1 += rinc;
		}
	}

	/* close up shop */

	IBISFileClose( in, 0 );
	if (fd) fclose(fd);
}

/* go through C-string format and grab each '%' element */

static void parse_user_format(userfmt,cfmt,csize)
char *userfmt;
char cfmt[MAXCOLPERROW][20];
int *csize;
{
	int len,index=0;
	char *ptr,*endpt,*charpt,*start;
	char fmt[80],test[100];
	
	for (ptr=userfmt; *ptr; ptr=endpt,csize++,index++)
	{
		start = ptr;
		endpt = strchr(ptr,'%'); /* find this % */
		if (!endpt) endpt = ptr+strlen(ptr);
		else endpt++; /* go past it */
                charpt = strchr(ptr,'s'); /* also, find s */
		ptr = endpt;
		endpt = strchr(ptr,'%'); /* find next % */
		if (!endpt) endpt = ptr+strlen(ptr);
		len = endpt - start;	
		strncpy(fmt,start,len);
		fmt[len]='\0';
                if (!charpt || charpt>endpt) sprintf(test,fmt,0.0);
                else sprintf(test,fmt,"woof");
		*csize = strlen(test);
		strcpy(cfmt[index],fmt);
	}
}


static void show_header(ibis,nr,nc,version)
int ibis;
int nr;
int nc;
char *version;
{
	char org[10],type[201];
	char outStr[200];

	IBISFileGet( ibis, IFILE_ORG, org,  1, 1, 9 );
	IBISFileGet( ibis, IFILE_TYPE, type,  1, 1, 200 );
	if (!type[0]) strcpy(type,"NONE");
	
	outmessage( " ", " ");
	sprintf( outStr, "Number of Rows:%-d  Number of Columns: %-8d", nr, nc );
	outmessage( outStr, " ");
	sprintf( outStr, "File Version:%s  Organization:%s  SubType:%s",
		 version, org, type );
	outmessage( outStr, " ");
}


static int show_groups(ibis,fmtstr,filegrp,type)
int ibis;
char *fmtstr;
char *filegrp;
char *type;
{
	char groupname[MAXGRPNAME+1];
	int ngcols,ngroups,group,i;
	int cols[1024];
	char valueelemt[200],outStr[200];

	ngroups = IBISFileGet( ibis, filegrp, groupname,  1, 0, MAXGRPNAME );
	if (ngroups<0) return ngroups;
	for (group=0;group<ngroups;group++)
	{
		ngcols = IBISFileGet( ibis, filegrp, groupname, group+1, 1, MAXGRPNAME );
		sprintf( outStr, fmtstr, groupname );
		ngcols=IBISColumnFind( ibis, type, groupname, cols, 1, 1024);
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
		outmessage( outStr, " ");
	}
	
	return ngroups;
}

#define MAX_PRE_LINES 5
#define MAX_PRE_LENGTH 200

static void show_preamble()
{
	char preamble[MAX_PRE_LINES][MAX_PRE_LENGTH];
	int count,def,i;

	/* print out any user preamble first */
	zvparm("preamble", preamble, &count, &def, MAX_PRE_LINES, MAX_PRE_LENGTH);
	if (!def && strlen(preamble[0])>0)
	{
		for (i=0;i<count;i++)
			outmessage(preamble[i]," ");
	}

}


void outmessage(char* msg,char* dummy)
{
	if (fd)
	{
		fprintf(fd,"%s\n",msg);
	}
	else zvmessage(msg,dummy);
}


