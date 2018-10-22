/*
 *  ibisstatfile.c
 *
 *    High-level interface to creating and reading
 *    STATS style IBIS-2 files.
 *
 *   Revision History:
 *   -----------------
 *
 *    6/13/95          New       NDR
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#define CLASS_NAME "CLASS_NAME"
#define NUMBER_OF_PIXELS "NUM_PIX"
#define NUMBER_OF_BANDS "NUM_BAND"
#define COVARIANCE "COVARIANCE"
#define MEAN "MEAN"

#define BAND_LIMIT 12
#define MAX_FILES 40

typedef struct {
    int ibis;
    int nbands;
    int nclasses;
    int classnamecol;
    int npixcol;
    int nbandcol;
    int meanrec;
    int covarrec;
} statrecord_t;

static statrecord_t stats[MAX_FILES];
static int first_time=1;

static void _InitializeStats()
{
	memset(stats,0,sizeof(stats));
	first_time=0;
}


static int _s_strcmp_nocase(str1,str2)
char *str1;
char *str2;
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}

/*
 * Open a STATS file for reading or writing. IBIS-level stuff hidden
 *  from user. Mode is 'READ' or 'WRITE'
 */
int ISTATFileOpen(unit, mode, nclasses, nbands, inst)
int unit; 
char *mode; 
int nclasses; 
int nbands; 
char *inst;
{
	int status;
	int ibis=0;
	int ncovar=0, ncol=0; 
	int create=0;
	int col,cur_col;
	int meancols[I2_MAX_COL];
	int covarcols[I2_MAX_COL];
	char *org = (char *)0;
	char *format = (char *)0, *cptr;
	
	if (first_time) _InitializeStats();
	
	if (nbands > BAND_LIMIT) return IBIS_COLUMN_LIMIT_EXCEEDED;
	
	switch (tolower(*mode))
	{
		case 'r': break;
		case 'w':
			org = IORG_ROW;
			create=1;
			ncovar = nbands*(nbands+1)/2;
			ncol = 3 + nbands + ncovar;
			if (ncol>I2_MAX_COL) return IBIS_COLUMN_LIMIT_EXCEEDED;
			
			/* set up column formatting */
			format = (char *)malloc((size_t)IFMT_SIZE*ncol);
			cptr = format+ IFMT_SIZE*3;
			cur_col = 4;
			for (col=0;col<nbands; col++,cptr+=IFMT_SIZE)
			{
				meancols[col] = cur_col++;
				strcpy(cptr,IFMT_DOUB);
			}
			for (col=0;col<ncovar; col++,cptr+=IFMT_SIZE)
			{
				covarcols[col] = cur_col++;
				strcpy(cptr,IFMT_REAL);
			}
			strcpy(format,"A8");   /* Class Name */
			strcpy(format+IFMT_SIZE,IFMT_FULL);  /* number of pix */
			strcpy(format+2*IFMT_SIZE,IFMT_FULL); /* number of bands */

			stats[unit].classnamecol = 1;
			stats[unit].npixcol = 2;
			stats[unit].nbandcol = 3;

			break;
		default:
			/* OWRITE mode not supported */
			return IBIS_INVALID_OPEN_MODE;
			break;
	}
	
	status = IBISFileOpen( unit, &ibis, mode, ncol,nclasses, format, org );
	if (status < 0) return status;
	
	if (create)
	{
		/* create the groups */
		
		status = ICLNewSTATISTICS(ibis,&stats[unit].classnamecol,
                     1,0,0,CLASS_NAME,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,&stats[unit].npixcol,
                     1,0,0,NUMBER_OF_PIXELS,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,&stats[unit].nbandcol,
                     1,0,0,NUMBER_OF_BANDS,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,meancols,nbands,0,0,MEAN,inst);
		if (status < 0) goto failure;
		status = ICLNewROOT(ibis,covarcols,ncovar,COVARIANCE);
		status = ICLNewSTATISTICS(ibis,covarcols,ncovar,
                   0,0,COVARIANCE,inst);
		if (status < 0) goto failure;

		/* Set the subfile type */
		status = IBISFileSet( ibis, IFILE_TYPE, "STATISTICS", 0 );
		if (status < 0) goto failure;
	}
	else
	{
		/* find the groups */
		ICLGetSTATISTICS(ibis,"$MyClass",0,CLASS_NAME,inst);
		status = IBISColumnFind(ibis,"any","$MyClass",
                            &stats[unit].classnamecol,1,1);
		if (status < 0) goto failure;
		ICLGetSTATISTICS(ibis,"$MyPix",0,NUMBER_OF_PIXELS,inst);
		status = IBISColumnFind(ibis,"any","$MyPix",
                            &stats[unit].npixcol,1,1);
		if (status < 0) goto failure;
		ICLGetSTATISTICS(ibis,"$MyBand",0,NUMBER_OF_BANDS,inst);
		status = IBISColumnFind(ibis,"any","$MyBand",
                            &stats[unit].nbandcol,1,1);
		if (status < 0) goto failure;

		ICLGetSTATISTICS(ibis,"$MyMean",0,MEAN,inst);
		nbands = IBISColumnFind(ibis,"any","$MyMean",
                       meancols,1,I2_MAX_COL);
		status = (nbands < 0) ? nbands : 1;
		if (status < 0) goto failure;
		stats[unit].nbands = nbands;

		ICLGetSTATISTICS(ibis,"$MyCovar",0,COVARIANCE,inst);
		ncovar = IBISColumnFind(ibis,"any","$MyCovar",
                      covarcols,1,I2_MAX_COL);
		status = (ncovar < 0) ? nbands : 1;
		if (status < 0) goto failure;
		IBISFileGet(ibis,IFILE_NR,&stats[unit].nclasses,1,0,0);
	}
	
	/* Set up records and formatting */
	status = IBISRecordOpen( ibis, &stats[unit].covarrec, 
		0, covarcols, ncovar,IFMT_REAL);
	if (status < 0) goto failure;
	status = IBISRecordOpen( ibis, &stats[unit].meanrec, 
		0, meancols, nbands,IFMT_REAL);
	if (status < 0) goto failure;
	
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, "A8", 
                stats[unit].classnamecol );
	if (status < 0) goto failure;
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, IFMT_FULL,
                stats[unit].npixcol );
	if (status < 0) goto failure;
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, IFMT_FULL, 
                stats[unit].nbandcol );
	if (status < 0) goto failure;

	/* store info in structure */
	stats[unit].ibis = ibis;
	
	return status;
failure:
	if (ibis) IBISFileClose(ibis,0);
	return status;
}

void ISTATSignal(unit, status, abendflag)
int unit;
int status;
int abendflag;
{
	int ibis = stats[unit].ibis;

	if (ibis) IBISSignal(ibis,status,abendflag);
	else IBISSignalU(unit,status,abendflag);
}

int ISTATFileClose(unit)
int unit;
{
	int ibis = stats[unit].ibis;
	stats[unit].ibis = 0;
	return IBISFileClose(ibis, 0);
}

void ISTATFileInfo(unit,nclasses,nbands,ibis)
int unit;
int *nclasses;
int *nbands;
int *ibis;
{
	if (nclasses) *nclasses = stats[unit].nclasses;
	if (nbands) *nbands = stats[unit].nbands;
	if (ibis) *ibis = stats[unit].ibis;
}

int ISTATRecordWrite( unit, recnum, name, npix, nbands, means, covar)
int unit;
int recnum;
char *name;
int npix;
int nbands;
float *means;
float *covar;
{
	int status=1;
	int npix1 = npix;
	int nband = nbands;
	int ibis = stats[unit].ibis;
	
	if (name)
	{
		status =  IBISColumnWrite(ibis,name,stats[unit].classnamecol,recnum,1);
		if (status < 0) return status;
	}
	status =  IBISColumnWrite(ibis,(char *)&npix1,stats[unit].npixcol,recnum,1);
	if (status < 0) return status;
	status =  IBISColumnWrite(ibis,(char *)&nband,stats[unit].nbandcol,recnum,1);
	if (status < 0) return status;
	
	if (means)
	{
		status = IBISRecordWrite(stats[unit].meanrec,(char *)means,recnum);
		if (status < 0) return status;
	}
	if (covar)
	{
		status = IBISRecordWrite(stats[unit].covarrec,(char *)covar,recnum);
		if (status < 0) return status;
	}
	
	return status;
}


int ISTATRecordRead( unit, recnum, name, npix, nbands, means, covar)
int unit;
int recnum;
char *name;
int *npix;
int *nbands;
float *means;
float *covar;
{
	int status=1;
	int ibis = stats[unit].ibis;
	
	if (name)
	{
		status =  IBISColumnRead(ibis,name,stats[unit].classnamecol,recnum,1);
		if (status < 0) return status;
	}
	if (npix)
	{
		status =  IBISColumnRead(ibis,(char *)npix,stats[unit].npixcol,recnum,1);
		if (status < 0) return status;
	}
	if (nbands)
	{
		status =  IBISColumnRead(ibis,(char *)nbands,stats[unit].nbandcol,recnum,1);
		if (status < 0) return status;
	}	
	if (means)
	{
		status = IBISRecordRead(stats[unit].meanrec,(char *)means,recnum);
		if (status < 0) return status;
	}
	if (covar)
	{
		status = IBISRecordRead(stats[unit].covarrec,(char *)covar,recnum);
		if (status < 0) return status;
	}
	
	return status;
}

/* return the record number of the named stat class */
int ISTATRecordFind(unit, classname)
int unit;
char *classname;
{
	int status=1;
	int row;
	int ibis = stats[unit].ibis;
	int nrow = stats[unit].nclasses;
	int colnum = stats[unit].classnamecol;
	char name[9];

	for (row=1; row<=nrow; row++)
	{
		status =  IBISColumnRead(ibis,name,colnum,row,1);
		if (status < 0) return 0;
		if (!_s_strcmp_nocase(name,classname)) break;
	}
	
	return (row <= nrow) ? row : 0;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/


void FTN_NAME2_(istat_file_open, ISTAT_FILE_OPEN) ( int *unit, char *mode,
			int *nclasses,
			int *nbands, char *inst, int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_mode[8],c_instance[33];
   char *instptr = (char *)0;

   zsfor2c(c_mode, 7, mode, &unit, 6, 2, 1, status);
   zsfor2c(c_instance, 32, inst, &unit, 6, 5, 2, status);
   if (c_instance[0]  && c_instance[0] != ' ') instptr = c_instance;

   *status = ISTATFileOpen(*unit,c_mode,*nclasses,*nbands,instptr);
}

void FTN_NAME2_(istat_signal, ISTAT_SIGNAL) ( unit,status,abendflag)
int *unit;
int *status;
int *abendflag;
{
	ISTATSignal( *unit, *status, *abendflag);
}

void FTN_NAME2_(istat_file_close, ISTAT_FILE_CLOSE) ( unit,status)
int *unit;
int *status;
{
	*status = ISTATFileClose( *unit );
}

void FTN_NAME2_(istat_file_info, ISTAT_FILE_INFO) (unit,nclasses,nbands,ibis) 
int *unit;
int *nclasses;
int *nbands;
int *ibis;
{
	ISTATFileInfo( *unit, nclasses, nbands, ibis );
}

void FTN_NAME2_(istat_record_write, ISTAT_RECORD_WRITE) (int *unit,
		int *recnum, char *name,
		int *npix, int *nbands, float *means, float *covar, int *status,
		ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   zsfor2c(c_name, 32, name, &unit, 8, 3, 1, status);
   
   *status = ISTATRecordWrite(*unit,*recnum,c_name,*npix,*nbands,means,covar);
}

void FTN_NAME2_(istat_record_read, ISTAT_RECORD_READ) (int *unit, int *recnum,
	char *name, int *npix,
	int *nbands, float *means, float *covar, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   *status = ISTATRecordRead(*unit,*recnum,c_name,npix,nbands,means,covar);

   zsc2for(c_name, 0, name, &unit, 8,3,1, status);
}

int FTN_NAME2_(istat_record_find, ISTAT_RECORD_FIND) (int *unit,
					char *classname, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   zsfor2c(c_name, 32, classname, &unit, 2, 2, 1, classname);
   return ISTATRecordFind(*unit,c_name);
}





