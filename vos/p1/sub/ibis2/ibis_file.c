#include "ibis.h"
#include <string.h>
#include <stdint.h>

/* XIBIS File Manipulation Routines */


static char *OptionList[]={
	ICLOSE_UKEEP,
	ICLOSE_UDELETE,
	(char *)0
};
typedef enum {
	CLOSE_UKEEP=1,
	CLOSE_UDELETE
} option_type;


static char *InitList[]={
	IINIT_ON,
	IINIT_OFF,
	(char *)0
};
typedef enum {
	INIT_ON=1,
	INIT_OFF
} init_type;

static char *ValueList[]={
	IFILE_NR,
	IFILE_NC,
	IFILE_ORG,
	IFILE_MODE,
	IFILE_TYPE,
	IFILE_FORMATS,
	IFILE_VUNIT,
	IFILE_GROUPS,
	IFILE_UNITS,
	IFILE_LOCALS,
	IFILE_FMT_DEFAULT,
	IFILE_HOST,
	IFILE_INTFMT,
	IFILE_REALFMT,
	IFILE_VERSION,
	IFILE_AUTO_INIT,
	IFILE_PIX_RECSIZE,
	IFILE_PIX_FORMAT,
	IFILE_PIX_HOST,
	IFILE_PIX_NL,
	IFILE_PIX_NS,
	(char *)0 /* end */
};

typedef enum {
	VALUE_NR=1,
	VALUE_NC,
	VALUE_ORG,
	VALUE_MODE,
	VALUE_TYPE2,
	VALUE_FORMATS,
	VALUE_VUNIT,
	VALUE_GROUPS,
	VALUE_UNITS,
	VALUE_LOCALS,
	VALUE_FMT_DEFLT,
	VALUE_HOST,
	VALUE_INTFMT,
	VALUE_REALFMT,
	VALUE_VERSION,
	VALUE_AUTO_INIT,
	VALUE_PIX_RECSIZE,
	VALUE_PIX_FORMAT,
	VALUE_PIX_HOST,
	VALUE_PIX_NL,
	VALUE_PIX_NS,
	VALUE_LAST=0 /* end */
} value_type;

static char *OrgList[]={
	IORG_ROW,
	IORG_COLUMN,
	(char *)0
};
typedef enum {
	VALUE_ROW=1,
	VALUE_COLUMN
} org_type;

static char *ModeList[]={
	IMODE_READ,
	IMODE_WRITE,
	IMODE_OWRITE,
	IMODE_UPDATE,
	(char *)0
};
typedef enum {
	MODE_READ=1,
	MODE_WRITE,
	MODE_OWRITE,
	MODE_UPDATE
} mode_type;

static char *VersionList[]={
	IVERSION_1,
	IVERSION_2,
	(char *)0
};
typedef enum {
	VERSION_1=1,
	VERSION_2
} version_type;


/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

static int _file_flush( ibis )
XIBIS *ibis;	
{
	int status=1;

	if (!(ibis->flags & FLAG_MODE_READ))
	{
		if (ibis->flags & FLAG_MOD_RECORDS)
		{
			int nr = ibis->nr;
			
			ibis->flags &= ~FLAG_MOD_RECORDS;
			status = _i_notify_records(ibis, NOTICE_NEED_ROWS, 1, nr);
			if (status != 1) return status;
		}
		
		if (ibis->flags & FLAG_MOD_LABELS)
		{
			status = ibis->labmethod->flush( ibis );
			ibis->flags &= ~FLAG_MOD_LABELS;
		}
	}

	return( status );
}


/**
 **   Look for "IBIS" property in file, flagging New IBIS.
 **   And if old ibis, check to see if this is a graphics file.
 **/

static int _check_for_old_ibis( ibis )
XIBIS *ibis;
{
	int status=1;
	int val=0;
	int old=1;

	if (ibis->flags & FLAG_MODE_WRITE )
		return 1; /* This has already been determined */

	/*
	 * We have to override the XVEACTION label action
	 * because we expect to encounter error statuses
	 */
	status = zvopen( ibis->unit, "op", "read","lab_act","  ", NULL );
	if (status != 1) return (status );
	
	/*
	 * Look for the IBIS "NC" property; if none, this is OLD 
	 */
	
	status = zlget( ibis->unit, "property",IFILE_NC, (char*) &val, "property","ibis",   NULL);
	if (val && status==1) old=0;

	status=1; /* reset */
	
	if (old) /* then we need to determine which IBIS-1 file this is */
	{
		char type[MAX_GRP_NAME+1];
		ibis->flags |= FLAG_FILE_OLD;
		
	    /* If the client manually set the ORG, don't change it */
	    /* Ref FR#85787   --NDR */

	    if (!(ibis->flags & MASK_ORG))
	    {
		/* determine if this is TABULAR or GRAPHICS */
	      status = zvget( ibis->unit, "type", type,  NULL);
		if (status != 1) goto end;
		
		ibis->flags &= ~MASK_ORG;
		if (!_i_strcmp_nocase(type,"tabular"))
			ibis->flags |= FLAG_ORG_COLUMN;
		else if (!_i_strcmp_nocase(type,"graph1"))
			ibis->flags |= FLAG_ORG_ROW;
                else
                {
                        /*
                         *  Some old IBIS files are IMAGE.
                         *  Fortunately there are other sanity
                         *  checks for old ibis later.
                         */
                        ibis->flags |= FLAG_ORG_COLUMN;
                }
	    }
		
	}
	else
		ibis->flags &= ~FLAG_FILE_OLD;

end:
	zvclose( ibis->unit, NULL);

	return (status);
}


int IBISFileOpen( vunit, xunit_id, mode, ncol, nrow, format, org )
int vunit;	/* input: VICAR file unit */
int *xunit_id;	/* output - XIBIS file unit */
char *mode;	/* input: "read" "write","owrite", or "update"   */
int ncol;	/* input: number of columns */
int nrow;	/* input: number of rows */
char *format;	/* input: column formats */
char *org;	/* input: "row" or "col" */
{
	int status;
	_ibis_current_module="IBISFileOpen";
	
	status = IBISFileUnit( vunit, xunit_id, mode, ncol, nrow, format, org );
	if (status!=1) return status;
	
	status = IBISFileUnitOpen( *xunit_id );
	
	return status;
}

int IBISFileUnit( vunit, xunit_id, mode, ncol, nrow, format, org )
int vunit;	/* input: VICAR file unit */
int *xunit_id;	/* output - XIBIS file unit */
char *mode;	/* input: "read" "write","owrite", or "update"   */
int ncol;	/* input: number of columns */
int nrow;	/* input: number of rows */
char *format;	/* input: column formats */
char *org;	/* input: "row" or "col" */
{
	int status=1;
	XIBIS *ibis;
	int ibis_id;
	List *ent;
#if POINTERS_64_BITS_OS
	int i;
#endif
	_ibis_current_module="IBISFileUnit";
	
	_init_ibis_globals();
	
	for (ent = x_ibis->next; ent; ent=ent->next)
	{
		if (((XIBIS*)ent->value)->unit == vunit)
			return ( IBIS_FILE_ALREADY_OPENED );
	}

	
	/* create the XIBIS structure */
	
	ibis = _i_new_ibis();
	if (!ibis) return ( IBIS_MEMORY_FAILURE );
	ibis->unit = vunit;
	ibis->default_fmt = default_format;
	ibis->pix_fmt = FMT_BYTE; /* pixel format */
	ibis->fmt_len = IFMT_SIZE;
	ibis->nc = ncol;
	ibis->nr = nrow;
	
	/* Determine IBIS ID, based on the pointer */

#if POINTERS_64_BITS_OS
	ibis_id = 0;
	for (i=1; i<MAX_NUM_IBIS_IDS; i++)
	{
		if (_ibis_id_table[i] == (XIBIS *)0)
		{
			_ibis_id_table[i] = ibis;
			ibis_id = i;
			break;
		}
	}
	if (ibis_id == 0)
	{
		status = IBIS_MEMORY_FAILURE;
		goto failure;
	}
#else
	ibis_id = (int)ibis;
#endif

	/* initialize the member values */
	
	if (!mode) return IBIS_INVALID_OPEN_MODE;
	status = IBISFileSet( ibis_id, IFILE_MODE, mode, 0 );
	if (status!=1) goto failure;

	/* If mode="read" and NCOL has been set, then the
	 * user wants this to be interpreted as GRAPHICS
	 * not as TABULAR, since standard tabular has NCOL
	 * built-in. FR#85787
	 */
	if (!org) org= ncol>0 ? IORG_ROW : IORG_COLUMN;
	status = IBISFileSet( ibis_id, IFILE_ORG, org, 0 );
	if (status != 1) goto failure;

	if (ibis->flags & FLAG_MODE_WRITE)
	{
		/* install the format values */
		status = IBISFileSet(ibis_id, IFILE_FORMATS, format, IFMT_SIZE);
		if (status != 1) goto failure;	
	}

	/* turn on file initialization flag (wont happen for READ)*/
	IBISFileSet( ibis_id, IFILE_AUTO_INIT, IINIT_ON, 0 );


	_ibis_current_module="IBISFileUnit";
	status=_check_for_old_ibis( ibis );
	if (status!=1) return (status);

	_i_insert_value( x_ibis, (list_value)ibis );

	*xunit_id = ibis_id;
	return status;
	
failure:

	_i_free_ibis( ibis );
	*xunit_id = 0;
	return (status );
}

int IBISFileUnitOpen(  ibis_id )
int ibis_id;	/*  XIBIS file unit */
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	_ibis_current_module="IBISFileUnitOpen";

	if (ibis->flags & FLAG_FILE_OPEN)
		return IBIS_FILE_ALREADY_OPENED;

	/* set up methods */

	if (ibis->flags & FLAG_FILE_OLD)
		_i_ibis1_install_methods( ibis );
	else
		_i_ibis2_install_methods( ibis );
		
	ibis->formats = _i_new_list((void(*)(int*))_i_free_group);
	if (!ibis->formats) return IBIS_MEMORY_FAILURE;

	status = ibis->labmethod->pre_open( ibis ) ;
	if (status!=1) goto failure;
	
	status = ibis->filemethod->open( ibis ) ;
	if (status!=1) goto failure;
	ibis->flags |= FLAG_FILE_OPEN;

	status = ibis->labmethod->post_open( ibis ) ;
	if (status!=1) goto failure;

	if ((ibis->flags&FLAG_MODE_WRITE)
		&& (ibis->flags&FLAG_AUTO_INIT))
	{
		status = ibis->filemethod->clear( ibis );
		if (status != 1) goto failure;
	}

	return (status);

failure:

	if  (ibis->flags & FLAG_FILE_OPEN)
	{
		zvclose( ibis->unit, NULL);
		ibis->flags &= ~FLAG_FILE_OPEN;
	}

	_i_purge_ibis( ibis );
	
	return (status );
	
}

int IBISFileClose( ibis_id, option )
int ibis_id;
char *option;
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int key;
	char *fmt = NULL;
	_ibis_current_module="IBISFileClose";
	
	CHECK_UNIT( ibis );

	if (!option) option=ICLOSE_UDELETE;
	key = _i_keymatch( option, OptionList );

	if (ibis->flags & MASK_MODS)
	{
		status = _file_flush( ibis );
		if (status != 1) return status;
	}

	if (key==CLOSE_UKEEP) /* save the formats before the purge */
	{
		fmt = (char *)malloc( ibis->nc * IFMT_SIZE );
		if (!fmt) return IBIS_MEMORY_FAILURE;
		IBISFileGet(ibis_id, IFILE_FORMATS, fmt, 1, ibis->nc,IFMT_SIZE);
	}

	if  (ibis->flags & FLAG_FILE_OPEN)
	{
		status = zvclose( ibis->unit, NULL);
		ibis->flags &= ~FLAG_FILE_OPEN;
	}

	_i_purge_ibis( ibis );

	switch (key)
	{
		case CLOSE_UDELETE:
			_i_delete_value( x_ibis, (list_value)ibis );
			/* Kill the ID table entry */
#if POINTERS_64_BITS_OS
			_ibis_id_table[ibis_id] = (XIBIS *)0;
#endif
			break;
		case CLOSE_UKEEP:
			/* restore the 'formats' list */
			ibis->fmt = fmt;
			ibis->fmt_len = IFMT_SIZE;
			break;
		default:
			return IBIS_INVALID_PARM;
			break;
	}	

	return (status);
}


/* public access to internal member values */

int IBISFileGet( ibis_id, name, value, sval, maxvals, length )
int ibis_id;
char *name;
void *value;
int sval;
int maxvals;  /* input: max vals to return */
int length;   /* input: length (including NULL) of value string */
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int  i;
	char *valStr;
	char *valptr=(char *)0;
	List *list=(List *)0;
	List *ent;
	XGROUP *group;
	int count=1,set=0;
	_ibis_current_module="IBISFileGet";

	CHECK_UNIT( ibis );

	if (!length && (maxvals > 1))
		return IBIS_LENGTH_REQUIRED;

	valStr = (char *)value;
		
	switch (_i_keymatch(name,ValueList))
	{
		case VALUE_FMT_DEFLT:  
			valptr = format_name[ ibis->default_fmt ];
			break;
			
		case VALUE_PIX_FORMAT:  
			valptr = format_name[ ibis->pix_fmt ];
			break;
			
		case VALUE_MODE:
			switch (ibis->flags & MASK_MODE)
			{
				case FLAG_MODE_READ:
					valptr = IMODE_READ;
					break;
				case FLAG_MODE_UPDATE:
					valptr = IMODE_UPDATE;
					break;
				case FLAG_MODE_WRITE:
					if (ibis->flags & FLAG_FILE_OLD)
						valptr = IMODE_OWRITE;
					else
						valptr = IMODE_WRITE;
					break;
				default:
					return IBIS_INVALID_OPEN_MODE;
					break;
			}
			break;
			
		case VALUE_FORMATS:
			if (maxvals==1)
			{
				valptr = format_name[ ibis->column[sval-1]->format ];
			}
			else if (maxvals>1)
			{
				count = maxvals;
				sval--;
				if (sval+maxvals > ibis->nc) count=ibis->nc-sval;
				for (i=0; i<count; i++,valStr+=length)
				{
					strncpy(valStr, format_name[ ibis->column[sval+i]->format ], length);
					valStr[length-1] = '\0';
				}
				count=i;
			}
			else return ibis->nc;

			break;

		case VALUE_LOCALS:
			list = ibis->locals; set=1;
			/* fall through */
		case VALUE_UNITS:
			if (!set) list = ibis->units; set=1;
			/* fall through */
		case VALUE_GROUPS: 
			if (!set) list = ibis->groups;
			if (!list) return 0;
			
			ent=list->next;
			
			if (maxvals>=1) /* return value or set up */
			{
				for (sval--;sval && ent; sval--,ent = ent->next);
				if (!ent) return 0;
				valptr = ((XGROUP *)ent->value)->name;
			}
			else return _i_count_list(list);
			
			if (maxvals>1)
			{
				valptr = (char *)0;
				for (i=0; ent && i<maxvals; ent = ent->next,valStr+=length,i++)
				{
					group = (XGROUP *)ent->value;
					strncpy(valStr, group->name,length );
					valStr[length-1]='\0';
				}
				count=i;
			}
			break;
			
		case VALUE_HOST: 
			valptr = ibis->hostfmt;
			break;

		case VALUE_INTFMT: 
			valptr = ibis->intfmt;
			break;

		case VALUE_REALFMT: 
			valptr = ibis->realfmt;
			break;

		case VALUE_NR: 
			*((int *)value) =  ibis->nr;
			return 1;
			break;
			
		case VALUE_NC:
			*((int *)value) =  ibis->nc;
			return 1;
			break;

		case VALUE_PIX_RECSIZE:
			*((int *)value) =  ibis->recsize;
			return 1;
			break;

		case VALUE_PIX_NL:
			*((int *)value) =  ibis->nl;
			return 1;
			break;

		case VALUE_PIX_NS:
			*((int *)value) =  ibis->ns;
			return 1;
			break;

		case VALUE_ORG:
			switch (ibis->flags & MASK_ORG)
			{
				case FLAG_ORG_COLUMN:
					valptr = IORG_COLUMN;
					break;
				case FLAG_ORG_ROW:
					valptr = IORG_ROW;
					break;
			}
			break;
			
		case VALUE_TYPE2:
			valptr = ibis->type ;
			break;
						
		case VALUE_VUNIT: /* vicar unit -- NOT UNIT ! */
			*((int *)value) =  ibis->unit;
			return 1;
			break;
			
		case VALUE_VERSION: 
			valptr = (ibis->flags&FLAG_FILE_OLD) ? 
				IVERSION_1 : IVERSION_2;	
			break;
			
		case VALUE_AUTO_INIT: 
			valptr = (ibis->flags&FLAG_AUTO_INIT) ? 
				IINIT_ON : IINIT_OFF;	
			break;
			
		default:
			return IBIS_INVALID_PARM;
			break;
	}

	if (valptr)  /* handle single strings */
	{
		count=1;
		if (length)
		{
			strncpy( valStr, valptr,length);
			valStr[length-1] = '\0';
		}
		else strcpy( valStr, valptr );
	}

	return count;
}

int IBISFileSet( ibis_id, name, value, length )
int ibis_id;
char *name;
char *value;
int length; /* length of multvalued string */
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int changed=0;
	int is_open;
	int size;
	int key;
	int code;
	_ibis_current_module="IBISFileSet";
	
	is_open = ibis->flags & FLAG_FILE_OPEN;

	switch (_i_keymatch(name,ValueList))
	{

		case VALUE_NC:
			if (ibis->nc != (int)(uintptr_t) value) /* 64-bit okay rgd 1-99 */
			{
				int nc = (int) (uintptr_t) value; /* 64-bit okay rgd 1-99 */
				if (is_open)
				{
					CHECK_WRITE( ibis );
					if (nc > MAX_COL)
						return IBIS_COLUMN_LIMIT_EXCEEDED;
					if (nc>ibis->nc)
						status=IBISColumnNew(ibis_id,0,nc-ibis->nc,0);
					else
						status=IBISColumnDelete(ibis_id,nc+1,ibis->nc-nc);
				}
				else ibis->nc = nc;
		
				if (status==1) changed = 1;
			}
			break;


		case VALUE_NR:
			if (ibis->nr != (int) (uintptr_t) value) /* 64-bit okay rgd 1-99 */
			{
				int nr = (int) (uintptr_t) value; /* 64-bit okay rgd 1-99 */
				if (is_open)
				{
					CHECK_WRITE( ibis );
					if (nr>ibis->nr)
						status=IBISRowNew(ibis_id,0,nr-ibis->nr);
					else
						status=IBISRowDelete(ibis_id,nr+1,ibis->nr-nr);
				}

				ibis->nr = nr;
				changed = 1;
			}
			break;

		case VALUE_FMT_DEFLT: 

			if (value) code = _i_IBISFormatCode(value) ;
			else code=FMT_REAL;
			if (code<FMT_BYTE)
				return IBIS_INVALID_FORMAT;
			if (ibis->default_fmt != code)
			{
				if (ibis->flags & FLAG_FILE_OLD)
				{
				   size = ibis->format_size[code];	
				   if (size>4) return IBIS_FILE_OLD_IBIS;
				}
				ibis->default_fmt = code;
				changed=1;
			}
			break;

		case VALUE_FORMATS:
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			CHECK_WRITE( ibis );
			
			if (value && length < 1) return IBIS_LENGTH_REQUIRED;
			
			if (ibis->fmt) free( ibis->fmt );
			ibis->fmt = _i_mem_dup( (char *)value, ibis->nc*length);
			if (value && !ibis->fmt) return IBIS_MEMORY_FAILURE;
			if (value && length) ibis->fmt_len = length;
			changed=1;
			break;

		case VALUE_ORG:
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			if (value) key=_i_keymatch( (char *)value, OrgList);
			else key=VALUE_COLUMN;
			switch (key)
			{
				case VALUE_ROW:
					ibis->flags &= ~MASK_ORG;
					ibis->flags |= FLAG_ORG_ROW;
					break;
				case VALUE_COLUMN:
					ibis->flags &= ~MASK_ORG;
					ibis->flags |= FLAG_ORG_COLUMN;
					break;
				default:
					return IBIS_INVALID_PARM;
					break;
			}
			
			changed=1;
			break;
			break;

		case VALUE_MODE:
			if (is_open) return IBIS_FILE_ALREADY_OPENED;				
			if (value) key=_i_keymatch( (char *)value, ModeList);
			else key=MODE_READ;
			switch (key)
			{
				case MODE_READ:
					ibis->flags &= ~MASK_MODE;
					ibis->flags |= FLAG_MODE_READ;
					break;
				case MODE_OWRITE: /* OLD Write */
					ibis->flags &= ~MASK_MODE;
					ibis->flags |= FLAG_FILE_OLD;
					ibis->flags |= FLAG_MODE_WRITE;
					break;
				case MODE_WRITE:
					ibis->flags &= ~MASK_MODE;
					ibis->flags |= FLAG_MODE_WRITE;
					ibis->flags &= ~FLAG_FILE_OLD;
					break;
				case MODE_UPDATE:
					ibis->flags &= ~MASK_MODE;
					ibis->flags |= FLAG_MODE_UPDATE;
					break;
				default: 
					return IBIS_INVALID_OPEN_MODE;
					break;
			}
			changed=1;
			break;
			
		case VALUE_VERSION:
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			CHECK_WRITE( ibis );
			if (value) key=_i_keymatch( (char *)value, VersionList);
			else key=VERSION_2;
			switch (key)
			{
				case VERSION_1:
					ibis->flags |= FLAG_FILE_OLD;
					break;
				case VERSION_2:
					ibis->flags &= ~FLAG_FILE_OLD;
					break;
				default:
					return IBIS_INVALID_PARM;
					break;
			}
			changed=1;
			break;

		case VALUE_AUTO_INIT:
			CHECK_WRITE( ibis );
			if (value) key=_i_keymatch( (char *)value, InitList);
			else key=INIT_ON;
			switch (key)
			{
				case INIT_ON:
					ibis->flags |= FLAG_AUTO_INIT;
					break;
				case INIT_OFF:
					ibis->flags &= ~FLAG_AUTO_INIT;
					break;
				default:
					return IBIS_INVALID_PARM;
					break;
			}
			break;

		case VALUE_TYPE2: 
			CHECK_WRITE( ibis );
			strncpy(ibis->type, (char *)value, MAX_GRP_NAME );
			ibis->type[MAX_GRP_NAME]='\0'; /* just in case */
			changed=1;
			break;
			
		case VALUE_HOST:
			if (!_i_strcmp_nocase( (char *)value, "native") ||
				_i_strcmp_nocase( (char *)value, "local") )
				strcpy( ibis->hostfmt, NATIVE_HOST_LABEL );	
			else strncpy(ibis->hostfmt, (char *)value,MAX_GRP_NAME );
			ibis->hostfmt[MAX_GRP_NAME]='\0'; /* just in case */
			status = _i_trans_init( ibis ); /* set up translation buffers */
			if (status!=1) return status;
			changed=1;
			break;

		case VALUE_INTFMT: 
			if (!_i_strcmp_nocase( (char *)value, "native") ||
				!_i_strcmp_nocase( (char *)value, "local") )
				strcpy( ibis->intfmt, NATIVE_INTFMT );	
			else strncpy(ibis->intfmt, (char *)value,MAX_GRP_NAME );
			ibis->intfmt[MAX_GRP_NAME]='\0'; /* just in case */
			_i_trans_reset( ibis );
			changed=1;
			break;

		case VALUE_REALFMT: 
			if (!_i_strcmp_nocase( (char *)value, "native") ||
				!_i_strcmp_nocase( (char *)value, "local") )
				strcpy( ibis->realfmt, NATIVE_REALFMT );	
			else strncpy(ibis->realfmt, (char *)value,MAX_GRP_NAME );
			ibis->realfmt[MAX_GRP_NAME]='\0'; /* just in case */
			_i_trans_reset( ibis );
			changed=1;
			break;


		case VALUE_PIX_NS:
			if (ibis->flags & FLAG_FILE_OLD) return IBIS_FILE_OLD_IBIS;
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			CHECK_WRITE( ibis );
			ibis->ns = (int) (uintptr_t) value;	/* 64-bit okay rgd 1-99 */
			_i_reset_recsize( ibis );
			break;

		case VALUE_PIX_HOST:
			if (ibis->flags & FLAG_FILE_OLD) return IBIS_FILE_OLD_IBIS;
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			CHECK_WRITE( ibis );
			if (!_i_strcmp_nocase( (char *)value, "native") ||
				!_i_strcmp_nocase( (char *)value, "local") )
				strcpy( ibis->pix_host, NATIVE_HOST_LABEL );	
			else strncpy(ibis->pix_host, (char *)value,MAX_GRP_NAME );
			ibis->pix_host[MAX_GRP_NAME]='\0'; /* just in case */
			_i_make_uppercase(ibis->pix_host);
			status = _i_reset_recsize( ibis );
			if (status!=1) return status;
			break;

		case VALUE_PIX_FORMAT: 
			if (ibis->flags & FLAG_FILE_OLD) return IBIS_FILE_OLD_IBIS;
			if (is_open) return IBIS_FILE_ALREADY_OPENED;
			CHECK_WRITE( ibis );
			if (!ibis->ns) return IBIS_MUST_SET_NS_FIRST;
			if (value) code = _i_IBISFormatCode(value) ;
			else code=FMT_BYTE;
			if (ibis->pix_fmt != code)
			{
				if (code >= FMT_ASCII) return IBIS_INVALID_FORMAT;
				ibis->pix_fmt = code;
				changed=1;
			}
			status = _i_reset_recsize( ibis );
			if (status!=1) return status;
			break;
			
		default:
			status = IBIS_INVALID_PARM;
			break;
	}

	if (changed && is_open) ibis->flags |= FLAG_MOD_LABELS;	
	return (status);
}

int IBISFileClear(  ibis_id )
int ibis_id;	/*  XIBIS file unit */
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	_ibis_current_module="IBISFileClear";

	CHECK_WRITE( ibis );

	if (ibis->record) 
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS,1,ibis->nr);
		if (status!=1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS,1,ibis->nr);
		if (status!=1) return status;
	}

	status = ibis->filemethod->clear( ibis );
	if (status != 1) return status;
	
	return (status );	
}



/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_file_open, IBIS_FILE_OPEN) ( int *vunit, int *xunit_id,
	char *mode, int *ncol,
	int *nrow, char *format, char *org, int *status, ZFORSTR_PARAM )
#if 0
int *vunit;		/* input: VICAR file unit */
int *xunit_id;		/* output - XIBIS file unit */
char *mode;		/* input: "r" "w" or "u"   */
int *ncol;		/* input: number of columns */
int *nrow;		/* input: number of rows */
char *format;		/* input: column formats */
char *org;		/* input: "row" or "col" */
int *status;		/* output: error status */
#endif
{
   ZFORSTR_BLOCK
   /****** Start: identical to fortran bridge to IBISFileUnit *******/
   char c_mode[8],c_org[8], *orgptr=(char *)0;
   char *c_format=(char *)0;
   char fmtval[8];
   int maxlen=0;
   _ibis_current_module="IBISFileOpen";
  
   zsfor2c(c_mode, 7, mode, &vunit, 8, 3, 1, status);
   zsfor2c(fmtval, 7, format, &vunit, 8, 6, 2, status);
   zsfor2c(c_org, 7, org, &vunit, 8, 7, 3, status);
   
   if (c_org[0])
   	orgptr=c_org;

   if (fmtval[0])
   {
   	zsfor2c_array( &c_format, &maxlen, *ncol,
   			 format, &vunit, 8, 6, 2, status);
    	if (!c_format) 
   	{
   		*status = IBIS_MEMORY_FAILURE;
   		return;
   	}
   	
   }
   
   /* create the unit without the format */
   
   *status = IBISFileUnit( *vunit, xunit_id, c_mode, *ncol,
   				 *nrow, (char *)0, orgptr );
   
   if (*status != 1) return;

   /* install the format, overriding the standard format length */
   
   *status = IBISFileSet(  *xunit_id, IFILE_FORMATS, c_format, maxlen );

   if (c_format) free(c_format);

   /****** End: identical to fortran bridge to IBISFileUnit *******/

   /* now open it up */
   
   *status = IBISFileUnitOpen( *xunit_id );
   
   return;
}


void FTN_NAME2_(ibis_file_unit, IBIS_FILE_UNIT) ( int *vunit, int *xunit_id,
	char *mode, int *ncol, int *nrow, char *format, char *org,
	int *status, ZFORSTR_PARAM )
#if 0
int *vunit;	/* input: VICAR file unit */
int *xunit_id;	/* output - XIBIS file unit */
char *mode;	/* input: "read" "write" "owrite" or "update"   */
int *ncol;	/* input: number of columns */
int *nrow;	/* input: number of rows */
char *format;	/* input: column format strings */
char *org;	/* input: "row" or "col" */
int *status;	/* output: error status */
#endif
{
   ZFORSTR_BLOCK
   char c_mode[8],c_org[8], *orgptr=(char *)0;
   char *c_format=(char *)0;
   char fmtval[8];
   int maxlen=0;
   _ibis_current_module="IBISFileUnit";
  
   zsfor2c(c_mode, 7, mode, &vunit, 8, 3, 1, status);
   zsfor2c(fmtval, 7, format, &vunit, 8, 6, 2, status);
   zsfor2c(c_org, 7, org, &vunit, 8, 7, 3, status);
   
   if (c_org[0])
   	orgptr=c_org;

   if (fmtval[0])
   {
   	zsfor2c_array( &c_format, &maxlen, *ncol,
   			  format, &vunit, 8, 6, 2, status);
    	if (!c_format) 
   	{
   		*status = IBIS_MEMORY_FAILURE;
   		return;
   	}
   	
   }
   
   /* create the unit without the format */
   
   *status = IBISFileUnit( *vunit, xunit_id, c_mode, *ncol,
   				 *nrow, (char *)0, orgptr );
   
   if (*status != 1) return;

   /* install the format */
   
   *status = IBISFileSet(  *xunit_id, IFILE_FORMATS, c_format, maxlen );

   if (c_format) free(c_format);
   return;
}

void FTN_NAME2_(ibis_file_unit_open, IBIS_FILE_UNIT_OPEN) ( int *xunit_id,
		int *status )
{
   *status = IBISFileUnitOpen( *xunit_id );
   return;
}

void FTN_NAME2_(ibis_file_close, IBIS_FILE_CLOSE) ( int *xunit_id,
		char *close_option, int *status, ZFORSTR_PARAM)
#if 0
int *xunit_id;	/* output - XIBIS file unit */
#endif
{
   ZFORSTR_BLOCK
   char c_option[21], *optptr=(char *)0;
  
   zsfor2c(c_option, 20, close_option, &xunit_id, 3, 2,1, status);
   if (c_option[0]) optptr = c_option;

   *status = IBISFileClose( *xunit_id, optptr );
   return;
}

#define MAX_STR_SIZE 256

int FTN_NAME2_(ibis_file_get, IBIS_FILE_GET) ( int *ibis_id, char *name,
		void *value, int *sval, int *maxval, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_name[MAX_VALUE_NAME+1];
   char *valptr=(char *)value;
   char *tempbuf=(char *)0;
   int c_length=0;
   int length=0;
   int count=1;
   int numval;
   int valuekey;
   _ibis_current_module="IBISFileGet";
 
   zsfor2c(c_name, MAX_VALUE_NAME, name, &ibis_id, 5, 2, 1, maxval); 
   valuekey=_i_keymatch(c_name,ValueList);  
   
   /* set up value counts for string arrays */
   
   numval = *maxval;
   
   /* Set up temp buffers, for string array conversion */

   if (numval > 0) switch (valuekey)
   {
	case VALUE_FMT_DEFLT:  
	case VALUE_PIX_FORMAT:  
	case VALUE_MODE:
	case VALUE_FORMATS:
	case VALUE_LOCALS:
	case VALUE_UNITS:
	case VALUE_GROUPS: 
	case VALUE_HOST: 
	case VALUE_INTFMT: 
	case VALUE_REALFMT: 
	case VALUE_ORG:
	case VALUE_TYPE2:
	case VALUE_VERSION: 
	case VALUE_AUTO_INIT: 
   		zsfor2len(c_length, value, &ibis_id, 5, 3, 2, maxval);
		c_length += 1;
		tempbuf = (char *)calloc( 1L, (long)numval*c_length);
		if (!tempbuf) return IBIS_MEMORY_FAILURE;
		valptr = tempbuf;
   		break;
   }

   
   count = IBISFileGet( *ibis_id, c_name, valptr, *sval, numval, c_length);
   
   if (tempbuf) /* convert the string array */
   {
    	if (count > 0)
   	{
	  	if (numval > 1) {
		   zsc2for_array(valptr, c_length, numval, value, &length, &ibis_id, 5,3,2, maxval);
		}
		else if (numval==1) {
		   zsc2for(valptr, 0, value, &ibis_id, 5,3,2, maxval);
		}
	}
    	free(tempbuf);
   }
   
   return count;
}

void FTN_NAME2_(ibis_file_set, IBIS_FILE_SET) ( int *ibis_id, char *name,
		void *value, int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   XIBIS *ibis=IBIS_FETCH_ID(*ibis_id);
   char c_name[MAX_VALUE_NAME+1];
   char *valptr=(char *)0;
   char *c_value=(char *)0;
   char val[MAX_STR_SIZE+1];
   int length=0;
   int key;
   int count=1;
   int multi=0;
   _ibis_current_module="IBISFileSet";
   
   zsfor2c(c_name, MAX_VALUE_NAME, name, &ibis_id, 4, 2, 1, status);

   /* Convert string-values to C */
   key = _i_keymatch(c_name,ValueList);
   if (key==VALUE_FORMATS) count=(ibis)->nc;
 
   switch (key)
   {
	case VALUE_FORMATS:
		multi=1;
		/* fall through */
	case VALUE_FMT_DEFLT: 
	case VALUE_PIX_FORMAT: 
	case VALUE_ORG:
	case VALUE_MODE:
	case VALUE_VERSION:
	case VALUE_AUTO_INIT:
	case VALUE_TYPE2: 
	case VALUE_HOST:
	case VALUE_INTFMT: 
	case VALUE_REALFMT:
    		zsfor2c(val, MAX_STR_SIZE, value, &ibis_id, 4,3,2, status);
    		if (val[0]) valptr=val;
		if (multi && val[0])
    		{
	    		zsfor2c_array(&c_value, &length, count, value, &ibis_id, 4,3,2, status);
	    		if (!c_value)
	    		{
	    			*status = IBIS_MEMORY_FAILURE;
	    			return;
	    		}
	   		valptr = c_value;
    		}
   		break;
   		
	case VALUE_NR:
	case VALUE_NC:
		valptr = (char *)((uintptr_t) *(int *)value); /* pass by value */ /* 64-bit okay(?) rgd 1-99 */
		break;
		
	default:
		*status = IBIS_INVALID_PARM;
		return;
		break;
   }

   *status = IBISFileSet( *ibis_id, c_name, valptr, length );
   
   if (c_value) free(c_value);
  
   return;
}

void FTN_NAME2_(ibis_file_clear, IBIS_FILE_CLEAR) ( int *xunit_id, int *status)
{
   *status = IBISFileClear( *xunit_id );
   return;
}


