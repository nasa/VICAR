#include "ibis.h"
#include <string.h>
/*
 *  IBIS Column I/O routines.
 */

typedef enum {
	COLVALUE_U_FORMAT=1,
	COLVALUE_U_SIZE,
	COLVALUE_FORMAT,
	COLVALUE_SIZE
} colvalue_type;

static char *ValueList[]={
	ICOLUMN_U_FORMAT,
	ICOLUMN_U_SIZE,
	ICOLUMN_FORMAT,
	ICOLUMN_SIZE,
	(char *)0
};

typedef enum {
	TYPE_FORMAT=1,
	TYPE_UNIT,
	TYPE_GROUP,
	TYPE_LOCAL,
	TYPE_ANY
} tvalue_type;

static char *TypeList[]={
	ITYPE_FORMAT,
	ITYPE_UNIT,
	ITYPE_GROUP,
	ITYPE_LOCAL,
	ITYPE_ANY,
	(char *)0
};

/* Allows private routines to create more (temporary) columns */
static int _override=0;

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/


#define TBUF_SIZE 2048
static int _copy_raw_column(XIBIS *ibis,int incolumn,int outcolumn)
{
	int status;
	XCOL *incol=ibis->column[incolumn];
	XCOL *outcol=ibis->column[outcolumn];
	char buffer[TBUF_SIZE];
	int nr=ibis->nr;
	int insize=ibis->format_size[incol->format];
	int outsize=ibis->format_size[outcol->format];
	int inrow=1,numin, outrow ,numout, outinc, offset=0, adj;
	
	/* 
	 * compute the number of outcolumn rows that will fit into
	 * buffer, allowing for one extra incolumn data row on each
	 * side of buffer.
	 */
	
	outinc = ((TBUF_SIZE-2*insize)/outsize); /* how many will fit */

	for (outrow=0; outrow<nr; outrow += outinc)
	{
		/* compute the number to put out this time */
		numout = outrow+outinc>nr ? (nr-outrow) : outinc;
		
		/* 
		 * compute the number of in rows to read in 
		 * in order to *cover* the output columns. Also
		 * determine the byte-adjustment to get to the
		 * start of the output data.
		 */
		
		inrow = offset / insize; 
		adj   = offset % insize;
		offset += numout*outinc;
		numin = HOW_MANY(offset, insize) - inrow;
		if (inrow+numin>nr) 
		{
			numin = nr-inrow;
			memset(buffer,0,(long)TBUF_SIZE); /* clear rest of data */
		}
		
		/* read 'em in... */
		status = ibis->colmethod->dofile( ibis, zvread,buffer,
				incolumn,inrow+1,numin,1);
		if (status!=1) return status;
		
		/* ...and write 'em out */
		status = ibis->colmethod->dofile( ibis, zvwrit,buffer+adj,
				outcolumn,outrow+1,numout,1);
		if (status!=1) return status;
		
	}
	return 1;
}


int IBISColumnGet(int ibis_id, char* name, void *value, int column )
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	XCOL *col;
	char *valptr=(char *)0;
	trans_type *trans;
	int format;
	_ibis_current_module="IBISColumnGet";
	
	CHECK_COLUMN( ibis, column )
	col = ibis->column[ column -1 ];

	switch (_i_keymatch(name,ValueList))
	{	
		case COLVALUE_FORMAT: 
			valptr = format_name[ col->format ];
			break;
		case COLVALUE_SIZE: 
			return _i_IBISFormatSize( ibis, col->format, value );
			break;
		case COLVALUE_U_SIZE: 
			trans = TRANS_USED( ibis, col);
			format = trans->infmt;
			if (format==FMT_ASCII || format==FMT_NONE) format=col->format;
			*(int *)value = format_size[ format ];
			return 1;
			break;
		case COLVALUE_U_FORMAT:
			trans = TRANS_USED( ibis, col);
			format = trans->infmt;
			if (format==FMT_ASCII) format=col->format;
			valptr = format_name[ format ];
			break;
		default:  
			return (IBIS_COLUMN_PARM_INVALID);
			break;
	}
	
	if (valptr) strcpy( (char *)value, valptr);
	
	return status;
}




int IBISColumnSet(int ibis_id, char* name, void* value, int column )
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int format;
	int osize,nsize;
	int changed=0;
	int old,oldcol;
	XCOL *col;
	trans_type *trans;
	_ibis_current_module="IBISColumnSet";
	
	CHECK_COLUMN( ibis, column )
	col = ibis->column[ column -1 ];
	old=(ibis->flags & FLAG_FILE_OLD);
	
	switch (_i_keymatch(name,ValueList))
	{	
		case COLVALUE_FORMAT:
		 
			format = _i_IBISFormatCode((char *)value);
		 	if (format == col->format) return 1;
		 	if (format < FMT_BYTE ) return IBIS_INVALID_FORMAT;
		 	
		 	CHECK_LOCK(ibis, column)
		 	
		 	osize =  ibis->format_size[col->format];
		 	nsize =  ibis->format_size[format];
		 	
			if (nsize<=osize || (nsize<=4 && old))
			{
				_i_detach_col_from_format( ibis, column-1 );
				_i_attach_format_to_column(ibis,format,column-1);
			}
			else /* need to relocate (This will fail for old IBIS ) */
			{
				if (old) return IBIS_FILE_OLD_IBIS;
					
				/* Append a new (temp)column after <column> */
				_override=1; /* allow extra column */
				status=IBISColumnNew(ibis_id, column, 1, value);
				if (status!=1) return status;
				
				/* copy the raw data */
				oldcol=column-1;
				_copy_raw_column(ibis,oldcol,column);
				
				/* copy the group memberships */
				status=IBISGroupTransfer(ibis_id,ibis_id,ITYPE_ANY,
					&oldcol,&column,1);
				
				/* kill off the old one */
				status=IBISColumnDelete( ibis_id, column, 1 );
				if (status!=1) return status;
			}
			changed = 1;
			break;
						
		case COLVALUE_U_FORMAT:
			if (!value) /* reset to default */
			{
				if (!col->trans) return 1; /* alread def */
				free( col->trans );
				col->trans = (trans_type *)0;
				return 1;
			}
			else	/* explicitly defined fmt */
			{
				int buffmt = _i_IBISFormatCode( value );
				
				trans = TRANS_USED( ibis, col);
				if (buffmt == trans->infmt) return 1;
				if (col->locked) return IBIS_COLUMN_LOCKED;

				if (col->trans) free(col->trans);
				trans = (trans_type*)0;
				col->trans = trans;

				if ( (buffmt!=col->format) ||
				    (!old && buffmt > FMT_ASCII) ||
				    (old && buffmt>=FMT_ASCII))
				{
					trans = (trans_type *)calloc( 1L, sizeof( trans_type ));
					if (!trans) return IBIS_MEMORY_FAILURE;
					status = _i_set_trans( ibis, trans, buffmt, col->format );
					if (status!=1) 
					{
						free(trans);
						return status;
					}
					col->trans = trans;
				}
			}
			break;

		default: 
			status=IBIS_COLUMN_PARM_INVALID;
			break;
	}
	
	if (changed && status==1 && !(ibis->flags & FLAG_MODE_READ))	
		ibis->flags |= FLAG_MOD_LABELS;

	return( status );	
}

int IBISColumnRead(int ibis_id,char* buffer,int column,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int translate=0;
	trans_type *trans;
	char *filebuf;
	int size;
	XCOL*col;
	_ibis_current_module="IBISColumnRead";

	CHECK_COLUMN( ibis, column);
	
	if (IS_LOCKED( ibis, column )) 
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, srow+nrows-1);
		if (status!=1) return status;
	}

	col = ibis->column[ column -1 ];

	column--;

	if (srow<=0) srow=1;
	if (nrows<=0) nrows=ibis->nr + 1 - srow;
	if (srow + nrows -1 > ibis->nr )
		return IBIS_LAST_ROW;

	/*
	 *  Determine if translation will be performed
	 */
	translate = NEED_TRANS( ibis, col );

	if (translate)
	{
		/*
		 *  Set up temporary buffer for translating file data
		 *  the size of the data is a function of the size of
		 *  the corresponding pixel in the file's host format.
		 */
	
		trans = TRANS_USED( ibis, col);
		size = ibis->format_size[col->format];
		filebuf = (char *)calloc(1L, size * nrows );
		if (!filebuf) return IBIS_MEMORY_FAILURE;
	}
	else filebuf = buffer;

	/*
	 * read and translate data, if needed.
	 */
	 
	status = ibis->colmethod->dofile( ibis, zvread,filebuf,column,srow,nrows,1) ;
	if (translate) _i_trans_buf_to_local( trans, col, filebuf, buffer, nrows );

	if (translate) free( filebuf );

	return( status );
}

int IBISColumnWrite(int ibis_id,char *buffer,int column,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int translate=0;
	trans_type *trans;
	char *filebuf;
	int size;
	XCOL*col;
	_ibis_current_module="IBISColumnWrite";

	CHECK_COLUMN( ibis, column);
	CHECK_WRITE( ibis );

	if (IS_LOCKED( ibis, column )) 
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, srow+nrows-1);
		if (status!=1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, srow+nrows-1);
		if (status!=1) return status;
	}

	col = ibis->column[ column -1 ];
	column--;

	if (srow<=0) srow=1;
	if (nrows<=0) nrows=ibis->nr + 1 - srow;
	if (srow + nrows -1 > ibis->nr )
		return IBIS_LAST_ROW;

	/*
	 *  Determine if translation will be performed
	 */
	translate = NEED_TRANS( ibis, col );


	if (translate)
	{
		/*
		 *  Set up temporary buffer for translating file data
		 */
		trans = TRANS_USED( ibis, col);
		size = ibis->format_size[col->format];
		filebuf = (char *)calloc(1L, size * nrows );
		if (!filebuf) return IBIS_MEMORY_FAILURE;	
	}
	else filebuf = buffer;
	
	/*
	 * This should actually be a loop if someone tries to read all at once
	 * a HUGE column, but hopefully they are using a buffer smaller
	 * than the memory limit; consequently, our buffer will be small.
	 */
	 
	if (translate) _i_trans_buf_from_local(trans, col, buffer, filebuf, nrows );
	status = ibis->colmethod->dofile( ibis, zvwrit,filebuf,column,srow,nrows,1) ;

	if (translate) free( filebuf );

	return( status );
}

/*
 *  Clear some columns
 */

int IBISColumnClear(int ibis_id,int column,int ncols)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status = 0;
	int col;
	char *bufptr;
	_ibis_current_module="IBISColumnClear";

	CHECK_COLUMN( ibis, column);
	CHECK_COLUMN( ibis, column+ncols-1);
	CHECK_WRITE( ibis );

	if (IS_LOCKED( ibis, column )) 
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS,1,ibis->nr);
		if (status!=1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS,1,ibis->nr);
		if (status!=1) return status;
	}
	
	column--;
	
	bufptr = (char *)calloc(1L, ibis->blocksize);
	if (!bufptr) return IBIS_MEMORY_FAILURE;

	for (col=0;col<ncols;col++)
	{
	    status = ibis->colmethod->dofile( ibis, zvwrit,
	    		 bufptr, column+col, 1, ibis->nr,  0);
	    if (status!=1) break;
	}

	free(bufptr);
	return status;
}

 
int IBISColumnNew(int ibis_id,int column,int ncols,char *fmt)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int col,lastcol;
	int size;
	int format;
	int off;
	_ibis_current_module="IBISColumnNew";

	if (column > ibis->nc || column <= 0) column=ibis->nc+1;
	column--;
	
	CHECK_WRITE( ibis );
	
	if (column+ncols > MAX_COL && !_override)
		return IBIS_COLUMN_LIMIT_EXCEEDED;
	_override=0;
	
	/* 
	 * We have to initialize the Gap Manager now
	 * before the new column pointers are inserted,
	 * which would confuse the manager.
	 */
	if (!ibis->gaps && !(ibis->flags&FLAG_FILE_OLD))
		 _i_find_space( ibis, &off, 1);

	lastcol=column + ncols;	

	if (!fmt) fmt=format_name[ ibis->default_fmt ];
	format = _i_IBISFormatCode( fmt );
	if (format < FMT_BYTE) return IBIS_INVALID_FORMAT;

	/* create column pointers */

	status = _i_insert_column_pointers( ibis, column, ncols );
	if (status!=1) return status;	

	/* place columns in format group */
	
	for (col=column; col<lastcol; col++)
		_i_attach_format_to_column(ibis, format, col);
	
	/* Let the file-dependent handlers deal with allocation */

	size = ibis->format_size[format];
	status = ibis->colmethod->new(ibis,column,ncols,size);
	if (status!=1) return (status);
	
	/* update file information */
	
	ibis->nc += ncols;
	ibis->flags |= FLAG_MOD_LABELS;

	if (ibis->flags & FLAG_AUTO_INIT)
		status = IBISColumnClear(ibis_id,column+1,ncols);

	return( status );
}

int IBISColumnDelete(int ibis_id,int column,int ncols)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status;
	int lastcol=column+ncols-1;
	int col;
	XCOL *colp;
	_ibis_current_module="IBISColumnDelete";
	
	CHECK_COLUMN( ibis, column );
	CHECK_COLUMN( ibis, lastcol );
	CHECK_WRITE( ibis );

	for (col=lastcol; col>=column; col--)
		CHECK_LOCK( ibis, lastcol )

	column--;
	
	/*
	 * Delete the actual file column first
	 */

	status = ibis->colmethod->delete(ibis, column, ncols);
	if (status != 1) return status;

	/*
	 * Eliminate all references to column pointers, and update
	 * extent value, if need be.
	 */
	
	for (col=lastcol-1; col>=column; col--)
	{
		colp = ibis->column[column];
		status = _i_detach_col_from_format( ibis, col );
		if (status!=1) return status;
		_i_detach_column_from_list(ibis, ibis->groups, col);
		_i_detach_column_from_list(ibis, ibis->units, col);
		_i_detach_column_from_list(ibis, ibis->locals, col);
	}

	/*
	 * Now zap all remaining traces and mark file as changed
	 * so the records know to refresh themselves.
	 */
	
	_i_delete_column_pointers(ibis, column, ncols);
	ibis->nc -= ncols;
	ibis->flags |= FLAG_MOD_LABELS;

	return( status );
}

int IBISColumnMove(int ibis_id,int sourcecol,int destcol,int ncols)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int col,tocol,i;
	int colsleft;
	int num,numdest,numfirst;
	int source[MAX_COL];
	_ibis_current_module="IBISColumnMove";
	
	if (ibis->flags & FLAG_FILE_OLD) return IBIS_FILE_OLD_IBIS;
	
	if (sourcecol == destcol) return 1; /* already there */
	if (destcol+ncols-1 > ibis->nc) return IBIS_NO_SUCH_COLUMN;

	CHECK_COLUMN( ibis, sourcecol );
	CHECK_COLUMN( ibis, destcol );
	
	sourcecol--;destcol--;
	numdest = sourcecol-destcol;
	if (numdest<0) numdest = -numdest;
	

	/* set up the inverse permutation index */
	
	for (i=0;i<ncols;i++)
		source[destcol+i]=sourcecol+i;
	for (i=0;i<numdest;i++)
		source[sourcecol+i]=destcol+i;
	
	
	/* move first block to temp */
	numfirst = (ncols>EXTRA_COL) ? EXTRA_COL : ncols;
	_i_move_column_pointers( ibis, sourcecol, MAX_COL,numfirst);
	ncols -= numfirst;
	col = sourcecol;

	for (colsleft=numdest+ncols; colsleft>0; colsleft-=num)
	{
		tocol = col;
		col = source[col]; /* permute */
		num = (colsleft>EXTRA_COL) ? EXTRA_COL : colsleft;
		_i_move_column_pointers( ibis, col, tocol, num);
	}

	/* move temp block to dest */
	_i_move_column_pointers( ibis, MAX_COL, destcol, numfirst);
	
	return( status );
}


/*
 *  Find the column <index> with group <group>, and
 *  place the logical column id in <log_column>
 *  The group <type> can be either groups, formats, units or null.
 *
 *  The group can be a relative name to <type> or absolute: "local:name",
 *  if <type> is null.
 *
 *  Returns total count of columns retrieved, or error status if other errors.
 */

int IBISColumnFind
(
  int ibis_id,
  char *type,	/* input: format, group, unit, local, all */
  char *group,	/* input: group name to search for or expression */
  int *column,	/* output: returned list of columns */
  int scol,	/* input: starting column in list */
  int maxcols	/* input: max number of columns to return */
)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	XCOL *colm;
	XGROUP *grp;
	int delete_copy=0;
	int count=0;
	_ibis_current_module="IBISColumnFind";
   
	if (!type) type=ITYPE_ANY;
	if (!group) return IBIS_INVALID_GRPNAME;
	
	/* find the group */
	
	switch (_i_keymatch(type,TypeList))
	{
    		case TYPE_FORMAT:
			grp = _i_find_group( ibis->formats, group );
    			break;
     		case TYPE_UNIT:
			grp = _i_find_group( ibis->units, group );
    			break;
     		case TYPE_LOCAL:
			grp = _i_find_group( ibis->locals, group );
    			break;
     		case TYPE_GROUP:
 			grp = _i_find_group( ibis->groups, group );
    			break;
     		case TYPE_ANY:
			grp=_i_group_construct(ibis, group);
    			break;
    		default:
    			return IBIS_INVALID_TYPE;
    			break;
	}
   
   	if (!grp) return 0;
   
   	/* return list or total count */
   
 	if (maxcols>0) /* columns requested */
    	{
		List *elt=grp->columns->next;
    		int col;
    		
    		/* scan for first element */
    		for (col=1; col<scol && elt; col++) elt = elt->next;
    		
    		if (!elt) return 0; /* ran out of columns */
    		
    		/* get columns */
    		for (count = 0; count<maxcols && elt;elt=elt->next )
    		{
			colm = (XCOL *) elt->value;
    			column[count++] = colm->id;
    		}
    		return count;
    	}
    	
    	count = _i_count_list(grp->columns);
    	if (delete_copy) _i_free_group( grp );
    	
    	return count;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_column_get,IBIS_COLUMN_GET) (int *ibis_id, char *name,
	void *value, int *column, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[MAX_VALUE_NAME+1];
   char vname[301];
   char *valptr;
   int stat;
   int uses_string=0;
  
   zsfor2c(c_name, MAX_VALUE_NAME, name, &ibis_id, 5, 2, 1, status);
   
	switch (_i_keymatch(c_name,ValueList))
	{	
		case COLVALUE_FORMAT:  
		case COLVALUE_U_FORMAT:  
			uses_string=1;
			valptr = vname;
			break;
		default:
			valptr = (char *)value;
			break;
	}
   
    stat=IBISColumnGet( *ibis_id, c_name, valptr, *column );
   
   /* deal with string values */
   if (uses_string && stat==1)
   {
   		zsc2for(valptr, 0, value, &ibis_id, 5,3,2, status);
   }
   
   *status = stat;
   return;
}


void FTN_NAME2_(ibis_column_set, IBIS_COLUMN_SET) (int *ibis_id,
	char *name, void *value, int *column, int *status, ZFORSTR_PARAM)
{
        ZFORSTR_BLOCK
	char c_name[MAX_VALUE_NAME+1];
	char vname[301];
	char *valptr;
  
	zsfor2c(c_name, MAX_VALUE_NAME, name, &ibis_id, 5, 2, 1, status);

	switch (_i_keymatch(c_name,ValueList))
	{	
		case COLVALUE_FORMAT:  
		case COLVALUE_U_FORMAT:  
			valptr = vname;		/* This is a string */
			break;
		default:  /* Implementation specific */
			valptr = (char *)value;
			break;
	}

	/* deal with string values */
	if (valptr==vname)
	{
		zsfor2c(valptr, 300, value, &ibis_id, 5,3,2, status);
		if (!valptr[0]) valptr = (char *)0; /* default */
	}
	
	
	*status=IBISColumnSet( *ibis_id, c_name, valptr, *column );

	return;
}



void FTN_NAME2_(ibis_column_read, IBIS_COLUMN_READ) (int *ibis_id,
		char *buffer, int *column, int *srow, int *nrows,
		int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   XIBIS *ibis=IBIS_FETCH_ID(*ibis_id);
   char *tempbuf=(char *)0;
   char *valptr;
   int sr = *srow;
   int nr = *nrows;
   int nchars;
   int maxlen=0;
   XCOL *col;
   trans_type  *trans;
   _ibis_current_module="IBISColumnRead";
   
   CHECK_COLUMN_FOR( ibis, *column )
   col = (ibis)->column[*column-1];
   
   /*
    * determine the translation that will be used
    * by the IBISWriteColumn routine.
    */

   trans = TRANS_USED( ibis, col);
   
   /*
    *  If the translated data is CHARACTER*n, we need to do some work to convert.
    *  The client had better have passed in a CHARACTER array or they
    *  will be in trouble. Create a temporary buffer here for the C-strings.
    */
   if (trans->infmt >= FMT_ASCII)
   {
	if (sr<=0) sr=1;
	if (nr<=0) nr=(ibis)->nr + 1 - sr;
   
	zsfor2len(nchars, buffer,&ibis_id,6,2,1, status);
	tempbuf = (char *)malloc( (nchars+1) * nr);
	if (!tempbuf)
	{
   		*status = IBIS_MEMORY_FAILURE;
   		return;
   	}   		
  	valptr = tempbuf;
   }
   else valptr = buffer;
  
      
   *status=IBISColumnRead( *ibis_id, valptr, *column, sr, nr );
  
   /*
    *  Post-process the string data into the CHARACTER array,
    *  if needed.
    */
   if (tempbuf) 
   {
	zsc2for_array( valptr, nchars+1, nr, buffer, &maxlen, &ibis_id, 6, 2, 1,
						status);
	free(tempbuf);
   }
   

   return;
}


void FTN_NAME2_(ibis_column_write, IBIS_COLUMN_WRITE) (int *ibis_id,
		char *buffer,int *column, int *srow, int *nrows,
		int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   XIBIS *ibis=IBIS_FETCH_ID(*ibis_id);
   char *tempbuf=(char *)0;
   char *valptr;
   int sr = *srow;
   int nr = *nrows;
   int nchars=0;
   XCOL *col = (ibis)->column[*column-1];
   trans_type *trans;
   _ibis_current_module="IBISColumnWrite";

   CHECK_WRITE_FOR( ibis )
   CHECK_COLUMN_FOR( ibis, *column )
   CHECK_LOCK_FOR( ibis, *column )   	
  
   /*
    * determine the translation that will be used
    * by the IBISWriteColumn routine.
    */

   trans = TRANS_USED( ibis, col);
   
   /*
    *  If the buffer data is CHARACTER*n, we need to do some work to convert.
    *  The client had better have passed in a CHARACTER array or they
    *  will be in trouble. Let the zsfor2c_array routine create the temp-buffer.
    */
   if (trans->infmt >= FMT_ASCII)
   {
	if (sr<=0) sr=1;
	if (nr<=0) nr=(ibis)->nr + 1 - sr;

	/*
	 * This call creates the temporary buffer,
	 * which we must delete at end of routine.
	 */
	
	zsfor2c_array(&tempbuf, &nchars, nr, buffer, &ibis_id, 6, 2, 1, status);
	if (!tempbuf)
	{
		*status = IBIS_MEMORY_FAILURE;
		return;
	}
	
	valptr = tempbuf;
   }
   else valptr = buffer;
      
   *status=IBISColumnWrite( *ibis_id, valptr, *column, sr, nr );

   if (tempbuf) free(tempbuf);
  
   return;
}

 
void FTN_NAME2_(ibis_column_new, IBIS_COLUMN_NEW) (int *ibis_id,
		int *column, int *ncols,
		char *format, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_format[IFMT_SIZE];
   char *fmtptr=(char *)0;

   zsfor2c(c_format, IFMT_SIZE-1, format, &ibis_id, 5, 4, 1, status);
   if (*c_format) fmtptr=c_format;

   *status = IBISColumnNew( *ibis_id, *column, *ncols, fmtptr );
   return;
}

void FTN_NAME2_(ibis_column_clear, IBIS_COLUMN_CLEAR) (int *ibis_id,
		int *column, int *ncols, int *status)
{
   *status = IBISColumnClear( *ibis_id, *column, *ncols );
   return;
}

void FTN_NAME2_(ibis_column_delete, IBIS_COLUMN_DELETE) (int *ibis_id,
		int *column, int *ncols, int *status)
{
   *status = IBISColumnDelete( *ibis_id, *column, *ncols );
   return;
}

void FTN_NAME2_(ibis_column_move, IBIS_COLUMN_MOVE) (int *ibis_id,
		int *sourcecol,int *destcol,int *ncols,int *status)
{
   *status = IBISColumnMove( *ibis_id, *sourcecol,*destcol, *ncols );
   return;
}

int FTN_NAME2_(ibis_column_find, IBIS_COLUMN_FIND) ( int *ibis_id, char *type,
	char *group, int *column, int *scol, int *maxcol, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_group[MAX_GRP_NAME+1];
   char c_type[MAX_TYPE_NAME+1];
   char *typeptr=(char *)0;
   
   zsfor2c(c_group, MAX_GRP_NAME, group, &ibis_id, 6, 3, 2, maxcol);
   zsfor2c(c_type, MAX_TYPE_NAME, type, &ibis_id, 6, 2, 1, maxcol);
   if (*c_type) typeptr=c_type;
  
   return IBISColumnFind( *ibis_id, typeptr, c_group, column, *scol, *maxcol );
}

