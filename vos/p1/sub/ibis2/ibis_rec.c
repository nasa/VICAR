#include "ibis.h"
#include <string.h>
#include <stdint.h>
/**
 **  IBIS Record Manipulation Routines
 **
 **  A record is a buffered window into a file, designed for
 **  sequential access. Initially the buffer is in a blank state,
 **  if records are written, the structure keeps track of which
 **  rows will need to be flushed to the file.
 **
 **  If records need to be read, the structure first broadcasts a request to all
 **  other records currently covering the area of interest to flush
 **  their buffers, and then the record reads in enough of the
 **  file to fill its buffer.
 **
 **  The record has a current-row pointer, which is advanced on
 **  each read/write call. It may be reset at any time, which may
 **  cause the buffer to flush its current modified contents, if any.
 **
 **  A record consists of two internal buffers. The first one is the
 **  untranslated file data, stored in contigous-column format. This
 **  is the "File" buffer. The second one ("Local") is used only if translation
 **  is turned on (default), and is the U_FORMAT translated column
 **  data. The reading and writing performed will operate on one or the
 **  other of these two buffers, depending on whether U_FORMAT is NONE.
 **/

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

typedef enum {
    VALUE_U_FORMAT=1,
    VALUE_U_SIZE,
    VALUE_COLUMNS,
    VALUE_IUNIT,
    VALUE_NR,
    VALUE_NC,
    VALUE_ROW,
    VALUE_REC_SIZE,
    VALUE_LAST
} colvalue_type;

static char *ValueList[]={
    IRECORD_U_FORMAT,
    IRECORD_U_SIZE,
    IRECORD_COLUMNS,
    IRECORD_IUNIT,
    IRECORD_NR,
    IRECORD_NC,
    IRECORD_ROW,
    IRECORD_REC_SIZE,
	(char *)0
};

#define IS_OUTSIDE( record, row ) \
	 ( ((row) > (record)->top_row+(record)->num_rows-1) || \
	   ((row) < record->top_row) )

static int _FlushRecord();		/* forward ref, shuts up compiler */

int _HandleNotice(XREC* record, notice_type notice,void * sval, int endval )
{
	int status = 1;
	
	switch (notice)
	{
		case NOTICE_NEED_ROWS:
			if (record->num_mods<1) return 1;
			if (endval - record->top_row < record->mod_row)
				 return 1;
			if ((record->mod_row + record->num_mods -1)
						< ((int)(uintptr_t) sval)-record->top_row )
				 return 1;
			
			/* must respond NOW !*/
			status = _FlushRecord( record );
			if (status !=1) return status;
				
			break;
			
		case NOTICE_FLUSHING_ROWS: 
			if (endval < record->top_row) return 1;
			if ((record->top_row + record->num_rows -1) < ((int) (uintptr_t) sval) ) return 1;
			
			/* passive notice for next read */
			record->flags |= FLAG_REC_REFRESH; 

			break;
			
		case NOTICE_OPEN:
			_i_insert_value( record->collaborators, (list_value) sval );
			break;
			
		case NOTICE_CLOSING:
			_i_delete_value( record->collaborators, (list_value) sval );
			break;
	}
	
	return status;
}

/*
 * Utility for notifying collaborators of change in status.
 * A collaborator is another record which shares some of 
 * the same columns as this record, and they often need to
 * notify each other of changes in file/buffer status.
 */

static int _NotifyCollaborators( record, notice, sval, endval )
XREC *record;
notice_type notice;
void *sval;
int endval;
{
	List *ent;
	XREC *rec;
	int status=1;

	for (ent=record->collaborators->next; ent; ent=ent->next)
	{
		rec = (XREC *)ent->value;
		status = rec->method->notice( rec, notice, sval, endval );
		if (status != 1) return status;
	}
	return status;
}

/*
 *  Translate the Local buffer data to the File Buffer
 *  "srow" is relative to the top of the buffers.
 */
 
static int _ConvertLocalToFile( record, srow, nrows)
XREC *record;
int srow;
int nrows;
{
	XCOL *colm;
	int ofmt;
	int col;
	int insize=format_size[record->format];
	int *size=record->ibis->format_size;
	trans_type *trans=record->trans;
	int status=1;
	
	if (record->format != FMT_NONE)
	{
		
		for (col=0;col<record->num_cols;col++)
		{
			colm = record->column[col];
			ofmt = colm->format;
			if (ofmt > FMT_ASCII) ofmt = FMT_ASCII;
			
			status = _i_trans_buf_from_local( trans+ofmt, colm,
						record->inbuffer[col] + insize*srow,
						 record->outbuffer[col] + size[ ofmt ]*srow,
						 nrows );
			if (status!=1) return status;
		}
	}
	
	return status;
}

/*
 *  Convert File buffer to Local. The whole buffer is
 *  always translated.
 */
 
static int _ConvertFileToLocal( record, nrows)
XREC *record;
int nrows;
{
	XCOL *colm;
	int ofmt;
	int col;
	int status=1;
	trans_type *trans=record->trans;
	char *inptr;
	char *outptr;
	
	if (record->format != FMT_NONE)
	{		
		for (col=0;col<record->num_cols;col++)
		{
			colm = record->column[col];
			ofmt = colm->format;
			if (ofmt > FMT_ASCII) ofmt = FMT_ASCII;
			inptr = record->inbuffer[col];
			outptr = record->outbuffer[col];
			
			status = _i_trans_buf_to_local( trans+ofmt, colm,
						 outptr, inptr, nrows );
			if (status!=1) return status;
		}
	}
	
	return status;
}

/*
 *  This utility copies the client-supplied buffer to the
 *  relevant "Local" buffer, which may be either the File
 *  or the local buffer, depending upon the U_FORMAT
 */
 
static int _TransferBufferToLocal( record, buffer, row)
XREC *record;
char *buffer;
int row;
{
	int col;
	int status=1;
	char *bptr;
	char **bufptr;
	
	row -= record->top_row;

	if (record->format == FMT_NONE) /* unformatted transfer */
	{
		XCOL *colm;
		int *size = record->ibis->format_size;
		int fsize;
		int offset;
		
		bptr = buffer;
		bufptr = record->outbuffer;
		for (col=0;col<record->num_cols;col++)
		{
			colm = record->column[col];
			fsize = size[ colm->format ];
			offset = fsize * row;
			memcpy( bufptr[col] + offset,  bptr, fsize );
			bptr += fsize;
		}
	}
	else /* formatted transfer */
	{
		int size = format_size[record->format];
		int offset = size*row;
		
		bptr = buffer;
		bufptr = record->inbuffer;
		for (col=0;col<record->num_cols;col++)
		{
			memcpy( bufptr[col] + offset, bptr, size );
			bptr += size;
		}
	}

	 /*
	  * Update the row pointers indicating what parts of
	  * the buffer have been modified.
	  */
	
	if (row < record->mod_row)
	{
		record->num_mods+=(record->mod_row - row);
		record->mod_row = row;
	}
	else if (!record->num_mods) 
	{
		record->mod_row = row;
		record->num_mods = 1;
	}
	else if (row + 1 > record->num_mods + record->mod_row)
	{
		record->num_mods = (1 + row - record->mod_row);
	}
	
	return status;
}

/*
 *  This utility copies the relevant "Local" buffer to
 *  the client-supplied buffer. The "Local" buffer may be
 *  either the File or the local buffer, depending upon the U_FORMAT
 */

static int _TransferLocalToBuffer( record, buffer, row)
XREC *record;
char *buffer;
int row;
{
	int col;
	int status=1;
	char *bptr;
	char **bufptr;
	
	row -= record->top_row;

	if (record->format == FMT_NONE) /* unformatted transfer */
	{
		XCOL *colm;
		int *size = record->ibis->format_size;
		int fsize;
		int offset;
		
		bptr = buffer;
		bufptr = record->outbuffer;
		for (col=0;col<record->num_cols;col++)
		{
			colm = record->column[col];
			fsize = size[ colm->format ];
			offset = fsize * row;
			memcpy( bptr, bufptr[col] + offset, fsize );
			bptr += fsize;
		}
	}
	else /* formatted transfer */
	{
		int size = format_size[record->format];
		int offset = size*row;
		
		bptr = buffer;
		bufptr = record->inbuffer;
		for (col=0;col<record->num_cols;col++)
		{	
			memcpy( bptr, bufptr[col] + offset, size );
			bptr += size;
		}
	}
	
	return status;
}
static int _ClearLocalBuffer( record, row, nrows)
XREC *record;
int row;
int nrows;
{
	int col;
	int status=1;
	char **bufptr;
	
	row -= record->top_row;

	if (record->format == FMT_NONE) /* unformatted transfer */
	{
		XCOL *colm;
		int *size = record->ibis->format_size;
		int fsize;
		int offset;
		
		bufptr = record->outbuffer;
		for (col=0;col<record->num_cols;col++)
		{
			colm = record->column[col];
			fsize = size[ colm->format ];
			offset = fsize * row;
			memset( bufptr[col] + offset,  0, fsize * nrows );
		}
	}
	else /* formatted transfer */
	{
		int size = format_size[record->format];
		int offset = size*row;
		
		bufptr = record->inbuffer;
		for (col=0;col<record->num_cols;col++)
		{
			memset( bufptr[col] + offset, 0, size * nrows );
		}
	}

	 /*
	  * Update the row pointers indicating what parts of
	  * the buffer have been modified.
	  */
	
	
	if (row < record->mod_row)
	{
		record->num_mods+=(record->mod_row - row);
		record->mod_row = row;
	}
	else if (!record->num_mods) 
	{
		record->mod_row = row;
		record->num_mods = nrows;
	}

	/* also make sure that lower bound is correct */

	if (row + nrows > record->num_mods + record->mod_row)
	{
		record->num_mods = (nrows + row - record->mod_row);
	}


	return status;
}


/*
 * Dump the contents of buffer to file, and broadcast the
 * change to collaborators for refresh, but
 * don't invoke _RefreshRecord (avoids infinite loop).
 */

static int _FlushRecord(record)
XREC *record;
{
	int status=1;
	int mod_row=record->mod_row;
	int num_mods=record->num_mods;

	if (record->num_mods < 1) return 1;
	
	/* translate data from local to file format, if need be */
	
	status=_ConvertLocalToFile( record, mod_row, num_mods);
	if (status!=1) return status;
		
	/* use dofile to write out data */

	status = record->method->dofile( record, zvwrit, mod_row, num_mods );
	if (status != 1) return status;

	/* notify collaborators of the change in file */
	status = _NotifyCollaborators( record, NOTICE_FLUSHING_ROWS,
				(void *)(uintptr_t) mod_row, (mod_row + num_mods -1) );
	if (status != 1) return status;

	/* reset all the important pointers */
		
	record->num_mods = 0;
	record->mod_row = 0;
	record->flags |= FLAG_REC_REFRESH; /* invalidates buffer for read */
	record->top_row = record->cur_row;
	return 1;
}

/*
 *  Refresh the buffer contents from the file, making
 *  sure that collaborators dump any relevant buffered rows.
 */

static int _RefreshRecord(record)
XREC *record;
{
	int status=1;
	int srow;
	int nrows;
	XIBIS *ibis=record->ibis;
	
	if (!(record->flags & FLAG_REC_REFRESH)) return 1;
	
	if (record->num_mods > 0)
	{
		status = _FlushRecord( record );
		if (status != 1) return status;
	}

	/* determine the exact window needed from file */
	if (IS_OUTSIDE( record, record->cur_row))
		record->top_row = record->cur_row;
	srow = record->top_row;
	nrows = record->num_rows;
	if ((srow + nrows -1) > ibis->nr) nrows = ibis->nr + 1 - srow;

	/*
	 * Broadcast to the collaborators the window of interest
	 * that needs to be read from file (srow,nrows).
	 */
	
	status=_NotifyCollaborators( record, NOTICE_NEED_ROWS, (void *)(uintptr_t) srow, (srow + nrows -1) );
	if (status!=1) return status;
	
	record->flags &= ~FLAG_REC_REFRESH; /* data will now be valid */
	 
	/*
	 *  Use dofile to read in data
	 */

	status = record->method->dofile( record, zvread, 0, nrows );
	if (status != 1) return status;

	/*
	 * Translate data into local format, if need be.
	 */
	status=_ConvertFileToLocal( record, nrows );
	if (status!=1) return status;
	
	
	return status;
}

static int _ResetRecordTrans( record, u_fmt )
XREC *record;
fmt_type u_fmt;
{
	XIBIS *ibis=record->ibis;
	trans_type *trans=record->trans;
	fmt_type fmt;
		
	/* reset all format translators */
	
	if (u_fmt < FMT_ASCII && u_fmt > FMT_NONE)
	{
		if (record->format > FMT_ASCII || record->format==FMT_NONE)
			return IBIS_CANT_TRANSLATE;
		for (fmt = FMT_BYTE; fmt<=FMT_COMP; fmt++)
			_i_set_trans( ibis, trans+fmt, u_fmt, fmt );
	}
	else if (u_fmt > FMT_ASCII)
	{
		if (record->format < FMT_ASCII || record->format==FMT_NONE)
			return IBIS_CANT_TRANSLATE;
		_i_set_trans( ibis, trans+FMT_ASCII, u_fmt, FMT_ASCII );
	}
	
	record->format = u_fmt;

	return 1;
}

static int _ResetRecordBuffers( record, num_rows )
XREC *record;
int num_rows;
{
	XIBIS *ibis=record->ibis;
	int buf_rows;
	int col;
	int status=1;
	int *size=ibis->format_size;
	char *bptr;

	if (num_rows > ibis->nr) num_rows=ibis->nr;
	buf_rows = ALIGN_UP( num_rows, IBYTE_ALIGN);
	record->num_rows = num_rows;
	
	if (record->outspace) free(record->outspace);
	record->outspace = (char *)malloc( buf_rows * record->recsize);
	if (!record->outspace)
	{
		status = IBIS_MEMORY_FAILURE;
		goto failure;
	}
			
	bptr = record->outspace;
	for (col=0; col<record->num_cols; col++)
	{
		record->outbuffer[col] = bptr;
		bptr += buf_rows * size[ record->column[col]->format];
	}
	
	if (record->inspace) free(record->inspace);
	record->inspace = (char *)0;
	if (record->format!=FMT_NONE)
	{
		int fsize=format_size[record->format];
		int nc = record->num_cols;
		int colsize = fsize*buf_rows;
		
		/* need translated buffer, too */
		record->inspace = (char *)malloc( nc*colsize );
		if (!record->inspace)
		{
			status = IBIS_MEMORY_FAILURE;
			goto failure;
		}
		bptr = record->inspace;
		for (col=0; col<nc; col++)
		{
			record->inbuffer[col] = bptr;
			bptr += colsize;
		}
	}

	return status;
failure:
	return status;
}

static int _InstallColumns( ibis, record, columns, ncols )
XIBIS *ibis;
XREC *record;
int *columns;
int ncols;
{
	int col;
	List *ent;
	XREC *rec;
	XCOL **rec_col;
	XCOL *colm;
 	int *sharecols;
 	int found_col;
 	int ofmt = record->format;
 	int cfmt;
 	int *size = record->ibis->format_size;
	int ncols_used=0;

	/*
	 * Scan through and install columns, but don't lock them yet.
	 * Now is also a good time to figure out the sizes of the
	 * untranslated record buffers and set up a temporary lookup table
	 * flagging which columns belong to this record. We will
	 * also make sure that the columns are compatible
	 * if the translation type is not 'NONE'
	 */

 	 sharecols = (int *)calloc(1L, (record->ibis->nc+1)*sizeof(int));
 	 if (!sharecols) return IBIS_MEMORY_FAILURE;
	 
	 for (col=0,record->recsize=0; col<ncols; col++)
	 {
	 	if (columns[col]<1 || columns[col]>ibis->nc)
	 		return IBIS_NO_SUCH_COLUMN;
	 	colm = ibis->column[ columns[col] -1 ];
	 	record->column[col] = colm;
	 	record->recsize += size[ colm->format ];
	 	if (ofmt > FMT_NONE)
	 	{
		 	cfmt = colm->format;
	 		if ((ofmt<FMT_ASCII && cfmt > FMT_ASCII)
			    || (ofmt > FMT_ASCII && cfmt < FMT_ASCII))
	 			return IBIS_CANT_TRANSLATE;
	 	}
		ncols_used += !(sharecols[colm->id]);
	 	sharecols[colm->id]=1;
	 }

	/*
	 * Scan through and install columns,and look for other 
	 * records sharing these columns (collaborators).
	 */

 	if (ibis->record) /* look for records sharing these columns */
 	{
 		for (ent=ibis->record->next; ent; ent=ent->next)
 		{
 			rec = (XREC *)ent->value;
 			rec_col = rec->column;
 			for (col=0, found_col=0; col<rec->num_cols && !found_col; col++)
 			{
 				if (sharecols[ rec_col[col]->id ])
 				{
 					/* 
 					 * We found a collaborator ! We don't install the
 					 * new record into the collaborator's list yet,
 					 * but wait until open call succeeds, and
 					 * then broadcast the fact.
 					 */
 					_i_insert_value( record->collaborators, (list_value) rec );
 					found_col=1;
 				}
 			}
 		}
 	}

	/* Flag whether all columns are used or not */
	if (ncols_used != ibis->nc)
		record->flags |= FLAG_REC_GAPS;
 	
 	free( sharecols );
 	return 1;
}


int IBISRecordOpen(int ibis_id, int *record_id,  char *group, int *columns, 
		   int ncols, char* u_format)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int delete_list=0;
	int status=1;
	int num_rows;
	int col;
	XREC *newrec=(XREC*)0;
#if POINTERS_64_BITS_OS
	int i;
#endif
	_ibis_current_module="IBISRecordOpen";

	CHECK_UNIT( ibis );

	/*
	 * construct the list of columns from either the literal
	 * list or the group.
	 */

	if (group)
	{
		ncols = IBISColumnFind( ibis_id, ITYPE_ANY, group, 0, 1, 0 );
		if (ncols < 1) return IBIS_GROUP_IS_EMPTY;
		columns = (int *)calloc( 1L, ncols*sizeof(int));
		if (!columns) return IBIS_MEMORY_FAILURE;
		delete_list=1;
		status = IBISColumnFind( ibis_id, ITYPE_ANY, group, columns, 1, ncols );
		if (status < 0) goto failure;
	}
	else
	{
		if (!columns || ncols < 1) return IBIS_GROUP_IS_EMPTY;
	}

	/*
	 *  Create record structure
	 */
	 
	newrec = _i_new_record( ncols );
	if (!newrec) 
	{
		status = IBIS_MEMORY_FAILURE;
		goto failure;
	}

	newrec->ibis = ibis;
	
	/* install methods from file and local methods */
	*newrec->method = *ibis->recmethod;
	newrec->method->notice = _HandleNotice;
	
	/* set up translation stuff */
        newrec->format = _i_IBISFormatCode( u_format );
        if (newrec->format < 0)
        {
        	status = newrec->format;
        	goto failure;
        }
        status = _ResetRecordTrans( newrec, newrec->format );
        if (status != 1) goto failure;

	status = _InstallColumns(ibis, newrec, columns, ncols );
	if (status != 1) goto failure;

	/*
	 * To set up internal buffers, work out how much memory
	 * we can allocate to them. If it's small enough,
	 * we can put the whole thing in memory.
	 */
	
	num_rows = HOW_MANY( I_TEMP_BUFSIZE, newrec->recsize );
	if ((num_rows) > ibis->nr) num_rows = ibis->nr;
	newrec->num_rows = num_rows;

	status = _ResetRecordBuffers( newrec, newrec->num_rows );
	if (status != 1) goto failure;

	/* attach record to ibis list */
	if (!ibis->record)
	{
		ibis->record = _i_new_list((void(*)(int*)) _i_free_record );
		if (!ibis->record)
		{
			status = IBIS_MEMORY_FAILURE;
			goto failure;
		}
	}
	_i_insert_value( ibis->record, (list_value)newrec);

	/* lock columns */
	 for (col=0;col<ncols;col++)
	 	newrec->column[col]->locked++; /* incremented lock status */

	/* notify collaborators */
	status = _NotifyCollaborators( newrec, NOTICE_OPEN, (void *)newrec, 0 );
	if (status != 1) return status;

	/*
	 *  Return new record and free up column list, if we created it.
	 */

	/* Determine record ID, based on the pointer */

#if POINTERS_64_BITS_OS
	*record_id = 0;
	for (i=1; i<MAX_NUM_IBIS_REC_IDS; i++)
	{
		if (_ibis_rec_id_table[i] == (XREC *)0)
		{
			_ibis_rec_id_table[i] = newrec;
			*record_id = i;
			break;
		}
	}
	if (*record_id == 0)
	{
		status = IBIS_MEMORY_FAILURE;
		goto failure;
	}
#else
	*record_id = (int)newrec;
#endif

	if (delete_list) free(columns);
	return status;

failure:
	if (delete_list) free(columns);
	if (newrec) _i_free_record(newrec);
	*record_id = 0;

	return status;

}


int IBISRecordClose(int record_id )
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	int status=1;
	int col;
	XIBIS *ibis=record->ibis;	
	_ibis_current_module="IBISRecordClose";

	/* unlock columns */
	 for (col=0; col<record->num_cols; col++)
	 	record->column[col]->locked--; /* deccrement lock status */

	/* flush the buffer */
	status = _FlushRecord( record );
	if (status!=1) return status;

	status=_NotifyCollaborators( record, NOTICE_CLOSING, (void *)record, 0);
	if (status != 1) return status;

	/* Delete from IBIS (this will kill the struct) */
	_i_delete_value (ibis->record, (list_value) record);

	/* Kill the ID table entry */
#if POINTERS_64_BITS_OS
	_ibis_rec_id_table[record_id] = (XREC *)0;
#endif

	return status;
}

int IBISRecordSet(int record_id, char *name, int value )
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	XIBIS* ibis=record->ibis;
	int status=1;
	int num_rows;
	int row,fmt;
	_ibis_current_module="IBISRecordSet";

	switch(_i_keymatch(name,ValueList))
	{
        	case VALUE_U_FORMAT:
			/* 64-bit machines can't accept a char * in an int */
			/* argument.  Rather than change the API right now,*/
			/* I'm simply disabling this function for now since*/
			/* it appears to be unused (presumably on a	   */
			/* temporary basis... yeah right)		   */
			/* rgd 1-28-99					   */
			if (sizeof(int) != sizeof(char *))
				return IBIS_INVALID_PARM;

	        	/* 
	        	 * if this is a change, need to resize buffers 
	        	 * and reset the trans routines.
	        	 */
	        	fmt = _i_IBISFormatCode( (char *) (uintptr_t) value );
	        	if (fmt==record->format) return 1;

			status = _FlushRecord( record );
			if (status != 1) return status;
	        
	        	status = _ResetRecordTrans( record, fmt );
	        	if (status != 1) return status;

			status = _ResetRecordBuffers( record, record->num_rows );
			if (status != 1) return status;
	        	break;
	        	
		case VALUE_NR:
			num_rows = (int)value;
			if (num_rows > ibis->nr) num_rows = ibis->nr;
			if (record->num_rows == num_rows) return 1; /* already done */

			status = _FlushRecord( record );
			if (status != 1) return status;

			status = _ResetRecordBuffers( record, num_rows );
			if (status != 1) return status;
			
			break;
			
	        case VALUE_ROW:
	 		row = (int)value;
	 		if (row == record->cur_row) return 1; /* already there */
	 		
	 		if (row > record->ibis->nr)
	 			return IBIS_LAST_ROW;
	 		if ( ! IS_OUTSIDE( record, row ))
	 		{
	 			/* we are still in the current window */
	 			record->cur_row = row;
	 			return 1;
	 		}
			
			/*
			 * If we got here, then we need to move buffer
			 */
			
			status = _FlushRecord( record );
			if (status != 1) return status;

			record->top_row = row;
			record->cur_row = row;
			record->flags |= FLAG_REC_REFRESH;
	 		
	        	break;
		default:
			return IBIS_INVALID_PARM;
			break;
	}

	return status;
}

int IBISRecordGet(int record_id, char* name, char* value, int sval, int nvals )
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	int count=1;
	int col;
	_ibis_current_module="IBISRecordGet";

	switch(_i_keymatch(name,ValueList))
	{
	        case VALUE_U_FORMAT:
	        	if (nvals > 0)
	        	strcpy( (char *)value, format_name[ record->format ] );
	        	break;
	        case VALUE_U_SIZE:
	        	if (nvals > 0)
			*(int *)value = format_size[ record->format ];
	        	break;
	        case VALUE_COLUMNS:
	        	if (nvals==0) return record->num_cols;
	        	if (sval + nvals -1 > record->num_cols) 
	        		nvals = record->num_cols + 1 - sval;
	        	count = nvals;
	        	for (col=0;col<nvals;col++)
	        		((int *)value)[col] = record->column[col+sval-1]->id;
	        	break;
	        case VALUE_IUNIT:
	        	if (nvals > 0)
			*(XIBIS **)value = record->ibis;
	        	break;
		case VALUE_NR:
	        	if (nvals > 0)
			*(int *)value = record->num_rows;
			break;		
	        case VALUE_NC:
	        	if (nvals > 0)
	 		*(int *)value = record->num_cols;
	       		break;
	        case VALUE_ROW:
	        	if (nvals > 0)
	 		*(int *)value = record->cur_row;
	        	break;
	        case VALUE_REC_SIZE:
	        	if (nvals > 0)
	        	{
		        	if (record->format == FMT_NONE)
		 			*(int *)value = record->recsize;
		 		else
		 			*(int *)value = format_size[ record->format ]
		 						*record->num_cols;
	 		}
	        	break;
		default:
			return IBIS_INVALID_PARM;
			break;
	}
  
	return count;
}

/*
 * We need to load up the local buffers here, and whenever things fill up
 * call a flush or refresh routine, which in turn calls
 * the "dofile" method with a translated buffer array.
 */


int IBISRecordRead(int record_id, char* buffer, int row)
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	int status=1;
	_ibis_current_module="IBISRecordRead";
	
	/* check for defaulted row */
	if (row < 1) row = record->cur_row;
	
	/* position row pointer, or return error */
	if (row != record->cur_row)
	{
		status = IBISRecordSet( record_id, IRECORD_ROW, row );
		if (status != 1) return status;
	}
	else if (row > record->ibis->nr)
		return IBIS_LAST_ROW;

	/* see if row pointer is beyond current buffer */
	if ( IS_OUTSIDE( record, row ) )
		record->flags |= FLAG_REC_REFRESH;

	/* refresh if needed */
	if (record->flags & FLAG_REC_REFRESH)
	{
		status = _RefreshRecord( record );
		if (status != 1) return status;
	}
	
	
	/*
	 *  Now we can just put the data into the client's buffer,
	 *  column-by-column.
	 */
	 
	_TransferLocalToBuffer( record, buffer, row );
	 
	/*
	 * Update the row pointer
	 */
	
	record->cur_row++;
	
	return status;
}

int IBISRecordWrite(int record_id,char* buffer,int row)
/* row: record to write, or 0 */
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	int status=1;
	_ibis_current_module="IBISRecordWrite";
   
	CHECK_WRITE( record->ibis );
	
	/* check for defaulted row */
	if (row < 1) row = record->cur_row;
	
	/* position row pointer, or return error */
	if (row != record->cur_row)
	{
		status = IBISRecordSet( record_id, IRECORD_ROW, row );
		if (status != 1) return status;
	}
	else if (row > record->ibis->nr)
		return IBIS_LAST_ROW;

	/* see if row pointer is beyond current buffer */
	if (IS_OUTSIDE( record, row ))
	{
		status = _FlushRecord( record );
		if (status!= 1) return status;
	}
	
	/*
	 *  Now we can just grab the data from the client's buffer,
	 *  column-by-column.
	 */

	_TransferBufferToLocal( record, buffer, row );
	 
	record->cur_row++;
	record->ibis->flags |= FLAG_MOD_RECORDS;
	
	return status;
}

int IBISRecordClear(int record_id,int row,int nrows)
{
	XREC *record=IBIS_FETCH_REC_ID(record_id);
	int status=1;
	int  rows_left, rows_now;
	_ibis_current_module="IBISRecordClear";

   
	CHECK_WRITE( record->ibis );
	
	/* check for defaulted row */
	if (row < 1) row = record->cur_row;
	
	if (row + nrows - 1 > record->ibis->nr )
		return IBIS_LAST_ROW;
	
	/* position row pointer, or return error */
	if (row != record->cur_row)
	{
		status = IBISRecordSet( record_id, IRECORD_ROW, row );
		if (status != 1) return status;
	}
	else if (row > record->ibis->nr)
		return IBIS_LAST_ROW;

	/* see if row pointer is beyond current buffer */
	if (IS_OUTSIDE( record, row ))
	{
		status = _FlushRecord( record );
		if (status!= 1) return status;
	}
	
	/*
	 *  Now we can just clear out the local buffers
	 */

	for (rows_left = nrows; rows_left>0; rows_left -= rows_now)
	{
		/* this will force a flush of previous cleared rows */
		
		status = IBISRecordSet( record_id, IRECORD_ROW, row );
		if (status != 1) return status;
		
		rows_now = (row + nrows > record->top_row + record->num_rows) ?
				record->top_row + record->num_rows - row : nrows;
		
		_ClearLocalBuffer( record, row, rows_now);
		row += rows_now;
	}
	
	record->cur_row = row;
	record->ibis->flags |= FLAG_MOD_RECORDS;
	
	return status;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/



void FTN_NAME2_(ibis_record_open, IBIS_RECORD_OPEN) ( int *ibis_id,
	int *record_id, char *group, int *columns, int *ncol,
	char *u_format, int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_group[MAX_GRP_NAME+1],*group_ptr=(char *)0;
   char c_format[MAX_VALUE_NAME+1],*format_ptr=(char *)0;
   int *cols=(int *)0;
  
   zsfor2c(c_group, MAX_GRP_NAME, group, &ibis_id, 7, 3, 1, status);
   zsfor2c(c_format, MAX_VALUE_NAME, u_format, &ibis_id, 7, 6, 2, status);
   if (*c_group) group_ptr=c_group;
   if (*c_format) format_ptr=c_format;
   
   if (*columns) cols=columns;
   
   *status = IBISRecordOpen(*ibis_id, record_id, group_ptr, cols, *ncol,format_ptr);
   
   return;
}

void FTN_NAME2_(ibis_record_close, IBIS_RECORD_CLOSE) ( int *record_id,
		int *status)
{
   *status = IBISRecordClose( *record_id );
   return;
}


void FTN_NAME2_(ibis_record_set, IBIS_RECORD_SET) ( int *record_id, char *name,
		void *value, int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_name[MAX_VALUE_NAME+1];
   char vname[MAX_VALUE_NAME+1];
   char *valptr;

   zsfor2c(c_name, MAX_VALUE_NAME, name, &record_id, 4, 2, 1, status);

   switch (_i_keymatch(c_name,ValueList))
   {	
	case VALUE_U_FORMAT:
   		valptr = vname;		/* This is a string */
   		break;
   	default:  /* Implementation specific */
   		valptr = (char *)value;
   		break;
   }

   /* deal with string values */
   if (valptr==vname)
   {
   	zsfor2c(valptr, MAX_VALUE_NAME, value, &record_id, 4,3,2, status);
   	if (!valptr[0]) valptr = (char *)0; /* default */
   }


   *status = IBISRecordSet( *record_id, c_name, (int) (uintptr_t) valptr );
   
   return;
}



int FTN_NAME2_(ibis_record_get, IBIS_RECORD_GET) ( int *record_id, char *name,
		char *value, int *sval, int *nvals, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[MAX_VALUE_NAME+1];
   char vname[MAX_VALUE_NAME+1];
   char *valptr;
   int count=1;
   int uses_string=0;
  
   zsfor2c(c_name, MAX_VALUE_NAME, name, &record_id, 5, 2, 1, nvals);

   switch (_i_keymatch(c_name,ValueList))
   {   
	case VALUE_U_FORMAT:
		uses_string=1;
		valptr = vname;
		break;
	default:
		valptr = (char *)value;
		break;
   }


   count = IBISRecordGet( *record_id, c_name, valptr, *sval, *nvals );
   
   /* deal with string values */
   if (uses_string && count==1)
   {
   		zsc2for(valptr, 0, value, &record_id, 5,3,2, nvals);
   }
   
   return count;
}


void FTN_NAME2_(ibis_record_read, IBIS_RECORD_READ) (int *record_id,
		char *buffer, int *row, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   XREC *record=IBIS_FETCH_REC_ID(*record_id);
   char *tempbuf=(char *)0;
   char *valptr;
   int nchars;
   int maxlen=0;
   int nc = (record)->num_cols;
   _ibis_current_module="IBISRecordRead";
   
  
   /*
    *  If the translated data is CHARACTER*n, we need to do some work to convert.
    *  The client had better have passed in a CHARACTER array or they
    *  will be in trouble. Create a temporary buffer here for the C-strings.
    */
   if ((record)->format > FMT_ASCII)
   {
	zsfor2len(nchars, buffer,&record_id,4,2,1, status);
	tempbuf = (char *)malloc( (nchars+1) * nc);
	if (!tempbuf)
	{
   		*status = IBIS_MEMORY_FAILURE;
   		return;
   	}   		
  	valptr = tempbuf;
   }
   else valptr = buffer;
  
   *status = IBISRecordRead(*record_id, valptr, *row);

   /*
    *  Post-process the string data into the CHARACTER array,
    *  if needed.
    */
   if (tempbuf) 
   {
	zsc2for_array(valptr, nchars+1, nc, buffer, &maxlen, &record_id,
			4, 2, 1, status);
	free(tempbuf);
   }
   

   return;
}


void FTN_NAME2_(ibis_record_write, IBIS_RECORD_WRITE) (int *record_id,
		char *buffer, int *row, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   XREC *record=IBIS_FETCH_REC_ID(*record_id);
   char *tempbuf=(char *)0;
   char *valptr;
   int nchars=0;
   _ibis_current_module="IBISRecordWrite";
   
  /*
    *  If the buffer data is CHARACTER*n, we need to do some work to convert.
    *  The client had better have passed in a CHARACTER array or they
    *  will be in trouble. Let the zsfor2c_array routine create the temp-buffer.
    */
   if ((record)->format > FMT_ASCII)
   {
	/*
	 * This call creates the temporary buffer,
	 * which we must delete at end of routine.
	 */
	
	zsfor2c_array( &tempbuf, &nchars, (record)->num_cols, buffer, &record_id, 4, 2, 1, status);
	if (!tempbuf)
	{
		*status = IBIS_MEMORY_FAILURE;
		return;
	}
	
	valptr = tempbuf;
   }
   else valptr = buffer;
   
   *status = IBISRecordWrite(*record_id, valptr, *row);
   
   if (tempbuf) free(tempbuf);
   
   return;
}


void FTN_NAME2_(ibis_record_clear, IBIS_RECORD_CLEAR) (int *record_id,
		int *row, int *nrows, int *status)
{
   *status = IBISRecordClear(*record_id, *row, *nrows);
   return;
}


