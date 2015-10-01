$!****************************************************************************
$!
$! Build proc for MIPL module ibis2
$! VPACK Version 1.9, Tuesday, March 02, 2010, 10:32:24
$!
$! Execute by entering:		$ @ibis2
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   DOC         Only the documentation files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module ibis2 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Doc = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "DOC" then Create_Doc = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Doc .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ibis2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Doc then gosub Doc_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ibis2.imake") .nes. ""
$   then
$      vimake ibis2
$      purge ibis2.bld
$   else
$      if F$SEARCH("ibis2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibis2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibis2.bld "STD"
$   else
$      @ibis2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibis2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibis2.com -mixed -
	-s ibis_col.c ibis_file.c ibis_globals.c ibis_group.c ibis_rec.c -
	   ibis_label.c ibis_row.c ibis_signal.c mthd_file_old.c -
	   mthd_file_new.c mthd_io_col.c mthd_io_gr1.c mthd_io_old.c -
	   mthd_io_row.c mthd_labl_new.c mthd_labl_old.c mthd_labl_gr1.c -
	   mthd_null.c struct_lists.c struct_xcol.c struct_xgap.c -
	   struct_xgroup.c struct_xrec.c struct_xibis.c util_format.c -
	   util_strings.c util_file.c util_trans.c ibis.h ibisdeclares.h -
	   ibisdefines.h ibisglobals.h ibislists.h ibisstructs.h -
	-d ibisfac.msg -
	-i ibis2.imake -
	-t tibis2.c tibis2_for.f tibis2.pdf tibis2.imake tstibis2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibis_col.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_file.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_globals.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <stdio.h>

/* Global variables for XIBIS module */

static List	_ibis_list;
List* 		x_ibis = &_ibis_list;
int 		default_format=FMT_REAL;
int		format_size[FMT_LAST+1];
static char	format_nameX[FMT_LAST+1][IFMT_SIZE];
char		*format_name[FMT_LAST+2];
char		*_ibis_current_module="NO MODULE";
char *format_label[] = {
		 "FMT_*ERROR*",
		 "FMT_NONE",
		 "FMT_BYTE",
		 "FMT_HALF",
		 "FMT_FULL",
		 "FMT_REAL",
		 "FMT_DOUB",
		 "FMT_COMP",
		 "FMT_ASCII",
		 (char *)0L
};

/* ID tables for external interface.  See ibisdefines.h. */

#if POINTERS_64_BITS_OS
XIBIS *_ibis_id_table[MAX_NUM_IBIS_IDS];
XREC *_ibis_rec_id_table[MAX_NUM_IBIS_REC_IDS];
#endif


int _init_ibis_globals(void)
{
	static int first_time=1;
	int i;

	if (!first_time) return 1;

	/* initialize ibis list */
	x_ibis->destruct = (void(*)(list_value))_i_free_ibis;
	
	/* initialize the format name converters */

	for (i = FMT_BASE; i<=FMT_ASCII; i++)
	{
		strcpy(format_nameX[i],(format_label[i] + 4));
		format_name[i] = (char *)format_nameX[i];
	}

	/* initialize format size array and ASCII names */

	format_size[FMT_BASE] = 	0;
	format_size[FMT_NONE] = 	0;
	format_size[FMT_BYTE] =		sizeof( char );
	format_size[FMT_HALF] =		sizeof( short );
	format_size[FMT_FULL] =		sizeof( int );
	format_size[FMT_REAL] =		sizeof( float );
	format_size[FMT_DOUB] =		sizeof( double );
	format_size[FMT_COMP] =		2*sizeof( float );
	format_size[FMT_ASCII] =	8*sizeof( char ); /* just in case ... */
	for (i=FMT_ASCII+1; i<=FMT_LAST; i++)
	{
		format_size[i] = i+1-FMT_ASCII;
		sprintf(format_nameX[i],"A%-d",i-FMT_ASCII);
		format_name[i] = (char *)format_nameX[i];
	}
        format_name[FMT_LAST+1] = (char *)0;

	
	/* Initialize the ID tables for 64-bit OS's. */

#if POINTERS_64_BITS_OS
	memset(_ibis_id_table, 0, sizeof(_ibis_id_table));
	memset(_ibis_rec_id_table, 0, sizeof(_ibis_id_table));
#endif

	first_time=0; /* dont do this again ! */
	
	return (1);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_group.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <stdio.h>
#include "ibisdeclares.h"

/* XIBIS Group Manipulation Routines */

static char *ValueList[]={
	ITYPE_GROUP,
	ITYPE_UNIT,
	ITYPE_FORMAT,
	ITYPE_LOCAL,
	ITYPE_ANY,
	(char *)0 /* end */
};

typedef enum {
	VALUE_GROUP=1,
	VALUE_UNIT,
	VALUE_FORMAT,
	VALUE_LOCAL,
	VALUE_ANY,
	VALUE_LAST=0 /* end */
} value_type;

static char *ModifyList[]={
	IGROUP_APPEND,
	IGROUP_INSERT,
	IGROUP_REMOVE,
	(char *)0 /* end */
};

typedef enum {
	MODIFY_APPEND=1,
	MODIFY_INSERT,
	MODIFY_REMOVE,
	MODIFY_LAST=0 /* end */
} modify_type;

#define CHECK_OPEN( ibis ) \
	if (!((ibis)->flags & FLAG_FILE_OPEN)) \
		return( IBIS_FILE_NOT_OPEN )

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

static int _GroupFind( ibis, type, name, group, grouplist )
XIBIS *ibis;
char *type;		/* "group", "unit" , "local", or null */
char *name;
XGROUP **group;
List **grouplist;
{
	XGROUP *grp=(XGROUP *)0;
	List *grplist=(List *)0;
	
	if (!type) type=ITYPE_ANY;
	
	switch (_i_keymatch(type, ValueList))
	{
		case VALUE_GROUP:
			grplist = ibis->groups;
			grp = _i_find_group(grplist, name);
			break;
		case VALUE_UNIT:
			grplist = ibis->units;
			grp = _i_find_group(grplist, name);
			break;
		case VALUE_LOCAL:
			grplist = ibis->locals;
			grp = _i_find_group(grplist, name);
			break;
		case VALUE_FORMAT:
			return IBIS_CANT_MODIFY_FORMAT;
			break;
		case VALUE_ANY:
			if ((grp = _i_find_group(ibis->locals, name)))
				grplist=ibis->locals;
			else if ((grp = _i_find_group(ibis->groups, name)))
				grplist=ibis->groups;
			else if ((grp = _i_find_group(ibis->units, name)))
				grplist=ibis->units;
			else if ((grp = _i_find_group(ibis->formats, name)))
				return IBIS_CANT_MODIFY_FORMAT;
			break;
		default:
			return IBIS_INVALID_TYPE;
			break;
	}

	if (!grp) return IBIS_GROUP_NOT_FOUND;

	*group = grp;
	*grouplist = grplist;

	return 1;
}

static void _SetUnit(group)
XGROUP *group;
{
	List *ent=group->columns->next;
	XCOL *col;

	for(ent=group->columns->next;ent;ent=ent->next)
	{
		col = (XCOL *)ent->value;
		if (col->unit)
			_i_delete_value(col->unit->columns, (list_value) col);
		col->unit = group;
	}
}

int IBISGroupNew(int ibis_id,char *type, char *name, int *cols, int ncols, 
		 char *expr )
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	return _IBISGroupNew(ibis, type, name, cols, ncols, expr);
}

/* This split is only so fetch_column_groups can access this function	*/
/* with a pointer instead of an ID.  The only instance, fortunately.	*/

int _IBISGroupNew(XIBIS* ibis, char* type, char* name, int* cols, int ncols, 
		  char* expr)
{
	XGROUP *grp=(XGROUP *)0;
	List *grplist=(List *)0;
	List **listptr;
	int col;
   _ibis_current_module="IBISGroupNew";
	
	CHECK_OPEN( ibis );

	if (! type) type=ITYPE_LOCAL;
	if (name && !_i_check_name( name )) return IBIS_INVALID_GRPNAME;

	if ((cols && ncols <=0) || (ncols==0 && !expr)) return IBIS_GROUP_IS_EMPTY;

	switch (_i_keymatch(type, ValueList))
	{
		case VALUE_GROUP:
			listptr = &ibis->groups;
			break;
		case VALUE_UNIT:
			listptr = &ibis->units;
			break;
		case VALUE_LOCAL:
			listptr = &ibis->locals;
			break;
		default:
			return IBIS_INVALID_TYPE;
			break;
	}

	if (!*listptr)
	{
		*listptr = _i_new_list((void(*)(int*))_i_free_group);
		if (!*listptr) return IBIS_MEMORY_FAILURE;
	}

	grplist = *listptr;
	if (_i_find_group(grplist, name))
		return IBIS_GROUP_ALREADY_EXISTS;
	
	if (expr) /* derive from expression */
	{
		grp = _i_group_construct(ibis, expr);
		if (!grp) return 0; /* group is empty */
		strcpy(grp->name, name);
	} 
	else if (ncols>0) /* build group manually */
	{
		/* sanity check */
		for (col=0;col<ncols;col++)
			CHECK_COLUMN( ibis, cols[col] )
		grp = _i_new_group( name );
		if (!grp) return IBIS_MEMORY_FAILURE;
		for (col=0;col<ncols;col++)
		    _i_append_value( grp->columns, (list_value)ibis->column[cols[col]-1]);
	}
	else return 0; /* empty group */


	/* install the group into the list */

	_i_append_value( grplist, (list_value)grp);
	if (grplist==ibis->units) _SetUnit( grp ); /* must be unique */
	if (grplist != ibis->locals)
		ibis->flags |= FLAG_MOD_LABELS;

	return _i_count_list(grp->columns);
}


int IBISGroupDelete(int ibis_id,char *type, char *name )
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	XGROUP *grp=(XGROUP *)0;
	List *grplist=(List *)0;
	int status;
   _ibis_current_module="IBISGroupDelete";
	
	CHECK_OPEN( ibis );

	if (!_i_check_name( name )) return IBIS_INVALID_GRPNAME;

	status=_GroupFind(ibis,type, name, &grp, &grplist);
	if (status != 1) return status;
	
	if (!grp) return IBIS_GROUP_NOT_FOUND;

	_i_delete_value( grplist, (list_value)grp );

	if (grplist != ibis->locals)
		ibis->flags |= FLAG_MOD_LABELS;

	return 1;
}



int IBISGroupModify(int ibis_id,char *type, char* name, char* mod, int* cols, 
		    int ncols )
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	XGROUP *grp=(XGROUP *)0;
	List *grplist=(List *)0;
	int col;
	int status;
   _ibis_current_module="IBISGroupModify";

	CHECK_OPEN( ibis );
	
	if (!_i_check_name( name )) return IBIS_INVALID_GRPNAME;
	status=_GroupFind(ibis,type, name, &grp, &grplist);
	if (status != 1) return status;
	
	switch (_i_keymatch(mod, ModifyList))
	{
		case MODIFY_APPEND:
			 /* install the columns into the group */
			for (col=0;col<ncols;col++)
				_i_append_value( grp->columns, (list_value)ibis->column[cols[col]-1]);
			break;
		case MODIFY_INSERT:
			 /* install the columns into the group */
			for (col=ncols-1;col>=0;col--)
				_i_insert_value( grp->columns, (list_value)ibis->column[cols[col]-1]);
			break;
		case MODIFY_REMOVE:
			 /* remove the columns from the group */
			for (col=0;col<ncols;col++)
				_i_delete_value( grp->columns, (list_value)ibis->column[cols[col]-1]);
			if (!grp->columns->next)
				_i_delete_value( grplist, (list_value) grp); /* empty group */
			break;
		default:
			return IBIS_INVALID_PARM;
			break;
	}
	

	if (grplist != ibis->locals)
		ibis->flags |= FLAG_MOD_LABELS;

	return 1;
}


/*
 *  find list of all groups containing specified column
 *  returns number of columns found or negative error status.
 */

int IBISGroupFind
(
 int ibis_id,
char *type,		  /* input: "format" "group" "unit" "local" or null=ALL */
int column,       /* input: column number to search for */
char *namelist,	  /* output: returned list of groupnames */
int sgroup,		  /* input: starting group in list */
int maxgroups,    /* input: max number of groups to return */
int length       /* input: length of namelist string array */
)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	XGROUP *grp=(XGROUP *)0;
	XCOL *col;
	List *grplist=(List *)0;
	List *lists[5];
	List **listptr=lists;
	List *ent;
	int index=0;
	int count=0;
	int key;
	int numgroups=0;
	char *nameptr=namelist;
	char grpname[100];
	static char *listname[]={
		ITYPE_FORMAT,
		ITYPE_UNIT,
		ITYPE_GROUP,
		ITYPE_LOCAL
	};
   _ibis_current_module="IBISGroupFind";

	CHECK_OPEN( ibis );	
	CHECK_COLUMN( ibis, column );
	col = ibis->column[column-1];
	
	if (!length && (maxgroups > 1))
		return IBIS_LENGTH_REQUIRED;
	
	if (sgroup<1) sgroup=1;
	
	/*
	 *  Determine which group lists to search
	 */
	

	if (! type) type=ITYPE_ANY;
	key = _i_keymatch(type, ValueList);

	switch (key)
	{
		case VALUE_FORMAT:
			grplist = ibis->formats;
			break;
		case VALUE_GROUP:
			grplist = ibis->groups;
			break;
		case VALUE_UNIT:
			grplist = ibis->units;
			break;
		case VALUE_LOCAL:
			grplist = ibis->locals;
			break;
		case VALUE_ANY:
			lists[0] = ibis->formats;
			lists[1] = ibis->units;
			lists[2] = ibis->groups;
			lists[3] = ibis->locals;
			lists[4] = (List *)0;
			break;
		default:
			return IBIS_INVALID_TYPE;
			break;
	}

	if (key!=VALUE_ANY)
	{
		if (!grplist) /* nothing there */
		{
			return 0;
		}
		lists[0]=grplist;
		lists[1]=(List *)0;
	}
	
	/*
	 *  Scan through set of groups and return group names.
	 */
	
	for (listptr=lists;*listptr;listptr++,index++)
	{
		grplist = *listptr;
		for (ent=grplist->next; ent; ent=ent->next)
		{
			grp = (XGROUP *)ent->value;
			if (_i_find_value(grp->columns, (list_value) col))
			{
				numgroups++;
				if (maxgroups>0) /* will return actual #retrieved */
				{
					if (numgroups>=sgroup && count<maxgroups)
					{
						count++;
						grpname[0]='\0';
						if (key==VALUE_ANY) /* use absolute addressing */
						{
								strcpy(grpname, listname[index]);
								strcat(grpname,":");
						}
						strcat(grpname, grp->name);
						if (length)
						{
							strncpy(nameptr, grpname, length);
							nameptr[length-1]='\0';
							nameptr += length;
						}
						else strcpy(nameptr,grpname);
					}
					if (count==maxgroups) return maxgroups; /* stop now */
				}
			}
		}
	}

	return maxgroups>0 ? count : numgroups;
}


/*
 *  Utility Routine for transferring all of the group-membership
 *  of a set of columns from one IBIS file to another.
 */

#define VALIDNAME "$__IBISGroupTransValid"

int IBISGroupTransfer(int in_id, int out_id, char *type, int *incols, 
		      int* outcols, int nc )
{
	XIBIS *in=IBIS_FETCH_ID(in_id);
	XIBIS *out=IBIS_FETCH_ID(out_id);
	int key;
	int status;
	int colindex[MAX_COL+1];
	int cols[MAX_COL];
	int ocols[MAX_COL];
	int ngcols,ngroups,group,i,nconvert=0;
	int inc,outc,*validcols;
	int use_in,use_out;
	char groupname[MAX_GRP_NAME+1],expression[200];
	static char *filetypes[]={
		IFILE_GROUPS,
		IFILE_UNITS,
		IFILE_FORMATS,
		IFILE_LOCALS
	};

   	_ibis_current_module="IBISGroupTransfer";

	CHECK_OPEN( in );
	CHECK_OPEN( out );
	
	if (! type) type=ITYPE_GROUP;
	key = _i_keymatch(type, ValueList);

	switch (key)
	{
		case VALUE_ANY:
		   status = IBISGroupTransfer( in_id, out_id,
		   	ITYPE_GROUP, incols, outcols, nc );
		   if (status < 0) return status;
		   status = IBISGroupTransfer( in_id, out_id,
		   	ITYPE_LOCAL, incols, outcols, nc );
		   if (status < 0) return status;
		   status = IBISGroupTransfer( in_id, out_id,
		   	ITYPE_UNIT, incols, outcols, nc );
		   return status;
		   break;
		case VALUE_LOCAL: case VALUE_GROUP: case VALUE_UNIT: break;
		default: return IBIS_INVALID_TYPE; /* cant transfer others */
	}
	
	ngroups = IBISFileGet( in_id, filetypes[key-1], groupname,  1, 0, MAX_GRP_NAME );
	if (ngroups<=0) return ngroups;

	use_in = (incols && incols[0]);
	use_out = (outcols && outcols[0]);
	
	/* create local group of "valid" input columns */
	if (use_in) validcols=incols;
	else
	{
		validcols = &colindex[0];
		for (i=0;i<nc;i++) colindex[i]=i+1;
	}
	IBISGroupDelete(in_id,ITYPE_LOCAL,VALIDNAME); /* just in case */
	status=IBISGroupNew(in_id,ITYPE_LOCAL,VALIDNAME,validcols,nc,0);
	if (status!=nc) goto end;

	/* create column index lookup table */
	memset(colindex,0,sizeof(colindex));
	for (i=0;i<nc;i++)
	{
		inc = use_in? incols[i] : i+1;
		outc = use_out? outcols[i] : i+1;
		colindex[inc]=outc;
	}

	/* Transfer the groups */
	for (group=0;group<ngroups;group++)
	{
		/* Find the valid columns in this group */
	        status = IBISFileGet( in_id, filetypes[key-1],
                       groupname,  group+1, 1, MAX_GRP_NAME );
		if (status!=1) goto end;
		
		sprintf(expression,"\'%s:%s\' & %s",type,groupname,VALIDNAME);
		ngcols=IBISColumnFind( in_id, ITYPE_ANY,
			 expression, cols, 1, MAX_COL);
		if (ngcols<0) 
		{
			status=ngcols;
			goto end;
		}
		else if (ngcols==0) continue;  /* empty set */
		
		/* compute the locations of new columns */
		for (i=0;i<ngcols;i++)
			ocols[i] = colindex[cols[i]];
		
		/* define the new group */
		status=IBISGroupNew( out_id, type, groupname, ocols, ngcols, 0);
		if (status!=ngcols) goto end;
		nconvert++;
	}

	status = nconvert;
end:
	/* clean up the temporary group */
	IBISGroupDelete(in_id,ITYPE_LOCAL,VALIDNAME);
	
	return status;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

int FTN_NAME2_(ibis_group_new, IBIS_GROUP_NEW) (int *ibis_id, char *type,
		char *name, int *cols, int *ncols, char *expr, ZFORSTR_PARAM)
#if 0
char *type;		/* "group", or "unit", etc */
#endif
{
   ZFORSTR_BLOCK
   char c_name[MAX_GRP_NAME+1];
   char c_type[MAX_TYPE_NAME+1];
   char *typeptr=(char *)0;
   char *c_expr;
   int length;
   int count;
   _ibis_current_module="IBISGroupNew";
  
   zsfor2c(c_name, MAX_GRP_NAME, name, &ibis_id, 6, 3, 2, expr);
   zsfor2c(c_type, MAX_TYPE_NAME, type, &ibis_id, 6, 2, 1, expr);
   if (c_type[0]) typeptr=c_type;
  
   zsfor2len( length, expr, &ibis_id, 6, 6, 3, expr);
   c_expr = (char *)malloc( length + 1);
   if (!c_expr)
   		return IBIS_MEMORY_FAILURE;
    zsfor2c( c_expr, length, expr, &ibis_id, 6, 6, 3, expr);
    if (!c_expr[0])
    {
    	free (c_expr);
    	c_expr=(char *)0;
    }

   count = IBISGroupNew( *ibis_id, typeptr, c_name, cols, *ncols, c_expr );
   
   if (c_expr) free(c_expr);
   
   return count;
}

void FTN_NAME2_(ibis_group_delete, IBIS_GROUP_DELETE)( int *ibis_id,
		char *type, char *name, int *status, ZFORSTR_PARAM )
#if 0
char *type;		/* "group", or "unit", etc */
#endif
{
   ZFORSTR_BLOCK
   char c_name[MAX_GRP_NAME+1];
   char c_type[MAX_TYPE_NAME+1];
   char *typeptr=(char *)0;
  
   zsfor2c(c_name, MAX_GRP_NAME, name, &ibis_id, 4, 3, 2, status);   
   zsfor2c(c_type, MAX_TYPE_NAME, type, &ibis_id, 4, 2, 1, status);
   if (c_type[0]) typeptr=c_type;
      
   *status = IBISGroupDelete( *ibis_id, typeptr, c_name );
   
   return;
}



void FTN_NAME2_(ibis_group_modify, IBIS_GROUP_MODIFY) ( int *ibis_id,
		char *type, char *name, char *mod, int *cols, int *ncols,
		int *status, ZFORSTR_PARAM )
#if 0
char *type;		/* "group", or "unit", etc */
#endif
{
   ZFORSTR_BLOCK
   char c_name[MAX_GRP_NAME+1];
   char c_type[MAX_TYPE_NAME+1];
   char c_mod[MAX_GRP_NAME+1];
   char *typeptr=(char *)0;
  
   zsfor2c(c_mod, MAX_GRP_NAME, mod, &ibis_id, 7, 4, 3, status);
   zsfor2c(c_name, MAX_GRP_NAME, name, &ibis_id, 7, 3, 2, status);
   zsfor2c(c_type, MAX_TYPE_NAME, type, &ibis_id, 7, 2, 1, status);
   if (c_type[0]) typeptr=c_type;

   *status = IBISGroupModify( *ibis_id, typeptr, c_name, c_mod, cols, *ncols );
   
   return;
}


int FTN_NAME2_(ibis_group_find, IBIS_GROUP_FIND) ( int *ibis_id, char *type,
	int *column, char *namelist, int *sgroup, int *maxgroups, ZFORSTR_PARAM)
#if 0
int *ibis_id;
char *type;		  /* input: "format" "group" "unit" "local" or null=ALL */
int *column;      /* input: column number to search for */
char *namelist;	  /* output: returned list of groupnames */
int *sgroup;	  /* input: starting group in list */
int *maxgroups;   /* input: max number of groups to return */
#endif
{
   ZFORSTR_BLOCK
   char c_type[MAX_TYPE_NAME+1];
   char *typeptr=(char *)0;
   char *tempbuf=(char *)0;
   int c_length=0;
   int length=0;
   int count;
   int num = *maxgroups;
   _ibis_current_module="IBISGroupFind";
  
   zsfor2c(c_type, MAX_TYPE_NAME, type, &ibis_id, 6, 2, 1, maxgroups); 
   if (c_type[0]) typeptr=c_type;
     
   /* Set up temp buffers, for string array conversion */

	if (num > 0)
	{
	   	zsfor2len(c_length, namelist, &ibis_id, 6, 4, 2, maxgroups);
		c_length += 1;
		tempbuf = (char *)calloc( 1L, (long)num*c_length );
		if (!tempbuf) 
		   return IBIS_MEMORY_FAILURE;
	}
   
   count = IBISGroupFind( *ibis_id, typeptr, *column,
   					 tempbuf, *sgroup, num, c_length );
   
   if ( num > 0) /* convert the string array */
   {
   		if (count > 0)
   		{
		   	if (num > 1) {
			   zsc2for_array(tempbuf, c_length, num, namelist, &length, &ibis_id, 6,4,2, maxgroups);
			}
			else {
			   zsc2for(tempbuf, 0, namelist, &ibis_id, 6,3,2, maxgroups);
			}
		}
	   free(tempbuf);
   }
 
   return count;
}

int FTN_NAME2_(ibis_group_transfer, IBIS_GROUP_TRANSFER) ( int *in_id,
		int *out_id, char *type, int *incols, int *outcols,
		int *nc, ZFORSTR_PARAM )
#if 0
char *type;		/* "group", or "unit", etc */
#endif
{
   ZFORSTR_BLOCK
   char c_type[MAX_TYPE_NAME+1];
   char *typeptr=(char *)0;
   int count;
   _ibis_current_module="IBISGroupTransfer";
  
   zsfor2c(c_type, MAX_TYPE_NAME, type, &in_id, 6, 3, 1, nc);
   if (c_type[0]) typeptr=c_type;
  
   count = IBISGroupTransfer( *in_id, *out_id, typeptr, incols, outcols, *nc );

   return count;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_rec.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_label.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include "errdefs.h"
#include <string.h>

/* IBIS Label Manipulation Routines */

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

int IBISLabelRemove( unit )
int unit;	/* input: VICAR file unit */
{
	int status=1;
	int maxlen,nelts;
	char key[MAX_GRP_NAME+1];
	char format[10];
	_ibis_current_module="IBISLabelRemove";


	/* Position file pointer at beginning of IBIS Property */
        status = zlinfo(unit,"property","property",format,&maxlen,
               &nelts,"property","ibis",NULL);

	if (status != 1) return 1; /* no such property */
	
	/* zap those sick little monkies! */       
	while (1)
	{
	    /* Get next keyword */
           status = zlninfo(unit,key,format,&maxlen,&nelts,NULL);
           if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
                (strcmp(key,"PROPERTY") == 0)) break;
	   else if (status != 1) break;
	
	   status=zldel( unit, "property",key, "property","ibis",NULL);
	   if (status != 1) break;
	}

	/* now zap the IBIS label */
	status=zldel( unit, "property","property", "property","ibis",NULL);

	
	return status;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_label_remove, IBIS_LABEL_REMOVE) ( int *unit , int *status)
#if 0
int *unit;	/* input: VICAR file unit */
int *status;
#endif
{
   *status = IBISLabelRemove( *unit );  
   return;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_row.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

#define _RBUFSIZE 200000

int IBISRowClear(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status;
	char *buffer=(char *)0;
   	_ibis_current_module="IBISRowClear";
  	
	CHECK_WRITE( ibis );
	
	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, srow+nrows-1);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, srow+nrows-1);
		if (status != 1) return status;
	}
	
	/* allocate memory for clearing rows */
	buffer = (char *)calloc( 1L, ibis->recsize);
	if (!buffer) return IBIS_MEMORY_FAILURE;

	/* formally clear the rows using dofile */
	status = ibis->rowmethod->dofile(ibis, zvwrit, buffer, srow, nrows, 0);
	if (status != 1) goto end;
	
end:
	if (buffer) free (buffer);
	return status;
}


int IBISRowNew(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int inc;
	int row, rows_now=0;
	int inrow;
	int size;
	char *buffer=(char *)0;
  	_ibis_current_module="IBISRowNew";
  	
	CHECK_WRITE( ibis );

	if (!srow) srow = ibis->nr+1;

	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, ibis->nr);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, ibis->nr);
		if (status != 1) return status;
	}
	
	/* let the file methods allocate file space */
	status = ibis->rowmethod->new( ibis, nrows );
	if (status != 1) return status;
		
	/* determine if we need to allocate memory for copying rows */
	size = ibis->rowmethod->getsize(ibis);
	inc = HOW_MANY(_RBUFSIZE, size );
	if (inc>ibis->nr +1 -srow) inc=ibis->nr+1-srow;
	if (inc > 0) 
	{
	   buffer = (char *)calloc( 1L, inc * size);
	   if (!buffer) return IBIS_MEMORY_FAILURE;
	}

	/* formally copy some rows down <nrows> using dofile */
	for (row = ibis->nr; row >=srow; row-=rows_now)
	{
		rows_now = (inc > row+1-srow ) ?  row+1-srow : inc;
		inrow = row+1-rows_now;
		status = ibis->rowmethod->dofile(ibis, zvread,
					 buffer, inrow, rows_now, 1);
		if (status != 1) goto end;
		status = ibis->rowmethod->dofile(ibis, zvwrit,
					 buffer, inrow+nrows, rows_now, 1);
		if (status != 1) goto end;
	}
	
	ibis->nr += nrows;
	ibis->flags |= FLAG_MOD_LABELS;
	
	if (ibis->flags & FLAG_AUTO_INIT)
		status = IBISRowClear(ibis_id,srow,nrows);
end:
	if (buffer) free (buffer);
	return status;
}


int IBISRowDelete(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status;
	int inc;
	int size;
	int row, rows_now=0;
	char *buffer=(char *)0;
  	_ibis_current_module="IBISRowDelete";
  	
	CHECK_WRITE( ibis );

	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, ibis->nr);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, ibis->nr);
		if (status != 1) return status;
	}

	/* determine if we need to allocate memory for copying rows */
	size = ibis->rowmethod->getsize(ibis);
	inc = HOW_MANY(_RBUFSIZE, size );
	if (srow + nrows + inc > ibis->nr) inc=ibis->nr + 1 -(srow+nrows);
	if (inc > 0)
	{
	   buffer = (char *)calloc( 1L, inc * size);
	   if (!buffer) return IBIS_MEMORY_FAILURE;
	}
	
	/* formally copy the rows up <nrows> using dofile */
	for (row = srow+nrows; row <= ibis->nr; row+=rows_now)
	{
		rows_now = (row + inc <= ibis->nr) ? inc : ibis->nr + 1 - row;
		status = ibis->rowmethod->dofile(ibis, zvread,
					 buffer, row, rows_now, 1);
		if (status != 1) goto end;
		status = ibis->rowmethod->dofile(ibis, zvwrit,
					 buffer, row-nrows, rows_now, 1);
		if (status != 1) goto end;
	}
			
	/* let the file methods deallocate file space */
	status = ibis->rowmethod->delete( ibis, nrows );
	if (status != 1) return status;		
	
	ibis->nr -= nrows;
	ibis->flags |= FLAG_MOD_LABELS;

end:
	if (buffer) free(buffer);	
	return status;
}



/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_row_clear, IBIS_ROW_CLEAR) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowClear(*ibis_id, *row, *nrows);
   return;
}

void FTN_NAME2_(ibis_row_new, IBIS_ROW_NEW) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowNew(*ibis_id, *row, *nrows);
   return;
}

void FTN_NAME2_(ibis_row_delete, IBIS_ROW_DELETE) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowDelete(*ibis_id, *row, *nrows);
   return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_signal.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <stdio.h>

/* Take the status of a previous XIBIS subroutine call and print	*/
/* the appropriate error message.  Abort the program if			*/
/* requested to.							*/


/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/


int IBISSignalU( unit, status, abend_flag )
int unit;
int status;
int abend_flag;
{
   static char msgbuf[200];

   static struct
   {
      char *key;
      char *text;
   } xi_messages[] =
   {
     {"MEMFAIL","Error allocating Memory; program error"},
     {"NOTIBIS","File unit is not an IBIS file"},
     {"ALRDYOPN","IBIS File is already opened"},
     {"RDONLY","IBIS File is opened read-only"},
     {"INVALMOD","Invalid mode for opening IBIS file"},
     {"INVALPAR","Invalid IBIS parameter"},
     {"NOTFOUND","IBIS Group not found"},
     {"LIMEXCD","Column Limit Exceeded"},
     {"INVALCPAR","Invalid Column Parameter"},
     {"NOTOPEN","IBIS File is not open"},
     {"OLDIBIS","Unsupported in old IBIS Files"},
     {"NOSUCHCOL","No such column exists"},
     {"LOCKCOL","Column is currently locked by a record"},
     {"INVALNAM","Group name has invalid characters"},
     {"MODFORMAT","Can't modify a FORMAT group"},
     {"INVALTYP","Invalid group TYPE for this routine"},
     {"GRPEXISTS","This group already exists"},
     {"EMPTYGRP","Defined group is empty (no columns)"},
     {"CANTTRANS","Unable to translate ASCII<->numeric"},
     {"INVALFMT","Invalid FORMAT specified"},
     {"LENGTHREQ","String length required"},
     {"LASTROW","Attempted to access past last row of file"},
     {"IMAGEDAT","File Contains Image Data; Can\'t extend"},
     {"NSNOTSET","You must set the pixel NS value before FORMAT"},
     {"NCREQD","The NC (dimension) value is required"},
     {0,0}		/* Terminator entry */
   };
   
   	if (status==1) return 0; /* not an error */
   	
	if (status < IBIS_BASE && status > IBIS_LAST )
	{
		char err_key[18];
		int msgcode=IBIS_BASE-status-1;
		
      		sprintf(msgbuf, "Exception in %s", _ibis_current_module);
   		zvmessage(msgbuf, "IBIS-GENERR");
		strcpy(err_key,"IBIS-");
		strcat(err_key,xi_messages[msgcode].key);
		
		zvmessage(xi_messages[msgcode].text, err_key);
		if (abend_flag) zabend();
	}
	else /* this is a VICAR signal */
	{
      		sprintf(msgbuf, "Exception in %s Reported by VICAR",
      				 _ibis_current_module);
   		zvmessage(msgbuf, "IBIS-VICMSG");
		zvsignal( unit, status, abend_flag );
	}
	return 0;
}

int IBISSignal( ibis_id, status, abend_flag )
int ibis_id;
int status;
int abend_flag;
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
 	return IBISSignalU( ibis->unit, status, abend_flag );
}


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2_(ibis_signal, IBIS_SIGNAL) 
(
  int *ibis_id,	/* In: unit number of file whose operation is being checked */
  int *status,	/* In: status being checked */
  int *abend_flag /* In: if TRUE, call abend() */
)
{
   IBISSignal(*ibis_id, *status, *abend_flag);

   return;
}

void FTN_NAME2_(ibis_signal_u, IBIS_SIGNAL_U)
(
  int *unit,	/* In: unit number of file whose operation is being checked */
  int *status,	/* In: status being checked */
  int *abend_flag /* In: if TRUE, call abend() */
)
{
   IBISSignalU(*unit, *status, *abend_flag);

   return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_file_old.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"


/**
 **   Implementation of XIBIS file-io methods for old files
 **   The only public (protected) routine is _i_ibis1_install_methods()
 **/


static int open_read( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "read", "convert", "off", NULL ) );
}

static int open_update( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "update", "convert", "off", NULL ));
}

static int open_write( ibis )
XIBIS *ibis;
{
	int status;
	int tabular=ibis->flags & FLAG_ORG_COLUMN;
	
	/*
	 * create the IBIS file using the
	 * parameters set by ibis->labmethod->pre_open()
	 *
	 */

	status = zvopen( ibis->unit, "op", "write", "o_format", "byte",
						"type", (tabular ? "tabular" : "graph1"),
						"u_ns", ibis->blocksize,
						"u_nl", ibis->numblocks,
						"host", ibis->hostfmt,
						 NULL) ;
	if (status != 1) return (status);

	/* Get rid of IBIS-2 Property label, if any */
	status = IBISLabelRemove(ibis->unit);
	if (status != 1) return (status);
	
	status = zvclose( ibis->unit, NULL );
	if (status != 1) return (status);
	
	return (open_update(ibis));
}

static int close_read( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}

static int close_write( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}


/*
 *  Method set-up
 */

void _i_ibis1_install_methods(XIBIS* ibis )
{

	if (!ibis) return;
	
	ibis->filemethod->init = _i_null_method;
	
	switch (ibis->flags & MASK_MODE)
	{
		case FLAG_MODE_READ:
			ibis->filemethod->open = open_read;
			ibis->filemethod->close = close_read;
			break;
		case FLAG_MODE_WRITE:
			ibis->filemethod->open = open_write;
			ibis->filemethod->close = close_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->filemethod->open = open_update;
			ibis->filemethod->close = close_write;
			break;
	}
	
	switch( ibis->flags & MASK_ORG)
	{
		case FLAG_ORG_ROW: /* graphics file */
			_i_install_grlabel_methods( ibis );
			break;
		case FLAG_ORG_COLUMN: /* standard ibis */
			_i_install_olabel_methods( ibis );
			break;
	}

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_file_new.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

/**
 **   Implementation of XIBIS file-io methods for IBIS-2 files.
 **   The only public (protected) routine is _i_ibis2_install_methods()
 **/


static int open_read( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "read", "cond", "binary","lab_act","  ", NULL ) );
}

static int open_update( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "update", "cond", "binary","lab_act","  ", NULL ));
}


static int open_write( ibis )
XIBIS *ibis;
{
	int status;
	
	/*
	 * create the IBIS file using the
	 * parameters set by ibis->labmethod->pre_open()
	 * The data is now put into the Binary label.
	 */
		
	status = zvopen( ibis->unit, "op", "write", 
						"cond", "binary",
						"lab_act","  ",
						"o_format", format_name[ibis->pix_fmt],
						"host", ibis->pix_host,
						"type", "tabular",
						"u_nlb",ibis->numblocks,
						"u_ns", ibis->ns,
						"u_nl", 1,     /* so that VAX won't complain */
						 NULL ) ;
	if (status != 1) return (status);

	/* 
	 * Since this is a new file, we kill off the "NL" label
	 * to indicate that there is no image, and get rid of
         * any inherited IBIS groups, etc.
	 */
	
        status = IBISLabelRemove(ibis->unit);
	if (status != 1) return (status);

	status = _i_install_numblocks(ibis, "nl", 0);
	if (status != 1) return (status);
	
	status = zvclose( ibis->unit, NULL );
	if (status != 1) return (status);
	
	return (open_update(ibis));
}



static int close_read( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}

static int close_write( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}


/*
 *  Method set-up
 */

void _i_ibis2_install_methods(XIBIS* ibis )
{

	ibis->filemethod->init = _i_null_method;
	
	if (ibis) switch (ibis->flags & MASK_MODE)
	{
		case FLAG_MODE_READ:
			ibis->filemethod->open = open_read;
			ibis->filemethod->close = close_read;
			break;
		case FLAG_MODE_WRITE:
			ibis->filemethod->open = open_write;
			ibis->filemethod->close = close_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->filemethod->open = open_update;
			ibis->filemethod->close = close_write;
			break;
	}
	
	_i_install_nlabel_methods( ibis );

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_io_col.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

/**
 **   Implementation of IBIS-2 I/O for Column-Oriented Files
 **   The only visible routine is _i_install_cfile_methods()
 **/

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inrow;
	int inc;
	int row;
	int size;
	int *index=(int *)0;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;

	/* create an offset index to column at location */
	index = (int *)calloc(1L, ibis->extent*sizeof(int));
	if (!index) return IBIS_MEMORY_FAILURE;
	for (i=0;i<ibis->nc;i++)
		index[ ibis->column[i]->offset ] = i;

	buffer = (char *)calloc(1L, I_TEMP_BUFSIZE );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan backwards through the file, copying columns as we go */

	for (i=ibis->extent-1; i>=0; i--)
	{
		if (!index[i]) continue;
		
		col = ibis->column[ index[i] ];
		size = ibis->format_size[ col->format ];
		
		/* 
		 * determine the number of rows we can process
		 * in memory
		 */
		inc = HOW_MANY( I_TEMP_BUFSIZE, size );
		if (inc > ibis->nr) inc = ibis->nr;
		
		for (row=ibis->nr; row > 0; row-=rows_now)
		{
			rows_now = row>inc ? inc : row;
			inrow = row+1-rows_now;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, index[i], inrow, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, index[i], inrow, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (index) free (index );
	if (buffer) free (buffer);
	return status;
}

static int _column_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int column,
  int srow,
  int nrows,
  int inc
)
{
	int status=1; 
	int unit=ibis->unit; 
	XCOL *col=ibis->column[column]; 
	int fsize=ibis->format_size[col->format]; 
	int blocksize = ibis->blocksize; 
	int offset= ((srow-1) * fsize) + col->offset*ibis->segment; 

	status = _i_process_contiguous(unit,function,buffer,offset,
		fsize*nrows,blocksize,ibis->recsize,inc);
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	XCOL *colm;
	int offset;
	int col;
	int status=1;

	for (col=0;col<ncols;col++)
	{
		colm = ibis->column[column+col];
		if (_i_find_space( ibis, &offset, size ))
		{
			colm->offset = offset;
			_i_allocate_space( ibis, colm->offset , size );
		}
		else break;
	}
	
	if (col<ncols) /* ran out of space */
	{
		_i_compute_new_offsets( ibis, column+col, ncols-col );
		ibis->numblocks = HOW_MANY(ibis->extent * ibis->segment, ibis->blocksize);
	
		/* Update System Label */
		status = _i_install_numblocks(ibis, ILABEL_NL, ibis->numblocks);
		if (status !=1) return status;
	}
	
	return status;
}

static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	XCOL **colm = ibis->column +column;
	XCOL *colp;
	int i;

	/*
	 *  We dont need to move file contents; just let the
	 *  Gap Manager keep track of available file space.
	 */
	
	for (i=0; i<ncols; i++,colm++)
	{
		colp= *colm;
		_i_deallocate_space( ibis, colp->offset,
			 ibis->format_size[ colp->format ] );
	}
	return 1;
}



static int _record_dofile
(
  XREC *record,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  int mod_row,
  int nrows
)
{
	int status=1;
	int col;
	int srow = record->top_row + mod_row;
	int ncols=record->num_cols;
	XIBIS *ibis=record->ibis;
	int *size = ibis->format_size;
	XCOL *colm;
	char **bufptr;
	
	bufptr = record->outbuffer;
	for (col=0; col<ncols; col++)
	{
		colm = record->column[col];
		status = _column_dofile( ibis, function, 
				bufptr[col] + size[ colm->format ] * mod_row,
				colm->id-1, srow, nrows,  1);
		if (status != 1) return status;
	}
	
	return status;
}

/*
 * Iteration loop for rows. This is only used externally for
 * clearing and transferring data, so we can effectively use
 * columns here and not ever tell the external routines what we did.
 */

static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int col;
	int ncols=ibis->nc;
	int *size = ibis->format_size;
	XMethod col_dofile=ibis->colmethod->dofile;

 	for (col=0;col<ncols;col++)
 	{
 		status = col_dofile( ibis, function, buffer, col, srow, nrows,  inc);
		if (status!=1) return status;
		if (inc) buffer += nrows*size[ibis->column[col]->format];
 	}
 	
	return status;
}


/*
 * Allocate space for <nrows> rows
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status=1;
	int new_segment;

	new_segment =  _i_useful_segment(ibis->nr + nrows);
	
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	if (ibis->blocksize % new_segment)
		new_segment = ALIGN_UP( new_segment, ibis->blocksize);
	
			/* Update blocksize */

	ibis->numblocks = HOW_MANY(ibis->extent * new_segment, ibis->blocksize);		
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);
	if (status !=1) return status;
	
	/* shuffle the data around */
	status=grow_segments( ibis, new_segment);
	if (status!=1) return status;
	
	return status;
}

/*
 * Deallocate space for <nrows> rows
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	/* not necessary to do anything */

	return 1;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->extent;
}


/**
 **  This is the routine that sets up file blocking
 **  and offset parameters for a new IBIS file
 **  organized by columns.
 **/

static int _file_init(XIBIS *ibis)
{	
	/* For column files, we compute things based on number of rows */
	
	_i_compute_blocksize(  ibis, ibis->nr );
	ibis->numblocks = HOW_MANY(ibis->extent * ibis->segment, ibis->blocksize);
	if (!ibis->numblocks) ibis->numblocks = 1;

	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


void _i_install_cfile_methods(XIBIS* ibis )
{
	ibis->colmethod->dofile = _column_dofile;
	ibis->colmethod->new = _columnnew;
	ibis->colmethod->delete = _columndelete;
	
	ibis->recmethod->dofile = _record_dofile;

	ibis->rowmethod->dofile = _row_dofile;
	ibis->rowmethod->new = _rownew;
	ibis->rowmethod->delete = _rowdelete;
	ibis->rowmethod->getsize = _rowsize;

	ibis->filemethod->init = _file_init;
	ibis->filemethod->clear = _file_clear;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_io_gr1.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>

/**
 **   Implementation of GRAPHICS-1 File I/O.
 **   The only public (protected) routine is _i_install_grfile_methods()
 **/

static int _row_dofile();	/* forward ref to shut up compiler */

static 
void memory_segment_move
(
  char *buffer,
  char *segbuf,
  int oldsegment,
  int newsegment,
  int rows_now
)
{
	char *inptr, *outptr;
	int i, oldinc, newinc;
	int size;
	
	if (oldsegment < newsegment)
	{
		inptr = buffer + (rows_now-1)*oldsegment;
		outptr = buffer + (rows_now-1)*newsegment;
		oldinc = -oldsegment;
		newinc = -newsegment;
		size = oldsegment;
	}
	else
	{
		inptr = buffer;
		outptr = buffer;
		oldinc = oldsegment;
		newinc = newsegment;
		size = newsegment;
	}
	
	/* reset the segments in memory to new locations */

	for (i=0;i<rows_now;i++)
	{
		memcpy( segbuf, inptr, size);
		memcpy( outptr, segbuf, size);
		inptr += oldinc;
		outptr += newinc;
	}
}

static int move_columns( XIBIS *ibis, int srccol, int destcol, int ncols )
{
	int status=1;
	int segment=ibis->segment;
	int rows_now=0;
	int inc;
	int i;
	int row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;
	char *inptr,*outptr;

	if (srccol==destcol) return 1;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	 
	if (segment > I_TEMP_BUFSIZE)inc = 1;
	else inc = HOW_MANY( I_TEMP_BUFSIZE, segment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * segment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, ncols*4 );
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=1; row<=ibis->nr; row+=rows_now)
	{
		rows_now = ibis->nr + 1 - row < inc ? ibis->nr + 1 - row : inc;
	
		/* read in rows  */
		status = _row_dofile(ibis, zvread, buffer, row, rows_now, 1);
		if (status != 1) goto end;
		
		/* shift columns to new locations */
		inptr = buffer + 4*(srccol-1);
		outptr = buffer + 4*(destcol-1);
		for (i=0;i<rows_now;i++)
		{
			memcpy( segbuf, inptr, ncols*4);
			memcpy( outptr, segbuf, ncols*4);
			inptr += segment;
			outptr += segment;
		}
	
		/* write out rows */
		status = _row_dofile(ibis, zvwrit, buffer, row, rows_now, 1);
		if (status != 1) 
		{
			/* this is bad news */
			return status;
		}
	}

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int start_row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	if (newsegment > I_TEMP_BUFSIZE)
	{
		inc = 1;
	}
	else inc = HOW_MANY( I_TEMP_BUFSIZE, newsegment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * newsegment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, newsegment );
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=ibis->nr; row > 0; row-=rows_now)
	{
		start_row = row + 1 - inc;
		if (start_row < 1) start_row =1;
		rows_now = row + 1 - start_row;
	
		/* read in with old segmentation */
		ibis->segment = oldsegment;
		status = _row_dofile(ibis, zvread, buffer, start_row, rows_now, 1);
		if (status != 1) goto end;

		memory_segment_move(buffer,segbuf,oldsegment,newsegment,rows_now);
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, start_row, rows_now, 1);
		if (status != 1) goto end;
	}
	ibis->segment = newsegment;

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}

static 	int shrink_segments( XIBIS *ibis, int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	if (oldsegment > I_TEMP_BUFSIZE)
	{
		inc = 1;
	}
	else inc = HOW_MANY( I_TEMP_BUFSIZE, oldsegment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * oldsegment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, oldsegment );
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=1; row <= ibis->nr; row+=rows_now)
	{
		rows_now = (row + inc > ibis->nr) ?  ibis->nr+1- row: inc;
	
		/* read in with old segmentation */
		ibis->segment = oldsegment;
		status = _row_dofile(ibis, zvread, buffer, row, rows_now, 1);
		if (status != 1) goto end;

		memory_segment_move(buffer,segbuf,oldsegment,newsegment,rows_now);
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, row, rows_now, 1);
		if (status != 1) goto end;
	}
	ibis->segment = newsegment;

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}


/* The column routines use this common interface */

static int _column_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int column,
  int srow,
  int nrows,
  int inc
)
{
	int status=1; 
	int unit=ibis->unit; 
	int size = ibis->format_size[ FMT_REAL ]; 
	int segment = ibis->segment; 
	int blocksize = ibis->blocksize; 
	int coffset = column*size; 
	int offset = ((srow-1) * segment) + coffset; 
	int end_offset = ((srow+nrows-1) * segment) + coffset; 

	/* loop for each column element */
	for (; offset<end_offset; offset += segment) 
	{ 
		/* 
		 * Grab a single column element. We know
		 * that the element is completely contained
		 * within a single line, since SIZE=4, BLOCKSIZE=512.
		 */

			status = (*function)( unit, buffer, 
				 "line", 1+(offset/blocksize), 
				 "samp", 1+(offset%blocksize), 
				 "nsamps", size, NULL); 
			if (status != 1) return status; 
			if (inc) buffer += size; 
	} 
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	int newnc = ibis->nc+ncols;
	int status;
	int new_segment;

	/* determine the new offsets and the extent of file */
	
	ibis->extent = newnc*4;
	new_segment =  ibis->extent;
	ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
	
	status = _i_install_numblocks(ibis, "nl", ibis->numblocks);
	if (status !=1) return status;

	status=grow_segments( ibis, new_segment);
	if (status!=1) return status;

	status = move_columns( ibis, column+1, column+1+ncols, ibis->nc - column );

	return status;
}


static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	int newnc = ibis->nc-ncols;
	int status;
	int new_segment;


	status = move_columns( ibis, column+1+ncols, column+1, newnc - column );
	if (status != 1) return status;

	/* determine the new offsets and the extent of file */
	
	ibis->extent = newnc*4;
	new_segment =  ibis->extent;

	status=shrink_segments( ibis, new_segment);
	if (status!=1) return status;

			/* Update numblocks */

	ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
	status = _i_install_numblocks(ibis, "nl", ibis->numblocks);
	if (status !=1) return status;

	return status;
}


static int _record_dofile(
  XREC *record,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  int mod_row,
  int nrows
)
{
	XIBIS *ibis=record->ibis;
	XCOL *colm; 
	int status=1; 
	int unit=ibis->unit; 
	int blocksize = ibis->blocksize; 
	int nc=record->num_cols;
	int top = (record->top_row + mod_row -1);
	int segment=ibis->segment;
	int topoff = top * segment;
	int row,col;
	int offset; 
	int foffsets[MAX_COL];
	int size = ibis->format_size[ FMT_REAL ]; 
	char *buffers[MAX_COL];
	char *tempbuf,*inbuf,*outbuf;
	int cur_off,rows_now,rows_left,row_inc,size_now;
	int writing = (function==zvwrit);
	int have_gaps = (record->flags & FLAG_REC_GAPS);
	
	/* precompute values that don't change for rows */
	for (col=0;col<nc;col++)
	{
		colm = record->column[col];
		foffsets[col] = topoff + (colm->id-1)*size;
		buffers[col] = record->outbuffer[col] + mod_row*size;
	}

	/* Determine buffer setup */	
	row_inc = HOW_MANY( I_TEMP_BUFSIZE, segment );
	if (row_inc > nrows) row_inc = nrows;
	tempbuf = (char *)calloc(1L, row_inc * segment);
	if (!tempbuf) return IBIS_MEMORY_FAILURE;

	cur_off = topoff;
	for (rows_left=nrows;rows_left;rows_left-=rows_now)
	{
		rows_now = rows_left > row_inc? row_inc : rows_left;
		size_now = rows_now*segment;		
		if (!writing || have_gaps)
		{
			status = _i_process_contiguous(unit,zvread,tempbuf,cur_off,
			  size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		
		/* Pure Memory Transfer from/to temporary buffer */
		for (row=0; row<rows_now; row++)
		{
			/* loop for a single row of record */
			for (col=0; col<nc; col++) 
			{ 
				/* Transfer a single column element */
				offset=foffsets[col] - cur_off;
				inbuf = writing ? buffers[col] : tempbuf+offset;
				outbuf = writing ? tempbuf+offset: buffers[col];
				/* All elements are 4 bytes so no memcpy */
				*(int *)outbuf = *(int *)inbuf;
				buffers[col] += size;
				foffsets[col] += segment;
			} 
		}
		if (writing)
		{
			status = _i_process_contiguous(unit,zvwrit,tempbuf,cur_off,
			 size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		cur_off += size_now;
	}

end:
	free (tempbuf);	
	return status;
}



static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...),
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int unit=ibis->unit;
	int segment=ibis->segment;
	int blocksize=ibis->blocksize;
	int offset=(segment*(srow-1));

	status = _i_process_contiguous(unit,function,buffer,offset,
		nrows*segment,blocksize,ibis->recsize,inc);
	
	return status;
}


/*
 * Allocate space for <nrows> rows. We don't need
 * to shuffle any data for row-oriented files.
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr + nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);

	return status;
}

/*
 * Deallocate space for <nrows> rows. We don't
 * need to shuffle data for row-oriented files.
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr - nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);

	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->segment;
}



static int _file_init(XIBIS *ibis)
{
	int segment;

	/* figure out the blocksizes, etc */
	ibis->blocksize = OLD_BLKSIZE;
	ibis->recsize = ibis->blocksize;
	ibis->ns = ibis->recsize;
	segment = ibis->nc * 4;
	ibis->segment = segment;
	ibis->numblocks = HOW_MANY( (segment * ibis->nr), OLD_BLKSIZE);
	
	return 1;
}


static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


int _i_install_grfile_methods(XIBIS *ibis )
{
	ibis->colmethod->dofile = _column_dofile;
	ibis->colmethod->new = _columnnew;
	ibis->colmethod->delete = _columndelete;
	
	ibis->recmethod->dofile = _record_dofile;

	ibis->rowmethod->dofile = _row_dofile;
	ibis->rowmethod->new = _rownew;
	ibis->rowmethod->delete = _rowdelete;
	ibis->rowmethod->getsize = _rowsize;

	ibis->filemethod->init = _file_init;
	ibis->filemethod->clear = _file_clear;
	
	return 1;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_io_old.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

/**
 **   Implementation of IBIS-1 File I/O.
 **   The only public (protected) routine is _i_install_ofile_methods()
 **/

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inrow;
	int inc;
	int row;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;


	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	inc = HOW_MANY( I_TEMP_BUFSIZE, 4 );
	if (inc > ibis->nr) inc = ibis->nr;
	buffer = (char *)calloc(1L, inc*4 );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan backwards through the file, copying columns as we go */
	for (i=ibis->nc-1; i>=0; i--)
	{		
		col = ibis->column[ i ];
		
		for (row=ibis->nr; row > 0; row-=rows_now)
		{
			rows_now = row>inc ? inc : row;
			inrow = row+1-rows_now;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, i, inrow, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, i, inrow, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (buffer) free (buffer);
	return status;
}

static 	int shrink_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;


	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	inc = HOW_MANY( I_TEMP_BUFSIZE, 4 );
	if (inc > ibis->nr) inc = ibis->nr;
	buffer = (char *)calloc(1L, inc*4 );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan forwards through the file, copying columns as we go */
	for (i=0; i<ibis->nc; i++)
	{		
		col = ibis->column[ i ];
		
		for (row=1; row <= ibis->nr; row+=rows_now)
		{
			rows_now = (row + inc > ibis->nr) ?  ibis->nr+1- row: inc;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, i, row, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, i, row, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (buffer) free (buffer);
	return status;
}



/* 
 *  Move ncols columns from column srccol to column destcol.
 */

static int move_columns( XIBIS *ibis, int srccol, int destcol, int ncols)
{
	int status=1;
	int unit=ibis->unit;
	int inc;
	int block;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	int srcblock;
	int destblock;
	int nblocks=ncols*blocks_per_col;
	char *blockbuf;
	
	if (srccol<destcol) /* go backwards */
	{		
		inc = -1;
		srcblock=(srccol+ncols-1)*blocks_per_col+1;
		destblock=(destcol+ncols-1)*blocks_per_col+1;
	}
	else if (srccol > destcol) /* go forwards */
	{
		inc=1;
		srcblock=(srccol-1)*blocks_per_col+2;
		destblock=(destcol-1)*blocks_per_col+2;
	}
	else return 1; /* src==dest */

	
	blockbuf= (char *)malloc( ibis->blocksize);
	if (!blockbuf) return IBIS_MEMORY_FAILURE;

	/* transfer the blocks */
	for (block = 0; block<nblocks; block++)
	{
		status = zvread( unit, blockbuf, "line", srcblock, NULL);
		if (status !=1) break;
		status = zvwrit( unit, blockbuf, "line", destblock, NULL);
		if (status !=1) break;
		srcblock += inc;
		destblock += inc;
	}
	
	free (blockbuf);
	return status;
}


static int _column_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int column,
  int srow,
  int nrows,
  int inc
)
{
	int status=1; 
	int unit=ibis->unit; 
	int offset = OLD_BLKSIZE+((column* ibis->segment) + (srow - 1))*4; 


        status=_i_process_contiguous(unit,function,buffer,offset,
                4*nrows,OLD_BLKSIZE,OLD_BLKSIZE,inc);

	return status;

}

static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	int status; 
	int newnc = ibis->nc+ncols;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	
	/* 
	 * update the file parameters first, as we'll be
	 * writing to new places in file.
	 */

	ibis->numblocks = 1 + newnc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;

	status = move_columns( ibis, column+1, column+1+ncols, ibis->nc - column );
	
	return status;
}


static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	int status; 
	int newnc = ibis->nc-ncols;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	ibis->numblocks = 1 + newnc * blocks_per_col;
	
	status = move_columns( ibis, column+1+ncols, column+1, newnc - column );
	if (status != 1) return status;

	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;
		
	return status;
}


static int _record_dofile
(
  XREC *record,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  int mod_row,
  int nrows
)
{
	int status=1;
	int col;
	int offset = 4*mod_row;
	int srow = record->top_row + mod_row;
	int ncols=record->num_cols;
	char **bufptr;
	XIBIS *ibis=record->ibis;
	
	bufptr = record->outbuffer;
	for (col=0; col<ncols; col++)
	{
		status = _column_dofile( ibis, function, 
				bufptr[col] + offset,
				record->column[col]->id-1, srow, nrows,  1);
		if (status != 1) return status;
	}
	
	return status;
}


/*
 * Iteration loop for rows. This is only used externally for
 * clearing and transferring data, so we can effectively use
 * columns here and not ever tell the external routines what we did.
 */

static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int col;
	int ncols=ibis->nc;
	int *size = ibis->format_size;
	XMethod col_dofile=ibis->colmethod->dofile;

 	for (col=0;col<ncols;col++)
 	{
 		status = col_dofile( ibis, function, buffer,
				 col, srow, nrows,  inc);
		if (status!=1) return status;
		if (inc) buffer += nrows*size[ibis->column[col]->format];
 	}
 	
	return status;
}



/*
 * Allocate space for <nrows> rows
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status=1;
	int new_segment;
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;
	
	/* work out the new segment size */

	blocks_per_col = HOW_MANY((ibis->nr+nrows), segblock);
	new_segment = blocks_per_col * segblock ;
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	/* Update blocksize */

	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;
	
	/* shuffle the data around */

	status=grow_segments( ibis, new_segment);
	if (status!=1) return status;
	
	return status;
}

/*
 * Deallocate space for <nrows> rows
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	int status=1;
	int new_segment;
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;
	
	/* work out the new segment size */

	blocks_per_col = HOW_MANY((ibis->nr-nrows), segblock);
	new_segment = blocks_per_col * segblock ;
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	/* shuffle the data around */

	status=shrink_segments( ibis, new_segment);
	if (status!=1) return status;
	
	/* Update blocksize */

	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;
	
	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->nc * 4;
}



static int _file_init(XIBIS *ibis)
{
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;

	/* figure out the blocksizes, etc */
	ibis->blocksize = OLD_BLKSIZE;
	ibis->recsize = ibis->blocksize;
	blocks_per_col = HOW_MANY(ibis->nr, segblock);
	ibis->segment = blocks_per_col * segblock ;
	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	
	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 2);
}

int _i_install_ofile_methods(XIBIS* ibis )
{
	ibis->colmethod->dofile = _column_dofile;
	ibis->colmethod->new = _columnnew;
	ibis->colmethod->delete = _columndelete;
	
	ibis->recmethod->dofile = _record_dofile;

	ibis->rowmethod->dofile = _row_dofile;
	ibis->rowmethod->new = _rownew;
	ibis->rowmethod->delete = _rowdelete;
	ibis->rowmethod->getsize = _rowsize;

	ibis->filemethod->init = _file_init;
	ibis->filemethod->clear = _file_clear;
	
	return 1;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_io_row.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>

/**
 **   Implementation of IBIS for Row-Oriented Files
 **   The only public (protected) routine is _i_install_rfile_methods()
 **/

static int _row_dofile();	/* forward ref to shut up compiler */

static 	int grow_segments( XIBIS *ibis, int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int i;
	int start_row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;
	char *inptr,*outptr;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	if (newsegment > I_TEMP_BUFSIZE)
	{
		inc = 1;
	}
	else inc = HOW_MANY( I_TEMP_BUFSIZE, newsegment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * newsegment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, oldsegment);
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=ibis->nr; row > 0; row-=rows_now)
	{
		start_row = row + 1 - inc;
		if (start_row < 1) start_row =1;
		rows_now = row + 1 - start_row;
	
		/* read in with old segmentation */
		ibis->segment = oldsegment;
		status = _row_dofile(ibis, zvread, buffer, start_row, rows_now, 1);
		if (status != 1) return status;
		
		/* spread out segments in memory to new locations */
		inptr = buffer + (rows_now-1)*oldsegment;
		outptr = buffer + (rows_now-1)*newsegment;
		for (i=0;i<rows_now;i++)
		{
			memcpy( segbuf, inptr, oldsegment);
			memcpy( outptr, segbuf, oldsegment);
			inptr -= oldsegment;
			outptr -= newsegment;
		}
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, start_row, rows_now, 1);
		if (status != 1) 
		{
			/* this is bad news */
			return status;
		}
	}

end:	
	ibis->segment = newsegment;
	free (buffer);
	return status;
}


static int _column_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int column,
  int srow,
  int nrows,
  int inc
)
{
	int status=1; 
	int unit=ibis->unit; 
	XCOL *col=ibis->column[column]; 
	int fsize = ibis->format_size[ col->format ]; 
	int size; 
	int segment = ibis->segment; 
	int blocksize = ibis->blocksize; 
	int coffset = col->offset; 
	int offset = ((srow-1) * segment) + coffset; 
	int end_offset = ((srow+nrows-1) * segment) + coffset; 
	int bytes_left, samp,line_left,offset1; 

	/* loop for each column element */
	for (; offset<end_offset; offset += segment) 
	{ 
		/* loop for a single column element */
		offset1=offset;
		for(bytes_left=fsize; bytes_left; bytes_left-=size) 
		{ 
			samp = (offset1%blocksize); 
			line_left = blocksize-samp; 
			size = line_left>bytes_left ? bytes_left : line_left; 
			status = (*function)( unit, buffer, 
				 "line", 1+offset1/blocksize, 
				 "samp", 1+samp, 
				 "nsamps", size, NULL); 
			if (status != 1) return status; 
			if (inc) buffer += size; 
			offset1 += size;
		} 
	} 
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	XCOL *colm;
	int offset;
	int col;
	int status = 0;

	for (col=0;col<ncols;col++)
	{
		/*
		 * Fill in the gaps, first, and then
		 * extend the file, if need be.
		 */
		colm = ibis->column[column+col];
		if (_i_find_space( ibis, &offset, size ))
		{
			colm->offset = offset;
			_i_allocate_space( ibis, colm->offset , size );
		}
		else break;
	}

	if (col<ncols) /* we ran out of space */
	{
		int new_segment;

		/* determine the new offsets and the extent of file */
		
		_i_compute_new_offsets( ibis, column+col, ncols-col );
				
		/*
		 * We have to shuffle file data around by resetting
		 * the 'segment' property. No need
		 * to call _i_allocate_space here, as no gap entries exist
		 * for these columns. Unless we're lucky enough that
		 * new_segment is a divisor of blocksize, we'll have
		 * to make new_segment a multiple of blocksize.
		 */
			 
		new_segment =  _i_useful_segment( ibis->extent );
		if (ibis->blocksize % new_segment)
			new_segment = ALIGN_UP( new_segment, ibis->blocksize);
		
				/* Update VICAR NL (NS cant be modified) */
	
		ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
		
		status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);
		if (status !=1) return status;

		status=grow_segments( ibis, new_segment);
		if (status!=1) return status;
	}

	return status;
}



static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	XCOL **colm = ibis->column +column;
	XCOL *colp;
	int i;

	/*
	 *  We dont need to move file contents; just let the
	 *  Gap Manager keep track of available file space.
	 */
	
	for (i=0; i<ncols; i++,colm++)
	{
		colp= *colm;
		_i_deallocate_space( ibis, colp->offset,
			 ibis->format_size[ colp->format ] );
	}
	return 1;
}


static int _record_dofile
(
  XREC *record,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  int mod_row,
  int nrows
)
{
	XIBIS *ibis=record->ibis;
	XCOL *colm; 
	int status=1; 
	int unit=ibis->unit; 
	int *fsize = ibis->format_size; 
	int blocksize = ibis->blocksize; 
	int nc=record->num_cols;
	int top = (record->top_row + mod_row -1);
	int segment=ibis->segment;
	int topoff = top * segment;
	int row,col;
	int offset; 
	int foffsets[MAX_COL];
	int sizes[MAX_COL];
	char *buffers[MAX_COL];
	char *tempbuf,*inbuf,*outbuf;
	int cur_off,rows_now,rows_left,row_inc,size_now;
	int writing = (function==zvwrit);
	int have_gaps = (record->flags & FLAG_REC_GAPS);
	
	/* precompute values that don't change for rows */
	for (col=0;col<nc;col++)
	{
		colm = record->column[col];
		foffsets[col] = topoff + colm->offset;
		sizes[col] = fsize[ colm->format ];
		buffers[col] = record->outbuffer[col] + mod_row*sizes[col];
	}

	/* Determine buffer setup */	
	row_inc = HOW_MANY( I_TEMP_BUFSIZE, segment );
	if (row_inc > nrows) row_inc = nrows;
	tempbuf = (char *)calloc(1L, row_inc * segment);
	if (!tempbuf) return IBIS_MEMORY_FAILURE;

	cur_off = topoff;
	for (rows_left=nrows;rows_left;rows_left-=rows_now)
	{
		rows_now = rows_left > row_inc? row_inc : rows_left;
		size_now = rows_now*segment;		
		if (!writing || have_gaps)
		{
			status = _i_process_contiguous(unit,zvread,tempbuf,cur_off,
			  size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		
		/* Pure Memory Transfer from/to temporary buffer */
		for (row=0; row<rows_now; row++)
		{
			/* loop for a single row of record */
			for (col=0; col<nc; col++) 
			{ 
				/* Transfer a single column element */
				offset=foffsets[col] - cur_off;
				inbuf = writing ? buffers[col] : tempbuf+offset;
				outbuf = writing ? tempbuf+offset: buffers[col];
				memcpy(outbuf, inbuf,sizes[col]);
				buffers[col] += sizes[col];
				foffsets[col] += segment;
			} 
		}
		if (writing)
		{
			status = _i_process_contiguous(unit,zvwrit,tempbuf,cur_off,
			 size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		cur_off += size_now;
	}

end:
	free (tempbuf);	
	return status;
}


static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...),
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int unit=ibis->unit;
	int segment=ibis->segment;
	int blocksize=ibis->blocksize;
	int offset=(segment*(srow-1));


        status = _i_process_contiguous(unit,function,buffer,offset,
           nrows*segment,blocksize,ibis->recsize,inc);
	
	return status;
}


/*
 * Allocate space for <nrows> rows. We don't need
 * to shuffle any data for row-oriented files.
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr + nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);

	return status;
}

/*
 * Deallocate space for <nrows> rows. We don't
 * need to shuffle data for row-oriented files.
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr - nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);

	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->segment;
}



/**
 **  This is the routine that sets up file blocking
 **  and offset parameters for a new IBIS file
 **  organized by rows.
 **/


static int _file_init(XIBIS *ibis)
{
	/* For row files, we compute things based on column extent */

	_i_compute_blocksize( ibis,  ibis->extent );
	ibis->numblocks = HOW_MANY( ibis->nr*ibis->segment, ibis->blocksize);	
	if (!ibis->numblocks) ibis->numblocks = 1;
	
	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


void _i_install_rfile_methods(XIBIS* ibis )
{
	ibis->colmethod->dofile = _column_dofile;
	ibis->colmethod->new = _columnnew;
	ibis->colmethod->delete = _columndelete;
	
	ibis->recmethod->dofile = _record_dofile;

	ibis->rowmethod->dofile = _row_dofile;
	ibis->rowmethod->new = _rownew;
	ibis->rowmethod->delete = _rowdelete;
	ibis->rowmethod->getsize = _rowsize;

	ibis->filemethod->init = _file_init;
	ibis->filemethod->clear = _file_clear;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_labl_new.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include "errdefs.h" /* Defines CANNOT_FIND_KEY, etc */
#include <string.h>
#include <stdio.h>

/*
 *  Private label methods for IBIS-2, read & write
 *  The only visible (protected) method is _i_install_nlabel_methods
 *   which is called by install_label_methods. All others are called by 
 *   method pointers.
 */


#define GET_IBIS( vname, pname )  \
   do { \
     status = zlget( ibis->unit, "property",(pname),(char*)(vname),	\
				"property","ibis",   NULL); \
		if (status!=1) goto end; \
   } while (0)

#define PUT_IBIS( pname, pvalue, pformat, pcount ) \
   do { \
		zldel( ibis->unit, "property",(pname),"property","ibis",NULL); \
		status=zladd( ibis->unit, "property",(pname),(char*)(pvalue), "property","ibis", \
		              "format",(pformat),"nelement",(pcount), NULL); \
		if (status!=1) goto failure; \
   } while (0)


static int _flush_write(XIBIS *ibis);	/* forward ref to shut up compiler */

/*
 *  Standard routine for fetching column lists, etc, out of IBIS label.
 */


static int fetch_property_array( int unit, char *prop, int *cptr, int *numelts )
{
	int status=1;
	char format[10];
	int maxlen;
	
	status=zlinfo( unit, "property", prop,  format,
	         &maxlen, numelts, "property","ibis", NULL);
	      
	if (status == CANNOT_FIND_KEY )
	{
		*numelts = 0;
		return (1); /* Not a failure */
	}
	else if (status!=1)
	      return status;
	
	status = zlget( unit, "property",prop,(char*)cptr, "property","ibis",
    			"nelement", *numelts,  NULL);
	
	return status;
}


/*
 *  Install format 'fmt' into a subset of 'columns' array
 *  using the set 'cols' of column #'s.
 */

static int fetch_column_formats( XIBIS *ibis, int fmt, int *cols, int ncols )
{
	XCOL **columns=ibis->column;
	int status=1;
	int i;

	/*
	 *  Set the column base format
	 */

	for (i=0; i<ncols; i++)
		_i_attach_format_to_column(ibis, fmt, cols[i]-1);
	
	/*
	 *  If ASCII, add on the ASCII_LEN values to format
	 */
	
	if (fmt==FMT_ASCII)
	{
		int num;
		int *size;
		
		size = (int*)calloc( 1L, sizeof(int)*ncols);
		if (!size) return (IBIS_MEMORY_FAILURE);
		
		status=fetch_property_array( ibis->unit, IFILE_ASCII_LEN, size, &num );
		if (status!=1) 
		{
			free(size);
			return status;
		}
		
		for (i=0; i<num; i++)
				columns[ cols[i]-1 ]->format += size[i];
		
		free( size );
	}
	
	return status;
}


/*
 *  Get File organization parameters
 */


static int fetch_file_properties( XIBIS *ibis )
{
	int status=1;
	char org[MAX_VALUE_NAME+1];
	char fmtstr[MAX_VALUE_NAME+1];
	char pixformat[MAX_VALUE_NAME+1];

	status = zvget( ibis->unit,  ILABEL_NL, &ibis->numblocks,
						         ILABEL_HOST, ibis->hostfmt,
						         ILABEL_INTFMT, ibis->intfmt,
						         ILABEL_REALFMT, ibis->realfmt,
						         "recsize", &ibis->recsize,
								 "nl", &ibis->nl,
								 "ns", &ibis->ns,
								 "format", pixformat,
								 "host", ibis->pix_host,
						          NULL ) ;
	if (status != 1) goto end;

	GET_IBIS( &ibis->nr,		IFILE_NR );
	GET_IBIS( &ibis->nc,		IFILE_NC );
	GET_IBIS( &ibis->segment,	IFILE_SEGMENT );
	GET_IBIS( &ibis->blocksize,	IFILE_BLOCKSIZE );
	GET_IBIS( org,				IFILE_ORG );
	GET_IBIS( fmtstr,			IFILE_FMT_DEFAULT );

	/* Get IBIS Subtype */
	
   	status = zlget( ibis->unit, "property",IFILE_TYPE,ibis->type,
				"property","ibis",   NULL);
	if (status != 1) 
	{
		/* That's ok, this property is optional */
		ibis->type[0] = '\0';
		status = 1;
	}

	/* set up condition flags and format codes */
	ibis->flags &= ~MASK_ORG;
	ibis->flags |= (_i_strcmp_nocase(org,IORG_ROW)) ? FLAG_ORG_COLUMN : FLAG_ORG_ROW;
	if (ibis->nl>0) ibis->flags |= FLAG_FILE_IMAGE;
	ibis->default_fmt = _i_IBISFormatCode(fmtstr);
	ibis->pix_fmt = _i_IBISFormatCode(pixformat);

end:
	return (status);
}


static int fetch_column_groups( XIBIS *ibis, char *type)
#if 0
char *type; /* UNIT, GROUP, etc */
#endif
{
	int status=1;
	int grp;
	char format[20];
	int unit = ibis->unit;
	int grouplen;
	int numgroups=0;
	int cols[MAX_COL];
	int num_col;
	char *grouplist=(char *)0;
	char *groupptr;
	char listname[MAX_GRP_NAME+1]; /* "UNITS", "GROUPS", etc */
	char prefix[MAX_GRP_NAME+1];   /* "UNIT_", "GROUP_", etc */
	char groupname[MAX_GRP_NAME+1];
	char grpsuffix[MAX_GRP_NAME+1];

	/* set up names */
	strcpy(listname, type);
	strcat(listname,"s");
	strcpy(prefix,type);
	strcat(prefix,"_");

	/*
	 *  Figure out if and how many groups we've got.
	 */
	
	status=zlinfo( unit, "property", listname,  format,
	          &grouplen, &numgroups, "property","ibis", NULL);
	if (status == CANNOT_FIND_KEY || !numgroups)
	{
		return (1); /* Not a failure, just no groups */
	}
	else if (status!=1)
	      return status;

	/*
	 *  Get the list of group names
	 */

	grouplen++;
	grouplist = (char *)calloc( 1L, numgroups * grouplen );
	if (!grouplist) return( IBIS_MEMORY_FAILURE );
	
    status = zlget( unit, "property",listname, grouplist, "property","ibis",
    			"nelement", numgroups,"ulen",grouplen,  NULL);
    if (status!=1) goto end;

	/*
	 *  Group Loop: for each group, get list of columns, and install 'em.
	 */
		   
	for (grp = 0,groupptr=grouplist; grp < numgroups; grp++,groupptr+=grouplen)
	{
		/* build the group property name */
		strcpy(groupname, prefix);
		sprintf(grpsuffix,"%-d",grp+1);
		strcat(groupname,grpsuffix);
		
		status = fetch_property_array( unit, groupname, cols, &num_col );
		if (status != 1) goto end;
		
		if (num_col) /* found some columns with this group */
		{
			num_col = _IBISGroupNew( ibis, type, groupptr, cols, num_col, 0 );
			if (num_col < 0 ) return num_col;
		}
	}

	
end:
	if (grouplist) free (grouplist);
	return (status);
}



/*
 *  Column stuff: get the format codes, groups, and units
 *   from the IBIS property label.
 */

static int fetch_column_properties( XIBIS *ibis )
{
	int status=1;
	int buffer[MAX_COL];
	int i,size;
	int fmt,num_fmt;
	XCOL **col;

	/*
	 *  Format Loop For non-defaulted formats
	 */
		   
	for (fmt = FMT_BYTE; fmt<=FMT_ASCII; fmt++)
	{
		if (fmt != ibis->default_fmt)
		{
			status = fetch_property_array( ibis->unit,
			     format_label[fmt], buffer, &num_fmt );
			if (status != 1) goto end;
			
			if (num_fmt) /* found some columns with this format */
			{
				status = fetch_column_formats( ibis, fmt,  buffer, num_fmt);
				if (status != 1) goto end;
			}
		} 
	}

	/*
	 *  Now set formats for the defaulted columns. We first
	 *  scan for columns not yet set, and add then to the
	 *  list of columns to to set to the default.
	 */

	col = ibis->column;
	for (i=0,num_fmt=0; i<ibis->nc; i++,col++)
		if (!(*col)->format) 
			buffer[num_fmt++]=i+1;
	if (num_fmt)
	{
		status = fetch_column_formats( ibis,
					 ibis->default_fmt,  buffer, num_fmt);
		if (status != 1) goto end;
	}

	/* Get COFFSET array and compute extent */
	status=fetch_property_array( ibis->unit, IFILE_COFFSET, buffer, &num_fmt );
 	if (status != 1) goto end;
	col = ibis->column;
	ibis->extent=0;
	for (i=0;i<ibis->nc; i++,col++)
	{
		(*col)->offset = buffer[i];
		size=ibis->format_size[(*col)->format];
		if (buffer[i]+size > ibis->extent) ibis->extent = buffer[i]+size;
	}

	status = fetch_column_groups( ibis, "group");
	if (status != 1) goto end;
	
	status = fetch_column_groups( ibis, "unit");
	if (status != 1) goto end;
	
end:
	return (status);
}

static int put_column_groups
(
  XIBIS *ibis,
  char *type, 		/* "UNIT", "GROUP", etc */
  List *grouplist  	/* ibis->units, ibis->groups, etc */
)
{
	int num_col=0;
	int status;
	char group_name[MAX_GRP_NAME+1];
	char group_suffix[MAX_GRP_NAME+1];
	char *groupnames;
	char listname[MAX_GRP_NAME+1]; /* "UNITS", "GROUPS", etc */
	char prefix[MAX_GRP_NAME+1];   /* "UNIT_", "GROUP_", etc */
	char format[20],key[40];
	XCOL *col;
	XGROUP *group;
	List *ent;
	int cols[MAX_COL];
	int ngroups;
	int grp,noldgrp=0;
	int stat;
	int maxlen,numelts,preflen;
	int grouplen=MAX_GRP_NAME + 1;


	/* set up names */
	strcpy(listname, type);
	strcat(listname,"s");
	strcpy(prefix,type);
	strcat(prefix,"_"); preflen=strlen(prefix);

	/* Get rid of old <GROUPS> property */
	zldel( ibis->unit, "property",listname,"property","ibis",NULL); 

	/* count the number of IBIS <GROUP> properties */
        status = zlinfo(ibis->unit,"property","property",format,&maxlen,
               &numelts,"property","ibis",NULL);
	while (1)
	{
	    /* Get next keyword and increment if it is a <GROUP>*/
           status = zlninfo(ibis->unit,key,format,&maxlen,&numelts,NULL);
           if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
                (strcmp(key,"PROPERTY") == 0)) break;
	   else if (status != 1) break;
	   if (_i_strncmp_nocase(key,prefix,preflen)==0) noldgrp++;
	}

	/* get rid of any old <GROUP>_<nnn> groups lying around */
	for (stat=1,grp=1;stat==1 && grp<=noldgrp;grp++)
	{
		strcpy(group_name, prefix);
		sprintf(group_suffix,"%-d",grp);
		strcat(group_name, group_suffix);
		stat=zldel( ibis->unit, "property",group_name,
                                        "property","ibis",NULL);
	}

	if (!grouplist) return 1;
	ngroups = _i_count_list(grouplist);
	if (!ngroups) return 1;

	groupnames = (char *)calloc(1L, ngroups*grouplen);
	if (!groupnames) return IBIS_MEMORY_FAILURE;

	/* build the whole list first */
	for (ent=grouplist->next,grp=0; grp<ngroups; grp++)
	{
		group = (XGROUP *)ent->value;
		strcpy(groupnames+grouplen*grp, group->name);
		ent = ent->next;
	}
	status=zladd( ibis->unit, "property",listname,groupnames, "property","ibis",
	              "format","string","nelement",ngroups,"ulen", grouplen, NULL); 
	if (status!=1) goto failure;
	
	/* now write out each group column list */

	for (grouplist=grouplist->next,grp=1; grouplist; grouplist=grouplist->next)
	{
		group = (XGROUP *)grouplist->value;
		num_col=0;
		for (ent=group->columns->next; ent; ent=ent->next)
		{
			col = (XCOL *)ent->value;
			cols[num_col++] = (col->id);
		}
		strcpy(group_name, prefix);
		sprintf(group_suffix,"%-d",grp++);
		strcat(group_name, group_suffix);
		PUT_IBIS( group_name, cols, "int", num_col );
	}
	
	return status;

failure:
	return status;
}

static int put_column_formats(XIBIS *ibis, int format)
{
	int num_fmt = 0;
	int i,status=1,def;
	char *fmt_name;
	XCOL *col;
	XCOL **colm;
	XGROUP *flist;
	List *ent;
	int buffer[MAX_COL];

	def = ibis->default_fmt;
	
	/*
	 * Get the list of columns with specified format. We
	 * use this to iteratively construct the buffer of columns.
	 */
	fmt_name = format_name[ format<FMT_ASCII? format : FMT_ASCII ];
	if (!(flist = _i_find_group( ibis->formats, fmt_name )))
		return 0;

	/*
	 * Now set up the buffer of columns to write out to property.
	 */
	
	if (format==FMT_ASCII)
		for (ent=flist->columns->next; ent; ent=ent->next)
		{
			col = (XCOL *)ent->value;
			/* 
			 * be careful! If the default format is specific ASCII
			 * then it shouldn't appear in the ASCII_LEN label.
			 */
			if ( col->format!=def )
				buffer[num_fmt++] = (col->id);
		}
	else
		for (ent=flist->columns->next; ent; ent=ent->next)
			buffer[num_fmt++] = (((XCOL *)ent->value)->id);

	/*
	 *  Write out columns to property label, unless this is default fmt.
	 */
	
	if (num_fmt)
	{
		if (format != def)
			PUT_IBIS( format_label[format], buffer, "int", num_fmt );
		
		if (format==FMT_ASCII) /* At least include ASCII_LEN label */
		{
			colm = ibis->column;
			for (i=0; i<num_fmt; i++)
			{
					buffer[i] = colm[buffer[i]-1]->format - FMT_ASCII;
			}
			PUT_IBIS( IFILE_ASCII_LEN, buffer, "int", num_fmt);
		}
	}
	
	return status;

failure:
	return status;
}



/*
 *  This is the main entry point for pre-processing of a new
 *  IBIS file.
 */

static int _pre_open_write( XIBIS *ibis )
{
	int status=1;
	int i;
	char *fmt=ibis->fmt;
	fmt_type format;
	int fmt_size=ibis->fmt_len;

	/* sanity check */
	
	if (ibis->nc > MAX_COL)
	{
		ibis->nc = 0; /* no columns allocated yet */
		return IBIS_COLUMN_LIMIT_EXCEEDED;
	}

	/* create column structure */

	status = _i_init_column_array( ibis );
	if (status != 1) return (status);
	
	/* set the specified or defaulted format types */

	if (fmt && *fmt)
	{
		for (i=0;i<ibis->nc; i++)
		{
			format = _i_IBISFormatCode(fmt);
			if (format < FMT_BYTE)
				return IBIS_INVALID_FORMAT;
			_i_attach_format_to_column(ibis,format,i);
			fmt += fmt_size;
		}
		free (ibis->fmt);
		ibis->fmt=(char *)0;
		ibis->fmt_len=IFMT_SIZE;
	}
	else
		for (i=0;i<ibis->nc; i++)
			_i_attach_format_to_column(ibis,ibis->default_fmt,i);

	/* Install file methods */

	switch( ibis->flags & MASK_ORG )
	{
		case FLAG_ORG_ROW:
			_i_install_rfile_methods( ibis );
			break;
		case FLAG_ORG_COLUMN: 
			_i_install_cfile_methods( ibis );
			break;
	}

	if (!*ibis->hostfmt)
		strcpy( ibis->hostfmt,NATIVE_HOST_LABEL);
	if (!*ibis->pix_host)
		strcpy( ibis->pix_host,NATIVE_HOST_LABEL);

	_i_trans_init( ibis ); /* set up translation buffers */

	_i_compute_new_offsets( ibis, 0, ibis->nc );
	
	ibis->filemethod->init( ibis ); /* implementation specific */

	return (status);
}


/*
 *  This is the main post-processing routine for a new IBIS
 *  file opened for reading. We need to read in all of the
 *  IBIS properties, set up ROW/COLUMN file methods, and
 *  set up the column offsets, formats, groups, and units.
 */


static int _post_open_read( XIBIS *ibis )
{
	int status=1;

	status=fetch_file_properties( ibis );
	if (status!=1) return status;

	_i_trans_reset( ibis ); /* set up translation buffers */

	switch( ibis->flags & MASK_ORG )
	{
		case FLAG_ORG_ROW:
			_i_install_rfile_methods( ibis );
			break;
		case FLAG_ORG_COLUMN: 
			_i_install_cfile_methods( ibis );
			break;
	}
	
	status = _i_init_column_array( ibis );
	if (status !=1 ) return (status);
	
	status = fetch_column_properties( ibis );
	if (status !=1 ) return (status);


	return (status);
}

static int _post_open_write( XIBIS *ibis )
{
	int status;

	/*
	 *  Just in case this brand-new file inherited some groups and units
	 *  from the primary image, this should be reflected in the ibis
	 *  structure, so that they may be used & manipulated.
	 */

	status = fetch_column_groups( ibis, "group");
	if (status != 1 && status!=NO_SUCH_PROPERTY) return status;
	
	status = fetch_column_groups( ibis, "unit");
	if (status != 1 && status!=NO_SUCH_PROPERTY) return status;

	status = _flush_write( ibis ); /* make sure the label is set up right */
	
	return status;
}


	/*
	 * Write label stuff out to property label 
	 */

static int _flush_write( XIBIS *ibis )
{
	int status=1;
	int i,fmt;
	int buffer[MAX_COL];
	XCOL **col;
	List *ent;
	XGROUP *group;

	/*
	 *  Prior to installing formats, get rid of all old format labels
	 *  and types which might have been inherited from a primary input image.
	 */
	zldel( ibis->unit, "property",IFILE_TYPE, "property","ibis",NULL);
	for (fmt=FMT_BYTE; fmt<=FMT_ASCII; fmt++)
			zldel( ibis->unit, "property",format_label[fmt],"property","ibis",NULL);
		
	/*
	 *  Update IBIS property label
	 */
	if (ibis->type[0] && ibis->type[0]!=' ') 
		PUT_IBIS(IFILE_TYPE,	ibis->type, "string" , 1);
	PUT_IBIS(IFILE_NR,		&ibis->nr,   "int" , 1);
	PUT_IBIS(IFILE_NC,		&ibis->nc,   "int" , 1);
	PUT_IBIS(IFILE_ORG,	(ibis->flags & FLAG_ORG_ROW? "ROW" : "COLUMN"), "string" , 1);
	PUT_IBIS(IFILE_FMT_DEFAULT,	format_name[ ibis->default_fmt ], "string" , 1);

	
	/*
	 * Install Formats. Use "formats" list to write out property label.
	 */
	for (ent=ibis->formats->next; ent; ent=ent->next)
	{
		group=(XGROUP *)ent->value;
		fmt = _i_IBISFormatCode( group->name );
		if (fmt != ibis->default_fmt)
			put_column_formats( ibis, fmt );
	}
	put_column_formats( ibis, ibis->default_fmt );


	/*
	 * Install user-defined groups and units
	 */

	status = put_column_groups( ibis, "group", ibis->groups);
	if (status != 1) return status;
	status = put_column_groups( ibis, "unit", ibis->units);
	if (status != 1) return status;
			

	/*
	 *  Column offsets (absolute)
	 */

	col = ibis->column;
	for (i=0;i<ibis->nc; i++,col++)
		buffer[i] = (*col)->offset;
	PUT_IBIS(IFILE_SEGMENT,	&ibis->segment, "int", 1);
	PUT_IBIS(IFILE_BLOCKSIZE, &ibis->blocksize, "int", 1);
	PUT_IBIS(IFILE_COFFSET,	buffer, "int", ibis->nc);


	/* Update System Label */

	status = _i_install_numblocks(ibis,ILABEL_NL, ibis->numblocks);
	if (status!=1) return status;

 	status=zldel( ibis->unit, "system",ILABEL_HOST,NULL);
	status=zladd( ibis->unit, "system",ILABEL_HOST,ibis->hostfmt,"format","string", NULL);
	if (status!=1) return status;
	status=zldel( ibis->unit, "system",ILABEL_INTFMT,NULL);
	status=zladd( ibis->unit, "system",ILABEL_INTFMT,ibis->intfmt,"format","string", NULL);
	if (status!=1) return status;
	status=zldel( ibis->unit, "system",ILABEL_REALFMT,NULL);
	status=zladd( ibis->unit, "system",ILABEL_REALFMT,ibis->realfmt,"format","string", NULL);
	if (status!=1) return status;
	status=zldel( ibis->unit, "system",ILABEL_BTYPE,NULL);
	status=zladd( ibis->unit, "system",ILABEL_BTYPE, IBTYPE_IBIS,"format","string", NULL);
	if (status!=1) return status;

    return status;
    
failure:
	return status;
}


int _i_install_nlabel_methods( XIBIS *ibis )
{
	int status=1;

	if (ibis) switch (ibis->flags & MASK_MODE)
	{
		case FLAG_MODE_READ:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _i_null_method;
			break;
		case FLAG_MODE_WRITE:
			ibis->labmethod->pre_open = _pre_open_write;
			ibis->labmethod->post_open = _post_open_write;
			ibis->labmethod->flush = _flush_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _flush_write;
			break;
	}
	
	return (status);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_labl_old.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <ctype.h>
#include <zvproto.h>

/*
 *  Private label methods for IBIS-1 files, read & write
 *  The only visible (protected) method is _i_install_olabel_methods.
 *   All others are called by method pointers.
 */
 
static int _flush_write(XIBIS *);	/* forward ref to shut up compiler */

/*
 *  Set up the file info prior to opening
 */

static int _pre_open_write( XIBIS *ibis )
{
	int col;
	int format;
	char *fmt=ibis->fmt;
	int status=1;
	int fmt_size=ibis->fmt_len;

	/* sanity check */
	if (ibis->nc > MAX_OLD_COL)
	{
		ibis->nc = 0; /* no columns allocated yet */
		return IBIS_FILE_OLD_IBIS;
	}

	/* create column structure */
	status = _i_init_column_array( ibis );
	if (status != 1) return status;

	/*
	 *  Unless someone actually manually changed the host-format
	 *  (using IBISFileUnit,IBISFileSet), we hardwire this to VAX-VMS host.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy(ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	/* set the specified or defaulted format types */

	if (fmt && *fmt) 
	{
		for (col=0;col<ibis->nc; col++)
		{
			/* check that size is ok */
	
			format = _i_IBISFormatCode(fmt);
			if (format < FMT_BYTE)
				return IBIS_INVALID_FORMAT;
			if (ibis->format_size[format]>4)
				return IBIS_FILE_OLD_IBIS;
	
			_i_attach_format_to_column(ibis,format,col);
			fmt += fmt_size;
		}
		free (ibis->fmt);
		ibis->fmt = (char *)0;
		ibis->fmt_len=IFMT_SIZE;
	}
	else for (col=0;col<ibis->nc; col++)
		_i_attach_format_to_column(ibis,ibis->default_fmt,col);

	ibis->extent = ibis->nc * 4;

	/* Install file methods */

	status = _i_install_ofile_methods( ibis );
	if (status != 1) return status;

	
	ibis->filemethod->init( ibis );

	return (1);
}


/*
 *  Work out details from file info
 */

static int _post_open_read( XIBIS *ibis )
{
	int status=1;
	int unit = ibis->unit;
	int col;
	int nr_value;
	char format[20];

	/* 
	 * get the IBIS-1 information from the label
	 */

	zvget( ibis->unit, "recsize", &ibis->recsize,
			   "nl", &ibis->numblocks,
			   "format", format,
			    NULL );

	_i_install_ofile_methods( ibis );

	/*
	 * somehow, somewhere, someone might have tried
	 * to port the old IBIS routines. If so, the file
	 * would most likely not be in VAX-VMS format.
	 *
	 * We hardwire the format here to VAX-VMS host.
	 *
	 * If this is a "bogus-IBIS-port" file, a client program can always set
	 * the "host" label item using IBISFileUnit, IBISFileSet.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy( ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	ibis->blocksize = ibis->recsize;
	
	/* sanity check for IBIS-1 */
	
	if (ibis->blocksize !=OLD_BLKSIZE || tolower(format[0]) != 'b')
		return(IBIS_FILE_IS_NOT_IBIS);

	/*
	 *  The old format contains one line of FULL, whose first sample
	 *  is the number of rows per column, in the ibis->host's FULL format.
	 */

	status = zvread( unit, &nr_value, "line", 1, "nsamps", sizeof(int), NULL);
	zvtrans((int*)&ibis->trans[FMT_FULL].intrans, &nr_value, &ibis->nr, 1 );
	
	/* work out the rest of the parameters from this */
	
	ibis->segment = ALIGN_UP(ibis->nr, OLD_BLKSIZE/4);
	ibis->nc = ((ibis->numblocks-1) * ibis->blocksize)/(ibis->segment*4) ;
	ibis->extent = ibis->nc * 4;
	
	/* Set up the columns */
	
	status = _i_init_column_array( ibis );
	if (status != 1) return status;
	for (col=0;col<ibis->nc;col++)
	{
		status=_i_attach_format_to_column(ibis,ibis->default_fmt,col);
		if (status!=1) return status;
	}

	return (status);
}

static int _post_open_write( XIBIS *ibis )
{
	int status;
	
	status = _flush_write( ibis ); /* make sure the label is set up right */
	
	return status;
}


	/*
	 * Write label stuff out to property label 
	 */


static int _flush_write( XIBIS *ibis )
{
	int status=1;
	int nl=ibis->numblocks;
	int nr_value;
	int size=ibis->format_size[FMT_FULL];
	
	ibis->extent = ibis->nc * 4;

	/*
	 * Write column-size parameter to first record of file;
	 *  The old format is one line of FULL, whose first sample
	 *  is the number of rows per column, in Host FULL format.
	 */

	zvtrans((int*)&ibis->trans[FMT_FULL].outtrans, &ibis->nr, &nr_value, 1);
	status = zvwrit( ibis->unit, &nr_value, "line", 1, "nsamps", size, NULL);
	if (status != 1) return status;

	/*
	 * Make sure NL is in sync with struct
	 */

	status=zldel( ibis->unit, "system","nl",NULL);
	status=zladd( ibis->unit, "system","nl",&nl,"format","int", NULL);

	return (status);
}



int _i_install_olabel_methods(XIBIS *ibis )
{
	int status=1;

	if (ibis) switch ( ibis->flags & MASK_MODE )
	{
		case FLAG_MODE_READ:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _i_null_method;
			break;
		case FLAG_MODE_WRITE:
			ibis->labmethod->pre_open = _pre_open_write;
			ibis->labmethod->post_open = _post_open_write;
			ibis->labmethod->flush = _flush_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _flush_write;
			break;
	}
	
	return (status);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_labl_gr1.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <ctype.h>

/*
 *  Private label methods for old GRAPHICS-1 files, read & write
 *  The only visible (protected) method is _i_install_grlabel_methods.
 *   All others are called by method pointers.
 */
 

/*
 *  Set up the file info prior to opening
 */

static int _pre_open_write( XIBIS *ibis )
{
	int col;
	int format;
	char *fmt=ibis->fmt;
	int status=1;
	int fmt_size=ibis->fmt_len;

	/* sanity check */
	if (ibis->nc <1) return IBIS_NC_REQUIRED;
	if (ibis->nc > MAX_OLD_COL)
	{
		ibis->nc = 0; /* no columns allocated yet */
		return IBIS_FILE_OLD_IBIS;
	}
	ibis->extent = ibis->nc * 4;

	/* create column structure */
	status = _i_init_column_array( ibis );
	if (status != 1) return status;

	/*
	 *  Unless someone actually manually changed the host-format
	 *  (using IBISFileUnit,IBISFileSet), we hardwire this to VAX-VMS host.
	 *  We need to do this here so that the "format_size" is known.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy(ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	/* set the specified or defaulted format types */

	if (fmt && *fmt) 
	{
		for (col=0;col<ibis->nc; col++)
		{
			/* check that size is ok */
	
			format = _i_IBISFormatCode(fmt);
			if (format < FMT_BYTE)
				return IBIS_INVALID_FORMAT;
			if (ibis->format_size[format]>4)
				return IBIS_FILE_OLD_IBIS;
	
			_i_attach_format_to_column(ibis,format,col);
			fmt += fmt_size;
		}
		free (ibis->fmt);
		ibis->fmt = (char *)0;
		ibis->fmt_len=IFMT_SIZE;
	}
	else for (col=0;col<ibis->nc; col++)
		_i_attach_format_to_column(ibis,ibis->default_fmt,col);

	/* Install file methods */

	status = _i_install_grfile_methods( ibis );
	if (status != 1) return status;

	
	ibis->filemethod->init( ibis );

	return (1);
}


/*
 *  Work out details from file info
 */

static int _post_open_read( XIBIS *ibis )
{
	int status=1;
	int col;
	char format[20];

	/*
	 *  The GRAPHICS-1 format has no NC parameter; this must have
	 *  been specified by the user. Complain if not.
	 */

	if (ibis->nc <1) return IBIS_NC_REQUIRED;
	
	_i_install_grfile_methods( ibis );

	/*
	 * somehow, somewhere, someone might have tried
	 * to port the old IBIS routines. If so, the file
	 * would most likely not be in VAX-VMS format.
	 *
	 * We hardwire the format here to VAX-VMS host.
	 *
	 * If this is a "bogus-IBIS-port" file, a client program can always set
	 * the "host" label item using IBISFileUnit, IBISFileSet.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy( ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	/* 
	 * get the IBIS-1 information from the label
	 */

	zvget( ibis->unit, "recsize", &ibis->recsize,
			   "nl", &ibis->numblocks,
			   "format", format,
			    NULL );
	ibis->blocksize = ibis->recsize;
	
	/* sanity check for IBIS-1 */
	
	if (ibis->blocksize !=OLD_BLKSIZE || tolower(format[0]) != 'b')
		return(IBIS_FILE_IS_NOT_IBIS);

	
	/* work out the sizing parameters from NC */
	
	ibis->segment = ibis->nc * ibis->format_size[ FMT_REAL ];
	ibis->extent = ibis->nc * 4;
	ibis->nr =  (ibis->numblocks * ibis->blocksize) / ibis->segment;
	
	/* Set up the columns */
	
	status = _i_init_column_array( ibis );
	if (status != 1) return status;
	for (col=0;col<ibis->nc;col++)
	{
		status=_i_attach_format_to_column(ibis,ibis->default_fmt,col);
		if (status!=1) return status;
	}

	return (status);
}

	/*
	 * Write label stuff out to property label 
	 */


static int _flush_write( XIBIS *ibis )
{
	int status=1;
	
	ibis->extent = ibis->nc * 4;

	/*
	 * There is no column-size parameter for GRAPHICS-1	 :-(
	 */

	/*
	 * Make sure NL is in sync with struct
	 */

	status = _i_install_numblocks(ibis, "nl",ibis->numblocks);

	return (status);
}


int _i_install_grlabel_methods(XIBIS * ibis )
{
	int status=1;

	if (ibis) switch ( ibis->flags & MASK_MODE )
	{
		case FLAG_MODE_READ:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _i_null_method;
			break;
		case FLAG_MODE_WRITE:
			ibis->labmethod->pre_open = _pre_open_write;
			ibis->labmethod->post_open = _i_null_method;
			ibis->labmethod->flush = _flush_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _flush_write;
			break;
	}
	
	return (status);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mthd_null.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

int _i_null_method()
{
	/* do nothing */
	return (1);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_lists.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Linked List Utilities for XIBIS module
 */

#include <stdlib.h>
#include <string.h>
#include "ibislists.h"

static void _free_element( List *elt )
{
	if (elt)
	{
		if (elt->destruct)
			(*elt->destruct)(elt->value);
		free (elt);
	}
}

/**
 **  Creation Method for lists
 **   A list consists of a list of values or pointers
 **   to objects. If the objects should be destroyed
 **   along with the list, the destruction method
 **   should be specified here.
 **/

List* _i_new_list(void (*kill_method)())
{
	List *newlist;
	
	newlist = (List *)calloc( 1L, sizeof(List) );
	if (newlist && kill_method)
	   newlist->destruct = kill_method;
	return ( newlist );
}

void _i_free_list(List *list )
{

	if (list)
	{
		List *ent=list->next;
		List *next;
		while (ent)
		{
			next = ent->next;
			_free_element( ent );
			ent = next;
		}

		free (list);
	}
}

/**
 **  Destruction of single list entry
 **   -- if the value has a destruction
 **      method, destroy it, too.
 **/



List* _i_find_value(List *list, list_value value)
{
	while(list && list->value!=value)
		list=list->next;

	return (list);
}

/**
 ** count the number of values in list
 **/
 
int _i_count_list(List *list)
{
	int count=0;
	
	if (list)
		for(count=0,list=list->next; list; list=list->next)
			count++;

	return (count);
}


/**
 **  Install a new value into the list.
 **  if there is a destruction method,
 **  install that, too.
 **/


void _i_insert_value(List* list, list_value value)
{
	register List *head, *ent;

	/* make sure its not already in list */
	
	if (!_i_find_value(list, value))
	{	
		/* initialize list element */
		ent = (List *)malloc(sizeof(List));
		ent->value=value;
		ent->destruct = list->destruct;  /* destruction method */
		ent->next=list->next;
		ent->prev=(List *)0;
		
		if ((head=list->next)) head->prev=ent;
		
		list->next=ent;
	}
}

void _i_append_value(List* list, list_value value)
{
	register List *ent;

	/* make sure its not already in list */
	
	if (!_i_find_value(list, value))
	{	
		/* initialize list element */
		ent = (List *)malloc(sizeof(List));
		ent->value=value;
		ent->destruct = list->destruct;  /* destruction method */
		
		/* find end of list */
		while (list->next) list=list->next;
		
		ent->prev=list;
		ent->next=(List *)0;		
		list->next=ent;
	}
}


void _i_delete_value(List* list, list_value value)
{
	register List *elt,*next,*prev;

	/* make sure its in list */
	if ((elt=(List*)_i_find_value(list,value)))
	{	
		/* unlink from chain */
		
		/* Handle special case if first element */
		if (elt==list->next)
		{
			list->next=elt->next;
			elt->prev=(List *)0;
		}
		else
		{
			next=elt->next;
			prev=elt->prev;
			if (next) next->prev=prev;
			if (prev) prev->next=next;
		}
		
		_free_element(elt);	
	}
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_xcol.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include "ibisdeclares.h"

XCOL *_i_new_column(void)
{
	XCOL *column;
	column = (XCOL *)calloc( 1L, sizeof(XCOL) );
	return (column);
}

void _i_free_column(XCOL *col)
{
	if (!col) return;
	if (col->trans) free( col->trans );
	free( col );
}

int _i_init_column_array(XIBIS *ibis )
{
	int status = 1;
	int col;
	XCOL *column;

	for (col=0;col<ibis->nc;col++)
	{
		column = _i_new_column();
		if (!column) goto failure; 
		column->id = col+1;
		ibis->column[col] = column;
	}
	
	return status;

failure:

	return (IBIS_MEMORY_FAILURE);
}

int _i_insert_column_pointers(XIBIS *ibis, int col, int ncol )
{
	int status=1;
	int nc;
	int i;
	XCOL *column;

	nc = ibis->nc - col; /* number to move up */

	_i_move_column_pointers( ibis, col, col+ncol, nc );
	
	/* Insert new columns and renumber */
	for (i=0; i< ncol; i++)
	{
		column = _i_new_column();
		if (!column) goto failure;
		column->id = col+i+1;
		ibis->column[col+i] = column;
	}

	return status;
	
failure:

	/* free up the new columns */
	for (i--; i>=0; i--)
		_i_free_column(ibis->column[col+i]);

	/* put everything back where we got it */
	_i_move_column_pointers( ibis, col+ncol, col, nc );
	
	return (IBIS_MEMORY_FAILURE);
}


/*
 * Remove all IBIS references to columns. 
 */

int _i_delete_column_pointers(XIBIS *ibis, int col, int ncol )
{
	int nc;
	int i;
	

	/* delete loop */
	for (i=0; i< ncol; i++)
	{
		if (ibis->column[col+i])
			_i_free_column( ibis->column[col+i] );
	}
	
	/*  Move the pointers back down */
	
	nc = ibis->nc  - (col+ncol);
	_i_move_column_pointers( ibis, col+ncol, col, nc );

	return 1;
}

/*
 * move columns starting at <srccol> to a location
 * starting at <destcol>, and renumber them.
 */

int _i_move_column_pointers(XIBIS * ibis, int srccol, int destcol, int ncol)
{
	int i;
	XCOL *buffer[MAX_COL];
	
	if (srccol==destcol) return 1;

	/*  Move the pointers  */
	
	if (ncol>0)
	{
		memcpy( buffer, ibis->column+srccol, sizeof( XCOL* ) * ncol );
		memcpy( ibis->column+destcol, buffer, sizeof( XCOL* ) * ncol );
	}

	/* renumber the moved columns */

	for (i=0; i< ncol; i++)
		ibis->column[destcol+i]->id = destcol+i+1;
	
	return 1;
}

void _i_free_col_array(XIBIS *ibis )
{
	int col;
	
	for (col=0;col<ibis->nc;col++)
	{
		if (ibis->column[col])
		{
			_i_free_column( ibis->column[col] );
			ibis->column[col]=(XCOL*)0;
		}
	}
	
}


int _i_attach_format_to_column(XIBIS *ibis,int fmt, int col)
{
	char *fmt_name;
	XCOL *colm=ibis->column[ col ];
	
	colm->format = fmt;
	
	if (fmt>FMT_ASCII) fmt=FMT_ASCII; /* only generic ASCII accepted */
	fmt_name = format_name[ fmt ];

	if (colm->trans) /* clear the old translation */
	{
		free( colm->trans );
		colm->trans = (trans_type *)0;
	}
	
	return (_i_attach_column_to_group( ibis->formats, fmt_name, colm )); 
}

int _i_attach_column_to_group(List* list,char *grp, XCOL *col)
{
	XGROUP *group;
	
	if (!(group = _i_request_group(list,grp)))
		return 0;

	/* add column to group's list */
	_i_append_value( group->columns, (list_value)col );

	return (1);
}


int _i_detach_col_from_format(XIBIS* ibis, int col)
{
	int fmt;
	XCOL *colm=ibis->column[ col ];
	XGROUP *fgroup;
	
	fmt = colm->format;
	if (fmt>FMT_ASCII) fmt=FMT_ASCII;
 
    fgroup = _i_find_group( ibis->formats, format_name[ fmt ] );
	if (!fgroup) return IBIS_INVALID_FORMAT;

	if (colm->trans)	/* revert to default translation */
	{
		free(colm->trans);
		colm->trans = (trans_type*)0;
	}

	_i_delete_value( fgroup->columns, (list_value) colm);
	
	return 1; 
}

int _i_detach_column_from_list(XIBIS *ibis,List* list,int col)
{
	List *ent;
	XGROUP *group;
	XCOL *colm=ibis->column[ col ];

	if (!list) return 0;
	
	for (ent=list->next; ent; ent=ent->next)
	{
		group = (XGROUP*)ent->value;
		_i_delete_value( group->columns, (list_value) colm);
	}
	return 1; 
}



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_xgap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  XGAP -- File Gap Manager:
 **   keeps track of available free space in IBIS file.
 **
 **  The publicly visible routines are _i_allocate_space(),
 **   _i_deallocate_space(), and _i_find_space().
 **/


#include "ibis.h"

void _i_init_space(XIBIS* ibis)
{
	XCOL *col;
	int i;
	int *size=ibis->format_size;
	
	_i_deallocate_space( ibis, 0, ibis->extent);

	for (i=0;i<ibis->nc;i++)
	{
		col = ibis->column[i];
		_i_allocate_space( ibis, col->offset, size[col->format]);
	}
}

static XGAP *find_offset( XIBIS *ibis, int offset )
{
	List *elt;
	XGAP *gap;

	for (elt=ibis->gaps->next; elt; elt=elt->next)
	{
		gap = (XGAP *)elt->value;
		if (offset >= gap->offset && offset < gap->end)
			return (gap);
	}
	
	return (XGAP *) 0;
}

static XGAP *find_size( XIBIS *ibis, int size )
{
	List *elt=ibis->gaps;
	XGAP *gap=(XGAP *) 0;
	
	if (!ibis->gaps) _i_init_space(ibis);

	for (elt=ibis->gaps->next; elt; elt=elt->next)
	{
		gap = (XGAP *)elt->value;
		if (gap->end - gap->offset >= size )
			return (gap);
	}
	
	return (XGAP *) 0;
}



/**
 **  Removes a chunk of free space <offset, size> from gap list
 **/

void _i_allocate_space(XIBIS *ibis, int offset, int size )
{
	XGAP *pos;
	XGAP *new;

	if (!ibis->gaps) _i_init_space(ibis);

	pos = find_offset( ibis, offset );
	
	if (!pos) return; /* not in list */

	if (offset == pos->offset)
	{
		if (offset + size == pos->end)
		  _i_delete_value( ibis->gaps, (list_value) pos );
		else
			pos->offset = offset + size;
	}
	else if (offset + size == pos->end)
	{
		pos->end = offset;
	}
	else
	{
		new = (XGAP *) calloc( 1L, sizeof (XGAP ) );
		new->offset = offset + size;
		new->end = pos->end;
		pos->end = offset;
		
		_i_insert_value( ibis->gaps, (list_value) new );
	}
	
	if (offset+size > ibis->extent)
		ibis->extent = offset+size;
}

/**
 **  Updates list of gaps to include offset & size.
 **  This is the only place that a new gap list is
 **  created.
 **/

void _i_deallocate_space(XIBIS* ibis, int offset, int size )
{
	XGAP *pos;
	XGAP *new;
	
	if (!ibis->gaps) ibis->gaps = _i_new_list( (void(*)(int*))free );

	if ((pos=find_offset( ibis, offset )))
	{
		pos->end = offset + size;
	}
	else if ((pos=find_offset( ibis, offset+size )))
	{
		pos->offset = offset;
	}
	else
	{
		new = (XGAP *) calloc( 1L, sizeof (XGAP ) );
		new->offset = offset;
		new->end = offset+size;
		
		_i_insert_value( ibis->gaps, (list_value) new );
	}
	if (offset + size == ibis->extent)
		ibis->extent = offset;
	
}

/**
 **  Looks through list of gaps in file and finds space
 **
 **  Returns 0 if no such offset found.
 **/

int _i_find_space(XIBIS* ibis, int* offset, int size)
{
	XGAP *pos;
	
	if ((pos = find_size(ibis, size )))
	{		
		*offset = pos->offset;
		return( 1 );
	}
	else return 0;

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_xgroup.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include <ctype.h>

/**
 ** XGROUP Structure/method Manipulation Routines 
 **
 **  groups are used in Groups, Units, and Formats
 **  and are simply named lists of columns.
 **/


#define NEW_TYPE( type ) ((type *)calloc(1L, sizeof(type)))

/*
 *  Creation -- The destruction method is turned off
 *  so that the columns can destroy themselves later.
 */

XGROUP* _i_new_group(char* group)
{
	XGROUP *newgrp=(XGROUP*)0;
	
	if (!_i_check_name(group)) return newgrp;

	newgrp = NEW_TYPE( XGROUP );
	if (!newgrp) goto failure;
	
	strncpy(newgrp->name, group, MAX_GRP_NAME);
	
	newgrp->columns = _i_new_list(NULL); /* no destruction */
	if (!newgrp->columns) goto failure;
	
	return (newgrp);

failure:
	_i_free_group( newgrp );
	return( (XGROUP*)0);
}


/**
 **  From a list of groups, get a named group
 **/

XGROUP* _i_find_group(List *list, char* grp )
{
	List *grpent;
	
	if (!(list &&  grp && *grp)) return (XGROUP *)0;
	
	for (grpent = list->next; grpent; grpent=grpent->next)
	{
		if (!_i_strcmp_nocase( ((XGROUP *)grpent->value)->name, grp ))
			break;
	}
	
	if (grpent)
		return (XGROUP *)grpent->value;
	else
		return (XGROUP *)0;
}

static char *ValueList[]={
	ITYPE_FORMAT,
	ITYPE_UNIT,
	ITYPE_GROUP,
	ITYPE_LOCAL,
	(char *)0 /* end */
};

typedef enum {
	VALUE_FORMAT=1,
	VALUE_UNIT,
	VALUE_GROUP,
	VALUE_LOCAL,
	VALUE_LAST=0 /* end */
} value_type;


char *_i_parse_group_name(XIBIS *ibis, char *name, List **list)
{
	char *gptr;
	char listname[MAX_GRP_NAME+1];
	int namelen;
	
	gptr = strpbrk(name, ":");
	if (!gptr) /* this is a standard name */
	{
		*list = (List *)0;
		return name;
	}
	
	/* extract out list name */
	namelen = gptr - name;
	if (namelen > MAX_GRP_NAME) namelen = MAX_GRP_NAME;
	strncpy(listname, name, namelen);
	listname[namelen]='\0';
	
	gptr++; /* return group name */

	switch( _i_keymatch(listname, ValueList) )
	{
		case VALUE_FORMAT:
			*list = ibis->formats;
			break;
		case VALUE_UNIT:
			*list = ibis->units;
			break;
		case VALUE_GROUP:
			*list = ibis->groups;
			break;
		case VALUE_LOCAL:
			*list = ibis->locals;
			break;
		default:
			*list = (List *)0;
			gptr = (char *)0; /* FAILURE - bad list name */
			break;
	}
	
	return gptr;
}


XGROUP* _i_find_group_all(XIBIS* ibis, char *grp )
{
	XGROUP* group=(XGROUP *)0;
	List *thelist;
	char *groupstr;
	
	if (!grp) return group;

	groupstr = _i_parse_group_name(ibis, grp, &thelist);
	if (!groupstr) return group;
	
	if (thelist)
		return _i_find_group( thelist, groupstr );

	/* else there was no list specified; look everywhere */

	group = _i_find_group( ibis->formats, grp );
	if (group) return group;

	group = _i_find_group( ibis->units, grp );
	if (group) return group;
	
	group = _i_find_group( ibis->groups, grp );
	if (group) return group;
	
	group = _i_find_group( ibis->locals, grp );
	return group;
}


/**
 **  From a list of groups, get or create a named group
 **/

XGROUP* _i_request_group(List* list, char *grp )
{
	XGROUP *group;
	
	if (!(list &&  grp)) return 0;
	
	if ((group=_i_find_group( list, grp ))) /* group was found */
		return (group);
	else /* add a new group to list */
	{
		group = _i_new_group(grp);
		if (!group) return (0);
		/* add group to list */
		_i_append_value( list, (list_value)group );
	}
	
	return (group);
}


#define INVALID_GROUP_CHARS ":"

/* 
 * a sanity check that a string is a single word,
 * containing anything printable except INVALID_GROUP_CHARS.
 */

int _i_check_name(char* namestr )
{
	if (!namestr) return 0;
	if (strpbrk(namestr,INVALID_GROUP_CHARS)) return 0;
	while (!*namestr)
		if (!isprint(*namestr++)) return 0;

	return 1;
}


#define FREE_MEMBER( mname ) if (group->mname) free( group->mname )
#define FREE_LIST( mname ) if (group->mname) _i_free_list( group->mname )

/*
 *  Destruction
 */


void _i_free_group(XGROUP* group )
{
	if ( group )
	{
		FREE_LIST( columns );
		free( group );
	}
}

/*
 *  Set Operations on groups
 */

static XGROUP* group_COPY(XGROUP *grp)
{
	List *vals;
	List *outvals;
	XGROUP *newgroup=(XGROUP *)0;

	if (!grp) return newgroup; /* nothing comes from nothing */

	newgroup = _i_new_group("temp");
	outvals = newgroup->columns;
	for(vals=grp->columns->next; vals; vals=vals->next)
		_i_append_value( outvals, vals->value);
	
	return newgroup;
}


static XGROUP* group_AND(XGROUP *grp1,XGROUP *grp2)
{
	List *vals1=grp1->columns->next;
	List *vals2=grp2->columns->next;
	List *outvals;
	List *ent;
	XGROUP *newgroup=(XGROUP *)0;
	list_value val;

	if (!grp1 || !grp2) return newgroup;

	/* whether we find anything or not, create the group */	
	newgroup = _i_new_group("temp");

	outvals = newgroup->columns;
	for(;vals1;vals1=vals1->next)
	{
		val = vals1->value;
		for (ent=vals2; ent && ent->value!=val; ent=ent->next);

		if (ent) /* found val in vals2 - good ! */
			_i_append_value( outvals, val);
	}
	
	return newgroup;
}

static XGROUP* group_OR(XGROUP *grp1,XGROUP *grp2)
{
	List *vals;
	List *outvals;
	XGROUP *newgroup=(XGROUP *)0;

	if (!grp1 && !grp2) return newgroup;

	newgroup = _i_new_group("temp");

	outvals = newgroup->columns;
	for(vals=grp1->columns->next; vals; vals=vals->next)
			_i_append_value(outvals, vals->value);
	for(vals=grp2->columns->next; vals; vals=vals->next)
			_i_append_value(outvals, vals->value);
	
	return newgroup;
}

/* in group1 not in group2 */

static XGROUP* group_DIFF(XGROUP *grp1,XGROUP *grp2)
{
	List *vals1=grp1->columns->next;
	List *vals2=grp2->columns->next;
	List *outvals;
	List *ent;
	XGROUP *newgroup=(XGROUP *)0;
	list_value val;

	if (!grp1) return newgroup;

	/* whether we find anything or not, create the group */	
	newgroup = _i_new_group("temp");

	outvals = newgroup->columns;
	for(;vals1;vals1=vals1->next)
	{
		val = vals1->value;
		for (ent=vals2; ent && ent->value!=val; ent=ent->next);

		if (!ent) /* failed to find val in vals2 - good ! */
			_i_append_value( outvals, val);
	}
	
	return newgroup;
}

/*
 * A simple group constructor, which uses a linear,
 * left-to-right parsing routine, and allows any combination
 * of operations of the form group1<OP1>group2<OP2>group3..., where
 * <OPn> is & (=AND), | (=OR), or - (=A but not B). Example:
 *
 *   (foot-pounds) | (hectares>3,eh?) - (kg*m/sec^2) & bob
 *
 */

#define OP_LIST "*|-&+"
#define DELIM_LIST "*|-&+ \t"
static char *token=(char *)0;
static int parens=0;
static char *QUOTE_LIST="-([{\'\"";
static char *UNQUOTE_LIST="-)]}\'\"";
#define NUM_QUOTES 5

static char *get_group(void)
{
	char *qchar;
	
	if (!token) return token;

	/* skip white space */
	while (*token && isspace(*token)) token++;
	if (!*token) return token;

	/* Check to see if this is a quoting mark */
	for (qchar=QUOTE_LIST+1,parens=1; *qchar; qchar++,parens++)
		if (*qchar==*token) break;
	
	if (*qchar) token++; /* found a quote */
	else  parens=0;
	
	return token;
}


static char get_op(void)
{
	char theOp;
	char *name_end;

	if (!token) return 0;
	
	/* find end of name */
	if (parens)
		token=strchr(token,UNQUOTE_LIST[parens]);
	else
		token=strpbrk(token,DELIM_LIST);
	if (!token) return 0;
	name_end = token;
	
	/* search for ops */
	token=strpbrk(token,OP_LIST);
	if (!token) theOp = 0;
	else theOp = *token++;
	
	/* null-delimit previous operand */
	*name_end = '\0';

	return theOp;
}

/*
 *  Group expression parser
 */

XGROUP* _i_group_construct(XIBIS* ibis, char* expression)
{
	char *expr;
	XGROUP *oldgroup;
	XGROUP *thisgroup=(XGROUP *)0;
	XGROUP *nextgroup=(XGROUP *)0;
	char thisOp,nextOp;
	char *groupname;
	
	expr = (char *)malloc(strlen(expression)+1);
	if (!expr) return thisgroup;
	
	strcpy(expr, expression);
	token = expr;
	
	/* Set up everything for the first op */
	groupname = get_group();
	thisOp = get_op();
	thisgroup = group_COPY( _i_find_group_all( ibis, groupname ) );
	if (! thisgroup ) return thisgroup;
	
	/* get next op & token */
	
	/* get the next op before it's zapped */
	groupname = get_group();
	nextOp = get_op();
	nextgroup = _i_find_group_all( ibis, groupname );
	
	/* computation loop */
	
	while (thisOp && nextgroup)
	{
		oldgroup = thisgroup;
		switch (thisOp)
		{
			case '*':
			case '&':
				thisgroup = group_AND( thisgroup, nextgroup);
				break;
			case '|':
			case '+':
				thisgroup = group_OR( thisgroup, nextgroup);
				break;
			case '-':
				thisgroup = group_DIFF( thisgroup, nextgroup);
				break;
		}
		_i_free_group(oldgroup);
		thisOp=nextOp;
		groupname = get_group();
		nextOp = get_op();
		nextgroup = _i_find_group_all( ibis, groupname );
	};
	
	
	free(expr);

	if (_i_count_list(thisgroup->columns))
		return thisgroup;
	else return (XGROUP *)0;
}



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_xrec.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  struct_xrec.c  - manipulates XREC structs.
 */

#include "ibis.h"

#define NEW_TYPE( type, num ) ((type *)calloc(1L, sizeof(type)*(num)))

XREC *_i_new_record(int ncols )
{
	XREC *rec;
	rec = NEW_TYPE( XREC, 1 );
	if (!rec) goto failure;
	
	rec->trans = NEW_TYPE( trans_type, (FMT_ASCII+1) );
	if (!rec->trans) goto failure;

	rec->column = NEW_TYPE( XCOL*, ncols );
	if (!rec->column) goto failure;

	rec->inbuffer = NEW_TYPE( char*, ncols );
	if (!rec->inbuffer) goto failure;

	rec->outbuffer = NEW_TYPE( char*, ncols );
	if (!rec->outbuffer) goto failure;

	rec->method = NEW_TYPE( t_recmethod, ncols );
	if (!rec->outbuffer) goto failure;

	rec->collaborators = _i_new_list(0);
	if (!rec->collaborators) goto failure;

	rec->num_cols = ncols;
	rec->cur_row = 1;
	rec->top_row = 1;
	rec->flags = FLAG_REC_REFRESH;

	return (rec);
	
failure:
	_i_free_record( rec );
	return (XREC *)0;
}


#define FREE_MEMBER( mname ) if (rec->mname) do \
	{free( rec->mname ); rec->mname=(void *)0;} while (0)
#define FREE_LIST( mname ) if (rec->mname) do \
	{_i_free_list( rec->mname ); rec->mname=(void *)0;} while (0)

void _i_free_record(XREC *rec)
{
	if (!rec) return;
	FREE_MEMBER( trans );
	FREE_MEMBER( column );
	FREE_MEMBER( inspace );
	FREE_MEMBER( outspace );
	FREE_MEMBER( inbuffer );
	FREE_MEMBER( outbuffer );
	FREE_MEMBER( method );
	FREE_LIST( collaborators );
	free( rec );
}

/*
 * General Purpose record-notification utility
 * (used by row and column ops )
 */

int _i_notify_records(XIBIS* ibis, notice_type notice, int sval, int endval)
{
	List *ent;
	XREC *rec;
	int status=1;
	
	if (!ibis->record) return 1;
	
	for (ent=ibis->record->next; ent; ent=ent->next)
	{
		rec = (XREC *)ent->value;
		status = rec->method->notice( rec, notice, sval, endval);
		if (status != 1) return status;
	}
	
	return status;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create struct_xibis.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"

/* XIBIS Structure/method Manipulation Routines */


#define NEW_TYPE( type ) ((type *)calloc(1L, sizeof(type)))

/*
 *  Creation -- the methods are separate structs to allow overrides.
 */

XIBIS* _i_new_ibis(void)
{
	XIBIS *newibis;
	
	newibis = NEW_TYPE( XIBIS );
	if (!newibis) goto failure;
	
	newibis->rowmethod = NEW_TYPE(t_rowmethod);
	if (!newibis->rowmethod) goto failure;
	
	newibis->colmethod = NEW_TYPE(t_colmethod);
	if (!newibis->colmethod) goto failure;
	
	newibis->recmethod = NEW_TYPE(t_recmethod);
	if (!newibis->recmethod) goto failure;
	
	newibis->filemethod = NEW_TYPE(t_filemethod);
	if (!newibis->filemethod) goto failure;
	
	newibis->labmethod = NEW_TYPE(t_labmethod);
	if (!newibis->labmethod) goto failure;

	newibis->trans = ((trans_type *)calloc(1L, sizeof(trans_type)*(FMT_ASCII+1) ));
	if (!newibis->trans) goto failure;
	
	newibis->format_size = ((int *)calloc(1L, sizeof(int)*(FMT_LAST+1) ));
	if (!newibis->format_size) goto failure;
	
	newibis->column = ((XCOL **)calloc(1L, sizeof(XCOL*)*(MAX_COL+EXTRA_COL) ));
	if (!newibis->column) goto failure;
	
	return (newibis);

failure:
	_i_free_ibis( newibis );
	return( (XIBIS*)0);
}


#define FREE_MEMBER( mname ) if (ibis->mname) do \
	{free( ibis->mname ); ibis->mname=(void *)0;} while (0)
	
#define FREE_LIST( mname ) if (ibis->mname) do \
	{_i_free_list( ibis->mname ); ibis->mname=(void *)0;} while (0)

/*
 *  Destruction
 */

void _i_purge_ibis(XIBIS* ibis )
{
	if ( ibis )
	{
		/* free and clear members not created by "_i_new_ibis" */

		_i_free_col_array( ibis );
		FREE_LIST( formats );		
		FREE_LIST( record );
		FREE_LIST( groups );
		FREE_LIST( units );
		FREE_LIST( locals );
		FREE_LIST( gaps );
		FREE_MEMBER( fmt );
		ibis->fmt_len = IFMT_SIZE;
		
		/* clear all transient flags */
	
		ibis->flags &= (MASK_ORG | MASK_MODE | FLAG_FILE_OLD);
	}
}



void _i_free_ibis( XIBIS *ibis )
{
	if ( ibis )
	{
		/* free data created by FileUnitOpen */
	
		_i_purge_ibis( ibis );

		/* free members allocated by _i_new_ibis */

		FREE_MEMBER( rowmethod );
		FREE_MEMBER( colmethod );
		FREE_MEMBER( recmethod );
		FREE_MEMBER( filemethod );
		FREE_MEMBER( labmethod );
		FREE_MEMBER( column );
		FREE_MEMBER( trans );
		FREE_MEMBER( format_size );

		/* free the struct itself */
				
		free( ibis );
	}
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util_format.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include "ibisdeclares.h"
#include <ctype.h>

/* XIBIS Column Format Code Routines */


int _i_IBISFormatCode( char *fmtstr )
{
	int code;
	
	code=_i_keymatch( fmtstr, format_name );
	if (code) /* good match */
		return code-1;
	
	/* If we got here, it's either ASCII or bad */
	
	if (tolower(*fmtstr)!='a') return 0; /* bad */

  	code = atoi( fmtstr+1 );
  	if (code<=0 || code > FMT_LAST-FMT_ASCII)
  		return IBIS_INVALID_FORMAT;
  	return FMT_ASCII+code;
}

int _i_IBISFormatSize( XIBIS *ibis, int format, int *size)
{
	int status=1;
	int old = (ibis->flags&FLAG_FILE_OLD) ? 1 : 0;
	
	if (format < FMT_NONE || format > FMT_LAST )
		return IBIS_INVALID_FORMAT;
	
	if (format > FMT_NONE && format < FMT_ASCII )
	{
		status = zvpixsize( size, format_name[ format ], 
		    ibis->intfmt,ibis->realfmt);
	}
	else if (old) /* Force ASCII size to be 4 or greater */
	{
		*size = format_size[ format ] - 1;
		if (*size < 4) *size=4;
	}
	else
		*size = format_size[ format ];
	
	return status;

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util_strings.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis.h"
#include <string.h>
#include "ibisdeclares.h"
#include <ctype.h>

int _i_strcmp_nocase(char* str1,char* str2)
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}

void _i_make_uppercase(char* str)
{
	if (!str) return;	
	for ( ; *str; str++) *str=toupper(*str);
}

int _i_strncmp_nocase(char* str1,char* str2, int n)
{
	char c1,c2;
	
	if (n<1) return 1;
	for((c1= *str1,c2= *str2); c1&&c2&&n; (c1= *++str1,c2= *++str2,n--) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return 0; /* they don't have to end at same time */
}

int _i_keymatch(char* keystr, char **keys)
{
	int keyno;
	
	if (!keystr) return 0;
	for (keyno=1; *keys && _i_strcmp_nocase(*keys,keystr); keys++)
		keyno++;
	
	if (*keys) return (keyno);
	else return 0;
}

void *_i_mem_dup(char* data, int size )
{
	char *datacopy=(char *)0;
	
	if (!data || !size ) return data;
	datacopy = (char *)calloc(1L, size);
	if (!datacopy) return datacopy;

	memcpy(datacopy, data, size);
	return datacopy;
}



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util_file.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* 
 *  util_file.c  -- common utility for setting up
 *  segments and blocksizes, etc. These routines
 *  happen to be used by both the mthd_io_row and mthd_io_col
 *  file methods, but with different lower_bounds.
 */

#include "ibis.h"

/**
 **  These are routines that set up file blocking
 **  and offset parameters for IBIS-2 files.
 **/

int _i_compute_blocksize(XIBIS* ibis, int lower_bound )
{
	int recsize=ibis->recsize;
	
	if (lower_bound <= 0) lower_bound=1;
	
	
	/*
	 *  Define a reasonable setup. Depending on the record
	 *  size, either blocksize will be a multiple of segment,
	 *  or segment will be a multiple of blocksize. And segment
	 *  is large enough to contain the lower_bound bytes.
	 *
	 *  ibis->recsize will only be non-zero if the PIX_NS
	 *  has been set. Otherwise, we have byte data as usual.
	 */
	
	if (!ibis->ns)
	{
		 recsize=NEW_BLKSIZE;
		ibis->recsize = recsize;
		ibis->ns = ibis->recsize / ibis->format_size[ ibis->pix_fmt ];
	}
	else recsize = ibis->recsize;
	
	ibis->recsize = recsize;

	if (lower_bound < recsize) /* blocksize is a mult of segment */
	{
		lower_bound = _i_useful_segment( lower_bound );
		ibis->segment = lower_bound;
		ibis->blocksize = (recsize/lower_bound)*lower_bound;
	}
	else	/* segment is a multiple of blocksize */
	{
		ibis->blocksize = recsize;
		ibis->segment = ALIGN_UP( lower_bound, recsize );
	}
	
	return 1;
}

/*
 *  Prior to creating a file, if the NS or image FORMAT
 *  is set, the recsize will be implicitly defined. compute
 *  its value here.
 */

int _i_reset_recsize(XIBIS* ibis)
{
	int pixsize, status=1;
	char intfmt[20],realfmt[20];
	
	if (!ibis->ns)
	{
		ibis->recsize = 0;
		return status;
	}
	
	status=zvhost( ibis->pix_host, intfmt, realfmt );
	if (status!=1) return status;
	status=zvpixsize( &pixsize, format_name[ ibis->pix_fmt ],
					intfmt, realfmt ); 
	if (status!=1) return status;
	
	ibis->recsize = pixsize * ibis->ns;
	
	return status;
}

int _i_useful_segment(int lower_bound )
{
	int bound=1;
	
	/* return first power of 2 not smaller than lower_bound */

	lower_bound--;
	while (lower_bound)
	{
		lower_bound = lower_bound >> 1;
		bound = bound << 1;
	}
	
	return bound;
}

void _i_compute_new_offsets(XIBIS* ibis,int scol,int ncol)
{
	int i;
	int csize,offset=ibis->extent;
	XCOL *col;

	/*
	 * compute the offsets, starting from current extent.
	 *
	 */
	
	for (i=scol; i<scol+ncol; i++)
	{
		col = ibis->column[i];
		col->offset = offset;
		csize = format_size[ col->format ];
		offset += csize;
	}
	ibis->extent = offset;
	
	return;
}

/* Access method for VICAR i/o which tries to
 * avoid as much paired-optional parsing as possible.
 */

int _i_process_contiguous
(
 int unit,
 int (*function)(int, void*, ...), /* zvwrit or zvread */
 char *buffer,
 int offset,
 int nbytes,
 int blksize, /* IBIS blocksize */
 int recsize, /* physical VICAR recsize */
 int inc /* increment buffer or not ? */
)
{
	int status=1; 
	int size=blksize; 
	int samp,line_left;
	int bytes_left=nbytes;
	int line=1+offset/blksize;
	int contiguous=(blksize==recsize);

	/* special if not at start of block */
	if (offset%blksize)
	{
	  samp = (offset%blksize); 
	  line_left = blksize-samp; 
	  size = line_left>bytes_left ? bytes_left : line_left; 

/* Don't call zvwrite or zvread with 0 nsamps, this defaults to
   read/write a whole row, which will likely stomp on the passed in
   buffer. Since there is nothing to read/write, there is no reason to
   call this anyways.
*/

	  if (size !=0) {
	    status = (*function)( unit, buffer, 
				  "line", line,
				  "samp", 1+samp, 
				  "nsamps", size, NULL); 
	    if (status != 1) return status; 
	  }
	  if (inc) buffer += size;
	  bytes_left-=size;
	  line++;
	} 
	else if (bytes_left >= blksize)
	{
	    if (contiguous)
		status = (*function)( unit, buffer,"line", line, NULL);
	    else
		status = (*function)( unit, buffer,"line", line,
						"nsamps",blksize, NULL);
	   if (status != 1) return status; 
	   if (inc) buffer += size;
	   bytes_left-=size;
	   line++;
	}


	/* From this point, we are block-aligned; read whole lines */
	for(; bytes_left>=blksize; bytes_left-=blksize)
	{
		if (!contiguous)
		    status = (*function)( unit, buffer,"line", line,
						"nsamps",blksize, NULL);
		else if (function==zvwrit)
		    status = (*function)( unit, buffer,"line", line,NULL);
		else
		    status = (*function)( unit, buffer,NULL);
		if (status != 1) return status; 
		if (inc) buffer += blksize;
		line++;
	}

	/* If any leftovers, read here */

	if (bytes_left > 0)
	{
		  status = (*function)( unit, buffer,"line", line,
			"nsamps",bytes_left, NULL); 
	}
	
	return status;
}

int _i_clear_ibis_file(XIBIS* ibis, int sblock)
{
	char *buffer;
	int status=1;
	int unit=ibis->unit;
	int blocksize=ibis->blocksize;
	int block, nblock=ibis->numblocks;

	/* clear the file */
	buffer = (char *)calloc(1L, ibis->blocksize);
	if (!buffer) return IBIS_MEMORY_FAILURE;
	
	for (block=sblock;block<=nblock;block++)
	{
		status = zvwrit( unit, buffer, "line", block, "nsamps",blocksize, NULL);
		if (status!=1) break;
	}

	free (buffer);
	return status;	
}

int _i_install_numblocks(XIBIS* ibis, char* label /* "NL" or "NLB" */,int numblocks)
{
	int status;
	int nblock=numblocks;
	
	if (ibis->flags & FLAG_FILE_IMAGE)
		return IBIS_CONTAINS_IMAGE_DATA;

	zldel( ibis->unit, "system",label,NULL);
	status = zladd( ibis->unit, "system",label,
			&nblock,"format","int", NULL);

	return status;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util_trans.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **   util_trans.c -- handles format translations
 **/

#include "ibis.h"
#include "string.h"
#include <zvproto.h>
/*
 * convert string data of different lengths,
 * appending null if required.
 */
 
static void ascii_to_ascii
(
  int inlen,
  int outlen,
  char *inbuf,
  char *outbuf,
  int num,
  int nullend
)
{
	register char *inptr=inbuf;
	register char *outptr=outbuf;
	char *nptr;
	int i,len0;
	
	len0 = inlen < outlen ? inlen : outlen-1;

	for (i=0;i<num;i++,inptr+=inlen,outptr+=outlen)
	{
		strncpy(outptr,inptr,outlen);
		if (nullend) outptr[len0]='\0'; /* just to be safe; */
		else  /* pad with spaces */
			for (nptr=outptr+outlen-1; (!*nptr) && nptr>=outptr; nptr--)
				*nptr=' ';
	}
}


int _i_set_trans(XIBIS* ibis,trans_type* trans,int buffmt,int filefmt)
{
	if (filefmt > FMT_ASCII) filefmt=FMT_ASCII; /* must be generic */
	
	/* make sure not ASCII-numeric translation */

	if ((filefmt == FMT_ASCII && buffmt < FMT_ASCII && buffmt>FMT_NONE) ||
		(filefmt < FMT_ASCII && buffmt>=FMT_ASCII)) 
		return IBIS_CANT_TRANSLATE;

	trans->infmt = buffmt;
	trans->outfmt = filefmt;
	trans->format_size = ibis->format_size;
	trans->new = (ibis->flags & FLAG_FILE_OLD) ? 0 : 1;
	
	if (buffmt==FMT_NONE || filefmt == FMT_ASCII) return 1;

	/* This is a numeric translation */

	zvtrans_in((int*)&trans->intrans, format_name[ filefmt ],
		format_name[buffmt],ibis->intfmt, ibis->realfmt);
	zvtrans_out((int*)&trans->outtrans, format_name[buffmt],
		format_name[filefmt],ibis->intfmt, ibis->realfmt);

	return 1;

}

/* initialize translation buffers based on hostfmt */

int _i_trans_init(XIBIS *ibis )
{
	int status;
	char intfmt[MAX_GRP_NAME+1], realfmt[MAX_GRP_NAME+1];

	/* 
	 * get the int and real formats. If the host is not
	 * recognized, we will hope that the individual fmts were
	 * set manually, but just to be sure, return the
	 * error status of zvhost.
	 */
	
	status = zvhost( ibis->hostfmt, intfmt, realfmt);
	
	if (status ==1)
	{
		strcpy( ibis->intfmt, intfmt);
		strcpy( ibis->realfmt, realfmt);
	}
	
	_i_make_uppercase(ibis->hostfmt);
	
	_i_trans_reset( ibis );
	
	return status;
}

/* Reset translators and format_size  */

void _i_trans_reset(XIBIS* ibis )
{
	int fmt;
	trans_type *trans=ibis->trans;
	
	/* reset all format translators */
	
	if (ibis->intfmt[0])
	{
		_i_make_uppercase(ibis->intfmt);
		for (fmt = FMT_BYTE; fmt<=FMT_FULL; fmt++)
			_i_set_trans( ibis, trans+fmt, fmt, fmt );
	}

	if (ibis->realfmt[0])
	{
		_i_make_uppercase(ibis->realfmt);
		for (fmt = FMT_REAL; fmt<=FMT_COMP; fmt++)
			_i_set_trans( ibis, trans+fmt, fmt, fmt );
	}

	_i_set_trans( ibis, trans+FMT_ASCII, FMT_ASCII, FMT_ASCII );
	
	/* reset size array */
	for (fmt=FMT_BYTE; fmt<=FMT_LAST; fmt++)
		_i_IBISFormatSize( ibis, fmt, ibis->format_size+fmt);
	
}

int _i_trans_buf_to_local(trans_type *trans, XCOL *col, char* inbuf, 
			  char *outbuf, int num )
{
	int infmt=trans->infmt;

	if (infmt<FMT_ASCII && infmt>FMT_NONE)
		zvtrans((int*)&trans->intrans, inbuf, outbuf, num );
	else
	{
		int format=col->format;
		int filesize=trans->format_size[format];
		
		if (infmt==FMT_NONE)
			memcpy( outbuf, inbuf, num * format_size[format]);
		else 
			ascii_to_ascii(filesize, format_size[infmt],  inbuf, outbuf, num, 1 );
	}

	return 1;
}


int _i_trans_buf_from_local(trans_type* trans, XCOL* col, char* inbuf, 
			    char* outbuf, int num )
{
	int infmt=trans->infmt;

	if (infmt<FMT_ASCII && infmt>FMT_NONE)
		zvtrans((int*)&trans->outtrans, inbuf, outbuf, num );
	else
	{
		int format=col->format;
		int filesize=trans->format_size[format];
		
		if (infmt==FMT_NONE)
			memcpy( outbuf, inbuf, num * format_size[format]);
		else 
			ascii_to_ascii(format_size[infmt], filesize, inbuf, outbuf, num, trans->new );
	}

	return 1;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **   ibis.h   -- private interface to XIBIS subroutine library
 **/

#ifndef _H_IBIS
#define _H_IBIS

/* public VICAR interface */
#include "xvmaininc.h"
#include "ftnbridge.h"

/* public IBIS2 interface */
#include "ibisfile.h"
#include "ibiserrs.h"

/* private includes */
#include "ibislists.h"
#include "ibisdefines.h"
#include "ibisdeclares.h"
#include "ibisglobals.h"
#include "ibisstructs.h"

#include <stdlib.h>

#endif  /* _H_IBIS */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisdeclares.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  ibisdeclares.h
 **
 **   Forward definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISDECLARES
#define _H_IBISDECLARES
#include "zvproto.h"
/**
 **  Forward references:
 **/

struct XIBIS;
typedef struct XIBIS XIBIS;
struct XREC;
typedef struct XREC XREC;
struct XGROUP;
typedef struct XGROUP XGROUP;
struct XGAP;
typedef struct XGAP XGAP;
struct XCOL;
typedef struct XCOL XCOL;
struct t_recmethod;
typedef struct t_recmethod t_recmethod;
struct t_rowmethod;
typedef struct t_rowmethod t_rowmethod;
struct t_colmethod;
typedef struct t_colmethod t_colmethod;
struct t_filemethod;
typedef struct t_filemethod t_filemethod;
struct t_labmethod;
typedef struct t_labmethod t_labmethod;
struct trans_type;
typedef struct trans_type trans_type;

/* generic function pointer - arg list actually can vary */
typedef int (*XMethod)();

/**
 ** Public (protected) Function declarations
 **/

XIBIS* _i_new_ibis(void);
XREC*  _i_new_record(int ncols);
XGROUP* _i_new_group(char *group);
XGROUP* _i_request_group(List *list, char *group);
XGROUP* _i_find_group(List *list, char *group);
XGROUP* _i__i_find_group_all(XIBIS *ibis, char *group);
XGROUP* _i_group_construct(XIBIS *ibis, char *expression);
char *_i_parse_group_name(XIBIS *ibis, char *name, List **list);
void *_i_mem_dup(char* data, int size );
void _i_free_ibis(XIBIS *ibis);
void _i_free_group(XGROUP *group);
void _i_free_record(XREC *rec);
int _i_null_method(void);
int _i_strcmp_nocase(char* str1,char* str2);
int _i_keymatch(char *keystr, char **keys);
int _i_IBISFormatSize(XIBIS *ibis, int format, int *size);
int _i_IBISFormatCode(char *fmtstr );
int _i_detach_col_from_format(XIBIS* ibis, int col);
int _i_attach_format_to_column(XIBIS *ibis,int fmt, int col);
XCOL *_i_new_column(void);
void _i_free_column(XCOL *col);
int _i_init_column_array(XIBIS *ibis );
int _i_insert_column_pointers(XIBIS *ibis, int col, int ncol );
int _i_delete_column_pointers(XIBIS *ibis, int col, int ncol );
int _i_move_column_pointers(XIBIS * ibis, int srccol, int destcol, int ncol);
void _i_free_col_array(XIBIS *ibis );
int _i_attach_column_to_group(List* list,char *grp, XCOL *col);
int _i_detach_column_from_list(XIBIS *ibis,List* list,int col);
int IBISGroupTransfer(int in_id, int out_id, char *type, int *incols, 
		      int* outcols, int nc );
int IBISGroupNew(int ibis_id,char *type, char *name, int *cols, int ncols, 
		 char *expr );
int _IBISGroupNew(XIBIS* ibis, char* type, char* name, int* cols, int ncols, 
		  char* expr);
int IBISGroupDelete(int ibis_id,char *type, char *name );
int IBISGroupModify(int ibis_id,char *type, char* name, char* mod, int* cols, 
		    int ncols );
int IBISGroupFind(int, char*, int, char*, int, int, int);
int IBISColumnRead(int ibis_id,char* buffer,int column,int srow,int nrows);
int IBISColumnWrite(int ibis_id,char *buffer,int column,int srow,int nrows);
int IBISColumnClear(int ibis_id,int column,int ncols);
int IBISColumnNew(int ibis_id,int column,int ncols,char *fmt);
int IBISColumnDelete(int ibis_id,int column,int ncols);
int IBISColumnMove(int ibis_id,int sourcecol,int destcol,int ncols);
int IBISColumnFind(int, char*, char*, int*, int, int);
int _i_set_trans(XIBIS* ibis,trans_type *trans,int buffmt,int filefmt);
int _i_trans_init(XIBIS *ibis );
void _i_trans_reset(XIBIS* ibis );
int _i_trans_buf_to_local(trans_type *trans, XCOL *col, char* inbuf, 
			  char *outbuf, int num );
int _i_trans_buf_from_local(trans_type* trans, XCOL* col, char* inbuf, 
			    char* outbuf, int num );
XREC *_i_new_record(int ncols );
void _i_free_record(XREC *rec);
int _i_notify_records(XIBIS* ibis, notice_type notice, int sval, int endval);
void _i_init_space(XIBIS* ibis);
void _i_allocate_space(XIBIS *ibis, int offset, int size );
void _i_deallocate_space(XIBIS* ibis, int offset, int size );
int _i_find_space(XIBIS* ibis, int* offset, int size);
List* _i_new_list(void (*kill_method)());
void _i_free_list(List *list );
List* _i_find_value(List *list, list_value value);
int _i_count_list(List *list);
void _i_insert_value(List* list, list_value value);
void _i_append_value(List* list, list_value value);
void _i_delete_value(List* list, list_value value);
int _init_ibis_globals(void);
void _i_ibis1_install_methods(XIBIS* ibis);
void _i_ibis2_install_methods(XIBIS* ibis );
void _i_purge_ibis(XIBIS* ibis);
int IBISRowClear(int ibis_id,int srow,int nrows);
int IBISRowNew(int ibis_id,int srow,int nrows);
int IBISRowDelete(int ibis_id,int srow,int nrows);
int _i_compute_blocksize(XIBIS* ibis, int lower_bound );
int _i_reset_recsize(XIBIS* ibis);
int _i_useful_segment(int lower_bound );
void _i_compute_new_offsets(XIBIS* ibis,int scol,int ncol);
int _i_process_contiguous(int unit, int (*function)(int, void*, ...), char *buffer, 
			  int offset, int nbytes, int blksize, int recsize, 
			  int inc);
int _i_install_ofile_methods(XIBIS* ibis );
void _i_install_cfile_methods(XIBIS* ibis );
void _i_install_rfile_methods(XIBIS* ibis );
int _i_install_grfile_methods(XIBIS *ibis );
int _i_clear_ibis_file(XIBIS* ibis, int sblock);
int _i_install_numblocks(XIBIS* ibis, char* label,int numblocks);
int _i_install_nlabel_methods(XIBIS * ibis );
int _i_install_olabel_methods(XIBIS *ibis );
int _i_install_grlabel_methods(XIBIS * ibis );
int _i_strcmp_nocase(char* str1,char* str2);
void _i_make_uppercase(char* str);
int _i_strncmp_nocase(char* str1,char* str2, int n);
int _i_keymatch(char* keystr, char **keys);
void *_i_mem_dup(char* data, int size );
int _i_check_name(char* namestr );
XGROUP* _i_new_group(char* group);
XGROUP* _i_find_group(List *list, char* grp );
char *_i_parse_group_name(XIBIS *ibis, char *name, List **list);
XGROUP* _i_find_group_all(XIBIS* ibis, char *grp );
XGROUP* _i_request_group(List* list, char *grp );
void _i_free_group(XGROUP* group );
XGROUP* _i_group_construct(XIBIS* ibis, char* expression);
int _HandleNotice(XREC* record, notice_type notice,void * sval, int endval );
int IBISRecordOpen(int ibis_id, int *record_id,  char *group, int *columns, 
		   int ncols, char* u_format);
int IBISRecordClose(int record_id );
int IBISRecordSet(int record_id, char *name, int value );
int IBISRecordGet(int record_id, char* name, char* value, int sval, 
		  int nvals );
int IBISRecordRead(int record_id, char* buffer, int row);
int IBISRecordWrite(int record_id,char* buffer,int row);
int IBISRecordClear(int record_id,int row,int nrows);
void ICLGetROOT(int ibis,char* group,char* inst);
void ICLGetPrimitive(int ibis,char *group,char* expression);
void ICLGetPOSITION(int ibis,char* group,char* inst);
void ICLGetPOS_IMAGE(int ibis,char* gline,char* gsamp,char* gband,char *inst);
void ICLGetPOS_GEOGRAPHIC(int ibis,char* glat,char* glong,char* inst);
void ICLGetVALUE(int ibis,char* group,char* inst);
void ICLGetRGB(int ibis,char* gred,char* ggreen,char* gblue,char* inst);
void ICLGetPIXEL(int ibis,char* gpos,char* gval,char* inst);
void ICLGetDIRECTION(int ibis,char* group,char* inst);
void ICLGetMATRIX(int ibis,char* gmat,char* gind,char* inst);
void ICLGetQUALITY(int ibis,char* gqual,char* inst);
void ICLGetHISTOGRAM(int ibis,char* gmat,char* gind,char* inst);
void ICLGetLOOKUP_TABLE(int ibis,char* gps,char* gind,char* memname,char* inst);void ICLGetSTATISTICS(int ibis,char* gmat,char* gind,char* memname,char *inst);
void ICLGetPOINT(int ibis,char* gpos,char* gval,char* memname,char* inst);
int icl_keymatch(char *keystr, char **keys);


#endif /* _H_IBISDECLARES */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisdefines.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  ibisdefs.h
 **
 **   Private definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISDEFS
#define _H_IBISDEFS

/* Conversion from external IBIS/record ID to internal pointer.  On	*/
/* machines where sizeof(int)>=sizeof(pointer), just use the pointer.	*/
/* On other machines (64 bits, e.g. AXP-UNIX), we must use a table and	*/
/* return an offset into it, rather than the pointer itself.  This is	*/
/* to avoid changing the public interface to support 64 bit machines, 	*/
/* as "int" for the IBIS ID is everywhere.				*/
/* Note that the table case does not check for the validity of the id,	*/
/* but neither did the original code (used directly as a pointer).	*/

#if POINTERS_64_BITS_OS
#define IBIS_FETCH_ID(id) (_ibis_id_table[id])
#define IBIS_FETCH_REC_ID(id) (_ibis_rec_id_table[id])
#define MAX_NUM_IBIS_IDS 1000
#define MAX_NUM_IBIS_REC_IDS 10000
#else
#define IBIS_FETCH_ID(id) ((XIBIS *)id)
#define IBIS_FETCH_REC_ID(id) ((XREC *)id)
#endif

/* private label properties */
#define IFILE_SEGMENT   "segment" 
#define IFILE_BLOCKSIZE "blocksize" 
#define IFILE_COFFSET   "coffset" 
#define IFILE_ASCII_LEN "ascii_len" 
#define ILABEL_NL        "nlb" 
#define ILABEL_HOST      "bhost" 
#define ILABEL_INTFMT    "bintfmt" 
#define ILABEL_REALFMT   "brealfmt" 
#define ILABEL_BTYPE     "bltype" 
#define IBTYPE_IBIS      "IBIS" 

#define MAX_COL I2_MAX_COL     /* Current somewhat arbitrary Max # columns */
#define MAX_OLD_COL I2_MAX_COL_OLD  /* Limit for IBIS-1 files */
#define EXTRA_COL 64     /* Extra column space for shuffling columns */
#define OLD_BLKSIZE 512L
#define NEW_BLKSIZE 512L
#define MAX_GRP_NAME (I2_MAX_GRP_NAME-1)
#define MAX_TYPE_NAME  (I2_MAX_TYPE_NAME-1)
#define MAX_VALUE_NAME (I2_MAX_VALUE_NAME-1)
#define I_TEMP_BUFSIZE 50000

#define CHECK_UNIT( ibis ) \
	if (!_i_find_value( x_ibis, (list_value) (ibis))) \
		return( IBIS_FILE_IS_NOT_IBIS )
#define CHECK_WRITE( ibis ) \
	if ((ibis)->flags & FLAG_MODE_READ) \
		return( IBIS_FILE_OPENED_READONLY )
#define CHECK_COLUMN( ibis, column ) \
	if ((column) <= 0 || (column) > (ibis)->nc ) \
		return (IBIS_NO_SUCH_COLUMN);
#define IS_LOCKED( ibis, col ) \
	((ibis)->column[(col)-1]->locked)
#define CHECK_LOCK( ibis, col ) \
	if (IS_LOCKED(ibis, col)) \
		return (IBIS_COLUMN_LOCKED);

#define CHECK_WRITE_FOR( ibis ) \
	if ((ibis)->flags & FLAG_MODE_READ) \
		{ *status=IBIS_FILE_OPENED_READONLY; return; }
#define CHECK_COLUMN_FOR( ibis, column ) \
	if ((column) <= 0 || (column) > (ibis)->nc ) \
		{ *status=IBIS_NO_SUCH_COLUMN; return; }
#define CHECK_LOCK_FOR( ibis, col ) \
	if ((ibis)->column[(col)-1]->locked) \
		{ *status=IBIS_COLUMN_LOCKED; return; }

/* 
 * useful macros giving the smallest
 * multiple of <alignv> greater than or equal to <minval>
 */
 
#define IBYTE_ALIGN 8
#define HOW_MANY( minval, alignv)  (((minval)+(alignv)-1)/(alignv))
#define ALIGN_UP( minval, alignv)  (HOW_MANY(minval,alignv)*(alignv))

/*
 *  Translation will be necessary only if:
 *     1) The file/host translation format-names are not equal; 
 *     2) The format is ASCII and this is an IBIS-1 file; or
 *     3) The formats are equal & non-ASCII, but the file/host
 *        translation method is nontrivial. In the docs for
 *        zvtrans it is noted that this is indicated by a nonzero
 *        value in the first integer entry of the trans object.
 *
 *	NEED_TRANS() gives the definition:
 */
	
#define NEED_TRANS( ibis, col ) \
	((col)->trans != NULL || \
	   ((col)->format >= FMT_ASCII && (ibis->flags & FLAG_FILE_OLD) ) || \
	   ((col)->format < FMT_ASCII  && (col)->format > FMT_NONE && \
	   	((ibis)->trans[(col)->format].outtrans.opaque[0] != 0)) )

#define TRANS_USED( ibis, col) \
  (((col)->trans) ? (col)->trans : \
   (ibis)->trans + ((col)->format > FMT_ASCII? \
     FMT_ASCII : (col)->format))

/**
 ** Format Codes
 **/
 
typedef enum {
	FMT_BASE=0,
	FMT_DEFAULT=0,
	FMT_NONE,
	FMT_BYTE,
	FMT_HALF,
	FMT_FULL,
	FMT_REAL,
	FMT_DOUB,
	FMT_COMP,
	FMT_ASCII,
	FMT_LAST=FMT_ASCII+256  /* This is where the max-ASCII is set */
} fmt_type;


/**
 ** Info flags and masks for XIBIS structure
 **/
 
typedef enum {
	FLAG_MOD_RECORDS = 1L,
	FLAG_MOD_LABELS  = FLAG_MOD_RECORDS	<<1,
	FLAG_FILE_OPEN	 = FLAG_MOD_LABELS	<<1,
	FLAG_FILE_OLD    = FLAG_FILE_OPEN 	<<1,
	FLAG_FILE_IMAGE  = FLAG_FILE_OLD 	<<1,
	FLAG_AUTO_INIT   = FLAG_FILE_IMAGE 	<<1,
	FLAG_ORG_ROW     = FLAG_AUTO_INIT 	<<1,
	FLAG_ORG_COLUMN  = FLAG_ORG_ROW 	<<1,
	FLAG_MODE_READ   = FLAG_ORG_COLUMN 	<<1,
	FLAG_MODE_WRITE  = FLAG_MODE_READ 	<<1,
	FLAG_MODE_UPDATE = FLAG_MODE_WRITE 	<<1,
	FLAG_LAST	 	 = FLAG_MODE_UPDATE <<1
} flag_type;

typedef enum {
	FLAG_REC_REFRESH=1,
	FLAG_REC_GAPS=2
} recflag_type;

typedef enum {
	MASK_MODS = FLAG_MOD_RECORDS	| FLAG_MOD_LABELS,
	MASK_ORG  = FLAG_ORG_ROW	| FLAG_ORG_COLUMN,
	MASK_MODE = FLAG_MODE_READ	| FLAG_MODE_WRITE	| FLAG_MODE_UPDATE
} mask_type;


/* messages which may be sent to records */
typedef enum {
	NOTICE_FLUSHING_ROWS,
	NOTICE_NEED_ROWS,
	NOTICE_OPEN,
	NOTICE_CLOSING
} notice_type;

#endif /* _H_IBISDEFS */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisglobals.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  ibisglobals.h
 **
 **  Global variables for IBIS subroutine library.
 **/
 
#ifndef _H_IBISGLOBALS
#define _H_IBISGLOBALS

/**
 **  Global Variables
 **/

extern List     *x_ibis;
extern char     *format_name[];
extern int	format_size[];
extern char	*format_label[];
extern char	*_ibis_current_module;
extern int 	default_format;

/* ID tables for external interface.  See ibisdefines.h. */

#if POINTERS_64_BITS_OS
extern XIBIS *_ibis_id_table[];
extern XREC *_ibis_rec_id_table[];
#endif

#endif /* _H_IBISGLOBALS */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibislists.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* ibislists.h */

#ifndef _H_ILIST
#define _H_ILIST

/* public types */

typedef int (*list_value);

struct List {
	struct List *next;
	struct List *prev;
	void (*destruct)(list_value); /* destruction method for value */
	list_value value;
};
typedef struct List List;


/* public declarations */

List* _i_new_list( void (*kill_method)(list_value));
void _i_free_list( List *list);
List* _i_find_value(List *list, list_value value);
void _i_insert_value(List *list, list_value value);
void _i_append_value(List *list, list_value value);
void _i_delete_value(List *list, list_value value);


#endif /* _H_ILIST */

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisstructs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  ibisstructs.h
 **
 **   Structure definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISSTRUCTS
#define _H_IBISSTRUCTS

#include "ibisdeclares.h"

struct XIBIS {

	/* basic properties */

	int unit;		/* VICAR unit number			*/
	int nc;			/* number of active columns		*/
	int nr;			/* number of active rows		*/
	int nl;		  	/* VICAR 'NL'				*/
	int ns;		  	/* VICAR 'NS'				*/
	fmt_type pix_fmt;	/* VICAR pixel data 'FORMAT'		*/
	int recsize;	  	/* VICAR 'RECSIZE'			*/
	int blocksize;  	/* Usable portion of each VICAR line	*/
	int numblocks;  	/* number of blocks (VICAR NL)		*/
	int segment;		/* columnsize = datasize*segment(org=col)*/
				/* rowsize = segment(org=row)		*/
	int extent;		/* max of offset+datasize over all cols */

	/* Auxiliary information */

	flag_type   flags;      /* flags indicating file condition	*/
	char        type[MAX_GRP_NAME+1]; /* IBIS subtype               */
	
	
	/* lists of "members" */
	
	XCOL **column;		/* Array of active columns (MAXCOL) 	*/
	List *record;		/* list of currently opened records	*/
	List *groups;		/* list of groups 			*/
	List *units;		/* list of units 			*/
	List *formats;		/* list of formats 			*/
	List *locals;		/* list of local groups 	*/
	List *gaps;		/* Available free space in file		*/

	/* Implementation-dependent methods */
	
	t_rowmethod	*rowmethod;
	t_colmethod	*colmethod;
	t_recmethod	*recmethod;
	t_filemethod	*filemethod;
	t_labmethod	*labmethod;
	
	/* format translators & related info */
	
	char            pix_host[MAX_GRP_NAME+1];/* VICAR Image Data host format        */
	char            hostfmt[MAX_GRP_NAME+1]; /* internal host format for conversion */
	char            intfmt[MAX_GRP_NAME+1];  /* internal host format for conversion */
	char            realfmt[MAX_GRP_NAME+1]; /* internal host format for conversion */
	char       	*fmt;		  /* holds fmt_len-char input format array 	*/
	int       	fmt_len;	  /* length of input format array elements 	*/
	fmt_type       	default_fmt;	  /* default value for fmt			*/
	trans_type      *trans;		  /* translators for file (BYTE...ASCII)	*/
	int		*format_size;	  /* internal host file data sizes 		*/
};

struct XCOL {
	int id;			/*  column number 			*/
	int offset;      	/*  offset in file to col		*/
	int locked;		/*  # of column translation locks 	*/
	fmt_type format; 	/*  column data format   		*/
	trans_type *trans;	/*  specified translation		*/
	XGROUP *unit;		/*  UNITS group (must be unique)	*/
};

struct XREC {
        XIBIS* ibis;		/* IBIS file owning record		*/
	int cur_row;		/* current active row			*/
	int top_row;		/* first row in buffer			*/
	int num_rows;		/* #rows in buffer			*/
	int mod_row;		/* first modified row (0=top of buffer)	*/
	int num_mods;		/* # modified rows	(0 if none)	*/
	int num_cols;		/* #cols				*/
	int recsize;		/* size of whole record	(in file)	*/
	int flags;		/* 1 =refresh,2=gaps			*/
	XCOL **column;		/* columns used in record		*/
	fmt_type format;	/* current translation type 		*/
	trans_type *trans;	/* translators for record 		*/
	char *inspace; 		/* memory allocated for inbuffer	*/
	char *outspace; 	/* memory allocated for outbuffer	*/
	char **inbuffer;	/* input data buffer			*/
	char **outbuffer;	/* output data buffer			*/
	List *collaborators;	/* other records sharing common columns */
	t_recmethod *method;	/* methods inherited from file		*/
};

struct t_recmethod {
	XMethod dofile; /* Performs read,write of buffer */
	XMethod notice; /* handle a message from somewhere */
};

struct t_rowmethod {
	XMethod dofile; /* Performs read,write,clear */
	XMethod new;
	XMethod delete;
	XMethod getsize; /* returns bytes-per-row of buffer passed to dofile */
};

struct t_colmethod {
	XMethod dofile; /* Performs read,write,clear */
	XMethod new;
	XMethod delete;
};

struct t_filemethod{
	XMethod open;
	XMethod init;
	XMethod clear;
	XMethod close;
};

struct t_labmethod{
	XMethod pre_open;	/* set up struct prior to open		*/
	XMethod post_open;	/* set up struct after open		*/
	XMethod flush;		/* sync struct values with file		*/
};

struct XGROUP {
	char name[MAX_GRP_NAME+1];
	List *columns; 		/* list of columns with group name 	*/
};

struct XGAP {
	int offset; 		/* offset to start (in COFFSET units)	*/
	int end; 		/* offset to end of gap			*/
};

typedef struct zv_trans {	/* External definition is "12 ints".	*/
	int opaque[12];		/* Internal definition is opaque to RTL */
} zv_trans;

struct trans_type {
	zv_trans intrans;  /* put these first so they're aligned well */
	zv_trans outtrans;
	int *format_size; /* file format sizes */
	int new;	/* new IBIS */
	fmt_type infmt;
	fmt_type outfmt;
};

#endif /* _H_IBISSTRUCTS */
$ VOKAGLEVE
$ Return
$!#############################################################################
$Doc_File:
$ create ibisfac.msg
$ DECK/DOLLARS="$ VOKAGLEVE"
!
!	Message file for facility IBIS
!
!  This file contains the message id's and detailed texts for
!  the IBIS2 sublib internal errors.
!
!
.KEY ALRDYOPN 	 !Symbolic Name: IBIS_FILE_ALREADY_OPENED
Explanation: 
The file unit passed to IBISFileUnit has already been installed
in another ibis descriptor.

User action:
This is a program error; the cognizant programmer should be notified.

Programmer action:
Make sure that previous IBIS file descriptors that were passed this
VICAR file unit have been closed and deleted.


.KEY CANTTRANS 	 !Symbolic Name: IBIS_CANT_TRANSLATE
Explanation:
Either an ASCII formatted column has just had its U_FORMAT set to
a numerical format, or a numerically formatted column has just had
its U_FORMAT set to an ASCII format. This may happen in either the
IBISColumnSet routines or the IBISRecord routines

User action:
Do not try to perform numerical operations on ASCII columns and vis-versa.
This may also be a program error.

Programmer action:
Make sure to check the FORMAT of a column before setting the
U_FORMAT attribute.


.KEY EMPTYGRP 	 !Symbolic Name: IBIS_GROUP_IS_EMPTY
Explanation:
A requested group of columns turned out to contain no columns. This
may happen if a group-expression contains no columns satisfying expression,
or if a null set of columns is passed into a group or record-defining routine.

User action:
If you are defining a group of columns using a group-expression,
make sure that the defined group contains some columns.

Programmer action:
Make sure that if a null pointer is passed to IBISRecordOpen, that a
nontrivial group is passed in instead.


.KEY GENERR 	 !Generic error in IBIS
Explanation: 
An error status was returned by IBIS.

User action: 
Refer to the Key indicated on the following line.


.KEY GRPEXISTS 	 !Symbolic Name: IBIS_GROUP_ALREADY_EXISTS
Explanation: 
An attempt was made to define a group name which has already been defined.

User action: 
Check the name of the group you have defined against the names defined in
the IBIS property label.


.KEY IMAGEDAT 	 !Symbolic Name: IBIS_CONTAINS_IMAGE_DATA
Explanation:
A VICAR image has been appended to the IBIS file, and the current
operation requires that the IBIS data be extended beyond its current
boundaries; the IBIS-2 library cannot currently move the image data down.

User action:
Copy the IBIS data to a separate file before modifying it.


.KEY INVALCPAR 	 !Symbolic Name: IBIS_COLUMN_PARM_INVALID
Explanation:
An attribute was requested in IBISColumnGet/Set which does not exist.

User action:
This is a program error; contact the cognizant programmer.

Programmer action:
Check the keywords passed into the Column routines for spelling. The
"ibisfile.h" include file has all of the defined keywords for the
C-language interface.


.KEY INVALFMT 	 !Symbolic Name: IBIS_INVALID_FORMAT
Explanation:
A format was specified which was not recognized or invalid for
this routine. This error may also be returned if an attempt is made
to set the pixel format of a file to an ASCII type.

User action:
Make sure the format parameters are spelled correctly. This may also be
a program or subroutine system error; contact the cognizant programmer.

Programmer action:
Check format parameters passed into routines. This error may also result
from an internal IBIS error on closing or deleting a file; if so, contact
the cognizant system programmer.


.KEY INVALNAM 	 !Symbolic Name: IBIS_INVALID_GRPNAME
Explanation:
An attempt was made to name a group using either colons or  non-printable
characters.

User action:
Do not use colons in defining a group name. Any other printable character
is permissible, including spaces. If none were used, this is most likely
a program error; contact the cognizant programmer.

Programmer action:
Make sure that valid strings are being passed into the group
definition routines.


.KEY INVALPAR 	 !Symbolic Name: IBIS_INVALID_PARM
Explanation:
An invalid keyword was passed into an IBIS routine.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check the keywords passed into the IBISFile, IBISGroup and
IBISRecord routines.


.KEY INVALTYP 	 !Symbolic Name: IBIS_INVALID_TYPE
Explanation:
An invalid group type was passed to a routine. 

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check "type" names passed into routines and group-expressions. Valid names
are singular, case-insensitive, and are in the list 
{FORMAT | UNIT | GROUP | LOCAL }. Not all of these are valid for all 
routines; for example, FORMAT groups may not be deleted or defined.


.KEY LASTROW 	 !Symbolic Name: IBIS_LAST_ROW
Explanation:
An attempt was made to read or write a row of data past the last
row of the file.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
In order to append rows to the end of a file, use IBISFileSet to change the
'NR' value of the file first; this will size up the file and change the label
to permit appending new rows.


.KEY LENGTHREQ 	 !Symbolic Name: IBIS_LENGTH_REQUIRED
Explanation:
An attempt was made to access a string-array without specifying the inner
length of the array.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
This error should only arise in the C-interface routines; check that the
"length" parameter was not defaulted for multi-valued string arrays.
If your code is using the FORTRAN bridge routines, this error is an
internal IBIS subroutine error; contact the cognizant system programmer.


.KEY LIMEXCD 	 !Symbolic Name: IBIS_COLUMN_LIMIT_EXCEEDED
Explanation:
An attempt was made to create an IBIS file with more than the current limit.

User action:
An IBIS file currently may have no more than 1024 columns. There is no
limit on the number of rows, so check to see if you can accomplish your
task by storing data in successive rows instead.


.KEY LOCKCOL 	 !Symbolic Name: IBIS_COLUMN_LOCKED
Explanation:
An attempt was made to delete a column which is locked by an open record.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Close all records containing this column before deleting it.


.KEY MEMFAIL 	 !Symbolic Name: IBIS_MEMORY_FAILURE
Explanation:
There is no more memory available for program. 

User action:
This may be a program error. Contact the cognizant programmer.

Programmer action:
A likely cause of this error is the attempt to read in a very large column
of data all at once, whose buffer ate up too much memory for the IBIS routines
to be able to do their work. Try working with only part of a column at a
time, using the "srow, nrows" parameters in IBISColumnRead/Write.
Alternatively, if you are working with records, try sizing down some of
the records (the NR value) with IBISRecordSet to free up some more space.


.KEY MODFORMAT 	 !Symbolic Name: IBIS_CANT_MODIFY_FORMAT
Explanation:
FORMAT groups are implicitly defined by setting the format of the column.
You cannot explicitly create or delete them.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Do not attempt to directly modify FORMAT groups. You can always create other
groups which are copies of the FORMAT group, and then modify as you wish.


.KEY NCREQD 	 !Symbolic Name: IBIS_NC_REQUIRED
Explanation:
This is an IBIS-1 GRAPHICS file, which contains no explicit NC (Dimension)
information.

User action:
You must supply a dimension parameter to a GRAPHICS-1 file, as there is
no default dimension and no dimension information in the file.

Programmer action:
To make an IBIS program "GRAPHICS-1 Friendly", you should provide a "G1DIM"
parameter to permit the specification of graphics file dimensions.


.KEY NOSUCHCOL 	 !Symbolic Name: IBIS_NO_SUCH_COLUMN
Explanation:
A column was referenced which does not exist, according to the current
IBIS property label.

User action:
Check the list of columns requested in your parameter list. This may
also be a program error; inform the cognizant programmer.

Programmer action:
Make sure that the columns passed into the IBISRecord/IBISColumn routines
are all valid columns.



.KEY NOTFOUND 	 !Symbolic Name: IBIS_GROUP_NOT_FOUND
Explanation:
A group was referenced which does not appear in the list of groups defined
for this file.

User action:
Check the list of defined groups in the IBIS property label.


.KEY NOTIBIS 	 !Symbolic Name: IBIS_FILE_IS_NOT_IBIS
Explanation:
The VICAR label of this file neither contains an IBIS property label,
nor does it have the attributes of an IBIS-1 file. 

User action:
Check the file label.


.KEY NOTOPEN 	 !Symbolic Name: IBIS_FILE_NOT_OPEN
Explanation:
The IBIS file descriptor contains a VICAR file unit which is not
currently open.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
If you have called IBISFileUnit, then you must now call IBISFileUnitOpen
to access or create the IBIS file. This also applies to file descriptors
that were closed with the "UKEEP" option in IBISFileClose.


.KEY NSNOTSET 	 !Symbolic Name: IBIS_MUST_SET_NS_FIRST
Explanation:
An attempt was made to change the VICAR image 'FORMAT' or 'HOST' values before
the pixel 'NS' was set.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
To set the VICAR image data fields prior to creating a new IBIS file,
first set the 'NS' sample count; from there you may then modify the
FORMAT and HOST values without error.


.KEY NVALMOD 	 !Symbolic Name: IBIS_INVALID_OPEN_MODE
Explanation:
A mode was chosen in IBISFileOpen or IBISFileUnit which was not "read",
"write", "owrite", or "update".

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check the keywords passed into IBISFile routines. If you are using "C",
use the macros defined in "ibisfile.h".


.KEY OLDIBIS 	 !Symbolic Name: IBIS_FILE_OLD_IBIS
Explanation:
The requested operation is not supported for old IBIS files. This includes
setting a column format to a size larger than 4-bytes per column element,
and other features introduced in the IBIS-2 format specifications.

User action:
Do not try to set column formats to anything requiring  more than 4 bytes
per element. This may also be a program error.

Programmer action:
Check the file version type before modifying the FORMAT attribute of a column.
The routine IBISColumnMove will also return this error for any IBIS-1
format file.


.KEY RDONLY 	 !Symbolic Name: IBIS_FILE_OPENED_READONLY
Explanation:
An attempt was made to modify an IBIS file which is currently opened read-only.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Open the file with 'update' mode in order to modify it.

.KEY VICMSG	!Error reported to IBIS from VICAR
Explanation:
An error was returned from IBIS from the VICAR2 runtime
library. Refer to the 'VIC2' keyword on the line following.

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibis2.imake
#define SUBROUTINE ibis2

#define P1_SUBLIB
#define USES_ANSI_C
#define FTN_STRING

#define TAE_ERRMSG ibisfac

#define MODULE_LIST \
	ibis_col.c \
	ibis_file.c \
	ibis_globals.c \
	ibis_label.c \
	ibis_group.c \
	ibis_rec.c \
	ibis_row.c \
	ibis_signal.c

#define MODULE_LIST2 \
	mthd_file_old.c \
	mthd_file_new.c \
	mthd_io_col.c \
	mthd_io_row.c \
	mthd_io_old.c \
	mthd_io_gr1.c \
	mthd_labl_new.c \
	mthd_labl_old.c \
	mthd_labl_gr1.c \
	mthd_null.c
	
#define MODULE_LIST3 \
	struct_lists.c \
	struct_xcol.c \
	struct_xgap.c \
	struct_xgroup.c \
	struct_xibis.c \
	struct_xrec.c \
	util_file.c \
	util_format.c \
	util_strings.c \
	util_trans.c

#define INCLUDE_LIST \
	ibis.h \
	ibisdeclares.h \
	ibisdefines.h \
	ibisglobals.h \
	ibislists.h \
	ibisstructs.h

$ Return
$!#############################################################################
$Test_File:
$ create tibis2.c
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




$!-----------------------------------------------------------------------------
$ create tibis2_for.f
C
C       This routine gives an example of how to use the IBIS-2
C       subroutine package using the FORTRAN bridges.
C
C	Uses: Ported VICAR runtime library,
C       IBIS-2 subroutine library.
C

	include 'VICMAIN_FOR'

	subroutine main44
	! Call the C routine from here so that
	! The file-I/O will work ok
	call main44_c
	return
	end

	subroutine fortran_test
	logical xvptst
	if (xvptst('A4TEST')) then
		call a4test
	else
		call copytest
	endif
	return
	end

c
c  a4test tests the fortran a4 conversion
c   for old IBIS files.
c

	subroutine a4test

	integer i,unit,ibis,status 
	character*4  buf(10) 


	call xvunit(unit,'inp',1,status,0) 
	if (status.ne.1) call xvsignal(unit,status,1) 

	call  ibis_file_open(unit,ibis,'read',0,0,
     +                                ' ',' ',status)
	if (status .ne. 1) call ibis_signal_u(unit,status,1) 

	call  ibis_column_set(ibis,'FORMAT','A4',1,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 
	call  ibis_column_set(ibis,'U_FORMAT','A4',1,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 

	call  ibis_column_read(ibis,buf,1,1,3,status) 
	if (status .ne. 1) call ibis_signal(ibis,status,1) 

	do i=1,3
		call xvmessage(buf(i),' ') 
	enddo

	call ibis_file_close(ibis,' ',status)
	return
	end


C	copytest: Copies one general new IBIS file to another,
C	possibly with a change in organization (row/column).
C
	subroutine copytest
	integer inunit,outunit,status
	integer count
	integer  ibis_out,ibis_in,ibufsize
	parameter (ibufsize=250)
	integer nc, nr, col,colsize
	integer row, row_inc, nrow 
	character*16  org 
	character*6 format(1000) 
	character*6 cfmt
	logical ascii
	character*64 cbuffer(ibufsize*8)
	real*8 buffer(ibufsize*64)
	! The equivalence is just to conserve memory:
	equivalence (buffer,cbuffer)

C   open the input IBIS file, abort on error. The four '0' arguments
C   in the ibis_file_open are only used for creating output files
	
	call xvunit( inunit, 'inp', 1, status, ' ') 
	call ibis_file_open( inunit, ibis_in, 'read', 0,0,' ',' ', status) 
	if (status.ne.1) call ibis_signal_u( inunit, status, 1) 

C  get the # rows & columns and the column-format
C  of the input IBIS file:

	count = ibis_file_get( ibis_in,'nc', nc, 1,1 )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)
	count = ibis_file_get( ibis_in,'nr', nr, 1,1 )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)
	count = ibis_file_get( ibis_in,'formats', format, 1, nc )
	if (count.lt.0) call ibis_signal( ibis_in, count, 1)

C open the output file with identical structure. The routine returns
C in variable 'ibis_out' the handle to the IBIS file, which should be used
C in all IBIS file-io operations
	
	call xvpone( 'org', org, 1, 0 ) 
	call xvunit( outunit, 'out', 1,status, ' ') 
	call ibis_file_open( outunit, ibis_out, 'write', nc, 
     +  	             nr, format, org, status ) 
	if (status.ne.1) call ibis_signal_u( outunit, status, 1) 
	
C   copy the column data from input to output

	do col=1,nc
	
		call ibis_column_get( ibis_in, 'format', cfmt, col, status )  
		if (status .ne.1) call ibis_signal( ibis_in, status, 1)
		ascii = (cfmt(1:1).eq.'a'.or.cfmt(1:1).eq.'A')
		if (ascii) then
			call ibis_column_set( ibis_in, 'u_format','a64',col,status)
			if (status .ne.1) call ibis_signal( ibis_in, status, 1)
			call ibis_column_set( ibis_out, 'u_format','a64',col,status)
			if (status .ne.1) call ibis_signal( ibis_out, status, 1)
		endif
		
		! Get the size of a column's element, in bytes
		call ibis_column_get( ibis_in, 'size', colsize, col, status ) 
		if (status .ne.1) call ibis_signal( ibis_in, status, 1)
		
		! Compute #elements that will fit into buffer
		row_inc = (ibufsize + colsize  - 1)/colsize  
		
		! Actually do some writing. The ibis_column_read/write calls
		! read/write <nrow> rows of column #<col> from buffer,
		! starting at row #<row>. The data is automatically converted
		! into the native host format of the given column.
		
		do row = 1,nr,row_inc
			nrow = MIN( nr+1-row , row_inc) 

			! If this is an ASCII column, need to use the A*64 buffer:
			! Note that we could have used the same buffer if we 
			! just turned the U_FORMAT to NONE.
			
			if (ascii) then
			   call ibis_column_read( ibis_in, cbuffer, 
     +				col, row, nrow,status )
			else
			   call ibis_column_read( ibis_in, buffer, 
     +				col, row, nrow,status )
			endif
     
     			! We know that ibis_in is a valid IBIS handle here
     			! so in case of error, we can call ibis_signal
     	
			if (status.ne.1) call ibis_signal( ibis_in, status, 1) 

			! If this is an ASCII column, need to use CHAR:
			if (ascii) then
			   call ibis_column_write( ibis_out, cbuffer, 
     +				col, row, nrow,status )
			else
			   call ibis_column_write( ibis_out, buffer, 
     +				col, row, nrow,status )
			endif
			if (status.ne.1) call ibis_signal( ibis_out, status, 1) 
		enddo
	enddo

C close up shop 

	call ibis_file_close( ibis_in, ' ', status ) 
	if (status.ne.1) call ibis_signal( ibis_in, status, 1) 
	call ibis_file_close( ibis_out,' ', status )
	if (status.ne.1) call ibis_signal( ibis_out, status, 1) 
	
	return
	end

$!-----------------------------------------------------------------------------
$ create tibis2.pdf
process

subcmd fortran
	parm inp string count=1
	parm out string count=0:1 def=--
	parm org keyword valid=(row,column) def=column
	parm mode keyword valid=(a4test,copytest) def=copytest
end-sub

subcmd new
	parm out string count=1
	parm gr1_nc integer default=0
	parm org keyword valid=(row,column) default=column
	parm nr integer def=10
	parm nc integer def=10
	parm format (string,4) string count=0:20 default=--
	parm version keyword valid=(ibis1,ibis2) def=ibis2
end-sub

subcmd list
	parm inp string count=1
	parm gr1_nc integer default=0
	parm sr integer def=1
	parm sc integer def=1
	parm nr integer def=5
	parm nc integer def=10
end-sub

subcmd coldelete
	parm inp string count=1
	parm gr1_nc integer default=0
	parm col integer count=1
	parm ncol integer count=1
end-sub

subcmd colinsert
	parm inp string count=1
	parm gr1_nc integer default=0
	parm col integer count=1
	parm ncol integer count=1
	parm format  keyword valid=(BYTE,HALF,FULL,REAL,DOUB,COMP,A8) +
		 count=1 def=REAL
end-sub

subcmd signal
end-sub

subcmd rowdelete
	parm inp string count=1
	parm gr1_nc integer default=0
	parm row integer count=1
	parm nrow integer count=1
end-sub

subcmd rowinsert
	parm inp string count=1
	parm gr1_nc integer default=0
	parm row integer count=1
	parm nrow integer count=1
end-sub

subcmd rowclear
	parm inp string count=1
	parm gr1_nc integer default=0
	parm row integer count=1
	parm nrow integer count=1
end-sub

subcmd recread
	parm inp string count=1
	parm gr1_nc integer default=0
	parm cols integer count=1:20
	parm srow integer count=1
	parm nrow integer count=1
	parm format keyword valid=(BYTE,HALF,FULL,REAL,DOUB,COMP,A8) +
		 count=1 def=REAL
end-sub

subcmd recwrite
	parm inp string count=1
	parm gr1_nc integer default=0
	parm cols integer count=1:20
	parm inrow integer count=1
	parm outrow integer count=1
	parm nrows integer count=1
	parm recrows integer count=1
	parm format keyword valid=(BYTE,HALF,FULL,REAL,DOUB,COMP,A8) +
		 count=1 def=REAL
end-sub

subcmd reczero
	parm inp string count=1
	parm gr1_nc integer default=0
	parm cols integer count=1:20
	parm srow integer count=1
	parm nrow integer count=1
	parm format keyword valid=(BYTE,HALF,FULL,REAL,DOUB,COMP,A8) +
		 count=1 def=REAL
end-sub

subcmd grpdefine
	parm inp string count=1
	parm gr1_nc integer default=0
	parm type keyword valid=(format,unit,group) def=group
	parm grpname string count=1
	parm cols integer count=0:20 def=--
	parm expr string count=0:1 def=--
end-sub

subcmd zaplabel
	parm inp string count=1
end-sub

end-proc

$!-----------------------------------------------------------------------------
$ create tibis2.imake
#define PROGRAM tibis2

#define MODULE_LIST tibis2_for.f tibis2.c
#define MAIN_LANG_FORTRAN
#define USES_C
#define USES_FORTRAN
#define FTN_STRING

#define TEST

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL

$!-----------------------------------------------------------------------------
$ create tstibis2.pdf
procedure
refgbl $autousage
body

let $autousage="none"

tibis2-new oldibis nc=20 nr=100 'ibis1 'column

let _onfail="continue"

! Check bad parameters (FR#83084)
write "*********************************************"
write "************ Error Checking Tests       *****"
write "*********************************************"
write "**** All of these tests should safely abend *"
write "**** with an [IBIS-KEYWORD] explanation   ***"
write "*********************************************"
 tibis2-recread oldibis cols=(1000) srow=1 nrow=4 'full
 tibis2-recread oldibis cols=(1) srow=1000 nrow=4 'full
 tibis2-recread oldibis cols=(1) srow=99 nrow=4 'full
 tibis2-new oldibis nc=200 nr=100 'ibis1 'column
 tibis2-new temp nc=5000 nr=100 'ibis2 'column
 tibis2-new temp  nr=30 form=("BYTE","HALF","BAD")

let _onfail="return"

write "*********************************************"
write "************ IBIS-1 COLUMNS (INTERFACE) *****"
write "*********************************************"

 ! With 2 columns and 10 rows and make sure that it works
 ! even though the file type is IMAGE (FR#85008). The file will 
 ! have the local host listed in label, which should be ignored.
 gen a ns=512 nl=3
  !This little hack generates a VAX byte-order FULL integer 10:
 f2 a oldibis fun="10*(samp==1)*(line==1)"
 write "This file should list as IBIS-1, ORG=COLUMN 10 rows and 2 columns"
 tibis2-list oldibis

 tibis2-new oldibis nc=20 nr=100 'ibis1 'column
 tibis2-list oldibis

 tibis2-coldelete oldibis col=2 ncol=2
 tibis2-list oldibis
 tibis2-colinsert oldibis col=3 ncol=4
 tibis2-list oldibis
 tibis2-coldelete oldibis col=3 ncol=4
 tibis2-list oldibis

 tibis2-rowinsert oldibis row=2 nrow=2
 tibis2-list oldibis
 tibis2-rowdelete oldibis row=3 nrow=2
 tibis2-list oldibis
 tibis2-rowclear oldibis row=4 nrow=1
 tibis2-list oldibis

 tibis2-recread oldibis cols=(1,3,5,7,5,3) srow=1 nrow=4 'full
 tibis2-recwrite oldibis cols=(1,3,5) inrow=1 outrow=6 nrow=6 rec=3 'full
 tibis2-list oldibis nr=15
 tibis2-reczero oldibis cols=(1,3,5) srow=2 nrow=2 'full
 tibis2-list oldibis

write "*********************************************"
write "************ IBIS-1 ROW (GRAPHICS) **********"
write "*********************************************"

 !Build an artificial VAX/VMS organized GRAPHICS-1 file
 ! even though the file type is IMAGE (FR#85787). The file will 
 ! have the local host listed in label, which should be ignored.
 gen a ns=512 nl=1 'BYTE  ival=0 linc=0 sinc=0
 write "This file should list as IBIS-1, ORG=ROW 64 rows and 2 columns"
 write "The values should be all 0 "
 tibis2-list a gr1=2 nc=10

 !Build an artificial VAX/VMS organized IBIS-1 file

 tibis2-new oldibis nc=20 nr=100 'ibis1 'row
 tibis2-list oldibis gr1_nc=20

 tibis2-coldelete oldibis col=2 ncol=2 gr1_nc=20
 tibis2-list oldibis gr1_nc=18
 tibis2-colinsert oldibis col=3 ncol=4 gr1_nc=18
 tibis2-list oldibis gr1_nc=22
 tibis2-coldelete oldibis col=3 ncol=4 gr1_nc=22
 tibis2-list oldibis gr1_nc=18

 tibis2-rowinsert oldibis row=2 nrow=2 gr1_nc=18
 tibis2-list oldibis gr1_nc=18
 tibis2-rowdelete oldibis row=3 nrow=2 gr1_nc=18
 tibis2-list oldibis gr1_nc=18
 tibis2-rowclear oldibis row=4 nrow=1 gr1_nc=18
 tibis2-list oldibis gr1_nc=18

 tibis2-recread oldibis cols=(1,3,5,7,5,3) srow=1 nrow=4 'full gr1_nc=18
 tibis2-recwrite oldibis cols=(1,3,5) inrow=1 outrow=6 nrow=6 rec=3 'full gr1_nc=18
 tibis2-list oldibis nr=15 gr1_nc=18
 tibis2-reczero oldibis cols=(1,3,5) srow=2 nrow=2 'full  gr1_nc=18
 tibis2-list oldibis gr1_nc=18

write "*********************************************"
write "************ IBIS-2 COLUMNS *****************"
write "*********************************************"

 tibis2-new temp nc=20 nr=100 'column
 tibis2-list temp

 tibis2-coldelete temp col=2 ncol=2
 tibis2-list temp
 tibis2-colinsert temp col=3 ncol=4
 tibis2-list temp
 tibis2-coldelete temp col=3 ncol=4
 tibis2-list temp

 tibis2-rowinsert temp row=2 nrow=2
 tibis2-list temp
 tibis2-rowdelete temp row=3 nrow=2
 tibis2-list temp
 tibis2-rowclear temp row=4 nrow=1
 tibis2-list temp

 tibis2-recread temp cols=(1,3,5,7,5,3) srow=1 nrow=4 'full
 tibis2-recwrite temp cols=(1,3,5) inrow=1 outrow=6 nrow=6 rec=3 'full
 tibis2-list temp nr=15
 tibis2-reczero temp cols=(1,3,5) srow=2 nrow=2 'full
 tibis2-list temp

write "*********************************************"
write "************ IBIS-2 ROWS ********************"
write "*********************************************"

 tibis2-new temp nc=20 nr=100 'row
 tibis2-list temp

 tibis2-coldelete temp col=2 ncol=2
 tibis2-list temp
 tibis2-colinsert temp col=3 ncol=4
 tibis2-list temp
 tibis2-coldelete temp col=3 ncol=4
 tibis2-list temp

 tibis2-rowinsert temp row=2 nrow=2
 tibis2-list temp
 tibis2-rowdelete temp row=3 nrow=2
 tibis2-list temp
 tibis2-rowclear temp row=4 nrow=1
 tibis2-list temp

 tibis2-recread temp cols=(1,3,5,7,5,3) srow=1 nrow=4 'full
 tibis2-recwrite temp cols=(1,3,5) inrow=1 outrow=6 nrow=6 rec=3 'full
 tibis2-list temp nr=15
 tibis2-reczero temp cols=(1,3,5) srow=2 nrow=2 'full
 tibis2-list temp


write "*********************************************"
write "************ IBIS-2 GROUP TESTS *************"
write "*********************************************"

! Test out group computations
 tibis2-new temp  nr=30 +
   form=("BYTE","HALF","FULL","REAL","DOUB", "FULL","HALF","HALF")
 tibis2-list temp
 tibis2-grpdefine temp  grpname="Evens" cols=(2,4,6,8) 'group
 tibis2-grpdefine temp  grpname="Odds" cols=(1,3,5,7) 'group
 tibis2-grpdefine temp  grpname="My Bytes" expr="format:byte" 'group
 tibis2-grpdefine temp +
   grpname="kg*m/sec^2" expr="format:half | real" 'unit
 tibis2-grpdefine temp +
   grpname="Even Newtons" expr="[kg*m/sec^2] * Evens" 'group
 tibis2-list temp

!
!Test IBISLabelRemove routine - zaps IBIS-2 labels (FR #85696)
!The label-list should yield no IBIS property
!
 tibis2-zaplabel temp
 label-list temp


write "*********************************************"
write "************ FORTRAN BRIDGE TESTS ***********"
write "*********************************************"

write "These two files should be equal:"

 tibis2-new temp  nr=30 form=("BYTE","A10","A100","FULL","REAL")
 tibis2-list temp
 tibis2-fortran temp temp1
 tibis2-list temp1
 tibis2-new temp  nr=3 nc=1 form="A4" 'ibis1 'column
 tibis2-fortran temp 'a4test

write "*********************************************"
write "************ ERROR SIGNALS TEST   ***********"
write "*********************************************"
 
! Test out the Error signals

 let _onfail="continue"

 tibis2-signal

end-proc
$ Return
$!#############################################################################
