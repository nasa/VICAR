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
