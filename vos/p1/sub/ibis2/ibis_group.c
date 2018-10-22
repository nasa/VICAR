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

