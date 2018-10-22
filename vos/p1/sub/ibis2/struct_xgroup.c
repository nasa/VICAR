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



