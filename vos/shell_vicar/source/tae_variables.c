#include "tae_lib.h"
#include "strcasecmp.h"
#include "zvproto.h"
#include <string.h>
/* initialize variable structure, adding storage pointers */
int alloc_parm_valids(struct VARIABLE *v,int minc,int maxc,int count);
int alloc_parm_defaults(struct VARIABLE *v, int count);
int alloc_parm_values(struct VARIABLE *v, int count);

int initialize_variable
(
struct VARIABLE *v,
char	*name,	/* variable name		*/
int	type,	/* V_INTEGER, V_REAL, V_STRING	*/
		/* or V_NAME			*/
int	class,	/* V_GLOBAL, V_LOCAL, V_PARM	*/
int	keyflag,/* TRUE if variable is a keyword*/
int minc,       /* valid counts */
int maxc,
int nvalids,    /* number of valid vals to allocate */
int ndefs	/* number of defaults to allocate */
)
{
	int status;

	memset(v,0,sizeof(struct VARIABLE));

	/* add name,type & class */
	if (strlen(name) > NAMESIZ)
		tae_abort(INVNAME,name);

	shvic_make_upper_case(v->v_name, name);
	
	if ((type != V_INTEGER) && (type != V_REAL) &&
		(type != V_STRING)&&(type != V_NAME) )
		tae_abort(TYPERR,name);
		
	v->v_type = type;
	
	if (class != V_PARM)
		tae_abort(NOTUSED,name);

	v->v_class = class;
	
	if (class == V_PARM)
	{
		/* initialize V_PARM class flags */
		
		v->v_count = 0;	/* until set later by alloc_parm_values */
		
		v->v_keyword = keyflag;
		v->v_default = TRUE;
		
		/* parms created post TAE V 1.2, so force this to TRUE: */
		v->v_pv12 = TRUE;
		
		/* these are not relevant to standalone code */
		v->v_iparm = FALSE;
		v->v_page = FALSE;
		v->v_wasname = FALSE;
		v->v_deref = FALSE;
		
		switch (type)
		{
			case V_STRING:
				v->v_size = keyflag ? (VALIDSIZ+1) : (MAXSTRSIZ + 1);
				break;
			case V_INTEGER:
				v->v_size = sizeof(TAEINT);
				break;
			case V_REAL:
				v->v_size = sizeof(TAEFLOAT);
				break;
		}
		
		v->v_nullable = (minc == 0);

		if (v->v_keyword && nvalids == 0)
			tae_abort(VALIDREQ,v->v_name);
		status = alloc_parm_valids(v,minc,maxc,nvalids);
		if (status != SUCCESS)
			tae_abort(status,name);

		status = alloc_parm_defaults(v,ndefs);
		if (status != SUCCESS)
			tae_abort(status,name);

	} /* end V_PARM case */
	
	return SUCCESS;
}

/* Checks values of v against valid struct & count, if any */
/* If replace is True, then case-insensitive comparison is done, and	*/
/* string values that match a valid are replaced by the valid (used to	*/
/* get the case of defaults right)					*/

void check_values(struct VARIABLE *v, int replace)
{
	int i,j,good_val, test;
	struct S_VALID *sv;
	struct I_VALID *iv;
	struct R_VALID *rv;
	
	if (v->v_valid == NULL || v->v_count == 0) return;
	if (((struct I_VALID *)v->v_valid)->count == 0) return;
	
	good_val = TRUE;
		
	switch (v->v_type)
	{
	    case V_STRING:
		    sv = (struct S_VALID *) v->v_valid;
		    for (i=0;i<v->v_count;i++)
		    {
			for (j=0;j<sv->count;j++) {
			    test = strcasecmp(SVAL(*v,i),
					      sv->slist[j].string) != 0;
			    if (replace && !test) {
				strcpy(SVAL(*v,i), sv->slist[j].string);
				replace = FALSE;	/* replace once only */
			    }
			    good_val &= test;	/* test==0 means match */
			}
			if (good_val)
			    tae_abort(INVSTR,v->v_name);
		    }
	 	    break;
	    case V_REAL:
		    rv = (struct R_VALID *) v->v_valid;
		    for (i=0;i<v->v_count;i++)
		    {
			for (j=0;j<rv->count;j++)
			    good_val &= (RVAL(*v,i) > rv->range[j].high) ||
			    		(RVAL(*v,i) < rv->range[j].low); 
			if (good_val)
			    tae_abort(RANGE,v->v_name);
		    }
		    break;
	    case V_INTEGER:
		    iv = (struct I_VALID *) v->v_valid;
		    for (i=0;i<v->v_count;i++)
		    {
			for (j=0;j<iv->count;j++)
			    good_val &= (IVAL(*v,i) > iv->range[j].high) ||
			    		(IVAL(*v,i) < iv->range[j].low); 
			if (good_val)
			    tae_abort(RANGE,v->v_name);
		    }
		    break;
	    default:
		    break;
	}
}


/* read the values in value string into variable v */

int read_parm_string(struct VARIABLE* v, char* value, int vallen)
{
   int nelements;		
   char *valarray;
   int status;


   nelements = count_values(value,vallen, FALSE);
      
   if (nelements > 0)
   {
	status = alloc_parm_values(v,nelements);
	if (status != SUCCESS)
		tae_abort(status,v->v_name);

   	if (v->v_type == V_STRING)
   		valarray = SVAL(*v,0);
   	else
   		valarray = (char*) &IVAL(*v,0);

	if (strncmp(value,"--",2)==0)	/* NULL value for PARM */
		v->v_count = 0;
	else
   		get_parm_values(valarray, value, vallen, v->v_type, v->v_size,
								FALSE);

	check_values(v, FALSE);

   }
   else	/* NULL value */
   {
   	if (!v->v_nullable)
		tae_abort(NOTNULL,v->v_name);
	 v->v_cvp = (char *)NULL;
	 v->v_count = 0;
   }

   return SUCCESS;
}


int alloc_parm_valids(struct VARIABLE *v,int minc,int maxc,int count)
{

if ((minc<0) || (maxc<minc))
	return SCRCNT;

v->v_minc = minc;
v->v_maxc = maxc;

if (count > 0)
{
	v->v_valid = (void *) new_valid(count,v->v_type);

	if (v->v_valid == NULL)
		return PSETOVER;
}

return SUCCESS;
}




int alloc_parm_defaults(struct VARIABLE *v, int count)
{

if (count > v->v_maxc)
	return CNTERR;

v->v_dcount = count;
if (count > 0)
{
	v->v_dvp = new_data(count,v->v_type);
	if (v->v_dvp == NULL)
		return PSETOVER;
}


return SUCCESS;
}




int alloc_parm_values(struct VARIABLE *v, int count)
{

if (count > v->v_maxc)
	return CNTERR;

if (count > 0)
{
	v->v_cvp = new_data(count,v->v_type);
	if (v->v_cvp == NULL)
		return PSETOVER;
}


v->v_count = count;
v->v_default = FALSE;

return SUCCESS;
}



char *new_data(int nval, int type)   /* allocates memory for new data */
{
	char *val = 0,*sPtr;
	int i;
	
	if (nval > MAXVAL)
	   return NULL;
	
	switch (type)
	{
		case V_INTEGER:
			val = (char *)calloc(1,(nval * sizeof(TAEINT)));
   			memset(val, 0, (nval * sizeof(TAEINT)));
			break;
		case V_REAL:
			val = (char *)calloc(1,(nval * sizeof( TAEFLOAT )));
   			memset(val, 0, (nval * sizeof(TAEFLOAT)));
			break;
		case V_STRING:
			val = (char *)calloc(1,nval * (sizeof(char *) + MAXSTRSIZ+1));
   			memset(val, 0, (nval * (sizeof(char *) + MAXSTRSIZ+1)));
			break;
	}

	if (val == NULL) 
	   return NULL;
	
	if (type == V_STRING)
	{
		/* set the pointers to offsets of val pointer */
		sPtr=val+(nval*sizeof(char *));  
		for (i=0;i<nval;i++)
		{
			((TEXT **) val)[i] = sPtr;
			sPtr += (MAXSTRSIZ+1);
		}
	}
	
	return (val);
}


char *new_valid(int nvalid, int type)   /* allocates memory for a new valid struct */
{
	char *valid = 0,*sptr;
	int i;
	
	switch (type)
	{
		case V_INTEGER:
			valid = (char *)calloc(1,(sizeof(struct I_VALID) + 
				nvalid * sizeof(struct I_RANGE)));
			if (valid == NULL)
			   return NULL;
			((struct I_VALID*) valid)->count = nvalid;
			break;
		case V_REAL:
			valid = (char *)calloc(1,(sizeof(struct R_VALID) + 
				nvalid * sizeof(struct R_RANGE)));
			if (valid == NULL) 
			   return NULL;
			((struct R_VALID*) valid)->count = nvalid;
			break;
		case V_STRING:
			/**** THIS USES THE NEW POINTER VERSION ****/
			valid = (char *)calloc(1,sizeof(struct S_VALID) + 
							nvalid * (sizeof(char *) + VALIDSIZ+1));
			if (valid == NULL) 
			   return NULL;

			sptr=(char *) &((struct S_VALID*)valid)->slist[nvalid];  
			for (i=0;i<nvalid;i++)
			{
				((struct S_VALID*) valid)->slist[i].string = sptr;
				sptr += (VALIDSIZ+1);
			}

			((struct S_VALID*) valid)->count = nvalid;
			break;
	}

	return (valid);
}

