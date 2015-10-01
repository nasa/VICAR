#include "tae_lib.h"
#include "zvproto.h"
#include seek_include
#include "strncasecmp.h"
#include <string.h>
#include <stdlib.h>

struct VARIABLE *shvic_p_fvar(struct PARBLK *parblk, char *key);
int add_std_vars_to_parblk(struct PARBLK *parblk, FILE *fp);

int add_pdf_variables_to_parblk(struct PARBLK *parblk,char* proc_file, 
				char*cmd_line)
{
	int status;
	char pdf_file[80],proc[80];
	FILE *fp;
	
	process_pdf_filename(proc_file,pdf_file,proc);

	fp = open_pdf(pdf_file);
	if ( fp == NULL)
		tae_abort(BADFILE,pdf_file);
	
	initialize_parblk(parblk,proc,fp);
	
	if (shvic_p_fvar(parblk,"_SUBCMD")!= NULL)
	{
		status = set_sub_command(parblk,cmd_line);
		if (status != SUCCESS)
			tae_abort(status,proc);
		add_subcmd_vars_to_parblk(parblk,fp);
	}

	add_std_vars_to_parblk(parblk,fp);

	fclose( fp );

	return SUCCESS;
}

int add_std_vars_to_parblk(struct PARBLK *parblk, FILE *fp)
{
	long cur_pos;
	int done;
	int in_subcmd=0;
	char *line;
	
	
	/* save current position and go to start of file */
	cur_pos = fseek( fp, 0L, SEEK_CUR);
	fseek(fp,0L, SEEK_SET);
	
	done = (fgetstr(&line,fp) < 0);
	if (done || (*line != 'P' && *line != 'p'))
		tae_abort(MISINTRO,"");

	/* scan the file for any PARM's not inside a SUBCMD */
	
	do 
	{
		done = (fgetstr(&line, fp) <0); 
		
		if (! done)
		{
			if (strncasecmp(line,"SUBCMD",6)==0)
				in_subcmd=TRUE;
			else if (strncasecmp(line,"END-",4)==0)
			{
				if (in_subcmd)
					in_subcmd = FALSE;
				else
					done=TRUE;
			}
		}
		if  ( !done && !in_subcmd && (strncasecmp(line,"PARM",4)==0) )
			add_one_pdf_variable_to_parblk(parblk,line);
		
	} while (! done);

	/* restore position of file */
	fseek(fp,cur_pos, SEEK_SET);
	
	return SUCCESS;
}


void add_one_pdf_variable_to_parblk(struct PARBLK *parblk,char line[])
{
	int ndefs,nvalids;
	int vcount[2],keyflag,type;
	int vlen,dlen;
	char parm[20], *vptr = 0, *dptr = 0;
	char *vvals,*dvals;
	struct VARIABLE var;

	parse_pdf_statement(line,parm,&type,
				&keyflag,vcount,&vvals,&vlen,&dvals,&dlen);
	nvalids = count_values(vvals,vlen, TRUE);
	ndefs = count_values(dvals,dlen, FALSE);

	initialize_variable(&var,parm,type,V_PARM,keyflag,
						vcount[0],vcount[1],nvalids,ndefs);
	
	if (nvalids > 0)
	{
		switch(type)
		{
		case V_STRING:
			vptr = ((struct S_VALID*)var.v_valid)->slist[0].string;
			break;
		case V_INTEGER:
			vptr = (char *) ((struct I_VALID*)var.v_valid)->range;
			break;
		case V_REAL:
			vptr = (char *) ((struct R_VALID*)var.v_valid)->range;
			break;
		}
		get_parm_values(vptr,vvals, vlen, type, var.v_size, TRUE);
	}	
	if (ndefs > 0)
	{
		switch(type)
		{
		case V_STRING:
			dptr = DSVAL(var,0);
			break;
		case V_INTEGER:
			dptr = (char *) (&DIVAL(var,0));
			break;
		case V_REAL:
			dptr = (char *) (&DRVAL(var,0));
			break;
		}
		if (strncmp(dvals,"--",2)==0 || strncmp(dvals,"\"--\"",4)==0)
			var.v_dcount = 0;
		else
			get_parm_values(dptr,dvals,dlen,type,var.v_size,FALSE);
	}	

	add_variable_to_parblk(parblk,&var);

}



int initialize_parblk(struct PARBLK *parblk,char *proc,FILE *fp)
{
	struct VARIABLE vptr;
	char   *value;

 	/* first, generate _PROC parm (keyflag=FALSE) */	

	initialize_variable(&vptr,"_PROC",V_STRING,V_PARM,NOT_KEYWORD,1,1,0,1);
	shvic_make_upper_case(proc,proc);
	read_parm_string(&vptr,proc,strlen(proc));
	add_variable_to_parblk(parblk,&vptr);
		
	/* next, generate $RUNTYPE keyword  */	

	initialize_variable(&vptr,"$RUNTYPE",V_STRING,V_PARM,IS_KEYWORD,1,1,1,1);
	strcpy(((struct S_VALID *)vptr.v_valid)->slist[0].string,"INTERACTIVE");
	read_parm_string(&vptr,"INTERACTIVE",11);
	add_variable_to_parblk(parblk,&vptr);
		
	/*  add $switch parm.  Value can be set via $V2SWITCH env var */

	initialize_variable(&vptr,"$switch",V_INTEGER,V_PARM,NOT_KEYWORD,1,1,0,1);
	value = getenv ("V2SWITCH");
	if (value == NULL)
		read_parm_string(&vptr, "0", 1);
	else
		read_parm_string(&vptr, value, strlen(value));
	add_variable_to_parblk(parblk,&vptr);

	/* We will need to check the pdf file to see if there are subcommands */

	add_subcmd_to_parblk(parblk,fp);

	/* add a $user_start var, which indicates the start of the user parms */
	/* This is needed to drive the positionally specified parm values in  */
	/* the command line; there are system-related variables preceding	  */
	/* the $user_start variable which we do not want to get from the args */
	
	initialize_variable(&vptr,"$user_start",V_INTEGER,V_PARM,NOT_KEYWORD,0,1,0,1);
	add_variable_to_parblk(parblk,&vptr); /* never accessesed */
	
	return SUCCESS;
}


int add_variable_to_parblk(struct PARBLK *parblk, struct VARIABLE *var)
{
	struct VARIABLE *vptr;
	int nvars,maxvars;
	/* int status;*/
	
	if (parblk == NULL)
		return FAIL;
	
	/* add new variable link*/
	
	maxvars = ((1+(P_BYTES-1)/sizeof(ALIGN)) * sizeof(ALIGN))
				/sizeof(struct VARIABLE);
				
	vptr=parblk->symtab.link;
	if (vptr == NULL)
	{
		/* first variable; initialize parblk link */
		if (maxvars > 0)
			vptr = (struct VARIABLE *)parblk->pool;
		else
		{
			vptr = (struct VARIABLE *)calloc(1,sizeof(struct VARIABLE));
			if (vptr == NULL)
				tae_abort(PSETOVER,"");

		}
		parblk->symtab.link = vptr;
	}
	else
	{
		/* link to last variable in list */
		for (nvars=1 ; (vptr->v_link !=NULL); vptr=vptr->v_link) nvars++;

		/* see if variable will fit into pool */
		if (nvars < maxvars) 
			vptr->v_link = vptr + 1;
		else 
		{
			/* need to dynamically allocate it */
			vptr->v_link = (struct VARIABLE *)calloc(1,sizeof(struct VARIABLE));
			if (vptr->v_link == NULL)
				tae_abort(PSETOVER,"");

		}
		vptr = vptr->v_link;
	}

	/* dump the values of the var struct into parblock */
	*vptr = *var;

	/* set the variable link to NULL */
	vptr->v_link = (struct VARIABLE *)NULL;
		
	return SUCCESS;
}


int put_pdf_values_in_parblk(struct PARBLK *parblk,char *cmd_line)
{
   struct VARIABLE *v;
   int vallen;
   int done,status;
   char *value;	/* points to current value */
   char *start;
   char key[20];


	/* Zeroth step: Check to see if the first arg was the subcommand */
	
	start = cmd_line;
	if ((v=shvic_p_fvar(parblk,"_SUBCMD")) != NULL)
	{
		if (!v->v_default)  /* Then the subcommand was set by first arg; skip. */
		{
			key[0] = '\0';
			find_parm(start, key, &value, &vallen);
			start = value + vallen;
		}
	}


	/* First Step: Perform a positional value search */
	
	v=shvic_p_fvar(parblk,"$user_start"); /* by-pass system parms */
	if (v == NULL)
		tae_abort(BADPAR,"$USER_START");
	v = v->v_link;					/* start at first user-defined parm  */
	key[0] = '\0';					/* this tells find_parm to return key */
	
	for (done=FALSE; !done;)
	{
		start = find_parm(start, key, &value, &vallen);
		done = (start == NULL) || (strlen(key) > 0) || (v == NULL);
		
		if (!done) /* then we found a positional value */
		{
			if (v->v_keyword)
			{
				status = set_keyword(parblk,value,vallen, v);
				if (status != SUCCESS)
				{
					*(value + vallen) = '\0';
					shvic_make_upper_case(value,value);
					tae_abort(status,value);
				}
			}
			else
				read_parm_string(v,value, vallen);
			v = v->v_link;
		}
	}

	/* Next Step: use command line to set specified parms and keywords */
	
	status=SUCCESS;
	done = (start == NULL);
	while(!done)
	{
		if (strcmp(key,"-") == 0)
		{
			status=set_keyword(parblk,value,vallen, NULL);
		}
		else if (strlen(key) > 0)
		{
			v = p_fvar_abbrev(parblk,key);
			if (v == NULL)
				tae_abort(BADPAR,key);
			if (v->v_keyword)
				status=set_keyword(parblk,value,vallen, v);
			else
				read_parm_string(v,value, vallen);
		}
		else
			tae_abort(POSERR,"");

		if (status != SUCCESS)
		{
			*(value + vallen) = '\0';
			shvic_make_upper_case(value,value);
			tae_abort(status,value);
		}

		/* Get next key or "-" keyword flag in command line */
		key[0] = '\0';
		start = find_parm(start, key, &value, &vallen);
		done = (start == NULL);
	}

	/* Final Step: check to see if any required values were omitted */
	/* and set the cur value pointer of defaulted vars to the deflt */

	for (v = shvic_p_fvar(parblk,"$user_start")->v_link; v!=NULL; v=v->v_link)
	{
	    if (v->v_default) {
		    if (v->v_dcount < v->v_minc)
			    tae_abort(MISPAR,v->v_name);
			else
			{
				v->v_cvp = v->v_dvp;
				v->v_count = v->v_dcount;
				check_values(v, TRUE); /* check dflt vs valid */
			}
	    }
	}
    return SUCCESS;
}

int set_keyword(struct PARBLK *parblk,char *key, int len, 
		struct VARIABLE* specific_v)
{
	struct VARIABLE *v,*f=0;
	struct S_VALID *sv;
	int i,found_key,found_abbrev_key,ambiguous;
	char ukey[NAMESIZ +1],*fs=0;

	shvic_make_upper_case_max(ukey,key,len);
	ukey[len] = '\0';

	if (specific_v)
		v = specific_v;
	else
		v = parblk->symtab.link;
	
	if (v!=NULL)
	{
		if (!specific_v)
			v = v->v_link;
		found_abbrev_key = ambiguous = FALSE;
		
		/* This loop looks for key, and also checks for ambiguity of key */
		while ( v!=NULL )
		{
			if (v->v_keyword)
			{
				sv = (struct S_VALID*) v->v_valid;
				for (i=0;(i< sv->count); i++)
				{
					found_key =(strncasecmp(ukey,sv->slist[i].string,len) == 0);
					if (found_key)
					{
						fs = sv->slist[i].string; 	/* save the pointers */
						f = v;
						if (strlen(fs) == len) 		/* Exact match, no need to check! */
							return read_parm_string(f,fs,len);
						else
						{
							ambiguous = found_abbrev_key; /* ambiguous abbrevs found */
							found_abbrev_key = TRUE;	/* 'Still may be an exact match later */
						}
						
					}/* End-if Found-key */
					
				}/* End-loop through Single Keyword valids */
			} /* End-if Keyword */
									
			/* We could only be here if either no key has matched yet, 	*/
			/* or an abbreviated key was found, and need to check ambig.*/
			
			if (specific_v)
				v = NULL;	/* stop searching */
			else
				v = v->v_link;

		} /* End-Loop through Symbol Table */
		
		if (found_abbrev_key && !ambiguous)	
			return read_parm_string(f,fs,strlen(fs));	/* Unique */
		else
			return KEYWORD;		
		
	} /* End-If Valid-Symbol-Table */
	
	return FAIL;
}


/* finds parameter named "key" in parblock, case insensitive 	*/
/* Unlike p_fvar, this call allows 'key' to be an abbreviation,	*/
/* but also checks through the rest of the parblk for uniqueness*/

struct VARIABLE *p_fvar_abbrev(struct PARBLK *parblk, char *key)
{
	int found_name,len;
	int found_abbrev_name,ambiguous;
	struct VARIABLE *v,*fptr = 0;
	char ukey[NAMESIZ+1];
	
	if (parblk == NULL) return  NULL;

	if (strlen(key) > NAMESIZ) return NULL;
	shvic_make_upper_case(ukey,key);
	
	len = strlen(ukey);
	
	found_abbrev_name = ambiguous = FALSE;
	
	if ((v=parblk->symtab.link) != NULL)
	{
		while (v != NULL)
		{
			found_name = (strncasecmp(v->v_name,ukey,len)==0);
			if (found_name)
			{
				fptr = v;					/* save the pointer */
				if (strlen(v->v_name)==len)
					return v;				/* Exact match, no need to check.  */
				else
				{
					ambiguous = found_abbrev_name; /* ambiguous abbrevs found */
					found_abbrev_name = TRUE;	/* 'Still may be an exact match later */
				}
			}

			/* We could only be here if either no key has matched yet, 	*/
			/* or an abbreviated key was found, and need to check ambig.*/
			
			v = v->v_link;
		} /* End While Not End-of-Symbol-Table */
		
		if (found_abbrev_name)
			if (!ambiguous)
				return fptr;				 /* Found Unique Abbrev. */
			else
				tae_abort(AMBIGPAR,ukey);	/* Not Unique */
		else
			tae_abort(BADPAR,ukey);			/* Not Found */

			
	} /* End-If valid symbol table */

	/* Alas, no variable of any kind was found */
	return  NULL;
}


/* finds parameter named "key" in parblock, case insensitive */
struct VARIABLE *shvic_p_fvar(struct PARBLK *parblk, char *key)
{
        int found_name=FALSE;
        struct VARIABLE *vptr;
        char ukey[NAMESIZ+1];

        if (parblk == NULL) return  FAIL;

        if (strlen(key) > NAMESIZ) return FAIL;
        shvic_make_upper_case(ukey,key);

        if ((vptr=parblk->symtab.link) != NULL)
        {
                while ((vptr != NULL) && (!found_name))
                        if (strcmp(vptr->v_name,ukey)==0)  found_name = TRUE;
                        else vptr = vptr->v_link;
                if (found_name)
                        return vptr;
        }

        return (struct VARIABLE *) FAIL;
}
