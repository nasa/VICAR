#include "tae_lib.h"
#include seek_include
#include <zvproto.h>
#include <string.h>
#include "strncasecmp.h"

/* Set the _SUBCMD value from the command line.		*/

int set_sub_command(struct PARBLK *parblk, char *cmd)
{
	struct VARIABLE *v;
	int vallen,status;
	char *value,key[12];
	
	v = p_fvar(parblk,"_SUBCMD");
	
	key[0]='\0';
	find_parm(cmd,key,&value,&vallen);
	
	if (key[0]=='-')		/* This must be a subcommand */
	{
			status=set_keyword(parblk,value,vallen,NULL);
			if (status != SUCCESS)
			{
				*(value + vallen) = '\0';
				shvic_make_upper_case(value,value);
				tae_abort(UNDEFSUB,value);
			}
	}
	else					/* subcommand is defaulted */
	{
		if (v->v_dvp == NULL)
			return SUBREQ;			 /* Subcommand can't be defaulted */
		else
		{
			v->v_count = v->v_dcount;/* Set SUBCMD to default */
			v->v_cvp = v->v_dvp;
		}
	}

	return SUCCESS;
}

int add_subcmd_vars_to_parblk(struct PARBLK *parblk,FILE *fp)
{
	int done,found_sub,vallen,count,len;
	char *line, key[8],*value,*start;
	char sub_cmd[16];
	
	/* Get the chosen subcommand */
	zvp("_SUBCMD",sub_cmd,&count);
	len = strlen(sub_cmd);
	
	/* make sure the first PDF line is "PROCESS"-something */
	
	done = (fgetstr(&line, fp) <0);
	if (*line != 'P' && *line != 'p')
		tae_abort(MISINTRO,"");
		
	/* add PARMS which are global to all SUBCMD'S */

	do 
	{
		done = (fgetstr(&line, fp) <0); 
		done = done || (strncasecmp(line,"END-PROC",8)==0) ||
					(strncasecmp(line,"SUBCMD",6) == 0);

		if  ((! done) && (strncasecmp(line,"PARM",4)==0))
			add_one_pdf_variable_to_parblk(parblk,line);
	} while (! done);

	/* Now look for the correct subcommand */
	
	for ((done =FALSE,found_sub = FALSE); !found_sub && !done;)
	{
		if (strncasecmp(line,"SUBCMD",6)==0)
		{
			key[0]='\0';
			start = find_parm(line+6,key,&value,&vallen);
			switch(key[0])
			{
				case '\0': /* Then this is a subcmd NAME */
					found_sub = (strncasecmp(sub_cmd,value,len)==0);
					break;
				case '-':  /* SUBCMD-DEFAULT */
				{
					key[0] = '\0';
					start = find_parm(start,key,&value,&vallen);
					found_sub = (strncasecmp(sub_cmd,value,len)==0);
					break;
				}
			}

		} /* End-If SUBCMD line found */
		
		done = (fgetstr(&line,fp) < 0);
		done = done || (strncasecmp(line,"END-PROC",8)==0);
		
	}/* End of Search for subcommand */

	if (found_sub)	/* found the subcommand ! */
	{
		/* add PARMS which are local to the subcommand */
		for (done=FALSE; !done;) 
		{
			if  (strncasecmp(line,"PARM",4)==0)
				add_one_pdf_variable_to_parblk(parblk,line);
				
			done = (fgetstr(&line, fp) <0); 
			done = done || (strncasecmp(line,"END-",4)==0);
	
		}
	}
	
	return SUCCESS;
}


/* Create the _SUBCMD parm by forming a PARM statement for it */

int add_subcmd_to_parblk(struct PARBLK *parblk, FILE *fp)
{
	long cur_pos;
	int done,vallen,nvalids;
	char *line, key[8],*value,*start;
	char parm[200],deflt[16],valid[16];
	
	strcpy(parm,"PARM _SUBCMD TYPE=KEYWORD VALID=(");
	
	/* save current position and go to start of file */
	cur_pos = fseek( fp, 0L, SEEK_CUR);
	fseek(fp,0L, SEEK_SET);
	
	done = (fgetstr(&line,fp) < 0);
	if (done)
		tae_abort(MISINTRO,"");

	done = (fgetstr(&line,fp) < 0);
	deflt[0] = '\0';
	nvalids = 0;
	
	/* Now Scan the file for SUBCMD statements */
	
	while (!done)
	{
		if (strncasecmp(line,"SUBCMD",6)==0)
		{
			key[0]='\0';
			start = find_parm(line+6,key,&value,&vallen);
			switch(key[0])
			{
				case '-':	/* probably default subcommand */
				
					if (strncasecmp(value,"DEFAULT",vallen)==0)
					{
						key[0]='\0';
						start = find_parm(start,key,&value,&vallen);
						strncpy(deflt,value,vallen);
						deflt[vallen] = '\0';
					}
					/* fall through to: */
					
				case '\0':	/* a VALID value for subcommand */
					nvalids++;
					strncpy(valid,value,vallen);
					valid[vallen] = '\0';
					strcat(parm,valid);	/* add valid to list */
					strcat(parm,",");	/* remember to delete last , */
					break;
			}
			
		} /* End-If SUBCMD line found */
		
		done = (fgetstr(&line,fp) < 0);
		done = done || (strncasecmp(line,"END-PROC",8)==0);
		
	}/* End of Loop through file */

	if (nvalids > 0)	/* found some subcommands ! */
	{
		/* Replace that last pesky comma with a ')' */
		parm[strlen(parm)-1] = ')';
		
		/* if a default was found, tack it on to the end */
		if (strlen(deflt) > 0)
		{
			strcat(parm," DEFAULT=");
			strcat(parm,deflt);
		}
		
		/* Now Convert the parm string into a _SUBCMD variable */
		
		add_one_pdf_variable_to_parblk(parblk,parm);
	}

	/* restore position of file */
	fseek(fp,cur_pos, SEEK_SET);
	
	return SUCCESS;
}

