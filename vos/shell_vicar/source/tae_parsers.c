#include <ctype.h>
#include "tae_lib.h"
#include "zvproto.h"
#include <string.h>
#include <stdlib.h>
#include "strncasecmp.h"

/*** TEMPORARY ***/
#define MAXSTRSIZ 250
int get_parm_item(char **value, int *vallen, char **element, int valid_flag);

/* token_type returns int value indicating whether the string	*/
/* appears to begin with a NAME=VAL, a "-KEYWORD" or a VALUE	*/
/* If string appears ill-formed, FAIL is returned.		*/

int token_type(char *str, int len)
{
	char *end;

   if (strlen(str) < len)
      len = strlen(str);
   end = str + len;
   

   if (isalpha(*str) || *str=='$' || *str=='_')
   {
	   /* This is either a name or a value */
	   
	   str += strcspn(str, " \t=") - 1;
	   
	   if (str >= end)  /* must be a value */
		  return VALUE_TYPE;
	   else
	   {
		   str += 1 + strspn(str+1," \t"); /* Skip white space */
		   if ((str < end) && (*str == '='))
			return NAME_VALUE_TYPE;
		   else			/* a value with more to follow */
			return VALUE_TYPE;
	   }
   }
   else if (*str == '-')
   {
	/* This is either a keyword or a signed number value or "--" null.*/
	
	str++;
	if (isalpha(*str) || *str=='$' || *str=='_')
		return KEYWORD_TYPE;
	else return VALUE_TYPE;
   }
   else return VALUE_TYPE;

}


/* Find a sub_string of 's' of the form "key=value", "-keyword" or 	*/
/* "value" and return a pointer						*/
/* to 'value' in the parameter of that name, and the length in vallen.	*/
/* The routine will return a pointer to the string AFTER the found item	*/
/* or NULL if key is not found; If 'key' is empty, then the next parm 	*/
/* item is returned, and the key for that item is returned in 'key'.	*/
/* The key returned will be <keyname> for key=value type, "-" for 	*/
/* -kvalue type and the empty string "" for "value" type.		*/

char *find_parm
(
char *s,		/* In: String to search */
char *key,		/* In/Out: key to search for (or found if blank) */
char **value,		/* Out: Returned pointer to value */
int *vallen		/* Out: length of value */
)
{
   int keylen;
   char *sk, *ek, *sv, *ev;

   keylen = strlen(key);

   /* Look thru the string for 'key'. */

   while (parse_parm(s, strlen(s), &sk, &ek, &sv, &ev)) {
      if (keylen == ek-sk+1 || keylen==0) {		      /* same length */
         if (strncasecmp(key, sk, ek-sk+1) == 0 || keylen==0) { /* same string*/

            /* Found it! */

            *value = sv;
            *vallen = ev-sv+1;

	    if (keylen == 0) {	   /* Key blank, so return the one found */
               strncpy(key, sk, ek-sk+1);
               key[ek-sk+1] = '\0';
		shvic_make_upper_case(key,key);
	    }

            return ev+1;
         }
      }
      s = ev+1;		/* go to next key (past end of value) */
   }
   return NULL;
}


/* parse_parm will parse out the next parm item in the passed-in	*/
/* string.  It looks in a string beginning at 'cmd' of length 'len',	*/
/* assuming that the next non-white entry is the beginning of a		*/
/* parm name, keyword or value.  The parm name is followed by an '=', and */
/* then by the value associated with the name.  The keyword consists of a */
/* dash '-' character followed by the keyword value. The starting and ending */
/* positions of	the name and value are returned in 'sk','ek', and 'sv','ev'	*/
/* respectively. The key returned in 'sk' is either the name of the parm, */
/* the string "-" for keywords or the string "+" for positional values */ 
/* The label is assumed to have a valid 0 terminator, even if len is	*/
/* shorter then the length of the string.  Several places use routines	*/
/* that search to the end of the string, for efficiency.  If any of the	*/
/* pointers go beyond the given length, however, FAIL is returned.	*/

int parse_parm
(
char *cmd,		/* in: pointer to beginning of string to parse  */
int len,		/* in:  length of string to be searched		*/
char **sk,	        /* out: pointers to start and end of parm key	*/
char **ek,
char **sv,		/* out: ptrs to start and end of value field */
char **ev
)
{
   int in_quote,type;
   char *s, *end, *start_key, *end_key, *start_value;

   if (strlen(cmd) < len)
      len = strlen(cmd);
   end = cmd + len;

   *sk = *ek = *sv = *ev = end;		/* in case of error */

   start_key = cmd + strspn(cmd, " \t"); /* skip white space at beginning */

   if (start_key >= end) {
      return FAIL;			/* fail if only white space */
   }

   type = token_type(start_key,len);
   switch (type)
   {
	case NAME_VALUE_TYPE:
	   end_key = start_key + strcspn(start_key, " \t=") - 1;  /* Find end of key */
	   if (end_key >= end)			/* No terminating '=' or white space */
		  return FAIL;
	
	   start_value = end_key+1 + strspn(end_key+1," \t="); /* Skip to value field */
	   if (start_value >= end)
		  return FAIL;		/* No value field */
	   break;
	case KEYWORD_TYPE:
	   end_key = start_key;			/* key = "-" */
	   start_value = end_key+1;
	   break;
	case VALUE_TYPE:
	   end_key = start_key-1;		/* key = "" */
	   start_value = start_key;
	   break;
	default:
		return FAIL;
   }
   
/* We are now pointing at the first non-white character in the value field. */
/* Search for the end of the value.					    */

   s = start_value;
   switch (*s++) {
      case '(':				/* Multi-valued item */
         in_quote = FALSE;
         while (s < end) {
            if (*s == ')' && !in_quote)
               break;			/* end of multi-valued item */
            if (*s == '\"')
               in_quote = !in_quote;	/* found a quote, toggle the flag */
            s++;
         }
         break;
      case '\"':			/* String item */
         while (s < end) {
            if (*s == '\"') 
                  break;	
            s++;
		 }
		 break;
      default:				/* Int or Real item or a:b item-pair */
         s += strcspn(s, " \t");	/* find end (whitespace) or */
	 s--;			/* want to point TO last char, not past it */
	 break;
   }
   if (s >= end)
      return FAIL;		/* We reached the end of string prematurely */

   *sk = start_key;		/* Set the return values */
   *ek = end_key;
   *sv = start_value;
   *ev = s;

   return SUCCESS;
}


int count_values(char *value,int vallen,int valid_flag)
{
   int nelements;
   char *element;

   /* Get each element value until no more remain */
   if (!(value) || !(vallen)) return 0; /* just in case */
   nelements = 0;
   while (get_parm_item(&value, &vallen, &element, valid_flag)) (nelements)++;
   return nelements;

}


/* parse_pdf_statement parses the PARM statements for the*/
/* variable name, type, etc. 							  */

int parse_pdf_statement(char* line, char* parm,int *type,int *keyflag,
			int vcount[2],char** vvals,int *vlen,char **dvals,
			int* dlen)
{
	char *start;
	char *value;
	char key[20];
	char *stype;
	enum {p_nam,p_typ,p_cnt,p_def,p_val};
	char *pdf_key="NTCDV";
	char *pdf_value[5];
	int pdf_vallen[5];
	int vallen,count,i,done;
	TAEINT *cptr;

	memset(pdf_value,0,sizeof(pdf_value));
	memset(pdf_vallen,0,sizeof(pdf_vallen));

	/* Scan for positional PARM values first */
	key[0] = '\0';			/* this tells find_parm to return key */
	i=(int)p_nam;
	start=line+5;
	for (done=FALSE; !done;)
	{
		start = find_parm(start, key, &value, &vallen);
		done = (start == NULL) || (strlen(key) > 0);
		
		if (!done) /* then we found a positional value */
		{
			pdf_value[i]=value;
			pdf_vallen[i]=vallen;
			i++;
		}
	}

	/* Now scan remaining values, by name */
	done = (start == NULL);
	while(!done)
	{
		if (strlen(key) > 0)
		{
			/* get the index of the parm name */
			for ( i=(int)p_nam; (i<=(int)p_val) && 
				(toupper(key[0])!=pdf_key[i]);   i++);
			if (i > (int)p_val)
				tae_abort(BADPAR,key);
			pdf_value[i]=value;
			pdf_vallen[i]=vallen;
		}
		else
			tae_abort(POSERR,"");

		/* Get next key in command line */
		key[0] = '\0';
		start = find_parm(start, key, &value, &vallen);
		done = (start == NULL);
	}


	/* Process name */
	if (pdf_value[p_nam] == NULL)
		tae_abort(INVPNAME,"");
	strncpy(parm, pdf_value[p_nam],pdf_vallen[p_nam]);
	parm[pdf_vallen[p_nam]]='\0';
	
	/* Get type and set keyflag */
	*keyflag = FALSE;
	if (pdf_value[p_typ] == NULL)
		*type = V_STRING;
	else
	{
		get_parm_item(&pdf_value[p_typ], &pdf_vallen[p_typ], &stype,
								 FALSE);
		switch (toupper(*stype))
		{
			case 'I':		/* Integer 	*/
				*type = V_INTEGER;
				break;
			case 'R':		/* Real		*/
				*type = V_REAL;
				break;
			case 'K':		/* keyword 	*/
				*keyflag = TRUE;
				*type = V_STRING;
				break;
			case 'N':		/* Name -> set to string */
			case 'S':
				*type = V_STRING;
				break;
			default:
				tae_abort(TYPERR,parm);
				break;
		}
	}
	
	/* Get valid Count */
	if (pdf_value[p_cnt] == NULL)
		vcount[0] = vcount[1] = 1;
	else
	{
		/* we need to count the # values first */
		
		count = count_values(pdf_value[p_cnt],pdf_vallen[p_cnt], FALSE);
  		if (count > 0)
		{
			cptr = (TAEINT *)calloc(count,sizeof(TAEINT));
			get_parm_values((char *) cptr, 
					pdf_value[p_cnt],pdf_vallen[p_cnt], 
					V_INTEGER, sizeof(TAEINT), FALSE);
			vcount[0] = vcount[1] = *cptr;
			for (i=1;i<count;i++)
			{
				vcount[0] = MIN(vcount[0],*(cptr+i));
				vcount[1] = MAX(vcount[1],*(cptr+i));
			}
			
			free (cptr);
		}
		else
			vcount[0] = vcount[1] = 1;
	}

	/* Set the valid&default values and vallen, if any*/
	*vvals=pdf_value[p_val];
	*vlen=pdf_vallen[p_val];
	*dvals=pdf_value[p_def];
	*dlen=pdf_vallen[p_def];

	if ((*keyflag) && ((*vlen) == 0))
		tae_abort(VALIDREQ,parm);

	*keyflag = (*type == V_STRING) && (*vlen);
	
	return SUCCESS;

}



/* read all the values in string 'value' into the array vals 	*/
/* assuming string is an array of 'type' variables and the	*/
/* space allocated in 'vals' array is 'size' bytes per element.	*/
/* If valid_flag is set, then the 'vals' array is actually a	*/
/* Valid structure, which is an array of (min,max) pairs.  So	*/
/* single items are doubled, and a:b items are parsed out.	*/

int get_parm_values(char *vals, char *value, int vallen, int type, int size, 
		    int valid_flag)
{
	char *element, *colon;

	while (get_parm_item(&value, &vallen, &element, valid_flag))
	{
		switch(type)
		{
		   case V_STRING:
		      strcpy(vals, element);
		      break;
		   case V_INTEGER:
		      if (valid_flag) {
			 *(int *)vals = atoi(element);
			 vals += size;
			 if ((colon=strchr(element, ':')))
			    *(int *)vals = atoi(colon+1);
			 else
			    *(int *)vals = atoi(element);
		      }
		      else
			 *(int *)vals = atoi(element);
		      break;
		   case V_REAL:
		      if (valid_flag) {
			 *(TAEFLOAT *)vals = (TAEFLOAT)atof(element);
			 vals += size;
			 if ((colon=strchr(element, ':')))
			    *(TAEFLOAT *)vals = (TAEFLOAT)atof(colon+1);
			 else
			    *(TAEFLOAT *)vals = (TAEFLOAT)atof(element);
		      }
		      else
			 *(TAEFLOAT *)vals = (TAEFLOAT)atof( element);
		      break;
		}
		vals += size;	/* jump to next element address */
	}
	return SUCCESS;
}


/* Return the first (or only) parm element from 'value'.	*/
/* 'Value' is incremented to point at the next item, so this	*/
/* routine can be called multiple times to get all the elements	*/
/* of a command-line item.Returns TRUE if an elemnt is returned,*/
/* FALSE if there are no more elements.				*/
/* NOTE: The element returned is kept in an internal static	*/
/* buffer (*element is set to it), so it better be used or	*/
/* copied elsewhere before this routine is called again.	*/
/* This code is a modified version of get_label_item -- the	*/
/* differences being that (1) such things as 3:5 must be	*/
/* recognizedas two values, so we need to check for the ':',	*/
/* and (2) strings in command lines use double-quotes, so that	*/
/* is changed as well.  The : is *not* an end-of-value separator*/
/* for valid lists (the internal : is parsed elsewhere) so	*/
/* that's what the valid_flag is for.				*/

int get_parm_item
(
char **value,		/* in/out: pointer to this (next) element in value */
int *vallen,		/* in/out: length (remaining) in value */
char **element,		/* out: pointer to this element's value (STATIC!) */
int valid_flag		/* in: True if this is for a Valid list */
)
{
   static char temp[MAXSTRSIZ+1];
   char *p, *start, *end, colon;
   int len, found_string=FALSE;
   int done;

   temp[0] = '\0';
   *element = temp;
   colon = ':';
   if (valid_flag)
      colon = ' ';

   if (*vallen <= 0 || strlen(*value) == 0)
      return FALSE;			/* no more elements */

   p = *value;
   end = *value + *vallen;

   done = FALSE;
   while (!done) {
      switch (*p++) {
         case '\0':
            p--;		/* point at last valid char */
            done = TRUE;
            break;
         case '(':			/* skip parens */
         case ',':			/* and commas */
         case ':':			/* and colons ! */
         case ' ':			/* and spaces ! */
         case '\t':			/* and tabs ! */
            break;
         case ')':			/* close paren - end of value */
            *vallen -= (p - *value);
            *value = p;
            return FALSE;
         case '\"':		/* string item */
		    found_string = TRUE;
            start = p;
            while (p < end) {
               if (*p == '\"') 	/* end quote */
                  break;
               p++;
            }
            /*if (p == end)	 whoops, no close quote! */
            /*  p--;		 make the best of it... */
            len = MIN(p-start, MAXSTRSIZ);
            strncpy(temp, start, len);
            temp[len] = '\0';
            done = TRUE;
            break;
         default:
            start = p-1;
	    if (isalpha(*start) || (*start=='$') || (*start=='_')) colon = ' '; 
            while (p < end) {
               if (*p == ','||*p == ')'|| *p == '\0' || *p == ' ' || *p == colon)
                  break;			/* end of value */
               p++;
            }
            p--;		/* point at last valid char */
            len = MIN(p-start+1, MAXSTRSIZ);
            strncpy(temp, start, len);
            temp[len] = '\0';
            done = TRUE;
            break;
      }
   }

   p++;				/* point to first char of next element */
   *vallen -= (p - *value);
   *value = p;

   if ((strlen(temp) == 0) && (!found_string))
      return FALSE;		/* no items found */
   return TRUE;

}

