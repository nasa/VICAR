/*								*/
/*	LABEL							*/
/*								*/
/*	VICAR label processing program				*/
/*								*/
/*	For help on operation, see help in PDF file		*/
/*								*/
/*	Initial release:  March 1, 1984				*/
/*			  Dan Stanfill (DFS)			*/
/*								*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "vicmain_c"
#include "defines.h"
#include "zifmessage.h"
#include "zmabend.h"

#define TRUE 1
#define FALSE 0

#define REPLACE 0
#define ADD 1

#define NOCOPY 0
#define COPY 1

#define MAXSTRINGSIZE	4000

#define WIDTH	80		/* width of the printout on the screen */

#define DEL_COUNT	10	/* Max count of parameters to -delete */

#if MAX_TASKS > MAX_PROPS	/* Get maximum label set size */
#define MAX_SETS MAX_TASKS
#else
#define MAX_SETS MAX_PROPS
#endif

/* Error handling macros */

#define return_on_error(A)   zvsignal(A,status,0); if (status <= 0) zabend()
#define continue_on_error(A) zvsignal(A,status,0); if (status <= 0) continue
#define abort_on_error(A)    zvsignal(A,status,1)
#define break_on_error(A)    zvsignal(A,status,0); if (status <= 0) break

/* Globally used variables */
char msg[MAXSTRINGSIZE];	/* Message buffer for printing */
int status;			/* Return status indicator */

/* Buffer for print_key_value_pairs() so it can pack items */
char printbuf[MAXSTRINGSIZE+1];

/* Boolean value of PACK keyword */
int pack_listed_items;

/* Multivalued label item structure */
struct multival
{
  int nelements;
  int maxlength;
  char *data;		/* ptr to array[nelements][maxlength] */
  int allocsize;
};

/* Prototypes */

void add_items(int action);
void concat_labels(void);
void add_system_label(void);
void delete_items(void);
void edit_label(void);
void list_label(void);
void remove_label(void);
void switch_labels(void);
int count_sig_digits (char *str);
void concat_set(int source_unit, int out_unit, char *type, char *set_opt, char *set_name, int instance);
int string_on_list(char *string, char list[][MAX_LABEL_KEY_SIZE+1], int list_len);
int int_on_list(int intval, int list[], int list_len);
void revise_instance_list(char names[MAX_SETS][MAX_LABEL_KEY_SIZE+1],
			  int subset, int new_list[], int list_len);
void dump_all_items(int unit);
void list_system_info(int unit);
void list_property_items(int unit);
void list_history_items(int unit,int nousrtim);
void list_task_headers(int unit,int nousrtim);
void print_key_value_pair(char key[], struct multival *value, char *format);
void flush_key_value_pair(void);

void my_abort3(char *str, char *p1, char *p2);
void my_abort2(char *str, char *p1);

void main44(void)
{   
  int count,def;
  char command[7];
    
  zifmessage("LABEL version 2017-03-30");

  zvparm("_SUBCMD",command,&count,&def,0,0);
  switch (command[0])
    {   case 'A' : add_items(ADD);
	break;
    case 'C' : if (command[1] == 'O')
	  concat_labels();
	else
	  add_system_label();
      break;
    case 'D' : delete_items();
      break;
    case 'E' : edit_label();
      break;
    case 'L' : list_label();
      break;
    case 'R' : if (command[2] == 'P')
	add_items(REPLACE);
      else
	remove_label();
      break;
    case 'S' : switch_labels();
    }
}

void add_items(int action)
{
  /*	Local variables:					*/
  /*								*/
  /*  in_unit	-- Input file unit number			*/
  /*  out_unit	-- Output file unit number			*/
  /*  count	-- Count of parameter from zvparm		*/
  /*  instance	-- instance of TASK or PROPERTY			*/
  /*  key		-- name of key to be inserted			*/
  /*  value	-- multival struct describing item to be added	*/
  /*  form	-- INT, REAL, DOUB, or STRING.  The format of	*/
  /*		   VALUE.					*/
  /*  set		-- the name of the task or property subset in	*/
  /*		    which the item is to be inserted		*/
  /*  mode	-- The mode to use when calling zladd		*/
  /*  data_copy   -- Flag indicating whether or not to copy data  */
  /*  		   to an output file.				*/
  /*  p		-- Pointer to the current char in string being	*/
  /*		   parse.					*/
  /*  element_number -- Starting elem given in () after key name	*/
  /*								*/

  int in_unit,out_unit,count,instance;
  int data_copy;
  char form[12],set[33],message[80],type[12];
  char key[33], mode[12], set_opt[12];
  char *p;
  int len;
  int maxlen, nelements;		/* from original label item */
  int element_number, element;
  int change_format;		/* TRUE iff we can change the format */
  int nlb, nbb;		/* BINARY header and prefix lengths */
  struct multival value;
  char *parse_value();
  int task_cnt, prop_cnt;
  int update;

  /* Check for update mode */
  update = zvptst("UPDATE");

  /* Find out whether data is to be copied */
  zvpcnt("OUT",&count);
  data_copy = (count == 1) ? TRUE : FALSE;

  /* Get the file unit numbers */
  if (data_copy) 
    {
      status = zvunit(&in_unit,"INP",1, NULL);
      abort_on_error(in_unit);

      status = zvunit(&out_unit,"OUT",1, NULL);
      abort_on_error(out_unit);

      status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
      status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
      if (status != 1)
	nlb = nbb = 0;
      status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
		      "U_NLB",nlb,"U_NBB",nbb, NULL);
    }
  else	/* open for update */
    {
      status = zvunit(&out_unit,"INP",1, NULL);
      abort_on_error(out_unit);

      status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }
  /* Get list of items to be added */
  zvp("ITEMS",msg,&count);

  /* Get type of labels to deal with (HISTORY, PROPERTY, or SYSTEM) */

  zvpcnt("TASK", &task_cnt);
  zvpcnt("PROPERTY", &prop_cnt);

  if (task_cnt != 0 && prop_cnt != 0)
    zmabend("Cannot specify both TASK and PROPERTY");

  zvp("TYPE",type,&count);
  if (count == 0)			/* No type specified */
    {
      if (prop_cnt != 0)
	strcpy(type,"PROPERTY");
      else				/* TASK given or default case */
	strcpy(type,"HISTORY");
    }
  else				/* Type given, check for consistency */
    {
      if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	zmabend("Cannot use PROPERTY parameter if TYPE=HISTORY");
      if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	zmabend("Cannot use TASK parameter if TYPE=PROPERTY");
      if (strcmp(type,"PROPERTY") == 0 && prop_cnt == 0)
	zmabend("Must specify PROPERTY parameter if TYPE=PROPERTY");
      if (strcmp(type,"SYSTEM") == 0 && (prop_cnt != 0 || task_cnt != 0))
	zmabend("Cannot use TASK or PROPERTY parameters if TYPE=SYSTEM");
    }

  if (strcmp(type,"PROPERTY") == 0)
    strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
  else
    strcpy(set_opt,"HIST");

  /* Initialize to beginning of buffer */
  p = msg;

  /* Loop through items, adding each in turn */
  while (*p != '\0')
    {
      form[0] = '\0';
      change_format = TRUE;
      nelements = 0;

      /* Skip over white space */
      p += strspn(p, " \t");
      if (*p == '\0')
	break;

      /* Find key word */
      len = strcspn(p, "=( \t");
      strncpy(key, p, len);
      key[len] = '\0';
      p += len;

      p += strspn(p, " \t");		/* skip white space */

      if (*p == '(')			/* element number */
	{
	  p++;
	  sscanf(p, "%d", &element_number);
	  if (element_number == 0 || element_number < -1)
	    my_abort3("Illegal starting element number of %d for keyword %s",
		      (void *)element_number, key);
	  p += strspn(p, "1234567890+");	/* find closing parenthesis */
	  p += strspn(p, " \t");		/* skip white space */
	  if (*p != ')')
	    my_abort2("Missing ')' for starting element of keyword %s",
		      key);
	  p++;			/* skip the ) */
	  p += strspn(p, " \t");		/* skip white space */

	  /* Get the type of the currently existing label so we can match it */

	  zvp("INSTANCE",&instance,&count);
	  if (strcmp(type,"HISTORY")==0)
	    zvp("TASK",set,&count);
	  else if (strcmp(type,"PROPERTY")==0)
	    zvp("PROPERTY",set,&count);
	  else				/* SYSTEM */
	    count = 0;
	  if (count == 0)
	    status = zlinfo(out_unit, type, key, form, &maxlen,
			    &nelements, NULL);
	  else
	    status = zlinfo(out_unit, type, key, form, &maxlen,
			    &nelements, set_opt, set, "INSTANCE", instance, NULL);
	  change_format = FALSE;
	  if (status == CANNOT_FIND_KEY)
	    {
	      form[0] = '\0';
	      nelements = 0;
	      change_format = TRUE;
	    }
	  else
	    abort_on_error(out_unit);

	}
      else
	element_number = 0;	     /* Element number subscript not present */

      if (*p != '=')
	my_abort2("Missing '=' for keyword %s", key);
      p++;

      p += strspn(p, " \t");		/* skip white space after the = */

      if (*p == '\0')
	my_abort2("Null value for keyword %s", key);

      /* We're now at the beginning of value part of buffer */
      value.data = NULL;
      value.nelements = 0;
      value.maxlength = 0;

      /* First time through parse_value.... just get maxlen, nelem, and format */

      parse_value(&value, p, form, key, change_format);

      if (strcmp(form, "INT") == 0)
	value.maxlength = sizeof(int);
      if (strcmp(form, "REAL") == 0)
	value.maxlength = sizeof(float);
      if (strcmp(form, "DOUB") == 0)
	value.maxlength = sizeof(double);

      value.data = malloc(value.nelements * value.maxlength);
      if (value.data == NULL)
	my_abort2("Out of memory processing keyword %s!!", key);

      /* Second time... fill the array.  All errors should have been caught the */
      /* first time through, so there is no need to free up value.data.	  */

      p = parse_value(&value, p, form, key, FALSE);

      /* If TASK was not given and type is HISTORY, then default it */
      /* If SYSTEM label, do that here too (since no TASK/PROP sets to worry about) */

      element = element_number;

      if (strcmp(type,"SYSTEM") == 0 ||
	  (strcmp(type,"HISTORY") == 0 && task_cnt == 0))
	{
	  if (action == REPLACE)
	    {
	      if (element_number == 0)		/* not given */
		{
		  status = zldel(out_unit,type,key, NULL);
		  if (!update) {		/* ignore errors in update */
		    if (status <= 0)
		      free(value.data);
		    abort_on_error(out_unit);
		  }
		  element = 1;
		}
	      strcpy(mode, "REPLACE");
	    }
	  else if (element_number == 0) {		/* not given */
	    if (update) {
	      status = zldel(out_unit,type,key,NULL);  /* ignore errors */
	      element = 1;
	      strcpy(mode, "REPLACE");
	    }
	    else
	      strcpy(mode, "ADD");
	  }
	  else
	    strcpy(mode, "INSERT");

	  status = zladd(out_unit,type,key,value.data,"FORMAT",form,
			 "ELEMENT", element,
			 "NELEMENT", value.nelements, "ULEN", value.maxlength,
			 "MODE", mode, NULL);
	}
      else
	{
	  /* PROPERTY or HISTORY with specified set name */

	  zvp("INSTANCE",&instance,&count);
	  if (strcmp(type,"PROPERTY")==0)
	    zvp("PROPERTY",set,&count);
	  else				/* HISTORY */
	    zvp("TASK",set,&count);

	  if (action == REPLACE)
	    {
	      if (element_number == 0)		/* not given */
		{
		  status = zldel(out_unit,type,key,set_opt,set,
				 "INSTANCE",instance, NULL);
		  if (!update) {		/* ignore errors in update */
		    if (status <= 0)
		      free(value.data);
		    abort_on_error(out_unit);
		  }
		  element = 1;
		}
	      strcpy(mode, "REPLACE");
	    }
	  else if (element_number == 0) {		/* not given */
	    if (update) {
	      status = zldel(out_unit,type,key,set_opt,set,
			     "INSTANCE",instance, NULL);	/* ignore errors */
	      element = 1;
	      strcpy(mode, "REPLACE");
	    }
	    else
	      strcpy(mode, "ADD");
	  }
	  else
	    strcpy(mode, "INSERT");

	  status = zladd(out_unit,type,key,value.data,"FORMAT",form,
			 set_opt, set, "INSTANCE", instance,
			 "ELEMENT", element,
			 "NELEMENT", value.nelements, "ULEN", value.maxlength,
			 "MODE", mode, NULL);
	}
      free(value.data);

      abort_on_error(out_unit);

      if (element_number == -1 || element_number > nelements)
	element_number = nelements+1;
      if (strcmp(mode, "ADD") == 0)
	sprintf(message, "Keyword %s added", key);
      else if (strcmp(mode, "REPLACE") == 0)
	{
	  if (element_number == 0)
	    sprintf(message, "Keyword %s replaced", key);
	  else
	    if (value.nelements == 1)
	      sprintf(message, "Element %d replaced in keyword %s",
		      element_number, key);
	    else
	      sprintf(message,
		      "Elements %d through %d replaced in keyword %s",
		      element_number, element_number+value.nelements-1, key);
	}
      else
	{
	  if (value.nelements == 1)
	    sprintf(message, "Element %d added to keyword %s",
		    element_number, key);
	  else
	    sprintf(message, "Elements %d through %d added to keyword %s",
		    element_number, element_number+value.nelements-1, key);
	}
      zifmessage(message);

      p += strspn(p, " \t");
      if (*p == ',')	     /* skip over any (optional) commas between items */
	{
	  p++;
	  p += strspn(p, " \t");
	}
    }	/* End of while loop */


  /* Copy the data over if requested */
  if (data_copy)
    {
      char *buf = NULL;
      int rec_size;
      status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
      abort_on_error(in_unit);
      buf = (char *)malloc(rec_size);
      if (buf == NULL) {
	zmabend("Out of memory!!!");
      }

      while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	  if (status == END_OF_FILE) break;
	  abort_on_error(in_unit);
	  status = zvwrit(out_unit,buf, NULL);
	  abort_on_error(out_unit);
	}
      free(buf);

      /* Close the files and exit */
      status = zvclose(in_unit, NULL);
    }

  status = zvclose(out_unit, NULL);

}

char *parse_value(value, p, format, key, change_format)

/* Parse out the value string pointed at by *p.  If value.data is NULL,	*/
/* then the string is only scanned, with format, value.nelements, and	*/
/* value.maxlength being updated.  If value.data is not NULL, then the	*/
/* values are actually stored in value.data, and value.maxlength must	*/
/* already be set.  If change_format == TRUE, then we can change the fmt*/
/* based on user input.  If it's FALSE, the label already exists, so	*/
/* don't change the format.						*/

     struct multival *value;
     char *p;
     char *format;
     char *key;
     int change_format;
{
  int paren, done, quote;
  int len, nelem, sig_digits;
  char savechar;
  char *start;

  paren = FALSE;
  done = FALSE;
  nelem = 0;

  if (*p == '(')			/* start of multival entry */
    {
      paren = TRUE;
      p++;
      p += strspn(p, " \t");		/* skip white space */
    }

  while (!done)
    {
      /* This item is a string if the rest of them are strings, if it	*/
      /* starts with a quote, or if the first char is not a number	*/

      if (strcmp(format, "STRING") == 0 ||
	  *p == '\'' || strchr("1234567890+-.Ee", *p) == 0)	   /* string */
	{
	  quote = FALSE;
	  if (*p == '\'')
	    {
	      p++;
	      quote = TRUE;
	    }

	  if (*format != '\0' && strcmp(format, "STRING") != 0 &&
	      !change_format)
	    my_abort3("Can't change format from %s to STRING for keyword %s",
		      format, key);
	  strcpy(format, "STRING");		/* change format to STRING */

	  start = p;

	  if (quote)
	    len = strcspn(p, "\'");	/* find end of string */
	  else
	    len = strcspn(p, ",)\' \t");
	  p += len;
	  if ((*p != '\'' && quote) || (*p == '\'' && !quote))
	    my_abort3(
		      "String for keyword %s, element %d improperly terminated",
		      key, (void *)(nelem+1));
	  if (!quote && len == 0)
	    my_abort3("Invalid value for keyword %s, element %d",
		      key, (void *)(nelem+1));

	  if (value->data != NULL)
	    {
	      strncpy(value->data + (nelem * value->maxlength), start, len);
	      *(value->data + (nelem * value->maxlength) + len) = '\0';
	    }
	  else
	    {
	      value->maxlength = MAX(value->maxlength, len+1); /*+1 for term*/
	      value->nelements = nelem+1;
	    }
	  if (*p == '\'')
	    p++;			/* skip closing quote */
	}
      else						/* INT, REAL or DOUB */
	{
	  start = p;
	  len = strspn(p, "1234567890+-.Ee");		/* find end of number */
	  if (len == 0)
	    my_abort3("Invalid value for keyword %s, element %d",
		      key, (void *)(nelem+1));
	  p += len;
	  savechar = *p;
	  *p = '\0';				/* make the number a string */

	  /* It's a real or a double if the rest of them are reals	*/
	  /* or doubles, or it has a decimal point or an 'E' or 'e'	*/ 
	  /* in it.							*/

	  if (strcmp(format, "REAL") == 0 || strcmp(format, "DOUB") == 0 ||
	      strchr(start, '.') != 0 || strchr(start, 'E') != 0 || 
	      strchr(start, 'e') != 0)
	    {						/* REAL or DOUB */
	      sig_digits = count_sig_digits (start);

	      /* If we've already found DOUB's, don't go back to REAL */
	      /* despite what sig_digits might say.			*/
	      if (sig_digits <= 6 && strcmp(format, "DOUB") != 0)
		{
		  /* Can't use REAL format if we're currently at STRING or*/
		  /* or we're not REAL and we can't change it.	    */

		  if ((*format != '\0' && strcmp(format, "REAL") != 0 &&
		       !change_format)  ||  (strcmp(format, "STRING") == 0))
		    my_abort3(
			      "Can't change format from %s to REAL for keyword %s",
			      format, key);
		  strcpy(format, "REAL");
		}
	      else
		{
		  /* Can't use DOUB format if we're currently at STRING or*/
		  /* we're not DOUB and we can't change it.  		    */

		  if ((*format != '\0' && strcmp(format, "DOUB") != 0 &&
		       !change_format)  ||  (strcmp(format, "STRING") == 0))
		    my_abort3(
			      "Can't change format from %s to DOUB for keyword %s",
			      format, key);
		  strcpy(format, "DOUB");
		}

	      if (value->data != NULL && *format == 'R')
		sscanf(start, "%f", value->data+(nelem*value->maxlength));
	      else if (value->data != NULL && *format == 'D')
		sscanf(start, "%lf", value->data+(nelem*value->maxlength));
	      else
		{	/* not maxlen of 4 in case we change to string later */
		  value->maxlength = MAX(value->maxlength, len+1);
		  value->nelements = nelem+1;
		}
	    }
	  else
	    {							/* INT */
	      /* Can't change format to INT from anything else */

	      if (*format != '\0' && strcmp(format, "INT") != 0)
		my_abort3(
			  "Can't change format from %s to INT for keyword %s",
			  format, key);
	      strcpy(format, "INT");

	      if (value->data != NULL)
		sscanf(start, "%d", value->data+(nelem*value->maxlength));
	      else
		{	/* not maxlen of 4 in case we change to string later */
		  value->maxlength = MAX(value->maxlength, len+1);
		  value->nelements = nelem+1;
		}
	    }
	  *p = savechar;
	}

      /* Done with one element, now check for others */

      nelem++;

      p += strspn(p, " \t");		/* skip white space */

      if (paren)			/* look for multi-values */
	{
	  if (*p == ')')
	    {
	      done = TRUE;
	      p++;
	    }
	  else if (*p == ',')		/* skip commas */
	    *p++;
	  else if (*p == '\0')
	    my_abort2("No closing parenthesis for multi-valued keyword %s",
		      key);
	}
      else
	done = TRUE;

      p += strspn(p, " \t");		/* skip white space */
    }

  if (!paren && *p == ')')
    my_abort2("Extra closing parenthesis found for keyword %s", key);

  return p;
}

void my_abort3(char *str, char *p1, char *p2)
{
  char message[80];

  sprintf(message, str, p1, p2);
  zmabend(message);
}

void my_abort2(char *str, char *p1)
{
  char message[80];

  sprintf(message, str, p1);
  zmabend(message);
}

int count_sig_digits (char *str)
{
  char *fsd;
  int len;

  fsd = strpbrk (str, "123456789");
  if (fsd == NULL)		/* Only 0 and . are present */
    return 1;
  len = strspn (fsd, "0123456789.");

  if (len != strspn (fsd, "0123456789"))
    return len - 1;
  else return len;
}

int isdouble(str)

/* Determines if string has more than single precision detail */

     char *str;
{
  double n1, n2;
  float nf;

  sscanf (str, "%lf", &n1);
  nf = (float) n1;
  n2 = (double) nf;

  return (n2 == n1);
}

void add_system_label(void)
{
  /* add_system_label 						*/
  /*								*/
  /* 	Take an unlabeled file and copy with a label		*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /*  in_unit	-- Input file unit number			*/
  /*  out_unit	-- Output file unit number			*/
  /*  count	-- Count of parameter from zvparm		*/
  /*  defaulted	-- Obsolete flag from zvparm (unused)		*/
  /*  comment	-- User comment label to add (always STRING)	*/
  /*  nl		-- Number of lines in image			*/
  /*  ns		-- Number of samples per line			*/
  /*  format	-- format of image (see pdf)			*/
  /*  nbb         -- Number of binary bytes to precede each line  */
  /*  nlb         -- Number of binary header lines                */
  /*  nrecs       -- Number of records or total lines in image    */
  /*  line	-- line number counter				*/
  /*  host	-- Host type name for input file		*/
  /*  intfmt,realfmt -- Integer and real formats for input file	*/
  /*  bhost,bintfmt,brealfmt -- Binary label versions		*/
  /*  bltype	-- Type of binary label				*/
  /*  recsize	-- Size of a record, needed for binary labels	*/
  /*  pixsize	-- Size of a pixel, needed for binary labels	*/

  int in_unit,out_unit,count,defaulted,nl,ns,nb,nbb,nlb,nrecs;
  int recsize, pixsize;
  int line,samp,band;
  char comment[81],format[8],org[8];
  char host[33], intfmt[33], realfmt[33];
  char bhost[33], bintfmt[33], brealfmt[33], bltype[33];

  struct		/* Window into the input		*/
  {
    int sl;		/* Starting line of input		*/
    int ss;		/* Starting sample of input		*/
    int nl;		/* Number of lines			*/
    int ns;		/* Number of samples			*/
  } window;

  struct		/* Window into the input		*/
  {
    int sb;		/* Starting band of input		*/
    int nb;		/* Number of bands			*/
  } bands;

  /* Get unit numbers */
  status = zvunit(&in_unit,"INP",1, NULL);
  abort_on_error(in_unit);
  status = zvunit(&out_unit,"OUT",1, NULL);
  abort_on_error(out_unit);

  /* Get file type info. */
  zvp("ORG",org,&count);

  zvp("FORMAT",format,&count);

  /* Get the host and data formats */
  zvp("HOST", host, &count);
  zveaction("sa","");			/* Abort if invalid host */
  zvhost(host, intfmt, realfmt);
  zveaction("", "");

  zvpcnt("INTFMT",&count);
  if (count != 0)
    zvp("INTFMT", intfmt, &count);		/* override zvhost setting */
  zvpcnt("REALFMT",&count);
  if (count != 0)
    zvp("REALFMT", realfmt, &count);	/* override zvhost setting */

  if (zvptst("BINARY"))
    {

      /* Get file size info and open accordingly.  Note that we must treat	*/
      /* the input in terms of bytes in case there's a weird binary prefix.	*/
      zvp("NBB",&nbb,&count);
      if (nbb) zvadd(in_unit, "U_NBB",nbb, NULL);

      zvp("NLB",&nlb,&count);
      if (nlb) zvadd(in_unit, "U_NLB",nlb, NULL);

      zvp("NL",&nl,&count);
      if (count != 0) status = zvadd(in_unit,"U_NL",nl, NULL);

      zvp("NS",&ns,&count);
      if (count != 0)
	{
	  if (strcmp(org,"BIP") == 0)		/* No change for BIP */
	    zvadd(in_unit, "U_NS",ns, NULL);
	  else
	    {
	      zvpixsize(&pixsize, format, intfmt, realfmt);
	      recsize = ns * pixsize + nbb;
	      zvadd(in_unit, "U_NS",recsize, NULL);
	    }
	}

      zvp("NB",&nb,&count);
      if (strcmp(org,"BIP") != 0)		/* No change unless BIP */
	zvadd(in_unit, "U_NB",nb, NULL);
      else
	{
	  zvpixsize(&pixsize, format, intfmt, realfmt);
	  recsize = nb * pixsize + nbb;
	  zvadd(in_unit, "U_NB",recsize, NULL);
	}

      status = zvopen(in_unit,"COND","NOLABELS","I_FORMAT","BYTE",
		      "U_FORMAT","BYTE","U_ORG",org,"OPEN_ACT","SA","IO_ACT","SA",
		      "CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
		      "REALFMT",realfmt, NULL);

      if (strncmp(org, "BIP", 3) == 0)
	nrecs = nl * ns + nlb;
      else
	nrecs = nl * nb + nlb;

      /* Get binary host and data formats */
      zvpcnt("BHOST",&count);
      if (count != 0)
	zvp("BHOST", bhost, &count);
      else
	strcpy(bhost, host);
      zveaction("sa","");			/* Abort if invalid host */
      zvhost(bhost, bintfmt, brealfmt);
      zveaction("", "");

      zvpcnt("BINTFMT",&count);
      if (count != 0)
	zvp("BINTFMT", bintfmt, &count);	/* override zvhost setting */
      zvpcnt("BREALFMT",&count);
      if (count != 0)
	zvp("BREALFMT", brealfmt, &count);	/* override zvhost setting */

      zvp("BLTYPE", bltype, &count);

      /* open the output file */
      zvopen(out_unit, "OP","WRITE", "OPEN_ACT","SA", "IO_ACT","SA",
	     "COND","BINARY", "U_NLB",nlb, "U_NBB",nbb,
	     "U_ORG",org, "U_NL",nl, "U_NS",ns, "U_NB",nb,
	     "CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
	     "REALFMT",realfmt, "BHOST",bhost, "BINTFMT",bintfmt,
	     "BREALFMT",brealfmt, "BLTYPE",bltype,
	     "U_FORMAT",format, "O_FORMAT",format, NULL);

      {
	char *buf = NULL;
	int rec_size;
	status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
	abort_on_error(in_unit);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	  zmabend("Out of memory!!!");
	}

	for (line=0; line<nrecs; line++)
	  {
	    zvread(in_unit, buf, NULL);
	    zvwrit(out_unit, buf, NULL);
	  }
	free(buf);
      }
    }
  else		/* Not binary */
    {
      zvp("NL",&nl,&count);
      if (count != 0)
	zvadd(in_unit, "U_NL",nl, NULL);

      zvp("NS",&ns,&count);
      if (count != 0)
	zvadd(in_unit, "U_NS",ns, NULL);

      zvp("NB",&nb,&count);
      zvadd(in_unit, "U_NB",nb, NULL);

      /* Open the input file	*/
      status = zvopen(in_unit,"COND","NOLABELS","I_FORMAT",format,
		      "U_FORMAT",format,"U_ORG",org,"OPEN_ACT","SA","IO_ACT","SA",
		      "CONVERT","OFF", "HOST",host, "INTFMT",intfmt,
		      "REALFMT",realfmt, NULL);

      zvparm("WINDOW",&window,&count,&defaulted,0,0);
      if (count==0)
	{
	  window.sl = window.ss = 1;
	  status = zvget(in_unit,"NL",&window.nl,"NS",&window.ns, NULL);
	}

      zvparm("BANDS",&bands,&count,&defaulted,0,0);
      if (count==0)
	{
	  bands.sb =  1;
	  bands.nb =  nb;
	}

      /* open the output file */
      status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",
		      "U_ORG",org,"U_NL",window.nl,"U_NS",window.ns,
		      "U_NB",bands.nb, "CONVERT","OFF", "HOST",host,
		      "INTFMT",intfmt, "REALFMT",realfmt,
		      "U_FORMAT",format, "O_FORMAT",format, NULL);

      /* Now copy over the data */

      {
	char *buf = NULL;
	int rec_size;
	status = zvget(out_unit, "RECSIZE", &rec_size, NULL);
	abort_on_error(out_unit);
	buf = (char *)malloc(rec_size);
	if (buf == NULL) {
	  zmabend("Out of memory!!!");
	}

	if ( strncmp( org, "BSQ",3 ) == 0 )
	  for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
	    for (line=window.sl; line<= (window.nl+window.sl-1); line++)
	      {
		status = zvread(in_unit,buf,"SAMP",window.ss,
				"LINE",line, "BAND",band,"NSAMPS",window.ns, NULL);
		status = zvwrit(out_unit,buf, NULL);
	      }
	else if ( strncmp( org, "BIL",3 ) == 0 )
	  for (line=window.sl; line <= (window.nl + window.sl-1); line++)
	    for (band=bands.sb; band <= (bands.nb + bands.sb-1); band++)
	      {
		status =zvread(in_unit,buf,"SAMP",window.ss,"LINE",line,
			       "BAND",band,"NSAMPS",window.ns, NULL);
		status = zvwrit(out_unit,buf, NULL);
	      }
	else				/* BIP */
	  for (line = window.sl; line <= (window.nl+window.sl-1); line++)
	    for (samp=window.ss; samp<=(window.ns+window.ss-1); samp++)
	      {
		status = zvread(in_unit,buf,"SAMP",samp,"LINE",line,
				"BAND",bands.sb,"NBANDS",bands.nb, NULL);
		status = zvwrit(out_unit,buf, NULL);
	      }
	free(buf);
      }
    }

  /* See if comment was given and if so add it in */
  zvparm("COMMENT",comment,&count,&defaulted,0,0);
  if (count == 1)
    {
      status = zladd(out_unit,"HISTORY","COMMENT",comment,
		     "FORMAT","STRING", NULL);
      abort_on_error(out_unit);
    }

  /* Close files and exit */
  status = zvclose(in_unit, NULL);
  status = zvclose(out_unit, NULL);

}

void concat_labels(void)
{
  /*	concat_labels						*/
  /*								*/
  /*	Concatenates (merges) all history or property labels,	*/
  /*	or a single label set, from input 1 into input 2.	*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /*  source_unit	-- Unit number for label source (inp #1)	*/
  /*  in_unit	-- Input file unit number (inp #2)		*/
  /*  out_unit	-- Output file unit number			*/
  /*  task_cnt	-- Flags if TASK parameter given		*/
  /*  prop_cnt	-- Flags if PROPERTY parameter given		*/
  /*  count	-- Count of parameter from zvparm		*/
  /*  data_copy   -- Flag indicating whether or not to copy data  */
  /*  		   to an output file.				*/
  /*  single_set	-- True if only one label set is to be xferred	*/
  /*  type	-- Label type, from TYPE parameter		*/
  /*  set_opt	-- Optional arg for zladd			*/
  /*  set_name	-- Value of TASK or PROPERTY parameter		*/
  /*  set_names	-- List of all source set names from zlp/hinfo	*/
  /*  instance	-- instance of TASK or PROPERTY			*/
  /*  instances	-- List of all src set instances from zlp/hinfo	*/
  /*  nsets_label	-- # of sets in source				*/

  int source_unit, in_unit, out_unit;
  int task_cnt, prop_cnt;
  int i, count, status;
  int data_copy, single_set;
  int nlb, nbb;		/* BINARY header and prefix lengths */
  char type[12], set_opt[12];
  char set_name[MAX_LABEL_KEY_SIZE+1];
  char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];
  int instance;
  int instances[MAX_SETS];
  int nsets_label;
  char *p;

  /* Open input 1 (the source of the labels */
  status = zvunit(&source_unit, "INP", 1, NULL);
  abort_on_error(source_unit);
  status = zvopen(source_unit, "OPEN_ACT","SA", "IO_ACT","SA", NULL);
  abort_on_error(source_unit);

  /* Find out whether data is to be copied */
  zvpcnt("OUT",&count);
  data_copy = (count == 1) ? TRUE : FALSE;

  /* Get the file unit numbers */
  if (data_copy) 
    {
      zvselpi(2);		/* inp #2 is the primary */

      status = zvunit(&in_unit,"INP",2, NULL); /* inp2 supplies data, orig lbl */
      abort_on_error(in_unit);

      status = zvunit(&out_unit,"OUT",1, NULL);
      abort_on_error(out_unit);

      status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
      status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
      if (status != 1)
	nlb = nbb = 0;
      status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
		      "U_NLB",nlb,"U_NBB",nbb, NULL);
    }
  else	/* open for update */
    {
      status = zvunit(&out_unit,"INP",2, NULL);
      abort_on_error(out_unit);

      status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }

  /* Get type of labels to deal with (HISTORY, PROPERTY, or BOTH) */

  zvpcnt("TASK", &task_cnt);
  zvpcnt("PROPERTY", &prop_cnt);

  if (task_cnt != 0 && prop_cnt != 0)
    zmabend("Cannot specify both TASK and PROPERTY");

  zvp("TYPE",type,&count);
  if (strcmp(type, "BOTH") == 0)	/* TASK/PROPERTY will override */
    {
      if (prop_cnt != 0)
	strcpy(type,"PROPERTY");
      else if (task_cnt != 0)
	strcpy(type,"HISTORY");
    }
  else				/* Type given, check for consistency */
    {
      if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	zmabend("Cannot use PROPERTY parameter if TYPE=HISTORY");
      if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	zmabend("Cannot use TASK parameter if TYPE=PROPERTY");
    }
  single_set = (prop_cnt != 0 || task_cnt != 0);
  if (single_set)
    {
      if (strcmp(type,"PROPERTY") == 0)
	{
	  strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
	  zvp("PROPERTY", set_name, &count);
	}
      else
	{
	  strcpy(set_opt,"HIST");
	  zvp("TASK", set_name, &count);
	}
      zvp("INSTANCE", &instance, &count);

      /* Convert set name to upper case for comparing strings */
      for (p=set_name; *p != '\0'; p++)
	{
	  if (islower(*p))
	    *p = toupper(*p);
	}
    }
  else
    strcpy(set_name, "");

  if (single_set)
    {
      concat_set(source_unit, out_unit, type, set_opt, set_name, instance);
    }
  else
    {
      if (strcmp(type, "BOTH") == 0 || strcmp(type, "PROPERTY") == 0)
	{
	  nsets_label = MAX_SETS;
	  status = zlpinfo(source_unit, (char *)set_names, &nsets_label,
			   "INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
	  abort_on_error(out_unit);

	  for (i=0; i < nsets_label; i++)
	    {
	      concat_set(source_unit, out_unit, "PROPERTY", "PROPERTY",
			 set_names[i], instances[i]);
	    }
	}
      if (strcmp(type, "BOTH") == 0 || strcmp(type, "HISTORY") == 0)
	{
	  nsets_label = MAX_SETS;
	  status = zlhinfo(source_unit, (char *)set_names, instances,
			   &nsets_label, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
	  abort_on_error(out_unit);

	  for (i=0; i < nsets_label; i++)
	    {
	      concat_set(source_unit, out_unit, "HISTORY", "HIST",
			 set_names[i], instances[i]);
	    }
	}
    }

  /* Copy the data over if requested */
  if (data_copy)
    {
      char *buf = NULL;
      int rec_size;
      status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
      abort_on_error(in_unit);
      buf = (char *)malloc(rec_size);
      if (buf == NULL) {
	zmabend("Out of memory!!!");
      }

      while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	  if (status == END_OF_FILE) break;
	  abort_on_error(in_unit);
	  status = zvwrit(out_unit,buf, NULL);
	  abort_on_error(out_unit);
	}
      free(buf);

      /* Close the files and exit */
      status = zvclose(in_unit, NULL);
    }

  status = zvclose(out_unit, NULL);

  status = zvclose(source_unit, NULL);
}

void concat_set(
		int source_unit,			/* label source and destination units */
		int out_unit,
		char *type,				/* HISTORY or PROPERTY */
		char *set_opt,			/* HIST or PROPERTY, for zladd opts */
		char *set_name,			/* Name of set to transfer */
		int instance			/* Instance number to transfer */
		)
{
  /*	concat_set						*/
  /*								*/
  /*	Concatenates (merges) a single history or property set	*/
  /*	from source_unit to out_unit.				*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /*  header_name	-- Name of set header keyword "TASK","PROPERTY"	*/
  /*  key		-- Keyword of each item				*/
  /*  format	-- Format of each item				*/
  /*  set_names	-- List of all output set names from zlp/hinfo	*/
  /*  instances	-- List of all out set instances from zlp/hinfo	*/
  /*  nsets_label	-- # of sets in source				*/
  /*  out_inst	-- Instance number for set in the output	*/
  /*  value	-- buffer for holding label value		*/

  int i, status;
  char header_name[MAX_LABEL_KEY_SIZE+1];
  char key[33], format[12];
  char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];
  int instances[MAX_SETS];
  int nsets_label;
  int out_inst;
  struct multival value;

  value.data = NULL;
  value.nelements = 0;
  value.maxlength = 0;
  value.allocsize = 0;

  if (strcmp(type, "HISTORY") == 0)
    strcpy(header_name, "TASK");
  else
    strcpy(header_name, "PROPERTY");

  /* Set current key to set name */
  status = zlinfo(source_unit, type, header_name, format,
		  &value.maxlength, &value.nelements,
		  set_opt, set_name,
		  "INSTANCE",instance,
		  "STRLEN", &value.maxlength, NULL);
  abort_on_error(source_unit);

  /* Determine the instance number for the output */

  nsets_label = MAX_SETS;
  if (strcmp(type, "PROPERTY") == 0)
    {
      status = zlpinfo(out_unit, (char *)set_names, &nsets_label,
		       "INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
  else
    {
      status = zlhinfo(out_unit, (char *)set_names, instances, &nsets_label,
		       "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
  abort_on_error(out_unit);

  out_inst = 1;

  for (i=nsets_label-1; i >= 0; i--)
    {
      if (strcmp(set_names[i], set_name) == 0)
	{
	  out_inst = instances[i] + 1;
	  break;
	}
    }

  /* Create the history set in the output if necessary.  This is a trick;	*/
  /* we add a TASK label to the end of the last existing task, which is	*/
  /* all the RTL really needs to see, then let the loop below add USER	*/
  /* and DAT_TIM.								*/
  /* Note:  Property label sets are automatically created.		*/

  if (strcmp(type, "HISTORY") == 0)
    {
      status = zladd(out_unit, type, "TASK", set_name, "FORMAT", "STRING",
		     "MODE", "ADD", NULL);/*"HIST", set_names[nsets_label-1],
					    "INSTANCE", instances[nsets_label-1], 0); !!!!*/
      /* Ignore errors in the above; it will return NO_SUCH_KEY */
    }

  /* Cycle through each key in the subset */
  while (TRUE)
    {

      /* Get next keyword */
      status = zlninfo(source_unit,key,format,&value.maxlength,
		       &value.nelements, NULL);
      if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
	  (strcmp(key,"PROPERTY") == 0)) break;
      break_on_error(source_unit);
      if (strcmp(format, "STRING") == 0)
	value.maxlength++;	/* leave room for null string terminator */

      /* Get next value */
      if (value.maxlength * value.nelements > value.allocsize)
	{
	  if (value.data != NULL)
	    free(value.data);
	  value.data = malloc(value.maxlength * value.nelements);
	  value.allocsize = value.maxlength * value.nelements;
	  if (value.data == NULL)
	    {
	      zmabend("Out of memory!!!");
	    }
	}
      status = zlget(source_unit,type,key,value.data,
		     set_opt,set_name, "INSTANCE",instance,
		     "FORMAT",format, "ULEN", value.maxlength,
		     "NELEMENT", value.nelements, NULL);
      continue_on_error(source_unit);

      /* Add value to output label */

      status = zladd(out_unit, type, key, value.data,
		     set_opt,set_name, "INSTANCE", out_inst,
		     "FORMAT", format, "ULEN", value.maxlength,
		     "NELEMENT", value.nelements, "MODE", "ADD", NULL);

    }	 	/* End of while loop */

  if (value.data != NULL)
    free(value.data);
}

void delete_items(void)
{
  /*	delete_items						*/
  /*								*/
  /*	Delete all or part of the history or property label	*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* count -- count returned from zvparm				*/
  /* defaulted -- flag indicating whether parameter was defaulted	*/
  /* format -- format of keyword returned from zlinfo and zlninfo	*/
  /* i -- increment variable					*/
  /* in_unit -- input file unit number				*/
  /* inst_to_delete -- list of instances for sets_to_delete	*/
  /* instances -- task/prop instances corresponding to set_names	*/
  /* j -- increment variable					*/
  /* key -- current key name from zlninfo				*/
  /* keys_to_delete -- list of keys to delete			*/
  /* maxlen -- maxlength parameter for zlinfo and zlninfo		*/
  /* n_inst -- count of INSTNCES parameter			*/
  /* nelem -- nelements parameter for zlinfo and zlninfo		*/
  /* nsets_label -- number of history or property subsets		*/
  /* nkeys -- count of KEYS parameter				*/
  /* nsets -- count of TASKS or PROPERTY parameter		*/
  /* out_unit -- output file unit number				*/
  /* revised_instances -- revised list of instances for passing	*/
  /*			to zldel				*/
  /* subset -- index variable for looping through subsets		*/
  /* set_names -- list of all history or property subset names	*/
  /* sets_to_delete -- list of tasks or properties to delete	*/
  /* element_to_delete -- ELEMENT param, starting el for deletion	*/
  /* nelement_to_delete -- NELEMENT param, num of el's to delete	*/
  /* n_elem, n_nelem -- count of ELEMENT and NELEMENT parameter	*/
  /* nret -- # of elements actually deleted by zldel call		*/

  int count,in_unit,out_unit,nsets_label,subset,maxlen,nelem,nsets,nkeys;
  int instances[MAX_SETS],revised_instances[MAX_SETS];
  int inst_to_delete[DEL_COUNT],n_inst,i,j,data_copy;
  char keys_to_delete[DEL_COUNT][MAX_LABEL_KEY_SIZE+1];
  char sets_to_delete[DEL_COUNT][MAX_LABEL_KEY_SIZE+1];
  int element_to_delete, nelement_to_delete;
  int n_elem, n_nelem, nret;
  int nlb, nbb;
  int task_cnt, prop_cnt;
  char type[12], set_opt[12];

  char format[12],key[33],*p;
  char set_names[MAX_SETS][MAX_LABEL_KEY_SIZE+1];

  /* Boolean variables */
  int defaulted,use_all_sets,delete_whole_subset,del_all_keys_in_set;
  int care_about_inst,set_header;

  /* Find out whether data is to be copied */
  zvpcnt("OUT",&count);
  data_copy = (count == 1) ? TRUE : FALSE;

  /* Get the file unit numbers */
  if (data_copy) 
    {
      status = zvunit(&in_unit,"INP",1, NULL);
      abort_on_error(in_unit);

      status = zvunit(&out_unit,"OUT",1, NULL);
      abort_on_error(out_unit);

      status = zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
      status = zvget(in_unit,"NLB",&nlb,"NBB",&nbb, NULL);
      if (status != 1)
	nlb = nbb = 0;
      status = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","COND","BINARY",
		      "U_NLB",nlb,"U_NBB",nbb, NULL);
    }
  else	/* open for update */
    {
      status = zvunit(&out_unit,"INP",1, NULL);
      abort_on_error(out_unit);

      status = zvopen(out_unit,"OP","UPDATE","OPEN_ACT","SA", NULL);
    }

  /* Get type of labels to deal with (HISTORY or PROPERTY) */

  zvpcnt("TASKS", &task_cnt);
  zvpcnt("PROPERTY", &prop_cnt);

  if (task_cnt != 0 && prop_cnt != 0)
    zmabend("Cannot specify both TASKS and PROPERTY");

  zvp("TYPE",type,&count);
  if (count == 0)			/* No type specified */
    {
      if (prop_cnt != 0)
	strcpy(type,"PROPERTY");
      else				/* TASK given or default case */
	strcpy(type,"HISTORY");
    }
  else				/* Type given, check for consistency */
    {
      if (strcmp(type,"HISTORY") == 0 && prop_cnt != 0)
	zmabend("Cannot use PROPERTY parameter if TYPE=HISTORY");
      if (strcmp(type,"PROPERTY") == 0 && task_cnt != 0)
	zmabend("Cannot use TASK parameter if TYPE=PROPERTY");
    }

  if (strcmp(type,"PROPERTY") == 0)
    strcpy(set_opt,"PROPERTY");	/* optional arg for zl routines */
  else
    strcpy(set_opt,"HIST");

  /* Get history or property label set information */
  nsets_label = MAX_SETS;

  if (strcmp(type,"PROPERTY") == 0)
    {
      status = zlpinfo(out_unit, (char *)set_names, &nsets_label,
		       "INST_NUM", instances, "ULEN",MAX_LABEL_KEY_SIZE+1, NULL);
    }
  else
    status = zlhinfo(out_unit, (char *)set_names, instances, &nsets_label,
		     "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  abort_on_error(out_unit);

  /* Make revised instance list the same as the original list */
  for (j = 0; j < nsets_label; j++) revised_instances[j] = instances[j];

  /* Initialize boolean flags */
  use_all_sets = FALSE;
  delete_whole_subset = FALSE;
  del_all_keys_in_set = FALSE;
  care_about_inst = FALSE;

  /* Get tasks for scope of deletion */
  if (strcmp(type,"PROPERTY")==0)
    zvparm("PROPERTY", sets_to_delete, &nsets, &defaulted,
	   DEL_COUNT, MAX_LABEL_KEY_SIZE+1);
  else
    zvparm("TASKS", sets_to_delete, &nsets, &defaulted,
	   DEL_COUNT, MAX_LABEL_KEY_SIZE+1);

  /* Convert all set names to upper case for comparing strings */
  for (i=0; i<nsets; i++)
    {
      for (p=sets_to_delete[i]; *p != '\0'; p++)
	if (islower(*p))
	  *p = toupper(*p);
    }

  /* If no set names were given, then use all sets for scope */
  use_all_sets = (nsets == 0);

  /* Get keys to delete */
  zvparm("KEYS", keys_to_delete, &nkeys, &defaulted,
	 DEL_COUNT, MAX_LABEL_KEY_SIZE+1);

  if (nkeys == 0)
    {
      delete_whole_subset = TRUE;
      del_all_keys_in_set = TRUE;
    }
  else
    {
      /* Convert all keywords to upper case for comparing strings */
      for (i=0; i<nkeys; i++)
	{
	  for (p=keys_to_delete[i]; *p != '\0'; p++)
	    if (islower(*p))
	      *p = toupper(*p);
	}

      /* See if user gave keyword /ALL */
      if (strcmp(keys_to_delete[0],"/ALL") == 0)
	{
	  del_all_keys_in_set = TRUE;
	}
      else 
	{
	  if (   string_on_list("TASK",keys_to_delete,nkeys)
		 || string_on_list("USER",keys_to_delete,nkeys)
		 || string_on_list("DAT_TIM",keys_to_delete,nkeys)
		 || string_on_list("PROPERTY",keys_to_delete,nkeys))
	    {
	      zifmessage("KEYS contained illegal keyword");
	      zmabend("Keywords TASK, USER, DAT_TIM, and PROPERTY not allowed");
	    }
	}	/* end else */
    }		/* end else */

  /* Get any instances for narrowing scope */
  zvparm("INSTNCES",inst_to_delete,&n_inst,&defaulted,DEL_COUNT,0);
  care_about_inst = (n_inst > 0);

  /* Get starting element to delete for multi-valued items */
  zvp("ELEMENT", &element_to_delete, &n_elem);
  if (n_elem == 0)
    element_to_delete = 1;		/* defaulted... start at beginning */
  else
    {						/* ELEMENT given */
      if (delete_whole_subset)
	zmabend("KEYS must be specified if ELEMENT is used");
    }

  /* Get number of elements to delete for multi-valued items */
  zvp("NELEMENT", &nelement_to_delete, &n_nelem);
  if (n_nelem == 0)
    nelement_to_delete = -1;	/* defaulted... delete all elements */
  else
    {						/* NELEMENT given */
      if (delete_whole_subset)
	zmabend("KEYS must be specified if NELEMENT is used");
    }

  /* Now cycle through each subset and check deletion scope */
  for (subset = 0; subset < nsets_label; subset++)
    {   if (((! use_all_sets) && 
	     (! string_on_list(set_names[subset],sets_to_delete,nsets)))
	    || (care_about_inst && 
	        (! int_on_list(instances[subset],inst_to_delete,n_inst))))
	continue;

      /* set label pointer to this subset */
      if (strcmp(type,"PROPERTY")==0)
	strcpy(key,"PROPERTY");
      else
	strcpy(key,"TASK");
      status = zlinfo(out_unit, type, key, format, &maxlen, &nelem,
		      set_opt,set_names[subset],"INSTANCE",revised_instances[subset], NULL);
      abort_on_error(out_unit);

      /* Now cycle through all keywords in the subset */
      do
	{   set_header = (strcmp(key,"TASK") == 0)
	    || (strcmp(key,"USER") == 0)
	    || (strcmp(key,"DAT_TIM") == 0)
	    || (strcmp(key,"PROPERTY") == 0);
	  if (   (del_all_keys_in_set && (! set_header))
		 || (string_on_list(key,keys_to_delete,nkeys)))
	    {
	      status = zldel(out_unit, type, key, set_opt,set_names[subset],
			     "INSTANCE",revised_instances[subset],
			     "ELEMENT", element_to_delete,
			     "NELEMENT", nelement_to_delete, "NRET", &nret, NULL);
	      abort_on_error(out_unit);
	      if (nret != 0 &&
		  (element_to_delete == 1 && element_to_delete+nret > nelem))
		{
		  sprintf(msg,"Keyword %s deleted",key);
		  zifmessage(msg);
		}
	      else if (nret == 1)
		{
		  sprintf(msg, "Element %d deleted from keyword %s",
			  element_to_delete, key);
		  zifmessage(msg);
		}
	      else if (nret != 0)
		{
		  sprintf(msg,
			  "Elements %d through %d deleted from keyword %s",
			  element_to_delete, element_to_delete+nret-1, key);
		  zifmessage(msg);
		}
	    }

	  /* Increment to next keyword */
	  status = zlninfo(out_unit,key,format,&maxlen,&nelem, NULL);

	} while ((strcmp(key,"TASK") != 0) && (strcmp(key,"PROPERTY") != 0) &&
		 (status == 1));

      /* Delete the set header if the whole set is to be deleted */
      if (delete_whole_subset)
	{
	  if (strcmp(type,"PROPERTY") == 0)
	    {
	      status = zldel(out_unit, "PROPERTY", "PROPERTY",
			     "PROPERTY",set_names[subset],
			     "INSTANCE",revised_instances[subset], NULL);
	      abort_on_error(out_unit);
	      sprintf(msg,"Property subset %s deleted",set_names[subset]);
	    }
	  else
	    {
	      status = zldel(out_unit, "HISTORY", "USER",
			     "HIST", set_names[subset],
			     "INSTANCE",revised_instances[subset], NULL);
	      abort_on_error(out_unit);

	      status = zldel(out_unit, "HISTORY", "DAT_TIM",
			     "HIST", set_names[subset],
			     "INSTANCE",revised_instances[subset], NULL);
	      abort_on_error(out_unit);

	      status = zldel(out_unit, "HISTORY", "TASK",
			     "HIST", set_names[subset],
			     "INSTANCE",revised_instances[subset], NULL);
	      abort_on_error(out_unit);
	      sprintf(msg,"History subset %s deleted",set_names[subset]);
	    }
	  revise_instance_list(set_names,subset,revised_instances,
			       nsets_label);

	  zifmessage(msg);

	}	/* End if block */
    }		/* End for loop */

  /* Copy the data over if requested */
  if (data_copy)
    {
      char *buf = NULL;
      int rec_size;
      status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
      abort_on_error(in_unit);
      buf = (char *)malloc(rec_size);
      if (buf == NULL) {
	zmabend("Out of memory!!!");
      }

      while (TRUE)
	{   status = zvread(in_unit,buf, NULL);
	  if (status == END_OF_FILE) break;
	  abort_on_error(in_unit);
	  status = zvwrit(out_unit,buf, NULL);
	  abort_on_error(out_unit);
	}
      free(buf);

      /* Close the files and exit */
      status = zvclose(in_unit, NULL);
    }

  status = zvclose(out_unit, NULL);
}

void revise_instance_list(char names[MAX_SETS][MAX_LABEL_KEY_SIZE+1],
			  int subset, int new_list[], int list_len)
{
  int i;

  for (i = subset + 1; i < list_len; i++)
    {
      if (strcmp(names[subset],names[i]) == 0)
	new_list[i] = new_list[i] - 1;
    }
}


int string_on_list(char *string, char list[][MAX_LABEL_KEY_SIZE+1], int list_len)

/*								*/
/*	string_on_list						*/
/*								*/
/*	Boolean function 					*/
/*		Returns true if string is on list		*/
/*		False otherwise					*/
/*								*/
/*	Passed variables:					*/
/*								*/
/* list -- List of C strings 					*/
/* list_len -- number of strings in list			*/
/* string -- string to find on list				*/
/*								*/
/* NOTE:  The inner dimension of list must be MAX_LABEL_KEY_SIZE+1 */
/*								*/
{
  int i;

  for (i = 0; i < list_len; i++)
    {
      if (strcmp(string,list[i]) == 0) return(TRUE);
    }
  return(FALSE);
}

int int_on_list(int intval, int list[], int list_len)
{
  int i;

  for (i = 0; i < list_len; i++)
    {
      if (list[i] == intval) return(TRUE);
    }
  return(FALSE);
}

void edit_label(void){}

void list_label(void)
{
  /*	List out an image label					*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* asterisks -- row of asterisks for delimiter line		*/
  /* count -- count returned from zvparm				*/
  /* current_input -- Input file number currently being listed	*/
  /* defaulted -- Flag indicating whether parameter was defaulted	*/
  /* extent -- Value of keyword EXTENT.  See PDF for description	*/
  /* header -- file header message buffer				*/
  /* input_file_name -- String with name of current input file	*/
  /* number_of_inputs -- Total number of input files given	*/
  /* packstr -- string value of PACK keyword.			*/

  int count,current_input,defaulted,number_of_inputs;
  int unit,nousrtim;
  char input_file_name[256],extent[8],packstr[8],header[256];
  static char asterisks[] =
    "************************************************************";

  /* Begin execution */

  zvpcnt("INP",&number_of_inputs);

  /* Get extent parameter for later use */
  zvparm("EXTENT",extent,&count,&defaulted,0,0);

  /* suppress printing of username and datetime from label */
  nousrtim = zvptst("nousrtim");

  /* Get PACK parameter and save it in a logical variable for later use */
  zvparm("PACK",packstr,&count,&defaulted,0,0);
  if (strcmp(packstr, "PACK") == 0)
    pack_listed_items = TRUE;
  else
    pack_listed_items = FALSE;

  /* Loop through listing each input file */
  for (current_input = 1; current_input <= number_of_inputs; current_input++)
    {

      /* Get input file name */
      zvpone("INP",input_file_name,current_input, 0);

      /* Capitalization removed 5/92 RGD since it's significant for Unix */
      /* Capitalize first letter of file name */
      /****	if (input_file_name[0] > 96)	****/
      /****	    input_file_name[0] &= 0xDF;	****/

      /* Limit length of filename to 256-33 (33 chars is the size of the */
      /* "*** File ***" message) so it will fit in the "header" buffer. */
      input_file_name[256-33] = '\0';

      /* Print out header for current image */
      zifmessage(asterisks);
      sprintf(header,"        ************  File %s ************",
	      input_file_name);
      zifmessage(" ");			/* newline */
      zifmessage(header);

      /* Get file unit number */
      status = zvunit(&unit,"INP",current_input, NULL);
      continue_on_error(unit);

      /* Open current input */
      /* 11/10 lwk:  change action to "SA" when only 1 input, since that is
	 the VICAR standard;  but keep "S" when >1 input because this was
	 apparently the original intent of the multiple input-file option */
      if (number_of_inputs==1)
	status = zvopen(unit,"OPEN_ACT","SA", NULL);
      else
	status = zvopen(unit,"OPEN_ACT","S", NULL);
      if (status != 1) continue;

      /* Find extent of listing to be done, and act accordingly */
      switch (extent[0])
	{   case 'D' : dump_all_items(unit);		/* DUMP		*/
	    break;
	case 'S' : list_system_info(unit);		/* SYSTEM	*/
	  break;
	case 'A' : list_system_info(unit);		/* ALL		*/
	  list_property_items(unit);
	case 'H' : list_history_items(unit,nousrtim);	/* HISTORY	*/
	  break;
	case 'T' : list_task_headers(unit,nousrtim);	/* TASKS	*/
	  break;
	case 'P' : list_property_items(unit);
	  break;
	}

      /* Output a row of asterisks as a delimiter			*/
      zifmessage(" ");
      zifmessage(asterisks);

      /* Close current input and go on to next if there is one	*/
      status = zvclose(unit, NULL);
    }	/* End of for loop 	*/
}	/* End of list_label()	*/

void list_system_info(int unit)
{
  /*	list_system_info					*/
  /*								*/
  /*	subroutine to list out specific system label 		*/
  /*	information to standard output				*/
  /*								*/
  /*	passed variables:					*/
  /*								*/
  /* unit -- unit number of file whose label is being looked at	*/
  /*								*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* dim 		-- number of dimensions in image		*/
  /* format	-- pixel format of image			*/
  /* message	-- single message line				*/
  /* nb		-- Number of bands in image			*/
  /* nl		-- Number of lines (per band) in image		*/
  /* ns		-- Number of samples per line in image		*/
  /* org		-- File organization				*/
  /* type		-- Type of image (usually  =IMAGE)		*/
  /* bltype       -- Type of binary label if specified		*/
  /*								*/

  int dim,nb,nl,ns,nlb,nbb;
  char format[33],message[200],org[33],type[33],host[33],bltype[33];

  /* Get the pertinent information from zvget */
  status = zvget(unit,"FORMAT",format,"NL",&nl,"NS",&ns,"NB",&nb,
		 "NLB",&nlb,"NBB",&nbb,"DIM",&dim,"ORG",org,"TYPE",type,"HOST",host,
		 "BLTYPE",bltype, NULL);
  return_on_error(unit);

  /* Start printing out the info */
  sprintf(message,"                %d dimensional %s file",dim,type);
  zifmessage(message);

  sprintf(message,"                File organization is %s",org);
  zifmessage(message);

  sprintf(message,"                Pixels are in %s format from a %s host",
	  format,host);
  zifmessage(message);

  /* Decide on whether to use nl,ns,nb or n1-n4 */
  if (dim <=3)
    {
      if (dim == 2)
	sprintf(message,"                %d lines",nl);
      else
	{
	  sprintf(message,"                %d bands",nb);
	  zifmessage(message);
	  sprintf(message,"                %d lines per band",nl);
	}
      zifmessage(message);

      sprintf(message,"                %d samples per line",ns);
      zifmessage(message);

      if (strlen(bltype) != 0)
	sprintf(message,"                %d lines of binary header of type %s",
		nlb,bltype);
      else
	sprintf(message,"                %d lines of binary header",nlb);
      zifmessage(message);

      sprintf(message,"                %d bytes of binary prefix per line",
	      nbb);
      zifmessage(message);
    }
  /*  else use n1-n4; */

}


void dump_all_items(int unit)
{
  /*	dump_all_items						*/
  /*								*/
  /*	List out every keyword value pair starting from the 	*/
  /*	beginning of the system label all the way to the 	*/
  /*	end of the label					*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* key -- Keyword returned from zlninfo				*/
  /* format -- format of label item from zlninfo			*/
  /* value -- Structure that describes the value for the item	*/
  /* instances -- array of instances corresp. to tasks		*/
  /* tasks -- array of task names of history subsets		*/
  /* nhist -- number of history subsets				*/
  /* subset -- number of current history subset			*/
  /* dummy -- holds LENGTH from zlinfo... needed since we need the*/
  /* 	    len of the string (via STRLEN optional) instead of  */
  /*	    the len of the int or real, since everything is	*/
  /*	    treated as a string by the zlget calls.		*/
  /*								*/

  int instances[MAX_TASKS],nhist,nprop,subset,length,dummy;
  char key[MAX_LABEL_KEY_SIZE+1],format[12];
  struct multival value;
  char tasks[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
  char props[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];

  printbuf[0] = '\0';			/* empty the string buffer */
  value.allocsize = 0;
  value.data = NULL;

  /***********************************************************************/
  /* Loop through system label */

  while (TRUE)
    {

      /* Next key */
      status = zlninfo(unit,key,format,&dummy,&value.nelements,
		       "STRLEN", &value.maxlength, NULL);
      value.maxlength++;	/* leave room for null string terminator */
      if (status == END_OF_LABEL) return;
      if (status <= 0 && value.data != NULL)
	free(value.data);
      return_on_error(unit);

      /* If end of system label, break out of loop */
      if ((strcmp(key,"TASK") == 0) || (strcmp(key,"PROPERTY") == 0))
	break;

      if (value.maxlength * value.nelements > value.allocsize)
	{
	  value.data = malloc(value.maxlength * value.nelements);
	  value.allocsize = value.maxlength * value.nelements;
	  if (value.data == NULL)
	    {
	      zmabend("Out of memory!");
	    }
	}
      /* Get value of current item */
      status = zlget(unit,"SYSTEM",key,value.data,"FORMAT","STRING",
		     "NELEMENT", value.nelements, "ULEN", value.maxlength, NULL);
      if (status <= 0 && value.data != NULL)
	free(value.data);
      return_on_error(unit);

      /* Output depending on data format */
      print_key_value_pair(key,&value,format);
    }

  flush_key_value_pair();

  /***********************************************************************/
  /* Now cycle through property items */

  nprop = MAX_PROPS;
  status = zlpinfo(unit,(char *)props,&nprop, "inst_num", instances,
		   "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  if (status <= 0 && value.data != NULL)
    free(value.data);
  return_on_error(unit);

  if (nprop > 0) {			/* Only if we have something to do */
    /* Write out current property name */
    sprintf(msg,"PROPERTY='%s'",props[0]);
    zifmessage(msg);

    subset = 0;
    while (TRUE)
      {
	/* Next key */
	status = zlninfo(unit,(char *)key,format,&dummy,&value.nelements,
			 "STRLEN", &value.maxlength, NULL);
	if (status == END_OF_LABEL) break;
	if (strcmp(key,"TASK") == 0) break;		/* end of props */
	if (status <= 0 && value.data != NULL)
	  free(value.data);
	return_on_error(unit);
	value.maxlength++;	/* leave room for null string terminator */

	/* Check for property header */
	if (strcmp(key,"PROPERTY") == 0) subset = subset + 1;

	/* Get value of current item */
	if (value.maxlength * value.nelements > value.allocsize)
	  {
	    if (value.data != NULL)
	      free(value.data);
	    value.data = malloc(value.maxlength * value.nelements);
	    value.allocsize = value.maxlength * value.nelements;
	    if (value.data == NULL)
	      {
		zmabend("Out of memory!!");
	      }
	  }
	status = zlget(unit,"PROPERTY",key,value.data,
		       "PROPERTY",props[subset], "INSTANCE",instances[subset],
		       "FORMAT","STRING",
		       "ULEN",value.maxlength, "NELEMENT",value.nelements, NULL);
	if (status <= 0 && value.data != NULL)
	  free(value.data);
	return_on_error(unit);

	/* Output depending on data format */
	print_key_value_pair(key,&value,format);
      }

    flush_key_value_pair();
  }

  /***********************************************************************/
  /* Now cycle through history items */

  nhist = MAX_TASKS;
  status = zlhinfo(unit,(char *)tasks,instances,&nhist,"ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  if (status <= 0 && value.data != NULL)
    free(value.data);
  return_on_error(unit);

  if (nhist > 0) {			/* Only if we have something to do */
    /* Write out current task */
    sprintf(msg,"TASK='%s'",tasks[0]);
    zifmessage(msg);

    subset = 0;
    while (TRUE)
      {
	/* Next key */
	status = zlninfo(unit,key,format,&dummy,&value.nelements,
			 "STRLEN", &value.maxlength, NULL);
	if (status == END_OF_LABEL) break;
	if (status <= 0 && value.data != NULL)
	  free(value.data);
	return_on_error(unit);
	value.maxlength++;	/* leave room for null string terminator */

	/* Check for task header */
	if (strcmp(key,"TASK") == 0) subset = subset + 1;

	/* Get value of current item */
	if (value.maxlength * value.nelements > value.allocsize)
	  {
	    if (value.data != NULL)
	      free(value.data);
	    value.data = malloc(value.maxlength * value.nelements);
	    value.allocsize = value.maxlength * value.nelements;
	    if (value.data == NULL)
	      {
		zmabend("Out of memory!!");
	      }
	  }
	status = zlget(unit,"HISTORY",key,value.data,"HIST",&tasks[subset],
		       "INSTANCE",instances[subset],"FORMAT","STRING",
		       "ULEN", value.maxlength, "NELEMENT", value.nelements, NULL);
	if (status <= 0 && value.data != NULL)
	  free(value.data);
	return_on_error(unit);

	/* Output depending on data format */
	print_key_value_pair(key,&value,format);
      }

    flush_key_value_pair();

  }

  if (value.data != NULL)
    free(value.data);

}

void list_history_items(int unit,int nousrtim)
{
  /*	list_history_items()					*/
  /*								*/
  /*	List out all items in the history label			*/
  /*								*/
  /*	Passed variables:					*/
  /*								*/
  /* unit -- input file unit number				*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* format -- The format of a given label item			*/
  /* i -- temporary increment variable				*/
  /* instances -- Array containing task instances of " "		*/
  /* key -- Name of a label item keyword				*/
  /* number_of_tasks -- Number of history subsets in label	*/
  /* subset -- Increment variable for subsets			*/
  /* task_names -- Array containing task names of history subsets	*/
  /* time -- Time returned by zlget				*/
  /* username -- User field of a given task			*/
  /* value -- Structure that describes the value for the item	*/
  /* dummy -- holds LENGTH from zlinfo... needed since we need the*/
  /* 	    len of the string (via STRLEN optional) instead of  */
  /*	    the len of the int or real, since everything is	*/
  /*	    treated as a string by the zlget calls.		*/
  /*								*/

  int instances[MAX_TASKS],number_of_tasks,subset,i,dummy;
  char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
  char username[132],time[28],key[MAX_LABEL_KEY_SIZE+1],format[32];
  struct multival value;

  printbuf[0] = '\0';			/* empty the string buffer */
  value.allocsize = 0;
  value.data = NULL;

  /* Get task names of history subsets */
  number_of_tasks = MAX_TASKS;	/* No more than MAX_TASKS allowed */
  status = zlhinfo(unit,(char *)task_names,instances,&number_of_tasks,
		   "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  return_on_error(unit);

  /* Cycle through each subset, listing out all labels */
  for (subset = 0; subset < number_of_tasks; subset++)
    {
      flush_key_value_pair();

      /* Get the user and the time (standard task info) for task header */
      status = zlget(unit,"HISTORY","USER",username,
		     "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      if (status <= 0 && value.data != NULL)
	free(value.data);
      return_on_error(unit);
      status = zlget(unit,"HISTORY","DAT_TIM",time,
		     "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      if (status <= 0 && value.data != NULL)
	free(value.data);
      return_on_error(unit);

      /* Print out the header for the task */
      if (nousrtim)
	sprintf(msg,"---- Task: %s -- User: %s -- %s ----", task_names[subset],"","");
      else
	sprintf(msg,"---- Task: %s -- User: %s -- %s ----", task_names[subset],username,time);
      zifmessage(msg);

      /* Set current key to task name */
      status = zlinfo(unit,"HISTORY","TASK",format,&dummy,&value.nelements,
		      "HIST",task_names[subset],"INSTANCE",instances[subset],
		      "STRLEN", &value.maxlength, NULL);
      continue_on_error(unit);

      /* Cycle through each key in the subset */
      while (TRUE)
	{

	  /* Get next keyword */
	  status = zlninfo(unit,key,format,&dummy,&value.nelements,
			   "STRLEN", &value.maxlength, NULL);
	  if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0)) break;
	  break_on_error(unit);
	  value.maxlength++;	/* leave room for null string terminator */

	  /* Don't print out DAT_TIM or USER again */
	  if ((strcmp(key,"DAT_TIM") ==0) || (strcmp(key,"USER") == 0))
	    continue;

	  /* Get next value */
	  if (value.maxlength * value.nelements > value.allocsize)
	    {
	      if (value.data != NULL)
		free(value.data);
	      value.data = malloc(value.maxlength * value.nelements);
	      value.allocsize = value.maxlength * value.nelements;
	      if (value.data == NULL)
		{
		  zmabend("Out of memory!!!");
		}
	    }
	  status = zlget(unit,"HISTORY",key,value.data,
			 "HIST",task_names[subset], "INSTANCE",instances[subset],
			 "FORMAT","STRING", "ULEN", value.maxlength,
			 "NELEMENT", value.nelements, NULL);
	  continue_on_error(unit);

	  /* Print out key and value pair */
	  print_key_value_pair(key,&value,format);
	}	/* End of while loop */
    }		/* End of for loop */

  if (value.data != NULL)
    free(value.data);

  flush_key_value_pair();
}		/* End of list_history_items */

void list_property_items(int unit)
{
  /*	list_property_items()					*/
  /*								*/
  /*	List out all items in the property label		*/
  /*								*/
  /*	Passed variables:					*/
  /*								*/
  /* unit -- input file unit number				*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* format -- The format of a given label item			*/
  /* i -- temporary increment variable				*/
  /* instances -- Array containing property instances		*/
  /* key -- Name of a label item keyword				*/
  /* number_of_props -- Number of property subsets in label	*/
  /* subset -- Increment variable for subsets			*/
  /* prop_names -- Array containing names of property subsets	*/
  /* value -- Structure that describes the value for the item	*/
  /* dummy -- holds LENGTH from zlinfo... needed since we need the*/
  /* 	    len of the string (via STRLEN optional) instead of  */
  /*	    the len of the int or real, since everything is	*/
  /*	    treated as a string by the zlget calls.		*/
  /*								*/

  int instances[MAX_PROPS],number_of_props,subset,i,dummy;
  char prop_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
  char key[MAX_LABEL_KEY_SIZE+1],format[32];
  struct multival value;

  printbuf[0] = '\0';			/* empty the string buffer */
  value.allocsize = 0;
  value.data = NULL;

  /* Get property names of property subsets */
  number_of_props = MAX_PROPS;	/* No more than MAX_PROPS allowed */
  status = zlpinfo(unit,(char *)prop_names,&number_of_props,
		   "inst_num", instances, "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  return_on_error(unit);

  /* Cycle through each subset, listing out all labels */
  for (subset = 0; subset < number_of_props; subset++)
    {

      flush_key_value_pair();

      /* Print out the header for the property */
      if (instances[subset] <= 1)
	sprintf(msg,"---- Property: %s ----", prop_names[subset]);
      else
	sprintf(msg,"---- Property: %s (#%d) ----", prop_names[subset],
		instances[subset]);
      zifmessage(msg);

      /* Set current key to task name */
      status = zlinfo(unit,"PROPERTY","PROPERTY",format,&dummy,
		      &value.nelements,"PROPERTY",prop_names[subset],
		      "INSTANCE",instances[subset],
		      "STRLEN", &value.maxlength, NULL);
      continue_on_error(unit);

      /* Cycle through each key in the subset */
      while (TRUE)
	{

	  /* Get next keyword */
	  status = zlninfo(unit,key,format,&dummy,&value.nelements,
			   "STRLEN", &value.maxlength, NULL);
	  if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
	      (strcmp(key,"PROPERTY") == 0)) break;
	  break_on_error(unit);
	  value.maxlength++;	/* leave room for null string terminator */

	  /* Get next value */
	  if (value.maxlength * value.nelements > value.allocsize)
	    {
	      if (value.data != NULL)
		free(value.data);
	      value.data = malloc(value.maxlength * value.nelements);
	      value.allocsize = value.maxlength * value.nelements;
	      if (value.data == NULL)
		{
		  zmabend("Out of memory!!!");
		}
	    }
	  status = zlget(unit,"PROPERTY",key,value.data,
			 "PROPERTY",prop_names[subset], "INSTANCE",instances[subset],
			 "FORMAT","STRING", "ULEN", value.maxlength,
			 "NELEMENT", value.nelements, NULL);
	  continue_on_error(unit);

	  /* Print out key and value pair */
	  print_key_value_pair(key,&value,format);
	}	/* End of while loop */
    }		/* End of for loop */

  if (value.data != NULL)
    free(value.data);

  flush_key_value_pair();
}		/* End of list_property_items */

void print_key_value_pair(char key[], struct multival *value, char *format)
{
  /*	subroutine to print out a key-value pair			*/
  /*	Note: You must call flush_key_value_pair() before printing	*/
  /*	      anything other than key-value pairs so the last line can	*/
  /*	      come out (nothing is printed until the line fills if the	*/
  /*	      pack option is set).					*/
  /*									*/

  int i,length;

  /* If packing, make sure key and at least one element will fit on the line */
  length = strlen(key) + strlen(value->data) + 8;   /* len of key + 1 elem */
  if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH))
    {
      zifmessage(printbuf);		/* flush old buffer */
      printbuf[0] = '\0';
    }

  if (strlen(printbuf) != 0)
    strcat(printbuf, "  ");		/* two spaces between items */
  strcat(printbuf, key);
  strcat(printbuf, "=");

  if (value->nelements > 1)		/* multivalued */
    strcat(printbuf, "(");

  for (i=0; i<value->nelements; i++)
    {
      length = strlen(value->data+(i*value->maxlength)) + 4;
      if ((strlen(printbuf)!=0) && (strlen(printbuf)+length >= WIDTH))
	{
	  zifmessage(printbuf);	/* flush old buffer */
	  printbuf[0] = '\0';
	}

      if (*format == 'S')
	strcat(printbuf, "'");
      strcat(printbuf, value->data+(i*value->maxlength));
      if (*format == 'S')
	strcat(printbuf, "'");
      if (i != value->nelements-1)
	strcat(printbuf, ", ");
    }

  if (value->nelements > 1)
    strcat(printbuf, ")");

  if (!pack_listed_items && strlen(printbuf) != 0)	/* flush buffer */
    {
      zifmessage(printbuf);
      printbuf[0]='\0';
    }
}

void flush_key_value_pair(void)
/*									*/
/*	Flushes print_key_value_pair()'s buffer.  Must be called before	*/
/*	printing anything else to the screen after calling		*/
/*	print_key_value_pair().						*/

{
  if (strlen(printbuf) != 0)
    {
      zifmessage(printbuf);
      printbuf[0]='\0';
    }
}

void list_task_headers(int unit,int nousrtim)
{
  /*	list_task_headers()					*/
  /*								*/
  /*	List out standard history subset info only		*/
  /*								*/
  /*	Passed variables:					*/
  /*								*/
  /* unit -- input file unit number				*/
  /*								*/
  /*	Local variables:					*/
  /*								*/
  /* instances -- Array containing task instances of " "		*/
  /* number_of_tasks -- Number of history subsets in label	*/
  /* subset -- Increment variable for subsets			*/
  /* task_names -- Array containing task names of history subsets	*/
  /* time -- Time returned by zlget				*/
  /* username -- User field of a given task			*/

  int instances[MAX_TASKS],number_of_tasks,subset;
  char task_names[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
  char time[28],username[132];

  /* Get task names of history subsets */
  number_of_tasks = MAX_TASKS;	/* No more than MAX_TASKS allowed */
  status = zlhinfo(unit,(char *)task_names,instances,&number_of_tasks,
		   "ulen",MAX_LABEL_KEY_SIZE+1, NULL);
  return_on_error(unit);

  /* Cycle through each subset, listing out standard task info */
  for (subset = 0; subset < number_of_tasks; subset++)
    {

      /* Get the user and the time (standard task info) for task header */
      status = zlget(unit,"HISTORY","USER",username,
		     "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      return_on_error(unit);
      status = zlget(unit,"HISTORY","DAT_TIM",time,
		     "HIST",task_names[subset],"INSTANCE",instances[subset], NULL);
      return_on_error(unit);

      /* Print out the header for the task */
      if (nousrtim)
	sprintf(msg,"---- Task: %s -- User: %s -- %s ----", task_names[subset],"","");
      else
	sprintf(msg,"---- Task: %s -- User: %s -- %s ----", task_names[subset],username,time);
      zifmessage(msg);
    }
}

void remove_label(void)
{
  /* 	remove entire label from image file			*/
  /*								*/
  /*	Local variables:					*/
  /*								*/

  int count,defaulted;
  int in_unit;		/* Input file unit number	*/
  int out_unit;               /* Output file unit number	*/
  int sl,ss,nl,ns;		/* Vicar size field		*/
  int nli,nsi;		/* Dummies for zvsize		*/
  int nlb,nsb;		/* # of binary lines & samps	*/
  int nrecs;			/* Number of records to copy	*/
  int line;			/* image line increment variable*/
  int samp;			/* image sample increment variable*/
  int band;			/* image band increment variable*/

  int lblsize;		/* vicar label size		*/
  int recsize;		/* record size of image file 	*/
  int numll;			/* number of lines of vicar label */
  char org[32];

  struct       		/* Window into the input        */
  {   
    int sb;	        	/* Starting band of input	*/
    int nb;		        /* Number of bands		*/
  } bands;

  char *buf = NULL;
  int rec_size;

  /* Get unit numbers */
  status = zvunit(&in_unit,"INP",1, NULL);
  abort_on_error(in_unit);
  status = zvunit(&out_unit,"OUT",1, NULL);
  abort_on_error(out_unit);

  /* Check for no blocking on output */
  if (zvptst("NOBLOCK"))
    status = zvadd(out_unit,"COND","NOBLOCK", NULL);

  /* Open files	*/
  if (zvptst("BINARY") || zvptst("NOBINHEAD") || zvptst("NOBINPREF"))
    zvopen(in_unit, "OPEN_ACT","SA", "IO_ACT","SA", "COND","BINARY", NULL);
  else
    zvopen(in_unit,"OPEN_ACT","SA","IO_ACT","SA", NULL);

  zvsize(&sl,&ss,&nl,&ns,&nli,&nsi);
  if ((ns < 14) && zvptst("NOBLOCK"))
    {
      zifmessage("Output record must be greater than 14 samples");
      zmabend("for unblocked tapes.");
    }

  zvparm("BANDS",&bands,&count,&defaulted,0,0);
  if (defaulted)
    {
      bands.sb =  1;
      status = zvget(in_unit,"NB",&bands.nb, NULL);
    }

  memset(org, 0, 8); 
  status = zvget(in_unit,"ORG",org, NULL);

  status = zvget(in_unit, "RECSIZE", &rec_size, NULL);
  abort_on_error(in_unit);
  buf = (char *)malloc(rec_size);
  if (buf == NULL) {
    zmabend("Out of memory!!!");
  }

  if (zvptst("BINARY"))
    {
      zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
	    "NL",&nl, "NS",&ns, "NB",&bands.nb, NULL);
      zvopen(out_unit, "OP","WRITE", "COND","NOLABELS BINARY",
	     "OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
	     "U_NBB",nsb, "U_NLB",nlb, "U_NL",nl, "U_NS",ns,
	     "U_NB",bands.nb, NULL);

      if (strncmp(org, "BIP", 3) == 0)
	nrecs = nlb + nl*ns;
      else
	nrecs = nlb + nl*bands.nb;

      for (line=0; line<nrecs; line++)
	{
	  zvread(in_unit, buf, NULL);
	  zvwrit(out_unit, buf, NULL);
	}
    }
  else if (zvptst("NOBINHEAD")) /* Binary header is removed */
    {
      zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
	    "NL",&nl, "NS",&ns, "NB",&bands.nb, NULL); 

      zvopen(out_unit, "OP","WRITE", "COND","BINARY",
	     "OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
	     "U_NBB",nsb, "U_NL",nl, "U_NS",ns,
	     "U_NB",bands.nb, NULL);

      /* numll=lblsize/recsize; 
	 if (lblsize%recsize != 0)   numll++; NO NEED FOR THIS */

      if (strncmp(org, "BIP", 3) == 0)
	nrecs = nlb + nl*ns;
      else
	nrecs = nlb + nl*bands.nb;

      /* nrecs += (numll-1); NO NEED FOR THIS */


      for (line=0; line<nrecs; line++)
	{

	  zvread(in_unit, buf, NULL);
	  if ((line+1)>=nlb) 
	    zvwrit(out_unit, buf, NULL);
	}
    }
  else if (zvptst("NOBINPREF"))  /* Binary prefix is removed */ 
    {
      zvget(in_unit, "NLB",&nlb, "NBB",&nsb,
	    "NL",&nl, "NS",&ns, "NB",&bands.nb, NULL);

      zvopen(out_unit, "OP","WRITE", "COND","BINARY",
	     "OPEN_ACT","SA", "IO_ACT","SA", "U_ORG",org,
	     "U_NLB",nlb, "U_NL",nl, "U_NS",ns,
	     "U_NB",bands.nb, NULL);

      if (strncmp(org, "BIP", 3) == 0)
	nrecs = nlb + nl*ns;
      else
	nrecs = nlb + nl*bands.nb;

      for (line=0; line<nrecs; line++)
	{
	  zvread(in_unit, buf, NULL);
	  zvwrit(out_unit, buf, NULL);
	}
    }
  else		/* Not binary */
    {
      status = zvopen(out_unit,"OP","WRITE","COND","NOLABELS","OPEN_ACT","SA",
		      "IO_ACT","SA","U_ORG",org,"U_NL",nl,"U_NS",ns,"U_NB",bands.nb, NULL);

      /*  Copy Data	*/
      if ( strncmp( org,"BSQ",3 ) == 0 )
	for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
	  for (line = sl; line <= nl + sl - 1; line++)
	    {
	      status = zvread(in_unit,buf,"SAMP",ss,"LINE",line,
			      "BAND",band, "NSAMPS",ns, NULL);
	      status = zvwrit(out_unit,buf, NULL);
	    }
      else if ( strncmp( org, "BIL",3 ) == 0 )
	for (line = sl; line <= nl + sl - 1; line++)
	  for (band = bands.sb; band <= (bands.nb + bands.sb - 1); band++)
	    {
	      status = zvread(in_unit,buf,"SAMP",ss,"LINE",line,
			      "BAND",band, "NSAMPS",ns, NULL);
	      status = zvwrit(out_unit,buf, NULL);
	    }
      else
	for (line = sl; line <= nl + sl - 1; line++)
	  for (samp = ss; samp <= ns + ss - 1; samp++)
	    {
	      status = zvread(in_unit,buf,"SAMP",samp,"LINE",line,
			      "BAND",bands.sb,"NBANDS",bands.nb, NULL);
	      status = zvwrit(out_unit,buf, NULL);
	    }
    }

  free(buf);

  /* Close files and exit */
  status = zvclose(in_unit, NULL);
  status = zvclose(out_unit, NULL);
}

void switch_labels(void)
{
  /*	switch_labels						*/
  /* 								*/
  /* Create an output containing the data and system label	*/
  /* of the second input and the history and property labels of	*/
  /* the first.							*/
  /* 								*/
  int input_1,input_2,output;		/* Unit numbers		*/
  int nl,ns,nb,nlb,nbb;		/* Size of output	*/
  int i;				/* temp variable	*/
  char format[12];			/* format of output	*/
  char type[32];			/* type of output	*/
  char org[8];			/* ORG of output	*/

  status = zvunit(&input_1,"INP",1, NULL);
  status = zvunit(&input_2,"INP",2, NULL);

  status = zvopen(input_1,"OPEN_ACT","SA","IO_ACT","SA", NULL);
  status = zvopen(input_2,"OPEN_ACT","SA","IO_ACT","","COND","BINARY", NULL);
  status = zvget(input_2,"NLB",&nlb,"NBB",&nbb, NULL);
  if (status != 1)
    nlb = nbb = 0;

  /* Get size of output */
  status = zvget(input_2,"FORMAT",format,"NL",&nl,"NS",&ns,"NB",&nb,
		 "TYPE", type, "ORG", org, NULL);

  /* Open output */
  status = zvunit(&output,"OUT",1, NULL);
  status = zvopen(output,"OP","WRITE","U_FORMAT",format,
		  "O_FORMAT",format,"U_NL",nl,"U_NS",ns,"U_NB",nb,
		  "U_NLB",nlb,"U_NBB",nbb,"COND","BINARY",
		  "OPEN_ACT","SA","IO_ACT","SA","TYPE",type,"U_ORG",org, NULL);

  /* Copy the data */

  {
    char *buf = NULL;
    int rec_size;
    status = zvget(input_2, "RECSIZE", &rec_size, NULL);
    abort_on_error(input_2);
    buf = (char *)malloc(rec_size);
    if (buf == NULL) {
      zmabend("Out of memory!!!");
    }

    while (TRUE)
      {   status = zvread(input_2,buf, NULL);
	if (status == END_OF_FILE) break;
	abort_on_error(input_2);
	status = zvwrit(output,buf, NULL);
	abort_on_error(output);
      }
    free(buf);
  }

  /* Close the data sets */
  status = zvclose(input_1, NULL);
  status = zvclose(input_2, NULL);
  status = zvclose(output, NULL);
}
