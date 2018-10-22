/************************************************************************/
/* getenv_vic - routine to return the value of an environment variable	*/
/* (Unix) or logical name (VMS).  On VMS, it will return multivalued	*/
/* logical names as a comma-separated list of translations.		*/
/*									*/
/* C call:								*/
/*   value = getenv_vic("NAME");					*/
/*									*/
/* Just like getenv(), the return value is a pointer to a static	*/
/* array.  So, the value must be used or copied before this routine	*/
/* (or getenv()) is called again.  NULL is returned if the name is	*/
/* not found.								*/
/*									*/
/* Fortran call:							*/
/*   call xgetenv_vic(name, value)					*/
/* Both arguments are character*n variables.  A blank string is		*/
/* returned if the name is not found.					*/
/************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"

#if VMS_OS

#include <descrip.h>
#include <lnmdef.h>
#include <ssdef.h>

char *getenv_vic(name)
char *name;
{
    int status, namelen, index;
    unsigned int buffer[32], max_index;
    static char output_buffer[512], temp[132];
    char *table = "LNM$DCL_LOGICAL";

    $DESCRIPTOR(name_desc,name);
    $DESCRIPTOR(table_desc,table);

    table_desc.dsc$w_length = strlen(table);

    name_desc.dsc$w_length = strlen(name);

    max_index = 0;
    buffer[0] = (LNM$_MAX_INDEX << 16) | sizeof(max_index);
    buffer[1] = (unsigned int)&max_index;
    buffer[2] = 0;
    buffer[3] = 0;
    status = sys$trnlnm(0,&table_desc,&name_desc,0,buffer);

    *output_buffer = '\0';

    for (index = 0;index <= max_index;index++) {
	buffer[0] = (LNM$_INDEX << 16) | sizeof(index);
	buffer[1] = (unsigned int)&index;
	buffer[2] = 0;
	buffer[3] = (LNM$_STRING << 16) | sizeof(temp);
	buffer[4] = (unsigned int)temp;
	buffer[5] = 0;
	buffer[6] = (LNM$_LENGTH << 16) | sizeof(namelen);
	buffer[7] = (unsigned int)&namelen;
	buffer[8] = 0;
	buffer[9] = 0;
	status = sys$trnlnm(0,&table_desc,&name_desc,0,buffer);

	if (status != SS$_NORMAL) return((char *)0);
	temp[namelen] = '\0';
	if (*output_buffer == '\0') strcpy(output_buffer,temp);
	else {
	    (void)strcat(output_buffer,",");
	    (void)strcat(output_buffer,temp);
	}
    }
    return(output_buffer);
}

#else

#include <stdlib.h>

char *getenv_vic(name)
char *name;
{
    return getenv(name);
}

#endif


/************************************************************************/
/* Fortran bridge							*/
/************************************************************************/

void FTN_NAME2_(xgetenv_vic,XGETENV_VIC)(char *name, char *value, ZFORSTR_PARAM)
{
    ZFORSTR_BLOCK
    char c_name[512], *c_value;

    zsfor2c(c_name, 511, name, &name, 2, 1, 1, value);

    c_value = getenv_vic(c_name);

    if (c_value == (char *)0)
        c_value = " ";		/* return blanks */

    zsc2for(c_value, 0, value, &name, 2, 2, 2, value);
}

