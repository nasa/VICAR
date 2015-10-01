/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*TDM         CHECKOUT FILE_TIME= 7-SEP-1983 15:00 DUA0:[TAEV1.OLB]FMTVAL.C;33 */
/*TDM         CHECKOUT FILE_TIME=22-AUG-1983 14:55 DUA0:[TAEV1.OLB]FMTVAL.C;21 */
/*TDM         CHECKOUT FILE_TIME=13-JUL-1983 15:38 DUA0:[TAEV1.OLB]FMTVAL.C;17 */

/*
 * Parameter formatting routines.
 *
 *
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	22-aug-83	Updated for NULLABLES...dm
 *	25-aug-83	Convert single quotes to double for strings,
 *		    	return code TRUNCATE if line truncated...dm
 *	02-sep-83	Added forrefnm() for NAME variables...dm
 *	07-sep-83	Change function names from forp...to m_f...dm
 *	14-sep-83	Fixed last character bug in m_fpval...dm
 *	11-oct-83	Fix unix compilations errors...palm
 *	04-may-84	VALUE_x to xVAL ...ces
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *
 ******************************************************************
 */


#include	"stdh.inp"		/* system standard */
#include	"taeconf.inp"		/* TAE configuration */
#include	"parblk.inc"		/* parameter block definitions */
#include	"tminc.inc"		/* TM  related definitions */
#include "taeintproto.h"

/*
 *	m_fpname : Format parameter name.
 */
 
FUNCTION  CODE  m_fpname
(

    TEXT		name[],			/* in: variable name	*/
    TEXT  		line[],			/* IN/OUT: formatted parameter rec */
    COUNT		length			/* IN: line length */

 )
    {
    COUNT		nchar;			/* number of characters */
    TEXT		param[F_Q_NAMESIZ+2];	/* parameter name */
    
    s_copy(name, param);			/* get parameter name */
    nchar = s_append("=", param);		/* append =  */
    if ((s_length(line) + nchar) <= (length-1)) /* leave space for + */
	{
	s_append(param, line);			/* move name to output rec */
	return (SUCCESS);
	}
    else
        return(FAIL);
    }

/*
 * 	m_fpval: Format parameter values .
 * 	Note: The buffer to hold a string parameter must allow for quotes, 
 * 	a paren, and a separator (a comma, and a blank) .
 *
 */

    FUNCTION  CODE  m_fpval(vv, index, line, stdlen, longlen)

    struct  VARIABLE	*vv;			/* IN: pointer to a variable */
    COUNT		index;			/* IN:index to value vector  */
    TEXT  		line[];			/* IN/OUT: formatted parameter rec */
    COUNT		stdlen;		 	/* IN: standard line length  */
    COUNT		longlen;		/* IN: maximum line length   */

    {
    TEXT		tmpbuf[2*STRINGSIZ+6];	/* buffer to hold one value */
    CODE		code;
    COUNT		max_index;		/* max possible index value */
    COUNT		linelen;		/* line length 		    */
    TEXT		string[2*STRINGSIZ+3];	/* scratch buffer 	    */
    TEXT		lastchar[2];	

    max_index = ((*vv).v_count >= 1) ? (*vv).v_count-1 : 0;
    tmpbuf[0] = string[0] = EOS;
    vv = RESOLVE(vv);				/* get ultimate ref	    */
    if ((*vv).v_count == -1)			/* no value specified	    */
	;					/* do nothing		    */
    else if ((*vv).v_count == 0)		/* null value specified	    */
	s_copy("--", tmpbuf);			/* give null value 	    */
    else					/* count >= 1		    */
	{
	if (index == 0 && (*vv).v_count > 1) 	/* first one of multi-value */
	    s_copy("(", tmpbuf);		/* move left paren  	    */
	if ((*vv).v_type == V_INTEGER)
	    {
	    s_i2s(IVAL(*vv, index), string);
	    s_append(string, tmpbuf);		/* copy integr value in text */
	    }
    	else if ((*vv).v_type == V_REAL)
	    {
	    s_r2s(RVAL(*vv, index), string);
	    s_append(string, tmpbuf);		/* copy real value in text   */
	    }
    	else if ((*vv).v_type == V_STRING)
	    {
	    s_copy(SVAL(*vv, index), string);	/* get string value  */
	    dblquote(string);			/* make single quotes to double */
	    s_append(string, tmpbuf);		/* copy the quoted string    */
            }
    	if ((*vv).v_count > 1)			/* if multi-valued parameter */
            {
            s_copy ((index == max_index ? ")" : ","), lastchar);
	    s_append(lastchar, tmpbuf);		/* add ) if last one, else , */
	    }
	}
    if (index == max_index)			/* if last value    	     */
	s_append(", ", tmpbuf); 		/* append separator  	     */
    linelen = (s_length(tmpbuf) >= stdlen) ? longlen:stdlen;  /* long string */
    if (s_length(tmpbuf)+s_length(line) <= linelen-1)
	{
	s_append(tmpbuf, line);
        code = SUCCESS;
	}
    else
	if (s_length(line) > 0  ||		/* if not fresh buffer 	    */
		(s_length(line) == 0 && linelen <= 7))	/* or too small	    */
	    code = FAIL;		/* tell no room for this value      */
  	else				
	    {
	    code = TRUNCATE;		/* truncate string (to avoid loop)  */
	    bytmov((GENPTR) tmpbuf, (GENPTR) line, (linelen-7));
	    line[linelen-7] = EOS;
	    s_append("...\"", line);
	    if ((*vv).v_count > 1)
	        s_append(lastchar, line);
    	    if (index == max_index)		/* if last value 	    */
		s_append(", ", line);	 	/* also append separator    */
	    }
    return(code);        
    }

/*	m_frname. Format reference name for a named parameter.
 *	NOTE: It gets the name of the ultimate reference
 *	if v_type is V_NAME. Else; it does nothing.
 */

FUNCTION  CODE  m_frname
(
    struct  VARIABLE	*vv,			/* IN: pointer to a variable */
    TEXT  		line[],			/* IN/OUT: formatted parameter rec */
    COUNT		stdlen,		 	/* IN: standard line length  */
    COUNT		longlen		/* IN: maximum line length   */

 )
    {
    TEXT		tmpbuf[STRINGSIZ+1];	/* buffer to hold one value */
    COUNT		linelen;		/* line length 		    */

 	
    if ((*vv).v_type != V_NAME)
	return(SUCCESS);

    vv = RESOLVE(vv);			/* point to ultimate ref    */
    s_copy((*vv).v_name, tmpbuf);	/* copy reference name      */
    s_append(", ", tmpbuf); 		/* append separator  	     */
    linelen = (s_length(tmpbuf) >= stdlen) ? longlen:stdlen;  /* long string */
    if (s_length(tmpbuf)+s_length(line) > linelen-1)
	return (FAIL);
    s_append(tmpbuf, line);
    return(SUCCESS);
    }

/*
 * dblquote: 	Take a string, convert any single quotes (") into double
 *		quotes ("") and add enclosing quotes.
 *
 *		NOTE: the string input must be at least large enough 
 *		to accomodate the two surrounding quotes and one additional
 *		set of quotes for every occurrence in the string.
 */

    FUNCTION	VOID	dblquote (s)

    TEXT	s [];				/* IN/OUT: String to quote */
    {
    COUNT	end_str, i, j;

    end_str = s_length (s);
    for (i= end_str; i>=0 ; i--)
	{
	if (s[i] == '"')		/* If there is an interior quote */
	    {
	    for (j=end_str+1; j>i; j--)
		s [j]=s[j-1];		/* Make it a double qoute */
	    end_str++;			/* Line is now longer by one */
	    }
	}
    addqu(s);				/* add quotes around */
    }

