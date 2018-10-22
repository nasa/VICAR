/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/*
 *	q_ functions to add/modify valid in a block
 *	Use q_ functions in other source files to send or write the block.
 *
 *      CHANGE LOG:
 *      
 *      12-FEB-87 Created...tpl
 *      24-JUL-87 Changed q_validstring to q_validstr...tpl
 *      26-AUG-87 Corrected Vm_allocvalid to Vm_AllocValid to keep UNIX
 *                happy...tpl
 *      19-FEB-88 Allocated valid string properly and check for 0 valid
 *                count in q_validstr...tpl
 *      29-feb-88 Don't have to allocate string since AllocValid take
 *                care of it...tpl
 *      21-apr-88 Corrected q_validintg and q_validreal call seq...tpl
 *	07-oct-88 Truncate valid strings rather than discarding...palm
 *	02-dec-88 q_validonestr...nci
 *      28-dec-88 Check current value against valid list...tpl
 *	26-jan-89 New POINTER_VALIDS logic...palm
 *      08-nov-89 fixed typo PONITER to POINTER...tpl
 *      10-nov-89 return code depending on mode...tpl
 *      17-nov-89 commented out previous changes...tpl
 *      17-nov-89 return p_error instead of fail since it did not fail
 *                to set the valids...tpl
 *	04-apr-92 PR1308: Use SVAL instead ov +i...ljn
 *	10-apr-92 Fixed memory leak in q_validprep when the count of valids
 *		  does NOT change. We still needed to free the valids..krw
 *	02-nov-92 PR1440, PR1717 only set v_size (for V_STRINGS) if it is too 
 *		  small to handle all the valid values...krw
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include        "tminc.inc"         /* Variable struct def                  */
#include	"parblk.inc"	/* parameter block definitions		*/
#include        "syninc.inc"        /* Syntax package defs                  */
#include	"resinc.inc"	/* restricted allocation package	*/
#include        "terminc.inc"       /* terminal package defs                */
#include "syninc.inc"
#include "taeintproto.h"

    IMPORT TEXT    pm_name[], pk_name[], pm_type[], pk_type[],
    		   pm_count[], pk_count[],
    		   pm_room[], pk_room[], pm_size[], pk_size[];

#define	VNOROOM	{/*x_error((*p).mode, pm_room, pk_room, name);*/	\
    		 return (P_NOROOM);  }	

#define VBADNAME {/*x_error((*p).mode, pm_name, pk_name, name);*/	\
    		 return (P_BADNAME); }

#define VBADTYPE {/*x_error((*p).mode, pm_type, pk_type, name);*/	\
    		 return (P_BADTYPE); }

#define VBADCOUNT {/*x_error((*p).mode, pm_count, pk_count, name);*/ \
		  return (P_BADCOUNT);  }


#define VBADSIZE {/*x_error((*p).mode, pm_size, pk_size, name);*/ \
		  return (P_OVER);	}
#define VBADVALUE {/*x_error((*p).mode,"Current value for variable '%s' is incompatible with new valids.","TAE-INVVAL",name); */\
		  return (P_ERROR);	}

#define VMINMAX {/* x_error((*p).mode, "Invalid ranges specified for variable '%s'.","TAE-RANGE",name); */\
		  return (P_BMINMAX);	}

FUNCTION static VOID putlist(struct VARIABLE *v);

/*
 *	q_validintg.	Place integer values into a V-block.
 */

    FUNCTION CODE q_validintg(

    struct PARBLK	*p,		/* V-block			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of variable	*/
    TAEINT		ilow[],		/* in: value vector		*/
    TAEINT		ihigh[]		/* in: value vector		*/
    )

    {
    COUNT		j,i;
    struct VARIABLE	*v;
    CODE		code;
    struct I_VALID      *ivalid;

    code = q_validprep(p, name, count, V_INTEGER, &v);
    if (code != SUCCESS)
        return(code);    

    ivalid = (struct I_VALID *)(*v).v_valid;
    if ( ivalid == NULL )
         return ( SUCCESS );
    (*ivalid).count = count;		/* set new count		*/
    for (i=0; i < count; i++)		/* no values if count < 1	*/
        {

        if ( ilow[i] > ihigh[i] )
             VMINMAX
        (*ivalid).range[i].high = ihigh[i];
        (*ivalid).range[i].low = ilow[i];
        }   
    /*
     * check current value against the new valid range
     */
    j=0;
    while ( j < (*v).v_count )
      {
      if ( SVAL(*v,j) != NULL )
	{
    	for (i=0; i < count; i++)	/* no values if count < 1	*/
        	{
                code = P_ERROR;
                
        	if ( IVAL(*v, j ) >= ilow[i] && IVAL(*v,j) <= ihigh[i] )
                       {
                       code = SUCCESS;
                       break; 
		       }
                }
         }
	if ( code != SUCCESS )
                VBADVALUE
        j++;
       }
    if ( code != SUCCESS )
                VBADVALUE
    return(SUCCESS);
    }

/*
 *	q_validreal.	Place real (TAEFLOAT) values into a V-block.
 */

    FUNCTION CODE q_validreal(

    struct PARBLK	*p,		/* V-block			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of variable	*/
    TAEFLOAT		rlow[],		/* in: reals			*/
    TAEFLOAT		rhigh[]		/* in: reals			*/
    )

    {
    COUNT		i,j;
    struct VARIABLE	*v;
    CODE		code;
    struct R_VALID      *rvalid;

    code = q_validprep(p, name, count, V_REAL, &v);
    if (code != SUCCESS)
        return(code);
     
    rvalid = (struct R_VALID *)(*v).v_valid;
    if ( rvalid == NULL )
         return ( SUCCESS );
    (*rvalid).count = count;		/* set new count		*/
    for (i=0; i < count; i++)		/* no values if count < 1	*/
        {
        if ( rlow[i] > rhigh[i] )
            VMINMAX
        (*rvalid).range[i].high = rhigh[i];
        (*rvalid).range[i].low = rlow[i];
        }
    j=0;
    while ( j < (*v).v_count )
      {
      if ( SVAL(*v,j) != NULL )
	{
    	for (i=0; i < count; i++)	/* no values if count < 1	*/
        	{
                code = P_ERROR;
                
        	if ( RVAL(*v, j ) >= rlow[i] && RVAL(*v,j) <= rhigh[i] )
                       {
                       code = SUCCESS;
                       break; 
		       }
                }
         }
	if ( code != SUCCESS )
            VBADVALUE
        j++;
       }
    if ( code != SUCCESS )
            VBADVALUE
    return(SUCCESS);
    }

/* 
 *	q_validstr.  Set string values in V-block.
 *
 */

    FUNCTION CODE q_validstr(

    struct PARBLK	*p,		/* V-block			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of vector		*/
					/* (0 means set count = 0)	*/
    TEXT		*vector[]	/* in: vector of string ptrs	*/
    )

    {
    struct VARIABLE	*v;
    COUNT		i,j;
    CODE		code;
    CODE                alloctype;
    struct S_VALID      *svalid;
    TEXT		*s;
    COUNT		maxLength;
    COUNT		length;
    
    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);

    code = q_validprep(p, name, count, V_STRING, &v);
    if (code != SUCCESS)
        return(code);

    svalid = (struct S_VALID *)(*v).v_valid;
    if ( svalid == NULL )
         return ( SUCCESS );
    (*svalid).count = count;		/* set new count		*/
    maxLength = 0;
    for(i=0; i < count; i++)		/* move string ptrs to (*v)	*/
	    {
#ifdef POINTER_VALIDS
	    length = s_length (vector[i]);
	    if (length > maxLength)
		maxLength = length;
	    if (alloctype == P_MODE_TAE_ALLOC)
	        s = (TEXT *) tae_alloc (1, length+1); 
	    else
		{
		s = (TEXT *) r_alloc ((*p).pool, length+1);
		if (s == NULL)
		    VNOROOM
		}
	    (*svalid).slist[i].string = s;
#endif
	    s_bcopy(vector[i], (*svalid).slist[i].string, VALIDSIZ);
	    }
#ifdef POINTER_VALIDS
    if ((*v).v_size < maxLength)		/* make sure size can handle */
        (*v).v_size = maxLength;		/* all valid values          */
#endif
    j = 0;
    while ( j < (*v).v_count )
	{
        if ( SVAL(*v,j) != NULL )
	  {
    	  for (i=0; i < count; i++)	/* no values if count < 1	*/
        	{
                code = P_ERROR;
        	if ( s_equal ( SVAL(*v,j), vector[i] ) )
                        {
                        code = SUCCESS;
                	break;
                        }
                }
          if ( code != SUCCESS )
                VBADVALUE
          j++;
          }
        }
     if ( code != SUCCESS )
                VBADVALUE
    return (SUCCESS);
    }		

/* 
 *	q_validonestr.  Set one string valid value in V-block.
 */

    FUNCTION CODE q_validonestr(

    struct PARBLK	*p,		/* V-block			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		idx,		/* in: which valid value 	*/
					/* (0 means set count = 0)	*/
    TEXT		onestr[]	/* in: one string ptr		*/
    )

    {
    struct VARIABLE	*v;
    CODE		code;
    CODE                alloctype;
    struct S_VALID      *svalid;
    
    if (s_length(onestr) > VALIDSIZ)
	    VBADSIZE

    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);


    code = q_validprep(p, name, idx, V_STRING, &v);	/* TBD: bug here */
    if (code != SUCCESS)				/* (old valids lost) */
        return(code);

    svalid = (struct S_VALID *)(*v).v_valid;
    if ( svalid == NULL )
         return ( SUCCESS );
    (*svalid).count = idx;		/* set new count		*/
    if (idx != 0) 			/* move string ptrs to (*v)	*/
	    {
#ifdef POINTER_VALIDS
	    abort();			/* TBD: q_validonestring bugs */
#endif
	    s_copy( onestr, (*svalid).slist[idx-1].string );
	    }
    return (SUCCESS);
    }		

/*
 *	q_validprep.   Prepare a variable for a q_ operation.
 *
 *	NOTE: In TM, a VARIABLE is always allocated with a vector count
 *	of v_maxc; however, to save space in the mailbox, the 'package'
 *	function allocates VARIABLEs with vectors only as long as needed,
 *	i.e., v_count.   So here, we must re-allocate a value vector
 *	if the new count is larger than the old count.
 */

    FUNCTION CODE q_validprep (

    struct PARBLK	*p,		/* in/out: PARBLK		*/
    TEXT		name[],		/* in: paramter name		*/
    FUNINT		count,		/* in: proposed valid count	*/
    FUNINT		type,		/* in: parm type		*/
    struct VARIABLE	**vv		/* out: VARIABLE ptr		*/
    )

    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/
    COUNT		size = 0;
    COUNT		i;
    CODE                alloctype;
    struct S_VALID      *svalid = 0;
    struct I_VALID      *ivalid;
    struct R_VALID      *rvalid;
    COUNT               valid_cnt;
    
    alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);
    v = p_fvar(p, name); 		/* find name in V-block (works for parm.qual)*/
    if (v == NULL)
         VBADNAME
    if ((*v).v_type != type)
	    VBADTYPE
    valid_cnt = 0;
    if (type == V_STRING)			/* deallocate old valids */
            {
            svalid = (struct S_VALID *)(*v).v_valid;
            if ( svalid != NULL )
                valid_cnt = (*svalid).count;
            }
    else if ( type == V_INTEGER )
            {
            ivalid = (struct I_VALID *)(*v).v_valid;
            if ( ivalid != NULL )
                valid_cnt = (*ivalid).count;
            }
    else if ( type == V_REAL )
            {
            rvalid = (struct R_VALID *)(*v).v_valid;
            if ( rvalid != NULL )
                valid_cnt = (*rvalid).count;
            }            

    if  (valid_cnt >= 1) {			/* any valids currently? */
        if ( alloctype == P_MODE_TAE_ALLOC )	/* Yes, so free them */
	    {
#ifdef POINTER_VALIDS
	    if ((*v).v_type == V_STRING && svalid)
		for (i=0; i < (*svalid).count; i++)
		    tae_free ((*svalid).slist[i].string);
#endif
    	    if (count != valid_cnt )	/* change in count	*/
               	tae_free ( (*v).v_valid );
	    }
        else
	    {
#ifdef POINTER_VALIDS
	    if ((*v).v_type == V_STRING && svalid)
		for (i=0; i < (*svalid).count; i++)
		   r_dealloc ((*p).pool, (*svalid).slist[i].string);
#endif
    	    if (count != valid_cnt )			/* change in count */
                r_dealloc((*p).pool, (*v).v_valid);	/* release the space */
	    }
    }

    if (count != valid_cnt)	/* change in count so reallocate v_valid */
	{
	if (count < 1)
	    (*v).v_valid = NULL;
	else
               /*	allocate valid structure in pool		*/
            {
            if ( alloctype == P_MODE_TAE_ALLOC )
                (*v).v_valid = Vm_AllocValid(v,count);
            else
                {
                if ((*v).v_type == V_INTEGER)
                    {
        	    size = sizeof (struct I_VALID) + 
		           (count-1)*sizeof (struct I_RANGE);
                    }
                else if ((*v).v_type == V_REAL)
                    {
	            size = sizeof (struct R_VALID) + 
		        (count-1)*sizeof (struct R_RANGE);
                    }
                else if ((*v).v_type == V_STRING)
                    {
	            size = sizeof (struct S_VALID) + 
		        (count-1)*sizeof (struct S_RANGE);
                    }
                (*v).v_valid = r_alloc((*p).pool, size);
                if ((*v).v_valid == NULL) 
        	    VNOROOM			/* allocation failure */
                }
            }
        }

    *vv = v;				/* pass VARIABLE ptr to caller	*/
    return (SUCCESS);
    }
/*
*        q_listproc
*
*/
FUNCTION CODE q_listproc 
(
 struct    PARBLK    *inblk,
 TEXT      name[],
struct    PARBLK    *outblk
)
{
        TEXT    string[STRINGSIZ+1];
        CODE    term;
	struct  VARIABLE *v;
        TAEINT   intsel;
        TAEFLOAT realsel;       
        TEXT    *locvec[1];

        v = p_fvar(inblk, name); 		/* find name in V-block (works for parm.qual)*/
        putlist(v);        /*output the list*/
        t_write("\nType in selection:",T_PROMPT);
        t_read ( string, &term );
/*
        output to outblk
*/
    if ( (*v).v_type == V_INTEGER )
        {
        intsel = atoi(string);
        q_intg( outblk, (*v).v_name, 1, &intsel, P_ADD );
        }
    else if ( (*v).v_type == V_REAL ) 
        {
        realsel = atof(string);
        q_real( outblk, (*v).v_name, 1, &realsel, P_ADD );
        }
    else if ( (*v).v_type == V_STRING )
        {
        locvec[0] = (TEXT *)string;
        q_string ( outblk, (*v).v_name, 1, locvec,P_ADD);
        }
    return 0;
}

/* putlist - Put the list of parameter values from the parmst onto the terminal
 *
 */
FUNCTION static VOID putlist
(
 struct VARIABLE	*v
 )
    {
    TEXT		request[STRINGSIZ+1];
    COUNT		msglen;
    struct I_VALID      *ivalid = 0;
    struct R_VALID      *rvalid = 0;
    struct S_VALID      *svalid = 0;
    COUNT               valid_cnt = 0;
    COUNT               i;

    msglen = s_copy("Select a value for the parameter -", request);	
    if (msglen+s_length((*v).v_name)+2 > STRINGSIZ-3)
    	    s_append("...", request);
    else
        {
        msglen = s_append((*v).v_name, request);
    	}
    t_write(request, T_STDCC);
/*
    now output the list of valid values
*/
    if ( (*v).v_type == V_INTEGER )
        {
        ivalid = (struct I_VALID *)(*v).v_valid;
        valid_cnt = (*ivalid).count;
        }
    else if ( (*v).v_type == V_REAL ) 
        {
        rvalid = (struct R_VALID *)(*v).v_valid;
        valid_cnt = (*rvalid).count;
        }
    else if ( (*v).v_type == V_STRING )
        {
        svalid = (struct S_VALID *)(*v).v_valid;
        valid_cnt = (*svalid).count;
        }
    for ( i=1; i<=valid_cnt; i++ )
        {
        if ( (*v).v_type == V_INTEGER )
            {
            sprintf ( request,"Valid %d: %d..%d",i,(*ivalid).range[i-1].low,
                                                    (*ivalid).range[i-1].high);
            t_write(request,T_DOUBLE);
            }
        else if ( (*v).v_type == V_REAL ) 
            {
            sprintf ( request,"Valid %d: %f..%f",i,(*rvalid).range[i-1].low,
                                                    (*rvalid).range[i-1].high);
            t_write(request,T_DOUBLE);
            }
        else if ( (*v).v_type == V_STRING )
            {
            sprintf ( request,"Valid %d: %s",i,(*svalid).slist[i-1].string);
            t_write(request,T_DOUBLE);
            }
        }
        
    return;
    }
