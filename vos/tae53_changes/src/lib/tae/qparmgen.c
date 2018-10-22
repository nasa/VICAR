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
 *	q_ functions to build and manipulate a V-block.
 *	Use q_ functions in other source files to send or write the block.
 *
 *	CHANGE LOG:
 *
 *	22-aug-83	Updated for nullables...dm
 *	11-oct-83	Fixed unix compilations...palm
 *	24-feb-84	In P_ADD mode, a name beginning with underscore
 *			becomes a V_LOCAL (for _PREFACE)....palm
 *	04-may-84	Deleted unused IMPORT declarations...lim
 *	04-may-84	VALUE_x to xVAL ... ces
 *	27-jun-84	Make prep q_prep and external...palm
 *	27-jun-84	Change save_string to q_save...palm
 *			Change alloc_var to q_alloc...palm
 *	09-nov-84	Police string size (PR 826)...palm
 *	27-nov-84	TCL 67: Allow update, addition of parm qualifiers...peb
 *      24-apr-87       Add handling of nopool parblk...tpl
 *	04-aug-87	Allow infinite levels of qualification...palm
 *	07-aug-87	PR1253: fix clearing of cvp vector...palm
 *      12-aug-87       Corrected lower case so UNIX would understand...tpl
 *      07-oct-87       Fixed q_max so deallocation will work...tpl
 *      22-feb-88       Removed count != v_count in Call to Vm_FreeValue...tpl
 *      31-mar-88       Added q_parmpage...tpl
 *      04-apr-88       Added string size to q_alloc...tpl
 *	02-dec-88	q_one_string...tni
 *	02-dec-88	Fix q_init to adjust pool_size a little because callers
 *			continue to pass the PARBLK size rather than the
 *			pool size desired...palm
 *	09-feb-89	zero parblk upon q_init...palm
 *      15-may-89       corrected q_max...tpl
 *      08-nov-89       check input value against valids...tpl
 *      13-nov-89       commented out last changes because of the catch 22
 *                      between valid and current value.  Since one cannot
 *                      change valid and current the same time.  Error 
 *                      usually occure because current value does no match
 *                      valid.
 *	01-dec-89	Move decl of p_fvar() to top...ljn
 *	04-apr-92	PR1308: Use SVAL instead of +i...ljn
 *      21-feb-93       PR1424: Fix function q_one_string ...ws
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"resinc.inc"	/* restricted allocation package	*/
#include	"vminc.inc"     /* dynamic allocation package	        */
#include	"symtab.inc"	/* TM symbol table			*/
#include "syninc.inc"
#include "taeintproto.h"

    FUNCTION CODE q_prep (
    struct PARBLK	*p,		/* in/out: PARBLK		*/
    TEXT		name[],		/* in: paramter name		*/
    FUNINT		count,		/* in: proposed parm count	*/
    FUNINT		type,		/* in: parm type		*/
    FUNINT		mode,		/* in: update or add		*/
    struct VARIABLE	**vv,		/* out: VARIABLE ptr		*/
    FUNINT              strsiz);         /* in: string size              */ 

/*  Standard XQ/XR  error messages and error keys:		*/

    IMPORT TEXT    pm_name[], pk_name[], pm_type[], pk_type[],
    		   pm_count[], pk_count[], pm_dup[], pk_dup[],
		   pm_nopar[], pk_nopar[], pm_oldp[], pk_oldp[],
    		   pm_room[], pk_room[], pm_size[], pk_size[],
                   pm_invsz[], pk_invsz[];


#define	NOROOM	{x_error((*p).mode, pm_room, pk_room, (uintptr_t) name, 0, 0);	\
    		 return (P_NOROOM);  }	

#define BADNAME	{x_error((*p).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);	\
    		 return (P_BADNAME); }

#define BADTYPE	{x_error((*p).mode, pm_type, pk_type, (uintptr_t) name, 0, 0);	\
    		 return (P_BADTYPE); }

#define BADCOUNT {x_error((*p).mode, pm_count, pk_count, (uintptr_t) name, 0, 0); \
		  return (P_BADCOUNT);  }

#define DUPNAME	{x_error((*p).mode, pm_dup, pk_dup, (uintptr_t) name, 0, 0);   \
    		  return (P_DUPNAME);   }

#define BADSIZE {x_error((*p).mode, pm_size, pk_size, (uintptr_t) name, 0, 0); \
		  return (P_OVER);	}
#define BADVALID {x_error((*p).mode,"Invalid value specified for variable '%s'.","TAE-INVVAL",(uintptr_t) name, 0, 0); \
		  return (P_FAIL);	}

#define INVSIZE {x_error((*p).mode, pm_invsz, pk_invsz, (uintptr_t) name, 0, 0); \
		  return (P_BADSIZE);	}

#define NOPARM {x_error((*p).mode, pm_nopar, pk_nopar, (uintptr_t) name, 0, 0); \
		  return (P_NOPARM);	}

#define OLDPARM {x_error((*p).mode, pm_oldp, pk_oldp, (uintptr_t) name, 0, 0); \
		  return (P_OLDPARM);	}

/*
 *	q_init.  Initialize V-block.
 */

FUNCTION VOID q_init
(
 struct PARBLK	*p,		/* PARBLK to initialize		*/
 FUNINT		pool_size,	/* bytes allocated in p.pool	*/
 FUNINT		mode		/* P_ABORT or P_CONT		*/
 )
    {

#define HEAD_SIZ (sizeof(struct PARBLK) - P_BYTES + 8)  /* 8 for align safety*/

    COUNT	l,c;
    CODE	code;

    /* TBD: remove t_init call in q_init.  Needed now to make m_put work. */
    t_init(&l, &c, &code);

    zero_block ((GENPTR) p, 
		(GENPTR) (*p).pool - (GENPTR) p);	/* zero header part */
    (*p).last   = 1;

    /*	Though the caller is supposed to give us the pool size and not
	the size of the full parblk, the following covers the many
	callers who never got the word...
    */
    r_init((*p).pool, pool_size - HEAD_SIZ);/* initialize for r_ package    */

    if ( mode == P_ABORT )
        (*p).mode = P_MODE_ABORT | P_MODE_RESTRICT;
    else if ( mode == P_CONT )
        (*p).mode = P_MODE_CONT | P_MODE_RESTRICT;
    else
        {
        mode = mode & (P_MODE_ABORT | P_MODE_CONT);
        (*p).mode = mode | P_MODE_RESTRICT;
        }
    return;
    }

/*
 *	q_intg.	Place integer values into a V-block.
 */

FUNCTION CODE q_intg
(
 struct PARBLK	*p,		/* V-block			*/
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of variable	*/
 TAEINT		intg[],		/* in: value vector		*/
 FUNINT		mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    COUNT		i;
    struct VARIABLE	*v;
    CODE		code;
/*
    if ( count > 0 && mode == P_UPDATE )
	{
 	code = q_checkvalid ( p, name, intg, count , V_INTEGER);
	if ( code != SUCCESS )
		return (code);
	}
*/
    code = q_prep(p, name, count, V_INTEGER, mode, &v, 0);
    if (code != SUCCESS)
        return(code);    
    for (i=0; i < count; i++)		/* no values if count < 1	*/
        IVAL(*v, i) = intg[i];
    return(SUCCESS);
    }

/*
 *	q_real.	Place real (TAEFLOAT) values into a V-block.
 */

FUNCTION CODE q_real
(
 struct PARBLK	*p,		/* V-block			*/
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of variable	*/
 TAEFLOAT	real[],		/* in: reals			*/
 FUNINT		mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    COUNT		i;
    struct VARIABLE	*v;
    CODE		code;

/*
    if ( count > 0 && mode == P_UPDATE )
        {
        code = q_checkvalid ( p, name, real, count , V_REAL);
        if ( code != SUCCESS )
                return (code);
        }
*/

    code = q_prep(p, name, count, V_REAL, mode, &v, 0);
    if (code != SUCCESS)
        return(code);
    for (i=0; i < count; i++)		/* no values if count < 1	*/
        RVAL(*v, i) = real[i];
    return(SUCCESS);
    }

/* 
 *	q_string.  Set string values in V-block.
 */

FUNCTION CODE q_string
(
 struct PARBLK	*p,		/* V-block			*/
 TEXT		name[],		/* in: variable name		*/
 FUNINT		count,		/* in: count of vector		*/
				/* (0 means set count = 0)	*/
 TEXT		*vector[],	/* in: vector of string ptrs	*/
 FUNINT         mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    struct VARIABLE	*v;
    COUNT		i;
    TEXT		*loc_vector[MAXVAL];	/* local ptr vector	*/
    CODE		code;
    CODE                alloctype;

    if ( count > MAXVAL )
	BADCOUNT;
    for (i=0; i < count; i++)
	if (s_length(vector[i]) > MAXSTRSIZ)
	    BADSIZE
/*
    if ( count > 0 && mode == P_UPDATE )
        {
        code = q_checkvalid ( p, name, vector, count , V_STRING);
        if ( code != SUCCESS )
                return (code);
        }

*/
    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);

    for(i=0; i < count; i++)			/* only if count >= 1	*/
	{					/* move strings to pool	*/
        if ( alloctype == P_MODE_RESTRICT )
        	loc_vector[i] = q_save((*p).pool, vector[i]);
        else
        	loc_vector[i] = Vm_Save(vector[i]);
	if (loc_vector[i] == NULL)
	    NOROOM
        }
    code = q_prep(p, name, count, V_STRING, mode, &v, STRINGSIZ);
    if (code != SUCCESS)
        return(code);
    for(i=0; i < count; i++)		/* move string ptrs to (*v)	*/
        SVAL(*v,i) = loc_vector[i];
    return (SUCCESS);
    }		

/* 
 *	q_one_string.  Set one string value in V-block, rather than
 *		a vector of strings as per q_string.
 */

FUNCTION CODE q_one_string
(
 struct PARBLK	*p,		/* V-block			*/
 TEXT		name[],		/* in: variable name		*/
 COUNT		idx,		/* in: which string value,      */
				/*     idx > 0	                */
 TEXT		onestr[],	/* in: string ptr		*/
 FUNINT	        mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    CODE	      code;
    CODE              alloctype;
    struct VARIABLE  *old_v;
    struct VARIABLE  *new_v;
    int               vec_idx;
    int               new_count;
    int               old_count;


    if (s_length(onestr) > MAXSTRSIZ) 
	BADSIZE

    if ((idx < 1) || (idx > MAXVAL))
        BADCOUNT


    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);

    if (mode == P_ADD) 
    {
      new_count = idx;

      /* allocate the new vector */
      code = q_prep (p, name, new_count, V_STRING, mode, &new_v, STRINGSIZ);
      if (code != SUCCESS)
          return(code);

      /* now set all of the strings into the new vector */
      for (vec_idx=0; vec_idx < new_count; vec_idx++)
      {
          TEXT *string;
          string = (vec_idx+1 == new_count) ? onestr : "";
          SVAL(*new_v,vec_idx) = (alloctype == P_MODE_RESTRICT) ?
                                 q_save((*p).pool,string) : Vm_Save(string);

      } /* end for vec_idx */

    }
    else /* P_UPDATE */
    {
      old_v = p_fvar(p, name);  
      old_count = (*old_v).v_count;

      if (idx <= old_count) 
      {
          /* replace an existing string with the caller supplied string */
          if (alloctype==P_MODE_RESTRICT) 
          {
              r_dealloc((*p).pool, (GENPTR)SVAL(*old_v, idx-1));
              SVAL(*old_v, idx-1) = q_save((*p).pool, onestr);
          }
          else /* TAE_ALLOC */
          {
              tae_free(SVAL(*old_v, idx-1));
              SVAL(*old_v, idx-1) = Vm_Save(onestr);
          } /* end if alloctype */
         
      }
      else  /* idx > current count, idx becomes the new count */
      {
          TEXT  *loc_vector[MAXVAL];    /* local ptr vector */


          new_count = idx;
  
          /* first save all of the current strings */
          for (vec_idx=0; vec_idx < old_count; vec_idx++)
          {
              loc_vector[vec_idx] = (alloctype == P_MODE_RESTRICT) ?
                                    q_save((*p).pool,SVAL(*old_v, vec_idx)):
                                    Vm_Save( SVAL(*old_v, vec_idx) );
          } /* end for vec_idx */
  
          /* allocate a new vector */
          code = q_prep(p, name, new_count, V_STRING, mode, &new_v, STRINGSIZ);
          if (code != SUCCESS)
              return(code);
  
          /* now copy strings to newly allocd cvp vec */
          for (vec_idx=0; vec_idx < new_count; vec_idx++)
          {
              if (vec_idx < old_count)    /* copy an existing string */
              {
                  SVAL(*new_v, vec_idx) = loc_vector[vec_idx];
              }
              else if (vec_idx+1 == idx)  /* copy new, caller supplied str */
              {
                  SVAL(*new_v, vec_idx) = (alloctype == P_MODE_RESTRICT) ?
                                          q_save((*p).pool, onestr) : 
                                          Vm_Save(onestr);
              }   
              else                        /* copy an empty str */
              {
                  SVAL(*new_v, vec_idx) = (alloctype == P_MODE_RESTRICT) ?
                                          q_save((*p).pool, "") : Vm_Save("");
              }
          } /* end for vec_idx */
   
      } /* end if idx */

    } /* end if mode */

    return (SUCCESS);
    } /* end q_one_string */

/* 
 *	q_shortstring.  Set string values in V-block.
 */

FUNCTION CODE q_shortstring
(
 struct PARBLK	*p,		/* V-block			*/
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of vector		*/
				/* (0 means set count = 0)	*/
 FUNINT         strsiz,         /* string size, 0 means set to STRINGSIZ*/
 TEXT		*vector[],	/* in: vector of string ptrs	*/
 FUNINT 	mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    struct VARIABLE	*v;
    COUNT		i;
    TEXT		*loc_vector[MAXVAL];	/* local ptr vector	*/
    CODE		code;
    CODE                alloctype;

    if ( strsiz > MAXSTRSIZ || strsiz < 1 )
        BADSIZE

    if ( count > MAXVAL )
	BADCOUNT;
    for (i=0; i < count; i++)
	if (s_length(vector[i]) > strsiz )
	    BADSIZE

    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);

    for(i=0; i < count; i++)			/* only if count >= 1	*/
	{					/* move strings to pool	*/
        if ( alloctype == P_MODE_RESTRICT )
        	loc_vector[i] = q_save((*p).pool, vector[i]);
        else
        	loc_vector[i] = Vm_Save(vector[i]);
	if (loc_vector[i] == NULL)
	    NOROOM
        }
    code = q_prep(p, name, count, V_STRING, mode, &v, strsiz);
    if (code != SUCCESS)
        return(code);
    for(i=0; i < count; i++)		/* move string ptrs to (*v)	*/
        SVAL(*v,i) = loc_vector[i];
    return (SUCCESS);
    }		

/*
 *	q_prep.   Prepare a variable for a q_ operation.
 *
 *	NOTE: In TM, a VARIABLE is always allocated with a vector count
 *	of v_maxc; however, to save space in the mailbox, the 'package'
 *	function allocates VARIABLEs with vectors only as long as needed,
 *	i.e., v_count.   So here, we must re-allocate a value vector
 *	if the new count is larger than the old count.
 */

FUNCTION CODE q_prep
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 FUNINT		count,		/* in: proposed parm count	*/
 FUNINT		type,		/* in: parm type		*/
 FUNINT		mode,		/* in: update or add		*/
 struct VARIABLE **vv,		/* out: VARIABLE ptr		*/
 FUNINT         strsiz         /* in: string size              */ 
 )
    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/
    struct SYMTAB	*symtab;
    TEXT		parent_name[STRINGSIZ+1];
    TEXT		child_name[STRINGSIZ+1];
    GENPTR		vector;		/* pointer to value vector	*/
    COUNT		size = 0;
    COUNT		i;
    CODE                alloctype;
    BOOL		goodCount;

    if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT )
        alloctype = P_MODE_RESTRICT;
    else
        alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC);
    if (mode == P_UPDATE)
    	{
	v = p_fvar(p, name); 		/* find name in V-block (works for parm.qual)*/
	if (v == NULL)
	    BADNAME
	if ((*v).v_type != type)
	    BADTYPE
	goodCount = (-2 <= count && count <= 0) ||  
	    ((*v).v_minc <= count && count <= (*v).v_maxc) ;
        if (!goodCount)
	    BADCOUNT
        if ( alloctype == P_MODE_TAE_ALLOC )   /* dynamic storage */
          {
            Vm_FreeValue ( (*v).v_cvp, type, (*v).v_count);
	    if (count < 1)
		(*v).v_cvp = NULL;
	    else
		{
	        (*v).v_cvp = Vm_AllocValue(v);
		}
          }
        else             /* restricted storage */
          {
	  if (type == V_STRING)			/* deallocate old strings	*/
          
	    for (i=0; i < (*v).v_count; i++)
	        r_dealloc((*p).pool, (GENPTR) SVAL(*v,i)); 
	  if (count != (*v).v_count )			/* change in count	*/
	    {
	    if  ((*v).v_count >= 1)
		r_dealloc((*p).pool, (*v).v_cvp);	/* release the space	*/
	    if (count < 1)
		(*v).v_cvp = NULL;
	    else
		{
	        if      (type == V_INTEGER) size = sizeof (TAEINT);
	        else if (type == V_REAL)    size = sizeof (TAEFLOAT);
	        else if (type == V_STRING)  size = sizeof (TEXT *);
	        vector = r_alloc((*p).pool, count*size);
	        if (vector == NULL)
	            NOROOM
	        (*v).v_cvp = vector;
		}
	    }
	  }
        }
    else					   /* mode is P_ADD        */
    	{
	if (p_fvar(p, name) != NULL)		   /* name exists?         */
	    DUPNAME
	if (count < -2  || count > MAXVAL)
	    BADCOUNT
	p_get_leaf (name, child_name, parent_name);
	if (NULLSTR(parent_name))		   /* if no qualification  */
            symtab = &(*p).symtab;		   /* symtab to use        */
        else
	    {
            v = p_fvar (p, parent_name);
	    if (v == NULL)
		NOPARM
	    if (!(*v).v_pv12)		    	   /* old TAE parblk?      */
		OLDPARM
            symtab = &(*v).v_qualst;		   /* symtab to use	   */
	    }
	if ( alloctype == P_MODE_RESTRICT )
	   v = q_alloc(symtab, (*p).pool, child_name, type, count, strsiz);
	else
	   v = Vm_Alloc(symtab, child_name, type, count, strsiz );
	if (v == NULL)
	    NOROOM
	(*v).v_class = ((*v).v_name[0] == '_') ? V_LOCAL : V_PARM;
	}
    (*v).v_count = count;		/* set new count		*/
    if ( count == 0 )
        (*v).v_nullable = TRUE;
    *vv = v;				/* pass VARIABLE ptr to caller	*/
    return (SUCCESS);
    }

/* 
 *	q_alloc.  Allocate VARIABLE structure in restricted storage.
 */

FUNCTION struct VARIABLE *q_alloc
( 
 struct SYMTAB	*symtab,	/* in: symtab to link to	*/
 ALIGN		pool[],		/* in: restricted storgae block	*/
 TEXT		name[],		/* in: name of variable		*/
 FUNINT		type,		/* in: variable type		*/
 FUNINT		count,		/* in: number of values		*/
 FUNINT		strsiz		/* in: string size		*/
)
    {
    struct VARIABLE	*v;
    struct VARIABLE	*pc;
    COUNT		vsize = 0;
    
    v = (struct VARIABLE *) r_alloc(pool, sizeof(struct VARIABLE));

    if (v == NULL) 
	    return (NULL);

    zero_block((GENPTR) v, sizeof (struct VARIABLE));
    s_copy(name, (*v).v_name);
    (*v).v_minc = (*v).v_maxc = (*v).v_count = count;
    (*v).v_type = type;
    (*v).v_class = V_PARM;
    (*v).v_pv12 = TRUE;			/* parm created by post V1.2 TAE  */

    if ( strsiz > STRINGSIZ || strsiz < 1 )
        (*v).v_size = STRINGSIZ;
    else
        (*v).v_size = strsiz;

    if      (type == V_INTEGER) vsize = sizeof(TAEINT);
    else if (type == V_REAL)    vsize = sizeof(TAEFLOAT);
    else if (type == V_STRING)  vsize = sizeof(TEXT *);

    if (count >= 1)
	{
	(*v).v_cvp = r_alloc(pool, count * vsize);
	if ((*v).v_cvp == NULL)
	    {
	    r_dealloc(pool, (GENPTR) v); /* deallocate the main struct	*/
	    return (NULL);
	    }
        zero_block ((*v).v_cvp, vsize*count);	/* clear value vector	*/	
	}
    else
	(*v).v_cvp = NULL;		/* no current values if < 1	*/
    
    for(pc=(struct VARIABLE *) symtab; (*pc).v_link != NULL; 
						  pc=(*pc).v_link)
    	;				/* find end of chain		*/
    (*pc).v_link = v;			/* link new entry to chain	*/
    return(v);
    }

/*
 *	q_save.   Copy string to restricted storage.
 */

FUNCTION TEXT *q_save
(
 ALIGN	pool[],			/* restrcted storage		*/
 TEXT	string[]		/* string to copy		*/
)
    {
    TEXT	*s;

    s = (TEXT *) r_alloc(pool, s_length(string) + 1);

    if (s == NULL) return (NULL);
    s_copy(string, s);
    return (s);
    }

/*	
 *    q_max - set the max count of a variable
 */

FUNCTION CODE q_max
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 FUNINT		count		/* in: proposed parm count	*/
)
    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/

    v = p_fvar(p, name); 		/* find name in V-block */
    if (v == NULL)
	BADNAME
    if (count <= 0 || count > MAXVAL)
	BADCOUNT

    if ( (*v).v_minc > count ) 
        (*v).v_minc = count;

    if ( (*v).v_count > count ) 
        (*v).v_count = count;

    (*v).v_maxc = count;

    return(SUCCESS);
}

/*	
 *    q_min - set the min count of a variable
 */

FUNCTION CODE q_min
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 FUNINT		count		/* in: proposed parm count	*/
)
    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/

    v = p_fvar(p, name); 		/* find name in V-block 	*/
    if (v == NULL)
	    BADNAME
    if (count < 0 || count > (*v).v_maxc) 
            BADCOUNT

    (*v).v_minc = count;
    if ( count == 0 )
       (*v).v_nullable = TRUE;
    return(SUCCESS);
    }

/*	
 *    q_stringlength - set the string length of a variable
 */

FUNCTION CODE q_stringlength
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 FUNINT		length		/* in: string length    	*/
)
    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/
    COUNT                i;

    v = p_fvar(p, name); 		/* find name in V-block 	*/
    if (v == NULL)
	    BADNAME
    if ( length < 0 || length > STRINGSIZ ) 
            INVSIZE
    i = 0;
    while ( i < (*v).v_count )
	{
	if ( SVAL(*v,i) != NULL )
		{
		if ( s_length( SVAL( *v, i ) ) > length )
			INVSIZE
		}
        i++;
	}
    (*v).v_size = length;

    return(SUCCESS);
    }

/*	
 *    q_parmpage - set the parmpage of a variable
 */

FUNCTION CODE q_parmpage
(
 struct PARBLK	*p,		/* in/out: PARBLK		*/
 TEXT		name[],		/* in: paramter name		*/
 BOOL           flag           /* in: TRUE or FALSE flag       */
 )
    {
    struct VARIABLE	*v;		/* local VARIABLE ptr		*/

    v = p_fvar(p, name); 		/* find name in V-block 	*/
    if (v == NULL)
	    BADNAME

    (*v).v_page = flag;
    return(SUCCESS);
    }

/* static FUNCTION CODE q_checkvalid ( p, name, value, count , type ) */

/*     struct PARBLK  	*p; */
/*     TEXT   		name[]; */
/*     GENPTR              value;          /\* genptr to value vector       *\/ */
/*     FUNINT              count;          /\* count of value               *\/ */
/*     CODE		type; */

/*     { */
/*     struct VARIABLE     *v;             /\* VARIABLE to check            *\/ */
/*     COUNT               i; */
/*     COUNT               j; */
/*     struct I_VALID      *ivalid; */
/*     struct R_VALID      *rvalid; */
/*     struct S_VALID      *svalid; */
/*     TAEINT              *ival;          /\* integer value vector ptr     *\/ */
/*     TAEFLOAT            *rval;          /\* real value vector ptr        *\/ */
/*     TEXT                **sval;         /\* string value vector ptr      *\/ */
/*     CODE                code; */
/*     COUNT               matches; */

/*     v = p_fvar(p, name);       /\* find name in V-block  *\/ */
/*     if (v == NULL) */
/*         BADNAME */
/*     if ((*v).v_type != type) */
/*         BADTYPE */

/*     if ((*v).v_valid == NULL) return (SUCCESS); */
/*     if ((*v).v_type == V_INTEGER) */
/*         { */
/*         ivalid = (struct I_VALID *) (*v).v_valid; */

/*         ival = (TAEINT *) value; */
/*         for (i=0; i < count; i++)       /\* for each value               *\/ */
/*             { */
/*             code = P_FAIL;                /\* assume no range found        *\/ */
/*             for (j=0; j < (*ivalid).count; j++) */
/*                 if (ival[i]  >=  (*ivalid).range[j].low   && */
/*                      ival[i]  <=  (*ivalid).range[j].high) */
/*                     { */
/*                     code = SUCCESS; */
/*                     break; */
/*                     } */
/*             if (code == P_FAIL) BADVALID  */
/*             } */
/*         } */
/*     else if ((*v).v_type == V_REAL) */
/*         { */
/*         rvalid = (struct R_VALID *) (*v).v_valid; */
/*         rval = (TAEFLOAT *) value; */
/*         for (i=0; i < count; i++)       /\* for each value               *\/ */
/*             { */
/*             code = P_FAIL;                /\* assume no range found        *\/ */

/*             for (j=0; j < (*rvalid).count; j++) */
/*                 if (rval[i]  >=  (*rvalid).range[j].low  && */
/*                     rval[i]  <=  (*rvalid).range[j].high) */
/*                     { */
/*                     code = SUCCESS; */
/*                     break; */
/*                     } */
/*             if (code == P_FAIL) BADVALID  */
/*             } */
/*         } */
/*     else if ((*v).v_type == V_STRING) */
/*         { */
/*         svalid = (struct S_VALID *) (*v).v_valid; */
/*         sval = (TEXT **) value; */
/*         for (i=0; i < count; i++)       /\* look for left substring      *\/ */
/*             { */
/*             matches = 0;                /\* number of matches            *\/ */
/*             for (j=0; j < (*svalid).count; j++) */
/*                 { */
/*                 if (s_equal(sval[i], (*svalid).slist[j].string)) */
/*                     { */

/*                     matches = 1; */
/*                     break; */
/*                     } */
/*                 if(s_lseq(sval[i], (*svalid).slist[j].string)) */
/*                     matches++; */
/*                 } */
/*             if (matches == 0 || matches > 1) */
/*                 BADVALID                /\* not found or ambiguous        *\/ */
/*             } */
/*         } */
/*     return (SUCCESS); */
/*     } */

/*
 *	 Vm_FreeValue.  Delete values.
 */

    UNSUPPORTED VOID Vm_FreeValue(

    TEXT	*pv,				/*in: pointer to values	*/
    FUNINT	type,				/*in: type of variable	*/
    FUNINT	count				/*in: current value cnt	*/
    )

    {
    FAST TEXT  **q;			/* ptr to vector of ptrs	*/
    FAST COUNT i;

    if (pv == NULL)
        return;				/* no values allocated 		*/
    if (type == V_STRING)
	{
	q = (TEXT **) pv;		/* cast to q format (from char*)*/
	for (i=0; i < count; i++)
	    if (q[i] != NULL)
		tae_free(q[i]);		/* delete string		*/
	}
    tae_free(pv);			/* delete value vector		*/
    return;
    }
