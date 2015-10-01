/******************************************************************************
*   xvplabel2(int unit, int mvlimit, int doubleflag, int incl_def, int status)
*   int status = zvplabel2(int unit, int mvlimit, int doubleflag, int incl_def)
*
*   parameter: unit In:
*              Specifies the unit number of the output file
*
*   parameter: mvlimit In:   (multi-value limit)
*              Puts a limit on the number of values of a 
*              multi-value parameter to be printed. If mvlimit==0 then
*              up to 32767 values for a multi-value parameter
*              will be printed. Otherwise mvlimit determines how
*              many values of a multi-value parameter will be printed.
*
*   parameter: doubleflag In: 
*              If doubleflag==0 then write all real
*              labels as float, if doubleflag==1 then write them as
*              double.
*
*   parameter: incl_def In:
*              if 0 (false), defaulted parameters are not included.  If non-0
*              (true), all parameters are included regardless of default status.
*
*   parameter: status Out:
*              Returns the status of the program
*
*
*   Prints all the user-specified parameters to the history of 
*   the output image file. The output image file must already
*   be opened by z/xvopen().
*
*   NOTE: If a label already exists in the output file with the
*   same name as the label to be added, then the value of
*   the existing label is NOT changed.
*   
*   Written:
*          ems   sept 22, 1999
*
*   Change Log:
*
*******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "strcasecmp.h"


/************************************************************************
*  Fortran-Callable Version, call the C-callable version zvplabel2()
*  REMEMBER: in Fortran all parameters are pass by reference
************************************************************************/

void FTN_NAME2(xvplabel2, XVPLABEL2) (	
   int *unit,		/* In: unit of the output file */
   int *mvlimit,	/* In: multi-value param limit */
   int *doubleflag,	/* in: write real labels as double */
   int *incl_def,	/* in: include default parameters */
   int *status		/* out: error status */
)

{     
   *status = zvplabel2(*unit,*mvlimit,*doubleflag,*incl_def);
   return;
}


/************************************************************************
*   C-Callable Version
*   Processes the symbol table and 
*   prints to the output file all the parameters that
*   are user-specified
*
*   NOTE: Any parameters in the symbol table starting
*   with $ or _ are considered system variables and not
*   added to the output file.						
************************************************************************/

int zvplabel2(
   int unit,			/* In: this is the output file */
   int mvlimit,			/* In: limit on multi-value params */
   int doubleflag,		/* In: write real labels as double */
   int incl_def			/* in: include default parameters */
)

{  
   struct SYMTAB *symtab;
   struct PARBLK *block;     
   struct VARIABLE *v;

   char *pname;
   int i;
   int limit;
   int status;	/* error status from zladd() */
   int error;	/* used if zladd() has an error */   
   char fmt[10];	/* format of the label */
      
   void *value_ptr = NULL;
   int int_num;
   float float_num;
   double double_num;   
           
   error=0;
   status=SUCCESS;
            
   if (doubleflag !=0 && doubleflag!=1) {
      zvmessage("x/zvplabel: Parameter: int doubleflag must be 0 or 1","");
      zabend();
   }
   if (mvlimit<0) {
      zvmessage("x/zvplabel: Parameter: int mvlimit must be >= 0","");
      zabend();
   }  
   if (mvlimit==0) {
      mvlimit=32767;
   }    
    
   block=(struct PARBLK*)&parb;   
   symtab=&((*block).symtab);

   for (v=(*symtab).link; v != NULL &&!error ; v=(*v).v_link) {                

      if ( incl_def || (*v).v_default==0 ) {
      
         limit = min( mvlimit,(*v).v_count );
         pname = (*v).v_name;
         
         /* system labels start with $ or _ */
         if ( pname[0]!='$' && pname[0]!='_') {
         
            for (i = 0;  i < limit ; i++) {
            
               switch ((*v).v_type) {	/* add value according to type */
	    
                  case V_INTEGER: 
                                                    	               
                     strcpy(fmt,"int");
                     int_num=IVAL(*v,i);
                     value_ptr=&int_num;                 
                     break;

                  case V_REAL:                                 
               
                     if (doubleflag) {
                        strcpy(fmt,"doub");
                        double_num=RVAL(*v,i);
                        value_ptr=&double_num;
                     }
                     else {
                        strcpy(fmt,"real");                     
                        float_num=RVAL(*v,i);
                        value_ptr=&float_num;                                                   
                     }
                     break;
	
                  case V_STRING:
                   
                     strcpy(fmt,"string");
                     value_ptr=(char*)SVAL(*v,i);
                     break;           
               }
            
               if (i==0) {
                  status = zladd(unit,"history",pname,value_ptr,
				 "format",fmt,"err_act","", NULL);
                  if (status!=SUCCESS) {
                     if (status==DUPLICATE_KEY) {
                        status=SUCCESS;
                        break;                     
                     }
                     else { 
                        /* re-issue zladd to allow it to handle any  */
                        /* non-DUPLICATE_KEY errors */              
                        status = zladd(unit,"history",pname,value_ptr,
				       "format",fmt, NULL);
                        if (status!=SUCCESS) {
                           error=1;
                           break;
                        }
                     }
                  }
               }
               else {
                  status = zladd(unit,"history",pname,value_ptr,
				 "format",fmt,"mode","insert", NULL);
                  if (status!=SUCCESS) {
                     error=1;
                     break;
                  }
               }
                                             
            } 	/* for i */   
         }	/* if ( pname[0]!= */                
      } 	/* if default */             
   } 	/* for v */            
   return status;
}
/******************************************************************************
*   xvplabel(int unit, int mvlimit, int doubleflag, int status)
*   int status = zvplabel(int unit, int mvlimit, int doubleflag)
*
*   See xvplabel2.  These simply call that routine with incl_def==false.
*******************************************************************************/

void FTN_NAME2(xvplabel, XVPLABEL) (
   int *unit,		/* In: unit of the output file */
   int *mvlimit,	/* In: multi-value param limit */
   int *doubleflag,	/* in: write real labels as double */
   int *status		/* out: error status */
)

{     
   *status = zvplabel2(*unit,*mvlimit,*doubleflag, 0);
   return;
}

int zvplabel(
   int unit,		/* In: this is the output file */
   int mvlimit,		/* In: limit on multi-value params */
   int doubleflag	/* In: write real labels as double */
)

{  
    return zvplabel2(unit, mvlimit, doubleflag, 0);
}

