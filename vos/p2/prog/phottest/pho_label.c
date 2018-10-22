#include "pho.h"
#include "defines.h"

/*****************************************************************************
This routine is supposed to write information specific to a certain photometric
funtion into a property label called "PHOT". 
The application program has to include the follwing file:
pho.h

calling from C : status=phoLabelWrite(inp_unit, pho_obj);

******************************************************************************/

int phoLabelWrite(
	int inpunit, 
	PHO pho_obj)

{
  int status, e[phoMAX_PARAM_PER_FUNC + 1], i, j, num;
  float fphoPar;
  double phoPar[phoMAX_PARAM_PER_FUNC];
  char phoFuncName[phoMAX_FUNC_NAME_LENGTH+1];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 



/* get the photometric function name : */

  status = phoGetFunc( pho_obj, phoFuncName);
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetFunc failed ***","");
	    		zmabend("phoLabelWrite abend");
  }

  status=zladd(inpunit,"property","PHO_FUNC",
  phoFuncName,"format","string",
  "property","PHOT","ERR_ACT","SU","ERR_MESS",
  "property=PHOT keyword 'PHO_FUNC' was not added", NULL);

  e[0]=status;


/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
  if(status!=phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  }


/* get the parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, keylist, &num);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  }

  for ( j=0; j<num; j++ )  /* for all function parameters */
  {

    status = phoGetVal( pho_obj, keylist[j], (phoPar+j) );
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("phoLabelWrite abend");
    }

    fphoPar = (float )phoPar[j];

    status=zladd( inpunit, "property", &keylist[j],&fphoPar,
    "format","real", "property","PHOT", "ERR_ACT","SU",
    "ERR_MESS","property=PHOT parameter was not added", NULL);

    e[j+1]=status;
  }



  for (j=0; j<(num + 1); j++)
  {
    if (e[j]<1) 
    {
	status=e[j];
	zvmessage(" *** phoLabelWrite error: pho-label does not written " ,"");
    }
}

return status;
}




/*****************************************************************************
This routine is supposed to read information specific from a certain photometric
function from a property label called "PHOT". 
The application program has to include the following file:
pho.h

calling from C : status=phoLabelRead(inp_unit, pho_obj);

******************************************************************************/


int phoLabelRead(
	int inpunit, 
	PHO pho_obj)

{
  int status, e[phoMAX_PARAM_PER_FUNC + 1], i, j, k, x, y, nprop, nret, num;
  float fphoPar;
  double phoPar[phoMAX_PARAM_PER_FUNC];
  char property_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
  char phoFuncName[phoMAX_FUNC_NAME_LENGTH+1];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 

  nprop=MAX_PROPS;

/* 'zlpinfo' gets the information about all existing propertys */

  status=zlpinfo(inpunit,property_names,&nprop,"nret",&nret,
  "ulen",MAX_LABEL_KEY_SIZE+1, NULL);

  y=(-1);

  for (i=0; i<nprop; i++)
  {
     x=strcmp("PHOT",&property_names[i][0]);

/*  'zlget' returns all values of defined items of a 'PHOT' property, */
/*  if it exists */
 
    if (x==0) 
    { 

/* get the name of the photometric function */

	status=zlget(inpunit,"property","PHO_FUNC",
	phoFuncName,
	"format","string","property","PHOT","ERR_ACT","SU","ERR_MESS",
	"keyword 'PHO_FUNC' was not read", NULL);
	e[0]=status;


  	status = phoSetFunc( pho_obj, phoFuncName);
  	if ( status != phoSUCCESS ) 
  	{
	   zvmessage(" ","");
	   zvmessage("*** ERROR in phoLabelRead : phoSetFunc failed ***","");
	   zvmessage(" ","");
	   return status;
  	}

/* get the parameters names of the current photometric function : */

  	status = phoGetKeys( pho_obj, 0, &num); 
  	if(status!=phoSUCCESS)
 	{
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  	}

  	status = phoGetKeys( pho_obj, keylist, &num);
  	if(status!=1)
  	{
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  	}


/* get the parameters names of the current photometric function : */

 	for (j=0; j<num; j++) /* for all function parameters */
  	{
	    status=zlget( inpunit, "property",  keylist[j],&fphoPar,
	   "format","double",  "property","PHOT",  "ERR_ACT","SU",
	   "ERR_MESS","property=PHOT parameter  was not read", NULL);
	   e[j+1]=status;
	   phoPar[j] = (double )fphoPar;
    	   status = phoSetVal( pho_obj, keylist[j], phoPar[j]);
    	   if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
    	   {
			zvmessage(" ","");
	    		zvmessage("***phoLabelWrite error***","");
	    		zvmessage("*** phoSetVal  9 failed ***","");
	    		zmabend("phoLabelWrite abend");
    	   }


  	}

	for (k=0;k<phoMAX_PARAM_PER_FUNC + 1;k++)
	{
		if (e[k]<1) status=e[k];
	}
	y=0;
	return status;
    } /* if (x==0) */

  } /* for (i=0; i<nprop; i++) */


  if (y==(-1))
  {
    zvmessage("there is no property label 'PHOT'","");
    status=(-2);
    return status;
  }

}
