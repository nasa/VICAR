#include "zvproto.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/**********************************************************************
    DEL_PROP_GRP

    Deletes Property Group  from image's label with unit number.

    unit          - unit number of file containing picture (input)  integer*4
    property_name - Name of the property group to be deleted
    instance      - Particular instance of the given property group
**********************************************************************/
void del_prop_grp(unit, property_name, instance)
     int unit;
     char* property_name;
     int instance;
{
    char buf[500];
    int status;
    int maxlen, nelem;
    char key[33] = "PROPERTY";
    char format[12];
    int element_to_delete = 1;

    /* set label pointer to this subset */
    status = zlinfo(unit,
		    "PROPERTY", "PROPERTY", 
		    format, &maxlen, &nelem,
		    "PROPERTY", property_name, 
		    "INSTANCE",instance,
		    NULL);
    do {
        if (strcmp(key, "PROPERTY") != 0) {
	    status = zldel(unit, 
			   "PROPERTY", key,
			   "PROPERTY", property_name,
			   "ELEMENT", element_to_delete,
			   NULL);

	    /*sprintf(buf,"Keyword %s deleted",key);
	    zvmessage(buf, "");
	    */
	}

	/* Increment to next keyword */
	status = zlninfo(unit,key,format,&maxlen,&nelem, NULL);

    } while ((strcmp(key,"TASK") != 0) && 
	     (strcmp(key,"PROPERTY") != 0) &&
	     (status == 1));

    /* Lastly delete the property label itself */
    status = zldel(unit, "PROPERTY", "PROPERTY", 
		   "PROPERTY", property_name,
		   "INSTANCE", instance,
		   NULL);

    sprintf(buf,"Deleted %s Property Group", property_name);
    zvmessage(buf, "");
}
