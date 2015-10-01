/**********************************************************************
    DEL_PROP_GRP

    Deletes Property Group  from image's label with unit number.

    unit          - unit number of file containing picture (input)  integer*4
    property_name - Name of the property group to be deleted
    instance      - Particular instance of the given property group
**********************************************************************/

#ifndef _DELPROPGRP_H
#define _DELPROPGRP_H

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _NO_PROTO
void del_prop_grp();
#else
void del_prop_grp(int unit, char* property_name, int instance);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _DELPROPGRP_H */

