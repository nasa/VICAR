#include <stdlib.h>
#include <assert.h>

#include "ibisControlMapper.h"
#include "ibishelper.h"

#define MAXCONTROLS 50

/******************************************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getMapper(IBISStruct *ibis, int controlCol)
{
   IBIS_CONTROL_MAPPER *mapper;
   int i, j, ncontrols, found, counts[MAXCONTROLS];
   double controls[MAXCONTROLS];

   if(controlCol <= 0) return IBISCONTROL_getSingleMapper(ibis);
   --controlCol;

   ncontrols = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      found = 0;
      for(j = 0; j < ncontrols; j++)
      {
         if(IBISHELPER_getDouble(ibis, controlCol, i) == controls[j])
         {
            found = 1;
            break;
         }
      }
      if(!found)
      {
         controls[ncontrols] = IBISHELPER_getDouble(ibis, controlCol, i);
         counts[ncontrols++] = 1;
      }
      else
         counts[j]++;
   }

   mapper = (IBIS_CONTROL_MAPPER*)malloc(sizeof(IBIS_CONTROL_MAPPER));
   mapper->nControls = ncontrols;

   mapper->maps = (IBIS_CONTROL_MAP**)malloc(sizeof(IBIS_CONTROL_MAP*)*ncontrols);
   for(i = 0; i < ncontrols; i++)
      mapper->maps[i] = IBISCONTROL_getMap(ibis, controlCol, controls[i], counts[i]);

   return mapper;
}

/******************************************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getSingleMapper(IBISStruct *ibis)
{
   int i;
   IBIS_CONTROL_MAPPER *mapper;

   mapper = (IBIS_CONTROL_MAPPER*)malloc(sizeof(IBIS_CONTROL_MAPPER));
   mapper->nControls = 1;
   mapper->maps = (IBIS_CONTROL_MAP**)malloc(sizeof(IBIS_CONTROL_MAP*));
   mapper->maps[0] = (IBIS_CONTROL_MAP*)malloc(sizeof(IBIS_CONTROL_MAP));

   mapper->maps[0]->length = ibis->nr;
   mapper->maps[0]->toIbisIndices = (int*)malloc(sizeof(int)*mapper->maps[0]->length);

   for(i = 0; i < ibis->nr; i++)
      mapper->maps[0]->toIbisIndices[i] = i;

   return mapper;
}

/******************************************************/
IBIS_CONTROL_MAP* IBISCONTROL_getMap(IBISStruct *ibis, int controlCol, double control, int count)
{
   int i;
   IBIS_CONTROL_MAP *map;

   map = (IBIS_CONTROL_MAP*)malloc(sizeof(IBIS_CONTROL_MAP));
   map->controlID = control;
   map->toIbisIndices = (int*)calloc(count, sizeof(int));
   map->length = 0;

   for(i = 0; i < ibis->nr; i++)
      if(IBISHELPER_getDouble(ibis, controlCol, i) == control)
         map->toIbisIndices[(map->length)++] = i;

   assert(map->length == count);

   return map;
}

/******************************************************/
void IBISCONTROL_deleteMapper(IBIS_CONTROL_MAPPER** mapper)
{
   int i;

   for(i = 0; i < (*mapper)->nControls; i++)
      IBISCONTROL_deleteMap(&((*mapper)->maps[i]));

   free((*mapper)->maps);
   free(*mapper);
}

/******************************************************/
void IBISCONTROL_deleteMap(IBIS_CONTROL_MAP** map)
{
   free((*map)->toIbisIndices);
   free(*map);
}
