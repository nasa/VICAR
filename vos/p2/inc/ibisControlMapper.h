#ifndef IBISCONTROLMAPPER
#define IBISCONTROLMAPPER

#include "ibishelper.h"

/***********************************/
typedef struct{
   int controlID;
   int length;
   int *toIbisIndices;
}IBIS_CONTROL_MAP;

/***********************************/
typedef struct{
   IBIS_CONTROL_MAP **maps;
   int nControls;
}IBIS_CONTROL_MAPPER;

/***********************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getMapper(IBISStruct *ibis, int controlCol);

/***********************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getSingleMapper(IBISStruct *ibis);

/***********************************/
IBIS_CONTROL_MAP* getIbisControlMap(IBISStruct *ibis, int controlCol, double control);

/******************************************************/
IBIS_CONTROL_MAP* IBISCONTROL_getMap(IBISStruct *ibis, int controlCol, double control, int count);

/******************************************************/
void IBISCONTROL_deleteMapper(IBIS_CONTROL_MAPPER** mapper);

/******************************************************/
void IBISCONTROL_deleteMap(IBIS_CONTROL_MAP** map);

#endif

