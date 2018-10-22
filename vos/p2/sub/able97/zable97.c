/**********************************************************************
    Revision History
     1-97  SP   Original version of C bridge.
     2-99  JRY  Added to able97_typ for CASSINI-ISS2 labels
    10-99  TLT  Added imagetime and compparam
                Renamed lab_prepdur to lab_prepcycle
		Renamed lab_readoutdur to lab_readoutcycle
		Added lab_shuttertype
     5-00  AYS  Modified calls to sfor2c to output to the c string
     8-00  AYS  Updated to accomodate new vicar property labels.
     5-01  VRH  Not to be called by Fortran, removed Fortran tags
                Moved c string creation to xlabel97
     4-03  VRH  Updated to accomodate tour labels.
**********************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"
/*---------------------------------------------------------------------------*/
/* c-callable version                                                        */
/*---------------------------------------------------------------------------*/
void zable97( ind, unit, lab)

      int  *ind, unit;
      able97_typ *lab;
{

FTN_NAME2(xable97, XABLE97) ( ind, &unit, &lab->sclk, 
             &lab->scety,     &lab->scetd,      &lab->sceth, 
             &lab->scetm,     &lab->scets,      &lab->scetms,
             &lab->scety2,     &lab->scetd2,      &lab->sceth2, 
             &lab->scetm2,     &lab->scets2,      &lab->scetms2,
             &lab->scety3,     &lab->scetd3,      &lab->sceth3, 
             &lab->scetm3,     &lab->scets3,      &lab->scetms3,
             &lab->scety4,     &lab->scetd4,      &lab->sceth4, 
             &lab->scetm4,     &lab->scets4,      &lab->scetms4,
             &lab->scety5,     &lab->scetd5,      &lab->sceth5, 
             &lab->scetm5,     &lab->scets5,      &lab->scetms5,
             &lab->sclkstart, &lab->sclkstartsub,
             &lab->sclkstop, &lab->sclkstopsub, &lab->sccp, &lab->cld,
             &lab->expos,     &lab->ccdtemp,    &lab->opttemp, &lab->flttemp, 
             &lab->blocks,    &lab->algorithm,  &lab->btype,   &lab->qfactor, 
             &lab->comprat,   &lab->missing, 
             &lab->radiance,  &lab->prepcycle,   &lab->readoutcycle,
             &lab->bias,     &lab->extpixval, &lab->sensortemp,
             &lab->datarate, &lab->compparam, &lab->comdseqnum, 
             &lab->elbias, &lab->pcvi,
             &lab->exppackets, &lab->ordernum, &lab->recpackets, &lab->seqnum,
             &lab->validmax, &lab->validmaxfw, &lab->expmax, &lab->expmaxfw,
             &lab->compratepred, &lab->comprateact, &lab->rearopttemp,
             lab->labtype,    lab->phase,     lab->camera, 
             lab->swvers,     lab->mode,
             lab->gain,       lab->comprsn,   lab->convrsn,  lab->ltfld,
             lab->antiblm,    lab->callamp,   lab->target,   lab->observid,
             lab->source,     lab->shuttermode,
	     lab->dlyreadout,lab->imagetime,
	     lab->filter1, lab->filter2,
             lab->mispacflg, lab->shutid,
             lab->ertstart, lab->ertstop, lab->fltsoftversid, 
             lab->starttime, lab->dsid, lab->ihn, lab->in, 
             lab->note, lab->pctime, lab->prid,
             lab->pvt, lab->sid, lab->cmdname, lab->imgobstype, 
             lab->seqtitle,
             lab->description, lab->midtime, lab->methoddesc, lab->targetdesc,
             lab->targetlist, lab->telemid);

}


