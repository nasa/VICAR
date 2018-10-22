#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "cloud_masks.h"

/******************************************************************************/
CLOUD_MASKS* get_CLOUD_MASKS(int nl, int ns)
{
   int i;
   CLOUD_MASKS* masks;

   masks = (CLOUD_MASKS*)malloc(sizeof(CLOUD_MASKS));

   masks->vars     = (ALGORITHM_VARS*)malloc(sizeof(ALGORITHM_VARS));
   masks->vars->b3_noncloud = 0;
   masks->vars->b6_noncloud = 0;
   masks->vars->snowCnt = 0;
   masks->vars->ndsi_noncloud = 0;
   masks->vars->pass2_revisit = 0;
   masks->vars->b5_noncloud = 0;
   masks->vars->b42ratio_tally = 0;
   masks->vars->b45ratio_tally = 0;
   masks->vars->warmcloud = 0;
   masks->vars->coldcloud = 0;
   masks->vars->scenePixels = 0;
   masks->vars->enterdesert = 0;
   masks->vars->exitdesert = 0;
   masks->vars->snowpresent = 0;

   masks->vars->nl = nl;
   masks->vars->ns = ns;

   masks->CMcloud       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMsnow        = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMdesert      = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMcloud_warm  = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMcloud_cold  = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->ice           = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter_cirrus = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->ambig         = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->valid         = (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   masks->filter1       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter2       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter3       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter4       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter5       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter6       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter7       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter8       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter9       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter10      = (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   masks->tambig_warm_mask =  (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->tambig_cold_mask =  (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   for(i = 0; i < nl; i++)
   {
      masks->CMcloud[i]       = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMsnow[i]        = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMdesert[i]      = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMcloud_warm[i]  = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMcloud_cold[i]  = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->ice[i]           = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->filter_cirrus[i] = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->ambig[i]         = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->valid[i]         = (unsigned char*)calloc(sizeof(unsigned char), ns);

      masks->filter1[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter2[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter3[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter4[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter5[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter6[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter7[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter8[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter9[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter10[i]      = (unsigned char*)malloc(sizeof(unsigned char)*ns);

      masks->tambig_warm_mask[i] = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->tambig_cold_mask[i] = (unsigned char*)malloc(sizeof(unsigned char)*ns);
   }

   return masks;
}

/******************************************************************************/
void delete_CLOUD_MASKS(CLOUD_MASKS **masks)
{
   int i;

   for(i = 0; i < (*masks)->vars->nl; i++)
   {
      free((*masks)->CMcloud[i]);
      free((*masks)->CMsnow[i]);
      free((*masks)->CMdesert[i]);
      free((*masks)->CMcloud_warm[i]);
      free((*masks)->CMcloud_cold[i]);
      free((*masks)->ice[i]);
      free((*masks)->filter_cirrus[i]);
      free((*masks)->ambig[i]);
      free((*masks)->valid[i]);

      free((*masks)->filter1[i]);
      free((*masks)->filter2[i]);
      free((*masks)->filter3[i]);
      free((*masks)->filter4[i]);
      free((*masks)->filter5[i]);
      free((*masks)->filter6[i]);
      free((*masks)->filter7[i]);
      free((*masks)->filter8[i]);
      free((*masks)->filter9[i]);
      free((*masks)->filter10[i]);

      free((*masks)->tambig_warm_mask[i]);
      free((*masks)->tambig_cold_mask[i]);
   }

   free((*masks)->CMcloud);
   free((*masks)->CMsnow);
   free((*masks)->CMdesert);
   free((*masks)->CMcloud_warm);
   free((*masks)->CMcloud_cold);
   free((*masks)->ice);
   free((*masks)->filter_cirrus);
   free((*masks)->ambig);
   free((*masks)->valid);

   free((*masks)->filter1);
   free((*masks)->filter2);
   free((*masks)->filter3);
   free((*masks)->filter4);
   free((*masks)->filter5);
   free((*masks)->filter6);
   free((*masks)->filter7);
   free((*masks)->filter8);
   free((*masks)->filter9);
   free((*masks)->filter10);

   free((*masks)->tambig_warm_mask);
   free((*masks)->tambig_cold_mask);

   free((*masks)->band_files);

   free(*masks);
}

/******************************************************************************/
void setBandImages(CLOUD_MASKS **cm, VICAR_IMAGE *green, VICAR_IMAGE *red, VICAR_IMAGE *nir,
                   VICAR_IMAGE *swir1, VICAR_IMAGE *tir1)
{
   (*cm)->band_files = (BAND_FILES*)malloc(sizeof(BAND_FILES));

   (*cm)->band_files->green_band = green;
   (*cm)->band_files->red_band   = red;
   (*cm)->band_files->nir_band   = nir;
   (*cm)->band_files->swir1_band  = swir1;
   (*cm)->band_files->tir_band1  = tir1;
}

/******************************************************************************/
void setWorkspaceImages(CLOUD_MASKS **cm, VICAR_IMAGE *ndsi, VICAR_IMAGE *bTempComp,
                        VICAR_IMAGE *gv, VICAR_IMAGE *sv, VICAR_IMAGE *rs)
{
   (*cm)->ws_files = (WORKSPACE_FILES*)malloc(sizeof(WORKSPACE_FILES));

   (*cm)->ws_files->ndsi_file      = ndsi;
   (*cm)->ws_files->bTempComp_file = bTempComp;
   (*cm)->ws_files->gv_file        = gv;
   (*cm)->ws_files->sv_file        = sv;
   (*cm)->ws_files->rs_file        = rs;
}


/******************************************************************************/
void init_CM_WORKSPACE(CLOUD_MASKS **cm)
{
   int ns;

   ns = (*cm)->vars->ns;

   (*cm)->ws = (CM_WORKSPACE*)malloc(sizeof(CM_WORKSPACE));

   if((*cm)->ws_files->ndsi_file == NULL)
      (*cm)->ws->ndsi = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->ndsi = (*cm)->ws_files->ndsi_file->buffer;

   if((*cm)->ws_files->bTempComp_file == NULL)
      (*cm)->ws->bTempComp = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->bTempComp = (*cm)->ws_files->bTempComp_file->buffer;

   if((*cm)->ws_files->gv_file == NULL)
      (*cm)->ws->gv = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->gv = (*cm)->ws_files->gv_file->buffer;

   if((*cm)->ws_files->sv_file == NULL)
      (*cm)->ws->sv = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->sv = (*cm)->ws_files->sv_file->buffer;

   if((*cm)->ws_files->rs_file == NULL)
      (*cm)->ws->rs = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->rs = (*cm)->ws_files->rs_file->buffer;
}

/******************************************************************************/
void delete_CM_WORKSPACE(CLOUD_MASKS **cm)
{
   if((*cm)->ws_files->ndsi_file == NULL) free((*cm)->ws->ndsi);
   if((*cm)->ws_files->bTempComp_file == NULL) free((*cm)->ws->bTempComp);
   if((*cm)->ws_files->gv_file == NULL) free((*cm)->ws->gv);
   if((*cm)->ws_files->sv_file == NULL) free((*cm)->ws->sv);
   if((*cm)->ws_files->rs_file == NULL) free((*cm)->ws->rs);

   free((*cm)->ws);
   free((*cm)->ws_files);
}

/******************************************************************************/
void readAllBands(CLOUD_MASKS *cm, int line)
{
  if(cm->band_files->green_band != NULL) readVicarImageLine(cm->band_files->green_band, line);
  if(cm->band_files->red_band   != NULL) readVicarImageLine(cm->band_files->red_band, line);
  if(cm->band_files->nir_band   != NULL) readVicarImageLine(cm->band_files->nir_band, line);
  if(cm->band_files->swir1_band != NULL) readVicarImageLine(cm->band_files->swir1_band, line);
  if(cm->band_files->tir_band1  != NULL) readVicarImageLine(cm->band_files->tir_band1, line);
}

/******************************************************************************/
void filter1(double *red_ref, unsigned char *finalmask, unsigned char* ambMask,
             unsigned char *valid, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
      if(valid[i] && red_ref[i] > 0.2) finalmask[i] = CM_CLOUD;
      else
      {
         finalmask[i] = CM_NONCLOUD;

         if(valid[i] && red_ref[i] > 0.07 && red_ref[i] < 0.5)
            ambMask[i] = CM_AMBIG;
      }
}

/******************************************************************************/
void filter2(double* ndsi, double *near_ref, unsigned char *finalmask,
             unsigned char *snow, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(ndsi[i] < 0.7) finalmask[i] = CM_CLOUD;
         else
         {
            finalmask[i] = CM_NONCLOUD;

            if(ndsi[i] > 0.7 && near_ref[i] > 0.25) snow[i] = CM_SNOW;
         }
      }
   }
}

/******************************************************************************/
void filter3(double *b_temp, unsigned char *finalmask, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(b_temp[i] < 300) finalmask[i] = CM_CLOUD;
         else finalmask[i] = CM_NONCLOUD;
      }
   }
}

/******************************************************************************/
void filter4(double *bTempcomp, double *shortwave_ref, double *b_temp, unsigned char *finalmask,
             unsigned char *ambig, unsigned char *ice, int ns, double thresh)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(bTempcomp[i] < thresh) finalmask[i] = CM_CLOUD;
         else if(shortwave_ref[i] > 0.2 && b_temp[i] < 300)
         {
            finalmask[i] = CM_NONCLOUD;
            ambig[i] = CM_AMBIG;
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            ice[i] = CM_ICE;
         }
      }
   }
}

/******************************************************************************/
void filter5(double *gv, unsigned char *finalmask, unsigned char *ambig, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
        if(gv[i] < 2.0)
           finalmask[i] = CM_CLOUD;
        else
        {
           finalmask[i] = CM_NONCLOUD;
           ambig[i] = CM_AMBIG;
        }
      }
   }
}

/******************************************************************************/
void filter6(double *sv, unsigned char *finalmask, unsigned char *ambig,
             long long int *enterdesert, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(sv[i] < 2.0)
         {
            finalmask[i] = CM_CLOUD;
            ++(*enterdesert);
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            ambig[i] = CM_AMBIG;
         }
      }
   }
}

/******************************************************************************/
void filter7(double *rs, unsigned char *finalmask, unsigned char *CMdesert,
             unsigned char *ambig, long long int *exitdesert, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(rs[i] > 0.8)
         {
            finalmask[i] = CM_CLOUD;
            ++(*exitdesert);
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            CMdesert[i] = CM_DESERT;
            ambig[i] = CM_AMBIG;
         }
      }
   }
}

/******************************************************************************/
void filter8(double *bTempComp, unsigned char *finalmask, unsigned char *CMcloud_cold,
             unsigned char *CMcloud_warm, int ns, double thresh,
             long long int *coldcloud, long long int *warmcloud)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(bTempComp[i] < thresh)
         {
            CMcloud_cold[i] = CM_COLD_CLOUD;
            ++(*coldcloud);
         }
         else
         {
            CMcloud_warm[i] = CM_WARM_CLOUD;
            ++(*warmcloud);
         }
      }
   }
}

/******************************************************************************/
void getNDSI(double *green_ref, double *shortwave_ref, double *ndsi, int ns)
{    
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(green_ref[i]) > 10E-10 && fabs(shortwave_ref[i]) > 10E-10)
         ndsi[i] = (green_ref[i] - shortwave_ref[i])/(green_ref[i] + shortwave_ref[i]);
      else
         ndsi[i] = 0.;
   }
}

/******************************************************************************/
void getBTempComp(double *b_temp, double *shortwave_ref, double *bTempComp, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
      bTempComp[i] = (1 - shortwave_ref[i])*b_temp[i];
}

/******************************************************************************/
void getGV(double *near_ref, double *red_ref, double *gv, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(red_ref[i]) < 10E-10) gv[i] = 0.;
      else gv[i] = near_ref[i]/red_ref[i];
   }
}

/******************************************************************************/
void getSV(double *near_ref, double *green_ref, double *sv, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(green_ref[i]) < 10E-10) sv[i] = 0.;
      else sv[i] = near_ref[i]/green_ref[i];
   }
}

/******************************************************************************/
void getRS(double *near_ref, double *shortwave_ref, double *rs, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(shortwave_ref[i]) < 10E-10) rs[i] = 0.;
      else rs[i] = near_ref[i]/shortwave_ref[i];
   }
}

/******************************************************************************/
void convertToAtSatTemp(double *thermal_rad, double *atSatTemp, int ns, int *scenePixels)
{
   int i;

   for(i = 0; i < ns; i++)
      atSatTemp[i] = 1282.71/log((666.09/thermal_rad[i])+1);
}

/******************************************************************************/
void CM_getValidMask(CLOUD_MASKS *cm)
{
   int i, j;
   char **tmpvalid;

   cm->vars->scenePixels = 0;
   tmpvalid = (char **)malloc(sizeof(char*)*cm->vars->nl);

   for(i = 0; i < cm->vars->nl; i++)
   {
      tmpvalid[i] = (char *)malloc(sizeof(char)*cm->vars->ns);
      readAllBands(cm, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         if(CM_GREEN_REF[j] > 10E-10 &&
            CM_RED_REF[j] > 10E-10 &&
            CM_NIR_REF[j] > 10E-10 &&
            CM_SWIR1_REF[j] > 10E-10 &&
            CM_TIR1_REF[j] > 10E-10)
         {
            tmpvalid[i][j] = CM_VALID;
         }
         else
            tmpvalid[i][j] = CM_NONVALID;
      }
   }

   for(i = 0; i < cm->vars->nl; i++)
   {
      for(j = 0; j < cm->vars->ns; j++)
      {
         if(i < 2 || i >= cm->vars->nl-2) cm->valid[i][j] = CM_NONVALID;
         else if(j < 2 || j >= cm->vars->ns-2) cm->valid[i][j] = CM_NONVALID;
         else if(tmpvalid[i-2][j-1] == CM_NONVALID ||
                 tmpvalid[i-2][j] == CM_NONVALID ||
                 tmpvalid[i-2][j+1] == CM_NONVALID ||
                 tmpvalid[i-1][j-1] == CM_NONVALID ||
                 tmpvalid[i-1][j] == CM_NONVALID ||
                 tmpvalid[i-1][j+1] == CM_NONVALID ||
                 tmpvalid[i][j-2] == CM_NONVALID ||
                 tmpvalid[i][j-1] == CM_NONVALID ||
                 tmpvalid[i][j] == CM_NONVALID ||
                 tmpvalid[i][j+1] == CM_NONVALID ||
                 tmpvalid[i][j+2] == CM_NONVALID ||
                 tmpvalid[i+1][j-1] == CM_NONVALID ||
                 tmpvalid[i+1][j] == CM_NONVALID ||
                 tmpvalid[i+1][j+1] == CM_NONVALID ||
                 tmpvalid[i+2][j-1] == CM_NONVALID ||
                 tmpvalid[i+2][j] == CM_NONVALID ||
                 tmpvalid[i+2][j+1] == CM_NONVALID)
            cm->valid[i][j] = CM_NONVALID;
         else
         {
            cm->valid[i][j] = CM_VALID;
            cm->vars->scenePixels++;
         }
      }
   }

   for(i = 0; i < cm->vars->nl; i++) free(tmpvalid[i]);
   free(tmpvalid);
}

/******************************************************************************/
void getSnowPercentage(CLOUD_MASKS *cm)
{
   int i, j;

   cm->vars->snowCnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->green_band, i);
      readVicarImageLine(cm->band_files->swir1_band, i);
      readVicarImageLine(cm->band_files->nir_band, i);

      getNDSI(CM_GREEN_REF, CM_SWIR1_REF, cm->ws->ndsi, cm->vars->ns);
      if(cm->ws_files->ndsi_file != NULL) writeVicarImageLine(cm->ws_files->ndsi_file, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         if((cm->ws->ndsi)[j] > 0.4 && CM_NIR_REF[j] > 0.25)
         {
            cm->CMsnow[i][j] = 1;
            ++(cm->vars->snowCnt);
         }
      }
   }

   printf("snow count: %lld\n", cm->vars->snowCnt);
}

/******************************************************************************/
double interp1(double *Tcloud, int len_Tcloud, double x)
{
   int x1, x2;
   double y1, y2;

   x1 = (int)x;
   x2 = (int)(x + 1);
   y1 = Tcloud[x1];
   y2 = Tcloud[x2];

   return y1 + ((x - x1)*(y2 - y1))/(double)(x2 - x1);
}

/******************************************************************************/
double interpThresh(double *xarray, double *yarray, double x)
{
   double x1, x2;
   double y1, y2;

   x1 = xarray[0];
   x2 = xarray[1];
   y1 = yarray[0];
   y2 = yarray[1];

   return y1 + ((x - x1)*(y2 - y1))/(x2-x1);

}

/******************************************************************************/
void getThresholds(CLOUD_MASKS *cm)
{
   double snowPer;
   double xarray[2] = {0., 5.};
   double yarray4[2] = {250., 220.};
   double yarray8[2] = {235., 205.};

   snowPer = cm->vars->snowCnt/(double)cm->vars->scenePixels*100;

   if(snowPer > 5.)
   {
      cm->vars->filter4Thresh = 220;
      cm->vars->filter8Thresh = 205;
   }
   else
   {
      cm->vars->filter4Thresh = interpThresh(xarray, yarray4, snowPer);
      cm->vars->filter8Thresh = interpThresh(xarray, yarray8, snowPer);
   }
}

/******************************************************************************/
void doPass1(CLOUD_MASKS *cm)
{
   int i, nl, ns;

   nl = cm->vars->nl;
   ns = cm->vars->ns;

   CM_getValidMask(cm);
   getSnowPercentage(cm);
   getThresholds(cm);
   for(i = 0; i < cm->vars->nl; i++)
   {
      readAllBands(cm, i);

      filter1(CM_RED_REF, cm->CMcloud[i], cm->ambig[i], cm->valid[i], ns);
      memcpy(cm->filter1[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getNDSI(CM_GREEN_REF, CM_SWIR1_REF, cm->ws->ndsi, ns);
      filter2(cm->ws->ndsi, CM_NIR_REF, cm->CMcloud[i], cm->CMsnow[i], ns);
      memcpy(cm->filter2[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      filter3(CM_TIR1_REF, cm->CMcloud[i], ns);
      memcpy(cm->filter3[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getBTempComp(CM_TIR1_REF, CM_SWIR1_REF, cm->ws->bTempComp, ns);
      if(cm->ws_files->bTempComp_file != NULL) writeVicarImageLine(cm->ws_files->bTempComp_file, i);
      filter4(cm->ws->bTempComp, CM_SWIR1_REF, CM_TIR1_REF, cm->CMcloud[i], cm->ambig[i], cm->ice[i], ns, cm->vars->filter4Thresh);
      memcpy(cm->filter4[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getGV(CM_NIR_REF, CM_RED_REF, cm->ws->gv, ns);
      if(cm->ws_files->gv_file != NULL) writeVicarImageLine(cm->ws_files->gv_file, i);
      filter5(cm->ws->gv, cm->CMcloud[i], cm->ambig[i], ns);
      memcpy(cm->filter5[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getSV(CM_NIR_REF, CM_GREEN_REF, cm->ws->sv, ns);
      if(cm->ws_files->sv_file != NULL) writeVicarImageLine(cm->ws_files->sv_file, i);
      filter6(cm->ws->sv, cm->CMcloud[i], cm->ambig[i], &(cm->vars->enterdesert), ns);
      memcpy(cm->filter6[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getRS(CM_NIR_REF, CM_SWIR1_REF, cm->ws->rs, ns);
      if(cm->ws_files->rs_file != NULL) writeVicarImageLine(cm->ws_files->rs_file, i);
      filter7(cm->ws->rs, cm->CMcloud[i], cm->CMdesert[i], cm->ambig[i], &(cm->vars->exitdesert), ns);
      memcpy(cm->filter7[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      filter8(cm->ws->bTempComp, cm->CMcloud[i], cm->CMcloud_cold[i], cm->CMcloud_warm[i], ns, cm->vars->filter8Thresh, &(cm->vars->coldcloud), &(cm->vars->warmcloud));
      memcpy(cm->filter8[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);
   }
}

/******************************************************************************/
long long int countMaskPixels(CLOUD_MASKS *cm, unsigned char **mask)
{
   long long int cnt;
   int i, j;

   cnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
       for(j = 0; j < cm->vars->ns; j++)
          if(mask[i][j]) ++cnt;

   return cnt;
}

/***********************************************************/
/* helper function for the qsort function                  */
/***********************************************************/
int compare_doubles(const double *x1, const double *x2)
{
   int result;

   if(*x1 <= *x2) result = -1;
   else result = 1;

   return result;
}

/******************************************************************************/
double getAvg(double *buf, int len_buf)
{
   int i;
   double sum;

   sum = 0.0;
   for(i = 0; i < len_buf; i++)
      sum += buf[i];

   return sum/len_buf;
}

/******************************************************************************/
void getTcloudStats(double *Tcloud, double *sd, double *avg, int len_Tcloud)
{
   int i;

   *avg = getAvg(Tcloud, len_Tcloud);

   *sd = 0.0;
   for(i = 0; i < len_Tcloud; i++)
      *sd += pow(Tcloud[i] - *avg, 2);

   *sd /= len_Tcloud - 1;
   *sd = pow(*sd, 0.5);
}

/******************************************************************************/
double getSkew(CLOUD_MASKS* cm, double *Tcloud, double stdev, double avg, int len_Tcloud)
{
   int i;
   double *sk;

   sk = (double*)malloc(sizeof(double)*len_Tcloud);
   for(i = 0; i < len_Tcloud; i++)
   {
      sk[i] = pow((Tcloud[i] - avg)/stdev, 3);
      //      printf("Tcloud: %f sk: %f\n", Tcloud[i], sk[i]);
   }

   return getAvg(sk, len_Tcloud);
}

/******************************************************************************/
void zeroCloud(CLOUD_MASKS *cm)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         cm->CMcloud[i][j] = 0;
}

/******************************************************************************/
void getTempByMask(CLOUD_MASKS *cm, unsigned char** mask, double *temp, long long int *len)
{
   int i, j;
   long long int cnt;

   cnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->tir_band1, i);

      for(j = 0; j < cm->vars->ns; j++)
         if(mask[i][j]) temp[cnt++] = CM_TIR1_REF[j];
   }

   *len = cnt;
}

/******************************************************************************/
void setCMcloudMask(CLOUD_MASKS *cm, unsigned char **mask)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         cm->CMcloud[i][j] = mask[i][j];
}

/******************************************************************************/
void getTambigWarmColdMask(CLOUD_MASKS *cm, unsigned char **warm, long long int *warm_len,
                           unsigned char **cold, long long int *cold_len, double pmax, double pmin)
{
   int i, j;

   *warm_len = *cold_len = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->tir_band1, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         warm[i][j] = cold[i][j] = 0;
         if(!cm->ambig[i][j]) continue;
         //         printf("tir ref: %lf pmin: %lf pmax: %lf\n", CM_TIR1_REF[j], pmin, pmax);
         if(CM_TIR1_REF[j] <= pmax && CM_TIR1_REF[j] >= pmin)
         {
            warm[i][j] = 1;
            (*warm_len)++;
            //            printf("---> WARM\n");
         }
         else if(CM_TIR1_REF[j] < pmin)
         {
            cold[i][j] = 1;
            (*cold_len)++;
            //            printf("---> COLD\n");
         }
      }
   }
}

/******************************************************************************/
void getTambigStats(CLOUD_MASKS *cm, unsigned char **mask, double *percent, double *avg, long long int len)
{
   int i;
   double *temp;

   if(len == 0)
   {
      *percent = 0;
      *avg = 0;

      return;
   }

   temp = (double*)malloc(sizeof(double)*len);
   getTempByMask(cm, mask, temp, &len);

   *percent = len/(double)cm->vars->scenePixels*100;

   *avg = 0.;
   for(i = 0; i < len; i++) *avg += temp[i];
   *avg /= len;

   free(temp);
}

/******************************************************************************/
void addCMcloudMask(CLOUD_MASKS *cm, unsigned char **mask)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         if(cm->CMcloud[i][j] == 0 && mask[i][j] == 1) cm->CMcloud[i][j] = mask[i][j];
}

/******************************************************************************/
void doPass2(CLOUD_MASKS *cm)
{
   long long int pass1Cnt, tcloud_len, tambig_len, tambig_warm_len, tambig_cold_len;
   double desratio, snowper, tcloud_avg, *tcloud, tcloud_skew, tcloud_stdev;
   double *tambig, tambig_warm_percent, tambig_cold_percent;
   double tambig_warm_avg, tambig_cold_avg;
   double tmax, tmin, pmax, pmin, skewfactor, tcloud_test, ptest, shift;

   desratio = 100 - (cm->vars->exitdesert/(double)cm->vars->enterdesert)*100;
   snowper = cm->vars->snowCnt/(double)cm->vars->scenePixels*100;
   pass1Cnt = countMaskPixels(cm, cm->CMcloud);

   // allocate tcloud array: size can't be bigger than pass1 clouds
   tcloud = (double *)malloc(sizeof(double)*pass1Cnt);

   printf("desratio: %lf snowper: %lf\n", desratio, snowper);
   if(pass1Cnt/(double)cm->vars->scenePixels*100 < 0.02)
   {
      printf("pass1 < 0.02 percent clouds -- a and b\n");
      zeroCloud(cm);
      return;
   }

   if(desratio > 50. || snowper > 1.)
   {
      if(cm->vars->coldcloud <= 1)
      {
         printf("no cold clouds\n");
         zeroCloud(cm);
         return;
      }

      printf("only cold clouds are clouds, reanalyzing warm clouds\n");
      getTempByMask(cm, cm->CMcloud_cold, tcloud, &tcloud_len);
   }
   else
   {
      printf("both warm and cold clouds are clouds -- a\n");
      getTempByMask(cm, cm->CMcloud, tcloud, &tcloud_len);
   }

   getTcloudStats(tcloud, &tcloud_stdev, &tcloud_avg, tcloud_len);
   if(tcloud_avg >= 295 || desratio >= 50)
   {
      if(countMaskPixels(cm, cm->CMcloud_cold) == 0)
      {
         printf("no cold clouds\n");
         zeroCloud(cm);
         return;
      }

      getTempByMask(cm, cm->CMcloud_cold, tcloud, &tcloud_len);
      getTcloudStats(tcloud, &tcloud_stdev, &tcloud_avg, tcloud_len);

      if(tcloud_avg < 295.)
      {
         printf("final clouds are cold clouds\n");
         setCMcloudMask(cm, cm->CMcloud_cold);
         return;
      }

      printf("tclouds_avg > 295\n");
      zeroCloud(cm);
      return;
   }

   tcloud_skew = getSkew(cm, tcloud, tcloud_stdev, tcloud_avg, tcloud_len);
   printf("tcloud_skew: %lf\n", tcloud_skew);

   qsort(tcloud, tcloud_len, sizeof(double), (int (*)(const void *, const void *)) compare_doubles);
   tmax = tcloud_len*0.975;
   tmin = tcloud_len*0.835;
   pmax = interp1(tcloud, tcloud_len, tmax);
   pmin = interp1(tcloud, tcloud_len, tmin);
   printf("pmin: %lf pmax: %lf\n", pmin, pmax);

   if(tcloud_skew > 0)
   {
      printf("positive skew\n");
      if(tcloud_skew > 1) skewfactor = 1;
      else skewfactor = tcloud_skew;

      shift = skewfactor*tcloud_stdev;
      pmax += shift;
      pmin += shift;

      tcloud_test = tcloud_len*0.9875;
      ptest = interp1(tcloud, tcloud_len, tcloud_test);

      printf("1 - skew: %lf skewfact: %lf pmax: %lf pmin: %lf ptest: %lf\n",
             tcloud_skew, skewfactor, pmax, pmin, ptest);
      if(pmax > ptest)
      {
         pmax = ptest;
         pmin -= pmax-ptest;
      }
      printf("2 - ptest: %lf\n", ptest);
   }
   else
   {
      printf("negative skew\n");
      skewfactor = 0;
   }

   tambig = (double*)malloc(sizeof(double)*cm->vars->scenePixels);
   getTempByMask(cm, cm->ambig, tambig, &tambig_len);
   getTambigWarmColdMask(cm, cm->tambig_warm_mask, &tambig_warm_len, cm->tambig_cold_mask, &tambig_cold_len, pmax, pmin);

   printf("tambig_len: %lld\n", tambig_len);
   printf("warm avg: %lf warm len: %lld\n", tambig_warm_avg, tambig_warm_len);
   getTambigStats(cm, cm->tambig_warm_mask, &tambig_warm_percent, &tambig_warm_avg, tambig_warm_len);
   printf("warm avg: %lf warm len: %lld\n", tambig_warm_avg, tambig_warm_len);
   getTambigStats(cm, cm->tambig_cold_mask, &tambig_cold_percent, &tambig_cold_avg, tambig_cold_len);
   printf("cold avg: %lf cold len: %lld\n", tambig_cold_avg, tambig_cold_len);

   printf("warm: %lf cold: %lf\n", tambig_warm_avg, tambig_cold_avg);

   if(tambig_warm_percent > 40. || tambig_warm_avg > 295. || snowper > 1.)
   {
      if(tambig_cold_percent > 40. || tambig_cold_avg > 295.)
      {
         printf("pass2 analysis is invalid\n");
         return;
      }
      else
      {
         printf("cold clouds are added to pass1\n");
         addCMcloudMask(cm, cm->tambig_cold_mask);
         return;
      }
   }
   else
   {
      printf("cold and warm clouds are added to pass1\n");
      addCMcloudMask(cm, cm->tambig_warm_mask);
      addCMcloudMask(cm, cm->tambig_cold_mask);
   }

}

