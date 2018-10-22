#include <zvproto.h>
#include <defines.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "ImageUtils.h"

/******************************************************************************/
void readError(char *fname, int line, int status)
{
   printf("Problem reading line %d in file %s -- error status: %d.\n",
             line, fname, status);
   assert(0);
}

/******************************************************************************/
VICAR_IMAGE* getImage(int unit)
{
   int status;
   VICAR_IMAGE* vi;

   vi = (VICAR_IMAGE*)malloc(sizeof(VICAR_IMAGE));
   assert(unit > -1);
   vi->unit = unit;

   status = zvget(unit, "NAME", (vi->fname), "NL", &(vi->nl), "NS", &(vi->ns),
                  "PIX_SIZE", &(vi->pixsize), "FORMAT", &(vi->format), NULL);
   assert(status == 1);

   vi->buffer = (double*)malloc(sizeof(double)*(vi->ns));
   vi->curr_line_in_buff = -1;

   vi->valid = NULL;
   vi->valid_cnt = -1;

   return vi;
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp(int inst)
{
   int status, unit;

   status = zvunit(&unit, "inp", inst, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_fname(char *fname, int inst)
{
   int status, unit;

   status = zvunit(&unit, "VI_INP", inst, "U_NAME", fname, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_parmName(char *parmName, int inst)
{
   int status, dumdef, cnt;
   char fname[IU_MAX_PARM][IU_MAX_FNAME_LEN];

   status = zvparm(parmName, fname, &cnt, &dumdef, IU_MAX_PARM, IU_MAX_FNAME_LEN);
   assert(status == 1);

   if(cnt < inst) return NULL;

   return getVI_inp_by_fname(fname[inst-1], inst);
}

/******************************************************************************/
VICAR_IMAGE* getVI_out(char *format, int inst, int nl, int ns)
{
   int status, unit;

   status = zvunit(&unit, "out", inst, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "WRITE", "O_FORMAT", format, "U_FORMAT", "DOUB",
                   "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
void getValidMask(VICAR_IMAGE **vi)
{
   int i, j;

   if((*vi)->valid != NULL) return;

   (*vi)->valid_cnt = 0;
   (*vi)->valid = (unsigned char**)malloc(sizeof(unsigned char*)*(*vi)->nl);
   for(i = 0; i < (*vi)->nl; i++)
   {
      readVicarImageLine(*vi, i);
      (*vi)->valid[i] = (unsigned char*)malloc(sizeof(unsigned char)*(*vi)->ns);
      for(j = 0; j < (*vi)->ns; j++)
      {
         if(fabs((*vi)->buffer[j]) > 10E-10)
         {
            (*vi)->valid[i][j] = 1;
            (*vi)->valid_cnt++;
         }
         else (*vi)->valid[i][j] = 0;
      }
   }
}

/******************************************************************************/
VICAR_IMAGE* getVI_out_by_fname(char *fname, char *type, char *format, int inst, int nl, int ns)
{
   int status, unit;

   status = zvunit(&unit, type, inst, "U_NAME", fname, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "WRITE", "O_FORMAT", format, "U_FORMAT", "DOUB",
                   "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_out_by_parmName(char *parmName, char *format, int inst, int nl, int ns)
{
   int status, dumdef, cnt;
   char fname[IU_MAX_PARM][IU_MAX_FNAME_LEN];

   status = zvparm(parmName, fname, &cnt, &dumdef, IU_MAX_PARM, IU_MAX_FNAME_LEN);
   assert(status == 1);

   if(cnt < inst) return NULL;

   return getVI_out_by_fname(fname[inst-1], parmName, format, inst, nl, ns);
}

/******************************************************************************/
void deleteImage(VICAR_IMAGE **vi)
{
   int i;

   if(*vi != NULL)
   {
      // free image line buffer
      if((*vi)->buffer != NULL) free((*vi)->buffer);
      (*vi)->buffer = NULL;

      // free image valid buffer
      if((*vi)->valid != NULL)
      {
        for(i = 0; i < (*vi)->nl; i++) free((*vi)->valid[i]);
        free((*vi)->valid);
        (*vi)->valid = NULL;
      }

      free(*vi);
      *vi = NULL;
   }
}

/******************************************************************************/
VICAR_TILE_IMAGE* getVTI(VICAR_IMAGE *vi, int tile_nl, int tile_ns)
{
   int i;
   VICAR_TILE_IMAGE *vti;

   // check to see that requested tile size is smaller than image size
   assert(tile_nl <= vi->nl && tile_ns <= vi->ns);

   vti = (VICAR_TILE_IMAGE*)malloc(sizeof(VICAR_TILE_IMAGE));
   vti->vi = vi;
   vti->tile_nl = tile_nl;
   vti->tile_ns = tile_ns;
   vti->buffer_size = vti->vi->ns + tile_ns;
   vti->startPos = (tile_ns-1)/2;

   vti->buffer = (double**)malloc(sizeof(double*)*tile_nl);
   vti->tile = (double**)malloc(sizeof(double*)*tile_nl);
   for(i = 0; i < tile_nl; i++)
      vti->buffer[i] = (double*)calloc(vti->buffer_size, sizeof(double));


   vti->last_line_requested = -1;

   return vti;
}

/******************************************************************************/
void readAllVTIBuffers(VICAR_TILE_IMAGE *vti, int line)
{
   int i, status, startline;

   startline = line - (vti->tile_nl-1)/2;

   for(i = 0; i < vti->tile_nl; i++)
   {
      if(startline+i < 0 || startline+i >= vti->vi->nl)
      {
         memset(vti->buffer[i], 0, sizeof(double)*vti->buffer_size);
         continue;
      }

      //      printf("reading all %d -- reading line: %d\n", line, startline+i+1);
      status = zvread(vti->vi->unit, vti->buffer[i]+vti->startPos, "LINE", startline+i+1, NULL);
      assert(status == 1);
   }
}

/******************************************************************************/
void rollVTIBuffers(VICAR_TILE_IMAGE **vti, int line)
{
   int i, status, lineToRead;
   double *tmpbuf;

   tmpbuf = (*vti)->buffer[0];
   for(i = 0; i < (*vti)->tile_nl-1; i++)
      (*vti)->buffer[i] = (*vti)->buffer[i+1];
   (*vti)->buffer[i] = tmpbuf;

   lineToRead = line + (*vti)->tile_nl/2 + 1;

   //   printf("rolling buff %d -- reading line: %d\n", line, lineToRead);

   if(lineToRead > (*vti)->vi->nl)
      memset((*vti)->buffer[i], 0, sizeof(double)*(*vti)->buffer_size);
   else
   {
      status = zvread((*vti)->vi->unit, (*vti)->buffer[i]+(*vti)->startPos, "LINE", lineToRead, NULL);
      assert(status == 1);
   }
}

/******************************************************************************/
void readVicarTileImage(VICAR_TILE_IMAGE *vti, int line, int samp)
{
   int i, startsamp;

   // check to see if current lines are in the buffer
   if(vti->last_line_requested == -1 || abs(vti->last_line_requested - line) > 1)
      readAllVTIBuffers(vti, line);
   else if(abs(vti->last_line_requested - line) == 1)
      rollVTIBuffers(&vti, line);
   vti->last_line_requested = line;

   startsamp = vti->startPos + samp-(vti->tile_ns-1)/2;
   for(i = 0; i < vti->tile_nl; i++)
      vti->tile[i] = vti->buffer[i]+startsamp;
}

/******************************************************************************/
void deleteVTI(VICAR_TILE_IMAGE **vti)
{
   int i;

   for(i = 0; i < (*vti)->tile_nl; i++)
      free((*vti)->buffer[i]);
   free((*vti)->buffer);
   free((*vti)->tile);

   free(*vti);
}

/******************************************************************************/
VICAR_RESAMPLE_IMAGE* getVRI(VICAR_IMAGE *from, VICAR_IMAGE *to, int resample_mode)
{
   int i;
   VICAR_RESAMPLE_IMAGE *vri;

   if(to == NULL) return NULL;
   vri = (VICAR_RESAMPLE_IMAGE*)malloc(sizeof(VICAR_RESAMPLE_IMAGE));

   vri->from = from;
   vri->to = to;

   vri->buffer = (double**)malloc(sizeof(double*)*4);
   for(i = 0; i < 4; i++)
   {
      vri->buffer[i] = (double*)calloc(from->ns, sizeof(double));
      vri->curr_lines_in_buff[i] = -1;
   }

   vri->resample_mode = resample_mode;

   return vri;
}

/******************************************************************************/
void deleteVRI(VICAR_RESAMPLE_IMAGE **vri)
{
   int i;

   for(i = 0; i < 4; i++) free((*vri)->buffer[i]);
   free((*vri)->buffer);

   free(*vri);
}

/******************************************************************************/
void getSkippedLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int to_line)
{
   int i, sskip, lskip, line;

   sskip = vri->from->ns/vri->to->ns;
   lskip = vri->from->nl/vri->to->nl;
   line = to_line*lskip;
   readVicarResampleImageLine(vri, 0, line);
   /*
   if(line != vri->curr_lines_in_buff[0])
   {
      status = zvread(vri->from->unit, vri->buffer[0], "LINE", line+1, NULL);
      if(status != 1) zabend();
      vri->curr_lines_in_buff[0] = line;
   }
   */

   for(i = 0; i < vri->to->ns; i++)
      buf[i] = vri->buffer[0][i*sskip];
}

/******************************************************************************/
/* Helper function called by prepareBuffer                                    */
/******************************************************************************/
void swapVRIBuffer(VICAR_RESAMPLE_IMAGE *vri, int ind1, int ind2)
{
   double *tmp;
   int tmpindex;

   assert(ind1 >= 0 && ind1 < 4 && ind2 >= 0 && ind2 < 4);

   tmp = vri->buffer[ind1];
   tmpindex = vri->curr_lines_in_buff[ind1];

   vri->buffer[ind1] = vri->buffer[ind2];
   vri->curr_lines_in_buff[ind1] = vri->curr_lines_in_buff[ind2];

   vri->buffer[ind2] = tmp;
   vri->curr_lines_in_buff[ind2] = tmpindex;
}

/******************************************************************************/
void prepareBilinInterpBuffer(VICAR_RESAMPLE_IMAGE *vri, double centerline)
{
   int lines[2];

   lines[0] = (int)centerline;
   lines[1] = (int)(centerline + 1);

   assert(lines[0] < vri->from->nl);
   if(lines[1] >= vri->from->nl) lines[1] = vri->from->nl - 1;

   // if already in buffer then just return
   if(vri->curr_lines_in_buff[0] == lines[0] && vri->curr_lines_in_buff[1] == lines[1])
      return;

   // swap condition: none of the buffers are in correct order and
   // there are buffers that can be swapped.
   if(lines[0] != vri->curr_lines_in_buff[0] &&
      lines[1] != vri->curr_lines_in_buff[1] &&
      (lines[0] == vri->curr_lines_in_buff[1] ||
      lines[1] == vri->curr_lines_in_buff[0])) swapVRIBuffer(vri, 1, 0);

   // if after swap, it's still not in buffer then read
   if(lines[0] != vri->curr_lines_in_buff[0])
      readVicarResampleImageLine(vri, 0, lines[0]);
   if(lines[1] != vri->curr_lines_in_buff[1])
      readVicarResampleImageLine(vri, 1, lines[1]);
}

/******************************************************************************/
double getBilinInterpSamp(VICAR_RESAMPLE_IMAGE *vri, int samp, double y, int y1, int y2)
{
   double x, f1, f2, f3, f4;
   double term1, term2, term3, term4;
   int x1, x2;

   assert(samp >= 0 && samp < vri->to->ns);

   /*pk change*/
   //   x = vri->from->ns/(double)vri->to->ns*(samp+0.5);
   x = vri->from->ns/(double)vri->to->ns*samp;
   x1 = (int)x;
   x2 = (int)(x+1);
   if(x1 <= 0) x1 = 0;
   if(x2 <= 0) x2 = 0;
   if(x1 >= vri->from->ns) x1 = vri->from->ns - 1;
   if(x2 >= vri->from->ns) x2 = vri->from->ns - 1;

   f1 = vri->buffer[0][x1];
   f2 = vri->buffer[0][x2];
   f3 = vri->buffer[1][x1];
   f4 = vri->buffer[1][x2];

   if(((x2-x1)*(y2-y1))*(x2-x)*(y2-y) == 0) term1 = 0.;
   else term1 = f1/((x2-x1)*(y2-y1))*(x2-x)*(y2-y);

   if(((x2-x1)*(y2-y1))*(x-x1)*(y2-y) == 0) term2 = 0.;
   else term2 = f2/((x2-x1)*(y2-y1))*(x-x1)*(y2-y);

   if(((x2-x1)*(y2-y1))*(x2-x)*(y-y1) == 0) term3 = 0.;
   else term3 = f3/((x2-x1)*(y2-y1))*(x2-x)*(y-y1);

   if(((x2-x1)*(y2-y1))*(x-x1)*(y-y1) == 0) term4 = 0.;
   else term4 = f4/((x2-x1)*(y2-y1))*(x-x1)*(y-y1);

   return term1 + term2 + term3 + term4;
}

/******************************************************************************/
void getBilinInterpLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int line)
{
   int i;
   double centerline;

   // initialize buffers
   /*pk change*/
   //   centerline = vri->from->nl/(double)vri->to->nl*(line+0.5);
   centerline = vri->from->nl/(double)vri->to->nl*line;
   prepareBilinInterpBuffer(vri, centerline);

   for(i = 0; i < vri->to->ns; i++)
   {
      vri->to->buffer[i] = getBilinInterpSamp(vri, i, centerline, (int)centerline, (int)(centerline+1));

      // debugging
      /*
      if(vri->to->buffer[i] < 0. || vri->to->buffer[i] > 1)
         printf("!!!error here: %d %d\n", line, i);
      */
   }
}

/******************************************************************************/
void getDownSampledLine(VICAR_RESAMPLE_IMAGE *vri, double *ds_buf, int line)
{
  if(vri->resample_mode == IU_SKIP_LINES) {
    getSkippedLine(vri, ds_buf, line);
    return;
  }
  else if(vri->resample_mode == IU_BILINEAR_INTERP) {
    getBilinInterpLine(vri, ds_buf, line);
    return;
  }
  else
    {
      printf("\n!!!RESAMPLING FROM %s TO %s: INVALID RESAMPLE MODE SPECIFIED!!!\n",
             vri->from->fname, vri->to->fname);
      zabend();
    }
}

/******************************************************************************/
void writeVicarImageLine(VICAR_IMAGE *vi, int line)
{
   int status;

   status = zvwrit(vi->unit, vi->buffer, "LINE", line+1, NULL);
   if(status != 1)
   {
      printf("Problem reading writing %d in file %s.\n", line, vi->fname);
      zabend();
   }
}

/******************************************************************************/
void readVicarImageLine(VICAR_IMAGE *vi, int line)
{
   int status;

   if(vi->curr_line_in_buff != line)
   {
      status = zvread(vi->unit, vi->buffer, "LINE", line+1, NULL);
      if(status != 1) readError(vi->fname, line+1, status);
   }

   vi->curr_line_in_buff = line;
}

/******************************************************************************/
void readVicarResampleImageLine(VICAR_RESAMPLE_IMAGE *vri, int buffer_index, int line)
{
   int status;

   if(vri->curr_lines_in_buff[buffer_index] != line)
   {
      status = zvread(vri->from->unit, vri->buffer[buffer_index], "LINE", line+1, NULL);
      if(status != 1) readError(vri->from->fname, line+1, status);
   }

   vri->curr_lines_in_buff[buffer_index] = line;
}

/******************************************************************************/
void createDownSampledImage(VICAR_RESAMPLE_IMAGE *vri)
{
   int i;

   for(i = 0; i < vri->to->nl; i++)
   {
      getDownSampledLine(vri, vri->to->buffer, i);
      writeVicarImageLine(vri->to, i);
   }
}

/******************************************************************************/
void deleteAndCloseImage(VICAR_IMAGE **vi)
{
   int status;

   status = zvclose((*vi)->unit, NULL);
   assert(status == 1);

   deleteImage(vi);
}
