/*---------------------------  convisos     ------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <zvproto.h>

int zgllgcor(float* is_line,float* is_samp,float* os_line,float* os_samp,
	     int mode,int icam);
int zcasgcor(float* is_line,float* is_samp,float* os_line,float* os_samp,
	     int mode,int icam);
void ztritra(int* ind,float* conv, int nph,int npv,float* line,
	     float* samp,int mode);

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zconvisos(project,icam,is_line,is_samp,os_line,os_samp,
          mode,conv,nph,npv,ind)
char *project;
float *is_line,*is_samp, *os_line,*os_samp;
float *conv;
int   icam,mode,nph,npv,*ind; 
{
/*  ==================================================================  */
    *ind = 0;
    if (strncmp(project, "GLL", 3) == 0)
    {
        zgllgcor(is_line,is_samp,os_line,os_samp,mode,icam);
    }
    else if (strncmp(project, "CAS", 3) == 0)
    {
        zcasgcor(is_line,is_samp,os_line,os_samp,mode,icam);
    }
    else
    {
        if (mode == 0)
        {
          *is_line= *os_line;               /*  OS to IS  */
          *is_samp= *os_samp;
          ztritra(ind,conv,nph,npv,is_line,is_samp,mode);
        }
        else
        {
          *os_line= *is_line;              /*  IS to OS  */
          *os_samp= *is_samp;
          ztritra(ind,conv,nph,npv,os_line,os_samp,mode);
        }
    }
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(convisos, CONVISOS) (char *project, int *icam,
	float *is_line, float *is_samp, float *os_line, float *os_samp,
	int *mode, float *conv, int *nph, int *npv, int *ind, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length;
/*  ==================================================================  */

   length = 5;

/* 11 args for convisos, project is 1st arg and 1st string   */

   zsfor2c(proj,length,project,&project,11,1,1, ind); 

   zconvisos(proj,*icam,is_line,is_samp,os_line,os_samp,
              *mode,conv,*nph,*npv,ind);

}

