static char sccsNewoutcon[] = "@(#) outcon.c 2.7 9/14/89 PDS Vicar2";

/* Outcon is a routine which puts a number into a character array
   in ASCII format.

   i.e.
     1  =     061
     20 = 062 060

   note: all constants are in octal format

   Parameters:

   numargs - the number of arguments being passed to outcon.
   input   - the value to put into the array.
   outloc  - a pointer to the right-most location in the array
             where the formatted number will be put.
   nchar   - the length allocated for the formatted number in the array
   nfract    - a nfract for printing real numbers:
               negative : format in E style .xxxxExx
	       zero     : format in I style  xxxx
	       positive : format in F style  xxxx.xx
 
*/
#include <math.h> /* included for log10(x) - log base 10 of x
		              and pow(x,y) - raises x to the y power */
#include <stdio.h>
#include <string.h>

outcon_(numargs,input,outloc,nchar,nfract)
     /* bridge to outcon from FORTRAN programs */
{
  outcon(numargs,input,outloc,nchar,nfract);
}

doutcon_(numargs,input,outloc,nchar,nfract)
     /* bridge to doutcon from FORTRAN programs */
{
  doutcon(numargs,input,outloc,nchar,nfract);
}


outcon(numargs,input,outloc,nchar,nfract)
int *numargs, *nchar, *nfract;
{
  if (*numargs == 3)
    print_int(input,input,outloc,*nchar);       /* print an integer    */
  else
    print_flt(input,outloc,*nchar,*nfract); /* print a real number */
}

doutcon(numargs,input,outloc,nchar,nfract)
     int *numargs, *nchar, *nfract;
{
  print_dflt(input,outloc,*nchar,*nfract);
}

print_int(sh_input,ln_input,outloc,nchar)

     /* this routine puts an integer into the character array */

int nchar;
short int *sh_input;
long  int *ln_input;
char *outloc;
{
  register i;
  char p_buf[80];
  char *ptr;
  int integer;
  char outbuf[80];

  if (nchar < 0) {
    integer = *sh_input;
    nchar = abs(nchar);
  }
  else
    integer = *ln_input;
  ptr = outloc - nchar + 1;
  
  sprintf(p_buf,"%%%d%s",nchar,"d");
  sprintf(outbuf,p_buf,integer);
  for (i=0;i<strlen(outbuf);i++) 
    ptr[i] = outbuf[i];
}


print_flt(input,outloc,nchar,nfract)

     /* this routine puts a  real number into the character array */
     
int nchar, nfract;
char *outloc;
float *input;
{
  register i;
  char outbuf[80];
  char p_buf[80];
  char format[2];
  char *ptr;
  int temp;

  ptr = outloc - nchar + 1;

  if (nfract < 0) {    /* E format */
    strcpy(format,"e");
    sprintf(p_buf,"%%%d.%d%s",nchar,abs(nfract),format);
    sprintf(outbuf,p_buf,*input);
  }
  else {
    strcpy(format,"f");
    sprintf(p_buf,"%%%d.%d%s",nchar,nfract,format);
    sprintf(outbuf,p_buf,*input);
  }
  for (i=0;i<strlen(outbuf);i++)
    ptr[i] = outbuf[i];
}

print_dflt(input,outloc,nchar,nfract)

     /* this routine puts a  real number into the character array */
     
int nchar, nfract;
char *outloc;
double *input;
{
  register i;
  char outbuf[80];
  char p_buf[80];
  char format[2];
  char *ptr;
  int temp;

  ptr = outloc - nchar + 1;

  if (nfract < 0) {    /* E format */
    strcpy(format,"e");
    sprintf(p_buf,"%%%d.%d%s",nchar,abs(nfract),format);
    sprintf(outbuf,p_buf,*input);
  }
  else {
    strcpy(format,"f");
    sprintf(p_buf,"%%%d.%d%s",nchar,nfract,format);
    sprintf(outbuf,p_buf,*input);
  }
  for (i=0;i<strlen(outbuf);i++)
    ptr[i] = outbuf[i];
}

