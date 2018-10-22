#ifndef __IO_VIEW_H
#define __IO_VIEW_H

int read_view_header(char *filename, int *type, int *ndim, int *dim, float *ori, float *inter, float *dispmin, float *dispmax);
int read_view(char *filename, int *type_adr, int *ndim_adr, int *dim, unsigned char **B_adr);
int write_view(char *filename, int type, int ndim, int *dim, unsigned char *B);
int write_bfile(char *outfile, int nr, int nc, unsigned char *B);
int write_sfile(char *outfile, int nr, int nc, short *S);
int write_ifile(char *outfile, int nr, int nc, int *I);
int write_ffile(char *outfile, int nr, int nc, float *F);
int write_dfile(char *outfile, int nr, int nc, double *D);

#endif
