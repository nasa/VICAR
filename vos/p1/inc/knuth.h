#ifndef KNUTH_H
#define KNUTH_H

/* Prototype for knuth etc. */

/* This structure implementation assumes 4-byte alignment */
typedef struct {
   float c_real;
   float c_imag;
   } complex_type;

int zknuth(char* string,float *buf);
int zknuth_var(char *cname);
void zknuth_dump(float *buf);
void zknuth_lookup(float *buf, unsigned char *table, int num_input, int flags);
void zxknuth(float *buf, float *result);
int zknuth_complex(char *string, complex_type *buf);
void zxknuth_complex(complex_type *buf, complex_type *result);


#endif

