
#define MAX_PAR_STRING 128



union value {
   char *c;
   int  *i;
   float *f;
   double *d;
};

struct parm {
   char name[64];
   char type[8];
   int count;
   int maxcnt;
   union value val;
};

struct gfile {
   int unit;
   int nl;
   int ns;
   int nb;
   char *name;
   char *pdf_name;
   int instance;
   char format[33];
};


struct filter {
   int    points;
   double cwave;
   double resp_sum;
   double *wavelen;
   double *respons;
};


struct table {
   int rows;
   int columns;
   struct column *col;
   char *title;
   struct table *nexttbl;
};

struct column {
   char *name;
   char *units;
   int exponent;
   int width;
   int rows;
   double *row;
};


struct newfilter {
   int    points;
   double cwave;
   double resp_sum;
   double *wavelen;
   double *respons;
   struct column wave;
   struct column resp;
};

