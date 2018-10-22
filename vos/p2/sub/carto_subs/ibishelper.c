#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "zmabend.h"
#include "zvproto.h"
#include "applic.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "ibishelper.h"
#include "cartoLinkedList.h"

/*****************************************************************/
void IBISHELPER_lowerString(char *dest, char *source)
{
   int len, i;

   len = strlen(source);

   for(i = 0; i < len; i++)
      dest[i] = tolower(source[i]);
   dest[i] = 0;

   //   printf("dest: %s source: %s\n", dest, source);
}

/*****************************************************************/
void IBISHELPER_checkModePrecondition(char *mode)
{
   char lowerMode[7] = "";

   IBISHELPER_lowerString(lowerMode, mode);
   if(!strcmp(lowerMode, "read")) return;
   if(!strcmp(lowerMode, "write")) return;
   if(!strcmp(lowerMode, "owrite")) return;
   if(!strcmp(lowerMode, "update")) return;

   printf("Error at IBISHELPER_checkModePrecondition.\n");
   printf("Input mode must be either read, write, owrite, or update.\n");
   zabend();
}

/*****************************************************************/
void IBISHELPER_checkFormatPrecondition(char *fmt)
{
   char lowerMode[6];

   IBISHELPER_lowerString(lowerMode, fmt);
   if(lowerMode[0] == 'a') return;
   if(!strcmp(lowerMode, "full")) return;
   if(!strcmp(lowerMode, "real")) return;
   if(!strcmp(lowerMode, "doub")) return;

   printf("Error at checkFormatPrecondition.\n");
   printf("Column format must be either full, real, or alpha-numeric.\n");
   zabend();
}

/*****************************************************************/
void IBISHELPER_readIBISData(IBISStruct *ibis)
{
   int i, status;

   for(i = 0; i < ibis->nc; i++)
   {
      status = IBISColumnRead(ibis->handle, (ibis->data)[i], i+1, 1, ibis->nr);
      if(status != 1) IBISSignal(ibis->handle, status, 1);
   }
}

/*****************************************************************/
void IBISHELPER_wrongFormatError(IBISStruct *ibis, int col)
{
   printf("Invalid column format found -- col %d as %s.\n", col, (ibis->formats)[col]);
   zabend();
}

/*****************************************************************/
IBISStruct* IBISHELPER_openIBIS(char *name, int instance, char *mode)
{
   int status, i, *lengths;
   IBISStruct *ibis;
   char lowerMode[5], *formats[MAXCOLS];
   char validColFormats[6][5] = {"byte", "half", "full", "real", "doub", "comp"};

   ibis = (IBISStruct *)malloc(sizeof(IBISStruct));

   IBISHELPER_lowerString(lowerMode, mode);
   ibis->totDataSize = 0;
   IBISHELPER_checkModePrecondition(mode);
   strcpy(ibis->mode, mode);
   lengths = ibis->colLens;

   if(!strcmp(name, "inp"))
      status = zvunit(&(ibis->unit), "inp", instance, NULL);
   else
   {
      int dumcnt, dumdef;
      char fname[200];

      status = zvparm("classibis", fname, &dumcnt, &dumdef, 1, 200);
      if(status != 1) zmabend("Error while acquiring classibis file name.\n");
      status = zvunit(&(ibis->unit), "classibis", instance, "u_name", fname, NULL);
   }
   if(status != 1) zmabend("Error while acquiring class ibis file.\n");

   status = IBISFileOpen(ibis->unit, &(ibis->handle), ibis->mode, 0, 0, 0, 0);
   if(status != 1) IBISSignalU(ibis->unit, status, 1);
   IBISFileGet(ibis->handle, "nr", &(ibis->nr), 1, 1, 0);
   IBISFileGet(ibis->handle, "nc", &(ibis->nc), 1, 1, 0);
   IBISFileGet(ibis->handle, "formats", ibis->formats, 1, MAXCOLS, 6);
   for(i = 0; i < ibis->nc; i++)
   {
       IBISHELPER_lowerString((ibis->formats)[i], (ibis->formats)[i]);
       formats[i] = (ibis->formats)[i];
   }

   for(i = 0; i < ibis->nc; i++)
   {
       int j;

       for(j = 0; j < 6; j++)
         if(!strcmp(formats[i], validColFormats[j])) break;

       if(j == 6 && formats[i][0] == 'a')
       {
           sscanf(formats[i], "a%d", &lengths[i]);
           ++(lengths[i]);
           continue;
       }

       if(j == 0) lengths[i] = sizeof(char);
       else if(j == 1) lengths[i] = sizeof(short int);
       else if(j == 2) lengths[i] = sizeof(int);
       else if(j == 3) lengths[i] = sizeof(float);
       else if(j == 4) lengths[i] = sizeof(double);
       else if(j == 5) lengths[i] = 2*sizeof(float);
       else IBISHELPER_wrongFormatError(ibis, i);
   }

   ibis->data = (char **)malloc(sizeof(void *)*(ibis->nc));
   for(i = 0; i < ibis->nc; i++)
   {
       int size;

       size = lengths[i]*ibis->nr;
       ibis->data[i] = (char *)malloc(size);
       ibis->totDataSize += size;
   }

   IBISHELPER_readIBISData(ibis);

   return ibis;
}

/*****************************************************************/
void IBISHELPER_setFormats(IBISStruct *ibis, char **formats)
{
   int i;

   for(i = 0; i < ibis->nc; i++)
   {
      IBISHELPER_checkFormatPrecondition(formats[i]);
      strcpy(ibis->formats[i], formats[i]);
   }
}

/*****************************************************************/
int IBISHELPER_getLenByFormat(char *fmt)
{
   int len;
   char lowerFmt[6];

   IBISHELPER_lowerString(lowerFmt, fmt);
   if(lowerFmt[0] == 'a')
   {
      sscanf(lowerFmt, "a%d", &len);
      ++len;
   }
   else if(!strcmp(lowerFmt, "doub"))
      len = sizeof(double);
   else if(!strcmp(lowerFmt, "real"))
      len = sizeof(float);
   else if(!strcmp(lowerFmt, "byte"))
      len = sizeof(char);
   else if(!strcmp(lowerFmt, "half"))
      len = sizeof(short);
   else if(!strcmp(lowerFmt, "full"))
      len = sizeof(int);
   else
   {
      printf("%s is unsupported column type.", lowerFmt);
      zabend();
   }

   return len;
}


/*****************************************************************/
void IBISHELPER_setColumnWidths(IBISStruct *ibis)
{
   int i;

   for(i = 0; i < ibis->nc; i++)
      ibis->colLens[i] = IBISHELPER_getLenByFormat(ibis->formats[i]);
}

/*****************************************************************/
void IBISHELPER_setTotRecordSize(IBISStruct *ibis)
{
   int i;

   ibis->totRecSize = 0;
   for(i = 0; i < ibis->nc; i++) ibis->totRecSize += ibis->colLens[i];
}

/*****************************************************************/
IBISStruct* IBISHELPER_openIBIS_out(char **format, int inst, int nr, int nc)
{
   int status, i;
   IBISStruct *ibis;

   ibis = (IBISStruct*)malloc(sizeof(IBISStruct));
   ibis->nr = nr;
   ibis->nc = nc;
   IBISHELPER_setFormats(ibis, format);
   IBISHELPER_setColumnWidths(ibis);
   IBISHELPER_setTotRecordSize(ibis);
   ibis->totDataSize = ibis->totRecSize*ibis->nr;
   strcpy(ibis->mode, "write");

   status = zvunit(&(ibis->unit), "out", inst, NULL);
   assert(status == 1);

   ibis->data = (char**)malloc(sizeof(char*)*ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      ibis->data[i] = (char*)calloc(ibis->nr, IBISHELPER_getLenByFormat(ibis->formats[i]));

   return ibis;
}

/*****************************************************************/
void IBISHELPER_writeIBIS(IBISStruct *ibis)
{
   int i, status;
   char *fmts;

   fmts = (char*)malloc(sizeof(char)*6*ibis->nc);
   for(i = 0; i < ibis->nc; i++) strncpy(fmts+6*i, ibis->formats[i], 6);

   if(!strcmp(ibis->mode, "write"))
   {
      status = IBISFileOpen(ibis->unit, &(ibis->handle), ibis->mode, ibis->nc, ibis->nr, fmts, NULL);
      assert(status == 1);
   }

   for(i = 0; i < ibis->nc; i++)
   {
      char *dataPtr;

      dataPtr = IBISHELPER_getBufPtr(ibis, i, 0);

      status = IBISColumnWrite(ibis->handle, dataPtr, i+1, 1, ibis->nr);
      assert(status == 1);
   }

   free(fmts);
}

/*****************************************************************/
void IBISHELPER_closeIBIS(IBISStruct **ibis)
{
   int i, status;

   if(!strcmp((*ibis)->mode, "write") || !strcmp((*ibis)->mode, "update"))
      IBISHELPER_writeIBIS(*ibis);
   status = IBISFileClose((*ibis)->handle, 0);
   if(status != 1) IBISSignal((*ibis)->handle, status, 1);

   for(i = 0; i < (*ibis)->nc; i++)
      if(((*ibis)->data)[i] != NULL) free(((*ibis)->data)[i]);
   free((*ibis)->data);

   free(*ibis);
}

/*****************************************************************/
int IBISHELPER_getMaxColLen(IBISStruct *ibis)
{
   int i, max;

   max = 0;
   for(i = 1; i < ibis->nc; i++)
      if((ibis->colLens)[max] < (ibis->colLens)[i]) max = i;

   return max;
}

/*****************************************************************/
void IBISHELPER_setString(IBISStruct *ibis, int col, int index, char *str)
{
   char *dataPtr;

   dataPtr = IBISHELPER_getBufPtr(ibis, col, index);
   assert(ibis->formats[col][0] == 'a' || ibis->formats[col][0] == 'A');
   strncpy(dataPtr, str, ibis->colLens[col]-1);
   *(dataPtr+(ibis->colLens[col]-1)) = 0;
}

/*****************************************************************/
void IBISHELPER_setDouble(IBISStruct *ibis, int col, int index, double data)
{
   char *dataPtr;

   dataPtr = IBISHELPER_getBufPtr(ibis, col, index);

   switch(tolower(ibis->formats[col][0]))
   {
      case 'a': printf("Please use IBISHELPER_setString function.\n");
                zabend();
                break;
      case 'c': printf("Complex not supported for now.\n");
                zabend();
                break;
      case 'b': ((char*)dataPtr)[0]      = (char)data;
                break;
      case 'h': ((short int*)dataPtr)[0] = (short int)data;
                break;
      case 'f': ((int*)dataPtr)[0]       = (int)data;
                break;
      case 'r': ((float*)dataPtr)[0]     = (float)data;
                break;
      case 'd': ((double*)dataPtr)[0]    = (double)data;
                break;
   }
}

/*****************************************************************/
int IBISHELPER_getInt(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (int)(data[0]);
      case 'h': return (int)(((short int*)data)[0]);
      case 'f': return (int)(((int*)data)[0]);
      case 'r': return (int)(((float*)data)[0]);
      case 'd': return (int)(((double*)data)[0]);
   }

   assert(0);
   return 0;
}

/*****************************************************************/
float IBISHELPER_getFloat(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (float)(data[0]);
      case 'h': return (float)(((short int*)data)[0]);
      case 'f': return (float)(((int*)data)[0]);
      case 'r': return (float)(((float*)data)[0]);
      case 'd': return (float)(((double*)data)[0]);
   }

   assert(0);
   return 0.0;
}

/*****************************************************************/
double IBISHELPER_getDouble(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (double)(data[0]);
      case 'h': return (double)(((short int*)data)[0]);
      case 'f': return (double)(((int*)data)[0]);
      case 'r': return (double)(((float*)data)[0]);
      case 'd': return (double)(((double*)data)[0]);
   }

   //   printf("%s\n", format[0]);
   printf("num of columns: %d col requested: %d\n", ibis->nc, col);
   zmabend("IBISHELPER_getDouble - data from outside the number of columns may have been requested.");
   return 0.0;
}

/*****************************************************************/
void IBISHELPER_getString(IBISStruct *ibis, char *buf, int col, int index)
{
   char *format;
   char *data;

   format = (ibis->formats)[col];
   if(format[0] != 'a')
   {
      printf("A string was requested from a non-string column -- col: %d index: %d format: %s\n", col, index, format);
      zabend();
   }

   data = IBISHELPER_getBufPtr(ibis, col, index);

   strcpy(buf, data);
}

/*****************************************************************/
char* IBISHELPER_getBufPtr(IBISStruct *ibis, int col, int index)
{
   if(col > ibis->nc)
   {
      printf("Requested column does not exist in the ibis file.\n");
      printf("col requested: %d  number of columns available: %d\n", col, ibis->nc);
      zabend();
   }
   if(index > ibis->nr)
   {
      printf("Requested index does not exist in the ibis file.\n");
      printf("Requested row: %d  number of rows available: %d\n", index, ibis->nr);
      zabend();
   }

   return (ibis->data)[col] + (index*(ibis->colLens)[col]);
}

/*****************************************************************/
void IBISHELPER_getFormats(IBISStruct *ibis, char formats[MAXCOLS][30])
{
   int i;
   char *type;

   for(i = 0; i < ibis->nc; i++)
   {
      type = (ibis->formats)[i];

      if(type[0] == 'a') {IBISFORMAT(formats[i], 'a', (ibis->colLens)[i] + 1);}
      else if(!strcmp(type, "comp")) {IBISFORMAT(formats[i], 'c', 21);}
      else {IBISFORMAT(formats[i], type[0], 13);}
   }
}

/*****************************************************************/
void IBISHELPER_printIBISPrep(IBISPrep *ibis)
{
   struct node **formatArray;
   struct node **colLenArray;
   int i;

   formatArray = (struct node**)malloc(sizeof(struct node*)*(ibis->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*(ibis->colLens)->size);
   LinkedList_setNodeArray(ibis->formats, formatArray);
   LinkedList_setNodeArray(ibis->colLens, colLenArray);
   printf("unit: %d\n", ibis->unit);
   printf("nr: %d\n", ibis->nr);
   printf("nc: %d\n", ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      printf("column %d: format: %s column length: %d\n", i, (char*)(formatArray[i]->data), *((int*)(colLenArray[i]->data)));
   free(formatArray);
   free(colLenArray);
}

/*****************************************************************/
IBISPrep* IBISHELPER_openIBIS_out2(char *name, int inst, int nr)
{
   int status;
   IBISPrep *ibis2;

   ibis2 = (IBISPrep*)malloc(sizeof(IBISPrep));
   ibis2->nc = 0;
   ibis2->nr = nr;
   strcpy(ibis2->mode, "write");

   if(!strcmp(name, "out"))
      status = zvunit(&(ibis2->unit), "out", inst, NULL);
   else
      status = zvunit(&(ibis2->unit), "outibis", inst, "u_name", name, NULL);
   if(status != 1) zmabend("Error while acquiring output ibis file.\n");

   ibis2->formats = LinkedList_getLinkedList();
   ibis2->colLens = LinkedList_getLinkedList();

   return ibis2;
}

/*****************************************************************/
void IBISHELPER_addColumn(IBISPrep *ibis2, char *format)
{
   int *colLen;
   char *fmt;

   ++(ibis2->nc);
   fmt = (char*)malloc(6);
   strcpy(fmt, format);

   LinkedList_add(ibis2->formats, fmt);

   colLen = (int*)malloc(sizeof(int));
   *colLen = IBISHELPER_getLenByFormat(format);
   LinkedList_add(ibis2->colLens, colLen);
}

/*****************************************************************/
void IBISHELPER_freeIBISPrep(IBISPrep **ibis2)
{
   struct node **formatArray;
   struct node **colLenArray;
   int i;

   formatArray = (struct node**)malloc(sizeof(struct node*)*((*ibis2)->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*((*ibis2)->colLens)->size);
   LinkedList_setNodeArray((*ibis2)->formats, formatArray);
   LinkedList_setNodeArray((*ibis2)->colLens, colLenArray);
   for(i = 0; i < (*ibis2)->nc; i++)
   {
      free(formatArray[i]->data);
      free(colLenArray[i]->data);
   }
   LinkedList_free(&((*ibis2)->formats));
   LinkedList_free(&((*ibis2)->colLens));

   free(formatArray);
   free(colLenArray);
   free(*ibis2);
}

/*****************************************************************/
IBISStruct* IBISHELPER_getIBISStruct(IBISPrep **prep)
{
   int i;
   struct node **formatArray;
   struct node **colLenArray;
   IBISStruct *ibis;

   ibis = (IBISStruct*)malloc(sizeof(IBISStruct));
   ibis->unit = (*prep)->unit;
   ibis->nr = (*prep)->nr;
   ibis->nc = (*prep)->nc;
   strcpy(ibis->mode, (*prep)->mode);
   //   strcpy(ibis->org, (*prep)->org);

   formatArray = (struct node**)malloc(sizeof(struct node*)*((*prep)->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*((*prep)->colLens)->size);
   LinkedList_setNodeArray((*prep)->formats, formatArray);
   LinkedList_setNodeArray((*prep)->colLens, colLenArray);
   for(i = 0; i < ibis->nc; i++)
   {
      strncpy(ibis->formats[i], (char*)(formatArray[i]->data), 6);
      ibis->colLens[i] = *((int*)(colLenArray[i]->data));
   }

   IBISHELPER_setColumnWidths(ibis);
   IBISHELPER_setTotRecordSize(ibis);
   ibis->totDataSize = ibis->totRecSize*ibis->nr;

   ibis->data = (char**)malloc(sizeof(char*)*ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      ibis->data[i] = (char*)calloc(ibis->nr, (ibis->colLens)[i]);

   IBISHELPER_freeIBISPrep(prep);

   free(formatArray);
   free(colLenArray);
   return ibis;
}


/*****************************************************************
char* IBISHELPER_getDataPtr(IBISPrep *ibis2, int row, int col)
{
   int i;
   int colLen;
   char *buf;

   assert(row < ibis2->nr && col < ibis2->nc);
   buf = LinkedList_get(ibis2->data, col);
   colLen = *((int*)(LinkedList_get(ibis2->colLens, col)));

   return (buf + row*colLen);
}
*/

/*****************************************************************
int IBISHELPER_getColLen(IBISPrep *ibis2, int col)
{
   int colLen = *((int*)LinkedList_get(ibis2->colLens, col));

   return colLen;
}
*/

/*****************************************************************
void IBISHELPER_setString(IBISPrep *ibis2, int row, int col, char *str)
{
   int colLen;
   char *ptr, head;

   head = ((char*)(LinkedList_get(ibis2->formats, col)))[0];
   if(head != 'a' && head != 'A')
      zmabend("Attempting to write string into non-string column.\n");
   ptr = IBISHELPER_getDataPtr(ibis2, row, col);
   colLen = IBISHELPER_getColLen(ibis2, col);

   strncpy(ptr, str, colLen);
}
*/

/*****************************************************************
void IBISHELPER_setDouble(IBISPrep *ibis2, int row, int col, double data)
{
   char *ptr, *format;

   format = (char*)LinkedList_get(ibis2->formats, col);
   if(format[0] == 'a')
      zmabend("Attempting to write numeric into string column.\n");

   //   printf("1) data: %lf\n", data);
   ptr = IBISHELPER_getDataPtr(ibis2, row, col);
   //   printf("2) data: %lf %s\n", data, format);
   switch(tolower(format[0]))
   {
      case 'c': printf("Complex not supported for now.\n");
                zabend();
                break;
      case 'b': ((char*)ptr)[0]      = (char)data;
                break;
      case 'h': ((short int*)ptr)[0] = (short int)data;
                break;
      case 'f': ((int*)ptr)[0]       = (int)data;
                break;
      case 'r': ((float*)ptr)[0]     = (float)data;
                break;
      case 'd': ((double*)ptr)[0]    = (double)data;
                break;
   }
}
*/

/*****************************************************************
void IBISHELPER_writeIBIS(IBISPrep *ibis2)
{
   char *formats;
   int i, status;

   formats = (char*)malloc(sizeof(char)*ibis2->nc*6);
   for(i = 0; i < ibis2->nc; i++)
   {
      strcpy(formats+(i*6), (char*)(LinkedList_get(ibis2->formats, i)));
   }

   if(!strcmp(ibis2->mode, "write"))
   {
      status = IBISFileOpen(ibis2->unit, &(ibis2->handle), ibis2->mode, ibis2->nc, ibis2->nr, (char*)formats, NULL);
      assert(status == 1);
   }

   for(i = 0; i < ibis2->nc; i++)
   {
      char *ptr;

      ptr = IBISHELPER_getDataPtr(ibis2, 0, i);

      status = IBISColumnWrite(ibis2->handle, ptr, i+1, 1, ibis2->nr);
      assert(status == 1);
   }

   //   for(i = 0; i < ibis2->nc; i++) free(formats[i]);
   free(formats);
}
*/

