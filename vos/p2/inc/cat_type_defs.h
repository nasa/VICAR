#ifndef CAT_TYPE_DEFS_INCLUDED
#define CAT_TYPE_DEFS_INCLUDED 1
/*****************************************************************************

  cat_type_defs.h

  Include file for general catalog interface

  Contains data type defines.

*******************************************************************************
  Original Programmer: Thuy Nguyen ,  February 27, 1997
  Source: C
*****************************************************************************/


typedef struct {         /*   Ascii string   */
  char value[256];
  int  Valid;
 } svalue;  

typedef struct {        /*  8 bits, range 0-255   */
  unsigned char value;
  int Valid;
} cat_tiny_int_type;

typedef struct {        /* 16 bits, range -32768 - 32767  */
  short int value;
  int Valid;
} cat_small_int_type;

typedef struct {        /* 32 bits, range -2147483648 - 2147483649 */ 
  long int value;
  int Valid;
} cat_int_type;

typedef struct {      /*   32 bits, 6 digits of precision  */
  float value;
  int   Valid;
} cat_real_type;

typedef struct {      /*   64 bits, 12 digits of precision  */
  double value;
  int   Valid;
} cat_double_type;

#endif
