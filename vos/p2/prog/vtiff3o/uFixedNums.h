/* Module uFixedNums public interface ========================================*/

#ifndef header_uFixedNums /*==================================================*/
#define header_uFixedNums


/* Data types */
typedef struct {
/* Reference: IM I-79 */
	long integerPart;
	unsigned long fractionalPart;
} DoubleFixed;


#define Boolean int

/* Routines */
extern void ConvertDoubleFixedToDouble (DoubleFixed *pFixedValue,
	double *pDoubleValue);
extern Boolean ConvertDoubleToDoubleFixed (double *pDoubleValue,
	DoubleFixed *pFixedValue);
/*  extern Boolean ConvertDoubleToFixed (double *pDoubleValue, Fixed *pFixedValue); */
/*  extern void ConvertFixedToDouble (Fixed *pFixedValue, double *pDoubleValue); */


#endif /* uFixedNums.h =======================================================*/
