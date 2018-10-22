/* Module uFixedNums implementation ==========================================*/


/* Public definitions ========================================================*/
#include "uFixedNums.h"	/* module's public interface */


/* Private definitions =======================================================*/

/* Links to the outside */
#include <limits.h>

#define FALSE 0
#define TRUE 1

#ifdef Examples
1. Convert from a double to a double fixed and a fixed and back again.

	#include <stdio.h>
	#include "uFixedNums.h"

#include "uFixedNums.proto.h"

	main (void)
	{
		Boolean success;
		Fixed aFixedValue;
		DoubleFixed aDoubleFixedValue;
		double aDoubleValue, a2ndDouble;
		FILE *pOut;


		pOut = fopen ("test fixed report", "w");
	
		aDoubleValue = 12345.067895;
		success = ConvertDoubleToDoubleFixed (&aDoubleValue, &aDoubleFixedValue);
		fprintf (pOut, "\n%.12f to dfixed: (%d), 0x%lx,0x%lx\n", aDoubleValue,
			success, aDoubleFixedValue.integerPart,
			aDoubleFixedValue.fractionalPart);
		ConvertDoubleFixedToDouble (&aDoubleFixedValue, &a2ndDouble);
		fprintf (pOut, "    and back: %.12f\n", a2ndDouble);
		success = ConvertDoubleToFixed (&aDoubleValue, &aFixedValue);
		fprintf (pOut, "%.12f to fixed: (%d), 0x%lx\n", aDoubleValue, success,
			aFixedValue);
		ConvertFixedToDouble (&aFixedValue, &a2ndDouble);
		fprintf (pOut, "    and back: %.12f\n", a2ndDouble);

		fclose (pOut);

	}  /* main */

#endif


/* Routine implementations ===================================================*/


void
ConvertDoubleFixedToDouble (
	DoubleFixed *pFixedValue,
	double *pDoubleValue
) {
	Boolean negative;
	unsigned long fractionalConverter;


/* Set up number as positive */
	if ((negative = (pFixedValue->integerPart < 0)))
		pFixedValue->integerPart = -pFixedValue->integerPart;
	fractionalConverter = (unsigned long)0xFFFFFFFF;

/* Convert */
	*pDoubleValue = (double)pFixedValue->integerPart +
		((double)pFixedValue->fractionalPart / (double)fractionalConverter);

/* Restore sign */
	if (negative) {
		pFixedValue->integerPart = -pFixedValue->integerPart;
		*pDoubleValue = -(*pDoubleValue);
	}

}  /* ConvertDoubleFixedToDouble */


Boolean
ConvertDoubleToDoubleFixed (
	double *pDoubleValue,
	DoubleFixed *pFixedValue
) {
	Boolean negative;
	unsigned long fractionalConverter;


/* Check for overflow */
	if (*pDoubleValue < LONG_MIN  ||  LONG_MAX < *pDoubleValue) {
		pFixedValue->integerPart = 0;
		pFixedValue->fractionalPart = 0;
		return (FALSE);
	}

/* Set up number as positive */
	if ((negative = (*pDoubleValue < 0.0)))
		*pDoubleValue = -(*pDoubleValue);
	fractionalConverter = (unsigned long)0xFFFFFFFF;

/* Convert */
	pFixedValue->integerPart = (long)(*pDoubleValue);
	pFixedValue->fractionalPart = (unsigned long)
		((*pDoubleValue - pFixedValue->integerPart) * fractionalConverter);

/* Restore sign */
	if (negative) {
		*pDoubleValue = -(*pDoubleValue);
		pFixedValue->integerPart = -pFixedValue->integerPart;
	}
	return (TRUE);

}  /* ConvertDoubleToFixed */


#if 0
Boolean
ConvertDoubleToFixed (
	double *pDoubleValue,
	Fixed *pFixedValue
) {
	Boolean negative;
	short integerPart;
	unsigned short fractionalConverter;


/* Check for overflow */
	if (*pDoubleValue < SHRT_MIN  ||  SHRT_MAX < *pDoubleValue) {
		*pFixedValue = 0;
		return (FALSE);
	}

/* Set up number as positive */
	if (negative = *pDoubleValue < 0.0)
		*pDoubleValue = -(*pDoubleValue);
	fractionalConverter = (unsigned short)0xFFFF;

/* Convert */
	integerPart = (short)(*pDoubleValue);
	*pFixedValue = ((long)integerPart << 16)  |  (unsigned short)
		((*pDoubleValue - integerPart) * fractionalConverter);

/* Restore sign */
	if (negative) {
		*pDoubleValue = -(*pDoubleValue);
		*pFixedValue = -(*pFixedValue);
	}
	return (TRUE);

}  /* ConvertDoubleToFixed */


void
ConvertFixedToDouble (
	Fixed *pFixedValue,
	double *pDoubleValue
) {
	Boolean negative;
	unsigned short fractionalConverter;


/* Set up number as positive */
	if (negative = *pFixedValue < 0)
		*pFixedValue = -(*pFixedValue);
	fractionalConverter = (unsigned short)0xFFFF;

/* Convert */
	*pDoubleValue = (double)((*pFixedValue) >> 16) +
		((double)((*pFixedValue) & 0xFFFF) / (double)fractionalConverter);

/* Restore sign */
	if (negative) {
		*pFixedValue = -(*pFixedValue);
		*pDoubleValue = -(*pDoubleValue);
	}

}  /* ConvertFixedToDouble */
#endif

/* uFixedNums.c ==============================================================*/

