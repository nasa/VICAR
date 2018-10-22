#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include "rtlintproto.h"

#if FORSTR_METHOD_A
#include <descrip>
#endif

#if FORSTR_METHOD_D
#include <fortran.h>
#endif

/************************************************************************/
/* sfor2len returns a the declared length of an input Fortran string.	*/
/* It is somewhat similar to sfor2c; however, it does NOT get a pointer	*/
/* to the characters, nor does it copy the string to an output C string.*/
/*									*/
/* See sfor2c.c for comments on how the various Fortran string types	*/
/* are handled, the three API's, and detailed descriptions of the	*/
/* parameters.  The same rules apply for macros, etc.			*/
/*									*/
/* A 0 may be returned if the length is somehow not available.		*/
/*									*/
/* Note that strno must never be less than *which (a 0 is returned if	*/
/* so, but that indicates a serious error).				*/
/************************************************************************/

/* OLD API */

int sfor2len(
   char *for_string,		/* In: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno			/* In: this string number (start at 1)	*/
)

{
   int which = 0;
   return v2_sfor2len(for_string, argptr, nargs, argno, strno, NULL, &which);
}

int v2_sfor2len(
   char *for_string,		/* In: the Fortran string argument	*/
   void *argptr,		/* In: ptr to original argument list	*/
   int nargs,			/* In: total number of arguments	*/
   int argno,			/* In: this argument number (start at 1)*/
   int strno,			/* In: this string number (start at 1)	*/
   va_list *valist,		/* In/Out: stdarg ptr past arg list (see*/
				/*     description in sfor2c.c).	*/
				/*     Modified by this routine.	*/
   int *which			/* In/Out: ptr to int indicating which	*/
				/*     strlen is pointed at by valist	*/
)

{
   int fortran_length = 0;
   int i;

/************************************************************************/

#if FORSTR_METHOD_STDARG
   if (valist != NULL && *valist != NULL) {
      if (strno < *which) {
         /* VERY BAD ERROR:  just return 0, for lack of anything better	*/
         return 0;
      }
      for (i=*which; i < strno; i++) {	/* Skip to this arg */
         fortran_length = (int)va_arg(*valist, FORSTR_STDARG_TYPE);
         (*which)++;
      }
      return fortran_length;
   }
   /* If valist is NULL, we don't have it; revert to old style */
#endif

/************************************************************************/

#if FORSTR_METHOD_A

/* Check if a descriptor is present.  If so, get the info from it.	*/
/* If not, assume it's passed by reference.				*/

   struct dsc$descriptor *possible_descriptor;

   possible_descriptor=(struct dsc$descriptor *)for_string; /* Check descrip */

   if ((possible_descriptor->dsc$b_dtype == DSC$K_DTYPE_T) &&
       ((possible_descriptor->dsc$b_class == DSC$K_CLASS_S) ||
        (possible_descriptor->dsc$b_class == DSC$K_CLASS_NCA) ||
        (possible_descriptor->dsc$b_class == DSC$K_CLASS_A)))

      return possible_descriptor->dsc$w_length;		/* yep,got one */
   else
      return 0;

#endif /* FORSTR_METHOD_A */

/************************************************************************/

#if FORSTR_METHOD_B

/* The length is in extra arguments after the main argument list, one	*/
/* per string in the argument list.  NOTE:  This assumes that all args	*/
/* are longwords, which is justified since Fortran passes everything by	*/
/* reference, and a pointer is a longword.				*/

/* GFORTRAN (thus GCC) 7 defines the length as size_t rather than int */

#if defined(__GNUC__) && (__GNUC__ > 6)
   return *(((size_t *)argptr)+nargs+strno-1);
#else
   return *(((int *)argptr)+nargs+strno-1);
#endif

#endif /* FORSTR_METHOD_B */

/************************************************************************/

#if FORSTR_METHOD_C

/* The length is in descriptors pointed at by longwords preceding the	*/
/* actual argument list.  See Appendix C of the _Concentrix System	*/
/* Reference Manual_, "Alliant Procedure Calling Standard", for more	*/
/* details.  Note that the docs say that "format" in the parameter list	*/
/* descriptor should indicate whether these descriptors are available,	*/
/* but "format" has been empiracally determined to always be 0 for	*/
/* these cases.  Therefore, "format" is ignored.  NOTE:  This routine	*/
/* assumes that all args are longwords, which is justified since	*/
/* Fortran passes everything by	reference, and a pointer is a longword.	*/

   struct alliant_descriptor {
      short int reserved;
      short int length;
   } *descrip;

   descrip = (struct alliant_descriptor *) *(argptr-2-nargs+argno);
   if (descrip != NULL)
      return descrip->length;
   return 0;

#endif /* FORSTR_METHOD_C */

/************************************************************************/

#if FORSTR_METHOD_D

/* The string is passed by descriptor.  Macros are provided in the	*/
/* include file to access the length and the string.  For_string should	*/
/* really be declared as type "_fcd", but this seems to work.		*/

   return _fcdlen(for_string);

#endif /* FORSTR_METHOD_D */

/************************************************************************/

#if FORSTR_METHOD_E

/* The length is in extra arguments after the main argument list, one	*/
/* per argument in the argument list.  NOTE:  This assumes that all	*/
/* args are longwords, which is justified since Fortran passes		*/
/* everything by reference, and a pointer is a longword.  This method	*/
/* is similar to method B, except every argument has space for a	*/
/* length, rather than just strings.  The length for non-string		*/
/* arguments is undefined.						*/

   return *(argptr+nargs+argno-1);

#endif /* FORSTR_METHOD_E */

/************************************************************************/

#if FORSTR_METHOD_F

/* The length is in extra arguments after the main argument list, one	*/
/* per string in the argument list.  This is exactly like method B,	*/
/* except successive arguments have lower addresses instead of higher.	*/

   return *(argptr-nargs-strno+1);

#endif /* FORSTR_METHOD_F */

/************************************************************************/

#if FORSTR_METHOD_G

/* The length is in extra arguments after the main argument list, one	*/
/* per string in the argument list.  Since pointers are 8 bytes (which	*/
/* is != sizeof(int)), we must cast the argptr to the pointer size,	*/
/* then cast it back to int to get the length.				*/

   return *(int *)(((void **)argptr)+nargs+strno-1);

#endif /* FORSTR_METHOD_G */

/************************************************************************/

#if FORSTR_METHOD_H

/* The length is in extra arguments after the main argument list, one	*/
/* per string in the argument list.  But, parameters are all 64 bits	*/
/* long, despite the fact that this is for SGI's -n32 ABI.  Plus,	*/
/* there's a major weirdness:  if varargs is NOT used on the original	*/
/* call, then there's a 4 byte offset that must be applied.  The effect	*/
/* is that you access the length via an (int *) rather than a		*/
/* (long long *), using the same address.  Since there's no way to tell	*/
/* if varargs is used, we use a HACK:  access via (int *) first, and if	*/
/* the length is 0, we assume that we should use a (long long *) (the	*/
/* top 4 bytes will always be 0; no strings longer than 2GB!) and try	*/
/* again.  Don't ask me, it works.  :-(					*/
/* Late Note:  That hack didn't work in some cases.  I guess the high-  */
/* order 32 bits of the 64-bit word are NOT always initialized to 0...  */
/* so the int test succeeds when it really shouldn't.  So, we reverse   */
/* the tests and try the 64-bit one first.  Since the result is being   */
/* stuffed into 32 bits, any values in the high 32 are discarded, so    */
/* the test really is if the low 32 are not 0.  This seems to work a    */
/* little better, and give varargs calls "preference" over non-varargs  */
/* calls (varargs should always work, non-varargs only if this word is  */
/* actually initialized to 0).  THIS MAKES ME VERY NERVOUS.  :-(        */
/* Bloody SGI.                                                          */
/* Later Note:  Okay, looks like the Real Deal is that the value        */
/* pointed at by this address is always aligned to 8-byte boundaries.   */
/* If the address is divisible by 8, pick up the lower half of a        */
/* 64-bit quantity.  If it's divisible by 4, pick up the 32-bit         */
/* quantity at that address.  Go figure.                                */

   long long *addr = ((long long *)argptr)+nargs+strno-1;
   if (((long long)addr) % 8 == 0)	/* lower part of 64-bit quantity */
      fortran_length = *addr;
   else					/* 32-bit quantity */
      fortran_length = *(int *)addr;

   return fortran_length;

#endif /* FORSTR_METHOD_H */

/************************************************************************/

#if FORSTR_METHOD_I

/* The length is in extra arguments after the main argument list, one	*/
/* per string in the argument list.  This is exactly like method B.	*/
/* However, it's not that simple (naturally).  Unlike most machines,	*/
/* when PPC stores register arguments (as all varargs routines must),	*/
/* they do not go in-line with the stack parameters as you'd expect.	*/
/* So, register arguments and stack arguments are found in different	*/
/* places.  And there is no way to find those using the typical varargs	*/
/* tricks, because we don't have access to the varargs pointer (at	*/
/* least not in the const-args case.  It turns out after much searching	*/
/* that stack arguments are found in the stack frame of the Fortran	*/
/* function.  Register arguments are found in the stack frame of the	*/
/* called C function.  Fortunately, the register arguments are saved	*/
/* before calling _another_ function (the sfor2c routines, usually), so	*/
/* we can find them in the C function's frame.  Thanks to Kai Staats,	*/
/* Ben Mesander and Scott Bassin from Yellow Dog for that tidbit.	*/
/* The final wrinkle is that the C function might not be directly	*/
/* calling the sfor2c routine... there can be intermediary routines.	*/
/* So, we have to walk up the frame pointers to get the right pair of	*/
/* stack frames to use.  That's where argptr comes in... it is the	*/
/* address of the first argument to the direct-called C function, so	*/
/* it must point to the frame of that function.  We make use of this	*/
/* fact to find the proper frame set to use.  Note that argptr points	*/
/* to different points within the frame based on whether it's a varargs	*/
/* routine or a constant-args routine... but in both cases, it's in the	*/
/* same stack frame.							*/
/* Whew!								*/
/* Later update:  Of course it is never that easy!  It turns out that	*/
/* if the direct-called C function itself calls another function with	*/
/* more than 8 arguments - ANY other function, it doesn't have to be	*/
/* in the call stack to the sfor2c routine - then extra space is	*/
/* allocated in the frame for those arguments, and the register save	*/
/* area gets pushed up a corresponding amount (to a higher address).	*/
/* The only way to fix this is to actually look *at* the argptr address.*/
/* Empirically, for the constant-args case, the offset from frame1 to	*/
/* argptr is 2 words (8 bytes) if the max # of args for any called	*/
/* routine is <=8, 4 words if the max is 9-10 args, 6 for 11-12 args,	*/
/* etc.  For the varargs case, the offsets are 28 words for <=8 args,	*/
/* 30 for 9-10, 32 for 11-12, etc.  So... we look at this offset.  If	*/
/* it's < 28 words, we assume it's the constant arg case and use argptr	*/
/* to find the right argument.  If it's >= 28 offset, we assume it's	*/
/* the varargs case and use argptr-26 to find the argument.  Yeesh.	*/
/* Of course all the above only applies if we're trying to find a	*/
/* register argument (frame1 case).  If the string length argument	*/
/* itself is >8 in the Fortran call, then we look at frame2 (the	*/
/* Fortran routine's frame) to find the stack args.  Fortunately these	*/
/* stack args are unaffected by the above problem, because they (stack	*/
/* args) are what the above offsets are for in the first place!		*/
/* Note that the above will break if there are more than about 33	*/
/* constant arguments, because it will be misinterpreted as the varargs	*/
/* case.  I think I can live with that.  ;-)				*/
/* Even later update:  all the above only works with no optimization	*/
/* on the direct-called C routine (-O0).  Not terribly surprising, but	*/
/* optimization plays havoc with the registers and frame allocations.	*/
/* Yikes.  If this all works I'll be amazed.				*/

    int *frame1, *frame2;

    /* Find the proper set of stack frames.  We do this by comparing	*/
    /* argptr against the address of the bounds of the frame pointers.	*/
    /* The argument can only be a constant so we can't use a loop.	*/

    frame1 = __builtin_frame_address(1);
    frame2 = __builtin_frame_address(2);

    if (argptr < frame1) {
	printf("FATAL ERROR in Fortran string passing: No stack frame found (too low)!!!! %x %x %x\n", argptr, frame1, frame2);
	exit(0);
    }
    if (argptr >= frame2) {
	frame1 = __builtin_frame_address(2);
	frame2 = __builtin_frame_address(3);
	if (argptr >= frame2) {
	    frame1 = __builtin_frame_address(3);
	    frame2 = __builtin_frame_address(4);
	    if (argptr >= frame2) {
		frame1 = __builtin_frame_address(4);
		frame2 = __builtin_frame_address(5);
		if (argptr >= frame2) {
		    frame1 = __builtin_frame_address(5);
		    frame2 = __builtin_frame_address(6);
		    if (argptr >= frame2) {
			printf("FATAL ERROR in Fortran string passing: No stack frame found (too deep:this is fixable)!!!! %x %x %x\n", argptr, frame1, frame2);
			exit(0);
		    }
		}
	    }
	}
    }

    if ((nargs + strno) <= 8) {
	/* For <=8 args, we have to find the register save area.  See the */
	/* comment block above. */
	int offset = (argptr - frame1);
	if (offset < 28)
	    return *(argptr + nargs+strno-1);
	else
	    return *(argptr - 26 + nargs+strno-1);
    }
    else {
	/* -7 is magic value for > 8 args (arg 9 is at frame2+2) */
	return *(frame2 - 7 + nargs + strno);
    }

   return 0;	/* ultimate fall-through default, should never be reached */

#endif /* FORSTR_METHOD_I */

/************************************************************************/

}

