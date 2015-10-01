$!****************************************************************************
$!
$! Build proc for MIPL module sl_lists_h
$! VPACK Version 1.9, Tuesday, November 20, 2001, 14:32:39
$!
$! Execute by entering:		$ @sl_lists_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sl_lists_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sl_lists_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sl_lists_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sl_lists_h.bld "STD"
$   else
$      @sl_lists_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sl_lists_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sl_lists_h.com -mixed -
	-s sl_lists.h sl_lists.cc
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sl_lists.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1994, California Institute of Technology.  U.S. Government */
/* sponsorship under NASA Contract NAS7-1270 is acknowledged.               */

#if !defined(LISTS)
#define LISTS

#include "xvmaininc.h"

#include <stdio.h>

#if !defined(VXWORKS)
#if !defined(VMS)
#include <memory.h>
#endif /* VMS */
#include <stdlib.h>
#if !defined(WIN31)
#include <unistd.h>
#endif /* WIN31 */
#if defined(XMOTIF)
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#endif /* XMOTIF */
#endif /* VXWORKS */


template <class T> class SL_ListWatch;

template <class T> class SL_List {
protected:
    T *items;
    int max_items;
    int current_items;
    int *scan_pts;
    SL_ListWatch<T> **watches;
    int current_scan;
public:
    int failed;

	// constructor

    SL_List();
    void clear();
    void describe(char *notation);

	// augmentation

    T add(T item);
    T add(T item,void (*trace_func)(T p,int length));

	// scanning initialization -- requires scan_done() below

    int init_scan(SL_ListWatch<T> *watch);

	// stepping (matching function optional)

    T next();
    T next(int (*func)(T p));
    T next(int (*func)(T p,void *p1),void *p1);
    T next(int (*func)(T p,void *p1,void *p2),void *p1,void *p2);
    T previous();

        // delete current or previous item

    T remove_current();
    T remove_current(void (*trace_func)(T p,int length));
    T remove_previous();

        // scanning cleanup

    int scan_done();

        // find first item (matching function optional)

    T find_first();
    T find_first(int (*func)(T p));
    T find_first(int (*func)(T p,void *p1),void *p1);
    T find_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2);

	// remove and return first item (matching function optional)

    T remove_first();
    T remove_first(int (*func)(T p));
    T remove_first(int (*func)(T p,void *p1),void *p1);
    T remove_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2);
    T remove_first(void (*trace_func)(T p,int length));
    T remove_first(int (*func)(T p),void (*trace_func)(T p,int length));
    T remove_first(int (*func)(T p,void *p1),void *p1,
	void (*trace_func)(T p,int length));
    T remove_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2,
	void (*trace_func)(T p,int length));

	// add or replace item (matching function optional)

    T add_replace(T p);
    T add_replace(T p,int (*func)(T orig,T p),
	void (*func2)(T orig,T p));
    T add_replace(T p,int (*func)(T orig,T p,void *p1),void *p1,
	void (*func2)(T orig,T p));
    T add_replace(T p,int (*func)(T orig,T p,void *p1,void *p2),void *p1,
	    void *p2,
	void (*func2)(T orig,T p));

        // insert item (placement-determination function optional)

    T insert(T p);
    T insert(T p,int (*func)(T orig,T p));
    T insert(T p,int (*func)(T orig,T p,void *p1),void *p1);
    T insert(T p,int (*func)(T orig,T p,void *p1,void *p2),void *p1,void *p2);
    T insert(T p,void (*trace_func)(T p,int length));
    T insert(T p,int (*func)(T orig,T p),void (*trace_func)(T p,int length));
    T insert(T p,int (*func)(T orig,T p,void *p1),void *p1,
	void (*trace_func)(T p,int length));
    T insert(T p,int (*func)(T orig,T p,void *p1,void *p2),void *p1,void *p2,
	void (*trace_func)(T p,int length));
    T insert_before_current(T p);

	// parameter retrieval

    int get_length() { return(current_items); }
    T *get_array();
    T get_nth(int n);

	// destructor

    ~SL_List();
};

template <class T> class SL_ListWatch {
private:
    SL_List<T> *list;
public:

	// constructor

    SL_ListWatch();

	// misc

    void set_list(SL_List<T> *l);

	// destructor

    ~SL_ListWatch();
};


#define MAX_SCAN_PTS	4

#define PREPARE_TO_EXPAND_LIST(T) { \
    if (current_items == max_items) { T *new_items; \
	max_items *= 2; \
	if ((new_items = new T[max_items]) == NULL) { \
	    (void)fprintf(stderr,"Insufficient memory\n"); failed = 1; \
		return((T)NULL); } \
	(void)memcpy((void *)new_items,(void *)items,current_items * \
	    sizeof(T)); \
	delete[] items; items = new_items; }}

#define ADD_TO_LIST(T,p) { \
    PREPARE_TO_EXPAND_LIST(T) \
    *(items + current_items) = (T)p; current_items++; failed = 0; }

#define INSERT(T,p,index) { int insert_i; \
    PREPARE_TO_EXPAND_LIST(T) \
    for (insert_i=current_items-1;insert_i >= index;insert_i--) \
	items[insert_i+1] = items[insert_i]; \
    items[index] = p; \
    current_items++; \
    for (insert_i=0;insert_i <= current_scan;insert_i++) \
	if (scan_pts[insert_i]-1 >= index) scan_pts[insert_i]++; \
    failed = 0; }

#define REMOVE(T,index,p) { int remove_i; \
    if (current_items == 0 || index < 0 || index >= current_items) { \
	(void)fprintf(stderr,"REMOVE: Internal error\n"); \
	failed = 1; return((T)NULL); } \
    p = items[index]; \
    for (remove_i=index+1;remove_i < current_items;remove_i++) \
	items[remove_i-1] = items[remove_i]; \
    current_items--; \
    for (remove_i=0;remove_i <= current_scan;remove_i++) \
	if (scan_pts[remove_i]-1 >= index) scan_pts[remove_i]--; \
    failed = 0; }

#define INIT(T) failed = 0; if (current_items == 0) return((T)NULL); int sp = 0;

#define CHECK_INTERNALS(T) { \
    if (current_scan < 0 || scan_pts[current_scan] < 0) { \
	(void)fprintf(stderr,"CHECK_INTERNALS: Internal scanning error\n"); \
	failed = 1; return((T)NULL); } \
    else failed = 0; }

#define RETURN_SCAN_PT(T) { \
    if (scan_pts[current_scan] == current_items) { \
	if (watches[current_scan]) watches[current_scan]->set_list(NULL); \
	current_scan--; return((T)NULL); } \
    else return((T)items[scan_pts[current_scan]++]); }

#define BACKUP(T) { \
    if (scan_pts[current_scan] < 2) return((T)NULL); \
    else { \
	scan_pts[current_scan]--; \
	return((T)items[scan_pts[current_scan]-1]); } }

#define RETURN_FIRST(T) { \
    if (sp == current_items) return((T)NULL); \
    else return((T)items[sp]); }

#if defined(WIN31) || defined(VXWORKS) || defined(VMS) || VMS_OS
#include "sl_lists.cc"
#endif
#if SUN_SOLARIS_ARCH || SUN4_ARCH
#include "sl_lists.cc"
#endif
#if (defined(SGI) || SGI_ARCH) && defined(GCC)
#include "sl_lists.cc"
#endif
#if ANY_LINUX_ARCH || AXP_UNIX_ARCH || ANY_OSX_ARCH
#include "sl_lists.cc"
#endif

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sl_lists.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1994, California Institute of Technology.  U.S. Government */
/* sponsorship under NASA Contract NAS7-1270 is acknowledged.               */

#if defined(SUN4_ARCH)
#   include "sl_lists.h"
#endif
#if VMS_OS
#   if defined(NULL)
#       undef NULL
#   endif
#   define NULL 0
#endif


    // SL_List class -- constructor

template <class T>
SL_List<T>::SL_List()
{
	/* initialize stuff used by destructor in case constructor fails
	   and app deletes partially-initialized object */

    items = NULL;
    scan_pts = NULL;
    watches = NULL;
    current_items = 0;

	/* create our working storage... */

    if (!(items = new T[8]) || !(scan_pts = new int[MAX_SCAN_PTS]) ||
	    !(watches = new SL_ListWatch<T> *[MAX_SCAN_PTS])) {
	(void)fprintf(stderr,"Insufficient memory\n");
	failed = 1;
    }

	/* and initialize it */

    else {
	max_items = 8;
	(void)memset((void *)scan_pts,0,MAX_SCAN_PTS * sizeof(int));
	(void)memset((void *)watches,0,MAX_SCAN_PTS *
	    sizeof(SL_ListWatch<T> *));
	current_scan = -1;
	failed = 0;
    }
}


    // SL_List class -- destructor

template <class T>
SL_List<T>::~SL_List()
{
	/* free allocated working storage */

    if (items) delete[] items;	     // pointers might be NULL if ctor failed
    if (scan_pts) delete[] scan_pts;
    if (watches) delete[] watches;
}


    // SL_List class -- add routines

template <class T>
T SL_List<T>::add(T p)
{
    ADD_TO_LIST(T,p)
    return((T)p);
}


template <class T>
T SL_List<T>::add(T p,void (*trace_func)(T p,int length))
{
    ADD_TO_LIST(T,p)
    (*trace_func)(p,current_items);
    return(p);
}


    // SL_List class -- add-replace routines

template <class T>
T SL_List<T>::add_replace(T p)
{
    int i;
    for (i=0;i < current_items;i++) if (items[i] == p) break;
    if (i != current_items) {
	items[i] = p;
	failed = 0;
	return(p);
    }
    else {
	ADD_TO_LIST(T,p)
	return(p);
    }
}


template <class T>
T SL_List<T>::add_replace(T p,int (*func)(T orig,T p),
    void (*func2)(T orig,T p))
{
    int i;
    for (i=0;i < current_items;i++)
	if ((!func && items[i] == p) || (func && (*func)(items[i],p))) break;
    if (i != current_items) {
	if (func2) (*func2)(items[i],p);
	items[i] = p;
	failed = 0;
	return(p);
    }
    else {
	ADD_TO_LIST(T,p)
	return(p);
    }
}


template <class T>
T SL_List<T>::add_replace(T p,int (*func)(T orig,T p,void *p1),void *p1,
    void (*func2)(T orig,T p))
{
    int i;
    if (!func) {
	(void)fprintf(stderr,
	    "add_replace: Internal error (missing matching function)\n");
	failed = 1;
	return((T)NULL);
    }
    for (i=0;i < current_items;i++) if ((*func)(items[i],p,p1)) break;
    if (i != current_items) {
	if (func2) (*func2)(items[i],p);
	items[i] = p;
	failed = 0;
	return((T)p);
    }
    else {
	ADD_TO_LIST(T,p)
	return((T)p);
    }
}


template <class T>
T SL_List<T>::add_replace(T p,int (*func)(T orig,T p,void *p1,void *p2),
	void *p1,void *p2,
    void (*func2)(T orig,T p))
{
    int i;
    if (!func) {
	(void)fprintf(stderr,
	    "add_replace: Internal error (missing matching function)\n");
	failed = 1;
	return((T)NULL);
    }
    for (i=0;i < current_items;i++) if ((*func)(items[i],p,p1,p2)) break;
    if (i != current_items) {
	if (func2) (*func2)(items[i],p);
	items[i] = p;
	failed = 0;
	return((T)p);
    }
    else {
	ADD_TO_LIST(T,p)
	return((T)p);
    }
}


    // SL_List class -- routines to remove current or previous

template <class T>
T SL_List<T>::remove_current()
{
    T p;
    if (current_scan == -1) {
	(void)fprintf(stderr,"remove_current: Internal scanning error\n");
	failed = 1;
	return((T)NULL);
    }
    else {
	REMOVE(T,scan_pts[current_scan]-1,p)
	return((T)p);
    }
}


template <class T>
T SL_List<T>::remove_current(void (*trace_func)(T p,int length))
{
    T p;
    if (current_scan == -1) {
	(void)fprintf(stderr,"remove_current: Internal scanning error\n");
	failed = 1;
	return((T)NULL);
    }
    else {
	REMOVE(T,scan_pts[current_scan]-1,p)
	(*trace_func)(p,current_items);
	return((T)p);
    }
}


template <class T>
T SL_List<T>::remove_previous()
{
    T p;
    if (current_scan == -1) {
	(void)fprintf(stderr,"remove_previous: Internal scanning error\n");
	failed = 1;
	return((T)NULL);
    }
    else if (scan_pts[current_scan]-2 < 0) {
	(void)fprintf(stderr,"remove_previous: Pointing at first element\n");
	failed = 1;
	return((T)NULL);
    }
    else {
	REMOVE(T,scan_pts[current_scan]-2,p)
	return((T)p);
    }
}


    // SL_List class -- clear routine

template <class T>
void SL_List<T>::clear() { current_items = 0; current_scan = -1; }


    // SL_List class -- describe routine

template <class T>
void SL_List<T>::describe(char *notation)
{
    (void)printf("%s: current_items = %d, max_items = %d\n",notation,
	current_items,max_items);
}


    // SL_List class -- get-array routine

template <class T>
T *SL_List<T>::get_array()
{
    T *array = new T[current_items];
    if (!array) (void)fprintf(stderr,"Insufficient memory\n");
    else (void)memcpy((void *)array,(void *)items,current_items * sizeof(T));
    return(array);
}


    // SL_List class -- get_nth element routine

template <class T>
T SL_List<T>::get_nth(int n)
{
    failed = 0;
    if (n < 0 || n >= current_items) {
	failed = 1;
	return((T)NULL);
    }
    return((T)items[n]);
}


    // SL_List class -- init-scan routines

template <class T>
int SL_List<T>::init_scan(SL_ListWatch<T> *watch)
{
    if (current_scan+1 >= MAX_SCAN_PTS) {
	(void)fprintf(stderr,"init_scan: Internal scanning error\n");
	failed = 1;
	return(-1);
    }
    else {
	current_scan++;
	scan_pts[current_scan] = 0;
	watches[current_scan] = watch;
	watch->set_list(this);
	failed = 0;
	return(0);
    }
}


    // SL_List class -- next routines

template <class T>
T SL_List<T>::next()
{
    CHECK_INTERNALS(T)
    RETURN_SCAN_PT(T)
}


template <class T>
T SL_List<T>::next(int (*func)(T p))
{
    CHECK_INTERNALS(T)
    while (scan_pts[current_scan] < current_items &&
            !(*func)(items[scan_pts[current_scan]]))
        scan_pts[current_scan]++;
    RETURN_SCAN_PT(T)
}


template <class T>
T SL_List<T>::next(int (*func)(T p,void *p1),void *p1)
{
    CHECK_INTERNALS(T)
    while (scan_pts[current_scan] < current_items &&
            !(*func)(items[scan_pts[current_scan]],p1))
        scan_pts[current_scan]++;
    RETURN_SCAN_PT(T)
}


template <class T>
T SL_List<T>::next(int (*func)(T p,void *p1,void *p2),void *p1,void *p2)
{
    CHECK_INTERNALS(T)
    while (scan_pts[current_scan] < current_items &&
            !(*func)(items[scan_pts[current_scan]],p1,p2))
        scan_pts[current_scan]++;
    RETURN_SCAN_PT(T)
}


template <class T>
T SL_List<T>::previous()
{
    CHECK_INTERNALS(T)
    BACKUP(T)
}


    // SL_List class -- find-first routines

template <class T>
T SL_List<T>::find_first()
{
    INIT(T)
    RETURN_FIRST(T)
}


template <class T>
T SL_List<T>::find_first(int (*func)(T p))
{
    INIT(T)
    while (!(*func)(items[sp]) && ++sp < current_items) ;
    RETURN_FIRST(T)
}


template <class T>
T SL_List<T>::find_first(int (*func)(T p,void *p1),void *p1)
{
    INIT(T)
    while (!(*func)(items[sp],p1) && ++sp < current_items) ;
    RETURN_FIRST(T)
}


template <class T>
T SL_List<T>::find_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2)
{
    INIT(T)
    while (!(*func)(items[sp],p1,p2) && ++sp < current_items) ;
    RETURN_FIRST(T)
}


    // SL_List class -- remove-first routines

template <class T>
T SL_List<T>::remove_first()
{
    T p;
    INIT(T)
    REMOVE(T,sp,p)
    return(p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p))
{
    T p;
    INIT(T)
    while (!(*func)(items[sp]) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p,void *p1),void *p1)
{
    T p;
    INIT(T)
    while (!(*func)(items[sp],p1) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2)
{
    T p;
    INIT(T)
    while (!(*func)(items[sp],p1,p2) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(void (*trace_func)(T p,int length))
{
    T p;
    INIT(T)
    REMOVE(T,sp,p)
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p),
    void (*trace_func)(T p,int length))
{
    T p;
    INIT(T)
    while (!(*func)(items[sp]) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p,void *p1),void *p1,
    void (*trace_func)(T p,int length))
{
    T p;
    INIT(T)
    while (!(*func)(items[sp],p1) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::remove_first(int (*func)(T p,void *p1,void *p2),void *p1,void *p2,
    void (*trace_func)(T p,int length))
{
    T p;
    INIT(T)
    while (!(*func)(items[sp],p1,p2) && ++sp < current_items) ;
    if (sp == current_items) return((T)NULL);
    REMOVE(T,sp,p)
    (*trace_func)(p,current_items);
    return((T)p);
}


    // SL_List class -- insert routines

template <class T>
T SL_List<T>::insert(T p)
{
    INSERT(T,p,0)
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p))
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p,void *p1),void *p1)
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p,p1)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p,void *p1,void *p2),void *p1,
    void *p2)
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p,p1,p2)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,void (*trace_func)(T p,int length))
{
    INSERT(T,p,0)
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p),
    void (*trace_func)(T p,int length))
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p,void *p1),void *p1,
    void (*trace_func)(T p,int length))
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p,p1)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::insert(T p,int (*func)(T orig,T p,void *p1,void *p2),void *p1,
    void *p2,void (*trace_func)(T p,int length))
{
    int i,status=0;
    for (i=0;i < current_items;i++) if (status=(*func)(items[i],p,p1,p2)) break;
    if (status != 2) INSERT(T,p,i)
    else /* status == 2 */ items[i] = p;
    (*trace_func)(p,current_items);
    return((T)p);
}


template <class T>
T SL_List<T>::insert_before_current(T p)
{
    if (current_scan == -1) {
	(void)fprintf(stderr,"insert_before_current: No context\n");
	failed = 1;
	return((T)NULL);
    }
    else if (scan_pts[current_scan]-1 < 0) {
	(void)fprintf(stderr,"insert_before_current: No context\n");
	failed = 1;
	return((T)NULL);
    }
    INSERT(T,p,scan_pts[current_scan]-1)
    return((T)p);
}


    // SL_List class -- scan-done routine

template <class T>
int SL_List<T>::scan_done()
{
    if (current_scan < 0) {
	(void)fprintf(stderr,"scan_done: Internal scanning error\n");
	failed = 1;
	return(-1);
    }
    else {
	if (watches[current_scan]) watches[current_scan]->set_list(NULL);
	current_scan--;
	failed = 0;
	return(0);
    }
}


    // SL_ListWatch class

template <class T>
SL_ListWatch<T>::SL_ListWatch() { list = NULL; }


template <class T>
void SL_ListWatch<T>::set_list(SL_List<T> *l) { list = l; }


template <class T>
SL_ListWatch<T>::~SL_ListWatch() { if (list) list->scan_done(); }
$ VOKAGLEVE
$ Return
$!#############################################################################
