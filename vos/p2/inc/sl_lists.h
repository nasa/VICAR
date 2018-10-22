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

