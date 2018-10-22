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
