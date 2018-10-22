///////////////////////////////////////////////////////////////////////////
// ViewMacro.h: Macros that expands an array of object pointers and 
//		adds a new object to the list
///////////////////////////////////////////////////////////////////////////

#define AttachViewMacro( ViewClass, viewsArray, numViews, view )	\
									\
        ViewClass **newList;						\
									\
        newList = new ViewClass *[numViews+1];				\
									\
        for (int i=0; i<numViews; i++)					\
                newList[i] = viewsArray[i];				\
									\
        if (viewsArray)							\
                delete[] viewsArray;					\
        viewsArray = newList;						\
									\
        viewsArray[numViews] = view;					\
        numViews++;


#define DetachViewMacro( ViewClass, viewsArray, numViews, view )	\
                                                                        \
        ViewClass **newList;                                            \
									\
        newList = new ViewClass *[numViews-1];                          \
									\
	int index = 0;							\
        for (int i=0; i<numViews; i++)                                  \
	    if ( viewsArray[i] !=  view)				\
                newList[index++] = viewsArray[i];                       \
									\
        if (viewsArray)                                                 \
                delete[] viewsArray;                                    \
        viewsArray = newList;                                           \
                                                                        \
        numViews--;

