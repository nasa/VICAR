///////////////////////////////////////////////////////////////////////////
// AttachViewMacro.h: 	Macro that expands an array of object pointers and 
//			adds a new object to the list
///////////////////////////////////////////////////////////////////////////

#define AttachViewMacro( numViews, View, views )			\
									\
        View **newList;							\
									\
        newList = new View *[numViews+1];				\
        for (int i=0; i<numViews; i++)					\
                newList[i] = views[i];					\
									\
        if (views)							\
                delete[] views;						\
        views = newList;						\
									\
        views[numViews] = view;						\
        numViews++;				
