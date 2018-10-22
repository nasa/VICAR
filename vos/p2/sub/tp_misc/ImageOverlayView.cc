////////////////////////////////////////////////////////////////
// ImageOverlayView.h: This class is responsible for displaying
// imaging widget with overlayed objects.
////////////////////////////////////////////////////////////////
#include "ImageOverlayView.h"
#include "DrawObject.h"
#include "ErrorManager.h"
#include "ImageData.h"

ImageOverlayView::ImageOverlayView(Widget parent, const char *name, 
				   ImageData *imageData)
	: ImageDisplayView(parent, name, imageData, 0, 0)
{
    _objects = new SL_List<DrawObject *>;
}

ImageOverlayView::~ImageOverlayView()
{
    SL_ListWatch<DrawObject *> watch;
    DrawObject *current;
    
    _objects->init_scan(&watch);
    while ((current = _objects->next()) != NULL) {
	_objects->remove_current();
	delete current;
    }	
    delete _objects;
}

XvicID ImageOverlayView::addObject(DrawObject *object)
{
    _objects->add(object);
    return (object->draw());
}

void ImageOverlayView::moveObject(DrawObject *, double, double)
{
    // Empty
}

void ImageOverlayView::deleteObject(DrawObject *o)
{
    SL_ListWatch<DrawObject *> watch;
    DrawObject *current;
    
    _objects->init_scan(&watch);
    while ((current = _objects->next()) != NULL) {
        if (current == o) {
	    _objects->remove_current();     
            delete current;
	} 
    }   
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<DrawObject *>
#pragma define_template SL_ListWatch<DrawObject *>
#endif

