////////////////////////////////////////////////////////////////////////
//
//    ImageLabel.cc: 
//
//    This is a class for image label objects.
//
////////////////////////////////////////////////////////////////////////
#include "ImageLabel.h"
#include "ImageData.h"
#include "BasicComponent.h"

ImageLabel::ImageLabel(ImageData *image, const char *name, const char *key) 
{
   _image = image;
   _numbSubset = 0;
   _name = strdup(name);;
   strcpy(_key, key);
   for (int i = 0; i < MAX_IMAGE_LABEL_SUBSET; i++) 
      _subsetList[i] = NULL;
}

ImageLabel::~ImageLabel() 
{
   delete[] _name;
}
   
ImageLabel * ImageLabel::getChildLabel(int index) 
{
   if (index >= 0 && index < _numbSubset)
      return _subsetList[index];
   else 
      return NULL;
}

void ImageLabel::addChild(ImageLabel *childLabel) 
{
   _subsetList[_numbSubset] = childLabel;
   _numbSubset++;
}

void ImageLabel::deleteLabelTree()
{
   int childNumb;

   if (!this) return;
   childNumb = this->getChildNumb();
   if (childNumb > 0) {
      for (int i=0; i<childNumb; i++) {
         // recursivly go down  
         ImageLabel *childLabel = this->getChildLabel(i);
         if (childLabel->getChildNumb() > 0) 
            this->getChildLabel(i)->deleteLabelTree();
         else 
            delete childLabel;
      }
   }

   return;
}

