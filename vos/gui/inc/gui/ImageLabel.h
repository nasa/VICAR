////////////////////////////////////////////////////////////////
//
//   ImageLabel.h: 
//
//   This is a class for image label objects.
//
////////////////////////////////////////////////////////////////
#ifndef IMAGELABEL_H
#define IMAGELABEL_H

#define MAX_IMAGE_LABEL_SUBSET         250     // max # of subsets for labels
#define MAX_IMAGE_LABEL_STRING_SIZE    132
#define MAX_IMAGE_LABEL_ITEM_SIZE      6120
#define MAX_IMAGE_LABEL_KEY_SIZE       200
#define MAX_IMAGE_LABEL_SIZE           10000

class ImageData;

class ImageLabel {
   protected:
      char *_name;                     // human-friendly name for this node
      char _key[MAX_IMAGE_LABEL_KEY_SIZE+1]; // opaque identifier for ImageData 
                                       // for this set
      ImageData *_image;
      int _numbSubset;
      ImageLabel *_subsetList[MAX_IMAGE_LABEL_SUBSET]; 

   public:
      ImageLabel(ImageData *, const char *, const char *);
      virtual ~ImageLabel();
      char *getLabelName() { return _name; } 
      char *getLabelKey() { return _key; } 
      int getChildNumb() { return _numbSubset; }
      ImageLabel *getChildLabel(int);
      void addChild(ImageLabel *);
      void deleteLabelTree();

      virtual const char *const className () { return "ImageLabel"; }
};
#endif
