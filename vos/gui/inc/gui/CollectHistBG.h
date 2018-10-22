////////////////////////////////////////////////////////////////
// CollectHistBG.h
////////////////////////////////////////////////////////////////
#ifndef COLLECTHISTBG_H
#define COLLECTHISTBG_H

class ImageData;
class Histogram;
class ImageTile;

void CollectHistBG(ImageData *imageModel, 
	Histogram *histR, Histogram *histG, Histogram *histB, void **active);

void CollectHistFromTile(Histogram *hist, int bufferIndex, ImageTile &tile,
	int width, int height);

#endif

