//////////////////////////////////////////////////////////////////////
//  CollectStretchedHist.h
//////////////////////////////////////////////////////////////////////
#ifndef COLLECTSTRETCHEDHIST_H
#define COLLECTSTRETCHEDHIST_H

class Lut;
class Histogram;

void CollectStretchedHist ( 
	Histogram *histR, Histogram *histG, Histogram *histB,
	Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
	Lut *lutR, Lut *lutG, Lut* lutB);


void CollectStretchedHist ( Histogram *hist, Histogram *strhist, Lut *lut);

#endif
