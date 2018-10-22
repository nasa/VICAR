//////////////////////////////////////////////////////////////////////
// SiCollectStretchedHist.h
//////////////////////////////////////////////////////////////////////
#ifndef SICOLLECTSTRETCHEDHIST_H
#define SICOLLECTSTRETCHEDHIST_H

class Lut;
class SiHistogram;

void SiCollectStretchedHist ( 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut* lutB);


void SiCollectStretchedHist ( SiHistogram *hist, 
		SiHistogram *strhist, Lut *lut);

#endif
