// ads_getcamcon.h
//
// Apr 22, 2002
// Michael Brady

#if !defined ADS_GETCAMCON_H_INCLUDED
#define ADS_GETCAMCON_H_INCLUDED

/**
 *  Wrapper for the VICAR getcamcon function.
 */
void ads_getcamcon( const char* miplMissionName,
                    int miplCameraId,
                    double focalLength,
                    double lineObjectSpace,
                    double sampleObjectSpace,
                    double scale );


#endif // !defined ADS_GETCAMCON_H_INCLUDED
