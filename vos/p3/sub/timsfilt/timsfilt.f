C******************************************************************************
	SUBROUTINE TIMSFILT(IDATE,WAVE,FFILTER,FWVLN)
C
C	Interface to return the filter and wavelength arrays explicitly
C       instead of in a common block (for some c programs such as)
C       tcal
C
	REAL WAVE(6),FILTER(158,6),WVLN(158),FFILTER(158,6),FWVLN(158)
	COMMON /RAWFIL/ FILTER,WVLN,ICALDATE
C
        CALL CENTWAV(IDATE,WAVE)
C
	CALL MVE(7,158,FILTER(1,1),FFILTER(1,1),1,1)
	CALL MVE(7,158,FILTER(1,2),FFILTER(1,2),1,1)
	CALL MVE(7,158,FILTER(1,3),FFILTER(1,3),1,1)
	CALL MVE(7,158,FILTER(1,4),FFILTER(1,4),1,1)
	CALL MVE(7,158,FILTER(1,5),FFILTER(1,5),1,1)
	CALL MVE(7,158,FILTER(1,6),FFILTER(1,6),1,1)
	CALL MVE(7,158,WVLN,FWVLN,1,1)
C
	RETURN
	END
