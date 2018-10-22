      SUBROUTINE SEARC_DISTOR( UNIT, IND)

c  This routine searches the entire label of a VICAR image in order to
c  determine whether it is "Object space" (perspective projection) or
c  "Image space" (contains camera-specific distortion).  Both cases
c  imply that no map labels are present, so it should be called only
c  after verifying that the image does not contain map labels (using
c  MP_LABEL_READ or SEARCV2).  Note that SEARCV2 (on the unported
c  system) performs the same function, but the current version does
c  not work for certain kinds of (very old) VICAR labels.

c  Currently, the criteria for determining that an unlabelled image
c  is Obect space are that it has been processed by a GEOM task or by
c  FARENC.  Thus, all that SEARC_DISTOR does is to search the entire
c  label buffer for these strings.  (This code is borrowed from the
c  old version of SEARCV2.)

c  25jul94 -lwk- initial version

c  Arguments:

c  UNIT (input, integer) = unit number assigned by XVUNIT

c  IND (output, integer) = indicator flag:  
c	IND=0 means Image space
c	IND=1 means Object space
c	IND=-1: label exceeds buffer size
c	IND=-2: error reading label

      INTEGER UNIT
      INTEGER TEMP(6000),BUFSIZE
      CHARACTER*24000 WORK
      equivalence (temp,work)

      CALL ZIA(TEMP,6000)
      BUFSIZE=0
      CALL XLGETLABEL(UNIT,WORK,BUFSIZE,ISTAT)
      IF (BUFSIZE.GT.24000) THEN
	CALL PRNT(4,1,BUFSIZE,'SEARCV2: BYTES IN LABEL = .')
	ind = -1
	return
      ENDIF
      CALL XLGETLABEL(UNIT,WORK,BUFSIZE,ISTAT)
      CALL CHKSTAT( ISTAT,' ERROR IN XLGETLABEL,SEARC_DISTOR', 0, i, 0)
      if (istat.lt.0) then
	ind = -2
	return
      endif

      IF (INDEX(WORK,'GEOM').NE.0 .OR. INDEX(WORK,'FARENC').NE.0) THEN
	ind = 1		! object space
      ELSE
	ind = 0		! image space
      ENDIF

      RETURN
      END
