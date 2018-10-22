C
      SUBROUTINE txpicscale( sbuf, data, mp, mptype, sclat, sclon, 
     &               nl, ns, pbuf, line, samp)
      REAL*8    sbuf(200)         !input SPICE buffer
      REAL*4    data(40)          !input projection information
      INTEGER*4 mp                !input projection information
      INTEGER*4 mptype            !input projection information
      REAL*8    sclat
      REAL*8    sclon
      INTEGER*4 nl
      INTEGER*4 ns                !size of input image
      REAL*4    pbuf(30)          !output buffer
      REAL*8    line, samp        !coordinates at which scale applies

      REAL*8    isbuf(200)        !input SPICE buffer
      REAL*4    idata(40)         !input projection information
      INTEGER*4 imp               !input projection information
      INTEGER*4 imptype           !input projection information
      REAL*8    isclat
      REAL*8    isclon
      INTEGER*4 inl
      INTEGER*4 ins               !size of input image
      REAL*4    ipbuf(30)         !output buffer
      INTEGER   I

C    CALL PICSCALE 
      ! Move data to loal declarations
      DO I = 1,200
        isbuf(I) = sbuf(I)         !input SPICE buffer
      END DO

      DO I = 1,40
        idata(I) = data(I)         !input projection information
      END DO

      isclat = sclat
      isclon = sclon
      inl = nl 
      ins = ns                     !size of input image
      
      DO I = 1,30
        ipbuf(I) = pbuf(I)         !input projection information
      END DO

      imptype = mptype             !input projection information
      imp = mp                     !input projection information
      CALL picscale(isbuf,idata,imp,imptype,isclat,isclon,
     &              inl,ins,ipbuf,line,samp) 

C   RETURN
      RETURN
      END
