      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real slat,slon,plat,plon,rpol,requ,range,rlat,rlon,tg,cg,ci,ce,
     *   lumlat,lumlon,rad
      DATA rad/57.2957795/

      call XVMESSAGE(
     .    'this pix vgr:1636832 image space line 500,samp 500',' ')
      slat=.55539604/rad
      slon=169.91901/rad
      plat=-.08596189/rad
      plon=155.07442/rad
      rpol=1815.
      requ=1815.
      range=806022.25
      rlat=-13.8540/rad
      rlon=149.8150/rad
      mode=1
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,0)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer: phase=14.85,inci=24.57,emis=14.75',' ')
      call XVMESSAGE('lumlat=-12.586,lumlon=55.616',' ')

      call XVMESSAGE
     *('this pix is vgr 1634522 image space line 276.5 samp 429.97 ',
     .     ' ')
      slat=.55803038/rad
      slon=12.827187/rad
      plat=1.1882980/rad
      plon=17.688618/rad
      rpol=1815.
      requ=1815.
      range=2612172.5
      rlat=1./RAD
      rlon=17./rad
      first=1
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,first)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer:phase=4.902,inic=4.9156,emiss=0.71436',' ')
      call XVMESSAGE('lumlat=-1.9970,lumlon=-0.95807',' ')
      call XVMESSAGE
     *('this is pix is vgr 1634522 image space line 300 sample 400',' ')
      rlat=19.6076/rad
      rlon=41.2608/RAD
      first=0
      call lumllp(slat,slon,plat,plon,rpol,requ,range,rlat,rlon,
     *  tg,cg,ci,ce,lumlat,lumlon,mode,first)
      tg=rad*(atan(tg))
      cg=rad*(acos(cg))
      ci=rad*(acos(ci))
      ce=rad*(acos(ce))
      call prnt(7,1,tg,    'phase=    .')
      call prnt(7,1,cg,    'phase=    .')
      call prnt(7,1,ci,    'incidence=.')
      call prnt(7,1,ce,    'emission= .')
      call prnt(7,1,lumlat,'lumlat=   .')
      call prnt(7,1,lumlon,'lumlon=   .')
      call XVMESSAGE('answer:phase=4.8850,inci=33.73,emis=29.53',' ')
      call XVMESSAGE('lumlat=-23.42,lumlon=20.972',' ')
      return
      end
