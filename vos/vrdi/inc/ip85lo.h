/*
  	DeAnza IP8500 LO-res specific
 */

#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#ifdef 	IP85LO_INITIALIZE
#define 	PRIVATE	globaldef noshare
#else
#define 	PRIVATE	globalref
#endif
#endif

#if UNIX_OS
#define PRIVATE
#endif

/* Buffers for the DeAnza level 1 driver */
#define VOCBUF (DCB[*Unit]->DeviceDependent[0])		/* short vocbuf[32] */

globalref int cursorshape[7][256];	/* array containing cursor shapes */

#define IP85LO_X_DEV(x)	((x)-1)
#define IP85LO_DEV_X(x)	((x)+1)
#define IP85LO_Y_DEV(y) (N_LINES-(y))
#define IP85LO_DEV_Y(y) (N_LINES-(y))

/* DeAnza driver level 1 calls */

/* Be very careful when using the MEM_* routines.  They are riddled with   */
/* bugs, so many are bypassed and the hardware is hit directly.  Also, the */
/* memory register buffer is kept internally in the MEMCTL routine, so is  */
/* NOT shared between processes.  Therefore, the buffer is usually invalid */
/* due to the lack of a RMC (read memory register) call!  Basically, you   */
/* can use any of these functions that modify only system registers by     */
/* calling MEM_SOFT_REG first.  If the routine uses a memory register, make*/
/* SURE you don't care about any of the other bits in the modified register*/
/* before using this routine!						   */

#define MEM_SOFT_REG(lun,chan,oper) \
	memctl(&0,lun,chan,oper)
/* NOTE:  MEMCTL function 1 has a bug where the values are written to the */
/* incorrect hardware registers.  This is gotten around by using function */
/* 1 to write to the soft registers, then calling memctl function 0 to    */
/* write them out to the hardware.  So MEM_SOFT_REG with an argument of 2 */
/* must be called after calling MEM_CHAN_CHAR.				  */
#define MEM_CHAN_CHAR(lun,chan,graphc,intrlc) \
	memctl(&-1,lun,chan,graphc,intrlc)
#define MEM_ZOOM_SCROLL(lun,chan,xscr,yscr,xzm,yzm,xbe,ybe) \
	memctl(&2,lun,chan,xscr,yscr,xzm,yzm,xbe,ybe)
#define MEM_ITT(lun,chan,sect,vocen,ditten) \
	memctl(&3,lun,chan,sect,vocen,ditten)
#define MEM_IRS(lun,chan,irsen) \
	memctl(&4,lun,chan,irsen)
#define MEM_BIT_PLANE_MASK(lun,chan,membpm) \
	memctl(&5,lun,chan,membpm)
#define MEM_FROM_DVP(lun,chan,dvpbpm,ditten,dvpwen,membus) \
	memctl(&6,lun,chan,dvpbpm,ditten,dvpwen,membus)
#define MEM_TO_DVP(lun,chan,srcdev,bussel) \
	memctl(&7,lun,chan,srcdev,bussel)
#define MEM_ACCESS_WINDOW(lun,xres,yres,xamin,xamax,yamin,yamax,memsel) \
	memctl(&8,lun,xres,yres,xamin,xamax,yamin,yamax,memsel)
#define MEM_LOG_COORD(lun,xmin,xmax,xt,ymin,ymax,yt) \
	memctl(&9,lun,xmin,xmax,xt,ymin,ymax,yt)
#define MEM_ADDR_MODE(lun,dx,dy,mode,pas,modop) \
	memctl(&10,lun,dx,dy,mode,pas,modop)
#define MEM_PIXEL_DEPTH(lun,cxc,cmrbuf) \
	memctl(&11,lun,cxc,cmrbuf)
#define MEM_DVP_MODE(lun,modea,start) \
	memctl(&12,lun,modea,start)
#define MEM_DVP_START(lun,xdest,ydest,xindex,yindex) \
	memctl(&13,lun,xdest,ydest,xindex,yindex)
#define MEM_BROADCAST(lun,chans,xscr,yscr,xzm,yzm,xbe,ybe) \
	memctl(&14,lun,chans,xscr,yscr,xzm,yzm,xbe,ybe)

#define MEMRD_ITT(lun,chan,lutadr,count,buffer) \
	memrd(&1,lun,chan,lutadr,count,buffer)
#define MEMRD_IMAGE(lun,cont,x,y,cc,count,buffer) \
	memrd(&2,lun,cont,x,y,cc,count,buffer)
#define MEMRD_SEQ(lun,cont,x,y,chan,count,buffer) \
	memrd(&6,lun,cont,x,y,chan,count,buffer)

#define MEMWR_ITT(lun,chan,lutadr,count,buffer) \
	memwr(&1,lun,chan,lutadr,count,buffer)
#define MEMWR_IMAGE(lun,cont,x,y,cc,count,buffer) \
	memwr(&2,lun,cont,x,y,cc,count,buffer)
#define MEMWR_RECT(lun,chan,intens) \
	memwr(&3,lun,chan,intens)
#define MEMWR_RECT_ALL(lun,intens,reg) \
	memwr(&4,lun,intens,reg)
#define MEMWR_VECTOR(lun,intens,cmrn,count,xybuf,reg) \
	memwr(&5,lun,intens,cmrn,count,xybuf,reg)
#define MEMWR_SEQ(lun,cont,x,y,chan,count,buffer) \
	memwr(&6,lun,cont,x,y,chan,count,buffer)

#define VOC_CURSOR(lun,vocbuf,curen) \
	vocctl(&1,lun,vocbuf,curen)
#define VOC_ALPHA(lun,vocbuf,alnuen,anmode) \
	vocctl(&2,lun,vocbuf,alnuen,anmode)
#define VOC_GRAPHICS(lun,vocbuf,gren,gmode) \
	vocctl(&3,lun,vocbuf,gren,gmode)
#define VOC_SPLIT_SCREEN(lun,vocbuf,xyen,xsplad,ysplad) \
	vocctl(&4,lun,vocbuf,xyen,xsplad,ysplad)
#define VOC_DVP(lun,vocbuf,rdbk,rdbkse) \
	vocctl(&5,lun,vocbuf,rdbk,rdbkse)
#define VOC_FCR(lun,vocbuf,splt,outch,inch,by,lutsel) \
	vocctl(&6,lun,vocbuf,splt,outch,inch,by,lutsel)
#define VOC_LOAD_REG(lun,vocbuf,streg,size,ubuf) \
	vocctl(&7,lun,vocbuf,streg,size,ubuf)
#define VOC_READ_REG(lun,vocbuf,streg,size,ubuf) \
	vocctl(&8,lun,vocbuf,streg,size,ubuf)

#define VOCRD_IMAGE_LUT(lun,vocbuf,lut,bywd,stad,size,ubuf) \
	vocrd(&1,lun,vocbuf,lut,bywd,stad,size,ubuf)
#define VOCRD_OVERLAY_LUT(lun,vocbuf,stad,size,ubuf) \
	vocrd(&2,lun,vocbuf,stad,size,ubuf)

#define VOCWR_IMAGE_LUT(lun,vocbuf,lut,bywd,stad,size,ubuf) \
	vocwr(&1,lun,vocbuf,lut,bywd,stad,size,ubuf)
#define VOCWR_OVERLAY_LUT(lun,vocbuf,stad,size,ubuf) \
	vocwr(&2,lun,vocbuf,stad,size,ubuf)

