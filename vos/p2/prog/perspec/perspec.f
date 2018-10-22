        include 'VICMAIN_FOR'
c       
        subroutine main44

        implicit none
c
        integer*4 status, def, count
	integer*4 dataformat,ninpfile
        integer*4 rdgr,wrgr,nextgr,putgr,getgr,clgr		!setgr

        real*4    viewelev, viewazi
        real*4    theta, phi, d2r
        real*4    x, y, z,  x2, y2
	real*4    rho, origin(3), zscale
        real*4    sinth, costh, costhcosph, sinthcosph, sinph
        real*4    costhsinph, sinthsinph, cosph

	logical*4 eof, zero
	character*3  dataform
        character*80  inpfile,outfile


        common /perspeccom/ dataformat, origin, zscale, rho,
     +                  sinth, costh, costhcosph, sinthcosph, sinph,
     +                  costhsinph, sinthsinph, cosph


       call ifmessage('PERSPEC version 2-10-2013 (64-bit) - rjb')

c______________________________________________________________________________
c                       start executable code
c------------------------------------------------------------------------------
c
        d2r = acos(-1.0)/180.0     !degree to radian conversion PI/180



        call xvparm('INP',inpfile,count,def,1)
          ninpfile=index(inpfile,'   ') - 1

        call xvp ('DATAFORM', dataform, count)
        if (dataform(1:3) .eq. 'XYZ') then              !X,Y,Z
            dataformat = 1
        else if (dataform(1:3) .eq. 'YXZ') then         !Y,X,Z
            dataformat = 2
        else if (dataform(1:3) .eq. 'LSZ') then         !line,sample,Z
            dataformat = 3
        else
            dataformat = 1                              !default is X,Y,Z
        endif

        call xvp ('ZSCALE', zscale, count )

c        call xvparm ('SCALE', scale, count, def,1 )
c        autoscaling = (def .eq. 1)
c        if (scale .ne. 0.0)  scale = 1.0/scale

        call xvp ('DISTANCE', rho, count)
        call xvp ('ELEV', viewelev, count)
        call xvp ('AZIMUTH', viewazi, count)
        call xvp ('ORIGIN', origin(1), count)
        phi = 90.0 - viewelev
        theta = 90.0 - viewazi

        sinth = sin (theta * d2r)
        costh = cos (theta * d2r)
        sinph = sin (phi * d2r)
        cosph = cos (phi * d2r)
        costhcosph = costh*cosph
        sinthcosph = sinth*cosph
        costhsinph = costh*sinph
        sinthsinph = sinth*sinph

c                       Open the input graphics file
        status = rdgr (1,1,3)
        if (status.ne.1) call signalgr(1,status,1)

c                       If output exists then output 2-D perspective
        call xvparm ('OUT', outfile, count, def,1 )
        if (def .eq. 0) then
            status = wrgr ( 1, 2, 3 )
            if (status.ne.1) call signalgr(2,status,1)
            eof = .false.
            do while (.not. eof)
c                       scan for the beginning of a line string
                status = nextgr (1,eof,x,y,z)
                if (status.ne.1) call signalgr(1,status,1)
                if (eof) go to 199

                zero = .false.
                do while (.not. zero .and. .not. eof)
                    call perspective (x, y, z,  x2, y2)
                    status = putgr (2,x2,y2,0.0)
                    if (status.ne.1) call signalgr(2,status,1)
                    status = getgr ( 1,zero,eof,x,y,z)
                    if (status.ne.1) call signalgr(1,status,1)
                enddo
                status = putgr (2,0.0,0.0,0.0)
                if (status.ne.1) call signalgr(2,status,1)
            enddo
199         continue
            status = clgr (1)
            if (status.ne.1) call signalgr(1,status,1)
            status = clgr (2)

            if (status.ne.1) call signalgr(2,status,1)
            return
        endif


        return
        end

C***********************************************************
        subroutine perspective (x, y, z, x2, y2)
        implicit none
        real    x, y, z,  x2, y2
        real    xp, yp, zp, x_eye, y_eye, z_eye, tmp

        integer dataformat
        real    rho, origin(3), zscale
        real    sinth, costh, costhcosph, sinthcosph, sinph
        real    costhsinph, sinthsinph, cosph

        common /perspeccom/ dataformat, origin, zscale, rho,
     +                  sinth, costh, costhcosph, sinthcosph, sinph,
     +                  costhsinph, sinthsinph, cosph

        xp = x - origin(1)
        yp = y - origin(2)
        zp = (z - origin(3))/zscale
        if (dataformat .eq. 2) then
            tmp = xp
            xp = yp
            yp = tmp
        else if (dataformat .eq. 3) then
            tmp = xp
            xp = yp
            yp = -tmp
        endif
        x_eye = -xp*sinth + yp*costh
        y_eye = -xp*costhcosph - yp*sinthcosph  + zp*sinph
        z_eye = 1.0 - (xp*costhsinph + yp*sinth*sinph + zp*cosph)/rho
        x2 = x_eye/z_eye
        y2 = y_eye/z_eye

        return
        end


