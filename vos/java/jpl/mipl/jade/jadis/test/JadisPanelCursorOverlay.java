// License Terms
// 
// Copyright (c) 2008, California Institute of Technology ("Caltech").
// U.S. Government sponsorship acknowledged.
// 
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// * Neither the name of Caltech nor its operating division, the Jet Propulsion
// Laboratory, nor the names of its contributors may be used to endorse or
// promote products derived from this software without specific prior written
// permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER  OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
// 
/**
 * <b>Purpose:</b>
 * Interface for overlay painter for the Jade component.
 * 
 * <PRE>
 * Copyright 2008, California Institute of Technology
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledged. 2008.
 * </PRE>
 *
 * <PRE>
 * ============================================================================
 * <B>Modification History: </B>
 * ----------------------
 *
 * <B>Date              Who              What</B>
 * ----------------------------------------------------------------------------
 * 04/03/2008        Nick             Initial Release
 * ============================================================================
 * </PRE>
 *
 * @author Nicholas Toole  (Nicholas.T.Toole@jpl.nasa.gov)
 * @version $Id: JadePanelCursorOverlay.java,v 1.1 2008/04/07 18:30:42 ntt Exp $
 *
 */
public interface JadisPanelCursorOverlay
{
    /**
     * Value same as declared in Marsviewer Constants file
     */
    public static final int NULL_COORDINATE = Integer.MIN_VALUE;
    
    //---------------------------------------------------------------------
    
    /**
     * Update the mouse cursor coordiantes
     * @param x x-coordiante
     * @param y y-coordiante
     */
    
    public void setCursorCoordinates(int x, int y);
    
    //---------------------------------------------------------------------
    
    /**
     * External notification that the cursor position is not
     * within the image bounds.
     * @param inImage True if in image, false otherwise
     */
    
    public void setCursorInImage(boolean inImage);
    
    //---------------------------------------------------------------------
    
    /**
     * Requests the cursor to be planted/unplanted from current position
     * @param flag True to plant cursor, false ot unplant
     */
    
    public void setCursorPlanted(boolean flag);

    //---------------------------------------------------------------------
    
    /**
     * External notification that the system mouse pointer should be visible
     * even if mouse cursor is not planted.
     * @param flag True if always visible, false otherwise
     */
    
    public void setCursorAlwaysVisible(boolean flag);
    
    //---------------------------------------------------------------------
    
    /**
     * Release any resources held by this instance.
     */
    public void nullify();

    //---------------------------------------------------------------------
    
    /**
     * Sets the cursor stereo state.  If true, the cursor color should
     * work with anaglpyh and hardware stereo.
     * @param flag True if stereo is enabled, false otherwise
     */
    public void setStereoEnabled(boolean flag);
    
    //---------------------------------------------------------------------
}


