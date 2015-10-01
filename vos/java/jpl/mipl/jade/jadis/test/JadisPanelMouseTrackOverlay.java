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
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;

import javax.swing.SwingUtilities;

import jpl.mipl.jade.JadeDisplay;
import jpl.mipl.jade.OverlayPainter;

/**
 * <b>Purpose:</b>
 * Implementation of the JadePanelCursorOverlay and OverlayPainter interface for drawing
 * a plantable cursor on a Jade display component.
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
 * @version $Id: JadePanelMouseTrackOverlay.java,v 1.2 2008/04/08 16:40:36 ntt Exp $
 *
 */
public class JadisPanelMouseTrackOverlay implements OverlayPainter, JadisPanelCursorOverlay
{
    enum CursorStyle {T_CURSOR, T_CURSOR_HOLE, X_CURSOR, X_CURSOR_HOLE};
    protected int _x, _y;
    
    protected Cursor blankCursor, originalCursor;
    protected int SIZE = 10;
    protected JadeDisplay display;
        
    protected boolean cursorVisible = false;    
    protected boolean cursorInImage = true;
    protected boolean alwaysShowCursor = false;
    protected boolean cursorPlanted = false;
    
    //indicates that stereo is enabled, which affects its color so that
    //it is viewable in anaglyph mode
    protected boolean stereoEnabled = false;
    
    protected double cursorScale = 0.0;
    
    //---------------------------------------------------------------------
    
    /**
     * Constructor that accepts a JadeDisplay instance without dimension
     * information.
     * @param jadeDisplay Instance of JadeDisplay where mouse cursor will
     * be tracked
     */
    
    public JadisPanelMouseTrackOverlay(JadeDisplay jadeDisplay)
    {
        this.display = jadeDisplay;
        this._x = this._y = NULL_COORDINATE ;
        
        //create blank cursor for when mouse moves, and cache original
        //for when mouse is planted to a position
        Image cursorImage = Toolkit.getDefaultToolkit().getImage("xparent.gif");
        blankCursor = Toolkit.getDefaultToolkit().createCustomCursor(cursorImage, 
                                new Point( 0, 0), "invisibleCursor" );
        
        originalCursor = this.display.getCursor();        
        this.display.setCursor(blankCursor);
    }
       
    //---------------------------------------------------------------------
    
    /**
     * Returns true if cursor if system cursor is considered visible.
     * @return True if visible.
     */
    
    public boolean isCursorVisible()
    {
        return this.cursorVisible;
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Returns true if cursor if system cursor is set to always be visible.
     * @return True if system pointer always visible.
     */
    
    public boolean isCursorAlwaysVisible()
    {
        return this.alwaysShowCursor;
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Overlay can hide regular system mouse cursor when tracking is active.
     * Calling this method will show or hide the original cursor, depending
     * on the parameter.
     * @param flag True to show the cursor, false to hide
     */
    
    protected void setCursorVisible(boolean flag)
    {
        if (this.display == null)
            return;       
        
        if (this.cursorVisible == flag)
            return;
        
        Cursor cursor = (flag) ? originalCursor : blankCursor;
        
        //System.out.println("DEBUG::Mono set cursor visible: "+flag);
        if (flag)
        {
            this.display.setCursor(cursor);
            this.cursorVisible = true;
        }
        else
        {
            this.display.setCursor(cursor);
            this.cursorVisible = false;
        }
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Toggles cursor planted state
     */
    
    public void toggleCursorPlanted()
    {
        setCursorPlanted(!this.cursorPlanted);
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Plants/unplants mouse cursor based on paramter.
     * @param flag True to plant, false to unplant
     */
    
    public void setCursorPlanted(boolean flag)
    {
        if (flag != this.cursorPlanted)
        {
            this.cursorPlanted = flag;
            determineCursorVisbility();
        }
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Overlay can hide regular mouse cursor when tracking is active.
     * Calling this method will show or hide the original cursor, depending
     * on the parameter.
     * @param flag True to show the cursor, false to hide
     */
    
    public void setCursorAlwaysVisible(boolean flag)
    {
        if (flag != this.alwaysShowCursor)
        {
            this.alwaysShowCursor = flag;
            determineCursorVisbility();
        }        
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Toggles system cursor always visible state
     */
    
    public void toggleCursorAlwaysVisible()
    {
        setCursorAlwaysVisible(!this.alwaysShowCursor);
    }
    
    //---------------------------------------------------------------------
        
    protected void determineCursorVisbility()
    {
        boolean visible = false;
        if (this.alwaysShowCursor)
            visible = true;
        if (this.cursorPlanted)
            visible = true;
        if (!this.cursorInImage)
            visible = true;
        setCursorVisible(visible);
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Notifies this object that the cursor is considered within or outside 
     * of the image bounds.
     * @param inImage True if in image, false is out.
     */
    
    public void setCursorInImage(boolean inImage)
    {
        if (this.cursorInImage != inImage)
        {
            this.cursorInImage = inImage;
            determineCursorVisbility();
        }        
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Sets the coordinates of the cursor to be overlaid on component
     * @param x X-coordiante, NULL_COORDINATE  if not available
     * @param y Y-coordinate, NULL_COORDINATE  if not available
     */
    
    public void setCursorCoordinates(int x, int y)
    {
        if (this.display == null)
            return;
        
        Point new_scroll = this.display.getLocation();
        
        int size = 2 * getScaledSize();
        Rectangle oldR=new Rectangle(_x-size, _y-size, 2*size+1, 2*size+1);
        

        int oldX = _x;
        int oldY = _y;
        this._x = x;
        this._y = y;

        Point origin = this.display.getCurrentImageOrigin();
        
        
        int xPrime = _x + origin.x;// - (int)new_scroll.getX();
        int yPrime = _y + origin.y;// - (int)new_scroll.getY();
        
        Rectangle newR = new Rectangle(xPrime-size, yPrime-size, 2*size+1, 2*size+1);
        
        if (oldX != NULL_COORDINATE  && 
            oldY != NULL_COORDINATE )
            newR.add(oldR);
        
        // Convert back to Viewport coordinates for paintNoErase()
        newR.translate(-origin.x, -origin.y);


        this._x = xPrime;
        this._y = yPrime;
        
        this.display.paintNoErase(newR);        
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Remove references to parent display and any other resources
     * created during life.  Once called, calling other methods will
     * likely result in exceptions.
     */
    
    public void nullify()
    {            
        this.display = null;
        this.blankCursor = null;
        this.originalCursor = null;
    }
    
    //---------------------------------------------------------------------
            
    /**
     * Implementation of the OverlayPainter interface.  Checks the current
     * active coordinates, and if within bounds, draws the marker cursor
     * on the component 
     */
    
    public void paintOverlay(Graphics g)
    {           
        
        Rectangle bounds = g.getClipBounds();

        int size = getScaledSize();
        
        if (bounds != null) 
        {
            if (bounds.x > _x+size)
                return;
            if (bounds.y > _y+size)
                return;
            if (bounds.x + bounds.width <= _x-size)
                return;
            if (bounds.y + bounds.height <= _y-size)
                return;
        }
        
        if (!this.cursorInImage)
            return;
        
        
        Point imageOrigin = this.display.getCurrentImageOrigin();
        
        int imageWidth = this.display.getImage().getWidth();
        int imageHeight = this.display.getImage().getHeight();        
        
        
        
        if (this.stereoEnabled)
            g.setColor(Color.white);
        else
            g.setColor(Color.green);
        
        CursorStyle cStyle = getCursorStyle();
        
        int drawX = _x;
        int drawY = _y;
        
        //we dont want to draw outside of the image
        int xMin = Math.max(drawX - size, imageOrigin.x);
        int xMax = Math.min(drawX + size, imageWidth - 1 + imageOrigin.x);
        int yMin = Math.max(drawY - size, imageOrigin.y);
        int yMax = Math.min(drawY + size, imageHeight - 1 + imageOrigin.y);
        
        //coordinate mapping might indicate that we are in the image
        //when we are in fact right outside of it.  This checks that
        //case and aborts if found to hold
        if (_x > xMax || _y > yMax)
            return;
        
        int xMid = xMin + (size);
        int yMid = yMin + (size);
        
        switch (cStyle)
        {
            case T_CURSOR:
                //draw solid +
                g.drawLine(xMin, drawY, xMax, drawY);
                g.drawLine(drawX, yMin, drawX, yMax);
                break;
                
            case T_CURSOR_HOLE:
                //draw + with hole in center
                g.drawLine(xMin, drawY, xMid - 2, drawY);
                if (xMid+2 < xMax)
                    g.drawLine(xMid + 2, drawY, xMax, drawY);
                
                g.drawLine(drawX, yMin, drawX, yMid-2);
                if (yMid+2 < yMax)
                    g.drawLine(drawX, yMid+2, drawX, yMax);
                break;
                
            case X_CURSOR:
                //draw solid X
                g.drawLine(xMin, yMin, xMax, yMax);
                g.drawLine(xMin, yMax, xMax, yMin);
                break;
                
            case X_CURSOR_HOLE:   
                //draw X with hole in center                
                int lXDelta, rXDelta, uYDelta, lYDelta;
                lXDelta = drawX - xMin;
                rXDelta = xMax - drawX;
                uYDelta = drawY - yMin;
                lYDelta = yMax - drawY;
                
                //upper-left quad
                int minDelta = Math.min(lXDelta, uYDelta);
                g.drawLine(drawX - minDelta, drawY - minDelta, drawX - 2, drawY - 2);
                
                //upper-right quad
                minDelta = Math.min(rXDelta, uYDelta);
                g.drawLine(drawX + 2, drawY - 2, drawX + minDelta, drawY-minDelta);
                
                //lower-left quad
                minDelta = Math.min(lXDelta, lYDelta);
                g.drawLine(drawX - minDelta, drawY + minDelta, drawX - 2, drawY + 2);
                
                //lower-right quad
                minDelta = Math.min(rXDelta, lYDelta);
                g.drawLine(drawX + 2, drawY + 2, drawX+ minDelta, drawY+minDelta);
                
//                g.drawLine(xMin, yMin, xMid - 2, yMid - 2);                
//                g.drawLine(xMid + 2, yMid + 2, xMax, yMax);
//                g.drawLine(xMin, yMax, xMid - 2, yMid + 2);                
//                g.drawLine(xMid + 2, yMid - 2, xMax, yMin);
                break;
                
            default:
                break;
        }
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Enables/disables stereo state
     * @param flag True to enable stereo, false to disable.
     */
    
    public void setStereoEnabled(boolean flag)
    {
        this.stereoEnabled = flag;
    }

    //---------------------------------------------------------------------
    
    /**
     * Set the cursor additive scale.  Allows calling object to associate a 
     * relation between disparity depth and cursor size.
     * @param scale Additive scale factor (0.0 is the default size), expected 
     *        range of 0.0 to 1.0.
     */
    
    public void setCursorScale(double scale)
    {
        if (scale < 0.0)
            scale = -1.0 * scale;
        this.cursorScale = scale;
    }
 
    //---------------------------------------------------------------------
    
    protected int getScaledSize()
    {
        return (int) (SIZE * (1.0 + 5.0 * this.cursorScale));
    }
    
    //---------------------------------------------------------------------
    
    protected CursorStyle getCursorStyle()
    {
        if (this.stereoEnabled)
            return CursorStyle.X_CURSOR_HOLE;
        else
            return CursorStyle.T_CURSOR_HOLE;
    }
    
    //---------------------------------------------------------------------
}
