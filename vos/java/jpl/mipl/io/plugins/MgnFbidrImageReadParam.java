package jpl.mipl.io.plugins;

import javax.imageio.ImageReadParam;

/**
 * ImageReadParam for MGN F-BIDR's.
 * <p>
 * Currently consists only of logical start and end line, and max number
 * of lines.
 *
 * @author rgd
 */
public class MgnFbidrImageReadParam extends ImageReadParam {
    protected int _minLogicalLine;
    protected boolean _minSet;
    protected int _maxLogicalLine;
    protected boolean _maxSet;
    protected int _maxNumLines;

    public MgnFbidrImageReadParam() {
        _minLogicalLine = 0;
        _minSet = false;
        _maxLogicalLine = 0;
        _maxSet = false;
        _maxNumLines = 50000;   // about as much as mac java can work with
    }
    public void setMinLogicalLine(int min) {
        _minLogicalLine = min;
        _minSet = true;
    }
    public void unsetMin() {
        _minSet = false;
    }
    public boolean isMinSet() {
        return _minSet;
    }
    public int getMinLogicalLine() {
        return _minLogicalLine;
    }

    public void setMaxLogicalLine(int max) {
        _maxLogicalLine = max;
        _maxSet = true;
    }
    public void unsetMax() {
        _maxSet = false;
    }
    public boolean isMaxSet() {
        return _maxSet;
    }
    public int getMaxLogicalLine() {
        return _maxLogicalLine;
    }

    public void setMaxNumLines(int n) {
        _maxNumLines = n;
    }
    public int getMaxNumLines() {
        return _maxNumLines;
    }

}
