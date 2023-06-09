package net.yeeyaa.eight.osgi.loader.config;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.osgi.framework.Version;

public class VersionRange implements Serializable {
	private static final long serialVersionUID = 4926756322513987331L;
    protected static final Pattern FUZZY_VERSION = Pattern.compile("(\\d+)(\\.(\\d+)(\\.(\\d+))?)?([^a-zA-Z0-9](.*))?", Pattern.DOTALL);
	public static final Version INFINITE_VERSION = new Version(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, "");
    public static final VersionRange ANY_VERSION = new VersionRange(false, Version.emptyVersion, INFINITE_VERSION, true);
    public static final int EXACT = 0;
    public static final int MICRO = 1;
    public static final int MINOR = 2;
    public static final int MAJOR = 3;
    public static final int ANY   = 40;
    protected static boolean[] removeable;
    static {
        removeable = new boolean[256];
        for (int i = 0; i < 256; i++) {
            removeable[i] = Character.isWhitespace(i);
        }
        removeable['"'] = true;
    }

    protected final boolean openFloor;
    protected final Version floor;
    protected final Version ceiling;
    protected final boolean openCeiling;
    
    public VersionRange( boolean openFloor, Version floor, Version ceiling, boolean openCeiling ) {
        this.openFloor = openFloor;
        this.floor = floor;
        this.ceiling = ceiling;
        this.openCeiling = openCeiling;
        checkRange();
    }

    public VersionRange( Version atLeast ) {
        this( atLeast, false );
    }

    public VersionRange( Version atLeast, boolean exact ) {
        this.openFloor = false;
        this.floor = atLeast;
        this.ceiling = exact ? atLeast : INFINITE_VERSION;
        this.openCeiling = exact ? false : true;
        checkRange();
    }


    public VersionRange( String val ) throws IllegalArgumentException, NumberFormatException {
        this( val, false );
    }

    public VersionRange( String val, boolean exact ) throws IllegalArgumentException, NumberFormatException {
        this( val, exact, true );
    }

    public VersionRange( String val, boolean exact, boolean clean ) throws IllegalArgumentException, NumberFormatException {
        val = removeQuotesAndWhitespaces(val);
        int fst = val.charAt( 0 );
        if ( fst == '[' ) openFloor = false;
        else if ( fst == '(' ) openFloor = true;
        else {
            openFloor = false;
            floor = getVersion( val, clean );
            ceiling = exact ? floor : INFINITE_VERSION;
            openCeiling = exact ? false : true;
            return;
        }
        int lst = val.charAt( val.length() - 1 );
        if ( lst == ']' ) openCeiling = false;
        else if ( lst == ')' ) openCeiling = true;
        else throw new IllegalArgumentException( "illegal version range syntax " + val + ": range must end in ')' or ']'" );
        int comma = val.indexOf( ',' );
        if ( comma < 0 ) throw new IllegalArgumentException( "illegal version range syntax " + "no comma" );
        if ( val.indexOf( ',', comma + 1 ) > 0 ) throw new IllegalArgumentException( "illegal version range syntax " + "too many commas" );
        String strFloor = val.substring( 1, comma );
        String strCeil = val.substring( comma + 1, val.length() - 1 );
        floor = getVersion( strFloor, clean );
        ceiling = "*".equals( strCeil ) ? INFINITE_VERSION : getVersion( strCeil, clean );
        checkRange();
    }
    
    protected String removeQuotesAndWhitespaces(String val) {
        for (int i = 0, l = val.length(); i < l; i++) {
            char ch = val.charAt(i);
            if (isRemoveable(ch)) {
                StringBuilder sb = new StringBuilder(l);
                sb.append(val, 0, i);
                for (i++; i < l; i++) {
                    ch = val.charAt(i);
                    if (!isRemoveable(ch)) sb.append(ch);
                }
                return sb.toString();
            }
        }
        return val;
    }

    protected boolean isRemoveable(char ch) {
        return ch < 256 ? removeable[ch] : Character.isWhitespace(ch);
    }

    public Version getCeiling() {
        return ceiling;
    }

    public Version getFloor() {
        return floor;
    }

    public boolean isOpenCeiling() {
        return openCeiling;
    }

    public boolean isOpenFloor() {
        return openFloor;
    }

    public boolean isPointVersion() {
        return !openFloor && !openCeiling && floor.equals( ceiling );
    }

    public boolean contains( Version version ) {
        if ( version.equals( INFINITE_VERSION )) return ceiling.equals( INFINITE_VERSION );
        else return ( version.compareTo( floor ) > 0 && version.compareTo( ceiling ) < 0 ) || ( !openFloor && version.equals( floor ) ) || ( !openCeiling && version.equals( ceiling ) );
    }

    public VersionRange intersect(VersionRange r) {
        final Version newFloor;
        final boolean newOpenFloor;
        int minCompare = floor.compareTo(r.getFloor());
        if (minCompare > 0) {
            newFloor = floor;
            newOpenFloor = openFloor;
        } else if (minCompare < 0) {
            newFloor = r.getFloor();
            newOpenFloor = r.isOpenFloor();
        } else {
            newFloor = floor;
            newOpenFloor = (openFloor || r.isOpenFloor());
        }
        final Version newCeiling;
        final boolean newOpenCeiling;
        int maxCompare = ceiling.compareTo(r.getCeiling());
        if (maxCompare < 0) {
            newCeiling = ceiling;
            newOpenCeiling = openCeiling;
        } else if (maxCompare > 0) {
            newCeiling = r.getCeiling();
            newOpenCeiling = r.isOpenCeiling();
        } else {
            newCeiling = ceiling;
            newOpenCeiling = (openCeiling || r.isOpenCeiling());
        }
        VersionRange result;
        if (isRangeValid(newOpenFloor, newFloor, newCeiling, newOpenCeiling)) result = new VersionRange(newOpenFloor, newFloor, newCeiling, newOpenCeiling);
        else result = null;
        return result;
    }

    protected void checkRange() {
        if (!isRangeValid(openFloor, floor, ceiling, openCeiling)) throw new IllegalArgumentException("invalid version range: " + makeString(openFloor, floor, ceiling, openCeiling));
    }

    public int hashCode() {
        int result = 1;
        result = 11 * result + ( ( ceiling == null ) ? 0 : ceiling.hashCode() );
        result = 13 * result + ( ( floor == null ) ? 0 : floor.hashCode() );
        result = 17 * result + ( openCeiling ? 1231 : 1237 );
        result = 19 * result + ( openFloor ? 1231 : 1237 );
        return result;
    }

    public boolean equals( Object obj ) {
        if ( this == obj ) return true;
        if ( obj == null ) return false;
        if ( getClass() != obj.getClass() ) return false;
        final VersionRange other = ( VersionRange ) obj;
        if ( ceiling == null ) {
            if ( other.ceiling != null ) return false;     
        } else if ( !ceiling.equals( other.ceiling ) ) return false;
        if ( floor == null ) {
            if ( other.floor != null ) return false;
        } else if ( !floor.equals( other.floor ) ) return false;
        if ( openCeiling != other.openCeiling ) return false;
        if ( openFloor != other.openFloor ) return false;
        return true;
    }

    public String toString() {
        if ( ANY_VERSION.equals( this ) ) return makeString( openFloor, Version.emptyVersion, INFINITE_VERSION, openCeiling );
        return makeString( openFloor, floor, ceiling, openCeiling );
    }

    protected String makeString( boolean openFloor, Version floor, Version ceiling, boolean openCeiling ) {
        StringBuffer vr = new StringBuffer( 32 );
        if ( INFINITE_VERSION.equals( ceiling ) ) vr.append( Version.emptyVersion.equals( floor ) ? "0" : floor.toString() );
        else {
            vr.append( openFloor ? "(" : "[" );
            String floorStr = Version.emptyVersion.equals( floor ) ? "0" : floor.toString();
            String ceilingStr = ceiling.toString();
            vr.append( floorStr ).append( "," ).append( ceilingStr );
            vr.append( openCeiling ? ")" : "]" );
        }
        return vr.toString();
    }
    
    protected static boolean isRangeValid(boolean openFloor, Version floor, Version ceiling, boolean openCeiling) {
        boolean result;
        int compare = floor.compareTo(ceiling);
        if (compare > 0) result = false;
        else if (compare == 0 && (openFloor || openCeiling)) result = false;
        else result = true;
        return result;
    }
    
    public static Version getVersion(String version, boolean clean) {
        if (clean) version = clean(version);
        Version v = Version.parseVersion(version);
        return v;
    }
    
    public static String clean(String version) {
        if (version == null || version.length() == 0) return "0.0.0";
        String clean = fastSyntax(version);
        if (clean != null) return clean;
        StringBuffer result = new StringBuffer();
        Matcher m = FUZZY_VERSION.matcher(version);
        if (m.matches()) {
            String major = m.group(1);
            String minor = m.group(3);
            String micro = m.group(5);
            String qualifier = m.group(7);
            if (major != null) {
                result.append(major);
                if (minor != null) {
                    result.append(".");
                    result.append(minor);
                    if (micro != null) {
                        result.append(".");
                        result.append(micro);
                        if (qualifier != null) {
                            result.append(".");
                            cleanupModifier(result, qualifier);
                        }
                    } else if (qualifier != null) {
                        result.append(".0.");
                        cleanupModifier(result, qualifier);
                    } else result.append(".0");
                } else if (qualifier != null) {
                    result.append(".0.0.");
                    cleanupModifier(result, qualifier);
                } else result.append(".0.0");
            }
        } else {
            result.append("0.0.0.");
            cleanupModifier(result, version);
        }
        return result.toString();
    }

    protected static void cleanupModifier(StringBuffer result, String modifier) {
        for (int i = 0; i < modifier.length(); i++) {
            char c = modifier.charAt(i);
            if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '-') result.append(c);
            else result.append('_');
        }
    }

    protected static String fastSyntax(String version) {
        int state = 0;
        for (int i = 0, l = version.length(); i < l; i++) {
            char ch = version.charAt(i);
            switch (state) {
	            case 0:
	            case 2:
	            case 4: if (ch < '0' || ch > '9') return null;
		                state++;
		                break;
	            case 1:
	            case 3:
	            case 5: if (ch == '.') state++; 
	            		else if (ch < '0' || ch > '9') return null;
	            		break;
	            case 6: if (ch == '.') return null;
            }
        }
        switch (state) {
	        case 0:
	        case 1: return version + ".0.0";
	        case 2:
	        case 3: return version + ".0";
	        default: return version;
        }
    }
    
    public static VersionRange parseVersionRange( String val ) throws IllegalArgumentException, NumberFormatException {
        if ( val == null || val.trim().length() == 0 ) return ANY_VERSION;
        return new VersionRange( val );
    }
}
