package net.yeeyaa.eight.osgi.loader.config;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PushbackReader;
import java.io.Writer;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.core.util.CoreUtil;


public class ConfigHandler{
	protected static final byte GZIP_MAGIC = 31;
	protected static final byte JAR_MAGIC = 80;
	protected static final byte CLZ_MAGIC = -54;	
	protected static final String ENCODING = "UTF-8";
    protected static final int TOKEN_NAME = 'N';
    protected static final int TOKEN_EQ = '=';
    protected static final int TOKEN_ARR_OPEN = '[';
    protected static final int TOKEN_ARR_CLOS = ']';
    protected static final int TOKEN_VEC_OPEN = '(';
    protected static final int TOKEN_VEC_CLOS = ')';
    protected static final int TOKEN_COMMA = ',';
    protected static final int TOKEN_VAL_OPEN = '"'; 
    protected static final int TOKEN_VAL_CLOS = '"'; 
    protected static final int TOKEN_SIMPLE_STRING = 'T';
    protected static final int TOKEN_SIMPLE_INTEGER = 'I';
    protected static final int TOKEN_SIMPLE_LONG = 'L';
    protected static final int TOKEN_SIMPLE_FLOAT = 'F';
    protected static final int TOKEN_SIMPLE_DOUBLE = 'D';
    protected static final int TOKEN_SIMPLE_BYTE = 'X';
    protected static final int TOKEN_SIMPLE_SHORT = 'S';
    protected static final int TOKEN_SIMPLE_CHARACTER = 'C';
    protected static final int TOKEN_SIMPLE_BOOLEAN = 'B';
    protected static final int TOKEN_PRIMITIVE_INT = 'i';
    protected static final int TOKEN_PRIMITIVE_LONG = 'l';
    protected static final int TOKEN_PRIMITIVE_FLOAT = 'f';
    protected static final int TOKEN_PRIMITIVE_DOUBLE = 'd';
    protected static final int TOKEN_PRIMITIVE_BYTE = 'x';
    protected static final int TOKEN_PRIMITIVE_SHORT = 's';
    protected static final int TOKEN_PRIMITIVE_CHAR = 'c';
    protected static final int TOKEN_PRIMITIVE_BOOLEAN = 'b';
    protected static final String CRLF = "\r\n";
    protected static final String INDENT = "  ";
    protected static final String COLLECTION_LINE_BREAK = " \\\r\n";
    protected static final Map code2Type;
    protected static final Map type2Code;
    private static final BitSet NAME_CHARS;
    private static final BitSet TOKEN_CHARS;

    static{
        type2Code = new HashMap();
        type2Code.put( Integer.class, new Integer( TOKEN_SIMPLE_INTEGER ) );
        type2Code.put( Long.class, new Integer( TOKEN_SIMPLE_LONG ) );
        type2Code.put( Float.class, new Integer( TOKEN_SIMPLE_FLOAT ) );
        type2Code.put( Double.class, new Integer( TOKEN_SIMPLE_DOUBLE ) );
        type2Code.put( Byte.class, new Integer( TOKEN_SIMPLE_BYTE ) );
        type2Code.put( Short.class, new Integer( TOKEN_SIMPLE_SHORT ) );
        type2Code.put( Character.class, new Integer( TOKEN_SIMPLE_CHARACTER ) );
        type2Code.put( Boolean.class, new Integer( TOKEN_SIMPLE_BOOLEAN ) );
        type2Code.put( Integer.TYPE, new Integer( TOKEN_PRIMITIVE_INT ) );
        type2Code.put( Long.TYPE, new Integer( TOKEN_PRIMITIVE_LONG ) );
        type2Code.put( Float.TYPE, new Integer( TOKEN_PRIMITIVE_FLOAT ) );
        type2Code.put( Double.TYPE, new Integer( TOKEN_PRIMITIVE_DOUBLE ) );
        type2Code.put( Byte.TYPE, new Integer( TOKEN_PRIMITIVE_BYTE ) );
        type2Code.put( Short.TYPE, new Integer( TOKEN_PRIMITIVE_SHORT ) );
        type2Code.put( Character.TYPE, new Integer( TOKEN_PRIMITIVE_CHAR ) );
        type2Code.put( Boolean.TYPE, new Integer( TOKEN_PRIMITIVE_BOOLEAN ) );
        code2Type = new HashMap();
        for ( Iterator ti = type2Code.entrySet().iterator(); ti.hasNext(); ) {
            Map.Entry entry = ( Map.Entry ) ti.next();
            code2Type.put( entry.getValue(), entry.getKey() );
        }
        code2Type.put( new Integer( TOKEN_SIMPLE_STRING ), String.class );
        NAME_CHARS = new BitSet();
        for ( int i = '0'; i <= '9'; i++ ) NAME_CHARS.set( i );
        for ( int i = 'a'; i <= 'z'; i++ ) NAME_CHARS.set( i );
        for ( int i = 'A'; i <= 'Z'; i++ ) NAME_CHARS.set( i );
        NAME_CHARS.set( '_' );
        NAME_CHARS.set( '-' );
        NAME_CHARS.set( '.' );
        NAME_CHARS.set( '\\' );
        TOKEN_CHARS = new BitSet();
        TOKEN_CHARS.set( TOKEN_EQ );
        TOKEN_CHARS.set( TOKEN_ARR_OPEN );
        TOKEN_CHARS.set( TOKEN_ARR_CLOS );
        TOKEN_CHARS.set( TOKEN_VEC_OPEN );
        TOKEN_CHARS.set( TOKEN_VEC_CLOS );
        TOKEN_CHARS.set( TOKEN_COMMA );
        TOKEN_CHARS.set( TOKEN_VAL_OPEN );
        TOKEN_CHARS.set( TOKEN_VAL_CLOS );
        TOKEN_CHARS.set( TOKEN_SIMPLE_STRING );
        TOKEN_CHARS.set( TOKEN_SIMPLE_INTEGER );
        TOKEN_CHARS.set( TOKEN_SIMPLE_LONG );
        TOKEN_CHARS.set( TOKEN_SIMPLE_FLOAT );
        TOKEN_CHARS.set( TOKEN_SIMPLE_DOUBLE );
        TOKEN_CHARS.set( TOKEN_SIMPLE_BYTE );
        TOKEN_CHARS.set( TOKEN_SIMPLE_SHORT );
        TOKEN_CHARS.set( TOKEN_SIMPLE_CHARACTER );
        TOKEN_CHARS.set( TOKEN_SIMPLE_BOOLEAN );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_INT );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_LONG );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_FLOAT );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_DOUBLE );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_BYTE );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_SHORT );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_CHAR );
        TOKEN_CHARS.set( TOKEN_PRIMITIVE_BOOLEAN );
    }

    public static void write( OutputStream out, Hashtable properties ) throws IOException {
        BufferedWriter bw = new BufferedWriter( new OutputStreamWriter( out, ENCODING ) );
        for ( Enumeration ce = orderedKeys(properties); ce.hasMoreElements(); ) {
            String key = ( String ) ce.nextElement();
            writeQuoted( bw, key );
            bw.write( TOKEN_EQ );
            writeValue( bw, properties.get( key ) );
            bw.write( CRLF );
        }
        bw.flush();
    }

    private static Enumeration orderedKeys(Hashtable properties) {
        String[] keyArray = new String[properties.size()];
        int i = 0;
        for ( Enumeration ce = properties.keys(); ce.hasMoreElements(); ) {
            keyArray[i] = ( String ) ce.nextElement();
            i++;
        }
        Arrays.sort(keyArray);
        return Collections.enumeration( Arrays.asList( keyArray ) );
    }

    public static Hashtable read( InputStream ins ) throws IOException {
        return new ConfigHandler().readInternal( ins );
    }

    private ConfigHandler(){}
    private int token;
    private String tokenValue;
    private int line;
    private int pos;

    private Hashtable readInternal( InputStream ins ) throws IOException {
        BufferedReader br = new BufferedReader( new InputStreamReader( ins, ENCODING ) );
        PushbackReader pr = new PushbackReader( br, 1 );
        token = 0;
        tokenValue = null;
        line = 0;
        pos = 0;
        Hashtable configuration = new Hashtable();
        while ( nextToken( pr ) == TOKEN_NAME ) {
            String key = tokenValue;
            if ( nextToken( pr ) != TOKEN_EQ ) throw readFailure( token, TOKEN_EQ );
            Object value = readValue( pr );
            if ( value != null ) configuration.put( key, value );
        }
        return configuration;
    }

    private Object readValue( PushbackReader pr ) throws IOException{
        int type = read( pr );
        int code;
        if ( code2Type.containsKey( new Integer( type ) ) ) code = read( pr );
        else {
            code = type;
            type = TOKEN_SIMPLE_STRING;
        }
        switch ( code ){
            case TOKEN_ARR_OPEN: return readArray( type, pr );
            case TOKEN_VEC_OPEN: return readCollection( type, pr );
            case TOKEN_VAL_OPEN: Object value = readSimple( type, pr );
                ensureNext( pr, TOKEN_VAL_CLOS );
                return value;
            default: return null;
        }
    }

    private Object readArray( int typeCode, PushbackReader pr ) throws IOException {
        List list = new ArrayList();
        for ( ;; ) {
            int c = ignorablePageBreakAndWhiteSpace( pr );
            if ( c == TOKEN_VAL_OPEN ) {
                Object value = readSimple( typeCode, pr );
                if ( value == null ) return null;
                ensureNext( pr, TOKEN_VAL_CLOS );
                list.add( value );
                c = ignorablePageBreakAndWhiteSpace( pr );
            }
            if ( c == TOKEN_ARR_CLOS ) {
                Class type = ( Class ) code2Type.get( new Integer( typeCode ) );
                Object array = Array.newInstance( type, list.size() );
                for ( int i = 0; i < list.size(); i++ ) Array.set( array, i, list.get( i ) );
                return array;
            } else if ( c < 0 || c != TOKEN_COMMA) return null;
        }
    }

    private Collection readCollection( int typeCode, PushbackReader pr ) throws IOException {
        Collection collection = new ArrayList();
        for ( ;; ) {
            int c = ignorablePageBreakAndWhiteSpace( pr );
            if ( c == TOKEN_VAL_OPEN ) {
                Object value = readSimple( typeCode, pr );
                if ( value == null ) return null;
                ensureNext( pr, TOKEN_VAL_CLOS );
                collection.add( value );
                c = ignorablePageBreakAndWhiteSpace( pr );
            }
            if ( c == TOKEN_VEC_CLOS ) return collection;
            else if ( c < 0 || c != TOKEN_COMMA ) return null;
        }
    }

    private Object readSimple( int code, PushbackReader pr ) throws IOException {
        switch ( code ) {
            case -1: return null;
            case TOKEN_SIMPLE_STRING: return readQuoted( pr );
            case TOKEN_SIMPLE_INTEGER:
            case TOKEN_PRIMITIVE_INT: return Integer.valueOf( readQuoted( pr ) );
            case TOKEN_SIMPLE_LONG:
            case TOKEN_PRIMITIVE_LONG: return Long.valueOf( readQuoted( pr ) );
            case TOKEN_SIMPLE_FLOAT:
            case TOKEN_PRIMITIVE_FLOAT: int fBits = Integer.parseInt( readQuoted( pr ) );
                return new Float( Float.intBitsToFloat( fBits ) );
            case TOKEN_SIMPLE_DOUBLE:
            case TOKEN_PRIMITIVE_DOUBLE: long dBits = Long.parseLong( readQuoted( pr ) );
                return new Double( Double.longBitsToDouble( dBits ) );
            case TOKEN_SIMPLE_BYTE:
            case TOKEN_PRIMITIVE_BYTE: return Byte.valueOf( readQuoted( pr ) );
            case TOKEN_SIMPLE_SHORT:
            case TOKEN_PRIMITIVE_SHORT: return Short.valueOf( readQuoted( pr ) );
            case TOKEN_SIMPLE_CHARACTER:
            case TOKEN_PRIMITIVE_CHAR: String cString = readQuoted( pr );
                if ( cString != null && cString.length() > 0 ) return new Character( cString.charAt( 0 ) );
                return null;
            case TOKEN_SIMPLE_BOOLEAN:
            case TOKEN_PRIMITIVE_BOOLEAN: return Boolean.valueOf( readQuoted( pr ) );
            default: return null;
        }
    }

    private void ensureNext( PushbackReader pr, int expected ) throws IOException {
        int next = read( pr );
        if ( next != expected ) readFailure( next, expected );
    }

    private String readQuoted( PushbackReader pr ) throws IOException {
        StringBuffer buf = new StringBuffer();
        for ( ;; ) {
            int c = read( pr );
            switch ( c ) {
                case '\\':
                    c = read( pr );
                    switch ( c ) {
                        case 'b': buf.append( '\b' );
                            break;
                        case 't': buf.append( '\t' );
                            break;
                        case 'n': buf.append( '\n' );
                            break;
                        case 'f': buf.append( '\f' );
                            break;
                        case 'r': buf.append( '\r' );
                            break;
                        case 'u': char[] cbuf = new char[4];
                            if ( read( pr, cbuf ) == 4 ) {
                                c = Integer.parseInt( new String( cbuf ), 16 );
                                buf.append( ( char ) c );
                            }
                            break;
                        default: buf.append( ( char ) c );
                    }
                    break;
                case -1:
                case TOKEN_EQ:
                case TOKEN_VAL_CLOS: pr.unread( c );
                    return buf.toString();
                default: buf.append( ( char ) c );
            }
        }
    }

    private int nextToken( PushbackReader pr ) throws IOException {
        int c = ignorableWhiteSpace( pr );
        if ( c < 0 ) return ( token = c );
        if ( NAME_CHARS.get( c ) || !TOKEN_CHARS.get( c ) ) {
            pr.unread( c );
            tokenValue = readQuoted( pr );
            return ( token = TOKEN_NAME );
        }
        if ( TOKEN_CHARS.get( c ) ) return ( token = c );
        return ( token = -1 );
    }

    private int ignorableWhiteSpace( PushbackReader pr ) throws IOException {
        int c = read( pr );
        while ( c >= 0 && Character.isWhitespace( ( char ) c ) ) c = read( pr );
        return c;
    }

    private int ignorablePageBreakAndWhiteSpace( PushbackReader pr ) throws IOException {
        int c = ignorableWhiteSpace( pr );
        for ( ;; ) {
            if ( c != '\\' ) break;
            int c1 = pr.read();
            if ( c1 == '\r' || c1 == '\n' ) c = ignorableWhiteSpace( pr );
            else {
                pr.unread(c1);
                break;
            }
        }
        return c;
    }

    private int read( PushbackReader pr ) throws IOException {
        int c = pr.read();
        if ( c == '\r' ){
            int c1 = pr.read();
            if ( c1 != '\n' ) pr.unread( c1 );
            c = '\n';
        }
        if ( c == '\n' ){
            line++;
            pos = 0;
        } else pos++;
        return c;
    }

    private int read( PushbackReader pr, char[] buf ) throws IOException {
        for ( int i = 0; i < buf.length; i++ ) {
            int c = read( pr );
            if ( c >= 0 ) buf[i] = ( char ) c;
            else return i;
        }
        return buf.length;
    }

    private IOException readFailure( int current, int expected ) {
        return new IOException( "Unexpected token " + current + "; expected: " + expected + " (line=" + line + ", pos=" + pos + ")" );
    }

    private static void writeValue( Writer out, Object value ) throws IOException {
        Class clazz = value.getClass();
        if ( clazz.isArray() ) writeArray( out, value );
        else if ( value instanceof Collection ) writeCollection( out, ( Collection ) value );
        else {
            writeType( out, clazz );
            writeSimple( out, value );
        }
    }

    private static void writeArray( Writer out, Object arrayValue ) throws IOException {
        int size = Array.getLength( arrayValue );
        writeType( out, arrayValue.getClass().getComponentType() );
        out.write( TOKEN_ARR_OPEN );
        out.write( COLLECTION_LINE_BREAK );
        for ( int i = 0; i < size; i++ ) writeCollectionElement(out, Array.get( arrayValue, i ));
        out.write( INDENT );
        out.write( TOKEN_ARR_CLOS );
    }

    private static void writeCollection( Writer out, Collection collection ) throws IOException {
        if ( collection.isEmpty() ) {
            out.write( TOKEN_VEC_OPEN );
            out.write( COLLECTION_LINE_BREAK );
            out.write( TOKEN_VEC_CLOS );
        } else {
            Iterator ci = collection.iterator();
            Object firstElement = ci.next();
            writeType( out, firstElement.getClass() );
            out.write( TOKEN_VEC_OPEN );
            out.write( COLLECTION_LINE_BREAK );
            writeCollectionElement( out, firstElement );
            while ( ci.hasNext() ) writeCollectionElement( out, ci.next() );
            out.write( TOKEN_VEC_CLOS );
        }
    }

    private static void writeCollectionElement(Writer out, Object element) throws IOException {
        out.write( INDENT );
        writeSimple( out, element );
        out.write( TOKEN_COMMA );
        out.write(COLLECTION_LINE_BREAK);
    }

    private static void writeType( Writer out, Class valueType ) throws IOException {
        Integer code = ( Integer ) type2Code.get( valueType );
        if ( code != null ) out.write( ( char ) code.intValue() );
    }

    private static void writeSimple( Writer out, Object value ) throws IOException {
        if ( value instanceof Double ) {
            double dVal = ( ( Double ) value ).doubleValue();
            value = new Long( Double.doubleToRawLongBits( dVal ) );
        } else if ( value instanceof Float ) {
            float fVal = ( ( Float ) value ).floatValue();
            value = new Integer( Float.floatToRawIntBits( fVal ) );
        }
        out.write( TOKEN_VAL_OPEN );
        writeQuoted( out, String.valueOf( value ) );
        out.write( TOKEN_VAL_CLOS );
    }

    private static void writeQuoted( Writer out, String simple ) throws IOException {
        if ( simple == null || simple.length() == 0 ) return;
        char c = 0;
        int len = simple.length();
        for ( int i = 0; i < len; i++ ) {
            c = simple.charAt( i );
            switch ( c ) {
                case '\\':
                case TOKEN_VAL_CLOS:
                case ' ':
                case TOKEN_EQ: out.write( '\\' );
                    out.write( c );
                    break;
                case '\b': out.write( "\\b" );
                    break;
                case '\t': out.write( "\\t" );
                    break;
                case '\n': out.write( "\\n" );
                    break;
                case '\f': out.write( "\\f" );
                    break;
                case '\r': out.write( "\\r" );
                    break;
                default: if ( c < ' ' ) {
                        String t = "000" + Integer.toHexString( c );
                        out.write( "\\u" + t.substring( t.length() - 4 ) );
                    } else out.write( c );
            }
        }
    }

	public static void main(String[] args) {
		if (args != null && args.length > 0) try {
			byte[] bin = CoreUtil.urlToBytes(CoreUtil.strToUrl(args[0]), 8192, -1L);
			if (args.length > 2) {
				Boolean[] flag = new Boolean[1];
				byte[] content = decode(new String(bin, "UTF-8"), flag);
				if (flag[0] == null) CoreUtil.bytesToFile(content, args[1] + ".jar");
				else {
		    		if (flag[0]) CoreUtil.bytesToFile(content, args[1] + ".class");
		    		else if ("stdout".equals(args[2])) System.out.println(new String(content,"UTF-8"));
		    		else CoreUtil.bytesToFile(content, args[1]);
				}
			} else {
				String content = encode(bin);
				if (args.length > 1) CoreUtil.bytesToFile(content.getBytes("UTF-8"), args[1]);
				else System.out.println(content);
			}
		} catch (Exception e) {
			e.printStackTrace();
		} else {
			System.out.println("useage -> compress: [inpiutFilePath] ([outputFilePath] or SystemOut) || decompress [inpiutFilePath] [outputFilePath] [output -> stdout: SystemOut : other: outputFile]");
		}
	}
	

	public static byte[] decode(String content, Boolean[] flag) throws Exception {
		byte[] bin = CoreUtil.decode(content);
		if (bin[0] == JAR_MAGIC) flag[0] = null;
		else {
			if (GZIP_MAGIC == bin[0]) bin = CoreUtil.decompress(bin, -8192);
			flag[0] = CLZ_MAGIC == bin[0];
		}
		return bin;
	}
	
	public static String encode(Object object) {
		try {
			byte[] bin = object instanceof byte[] ? (byte[]) object : object.toString().getBytes("UTF-8");
			if (bin[0] != 80) bin = CoreUtil.compress(bin, -8192);
			return CoreUtil.encode(bin);
		} catch (Exception e) {
			return null;
		}
	}
}
