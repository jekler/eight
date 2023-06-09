package net.yeeyaa.eight.core.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;


public class CoreUtil {
	protected final static char[] hexArray = "0123456789abcdef".toCharArray();
		
	public static byte[] strToBytes(String str, String charset) throws UnsupportedEncodingException {
		if(charset == null) return str.getBytes();
		else return str.getBytes(charset);
	}
	
	public static String uuid(Integer type, Integer length) {
		return new BASE64Encoder(type, length).encode(uuidToBytes(UUID.randomUUID()));
	}
	
	protected static BASE64Encoder encoder = new BASE64Encoder(3, 0);
	protected static BASE64Decoder decoder = new BASE64Decoder(3);	
	
	public static String randomUuid() {
		return encoder.encode(uuidToBytes(UUID.randomUUID()));
	}
	
	public static String encode(byte[] input) {
		return encoder.encode(input);
	}

	public static byte[] decode(String input) {
		return decoder.decode(input);
	}
	
	public static UUID uuid() {
		return UUID.randomUUID();
	}
	
	public static UUID uuid(byte[] name) {
		return UUID.nameUUIDFromBytes(name);
	}
	
	public static UUID strToUuid(String uuid) {
		return bytesToUuid(decoder.decode(uuid));
	}
	
	public static String uuidToStr(UUID uuid) {
		return encoder.encode(uuidToBytes(uuid));
	}
	
	public static UUID uuidFromStr(String str) {
		return UUID.fromString(str);
	}

	public static byte[] uuidToBytes(UUID uuid) {
		byte[] bytes = new byte[16];
		if (uuid != null) {
			long high = uuid.getMostSignificantBits();
			long low = uuid.getLeastSignificantBits();
			for (int i=7; i>=0; i--) {
				bytes[i] = (byte)(high & 0xff);
				high >>= 8;
			}
			for (int i=15; i>7; i--) {
				bytes[i] = (byte)(low & 0xff);
				low >>= 8;
			}
		}
		return bytes;
	}
	
	public static UUID bytesToUuid(byte[] bytes) {
		if (bytes == null || bytes.length < 16) {
			byte[] tmp = new byte[16];
			if (bytes != null && bytes.length > 0) for (int i = 0; i < bytes.length; i++) tmp[i] = bytes[i];
			bytes = tmp;
		}
		long high = 0;
		long low = 0;
        for (int i=0; i<8; i++) high = (high << 8) | (bytes[i] & 0xff);
        for (int i=8; i<16; i++) low = (low << 8) | (bytes[i] & 0xff);
	    return new UUID(high, low);
	}
	
	public static String bytesToStr(byte[] bytes, String charset) throws UnsupportedEncodingException {
		if(charset == null) return new String(bytes);
		else return new String(bytes, charset);
	}

	public static InputStream bytesToInput(byte[] buf){
		return new ByteArrayInputStream(buf);
	}
	
	public static byte[] inputToBytes(InputStream is, Integer buffer, Long maxLength) throws IOException{
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		int nRead;
		byte[] data = new byte[buffer];
		Long count = 0L;
		while ((nRead = is.read(data, 0, data.length)) != -1) {
			if (maxLength >= 0) {
				count += nRead;
				if (count >= maxLength) throw new IOException("Data overflow. The maxLength is " + maxLength);
			}
			buf.write(data, 0, nRead);
		}
		buf.flush();
		return buf.toByteArray();
	}
	
	public static String bytesToHex(byte[] bytes) {
	    char[] hexChars = new char[bytes.length * 2];
	    for ( int j = 0; j < bytes.length; j++ ) {
	        int v = bytes[j] & 0xFF;
	        hexChars[j * 2] = hexArray[v >>> 4];
	        hexChars[j * 2 + 1] = hexArray[v & 0x0F];
	    }
	    return new String(hexChars);
	}
	
	public static byte[] hexToBytes(String hexString) { 
		char[] hexChars = hexString.toUpperCase().toCharArray(); 
		byte[] result = new byte[hexChars.length / 2]; 
		for (int j = 0; j < hexChars.length; j += 2) 
			result[j / 2] = (byte) (Arrays.binarySearch(hexArray, hexChars[j]) * 16 + Arrays.binarySearch(hexArray, hexChars[j + 1])); 
		return result; 
	} 
	
	public static final Object bytesToObject(byte[] bytes) throws IOException, ClassNotFoundException { 
	    ByteArrayInputStream in = new ByteArrayInputStream(bytes); 
	    ObjectInputStream oi = new ObjectInputStream(in); 
	    Object o = oi.readObject(); 
	    oi.close(); 
	    return o; 
	} 

	public static final byte[] objectToBytes(Serializable s) throws IOException { 
	    ByteArrayOutputStream out = new ByteArrayOutputStream(); 
	    ObjectOutputStream ot = new ObjectOutputStream(out); 
	    ot.writeObject(s); 
	    ot.flush(); 
	    ot.close(); 
	    return out.toByteArray(); 
	} 

	public static byte[] md5Encode(byte[] bytes) {
		try {
			return MessageDigest.getInstance("MD5").digest(bytes); 
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
		
	public static byte[] sha256Encode(byte[] bytes) {
		try {
			return MessageDigest.getInstance("SHA-256").digest(bytes); 		
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	public static final Pattern pattern = Pattern.compile("(\\\\u(\\p{XDigit}{4}))"); 
    public static String unicodeToStr(String str) { 
        Matcher matcher = pattern.matcher(str);
        char ch;
        while (matcher.find()) {
            ch = (char) Integer.parseInt(matcher.group(2), 16);
            str = str.replace(matcher.group(1), String.valueOf(ch));    
        }
        return str;
    }
    
    public static String strToUnicode(String str) {
    	StringBuilder result = new StringBuilder();
    	for(int i = 0; i<str.length(); i++) result.append("\\u").append(Integer.toHexString(str.charAt(i))); 
        return result.toString();
    }
    
	public static final byte[] compress(byte[] source, Integer buffer) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream(buffer > 0 ? buffer : -buffer);
		Deflater deflater = null;
		OutputStream gos = buffer > 0 ? new DeflaterOutputStream(out, deflater = new Deflater(Deflater.DEFAULT_COMPRESSION, true)) : new GZIPOutputStream(out); 
		try {
			gos.write(source); 
		} finally {
			try {
				gos.close();
			} finally {
				if (deflater != null) deflater.end();
			}
		}
		return out.toByteArray();
	}
	
	public static final byte[] decompress(byte[] source, Integer buffer) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream(buffer > 0 ? buffer : -buffer);
		Inflater inflater = null;
		InputStream gis = buffer > 0 ? new InflaterInputStream(new ByteArrayInputStream(source), inflater = new Inflater(true)) : new GZIPInputStream(new ByteArrayInputStream(source));
		try {
			byte[] temp = new byte[buffer > 0 ? buffer : -buffer];
			int size = 0;
			while ((size = gis.read(temp)) != -1) out.write(temp, 0, size);	
		} finally {
			try {
				gis.close();
			} finally {
				if (inflater != null) inflater.end();
			}
		}
		return out.toByteArray();
	}
		
	public static URL strToUrl(String filepath) throws MalformedURLException{
		try {
			return new URL(filepath);
		} catch (MalformedURLException e) {
			return new File(filepath).toURI().toURL();
		}
	}
	
	public static byte[] urlToBytes(URL url, Integer buffer, Long maxLength){
        InputStream is = null;
        byte[] ret = null;
        try {
            is = new BufferedInputStream(url.openStream());
            ByteArrayOutputStream outstream = new ByteArrayOutputStream(buffer);
            byte[] b = new byte[buffer];
            int len;
			Long count = 0L;
            while ((len = is.read(b)) != -1) {
            	if (maxLength >= 0) {
            		count += len;
            		if (count > maxLength) throw new IOException("Data overflow. The maxLength is " + maxLength); 
            	}
            	outstream.write(b, 0, len);           
            }
            ret = outstream.toByteArray();
        } catch (Exception e){
			throw new RuntimeException(e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){new RuntimeException(e);} }
        }
        return ret;   
	}
	
	public static void bytesToFile(byte[] bytes, String filepath){
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(filepath);
			fos.write(bytes);
		} catch (Exception e) {
			throw new RuntimeException(e);
		} finally {
            if(fos!=null) {try{fos.close();} catch(Exception e){throw new RuntimeException(e);} }
        }
	}
	
	public static String getRegex(String input){

		return input.replace("`a", "~").replace("`z", "`");
	}
    
    public static Collection<?> asCollection(Object array){
    	if(array != null) if(array instanceof Collection) return (Collection<Object>) array;
    	else if(Map.class.isInstance(array)) return ((Map<Object, Object>) array).entrySet();
    	else if(array instanceof Object[]) return Arrays.asList((Object[])array);
    	else if(array.getClass().isArray()){
    		List<Object> ret = new ArrayList<Object>(Array.getLength(array));
    		for (int i = 0; i < Array.getLength(array); i ++) ret.add(Array.get(array, i));
    		return ret;
    	}else {
    		List<Object> ret = new LinkedList<Object>();
    		ret.add(array);
    		return ret;
    	}else return Collections.EMPTY_SET;
    }
    
    public static Object[] toArray(Collection<Object> col, Class<?> type){
    	Object[] ret = new Object[0];
    	if(col != null) {
    		if(type == null) ret = new Object[col.size()];
    		else {
    			if(type.isPrimitive()) type = PlatformUtil.toWrapper(type);
    			ret = (Object[])Array.newInstance(type, col.size());
    		}
    		col.toArray(ret);
    	}
    	return ret;
    }
    
	public static Object decimalAuto(BigDecimal in) {
		if (in.scale() == 0) {
			try {
				Long l = in.longValueExact();
				if (l < Byte.MAX_VALUE && l > Byte.MIN_VALUE) return in.byteValue();
				else if (l < Short.MAX_VALUE && l > Short.MIN_VALUE) return in.shortValue();
				else if (l < Integer.MAX_VALUE && l > Integer.MIN_VALUE) return in.shortValue();
				else return l;
			} catch (Exception e) {
				return in.toBigInteger();
			}
		} else {
			Double d = in.doubleValue();
			if (d < Float.MAX_VALUE && d > Float.MIN_VALUE) return in.floatValue();
			else return d;
		}
	}
    
	public static Object decimal(BigDecimal in, Boolean exact, Integer type) {
		switch (type) {
			case 0: return exact ? in.intValueExact() : in.intValue();
			case 1: return exact ? in.longValueExact() : in.longValue();
			case 2: return exact ? in.shortValueExact() : in.shortValue();
			case 3: return exact ? in.byteValueExact() : in.byteValue();
			case 4: return exact ? in.toBigIntegerExact() : in.toBigInteger();
			case 5: return in.doubleValue();
			case 6: return in.floatValue();
			case 7: return in.unscaledValue();
			case 8: return in.toString();
			case 9: return in.toPlainString();		
			case 10: return in.toEngineeringString();		
			default: return in;
		}
	}
	
	public static List<Object> flatList(Object in) {
		List<Object> ls = new LinkedList<Object>();
		if(in != null) merge(ls, in);
		return ls;
	}
	
	protected static void merge(List<Object> c, Object in){
		if(in != null) if(in instanceof Collection) for(Object o : (Collection) in) c.add(o);
		else if(in.getClass().isArray()) for (int i = 0; i < Array.getLength(in); i ++) c.add(Array.get(in, i));
		else c.add(in);
	}
	
	public static String toString(Object key){
		if(key == null) return null;
		else if(key instanceof String) return (String) key;
		else if(key instanceof Enum) return ((Enum)key).getDeclaringClass().getSimpleName() + '.' + ((Enum)key).name();
		else if(key instanceof Class) return ((Class<?>)key).getName();
		else return key.toString();
	}
}
