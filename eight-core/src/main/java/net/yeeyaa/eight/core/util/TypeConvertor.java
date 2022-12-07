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
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
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

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TypeConvertor implements IProcessor<Object, Object>{
	protected static final Logger logger = LoggerFactory.getLogger(TypeConvertor.class);
	protected final Logger log;
	protected final static char[] hexArray = "0123456789abcdef".toCharArray();
	
	public enum Method{strToBytes, bytesToStr, bytesToInput, inputToBytes, bytesToStrAuto, bytesToHex, hexToBytes, bytesToObject, objectToBytes, md5Encode, 
		unicodeToStr, strToUnicode, compress, decompress, strToUrl, urlToStr, storageToStr, urlToBytes, storageToBytes, uuidFromStr, uuidToBytes, bytesToUuid,
		sha256Encode, base64Encode, base64Decode, getRegex, getPinyin, asCollection, toArray, flatList, uuid, toString, parseEntity, decimal, decimalAuto, 
		streamToStream, urlToUrl, storageToStream, resourceToStream, encode, decode};	
	
	public TypeConvertor() {
		this.log = LoggerFactory.getLogger(TypeConvertor.class);
	}
	
	public TypeConvertor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TypeConvertor.class) : log;
	}
		
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

	public static byte[] decode(String input) throws IOException {
		return decoder.decode(input);
	}
	
	public static UUID uuid() {
		return UUID.randomUUID();
	}
	
	public static UUID uuid(byte[] name) {
		return UUID.nameUUIDFromBytes(name);
	}
	
	public static UUID strToUuid(String uuid) throws IOException {
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
		return md5Encode(bytes, logger);
	}
	
	public static byte[] md5Encode(byte[] bytes, Logger log){ 
	    try { 
	    	return MessageDigest.getInstance("MD5").digest(bytes); 
	    } catch (NoSuchAlgorithmException e) { 
			log.error("TypeConvertor : md5Encode perform error.", e);
	    	return new byte[0]; 
	    } 
	}
	
	public static byte[] sha256Encode(byte[] bytes) {
		return sha256Encode(bytes, logger);
	}
	
	public static byte[] sha256Encode(byte[] bytes, Logger log){ 
	    try { 
	    	return MessageDigest.getInstance("SHA-256").digest(bytes); 
	    } catch (NoSuchAlgorithmException e) { 
			log.error("TypeConvertor : sha256Encode perform error.", e);
	    	return new byte[0]; 
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
    
	public static final byte[] compress(byte[] source, Integer buffer, IProcessor<OutputStream, OutputStream> processor) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream(buffer > 0 ? buffer : -buffer);
		Deflater deflater = null;
		OutputStream gos = processor == null ? buffer > 0 ? new DeflaterOutputStream(out, deflater = new Deflater(Deflater.DEFAULT_COMPRESSION, true)) : new GZIPOutputStream(out) : processor.process(out); 
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
	
	public static final byte[] decompress(byte[] source, Integer buffer, IProcessor<InputStream, InputStream> processor) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream(buffer > 0 ? buffer : -buffer);
		Inflater inflater = null;
		InputStream gis = processor == null ? buffer > 0 ? new InflaterInputStream(new ByteArrayInputStream(source), inflater = new Inflater(true)) : new GZIPInputStream(new ByteArrayInputStream(source)) : processor.process(new ByteArrayInputStream(source));
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

	public static byte[] urlToBytes(URL url, Integer buffer, Long maxLength) {
		return urlToBytes(url, buffer, maxLength, logger);
	}
	
	public static byte[] urlToBytes(URL url, Integer buffer, Long maxLength, Logger log){
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
			log.error("TypeConvertor : transaction perform error.", e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
        return ret;   
	}

	public static byte[] storageToBytes(IExtendable<Object> storage, Integer buffer, Long maxLength) {
		return storageToBytes(storage, buffer, maxLength, logger);
	}
	
	public static byte[] storageToBytes(IExtendable<Object> storage, Integer buffer, Long maxLength, Logger log){
        InputStream is = null;
        byte[] ret = null;
        try {
			is = new BufferedInputStream(storage.<InputStream>extend(Storage.Method.input));
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
			log.error("TypeConvertor : transaction perform error.", e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
        return ret;   
	}
	
	public static void bytesToFile(byte[] bytes, String filepath) {
		bytesToFile(bytes, filepath, logger);
	}
	
	public static void bytesToFile(byte[] bytes, String filepath, Logger log){
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(filepath);
			fos.write(bytes);
		} catch (Exception e) {
			log.error("TypeConvertor : transaction perform error.", e);
		} finally {
            if(fos!=null) {try{fos.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
	}
	
	public static void bytesToStorage(byte[] bytes, IExtendable<Object> storage) {
		bytesToStorage(bytes, storage);
	}

	public static void bytesToStorage(byte[] bytes, IExtendable<Object> storage, Logger log){
		OutputStream os = null;
		try {
			os = storage.<OutputStream>extend(Storage.Method.output);
			os.write(bytes);
		} catch (Exception e) {
			log.error("TypeConvertor : transaction perform error.", e);
		} finally {
            if(os!=null) {try{os.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
	}
	
	public static String getRegex(String input){
		//%: super splitter #:system complex splitter  ~:complex splitter  @:data transfer(usally frontend structure) and extra splitter  ':':simple splitter (common not use)  ';':backup splitter |($._-/etc): self define splitter with other usage, can use with simple splitter
		return input.replace("`a", "~")/*.replace("`b", "#").replace("`c", "@").replace("`d", "|").replace("`e", ":")*/.replace("`z", "`");
	}
    
	public static Object getParas(IProcessor<Object, Object> factory, Map<String, String> paras, String key, String regex) {
		return getParas(factory, paras, key, regex, logger);
	}
	
    public static Object getParas(IProcessor<Object, Object> factory, Map<String, String> paras, String key, String regex, Logger log){
    	Object obj = null;
		if(paras.containsKey(key)) try{
			String[] inputset = paras.get(key).split(regex);
			if(inputset.length > 0){
				Object in = factory.process(inputset[0]);
				if(in instanceof IInputResource) {
					String[] para = new String[inputset.length - 1];
					for(int i = 0; i < para.length; i++) para[i] = inputset[i + 1]; 
					obj = ((IInputResource<String, Object>)in).find(para);
				}
			}
		}catch(Exception e){
			log.error("TypeConvertor : transaction perform error.", e);
		}
		return obj;
    }
    
    public static Object parseEntity(IProcessor<Object, Object> factory, String paras, String regex) {
    	return parseEntity(factory, paras, regex, logger);
    }
    
    public static Object parseEntity(IProcessor<Object, Object> factory, String paras, String regex, Logger log){
    	Object obj = null;
    	if(paras != null) try{
    		String[] result= paras.split(regex);
    		obj = result[0];
    		if(result.length > 1){
				if("boolean".equals(result[0])) obj = new Boolean(result[1]);
				else if("integer".equals(result[0])) obj = new Integer(result[1]);
				else if("double".equals(result[0]))obj = new Double(result[1]);
				else if("float".equals(result[0]))obj = new Float(result[1]);
				else if("short".equals(result[0]))obj = new Short(result[1]);
				else if("Long".equals(result[0]))obj = new Long(result[1]);
				else if("byte".equals(result[0]))obj = new Byte(result[1]);
				else if("uuid".equals(result[0]))obj = UUID.fromString(result[1]);
				else if("date".equals(result[0]))obj = new Strtotime().getTime(result[1]);
				else if("string".equals(result[0]))obj = result[1];
				else if("null".equals(result[0]))obj = null;
				else if("notnull".equals(result[0]))obj = new Object(){
					@Override
					public boolean equals(Object obj) {
						if(obj == null) return false;
						else return true;
					}				
				};
				else {
					Object i = factory.process(result[0]);
					if(i instanceof IInputResource) obj = ((IInputResource<String, Object>)i).find(result[1]);
					else obj = result[1];
				}
    		}
    	}catch(Exception e){
			log.error("TypeConvertor : transaction perform error.", e);
		}
    	return obj;
    }
    
    public static Collection<?> asCollection(Object array){
    	if(array != null) if(Collection.class.isInstance(array)) return (Collection<Object>) array;
    	else if(Map.class.isInstance(array)) return ((Map<Object, Object>) array).entrySet();
    	else if(array instanceof Object) return Arrays.asList((Object[])array);
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
	
	public static URL urlToUrl(URL url, IProcessor<URL, byte[]> convertor) {
		try {
			final byte[] content = convertor.process(url);
			return new URL("byte", url.getHost(), url.getPort(), url.getFile(), new URLStreamHandler(){
				@Override
				protected URLConnection openConnection(URL u) throws IOException {
					return new URLConnection(u){
						@Override
						public void connect() throws IOException {}    
						
						@Override
						public InputStream getInputStream() throws IOException {
							return new ByteArrayInputStream(content);
	        }};}});
		} catch (MalformedURLException e) {
			throw new PlatformException(PlatformError.ERROR_PARAMETERS, e);
		}
	}
	
	public static Object storageToStream(final IExtendable<Object> storage, final IProcessor<Object, Object> convertor, int mode) {
		if (mode == 0) if (storage instanceof IExtendable) return convertor.process(((IExtendable<Object>)storage).<OutputStream>extend(Storage.Method.output));
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		else return convertor.process(storage.<InputStream>extend(Storage.Method.input));
	}
	
	public static URL streamToStream(final URL url, final IProcessor<InputStream, InputStream> convertor) {
		try {
			return new URL("stream", url.getHost(), url.getPort(), url.getFile(), new URLStreamHandler(){
				@Override
				protected URLConnection openConnection(URL u) throws IOException {
					return new URLConnection(url){
						protected URLConnection con = url.openConnection();
						
						@Override
						public void connect() throws IOException {
							con.connect();
						}    
						
						@Override
						public InputStream getInputStream() throws IOException {
							return convertor.process(con.getInputStream());
	        }};}});
		} catch (MalformedURLException e) {
			throw new PlatformException(PlatformError.ERROR_PARAMETERS, e);
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
	
	protected Method type;
	protected Class<?> clazz;
	protected String strPara = "utf-8";
	protected Integer intPara = 8192;
	protected volatile Object objPara;
	
	public void setObjPara(Object objPara) {
		this.objPara = objPara;
	}

	public void setStrPara(String strPara) {
		this.strPara = strPara;
	}

	public void setIntPara(Integer intPara) {
		this.intPara = intPara;
	}

	public void setType(Method type) {
		this.type = type;
	}
	
	public void setClass(Class<?> type) {
		clazz = type;
	}
	
	@Override
	public Object process(Object instance) {
		if(instance != null) switch(type){
			case decimal: if (instance instanceof BigDecimal) return decimal((BigDecimal)instance, Boolean.TRUE.equals(objPara), intPara);
			break;
			case decimalAuto: if (instance instanceof BigDecimal) return decimalAuto((BigDecimal)instance);
			break;
			case urlToUrl: if (instance instanceof URL) return urlToUrl((URL) instance, (IProcessor<URL, byte[]>)objPara);
			break;
			case streamToStream: if (instance instanceof URL) return streamToStream((URL) instance, (IProcessor<InputStream, InputStream>)objPara);
			break;
			case storageToStream: if (instance instanceof IExtendable) return storageToStream((IExtendable<Object>) instance, (IProcessor<Object, Object>)objPara, intPara);
			break;
			case parseEntity: if (instance == null) return null;
			else return parseEntity((IProcessor<Object, Object>) objPara, instance.toString(), strPara, log);
			case uuid: if (intPara == 1 && instance instanceof byte[]) return uuid((byte[]) instance);
			else return uuid();
			case asCollection: return asCollection(instance);
			case toArray: if(instance instanceof Collection) return toArray((Collection<Object>)instance, clazz);
			break;
			case decode: try{
				return decode(instance.toString());
			}catch(Exception e){
				log.error("TypeConvertor : base64Decode perform error.", e);
				break;
			}
			case base64Decode: if (!(objPara instanceof BASE64Decoder)) objPara = new BASE64Decoder(intPara);
			try{
				if(instance instanceof InputStream) return ((BASE64Decoder) objPara).decode((InputStream) instance);
				else return ((BASE64Decoder) objPara).decode(instance.toString());
			}catch(Exception e){
				log.error("TypeConvertor : base64Decode perform error.", e);
				break;
			}
			case encode: if (instance instanceof byte[]) return encode((byte[])instance);
			break;
			case base64Encode: if (!(objPara instanceof BASE64Encoder)) if (objPara instanceof Integer) objPara = new BASE64Encoder(intPara, (Integer)objPara);
			else objPara = new BASE64Encoder(intPara, 0);
			try{
				if(instance instanceof byte[]) return ((BASE64Encoder) objPara).encode((byte[]) instance);
				else if(instance instanceof ByteBuffer) return ((BASE64Encoder) objPara).encode((ByteBuffer) instance);
				else return ((BASE64Encoder) objPara).encode(strToBytes(instance.toString(), strPara));
			}catch(Exception e){
				log.error("TypeConvertor : base64Encode perform error.", e);
				break;
			}
			case bytesToUuid: try{
				if(instance instanceof byte[]) return bytesToUuid((byte[]) instance);
				else return bytesToUuid(strToBytes(instance.toString(), strPara));
			}catch(Exception e){
				log.error("TypeConvertor : bytesToUuid perform error.", e);
				break;
			}	
			case uuidToBytes: if(instance instanceof UUID) return uuidToBytes((UUID)instance);
			break;
			case bytesToInput: if(instance instanceof byte[]) return bytesToInput((byte[])instance);
			break;
			case inputToBytes: try{
				if(instance instanceof InputStream) return inputToBytes((InputStream)instance, intPara, objPara == null ? -1L : new Long(objPara.toString()));
				break;
			}catch(Exception e){
				log.error("TypeConvertor : bytesToHexString perform error.", e);
				break;
			}			
			case bytesToHex: try{
				if(instance instanceof byte[]) return bytesToHex((byte[])instance);
				else return bytesToHex(strToBytes(instance.toString(), strPara));
			}catch(Exception e){
				log.error("TypeConvertor : bytesToHexString perform error.", e);
				break;
			}  						
			case hexToBytes: return hexToBytes(instance.toString());
			case bytesToObject: try{
				if(instance instanceof byte[]) return bytesToObject((byte[])instance);
				else return bytesToObject(strToBytes(instance.toString(), strPara));
			}catch(Exception e){
				log.error("TypeConvertor : bytesToObject perform error.", e);
				break;
			}
			case objectToBytes: try{
				if(instance instanceof Serializable) return objectToBytes((Serializable)instance);
				break;
			}catch(Exception e){
				log.error("TypeConvertor : objectToBytes perform error.", e);
				break;
			}
			case compress: try{
				if(instance instanceof byte[]) return compress((byte[])instance, intPara, (IProcessor<OutputStream, OutputStream>) objPara);
				else return compress(strToBytes(instance.toString(), strPara), intPara, (IProcessor<OutputStream, OutputStream>) objPara);
			}catch(Exception e){
				log.error("TypeConvertor : compress perform error.", e);
				break;
			}
			case decompress: try{
				if(instance instanceof byte[]) return decompress((byte[])instance, intPara, (IProcessor<InputStream, InputStream>) objPara);
				else return decompress(strToBytes(instance.toString(), strPara), intPara, (IProcessor<InputStream, InputStream>) objPara);
			}catch(Exception e){
				log.error("TypeConvertor : decompress perform error.", e);
				break;
			}
			case uuidFromStr: return uuidFromStr(instance.toString());
			case strToUnicode: return strToUnicode(instance.toString());
			case unicodeToStr: return unicodeToStr(instance.toString());
			case getRegex: return getRegex(instance.toString());
			case md5Encode: try{
				if(instance instanceof byte[]) return md5Encode((byte[])instance, log);
				else return md5Encode(strToBytes(instance.toString(), strPara), log);
			}catch(Exception e){
				log.error("TypeConvertor : md5Encode perform error.", e);
				break;
			}
			case sha256Encode: try{
				if(instance instanceof byte[]) return sha256Encode((byte[])instance, log);
				else return sha256Encode(strToBytes(instance.toString(), strPara), log);
			}catch(Exception e){
				log.error("TypeConvertor : sha256Encode perform error.", e);
				break;
			}
			case flatList: return flatList(instance);
			case toString: return toString(instance);
			case bytesToStr: try{
				if(instance instanceof byte[]) return bytesToStr((byte[])instance, strPara);
				break;
			}catch(Exception e){
				log.error("TypeConvertor : bytesToStr perform error.", e);
				break;
			}  				
			case storageToBytes: if(instance instanceof IExtendable) return storageToBytes((IExtendable<Object>)instance, intPara, objPara == null ? -1L : new Long(objPara.toString()), log);
				break;
			case strToBytes: try{
				return strToBytes(instance.toString(), strPara);
			}catch(Exception e){
				log.error("TypeConvertor : strToBytes perform error.", e);
				break;
			}  			
			case strToUrl: try{
				return strToUrl(instance.toString());
			}catch(Exception e){
				log.error("TypeConvertor : strToUrl perform error.", e);
				break;
			}  			
			case urlToBytes: if(instance instanceof URL) return urlToBytes((URL)instance, intPara, objPara == null ? -1L : new Long(objPara.toString()), log);
		}
		return null;
	}
}