package net.yeeyaa.eight.core.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;
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


public class TypeConvertor extends CoreUtil implements IProcessor<Object, Object>{
	protected static final Logger logger = LoggerFactory.getLogger(TypeConvertor.class);
	protected final Logger log;
	protected final static char[] hexArray = "0123456789abcdef".toCharArray();
	
	public enum Method{strToBytes, bytesToStr, bytesToInput, inputToBytes, bytesToStrAuto, bytesToHex, hexToBytes, bytesToObject, objectToBytes, md5Encode, 
		unicodeToStr, strToUnicode, compress, decompress, strToUrl, urlToStr, storageToStr, urlToBytes, storageToBytes, randomUuid, uuidFromStr, uuidToStr, 
		strToUuid, uuidToBytes, bytesToUuid, sha256Encode, base64Encode, base64Decode, getRegex, getPinyin, asCollection, toArray, flatList, uuid, toString, 
		parseEntity, decimal, decimalAuto, streamToStream, urlToUrl, storageToStream, resourceToStream, encode, decode};	
	
	public TypeConvertor() {
		this.log = LoggerFactory.getLogger(TypeConvertor.class);
	}
	
	public TypeConvertor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TypeConvertor.class) : log;
	}
	
	public static byte[] md5Encode(byte[] bytes, Logger log){ 
	    try { 
	    	return MessageDigest.getInstance("MD5").digest(bytes); 
	    } catch (NoSuchAlgorithmException e) { 
			log.error("TypeConvertor : md5Encode perform error.", e);
	    	return new byte[0]; 
	    } 
	}
	
	public static byte[] sha256Encode(byte[] bytes, Logger log){ 
	    try { 
	    	return MessageDigest.getInstance("SHA-256").digest(bytes); 
	    } catch (NoSuchAlgorithmException e) { 
			log.error("TypeConvertor : sha256Encode perform error.", e);
	    	return new byte[0]; 
	    } 
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
	
	public static byte[] urlToBytes(URL url, Integer buffer, Long maxLength, Logger log){
        byte[] ret = null;
        try {
            ret = urlToBytes(url, buffer, maxLength);
        } catch (Exception e){
			log.error("TypeConvertor : urlToBytes perform error.", e);
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
			log.error("TypeConvertor : storageToBytes perform error.", e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
        return ret;   
	}
	
	public static void bytesToFile(byte[] bytes, String filepath, Logger log){
		try {
			bytesToFile(bytes, filepath);
		} catch (Exception e) {
			log.error("TypeConvertor : bytesToFile perform error.", e);
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
			log.error("TypeConvertor : bytesToStorage perform error.", e);
		} finally {
            if(os!=null) {try{os.close();} catch(Exception e){log.error("TypeConvertor : bytesToStorage perform error.", e);} }
        }
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
			log.error("TypeConvertor : getParas perform error.", e);
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
				else if("long".equals(result[0]))obj = new Long(result[1]);
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
			log.error("TypeConvertor : parseEntity perform error.", e);
		}
    	return obj;
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
			case randomUuid : return randomUuid();
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
			case uuidToStr: return uuidToStr((UUID)instance);
			case strToUuid: return strToUuid(instance.toString());

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
