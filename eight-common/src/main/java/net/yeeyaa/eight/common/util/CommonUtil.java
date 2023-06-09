package net.yeeyaa.eight.common.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import net.sourceforge.pinyin4j.PinyinHelper;
import net.sourceforge.pinyin4j.format.HanyuPinyinCaseType;
import net.sourceforge.pinyin4j.format.HanyuPinyinOutputFormat;
import net.sourceforge.pinyin4j.format.HanyuPinyinToneType;
import net.sourceforge.pinyin4j.format.HanyuPinyinVCharType;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.mozilla.intl.chardet.nsDetector;
import org.mozilla.intl.chardet.nsICharsetDetectionObserver;
import org.mozilla.intl.chardet.nsPSMDetector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.framework.Advised;
import org.springframework.aop.support.AopUtils;
import org.springframework.core.io.Resource;
import org.springframework.core.io.WritableResource;


public class CommonUtil implements IProcessor<Object, Object>{
	protected static final Logger logger = LoggerFactory.getLogger(CommonUtil.class);
	protected final Logger log;
	public static enum MethodType {getRealObject, getRealClass, resourceToStream, getPinyin, bytesToStrAuto, storageToStr, urlToStr};
	
	protected static class Detector extends nsDetector{
		protected Observer observer;
		
		protected Detector(int charset) {
			super(charset);
			observer = new Observer();
			Init(observer);
		}
		
		protected class Observer implements nsICharsetDetectionObserver{
			protected String charset = "utf-8";

			@Override
			public void Notify(String charset) {
				this.charset = charset; 
			}
		}
	}
	
	public static String bytesToStr(byte[] bytes) {
		return bytesToStr(bytes, logger);
	}
	
	public static String bytesToStr(byte[] bytes, Logger log) {
        try {
        	Detector det = new Detector(nsPSMDetector.CHINESE);
			if(!det.isAscii(bytes,bytes.length)) det.DoIt(bytes,bytes.length, false);           
            return new String(bytes, det.observer.charset);
        } catch (Exception e){
			log.error("CommonUtil : transaction perform error.", e);
        }
		return new String(bytes);
	}
	
	public static <T> T getRealObject(T in){
		return getRealObject(in, logger); 
	}
	
	public static <T> T getRealObject(T in, Logger log){
		if(in instanceof IUniversal) in = (T)((IUniversal)in).realObject();
		if(in instanceof Advised && AopUtils.isAopProxy(in)) try{
			in = (T)((Advised)in).getTargetSource().getTarget();
		}catch(Exception e){
			log.error("CommonUtil: getRealObject perform error.", e);
		}
		return in;
	}

	public static String urlToStr(URL url, Integer buffer, Long maxLength) {
		return urlToStr(url, buffer, maxLength, logger);
	}
	
	public static String urlToStr(URL url, Integer buffer, Long maxLength, Logger log){
        InputStream is = null;
        String ret = null;
        try {
        	Detector det = new Detector(nsPSMDetector.CHINESE);
            is = new BufferedInputStream(url.openStream());
            ByteArrayOutputStream outstream = new ByteArrayOutputStream(buffer);
            byte[] b = new byte[buffer];
            int len;
			boolean done = false;
			Long count = 0L;
            while ((len = is.read(b)) != -1) {
            	if (maxLength >= 0) {
            		count += len;
            		if (count > maxLength) throw new IOException("Data overflow. The maxLength is " + maxLength); 
            	}
                if (!done && !det.isAscii(b,len)) done = det.DoIt(b,len, false);
                outstream.write(b, 0, len);
            }            
            ret = new String(outstream.toByteArray(), det.observer.charset);
        } catch (Exception e){
			log.error("TypeConvertor : transaction perform error.", e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
        return ret;   
	}

	public static Object resourceToStream(final Resource resource, String path, final IProcessor<Object, Object> convertor, int mode) {
		try {
			if (mode == 0) if (resource instanceof WritableResource) return convertor.process(((WritableResource)resource).getOutputStream());
			else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else if (mode == 1) return convertor.process(resource.getURL());
			else if (mode == 2) return convertor.process(resource.getURI());
			else if (mode == 3) return convertor.process(resource.getFile());
			else if (mode == 4) return convertor.process(resource.createRelative(path));
			else return convertor.process(resource.getInputStream());
		} catch (IOException e) {
			throw new PlatformException(PlatformError.ERROR_IO, e);
		}
	}
	
	public static String storageToStr(IExtendable<Object> storage, Integer buffer, Long maxLength) {
		return storageToStr(storage, buffer, maxLength, logger);
	}
	
	public static String storageToStr(IExtendable<Object> storage, Integer buffer, Long maxLength, Logger log){
        InputStream is = null;
        String ret = null;
        try {
        	Detector det = new Detector(nsPSMDetector.CHINESE);
			is = new BufferedInputStream(storage.<InputStream>extend(Method.input));
            ByteArrayOutputStream outstream = new ByteArrayOutputStream(buffer);
            byte[] b = new byte[buffer];
            int len;
			boolean done = false;
			Long count = 0L;
            while ((len = is.read(b)) != -1) {
            	if (maxLength >= 0) {
            		count += len;
            		if (count > maxLength) throw new IOException("Data overflow. The maxLength is " + maxLength); 
            	}
                if (!done && !det.isAscii(b,len)) done = det.DoIt(b,len, false);
                outstream.write(b, 0, len);
            }            
            ret = new String(outstream.toByteArray(), det.observer.charset);
        } catch (Exception e){
			log.error("TypeConvertor : transaction perform error.", e);
        } finally {
            if(is!=null) {try{is.close();} catch(Exception e){log.error("TypeConvertor : transaction perform error.", e);} }
        }
        return ret;   
	}
	
	public static String getPinyin(String input){
		return getPinyin(input, logger);
	}
	
	public static String getPinyin(String input, Logger log){
        StringBuilder pybf = new StringBuilder(); 
        char[] arr = input.toCharArray(); 
        HanyuPinyinOutputFormat defaultFormat = new HanyuPinyinOutputFormat(); 
        defaultFormat.setCaseType(HanyuPinyinCaseType.LOWERCASE); 
        defaultFormat.setToneType(HanyuPinyinToneType.WITHOUT_TONE); 
        defaultFormat.setVCharType(HanyuPinyinVCharType.WITH_V); 
        for (int i = 0; i < arr.length; i++) if (arr[i] > 128) try { 
           pybf.append(PinyinHelper.toHanyuPinyinStringArray(arr[i], defaultFormat)[0]); 
        } catch (Exception e) { 
			log.error("TypeConvertor : transaction perform error.", e);
        } else pybf.append(arr[i]);  
        return pybf.toString().trim(); 
	}
	
	public static <T> Class<T> getRealClass(T in){
		if(in instanceof IUniversal) in = (T)((IUniversal)in).realObject();
		if(in == null) return null;
		else return (Class<T>)AopUtils.getTargetClass(in);
	}
	
	protected MethodType type = MethodType.getRealObject;
	protected String strPara = "utf-8";
	protected Integer intPara = 8192;
	protected volatile Object objPara;
	
	public CommonUtil() {
		this.log = LoggerFactory.getLogger(CommonUtil.class);
	}

	public CommonUtil(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CommonUtil.class) : log;
	}
	
	public void setType(MethodType type) {
		if(type != null) this.type = type;
	}
	
	public void setObjPara(Object objPara) {
		this.objPara = objPara;
	}

	public void setStrPara(String strPara) {
		this.strPara = strPara;
	}

	public void setIntPara(Integer intPara) {
		this.intPara = intPara;
	}
	
	@Override
	public Object process(Object instance) {
		if(instance != null) switch (type) {
			case getRealObject: return getRealObject(instance, log);
			case getRealClass: return getRealClass(instance);
			case getPinyin: return getPinyin(instance.toString(), log);
			case resourceToStream: if (instance instanceof Resource) return resourceToStream((Resource) instance, strPara, (IProcessor<Object, Object>)objPara, intPara);
			else if (instance instanceof Object[]) {
				Object[] para = (Object[]) instance;
				if (para.length > 1 && para[0] instanceof Resource && para[1] instanceof String) return resourceToStream((Resource) para[0], para[1].toString(), (IProcessor<Object, Object>)objPara, intPara);
			}
			break;
			case bytesToStrAuto: if(instance instanceof byte[]) return bytesToStr((byte[])instance, log);
			break;
			case storageToStr: if(instance instanceof IExtendable) return storageToStr((IExtendable<Object>)instance, intPara, objPara == null ? -1L : new Long(objPara.toString()), log);
			break;
			case urlToStr: if(instance instanceof URL) return urlToStr((URL)instance, intPara, objPara == null ? -1L : new Long(objPara.toString()), log);
		}
		return null;
	}
}
