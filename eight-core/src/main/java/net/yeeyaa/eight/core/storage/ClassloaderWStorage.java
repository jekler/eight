package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

public class ClassloaderWStorage extends Storage<Object> implements IProcessor<String, URL>{
	final protected ClassLoader cl;
	protected String key;
	
	public ClassloaderWStorage(ClassLoader cl) {
		this.cl = cl;
	}
	
	public ClassloaderWStorage() {
		this.cl = ClassloaderWStorage.class.getClassLoader();
	}

	public void setKey(String key) {
		this.key = key;
	}

	public InputStream input() {
		InputStream in = null;
    	ClassLoader cl = this.cl;
    	do {
    		in = cl.getResourceAsStream(key);
    		if (in == null) cl = cl.getClass().getClassLoader();
    	} while (in == null && cl != null);
    	return in;
	}

	public Boolean exists() {
		URL in = null;
    	ClassLoader cl = this.cl;
    	do {
    		in = cl.getResource(key);
    		if (in == null) cl = cl.getClass().getClassLoader();
    	} while (in == null && cl != null);
    	return in != null;	
    }
	
	public OutputStream output() {
		try {
			URL in = null;
	    	ClassLoader cl = this.cl;
	    	do {
	    		in = cl.getResource(key);
	    		if (in == null) cl = cl.getClass().getClassLoader();
	    	} while (in == null && cl != null);
	    	return in.openConnection().getOutputStream();
		} catch (Exception e) {
			return null;
		}
	}
	
	public Object key() {
		return key;
	}

	public Long modified() {
		return 0L;
	}

	@Override
	public URL process(String object) {
		URL in = null;
		try {
	    	ClassLoader cl = this.cl;
	    	do {
	    		in = cl.getResource(key);
	    		if (in == null) cl = cl.getClass().getClassLoader();
	    	} while (in == null && cl != null);
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO);
		}
    	return in;
	}
}
