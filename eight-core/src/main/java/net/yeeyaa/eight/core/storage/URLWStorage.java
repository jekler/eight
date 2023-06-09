package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;

import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

public class URLWStorage extends Storage<Object> {
	protected final URL url;
	
	public URLWStorage(String path) {
		try {
			this.url = new URL(path);
		} catch (MalformedURLException e) {
			throw new PlatformException(PlatformError.ERROR_PARAMETERS);

		}
	}
	
	public URLWStorage(URL url) {
		this.url = url;
	}
	
	public InputStream input() {
    	try {
			return url.openStream();
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO);
		}
	}

	public Boolean exists() {
    	try {
			return url.getContent() != null;
		} catch (Exception e) {
			return false;
		}
    }
	
	public OutputStream output() {
    	try {
			return url.openConnection().getOutputStream();
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO);
		}
	}
	
	public Object key() {
		return url.toString();
	}

	public Long modified() {
		return 0L;
	}
}
