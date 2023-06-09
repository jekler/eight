package net.yeeyaa.eight.data.storage;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Calendar;

import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage;


public class MemcacheStorage extends Storage<Object> {
	protected String key;
	protected MemcachedClient mc;
	protected volatile CASValue<Object> cache;
	protected volatile Long lastModified = -1L;
	protected String charset = "UTF-8";
	protected Boolean lazy = true;
	protected IProcessor<Object, InputStream> inputStream;
	protected IProcessor<IProcessor<Object, Void>, OutputStream> outputStream;
	protected Boolean neverChange = false;
	protected Boolean neverDelete = false;
	protected Boolean hasSet = false;
	protected Integer buffer = 8192;
	protected Integer timeout = 0;
	
	public void setTimeout(Integer timeout) {
		if (timeout != null && timeout > 0) this.timeout = timeout;
	}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setNeverChange(Boolean neverChange) {
		if(neverChange != null) this.neverChange = neverChange;
	}

	public void setNeverDelete(Boolean neverDelete) {
		if(neverDelete != null) this.neverDelete = neverDelete;
	}
	
	public void setMc(MemcachedClient mc) {
		this.mc = mc;
	}

	public void setKey(String key) {
		this.key = key;
	}
	
	public void setCharset(String charset) {
		if(charset != null) this.charset = charset;
	}
	
	public void setLazy(Boolean lazy) {
		this.lazy = lazy;
	}

	public void setInputStream(IProcessor<Object, InputStream> inputStream) {
		this.inputStream = inputStream;
	}

	public void setOutputStream(IProcessor<IProcessor<Object, Void>, OutputStream> outputStream) {
		this.outputStream = outputStream;
	}
	
	public InputStream input() {
		Object source = cache;
		if(!lazy) source = mc.get(key);
		if(source == null) throw new PlatformException(PlatformError.ERROR_IO, "MemcacheStorage: cannot be resolved to absolute file path"); 
		else if(inputStream != null) return inputStream.process(source);
		else try {
			return new ByteArrayInputStream(source.toString().getBytes(charset));
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO, "MemcacheStorage: source invalid"); 
		}
	}

	public Boolean exists() {
		if(!lazy) modified();
		return cache != null;
	}

	public Object key() {
		return key;
	}

	public Long modified() {
		if(!neverChange || !hasSet){
			CASValue<Object> backup = mc.gets(key);
			if((!neverDelete || backup != null) && !(backup == null ? cache == null : backup.getCas() == (cache == null ? -1 : cache.getCas()))){
				lastModified = Calendar.getInstance().getTimeInMillis();
				cache = backup;
				hasSet = true;
			}
		}
		return lastModified;
	}

	public OutputStream output() {
		if(outputStream != null) return outputStream.process(new IProcessor<Object, Void>(){
			@Override
			public Void process(Object instance) {
				mc.set(key, timeout, instance);
				return null;
			}
		});else	return new OutputStream(){
			protected ByteArrayOutputStream out = new ByteArrayOutputStream(buffer);		
			@Override
			public void write(byte[] b) throws IOException {
				out.write(b);
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				out.write(b, off, len);
			}

			@Override
			public void flush() throws IOException {
				out.flush();
			}

			@Override
			public void close() throws IOException {
				mc.set(key, timeout, out.toString(charset));
			}

			@Override
			public void write(int b) throws IOException {
				out.write(b);		
			}			
		};
	}
}
