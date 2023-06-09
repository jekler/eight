package net.yeeyaa.eight.core.storage;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PipeWStorage extends Storage<Object>{
	protected final Logger log;
	protected PipedInputStream input;
	protected PipedOutputStream output;	
	protected volatile Object key;
	
	public void initialize(Integer size) {
		try {
			input = size != null && size > 0 ? new PipedInputStream(size) : new PipedInputStream();
			output = new PipedOutputStream(input);
		} catch (IOException e) {
			input = null;
			output = null;
			log.error("PipeWStorage: pipe connect error.", e);
		}
	}
	
	public PipeWStorage(Object key, Integer size, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PipeWStorage.class) : log;
		this.key = key;
		initialize(size);
	}

	public PipeWStorage(Integer size, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PipeWStorage.class) : log;
		initialize(size);
	}
	
	public PipeWStorage(){
		this.log = LoggerFactory.getLogger(PipeWStorage.class);
		initialize(null);
	}

	public void setKey(Object key) {
		this.key = key;
	}
	
	public InputStream input() {
		return input;
	}

	public Boolean exists() {
		return input != null;
	}

	public Object key() {
		return key;
	}

	public Long modified() {
		return System.currentTimeMillis();
	}

	@Override
	public String toString() {
		Object key = key();
		return key == null ? null : key.toString();
	}

	public OutputStream output() {
		return output;
	}
}
