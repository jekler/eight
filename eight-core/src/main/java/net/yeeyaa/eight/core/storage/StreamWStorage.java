package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;


public class StreamWStorage extends Storage<Object> {
	protected Object key;
	protected InputStream input;
	protected OutputStream output;
	
	public StreamWStorage(Object key, InputStream input) {
		this.key = key;
		this.input = input;
	}

	public StreamWStorage(InputStream input) {
		this.input = input;
	}
	
	public StreamWStorage() {}
	
	public StreamWStorage(OutputStream output) {
		this.output = output;
	}
	
	public StreamWStorage(InputStream input, OutputStream output) {
		this.input = input;
		this.output = output;
	}
	
	public StreamWStorage(Object key, InputStream input, OutputStream output) {
		this.input = input;
		this.output = output;
		this.key = key;
	}
	
	public void setOutput(OutputStream output) {
		this.output = output;
	}
	
	public void setInput(InputStream input) {
		this.input = input;
	}

	public void setKey(Object key) {
		this.key = key;
	}
	
	public InputStream input() {
		return input;
	}

	public Boolean exists() {
		return input != null || output != null;
	}
	
	public OutputStream output() {
		return output;
	}
	
	public Object key() {
		return key;
	}

	public Long modified() {
		return 0L;
	}
}
