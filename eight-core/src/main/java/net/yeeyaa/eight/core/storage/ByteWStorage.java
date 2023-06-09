package net.yeeyaa.eight.core.storage;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Calendar;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;


public class ByteWStorage extends Storage<Object> implements IProcessor<ByteWStorage, byte[]>, IBiProcessor<Boolean, ByteWStorage, Object>{
	protected volatile byte[] cache = new byte[0];
	protected volatile Long lastModified = Calendar.getInstance().getTimeInMillis();
	protected IProcessor<IProcessor<byte[], Void>, OutputStream> outputStream;
	protected IProcessor<byte[], byte[]> convertor;
	protected IProcessor<byte[], byte[]> reconvertor;
	protected Integer buffer = 8192;
	protected Object name;
	protected Boolean exist = true;
	
	public ByteWStorage(byte[] cache, Object name) {
		this.cache = cache;
		this.name = name;
	}

	public ByteWStorage(){}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null && buffer > 0) this.buffer = buffer;
	}

	public void setExist(Boolean exist) {
		if(exist != null) this.exist = exist;
	}

	public void setConvertor(IProcessor<byte[], byte[]> convertor) {
		this.convertor = convertor;
	}

	public void setReconvertor(IProcessor<byte[], byte[]> reconvertor) {
		this.reconvertor = reconvertor;
	}

	public void setCache(byte[] cache) {
		this.cache = cache;
	}
	
	public void setOutputStream(IProcessor<IProcessor<byte[], Void>, OutputStream> outputStream) {
		this.outputStream = outputStream;
	}
	
	public void setName(Object name) {
		this.name = name;
	}

	public InputStream input() {
		if(convertor != null) return new ByteArrayInputStream(convertor.process(cache));
		else return new ByteArrayInputStream(cache);
	}

	public Boolean exists() {
		return exist;
	}

	public Object key() {
		return name;
	}

	public Long modified() {
		return lastModified;
	}

	public OutputStream output() {
		if(outputStream != null) return outputStream.process(new IProcessor<byte[], Void>(){
			@Override
			public Void process(byte[] instance) {
				if(reconvertor != null) instance = reconvertor.process(instance);
				cache = instance;
				lastModified = Calendar.getInstance().getTimeInMillis();
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
				byte[] result = out.toByteArray();
				if(reconvertor != null) result = reconvertor.process(result);
				cache = result;
				lastModified = Calendar.getInstance().getTimeInMillis();
			}

			@Override
			public void write(int b) throws IOException {
				out.write(b);		
			}
		};
	}

	@Override
	public String toString() {
		return name == null ? null : name.toString();
	}

	@Override
	public byte[] process(ByteWStorage storage) {
		return storage.cache;
	}

	@Override
	public Object perform(Boolean method, ByteWStorage storage) {
		if (method == null) return storage.name;
		else if (method) return storage.lastModified;
		else return storage.exist;
	}
}
