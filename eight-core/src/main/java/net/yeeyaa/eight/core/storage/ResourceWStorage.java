package net.yeeyaa.eight.core.storage;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Calendar;

import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class ResourceWStorage<K, V, T extends IResource<K, V>> extends Storage<Object> {
	protected K[] key;
	protected IProcessor<V, InputStream> inputStream;
	protected IProcessor<V, V> convertor;
	protected IProcessor<Object[], Boolean> comparator;
	protected IProcessor<V, V> backupConvertor;
	protected volatile Long lastModified = -1L;
	protected String charset = "UTF-8";
	protected Boolean lazy = true;
	protected Boolean neverChange = false;
	protected Boolean neverDelete = false;
	protected Boolean hasSet = false;
	protected IProcessor<IProcessor<V, Void>, OutputStream> outputStream;
	protected IProcessor<byte[], V> reconvertor;
	protected Integer buffer = 8192;
	protected Integer keyType = 0; 
	protected volatile V cache;
	protected T resource;
	protected Boolean endPoint = false;
	
	public final void setEndPoint(Boolean endPoint) {
		if(endPoint != null) this.endPoint = endPoint;
	}

	public final void setResource(T resource) {
		this.resource = resource;
	}
	
	public final T process(T resource) {
		synchronized(this){
			if(this.resource == null || endPoint || !(this.resource instanceof IProcessor)) this.resource = resource;
			else this.resource = ((IProcessor<T, T>)this.resource).process(resource);		
		}
		return (T) this;
	}
	
	public void setBuffer(Integer buffer) {
		if(buffer != null) this.buffer = buffer;
	}

	public void setReconvertor(IProcessor<byte[], V> reconvertor) {
		this.reconvertor = reconvertor;
	}

	public void setOutputStream(IProcessor<IProcessor<V, Void>, OutputStream> outputStream) {
		this.outputStream = outputStream;
	}
	
	public void setKeyType(Integer keyType) {
		if(keyType != null) this.keyType = keyType;
	}

	public void setNeverChange(Boolean neverChange) {
		if(neverChange != null) this.neverChange = neverChange;
	}

	public void setNeverDelete(Boolean neverDelete) {
		if(neverDelete != null) this.neverDelete = neverDelete;
	}

	public void setKey(K[] key) {
		this.key = key;
	}
	
	public void setConvertor(IProcessor<V, V> convertor) {
		this.convertor = convertor;
	}

	public void setCharset(String charset) {
		if(charset != null) this.charset = charset;
	}
	
	public void setLazy(Boolean lazy) {
		this.lazy = lazy;
	}

	public void setInputStream(IProcessor<V, InputStream> inputStream) {
		this.inputStream = inputStream;
	}

	public void setComparator(IProcessor<Object[], Boolean> comparator) {
		this.comparator = comparator;
	}

	public void setBackupConvertor(IProcessor<V, V> backupConvertor) {
		this.backupConvertor = backupConvertor;
	}

	public InputStream input() {
		V source = cache;
		if(!lazy) source = resource.find(key);
		if(source == null) throw new PlatformException(PlatformError.ERROR_IO, "ResourceStorage: cannot be resolved to absolute file path"); 
		else if(inputStream != null) return inputStream.process(source);
		else {
			if(convertor != null) source = convertor.process(source);
			if(source instanceof InputStream) return(InputStream)source;
			else if(source instanceof byte[]) return new ByteArrayInputStream((byte[])source);
			else try {
				return new ByteArrayInputStream(source.toString().getBytes(charset));
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_IO, "ResourceStorage: source invalid"); 
			}
		}
	}

	public OutputStream output() {
		if(outputStream != null) return outputStream.process(new IProcessor<V, Void>(){
			@Override
			public Void process(V instance) {
				resource.store(instance, key);
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
				V o;
				if(reconvertor != null) o = reconvertor.process(out.toByteArray());
				else o = (V)out.toByteArray();
				resource.store(o, key);
			}

			@Override
			public void write(int b) throws IOException {
				out.write(b);		
			}
		};
	}
	
	public Boolean exists() {
		if(!lazy) modified();
		return cache != null;
	}
	
	public Object key() {
		if(key == null || key.length == 0) return null;
		else switch(keyType){
			case 0: Object ret = key[key.length - 1];
					return ret == null ? null : ret.toString();
			case 1: return key;
			case 2: StringBuilder sb = new StringBuilder();
					for(Object o : key) if(o != null) sb.append(o);
					return sb.toString();
			case 3: return key[key.length - 1];
			default: return Arrays.asList(key);
		}
	}

	public Long modified() {
		if(!neverChange || !hasSet){
			V backup = resource.find(key);
			if(!neverDelete || backup != null){
				if(!lazy && backupConvertor != null) backup = backupConvertor.process(backup);
				Boolean same = false;
				if(comparator != null) same = comparator.process(new Object[]{cache, backup});
				else same = compare(cache, backup);
				if(!same){
					lastModified = Calendar.getInstance().getTimeInMillis();
					cache = backup;
					hasSet = true;
				}
			}
		}
		return lastModified;
	}

	protected Boolean compare(Object left, Object right){
		if(left == null ? right == null : left.equals(right)) return true;
		else {
			if(left != null && right != null && left.getClass().isArray() && right.getClass().isArray()){
				int length = Array.getLength(left);
				if(length == Array.getLength(right)) for(int i = 0; i < length; i++) { if(!compare(Array.get(left, i), Array.get(right, i))) return false; }
				else return false;
				return true;
			}
			return false;
		}
	}
	
	public static class Comparator implements IProcessor<Object[], Boolean>{
		@Override
		public Boolean process(Object[] instance) {
			if(instance != null && instance.length > 1) {
				if(instance[0] == null ? instance[1] == null : instance[0].equals(instance[1])) return true;
				else if(instance[0] != null && instance[1] != null && instance[0].getClass().isArray() && instance[1].getClass().isArray()){
					 int length = Array.getLength(instance[0]);
					 if(length == Array.getLength(instance[1])) {
						 for(int i = 0; i < length; i++) {
							 Object left = Array.get(instance[0], i);
							 Object right = Array.get(instance[1], i);
							 if(left == null ? right != null : !left.equals(right)) return false;
						 }
						 return true;
					 }
				}
			}
			return false;
		}	
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (K k : key) sb.append(k).append(' ');
		return sb.toString();
	}
}
