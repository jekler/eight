package net.yeeyaa.eight.core.storage;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;


public class ForkWStorage extends Storage<Object> {
	protected Map<IProcessor<Object, IExtendable<Object>>, IProcessor<byte[], byte[]>> storages;
	protected volatile Object key;
	protected volatile Map<IExtendable<Object>, IProcessor<byte[], byte[]>> cache;
	
	public ForkWStorage(Map<IProcessor<Object, IExtendable<Object>>, IProcessor<byte[], byte[]>> storages) {
		this.storages = storages;
	}

	public ForkWStorage(){}

	public void setStorages(Map<IProcessor<Object, IExtendable<Object>>, IProcessor<byte[], byte[]>> storages) {
		this.storages = storages;
	}

	public void setKey(Object key) {
		this.key = key;
		getCache();
	}
	
	protected Map<IExtendable<Object>, IProcessor<byte[], byte[]>> getCache() {
		if (cache == null) synchronized(this) {
			if (storages == null) cache = new LinkedHashMap<IExtendable<Object>, IProcessor<byte[], byte[]>>(0);
			else {
				LinkedHashMap<IExtendable<Object>, IProcessor<byte[], byte[]>> tmp = new LinkedHashMap<IExtendable<Object>, IProcessor<byte[], byte[]>>(storages.size() * 2);
				for (Entry<IProcessor<Object, IExtendable<Object>>, IProcessor<byte[], byte[]>> entry : storages.entrySet()) if (entry.getKey() != null) {
					IExtendable<Object> storage =  entry.getKey().process(key);
					if (storage != null) tmp.put(storage, entry.getValue());
				}
				cache = tmp;
			}
		} 
		return cache;
	}

	public InputStream input() {
		Map<IExtendable<Object>, IProcessor<byte[], byte[]>> cache = getCache();
		if (cache.size() > 0) return (InputStream) cache.keySet().toArray(new IExtendable[cache.size()])[0].extend(Method.input);
		else return null;
	}

	public Boolean exists() {
		Map<IExtendable<Object>, IProcessor<byte[], byte[]>> cache = getCache();
		if (cache.size() > 0) return true;
		else return false;
	}

	public Object key() {
		return key;
	}

	public Long modified() {
		Map<IExtendable<Object>, IProcessor<byte[], byte[]>> cache = getCache();
		if (cache.size() > 0) return (Long) cache.keySet().toArray(new IExtendable[cache.size()])[0].extend(Method.modified);
		else return 0L;
	}

	@Override
	public String toString() {
		Object key = key();
		return key == null ? null : key.toString();
	}

	public OutputStream output() {
		Map<IExtendable<Object>, IProcessor<byte[], byte[]>> cache = getCache();
		final Map<OutputStream, IProcessor<byte[], byte[]>> stream = new LinkedHashMap<OutputStream, IProcessor<byte[], byte[]>>(cache.size() * 2);
		if (cache.size() > 0) for (Entry<IExtendable<Object>, IProcessor<byte[], byte[]>> entry : cache.entrySet()) {
			OutputStream output =  entry.getKey().extend(Method.output);
			if (output != null) stream.put(output, entry.getValue());
		}
		return new OutputStream(){
			@Override
			public void write(byte[] b) throws IOException {
				Exception error = null;
				if (stream.size() > 0) for (Entry<OutputStream, IProcessor<byte[], byte[]>> entry : stream.entrySet()) try {
					byte[] content = entry.getValue() == null ? b : entry.getValue().process(b);
					entry.getKey().write(content);
				} catch(Exception e) {
					error = e;
				}
				if (error != null) throw new IOException(error);
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				if (b == null) throw new NullPointerException();
				else if ((off < 0) || (off > b.length) || (len < 0) || ((off + len) > b.length) || ((off + len) < 0)) throw new IndexOutOfBoundsException();
				else if (len == 0) return;
				byte[] tmp = new byte[len];
				for (int i = 0 ; i < len ; i++) tmp[i] = b[off + i];
				write(tmp);
			}

			@Override
			public void flush() throws IOException {
				Exception error = null;
				if (stream.size() > 0) for (Entry<OutputStream, IProcessor<byte[], byte[]>> entry : stream.entrySet()) try {
					entry.getKey().flush();
				} catch(Exception e) {
					error = e;
				}
				if (error != null) throw new IOException(error);
			}

			@Override
			public void close() throws IOException {
				Exception error = null;
				if (stream.size() > 0) for (Entry<OutputStream, IProcessor<byte[], byte[]>> entry : stream.entrySet()) try {
					entry.getKey().close();
				} catch(Exception e) {
					error = e;
				}
				if (error != null) throw new IOException(error);
			}

			@Override
			public void write(int b) throws IOException {
				write(new byte[]{(byte)b});
			}		
		};
	}
}
