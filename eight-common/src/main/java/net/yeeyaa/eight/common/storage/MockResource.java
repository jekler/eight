package net.yeeyaa.eight.common.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.springframework.core.io.Resource;


public class MockResource<T extends IExtendable<Object>> implements Resource {
	protected T storage;

	public void setStorage(T storage) {
		this.storage = storage;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		return storage.extend(Method.input);
	}

	@Override
	public boolean exists() {
		return (Boolean) storage.extend(Method.exists);
	}

	@Override
	public boolean isReadable() {
		return true;
	}

	@Override
	public boolean isOpen() {
		return false;
	}

	@Override
	public URL getURL() throws IOException {
		return new URL(storage.extend(Method.key).toString());
	}

	@Override
	public URI getURI() throws IOException {
		try {
			return new URI(storage.extend(Method.key).toString());
		} catch (URISyntaxException e) {
			throw new IOException(storage.extend(Method.key).toString() + ": create uri fail.", e);
		}
	}

	@Override
	public File getFile() throws IOException {
		throw new FileNotFoundException("MockResource: cannot be resolved to absolute file path");
	}

	@Override
	public long contentLength() throws IOException {
		InputStream is = this.getInputStream();
		if(is != null)try {
			long size = 0;
			byte[] buf = new byte[255];
			int read;
			while ((read = is.read(buf)) != -1) {
				size += read;
			}
			return size;
		} finally {
			try {
				is.close();
			} catch (IOException ex) {}
		}
		return 0;
	}

	@Override
	public long lastModified() throws IOException {
		return (Long) storage.extend(Method.modified);
	}

	@Override
	public Resource createRelative(String relativePath) throws IOException {
		throw new FileNotFoundException("MockResource: Cannot create a relative resource.");
	}

	@Override
	public String getFilename() {
		Object key = storage.extend(Method.key);
		if(key != null) {
			StringBuilder sb = new StringBuilder();
			if(key.getClass().isArray()) for (int i = 0; i < Array.getLength(key); i ++) sb.append(Array.get(key, i));
			else if(key instanceof Collection) for(Object subkey : (Collection) key) sb.append(subkey);
			else sb.append(key);
			return sb.toString();
		}
		return null;
	}

	@Override
	public String getDescription() {
		return storage.extend(Method.key).toString();
	}

	@Override
	public String toString() {
		return storage.extend(Method.key).toString();
	}
}
