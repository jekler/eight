package net.yeeyaa.eight.common.storage;

import java.io.IOException;
import java.io.InputStream;

import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage;

import org.springframework.core.io.Resource;


public class MockStorage<T extends Resource> extends Storage<Object> {
	protected T resource;

	public void setResource(T resource) {
		this.resource = resource;
	}
	
	public MockStorage(T resource) {
		this.resource = resource;
	}

	public MockStorage() {}

	public InputStream input() {
		try {
			return resource.getInputStream();
		} catch (IOException e) {
			throw new PlatformException(PlatformError.ERROR_IO, e.getMessage());
		}
	}

	public Boolean exists() {
		return resource.exists();
	}
	
	public Object key() {
		return resource.getFilename();
	}

	public Long modified() {
		try {
			return resource.lastModified();
		} catch (IOException e) {
			throw new PlatformException(PlatformError.ERROR_IO, e.getMessage());
		}
	}

	@Override
	public String toString() {
		return resource.toString();
	}
}
