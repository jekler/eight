package net.yeeyaa.eight.common.storage;

import java.io.IOException;
import java.io.OutputStream;

import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.springframework.core.io.WritableResource;


public class MockWStorage extends MockStorage<WritableResource> {	
	public MockWStorage(WritableResource resource) {
		this.resource = resource;
	}

	public MockWStorage() {}
	
	public OutputStream output() {
		try {
			return resource.getOutputStream();
		} catch (IOException e) {
			throw new PlatformException(PlatformError.ERROR_IO, e.getMessage());
		}
	}
}
