package net.yeeyaa.eight.common.storage;

import java.io.IOException;
import java.io.OutputStream;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.springframework.core.io.WritableResource;


public class MockWResource extends MockResource<IExtendable<Object>> implements WritableResource {	
	@Override
	public boolean isWritable() {
		return true;
	}

	@Override
	public OutputStream getOutputStream() throws IOException {
		return storage.extend(Method.output);
	}
}
