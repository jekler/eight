package net.yeeyaa.eight.common.storage;

import java.io.IOException;
import java.io.OutputStream;

import net.yeeyaa.eight.IProcessor;

import org.springframework.core.io.Resource;
import org.springframework.core.io.WritableResource;


public class ProxyWResource extends ProxyResource implements WritableResource {
	protected IProcessor<Resource, OutputStream> outputProcessor;
	protected Boolean writable;
	protected IProcessor<Resource, Boolean> writableProcessor;	
	
	public void setOutputProcessor(IProcessor<Resource, OutputStream> outputProcessor) {
		this.outputProcessor = outputProcessor;
	}

	public void setWritable(Boolean writable) {
		this.writable = writable;
	}

	public void setWritableProcessor(IProcessor<Resource, Boolean> writableProcessor) {
		this.writableProcessor = writableProcessor;
	}

	@Override
	public boolean isWritable() {
		if (writableProcessor == null) if (writable == null) if (resource instanceof WritableResource) return ((WritableResource)resource).isWritable();
		else return false;
		else return writable;
		else return writableProcessor.process(resource);
	}

	@Override
	public OutputStream getOutputStream() throws IOException {
		if(outputProcessor == null) if (resource instanceof WritableResource) return ((WritableResource)resource).getOutputStream();
		else return null;
		else return outputProcessor.process(resource);
	}
}
