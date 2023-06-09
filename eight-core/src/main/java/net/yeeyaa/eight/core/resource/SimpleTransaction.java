package net.yeeyaa.eight.core.resource;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransaction;

public class SimpleTransaction<K, V, T extends IResource<K, V>, R> implements ITransaction<K, V, T, R> {
	protected T resource;
	
	public void setResource(T resource) {
		this.resource = resource;
	}

	@Override
	public R execute(IProcessor<T, R> processor) {
		return processor.process(resource);
	}
}
