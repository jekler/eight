package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;

public class DefaultKeyProcessor<T, R> implements IProcessor<T[], R> {
	protected T[] deafultKey;
	protected IInputResource<T, R> resource;
	
	public void setResource(IInputResource<T, R> resource) {
		this.resource = resource;
	}

	public void setDeafultKey(T[] deafultKey) {
		this.deafultKey = deafultKey;
	}

	@Override
	public R process(T[] key) {
		if(key == null) key = deafultKey;
		return resource.find(key);
	}
}
