package net.yeeyaa.eight.core.resource;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IResource;

public class GenericResource<K, V> implements IResource<K, V> {
	protected IInputResource<K, V> input;
	protected IOutputResource<K, V> output;

	public void setInput(IInputResource<K, V> input) {
		this.input = input;
	}

	public void setOutput(IOutputResource<K, V> output) {
		this.output = output;
	}

	@Override
	public V find(K... paras) {
		return input.find(paras);
	}

	@Override
	public <P> P store(V value, K... paras) {
		return output.store(value, paras);
	}

	@Override
	public <P> P discard(K... paras) {
		return output.discard(paras);
	}

	@Override
	public <P> P empty(K... paras) {
		return output.empty(paras);
	}
}
