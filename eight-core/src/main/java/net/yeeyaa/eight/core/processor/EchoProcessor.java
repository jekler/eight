package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class EchoProcessor<T> implements IProcessor<T, T> {//factory bean in spring
	@Override
	public T process(T in) {
		return in;
	}
}