package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class EchoProcessor<T> implements IProcessor<T, T> {
	@Override
	public T process(T in) {
		return in;
	}
}
