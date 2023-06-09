package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class ConsoleProcessor<T> implements IProcessor<T, T> {
	@Override
	public T process(T in) {
		System.out.println(in);
		return in;
	}
}
