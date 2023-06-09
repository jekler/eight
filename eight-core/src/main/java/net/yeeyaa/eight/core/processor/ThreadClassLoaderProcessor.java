package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

public class ThreadClassLoaderProcessor<T, R> implements IProcessor<T, R> {
	protected IProcessor<T, R> processor;
	protected ClassLoader classLoader;
	protected Boolean back;
	
	public void setProcessor(IProcessor<T, R> processor) {
		this.processor = processor;
	}

	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setBack(Boolean back) {
		this.back = back;
	}

	@Override
	public R process(T in) {
		boolean overrideClassLoader = false;
		ClassLoader threadContextClassLoader = null;
		Thread currentThread = Thread.currentThread();
		try {
			threadContextClassLoader = currentThread.getContextClassLoader();
		} catch (Throwable ex) {}
		if (!classLoader.equals(threadContextClassLoader)) try {
			currentThread.setContextClassLoader(classLoader);
			overrideClassLoader = true;
		} catch (Throwable ex) {}
		try {
			R ret = process(in);
			return ret;
		} finally {
			if (overrideClassLoader && !Boolean.FALSE.equals(back)) currentThread.setContextClassLoader(threadContextClassLoader);
		}
	}
}
