package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class OverflowProcessor<T, R> implements IProcessor<T, R> {
	protected IProcessor<T, R> proxy;
	protected Integer max;
	
	public void setMax(Integer max) {
		if (max != null && max > 0) this.max = max;
	}

	public void setProxy(IProcessor<T, R> proxy) {
		this.proxy = proxy;
	}

	@Override
	public R process(T in) {
		if (max != null && in != null) if (in.getClass().isArray() && Array.getLength(in) > max || in instanceof String && ((String)in).length() > max 
				|| in instanceof Map && ((Map) in).size() > max || in instanceof Collection && ((Collection) in).size() > max) throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, "Data overflow. The maxLength is " + max);
		return proxy.process(in);
	}
}
