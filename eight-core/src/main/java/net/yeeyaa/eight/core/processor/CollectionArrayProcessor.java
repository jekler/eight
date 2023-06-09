package net.yeeyaa.eight.core.processor;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class CollectionArrayProcessor<T> implements IProcessor<Collection<T>, T[]> {	
	@Override
	public T[] process(Collection<T> in) {
		if(in != null && in.size() > 0){
			T[] ret = PlatformUtil.newArrayOf(in.size(), (T)in.toArray()[0]);
			return in.toArray(ret);
		}
		return (T[])new Object[0];
	}
}
