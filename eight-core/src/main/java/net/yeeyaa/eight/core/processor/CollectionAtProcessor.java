package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.List;

import net.yeeyaa.eight.IProcessor;


public class CollectionAtProcessor<T> implements IProcessor<Object, T> {
	protected Integer index = 0;
	
	public void setIndex(Integer index) {
		this.index = index;
	}
	
	@Override
	public T process(Object in) {
		if(in != null) if(in instanceof List) {
			int size = ((List)in).size();
			if (Math.abs(index) < size) return index < 0 ? (T)((List)in).get(size + index) : (T)((List)in).get(index);
		} else if(in instanceof Collection) {
			int size = ((Collection)in).size();
			if (Math.abs(index) < size) return index < 0 ? (T)((Collection)in).toArray()[size + index] : (T)((Collection)in).toArray()[index];
		} else if (in.getClass().isArray()) {
			int size = Array.getLength(in);
			if (Math.abs(index) < size) return index < 0 ? (T)Array.get(in, size + index) : (T)Array.get(in, index);
		}
		return null;
	}
}
