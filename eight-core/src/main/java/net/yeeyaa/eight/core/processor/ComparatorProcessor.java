package net.yeeyaa.eight.core.processor;

import java.util.Comparator;

import net.yeeyaa.eight.IProcessor;


public class ComparatorProcessor implements IProcessor<Object[], Object> {
	protected Comparator<Object[]> comparator;

	public void setComparator(Comparator<Object[]> comparator) {
		this.comparator = comparator;
	}

	@Override
	public Object process(Object[] instance) {
		Object[] ret = new Object[1];
		if (comparator.compare(ret, instance) == 0) return ret[0];
		else return null;
	}
}
