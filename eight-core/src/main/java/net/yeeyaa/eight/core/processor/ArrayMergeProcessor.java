package net.yeeyaa.eight.core.processor;

import java.util.Arrays;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ArrayMergeProcessor implements IProcessor<Object, Object[]> {	
	protected Integer[] seq = new Integer[0];;
	protected Object[] template = new Object[0];
	protected Boolean discard; 
	protected Integer largest = -1;
	
	public void setDiscard(Boolean discard) {
		this.discard = discard;
	}

	public void setSeq(Integer[] seq) {
		if(seq != null) {
			for(Integer i : seq) if(i > largest) largest = i;
			this.seq = seq;
		}
	}

	public void setTemplate(Object[] template) {
		if(template != null) this.template = template;
	}

	@Override
	public Object[] process(Object instance) {
		Object[] origin;
		if(instance instanceof Object[]) origin = (Object[]) instance;
		else if(instance == null) origin = new Object[0];
		else {
			origin = PlatformUtil.newArrayOf(1, instance);
			origin[0] = instance;
		}
		Integer length = largest < template.length ? template.length : largest + 1;
		Integer append = origin.length - seq.length;
		if(append > 0) if(discard == null) length += append;
		else if(!discard) length = largest + append < template.length ? template.length : largest + append + 1;
		Object[] ret = Arrays.copyOf(template, length);
		for(int i = 0; i < origin.length; i++) if(seq.length > i) ret[seq[i]] = origin[i];
		else if(discard == null) ret[length - origin.length + i] = origin[i];
		else if(!discard) ret[largest - seq.length + i + 1] = origin[i];
		return ret;
	}
}
