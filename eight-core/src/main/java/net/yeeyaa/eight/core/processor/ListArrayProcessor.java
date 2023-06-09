package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.LinkedList;

import net.yeeyaa.eight.IProcessor;


public class ListArrayProcessor implements IProcessor<Object, Object[]> {
	protected Integer count = 0;
	protected Integer index = -1;
	protected IProcessor<Object, Object> processor;
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setIndex(Integer index) {
		if(index != null && index >= 0) this.index = index;
	}

	public void setCount(Integer count) {
		if(count != null && count > 0) this.count = count;
	}

	@Override
	public Object[] process(Object in) {
		Object[] ret = null;
		if(in != null){
			ret = new Object[count + 1];
			Collection<Object> ls = new LinkedList<Object>();
			if(in.getClass().isArray()) for (int i = 0; i < Array.getLength(in); i ++) ls.add(Array.get(in, i));
			else if(in instanceof Collection) ls = (Collection<Object>)in;
			Object[] os = ls.toArray();
			if(index < 0 || index > count) index = count;
			int c = os.length > count ? count : os.length;			
			for(int i = 0, j = 0; i < c; i++, j++) {
				if(j == index) j++;
				ret[j] = processor == null ? os[i] : processor.process(os[i]);
				ls.remove(os[i]);
			}
			if(in.getClass().isArray()) ret[index] = ls.toArray();
			else ret[index] = in;
		}
		return ret;
	}
}
