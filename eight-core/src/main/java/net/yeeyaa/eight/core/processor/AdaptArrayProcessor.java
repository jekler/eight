package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class AdaptArrayProcessor<T> implements IProcessor<T, Object> {	
	protected Integer mode = 0;
	
	public void setMode(Integer mode) {
		if(mode != null) this.mode = mode;
	}

	@Override
	public Object process(T in) {
		if(in != null){
			if(mode == 0 && in.getClass().isArray()) return in;
			else if(mode == 1 && in instanceof Object[]) return in;
			else {
				T[] ret = PlatformUtil.newArrayOf(1, in);
				ret[0] = in;
				return ret;
			}
		}else return null;
	}
}
