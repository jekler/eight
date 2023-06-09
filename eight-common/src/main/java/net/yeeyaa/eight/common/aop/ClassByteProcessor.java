package net.yeeyaa.eight.common.aop;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.agent.ClassCenter;


public class ClassByteProcessor implements IProcessor<Object, byte[]> {
	protected Class<?> clz;
	
	public void setClz(Class<?> clz) {
		this.clz = clz;
	}

	@Override
	public byte[] process(Object clz) {
		if(clz == null) if(this.clz == null)return null;
		else return ClassCenter.find(this.clz);
		else if(clz instanceof Class) return ClassCenter.find((Class<?>)clz);
		else return ClassCenter.find(clz.getClass());
	}
}
