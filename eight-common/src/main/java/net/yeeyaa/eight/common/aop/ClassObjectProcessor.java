package net.yeeyaa.eight.common.aop;

import net.yeeyaa.eight.IProcessor;

import org.springframework.objenesis.ObjenesisStd;


public class ClassObjectProcessor implements IProcessor<Object, Object> {
	protected Boolean objensis; 
	protected ObjenesisStd creator;
	protected Class<?> clz;
	
	public void setClass(Class<?> clz) {
		this.clz = clz;
	}

	public void setObjensis(Boolean objensis) {
		this.objensis = objensis;
		if(!Boolean.FALSE.equals(objensis)) creator = new ObjenesisStd(true);
	}

	@Override
	public Object process(Object instance) {
		Class<?> clz = this.clz;
		if(instance != null) if(instance instanceof Class<?>) clz = (Class<?>) instance;
		else clz = instance.getClass();
		if(clz == null) return null;
		else if(objensis == null) try{
			return clz.newInstance();
		} catch(Exception e) {
			return creator.newInstance(clz);
		} else if(objensis) return creator.newInstance(clz);
		else try{
			return clz.newInstance();
		} catch(Exception e) {
			return null;
		}
	}
}
