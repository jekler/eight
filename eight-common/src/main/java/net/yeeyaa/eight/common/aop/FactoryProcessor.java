package net.yeeyaa.eight.common.aop;

import java.lang.ref.WeakReference;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.CopyOnWriteMap;


public class FactoryProcessor implements IProcessor<Object, Object>{
	protected final CopyOnWriteMap<Class<?>, WeakReference<Class<?>>> cache = new CopyOnWriteMap<Class<?>, WeakReference<Class<?>>>(true);
	protected IProcessor<Object, Class<?>> classFactory;
	protected IProcessor<Class<?>, Object> objFactory;

	public void setClassFactory(IProcessor<Object, Class<?>> classFactory) {
		this.classFactory = classFactory;
	}

	public void setObjFactory(IProcessor<Class<?>, Object> objFactory) {
		this.objFactory = objFactory;
	}

	public Object getObject(){
		return process(null);
	}
	
	@Override
	public Object process(Object instance) {
		Class<?> key = null;
		if (instance != null) if (instance instanceof Class<?>) key = (Class<?>)instance;
		else key = instance.getClass();
		WeakReference<Class<?>> ref = cache.get(key);
		Class<?> clz = ref == null ? null : ref.get();
		if (clz == null) synchronized(this) {
			ref = cache.get(key);
			clz = ref == null ? null : ref.get();
			if (clz == null) {
				clz = classFactory.process(instance);
				cache.put(key, new WeakReference<Class<?>>(clz));
			}
		}
		if(clz == null) return null;
		else return objFactory.process(clz);
	}
}
