package net.yeeyaa.eight.common.spring;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;


public class PlatformReference<U> implements IProcessor<Object, U>{
	protected volatile U instance;
	protected Object name;
	protected PlatformBean platformBean;
	protected Boolean transparent = false; 
	protected Integer type = 0; 
	protected Boolean retry = true;  
	protected Boolean renew = false; 
	protected Boolean notry = false; 
	protected Integer ver = 0;
	protected ThreadLocal<Integer> tver = new InheritableThreadLocal<Integer>(){ 
		@Override
		protected Integer initialValue() {
			return 0;
		}
	};
	protected ThreadLocal<U> temp = new InheritableThreadLocal<U>(){ 
		@Override
		protected U initialValue() {
			return fetch();
		}
	};

	public void setInherit(Boolean inherit) { 
		if(Boolean.FALSE.equals(inherit)) {
			temp = new ThreadLocal<U>(){ 
				@Override
				protected U initialValue() {
					return fetch();
				}
			};
			tver = new ThreadLocal<Integer>(){ 
				@Override
				protected Integer initialValue() {
					return 0;
				}
			};
		}
	}

	public void setRenew(Boolean renew) {
		if(renew != null) this.renew = renew;
	}

	public void setPlatformBean(PlatformBean platformBean) {
		this.platformBean = platformBean;
	}

	public void setTransparent(Boolean transparent) {
		if(transparent != null) this.transparent = transparent;
	}

	public void setRetry(Boolean retry) {
		if(retry != null) this.retry = retry;
	}

	public void setInstance(U instance) {
		this.instance = instance;
	}
	
	public void setName(Object name) {
		this.name = name;
	}

	public void setType(Integer type) {
		if(type != null && type > -1 && type < 4) this.type = type;
	}

	public U get(){
		if(type == 3){
			Integer version = platformBean.getVer(name);
			if(version != null && version != tver.get()) temp.remove();
		}
		return temp.get();
	}
	
	public U renew(){
		temp.remove();
		return temp.get();
	}
	
	protected U fetch() {
		if(notry) return instance;
		else if(type == 1 && name != null) {
			Integer version = platformBean.getVer(name);
			if(version != null && version != ver)synchronized(this){
				if(version != ver && !notry){
					Object[] ret = platformBean.getVerBean(name);
					if(ret != null){
						instance = (U) ret[1];
						ver = (Integer)ret[0];
					}
					if(!retry) notry = true;
				}
			}
			return instance;
		}else if(instance != null && (type == 0 || type == 1)) {
			notry = true;
			return instance;
		} else if(name != null) if(type == 0) synchronized(this){
			if(instance == null && !notry) instance = (U)platformBean.getBean(name);
			if(!retry) notry = true;
			return instance;
		} else { 
			U instance = null;
			if(!notry) {
				Object[] ret = platformBean.getVerBean(name);
				if(ret != null){
					instance = (U) ret[1];
					tver.set((Integer)ret[0]);
				}
				if(instance == null && !retry) {
					this.instance = null;
					notry = true;
				}
			}
			return instance;
		}
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if(transparent) if(this == obj) return true;
		else if(PlatformReference.class.isInstance(obj)) {
			PlatformReference<U> another = (PlatformReference<U>)obj;
			if (!another.transparent) return false;
			else if (name != null || another.name != null) if ((name == null ? false : name.equals(another.name)) && type.equals(another.type)){
				if(type == 0 || type == 1) return instance == null ? another.instance == null : instance.equals(another.instance);
				else return true;
			}else return false;
			else return instance == null ? another.instance == null : instance.equals(another.instance);
		} else return false;
		else return super.equals(obj);
	}

	@Override
	public int hashCode() {
		if(transparent) if(name != null) return name.hashCode() * 17 + type.hashCode();
		else if(instance != null) return instance.hashCode();
		else return 15173;
		else return super.hashCode();
	}

	@Override
	public U process(Object instance) {
		if(renew) return renew();
		else return get();
	}
	
	public static class Proxy<U> implements IProcessor<Object, U>{
		protected IProcessor<Object, PlatformReference<U>> factory; 
		protected ConcurrentHashMap<Object, PlatformReference<U>> map = new ConcurrentHashMap<Object, PlatformReference<U>>();
		protected Collection<Object> constraint;
		
		public void setConstraint(Collection<Object> constraint) {
			this.constraint = constraint;
		}

		public void setFactory(IProcessor<Object, PlatformReference<U>> factory) {
			this.factory = factory;
		}

		@Override
		public U process(Object name) {
			if(name != null && (constraint == null || constraint.contains(name))) {
				PlatformReference<U> ref = map.get(name);
				if(ref == null) synchronized(this){
					ref = map.get(name);
					if(ref == null) {
						ref = factory.process(name);
						if(ref != null) map.put(name, ref);
					}
				}
				if(ref != null) return ref.process(null);
			}
			return null;
		}	
	}
	
	public static class MixProxy<U> implements IProcessor<Object, U>{
		protected volatile HashMap<Object, IProcessor<Object, U>> constraint = new HashMap<Object, IProcessor<Object, U>>();
		protected IProcessor<Object, U> defaultProcessor;
		protected Boolean compatible = false;

		public void setConstraint(Map<IProcessor<Object, U>, Collection<Object>> constraint) {
			HashMap<Object, IProcessor<Object, U>> map = new HashMap<Object, IProcessor<Object, U>>();
			if(constraint != null) for(Entry<IProcessor<Object, U>, Collection<Object>> entry: constraint.entrySet())
				if(entry.getKey() != null && entry.getValue() != null) for(Object o : entry.getValue()) map.put(o, entry.getKey());
			this.constraint = map;
		}

		public void setDefaultProcessor(IProcessor<Object, U> defaultProcessor) {
			this.defaultProcessor = defaultProcessor;
		}

		public void setCompatible(Boolean compatible) {
			if(compatible != null) this.compatible = compatible;
		}

		@Override
		public U process(Object name) {
			U ret = null;
			if(name != null) {
				IProcessor<Object, U> processor = constraint.get(name);
				if(processor == null) processor = defaultProcessor;
				if(processor != null) ret = processor.process(name);
				if(ret == null && compatible && defaultProcessor != null && defaultProcessor != processor) ret = defaultProcessor.process(name);
			}
			return ret;
		}	
	}
}
