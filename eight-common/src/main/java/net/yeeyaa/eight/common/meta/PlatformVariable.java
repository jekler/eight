package net.yeeyaa.eight.common.meta;

import java.io.Serializable;
import java.util.Arrays;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.core.util.TypeConvertor;

import org.springframework.beans.factory.BeanNameAware;


public class PlatformVariable<R> implements IProcessor<Object, Object>, IMetaBean, BeanNameAware {
	protected Object[] key;
	protected IResource<Object, R> resource;
	protected volatile R variable;
	protected Object factory;

	public PlatformVariable(){};
	
	public PlatformVariable(R variable) {
		this.variable = variable;
	}

	@Override
	public void setBeanName(String name) {
		if (factory == null) factory = name; 
	}
	
	public Object getFactory() {
		return factory;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public void setKey(Object[] key) {
		this.key = key;
	}

	public void setVariable(R variable) {
		this.variable = variable;
	}

	public void setResource(IResource<Object, R> resource) {
		this.resource = resource;
	}

	@Override
	public Object getInitValue() {
		return new Object[]{new InitValue(key), variable};
	}

	@Override
	public void setInitValue(Object initValue) {
		if(initValue instanceof Object[] && ((Object[])initValue).length > 1 && ((Object[])initValue)[0] instanceof InitValue) {
			key = ((InitValue)((Object[])initValue)[0]).getKey();
			variable = (R)((Object[])initValue)[1];
		}
	}
	
	public void initialize(){
		if (resource != null) {
			resource.store(variable, key);
			variable = null;
		}
	}
	
	public void reset(){
		if (resource != null) resource.store(variable, key);
	}
	
	public R get() {
		if(resource == null) return variable;
		else return resource.find(key);
	}

	public void set(R variable) {
		if(resource == null)  this.variable = variable;
		else this.resource.store(variable, key);
	}

	public R remove() {
		if(resource == null) {
			R tmp = variable;
			variable = null;
			return tmp;
		} else return resource.discard(key);
	}

	public <T> T execute(final IProcessor<PlatformVariable<R>, T> processor) {
		if ((resource instanceof ITransactionResource)) return ((ITransactionResource<Object, R, IResource<Object, R>, T>)resource).execute(new IProcessor<IResource<Object, R>, T>() {
			public T process(IResource<Object, R> resource) {
				PlatformVariable<R> local = new PlatformVariable<R>();
				local.key = key;
				local.variable = variable;
				local.resource = resource;
				return processor.process(local);
			}
	    }); else return processor.process(this);
	}

	public Object process(final Object instance) { 
		if (instance == null) return resource.discard(key);
		else if (instance instanceof Object[]) if (((Object[])instance).length == 0) return resource.find(key);
		else resource.store((R)((Object[])instance)[0], key);
		else if (instance instanceof IProcessor) if(resource instanceof ITransactionResource) return ((ITransactionResource<Object, R, IResource<Object, R>, Object>)resource).execute(new IProcessor<IResource<Object, R>, Object>() {
			public Object process(IResource<Object, R> resource) {
				PlatformVariable<R> local = new PlatformVariable<R>();
				local.key = key;
				local.variable = variable;
				local.resource = resource;
				return ((IProcessor<PlatformVariable<R>, Object>)instance).process(local);
			}
		}); else return ((IProcessor<PlatformVariable<R>, Object>)instance).process(this);
		return null;
	}
	
	public boolean equals(Object obj) {
		if (this == obj) return true;
		else if ((obj instanceof PlatformVariable)) {
			PlatformVariable<R> other = (PlatformVariable<R>) obj;
			return (resource == other.resource ||(resource != null && resource.equals(other.resource))) && PlatformUtil.compare(key, other.key) && PlatformUtil.compare(variable, other.variable);
		}
		return false;
	}

	public int hashCode() {
		int hash = 0;
		if (key != null) hash = key.hashCode();
		if (resource != null) hash = hash * 13 + resource.hashCode();
		if (variable != null) hash = hash * 17 + variable.hashCode();
		return hash;
	}

	public PlatformVariable<R> newInstance(){
		return newInstance(null, 0);
	}
	
	public PlatformVariable<R> newInstance(Object paras){
		return newInstance(paras, 0);
	}
	
	public PlatformVariable<R> newInstance(Object paras, int mode) {
		PlatformVariable<R> local = new PlatformVariable<R>();
		local.resource = resource;
		local.key = key;
		local.factory = factory;
		local.variable = variable;
		switch(mode){
			case 0 : if(paras instanceof IResource) {
						local.resource = (IResource<Object, R>) paras;
						local.factory = null;
					 } else if(paras instanceof Object[]) local.key = (Object[]) paras;
					 else if(paras instanceof PlatformVariable) local.key = ((PlatformVariable<R>)paras).key;
					 else local.variable = (R)paras;
			break;
			case 1: if(paras instanceof IResource) {
						local.resource = (IResource<Object, R>) paras;
						local.factory = null;
					} else if(paras instanceof Object[]) local.key = (Object[]) paras;
			 		else if(paras instanceof PlatformVariable) {
			 			local.resource = ((PlatformVariable<R>)paras).resource;
			 			local.factory = ((PlatformVariable<R>)paras).factory;
			 		} else local.variable = (R)paras;
			break;
			default: local.variable = (R)paras;
		}
		return local;
	}

	public class Instance implements IProcessor<Object, PlatformVariable<R>> {
		protected Integer mode = 0;
		
		public void setMode(Integer mode) {
			if(mode != null) this.mode = mode;
		}
		
		public PlatformVariable<R> process(Object paras) {
			return newInstance(paras, mode);
		}
	}
	
	protected static class InitValue implements Serializable{
		private static final long serialVersionUID = -660981619677647441L;
		protected Object[] key;

		public InitValue(Object[] key) {
			this.key = key;
		}

		public Object[] getKey() {
			return key;
		}
	}
	
	public static class Key implements IProcessor<Object, Object[]> {
		protected Object[] key;
		protected String prefix;
		protected String suffix;
		protected Integer index = 0;
		protected IProcessor<Object, Object> keyGenerator;

		public void setIndex(Integer index) {
			if (index != null && index > 0) this.index = index;
		}

		public void setKeyGenerator(IProcessor<Object, Object> keyGenerator) {
			this.keyGenerator = keyGenerator;
		}

		public void setKey(Object[] key) {
			this.key = key;
		}

		public void setPrefix(String prefix) {
			this.prefix = prefix;
		}

		public void setSuffix(String suffix) {
			this.suffix = suffix;
		}

		public Object[] process(Object instance) {
			if (key == null) key = new String[1];
			if (index >= key.length) index = key.length - 1;
			StringBuilder sb = new StringBuilder();
			if (prefix != null) sb.append(prefix);
			if (keyGenerator != null) sb.append(keyGenerator.process(instance));
			else if (instance != null) sb.append(instance);
			else sb.append(TypeConvertor.randomUuid());
			if (suffix != null) sb.append(suffix);
			key[index] = sb.toString();
			return Arrays.copyOf(key, key.length);
		}
	}
}
