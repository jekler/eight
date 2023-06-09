package net.yeeyaa.eight.core.resource;

import java.util.Collection;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class ResourceExtend<K,V> implements IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count, ResourceMethod.status});
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return new Long(object instanceof Object[] ? resource.keys((K[])object).size() : resource.keys((K)object).size());
		}
	};
	protected final IProcessor<Object, Object> status = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return object instanceof Object[] ? resource.find((K[])object) == null ? -1 : 0 : resource.find((K)object) == null ? -1 : 0;
		}	
	}; 
	protected final IReadonlyListable<K, V> resource;
	
	public ResourceExtend(IReadonlyListable<K, V> resource) {
		this.resource = resource;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
				case status : return (N) status;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
}
