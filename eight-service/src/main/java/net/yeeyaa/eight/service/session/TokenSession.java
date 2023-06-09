package net.yeeyaa.eight.service.session;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.enumerate.SessionMethod;
import net.yeeyaa.eight.core.util.MapperSet;

public class TokenSession <K, V> implements IListableResource<K, V>, IExtendable<Object>, IProcessor<Object, Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{SessionMethod.isAlive, SessionMethod.create, SessionMethod.destroy, ResourceMethod.count, ResourceMethod.status});
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final IProcessor<Object, Object> create = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return null;
		}
	};
	protected final IProcessor<Object, Object> isAlive = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return true;
		}
	};
	protected final IProcessor<Object, Object> destroy = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			resource.empty((K)object);
			return null;
		}
	};
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return new Long(resource.keys((K)object).size());
		}
	};
	protected final IProcessor<Object, Object> status =  new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			return new Integer(0);
		}	
	};
	protected final IListableResource<K, V> resource;
	protected IProcessor<Object, Map<K, V>> in;
	protected IProcessor<Object, Object> out;

	public TokenSession(IListableResource<K, V> resource) {
		this.resource = resource;
	}

	public void setIn(IProcessor<Object, Map<K, V>> in) {
		this.in = in;
	}

	public void setOut(IProcessor<Object, Object> out) {
		this.out = out;
	}
	
	@Override
	public V find(K... paras) {
		return resource.find(paras);
	}

	@Override
	public <P> P store(V value, K... paras) {
		return resource.store(value, paras);
	}

	@Override
	public <P> P discard(K... paras) {
		return resource.discard(paras);
	}

	@Override
	public <P> P empty(K... paras) {
		return resource.empty(paras);
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		return resource.keys(paras);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return resource.all(paras);
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = methods.process(object);
			if (method instanceof ResourceMethod) switch((ResourceMethod) method) {
				case count : return (N) count;
				case status : return (N) status;
			} else if (method instanceof SessionMethod) switch((SessionMethod) method) {
				case create : return (N) create;
				case destroy: return (N) destroy;
				case isAlive : return (N) isAlive;
			} 
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return resourceMethods;
	}

	@Override
	public Object process(Object object) {
		if (object instanceof Object[]) {
			Object[] para = (Object[]) object;
			if (para.length > 0 && para[0] != null) {
				Map<K, V> map = in.process(para[0]);
				if (map != null && map.size() > 0) for (Entry<K, V> entry : map.entrySet()) resource.store(entry.getValue(), entry.getKey());
				return map;
			} else return null;
		} else return out.process(resource);
	}
}

