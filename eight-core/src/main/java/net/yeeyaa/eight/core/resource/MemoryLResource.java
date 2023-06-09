package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class MemoryLResource<K, V> implements IListableResource<K, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			return new Long(resource.size());
		}
	};
	protected final Map<K, V> resource;
	
	public MemoryLResource() {
		resource = new ConcurrentHashMap<K, V>();
	}

	public MemoryLResource(Map<K, V> resource) {
		this.resource = resource;
	}

	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 0) return resource.get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paras != null && paras.length > 0) resource.put(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 0) resource.remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		resource.clear();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K> c = resource.keySet();
		Collection<K[]> ls = new ArrayList<K[]>(c.size());
		for(K o : c) {
			K[] key = PlatformUtil.newArrayOf(1, o);
			key[0] = o;
			ls.add(key);
		}
		return ls;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Map<K[], V> map = new HashMap<K[], V>(resource.size());
		for(Entry<K, V> o : resource.entrySet()) {
			K[] key = PlatformUtil.newArrayOf(1, o.getKey());
			key[0] = o.getKey();
			map.put(key, o.getValue());
		}
		return map;
	}
	
	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
}
