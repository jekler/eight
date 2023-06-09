package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class CollectionLResource<V> implements IListableResource<Integer, V> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Collection<V> resource = Collections.newSetFromMap(new ConcurrentHashMap<V, Boolean>());
	
	public void setResource(Collection<V> resources) {
		this.resource = resources;
	}

	@Override
	public V find(Integer ... paras) {
		int size = resource.size();
		if (paras == null || paras.length == 0 || paras[0] == null) return (V)resource.toArray()[size - 1];
		else if (paras[0] >= 0 && paras[0] < size) return (V)resource.toArray()[paras[0]];
		else return null;
	}

	@Override
	public <P> P store(V value, Integer ... paras) {
		resource.add(value);
		return null;
	}

	@Override
	public <P> P discard(Integer ... paras) {
		int size = resource.size();
		if (paras == null || paras.length == 0 || paras[0] == null) {
			resource.remove(resource.toArray()[size - 1]);
		} else if (paras[0] >= 0 && paras[0] < size) {
			resource.remove(resource.toArray()[paras[0]]);
		} 
		return null;
	}

	@Override
	public <P> P empty(Integer... paras) {
		resource.clear();
		return null;
	}

	@Override
	public Collection<Integer[]> keys(Integer... paras) {
		int size = resource.size();
		Collection<Integer[]> ret = new ArrayList<Integer[]>(size);
		for(int i = 0; i < size; i++) ret.add(new Integer[]{i});
		return ret;
	}

	@Override
	public Map<Integer[], V> all(Integer... paras) {
		Map<Integer[], V> map = new HashMap<Integer[], V>(resource.size() * 2);
		int i = 0;
		for(V value : resource) map.put(new Integer[]{i++}, value);
		return map;
	}
}
