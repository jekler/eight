package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class ListLResource<V> implements IListableResource<Integer, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected List<V> resource = new CopyOnWriteArrayList<V>();
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(resource.size());
		}
	};
	
	public void setResource(List<V> resources) {
		this.resource = resources;
	}

	@Override
	public V find(Integer ... paras) {
		int size = resource.size();
		if (paras == null || paras.length == 0 || paras[0] == null) return resource.get(size - 1);
		else if (paras[0] >= 0 && paras[0] < size) return resource.get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(V value, Integer ... paras) {
		if (paras == null || paras.length == 0 || paras[0] == null) resource.add(value);
		else if (paras[0] >= 0 && paras[0] <= resource.size()) resource.add(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(Integer ... paras) {
		int size = resource.size();
		if (paras == null || paras.length == 0 || paras[0] == null) resource.remove(size - 1);
		else if (paras[0] >= 0 && paras[0] < resource.size()) resource.remove(paras[0].intValue());
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
