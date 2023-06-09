package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class InheritableLResource<K, V> implements IListableResource<K, V> {
	protected final InheritableThreadLocal<ConcurrentHashMap<K, V>> resources = new InheritableThreadLocal<ConcurrentHashMap<K, V>>(){
		   protected ConcurrentHashMap<K, V> initialValue() {
	           return new ConcurrentHashMap<K, V>();
	       }
	};
	
	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 0) return resources.get().get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paras != null && paras.length > 0) resources.get().put(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 0) resources.get().remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		resources.remove();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K> c = resources.get().keySet();
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
		Map<K, V> c = resources.get();
		Map<K[], V> map = new HashMap<K[], V>(c.size());
		for(Entry<K, V> o : c.entrySet()) {
			K[] key = PlatformUtil.newArrayOf(1, o.getKey());
			key[0] = o.getKey();
			map.put(key, o.getValue());
		}
		return map;
	}
}
