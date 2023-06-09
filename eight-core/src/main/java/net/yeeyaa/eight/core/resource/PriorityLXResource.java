package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IOutputResource;


public class PriorityLXResource<K, V> implements IListableResource<K, V>{
	protected List<Object> resources;

	public void setResources(List<Object> resources) {
		this.resources = resources;
	}

	protected class Key<K> {
		protected final K[] key;

		public Key(K[] key) {
			this.key = key;
		}

		@Override
		public int hashCode() {
			int code = 0;
			if (key != null) for (K k : key) if (k != null) code = 13* code + k.hashCode();
			return code;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof Key) {
				K[] other = ((Key<K>)obj).key;
				if (key == null) return other == null;
				else if (other != null && key.length == other.length) {
					for(int i = 0; i < key.length; i++) if(!key[i].equals(other[i])) return false;
					return true;
				}
			}
			return false;
		}
	}
	
	@Override
	public V find(K... paras) {
		V ret = null;
		for (Object resource : resources) if (resource instanceof IInputResource) {
			ret = ((IInputResource<K,V>)resource).find(paras);
			if (ret != null) break;
		}
		return ret;
	}

	@Override
	public <P> P store(V value, K... paras) {
		P ret = null;
		for (Object resource : resources) if (resource instanceof IOutputResource) {
			ret = ((IOutputResource<K,V>)resource).store(value, paras);
			break;
		}
		return ret;
	}

	@Override
	public <P> P discard(K... paras) {
		P ret = null;
		for (Object resource : resources) if (resource instanceof IOutputResource) ret = ((IOutputResource<K,V>)resource).discard(paras);
		return ret;
	}

	@Override
	public <P> P empty(K... paras) {
		P ret = null;
		for (Object resource : resources) if (resource instanceof IOutputResource) ret = ((IOutputResource<K,V>)resource).empty(paras);
		return ret;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		HashSet<Key<K>> set = new HashSet<Key<K>>();
		for (Object resource : resources) if (resource instanceof IListable) {
			Collection<K[]> key = ((IListable<K,V>)resource).keys(paras);
			if (key != null && key.size() > 0) for (K[] k : key) set.add(new Key(k));
		}
		ArrayList<K[]> keys = new ArrayList<K[]>(set.size());
		for (Key<K> k : set) keys.add(k.key);
		return keys;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		HashMap<Key<K>, V> map = new HashMap<Key<K>, V>();
		for (Object resource : resources) if (resource instanceof IListable) {
			Map<K[], V> entries = ((IListable<K,V>)resource).all(paras);
			if (entries != null && entries.size() > 0) for (Entry<K[], V> entry : entries.entrySet()) {
				Key<K> k = new Key<K>(entry.getKey());
				if (!map.containsKey(k)) map.put(k, entry.getValue());
			}
		}
		HashMap<K[], V> ret = new HashMap<K[], V>(map.size() * 2);
		for (Entry<Key<K>, V> entry : map.entrySet()) ret.put(entry.getKey().key, entry.getValue());
		return ret;
	}
}
