package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class KeepKeyLPResource<K, V, U extends IReadonlyListable<K, V>> extends ProxyResource<U> implements IListableResource<K, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Set<List<K>> keys;
	protected Boolean readonly = true;
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(keys.size());
		}
	};
	
	public void setReadonly(Boolean readonly) {
		if(readonly != null) this.readonly = readonly;
	}

	public void setKeys(Set<List<K>> keys) {
		this.keys = keys;
	}

	@Override
	public V find(K... paras) {
		if(keys.contains(Arrays.asList(paras))) return resource.find(paras);
		return null;
	}

	@Override
	public <P> P store(V value, K... paras) {
		if(!readonly && resource instanceof IListableResource){
			((IListableResource)resource).store(value, paras);
			keys.add(Arrays.asList(paras));
		}
		return null;
	}

	@Override
	public <P> P discard(K... paras) {
		if(!readonly && resource instanceof IListableResource && keys.contains(Arrays.asList(paras))){
			((IListableResource<K, V>)resource).discard(paras);
			keys.remove(paras);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(!readonly && resource instanceof IListableResource) for(List<K> key : keys) if(key.size() > 0){
			K[] arr = PlatformUtil.newArrayOf(key.size(), key.get(0));
			((IListableResource)resource).discard(key.toArray(arr));
		}
		keys.clear();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K[]> ret = new ArrayList<K[]>(keys.size());
		for(List<K> key : keys) if(key.size() > 0){
			K[] arr = PlatformUtil.newArrayOf(key.size(), key.get(0));
			ret.add(key.toArray(arr));
		}
		return ret;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Map<K[], V> ret = new HashMap<K[], V>(keys.size());
		for(List<K> key : keys) if(key.size() > 0){
			K[] arr = PlatformUtil.newArrayOf(key.size(), key.get(0));
			K[] k = key.toArray(arr);
			ret.put(k, resource.find(k));
		}
		return ret;
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
