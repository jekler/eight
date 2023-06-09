package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ChainLPResource<K, V, U extends IInputResource<K, V>> extends ProxyResource<U> implements IListableResource<K, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected K[] key;
	protected Boolean setBack = true;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(key == null || resource == null) return null;
			Object hm = resource.find(key);
			if(Map.class.isInstance(hm)) return new Long(((Map)hm).size());
			return null;
		}
	};
	
	public void setSetBack(Boolean setBack) {
		if(setBack != null) this.setBack = setBack;
	}
	
	public void setKey(K[] key) {
		if (key!= null)this.key = key;
	}

	@Override
	public V find(K ... paras) {
		if(key == null || paras == null || paras.length == 0 || resource == null) return null;
		Object hm = resource.find(key);
		if(Map.class.isInstance(hm)) return ((Map<K, V>)hm).get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(key == null || paras == null || paras.length == 0  || resource == null) return null;
		Object hm = resource.find(key);
		if(!Map.class.isInstance(hm) && resource instanceof IOutputResource) hm = new ConcurrentHashMap<String, Object>();	
		if(Map.class.isInstance(hm)){
			((Map)hm).put(paras[0], value);
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, key);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(key == null || paras == null || resource == null) return null;
		Object hm = resource.find(key);
		if(Map.class.isInstance(hm)){		
			((Map<K, V>)hm).remove(paras[0]);
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, key);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(key == null || resource == null) return null;
		Object hm = resource.find(key);
		if(Map.class.isInstance(hm)){
			((Map)hm).clear();
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, key);
		}
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		if(key == null || resource == null) return null;
		Object hm = resource.find(key);
		if(Map.class.isInstance(hm)){
			ArrayList<K[]> ret = new ArrayList<K[]>(((Map)hm).size());
			for(K o : ((Map<K, V>)hm).keySet()){
				K[] key = PlatformUtil.newArrayOf(paras, 1);
				key[0] = o;
				ret.add(key);
			}
			return ret;
		}
		return null;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		if(key == null || resource == null) return null;
		Object hm = resource.find(key);
		if(Map.class.isInstance(hm)){
			Map<K[], V> ret = new HashMap<K[], V>(((Map)hm).size());
			for(Entry<K, V> o : ((Map<K, V>)hm).entrySet()){
				K[] key = PlatformUtil.newArrayOf(paras, 1);
				key[0] = o.getKey();
				ret.put(key, o.getValue());
			}
			return ret;
		}
		return null;
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
