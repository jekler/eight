package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
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


public class CascadeLPResource<K, V, U extends IInputResource<K, V>> extends ProxyResource<U> implements IListableResource<K, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Boolean setBack = true;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length > 0 && resource != null) {
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)) return new Long(((Map)hm).size());
			}
			return null;
		}
	};
	
	public void setSetBack(Boolean setBack) {
		if(setBack != null) this.setBack = setBack;
	}
	
	@Override
	public V find(K ... paras) {
		if(paras == null || paras.length == 0 || resource == null) return null;
		K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
		for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
		Object hm = resource.find(reparas);
		if(Map.class.isInstance(hm)) return ((Map<K, V>)hm).get(paras[paras.length - 1]);
		return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paras == null || paras.length == 0 || resource == null) return null;
		K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
		for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
		Object hm = resource.find(reparas);
		if(!Map.class.isInstance(hm) && resource instanceof IOutputResource) hm = new ConcurrentHashMap<String, Object>();	
		if(Map.class.isInstance(hm)){
			((Map)hm).put(paras[paras.length - 1], value);
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras == null || paras.length == 0 || resource == null) return null;
		K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
		for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
		Object hm = resource.find(reparas);
		if(Map.class.isInstance(hm)&&((Map)hm).containsKey(paras[paras.length - 1])){
			((Map<K, V>)hm).remove(paras[paras.length - 1]);
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras == null || paras.length == 0 || resource == null) return null;
		Object hm = resource.find(paras);
		if(Map.class.isInstance(hm)){
			((Map)hm).clear();
			if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, paras);
		}
		return null;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length > 0 && resource != null) {
			Object hm = resource.find(paras);
			if(Map.class.isInstance(hm)){
				ArrayList<K[]> ret = new ArrayList<K[]>(((Map)hm).size());
				List<K> ls = new ArrayList(Arrays.asList(paras));
				ls.add(null);
				for(K o : ((Map<K, V>)hm).keySet()){
					ls.set(paras.length, o);
					ret.add(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())));
				}
				return ret;
			}
		}
		return null;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		if(paras != null && paras.length > 0 && resource != null) {
			Object hm = resource.find(paras);
			if(Map.class.isInstance(hm)){
				Map<K[], V> ret = new HashMap<K[], V>(((Map)hm).size());
				List<K> ls = new ArrayList(Arrays.asList(paras));
				ls.add(null);
				for(Entry<K, V> o : ((Map<K, V>)hm).entrySet()){
					ls.set(paras.length, o.getKey());
					ret.put(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())), o.getValue());
				}
				return ret;
			}
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
