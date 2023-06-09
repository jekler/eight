package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
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


public class CascadeBLPResource<K, U extends IInputResource<K, Object>> extends ProxyResource<U> implements IListableResource<K, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Integer paraLength = 1;
	protected Boolean setBack = true;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length >= paraLength && resource != null) {
				Object hm = resource.find(paras);
				if(Map.class.isInstance(hm)) return new Long(((Map)hm).size());
			}
			return null;
		}
	};
	
	public void setParaLength(Integer paraLength) {
		if(paraLength != null && paraLength >1) this.paraLength = paraLength;
	}

	public void setSetBack(Boolean setBack) {
		if(setBack != null) this.setBack = setBack;
	}
	
	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > paraLength && resource != null) {
			K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
			for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
			Object hm = resource.find(reparas);
			if(Map.class.isInstance(hm)) {
				ArrayList<Object> ret = new ArrayList<Object>(paras.length - paraLength);
				for(int i = paraLength; i < paras.length; i++)try{
					ret.add(((Map)hm).get(paras[i]));
				}catch(Exception e){
					ret.add(null);
				}
				return ret;
			}
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(paras != null && paras.length >= paraLength && resource != null){
			K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
			for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
			Object hm = resource.find(reparas);
			if(!Map.class.isInstance(hm) && resource instanceof IOutputResource) hm = new ConcurrentHashMap<String, Object>();	
			if(Map.class.isInstance(hm)){
				if(Map.class.isInstance(value) && paras.length == paraLength) ((Map)hm).putAll((Map)value);
				else if(paras.length > paraLength) ((Map)hm).put(paras[paraLength], value);
				if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
			}
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > paraLength && resource != null) {
			K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
			for(int i =0; i < reparas.length; i++) reparas[i] = paras[i];
			Object hm = resource.find(reparas);
			if(Map.class.isInstance(hm)) {
				for(int i = paraLength; i < paras.length; i++) {
					((Map)hm).remove(paras[i]);
				}
				if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
			}
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length >= paraLength && resource != null){
			Object hm = resource.find(paras);
			if(Map.class.isInstance(hm)){
				((Map)hm).clear();
				if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, paras);
			}
		}
		return null;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length >= paraLength && resource != null) {
			Object hm = resource.find(paras);
			if(Map.class.isInstance(hm)){
				ArrayList<K[]> ret = new ArrayList<K[]>(((Map)hm).size());
				List<K> ls = new ArrayList<K>(paraLength + 1);
				for(int i = 0; i < paraLength; i ++) ls.add(paras[i]);
				ls.add(null);
				for(K o : ((Map<K, Object>)hm).keySet()){
					ls.set(paraLength, o);
					ret.add(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())));
				}
				return ret;
			}
		}
		return null;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		if(paras != null && paras.length >= paraLength && resource != null) {
			Object hm = resource.find(paras);
			if(Map.class.isInstance(hm)){
				Map<K[], Object> ret = new HashMap<K[], Object>(((Map)hm).size());
				List<K> ls = new ArrayList<K>(paraLength + 1);
				for(int i = 0; i < paraLength; i ++) ls.add(paras[i]);
				ls.add(null);
				for(Entry<K, Object> o : ((Map<K, Object>)hm).entrySet()){
					ls.set(paraLength, o.getKey());
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
