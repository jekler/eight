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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CascadeBLPXResource<K, U extends IInputResource<K, Object>> extends ProxyResource<U> implements IListableResource<K, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected Boolean setBack = true;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length > 0) {
				int paraLength = (Integer)paras[0];
				if(paras != null && paras.length > paraLength && resource != null) {
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
					for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
					Object hm = resource.find(reparas);
					if(Map.class.isInstance(hm)) return new Long(((Map)hm).size());
				}
			}
			return null;
		}
	};
	
	public CascadeBLPXResource() {
		this.log = LoggerFactory.getLogger(CascadeBLPXResource.class);
	}
	
	public CascadeBLPXResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CascadeBLPXResource.class) : log;
	}
	
	public void setSetBack(Boolean setBack) {
		if(setBack != null) this.setBack = setBack;
	}
	
	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > 1 && resource != null) try{
			int paraLength = (Integer)paras[0];
			if(paraLength >= 0 && paras.length > paraLength + 1){
				K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)) {
					ArrayList<Object> ret = new ArrayList<Object>(paras.length - paraLength);
					for(int i = paraLength + 1; i < paras.length; i++)try{
						ret.add(((Map)hm).get(paras[i]));
					}catch(Exception e){
						ret.add(null);
					}
					return ret;
				}
			}
		}catch(Exception e){
			log.error("CascadeBLPXResource: get resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(paras != null && paras.length > 0 && resource != null) try{
			int paraLength = (Integer)paras[0];
			if(paraLength >= 0 && paras.length >= paraLength + 1){
				K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(!Map.class.isInstance(hm) && resource instanceof IOutputResource) hm = new ConcurrentHashMap<String, Object>();	
				if(Map.class.isInstance(hm)){
					if(Map.class.isInstance(value) && paras.length == paraLength + 1) ((Map)hm).putAll((Map)value);
					else if(paras.length > paraLength + 1)	((Map)hm).put(paras[paraLength + 1], value);
					if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);				
				}
			}
		}catch(Exception e){
			log.error("CascadeBLPXResource: set resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 1 && resource != null) try{
			int paraLength = (Integer)paras[0];
			if(paraLength >= 0 && paras.length > paraLength + 1){
				K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)) {
					for(int i = paraLength + 1; i < paras.length; i++) {
						((Map)hm).remove(paras[i]);
					}
					if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
				}
			}
		}catch(Exception e){
			log.error("CascadeBLPXResource: remove resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0 && resource != null) try{
			int paraLength = (Integer)paras[0];
			if(paraLength >= 0 && paras.length >= paraLength + 1){
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)){
					((Map)hm).clear();
					if(setBack && resource instanceof IOutputResource) ((IOutputResource)resource).store(hm, reparas);
				}
			}
		}catch(Exception e){
			log.error("CascadeBLPXResource: clear resource error.", e);
		}
		return null;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length > 0) {
			int paraLength = (Integer)paras[0];
			if(paras != null && paras.length > paraLength && resource != null) {
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)){
					ArrayList<K[]> ret = new ArrayList<K[]>(((Map)hm).size());
					List<K> ls = new ArrayList<K>(paraLength + 2);
					for(int i = 0; i < paraLength + 1; i ++) ls.add(paras[i]);
					ls.add(null);
					for(K o : ((Map<K, Object>)hm).keySet()){
						ls.set(paraLength + 1, o);
						ret.add(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())));
					}
					return ret;
				}
			}
		}
		return null;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		if(paras != null && paras.length > 0) {
			int paraLength = (Integer)paras[0];
			if(paras != null && paras.length > paraLength && resource != null) {
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 1);
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+1];
				Object hm = resource.find(reparas);
				if(Map.class.isInstance(hm)){
					Map<K[], Object> ret = new HashMap<K[], Object>(((Map)hm).size());
					List<K> ls = new ArrayList<K>(paraLength + 2);
					for(int i = 0; i < paraLength + 1; i ++) ls.add(paras[i]);
					ls.add(null);
					for(Entry<K, Object> o : ((Map<K, Object>)hm).entrySet()){
						ls.set(paraLength + 1, o.getKey());
						ret.put(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())), o.getValue());
					}
					return ret;
				}
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
