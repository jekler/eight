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


public class CascadeBFLXResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected Boolean setBack = true;
	protected IProcessor<Object, Object> beanHolder;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length > 1) {
				int paraLength = (Integer)paras[1];
				if(paraLength >= 0 && paras.length >= paraLength + 2){
					Object input = beanHolder.process(paras[0]);
					if(IInputResource.class.isInstance(input)){		
						K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
						for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
						Object hm = ((IInputResource)input).find(reparas);
						if(Map.class.isInstance(hm)) return new Long(((Map)hm).size());
					}
				}
			}
			return null;
		}
	};
	
	public CascadeBFLXResource() {
		this.log = LoggerFactory.getLogger(CascadeBFLXResource.class);
	}
	
	public CascadeBFLXResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CascadeBFLXResource.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setSetBack(Boolean setBack) {
		if(setBack != null) this.setBack = setBack;
	}

	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > 2) try{
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length > paraLength + 2){
				Object input = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(input)){
					K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
					for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+2];
					Object hm = ((IInputResource)input).find(reparas);
					if(Map.class.isInstance(hm)) {
						ArrayList<Object> ret = new ArrayList<Object>(paras.length - paraLength - 2);
						for(int i = paraLength + 2; i < paras.length; i++)try{
							ret.add(((Map)hm).get(paras[i]));
						}catch(Exception e){
							ret.add(null);
						}
						return ret;
					}
				}
			}
		}catch(Exception e){
			log.error("CascadeBFLXResource: get resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(paras != null && paras.length >= 2) try{
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length >= paraLength + 2){
				Object resource = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(resource) || IOutputResource.class.isInstance(resource)){
					K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
					for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+2];
					Object hm = null;
					if(IInputResource.class.isInstance(resource)) hm = ((IInputResource)resource).find(reparas);
					if(!Map.class.isInstance(hm) && IOutputResource.class.isInstance(resource)) hm = new ConcurrentHashMap<String, Object>();	
					if(Map.class.isInstance(hm)){
						if(paras.length == paraLength + 2 && Map.class.isInstance(value)) ((Map)hm).putAll(((Map)value));
						else if(paras.length > paraLength + 2) ((Map)hm).put(paras[paraLength + 2], value);
						if(setBack && IOutputResource.class.isInstance(resource)) ((IOutputResource)resource).store(hm, reparas);
					}
				}
			}
		}catch(Exception e){
			log.error("CascadeBFLXResource: set resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 2) try{
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length > paraLength + 2){
				Object input = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(input)){
					K[] reparas = PlatformUtil.newArrayOf(paras, paraLength);
					for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+2];
					Object hm = ((IInputResource)input).find(reparas);
					if(Map.class.isInstance(hm)) {
						for(int i = paraLength + 2; i < paras.length; i++) {
							((Map)hm).remove(paras[i]);
						}
						if(setBack && IOutputResource.class.isInstance(input)) ((IOutputResource)input).store(hm, reparas);
					}
				}
			}
		}catch(Exception e){
			log.error("CascadeBFLXResource: remove resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 1) try{
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length >= paraLength + 2){
				Object input = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(input)){		
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
					Object hm = ((IInputResource)input).find(reparas);
					if(Map.class.isInstance(hm)){
						((Map)hm).clear();
						if(setBack && IOutputResource.class.isInstance(input)) ((IOutputResource)input).store(hm, reparas);
					}
				}
			}
		}catch(Exception e){
			log.error("CascadeBFLXResource: clear resource error.", e);
		}
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length > 1) {
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length >= paraLength + 2){
				Object input = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(input)){		
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
					Object hm = ((IInputResource)input).find(reparas);
					if(Map.class.isInstance(hm)){
						ArrayList<K[]> ret = new ArrayList<K[]>(((Map)hm).size());
						List<K> ls = new ArrayList<K>(paraLength + 3);
						for(int i = 0; i < paraLength + 2; i ++) ls.add(paras[i]);
						ls.add(null);
						for(K o : ((Map<K, Object>)hm).keySet()){
							ls.set(paraLength + 2, o);
							ret.add(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())));
						}
						return ret;
					}
				}
			}
		}
		return null;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		if(paras != null && paras.length > 1) {
			int paraLength = (Integer)paras[1];
			if(paraLength >= 0 && paras.length >= paraLength + 2){
				Object input = beanHolder.process(paras[0]);
				if(IInputResource.class.isInstance(input)){		
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
					Object hm = ((IInputResource)input).find(reparas);
					if(Map.class.isInstance(hm)){
						Map<K[], Object> ret = new HashMap<K[], Object>(((Map)hm).size());
						List<K> ls = new ArrayList<K>(paraLength + 3);
						for(int i = 0; i < paraLength + 2; i ++) ls.add(paras[i]);
						ls.add(null);
						for(Entry<K, Object> o : ((Map<K, Object>)hm).entrySet()){
							ls.set(paraLength + 2, o.getKey());
							ret.put(ls.toArray(PlatformUtil.newArrayOf(paras, ls.size())), o.getValue());
						}
						return ret;
					}
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
