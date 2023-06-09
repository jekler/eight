package net.yeeyaa.eight.share.processor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class ConfigProcessor implements IProcessor<Object, Map<String, Object>>, IListableResource<String, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected String[] paras;
	protected IProcessor<String[], IListableResource<Object, Object>> resource;
	protected volatile Map<String, Object> config;
	protected volatile IListableResource<Object, Object> cache;
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if (config == null) return 0L;
			else return new Long(config.size());
		}
	};
	
	public void setParas(String paras) {
		if (paras != null) {
			String[] tmp = paras.split("\\|\\|");
			if (tmp.length > 1 && tmp[0].trim().length() > 0 && tmp[1].trim().length() > 0) this.paras = tmp; 
		}
	}

	public void setResource(IProcessor<String[], IListableResource<Object, Object>> resource) {
		this.resource = resource;
	}

	public void setConfig(Map<String, Object> config) {
		this.config = config;
	}

	@PostConstruct
	public void initialize() {
		if (paras != null && resource != null) {
			cache = resource.process(paras[0].split("\\|"));
			paras = paras[1].split("\\|");
		}
	}
	
	@Override
	public Map<String, Object> process(Object instance) {
		Map<String, Object> ret = new HashMap<String, Object>();
		if (cache != null) {
			Map<Object[], Object> content = cache.all(paras);
			Map<String, Object> config = new ConcurrentHashMap<String, Object>(content.size() * 2);
			for (Entry<Object[], Object> entry : content.entrySet()) {
				ret.put(entry.getKey()[entry.getKey().length - 1].toString(), entry.getValue());
				config.put(entry.getKey()[entry.getKey().length - 1].toString(), entry.getValue());
			}
			this.config = config;
		}
		return ret;
	}

	@Override
	public Object find(String... paras) {
		if (config == null) return null;
		else return config.get(paras[0]);
	}

	@Override
	public <P> P store(Object value, String... paras) {
		if (config != null) config.put(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(String... paras) {
		if (config != null) config.remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(String... paras){
		if (config != null) config =  new ConcurrentHashMap<String, Object>();
		return null;
	}

	@Override
	public Collection<String[]> keys(String... paras) {
		if (config == null) return new ArrayList<String[]>();
		else {
			ArrayList<String[]> ret = new ArrayList<String[]>(config.size());
			for (String key : config.keySet()) ret.add(new String[]{key});
			return ret;
		}
	}

	@Override
	public Map<String[], Object> all(String... paras) {
		if (config == null) return new HashMap<String[], Object>();
		else {
			HashMap<String[], Object> ret = new HashMap<String[], Object>(config.size() * 2);
			for (Entry<String, Object> entry : config.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
			return ret;
		}
	}
	
	public class GetConfig implements IProcessor<String, Object> {
		protected Boolean reload = false;
		
		public void setReload(Boolean reload) {
			if (reload != null) this.reload = reload;
		}

		@Override
		public Object process(String instance) {
			if (reload) ConfigProcessor.this.process(null);
			if (config == null) return null;
			else return config.get(instance);
		}		
	}
	
	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : resourceMethods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return resourceMethods;
	}
}
