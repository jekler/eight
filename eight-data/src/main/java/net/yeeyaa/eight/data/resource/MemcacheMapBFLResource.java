package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import net.spy.memcached.CASMutation;
import net.spy.memcached.CASMutator;
import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.spy.memcached.transcoders.Transcoder;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MemcacheMapBFLResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected CASMutator<Map<K, Object>> mutator;
	protected Integer retry = 10;
	protected MemcachedClient mc;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				Map<K, Object> map;
				if (touch && timeout > 0) {
					CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
					map = value == null ? null : (Map<K, Object>) value.getValue();
				} else map = ((Map<K, Object>)mc.get(paras[0].toString()));
				return new Long(map.size());
			}
			return null;
		}
	};
	
	public MemcacheMapBFLResource() {
		log = LoggerFactory.getLogger(MemcacheMapBFLResource.class);
	}

	public MemcacheMapBFLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheMapBFLResource.class) : log;
	}
	
	public void setTouch(Boolean touch) {
		if (touch != null) this.touch = touch;
	}
	
	public void setTimeout(Integer timeout) {
		if (timeout != null && timeout > 0) this.timeout = timeout;
	}

	public void setMc(MemcachedClient mc) {
		this.mc = mc;
	}
	
	public void setRetry(Integer retry) {
		if (retry != null && retry >= 0) this.retry = retry;
	}
	
	@PostConstruct
	public void init() {
		Transcoder tc = mc.getTranscoder();
		this.mutator = new CASMutator<Map<K, Object>>(mc, tc, retry);
	}

	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > 1){
			Map<K, Object> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, Object>) value.getValue();
			} else map = ((Map<K, Object>)mc.get(paras[0].toString()));
			List<Object> ret = new ArrayList<Object>(paras.length - 1);
			if(map != null) for(int i = 1; i < paras.length; i++) ret.add(map.get(paras[i]));
			if(ret.size() < paras.length) for(int i = 0; i < paras.length - ret.size(); i++) ret.add(null);
			return ret;
		}else return null;
	}

	@Override
	public <P> P store(final Object value, final K ... paras) {
		if(paras != null && paras.length > 0) if(paras.length > 1) try {
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 current.put(paras[1], value);
			         return current;
			     }
			 };
			 Map<K, Object> init = new ConcurrentHashMap<K, Object>();
			 init.put(paras[1], value);
			 mutator.cas(paras[0].toString(), init, timeout, mutation);
		}catch(Exception e){
			log.error("put value fail:", e);
		} else if(value instanceof Map) try {
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 current.putAll((Map<K, Object>) value);
			         return current;
			     }
			 };
			 Map<K, Object> init = new ConcurrentHashMap<K, Object>();
			 init.putAll((Map<K, Object>) value);
			 mutator.cas(paras[0].toString(), init, timeout, mutation);
		}catch(Exception e){
			log.error("put value fail:", e);
		}
		return null;
	}

	@Override
	public <P> P discard(final K ... paras) {
		if(paras != null && paras.length > 1 && paras[0] != null) try {
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 for(int i = 1; i < paras.length; i++) current.remove(paras[i]);
			         return current;
			     }
			 };
			 mutator.cas(paras[0].toString(), null, timeout, mutation);
		}catch(Exception e){
			log.error("remove value fail:", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) mc.delete(paras[0].toString());
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			Map<K, Object> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, Object>) value.getValue();
			} else map = ((Map<K, Object>)mc.get(paras[0].toString()));
			Collection<K[]> ls = new ArrayList<K[]>(map == null ? 0 : map.size());
			if(map != null) for(K o : map.keySet()) {
				K[] k = PlatformUtil.newArrayOf(2, paras[0]);
				k[0] = paras[0];
				k[1] = o;
				ls.add(k);
			}
			return ls;
		}
		return null;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			Map<K, Object> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, Object>) value.getValue();
			} else map = ((Map<K, Object>)mc.get(paras[0].toString()));
			Map<K[], Object> ls = new HashMap<K[], Object>(map == null ? 0 : map.size());
			if(map != null) for(Entry<K, Object> o : map.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(2, paras[0]);
				k[0] = paras[0];
				k[1] = o.getKey();
				ls.put(k, o.getValue());
			}
			return ls;
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
