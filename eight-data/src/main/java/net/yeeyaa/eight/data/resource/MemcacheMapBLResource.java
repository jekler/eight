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


public class MemcacheMapBLResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected String key;
	protected CASMutator<Map<K, Object>> mutator;
	protected Integer retry = 10;
	protected MemcachedClient mc;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			Map<K, Object> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(key, timeout);
				map = value == null ? null : (Map<K, Object>) value.getValue();
			} else map = ((Map<K, Object>)mc.get(key));
			return new Long(map.size());
		}
	};
	
	public MemcacheMapBLResource() {
		log = LoggerFactory.getLogger(MemcacheMapBLResource.class);
	}

	public MemcacheMapBLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheMapBLResource.class) : log;
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

	public void setKey(String key) {
		this.key = key;
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
		if(paras != null && paras.length > 0)   {
			Map<K, Object> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(key, timeout);
				map = value == null ? null : (Map<K, Object>) value.getValue();
			} else map = ((Map<K, Object>)mc.get(key));
			List<Object> ls = new ArrayList<Object>(paras.length);
			for(K key : paras) if(map != null) ls.add(map.get(key));
			else ls.add(null);
			return ls;
		}
		return null;
	}

	@Override
	public <P> P store(final Object value, final K ... paras) {
		if(paras != null && paras.length > 0){
			if(paras[0] != null) try{
				CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
				     public Map<K, Object> getNewValue(Map<K, Object> current) {
				    	 current.put(paras[0], value);
				         return current;
				     }
				 };
				 Map<K, Object> init = new ConcurrentHashMap<K, Object>();
				 init.put(paras[0], value);
				 mutator.cas(key, init, timeout, mutation);
			} catch(Exception e){
				log.error("put value fail:", e);
			}
		}else if(Map.class.isInstance(value)) try{
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 current.putAll((Map<K, Object>)value);
			         return current;
			     }
			 };
			 Map<K, Object> init = new ConcurrentHashMap<K, Object>();
			 init.putAll((Map<K, Object>)value);
			 mutator.cas(key, init, timeout, mutation);
		} catch(Exception e){
			log.error("put value fail:", e);
		}
		return null;
	}

	@Override
	public <P> P discard(final K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null)  try {
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 for(K key : paras) current.remove(key);
			         return current;
			     }
			 };
			 mutator.cas(key, null, timeout, mutation);
		}catch(Exception e){
			log.error("remove value fail:", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		mc.delete(key);
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Map<K, Object> map;
		if (touch && timeout > 0) {
			CASValue<Object> value = mc.getAndTouch(key, timeout);
			map = value == null ? null : (Map<K, Object>) value.getValue();
		} else map = (Map<K, Object>)mc.get(key);
		Collection<K[]> ls = new ArrayList<K[]>(map == null ? 0 : map.size());
		if(map != null) for(K o : map.keySet()) {
			K[] k = PlatformUtil.newArrayOf(1, o);
			k[0] = o;
			ls.add(k);
		}
		return ls;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		Map<K, Object> map;
		if (touch && timeout > 0) {
			CASValue<Object> value = mc.getAndTouch(key, timeout);
			map = value == null ? null : (Map<K, Object>) value.getValue();
		} else map = (Map<K, Object>)mc.get(key);
		Map<K[], Object> ret = new HashMap<K[], Object>(map == null ? 0 : map.size());
		if(map != null) for(Entry<K, Object> o : map.entrySet()) {
			K[] k = PlatformUtil.newArrayOf(1, o.getKey());
			k[0] = o.getKey();
			ret.put(k, o.getValue());
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
