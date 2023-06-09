package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
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


public class MemcacheMapFLResource<K, V> implements IListableResource<K, V>, IExtendable<Object> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected CASMutator<Map<K, V>> mutator;
	protected Integer retry = 10;
	protected MemcachedClient mc;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				Map<K, V> map;
				if (touch && timeout > 0) {
					CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
					map = value == null ? null : (Map<K, V>) value.getValue();
				} else map = ((Map<K, V>)mc.get(paras[0].toString()));
				return new Long(map.size());
			}
			return null;
		}
	};
	
	public MemcacheMapFLResource() {
		log = LoggerFactory.getLogger(MemcacheMapFLResource.class);
	}

	public MemcacheMapFLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheMapFLResource.class) : log;
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
		this.mutator = new CASMutator<Map<K, V>>(mc, tc, retry);
	}

	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 1 && paras[0] != null && paras[1] != null)  {
			Map<K, V> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, V>) value.getValue();
			} else map = ((Map<K, V>)mc.get(paras[0].toString()));
			if(map != null) return map.get(paras[1]);
		}
		return null;
	}

	@Override
	public <P> P store(final V value, final K ... paras) {
		if(paras != null && paras.length > 1 && paras[0] != null && paras[1] != null) try {
			CASMutation<Map<K, V>> mutation=new CASMutation<Map<K, V>>() {
			     public Map<K, V> getNewValue(Map<K, V> current) {
			    	 current.put(paras[1], value);
			         return current;
			     }
			 };
			 Map<K, V> init = new ConcurrentHashMap<K, V>();
			 init.put(paras[1], value);
			 mutator.cas(paras[0].toString(), init, timeout, mutation);
		}catch(Exception e){
			log.error("put value fail:", e);
		}
		return null;
	}

	@Override
	public <P> P discard(final K ... paras) {
		if(paras != null && paras.length > 1 && paras[0] != null && paras[1] != null) try {
			CASMutation<Map<K, V>> mutation=new CASMutation<Map<K, V>>() {
			     public Map<K, V> getNewValue(Map<K, V> current) {
			    	 current.remove(paras[1]);
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
			Map<K, V> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, V>) value.getValue();
			} else map = (Map<K, V>)mc.get(paras[0].toString());
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
	public Map<K[], V> all(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			Map<K, V> map;
			if (touch && timeout > 0) {
				CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
				map = value == null ? null : (Map<K, V>) value.getValue();
			} else map = (Map<K, V>)mc.get(paras[0].toString());
			Map<K[], V> ret = new HashMap<K[], V>(map == null ? 0 : map.size());
			if(map != null) for(Entry<K, V> o : map.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(2, paras[0]);
				k[0] = paras[0];
				k[1] = o.getKey();
				ret.put(k, o.getValue());
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
