package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.spy.memcached.CASResponse;
import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MemcacheMapTransaction<K, V, R> implements ITransaction<K, V, IListableResource<K, V>, R> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected String key;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected MemcachedClient mc;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	
	public MemcacheMapTransaction() {
		log = LoggerFactory.getLogger(MemcacheMapTransaction.class);
	}

	public MemcacheMapTransaction(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheMapTransaction.class) : log;
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
	
	public void setFluctuate(Boolean fluctuate) {
		if(fluctuate != null) this.fluctuate = fluctuate;
	}

	public void setRetry(Integer retry) {
		if (retry != null && retry >= 0) this.retry = retry;
	}

	public void setSleep(Integer sleep) {
		if (sleep != null && sleep >= 0) this.sleep = sleep;
	}
	
	public void setKey(String key) {
		this.key = key;
	}
	
	protected static class Value<V>{
		int type;
		V value;
		
		protected Value(int type, V value) {
			this.type = type;
			this.value = value;
		}
	}
	
	protected class Resource implements IListableResource<K, V>, IExtendable<Object> {
		protected volatile Boolean needInit = true;
		protected Long version;
		protected Map<K, V> cache;
		protected Map<K, Value<V>> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value<V>>());
		protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
			@Override
			public Object process(final K[] paras) {
				if(needInit) init();
				return new Long(cache == null ? 0 : cache.size());
			}
		};
		
		protected synchronized void init(){
			if(needInit){
				CASValue<Object> value = mc.gets(key);
				if (touch && timeout > 0) mc.touch(key, timeout);
				if(value != null) { 
					version = value.getCas();
					cache = (Map<K, V>) value.getValue();
				}
				needInit = false;
			}
		}
		
		@Override
		public V find(K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				if(needInit) init();
				if(cache != null) return cache.get(paras[0]);
			}
			return null;
		}

		@Override
		public <P> P store(V value, K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				exec.remove(paras[0]);
				exec.put(paras[0], new Value<V>(0, value));
			}
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null){
				exec.remove(paras[0]);
				exec.put(paras[0], new Value<V>(1, null));
			}
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			exec.clear();
			exec.put(null, null);
			return null;
		}	

		@Override
		public Collection<K[]> keys(K... paras) {
			if(needInit) init();
			Collection<K[]> ls = new ArrayList<K[]>(cache == null ? 0 : cache.size());
			if(cache != null) for(K o : cache.keySet()) {
				K[] k = PlatformUtil.newArrayOf(1, o);
				k[0] = o;
				ls.add(k);
			}
			return ls;
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(needInit) init();
			Map<K[], V> ret = new HashMap<K[], V>(cache == null ? 0 : cache.size());
			if(cache != null) for(Entry<K, V> o : cache.entrySet()) {
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
		
		protected boolean perform(){
			if(exec.size() > 0) {
				CASValue<Object> value = mc.gets(key);
				if(version != null && value != null && value.getCas() != version) return false;
				final Boolean[] clear = new Boolean[]{false};
				final ConcurrentHashMap<K, Object> put = new ConcurrentHashMap<K, Object>();
				final HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value<V>> entry : exec.entrySet()) if(entry.getValue() == null) clear[0] = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
				if(value == null) try{
					return mc.add(key, timeout, put).get();
				}catch(Exception e){ return false;}
				else{
					Map<K, Object> current = (Map<K, Object>)value.getValue();
			    	if(clear[0]) current = new ConcurrentHashMap<K, Object>();
					if(put.size() > 0) current.putAll(put);
					if(remove.size() > 0) for(K key : remove) current.remove(key);
					CASResponse resp = mc.cas(key, value.getCas(), timeout, current);
					if(CASResponse.OK.equals(resp) || CASResponse.OBSERVE_MODIFIED.equals(resp)) return true;
					else if(CASResponse.OBSERVE_ERROR_IN_ARGS.equals(resp) || CASResponse.OBSERVE_TIMEOUT.equals(resp)) 
						throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
					else return false;
				}
			}
			return true;
		}
	}

	@Override
	public R execute(final IProcessor<IListableResource<K, V>, R> processor) {
		R ret = null;
		boolean suc = false;
		int count = 0;
		int max = 52;
		do try {
			count ++;
			Resource resource = new Resource();
			ret = processor.process(resource);
			suc = resource.perform();		
			if(!suc && sleep > 0){
				if(fluctuate) {
					long s = (long)sleep << ((count < max ? count: max) + 10);
					if(s < 0){
						max = count - 1;
						s = (long)sleep << ((count < max ? count: max) + 10);
					}
					Thread.sleep(s);
				} else  Thread.sleep((long)sleep << 10);
			} 
		} catch (Exception e) {
			log.error("MemcacheMapTransaction: sleep interrupted:", e);
			throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
		} while(!suc && (retry == 0 || count < retry));
		if(suc) return ret;
		else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
