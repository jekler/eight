package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

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
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;


public class RedisMapFTransaction<K, V, R> implements ITransaction<K, V, IListableResource<K, V>, R>, IExtendable<Object> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected RedisTemplate<K, V> template;
	protected HashOperations<K, K, V> ho;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected Long timeout = 0L;
	protected Boolean touch = false; 
	protected TimeUnit unit = TimeUnit.SECONDS;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) return timeout == 0 || !touch ? ho.size(paras[0]) : (Long) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
					ho.size(paras[0]);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashValueSerializer()).get(0); 
			else return null;
		}
	};
	
	public RedisMapFTransaction() {
		log = LoggerFactory.getLogger(RedisMapFTransaction.class);
	}

	public RedisMapFTransaction(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(RedisMapFTransaction.class) : log;
	}
	
	public void setUnit(TimeUnit unit) {
		if (unit != null) this.unit = unit;
	}

	public void setTouch(Boolean touch) {
		if (touch != null) this.touch = touch;
	}
	
	public void setTimeout(Long timeout) {
		if (timeout != null) this.timeout = timeout;
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
	
	public void setTemplate(RedisTemplate<K, V> template) {
		if(template != null) {
			this.template = template;
			this.ho = template.opsForHash();
		}
	}

	protected static class Value<V>{
		int type;
		V value;
		
		protected Value(int type, V value) {
			this.type = type;
			this.value = value;
		}
	}
	
	protected class Resource implements IListableResource<K, V>{
		protected ConcurrentHashMap<List<K>, V> cache = new ConcurrentHashMap<List<K>, V>();
		protected Map<List<K>, Value<V>> exec = Collections.synchronizedMap(new LinkedHashMap<List<K>, Value<V>>());
		protected Map<K, Set<List<K>>> map = new HashMap<K, Set<List<K>>>();		

		@Override
		public V find(K... paras) {
			if(paras != null && paras.length > 1){
				final ArrayList<K> keys = new ArrayList<K>(2);
				for(int i = 0; i < 2; i++) keys.add(paras[i]);
				if(cache.containsKey(keys)) return cache.get(keys);
				else{
					List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
						@Override
						public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
					        template.watch(keys.get(0));
					    	ho.get(keys.get(0), keys.get(1));
					        if (timeout > 0) template.expire(keys.get(0), timeout, unit); 	
					        else if (timeout < 0) template.expireAt(keys.get(0), new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
					        return null;
						}
					}, template.getHashValueSerializer());
					V value = (V)ls.get(0);
					cache.put(keys, value);
					return value;
				}
			}
			return null;
		}

		@Override
		public <P> P store(V value, K... paras) {
			if(paras != null && paras.length > 1) {
				final ArrayList<K> keys = new ArrayList<K>(2);
				for(int i = 0; i < 2; i++) keys.add(paras[i]);
				exec.remove(keys);
				exec.put(keys, new Value<V>(0, value));
				synchronized(map){
					Set<List<K>> set = map.get(keys.get(0));
					if(set == null) {
						set = new HashSet<List<K>>();
						map.put(keys.get(0), set);
					}
					set.add(keys);
				}
			}
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null && paras.length > 1) {
				final ArrayList<K> keys = new ArrayList<K>(2);
				for(int i = 0; i < 2; i++) keys.add(paras[i]);
				exec.remove(keys);
				exec.put(keys, new Value<V>(1, null));
				synchronized(map){
					Set<List<K>> set = map.get(keys.get(0));
					if(set == null) {
						set = new HashSet<List<K>>();
						map.put(keys.get(0), set);
					}
					set.add(keys);
				}
			}
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				final ArrayList<K> keys = new ArrayList<K>(2);
				keys.add(paras[0]);
				exec.remove(keys);
				exec.put(keys, new Value<V>(2, null));
				if(map.containsKey(keys.get(0)))synchronized(map){
					for(List<K> key : map.remove(keys.get(0))) exec.remove(key);
				}
			}
			return null;
		}	

		@Override
		public Collection<K[]> keys(final K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null){
				Collection<K> c = timeout == 0 || !touch ? ho.keys(paras[0]) : (Collection<K>) template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
						ho.keys(paras[0]);
				        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
				        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
					}
				}, template.getHashKeySerializer()).get(0); 
				Collection<K[]> ls = new ArrayList<K[]>(c.size());
				for(K o : c) {
					K[] k = PlatformUtil.newArrayOf(paras, 2);
					k[0] = paras[0];
					k[1] = o;
					ls.add(k);
				}
				return ls;
			}
			return null;
		}
		

		@Override
		public Map<K[], V> all(final K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null){
				Map<K, V> c = (Map<K, V>)template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
				        template.watch(paras[0]);
						ho.entries(paras[0]); 
				        if (touch) if (timeout > 0) template.expire(paras[0], timeout, unit); 	
				        else if (timeout < 0) template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				        return null;
					}
				}, template.getHashValueSerializer()).get(0);
				Map<K[], V> ret = new HashMap<K[], V>(c.size());
				for(Entry<K, V> o : c.entrySet()) {
					K[] k = PlatformUtil.newArrayOf(paras, 2);
					k[0] = paras[0];
					k[1] = o.getKey();
					List<K> key = Arrays.asList(k);
					V value = o.getValue();
					if(cache.contains(key)) value = cache.get(key);
					else cache.put(key, value);
					ret.put(k, value);
				}
				return ret;
			}
			return null;
		}
		
		protected boolean perform(){
			if(exec.size() > 0) {
				final HashSet<K> clear = new HashSet<K>();
				final Map<K, Map<K, V>> put = new HashMap<K, Map<K, V>>();
				final Map<K, Set<K>> remove = new HashMap<K, Set<K>>();
				for(Entry<List<K>, Value<V>> entry : exec.entrySet()) if(entry.getValue().type == 2) clear.add(entry.getKey().get(0));
				else if(entry.getValue().type == 0) {
					Map<K, V> submap = put.get(entry.getKey().get(0));
					if(submap == null){
						submap = new HashMap<K, V>();
						put.put(entry.getKey().get(0), submap);
					}
					submap.put(entry.getKey().get(1), entry.getValue().value);
				}
				else if(entry.getValue().type == 1) {
					Set<K> subset = remove.get(entry.getKey().get(0));
					if(subset == null){
						subset = new HashSet<K>();
						remove.put(entry.getKey().get(0), subset);
					}
					subset.add(entry.getKey().get(1));					
				}
				List<Object> ret = template.executePipelined(new SessionCallback<Object>(){
					@Override
					public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
						template.multi();
						if(clear.size() > 0) template.delete(clear);
						if(put.size() > 0) for(Entry<K, Map<K, V>> entry : put.entrySet()) {
							ho.putAll(entry.getKey(), entry.getValue());
					        if (timeout > 0) template.expire(entry.getKey(), timeout, unit); 	
					        else if (timeout < 0) template.expireAt(entry.getKey(), new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
						}
						if(remove.size() > 0) for(Entry<K, Set<K>> entry : remove.entrySet()) {
							ho.delete(entry.getKey(), entry.getValue().toArray()); 
					        if (timeout > 0) template.expire(entry.getKey(), timeout, unit); 	
					        else if (timeout < 0) template.expireAt(entry.getKey(), new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
						}
						return template.exec();
					}
				}, template.getHashValueSerializer()); 
				if(ret == null || (ret.size() == 1 && ret.get(0) == null)) return false;
			}
			return true;
		}
	}

	@Override
	public R execute(final IProcessor<IListableResource<K, V>, R> processor) {
		if(template != null && processor != null){
			R ret = null;
			final Boolean[] suc = {false};
			int count = 0;
			int max = 52;
			do try {
				count ++;
				ret = template.execute(new SessionCallback<R>(){
					@Override
					public <T, P> R execute(RedisOperations<T, P> operations)	throws DataAccessException {
						Resource resource = new Resource();
						R ret = processor.process(resource);
						suc[0] = resource.perform();
						return ret;
					}
				});
				if(!suc[0] && sleep > 0){
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
				log.error("RedisMapFTransaction: sleep interrupted:", e);
				throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
			} while(!suc[0] && (retry == 0 || count < retry));
			if(suc[0]) return ret;
		}
		throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
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
