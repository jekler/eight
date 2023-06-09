package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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


public class RedisMapTransaction<K, V, R> implements ITransaction<K, V, IListableResource<K, V>, R> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected K key;
	protected RedisTemplate<K, V> template;
	protected HashOperations<K, K, V> ho;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected Long timeout = 0L;
	protected Boolean touch = false; 
	protected TimeUnit unit = TimeUnit.SECONDS;
	
	public RedisMapTransaction() {
		log = LoggerFactory.getLogger(RedisMapTransaction.class);
	}

	public RedisMapTransaction(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(RedisMapTransaction.class) : log;
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
	
	public void setKey(K key) {
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
		protected ConcurrentHashMap<K, V> cache = new ConcurrentHashMap<K, V>();
		protected Map<K, Value<V>> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value<V>>());
		protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
			@Override
			public Object process(final K[] paras) {
				if(needInit) init();
				return new Long(cache.size());
			}
		};
		
		protected synchronized void init(){
			if(needInit){
				List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
				        template.watch(key);
				        ho.entries(key);  
				        if (timeout > 0) template.expire(key, timeout, unit); 	
				        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				        return null;
					}
				}, template.getHashValueSerializer());
				cache.putAll((Map<K, V>)ls.get(0));
				needInit = false;
			}
		}
		
		@Override
		public V find(final K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) {
				if(needInit) init();
				return cache.get(paras[0]);
			} else return null;
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
			Collection<K[]> ls = new ArrayList<K[]>(cache.size());
			for(K o : cache.keySet()){
				K[] k = PlatformUtil.newArrayOf(1, key);
				k[0] = o;
				ls.add(k);
			}
			return ls;
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(needInit) init();
			Map<K[], V> ret = new HashMap<K[], V>(cache.size());
			for(Entry<K, V> o : cache.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(1, key);
				k[0] = o.getKey();
				V value = o.getValue();
				ret.put(k, value);
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
				final Boolean[] clear = {false};
				final HashMap<K, V> put = new HashMap<K, V>();
				final HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value<V>> entry : exec.entrySet()) if(entry.getValue() == null) clear[0] = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
				List<Object> ret = template.executePipelined(new SessionCallback<Object>(){
					@Override
					public <T, P> Object execute(RedisOperations<T, P> operations) throws DataAccessException {
						template.multi();
						if(clear[0]) template.delete(key);
						if(put.size() > 0) ho.putAll(key, put);
						if(remove.size() > 0) ho.delete(key, remove.toArray());
				        if (timeout > 0) template.expire(key, timeout, unit); 	
				        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
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
		if(key != null && template != null && processor != null){
			R ret = null;
			final Boolean[] suc = {false};
			int count = 0;
			int max = 52;
			do try {
				count ++;
				ret = template.execute(new SessionCallback<R>(){
					@Override
					public <K, V> R execute(RedisOperations<K, V> operations)	throws DataAccessException {
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
				log.error("RedisMapTransaction: sleep interrupted:", e);
				throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
			} while(!suc[0] && (retry == 0 || count < retry));
			if(suc[0]) return ret;
		}
		throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
