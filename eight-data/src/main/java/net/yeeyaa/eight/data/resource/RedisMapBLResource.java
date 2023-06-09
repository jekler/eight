package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;


public class RedisMapBLResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected K key;
	protected RedisTemplate<K, Object> template;
	protected HashOperations<K, K, Object> ho;
	protected Long timeout = 0L;
	protected Boolean touch = false; 
	protected TimeUnit unit = TimeUnit.SECONDS;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			return timeout == 0 || !touch ? ho.size(key) : (Long) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.size(key);
			        if (timeout > 0) return template.expire(key, timeout, unit); 	
			        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashValueSerializer()).get(0); 
		}
	};
	
	public void setUnit(TimeUnit unit) {
		if (unit != null) this.unit = unit;
	}

	public void setTouch(Boolean touch) {
		if (touch != null) this.touch = touch;
	}
	
	public void setTimeout(Long timeout) {
		if (timeout != null) this.timeout = timeout;
	}
	
	public synchronized void setTemplate(RedisTemplate<K, Object> template) {
		if(template != null) {
			this.template = template;
			this.ho = template.opsForHash();
		}
	}
	
	public void setKey(K key) {
		this.key = key;
	}

	@Override
	public Object find(final K ... paras) {
		if(paras != null && paras.length > 0) if (timeout == 0 || !touch) return ho.multiGet(key, Arrays.asList(paras));
		else return template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ho.multiGet(key, Arrays.asList(paras));
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getHashValueSerializer()).get(0); 
		else return null;
	}

	@Override
	public <P> P store(final Object value, final K ... paras) {
		if(paras != null && paras.length > 0){
			if(paras[0] != null) if (timeout == 0) ho.put(key, paras[0], value);
			else template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.put(key, paras[0], value);
			        if (timeout > 0) return template.expire(key, timeout, unit); 	
			        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
				}
			}, template.getHashValueSerializer());
		}else if(value instanceof Map) if (timeout == 0) ho.putAll(key, (Map<K, Object>) value);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ho.putAll(key, (Map<K, Object>) value);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
			}
		}, template.getHashValueSerializer());
		return null;
	}

	@Override
	public <P> P discard(final K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) template.executePipelined(new SessionCallback<Object>() {
		    @Override
		    public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
		    	List<K> keys = Arrays.asList(paras);
		    	template.multi();
		        ho.delete(key, keys.toArray());
		        if (timeout > 0) template.expire(key, timeout, unit); 	
		        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
		        return template.exec();
		    }
		}); 
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		template.delete(key);
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K> c = timeout == 0 || !touch ? ho.keys(key) : (Collection<K>) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ho.keys(key);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getHashKeySerializer()).get(0); 
		Collection<K[]> ls = new ArrayList<K[]>(c.size());
		for(K o : c) {
			K[] k = PlatformUtil.newArrayOf(1, key);
			k[0] = o;
			ls.add(k);
		}
		return ls;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		Map<K, Object> c = timeout == 0 || !touch ? ho.entries(paras[0]) : (Map<K, Object>) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ho.entries(key);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getHashValueSerializer()).get(0); 
		Map<K[], Object> ret = new HashMap<K[], Object>(c.size());
		for(Entry<K, Object> o : c.entrySet()) {
			K[] k = PlatformUtil.newArrayOf(1, key);
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
