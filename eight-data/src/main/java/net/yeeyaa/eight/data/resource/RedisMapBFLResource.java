package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
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


public class RedisMapBFLResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected RedisTemplate<K, Object> template;
	protected HashOperations<K, K, Object> ho;
	protected Long timeout = 0L;
	protected Boolean touch = false; 
	protected TimeUnit unit = TimeUnit.SECONDS;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(final K[] paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) return timeout == 0 || !touch ? ho.size(paras[0]) : (Long) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.size(paras[0]);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashValueSerializer()).get(0); 
			else return null;
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

	@Override
	public Object find(final K ... paras) {
		if(paras != null && paras.length > 1){
			final ArrayList<K> keys = new ArrayList<K>(paras.length - 1);
			for(int i = 1; i < paras.length; i++) keys.add(paras[i]);
			if (timeout == 0 || !touch) return ho.multiGet(paras[0], keys);
			else return template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.multiGet(paras[0], keys);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashValueSerializer()).get(0); 
		} else return null;
	}

	@Override
	public <P> P store(final Object value, final K ... paras) {
		if(paras != null && paras.length > 0) {
			if(paras.length > 1) if (timeout == 0) ho.put(paras[0], paras[1], value);
			else template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.put(paras[0], paras[1], value);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
				}
			}, template.getHashValueSerializer());
		} else if(value instanceof Map) if (timeout == 0) ho.putAll(paras[0], (Map<K, Object>) value);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ho.putAll(paras[0], (Map<K, Object>) value);
		        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
		        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
			}
		}, template.getHashValueSerializer());
		return null;
	}

	@Override
	public <P> P discard(final K ... paras) {
		if(paras != null && paras.length > 1) template.executePipelined(new SessionCallback<Object>() {
		    @Override
		    public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				ArrayList<K> keys = new ArrayList<K>(paras.length - 1);
				for(int i = 1; i < paras.length; i++) keys.add(paras[i]);
				template.multi();
		        ho.delete(paras[0], keys.toArray());
		        if (timeout > 0) template.expire(paras[0], timeout, unit); 	
		        else if (timeout < 0) template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
		        return template.exec();
		    }
		}, template.getHashValueSerializer()); 
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) template.delete(paras[0]);
		return null;
	}

	@Override
	public Collection<K[]> keys(final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			Collection<K> c = timeout == 0 || !touch ? ho.keys(paras[0]) : (Collection<K>) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.keys(paras[0]);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashKeySerializer()).get(0); 
			Collection<K[]> ls = new ArrayList<K[]>(c.size());
			for(K o : c){
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
	public Map<K[], Object> all(final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			Map<K, Object> c = timeout == 0 || !touch ? ho.entries(paras[0]) : (Map<K, Object>) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					ho.entries(paras[0]);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getHashValueSerializer()).get(0); 
			Map<K[], Object> ret = new HashMap<K[], Object>(c.size());
			for(Entry<K, Object> o : c.entrySet()){
				K[] k = PlatformUtil.newArrayOf(paras, 2);
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
