package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.ValueOperations;


public class RedisLResource<K, V> implements IListableResource<K, V> {
	protected RedisTemplate<K, V> template;
	protected ValueOperations<K, V> vo;
	protected Long timeout = 0L;
	protected Boolean touch = false; 
	protected TimeUnit unit = TimeUnit.SECONDS;
	
	public void setUnit(TimeUnit unit) {
		if (unit != null) this.unit = unit;
	}

	public void setTouch(Boolean touch) {
		if (touch != null) this.touch = touch;
	}
	
	public void setTimeout(Long timeout) {
		if (timeout != null) this.timeout = timeout;
	}
	
	public synchronized void setTemplate(RedisTemplate<K, V> template) {
		if(template != null) {
			this.template = template;
			this.vo = template.opsForValue();
		}
	}

	@Override
	public <P> P store(final V resource, final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) if (timeout == 0) vo.set(paras[0], resource);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
		    	vo.set(paras[0], resource);
		        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
		        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
			}
		}, template.getValueSerializer());
		return null;
	}
	
	@Override
	public <P> P discard(final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) template.delete(paras[0]);
		return null;
	}
	
	@Override
	public <P> P empty(K... paras) {
		template.getConnectionFactory().getConnection().flushDb();
		return null;
	}
	
	@Override
	public V find(final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) if (timeout == 0 || !touch) return vo.get(paras[0]);
		else { 
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
			    	vo.get(paras[0]);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getValueSerializer()); 
			return (V)ls.get(0);
		} else return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		K key = (K)"*";
		if(paras != null && paras.length > 0 && paras[0] != null)key = paras[0];
		Collection<K> c = template.keys(key);
		Collection<K[]> ls = new ArrayList<K[]>(c.size());
		for(K o : c) {
			K[] k = PlatformUtil.newArrayOf(1, key);
			k[0] = o;
			ls.add(k);
		}
		return ls;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		K key = (K)"*";
		if(paras != null && paras.length > 0 && paras[0] != null)key = paras[0];
		Collection<K> c = template.keys(key);
		List<V> v = vo.multiGet(c);
		int count = 0;
		Map<K[], V> ret = new HashMap<K[], V>(c.size());
		for(K o : c) {
			K[] k = PlatformUtil.newArrayOf(1, key);
			k[0] = o;
			ret.put(k, v.get(count));
			count ++;
		}
		return ret;
	}
}
