package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Arrays;
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


public class RedisBLResource<K> implements IListableResource<K ,Object> {
	protected RedisTemplate<K, Object> template;
	protected ValueOperations<K, Object> vo;
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
	
	public synchronized void setTemplate(RedisTemplate<K, Object> template) {
		if(template != null) {
			this.template = template;
			this.vo = template.opsForValue();
		}
	}

	@Override
	public <P> P store(final Object resource, final K... paras) {
		if(paras != null && paras.length > 0) {
			if(paras[0] != null) if (timeout == 0) vo.set(paras[0], resource);
			else template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
			    	vo.set(paras[0], resource);
			        if (timeout > 0) return template.expire(paras[0], timeout, unit); 	
			        else return template.expireAt(paras[0], new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
				}
			}, template.getValueSerializer());
		}else if(resource instanceof Map) if (timeout == 0) vo.multiSet((Map<K, Object>) resource);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				Map<K, Object> map = (Map<K, Object>) resource;
				if (map.size() > 0) {
			    	vo.multiSet(map);
			    	for (K key : map.keySet()) if (timeout > 0) template.expire(key, timeout, unit); 	
			        else template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
				}
		    	return null;
			}
		}, template.getValueSerializer());
		return null;
	}
	
	@Override
	public <P> P discard(final K... paras) {
		if(paras != null && paras.length > 0) template.delete(Arrays.asList(paras));
		return null;
	}
	
	@Override
	public <P> P empty(K... paras) {
		template.getConnectionFactory().getConnection().flushDb();
		return null;
	}
	
	@Override
	public Object find(final K... paras) {
		if(paras != null && paras.length > 0) if (timeout == 0 || !touch) return (Object)vo.multiGet(Arrays.asList(paras));
		else {
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, R> List<Object> execute(RedisOperations<T, R> operations) throws DataAccessException {
			    	List<K> keys = Arrays.asList(paras);
			        vo.multiGet(keys);
			    	if (timeout != 0) for (K key : keys) if (timeout > 0) template.expire(key, timeout, unit); 	
			        else template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit)));
			        return null;
			    }
			}, template.getValueSerializer());
			return ls.get(0);
		}
		return null;
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
	public Map<K[], Object> all(K... paras) {
		K key = (K)"*";
		if(paras != null && paras.length > 0 && paras[0] != null)key = paras[0];
		Collection<K> c = template.keys(key);
		List<Object> v = vo.multiGet(c);
		int count = 0;
		Map<K[], Object> ret = new HashMap<K[], Object>(c.size());
		for(K o : c) {
			K[] k = PlatformUtil.newArrayOf(1, key);
			k[0] = o;
			ret.put(k, v.get(count));
			count ++;
		}
		return ret;
	}
}
