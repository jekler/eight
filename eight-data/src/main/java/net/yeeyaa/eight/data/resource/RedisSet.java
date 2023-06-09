package net.yeeyaa.eight.data.resource;

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.SetOperations;

public class RedisSet<K, V> implements Set<V> {
	protected K key;
	protected RedisTemplate<K, V> template;
	protected SetOperations<K, V> so;
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
			this.so = template.opsForSet();
		}
	}
	
	public void setKey(K key) {
		this.key = key;
	}
	
	protected Set<V> getSet() {
		if (timeout == 0 || !touch) return so.members(key);
		else return (Set<V>) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.members(key);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0);
	}
	
	@Override
	public int size() {
		if (timeout == 0 || !touch) return new Long(so.size(key)).intValue();
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.size(key);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)).intValue();
	}

	@Override
	public boolean isEmpty() {
		return size() == 0;
	}

	@Override
	public boolean contains(final Object o) {
		if (timeout == 0 || !touch) return so.isMember(key, o);
		else return (Boolean) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.isMember(key, o);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0);
	}

	@Override
	public Iterator<V> iterator() {
		return getSet().iterator();
	}

	@Override
	public Object[] toArray() {
		return getSet().toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return getSet().toArray(a);
	}

	@Override
	public boolean add(final V e) {
		if (timeout == 0) return so.add(key, e) > 0;
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.add(key, e);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)) > 0;
	}

	@Override
	public boolean remove(final Object o) {
		if (timeout == 0) return so.remove(key, o) > 0;
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.remove(key, o);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)) > 0;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		if (c == null || c.size() == 0) return true;
		else return getSet().containsAll(c);
	}

	@Override
	public boolean addAll(final Collection<? extends V> c) {
		if (c == null || c.size() == 0) return false;
		if (timeout == 0) return so.add(key, (V[])c.toArray()) > 0;
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.add(key, (V[])c.toArray());
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)) > 0;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		if (so.size(key) == 0) return false;
		else if (c == null || c.size() == 0) {
			template.delete(key);
			return true;
		} else {
			final Set<V> set = so.members(key);
			set.removeAll(c);
			if (timeout == 0) return so.remove(key, set.toArray()) > 0;
			else return ((Long) template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					so.remove(key, set.toArray());
			        if (timeout > 0) return template.expire(key, timeout, unit); 	
			        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getValueSerializer()).get(0)) > 0;
		}
	}

	@Override
	public boolean removeAll(final Collection<?> c) {
		if (c == null || c.size() == 0 || size() == 0) return false;
		else if (timeout == 0) return so.remove(key, c.toArray()) > 0;
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				so.remove(key, c.toArray());
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)) > 0;
	}

	@Override
	public void clear() {
		template.delete(key);
	}
}
