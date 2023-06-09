package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.TimeUnit;

import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;

public class RedisList<K, V> implements List<V> {
	protected K key;
	protected RedisTemplate<K, V> template;
	protected ListOperations<K, V> lo;
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
			this.lo = template.opsForList();
		}
	}
	
	public void setKey(K key) {
		this.key = key;
	}
	
	@Override
	public int size() {
		if (timeout == 0 || !touch) return lo.size(key).intValue();
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.size(key);
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
	public boolean contains(Object o) {
		return subList(0, -1).contains(o); 
	}

	@Override
	public Iterator<V> iterator() {
		return subList(0, -1).iterator();
	}

	@Override
	public Object[] toArray() {
		return subList(0, -1).toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return subList(0, -1).toArray(a);
	}

	@Override
	public boolean add(final V e) {
		if (timeout == 0) lo.rightPush(key, e);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.rightPush(key, e);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer());
		return true;
	}

	@Override
	public boolean remove(final Object o) {
		if (timeout == 0) return lo.remove(key, 1, o) > 0;
		else return ((Long) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.remove(key, 1, o);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0)) > 0;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return subList(0, -1).containsAll(c);
	}

	@Override
	public boolean addAll(final Collection<? extends V> c) {
		if (c == null || c.size() == 0) return false;
		if (timeout == 0) lo.rightPushAll(key, (V[])c.toArray());
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.rightPushAll(key, (V[])c.toArray());
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer());
		return true;
	}

	@Override
	public boolean addAll(final int index, final Collection<? extends V> c) {
		if (index < 0) throw new IndexOutOfBoundsException();
		if (c == null || c.size() == 0) return false;
		final List<? extends V> l = new ArrayList<V>(c);
		Collections.reverse(l);
		if (index == 0) if (timeout == 0) lo.leftPushAll(key, (V[])l.toArray());
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.leftPushAll(key, (V[])l.toArray());
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()); else {
			final List<V> list = lo.range(key, 0, index);
			if (list.size() < index) throw new IndexOutOfBoundsException();
			else if (list.size() == index) if (timeout == 0) lo.rightPushAll(key, (V[])c.toArray());
			else template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					lo.rightPushAll(key, (V[])c.toArray());
			        if (timeout > 0) return template.expire(key, timeout, unit); 	
			        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getValueSerializer()); else {
				list.remove(index);
				list.addAll(c);
				Collections.reverse(list);
				template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
						lo.trim(key, 0, index - 1);
						lo.leftPushAll(key, (V[])list.toArray());
				        if (timeout > 0) template.expire(key, timeout, unit); 	
				        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				        return null;
					}
				}, template.getValueSerializer());

			}
		}
		return true;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		final List<V> list = lo.range(key, 0, -1);
		if (list.removeAll(c)) return template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				template.delete(key);
				lo.rightPushAll(key, (V[])list.toArray());
		        if (timeout > 0) template.expire(key, timeout, unit); 	
		        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
		        return null;
			}
		}, template.getValueSerializer()) != null;
		else return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		final List<V> list = lo.range(key, 0, -1);
		if (list.retainAll(c)) return template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				template.delete(key);
				lo.rightPushAll(key, (V[])list.toArray());
		        if (timeout > 0) template.expire(key, timeout, unit); 	
		        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
		        return null;
			}
		}, template.getValueSerializer()) != null;
		else return false;
	}

	@Override
	public void clear() {
		template.delete(key);
	}

	@Override
	public V get(final int index) {
		if (timeout == 0 || !touch) return lo.index(key, index);
		else return (V) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.index(key, index);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0);
	}

	@Override
	public V set(final int index, final V element) {
		return (V) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.index(key, index);
				lo.set(key, index, element);
		        if (timeout > 0) template.expire(key, timeout, unit); 	
		        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
		        return null;
			}
		}, template.getValueSerializer()).get(0);
	}

	@Override
	public void add(final int index, final V element) {
		if (index < 0) throw new IndexOutOfBoundsException();
		if (index == 0) if (timeout == 0) lo.leftPush(key, element);
		else template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.leftPush(key, element);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()); else {
			final List<V> list = lo.range(key, 0, index);
			if (list.size() < index) throw new IndexOutOfBoundsException();
			else if (list.size() == index) if (timeout == 0) lo.rightPush(key, element);
			else template.executePipelined(new SessionCallback<Object>() {
				@Override
				public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
					lo.rightPush(key, element);
			        if (timeout > 0) return template.expire(key, timeout, unit); 	
			        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				}
			}, template.getValueSerializer()); else {
				list.set(index, element);
				Collections.reverse(list);
				template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
						lo.trim(key, 0, index - 1);
						lo.leftPushAll(key, (V[])list.toArray());
				        if (timeout > 0) template.expire(key, timeout, unit); 	
				        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				        return null;
					}
				}, template.getValueSerializer());
			}
		}
	}

	@Override
	public V remove(final int index) {
		if (index < 0) throw new IndexOutOfBoundsException();
		if (index == 0) if (timeout == 0) return lo.leftPop(key);
		else return (V) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.leftPop(key);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0); else {
			final List<V> list = lo.range(key, 0, index);
			if (list.size() <= index) throw new IndexOutOfBoundsException();
			else {
				V ret = list.remove(index);
				Collections.reverse(list);
				template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
						lo.trim(key, 0, index);
						lo.leftPushAll(key, (V[])list.toArray());
				        if (timeout > 0) template.expire(key, timeout, unit); 	
				        else if (timeout < 0) template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
				        return null;
					}
				}, template.getValueSerializer());
				return ret;
			}
		}
	}

	@Override
	public int indexOf(Object o) {
		return subList(0, -1).indexOf(o);
	}

	@Override
	public int lastIndexOf(Object o) {
		return subList(0, -1).lastIndexOf(o);
	}

	@Override
	public ListIterator<V> listIterator() {
		return subList(0, -1).listIterator();
	}

	@Override
	public ListIterator<V> listIterator(int index) {
		return subList(index, -1).listIterator();
	}

	@Override
	public List<V> subList(final int fromIndex, final int toIndex) {
		if (timeout == 0 || !touch) return lo.range(key, fromIndex, toIndex);
		else return (List<V>) template.executePipelined(new SessionCallback<Object>() {
			@Override
			public <T, R> Object execute(RedisOperations<T, R> operations) throws DataAccessException {
				lo.range(key, fromIndex, toIndex);
		        if (timeout > 0) return template.expire(key, timeout, unit); 	
		        else return template.expireAt(key, new Date(TimeUnit.MILLISECONDS.convert(-timeout, unit))); 
			}
		}, template.getValueSerializer()).get(0);
	}
}
