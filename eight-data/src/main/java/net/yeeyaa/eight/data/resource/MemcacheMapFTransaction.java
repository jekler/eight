package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import net.spy.memcached.CASMutation;
import net.spy.memcached.CASMutator;
import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.spy.memcached.internal.OperationFuture;
import net.spy.memcached.transcoders.Transcoder;
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


public class MemcacheMapFTransaction<K, V, R> implements ITransaction<K, V, IListableResource<K, V>, R> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected MemcachedClient mc;
	protected CASMutator<Map<K, V>> mutator;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	
	public MemcacheMapFTransaction() {
		log = LoggerFactory.getLogger(MemcacheMapFTransaction.class);
	}

	public MemcacheMapFTransaction(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheMapFTransaction.class) : log;
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

	@PostConstruct
	public void init() {
		Transcoder tc = mc.getTranscoder();
		this.mutator = new CASMutator<Map<K, V>>(mc, tc, retry);
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
		protected ConcurrentHashMap<String, CASValue<Object>> cache = new ConcurrentHashMap<String, CASValue<Object>>();
		protected Map<List<K>, Value<V>> exec = Collections.synchronizedMap(new LinkedHashMap<List<K>, Value<V>>());
		protected Map<K, Set<List<K>>> map = new HashMap<K, Set<List<K>>>();		
		protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
			@Override
			public Object process(final K[] paras) {
				if(paras != null && paras.length > 0 && paras[0] != null) {
					Map<K, V> map;
					if (touch && timeout > 0) {
						CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
						map = value == null ? null : (Map<K, V>) value.getValue();
					} else map = (Map<K, V>)mc.get(paras[0].toString());
					return new Long(map.size());			
				}
				return null;
			}
		};
		
		@Override
		public V find(K... paras) {
			if(paras != null && paras.length > 1){
				String key = paras[0].toString();
				if(cache.containsKey(key)){
					CASValue<Object> value = cache.get(key);
					if(value != null && value.getValue() instanceof Map) return ((Map<K,V>)value.getValue()).get(paras[1]);
				} else{
					CASValue<Object> value = mc.gets(key);
					if (touch && timeout > 0) mc.touch(key, timeout);
					cache.put(key, value);
					if(value != null && value.getValue() instanceof Map) return ((Map<K,V>)value.getValue()).get(paras[1]);
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
		public Collection<K[]> keys(K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null){
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
			if(paras != null && paras.length > 0 && paras[0] != null){
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
		
		protected boolean perform(){
			if(exec.size() > 0) {
				List<OperationFuture<CASValue<Object>>> tmp = new ArrayList<OperationFuture<CASValue<Object>>>(cache.size());
				for(String key : cache.keySet()) tmp.add(mc.asyncGets(key));
				for(OperationFuture<CASValue<Object>> of : tmp) try{
					CASValue<Object> old = cache.get(of.getKey());
					CASValue<Object> nw = of.get();
					if(!(old == null ? nw == null : old.getCas() == (nw == null ? -1 : nw.getCas()))) return false;
				}catch(Exception e){
					log.error("compare value fail:", e);
					throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
				}
				Map<K, Map<String, Object>> ops = new HashMap<K, Map<String, Object>>();
				for(Entry<List<K>, Value<V>> entry : exec.entrySet()) if(entry.getValue().type == 2) {
					Map<String, Object> submap = ops.get(entry.getKey().get(0));
					if(submap == null) {
						submap = new HashMap<String, Object>();
						ops.put(entry.getKey().get(0), submap);
					}
					submap.put("clear", entry.getValue());
				} else if(entry.getValue().type == 0) {
					Map<String, Object> submap = ops.get(entry.getKey().get(0));
					if(submap == null){
						submap = new HashMap<String, Object>();
						ops.put(entry.getKey().get(0), submap);
					}
					Map<K, V> subsubmap = (Map<K, V>)submap.get("put");
					if(subsubmap == null){
						subsubmap = new HashMap<K, V>();
						submap.put("put", subsubmap);
					}
					subsubmap.put(entry.getKey().get(1), entry.getValue().value);
				} else if(entry.getValue().type == 1) {
					Map<String, Object> submap = ops.get(entry.getKey().get(0));
					if(submap == null){
						submap = new HashMap<String, Object>();
						ops.put(entry.getKey().get(0), submap);
					}
					Set<K> subset = (Set<K>)submap.get("remove");
					if(subset == null){
						subset = new HashSet<K>();
						submap.put("remove", subset);
					}
					subset.add(entry.getKey().get(1));					
				}
				for(final Entry<K, Map<String, Object>> entry : ops.entrySet()) try{
					CASMutation<Map<K, V>> mutation=new CASMutation<Map<K, V>>() {
					     public Map<K, V> getNewValue(Map<K, V> current) {
					    	 if(entry.getValue().get("clear") != null) current = new ConcurrentHashMap<K, V>();
					    	 Map<K, V> put = (Map<K, V>)entry.getValue().get("put");
					    	 if(put != null) current.putAll(put);
					    	 Set<K> remove = (Set<K>)entry.getValue().get("remove");
					    	 if(remove != null) for(K key : remove) current.remove(key);
					         return current;
					     }
					 };
					 Map<K, V> init = new ConcurrentHashMap<K, V>();
			    	 Map<K, V> put = (Map<K, V>)entry.getValue().get("put");
			    	 if(put != null) init.putAll(put);
					 mutator.cas(entry.getKey().toString(), init, timeout, mutation);
				}catch(Exception e){
					log.error("set value fail:", e);
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
			log.error("sleep interrupted:", e);
			throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
		} while(!suc && (retry == 0 || count < retry));
		if(suc) return ret;
		else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
