package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class GenericTransaction<K, V, T extends IResource<K, V>, R> implements ITransaction<K, V, T, R> {
	protected final Logger log;
	protected T resource;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected Boolean batch = false;
	protected Integer prefixLength = 0;
	protected IProcessor<Object[], Boolean> comparator;
	protected IProcessor<Object[], Boolean> decision;

	public GenericTransaction() {
		this.log = LoggerFactory.getLogger(GenericTransaction.class);
	}
	
	public GenericTransaction(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(GenericTransaction.class) : log;
	}
	
	public void setBatch(Boolean batch) {
		if(batch != null) this.batch = batch;
	}

	public void setPrefixLength(Integer prefixLength) {
		if(prefixLength != null && prefixLength > 0) this.prefixLength = prefixLength;
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
		
	public void setResource(T resource) {
		this.resource = resource;
	}

	public void setComparator(IProcessor<Object[], Boolean> comparator) {
		this.comparator = comparator;
	}

	public void setDecision(IProcessor<Object[], Boolean> decision) {
		this.decision = decision;
	}


	public static class Descision<K, V> implements IProcessor<Object[], Boolean>{
		protected IProcessor<Object[], Boolean> comparator;
		
		public void setComparator(IProcessor<Object[], Boolean> comparator) {
			this.comparator = comparator;
		}

		@Override
		public Boolean process(Object[] instance) {
			if(instance != null && instance.length > 1 && Map.class.isInstance(instance[1]) && IResource.class.isInstance(instance[0])){
				Map<List<K>, Value<K, V>> cache = (Map<List<K>, Value<K, V>>)instance[1];
				IResource<K, V> resource = (IResource<K, V>)instance[0];
				if(comparator != null) for(Entry<List<K>, Value<K, V>> entry : cache.entrySet()){
					V value = resource.find(entry.getValue().paras);
					if(!Boolean.TRUE.equals(comparator.process(new Object[]{entry.getValue().paras, entry.getValue().value, value}))) return false;
				}else for(Entry<List<K>, Value<K, V>> entry : cache.entrySet()){
					V value = resource.find(entry.getValue().paras);
					if(!(value == null ? entry.getValue().value == null : value.equals(entry.getValue().value))) return false;
				}
				return true;
			} else return false;
		}	
	}
	
	public static class BatchDescision<K, V> implements IProcessor<Object[], Boolean>{
		protected IProcessor<Object[], Boolean> comparator;
		
		public void setComparator(IProcessor<Object[], Boolean> comparator) {
			this.comparator = comparator;
		}

		@Override
		public Boolean process(Object[] instance) {
			if(instance != null && instance.length > 2 && Map.class.isInstance(instance[2]) && Map.class.isInstance(instance[1]) && IResource.class.isInstance(instance[0])){
				 HashMap<List<K>, Map<K, Value<K, V>>> keys = (HashMap<List<K>, Map<K, Value<K, V>>>)instance[2]; 
				 IResource<Object, Object> resource = (IResource<Object, Object>)instance[0];
				 if(comparator != null) {
						for(Entry<List<K>, Map<K, Value<K, V>>> entry : keys.entrySet()) if(entry.getValue().size() > 0){
							List<K> k = entry.getKey();
							K[] arr = entry.getValue().keySet().toArray(PlatformUtil.newArrayOf(entry.getValue().values().iterator().next().paras, entry.getValue().size()));
							k.addAll(Arrays.asList(arr));
							Object news = resource.find(k.toArray(PlatformUtil.newArrayOf(arr, k.size())));
							if(!List.class.isInstance(news) || ((List)news).size() != arr.length) return false;
							else for(int i = 0; i < arr.length; i++){
								Value<K, V> oldvalue = entry.getValue().get(arr[i]);
								Object newone = ((List)news).get(i);		
								if(!Boolean.TRUE.equals(comparator.process(new Object[]{oldvalue.paras, oldvalue.value, newone}))) return false;				
							}
						}
					}else for(Entry<List<K>, Map<K, Value<K, V>>> entry : keys.entrySet()) if(entry.getValue().size() > 0){
						List<K> k = entry.getKey();
						K[] arr = entry.getValue().keySet().toArray(PlatformUtil.newArrayOf(entry.getValue().values().iterator().next().paras, entry.getValue().keySet().size()));
						k.addAll(Arrays.asList(arr));
						Object news = resource.find(k.toArray(PlatformUtil.newArrayOf(arr, k.size())));
						if(!List.class.isInstance(news) || ((List)news).size() != entry.getValue().keySet().size()) return false;
						else for(int i = 0; i < arr.length; i++){
							Object oldone = entry.getValue().get(arr[i]).value;
							Object newone = ((List)news).get(i);		
							if(!(oldone == null ? newone == null : oldone.equals(newone))) return false;				
						}
				}
				return true;
			} else return false;
		}	
	}
	
	public static class Comparator implements IProcessor<Object[], Boolean>{
		@Override
		public Boolean process(Object[] instance) {
			if(instance != null && instance.length > 2) return (instance[1] == null ? instance[2] == null : instance[1].equals(instance[2]));
			else return false;
		}	
	}
	
	protected static class Value<K, V>{
		protected int type;
		protected V value;
		protected K[] paras;
		
		protected Value(int type, V value, K[] paras) {
			this.type = type;
			this.value = value;
			this.paras = paras;
		}
	}
	
	protected class Proxy implements IResource<K, V>{
		protected ConcurrentHashMap<List<K>, Value<K, V>> cache = new ConcurrentHashMap<List<K>, Value<K, V>>();
		protected LinkedHashMap<List<K>, Value<K, V>> exec = new LinkedHashMap<List<K>, Value<K, V>>();
		protected IProcessor<T, R> performer;
		
		protected Proxy(IProcessor<T, R> performer) {
			this.performer = performer;
		}

		@Override
		public V find(K... paras) {
			if(paras != null){
				List<K> key = Arrays.asList(paras);
				if(cache.containsKey(key)) return cache.get(key).value;
				else{
					V ret = resource.find(paras);
					cache.put(key, new Value<K, V>(-1, ret, paras));
					return ret;
				}
			}else return null;
		}

		@Override
		public <P> P store(V value, K... paras) {
			if(paras != null){
				List<K> key = Arrays.asList(paras);
				synchronized(exec){
					exec.remove(key);
					exec.put(key, new Value<K, V>(0, value, paras));		
				}
			}
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null){
				List<K> key = Arrays.asList(paras);
				synchronized(exec){
					exec.remove(key);
					exec.put(key, new Value<K, V>(1, null, paras));
				}
			}
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			if(paras != null){
				List<K> key = Arrays.asList(paras);
				synchronized(exec){
					exec.remove(key);
					exec.put(key, new Value<K, V>(2, null, paras));
				}
			}
			return null;
		}
		
		protected boolean perform(){
			if(exec.size() > 0) synchronized(resource){
				if(decision != null && decision != null) {
					if(!Boolean.TRUE.equals(decision.process(new Object[]{performer, resource, cache, exec}))) return false;
				} else if(comparator != null && comparator != null) for(Entry<List<K>, Value<K, V>> entry : cache.entrySet()){
					V value = resource.find(entry.getValue().paras);
				    if(!Boolean.TRUE.equals(comparator.process(new Object[]{performer, entry.getValue().paras, entry.getValue().value, value}))) return false;
				} else for(Entry<List<K>, Value<K, V>> entry : cache.entrySet()){
					V value = resource.find(entry.getValue().paras);
					if(!(value == null ? entry.getValue().value == null : value.equals(entry.getValue().value))) return false;
				}
				for(Entry<List<K>, Value<K, V>> entry : exec.entrySet()) try{
					if(entry.getValue().type == 0) resource.store(entry.getValue().value, entry.getValue().paras);
					else if(entry.getValue().type == 1) resource.discard(entry.getValue().paras);
					else if(entry.getValue().type == 2) resource.empty(entry.getValue().paras);
				}catch(Exception e){
					log.error(entry.getKey() + " : transaction perform error.", e);
				}
			}
			return true;
		}
	}
	
	protected class ListableProxy extends Proxy implements IListableResource<K, V>{
		protected ListableProxy(IProcessor<T, R> performer) {
			super(performer);
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			return ((IListableResource<K, V>) resource).keys(paras);
		}

		@Override
		public Map<K[], V> all(K... paras) {
			Collection<K[]> keys = ((IListableResource<K, V>) resource).keys(paras);
			Map<K[], V> map = new LinkedHashMap<K[], V>();
			if(keys != null) for(K[] key : keys) map.put(key, find(key));
			return map;
		}
	}
	
	protected class BatchProxy implements IResource<K, V>{
		protected HashMap<List<K>, V> cache = new HashMap<List<K>, V>();
		protected HashMap<List<K>, Map<K, Value<K, V>>> keys = new HashMap<List<K>, Map<K, Value<K, V>>>();
		protected HashMap<List<K>, Set<List<K>>> map = new HashMap<List<K>, Set<List<K>>>();
		protected LinkedHashMap<List<K>, Value<K, V>> exec = new LinkedHashMap<List<K>, Value<K, V>>();
		protected IProcessor<T, R> performer;
		
		protected BatchProxy(IProcessor<T, R> performer) {
			this.performer = performer;
		}

		@Override
		public V find(K... paras) {
			if(paras != null && paras.length > prefixLength){
				K[] reparas;
				if(paras.length == prefixLength + 1) reparas = paras;
				else reparas = Arrays.copyOf(paras, prefixLength + 1);
				List<K> key = Arrays.asList(reparas);
				if(cache.containsKey(key)) return cache.get(key);
				else{
					V ret = resource.find(reparas);
					List<K> tmp = new ArrayList<K>(prefixLength);
					for(int i = 0; i < prefixLength; i++) tmp.add(paras[i]);
					synchronized(keys){
						synchronized(cache){
							Map<K, Value<K, V>> set = keys.get(tmp);
							if(set == null) {
								set = new HashMap<K, Value<K, V>>();
								keys.put(tmp, set);
							}
							set.put(reparas[prefixLength], new Value<K, V>(-1, ret, reparas));
							cache.put(key, ret);
						}
					}
					return ret;
				}
			}
			return null;
		}

		@Override
		public <P> P store(V value, K... paras) {
			if(paras != null && paras.length > prefixLength){
				K[] reparas;
				if(paras.length == prefixLength + 1) reparas = paras;
				else reparas = Arrays.copyOf(paras, prefixLength + 1);
				List<K> tmp = new ArrayList<K>(prefixLength);
				for(int i = 0; i < prefixLength; i++) tmp.add(paras[i]);
				List<K> key = Arrays.asList(reparas);
				synchronized(map){
					synchronized(exec){
						Set<List<K>> set = map.get(tmp);
						if(set == null) {
							set = new HashSet<List<K>>();
							map.put(tmp, set);
						}
						set.add(key);
						exec.remove(key);
						exec.put(key, new Value<K, V>(0, value, reparas));
					}
				}
			}
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null && paras.length > prefixLength){
				K[] reparas;
				if(paras.length == prefixLength + 1) reparas = paras;
				else reparas = Arrays.copyOf(paras, prefixLength + 1);
				List<K> tmp = new ArrayList<K>(prefixLength);
				for(int i = 0; i < prefixLength; i++) tmp.add(paras[i]);
				List<K> key = Arrays.asList(reparas);
				synchronized(map){
					synchronized(exec){
						Set<List<K>> set = map.get(tmp);
						if(set == null) {
							set = new HashSet<List<K>>();
							map.put(tmp, set);
						}
						set.add(key);
						exec.remove(key);
						exec.put(key, new Value<K, V>(1, null, reparas));
					}
				}
			}
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			if(paras != null && paras.length >= prefixLength){
				K[] reparas;
				List<K> key;
				if(paras.length == prefixLength) reparas = paras;
				else reparas = Arrays.copyOf(paras, prefixLength);
				key = Arrays.asList(reparas);
				synchronized(map){
					synchronized(exec){
						for(List<K> ls : map.remove(key)) exec.remove(ls);
						exec.remove(key);
						exec.put(key, new Value<K, V>(2, null, reparas));
					}
				}
			}
			return null;
		}
		
		protected boolean perform(){
			if(exec.size() > 0) synchronized(resource){
				if(decision != null && decision != null) {
					if(!decision.process(new Object[]{performer, resource, cache, keys, map, exec})) return false;
				} else if(comparator != null && comparator != null) {
					for(Entry<List<K>, Map<K, Value<K, V>>> entry : keys.entrySet()) if(entry.getValue().size() > 0){
						List<K> k = entry.getKey();
						K[] arr = entry.getValue().keySet().toArray(PlatformUtil.newArrayOf(entry.getValue().values().iterator().next().paras, entry.getValue().size()));
						k.addAll(Arrays.asList(arr));
						Object news = resource.find(k.toArray(PlatformUtil.newArrayOf(arr, k.size())));
						if(!List.class.isInstance(news) || ((List)news).size() != arr.length) return false;
						else for(int i = 0; i < arr.length; i++){
							Value<K, V> oldvalue = entry.getValue().get(arr[i]);
							Object newone = ((List)news).get(i);		
							if(!comparator.process(new Object[]{performer, oldvalue.paras, oldvalue.value, newone})) return false;				
						}
					}
				}else for(Entry<List<K>, Map<K, Value<K, V>>> entry : keys.entrySet()) if(entry.getValue().size() > 0){
					List<K> k = entry.getKey();
					K[] arr = entry.getValue().keySet().toArray(PlatformUtil.newArrayOf(entry.getValue().values().iterator().next().paras, entry.getValue().keySet().size()));
					k.addAll(Arrays.asList(arr));
					Object news = resource.find(k.toArray(PlatformUtil.newArrayOf(arr, k.size())));
					if(!List.class.isInstance(news) || ((List)news).size() != entry.getValue().keySet().size()) return false;
					else for(int i = 0; i < arr.length; i++){
						Object oldone = entry.getValue().get(arr[i]).value;
						Object newone = ((List)news).get(i);		
						if(!(oldone == null ? newone == null : oldone.equals(newone))) return false;				
					}
				}
				List<K> prefix = null;
				HashMap<K, V> put = new HashMap<K, V>();
				HashSet<K> remove = new HashSet<K>();
				int count = 0;
				for(Entry<List<K>, Value<K, V>> entry : exec.entrySet()) try{
					if(entry.getValue().type == 2){
						if(put.size() > 0) {
							if(prefix.size() == 0) resource.store((V)put);
							else resource.store((V)put, prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
							put = new HashMap<K, V>();
						}
						if(remove.size() > 0) {
							prefix.addAll(remove);
							resource.discard(prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
							remove = new HashSet<K>();
						}
						prefix = null;
						resource.empty(entry.getValue().paras);
					} else {
						List<K> ls = new ArrayList<K>(entry.getKey().size());
						ls.addAll(entry.getKey());
						K key = ls.remove(ls.size() - 1);
						if(prefix != null && !prefix.equals(ls)){
							if(put.size() > 0) {
								if(prefix.size() == 0) resource.store((V)put);
								else resource.store((V)put, prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
								put = new HashMap<K, V>();
							}
							if(remove.size() > 0) {
								prefix.addAll(remove);
								resource.discard(prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
								remove = new HashSet<K>();
							}
						}
						prefix = ls;
						if(entry.getValue().type == 0) put.put(key, entry.getValue().value);
						else if(entry.getValue().type == 1) remove.add(key);
						count ++;
						if(count >= exec.size()){
							if(put.size() > 0) {
								if(prefix.size() == 0) resource.store((V)put);
								else resource.store((V)put, prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
							}
							if(remove.size() > 0) {
								prefix.addAll(remove);
								resource.discard(prefix.toArray(PlatformUtil.newArrayOf(entry.getValue().paras, prefix.size())));
							}
						}
					}
				}catch(Exception e){
					log.error(entry.getKey() + " : transaction perform error.", e);
				}
			}
			return true;
		}
	}

	protected class ListableBatchProxy extends BatchProxy implements IListableResource<K, V>{
		protected ListableBatchProxy(IProcessor<T, R> performer) {
			super(performer);
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			return ((IListableResource<K, V>) resource).keys(paras);
		}

		@Override
		public Map<K[], V> all(K... paras) {
			Collection<K[]> keys = ((IListableResource<K, V>) resource).keys(paras);
			Map<K[], V> map = new LinkedHashMap<K[], V>();
			if(keys != null) for(K[] key : keys) map.put(key, find(key));
			return map;
		}
	}
	
	@Override
	public R execute(IProcessor<T, R> processor) {
		R ret = null;
		boolean suc = false;
		int count = 0;
		int max = 52;
		do try {
			count ++;
			IResource<K, V> proxy;
			if(IListableResource.class.isInstance(resource)) if(batch) proxy = new ListableBatchProxy(processor);
			else proxy = new ListableProxy(processor);
			else if(batch) proxy = new BatchProxy(processor);
			else proxy = new Proxy(processor);
			ret = processor.process((T) proxy);
			if(Proxy.class.isInstance(proxy)) suc = ((Proxy)proxy).perform();
			else suc = ((BatchProxy)proxy).perform();
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
