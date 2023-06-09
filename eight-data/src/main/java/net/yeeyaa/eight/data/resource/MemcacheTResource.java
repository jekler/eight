package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.spy.memcached.internal.OperationFuture;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MemcacheTResource<K, V, R> implements ITransactionResource<K, V, IResource<K, V>, R> {
	protected final Logger log;
	protected MemcachedClient mc;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected Integer timeout = 0;
	protected Boolean touch = false; 
	
	public MemcacheTResource() {
		log = LoggerFactory.getLogger(MemcacheTResource.class);
	}

	public MemcacheTResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheTResource.class) : log;
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

	@Override
	public <P> P store(V resource, K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) mc.set(paras[0].toString(), timeout, resource);
		return null;
	}
	
	@Override
	public <P> P discard(final K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) mc.delete(paras[0].toString());
		return null;
	}
	
	@Override
	public <P> P empty(K... paras) {
		mc.flush();
		return null;
	}
	
	@Override
	public V find(K... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) if (touch && timeout > 0) {
			CASValue<Object> value = mc.getAndTouch(paras[0].toString(), timeout);
			return value == null ? null : (V) value.getValue();
		} else return (V)mc.get(paras[0].toString());
		else return null;
	}

	protected static class Value<V>{
		int type;
		V value;
		
		protected Value(int type, V value) {
			this.type = type;
			this.value = value;
		}
	}
	
	protected class Resource implements IResource<K, V> {
		protected ConcurrentHashMap<String, CASValue<Object>> cache = new ConcurrentHashMap<String, CASValue<Object>>();
		protected Map<K, Value<V>> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value<V>>());
		
		@Override
		public V find(final K... paras) {
			if(paras != null && paras.length > 0 && paras[0] != null) if(cache.containsKey(paras[0])) return (V)cache.get(paras[0]).getValue();
			else {
				CASValue<Object> ret = mc.gets(paras[0].toString());
				if (touch && timeout > 0) mc.touch(paras[0].toString(), timeout);
				cache.put(paras[0].toString(), ret);
				return (V)ret.getValue();
			}
			return null;
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
			if(paras != null && paras.length > 0 && paras[0] != null) {
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
				Boolean clear = false;
				HashMap<K, V> put = new HashMap<K, V>();
				HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value<V>> entry : exec.entrySet()) if(entry.getValue() == null) clear = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
		    	if(clear) mc.flush();
				if(put.size() > 0) for(Entry<K, V> entry : put.entrySet()) mc.set(entry.getKey().toString(), timeout, entry.getValue());
				if(remove.size() > 0) for(K key : remove) mc.delete(key.toString());
			}
			return true;
		}
	}

	@Override
	public R execute(final IProcessor<IResource<K, V>, R> processor) {
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
			log.error("MemcacheTResource: sleep interrupted:", e);
			throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
		} while(!suc && (retry == 0 || count < retry));
		if(suc) return ret;
		else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
