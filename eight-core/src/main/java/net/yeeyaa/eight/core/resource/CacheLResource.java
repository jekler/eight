package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.core.util.ConcurrentWeakIdentityHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CacheLResource<K, V> implements IListableResource<K, V> {
	protected final Logger log;
	protected Boolean gc = true;
	protected Integer bucketSize = 0;
	protected Integer listSize = 0;
	protected IProcessor<Object, Object> destroy;
	protected volatile Map<K, V> resource;
	protected volatile Set<K>[] buckets;
	protected volatile Integer index = 0;
	protected ExecutorService executor;
	protected IProcessor<V, Object> finalize;
	protected IProcessor<V, Object> release;
	
	public CacheLResource() {
		this.log = LoggerFactory.getLogger(CacheLResource.class);
	}
	
	public CacheLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CacheLResource.class) : log;
	}
	
	public void setFinalize(IProcessor<V, Object> finalize) {
		this.finalize = finalize;
	}

	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}
	
	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	public void setRelease(IProcessor<V, Object> release) {
		this.release = release;
	}

	public void setBucketSize(Integer bucketSize) {
		if (bucketSize != null && bucketSize > 0) this.bucketSize = bucketSize;
	}

	public void setListSize(Integer listSize) {
		if (listSize != null && listSize > 0) this.listSize = listSize;
	}

	public void setGc(Boolean gc) {
		if (gc != null) this.gc = gc;
	}

	@PostConstruct
	public void initialize(){
		if (gc) if (bucketSize > 0 && listSize > 0) resource = new ConcurrentWeakIdentityHashMap<K, V>(bucketSize * listSize, false);
		else resource = new ConcurrentWeakIdentityHashMap<K, V>(false);
		else if (bucketSize > 0 && listSize > 0) resource = new ConcurrentHashMap<K, V>(bucketSize * listSize);
		else resource = new ConcurrentHashMap<K, V>();
		if(bucketSize > 0 && listSize > 0) {
			buckets = new Set[listSize];
			for(int i = 0; i < listSize; i++) buckets[i] = Collections.newSetFromMap(new ConcurrentHashMap<K, Boolean>());
		}
	}
	
	@PreDestroy
	public void destroy(){
		try {
			if(destroy != null) destroy.process(executor);
			executor = null;
		} finally {
			if (release != null) for (V value : resource.values()) try{
				release.process(value);
			} catch (Exception e) {
				log.error("CacheLResource : release resource error.", e);
			}
			if (finalize != null) for (V ret : resource.values()) try{
				finalize.process(ret);
			} catch (Exception e) {
				log.error("CacheLResource : release resource error.", e);
			}
		}
	}
	
	protected synchronized Set<K> lru(){
		if(buckets[index].size() > bucketSize){
			Integer i = (index + 1) % listSize;
			final Set<K> set = buckets[i];
			if(set.size() >= bucketSize) {
				buckets[i] = Collections.newSetFromMap(new ConcurrentHashMap<K, Boolean>());
				Runnable run = new Runnable(){
					@Override
					public void run() {
						for(K key : set) {
							Boolean del = true;
							for(Set<K> bucket : buckets) if(bucket.contains(key)){
								del = false;
								break;
							}
							if(del) {
								V ret;
								if (release == null) ret = resource.remove(key);
								else {
									ret = resource.get(key);
									if (ret != null) {
										release.process(ret);
										resource.remove(key);
									}
								} 
								if (ret != null && finalize != null) try{
									finalize.process(ret);
								} catch (Exception e) {
									log.error("CacheLResource : release resource error.", e);
								}
							}
						}			
					}		
				};
				if (executor == null) new Thread(run).start();
				else executor.execute(run);
			}
			index = i;
		}
		return buckets[index];
	}
	
	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			V ret = resource.get(paras[0]);
			if(ret != null && bucketSize > 0 && listSize > 0) lru().add(paras[0]);
			return ret;
		} else return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null && value != null) {
			resource.put(paras[0], value);
			if(bucketSize > 0 && listSize > 0) lru().add(paras[0]);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 0 && paras[0] != null) {
			V ret;
			if (release == null) ret = resource.remove(paras[0]);
			else {
				ret = resource.get(paras[0]);
				if (ret != null) {
					release.process(ret);
					resource.remove(paras[0]);
				}
			} 
			if (ret != null && finalize != null) try{
				finalize.process(ret);
			} catch (Exception e) {
				log.error("CacheLResource : release resource error.", e);
			}
		}
		return null;
	}

	@Override
	public synchronized <P> P empty(K... paras) {
		if (release != null) for (V value : resource.values()) try{
			release.process(value);
		} catch (Exception e) {
			log.error("CacheLResource : release resource error.", e);
		}
		if (finalize != null) for (V value : resource.values()) try{
			finalize.process(value);
		} catch (Exception e) {
			log.error("CacheLResource : release resource error.", e);
		}
		if(bucketSize > 0 && listSize > 0) {
			Set<K>[] buckets = new Set[listSize];
			index = 0;
			for(int i = 0; i < listSize; i++) buckets[i] = Collections.newSetFromMap(new ConcurrentHashMap<K, Boolean>());
			this.buckets = buckets;
		}
		if (gc) if (bucketSize > 0 && listSize > 0) resource = new ConcurrentWeakIdentityHashMap<K, V>(bucketSize * listSize, false);
		else resource = new ConcurrentWeakIdentityHashMap<K, V>(false);
		else if (bucketSize > 0 && listSize > 0) resource = new ConcurrentHashMap<K, V>(bucketSize * listSize);
		else resource = new ConcurrentHashMap<K, V>();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K> c = resource.keySet();
		Collection<K[]> ls = new ArrayList<K[]>(c.size());
		for(K o : c) {
			K[] key = PlatformUtil.newArrayOf(1, o);
			key[0] = o;
			ls.add(key);
		}
		return ls;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Map<K[], V> map = new HashMap<K[], V>(resource.size());
		for(Entry<K, V> o : resource.entrySet()) {
			K[] key = PlatformUtil.newArrayOf(1, o.getKey());
			key[0] = o.getKey();
			map.put(key, o.getValue());
		}
		return map;
	}
	
	public class CacheProcessor implements IProcessor<K, V> {
		protected IProcessor<K, V> processor;
		protected Boolean cache;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setProcessor(IProcessor<K, V> processor) {
			this.processor = processor;
		}

		@Override
		public V process(K instance) {
			V ret = find(instance);
			if (ret == null && processor != null) {
				ret = processor.process(instance);
				if (ret != null && Boolean.TRUE.equals(cache)) store(ret, instance);
			}
			return ret;
		}
	}
	
	public class CacheResource implements IInputResource<K, V> {
		protected IInputResource<K, V> resource;
		protected Boolean cache;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}
		
		public void setResource(IInputResource<K, V> resource) {
			this.resource = resource;
		}

		@Override
		public V find(K... paras) {
			V ret = find(paras);
			if (ret == null) {
				ret = resource.find(paras);
				if (ret != null && Boolean.TRUE.equals(cache)) store(ret, paras);
			}
			return ret;
		}
	}
}
