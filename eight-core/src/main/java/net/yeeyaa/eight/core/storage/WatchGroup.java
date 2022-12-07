package net.yeeyaa.eight.core.storage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class WatchGroup<K, V extends IExtendable<Object>> extends Storage<V> implements IProcessor<Object, Collection<V>> {
	protected final Logger log;
	protected IReadonlyListable<K, V> resource;
	protected K[] key;
	protected Integer mode = 0;//0: return all, modified time all 1: return news and modified, modified time all 2:return news, modified time news 3: same to 2, but storages increasing without remove (recreate news will not be detected)
	protected volatile Long lastModified = -1L;
	protected volatile ConcurrentHashMap<Object, V> storages = new ConcurrentHashMap<Object, V>();
	protected Boolean delete = false;//whether return delete storage(new ByteWStorage), usually in mode 0 and 1, be careful in mode 3.
	protected Boolean prefetch = false;
	protected IProcessor<K[], K[]> keyProcessor;
	
	public WatchGroup() {
		this.log = LoggerFactory.getLogger(WatchGroup.class);
	}
	
	public WatchGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(WatchGroup.class) : log;
	}
	
	public void setKeyProcessor(IProcessor<K[], K[]> keyProcessor) {
		this.keyProcessor = keyProcessor;
	}

	public void setPrefetch(Boolean prefetch) {
		if (prefetch != null) this.prefetch = prefetch;
	}

	public void setMode(Integer mode) {
		if(mode != null) this.mode = mode;
	}

	public void setDelete(Boolean delete) {
		if(delete != null) this.delete = delete;
	}

	public void setResource(IReadonlyListable<K, V> resource) {
		this.resource = resource;
	}

	public void setKey(K[] key) {
		this.key = key;
	}

	public Collection<V> list() {
		return list(key);
	}
	
	public Collection<V> list(K[] key) {
		Map<K[], V> map = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			map = key == null ? resource.all() : resource.all(key);
			paras = map.keySet();
		} else paras = key == null ? resource.keys() : resource.keys(key);
		ConcurrentHashMap<Object, V> ss = new ConcurrentHashMap<Object, V>();
		Set<V> news = new HashSet<V>();
		Set<V> delset = new HashSet<V>();
		if(delete) delset.addAll(storages.values());
		if(paras != null) synchronized(this){	
			for(K[] k : paras) {
				Object ls = keyProcessor == null ? Arrays.asList(k) : keyProcessor.process(k);
				if(storages.containsKey(ls)) {
					V oldone = storages.get(ls);
					ss.put(ls, map == null ? oldone : map.get(k));
					if(delete) delset.remove(oldone);
				}else {
					V newone = map == null ? resource.find(k) : map.get(k);
					news.add(newone);
					ss.put(ls, newone);
					storages.put(ls, newone);
				}
			}
		}
		if(mode != 3) storages = ss; //mode == 3: storages increasing without remove (recreate news will not be detected)
		switch(mode) {
			case 1: synchronized(this){ //mode == 1: return all modified
				Long modified = lastModified;		
				for (V r : storages.values()) try{
					Long thisModified = r.extend(Method.modified);
					if(thisModified > lastModified){
						news.add(r);
						if(thisModified > modified) modified = thisModified;
					}
				}catch(Exception e){
					log.error("WatchGroup: processor failed.", e);
				}
				lastModified = modified;
				break;
			} default : { 
				Long modified = lastModified;
				if(mode == 0) news.addAll(storages.values());//mode ==0: return all, modified time all
				for (IExtendable<Object> r : news) try{//mode 2: modified time all 3: modified time news
					Long thisModified = r.extend(Method.modified);
					if(thisModified > modified) modified = thisModified;
				}catch(Exception e){
					log.error("WatchGroup: processor failed.", e);
				}
				lastModified = modified;
			}
		}
		if(delset.size() > 0) for(V d : delset){
			ByteWStorage storage = new ByteWStorage();
			storage.setName(d.extend(Method.key));
			storage.setExist(false);
			news.add((V)storage);
		}
		return news;
	}

	public Long modified() {
		return modified(key);
	}
	
	public Long modified(K[] key) {
		Long modified = lastModified;
		Map<K[], V> map = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			map = key == null ? resource.all() : resource.all(key);
			paras = map.keySet();
		} else paras = key == null ? resource.keys() : resource.keys(key);
		List<V> news = new ArrayList<V>(paras.size());
		if(paras != null){	
			for(K[] k : paras) {
				Object ls = keyProcessor == null ? Arrays.asList(k) : keyProcessor.process(k);
				if(!storages.containsKey(ls)) news.add(map == null ? resource.find(k) : map.get(k));
				else if(mode != 2 && mode != 3) news.add(storages.get(ls));
			}
			for (IExtendable<Object> r : news) try{
				Long thisModified = r.extend(Method.modified);
				if(thisModified > modified) modified = thisModified;
			}catch(Exception e){
				log.error("WatchGroup: processor failed.", e);
			}
		}
		return modified;
	}

	@Override
	public Collection<V> process(Object instance) {
		return instance != null && instance instanceof Object[] ? list((K[]) instance) : list();
	}
}