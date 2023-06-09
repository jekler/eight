package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.data.dao.PlatformDao;
import net.yeeyaa.eight.data.entity.ResourceEntity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DbLResource<K, V> implements IListableResource<K, V>, IExtendable<Object>  {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final ConcurrentHashMap<K, Map<K, V>> map = new ConcurrentHashMap<K, Map<K, V>>();
	protected Boolean cache = false;
	protected PlatformDao<String, Object, ResourceEntity, Object> platformDao;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length > 0) if(cache && map.containsKey(paras[0])) {
				if(map.get(paras[0]) != null) return new Long(map.get(paras[0]).size());
			} else try {
				synchronized(this){
					ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
					if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
					if(instance != null && instance.getExt() != null) return new Long(instance.getExt().size());
				}
			}catch(Exception e){
				log.error("cannot list resource", e);		
			}
			return null;
		}
	};
	
	public DbLResource() {
		log = LoggerFactory.getLogger(DbLResource.class);
	}

	public DbLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(DbLResource.class) : log;
	}
	
	public void setPlatformDao(PlatformDao<String, Object, ResourceEntity, Object> platformDao) {
		if(platformDao != null) this.platformDao = platformDao;
	}
	
	public void setCache(Boolean cache) {
		if(cache != null) this.cache = cache;
	}
	
	@Override
	public <P> P store(V resource, K ... paras) {
		if(paras != null && paras.length > 1) synchronized(this){
			try{
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(instance == null) {
					instance = new ResourceEntity();
					instance.setVariable(paras[0].toString());
				}
				Map<K, V> hm = instance.getExt();
				if(hm == null) hm = new ConcurrentHashMap<K, V>();
				hm.put(paras[1], resource);
				instance.setExt(hm);
				platformDao.saveOrUpdate(instance);
				if(cache) map.put(paras[0], hm);
			}catch(Exception e){
				log.error("cannot set resource "+paras[0]+" in resource", e);		
			}
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 1) try{
			synchronized(this){
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(instance == null) {
					if(cache) map.put(paras[0], null);
				}else {
					Map<K, V> hm = instance.getExt();
					if(hm == null) {
						if(cache) map.put(paras[0], null);
					}else {
						hm.remove(paras[1]);
						instance.setExt(hm);
						platformDao.saveOrUpdate(instance);
						if(cache) map.put(paras[0], hm);
					}
				}
			}
		}catch(Exception e){
			log.error("cannot set resource "+paras[0]+" in resource", e);		
		}
		return null;
	}

	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 1){
			if(cache && map.containsKey(paras[0])) {
				if(map.get(paras[0]) != null) return map.get(paras[0]).get(paras[1]);
			} else try{
				synchronized(this){
					ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
					if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
					if(instance != null && instance.getExt() != null) return (V)instance.getExt().get(paras[1]);
				}
			}catch(Exception e){
				log.error("cannot find "+paras[0]+" resource in resource", e);
			}
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0) synchronized(this){
			try {
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(instance != null) if(paras.length > 1 && "delete".equals(paras[1])) platformDao.delete(instance);
				else {
					instance.setExt(null);
					platformDao.saveOrUpdate(instance);
				}
				if(cache) map.put(paras[0], null);
			}catch(Exception e){
				log.error("cannot clear resource "+paras[0]+" in resource", e);		
			}
		}
		return null;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		if(paras != null && paras.length > 0) if(cache && map.containsKey(paras[0])) {
			if(map.get(paras[0]) != null) {
				ArrayList<K[]> ret = new ArrayList<K[]>(map.get(paras[0]).size());
				for(K o : map.get(paras[0]).keySet()) {
					K[] key = PlatformUtil.newArrayOf(paras, 2);
					key[0] = paras[0];
					key[1] = o;
					ret.add(key);
				}
				return ret;
			}
		} else try {
			synchronized(this){
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
				if(instance != null && instance.getExt() != null) {
					ArrayList<K[]> ret = new ArrayList<K[]>(instance.getExt().size());
					for(Object o : instance.getExt().keySet()) {
						K[] key = PlatformUtil.newArrayOf(paras, 2);
						key[0] = paras[0];
						key[1] = (K)o;
						ret.add(key);
					}
					return ret;
				}
			}
		}catch(Exception e){
			log.error("cannot list resource", e);		
		}
		return null;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		if(paras != null && paras.length > 0) if(cache && map.containsKey(paras[0])) {
			if(map.get(paras[0]) != null) {
				Map<K[], V> ret = new HashMap<K[], V>(map.get(paras[0]).size());
				for(Entry<K, V> o : map.get(paras[0]).entrySet()) {
					K[] key = PlatformUtil.newArrayOf(paras, 2);
					key[0] = paras[0];
					key[1] = o.getKey();
					ret.put(key, o.getValue());
				}
				return ret;
			}
		} else try {
			synchronized(this){
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
				if(instance != null && instance.getExt() != null) {
					Map<K[], V> ret = new HashMap<K[], V>(instance.getExt().size());
					for(Entry<K, V> o : ((Map<K, V>)instance.getExt()).entrySet()) {
						K[] key = PlatformUtil.newArrayOf(paras, 2);
						key[0] = paras[0];
						key[1] = o.getKey();
						ret.put(key, o.getValue());
					}
					return ret;
				}
			}
		}catch(Exception e){
			log.error("cannot list resource", e);		
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
