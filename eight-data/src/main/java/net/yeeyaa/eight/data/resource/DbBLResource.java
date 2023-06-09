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


public class DbBLResource<K> implements IListableResource<K, Object>, IExtendable<Object> {
	protected final Logger log;
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final ConcurrentHashMap<K, Map<K, Object>> map = new ConcurrentHashMap<K, Map<K, Object>>();
	protected Boolean cache = false;
	protected PlatformDao<String, Object, ResourceEntity, Object> platformDao;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			if(paras != null && paras.length > 0){
				Map<K[], Object> ret = new HashMap<K[], Object>();
				if(cache && map.containsKey(paras[0])) {
					if(map.get(paras[0]) != null) return new Long(map.get(paras[0]).size());
				} else try {
					synchronized(this){
						ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
						if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
						if(instance != null && instance.getExt() != null) return new Long(instance.getExt().size());
					}
				}catch(Exception e){
					log.error("cannot count "+paras[0]+" in resource", e);		
				}
			}
			return null;
		}
	};
	
	public DbBLResource() {
		log = LoggerFactory.getLogger(DbBLResource.class);
	}

	public DbBLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(DbBLResource.class) : log;
	}
	
	public void setPlatformDao(PlatformDao<String, Object, ResourceEntity, Object> platformDao) {
		if(platformDao != null) this.platformDao = platformDao;
	}

	public void setCache(Boolean cache) {
		if(cache != null) this.cache = cache;
	}
	
	@Override
	public <P> P store(Object resource, K ... paras) {
		if(paras != null && paras.length > 0) synchronized(this){
			try{
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(instance == null) {
					instance = new ResourceEntity();
					instance.setVariable(paras[0].toString());
				}
				Map<K, Object> hm = instance.getExt();
				if(hm == null) hm = new ConcurrentHashMap<K, Object>();
				if(paras.length > 1) hm.put(paras[1], resource);
				else if(Map.class.isInstance(resource)) hm.putAll((Map<K, Object>)resource);
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
		if(paras != null && paras.length > 1) synchronized(this){
			try{
				ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
				if(instance == null) {
					if(cache) map.put(paras[0], null);
				}else {
					Map<K, Object> submap = instance.getExt();
					if(submap == null) {
						if(cache) map.put(paras[0], null);
					}else {
						for(int i = 1; i < paras.length; i++) submap.remove(paras[i]);
						instance.setExt(submap);
						platformDao.saveOrUpdate(instance);
						if(cache) map.put(paras[0], submap);
					}
				}
			}catch(Exception e){
				log.error("cannot set resource "+paras[0]+" in resource", e);		
			}
		}
		return null;
	}

	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > 1) {
			ArrayList<Object> ret = new ArrayList<Object>(paras.length - 1);
			if(cache && map.containsKey(paras[0])) {
				if(map.get(paras[0]) != null) for(int i = 1; i < paras.length; i++) ret.add(map.get(paras[0]).get(paras[i]));
			} else synchronized(this){
				try{
					ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
					if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
					if(instance != null && instance.getExt() != null) for(int i = 1; i < paras.length; i++) ret.add(instance.getExt().get(paras[i]));
				}catch(Exception e){
					log.error("cannot find "+paras[0]+" resource in resource", e);
				}
			}
			return ret;
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0) for(K para : paras) synchronized(this){
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
		if(paras != null && paras.length > 0){
			ArrayList<K[]> ret = new ArrayList<K[]>();
			if(cache && map.containsKey(paras[0])) {
				if(map.get(paras[0]) != null) for(K o : map.get(paras[0]).keySet()) {
					K[] key = PlatformUtil.newArrayOf(paras, 2);
					key[0] = paras[0];
					key[1] = o;
					ret.add(key);
				}
			} else try {
				synchronized(this){
					ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
					if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
					if(instance != null && instance.getExt() != null) for(Object o : instance.getExt().keySet()){
						K[] key = PlatformUtil.newArrayOf(paras, 2);
						key[0] = paras[0];
						key[1] = (K)o;
						ret.add(key);
					}
				}
			}catch(Exception e){
				log.error("cannot list resource "+paras[0]+" in resource", e);		
			}
			return ret;
		}
		return null;
	}

	@Override
	public Map<K[], Object> all(K... paras) {
		if(paras != null && paras.length > 0){
			Map<K[], Object> ret = new HashMap<K[], Object>();
			if(cache && map.containsKey(paras[0])) {
				if(map.get(paras[0]) != null) for(Entry<K, Object> o : map.get(paras[0]).entrySet()) {
					K[] key = PlatformUtil.newArrayOf(paras, 2);
					key[0] = paras[0];
					key[1] = o.getKey();
					ret.put(key, o.getValue());
				}
			} else try {
				synchronized(this){
					ResourceEntity instance = platformDao.findById(ResourceEntity.class, paras[0].toString());
					if(cache) map.put(paras[0], instance == null ? null : instance.getExt());
					if(instance != null && instance.getExt() != null) for(Entry<K, Object> o : ((Map<K, Object>)instance.getExt()).entrySet()){
						K[] key = PlatformUtil.newArrayOf(paras, 2);
						key[0] = paras[0];
						key[1] = o.getKey();
						ret.put(key, o.getValue());
					}
				}
			}catch(Exception e){
				log.error("cannot list all "+paras[0]+" in resource", e);		
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
}
