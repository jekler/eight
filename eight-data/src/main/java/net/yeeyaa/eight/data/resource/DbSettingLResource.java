package net.yeeyaa.eight.data.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.data.dao.PlatformDao;
import net.yeeyaa.eight.data.entity.SettingEntity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DbSettingLResource<K> implements IListableResource<K, String>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected ConcurrentHashMap<K, String> settingmap = new ConcurrentHashMap<K, String>();
	protected Boolean cache = false;
	protected PlatformDao<String, Object, SettingEntity, Object> platformDao;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			try{
				return platformDao.countAll(SettingEntity.class);
			}catch(Exception e){
				log.error("cannot count resource", e);		
			}
			return null;
		}
	};
	
	public DbSettingLResource() {
		log = LoggerFactory.getLogger(DbSettingLResource.class);
	}

	public DbSettingLResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(DbSettingLResource.class) : log;
	}

	public void setPlatformDao(PlatformDao<String, Object, SettingEntity, Object> platformDao) {
		if(platformDao != null) this.platformDao = platformDao;
	}
		
	public void setCache(Boolean cache) {
		if(cache != null) this.cache = cache;
	}
	
	@Override
	public <P> P store(String resource, K ... paras)  {
		if(paras != null && paras.length > 0)try{
			synchronized(this){
				SettingEntity instance = platformDao.findById(SettingEntity.class, paras[0].toString());
				if(instance == null) instance = new SettingEntity();
				if(instance.getVariable() == null)instance.setVariable(paras[0].toString());
				if(resource == null)instance.setData("");
				else instance.setData(resource);
				platformDao.saveOrUpdate(instance);
				if(cache) settingmap.put(paras[0], instance.getData());
			}
		}catch(Exception e){
			log.error("cannot set resource "+paras[0]+" in string setting", e);		
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 0) try{
			synchronized(this){
				SettingEntity instance = platformDao.findById(SettingEntity.class, paras[0].toString());
				if(instance != null) platformDao.delete(instance);
				if(cache) settingmap.put(paras[0], null);
			}
		}catch(Exception e){
			log.error("cannot remove resource "+paras[0]+" in string setting", e);		
		}
		return null;
	}

	@Override
	public String find(K ... paras) {
		if(paras != null && paras.length > 0) if(cache && settingmap.containsKey(paras[0])) return settingmap.get(paras[0]);
		else try{
			synchronized(this){
				SettingEntity instance = platformDao.findById(SettingEntity.class, paras[0].toString());
				if(instance != null) {
					if(cache) settingmap.put(paras[0], instance.getData());
					return instance.getData();
				}else if(cache) settingmap.put(paras[0], null);
			}
		}catch(Exception e){
			log.error("cannot find "+paras[0]+" resource in setting", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		try{
			synchronized(this){
				if(cache)settingmap = new ConcurrentHashMap<K, String>();
				platformDao.executeHql("delete from Setting", null);
			}
		}catch(Exception e){
			log.error("cannot clear resource", e);		
		}
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		try{
			synchronized(this){
				List<SettingEntity> ls = platformDao.findAll(SettingEntity.class);
				ArrayList ret = new ArrayList(ls.size());
				if(cache)settingmap.clear();
				for(SettingEntity setting : ls) {
					String[] str = {setting.getVariable()};
					ret.add(str);
					if(cache)settingmap.put((K)setting.getVariable(), setting.getData());
				}
				return ret;
			}
		}catch(Exception e){
			log.error("cannot list resource", e);		
		}
		return null;
	}

	@Override
	public Map<K[], String> all(K... paras) {
		try{
			synchronized(this){
				List<SettingEntity> ls = platformDao.findAll(SettingEntity.class);
				Map<K[], String> ret = new HashMap<K[], String>(ls.size());
				if(cache)settingmap.clear();
				for(SettingEntity setting : ls) {
					String[] str = {setting.getVariable()};
					ret.put((K[])str, setting.getData());
					if(cache)settingmap.put((K)setting.getVariable(), setting.getData());
				}
				return ret;
			}
		}catch(Exception e){
			log.error("cannot list all", e);		
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
