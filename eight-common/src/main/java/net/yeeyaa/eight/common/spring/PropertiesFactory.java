package net.yeeyaa.eight.common.spring;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IProcessor;

import org.springframework.beans.factory.FactoryBean;


public class PropertiesFactory implements FactoryBean<Properties>, IProcessor<Object, Properties> {
	protected IListable<Object, Object> resource;
	protected Boolean singleton = true;
	protected Object[] key;
	
	public void setKey(Object[] key) {
		this.key = key;
	}

	public void setSingleton(Boolean singleton) {
		if(singleton != null) this.singleton = singleton;
	}

	public void setResource(IListable<Object, Object> resource) {
		this.resource = resource;
	}

	@Override
	public Properties getObject() throws Exception {
		return process(null);
	}

	@Override
	public Class<?> getObjectType() {
		return Properties.class;
	}

	@Override
	public boolean isSingleton() {
		return singleton;
	}

	@Override
	public Properties process(Object instance) {
		Map<Object[], Object> map;
		if(instance instanceof Object[]) map = resource.all((Object[]) instance);
		else if(instance != null) map = resource.all(instance);
		else if(key == null) map = resource.all();
		else map = resource.all(key);
		Properties ret = new Properties();
		if(map != null) for(Entry<Object[], Object> entry : map.entrySet()) ret.put(entry.getKey()[key == null ? 0 : key.length], entry.getValue());
		return ret;
	}
}
