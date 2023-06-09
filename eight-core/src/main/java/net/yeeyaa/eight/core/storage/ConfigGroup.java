package net.yeeyaa.eight.core.storage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ConfigGroup<T extends IExtendable<Object>> extends Storage<T> implements IProcessor<Object, Collection<T>> {
	protected final Logger log;
	protected List<T> storages = new ArrayList<T>();
	protected Boolean changeOnly = false;
	protected Long lastModified = -1L;
	
	public ConfigGroup() {
		this.log = LoggerFactory.getLogger(ConfigGroup.class);
	}
	
	public ConfigGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ConfigGroup.class) : log;
	}
	
	public void setChangeOnly(Boolean changeOnly) {
		if(changeOnly != null) this.changeOnly = changeOnly;
	}

	public void setStorages(List<T> storages) {
		if(storages != null) this.storages = storages;
	}

	public List<T> list() {
		if(changeOnly) synchronized(this){
			List<T> ls = new LinkedList<T>();
			Long modified = lastModified;
			for (T r : storages) try{
				Long thisModified = r.extend(Method.modified);
				if(thisModified > lastModified){
					ls.add(r);
					if(thisModified > modified) modified = thisModified;
				}
			}catch(Exception e){
				log.error("ConfigGroup: processor failed.", e);
			}
			lastModified = modified;
			return ls;
		} else return new ArrayList<T>(storages);
	}

	public Long modified() {
		Long modified = lastModified;
		if(storages != null) for (IExtendable<Object> r : storages) try{
			Long thisModified = r.extend(Method.modified);
			if(thisModified > modified) modified = thisModified;
		}catch(Exception e){
			log.error("ConfigGroup: processor failed.", e);
		}
		return modified;
	}

	@Override
	public Collection<T> process(Object instance) {
		return list();
	}
}
