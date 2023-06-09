package net.yeeyaa.eight.core.storage;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class GroupGroup<K, V extends IExtendable<Object>> extends Storage<V> implements IProcessor<Object, Collection<V>> {
	protected final Logger log;
	protected volatile Long lastModified = -1L;
	protected Collection<IExtendable<Object>> groups;
	protected IProcessor<IExtendable<Object>, V> convertor;
	protected Boolean set = true; 
	
	public GroupGroup() {
		this.log = LoggerFactory.getLogger(GroupGroup.class);
	}
	
	public GroupGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(GroupGroup.class) : log;
	}
	
	public void setGroups(Collection<IExtendable<Object>> groups) {
		this.groups = groups;
	}

	public void setConvertor(IProcessor<IExtendable<Object>, V> convertor) {
		this.convertor = convertor;
	}

	public void setSet(Boolean set) {
		if (set != null) this.set = set;
	}

	public synchronized Collection<V> list() {
		Collection<V> ret = set ? new HashSet<V>() : new LinkedList<V>();
		for (IExtendable<Object> group : groups) try {
			if (convertor == null) ret.addAll(group.<Collection<V>>extend(Method.list));
			else for (IExtendable<Object> storage : group.<Collection<IExtendable<Object>>>extend(Method.list)) {
				V s = convertor.process(storage);
				if (s != null) ret.add(s);
			}
		} catch (Exception e) {
			log.error("GroupGroup: list failed: ", e);
		}
		return ret;
	}

	public synchronized Long modified() {
		Long modified = lastModified;
		for (IExtendable<Object> group : groups) try {
			Long thisModified = group.extend(Method.modified);
			if(thisModified > modified) modified = thisModified;
		} catch (Exception e) {
			log.error("GroupGroup: groupModified failed: ", e);
		}
		lastModified = modified;
		return modified;
	}

	@Override
	public Collection<V> process(Object instance) {
		return list();
	}
}
