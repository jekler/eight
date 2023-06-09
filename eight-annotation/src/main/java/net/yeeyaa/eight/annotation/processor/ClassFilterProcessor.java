package net.yeeyaa.eight.annotation.processor;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.annotation.tag.ServiceClass;
import net.yeeyaa.eight.annotation.tag.ServiceMethod;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ClassFilterProcessor implements IProcessor<Collection<Class<?>>, Collection<Class<?>>> {
	protected final Logger log;
	protected Map<String, IProcessor<Collection<Class<?>>, Void>> processors;
	protected IProcessor<Collection<Class<?>>, Void> defaultProcessor;
	protected Boolean strict;
	protected Boolean method;

	public ClassFilterProcessor() {
		this.log = LoggerFactory.getLogger(ClassFilterProcessor.class);
	}

	public ClassFilterProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ClassFilterProcessor.class) : log;
	}
	
	public void setProcessors(Map<String, IProcessor<Collection<Class<?>>, Void>> processors) {
		this.processors = processors;
	}

	public void setStrict(Boolean strict) {
		this.strict = strict;
	}

	public void setMethod(Boolean method) {
		this.method = method;
	}

	public void setDefaultProcessor(IProcessor<Collection<Class<?>>, Void> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	@Override
	public Collection<Class<?>> process(Collection<Class<?>> instance) {
		if (instance != null && instance.size() > 0 && (defaultProcessor != null || processors != null)) {
			Map<String, Collection<Class<?>>> map = new HashMap<String, Collection<Class<?>>>();
			Collection<Class<?>> set = new HashSet<Class<?>>();
			for (Class<?> c : instance) if(c.isAnnotationPresent(ServiceClass.class)) {
				String filter = c.getAnnotation(ServiceClass.class).filter();
				if (filter != null) filter = filter.trim();
				if (Boolean.FALSE.equals(method)) {
					boolean needput = true;
					for (Method me : c.getMethods()) if (me.isAnnotationPresent(ServiceMethod.class)){
						String mfilter = me.getAnnotation(ServiceMethod.class).filter();
						if (mfilter != null) mfilter = mfilter.trim();
						if (mfilter != null && mfilter.length() > 0 && !mfilter.equals(filter)) {
							needput = false;
							break;
						}
					}
					if (needput) if (filter == null || filter.length() == 0) set.add(c);
					else {
						Collection<Class<?>> subset = map.get(filter);
						if (subset == null) {
							subset = new HashSet<Class<?>>();
							map.put(filter, subset);
						}
						subset.add(c);
					}
				} else {
					if (filter == null || filter.length() == 0) set.add(c);
					else {
						Collection<Class<?>> subset = map.get(filter);
						if (subset == null) {
							subset = new HashSet<Class<?>>();
							map.put(filter, subset);
						}
						subset.add(c);
					} 
					if (method != null) for (Method me : c.getMethods()) if (me.isAnnotationPresent(ServiceMethod.class)){
						String mfilter = me.getAnnotation(ServiceMethod.class).filter();
						if (mfilter != null) mfilter = mfilter.trim();
						if (mfilter != null && mfilter.length() > 0) {
							Collection<Class<?>> subset = map.get(mfilter);
							if (subset == null) {
								subset = new HashSet<Class<?>>();
								map.put(mfilter, subset);
							}
							subset.add(c);
						}
					}
				}
			} else if (Boolean.FALSE.equals(strict)) set.add(c);
			Iterator<Entry<String, Collection<Class<?>>>> itr = map.entrySet().iterator();
			if (processors != null) while (itr.hasNext()) {
				Entry<String, Collection<Class<?>>> entry = itr.next();
				IProcessor<Collection<Class<?>>, Void> processor = processors.get(entry.getKey());
				if (processor != null) try {
					itr.remove();
					processor.process(entry.getValue());
				} catch (Exception e) {
					log.error("ClassFilterProcessor: processor failed.", e);
				}
			}
			if (defaultProcessor != null) try {
				if (!Boolean.TRUE.equals(strict)) for (Collection<Class<?>> cs : map.values()) set.addAll(cs);
				defaultProcessor.process(set);
			} catch (Exception e) {
				log.error("ClassFilterProcessor: processor failed.", e);
			}
		}
		return instance;
	}
}
