package net.yeeyaa.eight.annotation;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.annotation.tag.ServiceClass;
import net.yeeyaa.eight.annotation.tag.ServiceMethod;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class AnnotationServiceFactory implements IProcessor<IReadonlyListable<Object, Class<?>>, IReadonlyListable<Object, Class<?>>>{
	protected final Logger log;
	protected IListableResource<Object, Object> defaultService;
	protected Map<String, IListableResource<Object, Object>> services;
	protected IReadonlyListable<Object, Class<?>> beanResource;
	protected IProcessor<String, IProcessor<?,?>> beanHolder;
	protected Boolean strict = true;
	protected Boolean refresh = false;
	protected Boolean overlap = false;
	protected String regex = "@";
	
	public AnnotationServiceFactory() {
		this.log = LoggerFactory.getLogger(AnnotationServiceFactory.class);
	}

	public AnnotationServiceFactory(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(AnnotationServiceFactory.class) : log;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	public void setRefresh(Boolean refresh) {
		if(refresh != null) this.refresh = refresh;
	}
	
	public void setStrict(Boolean strict) {
		if(strict != null) this.strict = strict;
	}

	public void setOverlap(Boolean overlap) {
		if(overlap != null) this.overlap = overlap;
	}
	
	public void setDefaultService(IListableResource<Object, Object> defaultService) {
		this.defaultService = defaultService;
	}

	public void setServices(Map<String, IListableResource<Object, Object>> services) {
		this.services = services;
	}

	public void setBeanResource(IReadonlyListable<Object, Class<?>> beanResource) {
		this.beanResource = beanResource;
	}
	
	public void setBeanHolder(IProcessor<String, IProcessor<?,?>> beanHolder) {
		this.beanHolder = beanHolder;
	}

	@Override
	public IReadonlyListable<Object, Class<?>> process(IReadonlyListable<Object, Class<?>> resource) {
		if(resource == null) resource = beanResource;
		if(resource != null && beanHolder != null && (services != null || defaultService != null)){
			HashMap<String, HashMap<String, Object>> newservices = new HashMap<String, HashMap<String, Object>>();
			HashMap<String, Object> defaults = defaultService == null ? null : new HashMap<String, Object>();
			for(Entry<Object[], Class<?>> entry : resource.all().entrySet()) if (entry.getValue() != null && entry.getKey() != null && entry.getKey().length > 0 && entry.getKey()[0] != null) try {
				Class<?> c = entry.getValue();
				String bean = entry.getKey()[0].toString();
				String filter = null;
				if (c.isAnnotationPresent(ServiceClass.class)) filter = c.getAnnotation(ServiceClass.class).filter();
				if (filter != null) filter = filter.trim();
				HashMap<String, Object> map = null;
				IListableResource<Object, Object> res = null;
				if (filter == null || filter.length() == 0) {
					map = defaults;
					res = defaultService;
				} else if (services != null && (res = services.get(filter)) != null) {
					map = newservices.get(filter);
					if (map == null) {
						map = new HashMap<String, Object>();
						newservices.put(filter, map);
					}
				} else if (!strict) {
					map = defaults;
					res = defaultService;
				}
				if(res != null && IProcessor.class.isAssignableFrom(c) && (overlap || res.find(bean) == null)) try{
					IProcessor processor = beanHolder.process(bean);
					if(processor != null) {
						Object[] sw = new Object[2];
						sw[0] = processor;
						sw[1] = c.getName();
						map.put(bean, sw);
					}
				}catch(Exception e){
					log.error("AnnotationServiceFactory: processor failed.", e);
				}
				if(c.isAnnotationPresent(ServiceClass.class)) for (Method me : c.getMethods()) if (me.isAnnotationPresent(ServiceMethod.class)) try {
					HashMap<String, Object> submap = map;
					IListableResource<Object, Object> subres = res;
					ServiceMethod sm = me.getAnnotation(ServiceMethod.class);
					String mfilter = sm.filter() == null ? null : sm.filter().trim();
					if (mfilter != null && mfilter.length() > 0 && !mfilter.equals(filter)) if (services != null && (subres = services.get(mfilter)) != null) {
						submap = newservices.get(mfilter);
						if (submap == null) {
							submap = new HashMap<String, Object>();
							newservices.put(mfilter, submap);
						}
					} else if (strict) subres = null;
					else {
						submap = defaults;
						subres = defaultService;
					}
					if (subres != null) {
						String methodName = sm.name();
						if(methodName == null || methodName.trim().length() == 0) methodName = me.getName();
						String name = bean + regex + methodName;
						if(overlap || subres.find(name) == null){
							IProcessor processor = beanHolder.process(name);
							if(processor != null) submap.put(name, processor);
						}
					}
				} catch(Exception e) {
					log.error("AnnotationServiceFactory: processor failed.", e);
				}
			} catch(Exception e){
				log.error("AnnotationServiceFactory: processor failed.", e);
			}
			for (Entry<String, HashMap<String, Object>> s : newservices.entrySet()) {
				IListableResource<Object, Object> service = services.get(s.getKey());
				if(refresh) service.empty();
				for(Entry<String, Object> entry : s.getValue().entrySet()) try{
					service.store(entry.getValue(), entry.getKey());
				} catch(Exception e){
					log.error("AnnotationServiceFactory: processor failed.", e);
				}
			}
			if (defaultService != null) {
				if(refresh) defaultService.empty();
				if(defaults.size() > 0) for(Entry<String, Object> entry : defaults.entrySet()) try{
					defaultService.store(entry.getValue(), entry.getKey());
				} catch(Exception e){
					log.error("AnnotationServiceFactory: processor failed.", e);
				}
			}
		}
		return resource;
	}
}
