package net.yeeyaa.eight.core.util;

import java.net.URL;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;


public class BeanURLStreamHandlerFactory implements URLStreamHandlerFactory, IListableResource<Object, Object>, IProcessor<Object, URLStreamHandler> {
	protected final static ConcurrentHashMap<String, Object> registration = new ConcurrentHashMap<String, Object>();
	protected static BeanURLStreamHandlerFactory factory;
	protected IProcessor<Object, Object> beanHolder;
	protected URLStreamHandlerFactory proxy;
	protected IProcessor<String, String> convertor;
	
	public BeanURLStreamHandlerFactory(){}
	
	public BeanURLStreamHandlerFactory(URLStreamHandlerFactory proxy) {
		this.proxy = proxy;
	}

	public void initialize() {
		setURLStreamHandlerFactory(this);
	}
	
	public void destroy() {
		removeURLStreamHandlerFactory(this);
	}

	public void setConvertor(IProcessor<String, String> convertor) {
		this.convertor = convertor;
	}

	public void setProxy(URLStreamHandlerFactory proxy) {
		this.proxy = proxy;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
    
	@Override
	public URLStreamHandler createURLStreamHandler(String protocol) {
		if (convertor != null) protocol = convertor.process(protocol);
		if (proxy == null) if (beanHolder == null) {
			Object o = registration.get(protocol);
			if (o instanceof URLStreamHandler) return (URLStreamHandler) o;
			else if (o instanceof IProcessor) {
				Object so = ((IProcessor)o).process(protocol);
				if (so instanceof URLStreamHandler) return (URLStreamHandler) so;
			}
		} else {
			Object o = beanHolder.process(protocol);
			if (o instanceof URLStreamHandler) return (URLStreamHandler) o;
			else if (o instanceof IProcessor) {
				Object so = ((IProcessor)o).process(protocol);
				if (so instanceof URLStreamHandler) return (URLStreamHandler) so;
			}
		} else return proxy.createURLStreamHandler(protocol);
		return null;
	}

	@Override
	public URLStreamHandler process(Object instance) {
		return createURLStreamHandler(instance == null ? null : instance.toString());
	}
	
	@Override
	public Object find(Object... paras) {
		return registration.get(paras[0]);
	}

	@Override
	public <P> P store(Object value, Object... paras) {
		registration.put((String)paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(Object... paras) {
		registration.remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(Object... paras) {
		registration.clear();
		return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		ArrayList<Object[]> ret = new ArrayList<Object[]> (registration.size());
		for (String key : registration.keySet()) ret.add(new String[]{key});
		return ret;
	}

	@Override
	public Map<Object[], Object> all(Object... paras) {
		HashMap<Object[], Object> ret = new HashMap<Object[], Object> (registration.size() * 2);
		for (Entry<String, Object> entry : registration.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
		return ret;
	}
	
	public static synchronized void setURLStreamHandlerFactory(URLStreamHandlerFactory factory) {
		if (BeanURLStreamHandlerFactory.factory == null) {
			BeanURLStreamHandlerFactory parent = factory == null ? new BeanURLStreamHandlerFactory() : new BeanURLStreamHandlerFactory(factory);
			URL.setURLStreamHandlerFactory(parent);
			BeanURLStreamHandlerFactory.factory = parent;
		} else BeanURLStreamHandlerFactory.factory.setProxy(factory);
	}
	
	public static synchronized URLStreamHandlerFactory removeURLStreamHandlerFactory(URLStreamHandlerFactory factory){
		if (BeanURLStreamHandlerFactory.factory == null) return null;
		else if (factory == null) return BeanURLStreamHandlerFactory.factory.proxy;
		else if (factory.equals(BeanURLStreamHandlerFactory.factory.proxy)) {
			factory = BeanURLStreamHandlerFactory.factory.proxy;
			BeanURLStreamHandlerFactory.factory.proxy = null;
			return factory;
		} else return null;
	}
	
	public static void register(String protocol, Object handler) {
		registration.put(protocol, handler);
	}
	
	public static Object unregister(String protocol) {
		return registration.remove(protocol);
	}
	
	public static void clear() {
		registration.clear();
	}
}
