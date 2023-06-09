package net.yeeyaa.eight.osgi.runtime;

import java.util.Dictionary;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;


public class BundleService<T> implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object> {
	protected BundleContext context;
	protected Dictionary<String, Object> properties;
	protected Object service;
	protected String[] interfaces;
	protected volatile ServiceRegistration registration;
	
	public BundleService(BundleContext context,	Dictionary<String, Object> properties, Object service,	String[] interfaces) {
		this.context = context;
		this.properties = properties;
		this.service = service;
		this.interfaces = interfaces;
	}

	public BundleService() {}

	public void setService(Object service) {
		this.service = service;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setProperties(Dictionary<String, Object> properties) {
		this.properties = properties;
	}

	public void setInterfaces(String[] interfaces) {
		this.interfaces = interfaces;
	}

	@PostConstruct
	public void initialize(){
		if (service != null) registration = context.registerService(interfaces, service, properties);
	}

	@PreDestroy
	public void destroy(){
		if (registration != null) registration.unregister();
	}

	@Override
	public Object process(Object instance) {
		try {
			if (instance instanceof Class) return context.getServiceReference((Class)instance);
			else if (instance instanceof String) return context.getServiceReference((String)instance);
			else if (instance instanceof Object[] && ((Object[])instance).length > 1 && ((Object[])instance)[0] != null) 
				return context.getAllServiceReferences(((Object[])instance)[0].toString(), (String) ((Object[])instance)[1]);
		} catch (Exception e) {
			throw new PlatformException(BundleError.SERVICE_CANNOT_FIND, e);
		}
		return null;
	}

	@Override
	public Object perform(Object first, Object second) {
		try {
			if (first instanceof ServiceReference) if (Boolean.FALSE.equals(second)) return context.ungetService((ServiceReference) first);
			else return context.getService((ServiceReference) first);
			if (first instanceof Class) return context.getServiceReferences((Class) first, (String) second);
			else if (first != null) return context.getServiceReferences(first.toString(), (String) second);
		} catch (Exception e) {
			throw new PlatformException(BundleError.SERVICE_CANNOT_FIND, e);
		}
		return null;
	}
}
