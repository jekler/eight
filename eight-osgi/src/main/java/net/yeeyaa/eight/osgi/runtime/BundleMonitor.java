package net.yeeyaa.eight.osgi.runtime;

import java.util.Collections;
import java.util.EventObject;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;

import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.FrameworkListener;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BundleMonitor implements IProcessor<Object[], Void>, BundleListener, FrameworkListener, ServiceListener{
	protected final Logger log;
	protected BundleContext context;
	protected Map<String, Map<Integer, Set<IProcessor<EventObject, Void>>>> bundleMap = new ConcurrentHashMap<String, Map<Integer, Set<IProcessor<EventObject, Void>>>>();
	protected Map<Object, Map<Integer, Set<IProcessor<EventObject, Void>>>> serviceMap = new ConcurrentHashMap<Object, Map<Integer, Set<IProcessor<EventObject, Void>>>>();	
	protected Map<String, Map<Integer, Set<IProcessor<EventObject, Void>>>> frameworkMap = new ConcurrentHashMap<String, Map<Integer, Set<IProcessor<EventObject, Void>>>>();
	protected Set<IProcessor<EventObject, Void>> bundleSet = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
	protected Set<IProcessor<EventObject, Void>> serviceSet = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
	protected Set<IProcessor<EventObject, Void>> frameworkSet = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
	protected Map<Integer, Set<IProcessor<EventObject, Void>>> bundleInt = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();
	protected Map<Integer, Set<IProcessor<EventObject, Void>>> serviceInt = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();
	protected Map<Integer, Set<IProcessor<EventObject, Void>>> frameworkInt = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();	
	protected Map<Object, Set<IProcessor<EventObject, Void>>> service = new ConcurrentHashMap<Object, Set<IProcessor<EventObject, Void>>>();	
	protected Map<String, Set<IProcessor<EventObject, Void>>> bundle = new ConcurrentHashMap<String, Set<IProcessor<EventObject, Void>>>();
	protected Map<String, Set<IProcessor<EventObject, Void>>> framework = new ConcurrentHashMap<String, Set<IProcessor<EventObject, Void>>>();
	protected String property = Constants.SERVICE_DESCRIPTION;
	protected String head = Constants.BUNDLE_SYMBOLICNAME;
	protected String locale;

	public BundleMonitor() {
		log = LoggerFactory.getLogger(BundleMonitor.class);
	}

	public BundleMonitor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BundleMonitor.class) : log;
	}
	
	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setProperty(String property) {
		if(property != null) this.property = property;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}

	public void setHead(String head) {
		if(head != null) this.head = head;
	}
	
	@PostConstruct
	public void initialize(){
		if(context != null) {
			context.addBundleListener(this);
			context.addFrameworkListener(this);
			context.addServiceListener(this);
		}
	}

	@PreDestroy
	public void destroy(){
		if(context != null) {
			context.removeBundleListener(this);
			context.removeFrameworkListener(this);
			context.removeServiceListener(this);
		}
	}
	
	@Override
	public Void process(Object[] instance) {
		if(instance != null && instance.length > 3 && instance[0] instanceof Integer && instance[3] instanceof IProcessor && (instance[1] instanceof Integer || instance[1] == null))switch((Integer) instance[0]){
			case 0: if(instance[1] == null) if(instance[2] == null) frameworkSet.add((IProcessor<EventObject, Void>) instance[3]);
					else {
						Set<IProcessor<EventObject, Void>> set = framework.get(instance[2].toString());
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							framework.put(instance[2].toString(), set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else if(instance[2] == null){
						Set<IProcessor<EventObject, Void>> set = frameworkInt.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							frameworkInt.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else{
						 Map<Integer, Set<IProcessor<EventObject, Void>>> map = frameworkMap.get(instance[2].toString());
						 if(map == null){
							 map = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();
							 frameworkMap.put(instance[2].toString(), map);
						 }
						Set<IProcessor<EventObject, Void>> set = map.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							map.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}
					break;
			case 1: if(instance[1] == null) if(instance[2] == null) bundleSet.add((IProcessor<EventObject, Void>) instance[3]);
					else {
						Set<IProcessor<EventObject, Void>> set = bundle.get(instance[2].toString());
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							bundle.put(instance[2].toString(), set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else if(instance[2] == null){
						Set<IProcessor<EventObject, Void>> set = bundleInt.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							bundleInt.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else{
						 Map<Integer, Set<IProcessor<EventObject, Void>>> map = bundleMap.get(instance[2].toString());
						 if(map == null){
							 map = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();
							 bundleMap.put(instance[2].toString(), map);
						 }
						Set<IProcessor<EventObject, Void>> set = map.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							map.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}
					break;
			default: if(instance[1] == null) if(instance[2] == null) serviceSet.add((IProcessor<EventObject, Void>) instance[3]);
					else {
						Set<IProcessor<EventObject, Void>> set = service.get(instance[2]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							service.put(instance[2], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else if(instance[2] == null){
						Set<IProcessor<EventObject, Void>> set = serviceInt.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							serviceInt.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}else{
						 Map<Integer, Set<IProcessor<EventObject, Void>>> map = serviceMap.get(instance[2]);
						 if(map == null){
							 map = new ConcurrentHashMap<Integer, Set<IProcessor<EventObject, Void>>>();
							 serviceMap.put(instance[2], map);
						 }
						Set<IProcessor<EventObject, Void>> set = map.get((Integer)instance[1]);
						if(set == null){
							set = Collections.newSetFromMap(new ConcurrentHashMap<IProcessor<EventObject, Void>, Boolean>());
							map.put((Integer)instance[1], set);
						}
						set.add((IProcessor<EventObject, Void>) instance[3]);
					}						
		}
		return null;
	}

	@Override
	public void serviceChanged(ServiceEvent event) {
		Integer type = event.getType();
		Object prop = event.getServiceReference().getProperty(property);
		if(serviceInt.containsKey(type)) for(IProcessor<EventObject, Void> processor : serviceInt.get(type)) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		for(IProcessor<EventObject, Void> processor : serviceSet) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		if(prop != null){
			if(service.containsKey(prop)) for(IProcessor<EventObject, Void> processor : service.get(prop)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
			if(serviceMap.containsKey(prop)) if(serviceMap.get(prop).containsKey(type)) for(IProcessor<EventObject, Void> processor : serviceMap.get(prop).get(type)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
		}
	}

	@Override
	public void frameworkEvent(FrameworkEvent event) {
		Integer type = event.getType();
		Object name = event.getBundle().getHeaders(locale).get(head);
		if(frameworkInt.containsKey(type)) for(IProcessor<EventObject, Void> processor : frameworkInt.get(type)) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		for(IProcessor<EventObject, Void> processor : frameworkSet) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		if(name != null){
			if(framework.containsKey(name)) for(IProcessor<EventObject, Void> processor : framework.get(name)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
			if(frameworkMap.containsKey(name)) if(frameworkMap.get(name).containsKey(type)) for(IProcessor<EventObject, Void> processor : frameworkMap.get(name).get(type)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
		}
	}

	@Override
	public void bundleChanged(BundleEvent event) {
		Integer type = event.getType();
		Object name = event.getBundle().getHeaders(locale).get(head);
		if(bundleInt.containsKey(type)) for(IProcessor<EventObject, Void> processor : bundleInt.get(type)) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		for(IProcessor<EventObject, Void> processor : bundleSet) try{
			processor.process(event);
		}catch(Exception e){
			log.error("BundleMonitor: event invokes fail.", e);
		}
		if(name != null){
			if(bundle.containsKey(name)) for(IProcessor<EventObject, Void> processor : bundle.get(name)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
			if(bundleMap.containsKey(name)) if(bundleMap.get(name).containsKey(type)) for(IProcessor<EventObject, Void> processor : bundleMap.get(name).get(type)) try{
				processor.process(event);
			}catch(Exception e){
				log.error("BundleMonitor: event invokes fail.", e);
			}
		}
	}
}
