package net.yeeyaa.eight.osgi.loader;

import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.osgi.loader.config.ConfigInstaller;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ConfigProcessor implements IProcessor<IExtendable<Object>, IExtendable<Object>> {
	protected final Logger log;
	protected BundleContext context;
	protected Boolean write = false; 
	protected String name = "org.osgi.service.cm.ConfigurationAdmin";
	protected ConfigTracker<Object, Object> tracker;
	protected String region;
	protected String version;
	
	protected class ConfigTracker<S, T> extends ServiceTracker<S, T>{
		protected final ConcurrentHashMap<Long, ConfigInstaller> configInstallers = new ConcurrentHashMap<Long, ConfigInstaller>();

		protected ConfigTracker(BundleContext context, String name) {
			super(context, name, null);
		}

		@Override
		public T addingService(ServiceReference<S> reference) {
			T cm = super.addingService(reference);
	        try{
		        Long id = (Long) reference.getProperty(Constants.SERVICE_ID);
		        if(!configInstallers.contains(id)) synchronized(this) {
		        	if(!configInstallers.contains(id)) configInstallers.put(id, new ConfigInstaller(context, cm, write, region, version, log));
		        }
	        }catch(Exception e){
				log.error("ConfigTracker: add service fail.", e);
	        }
	        return cm;
		}
		
		@Override
		public void removedService(ServiceReference<S> reference, T service) {
			try{
		        Long id = (Long) reference.getProperty(Constants.SERVICE_ID);
		        ConfigInstaller configInstaller = configInstallers.remove(id);
		        if (configInstaller != null) configInstaller.destroy();
			}catch(Exception e){
				log.error("ConfigTracker: remove service fail.", e);
			}
			super.removedService(reference, service);
		}
		
		@Override
		public void close() {
			try{
				for(ConfigInstaller configInstaller : configInstallers.values()) configInstaller.destroy();
			}catch(Exception e){
				log.error("ConfigTracker: remove service fail.", e);
			}
			super.close();
		}

		protected void setConfig(IExtendable<Object> storage){
			for(ConfigInstaller installer : configInstallers.values()) try{
				installer.setConfig(storage);
			}catch(Exception e){
				log.error("ConfigTracker: set config fail.", e);
			}
		}
	}

	public ConfigProcessor() {
		log = LoggerFactory.getLogger(ConfigProcessor.class);
	}

	public ConfigProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ConfigProcessor.class) : log;
	}
	
	public void setVersion(String version) {
		this.version = version;
	}

	public void setRegion(String region) {
		this.region = region;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setName(String name) {
		if(name != null) this.name = name;
	}

	public void setWrite(Boolean write) {
		if(write != null) this.write = write;
	}

	@PostConstruct
	public void initialize(){
		try{
			tracker = new ConfigTracker<Object, Object>(context, name);
			tracker.open();
		}catch(Exception e){
			log.error("ConfigProcessor: initialize fail.", e);
		}
	}

	@PreDestroy
	public void destroy(){
		if(tracker != null) try{
			tracker.close();
			tracker = null;
		}catch(Exception e){
			log.error("ConfigProcessor: destory fail.", e);
		}
	}
	
	@Override
	public IExtendable<Object> process(IExtendable<Object> storage) {
		if(storage != null && tracker != null) tracker.setConfig(storage);
		return storage;
	}
}
