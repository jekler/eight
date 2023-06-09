package net.yeeyaa.eight.osgi.runtime;

import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.concurrent.CountDownLatch;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;

import org.osgi.framework.BundleContext;
import org.osgi.framework.launch.Framework;
import org.osgi.framework.launch.FrameworkFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BundleFramework implements IProcessor<Object, BundleContext>{
	protected final Logger log;
	protected volatile CountDownLatch startSignal = new CountDownLatch(1);
	protected volatile Framework framework;
	protected FrameworkFactory factory;
	protected ClassLoader classLoader;
	protected Map<String, String> configuration;
	protected IProcessor<Framework, Void> afterInit;
	
	public BundleFramework() {
		log = LoggerFactory.getLogger(BundleFramework.class);
	}

	public BundleFramework(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BundleFramework.class) : log;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setAfterInit(IProcessor<Framework, Void> afterInit) {
		this.afterInit = afterInit;
	}

	public void setFactory(FrameworkFactory factory) {
		this.factory = factory;
	}
	
	public void setConfiguration(Map<String, String> configuration) {
		this.configuration = configuration;
	}

	@PostConstruct
	public void initialize(){
		try{
			if(factory == null) {
				Iterator<FrameworkFactory> itr;
				if(classLoader != null) itr = ServiceLoader.load(FrameworkFactory.class, classLoader).iterator();
				else itr = ServiceLoader.load(FrameworkFactory.class).iterator();
				if(itr.hasNext()) factory = itr.next();
			}
			if(factory != null) {
				Framework framework = factory.newFramework(configuration);
				framework.init();
				if(afterInit != null) afterInit.process(framework);
				framework.start();
				this.framework = framework;
			}
		}catch(Exception e){
			log.error("FrameworkBean: init fail.", e);
		}finally{
			startSignal.countDown();
		}
	}
	
	@Override
	public BundleContext process(Object instance) {
		try{
			startSignal.wait();
			if(framework == null) return null;
			else return framework.getBundleContext();
		}catch(Exception e){
			log.error("FrameworkBean: performing fail.", e);
			throw new PlatformException(BundleError.SERVICE_INVOKE_ERROR, e);
		}
	}
	
	@PreDestroy
    public void destroy(){
    	if(framework != null) try{
    		if(startSignal.getCount() == 0) startSignal = new CountDownLatch(1);
    		framework.stop();
    		framework = null;
    	}catch(Exception e){
			log.error("FrameworkBean: destroy fail.", e);
    	}
    }
}
