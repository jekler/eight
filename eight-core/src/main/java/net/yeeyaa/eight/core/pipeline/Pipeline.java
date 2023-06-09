package net.yeeyaa.eight.core.pipeline;

import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;
import net.yeeyaa.eight.core.event.PlatformEvent;
import net.yeeyaa.eight.core.pipeline.PipeEntity.NodeEntity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class Pipeline implements IProcessor<Map<String, String>, Void>, Callable<Void> {
	protected final Logger log;
	public static final String START = "start";
	public static final String RESOURCE = "resource";
	public static final String PARAS = "paras";
	public static final String REGEX = "regex";
	protected PipeEntity configuration;
	protected InheritableThreadLocal<PipeEntity> config = new InheritableThreadLocal<PipeEntity>(){
		@Override
		protected PipeEntity initialValue() {
			return configuration;
		}
	};
	protected IInputResource<Object, PipeEntity> resource;
	protected IProcessor<Object, Object> beanHolder;
	protected Object[] paras;
	protected ExecutorService executor;
	
	protected class PipeRunner implements Callable<Void>{
		protected NodeEntity node;
		
		protected PipeRunner(NodeEntity node) {
			this.node = node;
		}

		@Override
		public Void call() {
			try {
				if (node.bean != null){
					Object o = beanHolder.process(node.bean);
					if (IProcessor.class.isInstance(o)) ((IProcessor) o).process(node.paras);
				}
				Integer count = 3;
				Integer mode = 0;
				if(node.threadCount != null) count = node.threadCount;
				if(node.threadMode != null) mode = node.threadMode;
				Boolean newExecutor = false;
				ExecutorService exec = null;
				if(mode != 0){
					exec = executor;
					if(mode == 1)  { 
						if(node.pool != null) {
							Object o = beanHolder.process(node.pool);
							if(o instanceof ExecutorService) exec = (ExecutorService)o;
						}
					} else if(executor == null || executor.isShutdown()) {
						if (count > 0) exec = PlatformPool.getPool(count);
						else  exec = PlatformPool.getCachedPool();
						newExecutor = true;
					}
				}
				if(exec != null && !exec.isShutdown()){
					LinkedList<Future<Void>> flist = new LinkedList<Future<Void>>();
					if(node.linktos != null && node.linktos.size() > 0) for (String linkto : node.linktos) {
						NodeEntity next = config.get().nodes.get(linkto);
						if(next != null) flist.add(exec.submit(new PipeRunner(next)));
					}
					if (newExecutor) exec.shutdown();
					if(node.wait) for(Future<Void> f : flist) f.get(); 
				}else if(node.linktos != null && node.linktos.size() > 0) for (String linkto : node.linktos) try{
					NodeEntity next = config.get().nodes.get(linkto);
					if(next != null)  new PipeRunner(next).call();
				}catch(Exception e){
					log.error(linkto + ": pipeline performing error:", e);						
				}
			} catch (Exception e) {
				log.error(node.name + ": pipeline performing error:", e);
			}
			return null;
		}
	}

	public Pipeline(IProcessor<Object, Object> beanHolder, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformEvent.class) : log;
		this.beanHolder = beanHolder;
	}
	
	public Pipeline(IProcessor<Object, Object> beanHolder) {
		this.log = LoggerFactory.getLogger(PlatformEvent.class);
		this.beanHolder = beanHolder;
	}

	@PreDestroy
	public void destroy() {
		if (executor != null) executor.shutdown();
	}

	public void setResource(IInputResource<Object, PipeEntity> resource) {
		this.resource = resource;
	}

	public void setParas(Object[] paras) {
		this.paras = paras;
	}

	public void setConfiguration(PipeEntity configuration){
		if(configuration != null) this.configuration = configuration;
	}
	
	public PipeEntity getConfig() {
		return config.get();
	}

	public void setConfig(PipeEntity config){
		if(config != null) this.config.set(config);
	}
	
	public void setConfig(String resource, Object ... paras) {
		Object r = beanHolder.process(resource);
		if(IInputResource.class.isInstance(r)) config.set(((IInputResource<Object, PipeEntity>)r).find(paras));
	}

	public void setConfig(IInputResource<Object, PipeEntity> resource, Object ... paras) {
		if(resource != null) config.set(resource.find(paras));
	}
	
	public void setConfig(){
		if(resource != null) {
			configuration = resource.find(paras);
			config.set(configuration);
		}
	}

	@Override
	public Void call() {
		PipeEntity config = this.config.get();
		try{
			Integer count = 3;
			Integer mode = 0;
			if(config.threadCount != null) count = config.threadCount;
			if(config.threadMode != null) mode = config.threadMode;
			if(mode != 0){
				ExecutorService exec = null;
				if(mode == 1) { 
					if(config.pool != null) {
						Object o = beanHolder.process(config.pool);
						if(o instanceof ExecutorService) exec = (ExecutorService)o;
					}
				} 
				if(exec == null || exec.isShutdown()) exec = executor;
				if(exec == null || exec.isShutdown()) synchronized(this){
					if(executor == null || executor.isShutdown()) if (count > 0) executor = PlatformPool.getPool(count);
					else executor = PlatformPool.getCachedPool();
					exec = executor;
				}
				if(config.nodes != null && config.nodes.get(START) != null){
					Future<Void> f = exec.submit(new PipeRunner(config.nodes.get(START)));
			        if(config.wait) f.get();
				}
			} else if(config.nodes != null && config.nodes.get(START) != null) new PipeRunner(config.nodes.get(START)).call();
		}catch(Exception e){
			log.error("start: pipeline performing error:", e);			
		}
		return null;
	}
	
	@Override
	public Void process(Map<String, String> paras) {
		String resource = paras.get(RESOURCE);
		String regex = paras.get(REGEX);
		String newparas = paras.get(PARAS);
		if(resource != null) if(newparas != null) if(regex != null) setConfig(resource, newparas.split(regex));
		else setConfig(resource, newparas);
		else setConfig(resource);
		if(config.get() != null) call();
		return null;
	}

	public class Run implements IProcessor<Object, Object>{
		public Object process(Object in) {
			if(getConfig() == null) setConfig();
			if(getConfig() != null) call();
			return in;
		}
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}
}
