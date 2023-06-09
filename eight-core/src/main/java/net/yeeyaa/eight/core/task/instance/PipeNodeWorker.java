package net.yeeyaa.eight.core.task.instance;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PipeNodeWorker implements IProcessor<String, String> {
	protected final Logger log;
	protected String regex="~";
	protected Integer threadMode = 0;
	protected Integer threadCount = 3;
	protected ExecutorService executor;
	protected String pool;	
	protected IProcessor<Object, Object> beanHolder;

	public PipeNodeWorker() {
		this.log = LoggerFactory.getLogger(PipeNodeWorker.class);
	}
	
	public PipeNodeWorker(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PipeNodeWorker.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
		
	public void setPool(String pool) {
		this.pool = pool;
	}
	
	public void setThreadCount(Integer threadCount) {
		if(threadCount != null && threadCount >= 0) this.threadCount = threadCount;
	}	
	
	public void setThreadMode(Integer threadMode) {
		if(threadMode != null) this.threadMode = threadMode;
	}	
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	protected class SubRunner implements Runnable{
		IProcessor<Map<String, String>, Void> node;
		HashMap<String, String> paras;

		protected SubRunner(IProcessor<Map<String, String>, Void> node, HashMap<String, String> paras) {
			this.node = node;
			this.paras = paras;
		}

		@Override
		public void run() {
			node.process(paras);
		}
	}
	
	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	@PreDestroy
	public void destroy(){
		if (executor != null) executor.shutdown();
	}
	
	@Override
	public String process(String paras) {
		if(paras != null && paras.length() > 0){
			Boolean newExecutor = false;
			ExecutorService exec = null;
			switch (threadMode) { 
				case 1: if(pool != null) {
						Object obj = beanHolder.process(pool);
						if(obj instanceof ExecutorService) exec = (ExecutorService)obj;
					}
					break;
				case 2: exec = executor;
					break;
				case 3: if (threadCount > 0) exec = PlatformPool.getPool(threadCount);
					else  exec = PlatformPool.getCachedPool();
					newExecutor = true;
			}
			for(String para : paras.split(regex+regex+regex)) try{
				HashMap<String, String> newparas = new HashMap<String, String>();
				IProcessor<Map<String, String>, Void> node = null;
				for (String subpara : para.split(regex+regex)){
					String[] kv = subpara.split(regex);
					if(kv.length > 1) newparas.put(kv[0], kv[1]);
					else if(node == null){
						Object o = beanHolder.process(subpara);
						if(IProcessor.class.isInstance(o)) node = (IProcessor<Map<String, String>, Void>) o;
					}
				}
				if(node != null) if(executor != null && !executor.isShutdown()) executor.execute(new SubRunner(node, newparas));
				else new SubRunner(node, newparas).run();
			}catch(Exception e){
				log.error("PipeNodeWorker: task failed.", e);
			}
			if(newExecutor) exec.shutdown();
		}	
		return paras;
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}
}
