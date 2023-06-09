package net.yeeyaa.eight.core.task.instance;

import java.util.concurrent.ExecutorService;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MultiTasker implements IProcessor<String, String> {
	protected final Logger log;
	protected String regex="~";
	protected Integer threadMode = 0;
	protected Integer threadCount = 5;
	protected ExecutorService executor;
	protected String pool;	
	protected IProcessor<Object, Object> beanHolder;
	
	public MultiTasker() {
		this.log = LoggerFactory.getLogger(MultiTasker.class);
	}
	
	public MultiTasker(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MultiTasker.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
		
	public void setPool(String pool) {
		this.pool = pool;
	}
	
	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
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
		protected IProcessor<String, Object> task;
		protected String paras;

		protected SubRunner(IProcessor<String, Object> task, String paras) {
			this.task = task;
			this.paras = paras;
		}
		
		@Override
		public void run() {
			task.process(paras);
		}
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
			for(String para : paras.split(regex+regex)) try{
				String[] kv = para.split(regex);
				if(kv.length > 1){
					Object o = beanHolder.process(kv[0]);
					if(IProcessor.class.isInstance(o)){
						SubRunner sr = new SubRunner((IProcessor) o, kv[1]);
						if(exec != null && !exec.isShutdown()) exec.execute(sr);
						else sr.run();
					}
				}
			}catch(Exception e){
				log.error("MultiTasker: task failed.", e);
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
