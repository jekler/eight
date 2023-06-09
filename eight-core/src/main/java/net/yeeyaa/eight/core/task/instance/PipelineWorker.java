package net.yeeyaa.eight.core.task.instance;

import java.util.concurrent.ExecutorService;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;
import net.yeeyaa.eight.core.pipeline.Pipeline;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PipelineWorker implements IProcessor<String, String> {
	protected final Logger log;
	protected String regex="\\|";
	protected Integer threadMode = 0;
	protected Integer threadCount = 3;
	protected ExecutorService executor;
	protected String pool;	
	protected IProcessor<Object, Object> beanHolder;
	
	public PipelineWorker() {
		this.log = LoggerFactory.getLogger(PipelineWorker.class);
	}
	
	public PipelineWorker(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PipelineWorker.class) : log;
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
			switch (threadMode)  { 
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
				String resource =null;
				String[] newparas = new String[0];
				Pipeline pipeline = null;
				for (String subpara : para.split(regex+regex)){
					String[] kv = subpara.split(regex);
					if(kv.length > 1){
						if(kv[0].trim().equals(Pipeline.RESOURCE)) {
							resource = kv[1];
						}else if(kv[0].trim().equals(Pipeline.PARAS)){
							newparas = new String[kv.length - 1];
							for(int i = 0; i < kv.length - 1; i++) newparas[i] = kv[i + 1];
						}
					}else if(pipeline == null){
						Object o = beanHolder.process(subpara);
						if(Pipeline.class.isInstance(o)) pipeline= (Pipeline) o;
					}
				}
				if(resource != null){
					if(pipeline == null) pipeline = new Pipeline(beanHolder);
					pipeline.setConfig(resource, newparas);
				}
				if(pipeline != null) if(exec != null && !exec.isShutdown()) exec.submit(pipeline);
				else pipeline.call();
			}catch(Exception e){
				log.error("PipelineWorker: task failed.", e);
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
