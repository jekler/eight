package net.yeeyaa.eight.core.task.instance;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class AutoExecutor implements IProcessor<String, String> {
	protected final Logger log;
	protected String regex="\\|";
	protected Integer threadMode = 0;
	protected Integer threadCount = 5;
	protected ExecutorService executor;
	protected String pool;	
	protected IProcessor<Object, Object> beanHolder;
	
	public AutoExecutor() {
		this.log = LoggerFactory.getLogger(AutoExecutor.class);
	}
	
	public AutoExecutor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(AutoExecutor.class) : log;
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
	
	protected class ExeCom implements Runnable {
		protected String[] command;
		protected IProcessor<Object, Void> processor;
		protected String charset = "UTF-8";
		protected IProcessor<Object, Void> errorProcessor;
        
        protected ExeCom(String[] command, String charset, IProcessor<Object, Void> processor, IProcessor<Object, Void> errorProcessor) {   
        	this.command = command;   
        	if(charset != null) this.charset = charset;
        	this.processor = processor;
        	this.errorProcessor = errorProcessor;
        }
        
        public void run() {
        	try {
        		Process process=Runtime.getRuntime().exec(command);
        		BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream(), charset));  
        		StringBuilder sb = new StringBuilder();
        		String line = null;   
        		while ((line = br.readLine()) != null) {
        			sb.append(line);
        			sb.append(System.getProperty("line.separator"));
        		}
        		br.close();
        		if(processor != null) processor.process(sb.toString());
        		br = new BufferedReader(new InputStreamReader(process.getErrorStream(), charset));  
        		sb = new StringBuilder();
        		line = null;   
        		while ((line = br.readLine()) != null) {
        			sb.append(line);
        			sb.append(System.getProperty("line.separator"));
        		}
        		br.close();
        		if(errorProcessor != null) errorProcessor.process(sb.toString());
        		process.waitFor();
        	} catch (Exception e) {   
        		log.error("AutoExecutor: Run command failed.", e);
        	}   
        }   
    }
	
	@Override
	public String process(String paras) {
		if(paras != null && paras.length() > 0){
			Boolean newExecutor = false;
			ExecutorService exec = null;
			switch(threadMode) { 
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
			for(String para : paras.split(regex+regex+regex))try{
				String[] command = null;
				String charset = null;
				IProcessor<Object, Void> processor = null;
				IProcessor<Object, Void> errorProcessor = null; 
				for(String subpara : para.split(regex+regex)) {
					String[] endpara = subpara.split(regex);
					if(endpara.length > 1) if("charset".equals(endpara[0])) charset = endpara[1];
					else if("command".equals(endpara[0])) {
						command = new String[endpara.length - 1];
						for(int i = 0; i < endpara.length - 1; i++) command[i] = endpara[i + 1];
					} else if("processor".equals(endpara[0])) {
						Object o = beanHolder.process(endpara[1]);
						if(IProcessor.class.isInstance(o)) processor = (IProcessor) o;
					} else if("errorprocessor".equals(endpara[0])) {
						Object o = beanHolder.process(endpara[1]);
						if(IProcessor.class.isInstance(o)) errorProcessor = (IProcessor) o;
					}			
				}
				if(command != null){
					ExeCom exe = new ExeCom(command, charset, processor, errorProcessor);
		            if(exec != null && !exec.isShutdown()) exec.execute(exe);
		            else exe.run();
	            }
	        } catch (Exception e) {   
	            log.error("AutoExecutor: task failed: "+paras, e);            
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
