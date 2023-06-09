package net.yeeyaa.eight.core.pipeline.common;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MultiNode implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="~";
	protected Integer threadMode = 0;
	protected ExecutorService executor;
	protected IProcessor<Object, Object> beanHolder;
	
	public MultiNode() {
		this.log = LoggerFactory.getLogger(MultiNode.class);
	}

	public MultiNode(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MultiNode.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setThreadMode(Integer threadMode) {
		if(threadMode != null) this.threadMode = threadMode;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	protected static class SubRunner implements Callable<Void>{
		IProcessor<Map<String, String>, Void> node;
		HashMap<String, String> paras;

		protected SubRunner(IProcessor<Map<String, String>, Void> node, HashMap<String, String> paras) {
			this.node = node;
			this.paras = paras;
		}

		@Override
		public Void call() throws Exception {
			node.process(paras);
			return null;
		}
	}
	
	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	@PreDestroy
	public void destroy() {
		if (executor != null) executor.shutdown();
	}
	
	@Override
	public Void process(Map<String, String> paras) {
		try{
			String nodeRegex = paras.get("regex");
			if(nodeRegex == null) nodeRegex = regex;
			String[] inputset = new String[0];
			if(paras.get("input") != null) inputset = paras.get("input").split(regex);
			if(inputset.length > 0){
				Object in = beanHolder.process(inputset[0]);
				if(IInputResource.class.isInstance(in)) {
					String[] para = new String[inputset.length - 1];
					for(int i = 0; i < para.length; i++) para[i] = inputset[i + 1];
					Object o = ((IInputResource)in).find(para);	
					if(o != null) {
						Boolean wait = "true".equals(paras.get("wait"));
						Boolean multi = "true".equals(paras.get("multithread"));
						Boolean newExecutor = false;
						ExecutorService exec = null;
						switch (threadMode) { 
							case 1: if(paras.get("pool") != null) {
									Object obj = beanHolder.process(paras.get("pool"));
									if(obj instanceof ExecutorService) exec = (ExecutorService)obj;
								}
								break;
							case 2: exec = executor;
								break;
							case 3: if (multi) {
									Integer count = 0;
									try{
										count = new Integer((String)paras.get("threadcount"));
									}catch(Exception e){
										log.error(paras + ": init parameters error:", e);						
									}
									if (count > 0) exec = PlatformPool.getPool(count);
									else  exec = PlatformPool.getCachedPool();
									newExecutor = true;
								}
						}
						LinkedList<Future<Void>> flist = new LinkedList<Future<Void>>();
						for(String p : o.toString().split(nodeRegex+nodeRegex+nodeRegex)) try{
							HashMap<String, String> newparas = new HashMap<String, String>();
							IProcessor<Map<String, String>, Void> node = null;
							for (String subpara : p.split(nodeRegex+nodeRegex)){
								String[] kv = subpara.split(nodeRegex);
								if(kv.length > 1) newparas.put(kv[0], kv[1]);
								else if(node == null){
									Object bean = beanHolder.process(subpara);
									if(IProcessor.class.isInstance(bean)) node = (IProcessor) bean;
								}
							}
							if(node != null) if(exec != null && !exec.isShutdown()) flist.add(exec.submit(new SubRunner(node, newparas)));
								else node.process(newparas);
						}catch(Exception e){
							log.error("MultiNode: performing error.", e);
						}
						if(newExecutor) exec.shutdown();
						if(wait) for(Future<Void> f : flist) f.get();
					}
				}
			}	
		}catch(Exception e){
			log.error("MultiNode: performing error.", e);
		}
		return null;
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		public Object process(Object in) {
			destroy();
			return in;
		}
	}
}
