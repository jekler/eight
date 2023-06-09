package net.yeeyaa.eight.core.processor;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class VoidProcessor<T, R> implements IProcessor<T, R> {
	protected final Logger log;
	protected T paras;
	protected IProcessor<T, R> processor;
	protected Boolean stub;
	protected Object thread;
	protected class Run implements Runnable {
		protected T paras;
		
		public Run(T paras) {
			this.paras = paras;
		}

		@Override
		public void run() {
			processor.process(paras);
	}};
	protected class Call implements Callable<R> {
		protected T paras;
		
		public Call(T paras) {
			this.paras = paras;
		}

		@Override
		public R call() throws Exception {
			return processor.process(paras);
	}};

	public VoidProcessor() {
		this.log = LoggerFactory.getLogger(VoidProcessor.class);
	}
	
	public VoidProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(VoidProcessor.class) : log;
	}
	
	public void setThread(Object thread) {
		this.thread = thread;
	}

	public void setStub(Boolean stub) {
		this.stub = stub;
	}

	public void setParas(T paras) {
		this.paras = paras;
	}

	public void setProcessor(IProcessor<T, R> processor) {
		this.processor = processor;
	}

	@Override
	public R process(T instance) {
		if(processor == null) if(Boolean.TRUE.equals(stub)) return (R)instance; 
		else return null;
		else if(thread != null) if (thread instanceof ExecutorService) {
			return (R) ((ExecutorService)thread).submit(new Call(paras == null ? instance : paras));
		} else {
			new Thread(new Run(paras == null ? instance : paras)).start();
			return null;
		} else if(stub == null) return processor.process(paras);
		else try{
			return processor.process(paras);
		} catch(Exception e){
       	 	log.error("VoidProcessor: performing error", e);
			if(stub) return (R)instance; 
			else return null;
		}
	}
	
	public void initialize(){
		if(thread!= null) if (thread instanceof ExecutorService) {
			((ExecutorService)thread).submit(new Run(paras));
		} else new Thread(new Run(paras)).start();
		else if(Boolean.TRUE.equals(stub)) try{
			processor.process(paras);
		}catch(Exception e){
       	 	log.error("VoidProcessor: performing error", e);
		}else processor.process(paras);
	}
}
