package net.yeeyaa.eight.core.processor;

import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CloseProcessor implements IProcessor<Object, Object> {
	protected Logger log;
	protected volatile ConcurrentHashMap<Object, Object> closers = new ConcurrentHashMap<Object, Object>();
	protected Object paras; 
	
	public CloseProcessor() {
		this.log = LoggerFactory.getLogger(CloseProcessor.class);
	}
	
	public CloseProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CloseProcessor.class) : log;
	}
	
	public void setParas(Object paras) {
		this.paras = paras;
	}

	public void setClosers(ConcurrentHashMap<Object, Object> close) {
		if(close != null) this.closers = close;
	}

	@Override
	public Object process(Object instance) {
		if(instance == null) {
			ConcurrentHashMap<Object, Object> close = closers;
			closers = new ConcurrentHashMap<Object, Object>();
			for(Object closer : close.values()) close(closer);
		}else {
			Object closer = closers.remove(instance);
			if(closer != null) close(closer);
		}
		return instance;
	}

	public void destroy(){
		process(null);
	}
	
	protected void close(Object closer){
		try{
			Object paras = this.paras;
			if(closer instanceof IProcessor) ((IProcessor<Object, Void>) closer).process(paras);
			else if(closer instanceof Object[] && ((Object[])closer).length > 0 && ((Object[])closer)[0] instanceof IProcessor){
				if(((Object[])closer).length > 1) paras = ((Object[])closer)[1];
				((IProcessor<Object, Void>) ((Object[])closer)[0]).process(paras);
			}
		}catch(Exception e){
            log.error("CloseProcessor: close failed." + closer, e);	
		}
	}
	
	public void register(Object key, Object value) {
		if(key != null && value != null && !closers.contains(key)) closers.put(key, value);
	}
	
	public class Close implements IProcessor<Object, Object>{
		protected IProcessor<Object, IProcessor<Void, Void>> closer; 
		protected Object paras; 
		
		public void setParas(Object paras) {
			this.paras = paras;
		}

		public void setCloser(IProcessor<Object, IProcessor<Void, Void>> closer) {
			this.closer = closer;
		}

		@Override
		public Object process(Object instance) {
			if(instance != null) {
				IProcessor<Void, Void> process = closer.process(instance);
				if(process != null) register(instance, new Object[]{process, paras});
			}
			return instance;
		}
	}
}
