package net.yeeyaa.eight.core.processor;

import java.util.List;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DecisionXProcessor<T, R> implements IProcessor<T, R>{
	protected final Logger log;
	protected List<IProcessor<T, Boolean>> prePorecessors;
	protected IProcessor<T, R> next;
	protected List<IProcessor<R, Boolean>> postPorecessors;
	protected Boolean ignoreError = false;;	
	protected Boolean nullable = false;
	
	public DecisionXProcessor() {
		this.log = LoggerFactory.getLogger(DecisionXProcessor.class);
	}
	
	public DecisionXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(DecisionXProcessor.class) : log;
	}
	
	public void setIgnoreError(Boolean ignoreError) {
		if(ignoreError != null) this.ignoreError = ignoreError;
	}
	
	public void setNext(IProcessor<T, R> next) {
		this.next = next;
	}

	public void setNullable(Boolean nullable) {
		if(nullable != null) this.nullable = nullable;
	}

	public void setPrePorecessors(List<IProcessor<T, Boolean>> prePorecessors) {
		this.prePorecessors = prePorecessors;
	}

	public void setPostPorecessors(List<IProcessor<R, Boolean>> postPorecessors) {
		this.postPorecessors = postPorecessors;
	}

	@Override
	public R process(T in) {	
		Boolean decision = true;
		R ret = null;
		if(prePorecessors != null) for(IProcessor<T, Boolean> processor : prePorecessors)  if(decision) try{
			decision = processor.process(in);
		}catch(Exception e){
			log.error("DecisionXProcessor: processor failed.", e);
			if(!ignoreError)  if (e instanceof PlatformException) throw (PlatformException) e;
			else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
		}
		if(decision) ret = next.process(in);
		if(postPorecessors != null && (ret != null || nullable) && decision) for(IProcessor<R, Boolean> processor : postPorecessors)  if(decision) try{
			decision = processor.process(ret);
		}catch(Exception e){
			log.error("DecisionXProcessor: processor failed.", e);
			if(!ignoreError)  if (e instanceof PlatformException) throw (PlatformException) e;
			else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
		}
		if(decision) return ret;
		else return null;
	}
}
