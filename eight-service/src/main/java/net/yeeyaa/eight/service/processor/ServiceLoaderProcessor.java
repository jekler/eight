package net.yeeyaa.eight.service.processor;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ServiceLoaderProcessor<K, T, R> implements IProcessor<Object, Object> {
	protected final Logger log;
	protected Map<IReadonlyListable<K, Entry<String, IProcessor<T, R>>>, K[]> inputs;
	protected Boolean clear = true;;	
	protected IOutputResource<String, IProcessor<T, R>> output;

	public ServiceLoaderProcessor() {
		log = LoggerFactory.getLogger(ServiceLoaderProcessor.class);
	}

	public ServiceLoaderProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ServiceLoaderProcessor.class) : log;
	}
	
	public void setInputs(Map<IReadonlyListable<K, Entry<String, IProcessor<T, R>>>, K[]> inputs) {
		this.inputs = inputs;
	}

	public void setClear(Boolean clear) {
		if(clear != null) this.clear = clear;
	}

	public void setOutput(IOutputResource<String, IProcessor<T, R>> output) {
		this.output = output;
	}

	@Override
	public Object process(Object in) {	
		Map<String, IProcessor<T, R>> map = new HashMap<String, IProcessor<T, R>>();
		for(Entry<IReadonlyListable<K, Entry<String, IProcessor<T, R>>>, K[]> entry : inputs.entrySet()) try{
			for(Entry<String, IProcessor<T, R>> value : entry.getKey().all(entry.getValue()).values()) try{
				map.put(value.getKey(), value.getValue());
			}catch(Exception e){
				log.error("ServiceLoaderProcessor: processor failed.", e);
			}
		}catch(Exception e){
			log.error("ServiceLoaderProcessor: processor failed.", e);
		}
		synchronized(output){
			if(clear) output.empty();
			for(Entry<String, IProcessor<T, R>> entry : map.entrySet()) output.store(entry.getValue(), entry.getKey());
		}
		return in;
	}
}
