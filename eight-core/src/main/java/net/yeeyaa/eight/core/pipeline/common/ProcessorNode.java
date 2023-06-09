package net.yeeyaa.eight.core.pipeline.common;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ProcessorNode implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="\\|";
	protected IProcessor<Object, Object> beanHolder;
	
	public ProcessorNode() {
		this.log = LoggerFactory.getLogger(ProcessorNode.class);
	}

	public ProcessorNode(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ProcessorNode.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	@Override
	public Void process(Map<String, String> paras) {
		try{
			Object todo = null;
			String[] inputset = new String[0];
			if(paras.get("input") != null) inputset = paras.get("input").split(regex);
			if(inputset.length > 0){
				Object in = beanHolder.process(inputset[0]);
				if(IInputResource.class.isInstance(in)) {
					String[] para = new String[inputset.length - 1];
					for(int i = 0; i < para.length; i++) para[i] = inputset[i + 1];
					todo = ((IInputResource)in).find(para);				
				}
			}
			LinkedList<IProcessor<Object, Object>> procs = new LinkedList<IProcessor<Object, Object>>();
			if(paras.get("processors") != null) for(String proc : paras.get("processors").split(regex)){
				Object o = beanHolder.process(proc);
				if(IProcessor.class.isInstance(o)) procs.add((IProcessor<Object, Object>) o);
			}
			for(IProcessor<Object, Object> proc : procs) todo = proc.process(todo);
			HashMap<String[], IOutputResource> outs = new HashMap<String[], IOutputResource>();
			if(paras.get("output") != null) for(String output : paras.get("output").split(regex+regex)){
				String[] outputset = output.split(regex);
				if(outputset.length > 0){
					Object out = beanHolder.process(outputset[0]);
					if(IOutputResource.class.isInstance(out)) {
						String[] para = new String[outputset.length - 1];
						for(int i = 0; i < para.length; i++) para[i] = outputset[i + 1];
						outs.put(para, (IOutputResource)out);
					}
				}
			}
			if(outs.size() > 0) for(Entry<String[], IOutputResource> out : outs.entrySet()) out.getValue().store(todo,out.getKey());
		}catch(Exception e){
			log.error("ProcessorNode: performing error.", e);
		}
		return null;
	}
}
