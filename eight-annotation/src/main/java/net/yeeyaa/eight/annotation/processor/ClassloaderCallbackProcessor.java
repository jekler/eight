package net.yeeyaa.eight.annotation.processor;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ClassloaderCallbackProcessor implements IProcessor<String, String> {
	protected final Logger log;
	protected volatile Set<String> classNames = Collections.newSetFromMap(new ConcurrentHashMap<String, Boolean>());
	protected IProcessor<String, Class<?>> classLoader;
	
	public ClassloaderCallbackProcessor() {
		this.log = LoggerFactory.getLogger(ClassloaderCallbackProcessor.class);
	}

	public ClassloaderCallbackProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ClassloaderCallbackProcessor.class) : log;
	}
	
	public void setClassLoader(IProcessor<String, Class<?>> classLoader) {
		this.classLoader = classLoader;
	}

	@Override
	public String process(String in) {
		if(in != null) classNames.add(in);
		return in;
	}
	
	public synchronized Set<Class<?>> getClasses(){
		Set<String> classNames = this.classNames;
		this.classNames = Collections.newSetFromMap(new ConcurrentHashMap<String, Boolean>());
		HashSet<Class<?>> ret = new HashSet<Class<?>>();
		if(classNames != null && classNames.size() > 0) for(String className : classNames)try{
			ret.add(classLoader.process(className));
		}catch(Exception e){
			log.error("ClassloaderCallbackProcessor: processor failed.", e);
		}
		return ret;
	}
	
	public class GetClasses implements IProcessor<Object, Set<Class<?>>>{
		@Override
		public Set<Class<?>> process(Object instance) {
			return getClasses();
		}
	}
}
