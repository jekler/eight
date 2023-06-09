package net.yeeyaa.eight.core.processor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CollectionXProcessor implements IProcessor<Object, Object> {
	protected final Logger log;
	protected IProcessor<Object, Object> processor;
	protected Boolean ignoreError = true;
	protected Boolean nullable = true;
	protected Set<Integer> index;
	protected IProcessor<Object, Collection<Object>> convertor;
	protected Boolean type; 
	protected Boolean adapt; 
	
	public CollectionXProcessor() {
		this.log = LoggerFactory.getLogger(CollectionXProcessor.class);
	}
	
	public CollectionXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CollectionXProcessor.class) : log;
	}
	
	public void setConvertor(IProcessor<Object, Collection<Object>> convertor) {
		this.convertor = convertor;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}
	
	public void setIgnoreError(Boolean ignoreError) {
		if (ignoreError != null) this.ignoreError = ignoreError;
	}

	public void setAdapt(Boolean adapt) {
		this.adapt = adapt;
	}

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setIndex(Set<Integer> index) {
		this.index = index;
	}

	@Override
	public Object process(Object in) {	
		if(in != null){
			if(convertor != null) in = convertor.process(in);
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(in instanceof Collection) if (Boolean.TRUE.equals(type)) {
				LinkedHashMap<Object, Object> ret = new LinkedHashMap<Object, Object>(((Collection<Object>) in).size() * 2);
				int i = 0;
				for(Object o : (Collection<Object>) in) try{
					Object result = null;
					if (index == null || index.contains(i++)) if (Boolean.TRUE.equals(adapt) && o != null && (o instanceof Collection || o.getClass().isArray())) result = process(o);
					else result = processor.process(o);
					if(result != null || nullable) ret.put(o, result);
				}catch(Exception e){
					log.error("CollectionXProcessor: processor failed.", e);
					if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
					else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
				}
				return ret;
			} else {
				Collection<Object> ret = new ArrayList<Object>(((Collection<Object>) in).size());
				int i = 0;
				for(Object o : (Collection<Object>) in) try{
					if (index == null || index.contains(i++)) if (Boolean.TRUE.equals(adapt) && o != null && (o instanceof Collection || o.getClass().isArray())) o = process(o);
					else o = processor.process(o);
					if(o != null || nullable) ret.add(o);
				}catch(Exception e){
					log.error("CollectionXProcessor: processor failed.", e);
					if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
					else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
				}
				if (Boolean.FALSE.equals(type)) return ret.toArray();
				else return ret;
			}
		}
		try{
			if (Boolean.TRUE.equals(type)) {
				LinkedHashMap<Object, Object> ret = new LinkedHashMap<Object, Object>(2);
				ret.put(in, (index == null || index.contains(0)) ? processor.process((Object)in) : in);
				return ret;
			} else {
				if (!Boolean.FALSE.equals(adapt)) return index == null || index.contains(0) ? processor.process((Object)in) : in;
				Collection<Object> ret = new ArrayList<Object>(1);
				ret.add(index == null || index.contains(0) ? processor.process((Object)in) : in);
				if (Boolean.FALSE.equals(type)) return ret.toArray();
				else return ret;
			}
		}catch(Exception e){
			log.error("CollectionXProcessor: processor failed.", e);
		}
		return null;
	}
}
