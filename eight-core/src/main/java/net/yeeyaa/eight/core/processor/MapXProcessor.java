package net.yeeyaa.eight.core.processor;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MapXProcessor<K, V, R> implements IProcessor<Map<K, V>, Map<K, R>> {
	protected final Logger log;
	protected IProcessor<V, R> processor;
	protected IProcessor<K, R> keyProcessor;
	protected IProcessor<Object, R> entryProcessor;
	protected Boolean ignore = true;
	protected Boolean nullable = true;
	
	public MapXProcessor() {
		this.log = LoggerFactory.getLogger(MapXProcessor.class);
	}
	
	public MapXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MapXProcessor.class) : log;
	}
	
	public void setNullable(Boolean nullable) {
		this.nullable = nullable;
	}

	public void setEntryProcessor(IProcessor<Object, R> entryProcessor) {
		this.entryProcessor = entryProcessor;
	}

	public void setKeyProcessor(IProcessor<K, R> keyProcessor) {
		this.keyProcessor = keyProcessor;
	}

	public void setIgnore(Boolean ignore) {
		this.ignore = ignore;
	}

	public void setProcessor(IProcessor<V, R> processor) {
		this.processor = processor;
	}

	@Override
	public Map<K, R> process(Map<K, V> in) {	
		if(in != null){
			Map<K, R> ret = new LinkedHashMap<K, R>();
			for(Entry<K, V> entry : in.entrySet()) try{
				R o = entryProcessor == null ? keyProcessor == null ? processor == null ? (R) entry.getValue() : processor.process(entry.getValue()) : 
						processor == null ? keyProcessor.process(entry.getKey()) : (R) new Object[]{keyProcessor.process(entry.getKey()), processor.process(entry.getValue())} :
						entryProcessor.process(new Object[]{keyProcessor == null ? entry.getKey() : keyProcessor.process(entry.getKey()), 
						processor == null ? entry.getValue() : processor.process(entry.getValue())});
				if(o != null || nullable) ret.put(entry.getKey(), o);
			}catch(Exception e){
				log.error("MapXProcessor: processor failed.", e);
				if(!ignore) if (e instanceof PlatformException) throw (PlatformException) e;
				else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
			}
			return ret;
		}
		return null;
	}
}
