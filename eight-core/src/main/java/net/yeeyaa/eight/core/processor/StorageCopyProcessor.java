package net.yeeyaa.eight.core.processor;

import java.util.Arrays;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage.Method;




public class StorageCopyProcessor implements IProcessor<IExtendable<Object>, Object> {
	protected IOutputResource<Object, Object> output;
	protected Object[] key;
	protected IProcessor<Object, Object> nameProcessor;
	protected IProcessor<IExtendable<Object>, Object> valueProcessor;
	protected Boolean delete = false;
	
	public void setDelete(Boolean delete) {
		if(delete != null) this.delete = delete;
	}

	public void setOutput(IOutputResource<Object, Object> output) {
		this.output = output;
	}

	public void setKey(Object[] key) {
		this.key = key;
	}

	public void setNameProcessor(IProcessor<Object, Object> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setValueProcessor(IProcessor<IExtendable<Object>, Object> valueProcessor) {
		this.valueProcessor = valueProcessor;
	}

	@Override
	public Object process(IExtendable<Object> storage) {
		if(storage != null){
			Object k;
			if(nameProcessor == null) k = storage.extend(Method.key);
			else k = nameProcessor.process(storage.extend(Method.key));
			Object[] realkey;
			if(key == null) if(k == null) realkey = new Object[0];
			else if(k instanceof Object[]) realkey = (Object[]) k;
			else realkey = new Object[]{k};
			else if(k == null) realkey = key;
			else if(k instanceof Object[]){
				realkey = Arrays.copyOf(key, key.length + ((Object[])k).length);
				for(int i = 0; i < ((Object[])k).length; i++) realkey[key.length + i] = ((Object[])k)[i];
			}else{
				realkey = Arrays.copyOf(key, key.length + 1);
				realkey[key.length] = k;
			}
			if(!delete || Boolean.TRUE.equals(storage.extend(Method.exists))){
				Object v;
				if(valueProcessor == null) v = storage;
				else v = valueProcessor.process(storage);
				if(v != null) output.store(v, realkey);
			}else output.discard(realkey);
			return realkey;
		}
		return null;
	}
}
