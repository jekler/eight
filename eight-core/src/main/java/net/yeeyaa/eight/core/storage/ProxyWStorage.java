package net.yeeyaa.eight.core.storage;

import java.io.InputStream;
import java.io.OutputStream;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;


public class ProxyWStorage extends Storage<Object> {
	protected volatile IExtendable<Object> storage;
	protected Object key;
	protected Boolean exist;
	protected Long modify;
	protected IProcessor<IExtendable<Object>, Object> keyProcessor;
	protected IProcessor<IExtendable<Object>, Long> modifyProcessor;
	protected IProcessor<IExtendable<Object>, Boolean> existProcessor;	
	protected IProcessor<IExtendable<Object>, InputStream> inputProcessor;
	protected IProcessor<IExtendable<Object>, OutputStream> outputProcessor;
	
	public ProxyWStorage(IExtendable<Object> storage) {
		this.storage = storage;
	}

	public ProxyWStorage(){}
	

	public void setExist(Boolean exist) {
		this.exist = exist;
	}

	public void setStorage(IExtendable<Object> storage) {
		if (storage != null) this.storage = storage;
	}

	public void setModify(Long modify) {
		this.modify = modify;
	}

	public void setKey(Object key) {
		this.key = key;
	}

	public void setOutputProcessor(IProcessor<IExtendable<Object>, OutputStream> outputProcessor) {
		this.outputProcessor = outputProcessor;
	}
	
	public void setKeyProcessor(IProcessor<IExtendable<Object>, Object> keyProcessor) {
		this.keyProcessor = keyProcessor;
	}

	public void setModifyProcessor(IProcessor<IExtendable<Object>, Long> modifyProcessor) {
		this.modifyProcessor = modifyProcessor;
	}

	public void setExistProcessor(IProcessor<IExtendable<Object>, Boolean> existProcessor) {
		this.existProcessor = existProcessor;
	}

	public void setInputProcessor(IProcessor<IExtendable<Object>, InputStream> inputProcessor) {
		this.inputProcessor = inputProcessor;
	}

	public InputStream input() {
		if(inputProcessor == null) return storage.extend(Method.input);
		else return inputProcessor.process(storage);
	}

	public OutputStream output() {
		if(outputProcessor == null) return storage.extend(Method.output);
		else return outputProcessor.process(storage);
	}
	
	public Boolean exists() {
		if (existProcessor == null) if (exist == null) return storage.extend(Method.exists);
		else return exist;
		else return existProcessor.process(storage);
	}

	public Object key() {
		if (keyProcessor == null) if (key == null) return storage.extend(Method.key);
		else return key;
		else return keyProcessor.process(storage);
	}

	public Long modified() {
		if (modifyProcessor == null) if (modify == null) return storage.extend(Method.modified);
		else return modify;
		else return modifyProcessor.process(storage);
	}

	@Override
	public String toString() {
		Object key = key();
		return key == null ? null : key.toString();
	}
}
