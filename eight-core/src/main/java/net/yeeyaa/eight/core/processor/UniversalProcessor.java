package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.IUniversal;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class UniversalProcessor implements IProcessor<Object, Object>{
	protected Logger log;
	protected IProcessor<Object, Object> service;
	protected IProcessor<Object[], Object> preProcessor;
	protected MethodType method;
	protected Boolean passName = false;
	
	protected enum MethodType{
		execute,
		find,
		store,
		delete,
		empty,
		keys,
		all,
		process,
		perform,
		operate,
		realObject,
		extend,
		methods;
	}

	public UniversalProcessor() {
		this.log = LoggerFactory.getLogger(UniversalProcessor.class);
	}
	
	public UniversalProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(UniversalProcessor.class) : log;
	}
	
	public void setPassName(Boolean passName) {
		if(passName != null) this.passName = passName;
	}

	public void setService(IProcessor<Object, Object> service) {
		this.service = service;
	}
	
	public void setPreProcessor(IProcessor<Object[], Object> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setMethod(String method) {
		try{
			this.method = MethodType.valueOf(method);
		}catch(Exception e){
            log.error("UniversalProcessor: convert method type fail: "+method, e);	
		}
	}

	@Override
	public Object process(Object ps) {	
		MethodType method = this.method;
		Object paras = ps;
		Object service;
		if(passName && ps instanceof Object[] && ((Object[])ps).length > 1) {
			service = this.service.process(((Object[])ps)[0]);
			paras = ((Object[])ps)[((Object[])ps).length - 1];
		} else service = this.service.process(null);
		if(method == null && ps instanceof Object[]) try{
			if(passName){
				if(((Object[])ps).length > 2) method = MethodType.valueOf(((Object[])ps)[1].toString());
			}else if(((Object[])ps).length > 1){
				method = MethodType.valueOf(((Object[])ps)[0].toString());
				paras = ((Object[])ps)[((Object[])ps).length - 1];
			}
		}catch(Exception e){
            log.error("UniversalProcessor: convert method type fail: "+method, e);	
		}
		if(preProcessor != null) paras = preProcessor.process(new Object[]{method == null ? "process" : method.name(), paras});
		if(method != null) switch(method){
			case process: if(service instanceof IProcessor) return ((IProcessor)service).process(paras);
			break;
			case find: if(service instanceof IInputResource && paras != null) {
				if(paras instanceof Object[]) return ((IInputResource)service).find((Object[])paras);
				else return ((IInputResource)service).find(paras);
			}
			break;
			case store: if(service instanceof IOutputResource && paras instanceof Object[]) {
				Object[] p = (Object[])paras;
				if(p.length > 1 && p[1] instanceof Object[]) return ((IOutputResource)service).store(p[0], (Object[])p[1]);
			}
			break;
			case delete: if(service instanceof IOutputResource && paras != null) {
				if(paras instanceof Object[]) return ((IOutputResource)service).discard((Object[])paras);
				else return ((IOutputResource)service).discard(paras);
			}
			break;
			case empty: if(service instanceof IOutputResource) {
				if(paras == null) return ((IOutputResource)service).empty();
				else if(paras instanceof Object[]) return ((IOutputResource)service).empty((Object[])paras);
				else return ((IOutputResource)service).empty(paras);
			}
			break;
			case keys: if(service instanceof IListable){
				if(paras == null) return ((IListable)service).keys();
				else if(paras instanceof Object[]) return ((IListable)service).keys((Object[])paras);
				else return ((IListable)service).keys(paras);
			}
			break;
			case all: if(service instanceof IListable){
				if(paras == null) return ((IListable)service).all();
				else if(paras instanceof Object[]) return ((IListable)service).all((Object[])paras);
				else return ((IListable)service).all(paras);
			}
			break;
			case execute: if(service instanceof ITransaction && paras instanceof IProcessor) return ((ITransaction)service).execute((IProcessor)paras);
			break;
			case extend: if(service instanceof IExtendable) return ((IExtendable)service).extend(paras);
			break;
			case methods: if(service instanceof IExtendable) return ((IExtendable)service).methods();
			break;						
			case perform: if(service instanceof IBiProcessor && paras instanceof Object[] && ((Object[])paras).length > 1) 
				return ((IBiProcessor)service).perform(((Object[])paras)[0], ((Object[])paras)[1]);
			break;
			case operate: if(service instanceof ITriProcessor && paras instanceof Object[] && ((Object[])paras).length > 2) 
				return ((ITriProcessor)service).operate(((Object[])paras)[0], ((Object[])paras)[1], ((Object[])paras)[2]);
			break;			
			case realObject: if(service instanceof IUniversal) return ((IUniversal)service).realObject();
			
		}else if(service instanceof IProcessor) return ((IProcessor)service).process(paras);
		throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
	}
}
