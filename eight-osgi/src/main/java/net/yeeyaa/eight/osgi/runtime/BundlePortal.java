package net.yeeyaa.eight.osgi.runtime;

import java.lang.reflect.Array;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;


public class BundlePortal implements IProcessor<Object, Object>{
	protected BundleContext context;
	protected Integer wait = 0;
	protected String filter;
	
	public BundlePortal(BundleContext context, String filter) {
		this.context = context;
		this.filter = filter;
	}

	public BundlePortal() {}

	public void setWait(Integer wait) {
		if(wait != null && wait > 0) this.wait = wait;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setFilter(String filter) {
		this.filter = filter;
	}
	
	@Override
	public Object process(Object instance) {
		Object[] in;
		if(instance instanceof Object[]) in = (Object[]) instance;
		else in = new Object[]{instance};
		if(in.length == 0) in = new Object[]{null};
		String filter = (String)in[0];
		if(filter == null) filter = this.filter;
		Object para;
		if(in.length == 2) para = in[1];
		else if(in.length > 2) {
			Integer length = in.length - 1;
			Object[] tmp = in.getClass() == Object[].class ? new Object[length] : (Object[])Array.newInstance(in.getClass().getComponentType(), length);
	        System.arraycopy(in, 1, tmp, 0,  length);
	        para = tmp;
		}else para = new Object[0];
		Object ret = null;
		if (context != null) try {
			ServiceReference<IProcessor>  ref = findReference(context, filter);
			if(ref == null && wait > 0) {
				Thread.sleep(wait * 1000);
				ref = findReference(context, filter);
			}
			if(ref != null) {
				IProcessor<Object, Object> processor = context.getService(ref);
				if(processor != null) try{
					ret = processor.process(para);
				} finally {
					context.ungetService(ref);
				}else throw new PlatformException(BundleError.SERVICE_CANNOT_FIND);
			}else throw new PlatformException(BundleError.SERVICE_CANNOT_FIND);
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return ret;
	}
	
	protected ServiceReference<IProcessor> findReference(BundleContext bc, String filter){
		ServiceReference<IProcessor>  ref = null;
		Integer rank = null;
		try{
			for(ServiceReference<IProcessor>  sr : bc.getServiceReferences(IProcessor.class, filter)){ 
				Object r = sr.getProperty(Constants.SERVICE_RANKING);
				if(r instanceof Integer && (rank == null || (Integer)r > rank)){
					ref= sr;
					rank = (Integer) r;
				}else if(ref == null) ref = sr;
			} 
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return ref;
	}
}
