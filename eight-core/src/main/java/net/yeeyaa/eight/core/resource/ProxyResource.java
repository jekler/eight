package net.yeeyaa.eight.core.resource;

import net.yeeyaa.eight.IProcessor;

public abstract class ProxyResource<U> implements IProcessor<U, U>{
	protected U resource;
	protected Boolean endPoint = false;
	
	public final void setEndPoint(Boolean endPoint) {
		if(endPoint != null) this.endPoint = endPoint;
	}

	public final void setResource(U resource) {
		this.resource = resource;
	}
	
	public final U process(U resource) {
		synchronized(this){
			if(this.resource == null || endPoint || !(this.resource instanceof IProcessor)) this.resource = resource;
			else this.resource = ((IProcessor<U, U>)this.resource).process(resource);		
		}
		return (U) this;
	}
}
