package net.yeeyaa.eight.osgi.runtime;

import net.yeeyaa.eight.PlatformException.Type;

public enum BundleError implements Type {
	ERROR_PARAMETER,
	ERROR_CONFIG,
	WAIT_INTERRUPTED,
	CLASS_LOAD_ERROR,
	SERVICE_CANNOT_FIND,
	SERVICE_NOT_EXIST,
	SERVICE_INVOKE_ERROR,
	SERVICE_UNAVAILABLE;
	
	@Override
	public String getMessage() {	
		return name();
	}

	@Override
	public void setMessage(String message) {}

	@Override
	public Integer getCode() {
		return ordinal();
	}

	@Override
	public void setCode(Integer code) {}

	@Override
	public String getCate() {
		return getDeclaringClass().getSimpleName();
	}
	
	@Override
	public void setCate(String cate) {}
}
