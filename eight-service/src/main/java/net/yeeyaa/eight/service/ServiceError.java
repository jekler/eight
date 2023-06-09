package net.yeeyaa.eight.service;

import net.yeeyaa.eight.PlatformException.Type;

public enum ServiceError implements Type {
	ERROR_QUERY_MSG,
	SESSION_TIMEOUT,
	FILTER_FAIL,
	NO_SUCH_SERVICE,
	NO_RIGHT_TO_ACCESS_THE_RESOURCE,
	SERVICE_PERFORMING_FAIL,
	NO_SUCH_SERVICE_META,
	SEVERE_UNEXPECTED_ERROR;
	
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
