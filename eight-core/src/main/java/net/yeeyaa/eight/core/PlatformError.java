package net.yeeyaa.eight.core;

import net.yeeyaa.eight.PlatformException.Type;

public enum PlatformError implements Type{
	ERROR_TASK_FAIL,
	ERROR_TRANSACTION_FAIL,
	ERROR_OTHER_FAIL,
	ERROR_PERFORMING_FAIL,
	ERROR_DATA_ACCESS,
	ERROR_IO,
	ERROR_NOT_SUPPORT,
	ERROR_PARAMETERS,
	ERROR_NOT_EXIST;

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
