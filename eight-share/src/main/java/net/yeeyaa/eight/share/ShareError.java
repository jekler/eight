package net.yeeyaa.eight.share;

import net.yeeyaa.eight.PlatformException.Type;

public enum ShareError implements Type {
	NO_SUCH_SERVICE;
	
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
