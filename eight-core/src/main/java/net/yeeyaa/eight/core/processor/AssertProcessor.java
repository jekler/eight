package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class AssertProcessor<T> implements IProcessor<T, T> {
	protected Class<?> classType;
	protected Boolean nullable = false;
	
	public void setNullable(Boolean nullable) {
		if(nullable != null) this.nullable = nullable;
	}

	public void setClassType(Class<?> classType) {
		this.classType = classType;
	}

	@Override
	public T process(T in) {	
		if(in != null){
			if(classType != null && classType.isInstance(in)) return in;
			else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		}else if(nullable) return null;
		else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	}
}
