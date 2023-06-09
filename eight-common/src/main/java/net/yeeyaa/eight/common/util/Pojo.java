package net.yeeyaa.eight.common.util;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.PlatformUtil;

import com.sun.xml.bind.CycleRecoverable;


public abstract class Pojo implements CycleRecoverable, Cloneable, Serializable {
	protected volatile Object tempInstance;
	
	@Override
	public final Object onCycleDetected(Context context) {
		if(tempInstance == null) try {
			Object pojo = clone();
			for(Field field : PlatformUtil.getAllField(getClass())) if(!Modifier.isStatic(field.getModifiers()) && !Pojo.class.equals(field.getDeclaringClass())){
				Class<?> clz = field.getType();
				if(CycleRecoverable.class.isAssignableFrom(clz) || Collection.class.isAssignableFrom(clz) || Map.class.isAssignableFrom(clz)){
					field.setAccessible(true);
					field.set(pojo, null);
				}
			}
			tempInstance = pojo;
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_OTHER_FAIL);
		}
		return tempInstance;
	}
}
