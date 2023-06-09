package net.yeeyaa.eight.core.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformUtil extends ClassLoader implements IProcessor<Object, Object>{
	protected final Logger log;
	protected static final CallerHandler caller = new CallerHandler();
	protected static final Set<Class<?>> WRAPPER_TYPES = new HashSet<Class<?>>(Arrays.<Class<?>>asList(
		    Boolean.class, Character.class, Byte.class, Short.class, Integer.class, Long.class, Float.class, Double.class, Void.class));
	protected static final Map<Class<?>, Class<?>> PRIMITIVE_MAP = new HashMap<Class<?>, Class<?>>(25);
	protected static final Map<Class<?>, Class<?>> WRAPPER_MAP = new HashMap<Class<?>, Class<?>>(25);
	public static enum MethodType{copy, isWrapperType, isPrimitiveType, toWrapper, toPrimitive, getAllField, getClass, getClassReverse, 
		getDefaultClassLoader, getClassLoader, getThreadClassLoader, getCallerClass, getCallerClassLoader, getCallerClassDepth, getCallerClassLoaderDepth, getClassStack};
	static{
		PRIMITIVE_MAP.put(boolean.class, Boolean.class);
		PRIMITIVE_MAP.put(char.class, Character.class);
		PRIMITIVE_MAP.put(byte.class, Byte.class);
		PRIMITIVE_MAP.put(short.class, Short.class);
		PRIMITIVE_MAP.put(int.class, Integer.class);
		PRIMITIVE_MAP.put(long.class, Long.class);
		PRIMITIVE_MAP.put(float.class, Float.class);
		PRIMITIVE_MAP.put(double.class, Double.class);
		PRIMITIVE_MAP.put(void.class, Void.class);
		PRIMITIVE_MAP.put(Boolean.class, Boolean.class);
		PRIMITIVE_MAP.put(Character.class, Character.class);
		PRIMITIVE_MAP.put(Byte.class, Byte.class);
		PRIMITIVE_MAP.put(Short.class, Short.class);
		PRIMITIVE_MAP.put(Integer.class, Integer.class);
		PRIMITIVE_MAP.put(Long.class, Long.class);
		PRIMITIVE_MAP.put(Float.class, Float.class);
		PRIMITIVE_MAP.put(Double.class, Double.class);
		PRIMITIVE_MAP.put(Void.class, Void.class);				
		WRAPPER_MAP.put(Boolean.class, boolean.class);
		WRAPPER_MAP.put(Character.class, char.class);
		WRAPPER_MAP.put(Byte.class, byte.class);
		WRAPPER_MAP.put(Short.class, short.class);
		WRAPPER_MAP.put(Integer.class, int.class);
		WRAPPER_MAP.put(Long.class, long.class);
		WRAPPER_MAP.put(Float.class, float.class);
		WRAPPER_MAP.put(Double.class, double.class);
		WRAPPER_MAP.put(Void.class, void.class);	
		WRAPPER_MAP.put(boolean.class, boolean.class);
		WRAPPER_MAP.put(char.class, char.class);
		WRAPPER_MAP.put(byte.class, byte.class);
		WRAPPER_MAP.put(short.class, short.class);
		WRAPPER_MAP.put(int.class, int.class);
		WRAPPER_MAP.put(long.class, long.class);
		WRAPPER_MAP.put(float.class, float.class);
		WRAPPER_MAP.put(double.class, double.class);
		WRAPPER_MAP.put(void.class, void.class);	
	}
	
	protected static class CallerHandler extends SecurityManager{
		protected Class<?> getCallerClass(){
			Class<?>[] classes = getClassContext();
			if(classes != null && classes.length > 3) return classes[3];
			else return null;
		}
		
		protected ClassLoader getCallerClassLoader(){
			Class<?>[] classes = getClassContext();
			if(classes != null && classes.length > 3) return classes[3].getClassLoader();
			else return null;
		}
		
		protected Class<?> getCallerClass(Integer depth){
			if(depth != null && depth >= 0){
				Class<?>[] classes = getClassContext();
				if(classes != null && classes.length > depth + 2) return classes[depth + 2];
			}
			return null;
		}
		
		protected ClassLoader getCallerClassLoader(Integer depth){
			if(depth != null && depth >= 0){
				Class<?>[] classes = getClassContext();
				if(classes != null && classes.length > depth + 2) return classes[depth + 2].getClassLoader();
			}
			return null;
		}
		
		protected Class<?>[] getClassStack(){
			Class<?>[] classes = getClassContext();
			if(classes != null && classes.length > 2) {
				Class<?>[] tmp = new Class<?>[classes.length - 2];
				for(int i = 0; i < classes.length - 2; i++) tmp[i] = classes[i + 2];
			}else classes = new Class<?>[0];
			return classes;
		}
	}
	
	public static Class<?> getCallerClass(){
		return caller.getCallerClass();
	}
	
	public static ClassLoader getCallerClassLoader(){
		return caller.getCallerClassLoader();
	}

	public static Class<?> getCallerClass(Integer depth){
		return caller.getCallerClass(depth);
	}
	
	public static ClassLoader getCallerClassLoader(Integer depth){
		return caller.getCallerClassLoader(depth);
	}
	
	public static Class<?>[] getClassStack(){
		return caller.getClassStack();
	}
	
	public static void deleteFiles(File f){
		try{
			if(f != null && f.exists()) if(f.isDirectory()) for(File file : f.listFiles()) deleteFiles(file);
			else f.delete();
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
	}
	
	public static Boolean compare(Object left, Object right){
		if(left == null ? right == null : left.equals(right)) return true;
		else {
			if(left != null && right != null && left.getClass().isArray() && right.getClass().isArray()){
				int length = Array.getLength(left);
				if(length == Array.getLength(right)) for(int i = 0; i < length; i++) { if(!compare(Array.get(left, i), Array.get(right, i))) return false; }
				else return false;
				return true;
			}
			return false;
		}
	}
	
	public static int hash(Object instance){
		int hash = 0;
		if(instance != null) if (instance.getClass().isArray()){
			int length = Array.getLength(instance);
			for(int i = 0; i < length; i++) {
				Object o = Array.get(instance, i);
				if (o != null) hash = hash * 37 + o.hashCode();
			}
		} else hash = instance.hashCode();
		return hash;
	}

	public static String toString(Object instance){
		if(instance != null) if (instance.getClass().isArray()){
			int length = Array.getLength(instance);
			ArrayList<Object> ret = new ArrayList<Object>(length);
			for(int i = 0; i < length; i++) ret.add(Array.get(instance, i));
			return ret.toString();
		} else return instance.toString();
		return null;
	}
	
    public static <T> T[] newArrayOf(T[] t, int len){
    	if(t != null) return (T[]) Array.newInstance(t.getClass().getComponentType(), len);
    	else return (T[]) new Object[len];
    }
    
    public static <T> T[] newArrayOf(int len, T t){
        return (T[]) Array.newInstance(t.getClass(), len);
    }
    
    protected static Object copy(Object instance, Map<Object, Object> map){
		if (instance == null) return null;
		else {
			Object o = map.get(instance);
			if (o == null) {
				if (instance instanceof Object[]) {
					Object[] tmp = newArrayOf((Object[])instance, ((Object[])instance).length);
					map.put(instance, tmp);
					for(int i = 0; i < tmp.length; i++) tmp[i] = copy(((Object[])instance)[i], map);
					instance = tmp;
				} else if (instance instanceof Collection) {
					Collection<Object> tmp = instance instanceof Set ? new HashSet<Object>(((Collection<Object>) instance).size() * 2) : new ArrayList<Object>(((Collection<Object>) instance).size());
					map.put(instance, tmp);
					for(Object so : (Collection<Object>) instance) tmp.add(copy(so, map));
					instance = tmp;
				} else if (instance instanceof Map) {
					Map<Object, Object> tmp = new HashMap<Object, Object>(((Map<Object, Object>)instance).size() * 2);
					map.put(instance, tmp);
					for(Entry<Object, Object> entry : ((Map<Object, Object>)instance).entrySet()) tmp.put(copy(entry.getKey(),  map), copy(entry.getValue(), map));
					instance = tmp;
				} else map.put(instance, instance);
				return instance;
			} else return o;
		}
    }
    
    public static Object copy(Object instance){
    	return copy(instance, new HashMap<Object, Object>());
    }
    
	public static <T> Type[] resolveActualTypeArgs (Class<? extends T> offspring, Class<T> base, Type... actualArgs) {
	    if (actualArgs.length == 0) actualArgs = offspring.getTypeParameters();
	    Map<String, Type> typeVariables = new HashMap<String, Type>();
	    for (int i = 0; i < actualArgs.length; i++) {
	        TypeVariable<?> typeVariable = (TypeVariable<?>) offspring.getTypeParameters()[i];
	        typeVariables.put(typeVariable.getName(), actualArgs[i]);
	    }
	    List<Type> ancestors = new LinkedList<Type>();
	    if (offspring.getGenericSuperclass() != null) ancestors.add(offspring.getGenericSuperclass());
	    for (Type t : offspring.getGenericInterfaces()) ancestors.add(t);
	    for (Type type : ancestors) {
	        if (type instanceof Class<?>) {
	            Class<?> ancestorClass = (Class<?>) type;
	            if (base.isAssignableFrom(ancestorClass)) {
	                Type[] result = resolveActualTypeArgs((Class<? extends T>) ancestorClass, base);
	                if (result != null) return result;
	            }
	        }
	        if (type instanceof ParameterizedType) {
	            ParameterizedType parameterizedType = (ParameterizedType) type;
	            Type rawType = parameterizedType.getRawType();
	            if (rawType instanceof Class<?>) {
	                Class<?> rawTypeClass = (Class<?>) rawType;
	                if (base.isAssignableFrom(rawTypeClass)) {
	                    List<Type> resolvedTypes = new LinkedList<Type>();
	                    for (Type t : parameterizedType.getActualTypeArguments()) if (t instanceof TypeVariable<?>) {
                            Type resolvedType = typeVariables.get(((TypeVariable<?>) t).getName());
                            resolvedTypes.add(resolvedType != null ? resolvedType : t);
                        } else resolvedTypes.add(t);
	                    Type[] result = resolveActualTypeArgs((Class<? extends T>) rawTypeClass, base, resolvedTypes.toArray(new Type[] {}));
	                    if (result != null) return result;
	                }
	            }
	        }
	    }
	    return offspring.equals(base) ? actualArgs : null;
	}
	
	public static Boolean isWrapperType(Object clazz) {
	    if(clazz instanceof Class) return WRAPPER_TYPES.contains(clazz);
	    else if(clazz == null) return false;
	    else return WRAPPER_TYPES.contains(clazz.getClass());
	}

	public static Boolean isPrimitiveType(Object clazz) {
	    if(clazz instanceof Class) return ((Class<?>)clazz).isPrimitive();
	    else if(clazz == null) return false;
	    else return clazz.getClass().isPrimitive();
	}
	
	public static Class<?> toWrapper(Object clazz){
	    if(clazz instanceof Class) return PRIMITIVE_MAP.get(clazz);
	    else if(clazz == null) return null;
	    else return PRIMITIVE_MAP.get(clazz.getClass());
	}

	public static Class<?> toPrimitive(Object clazz){
	    if(clazz instanceof Class) return WRAPPER_MAP.get(clazz);
	    else if(clazz == null) return null;
	    else return WRAPPER_MAP.get(clazz.getClass());
	}
	
	public static Field[] getAllField(Class<?> clazz) {
		List<Field> ls = new LinkedList<Field>();
	    while(clazz != null){
	    	ls.addAll(Arrays.asList(clazz.getDeclaredFields()));
	    	clazz = clazz.getSuperclass();
	    }
	    return ls.toArray(new Field[ls.size()]);
	}
	
	public static ClassLoader getDefaultClassLoader(Object o) {
		ClassLoader cl = null;
		if(o instanceof Class) cl = ((Class<?>) o).getClassLoader();
		else if(o != null) cl = o.getClass().getClassLoader();	
		if (cl == null) try {
			cl = Thread.currentThread().getContextClassLoader();
		}catch (Throwable ex) {}		
		if (cl == null) try {
			cl = ClassLoader.getSystemClassLoader();
		}catch (Throwable ex) {}
		return cl;
	}
	
	public static ClassLoader getClassLoader(Object o) {
		ClassLoader cl = null;
		if(o instanceof Class) cl = ((Class<?>) o).getClassLoader();
		else if(o != null) cl = o.getClass().getClassLoader();		
		if (cl == null) try {
			cl = ClassLoader.getSystemClassLoader();
		}catch (Throwable ex) {}
		return cl;
	}

	public static ClassLoader getThreadClassLoader() {
		ClassLoader cl = null;
		try {
			cl = Thread.currentThread().getContextClassLoader();
		}catch (Throwable ex) {}		
		if (cl == null) try {
			cl = ClassLoader.getSystemClassLoader();
		}catch (Throwable ex) {}
		return cl;
	}
	
	protected MethodType type = MethodType.getClass;
	protected Boolean cache = false;
	protected volatile ClassLoader classLoader;
	
	public PlatformUtil() {
		this.log = LoggerFactory.getLogger(ClassUtil.class);
	}

	public PlatformUtil(ClassLoader parent, MethodType type, Boolean cache, Logger log) {
		super(parent);
		this.log = log == null ? LoggerFactory.getLogger(PlatformUtil.class) : log;
		if(type != null) this.type = type;
		if(cache != null) this.cache = cache;
	}

	public PlatformUtil(ClassLoader parent, Logger log) {
		super(parent);
		this.log = log == null ? LoggerFactory.getLogger(PlatformUtil.class) : log;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setType(MethodType type) {
		if(type != null) this.type = type;
	}
	
	public void setCache(Boolean cache) {
		if(cache != null) this.cache = cache;
	}
	
	@Override
	public Object process(Object instance) {
		if(instance != null) switch(type){
			case copy: return copy(instance);
			case getDefaultClassLoader: if(classLoader != null) return classLoader;
										else {
											ClassLoader ret = getDefaultClassLoader(instance);
											if(cache) classLoader = ret;
											return ret;
										}
			case getClassLoader: if(classLoader != null) return classLoader;
								else {
									ClassLoader ret = getClassLoader(instance);
									if(cache) classLoader = ret;
									return ret;
								}	
			case getThreadClassLoader: return getThreadClassLoader();
			case getClassReverse: try{
				if(classLoader != null) return classLoader.loadClass(instance.toString());
				else if(cache) return super.loadClass(instance.toString());
				else throw new ClassNotFoundException(instance.toString());
			}catch(Exception e){
				try{
					return Thread.currentThread().getContextClassLoader().loadClass(instance.toString());
				}catch(Exception e1){
					try{
						return getCallerClassLoader().loadClass(instance.toString());
					}catch(Exception e2){
						try{
							return Class.forName(instance.toString(), false, getClass().getClassLoader());	
						}catch(Exception e3){}
					}				
				}
			}
			break;
			case getClass: try{
				if(classLoader != null) return classLoader.loadClass(instance.toString());
				else if(cache) return super.loadClass(instance.toString());
				else throw new ClassNotFoundException(instance.toString());
			}catch(Exception e){
				try{
					return getCallerClassLoader().loadClass(instance.toString());
				}catch(Exception e1){
					try{
						return Thread.currentThread().getContextClassLoader().loadClass(instance.toString());
					}catch(Exception e2){
						try{
							return Class.forName(instance.toString(), false, getClass().getClassLoader());	
						}catch(Exception e3){}
					}				
				}
			}
			break;			
			case isWrapperType: return isWrapperType((Class<?>)instance);
			case isPrimitiveType: return isPrimitiveType((Class<?>)instance);
			case toWrapper: return toWrapper((Class<?>)instance);
			case toPrimitive: return toPrimitive((Class<?>)instance);
			case getAllField: if(instance instanceof Class) return getAllField((Class<?>)instance);
			break;
			case getCallerClass: return caller.getCallerClass();
			case getCallerClassLoader: return caller.getCallerClassLoader();
			case getClassStack: return caller.getClassStack();
			case getCallerClassLoaderDepth: if(instance instanceof Integer) return caller.getCallerClassLoader((Integer) instance);
			break;
			case getCallerClassDepth: if(instance instanceof Integer) return caller.getCallerClass((Integer) instance);
		}
		return null;
	}	
	
	@Override
	public Class<?> loadClass(String name) throws ClassNotFoundException {
		try{
			if(classLoader != null) return classLoader.loadClass(name);
			else if(cache) return super.loadClass(name);
			else throw new ClassNotFoundException(name);
		}catch(Exception e){
			if(MethodType.getClassReverse.equals(type)) try{
				return Thread.currentThread().getContextClassLoader().loadClass(name);
			}catch(Exception e1){
				try{
					return getCallerClassLoader().loadClass(name);
				}catch(Exception e2){
					return Class.forName(name, false, getClass().getClassLoader());	
				}				
			}else try{
				return getCallerClassLoader().loadClass(name);
			}catch(Exception e1){
				try{
					return Thread.currentThread().getContextClassLoader().loadClass(name);
				}catch(Exception e2){
					return Class.forName(name, false, getClass().getClassLoader());	
				}				
			}
		}
	}

	@Override
	public URL getResource(String name) {
		URL resource = null;
		if(classLoader != null) resource = classLoader.getResource(name);
		else if(cache) resource = super.getResource(name);
		if(resource == null) if(MethodType.getClassReverse.equals(type)){
			resource = Thread.currentThread().getContextClassLoader().getResource(name);
			if(resource == null){
				try{
					resource = getCallerClassLoader().getResource(name);
				} catch(Exception e){}
				if(resource == null) resource = getClass().getClassLoader().getResource(name);
			}
		} else {
			try{
				resource = getCallerClassLoader().getResource(name);
			} catch(Exception e){}
			if(resource == null){
				resource = Thread.currentThread().getContextClassLoader().getResource(name);
				if(resource == null) resource = getClass().getClassLoader().getResource(name);
			}
		}
		return resource;
	}

	@Override
	public Enumeration<URL> getResources(String name) throws IOException {
		Enumeration<URL> resources = null;
		try{
			if(classLoader != null) {
				resources = classLoader.getResources(name);
				if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
			} else if(cache) {
				resources = super.getResources(name);
				if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
			} else throw new IOException(name);
		}catch(Exception e){
			if(MethodType.getClassReverse.equals(type)) try{
				resources = Thread.currentThread().getContextClassLoader().getResources(name);
				if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
			}catch(Exception e1){
				try{
					resources = getCallerClassLoader().getResources(name);
					if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
				}catch(Exception e2){
					resources = getClass().getClassLoader().getResources(name);
				}				
			}else try{
				resources = getCallerClassLoader().getResources(name);
				if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
			}catch(Exception e1){
				try{
					resources = Thread.currentThread().getContextClassLoader().getResources(name);
					if(resources == null || !resources.hasMoreElements()) throw new IOException(name);
				}catch(Exception e2){
					resources = getClass().getClassLoader().getResources(name);
				}				
			}
		}
		return resources == null ? Collections.enumeration(Collections.<URL>emptySet()) : resources;
	}

	@Override
	public InputStream getResourceAsStream(String name) {
		InputStream resource = null;
		if(classLoader != null) resource = classLoader.getResourceAsStream(name);
		else if(cache) resource = super.getResourceAsStream(name);
		if(resource == null) if(MethodType.getClassReverse.equals(type)){
			resource = Thread.currentThread().getContextClassLoader().getResourceAsStream(name);
			if(resource == null){
				try{
					resource = getCallerClassLoader().getResourceAsStream(name);
				} catch(Exception e){}
				if(resource == null) resource = getClass().getClassLoader().getResourceAsStream(name);
			}
		} else {
			try{
				resource = getCallerClassLoader().getResourceAsStream(name);
			} catch(Exception e){}
			if(resource == null){
				resource = Thread.currentThread().getContextClassLoader().getResourceAsStream(name);
				if(resource == null) resource = getClass().getClassLoader().getResourceAsStream(name);
			}
		}
		return resource;
	}
}
