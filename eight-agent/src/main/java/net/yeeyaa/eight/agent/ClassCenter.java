package net.yeeyaa.eight.agent;

import java.lang.instrument.ClassFileTransformer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;

public final class ClassCenter{
	private ClassCenter() {}
	
	private static volatile ConcurrentHashMap<String, byte[]> classes = new ConcurrentHashMap<String, byte[]>();
	private static volatile HashSet<String> expression = new HashSet<String>();
	private static volatile List<ClassFileTransformer> transformers = new CopyOnWriteArrayList<ClassFileTransformer>();
	private static volatile Pattern pattern;
	static volatile Boolean classloaderPrefix = false;
	static volatile Boolean initialized = false;
	
	public static byte[] find(Class<?> clz){ 
		if(clz != null) if(classloaderPrefix) return classes.get(clz.getClassLoader() + clz.getName());
		else return classes.get(clz.getName());
		else return null;
	}
	
	public static void store(byte[] bytecode, String classname, String classloader){
		if(pattern != null && pattern.matcher(classname).matches()) if(classloaderPrefix) classes.put(classloader+classname, bytecode);
		else classes.put(classname, bytecode);
	}
	
	public static byte[] discard(Class<?> clz){ 
		if(clz != null) if(classloaderPrefix) return classes.remove(clz.getClassLoader() + clz.getName());
		else return classes.remove(clz.getName());
		else return null;
	}
	
	public static void empty(){
		classes = new ConcurrentHashMap<String, byte[]>();
	}

	public static Collection<Object[]> keys() {
		ArrayList<Object[]> list = new ArrayList<Object[]>(classes.size());
		for (String key : classes.keySet()) list.add(new String[]{key});
		return list;
	}

	public static Map<Object[], byte[]> all() {
		Map<Object[], byte[]> map = new HashMap<Object[], byte[]>(classes.size());
		for (Entry<String, byte[]> entry : classes.entrySet()) map.put(new String[]{entry.getKey()}, entry.getValue());
		return map;
	}
	
	public static Long count() {
		return new Long(classes.size());
	}
	
	static List<ClassFileTransformer> getTransformers(){
		return transformers;
	}
	
	public static synchronized void regTransformer(ClassFileTransformer transformer){
		if(!transformers.contains(transformer)) transformers.add(transformer);
	}
	
	public static synchronized void unregTransformer(ClassFileTransformer transformer){
		transformers.remove(transformer);
	}
	
	public static synchronized void clearTransformer(){
		transformers = new CopyOnWriteArrayList<ClassFileTransformer>();
	}
	
	public static synchronized void regRegex(String regex){
		if(regex != null && !expression.contains(regex)){
			expression.add(regex);
			StringBuilder sb = new StringBuilder();
			for(String e : expression) sb.append('|').append(e);
			pattern = Pattern.compile(sb.substring(1));
		}
	}
	
	public static synchronized void unregRegex(String regex){
		if(regex != null && expression.contains(regex)){
			expression.remove(regex);
			if(expression.isEmpty()) pattern = null;
			else {
				StringBuilder sb = new StringBuilder();
				for(String e : expression) sb.append('|').append(e);
				pattern = Pattern.compile(sb.substring(1));
			}
		}
	}
	
	public static synchronized void clearRegex(){
		expression = new HashSet<String>();
		pattern = null;
	}
}
