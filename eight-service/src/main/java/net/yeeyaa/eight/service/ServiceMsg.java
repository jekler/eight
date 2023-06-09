package net.yeeyaa.eight.service;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.PlatformException.Type;


public class ServiceMsg {
	public String name;
	public Object content;
	public Type error;
	public Map<String, Object> ext;
	public String token;
	public String id;
	
	public ServiceMsg(String token, String name, String id, Object content, Type error) {
		this.token = token;
		this.name = name;
		this.id = id;
		this.content = content;
		this.error = error;
	}
	
	public Object getParameter(String key){
		if(key != null && ext != null) return ext.get(key);
		else return null;
	}
	
	public void setParameter(String key, Object value){
		if(key != null){
			if(ext == null) ext = new ConcurrentHashMap<String, Object>();
			ext.put(key, value);
		}
	}
	
	public Object removeParameter(String key){
		if(key != null && ext != null) return ext.remove(key);
		else return null;
	}
	
	public void clearParameters(){
		ext = null;
	}

	public Set<String> listParameters(){
		if(ext != null) return ext.keySet();
		else return null;
	}
	
	public ServiceMsg(){}
}
