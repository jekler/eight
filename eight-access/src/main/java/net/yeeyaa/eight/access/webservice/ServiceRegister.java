package net.yeeyaa.eight.access.webservice;

import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.xml.ws.Endpoint;

import net.yeeyaa.eight.IProcessor;

import com.sun.net.httpserver.HttpContext;

public class ServiceRegister {
	protected Map<String, Object> services;
	protected IProcessor<String, HttpContext> contextHolder;

	public void setContextHolder(IProcessor<String, HttpContext> contextHolder) {
		this.contextHolder = contextHolder;
	}

	public void setServices(Map<String, Object> services) {
		this.services = services;
	}

	@PostConstruct
	public void initialize(){
		if(contextHolder == null) for(Entry<String, Object> service : services.entrySet()) Endpoint.publish((String)service.getKey(), service.getValue());
		else for(Entry<String, Object> service : services.entrySet()){
			Endpoint endPoint = Endpoint.create(service.getValue());
			endPoint.publish(contextHolder.process(service.getKey()));
		}
		
	}
}
