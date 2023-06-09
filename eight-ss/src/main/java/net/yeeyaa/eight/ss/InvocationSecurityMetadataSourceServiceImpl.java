package net.yeeyaa.eight.ss;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.web.access.intercept.FilterInvocationSecurityMetadataSource;
import org.springframework.util.PathMatcher;
import org.springframework.security.web.FilterInvocation;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.Content.Couple;


public class InvocationSecurityMetadataSourceServiceImpl implements FilterInvocationSecurityMetadataSource, IProcessor<Object, Object> {
	protected static final Logger log = LoggerFactory.getLogger(InvocationSecurityMetadataSourceServiceImpl.class);
	protected Integer type = 0;
	protected IProcessor<Object, Collection<Entry<Entry<Object, Map<Object, Object>>, Entry<Set<Object>, Set<Object>>>>> serviceProcessor;
	protected volatile Map<Object, Entry<Map<Object, Object>, Collection<ConfigAttribute>>> configMap = Collections.EMPTY_MAP;
	protected PathMatcher pathMatcher;
	protected Object product;
	
	public void setType(Integer type) {
		if(type != null) this.type = type;
	}

	public void setPathMatcher(PathMatcher pathMatcher) {
		this.pathMatcher = pathMatcher;
	}

	public void setProduct(Object product) {
		this.product = product;
	}

	public void setServiceProcessor(IProcessor<Object, Collection<Entry<Entry<Object, Map<Object, Object>>, Entry<Set<Object>, Set<Object>>>>> serviceProcessor) {
		this.serviceProcessor = serviceProcessor;
	}

	protected class SecurityConfig implements ConfigAttribute {
		protected String attribute;
		protected boolean permit;
		
		public SecurityConfig(String attribute, boolean permit) {
			this.attribute = attribute;
			this.permit = permit;
		}

		@Override
		public String getAttribute() {

			return attribute;
		}		
	}
	
	public void loadFunctionDefine() {
		try{
			Collection<Entry<Entry<Object, Map<Object, Object>>, Entry<Set<Object>, Set<Object>>>> services = serviceProcessor.process(product);
			Map<Object, Entry<Map<Object, Object>, Collection<ConfigAttribute>>> configMap = new LinkedHashMap<Object, Entry<Map<Object, Object>, Collection<ConfigAttribute>>>(services == null ? 0 : services.size() * 2);
			if(services != null) for (Entry<Entry<Object, Map<Object, Object>>, Entry<Set<Object>, Set<Object>>> service : services) {
				LinkedList<ConfigAttribute> config = new LinkedList<ConfigAttribute>();
				if(service.getKey() != null && service.getValue() != null && service.getKey().getKey() != null) {
					configMap.put(service.getKey().getKey(), new Couple<Map<Object, Object>, Collection<ConfigAttribute>>(service.getKey().getValue(), config));
					Entry<Set<Object>, Set<Object>> auth = service.getValue();
					if(auth != null) {
						if (auth.getValue() != null) for(Object role : auth.getValue()) if (role != null) config.add(new SecurityConfig(role.toString(), false));
						if (auth.getKey() != null) for(Object role : auth.getKey()) if (role != null) config.add(new SecurityConfig(role.toString(), true));
					}
				}
			}
			this.configMap = configMap;
		}catch(Exception e){
			log.error("InvocationSecurityMetadataSourceServiceImpl: Cannot load function define。 product: " + product, e);
		}
	}

	public Collection<ConfigAttribute> getAttributes(Object object) throws IllegalArgumentException {
		String path = ((FilterInvocation) object).getHttpRequest().getServletPath();
		if(type != 1 && configMap.containsKey(path)) return configMap.get(path).getValue();
		else if(type != 0 && path != null) for (Entry<Object, Entry<Map<Object, Object>, Collection<ConfigAttribute>>> entry : configMap.entrySet())
			if (pathMatcher.match(entry.getKey().toString(), path)) return entry.getValue().getValue();
		return null;
	}
	
	public boolean supports(Class<?> clazz) {
		return true;
	}
	
	public Collection<ConfigAttribute> getAllConfigAttributes() {
		return null;
	}

	@Override
	public Object process(Object object) {
		loadFunctionDefine();
		return object;
	}
}
