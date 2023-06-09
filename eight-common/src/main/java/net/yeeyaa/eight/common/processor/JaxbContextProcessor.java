package net.yeeyaa.eight.common.processor;

import java.util.List;

import net.yeeyaa.eight.IProcessor;

import org.eclipse.persistence.dynamic.DynamicClassLoader;
import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.jaxb.JAXBContext;
import org.eclipse.persistence.jaxb.dynamic.DynamicJAXBContext;
import org.eclipse.persistence.oxm.NamespaceResolver;


public class JaxbContextProcessor implements IProcessor<JAXBContext, JAXBContext>{
	protected volatile JAXBContext context;
	
	public void setContext(JAXBContext context) {
		this.context = context;
	}

	public class GetDynamicEntity implements IProcessor<String, DynamicEntity> {
		@Override
		public DynamicEntity process(String instance) {
			if (instance == null || context == null) return null;
			else return ((DynamicJAXBContext)context).newDynamicEntity(instance);
		}
	}
	
	public class GetDynamicType implements IProcessor<String, DynamicType> {
		@Override
		public DynamicType process(String instance) {
			if (instance == null || context == null) return null;
			else return ((DynamicJAXBContext)context).getDynamicType(instance);
		}
	}
	
	public class GetProperties implements IProcessor<Object, List<String>> {
		@Override
		public List<String> process(Object instance) {
			if (instance == null || context == null) return null;
			else {
				DynamicType type = ((DynamicJAXBContext)context).getDynamicType(instance instanceof String ? (String) instance : instance.getClass().getName());
				if (type == null) return null;
				else return type.getPropertiesNames();
			}
		}
	}

	public class GetDynamicClassLoader implements IProcessor<Object, DynamicClassLoader> {
		@Override
		public DynamicClassLoader process(Object instance) {
			if (context == null) return null;
			else return ((DynamicJAXBContext)context).getDynamicClassLoader();
		}
	}
	
	public class GetXpath implements IProcessor<Object, Object> {
		protected Object object;
		protected String xpath;
		protected NamespaceResolver  namespace; 
		protected Class<?> type;
			
		public void setObject(Object object) {
			this.object = object;
		}

		public void setXpath(String xpath) {
			this.xpath = xpath;
		}

		public void setNamespace(NamespaceResolver namespace) {
			this.namespace = namespace;
		}

		public void setType(Class<?> type) {
			this.type = type;
		}

		@Override
		public Object process(Object instance) {
			if (context == null || instance == null) return null;
			else if (instance instanceof Object[] && ((Object[])instance).length > 1){
				if (((Object[])instance).length > 3) return context.getValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), (NamespaceResolver)((Object[])instance)[3], (Class<?>)((Object[])instance)[2]);
				else if (((Object[])instance).length > 2) return context.getValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), namespace, (Class<?>)((Object[])instance)[2]);
				else return context.getValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), namespace, type);
			} else if (object == null) return context.getValueByXPath(instance, xpath, namespace, type);
			else return context.getValueByXPath(object, instance.toString(), namespace, type);
		}
	}

	public class SetXpath implements IProcessor<Object, Object> {
		protected Object object;
		protected String xpath;
		protected NamespaceResolver  namespace; 
		protected Class<?> type;
			
		public void setObject(Object object) {
			this.object = object;
		}

		public void setXpath(String xpath) {
			this.xpath = xpath;
		}

		public void setNamespace(NamespaceResolver namespace) {
			this.namespace = namespace;
		}

		public void setType(Class<?> type) {
			this.type = type;
		}

		@Override
		public Object process(Object instance) {
			if (context != null && instance != null) if (instance instanceof Object[] && ((Object[])instance).length > 1){
				if (((Object[])instance).length > 3) context.setValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), (NamespaceResolver)((Object[])instance)[3], (Class<?>)((Object[])instance)[2]);
				else if (((Object[])instance).length > 2) context.setValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), namespace, (Class<?>)((Object[])instance)[2]);
				else context.setValueByXPath(((Object[])instance)[0], ((Object[])instance)[1].toString(), namespace, type);
			} else if (object == null) context.setValueByXPath(instance, xpath, namespace, type);
			else context.setValueByXPath(object, instance.toString(), namespace, type);
			return instance;
		}
	}
	
	@Override
	public JAXBContext process(JAXBContext instance) {
		context = instance;
		return instance;
	}
}
