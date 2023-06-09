package net.yeeyaa.eight.common.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.JAXBIntrospector;
import javax.xml.bind.MarshalException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller.Listener;
import javax.xml.bind.ValidationEventHandler;
import javax.xml.bind.ValidationException;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.attachment.AttachmentMarshaller;
import javax.xml.bind.attachment.AttachmentUnmarshaller;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.oxm.MarshallingFailureException;
import org.springframework.oxm.UncategorizedMappingException;
import org.springframework.oxm.UnmarshallingFailureException;
import org.springframework.oxm.ValidationFailureException;
import org.springframework.oxm.XmlMappingException;
import org.xml.sax.SAXException;


public class JaxbUtil implements IProcessor<Object, JAXBContext> {
	protected final Logger log;
    protected ClassLoader classLoader = getClass().getClassLoader();
    protected String contextPath;
    protected String factory;
    protected Method jcf;
    protected Boolean lazyInit; 
    protected IProcessor<Object, Map<String, Object>> propertyProcessor;
    protected volatile Map<String, Object> properties;
    protected volatile JAXBContext jc;
    protected volatile JAXBIntrospector ji;
    protected volatile int ver;

	public JaxbUtil() {
		this.log = LoggerFactory.getLogger(JaxbUtil.class);
	}

	public JaxbUtil(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(JaxbUtil.class) : log;
	}
    
	public void setFactory(String factory) {
		this.factory = factory;
	}

	public void setPropertyProcessor(IProcessor<Object, Map<String, Object>> propertyProcessor) {
		this.propertyProcessor = propertyProcessor;
	}

	public void setClassLoader(ClassLoader classLoader) {
		if (classLoader != null) this.classLoader = classLoader;
	}

	public void setProperties(Map<String, Object> properties) {
		this.properties = properties;
	}

	public void setContextPath(String contextPath) {
		this.contextPath = contextPath;
	}
	
	public void setLazyInit(Boolean lazyInit) {
		this.lazyInit = lazyInit;
	}

	@PostConstruct
	public void initialize() {
		if (factory != null) try {
			jcf = classLoader.loadClass(factory).getMethod("createContext", String.class, ClassLoader.class, Map.class);
		} catch (Exception e) {
			log.error("JaxbUtil: factory class" + factory + " can not be loaded.", e);
		}
		if(!Boolean.TRUE.equals(lazyInit)) process(lazyInit == null);
	}
    
    public class Unmarshaller implements org.springframework.oxm.Unmarshaller, IProcessor<Object, Object> {
        protected Map<String, Object> setting;
        protected Map<Object, XmlAdapter> adaptor;
        protected Schema schema;
        protected AttachmentUnmarshaller attachment;
        protected ValidationEventHandler handler;
        protected Listener listener;
        protected Integer cacheSize = 10;
        protected Class<?> type;
        protected volatile LinkedList<javax.xml.bind.Unmarshaller> unmarshaller = new LinkedList<javax.xml.bind.Unmarshaller>();
        protected volatile int ver;
        
        public class Refresh implements IProcessor<Object, Object> {
    		public void setSetting(Map<String, Object> setting) {
    			Unmarshaller.this.setSetting(setting);
    		}

    		public void setAdaptor(Map<Object, XmlAdapter> adaptor) {
    			Unmarshaller.this.setAdaptor(adaptor);
    		}
    		
    		public void setAttachment(AttachmentUnmarshaller attachment) {
    			Unmarshaller.this.setAttachment(attachment);
    		}

    		public void setHandler(ValidationEventHandler handler) {
    			Unmarshaller.this.setHandler(handler);
    		}

    		public void setListener(Listener listener) {
    			Unmarshaller.this.setListener(listener);
    		}

    		public void setCacheSize(Integer cacheSize) {
    			Unmarshaller.this.setCacheSize(cacheSize);
    		}

    		public void setSchema(Object schema) {
    			Unmarshaller.this.setSchema(schema);
    		}
    		
    		public void setRoot(String root) {
    			Unmarshaller.this.setRoot(root);
    		}

			@Override
			public Object process(Object instance) {
				return Unmarshaller.this.process(instance);
			}
        }
        
		public void setSetting(Map<String, Object> setting) {
			this.setting = setting;
		}

		public void setAdaptor(Map<Object, XmlAdapter> adaptor) {
			this.adaptor = adaptor;
		}
		
		public void setAttachment(AttachmentUnmarshaller attachment) {
			this.attachment = attachment;
		}

		public void setHandler(ValidationEventHandler handler) {
			this.handler = handler;
		}

		public void setListener(Listener listener) {
			this.listener = listener;
		}

		public void setCacheSize(Integer cacheSize) {
			if(cacheSize != null && cacheSize >= 0) this.cacheSize = cacheSize;
		}

		public void setSchema(Object schema) {
			if (schema != null) if (schema instanceof Schema) this.schema = (Schema) schema;
			else try {
				if (schema instanceof Source) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((Source) schema);
				else if (schema instanceof Source[]) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((Source[]) schema);
				else if (schema instanceof URL) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((URL) schema);
				else if (schema instanceof File) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((File) schema);
				else if (schema instanceof InputStream) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource((InputStream) schema));
				else if (schema instanceof Reader) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource((Reader) schema));
				else this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource(new StringReader(schema.toString())));
			} catch (SAXException e) {
				log.error("JaxbUtil: schema error.", e);
			}
		}
		
		public void setRoot(String root) {
			if (root != null) try {
				type = classLoader.loadClass(root);
			} catch (Exception e) {
				log.error("JaxbUtil: root class" + root + " can not be loaded.", e);
			}
		}
		
		@Override
		public Object unmarshal(Source source) throws IOException, XmlMappingException {
			try{
				if (JaxbUtil.this.ver == 0) synchronized(JaxbUtil.this) {
					if (JaxbUtil.this.ver == 0) JaxbUtil.this.process(true);
				}
				javax.xml.bind.Unmarshaller unmarshal = null;
				synchronized(this){
					if (JaxbUtil.this.ver != ver) {
						unmarshaller = new LinkedList<javax.xml.bind.Unmarshaller>();
						ver = JaxbUtil.this.ver;
					}
					if (unmarshaller.size() > 0) unmarshal = unmarshaller.removeFirst();
					
				}
				if(unmarshal == null) {
					unmarshal = jc.createUnmarshaller();
			        if(setting != null && setting.size() > 0) for(Entry<String, Object> s : setting.entrySet())
			        	unmarshal.setProperty(s.getKey(), s.getValue());
			        if(adaptor != null && adaptor.size() > 0) for(Entry<Object, XmlAdapter> entry : adaptor.entrySet())
			        	if (entry.getKey() instanceof Class) unmarshal.setAdapter((Class)entry.getKey(), entry.getValue());
			        	else if (entry.getKey() instanceof String) try {
							unmarshal.setAdapter((Class)classLoader.loadClass((String)entry.getKey()), entry.getValue());
						} catch (ClassNotFoundException e) {
							if (entry.getValue() != null) unmarshal.setAdapter(entry.getValue());
						} else if (entry.getValue() != null) unmarshal.setAdapter(entry.getValue());
			        if (schema != null) unmarshal.setSchema(schema);
			        if (attachment != null) unmarshal.setAttachmentUnmarshaller(attachment);
			        if (handler != null) unmarshal.setEventHandler(handler);
			        if (listener != null) unmarshal.setListener(listener);
				}
				Object ret = type == null ? unmarshal.unmarshal(source) : unmarshal.unmarshal(source, type).getValue();
				if(unmarshaller.size() < cacheSize) synchronized(this){
					unmarshaller.add(unmarshal);
				}
				return ret;
			}catch(JAXBException e){
				throw convertJaxbException(e);
			}
		}
	
		@Override
		public boolean supports(Class<?> clazz) {
			try{
				return ji.isElement(clazz.newInstance());
			}catch(Exception e){
				throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, "JaxbUtil: supports fail.", e);
			}
		}

		@Override
		public Object process(Object instance) {
			unmarshaller = new LinkedList<javax.xml.bind.Unmarshaller>();
			return instance;
		}
    }

    public class Marshaller implements org.springframework.oxm.Marshaller, IProcessor<Object, Object>{
        protected Map<String, Object> setting;
        protected Map<Object, XmlAdapter> adaptor;
        protected Schema schema;
        protected AttachmentMarshaller attachment;
        protected ValidationEventHandler handler;
        protected javax.xml.bind.Marshaller.Listener listener;
        protected volatile javax.xml.bind.Marshaller marshaller;
        protected volatile int ver;
        
        public class Refresh implements IProcessor<Object, Object> {
    		public void setSetting(Map<String, Object> setting) {
    			Marshaller.this.setSetting(setting);
    		}

    		public void setAdaptor(Map<Object, XmlAdapter> adaptor) {
    			Marshaller.this.setAdaptor(adaptor);
    		}
    		
    		public void setAttachment(AttachmentMarshaller attachment) {
    			Marshaller.this.setAttachment(attachment);
    		}

    		public void setHandler(ValidationEventHandler handler) {
    			Marshaller.this.setHandler(handler);
    		}

    		public void setListener(javax.xml.bind.Marshaller.Listener listener) {
    			Marshaller.this.setListener(listener);
    		}

    		public void setSchema(Object schema) {
    			Marshaller.this.setSchema(schema);
    		}
    		
			@Override
			public Object process(Object instance) {
				return Marshaller.this.process(instance);
			}      	
        }

		public void setSetting(Map<String, Object> setting) {
			this.setting = setting;
		}

		public void setAdaptor(Map<Object, XmlAdapter> adaptor) {
			this.adaptor = adaptor;
		}

		public void setAttachment(AttachmentMarshaller attachment) {
			this.attachment = attachment;
		}

		public void setHandler(ValidationEventHandler handler) {
			this.handler = handler;
		}

		public void setListener(javax.xml.bind.Marshaller.Listener listener) {
			this.listener = listener;
		}

		public void setSchema(Object schema) {
			if (schema != null) if (schema instanceof Schema) this.schema = (Schema) schema;
			else try {
				if (schema instanceof Source) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((Source) schema);
				else if (schema instanceof Source[]) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((Source[]) schema);
				else if (schema instanceof URL) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((URL) schema);
				else if (schema instanceof File) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema((File) schema);
				else if (schema instanceof InputStream) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource((InputStream) schema));
				else if (schema instanceof Reader) this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource((Reader) schema));
				else this.schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(new StreamSource(new StringReader(schema.toString())));
			} catch (SAXException e) {
				log.error("JaxbUtil: schema error.", e);
			}
		}
		
		@Override
		public synchronized Object process(Object instance) {
			try{
		    	javax.xml.bind.Marshaller marshaller = jc.createMarshaller();
		        if(setting != null && setting.size() > 0) for(Entry<String, Object> s : setting.entrySet())
		        	marshaller.setProperty(s.getKey(), s.getValue());
		        if(adaptor != null && adaptor.size() > 0) for(Entry<Object, XmlAdapter> entry : adaptor.entrySet())
		        	if (entry.getKey() instanceof Class) marshaller.setAdapter((Class)entry.getKey(), entry.getValue());
		        	else if (entry.getKey() instanceof String) try {
		        		marshaller.setAdapter((Class)classLoader.loadClass((String)entry.getKey()), entry.getValue());
					} catch (ClassNotFoundException e) {
						if (entry.getValue() != null) marshaller.setAdapter(entry.getValue());
					} else if (entry.getValue() != null) marshaller.setAdapter(entry.getValue());
		        if (schema != null) marshaller.setSchema(schema);
		        if (attachment != null) marshaller.setAttachmentMarshaller(attachment);
		        if (handler != null) marshaller.setEventHandler(handler);
		        if (listener != null) marshaller.setListener(listener);
		        this.marshaller = marshaller;
			} catch (Exception e) {
				log.error("JaxbUtil: create marshal error.", e);
			}
			return instance;
		}
		
		@Override
		public void marshal(Object obj, Result result) throws IOException, XmlMappingException {
			try{
				if (JaxbUtil.this.ver == 0) synchronized(JaxbUtil.this) {
					if (JaxbUtil.this.ver == 0) JaxbUtil.this.process(true);
				}
				if (JaxbUtil.this.ver != ver) synchronized(this){
					if (JaxbUtil.this.ver != ver) {
						process(null);
						ver = JaxbUtil.this.ver;
					}
				}
				if(result instanceof StreamResult) {
					if(((StreamResult)result).getWriter() != null) marshaller.marshal(obj, ((StreamResult)result).getWriter());
					else if(((StreamResult)result).getOutputStream() != null) marshaller.marshal(obj, ((StreamResult)result).getOutputStream());
				} else marshaller.marshal(obj, result);
			}catch(JAXBException e){
				throw convertJaxbException(e);
			}
		}
		
		@Override
		public boolean supports(Class<?> clazz) {
			try{
				return ji.isElement(clazz.newInstance());
			}catch(Exception e){
				throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, "JaxbUtil: supports fail.", e);
			}
		}
    }

	protected XmlMappingException convertJaxbException(JAXBException ex) {
		if (ex instanceof ValidationException) return new ValidationFailureException("JAXB validation exception", ex);
		else if (ex instanceof MarshalException) return new MarshallingFailureException("JAXB marshalling exception", ex);
		else if (ex instanceof UnmarshalException) return new UnmarshallingFailureException("JAXB unmarshalling exception", ex);
		else return new UncategorizedMappingException("Unknown JAXB exception", ex);
	}

	@Override
	public synchronized JAXBContext process(Object instance) {
		try {
			if (instance == null) return jc;
	    	StringBuilder sb = new StringBuilder();
	    	if(contextPath != null) sb.append(contextPath);
	    	if(instance instanceof String) sb.append(':').append(instance);
	    	else if (instance instanceof Map) properties = (Map<String, Object>)instance;
	    	if(sb.length() == 0) sb.append("java.lang");
	    	String contextPath = sb.toString();
	    	jc = null;
	    	if (jcf != null) try {
	    		jc = (JAXBContext) jcf.invoke(null, contextPath, classLoader, Boolean.TRUE.equals(instance)  || propertyProcessor == null ? properties : propertyProcessor.process(properties));
	    	} catch (Exception e) {
	    		log.error("JaxbUtil: new context can not be loaded.", e);
	    	}
	    	if (jc == null) jc = JAXBContext.newInstance(contextPath, classLoader, Boolean.TRUE.equals(instance)  || propertyProcessor == null ? properties : propertyProcessor.process(properties));
	    	ji = jc.createJAXBIntrospector();
	    	ver++;
	    	return jc;
		} catch(JAXBException e){
			throw convertJaxbException(e);
		} 
	}
}
