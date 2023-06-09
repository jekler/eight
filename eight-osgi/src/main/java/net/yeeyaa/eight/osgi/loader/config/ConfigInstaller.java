package net.yeeyaa.eight.osgi.loader.config;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.util.Comparator;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.osgi.runtime.BundleError;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.cm.ConfigurationEvent;
import org.osgi.service.cm.ConfigurationListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ConfigInstaller implements ConfigurationListener, IProcessor<Object, String>, IBiProcessor<String, Boolean[], byte[]>{
	protected Logger log;
    protected ConfigurationAdmin configAdmin;
    protected String region;
    protected String version;
    protected Comparator<Object[]> processor;
    protected ServiceRegistration<ConfigurationListener> registration;
    protected ConcurrentHashMap<String, IExtendable<Object>> map;

	public ConfigInstaller() {}

	public ConfigInstaller(BundleContext context, Object configAdmin, Boolean listen, String region, String version, Logger log) {
    	this.log = log == null ? LoggerFactory.getLogger(ConfigInstaller.class) : log;
        if(configAdmin instanceof ConfigurationAdmin) this.configAdmin = (ConfigurationAdmin)configAdmin;
        else throw new PlatformException(BundleError.SERVICE_NOT_EXIST);
        if(configAdmin instanceof Comparator) processor = (Comparator<Object[]>)configAdmin;
        this.region = region == null ? "configinstaller.resource.key" : "configinstaller.resource." + region + ".key";
        this.version = version == null ? "version" : version;
        if(Boolean.TRUE.equals(listen)) {
        	map = new ConcurrentHashMap<String, IExtendable<Object>>();
        	registration = context.registerService(ConfigurationListener.class, this, null);
        }
    }

    public synchronized void destroy() {
        if(registration !=  null) registration.unregister();
        registration = null;
    }
    
	@Override
    public void configurationEvent(ConfigurationEvent configurationEvent) {
        if (configurationEvent.getType() == ConfigurationEvent.CM_UPDATED) try{
            Configuration config = configAdmin.getConfiguration(configurationEvent.getPid(), null);
            Dictionary<String, Object> dict = config.getProperties();
            if(dict != null){
            	Object k = dict.get(region);
            	if(k != null){
		            IExtendable<Object> storage = map.get(k);
		            if(storage != null) {
		                Hashtable<String, Object> props = new Hashtable<String, Object>();
		                for(Enumeration<String> e  = dict.keys(); e.hasMoreElements();) {
		                    String key = e.nextElement();
		                    if(!Constants.SERVICE_PID.equals(key) && !ConfigurationAdmin.SERVICE_FACTORYPID.equals(key) && !region.equals(key)) props.put(key, dict.get(key));
		                }
		                OutputStream out = storage.extend(Method.output);
		                try {
		                    ConfigHandler.write(out, props);
		                } finally {
		                    out.close();
		                }
		            }
            	}
            }
        } catch (Exception e) {
        	log.error("ConfigInstaller: unable to save configuration.", e);
        }
    }
	
	protected Boolean compare(Object left, Object right){
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

	public void setConfig(IExtendable<Object> storage) throws Exception {
        String[] id;
        String name = storage.extend(Method.key).toString();
        String factory = name.substring(0, name.lastIndexOf('.'));
        int n = factory.indexOf('-');
        if (n > 0) {
            String pid = factory.substring(n + 1);
            factory = factory.substring(0, n);
            id = new String[] {factory, pid, null};
        } else id = new String[] {factory, null};
        Configuration config = null;
        Configuration[] configurations = configAdmin.listConfigurations("(" + region + "=" + name.replaceAll("[(]", "\\\\(").replaceAll("[)]", "\\\\)").replaceAll("[=]", "\\\\=").replaceAll("[\\*]", "\\\\*") + ")");
        if (configurations != null && configurations.length > 0) config = configurations[0];
        if (Boolean.TRUE.equals(storage.extend(Method.exists))) {
            if(config == null) if (id[1] != null) if(processor != null) {
            	Configuration[] ret = new Configuration[1];
            	if (processor.compare(id, ret) == 0) config = ret[0];
            	else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
            } else config = configAdmin.createFactoryConfiguration(id[0], null);
            else config = configAdmin.getConfiguration(id[0], null);
	        final InputStream in = storage.<InputStream>extend(Method.input);
	        try {
	            final Hashtable<String, Object> ht = ConfigHandler.read(in);
	            Dictionary<String, Object> props = config.getProperties();
                Boolean equals = true;
                Boolean versionChange = false;
                int count = 0;
                if(props != null && props.size() > 0) {
                	versionChange = !compare(props.get(version), ht.get(version));
                	if (versionChange) equals = false;
                	else for(Enumeration<String> e  = props.keys(); e.hasMoreElements();) {
	                    String key = e.nextElement();
	                    if(!Constants.SERVICE_PID.equals(key) && !ConfigurationAdmin.SERVICE_FACTORYPID.equals(key) && !region.equals(key)) {
	                    	count ++;
	                    	if(!compare(props.get(key), ht.get(key))) {
	                    		equals = false;
	                    		break;
	                    	}
	                    }
	                }
                }
                if(!equals || count != ht.size()) {
	                ht.put(region, name);
	                if (versionChange) {
	                	config.delete();
	                	if (id[1] != null) if(processor != null)  {
	                    	Configuration[] ret = new Configuration[1];
	                    	if (processor.compare(id, ret) == 0) config = ret[0];
	                    	else throw new PlatformException(PlatformError.ERROR_PARAMETERS);
	                    }
	                    else config = configAdmin.createFactoryConfiguration(id[0], null);
	                    else config = configAdmin.getConfiguration(id[0], null);
	                } 
	                config.update(ht);
	                if(map != null && storage instanceof IExtendable && ((IExtendable<Object>)storage).extend(Method.output) != null) map.put(name, (IExtendable<Object>)storage);
	            }
	        } finally {
	            in.close();
	        }
        } else {
        	if(map != null) map.remove(name);
        	if(config == null) config = configAdmin.getConfiguration(id[0], null);
        	if(config != null) config.delete();
        }
    }
	
	@Override
	public byte[] perform(String content, Boolean[] flag) {
		try {
			return ConfigHandler.decode(content, flag == null ? new Boolean[1] : flag);
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public String process(Object object) {
		try {
			return ConfigHandler.encode(object);
		} catch (Exception e) {
			return null;
		}		
	}
}
