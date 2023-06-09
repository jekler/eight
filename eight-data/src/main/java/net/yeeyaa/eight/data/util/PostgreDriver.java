package net.yeeyaa.eight.data.util;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Properties;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.postgresql.Driver;
import org.postgresql.PGConnection;
import org.postgresql.util.PGobject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PostgreDriver extends Driver {
	protected final static Logger log = LoggerFactory.getLogger(PostgreDriver.class);
	protected final Pattern pattern = Pattern.compile("[\\w.$]*[.$]type_(\\d+)[\\w.$]*");
	protected Type type;
	protected IBiProcessor<String, Object, Object> bi;
	protected String classname;
	
	protected class Type implements IProcessor<Object, Map<String, Class<? extends PGobject>>> {
		protected volatile int resume;
		protected volatile Map<String, Class<? extends PGobject>> type = new HashMap<String, Class<? extends PGobject>>();

		@Override
		public Map<String, Class<? extends PGobject>> process(Object instance) {
			if (resume >= 0) {
				Object t = bi.perform(classname, "type");
				if (t instanceof Map && ((Map)t).size() > 0) {
					Map type = (Map) t;	
					Object cl = bi.perform(classname, "classloader");
					ClassLoader classloader = cl instanceof ClassLoader ? (ClassLoader) cl : (ClassLoader) bi;
					for (Object key : type.keySet().toArray()) {
						Object value = type.get(key);
						if (value == null) type.remove(key);
						else if (value instanceof Class) {
							if (!PGobject.class.isAssignableFrom((Class)value)) type.remove(key);
						} else try {
							value = classloader.loadClass((String)value);
							if (PGobject.class.isAssignableFrom((Class)value)) type.put((String)key, value);
							else type.remove(key);
						} catch (Exception e) {
							type.remove(key);
							log.error("PostgreDriver: load class failed", e);
						}
					}
					if (type.size() > 0) this.type = (Map<String, Class<? extends PGobject>>)type;
				}
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (t != null) resume = -1;
					break;
					case 2: if (type.size() > 0) resume = -1;
				}
			}
			return type;
		}
	}
	
	public PostgreDriver() {
		ClassLoader classloader = getClass().getClassLoader();
		if (classloader instanceof IBiProcessor) {
			bi = (IBiProcessor<String, Object, Object>) classloader;
			classname = getClass().getName();
			Matcher matcher = pattern.matcher(classname);
			if(matcher.find()) try {
				type = new Type();
				type.resume = Integer.parseInt(matcher.group(1));
			} catch (Exception e) {
				log.error("PostgreDriver: init param error", e);
			}
		}
	}
	
	@Override
	public Connection connect(String url, Properties info) throws SQLException {
        Connection result = super.connect(url, info);
        if (this.type != null) {
	        Map<String, Class<? extends PGobject>> type = this.type.process(null);
			if (result instanceof PGConnection && type.size() > 0) for (Entry<String, Class<? extends PGobject>> entry : type.entrySet()) try{
				((PGConnection)result).addDataType(entry.getKey(), entry.getValue());
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS);
			}
        }
		return result;
	}
}
