package net.yeeyaa.eight.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLConnection;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IThing;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.storage.Storage;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformAdaptor extends URLConnection implements Comparator<Object[]>, IProcessor<Object[], Object>, IExtendable<Object> {
	protected final Logger log;
	protected static final Pattern th = Pattern.compile("[\\w.$]*[.$]th_(\\d+)[\\w.$]*");
	protected static final Pattern ex = Pattern.compile("[\\w.$]*[.$]ex_(\\d+)[\\w.$]*");
	protected static final Pattern mp = Pattern.compile("[\\w.$]*[.$]map_(\\d+)[\\w.$]*");
	protected static final Pattern tr = Pattern.compile("[\\w.$]*[.$]tri_(\\d+)[\\w.$]*");
	protected static final Pattern bi = Pattern.compile("[\\w.$]*[.$]bi_(\\d+)[\\w.$]*");
	protected static final Pattern pr = Pattern.compile("[\\w.$]*[.$]processor_(\\d+)[\\w.$]*");
	protected static final Pattern po = Pattern.compile("[\\w.$]*[.$]proxy_(\\d+)[\\w.$]*");
	protected static final Pattern co = Pattern.compile("[\\w.$]*[.$]connection_(\\d+)[\\w.$]*");
	protected static final Pattern cm = Pattern.compile("[\\w.$]*[.$]comparator_(\\d+)[\\w.$]*");
	protected IExtendable<Object> extension;
	protected IThing thing;
	protected Map<String,List<String>> map; 
	protected ITriProcessor<Object, Object, Object, Object> triprocessor;
	protected IBiProcessor<Object, Object, Object> biprocessor;
	protected IProcessor<Object, Object> processor;
	protected IProcessor<Object[], Object> proxy;
	protected Comparator<Object[]> comparator;
	protected URLConnection connection;
	protected IBiProcessor<String, Object, Object> loader;
	protected String classname;
	protected Boolean writable;
	
	public PlatformAdaptor() {
		this(null);
	}
	
	public PlatformAdaptor(Logger log) {
		super(null);
		this.log = log == null ? LoggerFactory.getLogger(PlatformAdaptor.class) : log;
		ClassLoader classloader = getClass().getClassLoader();
		if (classloader instanceof IBiProcessor && getClass() != PlatformAdaptor.class) {
			classname = getClass().getName();
			loader = (IBiProcessor<String, Object, Object>) classloader;
			Matcher matcher = pr.matcher(classname);
			if(matcher.find()) try {
				processor = new Processor<Object, Object>(Integer.parseInt(matcher.group(1)), "processor");
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = po.matcher(classname);
			if(matcher.find()) try {
				proxy = new Processor<Object[], Object>(Integer.parseInt(matcher.group(1)), "proxy");
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = ex.matcher(classname);
			if(matcher.find()) try { 
				extension = new Extension(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = th.matcher(classname);
			if(matcher.find()) try { 
				thing = new Thing(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = mp.matcher(classname);
			if(matcher.find()) try { 
				map = new Maproxy(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = bi.matcher(classname);
			if(matcher.find()) try { 
				biprocessor = new BiProcessor(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = tr.matcher(classname);
			if(matcher.find()) try { 
				triprocessor = new TriProcessor(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = cm.matcher(classname);
			if(matcher.find()) try { 
				comparator = new Compare(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
			matcher = co.matcher(classname);
			if(matcher.find()) try { 
				connection = new Connection(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("PlatformAdaptor: init param error", e);
			}
		}
	}
	
	protected class Processor<T, R> implements IProcessor<T, R> {
		protected volatile int resume; 
		protected volatile IProcessor<T, R> processor;
		protected String key;
		
		public Processor(int resume, String key) {
			this.resume = resume;
			this.key = key;
		}

		@Override
		public R process(T instance) {
			if (resume >= 0) {
				Object processor = loader.perform(classname, key);
				if (processor instanceof IProcessor) this.processor = (IProcessor<T, R>) processor;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (processor != null) resume = -1;
					break;
					case 2: if (this.processor != null) resume = -1;
				}
			}
			if (processor == null) return null;
			else return processor.process(instance);
		}
	}
	
	protected class TriProcessor implements ITriProcessor<Object, Object, Object, Object> {
		protected volatile int resume; 
		protected volatile ITriProcessor<Object, Object, Object, Object> tri;
		
		public TriProcessor(int resume) {
			this.resume = resume;
		}

		@Override
		public Object operate(Object first, Object second, Object third) {
			if (resume >= 0) {
				Object tri = loader.perform(classname, "tri");
				if (tri instanceof ITriProcessor) this.tri = (ITriProcessor<Object, Object, Object, Object>) tri;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (tri != null) resume = -1;
					break;
					case 2: if (this.tri != null) resume = -1;
				}
			}
			if (tri == null) return null;
			else return tri.operate(first, second, third);
		}
	}
	
	protected class BiProcessor implements IBiProcessor<Object, Object, Object> {
		protected volatile int resume; 
		protected volatile IBiProcessor<Object, Object, Object> bi;
		
		public BiProcessor(int resume) {
			this.resume = resume;
		}

		@Override
		public Object perform(Object first, Object second) {
			if (resume >= 0) {
				Object bi = loader.perform(classname, "bi");
				if (bi instanceof IBiProcessor) this.bi = (IBiProcessor<Object, Object, Object>) bi;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (bi != null) resume = -1;
					break;
					case 2: if (this.bi != null) resume = -1;
				}
			}
			if (bi == null) return null;
			else return bi.perform(first, second);
		}
	}
	
	protected class Compare implements Comparator<Object[]> {
		protected volatile int resume; 
		protected volatile Comparator<Object[]> comparator;
		
		public Compare(int resume) {
			this.resume = resume;
		}

		@Override
		public int compare(Object[] ret, Object[] para) {
			if (resume >= 0) {
				Object comparator = loader.perform(classname, "comparator");
				if (comparator instanceof Comparator) this.comparator = (Comparator<Object[]>) comparator;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (comparator != null) resume = -1;
					break;
					case 2: if (this.comparator != null) resume = -1;
				}
			}
			if (comparator == null) return -1;
			else return comparator.compare(ret, para);
		}
	}

	protected class Thing implements IThing {
		protected volatile int resume; 
		protected volatile IThing thing;
		
		public Thing(int resume) {
			this.resume = resume;
		}

		protected void init() {
			if (resume >= 0) {
				Object th = loader.perform(classname, "th");
				if (th instanceof IThing) this.thing = (IThing) th;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (th != null) resume = -1;
					break;
					case 2: if (this.thing != null) resume = -1;
				}
			}
		}

		@Override
		public <L> L present(Class<L> clazz) {
			init();
			if (thing == null) return null;
			else return thing.present(clazz);
		}
	}
	
	protected class Extension implements IExtendable<Object> {
		protected volatile int resume; 
		protected volatile IExtendable<Object> extension;
		
		public Extension(int resume) {
			this.resume = resume;
		}

		protected void init() {
			if (resume >= 0) {
				Object ex = loader.perform(classname, "ex");
				if (ex instanceof IExtendable) this.extension = (IExtendable) ex;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (ex != null) resume = -1;
					break;
					case 2: if (this.extension != null) resume = -1;
				}
			}
		}

		@Override
		public Collection<Object> methods() {
			init();
			if (extension == null) return null;
			else return extension.methods();
		}

		@Override
		public <N> N extend(Object method) {
			init();
			if (extension == null) return null;
			else return extension.extend(method);
		}
	}
	
	protected class Connection extends URLConnection {
		protected volatile int resume; 
		protected volatile URLConnection connection;
		
		public Connection(int resume) {
			super(null);
			this.resume = resume;
		}

		protected void init() {
			if (resume >= 0) {
				Object connection = loader.perform(classname, "connection");
				if (connection instanceof URLConnection) this.connection = (URLConnection) connection;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (connection != null) resume = -1;
					break;
					case 2: if (this.connection != null) resume = -1;
				}
			}
		}

		@Override
		public InputStream getInputStream() throws IOException {
			init();
			if (connection == null) return null;
			else return connection.getInputStream();
		}

		@Override
		public OutputStream getOutputStream() throws IOException {
			init();
			if (connection == null) return null;
			else return connection.getOutputStream();
		}

		@Override
		public long getLastModified() {
			init();
			if (connection == null) return -1L;
			else return connection.getLastModified();
		}

		@Override
		public Map<String, List<String>> getHeaderFields() {
			init();
			if (connection == null) return Collections.EMPTY_MAP;
			else return connection.getHeaderFields();
		}

		@Override
		public Object getContent() throws IOException {
			init();
			if (connection == null) return null;
			else return connection.getContent();
		}

		@Override
		public boolean getDoInput() {
			init();
			if (connection == null) return false;
			else return connection.getDoInput();
		}

		@Override
		public boolean getDoOutput() {
			init();
			if (connection == null) return false;
			else return connection.getDoOutput();
		}
		
		@Override
		public void connect() throws IOException {}
	}
	
	protected class Maproxy implements Map {
		protected volatile int resume; 
		protected volatile Map map;
		
		public Maproxy(int resume) {
			this.resume = resume;
		}

		protected void init() {
			if (resume >= 0) {
				Object map = loader.perform(classname, "map");
				if (map instanceof Map) this.map = (Map) map;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (map != null) resume = -1;
					break;
					case 2: if (this.map != null) resume = -1;
				}
			}
		}

		@Override
		public int size() {
			init();
			if (map == null) return 0;
			else return map.size();
		}

		@Override
		public boolean isEmpty() {
			init();
			if (map == null) return true;
			else return map.isEmpty();
		}

		@Override
		public boolean containsKey(Object key) {
			init();
			if (map == null) return false;
			else return map.containsKey(key);
		}

		@Override
		public boolean containsValue(Object value) {
			init();
			if (map == null) return false;
			else return map.containsValue(value);
		}

		@Override
		public Object get(Object key) {
			init();
			if (map == null) return null;
			else return map.get(key);
		}

		@Override
		public Object put(Object key, Object value) {
			init();
			if (map == null) return null;
			else return map.put(key, value);
		}

		@Override
		public Object remove(Object key) {
			init();
			if (map == null) return null;
			else return map.remove(key);
		}

		@Override
		public void putAll(Map m) {
			init();
			if (map != null) map.putAll(m);
		}

		@Override
		public void clear() {
			init();
			if (map != null) map.clear();
		}

		@Override
		public Set keySet() {
			init();
			if (map == null) return Collections.EMPTY_SET;
			else return map.keySet();
		}

		@Override
		public Collection values() {
			init();
			if (map == null) return Collections.EMPTY_SET;
			else return map.values();
		}

		@Override
		public Set entrySet() {
			init();
			if (map == null) return Collections.EMPTY_SET;
			else return map.entrySet();
		}
	}
	
	public void setWritable(Boolean writable) {
		this.writable = writable;
	}
	
	public void setProxy(IProcessor<Object[], Object> proxy) {
		this.proxy = proxy;
	}

	public void setComparator(Comparator<Object[]> comparator) {
		this.comparator = comparator;
	}

	public void setConnection(URLConnection connection) {
		this.connection = connection;
	}

	public void setTri(ITriProcessor<Object, Object, Object, Object> tri) {
		this.triprocessor = tri;
	}

	public void setBi(IBiProcessor<Object, Object, Object> bi) {
		this.biprocessor = bi;
	}

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setMap(Map<String, List<String>> map) {
		this.map = map;
	}

	public void setExtension(IExtendable<Object> extension) {
		this.extension = extension;
	}

	public void setThing(IThing thing) {
		this.thing = thing;
	}

	@Override
	public void connect() throws IOException {}
	
    public Map<String,List<String>> getHeaderFields() {
        return map == null ? Collections.EMPTY_MAP : map;
    }

	@Override
	public long getLastModified() {
		return (Long) extension.extend(Method.modified);
	}

	@Override
	public Object getContent() throws IOException {
		return extension.extend(Method.key) == null ? extension.extend(Method.list) :  extension.extend(Method.key);
	}

	@Override
	public boolean getDoInput() {
		return (Boolean) extension.extend(Method.exists);
	}

	@Override
	public boolean getDoOutput() {
		return !Boolean.FALSE.equals(writable) && Boolean.TRUE.equals(extension.extend(Method.exists));
	}
	
	@Override
	public InputStream getInputStream() throws IOException {
		return extension.extend(Method.input);
	}

	@Override
	public OutputStream getOutputStream() throws IOException {
		return extension.extend(Method.output);
	}

	@Override
	public Collection<Object> methods() {
		if (extension == null) return Storage.methods;
		else return extension.methods();
	}

	@Override
	public <N> N extend(Object object) {
		if (extension != null) return extension.extend(object);
		else if (object != null && connection != null) try {
			Object method = object instanceof Method ? object : Storage.methods.process(object);
			if (method!= null) switch((Method) method) {
				case input : return (N) connection.getInputStream();
				case output : return (N) connection.getOutputStream();
				case exists : return (N) new Boolean(connection.getDoOutput());
				case modified : return (N) new Long(connection.getLastModified());
				case key : case list : return (N) connection.getContent();
			}
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_IO, e.getMessage());
		}
		return null;
	}
	
	@Override
	public int compare(Object[] ret, Object[] para) {
		Object r;
		if (comparator != null) return comparator.compare(ret, para);
		else if (proxy != null) r = proxy.process(para);
		else if (para == null || para.length < 1) return -1;
		else if (para.length > 2) if (triprocessor == null) return 1;
		else r = triprocessor.operate(para[0], para[1], para[2]);
		else if (para.length > 1) if (biprocessor == null) return 1;
		else r = biprocessor.perform(para[0], para[1]);
		else if (thing != null && para[0] instanceof Class) r = thing.present((Class<?>) para[0]);
		else if (processor == null) return 1;
		else r = processor.process(para[0]);
		if (ret != null && ret.length > 0) ret[0] = r;
		return 0;		
	}

	@Override
	public Object process(Object[] instance) {
		Object[] ret = new Object[1];
		if (compare(ret, instance) == 0) return ret[0];
		else return null;
	}
}
