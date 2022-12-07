package net.yeeyaa.eight.osgi.loader;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.core.storage.Storage.Method;
import net.yeeyaa.eight.osgi.loader.internal.Clause;
import net.yeeyaa.eight.osgi.loader.internal.VersionRange;
import net.yeeyaa.eight.osgi.runtime.BundleProtocol;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.FrameworkListener;
import org.osgi.framework.Version;
import org.osgi.framework.startlevel.BundleStartLevel;
import org.osgi.framework.startlevel.FrameworkStartLevel;
import org.osgi.framework.wiring.BundleRevision;
import org.osgi.framework.wiring.FrameworkWiring;
import org.osgi.service.url.AbstractURLStreamHandlerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanNameAware;

//2015-08-18 the last nail. Jessler Wang
public class BundleProcessor extends AbstractURLStreamHandlerService implements IProcessor<Collection<IExtendable<Object>>, Collection<IExtendable<Object>>>, BeanNameAware,  BundleListener{
	protected final Logger log;
	protected final static char[] hexArray = "0123456789ABCDEF".toCharArray();
	protected final HashMap<String, Object[]> currentBundles = new HashMap<String, Object[]>();
	protected final HashSet<Bundle> delayedStart = new HashSet<Bundle>();
	protected final AtomicBoolean stateChanged = new AtomicBoolean();
	protected Boolean refresh = true; //true: start and refresh. null: refresh but not start, false: do not refresh nor start
	protected Boolean startTransient = false;
	protected Boolean startActivationPolicy = true;
	protected Boolean cache = false; //true: cache jar bytes, false: cache IExtendable<Object>
	protected Boolean checksum = true;
	protected Integer startLevel = 0;
	protected Long max = 0L;
	protected BundleContext context;
	protected Boolean fragmentScope; //null: for managed, true: for all, false: for none;
	protected Boolean optionalScope; //null: for managed, true: for all, false: for none;
	protected String handlerName;
	protected String region;	
	protected IProcessor<InputStream, Manifest> manifest;
	
	public BundleProcessor() {
		log = LoggerFactory.getLogger(BundleProcessor.class);
	}

	public BundleProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BundleProcessor.class) : log;
	}
	
	@Override
	public void setBeanName(String name) {
		if (handlerName == null) handlerName = name;
	}
	
	public void setManifest(IProcessor<InputStream, Manifest> manifest) {
		this.manifest = manifest;
	}

	public void setHandlerName(String handlerName) {
		this.handlerName = handlerName;
	}

	public void setMax(Long max) {
		if (max != null && max > 0) this.max = max;
	}

	public void setChecksum(Boolean checksum) {
		this.checksum = checksum;
	}

	public void setCache(Boolean cache) {
		if (cache != null) this.cache = cache;
	}

	public void setRefresh(Boolean refresh) {
		this.refresh = refresh;
	}

	public void setFragmentScope(Boolean fragmentScope) {
		this.fragmentScope = fragmentScope;
	}

	public void setOptionalScope(Boolean optionalScope) {
		this.optionalScope = optionalScope;
	}

	public void setContext(BundleContext context) {
		this.context = context;
	}

	public void setStartTransient(Boolean startTransient) {
		if(startTransient != null) this.startTransient = startTransient;
	}

	public void setStartActivationPolicy(Boolean startActivationPolicy) {
		if(startActivationPolicy != null) this.startActivationPolicy = startActivationPolicy;
	}

	public void setStartLevel(Integer startLevel) {
		if(startLevel > 0) this.startLevel = startLevel;
	}

	public void setRegion(String region) {
		this.region = region;
	}

	@PostConstruct
	public void initialize(){
		StringBuilder protocol = new StringBuilder();
		protocol.append(BundleProtocol.PROTOCOL).append(context.getBundle().getBundleId());
		if (region != null) protocol.append(region);
		protocol.append("://").append(handlerName).append('/');
		region = protocol.toString();
		context.addBundleListener(this);
	}

	@PreDestroy
	public void destroy() {
		context.removeBundleListener(this);
	}
	
	@Override
	public synchronized Collection<IExtendable<Object>> process(Collection<IExtendable<Object>> storages) {
		BundleContext context = this.context.getBundle(0).getBundleContext();
		if(storages != null && storages.size() > 0) {
	        List<IExtendable<Object>> deleted = new LinkedList<IExtendable<Object>>();
	        List<IExtendable<Object>> modified = new LinkedList<IExtendable<Object>>();
	        List<IExtendable<Object>> created = new LinkedList<IExtendable<Object>>();
	        HashSet<Bundle> bundles = new HashSet<Bundle>();
	        HashMap<Long, String> sum = new HashMap<Long, String>();
	        if (checksum) {
	        	String sums = System.getProperty(region);
	        	if (sums != null) for(String p : sums.split("\\|\\|")){
            		String[] ps = p.split("\\|");
            		if(ps.length > 1) sum.put(Long.parseLong(ps[0]), ps[1]);
	        	}
	        }
			for(IExtendable<Object> storage : storages) if(storage != null) if (Boolean.TRUE.equals(storage.extend(Method.exists))) if (currentBundles.containsKey(storage.extend(Method.key))) modified.add(storage);
			else created.add(storage);
			else deleted.add(storage);
			for (IExtendable<Object> storage : deleted) {
				Object[] bundleinfo = currentBundles.remove(storage.extend(Method.key));
				if (bundleinfo != null && bundleinfo[0] instanceof Long && ((Long)bundleinfo[0]) != 0) {
					Bundle bundle = context.getBundle((Long)bundleinfo[0]);
					if (checksum) sum.remove((Long)bundleinfo[0]);
					if (bundle != null) try {
						bundles.add(bundle);
						if (Boolean.TRUE.equals(refresh)) delayedStart.remove(bundle);
						bundle.uninstall();
					} catch(Exception e) {
						log.error("BundleProcessor: delete bundle fail.", e);
					}
				}
			} 
			for (IExtendable<Object> storage : modified) {
				Object[] bundleinfo = currentBundles.get(storage.extend(Method.key));
				if (bundleinfo != null && bundleinfo[0] instanceof Long && ((Long)bundleinfo[0]) != 0) {
					Bundle bundle = context.getBundle((Long)bundleinfo[0]);
					InputStream in = null;
					if (bundle != null) try {
						if (Boolean.TRUE.equals(refresh) && (bundle.adapt(BundleRevision.class).getTypes() & BundleRevision.TYPE_FRAGMENT) == 0) bundle.stop(Bundle.STOP_TRANSIENT);
						byte[] jar = null;
						if (cache || checksum) jar = streamToBytes(storage.<InputStream>extend(Method.input));
						in = jar == null ? storage.<InputStream>extend(Method.input) : new ByteArrayInputStream(jar);
						bundle.update(in);
						bundles.add(bundle);
						bundleinfo[1] = cache ? jar : storage;
						if (checksum) {
							String cs = checkSum(jar);
							if (cs != null) sum.put((Long)bundleinfo[0], cs);
						}
					} catch(Exception e) {
						currentBundles.remove(storage.extend(Method.key));
						log.error("BundleProcessor: update bundle fail.", e);
					} finally {
						if (in != null)	try {
							in.close();
						} catch (IOException e) {
							log.error("BundleProcessor: update bundle fail.", e);
						}
					} else {
						created.add(storage);
						currentBundles.remove(storage.extend(Method.key));
						if (checksum) sum.remove((Long)bundleinfo[0]);
					}
				}
			} 
			for (IExtendable<Object> storage : created) {
				InputStream in = null;
				JarInputStream file = null;
				try {
					in = new BufferedInputStream(storage.<InputStream>extend(Method.input));
			        in.mark(262144);
			        Manifest m;
			        if (manifest == null) {
			        	file = new JarInputStream(in);
			        	m = file.getManifest();
			        } else m = manifest.process(in);
			        if(m != null) {
				        String sn = m.getMainAttributes().getValue(Constants.BUNDLE_SYMBOLICNAME);
				        if(sn != null) {
					        String vStr = m.getMainAttributes().getValue(Constants.BUNDLE_VERSION);
					        Version v = vStr == null ? Version.emptyVersion : Version.parseVersion(vStr);
			                in.reset();
							byte[] jar = null;
							if (cache || checksum) jar = streamToBytes(in);
							if (jar != null) in = new ByteArrayInputStream(jar);
					        Object[] value = new Object[2];
					        value[1] = cache ? jar : storage;
					        boolean update = false;
					        for (Bundle bundle : context.getBundles()) if (bundle.getBundleId() != 0 && bundle.getSymbolicName() != null && bundle.getSymbolicName().equals(sn)) {
					            vStr = bundle.getHeaders().get(Constants.BUNDLE_VERSION);
					            Version bv = vStr == null ? Version.emptyVersion : Version.parseVersion(vStr);
					            if (v.equals(bv)) {
					                value[0] = bundle.getBundleId();
					            	String s = null;
					            	String c = null;
					            	if (checksum) try {
					            		c = checkSum(jar);
					            		s = sum.get((Long)value[0]);
					            		if (s == null) {
						            		URLConnection conn = new URL(bundle.getLocation()).openConnection();
						            		conn.connect();
						            		s = checkSum(streamToBytes(conn.getInputStream()));
						            		if (s != null) sum.put((Long)value[0], s);
					            		}
					            	} catch (Exception e) {
					            		log.error("BundleProcessor: url connection fail. " + bundle.getLocation(), e);
					            	}
					            	if (s == null || !s.equals(c)) {
						            	if (Boolean.TRUE.equals(refresh) && (bundle.adapt(BundleRevision.class).getTypes() & BundleRevision.TYPE_FRAGMENT) == 0) bundle.stop(Bundle.STOP_TRANSIENT);
						                bundle.update(in);
										if (checksum && c != null) sum.put((Long)value[0], c);
						                bundles.add(bundle);
					            	}
					                update = true;
					                break;
					            }
					        }
					        if (!update) {
						        Bundle bundle = context.installBundle(region + storage.extend(Method.key), in);
						        if (startLevel != 0) bundle.adapt(BundleStartLevel.class).setStartLevel(startLevel);
				                value[0] = bundle.getBundleId();
								if (checksum) {
									String cs = checkSum(jar);
									if (cs != null) sum.put((Long)value[0], cs);
								}
				                bundles.add(bundle);
				                if (Boolean.TRUE.equals(refresh) && (bundle.adapt(BundleRevision.class).getTypes() & BundleRevision.TYPE_FRAGMENT) == 0) delayedStart.add(bundle);
					        }
					        currentBundles.put(storage.extend(Method.key).toString(), value);
				        }
			        }
				} catch(Exception e) {
					log.error("BundleProcessor: install bundle fail.", e);
				} finally {
					if (file != null) try {
						file.close();
					} catch (Exception e) {
						log.error("BundleProcessor: install bundle fail.", e);
					} else if (in != null)	try {
						in.close();
					} catch (Exception e) {
						log.error("BundleProcessor: install bundle fail.", e);
					}
				}
			}
			if (checksum) {
				StringBuilder sb = new StringBuilder();
				for (Entry<Long, String> entry : sum.entrySet()) sb.append("||").append(entry.getKey()).append('|').append(entry.getValue());
				if (sb.length() > 2) System.setProperty(region, sb.substring(2));
			}
			if (!Boolean.FALSE.equals(refresh) && bundles.size() > 0) {
				HashSet<Bundle> scope = getScopedBundles(context, fragmentScope);
				scope.removeAll(bundles);
		        if (scope.size() > 0) {
					HashSet<Clause> paths = new HashSet<Clause>();
		        	for (Bundle b : bundles) if (b.getState() != Bundle.UNINSTALLED) {
		                String hostHeader = b.getHeaders().get(Constants.FRAGMENT_HOST);
		                if (hostHeader != null) {
		                    Clause[] clauses = Clause.parseHeader(hostHeader);
		                    if (clauses != null && clauses.length > 0) paths.add(clauses[0]);
		                }
		            }
                    for (Bundle hostBundle : scope) for (Clause path : paths) if (hostBundle.getSymbolicName() != null && hostBundle.getSymbolicName().equals(path.getName())) {
                        String ver = path.getAttribute(Constants.BUNDLE_VERSION_ATTRIBUTE);
                        if (ver == null || VersionRange.parseVersionRange(ver).contains(VersionRange.getVersion(hostBundle.getHeaders().get(Constants.BUNDLE_VERSION), true))) {
                        	bundles.add(hostBundle);
                        	break;
                        }
                    }
		        }
				scope = getScopedBundles(context, optionalScope);
				scope.removeAll(bundles);
				if (scope.size() > 0) {
					HashMap<Bundle, List<Clause>> imports = new HashMap<Bundle, List<Clause>>();
			        for (Iterator<Bundle> it = scope.iterator(); it.hasNext();) {
			            Bundle b = it.next();
			            List<Clause> importsList = new LinkedList<Clause>();
			            for (Clause anImport : Clause.parseHeader(b.getHeaders().get(Constants.IMPORT_PACKAGE))) 
			            	if (Constants.RESOLUTION_OPTIONAL.equals(anImport.getDirective(Constants.RESOLUTION_DIRECTIVE))) importsList.add(anImport);
			            if (importsList.isEmpty()) it.remove();
			            else imports.put(b, importsList);
			        }
			        if (scope.size() > 0) {
				        List<Clause> exports = new LinkedList<Clause>();
				        for (Bundle b : bundles) if (b.getState() != Bundle.UNINSTALLED) {
			                String exportsStr = b.getHeaders().get(Constants.EXPORT_PACKAGE);
			                if (exportsStr != null) for (Clause cl : Clause.parseHeader(exportsStr)) exports.add(cl);
			            }
				        for (Iterator<Bundle> it = scope.iterator(); it.hasNext();) {
				            List<Clause> importsList = imports.get(it.next());
				            for (Iterator<Clause> itpi = importsList.iterator(); itpi.hasNext();) {
				                Clause pi = itpi.next();
				                boolean matching = false;
				                for (Clause pe : exports) if (pi.getName().equals(pe.getName())) {
			                        String evStr = pe.getAttribute(Constants.VERSION_ATTRIBUTE);
			                        String ivStr = pi.getAttribute(Constants.VERSION_ATTRIBUTE);
			                        Version exported = evStr != null ? Version.parseVersion(evStr) : Version.emptyVersion;
			                        VersionRange imported = ivStr != null ? VersionRange.parseVersionRange(ivStr) : VersionRange.ANY_VERSION;
			                        if (imported.contains(exported)) {
			                            matching = true;
			                            break;
			                        }
			                    }
				                if (!matching) itpi.remove();
				            }
				            if (importsList.isEmpty()) it.remove();
				        }
			        }
			        bundles.addAll(scope);
				}
		        try {
			        final CountDownLatch latch = new CountDownLatch(1);
			        FrameworkWiring wiring = context.getBundle(0).adapt(FrameworkWiring.class);
			        wiring.refreshBundles(bundles, new FrameworkListener() {
			            public void frameworkEvent(FrameworkEvent event) {
			                latch.countDown();
			            }
			        });
					latch.await();
				} catch (InterruptedException e) {
					log.error("BundleProcessor: refresh bundle fail.", e);
				}
			}
		}
		if (Boolean.TRUE.equals(refresh) && stateChanged.get()) {
            int options = (startTransient ? Bundle.START_TRANSIENT : 0) | (startActivationPolicy ? Bundle.START_ACTIVATION_POLICY : 0);
	        int systemStartLevel = context.getBundle(0).adapt(FrameworkStartLevel.class).getStartLevel();
	        for (Object[] bundleinfo : currentBundles.values()) if (bundleinfo != null && bundleinfo[0] instanceof Long && ((Long)bundleinfo[0]) != 0) {
                Bundle bundle = context.getBundle((Long)bundleinfo[0]);
                if (bundle != null && (bundle.adapt(BundleRevision.class).getTypes() & BundleRevision.TYPE_FRAGMENT) == 0 && bundle.getState() != Bundle.STARTING && bundle.getState() != Bundle.ACTIVE 
                		&& (startTransient || bundle.adapt(BundleStartLevel.class).isPersistentlyStarted()) && systemStartLevel >= bundle.adapt(BundleStartLevel.class).getStartLevel()) try {
                    bundle.start(options);
                } catch (Exception e) {
					log.error("BundleProcessor: start bundle fail.", e);
                }
            }
	        for (Iterator<Bundle> it = delayedStart.iterator(); it.hasNext();) {
	        	Bundle bundle = it.next();
	        	if (bundle.getState() != Bundle.UNINSTALLED && bundle.getState() != Bundle.STARTING && bundle.getState() != Bundle.ACTIVE && systemStartLevel >= bundle.adapt(BundleStartLevel.class).getStartLevel()) try {
	        		bundle.start(options);
	        		it.remove();
	        	} catch (Exception e) {
					log.error("BundleProcessor: start bundle fail.", e);
                } else it.remove();
	        	
	        }
	        stateChanged.set(false);
		}
		return storages;
	}
	
	protected byte[] streamToBytes(InputStream is) {
		try{
			ByteArrayOutputStream buf = new ByteArrayOutputStream();
			int nRead;
			byte[] data = new byte[8192];
			Long count = 0L;
			while ((nRead = is.read(data, 0, data.length)) != -1) {
				if (max > 0) {
					count += nRead;
					if (count > max) throw new IOException("Data overflow. The maxLength is " + max);
				}
				buf.write(data, 0, nRead);
			}
			buf.flush();
			return buf.toByteArray();
		} catch(Exception e) {
			log.error("BundleProcessor: get storage content fail.", e);			
		} finally {
			if (is != null) try{
				is.close();
			} catch (Exception e) {
				log.error("BundleProcessor: get storage content fail.", e);
			}
		}
		return null;
	}
	
	protected String checkSum(byte[] bytes){ 
	    try { 
	    	bytes = MessageDigest.getInstance("MD5").digest(bytes); 
		    char[] hexChars = new char[bytes.length * 2];
		    for ( int j = 0; j < bytes.length; j++ ) {
		        int v = bytes[j] & 0xFF;
		        hexChars[j * 2] = hexArray[v >>> 4];
		        hexChars[j * 2 + 1] = hexArray[v & 0x0F];
		    }
		    return new String(hexChars);
	    } catch (NoSuchAlgorithmException e) { 
			log.error("BundleProcessor : generate checksum error.", e);
	    	return null; 
	    } 
	}
	
    protected HashSet<Bundle> getScopedBundles(BundleContext context, Boolean scope) {
    	HashSet<Bundle> bundles = new HashSet<Bundle>();
		if (scope == null) for (Object[] bundleinfo : currentBundles.values()) {
            Bundle bundle = context.getBundle((Long)bundleinfo[0]);
            if (bundle != null) bundles.add(bundle);
        } else if (scope) bundles.addAll(Arrays.asList(context.getBundles()));
		bundles.remove(context.getBundle(0));
		return bundles;
    }
    
	@Override
	public void bundleChanged(BundleEvent event) {
		int type = event.getType();
        if (type == BundleEvent.INSTALLED || type == BundleEvent.RESOLVED || type == BundleEvent.UNINSTALLED || type == BundleEvent.UNRESOLVED || type == BundleEvent.UPDATED) stateChanged.set(true);
	}
	
	@Override
	public URLConnection openConnection(URL url) throws IOException {
		if (url != null) {
			String name = url.toExternalForm();
			if (name != null) {
				name = name.substring(region.length());
				Object[] os = currentBundles.get(name);
				final Object data = os == null ? null : os[1];
				if (data instanceof byte[]) return new URLConnection(url){      	 
		    	    public void connect() {} 
		       	 
		    	    public InputStream getInputStream() throws IOException { 
		    	        return new ByteArrayInputStream((byte[]) data);
		    	    }
		        }; else if (data instanceof IExtendable) return new URLConnection(url){      	 
		    	    public void connect() {} 
		       	 
		    	    public InputStream getInputStream() throws IOException { 
		    	        return ((IExtendable<Object>)data).<InputStream>extend(Method.input);
		    	    }
		        }; 
			}
		}
		throw new IOException("BundleStreamHadnler: no such resource." + url);
	}
	
	public class BundleStreamHadnler implements IProcessor<URL, URLConnection>{
		@Override
		public URLConnection process(URL url) {
			try {
				openConnection(url);
			} catch (Exception e) {
				log.error("BundleStreamHadnler: no such resource.", e);
			}
			return null;
		}
	}
}
