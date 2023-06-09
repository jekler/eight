package net.yeeyaa.eight.osgi.spring;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.wiring.BundleCapability;
import org.osgi.framework.wiring.BundleRevision;
import org.osgi.framework.wiring.BundleWire;
import org.osgi.framework.wiring.BundleWiring;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.UrlResource;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.ReflectionUtils.FieldCallback;
import org.springframework.util.ReflectionUtils.FieldFilter;


public class BundleUtil implements IProcessor<Object, Object> {
	public static final String GET_BUNDLE_CONTEXT_METHOD = "getBundleContext";
	public static final String GET_CONTEXT_METHOD = "getContext";
	public static final char ROUND_BRACKET_CHAR = '(';
	public static final char SQUARE_BRACKET_CHAR = '[';
	public static final char QUOTE_CHAR = '\"';
	public static final char COMMA_CHAR = ',';
	public static final String SEMI_COLON = ";";
	public static final String DOUBLE_QUOTE = "\"";
	public static final String DEFAULT_VERSION = "0.0.0";
	public static final String EMPTY_PREFIX = "";
	public static final String PREFIX_DELIMITER = ":";
	public static final String FOLDER_DELIMITER = "/";
	public static final int PREFIX_TYPE_UNKNOWN = -1;
	public static final int PREFIX_TYPE_NOT_SPECIFIED = 0x00000000;
	public static final int PREFIX_TYPE_BUNDLE_JAR = 0x00000001;
	public static final int PREFIX_TYPE_BUNDLE_SPACE = 0x00000010;
	public static final int PREFIX_TYPE_CLASS_SPACE = 0x00000100;
	public static final int PREFIX_TYPE_CLASS_ALL_SPACE = 0x00000200;
	public static enum Vendor{Equinox, Knopflerfish, Felix, Other}
	public static enum MethodType{getPrefix, getSearchType, isClassPathType, stripPrefix, convertURLArraytoResourceArray, convertURLEnumerationToResourceArray, findUpperFolder,
		getBundleClassPath, getRequireBundle, getHeaderAsTrimmedStringArray, getHeaderWithAttributesAsTrimmedStringArray, parseRequiredBundleString, getPlatformName, 
		isPlatformVendorMatch, getVendor, getBundleContext, matchingResources, matchingEntries, isPattern, getImportedBundles};
	protected MethodType type = MethodType.convertURLArraytoResourceArray;
	protected int options;
	protected IProcessor<Object, Object> processor;

	public void setContext(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setOptions(Integer options) {
		if(options != null) this.options = options;
	}

	public void setType(MethodType type) {
		if(type != null) this.type = type;
	}
	
	@Override
	public Object process(Object instance) {
		if(instance != null) switch(type){
			case getPrefix: return getPrefix(instance.toString());
			case getSearchType: return getSearchType(instance.toString());
			case stripPrefix: return stripPrefix(instance.toString());
			case findUpperFolder: return findUpperFolder(instance.toString());
			case isPattern: return isPattern(instance.toString());
			case parseRequiredBundleString: return parseRequiredBundleString(instance.toString());
			case isClassPathType: if(instance instanceof Integer) return isClassPathType((Integer) instance);
			break;
			case convertURLArraytoResourceArray: if(instance instanceof URL[]) return convertURLArraytoResourceArray((URL[]) instance, processor);
			break;
			case convertURLEnumerationToResourceArray: if(instance instanceof Enumeration) return convertURLEnumerationToResourceArray((Enumeration<URL>) instance, processor);
			break;
			case getBundleClassPath: if(instance instanceof Bundle) return getBundleClassPath((Bundle) instance);
			else return getBundleClassPath(((BundleContext)processor.process(null)).getBundle());
			case getRequireBundle: if(instance instanceof Bundle) return getRequireBundle((Bundle) instance);
			else return getRequireBundle(((BundleContext)processor.process(null)).getBundle());
			case getBundleContext: if(instance instanceof Bundle) return getBundleContext((Bundle) instance);
			else return getBundleContext(((BundleContext)processor.process(null)).getBundle());
			case getImportedBundles: if(instance instanceof Bundle) return getImportedBundles((Bundle) instance);
			else return getImportedBundles(((BundleContext)processor.process(null)).getBundle());
			case getPlatformName: if(instance instanceof BundleContext) return getPlatformName((BundleContext) instance);
			else return getPlatformName(((BundleContext)processor.process(null)));
			case getVendor: if(instance instanceof BundleContext) return getVendor((BundleContext) instance);
			else return getVendor(((BundleContext)processor.process(null)));
			case getHeaderAsTrimmedStringArray: if(instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof String)
				return getHeaderAsTrimmedStringArray((String) ((Object[])instance)[0], ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Bundle ? (Bundle)((Object[])instance)[1] : ((BundleContext)processor.process(null)).getBundle());
			else if (instance instanceof String) return getHeaderAsTrimmedStringArray(instance.toString(), ((BundleContext)processor.process(null)).getBundle());
			break; 
			case getHeaderWithAttributesAsTrimmedStringArray: if(instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof String)
				return getHeaderWithAttributesAsTrimmedStringArray((String) ((Object[])instance)[0], ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Bundle ? (Bundle)((Object[])instance)[1] : ((BundleContext)processor.process(null)).getBundle());
			else if (instance instanceof String) return getHeaderWithAttributesAsTrimmedStringArray(instance.toString(), ((BundleContext)processor.process(null)).getBundle());
			break; 
			case matchingResources: if(instance instanceof String) return matchingResources(instance.toString(), ((BundleContext)processor.process(null)).getBundle(), options);
			else if (instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof String)
				return matchingResources((String) ((Object[])instance)[0], ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Bundle ? (Bundle)((Object[])instance)[1] : ((BundleContext)processor.process(null)).getBundle(), 
						((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Integer ? (Integer)((Object[])instance)[1] : ((Object[])instance).length > 2 && ((Object[])instance)[2] instanceof Integer ? (Integer)((Object[])instance)[2] : options);
			break;
			case matchingEntries: if(instance instanceof String) return matchingEntries(instance.toString(), ((BundleContext)processor.process(null)).getBundle(), options);
			else if (instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof String)
				return matchingEntries((String) ((Object[])instance)[0], ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Bundle ? (Bundle)((Object[])instance)[1] : ((BundleContext)processor.process(null)).getBundle(), 
						((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof Integer ? (Integer)((Object[])instance)[1] : ((Object[])instance).length > 2 && ((Object[])instance)[2] instanceof Integer ? (Integer)((Object[])instance)[2] : options);
			break;
			case isPlatformVendorMatch: if(instance instanceof Object[] && ((Object[])instance).length > 0 && ((Object[])instance)[0] instanceof String)
				return isPlatformVendorMatch((String) ((Object[])instance)[0], ((Object[])instance).length > 1 && ((Object[])instance)[1] instanceof BundleContext ? (BundleContext)((Object[])instance)[1] : ((BundleContext)processor.process(null)));
			else if (instance instanceof String) return isPlatformVendorMatch(instance.toString(), ((BundleContext)processor.process(null)));
		}
		return null;
	}
	
	public static String getPrefix(String path) {
		if (path == null) return EMPTY_PREFIX;
		int index = path.indexOf(PREFIX_DELIMITER);
		return ((index > 0) ? path.substring(0, index + 1) : EMPTY_PREFIX);
	}

	public static Integer getSearchType(String path) {
		Assert.notNull(path);
		int type = PREFIX_TYPE_NOT_SPECIFIED;
		String prefix = getPrefix(path);
		if (!StringUtils.hasText(prefix)) type = PREFIX_TYPE_NOT_SPECIFIED;
		else if (prefix.startsWith(BundleResource.BUNDLE_URL_PREFIX)) type = PREFIX_TYPE_BUNDLE_SPACE;
		else if (prefix.startsWith(BundleResource.BUNDLE_JAR_URL_PREFIX)) type = PREFIX_TYPE_BUNDLE_JAR;
		else if (prefix.startsWith(ResourceLoader.CLASSPATH_URL_PREFIX))  type = PREFIX_TYPE_CLASS_SPACE;
		else if (prefix.startsWith(ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX)) type = PREFIX_TYPE_CLASS_ALL_SPACE;
		else type = PREFIX_TYPE_UNKNOWN;
		return type;
	}

	public static Boolean isClassPathType(Integer type) {
		return (type == PREFIX_TYPE_CLASS_SPACE || type == PREFIX_TYPE_CLASS_ALL_SPACE);
	}

	public static String stripPrefix(String path) {
		int index = path.indexOf(PREFIX_DELIMITER);
		return (index > -1 ? path.substring(index + 1) : path);

	}

	public static Resource[] convertURLArraytoResourceArray(URL[] urls, IProcessor<Object, Object> processor) {
		if (urls == null) return new Resource[0];
		Resource[] res = new Resource[urls.length];
		for (int i = 0; i < urls.length; i++) res[i] = processor == null ? new UrlResource(urls[i]) : (Resource) processor.process(new UrlResource(urls[i]));
		return res;
	}

	public static Resource[] convertURLEnumerationToResourceArray(Enumeration<URL> enm, IProcessor<Object, Object> processor) {
		Set<Resource> resources = new LinkedHashSet<Resource>(4);
		while (enm != null && enm.hasMoreElements())resources.add(processor == null ? new UrlResource(enm.nextElement()) : (Resource) processor.process(new UrlResource(enm.nextElement())));
		return resources.toArray(new Resource[resources.size()]);
	}

	public static String findUpperFolder(String path) {
		if (path.length() < 2) return path;
		String newPath = path;
		if (path.endsWith(FOLDER_DELIMITER)) newPath = path.substring(0, path.length() - 1);
		int index = newPath.lastIndexOf(FOLDER_DELIMITER);
		if (index > 0) return newPath.substring(0, index + 1);
		else return path;
	}
	
	public static String[] getBundleClassPath(Bundle bundle) {
		return getHeaderAsTrimmedStringArray(Constants.BUNDLE_CLASSPATH, bundle);
	}

	public static String[] getRequireBundle(Bundle bundle) {
		return getHeaderWithAttributesAsTrimmedStringArray(Constants.REQUIRE_BUNDLE, bundle);
	}

	public static String[] getHeaderAsTrimmedStringArray(String header, Bundle bundle) {
		if (bundle == null || !StringUtils.hasText(header)) return new String[0];
		String headerContent = (String) bundle.getHeaders().get(header);
		String[] entries = StringUtils.commaDelimitedListToStringArray(headerContent);
		for (int i = 0; i < entries.length; i++) entries[i] = entries[i].trim();
		return entries;
	}

	public static String[] getHeaderWithAttributesAsTrimmedStringArray(String header, Bundle bundle) {
		if (bundle == null || !StringUtils.hasText(header)) return new String[0];
		String headerContent = (String) bundle.getHeaders().get(header);
		if (!StringUtils.hasText(headerContent)) return new String[0];
		List tokens = new ArrayList(2);
		StringBuffer token = new StringBuffer();
		boolean ignoreComma = false;
		for (int stringIndex = 0; stringIndex < headerContent.length(); stringIndex++) {
			char currentChar = headerContent.charAt(stringIndex);
			if (currentChar == COMMA_CHAR) if (ignoreComma) token.append(currentChar);
			else {
				tokens.add(token.toString().trim());
				token.delete(0, token.length());
				ignoreComma = false;
			} else {
				if (currentChar == QUOTE_CHAR) ignoreComma = !ignoreComma;
				token.append(currentChar);
			}
		}
		tokens.add(token.toString().trim());
		return (String[]) tokens.toArray(new String[tokens.size()]);
	}

	public static String[] parseRequiredBundleString(String entry) {
		String[] value = new String[2];
		int index = entry.indexOf(SEMI_COLON);
		if (index > 0)  value[0] = entry.substring(0, index);
		else {
			value[0] = entry;
			value[1] = DEFAULT_VERSION;
			return value;
		}
		index = entry.indexOf(Constants.BUNDLE_VERSION_ATTRIBUTE);
		if (index > 0) {
			int firstQuoteIndex = index + Constants.BUNDLE_VERSION_ATTRIBUTE.length() + 1;
			boolean isQuoted = entry.charAt(firstQuoteIndex) == QUOTE_CHAR;
			if (!isQuoted) {
				int nextAttribute = entry.indexOf(SEMI_COLON, firstQuoteIndex);
				value[1] = (nextAttribute > -1 ? entry.substring(firstQuoteIndex, nextAttribute) : entry.substring(firstQuoteIndex));
			} else {
				char testChar = entry.charAt(firstQuoteIndex + 1);
				boolean isRange = (testChar == SQUARE_BRACKET_CHAR || testChar == ROUND_BRACKET_CHAR);
				int secondQuoteStartIndex = (isRange ? firstQuoteIndex + 4 : firstQuoteIndex + 1);
				int numberStart = (isRange ? firstQuoteIndex + 2 : firstQuoteIndex + 1);
				int numberEnd = entry.indexOf(DOUBLE_QUOTE, secondQuoteStartIndex) - (isRange ? 1 : 0);
				value[1] = entry.substring(numberStart, numberEnd);
				if (isRange) value[1] = entry.charAt(firstQuoteIndex + 1) + value[1] + entry.charAt(numberEnd);
			}
		} else value[1] = DEFAULT_VERSION;
		return value;
	}
	
	public static String getPlatformName(BundleContext bundleContext) {
		String vendorProperty = bundleContext.getProperty(Constants.FRAMEWORK_VENDOR);
		String frameworkVersion = bundleContext.getProperty(Constants.FRAMEWORK_VERSION);
		Bundle bundle = bundleContext.getBundle(0);
		String name = (String) bundle.getHeaders().get(Constants.BUNDLE_NAME);
		String version = (String) bundle.getHeaders().get(Constants.BUNDLE_VERSION);
		String symName = bundle.getSymbolicName();
		StringBuffer buf = new StringBuffer();
		buf.append(name);
		buf.append(" ");
		buf.append(symName);
		buf.append("|");
		buf.append(version);
		buf.append("{");
		buf.append(frameworkVersion);
		buf.append(" ");
		buf.append(vendorProperty);
		buf.append("}");
		return buf.toString();
	}

	public static boolean isPlatformVendorMatch(String vendorString, BundleContext bundleContext) {
		String vendor = bundleContext.getProperty(Constants.FRAMEWORK_VENDOR);
		if (vendor != null) return vendor.indexOf(vendorString) >= -1;
		return false;
	}

	public static Vendor getVendor(BundleContext bundleContext) {
		if (isPlatformVendorMatch("clispe", bundleContext)) return Vendor.Equinox;
		else if (isPlatformVendorMatch("fish", bundleContext)) return Vendor.Knopflerfish;
		else if (isPlatformVendorMatch("pache", bundleContext)) return Vendor.Felix;
		else return Vendor.Other;
	}

	public static Map<Bundle, Collection<String>> getImportedBundles(Bundle bundle){
	    BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);
	    if (bundleWiring == null) return Collections.EMPTY_MAP;
	    List<BundleWire> bundleWires = bundleWiring.getRequiredWires(null);
	    if (bundleWires == null) return Collections.EMPTY_MAP;
	    Map<Bundle, Collection<String>> bundleDependencies = new HashMap<Bundle, Collection<String>>();
	    for (BundleWire bundleWire : bundleWires) {
	    	BundleWiring provider = bundleWire.getProviderWiring();
	        if (provider == null || bundle.equals(provider)) continue;
	        Bundle providerBundle = provider.getBundle();
	        Collection<String> packages = bundleDependencies.get(providerBundle);
	        if (packages == null) packages = new HashSet<String>();
	        BundleCapability capability = bundleWire.getCapability();
	        if (capability != null) {
	        	String ns = capability.getNamespace();
	        	if (BundleRevision.PACKAGE_NAMESPACE.equals(ns)) {
		        	Object pkg = capability.getAttributes().get(BundleRevision.PACKAGE_NAMESPACE);
		            if (pkg != null) packages.add(pkg.toString());
	        	} else if (BundleRevision.BUNDLE_NAMESPACE.equals(ns) || BundleRevision.HOST_NAMESPACE.equals(ns)) {
	            	List<BundleCapability> caps = provider.getCapabilities(BundleRevision.PACKAGE_NAMESPACE);
	            	if (caps != null && !caps.isEmpty()) for (BundleCapability cap : caps) {
	            		Object p = cap.getAttributes().get(BundleRevision.PACKAGE_NAMESPACE);
	            		if (p != null) packages.add(p.toString());
	            	}
	            }
	        }
	        if (packages.size() > 0) bundleDependencies.put(providerBundle, packages);
	    }
	    return bundleDependencies;
	}
	
	public static Boolean isPattern(String path) {
		return (path.indexOf('*') != -1 || path.indexOf('?') != -1);
	}
	
	public static Collection<String> matchingResources(String location, Bundle bundle, Integer options) {
		int prefixEnd = location.lastIndexOf(":") + 1;
		if (prefixEnd > 0 && location.length() > prefixEnd && Character.isDigit(location.charAt(prefixEnd))) {
			int index = location.substring(prefixEnd).indexOf("/");
			if (index >= 0) prefixEnd += index;
		}
		int rootDirEnd = location.lastIndexOf('/') + 1;
		while (rootDirEnd > prefixEnd && isPattern(location.substring(prefixEnd, rootDirEnd))) rootDirEnd = location.lastIndexOf('/', rootDirEnd - 2) + 1;
		if (rootDirEnd == 0 || rootDirEnd < prefixEnd) rootDirEnd = prefixEnd;
		String rootDirPath = location.substring(prefixEnd, rootDirEnd);
		String subPattern = location.substring(rootDirEnd);
		return bundle.adapt(BundleWiring.class).listResources(rootDirPath, subPattern.trim().length() == 0 ? null : subPattern, options);
	}
	
	public static List<URL> matchingEntries(String location, Bundle bundle, Integer options) {
		int prefixEnd = location.lastIndexOf(":") + 1;
		if (prefixEnd > 0 && location.length() > prefixEnd && Character.isDigit(location.charAt(prefixEnd))) {
			int index = location.substring(prefixEnd).indexOf("/");
			if (index >= 0) prefixEnd += index;
		}
		int rootDirEnd = location.lastIndexOf('/') + 1;
		while (rootDirEnd > prefixEnd && isPattern(location.substring(prefixEnd, rootDirEnd))) rootDirEnd = location.lastIndexOf('/', rootDirEnd - 2) + 1;
		if (rootDirEnd == 0 || rootDirEnd < prefixEnd) rootDirEnd = prefixEnd;
		String rootDirPath = location.substring(prefixEnd, rootDirEnd);
		String subPattern = location.substring(rootDirEnd);
		return bundle.adapt(BundleWiring.class).findEntries(rootDirPath, subPattern.trim().length() == 0 ? null : subPattern, options);
	}
	
	public static BundleContext getBundleContext(final Bundle bundle) {
		if (bundle == null) return null;
		if (System.getSecurityManager() != null) {
			return AccessController.doPrivileged(new PrivilegedAction<BundleContext>() {
				public BundleContext run() {
					return getBundleContextWithPrivileges(bundle);
				}
			});
		} else return getBundleContextWithPrivileges(bundle);
	}

	protected static BundleContext getBundleContextWithPrivileges(final Bundle bundle) {
		Method meth = ReflectionUtils.findMethod(bundle.getClass(), GET_CONTEXT_METHOD, new Class[0]);
		if (meth == null) meth = ReflectionUtils.findMethod(bundle.getClass(), GET_BUNDLE_CONTEXT_METHOD, new Class[0]);
		final Method m = meth;
		if (meth != null) {
			ReflectionUtils.makeAccessible(meth);
			return (BundleContext) ReflectionUtils.invokeMethod(m, bundle);
		}
		final BundleContext[] ctx = new BundleContext[1];
		ReflectionUtils.doWithFields(bundle.getClass(), new FieldCallback() {
			public void doWith(final Field field) throws IllegalArgumentException, IllegalAccessException {
				ReflectionUtils.makeAccessible(field);
				ctx[0] = (BundleContext) field.get(bundle);
			}
		}, new FieldFilter() {
			public boolean matches(Field field) {
				return BundleContext.class.isAssignableFrom(field.getType());
			}
		});
		return ctx[0];
	}
}
