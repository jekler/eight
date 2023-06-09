package net.yeeyaa.eight.osgi.spring;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.osgi.framework.Bundle;
import org.springframework.core.io.AbstractResource;
import org.springframework.core.io.ContextResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;

import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;


public class BundleResource extends AbstractResource implements ContextResource {
	public static final String BUNDLE_URL_PREFIX = "osgibundle:";
	public static final String BUNDLE_PREFIX = "bundle:";
	public static final String BUNDLE_JAR_URL_PREFIX = "osgibundlejar:";
	protected static final char PREFIX_SEPARATOR = ':';
	protected static final String ABSOLUTE_PATH_PREFIX = "/";
	protected final String path;
	protected final String pathWithoutPrefix;
	protected int searchType = BundleUtil.PREFIX_TYPE_NOT_SPECIFIED;
	protected Bundle bundle;
	
	public BundleResource(Bundle bundle, String path) {
		this.bundle = bundle;
		this.path = StringUtils.cleanPath(path);
		this.searchType = BundleUtil.getSearchType(this.path);
		switch (this.searchType) {
			case BundleUtil.PREFIX_TYPE_NOT_SPECIFIED:
				pathWithoutPrefix = path;
				break;
			case BundleUtil.PREFIX_TYPE_BUNDLE_SPACE:
				pathWithoutPrefix = path.substring(BUNDLE_URL_PREFIX.length());
				break;
			case BundleUtil.PREFIX_TYPE_BUNDLE_JAR:
				pathWithoutPrefix = path.substring(BUNDLE_JAR_URL_PREFIX.length());
				break;
			case BundleUtil.PREFIX_TYPE_CLASS_SPACE:
				pathWithoutPrefix = path.substring(ResourceLoader.CLASSPATH_URL_PREFIX.length());
				break;
			default:
				pathWithoutPrefix = null;
		}
	}

	protected final String getPath() {
		return path;
	}

	protected final Bundle getBundle() {
		return bundle;
	}

	public InputStream getInputStream() throws IOException {
		URLConnection con = getURL().openConnection();
		con.setUseCaches(false);
		return con.getInputStream();
	}

	public URL getURL() throws IOException {
		ContextResource res = null;
		URL url = null;
		switch (searchType) {
			case BundleUtil.PREFIX_TYPE_NOT_SPECIFIED:
				res = getResourceFromBundleSpace(pathWithoutPrefix);
				break;
			case BundleUtil.PREFIX_TYPE_BUNDLE_SPACE:
				res = getResourceFromBundleSpace(pathWithoutPrefix);
				break;
			case BundleUtil.PREFIX_TYPE_BUNDLE_JAR:
				url = getResourceFromBundleJar(pathWithoutPrefix);
				break;
			case BundleUtil.PREFIX_TYPE_CLASS_SPACE:
				url = getResourceFromBundleClasspath(pathWithoutPrefix);
				break;
			default:
				url = new URL(path);
				break;
		}
		if (res != null) url = res.getURL();
		if (url == null) throw new FileNotFoundException(getDescription() + " cannot be resolved to URL because it does not exist");
		return url;
	}

	public static URL convert(final URL url, final IProcessor<InputStream, InputStream> convertor) {
		try {
			return new URL("brstream", url.getHost(), url.getPort(), url.getFile(), new URLStreamHandler(){
				@Override
				protected URLConnection openConnection(URL u) throws IOException {
					return new URLConnection(url){
						protected URLConnection con = url.openConnection();
						
						@Override
						public void connect() throws IOException {
							con.connect();
						}    
						
						@Override
						public InputStream getInputStream() throws IOException {
							return convertor.process(con.getInputStream());
	        }};}});
		} catch (MalformedURLException e) {
			throw new PlatformException(PlatformError.ERROR_PARAMETERS, e);
		}
	}
	
	protected ContextResource getResourceFromBundleSpace(String bundlePath) throws IOException {
		ContextResource[] res = getAllUrlsFromBundleSpace(bundlePath);
		return (ObjectUtils.isEmpty(res) ? null : res[0]);
	}

	protected URL getResourceFromBundleJar(String bundlePath) throws IOException {
		return bundle.getEntry(bundlePath);
	}

	protected URL getResourceFromBundleClasspath(String bundlePath) {
		return bundle.getResource(bundlePath);
	}

	protected boolean isRelativePath(String locationPath) {
		return ((locationPath.indexOf(PREFIX_SEPARATOR) == -1) && !locationPath.startsWith(ABSOLUTE_PATH_PREFIX));
	}

	public Resource createRelative(String relativePath) {
		String pathToUse = StringUtils.applyRelativePath(this.path, relativePath);
		return new BundleResource(this.bundle, pathToUse);
	}

	public String getFilename() {
		return StringUtils.getFilename(this.path);
	}

	public File getFile() throws IOException {
		if (searchType != BundleUtil.PREFIX_TYPE_UNKNOWN) {
			String bundleLocation = bundle.getLocation();
			int prefixIndex = bundleLocation.indexOf(ResourceUtils.FILE_URL_PREFIX);
			if (prefixIndex > -1) bundleLocation = bundleLocation.substring(prefixIndex + ResourceUtils.FILE_URL_PREFIX.length());
			File file = new File(bundleLocation, path);
			if (file.exists()) return file;
		}
		return ResourceUtils.getFile(getURI(), getDescription());
	}

	public String getDescription() {
		StringBuffer buf = new StringBuffer();
		buf.append("OSGi resource[");
		buf.append(this.path);
		buf.append("|bnd.id=");
		buf.append(bundle.getBundleId());
		buf.append("|bnd.sym=");
		buf.append(bundle.getSymbolicName());
		buf.append("]");
		return buf.toString();
	}

	public boolean equals(Object obj) {
		if (obj == this) return true;
		if (obj instanceof BundleResource) {
			BundleResource otherRes = (BundleResource) obj;
			return (this.path.equals(otherRes.path) && ObjectUtils.nullSafeEquals(this.bundle, otherRes.bundle));
		}
		return false;
	}
	
	public int hashCode() {
		return this.path.hashCode();
	}

	public long lastModified() throws IOException {
		URLConnection con = getURL().openConnection();
		con.setUseCaches(false);
		long time = con.getLastModified();
		if (time == 0) if (BundleUtil.PREFIX_TYPE_BUNDLE_JAR == searchType) return bundle.getLastModified();
		return time;
	}

	protected int getSearchType() {
		return searchType;
	}

	protected ContextResource[] getAllUrlsFromBundleSpace(String location) throws IOException {
		if (bundle == null) throw new IllegalArgumentException("cannot locate items in bundle-space w/o a bundle; specify one when creating this resolver");
		Assert.notNull(location);
		Set resources = new LinkedHashSet(5);
		location = StringUtils.cleanPath(location);
		location = BundleUtil.stripPrefix(location);
		if (!StringUtils.hasText(location)) location = BundleUtil.FOLDER_DELIMITER;
		if (BundleUtil.FOLDER_DELIMITER.equals(location)) {
			Enumeration<URL> candidates = bundle.findEntries("/", null, false);
			while (candidates != null && candidates.hasMoreElements()) resources.add(new UrlContextResource(BundleUtil.findUpperFolder(candidates.nextElement().toExternalForm())));
		} else {
			if (location.startsWith(BundleUtil.FOLDER_DELIMITER)) location = location.substring(1);
			if (location.endsWith(BundleUtil.FOLDER_DELIMITER)) location = location.substring(0, location.length() - 1);
			boolean hasFolder = (location.indexOf(BundleUtil.FOLDER_DELIMITER) != -1);
			String path = (hasFolder ? location : BundleUtil.FOLDER_DELIMITER);
			String file = (hasFolder ? null : location);
			int separatorIndex = location.lastIndexOf(BundleUtil.FOLDER_DELIMITER);
			if (separatorIndex > -1 && separatorIndex + 1 < location.length()) {
				path = location.substring(0, separatorIndex);
				if (separatorIndex + 1 < location.length()) file = location.substring(separatorIndex + 1);
			}
			Enumeration<URL> candidates = bundle.findEntries(path, file, false);
			while (candidates != null && candidates.hasMoreElements()) resources.add(new UrlContextResource(candidates.nextElement(), BundleUtil.FOLDER_DELIMITER + location));
		}
		return (ContextResource[]) resources.toArray(new ContextResource[resources.size()]);
	}

	public String getPathWithinContext() {
		return pathWithoutPrefix;
	}

	public boolean exists() {
		try {
			InputStream is = getInputStream();
			is.close();
			return true;
		} catch (Throwable isEx) {
			return false;
		}
	}
}
