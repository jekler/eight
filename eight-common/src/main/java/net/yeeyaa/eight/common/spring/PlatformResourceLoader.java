package net.yeeyaa.eight.common.spring;

import java.io.File;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.ContextResource;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.Assert;
import org.springframework.util.PathMatcher;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;


public class PlatformResourceLoader implements ResourcePatternResolver {
	protected final Logger log;
	protected String regex = ":";
	protected ClassLoader classLoader;
	protected IProcessor<Object, Object> beanHolder;
	protected Boolean adapt; 
	protected IProcessor<IExtendable<Object>, Resource> convertor;
	protected IProcessor<String, Collection<Object>> finder;
	protected PathMatcher pathMatcher;
	protected IProcessor<Resource, Resource> resourceProcessor;
	
	public PlatformResourceLoader() {
		this.log = LoggerFactory.getLogger(PlatformResourceLoader.class);
	}

	public PlatformResourceLoader(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformResourceLoader.class) : log;
	}
	
	public void setResourceProcessor(IProcessor<Resource, Resource> resourceProcessor) {
		this.resourceProcessor = resourceProcessor;
	}

	public void setFinder(IProcessor<String, Collection<Object>> finder) {
		this.finder = finder;
	}

	public void setAdapt(Boolean adapt) {
		this.adapt = adapt;
	}
	
	public void setConvertor(IProcessor<IExtendable<Object>, Resource> convertor) {
		this.convertor = convertor;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	public void setPathMatcher(PathMatcher pathMatcher) {
		this.pathMatcher = pathMatcher;
	}

	@PostConstruct
	public void initialize() {
		if (pathMatcher == null) pathMatcher = new AntPathMatcher();
	}
	
	protected class ClassPathContextResource extends ClassPathResource implements ContextResource {
		public ClassPathContextResource(String path, ClassLoader classLoader) {
			super(path, classLoader);
		}

		@Override
		public String getPathWithinContext() {
			return getPath();
		}

		@Override
		public Resource createRelative(String relativePath) {
			String pathToUse = StringUtils.applyRelativePath(getPath(), relativePath);
			return new ClassPathContextResource(pathToUse, getClassLoader());
		}
	}
	
	@Override
	public Resource getResource(String location) {
		Assert.notNull(location, "Location must not be null");
		if (location.startsWith("/")) {
			return resourceProcessor == null ? new ClassPathContextResource(location, getClassLoader()) : resourceProcessor.process(new ClassPathContextResource(location, getClassLoader()));
		} else if (location.startsWith(CLASSPATH_URL_PREFIX)) {
			return resourceProcessor == null ? new ClassPathResource(location.substring(CLASSPATH_URL_PREFIX.length()), getClassLoader()) : resourceProcessor.process(new ClassPathResource(location.substring(CLASSPATH_URL_PREFIX.length()), getClassLoader()));
		} else try {
			if(location.indexOf(regex) > -1){
				String[] paths = location.split(regex);
				Object o = beanHolder.process(paths[0]);
				if(o instanceof IExtendable) return convertor.process((IExtendable<Object>)o);
				else if(o instanceof Resource) return resourceProcessor == null ? (Resource)o : resourceProcessor.process((Resource)o);
				else if(o instanceof URL) return resourceProcessor == null ? new UrlResource((URL)o) : resourceProcessor.process(new UrlResource((URL)o));
				else if(o instanceof URI) return resourceProcessor == null ? new UrlResource((URI)o) : resourceProcessor.process(new UrlResource((URI)o));
				else if(o instanceof String) return resourceProcessor == null ? new UrlResource((String)o) : resourceProcessor.process(new UrlResource((String)o));
				else if(o instanceof IProcessor){
					Object newo;
					if(adapt == null) newo = ((IProcessor)o).process(location);
					else {
						String[] paras = new String[paths.length - 1];
						for(int i = 0; i < paths.length - 1; i++) paras[i] = paths[i + 1];
						if(paras.length > 1 || !adapt) newo = ((IProcessor)o).process(paras);
						else newo = ((IProcessor)o).process(paras[0]);
					}
					if(newo instanceof IExtendable) return convertor.process((IExtendable<Object>)newo);
					else if(newo instanceof Resource) return resourceProcessor == null ? (Resource)newo : resourceProcessor.process((Resource)newo);
					else if(newo instanceof URL) return resourceProcessor == null ? new UrlResource((URL)newo) : resourceProcessor.process(new UrlResource((URL)newo));
					else if(newo instanceof URI) return resourceProcessor == null ? new UrlResource((URI)newo) : resourceProcessor.process(new UrlResource((URI)newo));
					else if(newo instanceof String) return resourceProcessor == null ? new UrlResource((String)newo) : resourceProcessor.process(new UrlResource((String)newo));
				}else if(o instanceof IInputResource){
					Object newo;
					if(adapt == null) newo = ((IInputResource)o).find(location);
					else {
						String[] paras = new String[paths.length - 1];
						for(int i = 0; i < paths.length - 1; i++) paras[i] = paths[i + 1];
						if(paras.length > 1 || !adapt) newo = ((IInputResource)o).find(paras);
						else newo = ((IInputResource)o).find(paras[0]);
					}
					if(newo instanceof IExtendable) return convertor.process((IExtendable<Object>)newo);
					else if(newo instanceof Resource) return resourceProcessor == null ? (Resource)newo : resourceProcessor.process((Resource)newo);
					else if(newo instanceof URL) return resourceProcessor == null ? new UrlResource((URL)newo) : resourceProcessor.process(new UrlResource((URL)newo));
					else if(newo instanceof URI) return resourceProcessor == null ? new UrlResource((URI)newo) : resourceProcessor.process(new UrlResource((URI)newo));
					else if(newo instanceof String) return resourceProcessor == null ? new UrlResource((String)newo) : resourceProcessor.process(new UrlResource((String)newo));
				}
			}
			return resourceProcessor == null ? new UrlResource(new URL(location)) : resourceProcessor.process(new UrlResource(new URL(location)));
		} catch (Exception ex) {
			return resourceProcessor == null ? new ClassPathContextResource(location, getClassLoader()) : resourceProcessor.process(new ClassPathContextResource(location, getClassLoader()));
		}
	}
	
	@Override
	public ClassLoader getClassLoader() {
		return classLoader;
	}

	@Override
	public Resource[] getResources(String locationPattern) throws IOException {
		if (finder != null) {
			Collection<Object> resources = finder.process(locationPattern);
			if (resources != null && resources.size() > 0) {
				ArrayList<Resource> ret = new ArrayList<Resource>(resources.size());
				Iterator<Object> itr = resources.iterator();
				while(itr.hasNext()) {
					Object resource = itr.next();
					if (resource instanceof Resource) ret.add(resourceProcessor == null ? (Resource)resource : resourceProcessor.process((Resource)resource));
					else if(resource instanceof IExtendable) ret.add(convertor.process((IExtendable<Object>)resource));
					else if(resource instanceof URL) ret.add(resourceProcessor == null ? new UrlResource((URL)resource) : resourceProcessor.process(new UrlResource((URL)resource)));
					else if(resource instanceof URI) ret.add(resourceProcessor == null ? new UrlResource((URI)resource) : resourceProcessor.process(new UrlResource((URI)resource)));
					else ret.add(getResource(resource.toString()));
				}
				return ret.toArray(new Resource[resources.size()]);
			} else return new Resource[0];
		} else if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) if (pathMatcher.isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) return findPathMatchingResources(locationPattern);
		else {
			String path = locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length());
			if (path.startsWith("/")) path = path.substring(1);
			Set<Resource> result = new LinkedHashSet<Resource>(16);
			ClassLoader cl = classLoader == null ? getClass().getClassLoader() : classLoader;
			Enumeration<URL> resourceUrls = cl.getResources(path);
			while (resourceUrls.hasMoreElements()) result.add(getResource(resourceUrls.nextElement().toString()));
			if ("".equals(path)) addAllClassLoaderJarRoots(cl, result);
			Resource[] ret = result.toArray(new Resource[result.size()]);
			if (resourceProcessor != null && ret.length > 0) for (int i = 0; i < ret.length; i++) ret[i] = resourceProcessor.process(ret[i]);
			return ret;
		} else if (pathMatcher.isPattern(locationPattern.substring(locationPattern.indexOf(":") + 1))) return findPathMatchingResources(locationPattern);
		else return new Resource[] {getResource(locationPattern)};
	}
	
	protected void addAllClassLoaderJarRoots(ClassLoader classLoader, Set<Resource> result) {
		if (classLoader instanceof URLClassLoader) try {
			for (URL url : ((URLClassLoader) classLoader).getURLs()) if (ResourceUtils.isJarFileURL(url)) try {
				UrlResource jarResource = new UrlResource(ResourceUtils.JAR_URL_PREFIX + url.toString() + ResourceUtils.JAR_URL_SEPARATOR);
				if (jarResource.exists()) result.add(jarResource);
			} catch (MalformedURLException ex) {
				log.error("add resource error.", ex);
			}
		} catch (Exception ex) {
			log.error("add resource error.", ex);
		}
		if (classLoader != null) try {
			addAllClassLoaderJarRoots(classLoader.getParent(), result);
		} catch (Exception ex) {
			log.error("add resource error.", ex);
		}
	}
	
	protected Resource[] findPathMatchingResources(String location) throws IOException {
		int prefixEnd = location.indexOf(":") + 1;
		int rootDirEnd = location.length();
		while (rootDirEnd > prefixEnd && pathMatcher.isPattern(location.substring(prefixEnd, rootDirEnd))) rootDirEnd = location.lastIndexOf('/', rootDirEnd - 2) + 1;
		if (rootDirEnd == 0) rootDirEnd = prefixEnd;
		String rootDirPath = location.substring(0, rootDirEnd);
		String subPattern = location.substring(rootDirPath.length());
		Resource[] rootDirResources = getResources(rootDirPath);
		Set<Resource> result = new LinkedHashSet<Resource>(16);
		for (Resource rootDirResource : rootDirResources) if (ResourceUtils.isJarURL(rootDirResource.getURL())) try {
			URLConnection con = rootDirResource.getURL().openConnection();
			JarFile jarFile;
			String jarFileUrl;
			String rootEntryPath;
			boolean newJarFile = false;
			if (con instanceof JarURLConnection) {
				JarURLConnection jarCon = (JarURLConnection) con;
				ResourceUtils.useCachesIfNecessary(jarCon);
				jarFile = jarCon.getJarFile();
				jarFileUrl = jarCon.getJarFileURL().toExternalForm();
				JarEntry jarEntry = jarCon.getJarEntry();
				rootEntryPath = (jarEntry != null ? jarEntry.getName() : "");
			} else {
				String urlFile = rootDirResource.getURL().getFile();
				int separatorIndex = urlFile.indexOf(ResourceUtils.JAR_URL_SEPARATOR);
				if (separatorIndex != -1) {
					jarFileUrl = urlFile.substring(0, separatorIndex);
					rootEntryPath = urlFile.substring(separatorIndex + ResourceUtils.JAR_URL_SEPARATOR.length());
					if (jarFileUrl.startsWith(ResourceUtils.FILE_URL_PREFIX)) try {
						jarFile = new JarFile(ResourceUtils.toURI(jarFileUrl).getSchemeSpecificPart());
					} catch (URISyntaxException ex) {
						jarFile = new JarFile(jarFileUrl.substring(ResourceUtils.FILE_URL_PREFIX.length()));
					} else jarFile = new JarFile(jarFileUrl);
				}
				else {
					jarFile = new JarFile(urlFile);
					jarFileUrl = urlFile;
					rootEntryPath = "";
				}
				newJarFile = true;
			}
			try {
				if (!"".equals(rootEntryPath) && !rootEntryPath.endsWith("/")) rootEntryPath = rootEntryPath + "/";
				for (Enumeration<JarEntry> entries = jarFile.entries(); entries.hasMoreElements();) {
					JarEntry entry = entries.nextElement();
					String entryPath = entry.getName();
					if (entryPath.startsWith(rootEntryPath)) {
						String relativePath = entryPath.substring(rootEntryPath.length());
						if (pathMatcher.match(subPattern, relativePath)) result.add(rootDirResource.createRelative(relativePath));
					}
				}
			} finally {
				if (newJarFile) jarFile.close();
			}
		} catch (Exception ex) {
			log.error("add resource error.", ex);
		} else try {
			File rootDir = rootDirResource.getFile().getAbsoluteFile();
			if (!rootDir.exists()|| !rootDir.isDirectory() || !rootDir.canRead()) continue;
			String fullPattern = StringUtils.replace(rootDir.getAbsolutePath(), File.separator, "/");
			if (!subPattern.startsWith("/")) fullPattern += "/";
			fullPattern = fullPattern + StringUtils.replace(subPattern, File.separator, "/");
			Set<File> matchingFiles = new LinkedHashSet<File>(8);
			doRetrieveMatchingFiles(fullPattern, rootDir, matchingFiles);
			for (File file : matchingFiles) result.add(new FileSystemResource(file));
		} catch (Exception ex) {
			log.error("add resource error.", ex);
		}
		Resource[] ret = result.toArray(new Resource[result.size()]);
		if (resourceProcessor != null && ret.length > 0) for (int i = 0; i < ret.length; i++) ret[i] = resourceProcessor.process(ret[i]);
		return ret;
	}
	
	protected void doRetrieveMatchingFiles(String fullPattern, File dir, Set<File> result) throws IOException {
		File[] dirContents = dir.listFiles();
		if (dirContents == null) return;
		for (File content : dirContents) {
			String currPath = StringUtils.replace(content.getAbsolutePath(), File.separator, "/");
			if (content.isDirectory() && pathMatcher.matchStart(fullPattern, currPath + "/") && content.canRead()) doRetrieveMatchingFiles(fullPattern, content, result);
			if (pathMatcher.match(fullPattern, currPath)) result.add(content);
		}
	}
}
