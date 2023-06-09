package net.yeeyaa.eight.osgi.spring;

import java.io.IOException;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.springframework.core.io.ContextResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.UrlResource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import net.yeeyaa.eight.IProcessor;

import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.PathMatcher;
import org.springframework.util.StringUtils;


public class BundleResourcePatternResolver extends PathMatchingResourcePatternResolver {
	protected Bundle bundle;
	protected IProcessor<Resource, Resource> resourceProcessor;
	protected static final String FOLDER_SEPARATOR = "/";
	protected static final String FOLDER_WILDCARD = "**";
	protected static final String JAR_EXTENSION = ".jar";
	protected static final String BUNDLE_DEFAULT_CP = ".";
	protected static final char SLASH = '/';
	protected static final char DOT = '.';

	public BundleResourcePatternResolver() {
		super(new BundleResourceLoader());
	}

	public BundleResourcePatternResolver(ClassLoader classloader) {
		super(new BundleResourceLoader(classloader));
	}
	
	public BundleResourcePatternResolver(Bundle bundle) {
		super(new BundleResourceLoader(bundle));
		this.bundle = bundle;
	}
	
	public BundleResourcePatternResolver(Bundle bundle, ClassLoader classloader) {
		super(new BundleResourceLoader(bundle, classloader));
		this.bundle = bundle;
	}
	
	public BundleResourcePatternResolver(ResourceLoader resourceLoader) {
		super(resourceLoader);
		if (resourceLoader instanceof BundleResourceLoader) this.bundle = ((BundleResourceLoader) resourceLoader).bundle;
	}

	public void setBundle(Bundle bundle) {
		this.bundle = bundle;
		if (getResourceLoader() instanceof BundleResourceLoader) ((BundleResourceLoader) getResourceLoader()).setBundle(bundle);
	}

	public void setContext(BundleContext context) {
		if (context != null) this.bundle = context.getBundle();
		if (getResourceLoader() instanceof BundleResourceLoader) ((BundleResourceLoader) getResourceLoader()).setContext(context);
	}

	public void setResourceProcessor(IProcessor<Resource, Resource> resourceProcessor) {
		this.resourceProcessor = resourceProcessor;
	}
	
	protected Resource[] findResources(String locationPattern) throws IOException {
		int type = BundleUtil.getSearchType(locationPattern);
		if (getPathMatcher().isPattern(locationPattern)) {
			if (BundleUtil.isClassPathType(type)) return findClassPathMatchingResources(locationPattern, type);
			return findPathMatchingResources(locationPattern, type);
		} else {
			Resource[] result = null;
			BundleResource resource = new BundleResource(bundle, locationPattern);
			switch (type) {
				case BundleUtil.PREFIX_TYPE_NOT_SPECIFIED:
				case BundleUtil.PREFIX_TYPE_BUNDLE_SPACE:
					result = resource.getAllUrlsFromBundleSpace(locationPattern);
					break;
				default:
					if (!resource.exists()) result = new Resource[] { resource };
					break;
			}
			return result;
		}
	}

	public Resource[] getResources(final String locationPattern) throws IOException {
		Resource[] resources = findResources(locationPattern);
		if (ObjectUtils.isEmpty(resources) && (!getPathMatcher().isPattern(locationPattern))) return new Resource[] { getResourceLoader().getResource(locationPattern) };
		if (resourceProcessor != null) for (int i = 0; i < resources.length; i++) resources[i] = resourceProcessor.process(resources[i]);
		return resources;

	}

	protected Resource[] findClassPathMatchingResources(String locationPattern, int type) throws IOException {
		final Map<Bundle, Collection<String>> importedBundles = BundleUtil.getImportedBundles(bundle);
		final String path = BundleUtil.stripPrefix(locationPattern);
		final Collection foundPaths = new LinkedHashSet();
		final String rootDirPath = determineFolderPattern(path);
		if (System.getSecurityManager() != null) {
			AccessController.doPrivileged(new PrivilegedAction<Object>() {
				public Object run() {
					for (Entry<Bundle, Collection<String>> importedBundle : importedBundles.entrySet()) if (!bundle.equals(importedBundle.getKey())) findImportedBundleMatchingResource(importedBundle, rootDirPath, path, foundPaths);
					return null;
				}
			});
		} else for (Entry<Bundle, Collection<String>> importedBundle : importedBundles.entrySet()) if (!bundle.equals(importedBundle.getKey())) findImportedBundleMatchingResource(importedBundle, rootDirPath, path, foundPaths);
		findSyntheticClassPathMatchingResource(bundle, path, foundPaths);
		List resources = new ArrayList(foundPaths.size());
		for (Iterator iterator = foundPaths.iterator(); iterator.hasNext();) {
			String resourcePath = (String) iterator.next();
			if (BundleUtil.PREFIX_TYPE_CLASS_ALL_SPACE == type) CollectionUtils.mergeArrayIntoCollection(convertURLEnumerationToResourceArray(bundle.getResources(resourcePath), resourcePath), resources);
			else {
				URL url = bundle.getResource(resourcePath);
				if (url != null) resources.add(new UrlContextResource(url, resourcePath));
			}
		}
		return (Resource[]) resources.toArray(new Resource[resources.size()]);
	}

	protected String determineFolderPattern(String path) {
		int index = path.lastIndexOf(FOLDER_SEPARATOR);
		return (index > 0 ? path.substring(0, index + 1) : "");
	}

	protected ContextResource[] convertURLEnumerationToResourceArray(Enumeration<URL> enm, String path) {
		Set resources = new LinkedHashSet(4);
		while (enm != null && enm.hasMoreElements()) resources.add(new UrlContextResource(enm.nextElement(), path));
		return (ContextResource[]) resources.toArray(new ContextResource[resources.size()]);
	}

	protected void findImportedBundleMatchingResource(final Entry<Bundle, Collection<String>> importedBundle, String rootPath, String path, final Collection foundPaths) {
		Collection<String> packages = importedBundle.getValue();
		final boolean startsWithSlash = rootPath.startsWith(FOLDER_SEPARATOR);
		for (String pkg : packages) {
			pkg = pkg.replace(DOT, SLASH) + SLASH;
			if (startsWithSlash) pkg = FOLDER_SEPARATOR + pkg;
			final PathMatcher matcher = getPathMatcher();
			if (matcher.matchStart(path, pkg)) {
				Enumeration<String> entries = importedBundle.getKey().getEntryPaths(pkg);
				while (entries != null && entries.hasMoreElements()) {
					String entry = (String) entries.nextElement();
					if (startsWithSlash) entry = FOLDER_SEPARATOR + entry;
					if (matcher.match(path, entry)) foundPaths.add(entry);
				}
			}
		}
	}

	protected void findSyntheticClassPathMatchingResource(Bundle bundle, String path, Collection foundPaths) throws IOException {
		BundleResourcePatternResolver localPatternResolver = new BundleResourcePatternResolver(bundle);
		Resource[] foundResources = localPatternResolver.findResources(path);
		for (int j = 0; j < foundResources.length; j++) foundPaths.add(foundResources[j].getURL().getPath());
		Collection cpMatchingPaths = findBundleClassPathMatchingPaths(bundle, path);
		foundPaths.addAll(cpMatchingPaths);
	}

	protected Collection findBundleClassPathMatchingPaths(Bundle bundle, String pattern) throws IOException {
		List list = new ArrayList(4);
		for (String entry : BundleUtil.getBundleClassPath(bundle)) if (!entry.equals(BUNDLE_DEFAULT_CP)) {
			ContextResource res = new BundleResource(bundle, entry).getResourceFromBundleSpace(entry);
			if (res != null) {
				URL url = res.getURL();
				if (url != null) if (entry.endsWith(JAR_EXTENSION)) findBundleClassPathMatchingJarEntries(list, url, pattern);
				else findBundleClassPathMatchingFolders(list, bundle, url.getPath(), pattern);
			}
		}
		return list;
	}

	protected void findBundleClassPathMatchingJarEntries(List list, URL url, String pattern) throws IOException {
		JarInputStream jis = new JarInputStream(url.openStream());
		Set result = new LinkedHashSet(8);
		try {
			while (jis.available() > 0) {
				JarEntry jarEntry = jis.getNextJarEntry();
				if (jarEntry != null) {
					String entryPath = jarEntry.getName();
					if (entryPath.startsWith(FOLDER_SEPARATOR)) entryPath = entryPath.substring(FOLDER_SEPARATOR.length());
					if (getPathMatcher().match(pattern, entryPath)) result.add(entryPath);
				}
			}
		} finally {
			try {
				jis.close();
			} catch (IOException io) {}
		}
		list.addAll(result);
	}

	protected void findBundleClassPathMatchingFolders(List list, Bundle bundle, String cpEntryPath, String pattern) throws IOException {
		String bundlePathPattern;
		boolean entryWithFolderSlash = cpEntryPath.endsWith(FOLDER_SEPARATOR);
		boolean patternWithFolderSlash = pattern.startsWith(FOLDER_SEPARATOR);
		if (entryWithFolderSlash) if (patternWithFolderSlash) bundlePathPattern = cpEntryPath + pattern.substring(1, pattern.length());
		else bundlePathPattern = cpEntryPath + pattern;
		else if (patternWithFolderSlash) bundlePathPattern = cpEntryPath + pattern;
		else bundlePathPattern = cpEntryPath + FOLDER_SEPARATOR + pattern;
		BundleResourcePatternResolver localResolver = new BundleResourcePatternResolver(bundle);
		Resource[] resources = localResolver.getResources(bundlePathPattern);
		if (resources.length == 1 && !resources[0].exists()) return;
		else {
			int cutStartingIndex = cpEntryPath.length();
			for (Resource resource : resources) list.add(resource.getURL().getPath().substring(cutStartingIndex));
		}
	}

	protected Resource[] findPathMatchingResources(String locationPattern, int searchType) throws IOException {
		String rootDirPath = determineRootDir(locationPattern);
		String subPattern = locationPattern.substring(rootDirPath.length());
		Resource[] rootDirResources = getResources(rootDirPath);
		Set<Resource> result = new LinkedHashSet<Resource>();
		for (Resource rootDirResource : rootDirResources) if (isJarResource(rootDirResource))result.addAll(doFindPathMatchingJarResources(rootDirResource, subPattern));
		else result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern, searchType));
		return result.toArray(new Resource[result.size()]);
	}
	
	protected boolean isJarResource(Resource resource) throws IOException {
		if (resource instanceof BundleResource && ((BundleResource) resource).getSearchType() != BundleUtil.PREFIX_TYPE_UNKNOWN) return false;
		return super.isJarResource(resource);
	}

	protected Set doFindPathMatchingFileResources(Resource rootDirResource, String subPattern, int searchType) throws IOException {
		String rootPath = null;
		if (rootDirResource instanceof BundleResource) {
			rootPath = ((BundleResource) rootDirResource).getPath();
			searchType = ((BundleResource) rootDirResource).getSearchType();
		} else if (rootDirResource instanceof UrlResource) rootPath = rootDirResource.getURL().getPath();
		if (rootPath != null) {
			String cleanPath = BundleUtil.stripPrefix(rootPath);
			if (!cleanPath.endsWith(FOLDER_SEPARATOR)) cleanPath = cleanPath + FOLDER_SEPARATOR;
			Set result = new LinkedHashSet();
			doRetrieveMatchingBundleEntries(bundle, cleanPath + subPattern, cleanPath, result, searchType);
			return result;
		} else return super.doFindPathMatchingFileResources(rootDirResource, subPattern);
	}

	protected void doRetrieveMatchingBundleEntries(Bundle bundle, String fullPattern, String dir, Set result, int searchType) throws IOException {
		Enumeration candidates;
		switch (searchType) {
			case BundleUtil.PREFIX_TYPE_NOT_SPECIFIED:
			case BundleUtil.PREFIX_TYPE_BUNDLE_SPACE:
				candidates = bundle.findEntries(dir, null, false);
				break;
			case BundleUtil.PREFIX_TYPE_BUNDLE_JAR:
				candidates = bundle.getEntryPaths(dir);
				break;
			case BundleUtil.PREFIX_TYPE_CLASS_SPACE:
				throw new IllegalArgumentException("class space does not support pattern matching");
			default:
				throw new IllegalArgumentException("unknown searchType " + searchType);
		}
		if (candidates != null) {
			boolean dirDepthNotFixed = (fullPattern.indexOf(FOLDER_WILDCARD) != -1);
			while (candidates.hasMoreElements()) {
				Object path = candidates.nextElement();
				String currPath;
				if (path instanceof URL) currPath = ((URL)path).getPath();
				else currPath = FOLDER_SEPARATOR.concat(path.toString());
				if (!currPath.startsWith(dir)) {
					int dirIndex = currPath.indexOf(dir);
					if (dirIndex != -1) currPath = currPath.substring(dirIndex);
				}
				if (currPath.endsWith(FOLDER_SEPARATOR) && (dirDepthNotFixed || StringUtils.countOccurrencesOf(currPath, FOLDER_SEPARATOR) < StringUtils.countOccurrencesOf(fullPattern, FOLDER_SEPARATOR))) doRetrieveMatchingBundleEntries(bundle, fullPattern, currPath, result, searchType);
				if (getPathMatcher().match(fullPattern, currPath))  if (path instanceof URL) result.add(new UrlContextResource((URL) path, currPath));
				else result.add(new BundleResource(bundle, currPath));
			}
		}
	}
}
