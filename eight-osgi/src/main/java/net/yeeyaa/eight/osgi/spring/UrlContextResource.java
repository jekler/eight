package net.yeeyaa.eight.osgi.spring;

import java.net.MalformedURLException;
import java.net.URL;


import org.springframework.core.io.ContextResource;
import org.springframework.core.io.UrlResource;


public class UrlContextResource extends UrlResource implements ContextResource {
	protected final String pathWithinContext;

	public UrlContextResource(String path) throws MalformedURLException {
		super(path);
		pathWithinContext = checkPath(path);
	}
	
	public UrlContextResource(URL url, String path) {
		super(url);
		this.pathWithinContext = checkPath(path);
	}

	public String getPathWithinContext() {
		return pathWithinContext;
	}
	
	protected String checkPath(String path) {
		return (path.startsWith(BundleUtil.FOLDER_DELIMITER) ? path : BundleUtil.FOLDER_DELIMITER + path);
	}
}
