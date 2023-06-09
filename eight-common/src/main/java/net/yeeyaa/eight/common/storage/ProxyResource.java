package net.yeeyaa.eight.common.storage;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.springframework.core.io.Resource;


public class ProxyResource implements Resource {
	protected volatile Resource resource;
	protected String name;
	protected Boolean exist;
	protected Boolean readable;
	protected Boolean open;
	protected Long modify;
	protected Long length;
	protected String description;
	protected IProcessor<Resource, Long> lengthProcessor;
	protected IProcessor<Resource, String> descriptionProcessor;
	protected IProcessor<Resource, String> nameProcessor;
	protected IProcessor<Resource, Long> modifyProcessor;
	protected IProcessor<Resource, Boolean> existProcessor;	
	protected IProcessor<Resource, Boolean> readableProcessor;	
	protected IProcessor<Resource, Boolean> openProcessor;	
	protected IProcessor<Resource, InputStream> inputProcessor;
	protected IProcessor<Resource, URL> urlProcessor;
	protected IProcessor<Resource, URI> uriProcessor;
	protected IProcessor<Resource, File> fileProcessor;
	protected IBiProcessor<Resource, String, Resource> relativeProcessor;	
	
	public ProxyResource() {}

	public ProxyResource(Resource resource) {
		this.resource = resource;
	}

	public void setResource(Resource resource) {
		this.resource = resource;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setExist(Boolean exist) {
		this.exist = exist;
	}

	public void setReadable(Boolean readable) {
		this.readable = readable;
	}

	public void setOpen(Boolean open) {
		this.open = open;
	}

	public void setModify(Long modify) {
		this.modify = modify;
	}

	public void setLength(Long length) {
		this.length = length;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setLengthProcessor(IProcessor<Resource, Long> lengthProcessor) {
		this.lengthProcessor = lengthProcessor;
	}

	public void setDescriptionProcessor(IProcessor<Resource, String> descriptionProcessor) {
		this.descriptionProcessor = descriptionProcessor;
	}

	public void setNameProcessor(IProcessor<Resource, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setModifyProcessor(IProcessor<Resource, Long> modifyProcessor) {
		this.modifyProcessor = modifyProcessor;
	}

	public void setExistProcessor(IProcessor<Resource, Boolean> existProcessor) {
		this.existProcessor = existProcessor;
	}

	public void setReadableProcessor(IProcessor<Resource, Boolean> readableProcessor) {
		this.readableProcessor = readableProcessor;
	}

	public void setOpenProcessor(IProcessor<Resource, Boolean> openProcessor) {
		this.openProcessor = openProcessor;
	}

	public void setInputProcessor(IProcessor<Resource, InputStream> inputProcessor) {
		this.inputProcessor = inputProcessor;
	}

	public void setUrlProcessor(IProcessor<Resource, URL> urlProcessor) {
		this.urlProcessor = urlProcessor;
	}

	public void setUriProcessor(IProcessor<Resource, URI> uriProcessor) {
		this.uriProcessor = uriProcessor;
	}

	public void setFileProcessor(IProcessor<Resource, File> fileProcessor) {
		this.fileProcessor = fileProcessor;
	}

	public void setRelativeProcessor(IBiProcessor<Resource, String, Resource> relativeProcessor) {
		this.relativeProcessor = relativeProcessor;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		if(inputProcessor == null) return resource.getInputStream();
		else return inputProcessor.process(resource);
	}

	@Override
	public boolean exists() {
		if (existProcessor == null) if (exist == null) return resource.exists();
		else return exist;
		else return existProcessor.process(resource);
	}

	@Override
	public boolean isReadable() {
		if (readableProcessor == null) if (readable == null) return resource.isReadable();
		else return readable;
		else return readableProcessor.process(resource);
	}

	@Override
	public boolean isOpen() {
		if (openProcessor == null) if (open == null) return resource.isOpen();
		else return open;
		else return openProcessor.process(resource);
	}

	@Override
	public URL getURL() throws IOException {
		if(urlProcessor == null) return resource.getURL();
		else return urlProcessor.process(resource);
	}

	@Override
	public URI getURI() throws IOException {
		if(uriProcessor == null) return resource.getURI();
		else return uriProcessor.process(resource);
	}

	@Override
	public File getFile() throws IOException {
		if(fileProcessor == null) return resource.getFile();
		else return fileProcessor.process(resource);
	}

	@Override
	public long contentLength() throws IOException {
		if (lengthProcessor == null) if (length == null) return resource.contentLength();
		else return length;
		else return lengthProcessor.process(resource);
	}

	@Override
	public long lastModified() throws IOException {
		if (modifyProcessor == null) if (modify == null) return resource.lastModified();
		else return modify;
		else return modifyProcessor.process(resource);
	}

	@Override
	public Resource createRelative(String relativePath) throws IOException {
		if(relativeProcessor == null) return resource.createRelative(relativePath);
		else return relativeProcessor.perform(resource, relativePath);
	}

	@Override
	public String getFilename() {
		if (nameProcessor == null) if (name == null) return resource.getFilename();
		else return name;
		else return nameProcessor.process(resource);
	}

	@Override
	public String getDescription() {
		if (descriptionProcessor == null) if (description == null) return resource.getDescription();
		else return description;
		else return descriptionProcessor.process(resource);
	}
}
