package net.yeeyaa.eight.core.storage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

public class FileWStorage extends Storage<Object> implements IProcessor<FileWStorage, File>{
	protected final File file;
	
	public FileWStorage(String path) {
		this.file = new File(path);
	}
	
	public FileWStorage(File file) {
		this.file = file;
	}
	
	public InputStream input() {
    	try {
			return new FileInputStream(file);
		} catch (FileNotFoundException e) {
			throw new PlatformException(PlatformError.ERROR_IO);
		}
	}

	public Boolean exists() {
    	return file.exists();
    }
	
	public OutputStream output() {
    	try {
			return new FileOutputStream(file);
		} catch (FileNotFoundException e) {
			throw new PlatformException(PlatformError.ERROR_IO);
		}
	}
	
	public Object key() {
		return file.getAbsolutePath();
	}

	public Long modified() {
		return file.lastModified();
	}

	@Override
	public File process(FileWStorage storage) {
		return storage.file;
	}
}
