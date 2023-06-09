package net.yeeyaa.eight.core.util;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ClassUtil<T> {
	protected final Logger log;
	protected CharSequenceCompiler<T> compiler;
	
	public ClassUtil() {
		this.log = LoggerFactory.getLogger(ClassUtil.class);
	}
	
	public ClassUtil(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ClassUtil.class) : log;
	}
	
	public void setDefaultLoader(IProcessor<Object, ClassLoader> defaultLoader) {
		this.compiler = new CharSequenceCompiler<T>(defaultLoader.process(this), null);
	}

	public List<String> getClassList(String path){
		List<String> clslist = new LinkedList<String>();
    	if (path != null) try{
    		if (path.trim().toLowerCase().endsWith(".jar")){ 
    			File file = new File(path);	            			
    			if(file.exists()){
    				JarFile jarf = new JarFile(file);
                    Enumeration<JarEntry> enumje = jarf.entries();
                    while(enumje.hasMoreElements()){
                    	JarEntry je = enumje.nextElement();
                    	String className = je.getName();
                        if(className.endsWith(".class"))   
                        {   
                        	className = className.substring(0, className.length()-6).replace('/', '.');
                        	clslist.add(className);
                        }
                    }
                    jarf.close();
    			}
    		}
    		else if (path.trim().toLowerCase().endsWith(".zip")){
    			File file = new File(path);
    			if (file.exists()){
        			ZipFile zipf = new ZipFile(path);
                    Enumeration enumze = zipf.entries();
                    while(enumze.hasMoreElements()){
                    	ZipEntry ze = (ZipEntry)enumze.nextElement();
                    	String className = ze.getName();
                        if(className.endsWith(".class"))   
                        {   
                        	className = className.substring(0, className.length()-6).replace('/', '.');
                        	clslist.add(className);
                        }
                    }
                    zipf.close();	  
    			}
    		}
    		else if (path.trim().toLowerCase().endsWith(".class")){
    			File file = new File(path);
    			if(file.exists()){
    				clslist.add(file.getName().substring(0, file.getName().length()-6));
    			}
    		}
    		else {
		        File dir = new File(path);        		        
		        if (dir.exists()) {
		        	LinkedList<File> dirlist = new LinkedList<File>();
		        	dirlist.add(dir);
		        	File tmp;
		        	while (!dirlist.isEmpty()){
		        		tmp = dirlist.removeFirst();
		        		File[] d = tmp.listFiles();
		        		if (d != null && d.length > 0) for (File f : d){
    		        		if (f.isDirectory()) {
    		        			dirlist.add(f);
    		        		}
    		        		else if (f.getName().trim().toLowerCase().endsWith(".class")){
    		        			clslist.add(f.getAbsolutePath().substring(dir.getAbsolutePath().length()+1, f.getAbsolutePath().length()-6).replace(File.separator, "."));
    		        		}          		        			
		        		}
		        	}
		        }
    		}
    	}catch(Exception e){
			log.error("ClassUtil listing classes error:", e); 		
    	}
    	return clslist;
	}
	
	public ClassLoader getClassLoader(List<String> paths){
        List<URL> pathlist = new LinkedList<URL>();
		for(String path : paths)try{
	    	if (path != null){
	    		if (path.trim().toLowerCase().endsWith(".jar")){ 
	    			File file = new File(path);	            			
	    			if(file.exists()){
	    				pathlist.add(file.toURI().toURL());
	    			}
	    		}
	    		else if (path.trim().toLowerCase().endsWith(".zip")){
	    			File file = new File(path);
	    			if (file.exists()){
	    				pathlist.add(file.toURI().toURL());
	    			}
	    		}
	    		else if (path.trim().toLowerCase().endsWith(".class")){
	    			File file = new File(path);
	    			if(file.exists()){
	    				pathlist.add(file.getParentFile().toURI().toURL());
	    			}
	    		}
	    		else {
			        File dir = new File(path);        		        
			        if (dir.exists()) {
	        			pathlist.add(dir.toURI().toURL());
			        }
	    		}
	    	}
		}catch(Exception e){
			log.error("ClassUtil get classloader error:", e); 				
		}
        return new URLClassLoader(pathlist.toArray(new URL[]{}));
	}
	
	public HashMap<String, Class> compileClassfromSource(HashMap<String, String> sourcemap){
		if(sourcemap != null && sourcemap.size() > 0){
			HashMap<String, Class> ret = new HashMap<String, Class>();
			for(String classname : sourcemap.keySet()) try{
				Class cls = compiler.compile(classname, sourcemap.get(classname), null);
				ret.put(classname, cls);
				return ret;
			}catch(Exception e){
				log.error("Comple classes error:", e); 				
			}
		}
		return null;
	}
}
