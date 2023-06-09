package net.yeeyaa.eight.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject.Kind;


public class CharSequenceCompiler<T> {

   static final String JAVA_EXTENSION = ".java";

   private final ClassLoaderImpl classLoader;


   private final JavaCompiler compiler;


   private final List<String> options;


   private DiagnosticCollector<JavaFileObject> diagnostics;


   private final FileManagerImpl javaFileManager;

   
   public CharSequenceCompiler(ClassLoader loader, Iterable<String> options) {
      compiler = ToolProvider.getSystemJavaCompiler();
      if (compiler == null) {
         throw new IllegalStateException("Cannot find the system Java compiler. "
               + "Check that your class path includes tools.jar");
      }
      classLoader = new ClassLoaderImpl(loader);
      diagnostics = new DiagnosticCollector<JavaFileObject>();
      final JavaFileManager fileManager = compiler.getStandardFileManager(diagnostics,
            null, null);


      javaFileManager = new FileManagerImpl(fileManager, classLoader);
      this.options = new ArrayList<String>();
      if (options != null) { 
         for (String option : options) {
            this.options.add(option);
         }
      }
   }

   
   public synchronized Class<T> compile(final String qualifiedClassName,
         final CharSequence javaSource,
         final DiagnosticCollector<JavaFileObject> diagnosticsList,
         final Class<?>... types) throws CharSequenceCompilerException,
         ClassCastException {
      if (diagnosticsList != null)
         diagnostics = diagnosticsList;
      else
         diagnostics = new DiagnosticCollector<JavaFileObject>();
      Map<String, CharSequence> classes = new HashMap<String, CharSequence>(1);
      classes.put(qualifiedClassName, javaSource);
      Map<String, Class<T>> compiled = compile(classes, diagnosticsList);
      Class<T> newClass = compiled.get(qualifiedClassName);
      return castable(newClass, types);
   }

   
   public synchronized Map<String, Class<T>> compile(
         final Map<String, CharSequence> classes,
         final DiagnosticCollector<JavaFileObject> diagnosticsList)
         throws CharSequenceCompilerException {
      List<JavaFileObject> sources = new ArrayList<JavaFileObject>();
      for (Entry<String, CharSequence> entry : classes.entrySet()) {
         String qualifiedClassName = entry.getKey();
         CharSequence javaSource = entry.getValue();
         if (javaSource != null) {
            final int dotPos = qualifiedClassName.lastIndexOf('.');
            final String className = dotPos == -1 ? qualifiedClassName
                  : qualifiedClassName.substring(dotPos + 1);
            final String packageName = dotPos == -1 ? "" : qualifiedClassName
                  .substring(0, dotPos);
            final JavaFileObjectImpl source = new JavaFileObjectImpl(className,
                  javaSource);
            sources.add(source);



            javaFileManager.putFileForInput(StandardLocation.SOURCE_PATH, packageName,
                  className + JAVA_EXTENSION, source);
         }
      }

      final CompilationTask task = compiler.getTask(null, javaFileManager, diagnostics,
            options, null, sources);
      final Boolean result = task.call();
      if (result == null || !result.booleanValue()) {
         throw new CharSequenceCompilerException("Compilation failed.", classes
               .keySet(), diagnostics);
      }
      try {


         Map<String, Class<T>> compiled = new HashMap<String, Class<T>>();
         for (String qualifiedClassName : classes.keySet()) {
            final Class<T> newClass = loadClass(qualifiedClassName);
            compiled.put(qualifiedClassName, newClass);
         }
         return compiled;
      } catch (ClassNotFoundException e) {
         throw new CharSequenceCompilerException(classes.keySet(), e, diagnostics);
      } catch (IllegalArgumentException e) {
         throw new CharSequenceCompilerException(classes.keySet(), e, diagnostics);
      } catch (SecurityException e) {
         throw new CharSequenceCompilerException(classes.keySet(), e, diagnostics);
      }
   }

   
   @SuppressWarnings("unchecked")
   public Class<T> loadClass(final String qualifiedClassName)
         throws ClassNotFoundException {
      return (Class<T>) classLoader.loadClass(qualifiedClassName);
   }

   
   private Class<T> castable(Class<T> newClass, Class<?>... types)
         throws ClassCastException {
      for (Class<?> type : types)
         if (!type.isAssignableFrom(newClass)) {
            throw new ClassCastException(type.getName());
         }
      return newClass;
   }

   
   static URI toURI(String name) {
      try {
         return new URI(name);
      } catch (URISyntaxException e) {
         throw new RuntimeException(e);
      }
   }

   
   public ClassLoader getClassLoader() {
      return javaFileManager.getClassLoader();
   }
}


final class FileManagerImpl extends ForwardingJavaFileManager<JavaFileManager> {

   private final ClassLoaderImpl classLoader;


   private final Map<URI, JavaFileObject> fileObjects = new HashMap<URI, JavaFileObject>();

   
   public FileManagerImpl(JavaFileManager fileManager, ClassLoaderImpl classLoader) {
      super(fileManager);
      this.classLoader = classLoader;
   }

   
   public ClassLoader getClassLoader() {
      return classLoader;
   }

   
   @Override
   public FileObject getFileForInput(Location location, String packageName,
         String relativeName) throws IOException {
      FileObject o = fileObjects.get(uri(location, packageName, relativeName));
      if (o != null)
         return o;
      return super.getFileForInput(location, packageName, relativeName);
   }

   
   public void putFileForInput(StandardLocation location, String packageName,
         String relativeName, JavaFileObject file) {
      fileObjects.put(uri(location, packageName, relativeName), file);
   }

   
   private URI uri(Location location, String packageName, String relativeName) {
      return CharSequenceCompiler.toURI(location.getName() + '/' + packageName + '/'
            + relativeName);
   }

   
   @Override
   public JavaFileObject getJavaFileForOutput(Location location, String qualifiedName,
         Kind kind, FileObject outputFile) throws IOException {
      JavaFileObject file = new JavaFileObjectImpl(qualifiedName, kind);
      classLoader.add(qualifiedName, file);
      return file;
   }

   @Override
   public ClassLoader getClassLoader(JavaFileManager.Location location) {
      return classLoader;
   }

   @Override
   public String inferBinaryName(Location loc, JavaFileObject file) {
      String result;


      if (file instanceof JavaFileObjectImpl)
         result = file.getName();
      else
         result = super.inferBinaryName(loc, file);
      return result;
   }

   @Override
   public Iterable<JavaFileObject> list(Location location, String packageName,
         Set<Kind> kinds, boolean recurse) throws IOException {
      Iterable<JavaFileObject> result = super.list(location, packageName, kinds,
            recurse);
      ArrayList<JavaFileObject> files = new ArrayList<JavaFileObject>();
      if (location == StandardLocation.CLASS_PATH
            && kinds.contains(JavaFileObject.Kind.CLASS)) {
         for (JavaFileObject file : fileObjects.values()) {
            if (file.getKind() == Kind.CLASS && file.getName().startsWith(packageName))
               files.add(file);
         }
         files.addAll(classLoader.files());
      } else if (location == StandardLocation.SOURCE_PATH
            && kinds.contains(JavaFileObject.Kind.SOURCE)) {
         for (JavaFileObject file : fileObjects.values()) {
            if (file.getKind() == Kind.SOURCE && file.getName().startsWith(packageName))
               files.add(file);
         }
      }
      for (JavaFileObject file : result) {
         files.add(file);
      }
      return files;
   }
}


final class JavaFileObjectImpl extends SimpleJavaFileObject {

   private ByteArrayOutputStream byteCode;


   private final CharSequence source;

   
   JavaFileObjectImpl(final String baseName, final CharSequence source) {
      super(CharSequenceCompiler.toURI(baseName + CharSequenceCompiler.JAVA_EXTENSION),
            Kind.SOURCE);
      this.source = source;
   }

   
   JavaFileObjectImpl(final String name, final Kind kind) {
      super(CharSequenceCompiler.toURI(name), kind);
      source = null;
   }

   
   @Override
   public CharSequence getCharContent(final boolean ignoreEncodingErrors)
         throws UnsupportedOperationException {
      if (source == null)
         throw new UnsupportedOperationException("getCharContent()");
      return source;
   }

   
   @Override
   public InputStream openInputStream() {
      return new ByteArrayInputStream(getByteCode());
   }

   
   @Override
   public OutputStream openOutputStream() {
      byteCode = new ByteArrayOutputStream();
      return byteCode;
   }

   
   byte[] getByteCode() {
      return byteCode.toByteArray();
   }
}


final class ClassLoaderImpl extends ClassLoader {
   private final Map<String, JavaFileObject> classes = new HashMap<String, JavaFileObject>();

   ClassLoaderImpl(final ClassLoader parentClassLoader) {
      super(parentClassLoader);
   }

   
   Collection<JavaFileObject> files() {
      return Collections.unmodifiableCollection(classes.values());
   }

   @Override
   protected Class<?> findClass(final String qualifiedClassName)
         throws ClassNotFoundException {
      JavaFileObject file = classes.get(qualifiedClassName);
      if (file != null) {
         byte[] bytes = ((JavaFileObjectImpl) file).getByteCode();
         return defineClass(qualifiedClassName, bytes, 0, bytes.length);
      }


      try {
         Class<?> c = Class.forName(qualifiedClassName);
         return c;
      } catch (ClassNotFoundException nf) {

      }
      return super.findClass(qualifiedClassName);
   }

   
   void add(final String qualifiedClassName, final JavaFileObject javaFile) {
      classes.put(qualifiedClassName, javaFile);
   }

   @Override
   protected synchronized Class<?> loadClass(final String name, final boolean resolve)
         throws ClassNotFoundException {
      return super.loadClass(name, resolve);
   }

   @Override
   public InputStream getResourceAsStream(final String name) {
      if (name.endsWith(".class")) {
         String qualifiedClassName = name.substring(0,
               name.length() - ".class".length()).replace('/', '.');
         JavaFileObjectImpl file = (JavaFileObjectImpl) classes.get(qualifiedClassName);
         if (file != null) {
            return new ByteArrayInputStream(file.getByteCode());
         }
      }
      return super.getResourceAsStream(name);
   }
}
