package net.yeeyaa.eight.core.processor;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.management.ClassLoadingMXBean;
import java.lang.management.CompilationMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.RuntimeMXBean;
import java.lang.management.ThreadMXBean;
import java.util.Properties;

import net.yeeyaa.eight.IProcessor;


public class SysInfoProcessor implements IProcessor<Object, Object> {
	protected volatile long value;
	protected Object bean;
	protected Type type;
	
	public enum Type{serial, timeSerial, nanoTime, time, gc, objectPendingFinalizationCount, isMemVerbose, setMemVerbose, heapMemoryCommitted, heapMemoryInit, heapMemoryMax, heapMemoryUsed,
		nonHeapMemoryCommitted, nonHeapMemoryInit, nonHeapMemoryMax, nonHeapMemoryUsed, loadedClassCount, isLoaderVerbose, setLoaderVerbose, totalLoadedClassCount, unloadedClassCount,
		compileName, compilationTimeMonitoringSupported, totalCompilationTime, arch, availableProcessors, systemNmae, systemLoadAverage, systemVersion, bootClassPath, classPath, 
		inputArguments, libraryPath, managementSpecVersion, runtimeName, specName, specVendor, specVersion, vmName, vmVendor, vmVersion, startTime, systemProperties, uptime, bootClassPathSupported,
		dumpAllThreads, findDeadlockedThreads, findMonitorDeadlockedThreads, allThreadIds, currentThreadCpuTime, currentThreadUserTime, daemonThreadCount, peakThreadCount, threadCount, 
		threadCpuTime, threadInfo, threadUserTime, totalStartedThreadCount, currentThreadCpuTimeSupported, objectMonitorUsageSupported, synchronizerUsageSupported, threadContentionMonitoringEnabled,
		threadContentionMonitoringSupported, threadCpuTimeEnabled, threadCpuTimeSupported, resetPeakThreadCount, setThreadContentionMonitoringEnabled, setThreadCpuTimeEnabled, property, env,
		clearProperty, securityManager, hash, channel, load, loadLib, mapLib, finalize, in, out, err, properties}

	public void setType(Type type) {
		if (type != null) {
			this.type = type;
			switch (type) {
				case objectPendingFinalizationCount:
				case isMemVerbose:
				case setMemVerbose:
				case heapMemoryCommitted:
				case heapMemoryInit:
				case heapMemoryMax:
				case heapMemoryUsed:
				case nonHeapMemoryCommitted:
				case nonHeapMemoryInit:
				case nonHeapMemoryMax:
				case nonHeapMemoryUsed: bean = ManagementFactory.getMemoryMXBean();
				break;
				case loadedClassCount:
				case isLoaderVerbose:
				case setLoaderVerbose:
				case totalLoadedClassCount:
				case unloadedClassCount: bean = ManagementFactory.getClassLoadingMXBean();
				break;
				case compileName:
				case compilationTimeMonitoringSupported:
				case totalCompilationTime: bean = ManagementFactory.getCompilationMXBean();
				break;
				case arch:
				case availableProcessors:
				case systemNmae:
				case systemVersion:
				case systemLoadAverage: bean = ManagementFactory.getOperatingSystemMXBean();
				break;
				case startTime:
				case systemProperties:
				case uptime:
				case bootClassPathSupported:
				case bootClassPath:
				case classPath:
				case inputArguments:
				case libraryPath:
				case managementSpecVersion:
				case runtimeName:
				case specName:
				case specVendor:
				case specVersion:
				case vmName:
				case vmVendor:
				case vmVersion: bean = ManagementFactory.getRuntimeMXBean();
				break;
				case setThreadContentionMonitoringEnabled:
				case setThreadCpuTimeEnabled:
				case synchronizerUsageSupported:
				case threadContentionMonitoringEnabled:
				case threadContentionMonitoringSupported:
				case threadCpuTimeEnabled:
				case threadCpuTimeSupported:
				case resetPeakThreadCount:				
				case dumpAllThreads:
				case findDeadlockedThreads:
				case findMonitorDeadlockedThreads:
				case allThreadIds:
				case currentThreadCpuTime:
				case currentThreadUserTime:
				case daemonThreadCount:
				case peakThreadCount:
				case threadCount:
				case threadCpuTime:
				case threadInfo:
				case threadUserTime:
				case totalStartedThreadCount:
				case currentThreadCpuTimeSupported:
				case objectMonitorUsageSupported: bean = ManagementFactory.getThreadMXBean();	
			}
		}
	}
	
	@Override
	public Object process(Object instance) {
		if (type == null) {
			if (value == 0) value = System.nanoTime();
			return value;
		} else switch (type) {
			case dumpAllThreads: if(instance instanceof Object[] && ((Object[])instance).length > 1) return ((ThreadMXBean)bean).dumpAllThreads(Boolean.TRUE.equals(((Object[])instance)[0]), Boolean.TRUE.equals(((Object[])instance)[1]));
			break;
			case findDeadlockedThreads: return ((ThreadMXBean)bean).findDeadlockedThreads();
			case findMonitorDeadlockedThreads: return ((ThreadMXBean)bean).findMonitorDeadlockedThreads();
			case allThreadIds: return ((ThreadMXBean)bean).getAllThreadIds();
			case currentThreadCpuTime: return ((ThreadMXBean)bean).getCurrentThreadCpuTime();
			case currentThreadUserTime: return ((ThreadMXBean)bean).getCurrentThreadUserTime();
			case daemonThreadCount: return ((ThreadMXBean)bean).getDaemonThreadCount();
			case peakThreadCount: return ((ThreadMXBean)bean).getPeakThreadCount();
			case threadCount: return ((ThreadMXBean)bean).getThreadCount();
			case threadCpuTime: if(instance instanceof Long) return ((ThreadMXBean)bean).getThreadCpuTime((Long) instance);
			break;
			case threadUserTime:if(instance instanceof Long) return ((ThreadMXBean)bean).getThreadUserTime((Long) instance);
			break;
			case totalStartedThreadCount: return ((ThreadMXBean)bean).getTotalStartedThreadCount();
			case currentThreadCpuTimeSupported: return ((ThreadMXBean)bean).isCurrentThreadCpuTimeSupported();
			case objectMonitorUsageSupported: return ((ThreadMXBean)bean).isObjectMonitorUsageSupported();
			case synchronizerUsageSupported: return ((ThreadMXBean)bean).isSynchronizerUsageSupported();
			case threadContentionMonitoringEnabled: return ((ThreadMXBean)bean).isThreadContentionMonitoringEnabled();
			case threadContentionMonitoringSupported: return ((ThreadMXBean)bean).isThreadContentionMonitoringSupported();		
			case threadCpuTimeEnabled: return ((ThreadMXBean)bean).isThreadCpuTimeEnabled();
			case threadCpuTimeSupported: return ((ThreadMXBean)bean).isThreadCpuTimeSupported();
			case resetPeakThreadCount: ((ThreadMXBean)bean).resetPeakThreadCount();
			break;
			case setThreadContentionMonitoringEnabled: ((ThreadMXBean)bean).setThreadContentionMonitoringEnabled(Boolean.TRUE.equals(instance));
			break;
			case setThreadCpuTimeEnabled: ((ThreadMXBean)bean).setThreadCpuTimeEnabled(Boolean.TRUE.equals(instance));
			break;
			case threadInfo: if(instance instanceof Long) return ((ThreadMXBean)bean).getThreadInfo((Long) instance);
			else if(instance instanceof long[]) return ((ThreadMXBean)bean).getThreadInfo((long[]) instance);
			else if(instance instanceof Object[] && ((Object[])instance).length > 2 && ((Object[])instance)[0] instanceof long[]) return ((ThreadMXBean)bean).getThreadInfo((long[])((Object[])instance)[0], Boolean.TRUE.equals(((Object[])instance)[1]), Boolean.TRUE.equals(((Object[])instance)[2]));
			else if(instance instanceof Object[] && ((Object[])instance).length > 1) if (((Object[])instance)[0] instanceof Long && ((Object[])instance)[1] instanceof Integer) return ((ThreadMXBean)bean).getThreadInfo((Long)((Object[])instance)[0], (Integer)((Object[])instance)[1]);
			else if (((Object[])instance)[0] instanceof long[] && ((Object[])instance)[1] instanceof Integer) return ((ThreadMXBean)bean).getThreadInfo((long[])((Object[])instance)[0], (Integer)((Object[])instance)[1]);
			break;	
			case serial: synchronized (this) {
				return value ++;
			} case timeSerial: synchronized (this) {
				if (value == 0) value = System.nanoTime();
				return value ++;
			} case nanoTime: return System.nanoTime();
			case time: return System.currentTimeMillis();
			case gc: System.gc();
			break;
			case objectPendingFinalizationCount: return ((MemoryMXBean)bean).getObjectPendingFinalizationCount();
			case isMemVerbose: return ((MemoryMXBean)bean).isVerbose();
			case setMemVerbose: ((MemoryMXBean)bean).setVerbose(Boolean.TRUE.equals(instance));
			break;
			case heapMemoryCommitted: return ((MemoryMXBean)bean).getHeapMemoryUsage().getCommitted();
			case heapMemoryInit: return ((MemoryMXBean)bean).getHeapMemoryUsage().getInit();
			case heapMemoryMax: return ((MemoryMXBean)bean).getHeapMemoryUsage().getMax();
			case heapMemoryUsed: return ((MemoryMXBean)bean).getHeapMemoryUsage().getUsed();
			case nonHeapMemoryCommitted: return ((MemoryMXBean)bean).getNonHeapMemoryUsage().getCommitted();
			case nonHeapMemoryInit: return ((MemoryMXBean)bean).getNonHeapMemoryUsage().getInit();
			case nonHeapMemoryMax: return ((MemoryMXBean)bean).getNonHeapMemoryUsage().getMax();
			case nonHeapMemoryUsed: return ((MemoryMXBean)bean).getNonHeapMemoryUsage().getUsed();	
			case loadedClassCount: return ((ClassLoadingMXBean)bean).getLoadedClassCount();
			case totalLoadedClassCount: return ((ClassLoadingMXBean)bean).getTotalLoadedClassCount();
			case unloadedClassCount: return ((ClassLoadingMXBean)bean).getUnloadedClassCount();
			case isLoaderVerbose: return ((ClassLoadingMXBean)bean).isVerbose();
			case setLoaderVerbose: ((ClassLoadingMXBean)bean).setVerbose(Boolean.TRUE.equals(instance));
			break;
			case compileName: return ((CompilationMXBean)bean).getName();
			case compilationTimeMonitoringSupported: return ((CompilationMXBean)bean).isCompilationTimeMonitoringSupported();
			case totalCompilationTime: return ((CompilationMXBean)bean).getTotalCompilationTime();
			case arch: return ((OperatingSystemMXBean)bean).getArch();
			case availableProcessors: return ((OperatingSystemMXBean)bean).getAvailableProcessors();
			case systemNmae: return ((OperatingSystemMXBean)bean).getName();
			case systemLoadAverage: return ((OperatingSystemMXBean)bean).getSystemLoadAverage();
			case systemVersion: return ((OperatingSystemMXBean)bean).getVersion();
			case bootClassPath: return ((RuntimeMXBean)bean).getBootClassPath();
			case classPath: return ((RuntimeMXBean)bean).getClassPath();
			case inputArguments: return ((RuntimeMXBean)bean).getInputArguments();
			case libraryPath: return ((RuntimeMXBean)bean).getLibraryPath();
			case managementSpecVersion: return ((RuntimeMXBean)bean).getManagementSpecVersion();
			case runtimeName: return ((RuntimeMXBean)bean).getName();
			case specName: return ((RuntimeMXBean)bean).getSpecName();
			case specVendor: return ((RuntimeMXBean)bean).getSpecVendor();
			case specVersion: return ((RuntimeMXBean)bean).getSpecVersion();
			case vmName: return ((RuntimeMXBean)bean).getVmName();
			case vmVendor: return ((RuntimeMXBean)bean).getVmVendor();
			case vmVersion: return ((RuntimeMXBean)bean).getVmVersion();
			case startTime: return ((RuntimeMXBean)bean).getStartTime();
			case systemProperties: return ((RuntimeMXBean)bean).getSystemProperties();
			case uptime: return ((RuntimeMXBean)bean).getUptime();
			case bootClassPathSupported: return ((RuntimeMXBean)bean).isBootClassPathSupported();		
			case property: if (instance == null) return System.getProperties();
			else System.getProperty(instance.toString());
			case env: if (instance == null) return System.getenv();
			else System.getenv(instance.toString());
			case clearProperty: if (instance != null) return System.clearProperty(instance.toString());
			break;
			case securityManager: if (instance instanceof SecurityManager) {
				System.setSecurityManager((SecurityManager) instance);
				break;
			} else return System.getSecurityManager();
			case hash: return System.identityHashCode(instance);
			case channel: try {
				return System.inheritedChannel();
			} catch (IOException e) {
				return null;
			}
			case load: if (instance != null) System.load(instance.toString());
			break;
			case loadLib: if (instance != null) System.loadLibrary(instance.toString());
			break;
			case mapLib: if (instance != null) return System.mapLibraryName(instance.toString());
			break;
			case finalize: System.runFinalization();
			break;
			case err: if (instance instanceof PrintStream) System.setErr((PrintStream) instance);
			break;
			case in: if (instance instanceof InputStream) System.setIn((InputStream) instance);
			break;
			case out: if (instance instanceof PrintStream) System.setOut((PrintStream) instance);
			break;
			case properties: if (instance instanceof Properties) System.setProperties((Properties)instance);
			else if (instance instanceof Object[] && ((Object[])instance).length > 1 && ((Object[])instance)[0] instanceof String && ((Object[])instance)[1] instanceof String) System.setProperty(((Object[])instance)[0].toString(), ((Object[])instance)[1].toString());
		}
		return instance;
	}
}
