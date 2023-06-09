package net.yeeyaa.eight.core.pipeline;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class PipeEntity implements Serializable{
	public Integer threadMode;
	public Integer threadCount;
	public String pool;
	public Boolean wait;
	public Map<String, NodeEntity> nodes;
	
	public static class NodeEntity implements Serializable{
		public String name;
		public String bean;
		public Map<String, String> paras;
		public Integer threadMode;
		public Integer threadCount;
		public String pool;
		public Boolean wait;
		public List<String> linktos;
	}
}
