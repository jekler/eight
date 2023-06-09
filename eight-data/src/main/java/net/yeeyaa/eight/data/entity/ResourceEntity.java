package net.yeeyaa.eight.data.entity;

import java.util.Map;

public class ResourceEntity implements java.io.Serializable {
	protected String variable;
	protected Map ext;

	public ResourceEntity() {
	}

	public ResourceEntity(String variable, Map ext) {
		this.variable = variable;
		this.ext = ext;
	}

	public String getVariable() {
		return this.variable;
	}

	public void setVariable(String variable) {
		this.variable = variable;
	}

	public Map getExt() {
		return this.ext;
	}

	public void setExt(Map ext) {
		this.ext = ext;
	}
}
