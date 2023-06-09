package net.yeeyaa.eight.data.entity;

public class SettingEntity implements java.io.Serializable {
	protected String variable;
	protected String data;

	public SettingEntity() {
	}

	public SettingEntity(String variable, String data) {
		this.variable = variable;
		this.data = data;
	}

	public String getVariable() {
		return this.variable;
	}

	public void setVariable(String variable) {
		this.variable = variable;
	}

	public String getData() {
		return this.data;
	}

	public void setData(String data) {
		this.data = data;
	}
}
