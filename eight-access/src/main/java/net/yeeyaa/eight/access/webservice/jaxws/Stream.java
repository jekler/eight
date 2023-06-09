package net.yeeyaa.eight.access.webservice.jaxws;

import javax.activation.DataHandler;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlMimeType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "stream", namespace = "http://webservice.access.eight.yeeyaa.net/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "stream", namespace = "http://webservice.access.eight.yeeyaa.net/", propOrder = {
		"target", "token", "data" })
public class Stream {
	@XmlElement(name = "target", namespace = "")
	protected String target;
	@XmlElement(name = "token", namespace = "")
	protected String token;
	@XmlMimeType("application/octet-stream")
	@XmlElement(name = "data", namespace = "")
	protected DataHandler data;

	public String getTarget() {
		return this.target;
	}

	public void setTarget(String target) {
		this.target = target;
	}

	public String getToken() {
		return this.token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public DataHandler getData() {
		return this.data;
	}

	public void setData(DataHandler data) {
		this.data = data;
	}
}
