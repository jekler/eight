package net.yeeyaa.eight.access.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "service", namespace = "http://webservice.access.eight.yeeyaa.net/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "service", namespace = "http://webservice.access.eight.yeeyaa.net/")
public class Service {
	@XmlElement(name = "msg", namespace = "")
	protected String msg;

	public String getMsg() {
		return this.msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}
}
