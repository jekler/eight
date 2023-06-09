package net.yeeyaa.eight.access.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "serviceResponse", namespace = "http://webservice.access.eight.yeeyaa.net/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "serviceResponse", namespace = "http://webservice.access.eight.yeeyaa.net/")
public class ServiceResponse {
	@XmlElement(name = "return", namespace = "")
	protected String _return;

	public String getReturn() {
		return this._return;
	}

	public void setReturn(String _return) {
		this._return = _return;
	}
}
