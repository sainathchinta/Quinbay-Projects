package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AnalyticPublisherDTO extends GdnBaseDomainEventModel {
  private String event;
  private String attrtype;
  private String attrname;
  private String attrvalue;
  private String customfield;
  public AnalyticPublisherDTO() {
    super();
  }
  public String getEvent() {
    return event;
  }
  public void setEvent(String event) {
    this.event = event;
  }
  public String getAttrtype() {
    return attrtype;
  }
  public void setAttrtype(String attrtype) {
    this.attrtype = attrtype;
  }
  public String getAttrname() {
    return attrname;
  }
  public void setAttrname(String attrname) {
    this.attrname = attrname;
  }
  public String getAttrvalue() {
    return attrvalue;
  }
  public void setAttrvalue(String attrvalue) {
    this.attrvalue = attrvalue;
  }
  public String getCustomfield() {
    return customfield;
  }
  public void setCustomfield(String customfield) {
    this.customfield = customfield;
  }
  
}
