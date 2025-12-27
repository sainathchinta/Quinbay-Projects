package com.gdn.mta.bulk.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageTmpMap implements Serializable  {
  
  private static final long serialVersionUID = 7997333826859078214L;
  private String generatedImageName;
  private String tmpImageName;
  
  public ImageTmpMap() {
    super();
  }
  public ImageTmpMap(String generatedImageName, String tmpImageName) {
    super();
    this.generatedImageName = generatedImageName;
    this.tmpImageName = tmpImageName;
  }
  public String getGeneratedImageName() {
    return generatedImageName;
  }
  public void setGeneratedImageName(String generatedImageName) {
    this.generatedImageName = generatedImageName;
  }
  public String getTmpImageName() {
    return tmpImageName;
  }
  public void setTmpImageName(String tmpImageName) {
    this.tmpImageName = tmpImageName;
  }
  
}
