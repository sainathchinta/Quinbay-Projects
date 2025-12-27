package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class IndexingRequest implements Serializable {
  private static final long serialVersionUID = -5754263028395827362L;
  private Map<String, Object> request;

  public Map<String, Object> getRequest() {
    return request;
  }

  public void setRequest(Map<String, Object> request) {
    this.request = request;
  }
}
