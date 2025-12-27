package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProcessImageDomainEvent extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -6837902036065639923L;
  private ProductRequest productRequest;
  private String message;

  public ProcessImageDomainEvent(ProductRequest productRequest, String message) {
    this.productRequest = productRequest;
    this.message = message;
  }

  public ProcessImageDomainEvent() {

  }

  public ProductRequest getProductRequest() {
    return productRequest;
  }

  public void setProductRequest(ProductRequest productRequest) {
    this.productRequest = productRequest;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }
}
