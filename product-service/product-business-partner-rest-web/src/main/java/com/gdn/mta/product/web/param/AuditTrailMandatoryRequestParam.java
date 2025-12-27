package com.gdn.mta.product.web.param;

import java.io.Serializable;

import lombok.Builder;

@Builder
public class AuditTrailMandatoryRequestParam implements Serializable {
  private static final long serialVersionUID = -4019987027004906523L;
  private String clientHost;

  public String getClientHost() {
    return clientHost;
  }

  public void setClientHost(String clientHost) {
    this.clientHost = clientHost;
  }
}
