package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by hardikbohra on 06/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BaseRecategorizationMappingRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1904225079837128667L;

  private String recatId;
  private String status;
  private String requestId;
  private String storeId;
  private String username;

  public BaseRecategorizationMappingRequest() {
  }

  public BaseRecategorizationMappingRequest(String recatId, String status, String requestId, String storeId,
      String username) {
    this.recatId = recatId;
    this.status = status;
    this.requestId = requestId;
    this.storeId = storeId;
    this.username = username;
  }

  public String getRecatId() {
    return recatId;
  }

  public void setRecatId(String recatId) {
    this.recatId = recatId;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("recatId", recatId).append("status", status).append("requestId",
        requestId).append("storeId", storeId).append("username", username).toString();
  }
}
