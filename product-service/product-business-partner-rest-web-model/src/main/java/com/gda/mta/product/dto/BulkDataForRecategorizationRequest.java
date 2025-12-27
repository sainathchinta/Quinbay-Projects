package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by hardikbohra on 01/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDataForRecategorizationRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 8509787908370974299L;

  private List<List<Object>> rowsData;
  private String requestId;
  private String username;
  private String storeId;
  private String recatId;

  public BulkDataForRecategorizationRequest() {
    // default constructor
  }

  public BulkDataForRecategorizationRequest(List<List<Object>> rowsData, String recatId, String requestId,
      String username, String storeId) {
    this.rowsData = rowsData;
    this.requestId = requestId;
    this.username = username;
    this.storeId = storeId;
    this.recatId = recatId;
  }

  public List<List<Object>> getRowsData() {
    return rowsData;
  }

  public void setRowsData(List<List<Object>> rowsData) {
    this.rowsData = rowsData;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getRecatId() {
    return recatId;
  }

  public void setRecatId(String recatId) {
    this.recatId = recatId;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("rowsData", rowsData).append("requestId", requestId).append("username",
        username).append("storeId", storeId).append("recatId", recatId).toString();
  }
}


