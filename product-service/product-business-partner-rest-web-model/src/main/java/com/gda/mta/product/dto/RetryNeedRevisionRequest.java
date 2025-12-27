package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class RetryNeedRevisionRequest implements Serializable {

  private static final long serialVersionUID = -1274706227696356728L;

  private String storeId;
  private String productCode;
  private String notes;
  private String contentNeedRevision;

  public RetryNeedRevisionRequest(String storeId, String productCode, String notes) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.notes = notes;
  }
}
