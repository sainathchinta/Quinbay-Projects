package com.gdn.x.mta.distributiontask.request;

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
public class RemoveProductRequest implements Serializable {

  private String productCode;
  private String state;

  public RemoveProductRequest(String productCode) {
    this.productCode = productCode;
  }
}
