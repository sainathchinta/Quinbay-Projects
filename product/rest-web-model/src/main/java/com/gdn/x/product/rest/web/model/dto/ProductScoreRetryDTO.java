package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductScoreRetryDTO implements Serializable {

  private String storeId;
  private String productCode;
  private String productSku;
  private boolean updateCategory;
  private String userName;
  private String requestId;
}
