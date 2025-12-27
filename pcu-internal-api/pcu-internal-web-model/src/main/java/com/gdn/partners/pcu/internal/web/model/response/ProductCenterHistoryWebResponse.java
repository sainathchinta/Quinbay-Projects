package com.gdn.partners.pcu.internal.web.model.response;

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
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCenterHistoryWebResponse {

  private String productSku;
  private Long date;
  private String activity;
  private String description;
  private String user;
}
