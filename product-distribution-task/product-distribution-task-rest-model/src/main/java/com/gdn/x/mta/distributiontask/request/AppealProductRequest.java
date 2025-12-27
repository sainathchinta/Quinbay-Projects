package com.gdn.x.mta.distributiontask.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AppealProductRequest implements Serializable {
  private static final long serialVersionUID = 3281983323862422144L;
  String productCode;
  String productSku;
  String businessPartnerCode;
  String notes;
}