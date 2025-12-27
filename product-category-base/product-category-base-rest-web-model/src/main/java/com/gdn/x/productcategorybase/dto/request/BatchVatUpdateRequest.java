package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BatchVatUpdateRequest implements Serializable {

  private String businessPartnerCode;
  private Map<String, Boolean> itemCodeToVatMap;
}
