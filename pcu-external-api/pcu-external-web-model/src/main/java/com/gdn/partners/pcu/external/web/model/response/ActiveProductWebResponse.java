package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.core.web.dto.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveProductWebResponse {

  private String productSku;

  private String productCode;

  private String productName;

  private String imageUrl;

  private String merchantCode;

  private int variantCount;

  private List<ItemDetailWebResponse> itemDetailWebResponse;
  private Boolean bundleProduct;

}

