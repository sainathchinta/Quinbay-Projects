package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties
@JsonInclude
public class ProductSalesCategoryMappingResponse extends BaseResponse {

  private String productCode;
  private List<String> oldSalesCategoryCodes;
  private List<String> newSalesCategoryCodes;
  private List<String> newUmkmSalesCategoryCodes;
  private List<String> oldB2bSalesCategoryCodes;
  private List<String> newB2bSalesCategoryCodes;
}
