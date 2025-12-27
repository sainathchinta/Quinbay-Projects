package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPriceStockQuickUpdateResponse extends BaseResponse {
  private static final long serialVersionUID = -437451627227781750L;
  private ApiErrorCode apiErrorCode;
  private List<ItemErrorListResponse> variantsErrorList = new ArrayList<>();
}