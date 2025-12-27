package com.gda.mta.product.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AvailableToCopyProductDetailsResponse extends BaseResponse {

  private String productSku;

  private String categoryName;

  private String productName;

  private String status;

  private int totalItemSkuCount;

  private List<AvailableToCopyItemDetailsResponse> itemDetails;
}
