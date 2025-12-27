package com.gda.mta.product.dto.response;


import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.response.BundleItemResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryL4Response extends BaseResponse {
  private static final long serialVersionUID = -8629985438791728766L;
  private String itemSku;
  private String itemCode;
  private String productSku;
  private String sellerSku;
  private String upcCode;
  private String merchantCode;
  private String categoryCode;
  private String generatedItemName;
  private String mainImageUrl;
  private boolean active;
  private String brand;
  private boolean sharedProduct;
  private List<BundleItemResponse> bundleItemResponses;
}
