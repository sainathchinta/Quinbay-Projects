package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Navya Naveli
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSkuDetailResponse extends BaseResponse {

  private String itemSku;
  private String skuName;
  private String productImage;
  private String masterItemSku;
  private String action;
  private String brand;
  private String pdpRedirectionLink;
  private String categoryCode;
  private String categoryName;
  private double price;
  private int itemSkuCount;
  private SellerResponse seller;
}
