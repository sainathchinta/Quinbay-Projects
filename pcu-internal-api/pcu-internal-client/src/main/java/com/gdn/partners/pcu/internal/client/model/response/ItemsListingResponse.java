package com.gdn.partners.pcu.internal.client.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ItemsListingResponse extends BaseResponse {
  private String itemSku;
  private String masterItemSku;
  private String skuName;
  private String categoryCode;
  private String categoryName;
  private Date mappedDate;
  private Date updatedDate;
  private String action;
  private String brand;
  private double price;
  private String productImage;
  private String pdpRedirectionLink;
  private SellerResponse sellerResponse;
}
