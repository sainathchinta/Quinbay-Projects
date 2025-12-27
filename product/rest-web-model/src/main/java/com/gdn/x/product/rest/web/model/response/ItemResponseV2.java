package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemResponseV2 extends BaseResponse {
  private String brand;
  private String categoryCode;
  private String categoryHierarchy;
  private String categoryId;
  private String categoryName;
  private String itemName;
  private String itemSku;
  private String mainImageURL;
  private String merchantCode;
  private String merchantSku;
  private String pickUpPointCode;
  private String pickUpPointName;
  private String productCode;
  private String productName;
  private String productDetailPageLink;
  private String productSku;
  private String skuCode;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean flashSaleActive;
  private boolean freeSample;
  private boolean isArchived;
  private boolean markForDelete;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean off2OnActiveFlag;
  private boolean promoBundling;
  private boolean wholesalePriceConfigEnabled;
  private boolean wholesalePromoActivated;
  private Boolean wholesalePriceActivated;
  private double originalSellingPrice;
  private List<PriceResponse> prices;
  private List<ViewConfigResponse> viewConfigs;
  private List<String> activePromoBundlings;
  private List<String> promoTypes;
  private B2BResponse b2BResponse;
  private boolean isSuspended;
  private PreOrderDTO preOrder;
}
