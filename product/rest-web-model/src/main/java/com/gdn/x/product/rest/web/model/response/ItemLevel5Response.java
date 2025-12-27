package com.gdn.x.product.rest.web.model.response;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemLevel5Response extends ItemLevel4ListingResponse {
  private static final long serialVersionUID = 5866439029529618705L;
  private String offlineItemId;
  private String pickupPointCode;
  private Boolean cncActivated;
  private Boolean delivery;
  private List<PriceResponse> prices;
  private List<ViewConfigResponse> viewConfigs;
  private boolean wholesalePriceExists;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private Boolean cncActive;
  private String productName;
  private B2BResponse b2BResponse;
  private double originalSellingPrice;
  private ProductType productType;
  private String categoryCode;
  private List<CategoryDataResponse> categoryHierarchy;
  private String productCode;
  private boolean distribution;
  private PreOrderDTO preOrder;
}
