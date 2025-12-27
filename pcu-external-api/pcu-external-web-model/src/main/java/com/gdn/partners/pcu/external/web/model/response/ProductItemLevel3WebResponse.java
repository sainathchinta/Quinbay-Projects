package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

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
public class ProductItemLevel3WebResponse {
  private static final long serialVersionUID = 1321315652200356490L;
  private String id;
  private String itemSku;
  private String skuCode;
  private String merchantSku;
  private String upcCode;
  private String itemName;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private Integer dangerousGoodsLevel;
  private Boolean lateFulfillment;
  private String pickupPointCode;
  private String pickupPointName;
  private Integer availableStockLevel1;
  private Integer reservedStockLevel1;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private Integer minimumStock;
  private Boolean synchronizeStock;
  private Boolean off2OnActiveFlag;
  private String pristineId;
  private List<ProductLevel3PriceWebResponse> prices;
  private List<ProductLevel3ViewConfigWebResponse> viewConfigs;
  private List<ProductLevel3ImageWebResponse> images;
  private Double cogs;
  private String cogsErrorCode;
  private boolean promoBundling;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean disableUnSync;
  private boolean priceEditDisabled;
  private boolean itemCampaignMapped;
  private Boolean wholesalePriceActivated;
  private boolean wholesalePromoActivated;
  private List<ProductItemWholesalePriceWebResponse> productItemWholesalePriceResponses = new ArrayList<>();
  private List<ProductItemLevel3LogisticsWebResponse> productItemLevel3LogisticsWebResponses =
      new ArrayList<>();
  private Boolean archived;
  private boolean rejected;
}
