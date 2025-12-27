package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointPriceResponse extends BaseResponse {
  private static final long serialVersionUID = 8588443960864226238L;
  private String itemSku;
  private String pickupPointCode;
  private List<DiscountPriceDTO> listOfDiscountPrices = new ArrayList<DiscountPriceDTO>();
  private boolean isBuyable;
  private boolean isDiscoverable;
  private boolean markForDelete;
  private boolean isMerchantPromoDiscountActivated;
  private double offerPrice;
  private double listPrice;
  private DiscountPrice merchantPromoDiscountPrice;
  private boolean cncActive;
  private boolean cncBuyable;
  private boolean cncDiscoverable;
  private String lastUpdatedBy;
  private Date lastUpdatedDate;
}
