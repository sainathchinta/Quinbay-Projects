package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ItemL4SummaryResponse {

  private String storeId;
  private String itemSku;
  private String itemCode;
  private String generatedItemName;
  private String merchantSku;
  private String pickupPointCode;
  private String pickupPointName;
  private boolean pickupPointCncActive;
  private boolean pickupPointDeliveryActive;
  private String productType;
  private Date createdDate;
  private Date updatedDate;
  private boolean markForDelete;
  private boolean promoBundling;
  private boolean cncActivated;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActivated;
  private boolean priceEditDisabled;
  private boolean forceReview;
  private boolean wholesalePriceExists;
  private Boolean wholesalePriceActivated;
  private Boolean isLateFulfillment;
  private List<String> promoTypes = new ArrayList<String>();
  private List<String> promoLabels = new ArrayList<String>();
  private Set<String> activePromoBundlings = new HashSet<String>();
  private Set<PriceDTO> price;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private List<ItemViewConfigDTO> itemViewConfigs;
  private B2bFieldsDTO b2bFieldsDTO;
  private Long version;
  private boolean freeSample;
  private boolean distribution;
}
