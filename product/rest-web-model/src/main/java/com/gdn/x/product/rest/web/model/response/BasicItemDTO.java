package com.gdn.x.product.rest.web.model.response;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicItemDTO {

  private String merchantCode;
  private String productSku;
  private String itemSku;
  private String itemCode;
  private String pickupPointCode;
  private boolean cncActive;
  private Set<PriceDTO> price;
  private Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<ItemViewConfigDTO>();
  private BasicMasterDataItemDTO masterDataItem;
  private boolean merchantPromoDiscount;

}
