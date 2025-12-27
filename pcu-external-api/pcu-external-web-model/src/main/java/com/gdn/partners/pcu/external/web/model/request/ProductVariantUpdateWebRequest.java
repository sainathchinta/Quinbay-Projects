package com.gdn.partners.pcu.external.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductVariantUpdateWebRequest extends BaseRequest {

  private static final long serialVersionUID = -3594765958936771851L;
  private List<ProductVariantPriceStockAndImagesWebRequest> productItems = new ArrayList<>();
  private boolean productEditable;
  private boolean synchronize;
  private boolean needCorrection;
  private String productSku;
  private String productCode;
  private String productName;
  private List<ProductLevel3SummaryDetailsImageWebRequest> commonImages = new ArrayList<>();
  private List<ItemPickupPointWebRequest> addPickupPoints;
  private List<ItemPickupPointDeleteWebRequest> deletePickupPoints;
}
