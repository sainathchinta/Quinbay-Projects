package com.gdn.x.product.domain.event.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class OdooCreationEventModel extends GdnBaseDomainEventModel {
  private String sellerCode;
  private String productCode;
  private String productSku;
  private boolean markForDelete;
  private List<ProductItem> productItems = new ArrayList<>();
  private String brand;
  private String categoryCode;
  private boolean inStore;
  private boolean preOrder;
  private boolean imeiRequired;
}
