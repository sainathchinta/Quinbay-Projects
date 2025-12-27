package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 2124296238512846535L;
  private String productSku;
  private String merchantCode;
  private ProductEventModel product;
  private List<ItemEventModel> items = new ArrayList<>();
  private boolean rejected;
  private boolean needToOverrideL4DetailsFromL5;
  private Map<String, Object> fieldsAndValues;
  private boolean skipInventoryCallForAtomicUpdate = true;

  public ProductAndItemEventModel(String productSku, ProductEventModel product, List<ItemEventModel> items,
      boolean rejected, boolean needToOverrideL4DetailsFromL5, String merchantCode) {
    this.productSku = productSku;
    this.product = product;
    this.items = items;
    this.rejected = rejected;
    this.needToOverrideL4DetailsFromL5 = needToOverrideL4DetailsFromL5;
    this.merchantCode = merchantCode;
  }

  public ProductAndItemEventModel(String productSku, Map<String, Object> fieldsAndValues,
    String merchantCode) {
    this.productSku = productSku;
    this.fieldsAndValues = fieldsAndValues;
    this.merchantCode = merchantCode;
  }
}
