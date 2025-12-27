package com.gdn.mta.bulk.dto.product;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemV2Request implements Serializable {
  private static final long serialVersionUID = 5333219168479768362L;

  private String upcCode;
  private String merchantSku;
  private double price;
  private double salePrice;
  private int stock;
  private int minimumStock;
  private boolean displayable;
  private boolean buyable;
  private List<String> images;
  private TreeMap<String, String> attributesMap;
  private int dangerousGoodsLevel;
  private Map<String, String> nonDefiningItemAttributes;
  private Boolean wholesalePriceActivated;
  private List<ProductWholesaleRequest> wholesale;

  public ProductItemV2Request() {
    super();
  }

}
