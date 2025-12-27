package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductVariantPriceStockAndImagesRequest implements Serializable {

  private static final long serialVersionUID = 2708430425798540774L;
  private String productItemId;
  private String productCode;
  private String itemSku;
  private String skuCode;
  private String productSku;
  private String merchantSku;
  private String merchantCode;
  private String itemName;
  private Long version;
  private String upcCode;
  private boolean freeSample;
  private boolean newlyAddedItem;
  private TreeMap<String, String> attributesMap = new TreeMap<>();
  private TreeMap<String, String> attributesValueTypeMap = new TreeMap<>();
  private List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
  private List<ItemPickupPointRequest> modifiedItemPickupPoints = new ArrayList<>();

}