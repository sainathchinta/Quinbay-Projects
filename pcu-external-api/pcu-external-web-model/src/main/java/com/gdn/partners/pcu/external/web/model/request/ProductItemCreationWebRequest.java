package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 20/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemCreationWebRequest {

  private String productItemId;
  private String productItemHashCode;
  private Integer productType;
  private String merchantSku;
  private String gdnProductItemSku;
  private Double price;
  private Double salePrice;
  private Integer stock;
  private boolean markDefaultAddress;
  private Integer minimumStock;
  private String pickupPointId;
  private boolean display;
  private boolean buyable;
  private TreeMap<String, String> attributesMap;
  private TreeMap<String, String> attributesValueTypeMap;
  private List<ImageRequest> images;
  private String upcCode;
  private Boolean wholesalePriceActivated;
  private List<ProductItemAttributeValueWebRequest> productItemAttributeValueRequests;
  private List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests;
  private List<PickupPointCreateWebRequest> pickupPoints = new ArrayList<>();
  private boolean isContentChanged;
  private String sourceItemCode;
  private Set<BundleRecipeWebRequest> bundleRecipe;
  private DistributionItemWebRequest distributionItemInfoRequest;
  private List<DimensionAndUomWebRequest> dimensionsAndUOMRequest;
}
