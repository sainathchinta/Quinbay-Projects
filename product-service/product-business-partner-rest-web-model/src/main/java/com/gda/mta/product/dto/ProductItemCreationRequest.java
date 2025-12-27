package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

/**
 * Created by Vishal on 29/06/18.
 */
@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemCreationRequest implements Serializable {

  private static final long serialVersionUID = -5947413341629407592L;
  private String itemGeneratedName;
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
  private List<Image> images;
  private String upcCode;
  private List<ProductItemAttributeValueRequest> productItemAttributeValueRequests;
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests;
  private List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
  private List<PickupPointCreateRequest> pickupPoints = new ArrayList<>();
  private Boolean wholesalePriceActivated;
  private boolean isContentChanged;
  private String sourceItemCode;
  private Set<BundleRecipeRequest> bundleRecipe = new HashSet<>();
  private DistributionItemRequest distributionItemInfoRequest;
  private List<DimensionAndUomRequest> dimensionsAndUOMRequest;

  public ProductItemCreationRequest(String productItemId, String productItemHashCode, Integer productType,
      String merchantSku, String gdnProductItemSku, Double price, Double salePrice, Integer stock,
      boolean markDefaultAddress, Integer minimumStock, String pickupPointId, boolean display,
      boolean buyable, TreeMap<String, String> attributesMap, List<Image> images, List<ProductItemLogisticsRequest> productItemLogisticsRequests) {
    this.productItemId = productItemId;
    this.productItemHashCode = productItemHashCode;
    this.productType = productType;
    this.merchantSku = merchantSku;
    this.gdnProductItemSku = gdnProductItemSku;
    this.price = price;
    this.salePrice = salePrice;
    this.stock = stock;
    this.markDefaultAddress = markDefaultAddress;
    this.minimumStock = minimumStock;
    this.pickupPointId = pickupPointId;
    this.display = display;
    this.buyable = buyable;
    this.attributesMap = attributesMap;
    this.images = images;
    this.productItemLogisticsRequests = productItemLogisticsRequests;
  }
}
