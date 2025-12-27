package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.List;

import com.gdn.x.product.rest.web.model.WholesaleRule;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemDetailDTO implements Serializable {

  private static final long serialVersionUID = -4329353964999680648L;
  private String productName;
  private String itemName;
  private String merchantCode;
  private String merchantSku;
  private int productTypeCode;
  private String productTypeName;
  private Double shippingWeight;
  private List<ItemCatalogDTO> itemCatalogs;
  private String productCatentryId;
  private String itemCatentryId;
  private String productSku;
  private String brandName;
  private String settlementType;
  private Double itemLength;
  private Double itemWidth;
  private Double itemHeight;
  private String pickupPointCode;
  private String etdNote;
  private Double itemWeight;
  private String ticketTemplateCode;
  private boolean isLateFulfillment;
  private int dangerousLevel;
  private String productCode;
  private boolean installationRequired;
  private boolean off2OnChannelActive;
  private String warrantyInfo;
  private List<String> documentType;
  private PreOrderDTO preOrder;
  private String pristineId;
  private String imageUrl;
  private List<ProductAttributeDetailDTO> masterDataItemAttributes;
  private List<WholesaleRule> wholesaleRules;
  private double offerPrice;
  private boolean buyable;
  private boolean discoverable;
  private double listPrice;

  public ProductItemDetailDTO(String itemName, String merchantCode, String merchantSku,
      int productTypeCode, String productTypeName, Double shippingWeight,
      List<ItemCatalogDTO> itemCatalogs, String productCatentryId, String itemCatentryId,
      String productSku, String brandName, String settlementType, Double itemLength,
      Double itemWidth, Double itemHeight, String pickupPointCode, String etdNote,
      Double itemWeight, String ticketTemplateCode, String productCode, Boolean isLateFulfillment,
      int dangerousLevel) {
    this(itemName, merchantCode, merchantSku, productTypeCode, productTypeName, shippingWeight,
        itemCatalogs, productCatentryId, itemCatentryId, productSku, brandName, settlementType,
        itemLength, itemWidth, itemHeight, pickupPointCode, etdNote, itemWeight, ticketTemplateCode,
        productCode, isLateFulfillment, dangerousLevel, null);
  }
    
  public ProductItemDetailDTO(String itemName, String merchantCode, String merchantSku,
      int productTypeCode, String productTypeName, Double shippingWeight,
      List<ItemCatalogDTO> itemCatalogs, String productCatentryId, String itemCatentryId,
      String productSku, String brandName, String settlementType, Double itemLength,
      Double itemWidth, Double itemHeight, String pickupPointCode, String etdNote,
      Double itemWeight, String ticketTemplateCode, String productCode, Boolean isLateFulfillment,
      int dangerousLevel, String warrantyInfo) {
    super();
    this.itemName = itemName;
    this.merchantCode = merchantCode;
    this.merchantSku = merchantSku;
    this.productTypeCode = productTypeCode;
    this.productTypeName = productTypeName;
    this.shippingWeight = shippingWeight;
    this.itemCatalogs = itemCatalogs;
    this.productCatentryId = productCatentryId;
    this.itemCatentryId = itemCatentryId;
    this.productSku = productSku;
    this.brandName = brandName;
    this.settlementType = settlementType;
    this.itemLength = itemLength;
    this.itemWidth = itemWidth;
    this.itemHeight = itemHeight;
    this.pickupPointCode = pickupPointCode;
    this.etdNote = etdNote;
    this.itemWeight = itemWeight;
    this.ticketTemplateCode = ticketTemplateCode;
    this.productCode = productCode;
    this.isLateFulfillment = isLateFulfillment;
    this.dangerousLevel = dangerousLevel;
    this.warrantyInfo = warrantyInfo;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
}
