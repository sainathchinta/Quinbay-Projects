package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties
public class ProductItemDetailVO implements Serializable {

  public static final class ItemDetailVOBuilder implements GdnBaseBuilder<ProductItemDetailVO> {

    private ProductItemDetailVO productItemDetailVO;

    public ItemDetailVOBuilder() {
      this.productItemDetailVO = new ProductItemDetailVO();
    }

    public ItemDetailVOBuilder brandName(String brandName) {
      this.productItemDetailVO.setBrandName(brandName);
      return this;
    }

    @Override
    public ProductItemDetailVO build() {
      ProductItemDetailVO returnObject = this.productItemDetailVO;
      this.productItemDetailVO = new ProductItemDetailVO();
      return returnObject;
    }

    public ItemDetailVOBuilder dangerousLevel(int dangerousLevel) {
      this.productItemDetailVO.setDangerousLevel(dangerousLevel);
      return this;
    }

    public ItemDetailVOBuilder etdNote(String etdNote) {
      this.productItemDetailVO.setEtdNote(etdNote);
      return this;
    }

    public ItemDetailVOBuilder installationRequired(boolean installationRequired) {
      this.productItemDetailVO.setInstallationRequired(installationRequired);
      return this;
    }

    public ItemDetailVOBuilder isLateFulfillment(Boolean isLateFulfillment) {
      this.productItemDetailVO.setLateFulfillment(isLateFulfillment);
      return this;
    }

    public ItemDetailVOBuilder itemCatalogs(List<ItemCatalogVO> itemCatalogs) {
      this.productItemDetailVO.setItemCatalogs(itemCatalogs);
      return this;
    }

    public ItemDetailVOBuilder itemCatentryId(String itemCatentryId) {
      this.productItemDetailVO.setItemCatentryId(itemCatentryId);
      return this;
    }

    public ItemDetailVOBuilder itemHeight(Double itemHeight) {
      this.productItemDetailVO.setItemHeight(itemHeight);
      return this;
    }

    public ItemDetailVOBuilder itemLength(Double itemLength) {
      this.productItemDetailVO.setItemLength(itemLength);
      return this;
    }

    public ItemDetailVOBuilder itemName(String itemName) {
      this.productItemDetailVO.setItemName(itemName);
      return this;
    }

    public ItemDetailVOBuilder itemSku(String itemSku) {
      this.productItemDetailVO.setItemSku(itemSku);
      return this;
    }

    public ItemDetailVOBuilder itemWeight(Double itemWeight) {
      this.productItemDetailVO.setItemWeight(itemWeight);
      return this;
    }

    public ItemDetailVOBuilder itemWidth(Double itemWidth) {
      this.productItemDetailVO.setItemWidth(itemWidth);
      return this;
    }

    public ItemDetailVOBuilder merchantCode(String merchantCode) {
      this.productItemDetailVO.setMerchantCode(merchantCode);
      return this;
    }

    public ItemDetailVOBuilder merchantSku(String merchantSku) {
      this.productItemDetailVO.setMerchantSku(merchantSku);
      return this;
    }

    public ItemDetailVOBuilder off2OnChannelActive(boolean off2OnChannelActive) {
      this.productItemDetailVO.setOff2OnChannelActive(off2OnChannelActive);
      return this;
    }

    public ItemDetailVOBuilder pickupPointCode(String pickupPointCode) {
      this.productItemDetailVO.setPickupPointCode(pickupPointCode);
      return this;
    }

    public ItemDetailVOBuilder productCatentryId(String productCatentryId) {
      this.productItemDetailVO.setProductCatentryId(productCatentryId);
      return this;
    }

    public ItemDetailVOBuilder productCode(String productCode) {
      this.productItemDetailVO.setProductCode(productCode);
      return this;
    }

    public ItemDetailVOBuilder productSku(String productSku) {
      this.productItemDetailVO.setProductSku(productSku);
      return this;
    }

    public ItemDetailVOBuilder productTypeCode(int productTypeCode) {
      this.productItemDetailVO.setProductTypeCode(productTypeCode);
      return this;
    }

    public ItemDetailVOBuilder productTypeName(String productTypeName) {
      this.productItemDetailVO.setProductTypeName(productTypeName);
      return this;
    }

    public ItemDetailVOBuilder settlementType(String settlementType) {
      this.productItemDetailVO.setSettlementType(settlementType);
      return this;
    }

    public ItemDetailVOBuilder shippingWeight(Double shippingWeight) {
      this.productItemDetailVO.setShippingWeight(shippingWeight);
      return this;
    }

    public ItemDetailVOBuilder ticketTemplateCode(String ticketTemplateCode) {
      this.productItemDetailVO.setTicketTemplateCode(ticketTemplateCode);
      return this;
    }

    public ItemDetailVOBuilder warrantyInfo(String warrantyInfo) {
      this.productItemDetailVO.setWarrantyInfo(warrantyInfo);
      return this;
    }

    public ItemDetailVOBuilder cncActive(Boolean cncActive) {
      this.productItemDetailVO.setCncActive(cncActive);
      return this;
    }

    public ItemDetailVOBuilder documentType(List<String> documentType) {
      this.productItemDetailVO.setDocumentType(documentType);
      return this;
    }

    public ItemDetailVOBuilder pickupPointName(String pickupPointName) {
      this.productItemDetailVO.setPickupPointName(pickupPointName);
      return this;
    }
  }

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String itemName;
  private String merchantCode;
  private String merchantSku;
  private int productTypeCode;
  private String productTypeName;
  private String settlementType;
  private List<ItemCatalogVO> itemCatalogs;
  private String productCatentryId;
  private String itemCatentryId;
  private String productSku;
  private String brandName;
  private Double shippingWeight;
  private Double itemLength;
  private Double itemWidth;
  private Double itemHeight;
  private String pickupPointCode;
  private String etdNote;
  private Double itemWeight;
  private String ticketTemplateCode;
  private boolean installationRequired;
  private boolean isLateFulfillment;
  private int dangerousLevel;
  private String productCode;
  private boolean off2OnChannelActive;
  private String warrantyInfo;
  private PreOrderVO preOrder;
  private String productName;
  private String pristineId;
  private String imageUrl;
  private List<MasterDataItemAttributeVO> masterDataItemAttributes;
  private List<WholesaleRuleVO> wholesaleRules;
  private double offerPrice;
  private boolean buyable;
  private boolean discoverable;
  private double listPrice;
  private boolean cncActive;
  private List<String> documentType;
  private String pickupPointName;
  private boolean imeiRequired;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
}
