package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by govind on 03/07/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPriceChangeEventModel extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6391831695666874911L;

  private String itemSku;
  private String pickupPointCode;
  private String productCode;
  private String itemCode;
  private String pristineId;
  private String merchantCode;
  private ParentCategory parentCategory;
  private boolean isArchived;
  private Set<Price> price = new HashSet<Price>();

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public ParentCategory getParentCategory() {
    return parentCategory;
  }

  public void setParentCategory(ParentCategory parentCategory) {
    this.parentCategory = parentCategory;
  }

  public boolean isArchived() {
    return isArchived;
  }

  public void setArchived(boolean archived) {
    isArchived = archived;
  }

  public Set<Price> getPrice() {
    return price;
  }

  public void setPrice(Set<Price> price) {
    this.price = price;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemSku", itemSku).append("productCode", productCode)
        .append("itemCode", itemCode).append("pristineId", pristineId).append("merchantCode", merchantCode)
        .append("parentCategory", parentCategory).append("isArchived", isArchived).append("price", price)
        .append("pickupPointCode", pickupPointCode).toString();
  }
}
