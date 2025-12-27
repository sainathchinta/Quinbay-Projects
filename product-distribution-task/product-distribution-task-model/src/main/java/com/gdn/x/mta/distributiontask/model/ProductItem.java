package com.gdn.x.mta.distributiontask.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import org.hibernate.annotations.BatchSize;

import lombok.ToString;

@Table(name = "PDT_PRODUCT_ITEM")
@Entity
public class ProductItem extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  @Column(name = "SKU_CODE")
  private String skuCode;

  @Column(name = "GENERATED_ITEM_NAME")
  private String generatedItemName;

  @Column(name = "UPC_CODE")
  private String upcCode;

  @Column(name = "DANGEROUS_GOODS_LEVEL")
  private Integer dangerousGoodsLevel;

  @Column(name = "ITEM_NOTES")
  private String  itemNotes;

  @Column(name = "ITEM_SKU")
  private String itemSku;

  @Column(name = "MIN_PRICE")
  private Double minPrice;

  @Column(name = "MAX_PRICE")
  private Double maxPrice;

  @ToString.Exclude
  @JoinColumn(name = "PRODUCT")
  @ManyToOne
  @JsonIgnore
  private Product product;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productItem", fetch = FetchType.LAZY,
      orphanRemoval = true)
  private List<ProductItemImage> productItemImages = new ArrayList<>();

  @Column(name = "HASH")
  private byte[] hash;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "productItem", fetch = FetchType.LAZY,
      orphanRemoval = true)
  @BatchSize(size = 5)
  private List<ProductItemAttribute> productItemAttributes = new ArrayList<>();

  @Column(name = "NEWLY_ADDED")
  private boolean newlyAdded;

  public ProductItem() {}

  public ProductItem(Product product, String generatedItemName, String upcCode, String skuCode,
      byte[] hash, Integer dangerousGoodsLevel, String storeId) {
    this(product, upcCode, skuCode, generatedItemName, hash, storeId);
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public ProductItem(Product product, String upcCode, String skuCode, String generatedItemName,
      byte[] hash, String storeId) {
    this.product = product;
    this.upcCode = upcCode;
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.hash = hash;
    this.setStoreId(storeId);
  }

  public Integer getDangerousGoodsLevel() {
    return dangerousGoodsLevel;
  }

  public String getGeneratedItemName() {
    return generatedItemName;
  }

  public byte[] getHash() {
    return hash;
  }

  public Product getProduct() {
    return product;
  }

  public List<ProductItemImage> getProductItemImages() {
    return productItemImages;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public String getUpcCode() {
    return upcCode;
  }

  public void setDangerousGoodsLevel(Integer dangerousGoodsLevel) {
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public void setHash(byte[] hash) {
    this.hash = hash;
  }

  public void setProduct(Product product) {
    this.product = product;
  }

  public void setProductItemImages(List<ProductItemImage> productItemImages) {
    this.productItemImages = productItemImages;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public List<ProductItemAttribute> getProductItemAttributes() {
    return productItemAttributes;
  }

  public void setProductItemAttributes(
      List<ProductItemAttribute> productItemAttributes) {
    this.productItemAttributes = productItemAttributes;
  }

  public String getItemNotes() {
    return itemNotes;
  }

  public void setItemNotes(String itemNotes) {
    this.itemNotes = itemNotes;
  }

  public boolean isNewlyAdded() {
    return newlyAdded;
  }

  public void setNewlyAdded(boolean newlyAdded) {
    this.newlyAdded = newlyAdded;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public Double getMinPrice() {
    return minPrice;
  }

  public void setMinPrice(Double minPrice) {
    this.minPrice = minPrice;
  }

  public Double getMaxPrice() {
    return maxPrice;
  }

  public void setMaxPrice(Double maxPrice) {
    this.maxPrice = maxPrice;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductItem{");
    sb.append("skuCode='").append(skuCode).append('\'');
    sb.append(", generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", upcCode='").append(upcCode).append('\'');
    sb.append(", dangerousGoodsLevel=").append(dangerousGoodsLevel);
    sb.append(", productItemImages=").append(productItemImages);
    sb.append(", hash=").append(Arrays.toString(hash));
    sb.append(", itemNotes=").append(itemNotes);
    sb.append(", productItemAttributes=").append(productItemAttributes);
    sb.append(", newlyAdded=").append(newlyAdded);
    sb.append(", itemSku=").append(itemSku);
    sb.append(", minPrice=").append(minPrice);
    sb.append(", maxPrice=").append(maxPrice);
    sb.append('}');
    return sb.toString();
  }
}
