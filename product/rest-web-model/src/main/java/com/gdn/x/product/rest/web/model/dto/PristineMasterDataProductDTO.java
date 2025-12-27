package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;


/**
 * Created by govind on 21/03/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineMasterDataProductDTO implements Serializable{

  public PristineMasterDataProductDTO() {
  }
  private static final long serialVersionUID = 1L;
  private String brand;
  private String specificationDetail;
  private String productName;
  private String description;
  private String uniqueSellingPoint;
  private boolean isActivated;
  private boolean isViewable;
  private String uom;
  private String url;
  private MasterCatalogDTO masterCatalog;
  private String brandLogoUrl;

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public boolean isActivated() {
    return isActivated;
  }

  public void setActivated(boolean activated) {
    isActivated = activated;
  }

  public boolean isViewable() {
    return isViewable;
  }

  public void setViewable(boolean viewable) {
    isViewable = viewable;
  }

  public String getUom() {
    return uom;
  }

  public void setUom(String uom) {
    this.uom = uom;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public MasterCatalogDTO getMasterCatalog() {
    return masterCatalog;
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public String getBrandLogoUrl() {
    return brandLogoUrl;
  }

  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("brand", brand)
        .append("specificationDetail", specificationDetail).append("productName", productName)
        .append("description", description).append("uniqueSellingPoint", uniqueSellingPoint)
        .append("isActivated", isActivated).append("isViewable", isViewable).append("uom", uom)
        .append("url", url).append("masterCatalog", masterCatalog)
        .append("brandLogoUrl", brandLogoUrl).toString();
  }
}
