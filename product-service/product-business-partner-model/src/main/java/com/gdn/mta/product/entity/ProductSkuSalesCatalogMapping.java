package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

/**
 * Created by hardikbohra on 03/06/18.
 */
@Entity
@Table(name = ProductSkuSalesCatalogMapping.TABLE_NAME)
public class ProductSkuSalesCatalogMapping extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_PRODUCT_SKU_SALES_CATALOG_MAPPING";
  public static final String COLUMN_SALES_CATALOG = "SALES_CATALOG";
  public static final String COLUMN_SALES_CATEGORY = "SALES_CATEGORY";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_RECAT_ID = "RECAT_ID";
  public static final String COLUMN_STATUS = "STATUS";

  @Column(name = COLUMN_RECAT_ID, nullable = false)
  private String recatId;

  @Column(name = COLUMN_SALES_CATALOG, nullable = false)
  private String salesCatalog;

  @Column(name = COLUMN_SALES_CATEGORY, nullable = false)
  private String salesCategory;

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  public ProductSkuSalesCatalogMapping() {
    // default constructor
  }

  public ProductSkuSalesCatalogMapping(String recatId, String salesCatalog, String salesCategory, String productSku,
      String status, String storeId) {
    this.recatId = recatId;
    this.salesCatalog = salesCatalog;
    this.salesCategory = salesCategory;
    this.productSku = productSku;
    this.status = status;
    setStoreId(storeId);
  }

  public String getRecatId() {
    return recatId;
  }

  public void setRecatId(String recatId) {
    this.recatId = recatId;
  }

  public String getSalesCatalog() {
    return salesCatalog;
  }

  public void setSalesCatalog(String salesCatalog) {
    this.salesCatalog = salesCatalog;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public String getSalesCategory() {
    return salesCategory;
  }

  public void setSalesCategory(String salesCategory) {
    this.salesCategory = salesCategory;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("recatId", recatId).append("salesCatalog", salesCatalog).append
        ("salesCategory", salesCategory).append("productSku", productSku).append("status", status).toString();
  }
}
