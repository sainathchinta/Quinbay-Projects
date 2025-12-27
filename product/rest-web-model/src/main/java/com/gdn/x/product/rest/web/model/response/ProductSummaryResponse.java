package com.gdn.x.product.rest.web.model.response;

import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSummaryResponse extends BaseResponse {
  private static final long serialVersionUID = 1L;
  private String productSku;
  private String productName;
  private MasterCatalogDTO masterCatalog;
  private List<SalesCatalogDTO> salesCatalogs;

  public ProductSummaryResponse() {}

  public ProductSummaryResponse(String productSku, String productName,
      MasterCatalogDTO masterCatalog, List<SalesCatalogDTO> salesCatalogs) {
    super();
    this.productSku = productSku;
    this.productName = productName;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public MasterCatalogDTO getMasterCatalog() {
    return this.masterCatalog;
  }

  public String getProductName() {
    return this.productName;
  }

  public String getProductSku() {
    return this.productSku;
  }

  public List<SalesCatalogDTO> getSalesCatalogs() {
    return this.salesCatalogs;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setMasterCatalog(MasterCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSalesCatalogs(List<SalesCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductSummaryResponse [productSku=%s, productName=%s, masterCatalog=%s, salesCatalogs=%s, toString()=%s]",
            this.productSku, this.productName, this.masterCatalog, this.salesCatalogs,
            super.toString());
  }

}
