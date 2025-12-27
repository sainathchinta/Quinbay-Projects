package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemsDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataDetailWithProductAndItemsResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;
  private Map<String, MasterDataProductDTO> masterDataProducts =
      new HashMap<String, MasterDataProductDTO>();
  private Map<String, MasterDataItemDTO> masterDataItems = new HashMap<String, MasterDataItemDTO>();
  private List<ProductAndItemsDTO> productAndItems = new ArrayList<ProductAndItemsDTO>();
  private int activeProductCount;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public int getActiveProductCount() {
    return this.activeProductCount;
  }

  public Map<String, MasterDataItemDTO> getMasterDataItems() {
    return this.masterDataItems;
  }

  public Map<String, MasterDataProductDTO> getMasterDataProducts() {
    return this.masterDataProducts;
  }

  public List<ProductAndItemsDTO> getProductAndItems() {
    return this.productAndItems;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setActiveProductCount(int activeProductCount) {
    this.activeProductCount = activeProductCount;
  }

  public void setMasterDataItems(Map<String, MasterDataItemDTO> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public void setMasterDataProducts(Map<String, MasterDataProductDTO> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public void setProductAndItems(List<ProductAndItemsDTO> productAndItems) {
    this.productAndItems = productAndItems;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataDetailWithProductAndItemsResponse [masterDataProducts=%s, masterDataItems=%s, productAndItems=%s, activeProductCount=%s, toString()=%s]",
            this.masterDataProducts, this.masterDataItems, this.productAndItems,
            this.activeProductCount, super.toString());
  }


}
