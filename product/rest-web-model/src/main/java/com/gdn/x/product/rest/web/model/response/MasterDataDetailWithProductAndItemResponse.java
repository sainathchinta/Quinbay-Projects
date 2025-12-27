package com.gdn.x.product.rest.web.model.response;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataDetailWithProductAndItemResponse extends BaseResponse {

  private Map<String, MasterDataProductDTO> masterDataProducts = new HashMap<String, MasterDataProductDTO>();
  private Map<String, MasterDataItemDTO> masterDataItems = new HashMap<String, MasterDataItemDTO>();
  private ProductResponse product;
  private ItemResponse item;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Map<String, MasterDataItemDTO> getMasterDataItems() {
    return this.masterDataItems;
  }

  public Map<String, MasterDataProductDTO> getMasterDataProducts() {
    return this.masterDataProducts;
  }

  public ProductResponse getProduct() {
    return product;
  }

  public ItemResponse getItem() {
    return item;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setMasterDataItems(Map<String, MasterDataItemDTO> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public void setMasterDataProducts(Map<String, MasterDataProductDTO> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public void setProduct(ProductResponse product) {
    this.product = product;
  }

  public void setItem(ItemResponse item) {
    this.item = item;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataDetailWithProductAndItemResponse [masterDataProducts=%s, masterDataItems=%s, product=%s, item=%s, toString()=%s]",
            this.masterDataProducts, this.masterDataItems, this.product, this.item, super.toString());
  }


}
