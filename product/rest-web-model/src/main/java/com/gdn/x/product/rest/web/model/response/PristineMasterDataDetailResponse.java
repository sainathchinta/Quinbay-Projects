package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.PristineMasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.PristineMasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PristineProductAndItemsDTO;

/**
 * Created by govind on 21/03/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineMasterDataDetailResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;
  private Map<String, PristineMasterDataProductDTO> masterDataProducts =
      new HashMap<String, PristineMasterDataProductDTO>();
  private Map<String, PristineMasterDataItemDTO> masterDataItems = new HashMap<String, PristineMasterDataItemDTO>();
  private List<PristineProductAndItemsDTO> productAndItems = new ArrayList<PristineProductAndItemsDTO>();


  public Map<String, PristineMasterDataProductDTO> getMasterDataProducts() {
    return masterDataProducts;
  }

  public void setMasterDataProducts(Map<String, PristineMasterDataProductDTO> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public Map<String, PristineMasterDataItemDTO> getMasterDataItems() {
    return masterDataItems;
  }

  public void setMasterDataItems(Map<String, PristineMasterDataItemDTO> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public List<PristineProductAndItemsDTO> getProductAndItems() {
    return productAndItems;
  }

  public void setProductAndItems(List<PristineProductAndItemsDTO> productAndItems) {
    this.productAndItems = productAndItems;
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
    return new ToStringBuilder(this).append("masterDataProducts", masterDataProducts)
        .append("masterDataItems", masterDataItems).append("productAndItems", productAndItems)
        .toString();
  }

}
