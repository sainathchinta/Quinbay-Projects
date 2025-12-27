package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleMasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleMasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductAndItemsDTO;

/**
 * Created by govind on 05/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleProductAndItemsMasterDataDetailResponse extends BaseResponse {

  private Map<String, SimpleMasterDataProductDTO> masterDataProducts =
      new HashMap<String, SimpleMasterDataProductDTO>();
  private Map<String, SimpleMasterDataItemDTO> masterDataItems =
      new HashMap<String, SimpleMasterDataItemDTO>();
  private List<SimpleProductAndItemsDTO> productAndItems = new ArrayList<SimpleProductAndItemsDTO>();

  public Map<String, SimpleMasterDataProductDTO> getMasterDataProducts() {
    return masterDataProducts;
  }

  public void setMasterDataProducts(Map<String, SimpleMasterDataProductDTO> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public Map<String, SimpleMasterDataItemDTO> getMasterDataItems() {
    return masterDataItems;
  }

  public void setMasterDataItems(Map<String, SimpleMasterDataItemDTO> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public List<SimpleProductAndItemsDTO> getProductAndItems() {
    return productAndItems;
  }

  public void setProductAndItems(List<SimpleProductAndItemsDTO> productAndItems) {
    this.productAndItems = productAndItems;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("masterDataProducts", masterDataProducts)
        .append("masterDataItems", masterDataItems).append("productAndItems", productAndItems)
        .toString();
  }
}
