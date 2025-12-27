package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 05/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleMasterDataDetailWithProductAndItemsResponseVo implements Serializable{


  private static final long serialVersionUID = 3695444924906408943L;
  private Map<String, SimpleMasterDataProductVO> masterDataProducts =
      new HashMap<String, SimpleMasterDataProductVO>();
  private Map<String, SimpleMasterDataItemVO> masterDataItems =
      new HashMap<String, SimpleMasterDataItemVO>();
  private List<SimpleProductAndItemsVO> productAndItems = new ArrayList<SimpleProductAndItemsVO>();

  public Map<String, SimpleMasterDataProductVO> getMasterDataProducts() {
    return masterDataProducts;
  }

  public void setMasterDataProducts(Map<String, SimpleMasterDataProductVO> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public Map<String, SimpleMasterDataItemVO> getMasterDataItems() {
    return masterDataItems;
  }

  public void setMasterDataItems(Map<String, SimpleMasterDataItemVO> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public List<SimpleProductAndItemsVO> getProductAndItems() {
    return productAndItems;
  }

  public void setProductAndItems(List<SimpleProductAndItemsVO> productAndItems) {
    this.productAndItems = productAndItems;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("masterDataProducts", masterDataProducts)
        .append("masterDataItems", masterDataItems).append("productAndItems", productAndItems)
        .toString();
  }
}
