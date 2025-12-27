package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MasterDataDetailWithProductAndItemsResponseVo implements Serializable {

  /**
   *
   */
  private static final long serialVersionUID = 1L;

  private Map<String, MasterDataProduct> masterDataProducts =
      new HashMap<String, MasterDataProduct>();
  private Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
  private List<ProductAndItemsVO> productAndItems = new ArrayList<ProductAndItemsVO>();
  private long activeProductCount;

  public MasterDataDetailWithProductAndItemsResponseVo() {}

  public MasterDataDetailWithProductAndItemsResponseVo(
      Map<String, MasterDataProduct> masterDataProducts,
      Map<String, MasterDataItem> masterDataItems, List<ProductAndItemsVO> productAndItems) {
    super();
    this.masterDataProducts = masterDataProducts;
    this.masterDataItems = masterDataItems;
    this.productAndItems = productAndItems;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public long getActiveProductCount() {
    return this.activeProductCount;
  }

  public Map<String, MasterDataItem> getMasterDataItems() {
    return this.masterDataItems;
  }

  public Map<String, MasterDataProduct> getMasterDataProducts() {
    return this.masterDataProducts;
  }

  public List<ProductAndItemsVO> getProductAndItems() {
    return this.productAndItems;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setActiveProductCount(long activeProductCount) {
    this.activeProductCount = activeProductCount;
  }

  public void setMasterDataItems(Map<String, MasterDataItem> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public void setMasterDataProducts(Map<String, MasterDataProduct> masterDataProducts) {
    this.masterDataProducts = masterDataProducts;
  }

  public void setProductAndItems(List<ProductAndItemsVO> productAndItems) {
    this.productAndItems = productAndItems;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataDetailWithProductAndItemsResponseVo [masterDataProducts=%s, masterDataItems=%s, productAndItems=%s, totalProduct=%s, toString()=%s]",
            this.masterDataProducts, this.masterDataItems, this.productAndItems,
            this.activeProductCount, super.toString());
  }


}
