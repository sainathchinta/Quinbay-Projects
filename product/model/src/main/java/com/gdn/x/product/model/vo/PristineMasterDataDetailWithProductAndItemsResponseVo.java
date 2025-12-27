package com.gdn.x.product.model.vo;

import java.util.List;

import com.gdn.x.product.model.entity.ProductAttributeDetail;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


/**
 * Created by govind on 24/10/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineMasterDataDetailWithProductAndItemsResponseVo
    extends MasterDataDetailWithProductAndItemsResponseVo {


  private List<ProductAndItemsVO> pristineMasterProductAndItems;
  private List<ProductAttributeDetail> productAttributeDetails;


  public List<ProductAndItemsVO> getPristineMasterProductAndItems() {
    return pristineMasterProductAndItems;
  }

  public void setPristineMasterProductAndItems(
      List<ProductAndItemsVO> pristineMasterProductAndItems) {
    this.pristineMasterProductAndItems = pristineMasterProductAndItems;
  }

  public List<ProductAttributeDetail> getProductAttributeDetails() {
    return productAttributeDetails;
  }

  public void setProductAttributeDetails(List<ProductAttributeDetail> productAttributeDetails) {
    this.productAttributeDetails = productAttributeDetails;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("pristineMasterProductAndItems", pristineMasterProductAndItems)
        .append("productAttributeDetails", productAttributeDetails)
        .toString();
  }

}
