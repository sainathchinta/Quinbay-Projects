package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.commons.lang3.builder.ToStringBuilder;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineProductAndItemsResponseVO implements Serializable {

  private List<PristineItemVO> pristineItems;
  private List<ProductAttributeDetail> productAttributeDetails;

  public List<PristineItemVO> getPristineItems() {
    return pristineItems;
  }

  public void setPristineItems(List<PristineItemVO> pristineItems) {
    this.pristineItems = pristineItems;
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
        .append("pristineItems", pristineItems)
        .append("productAttributeDetails", productAttributeDetails)
        .toString();
  }
}
