package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.ArrayList;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineProductAndItemsResponse extends BaseResponse{

  private List<PristineItemResponse> pristineItems = new ArrayList<PristineItemResponse>();
  private List<ProductAttributeDetailDTO> productAttributeDetails;

  public List<PristineItemResponse> getPristineItems() {
    return pristineItems;
  }

  public void setPristineItems(List<PristineItemResponse> pristineItems) {
    this.pristineItems = pristineItems;
  }

  public List<ProductAttributeDetailDTO> getProductAttributeDetails() {
    return productAttributeDetails;
  }

  public void setProductAttributeDetails(List<ProductAttributeDetailDTO> productAttributeDetails) {
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
