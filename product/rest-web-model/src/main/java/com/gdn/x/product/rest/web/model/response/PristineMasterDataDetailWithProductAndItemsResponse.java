package com.gdn.x.product.rest.web.model.response;


import java.util.List;

import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemsDTO;

/**
 * Created by govind on 24/10/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineMasterDataDetailWithProductAndItemsResponse
    extends MasterDataDetailWithProductAndItemsResponse {

  private List<ProductAndItemsDTO> pristineMasterProductAndItems;
  private List<ProductAttributeDetailDTO> productAttributeDetails;

  public List<ProductAndItemsDTO> getPristineMasterProductAndItems() {
    return pristineMasterProductAndItems;
  }

  public void setPristineMasterProductAndItems(
      List<ProductAndItemsDTO> pristineMasterProductAndItems) {
    this.pristineMasterProductAndItems = pristineMasterProductAndItems;
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
        .append("pristineMasterProductAndItems", pristineMasterProductAndItems)
        .append("productAttributeDetails", productAttributeDetails)
        .toString();
  }

}
