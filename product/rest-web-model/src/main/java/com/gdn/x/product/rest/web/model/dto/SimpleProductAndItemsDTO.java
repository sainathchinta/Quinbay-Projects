package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleProductAndItemsDTO implements Serializable{

  private static final long serialVersionUID = -179882124765871990L;
  private SimpleProductResponseDTO simpleProduct;
  private List<SimpleItemResponseDTO> simpleItems = new ArrayList<SimpleItemResponseDTO>();

  public SimpleProductResponseDTO getSimpleProduct() {
    return simpleProduct;
  }

  public void setSimpleProduct(SimpleProductResponseDTO simpleProduct) {
    this.simpleProduct = simpleProduct;
  }

  public List<SimpleItemResponseDTO> getSimpleItems() {
    return simpleItems;
  }

  public void setSimpleItems(List<SimpleItemResponseDTO> simpleItems) {
    this.simpleItems = simpleItems;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("simpleProduct", simpleProduct)
        .append("simpleItems", simpleItems).toString();
  }
}
