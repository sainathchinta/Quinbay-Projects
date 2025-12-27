package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.gdn.x.product.rest.web.model.response.PristineItemResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;


/**
 * Created by govind on 21/03/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineProductAndItemsDTO implements Serializable{

  public PristineProductAndItemsDTO() {
  }

  private static final long serialVersionUID = 1L;

  private PristineProductResponse product;
  private List<PristineItemResponse> items = new ArrayList<PristineItemResponse>();

  public PristineProductResponse getProduct() {
    return product;
  }

  public void setProduct(PristineProductResponse product) {
    this.product = product;
  }

  public List<PristineItemResponse> getItems() {
    return items;
  }

  public void setItems(List<PristineItemResponse> items) {
    this.items = items;
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
    return new ToStringBuilder(this).append("product", product).append("items", items).toString();
  }
}
