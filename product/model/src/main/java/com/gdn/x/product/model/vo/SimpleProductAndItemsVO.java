package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleProductAndItemsVO implements Serializable{

  private static final long serialVersionUID = -5343801826335295781L;
  private SimpleProductVO simpleProduct;
  private List<SimpleItemVO> simpleItems = new ArrayList<>();

  public SimpleProductAndItemsVO() {}

  public SimpleProductAndItemsVO(SimpleProductVO simpleProduct, List<SimpleItemVO> simpleItem) {
    this.simpleProduct = simpleProduct;
    this.simpleItems = simpleItem;
  }

  public SimpleProductVO getSimpleProduct() {
    return simpleProduct;
  }

  public void setSimpleProduct(SimpleProductVO simpleProduct) {
    this.simpleProduct = simpleProduct;
  }

  public List<SimpleItemVO> getSimpleItems() {
    return simpleItems;
  }

  public void setSimpleItems(List<SimpleItemVO> simpleItems) {
    this.simpleItems = simpleItems;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("simpleProduct", simpleProduct)
        .append("simpleItems", simpleItems).toString();
  }
}
