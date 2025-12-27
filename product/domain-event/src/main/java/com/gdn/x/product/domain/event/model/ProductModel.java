package com.gdn.x.product.domain.event.model;

import java.util.Calendar;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.ToString;

@AllArgsConstructor
@Data
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductModel extends Product {

  private long timestamp;

  public ProductModel() {
    this.timestamp = Calendar.getInstance().getTimeInMillis();
  }

}
