package com.gdn.mta.product.valueobject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class FilterMarginsByOrderItemsRequest implements Serializable {

  private static final long serialVersionUID = 2598658322149346188L;
  private List<OrderItem> marginOrderItem = new ArrayList<>();

}