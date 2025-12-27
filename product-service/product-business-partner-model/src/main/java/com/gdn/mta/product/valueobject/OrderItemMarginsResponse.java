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
public class OrderItemMarginsResponse implements Serializable {

  private static final long serialVersionUID = 8189582998385805003L;
  private String orderItemId;
  private List<Margin> margins = new ArrayList<>();
}