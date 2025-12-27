package com.gdn.mta.product.valueobject;

import java.io.Serializable;
import java.util.Date;

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
public class OrderItem implements Serializable {
  private static final long serialVersionUID = 7891316162604038179L;
  private String storeCode;
  private Date transactionDate;
  private String orderItemId;
  private String categoryCode;
  private String orderType;
}
