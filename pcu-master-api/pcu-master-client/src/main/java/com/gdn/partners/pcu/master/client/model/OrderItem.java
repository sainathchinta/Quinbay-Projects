package com.gdn.partners.pcu.master.client.model;

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

  private static final long serialVersionUID = 980539659385073935L;

  private String storeCode;
  private Date transactionDate;
  private String orderItemId;
  private String categoryCode;
  private boolean officialStore;
  private String orderType;
}