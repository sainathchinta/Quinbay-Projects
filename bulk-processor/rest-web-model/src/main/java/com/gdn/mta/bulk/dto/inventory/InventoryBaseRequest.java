package com.gdn.mta.bulk.dto.inventory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class InventoryBaseRequest implements Serializable {
  private static final long serialVersionUID = 8159841818356290819L;

  private String uniqueId;
  private String cartId;
  private String customerLogonId;
  private String orderId;
  private String orderItemId;
  private String orderItemStatus;
  private String actionKey;
  private String sequenceActionKey;
  private String additionalKey;
  private String trackingId;
  private String originator;
  private String transactionDescription;
  private Date transactionDate;
}