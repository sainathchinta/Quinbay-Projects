package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

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
public class SimpleItemV2ResponseDTO implements Serializable {

  private static final long serialVersionUID = 7105321414393354133L;
  private String itemSku;
  private String itemCode;
  private String productSku;
  private boolean isSynchronized;
  private boolean isArchived ;
  private boolean cncActivated;
  private Date createdDate;
  private boolean off2OnChannelActive;
  private SimplePristineDataItemDTO simplePristineDataItem;
  private SimpleAsyncMasterDataItemDTO simpleAsyncMasterDataItem;
  private boolean isSubscribable;
  private Set<String> preferredSubscriptionType = new HashSet<>();
  private double length;
  private double width;
  private double height;
  private double weight;
  private double shippingWeight;
  private String masterSku;
}
