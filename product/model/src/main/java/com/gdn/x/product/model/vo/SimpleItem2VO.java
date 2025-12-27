package com.gdn.x.product.model.vo;

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
public class SimpleItem2VO implements Serializable {

  private static final long serialVersionUID = -6111444637869024510L;
  private String itemSku;
  private String itemCode;
  private String productSku;
  private boolean isSynchronized;
  private boolean isArchived ;
  private boolean cncActivated;
  private Set<String> activePromoBundlings;
  private boolean off2OnChannelActive ;
  private Date createdDate;
  private SimplePristineDataItemVO simplePristineDataItem;
  private SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItem;
  private boolean isSubscribable;
  private Set<String> preferredSubscriptionType = new HashSet<>();
  private double length;
  private double width;
  private double height;
  private double weight;
  private double shippingWeight;
  private String masterSku;
}
