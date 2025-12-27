package com.gdn.partners.pbp.entity.mailEvent;


import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMailDomainEvent extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 123123456L;
  private String notificationType;
  private String merchantCode;
  private String productSku;
  private String productName;
  private String notes;
  private String createdOn;
  private List<List<String>> productDatas;
}
