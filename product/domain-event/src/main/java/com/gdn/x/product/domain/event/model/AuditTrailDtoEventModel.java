package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AuditTrailDtoEventModel {

  private String businessPartnerCode;
  private String gdnSku;
  private String actionKey;
  private String oldValue;
  private String newValue;
  private String attributeName;
  private String productSku;
  private String name;
  private String pickupPointCode;
  private boolean onlineStatus;
}