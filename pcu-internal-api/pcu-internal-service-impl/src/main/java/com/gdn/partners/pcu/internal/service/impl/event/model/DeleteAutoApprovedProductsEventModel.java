package com.gdn.partners.pcu.internal.service.impl.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DeleteAutoApprovedProductsEventModel implements Serializable {

  private static final long serialVersionUID = -5768159391128991874L;
  private String productCode;
  private String action;
  private long timestamp;
}
