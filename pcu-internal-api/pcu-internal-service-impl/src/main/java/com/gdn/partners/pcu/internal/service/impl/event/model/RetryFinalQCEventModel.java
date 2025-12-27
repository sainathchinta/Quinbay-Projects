package com.gdn.partners.pcu.internal.service.impl.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RetryFinalQCEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -8279884067409584092L;
  private String productCode;
}