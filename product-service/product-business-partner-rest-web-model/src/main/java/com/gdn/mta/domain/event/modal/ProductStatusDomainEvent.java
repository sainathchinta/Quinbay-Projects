package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.valueobject.BasicProductDetail;
import com.gdn.partners.pbp.commons.constants.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductStatusDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 7054318082855617899L;
  private String productStatus;
  private String reason;
  private boolean postLive;
  private String businessPartnerCode;
  private BasicProductDetail product;

}
