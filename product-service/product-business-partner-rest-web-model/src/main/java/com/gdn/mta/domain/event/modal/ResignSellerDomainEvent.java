package com.gdn.mta.domain.event.modal;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ResignSellerDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private String storeId;
  private String businessPartnerCode;
}
