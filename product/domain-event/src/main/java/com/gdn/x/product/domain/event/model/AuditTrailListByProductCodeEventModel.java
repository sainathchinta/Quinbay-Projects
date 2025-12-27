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
public class AuditTrailListByProductCodeEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -1514634856825926046L;

  private List<AuditTrailDtoEventModel> auditTrailResponseList;
  private String businessPartnerCode;
  private String productCode;
  private String accessChannel;
  private String changedBy;
  private String clientId;
  private String requestId;
}