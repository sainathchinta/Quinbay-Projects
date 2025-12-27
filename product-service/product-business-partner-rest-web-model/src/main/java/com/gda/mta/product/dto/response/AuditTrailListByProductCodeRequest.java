package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AuditTrailListByProductCodeRequest extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -1514634856825926046L;

  private List<AuditTrailDto> auditTrailResponseList;
  private String businessPartnerCode;
  private String productCode;
  private String accessChannel;
  private String changedBy;
  private String clientId;
  private String requestId;
}
