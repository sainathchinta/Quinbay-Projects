package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AuditTrailListResponse extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 6848355290854207278L;

  private List<AuditTrailDto> auditTrailResponseList = new ArrayList<>();
  private String accessChannel;
  private String changedBy;
  private String clientId;
  private String requestId;
  private boolean updateDirectly;
  private boolean updateDirectlyToDB;
  private boolean priceFromInRegionPpCode;

  public AuditTrailListResponse(List<AuditTrailDto> auditTrailResponseList, String accessChannel, String changedBy,
      String clientId, String requestId, boolean updateDirectly) {
    this.auditTrailResponseList = auditTrailResponseList;
    this.accessChannel = accessChannel;
    this.changedBy = changedBy;
    this.clientId = clientId;
    this.requestId = requestId;
    this.updateDirectly = updateDirectly;
  }

}
