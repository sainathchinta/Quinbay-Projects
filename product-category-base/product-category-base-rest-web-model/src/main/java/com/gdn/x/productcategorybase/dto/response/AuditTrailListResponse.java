package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
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

}
