package com.gdn.x.mta.distributiontask.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTHistoryEventModel extends GdnBaseDomainEventModel {
  private static final long serialVersionUID = -2099816278785343232L;
  private String productCode;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private Vendor vendor;
  private String reason;
  private WorkflowState state;
  private String storeId;
  private String executor;
  private String taskCode;
}
