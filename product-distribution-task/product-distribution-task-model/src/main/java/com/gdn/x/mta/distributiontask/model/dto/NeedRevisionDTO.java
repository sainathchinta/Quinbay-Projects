package com.gdn.x.mta.distributiontask.model.dto;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class NeedRevisionDTO {
  private boolean eligibleForNeedRevision;
  private WorkflowState workflowState;
}
