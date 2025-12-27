package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

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
public class AutoApprovalTypeRequestModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -2629907105778530733L;
  private String categoryCode;
  private boolean edited;
  private String reviewType;
  private boolean revised;
  private String productCode;
  private String storeId;
  private String destinationCategoryCode;

}
