package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.enums.AutoApprovalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageQcProcessedResponseDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6325544790841232136L;
  private String productCode;
  private int productPredictionScore;
  private String imageViolations;
  private String textViolations;
  private String imageQcResponse;
  private boolean forceReview;
  private AutoApprovalType autoApprovalType;
  private AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse;
  private String predictedBrand;
  private String categoryCode;
}
