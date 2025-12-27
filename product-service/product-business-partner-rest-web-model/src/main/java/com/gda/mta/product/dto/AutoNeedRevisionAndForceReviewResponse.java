package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoNeedRevisionAndForceReviewResponse implements Serializable {

  private static final long serialVersionUID = 1473907318068611450L;
  private boolean autoNeedRevision;
  private boolean forceReview;
  private boolean sendProductToReview;
  private Set<String> predictionTypeSet = new HashSet<>();
  private RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType;
  private boolean contentNeedRevision;
  private String notes;
  private boolean brandTakeDown;
  private boolean categoryTakeDown;

  public AutoNeedRevisionAndForceReviewResponse(boolean autoNeedRevision, boolean forceReview,
    boolean sendProductToReview, Set<String> predictionTypeSet,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
    boolean contentNeedRevision, String notes) {
    this.autoNeedRevision = autoNeedRevision;
    this.forceReview = forceReview;
    this.sendProductToReview = sendProductToReview;
    this.predictionTypeSet = predictionTypeSet;
    this.restrictedKeywordsByFieldAndActionType = restrictedKeywordsByFieldAndActionType;
    this.contentNeedRevision = contentNeedRevision;
    this.notes = notes;
  }

  public AutoNeedRevisionAndForceReviewResponse(boolean autoNeedRevision, boolean forceReview,
    boolean sendProductToReview, Set<String> predictionTypeSet,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
    boolean contentNeedRevision, String notes, boolean brandTakeDown) {
    this.autoNeedRevision = autoNeedRevision;
    this.forceReview = forceReview;
    this.sendProductToReview = sendProductToReview;
    this.predictionTypeSet = predictionTypeSet;
    this.restrictedKeywordsByFieldAndActionType = restrictedKeywordsByFieldAndActionType;
    this.contentNeedRevision = contentNeedRevision;
    this.notes = notes;
    this.brandTakeDown = brandTakeDown;
  }

  public AutoNeedRevisionAndForceReviewResponse(boolean autoNeedRevision, boolean forceReview,
      boolean sendProductToReview, Set<String> predictionTypeSet) {
    this.autoNeedRevision = autoNeedRevision;
    this.forceReview = forceReview;
    this.sendProductToReview = sendProductToReview;
    this.predictionTypeSet = predictionTypeSet;
  }

  public AutoNeedRevisionAndForceReviewResponse(boolean autoNeedRevision, boolean forceReview,
      boolean sendProductToReview, Set<String> predictionTypeSet,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    this.autoNeedRevision = autoNeedRevision;
    this.forceReview = forceReview;
    this.sendProductToReview = sendProductToReview;
    this.predictionTypeSet = predictionTypeSet;
    this.restrictedKeywordsByFieldAndActionType = restrictedKeywordsByFieldAndActionType;
  }
}
