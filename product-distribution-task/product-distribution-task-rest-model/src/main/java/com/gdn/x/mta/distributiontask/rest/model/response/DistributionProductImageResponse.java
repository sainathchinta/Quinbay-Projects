package com.gdn.x.mta.distributiontask.rest.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;


/**
 * Created by virajjasani on 20/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionProductImageResponse extends BaseResponse {

  private static final long serialVersionUID = -7874013420303069888L;

  private String locationPath;
  private Integer sequence;
  private boolean mainImage;
  private Boolean originalImage;
  private boolean active;
  private boolean edited;
  private boolean revised;
  private boolean commonImage;

  public DistributionProductImageResponse() {
    // no implementation
  }

  public DistributionProductImageResponse(String locationPath, Integer sequence,
      boolean mainImage) {
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
  }

  public String getLocationPath() {
    return locationPath;
  }

  public void setLocationPath(String locationPath) {
    this.locationPath = locationPath;
  }

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public boolean isMainImage() {
    return mainImage;
  }

  public void setMainImage(boolean mainImage) {
    this.mainImage = mainImage;
  }

  public Boolean getOriginalImage() {
    return originalImage;
  }

  public void setOriginalImage(Boolean originalImage) {
    this.originalImage = originalImage;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public boolean isRevised() {
    return revised;
  }

  public void setRevised(boolean revised) {
    this.revised = revised;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("DistributionProductImageResponse{");
    sb.append("locationPath='").append(locationPath).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append(", mainImage=").append(mainImage);
    sb.append(", originalImage").append(originalImage);
    sb.append(", active").append(active);
    sb.append(", edited").append(edited);
    sb.append(", revised").append(revised);
    sb.append(", commonImage").append(commonImage);
    sb.append('}');
    return sb.toString();
  }
}
