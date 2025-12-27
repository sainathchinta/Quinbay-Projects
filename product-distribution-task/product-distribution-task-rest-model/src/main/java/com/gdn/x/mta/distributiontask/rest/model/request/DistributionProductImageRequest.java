package com.gdn.x.mta.distributiontask.rest.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.Date;

import com.gdn.x.mta.distributiontask.rest.model.base.DistributionBaseRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;

/**
 * Created by virajjasani on 21/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@AllArgsConstructor
public class DistributionProductImageRequest extends DistributionBaseRequest {

  private static final long serialVersionUID = -5781283762065586702L;

  private String locationPath;
  private Integer sequence;
  private boolean mainImage;
  private Boolean originalImage;
  private boolean active;
  private boolean edited;
  private boolean commonImage;

  public DistributionProductImageRequest() {
    // no implementation
  }

  public DistributionProductImageRequest(String locationPath, Integer sequence, boolean mainImage) {
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
  }

  public DistributionProductImageRequest(String locationPath, Integer sequence, boolean mainImage,
      Boolean originalImage) {
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
    this.originalImage = originalImage;
  }

  public DistributionProductImageRequest(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, Long version, String locationPath, Integer sequence, boolean mainImage,
      Boolean originalImage, boolean active) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy, version);
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
    this.originalImage = originalImage;
    this.active = active;
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

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
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
        new StringBuilder("DistributionProductImageRequest{");
    sb.append("locationPath='").append(locationPath).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append(", mainImage=").append(mainImage);
    sb.append(", originalImage=").append(originalImage);
    sb.append(", active=").append(active);
    sb.append(", edited=").append(edited);
    sb.append(", commonImage").append(commonImage);
    sb.append('}');
    return sb.toString();
  }
}
