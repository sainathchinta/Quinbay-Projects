package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Image implements Serializable {

  private static final long serialVersionUID = -5704667152880918036L;
  
  private boolean isMainImages = false;
  private Boolean originalImage;
  private String locationPath;
  private Integer sequence;
  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
  private String hashCode;
  private boolean active;
  private String urlPath;
  private boolean edited;
  private boolean revised;
  private boolean commonImage;
  @Getter
  @Setter
  private String reviewType;

  public Image() {}

  public Image(boolean isMainImages, String locationPath, Integer sequence) {
    this.isMainImages = isMainImages;
    this.locationPath = locationPath;
    this.sequence = sequence;
  }
  
  public Image(boolean isMainImages, String locationPath, Integer sequence, String hashCode, boolean active) {
    this(isMainImages, locationPath, sequence);
    this.hashCode = hashCode;
    this.active = active;
  }

  public String getCreatedBy() {
    return this.createdBy;
  }

  public Date getCreatedDate() {
    return this.createdDate;
  }

  public String getId() {
    return this.id;
  }

  public String getLocationPath() {
    return this.locationPath;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public String getUpdatedBy() {
    return this.updatedBy;
  }

  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  public Long getVersion() {
    return this.version;
  }

  public String getHashCode() {
    return hashCode;
  }

  public boolean isMainImages() {
    return this.isMainImages;
  }

  public boolean isMarkForDelete() {
    return this.markForDelete;
  }
  
  public boolean isActive() {
    return active;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setLocationPath(String locationPath) {
    this.locationPath = locationPath;
  }

  public void setMainImages(boolean isMainImages) {
    this.isMainImages = isMainImages;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  public void setHashCode(String hashCode) {
    this.hashCode = hashCode;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public Boolean getOriginalImage() {
    return originalImage;
  }

  public void setOriginalImage(Boolean originalImage) {
    this.originalImage = originalImage;
  }

  public String getUrlPath() {
    return urlPath;
  }

  public void setUrlPath(String urlPath) {
    this.urlPath = urlPath;
  }

  public boolean isEdited() {
    return edited;
  }

  public void setEdited(boolean edited) {
    this.edited = edited;
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
    return String.format("Image [isMainImages=%s, locationPath=%s, sequence=%s, id=%s, "
            + "storeId=%s, version=%s, createdDate=%s, updatedDate=%s, markForDelete=%s, "
            + "hashCode=%s, active=%s, originalImage=%s, urlPath=%s, edited=%s, revised=%s, "
            + "commonImage=%s, reviewType=%s]",
        this.isMainImages, this.locationPath, this.sequence, this.id, this.storeId, this.version,
        this.createdDate, this.updatedDate, this.markForDelete, this.hashCode, this.active,
        this.originalImage, this.urlPath, this.edited, this.revised, this.commonImage,
        this.reviewType);
  }

}
