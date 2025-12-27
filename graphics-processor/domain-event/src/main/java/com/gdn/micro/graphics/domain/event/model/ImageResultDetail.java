package com.gdn.micro.graphics.domain.event.model;

import java.awt.image.ImagingOpException;
import java.io.Serializable;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.ToString;

/**
 * Created by Yudhi K. Surtan on 11/30/2015.
 */
@ToString
public class ImageResultDetail extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -6913216149892657031L;

  private String imagePathLocation;
  private boolean success = false;
  private String requestId;
  private String clientId;
  private String errorMessage;
  private String destinationPath;
  private boolean resize;
  private String productCodeFileName;
  private Boolean isEdited;
  private boolean uploadRequired;
  private boolean uploadedToGCs;
  private String tempFileLocation;
  private boolean active;


  public String getClientId() {
    return clientId;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public String getImagePathLocation() {
    return imagePathLocation;
  }

  public String getRequestId() {
    return requestId;
  }

  public boolean isSuccess() {
    return success;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public void setImagePathLocation(String imagePathLocation) {
    this.imagePathLocation = imagePathLocation;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public String getDestinationPath() {
    return destinationPath;
  }

  public void setDestinationPath(String destinationPath) {
    this.destinationPath = destinationPath;
  }

  public boolean isResize() {
    return resize;
  }

  public void setResize(boolean resize) {
    this.resize = resize;
  }

  public String getProductCodeFileName() {
    return productCodeFileName;
  }

  public void setProductCodeFileName(String productCodeFileName) {
    this.productCodeFileName = productCodeFileName;
  }

  public Boolean getEdited() {
    return isEdited;
  }

  public void setEdited(Boolean edited) {
    isEdited = edited;
  }

  public boolean isUploadRequired() {
    return uploadRequired;
  }

  public void setUploadRequired(boolean uploadRequired) {
    this.uploadRequired = uploadRequired;
  }

  public boolean isUploadedToGCs() {
    return uploadedToGCs;
  }

  public void setUploadedToGCs(boolean uploadedToGCs) {
    this.uploadedToGCs = uploadedToGCs;
  }

  public String getTempFileLocation() {
    return tempFileLocation;
  }

  public void setTempFileLocation(String tempFileLocation) {
    this.tempFileLocation = tempFileLocation;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }
}
