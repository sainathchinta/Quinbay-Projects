package com.gdn.micro.graphics.domain.event.model;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by Vishal on 10/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -1388900692689911919L;
  private String imagePathLocation;
  private String hashCode;
  private boolean success;
  private boolean commonImage;
  private String clientId;
  private String errorMessage;
  private String destinationPath;
  private boolean resize;
  private String productCodeFileName;
  private Boolean isEdited;
  private boolean uploadRequired;

  public ImageResponse(String imagePathLocation, String hashCode, boolean success, String clientId,
      String errorMessage) {
    this.imagePathLocation = imagePathLocation;
    this.hashCode = hashCode;
    this.success = success;
    this.clientId = clientId;
    this.errorMessage = errorMessage;
  }


  public ImageResponse() {
  }

  public String getHashCode() {
    return hashCode;
  }

  public void setHashCode(String hashCode) {
    this.hashCode = hashCode;
  }

  public String getClientId() {
    return clientId;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public String getImagePathLocation() {
    return imagePathLocation;
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

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
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

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    return "ImageResponse{" + "imagePathLocation='" + imagePathLocation + '\'' + ", hashCode='" + hashCode + '\''
        + ", success=" + success + ", commonImage=" + commonImage + ", clientId='" + clientId + '\''
        + ", errorMessage='" + errorMessage + '\'' + ", destinationPath='" + destinationPath + '\'' + ", resize="
        + resize + ", productCodeFileName='" + productCodeFileName + '\'' + ", isEdited=" + isEdited
        + ", uploadRequired=" + uploadRequired + '}';
  }
}
