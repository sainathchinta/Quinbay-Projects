package com.gda.mta.product.dto;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by Vishal on 11/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageInformationRequest extends GdnBaseDomainEventModel implements Serializable{

  private static final long serialVersionUID = 3859437441599219362L;
  private String imagePathLocation;
  private String hashCode;
  private boolean success;
  private String clientId;
  private String errorMessage;

  public String getImagePathLocation() {
    return imagePathLocation;
  }

  public void setImagePathLocation(String imagePathLocation) {
    this.imagePathLocation = imagePathLocation;
  }

  public String getHashCode() {
    return hashCode;
  }

  public void setHashCode(String hashCode) {
    this.hashCode = hashCode;
  }

  public boolean isSuccess() {
    return success;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Override
  public boolean equals(Object o) {
    return GdnObjects.equals(o, this);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("imagePathLocation", imagePathLocation)
        .append("hashCode", hashCode).append("success", success).append("clientId", clientId)
        .append("errorMessage", errorMessage).toString();
  }
}
