package com.gdn.micro.graphics.web.helper;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ConvertImageResponse extends BaseResponse {

  private static final long serialVersionUID = 658674126550531407L;

  private String generatedImageLocation;
  private Integer width;
  private Integer height;
  private String imageName;
  private boolean uploadedToGcs;

  public ConvertImageResponse() {
    super();
  }

  public ConvertImageResponse(String generatedImageLocation, String imageName, Integer width,
      Integer height) {
    this.generatedImageLocation = generatedImageLocation;
    this.imageName = imageName;
    this.width = width;
    this.height = height;
  }

  public ConvertImageResponse(String generatedImageLocation, String imageName, Integer width,
      Integer height, boolean uploadedToGcs) {
    this.generatedImageLocation = generatedImageLocation;
    this.imageName = imageName;
    this.width = width;
    this.height = height;
    this.uploadedToGcs = uploadedToGcs;
  }

  public String getGeneratedImageLocation() {
    return generatedImageLocation;
  }

  public Integer getHeight() {
    return height;
  }

  public String getImageName() {
    return imageName;
  }

  public Integer getWidth() {
    return width;
  }

  public void setGeneratedImageLocation(String generatedImageLocation) {
    this.generatedImageLocation = generatedImageLocation;
  }

  public void setHeight(Integer height) {
    this.height = height;
  }

  public void setImageName(String imageName) {
    this.imageName = imageName;
  }

  public void setWidth(Integer width) {
    this.width = width;
  }

  public boolean isUploadedToGcs() {
    return uploadedToGcs;
  }

  public void setUploadedToGcs(boolean uploadedToGcs) {
    this.uploadedToGcs = uploadedToGcs;
  }

  @Override
  public String toString() {
    return "ConvertImageResponse [generatedImageLocation=" + generatedImageLocation + ", width="
        + width + ", height=" + height + ", imageName=" + imageName + ", uploadedToGcs=" + uploadedToGcs + "]";
  }

}
