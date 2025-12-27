package com.gdn.micro.graphics.web.model;

import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

/**
 * Created by Vishal on 09/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class BulkImagesProcessRequest extends BaseRequest{
  private static final long serialVersionUID = -7745152065164960657L;

  private String groupCode;
  private String customGraphicsSettings;
  private List<ImageRequest> imageRequests;
  private boolean isRevised;
  private int prioritySeller;

  public BulkImagesProcessRequest(String groupCode, String customGraphicsSettings,
      List<ImageRequest> imageRequests) {
    this.groupCode = groupCode;
    this.customGraphicsSettings = customGraphicsSettings;
    this.imageRequests = imageRequests;
  }

  public String getGroupCode() {
    return groupCode;
  }

  public void setGroupCode(String groupCode) {
    this.groupCode = groupCode;
  }

  public List<ImageRequest> getImageRequests() {
    return imageRequests;
  }

  public void setImageRequests(List<ImageRequest> imageRequests) {
    this.imageRequests = imageRequests;
  }

  public String getCustomGraphicsSettings() {
    return customGraphicsSettings;
  }

  public void setCustomGraphicsSettings(String customGraphicsSettings) {
    this.customGraphicsSettings = customGraphicsSettings;
  }

  public boolean isRevised() {
    return isRevised;
  }

  public void setRevised(boolean revised) {
    isRevised = revised;
  }

  public int getPrioritySeller() {
    return prioritySeller;
  }

  public void setPrioritySeller(int prioritySeller) {
    this.prioritySeller = prioritySeller;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("groupCode", groupCode)
        .append("customGraphicsSettings", customGraphicsSettings)
        .append("imageRequests", imageRequests).append("isRevised", isRevised).toString();
  }
}
